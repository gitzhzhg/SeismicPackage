!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- stathelper.f90 --------------------------------!!
!!---------------------------- stathelper.f90 --------------------------------!!
!!---------------------------- stathelper.f90 --------------------------------!!


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
! Name       : STATHELPER 
! Category   : math
! Written    : 2000-06-16   by: Tom Stoeckley
! Revised    : 2007-04-24   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Helper module for statics processes IMS, FISH, and SISC.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive encapsulates common code for the IMS, FISH, and SISC statics
! process modules, and potentially for other surface-consistent residual
! statics processes.
!
!-------------------------------------------------------------------------------
!                  WHAT THIS PRIMITIVE WILL DO FOR YOU 
!
! This primitive owns the following process parameters, and also includes the
! GUI layout and context sensitive help for them:
!
!     hdr_flag       dead_end       
!     path_src       hdr_src        hdr_sx         hdr_sy
!     path_rec       hdr_src        hdr_rx         hdr_ry      
!     corr_files     len_corr       no_dead
!
! This primitive also encapsulates the LATWIN primitive with its associated
! process parameters, GUI layout, and context sensitive help.
!
! This primitive also encapsulates the following major primitives:
!
!     LATWIN       (getting trace correlation windows)
!     STATSHIFT    (optional saving and output of shifted traces, using
!                    TEMPTFILE and SHFT)
!     PACK8IO      (reading and writing individual and stacked trace windows)
!     STATFILE     (object which manages a single static file, using
!                    PACK8IO, PERMTFILE, and STATIO, including saving
!                    static files and correlation files)
!     STATUTIL     (various static file utilities)
!
! This primitive also encapsulates the following:
!     The cumulative source and receiver statics arrays.
!     A number of additional statics parameters for each file.
!     Information about the CMP bins and offsets.
!     Correlation function file I/O.
!
! This primitive also registers the following parameters:
!
!     call pc_put_global  ('numtr'       ,    1    )
!     call pc_put_global  ('gathered'    , .false. )
!     call pc_put_control ('ntapes'      ,    0    )
!     call pc_put_control ('need_request', .true.  )
!     call pc_put_control ('need_label'  , .true.  )
!     call pc_put_control ('twosets'     , .false. )
!     call pc_put_control ('nscratch'    ,    0    )   ! cannot be known.
!     call pc_put_control ('nstore'      ,    0    )   ! cannot be known.
!     call pc_put_control ('iftd'        , .true.  )
!     call pc_put_control ('ndisk'       ,    0    )   ! cannot be known.
!     call pc_put_control ('setup_only'  , .false. )
!
!-------------------------------------------------------------------------------
!                       HOW TO USE THIS PRIMITIVE
!
! The calling process must include in its documentation information in the
! following documentation sections in this primitive:
!
!    trace_in_doc: TRACE INPUT REQUIREMENTS
!   trace_out_doc: TRACE OUTPUT CHARACTERISTICS
!      global_doc: PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
! header_word_doc: TRACE HEADER WORDS USED OR CHANGED
!     calling_doc: SPECIFIC CALLING CHARACTERISTICS
!
! The calling process must include the following two lines in its GUI layout:
!
!     < include stathelper.f90 >
!     < include latwin.f90     >
!
! To use this primitive from a process module named xxxx:
!
!     stathelper_create     should be called from xxxx_create.
!     stathelper_initialize should be called from xxxx_initialize.
!     stathelper_update     should be called from xxxx_update.
!     stathelper_delete     should be called from xxxx_delete.
!
! These routines should be called from the main trace processing routine:
!
!     stathelper_input_traces
!     stathelper_output_traces
!
! The rest of the routines should be called from xxxx_solve.
!
!-------------------------------------------------------------------------------
!                   THE MAIN TRACE PROCESSING ROUTINE
!
! The main trace processing subroutine in the statics processing module which
! uses this primitive should look like this:
!
!     subroutine xxxx (obj,ntr,hd,tr)
!     implicit none
!     type(xxxx_struct),intent(inout) :: obj                    ! arguments
!     integer          ,intent(inout) :: ntr                    ! arguments
!     double precision ,intent(inout) :: hd(:,:)                ! arguments
!     real             ,intent(inout) :: tr(:,:)                ! arguments
!     logical                         :: error                  ! local
!
!     if(ntr /= NEED_TRACES) then
!          call stathelper_input_traces (obj%helper,ntr,hd,tr)
!          if (ntr == NEED_TRACES) return
!     end if
!
!     if (ntr == NO_MORE_TRACES) then
!          call xxxx_solve (obj,error)
!          if (error) then
!               ntr = FATAL_ERROR
!          else
!               ntr = NEED_TRACES
!          end if
!     end if
!
!     if(ntr == NEED_TRACES .or. ntr == FATAL_ERROR) then
!          call xxxx_wrapup (obj)
!          call stathelper_output_traces (obj%helper,ntr,hd,tr)
!     end if
!     return
!     end subroutine xxxx
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
! CALLED from XXXX_CREATE:
!
!                                       o     i
!              call stathelper_create (obj,procname)
!
! type(stathelper_struct) obj = pointer to the STATHELPER structure.
! character(len=*)   procname = name of statics process calling this primitive.
!
!-------------------------------------------------------------------------------
! CALLED from XXXX_INITIALIZE:
!
!                                           b
!              call stathelper_initialize (obj)
!
! type(stathelper_struct) obj = pointer to the STATHELPER structure.
!
!-------------------------------------------------------------------------------
! CALLED from XXXX_DELETE:
!
!                                       b
!              call stathelper_delete (obj)
!
! type(stathelper_struct) obj = pointer to the STATHELPER structure.
!
!-------------------------------------------------------------------------------
! CALLED from XXXX_UPDATE:
!
!                             b     i         i         i
!    call stathelper_update (obj,num_iter,max_static,converge,
!                            ra_src,ra_rec,hdr_corr_src,hdr_corr_rec,error)
!                              i      i         i            i         o
!
! type(stathelper_struct)    obj = pointer to the STATHELPER structure.
! integer               num_iter = total number of iterations.
! real                max_static = maximum static shift in milliseconds.
! real                  converge = convergence criterion in milliseconds.
! integer                 ra_src = running average in X dir for source file.
! integer                 ra_rec = running average in X dir for receiver file.
! character(len=*)  hdr_corr_src = 'REC' or 'CMP' or 'SOFF' or 'AOFF'.
! character(len=*)  hdr_corr_rec = 'SRC' or 'CMP' or 'SOFF' or 'AOFF'.
! logical                  error = error flag (true if an error occurred).
!
! SX_RUN and RX_RUN are ground position ranges, not number of points.
! SX_RUN and RX_RUN are used only for SISC files.
!
! HDR_CORR_SRC and HDR_CORR_REC must be 'CMP' for the FISH and IMS processes.
!
!-------------------------------------------------------------------------------
! CALLED from XXXX_UPDATE or XXXX_SOLVE (after calling stathelper_update):
!
!            nfiles      = stathelper_get_nfiles       (obj)
!            nwin        = stathelper_get_nwin         (obj)
!            itwin       = stathelper_get_itwin        (obj)
!            ibwin       = stathelper_get_ibwin        (obj)
!            ncorr       = stathelper_get_ncorr        (obj)
!            is_source   = stathelper_file_is_source   (obj,ifile)
!            is_receiver = stathelper_file_is_receiver (obj,ifile)
!               o                                        i    i
!
! type(stathelper_struct) obj = pointer to the STATHELPER structure.
! integer               ifile = static file index (1 or 2).
! integer              nfiles = number of static files (1 or 2) to solve for.
! integer                nwin = number of values in trace correlation window.
! integer               itwin = index of first value in trace corr window.
! integer               ibwin = index of last  value in trace corr window.
! integer               ncorr = number of values in correlation function.
! logical           is_source = whether IFILE is a source file.
! logical         is_receiver = whether IFILE is a receiver file.
!
!-------------------------------------------------------------------------------
! CALLED from the MAIN TRACE PROCESSING ROUTINE:
!
!                                          b   b  i  i
!          call stathelper_input_traces  (obj,ntr,hd,tr)
!          call stathelper_output_traces (obj,ntr,hd,tr)
!                                          b   b  o  o
!
! type(stathelper_struct) obj = pointer to the STATHELPER structure.
! integer                 ntr = number of input or output traces.
! double precision    hd(:,:) = input or output headers.
! real                tr(:,:) = input or output traces.
!
!-------------------------------------------------------------------------------
! CALLED only from XXXX_SOLVE:
!
!                                           i    i     o    o
!    call stathelper_read_shifted_window  (obj,itrace,win,error)
!
!                                           i    i    o    o    o
!    call stathelper_read_stacked_window  (obj,icmp,ifold,win,error)
!    call stathelper_read_base_window     (obj,icmp,ifold,win,error)
!
!                                           i    i    i    i    o
!    call stathelper_write_stacked_window (obj,icmp,ifold,win,error)
!    call stathelper_write_base_window    (obj,icmp,ifold,win,error)
!
!                           i   i   i               o     o
!    call stathelper_corr (obj,win,ref,            corr,denom)
!    call stathelper_pick (obj,corr,ccmin,denom,   static,ccoef)
!                           i   i     i     i        o      o
!
!    call stathelper_report_static 
!                      (obj,ifile,igp,corr,static,ccoef,nnt,nnc,error)
!                        b    i    i   i     i      i    i   i    o
!
!    call stathelper_begin_iteration      (obj,          error)
!    call stathelper_end_iteration        (obj,converged,error)
!                                           b      o       o
!
!    call stathelper_begin_file_iteration (obj,ifile,error)
!    call stathelper_end_file_iteration   (obj,ifile,error)
!                                           b    i     o
!
!    call stathelper_sort_to_cmps         (obj)
!    call stathelper_sort_to_file         (obj,ifile)
!                                           b    i
!
!         ntraces  = stathelper_get_ntraces  (obj)
!         ncmp     = stathelper_get_ncmp     (obj)
!         nxcmp    = stathelper_get_nxcmp    (obj)
!         nycmp    = stathelper_get_nycmp    (obj)
!         noff     = stathelper_get_noff     (obj)
!           o                                  i
!
!         ngp      = stathelper_get_ngp      (obj,ifile)
!         igp      = stathelper_get_this_igp (obj,ifile,itrace)
!           o                                  i    i     i
!
!         irecord  = stathelper_get_irecord  (obj,itrace)
!         igp      = stathelper_get_igp      (obj,itrace)  ! for current sort.
!         icorr    = stathelper_get_icorr    (obj,itrace)  ! for current sort.
!         icmp     = stathelper_get_icmp     (obj,itrace)
!         isoff    = stathelper_get_isoff    (obj,itrace)
!         iaoff    = stathelper_get_iaoff    (obj,itrace)
!         ixcmp    = stathelper_get_ixcmp    (obj,itrace)
!         iycmp    = stathelper_get_iycmp    (obj,itrace)
!           o                                  i     i
!
!
! type(stathelper_struct) obj = pointer to the STATHELPER structure.
! logical               error = error flag (true if an error occurred).
!
! integer      ifile = static file index (1 or 2).
! integer        igp = index of source or receiver static value.
! real     win(nwin) = trace correlation window.
! real     ref(nwin) = reference trace correlation window.
! real   corr(ncorr) = trace correlation function.
! real         denom = normalizing denominator.
! real         ccmin = minimum correlation coefficient for good pick.
! real         ccoef = correlation coefficient.
! real        static = source or receiver static value (in sample
!                                interval units) to add to cumulative statics.
! real         ccoef = correlation coefficient (normalized).
! integer        nnt = number of traces used in the correlation.
! integer        nnc = number of correlations added together.
! logical  converged = whether the iterations have converged.
!
! integer  itrace  = which trace desired (1 thru ntraces) in current sort order.
! integer  ntraces = number of traces.
! integer  ncmp    = total number of CMPs.
! integer  nxcmp   = number of CMPs in X direction.
! integer  nycmp   = number of CMPs in Y direction.
! integer  noff    = total number of offsets.
! integer  ngp     = total number of ground positions.
! integer  irecord = record number on disk.
! integer  ifold   = fold of this stacked trace or base trace.
! integer  igp     = sequential ground position index.
! integer  icorr   = sequential correlation index (other file or cmp or offset).
! integer  icmp    = sequential CMP index.
! integer  isoff   = signed offset index.
! integer  iaoff   = absolute offset index.
! integer  ixcmp   = X CMP index.
! integer  iycmp   = Y CMP index.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
! 12. 2007-04-24  Stoeckley  Never set NUMTR to zero.
!011. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!010. 2006-01-10  B. Menger  Removed Unused Variables.
!  9. 2003-06-16  Goodger    Add use getlun_module.
!  8. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
!  7. 2001-05-14  Stoeckley  Change default correlation file extension to trc8.
!  6. 2000-09-27  Stoeckley  Add protective code to guard against problems
!                             when trace header values fall on the edge of
!                             a bin.
!  5. 2000-08-21  Stoeckley  Add STATHELPER_GET_THIS_IGP; fix some
!                             documentation errors; fix to allow IMS to
!                             read and write base traces; fix bugs in
!                             STATHELPER_GET_IXCMP and STATHELPER_GET_IYCMP.
!  4. 2000-08-08  Stoeckley  Minor documentation change regarding header words
!                             7, 8, WIN_HDR_X, and WIN_HDR_Y.
!  3. 2000-07-20  Stoeckley  Change STATHELPER_FILE_IS_SOURCE and
!                             STATHELPER_FILE_IS_RECEIVER to allow them to
!                             be called from the process update routine;
!                             replace PACK8IO with TEMPTFILE.
!  2. 2000-06-23  Stoeckley  Add functions STATHELPER_GET_ITWIN and
!                             STATHELPER_GET_IBWIN; fix bug regarding
!                             SISC static file names.
!  1. 2000-06-16  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!-------------------------------------------------------------------------------
!                            (trace_in_doc)
!                        TRACE INPUT REQUIREMENTS
!          (this section should be included in the calling process)
!
! This process is an all-trace (loop-splitting) process.
! This process allows traces to be input in gathers or one at a time.
! This process allows traces to be input in any sort order.
! Traces should have been expanded with XP or MVXP or equivalent.
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!                             (trace_out_doc)
!                      TRACE OUTPUT CHARACTERISTICS
!          (this section should be included in the calling process)
!
! If DEAD_END = YES, this process does not output any traces.
! If DEAD_END = NO, this process outputs the same traces it received,
! one at a time, shifted by the statics solution, in the same order it
! received them.
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!                             (global_doc)
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!          (this section should be included in the calling process)
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       input unused; set to 1.
! GATHERED  whether traces are a legitimate gather  input unused; set to false.
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!                          (header_word_doc)
!                    TRACE HEADER WORDS USED OR CHANGED
!          (this section should be included in the calling process)
!
! Hwd#       Description                       Action taken
! ----       -----------                       ------------
!  6         offset                            used but not changed
!  7         CMP X grid coordinate             used but not changed
!  8         CMP Y grid coordinate             used but not changed
! HDR_FLAG   trace flag                        used but not changed
! HDR_SX     source X ground position          used but not changed
! HDR_SY     source Y ground position          used but not changed
! HDR_RX     source X ground position          used but not changed
! HDR_RY     source Y ground position          used but not changed
! WIN_HDR_X  first coord of window location    used but not changed
! WIN_HDR_Y  second coord of window location   used but not changed
!  2         top mute index                    used (updated if DEAD_END is NO)
! 64         bottom mute index                 used (updated if DEAD_END is NO)
! 43         cumulative residual static             (updated if DEAD_END is NO)
! 25         largest absolute value                     (set if DEAD_END is NO)
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!                             (calling_doc)
!                    SPECIFIC CALLING CHARACTERISTICS
!          (this section should be included in the calling process)
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed (unknown).
! NSTORE           0       amount of permanent memory needed (unknown).
! IFTD           true      whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large (unknown).
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces (DEAD_END = NO).
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces (DEAD_END = NO).
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<gui_def>
!                  Input and Output Trace Specifications
! `---------------------------------------------------------------------------
!                     HDR_FLAG=`I        DEAD_END=`CC
! `---------------------------------------------------------------------------
!
!               Input and Output Static File Specifications
! `---------------------------------------------------------------------------
!   [/L]Source file:    HDR_SRC=`CCCC      HDR_SX=`X  HDR_SY=`X
!   [/L]Receiver file:  HDR_REC=`CCCC      HDR_RX=`X  HDR_RY=`X
!   Select PATH_SRC[PATH_SRC]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [PATH_SRC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!   Select PATH_REC[PATH_REC]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [PATH_REC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   CORR_FILES=`CCCC   LEN_CORR=`FFFFFF ms   NO_DEAD=`CC
! `---------------------------------------------------------------------------
!<PARMS path_src     [/ML=128/XST]>
!<PARMS path_rec     [/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATH_SRC">
!<Tip> Choose PATH_SRC using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATH_REC">
!<Tip> Choose PATH_REC using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_SRC_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_SRC. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_REC_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_REC. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_SRC">
!<Tip> Pathname for the file which will contain source statics. </Tip>
! Default = NONE
! Allowed = char
!
! If PATH_SRC = NONE, then no source statics will be calculated.
! PATH_SRC and PATH_REC cannot both be set to NONE.
! Static files will will have an extension named after the static process.
!
! For the SISC process, a static increment file with the extension .inc is
! also saved.
!
! If there are multiple iterations, files are saved after each iteration,
! overwriting the previous contents of the file.  The iteration count will
! be included in the file history cards.
!</Help>
!
!
!<Help KEYWORD="PATH_REC">
!<Tip> Pathname for the file which will contain receiver statics. </Tip>
! Default = NONE
! Allowed = char
!
! If PATH_REC = NONE, then no receiver statics will be calculated.
! PATH_SRC and PATH_REC cannot both be set to NONE.
! Static files will will have an extension named after the static process.
!
! For the SISC process, a static increment file with the extension .inc is
! also saved.
!
! If there are multiple iterations, files are saved after each iteration,
! overwriting the previous contents of the file.  The iteration count will
! be included in the file history cards.
!</Help>
!
!
!<Help KEYWORD="CORR_FILES">
!<Tip> Type of static correlation files to save. </Tip>
! Default = TRC8
! Allowed = TRC8 (save TRCIO byte files with the extension .trc8)
! Allowed = NONE (do not save correlation files)
!
! The names of the static correlation files will be the same as PATH_SRC and
! PATH_REC except for the extension.
!
! Choice NONE is not allowed for the FISH and SISC processes.
!
! If there are multiple iterations, files are saved after each iteration,
! overwriting the previous contents of the file.
!
! A CBYT option allows the correlation files to be plotted and edited easily.
! This option automatically updates the statics files based on correlation
! edits.
!</Help>
!
!
!<Help KEYWORD="HDR_SRC">
!<Tip> Header word(s) to use to identify source locations. </Tip>
! Default = SEQU
! Allowed = SEQU  (source sequential ground position - header word  46)
! Allowed = GROUP (original source group number      - header word   9)
! Allowed = LNSP  (source shotpoint and line number  - header words 29 and 26)
! Allowed = GRID  (source grid coordinates           - header words 33 and 34)
!
! SEQU coordinates:
!  Normally, if sequential ground position header words have been set in the
!  input dataset, the sequential ground position is a good choice for source
!  and receiver coordinates in 2D and 3D surveys.
!
! GROUP coordinates:
!  The original source group number should generally be used for 2D and 3D
!  surveys if a source occupies the same location more than once.
!
! LNSP coordinates:
!  Line number and shotpoint header words are generally good choices in 3D
!  surveys.
!
! GRID coordinates:
!  Grid coordinates are less desirable than line number and shotpoint for
!  the following reasons:
!   1. Grid coordinates may not be densely populated, making internal
!       calculations slower, disk use higher, and static and correlation
!       files much larger.
!   2. Statics are calculated only at regular grid intersections.  If
!       sources and receivers are not always located exactly on the
!       intersections then applying the statics files will probably produce
!       somewhat inaccurate results.
!
! Grid coordinates are not allowed for the SISC process.
!
! Sequential ground position coordinates should not be used for the SISC
! process for 3D surveys because there is no way to identify separate lines,
! which must be integrated independently.
!</Help>
!
!
!<Help KEYWORD="HDR_REC">
!<Tip> Header word(s) to use to identify receiver locations. </Tip>
! Default = SEQU
! Allowed = SEQU  (receiver sequential ground position - header word  47)
! Allowed = LNSP  (receiver shotpoint and line number  - header words 28 and 27)
! Allowed = GRID  (receiver grid coordinates           - header words 35 and 36)
!
! SEQU coordinates:
!  Normally, if sequential ground position header words have been set in the
!  input dataset, the sequential ground position is a good choice for source
!  and receiver coordinates in 2D and 3D surveys.
!
! LNSP coordinates:
!  Line number and shotpoint header words are generally good choices in 3D
!  surveys.
!
! GRID coordinates:
!  Grid coordinates are less desirable than line number and shotpoint for
!  the following reasons:
!   1. Grid coordinates may not be densely populated, making internal
!       calculations slower, disk use higher, and static and correlation
!       files much larger.
!   2. Statics are calculated only at regular grid intersections.  If
!       sources and receivers are not always located exactly on the
!       intersections then applying the statics files will probably produce
!       somewhat inaccurate results.
!
! Grid coordinates are not allowed for the SISC process.
!
! Sequential ground position coordinates should not be used for the SISC
! process for 3D surveys because there is no way to identify separate lines,
! which must be integrated independently.
!</Help>
!
!
!<Help KEYWORD="HDR_SX">
!<Tip> First header word to use for designating source locations. </Tip>
! Default = 46
! Allowed = 46  (source sequential ground position - header word  46)
! Allowed = 9   (original source group number      - header word   9)
! Allowed = 29  (source shotpoint and line number  - header words 29 and 26)
! Allowed = 33  (source grid coordinates           - header words 33 and 34)
!</Help>
!
!
!<Help KEYWORD="HDR_SY">
!<Tip> Second header word to use for designating source locations. </Tip>
! Default = 0
! Allowed = 0   (source sequential ground position - header word  46)
! Allowed = 0   (original source group number      - header word   9)
! Allowed = 26  (source shotpoint and line number  - header words 29 and 26)
! Allowed = 34  (source grid coordinates           - header words 33 and 34)
!</Help>
!
!
!<Help KEYWORD="HDR_RX">
!<Tip> First header word to use for designating receiver locations. </Tip>
! Default = 47
! Allowed = 47  (receiver sequential ground position - header word  47)
! Allowed = 28  (receiver shotpoint and line number  - header words 28 and 27)
! Allowed = 35  (receiver grid coordinates           - header words 35 and 36)
!</Help>
!
!
!<Help KEYWORD="HDR_RY">
!<Tip> Second header word to use for designating receiver locations. </Tip>
! Default = 0
! Allowed = 0   (receiver sequential ground position - header word  47)
! Allowed = 27  (receiver shotpoint and line number  - header words 28 and 27)
! Allowed = 36  (receiver grid coordinates           - header words 35 and 36)
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! If HDR_FLAG = 0, then all traces are used for the statics solution.
! Otherwise, only traces with a flag set in header word HDR_FLAG are used.
!</Help>
!
!
!<Help KEYWORD="DEAD_END">
!<Tip> Whether to stop before outputting any traces. </Tip>
! Default = YES
! Allowed = YES/NO
!
! If this parameter is YES, this will be a dead-end process which does not
! output any traces, but the job uses fewer resources and runs faster.
!
! If this parameter is NO, all input traces will have the newly-derived
! static shift applied and will be output, in the same order they were
! received.
!
! Static files and correlation files will always be written, regardless of the
! value of DEAD_END.
!
! Stopping before outputting any traces reduces the required temporary disk
! space by a factor of at least 5, and more commonly by about 10 on average.
! The disk savings are more significant when trace correlation windows are
! small.
!</Help>
!
!
!<Help KEYWORD="LEN_CORR">
!<Tip> Length of each side of the correlation function (milliseconds). </Tip>
! Default = 100.0
! Allowed = real >= MAX_STATIC.
!
! The plotted correlations will be twice this length (since both sides are
! plotted.)
!
! This value, plus the number of iterations, determines the length of the
! correlation traces output to byte files.
!</Help>
!
!
!<Help KEYWORD="NO_DEAD">
!<Tip> Whether to omit dead traces from the output correlation files. </Tip>
! Default = NO
! Allowed = YES/NO
!
! Setting NO_DEAD = YES may be desirable if the correlation files are very
! large and consist mostly of dead traces.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------




!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module stathelper_module
      use getlun_module
      use named_constants_module
      use latwin_module
      use temptfile_module
      use statutil_module
      use statshift_module
      use statfile_module
      use increment_module
      use string_module
      use pathcheck_module
      use pathchoose_module
      use mth_module
      use mem_module
      use pc_module
      use sizeof_module
      use statcc_module
      use addext_module
      use sortkeys_module
      implicit none
      public
      private :: stathelper_private_free
      private :: stathelper_private_trace_input
      private :: stathelper_private_pre_solve
      private :: stathelper_private_post_solve
      private :: stathelper_remember_open 
      private :: stathelper_remember_close
      private :: stathelper_remember_rewind
      private :: stathelper_remember_write
      private :: stathelper_remember_read 


      character(len=100),public,save :: STATHELPER_IDENT = &
       '$Id: stathelper.f90,v 1.12 2007/04/25 15:46:23 Stoeckley beta sps $'


      type,private :: statfile_container
        private
        type(statfile_struct),pointer :: statfile
        integer                       :: ikey,ngp
      end type statfile_container


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: stathelper_struct              

        private
        integer                        :: hdr_flag           ! process params
        logical                        :: dead_end           ! process params
        logical                        :: no_dead            ! process params
        character(len=FILENAME_LENGTH) :: path_src,path_rec  ! process params
        character(len=8)               :: hdr_src,hdr_rec    ! process params
        character(len=8)               :: corr_files         ! process params
        real                           :: len_corr           ! process params
        real                           :: dt                 ! globals

        character(len=40) :: procname                   ! supplied parameter 
        integer           :: num_iter                   ! supplied parameter 
        real              :: max_static,converge        ! supplied parameter 
        integer           :: ra_src,ra_rec              ! supplied parameter
        integer           :: sx_tot,sy_tot,off_tot      ! obtained from traces
        integer           :: rx_tot,ry_tot              ! obtained from traces
        integer           :: cx_tot,cy_tot              ! obtained from traces
        real              :: sx_init,sx_inc,sx_last     ! obtained from traces
        real              :: sy_init,sy_inc,sy_last     ! obtained from traces
        real              :: rx_init,rx_inc,rx_last     ! obtained from traces
        real              :: ry_init,ry_inc,ry_last     ! obtained from traces
        real              :: cx_init,cx_inc,cx_last     ! obtained from traces
        real              :: cy_init,cy_inc,cy_last     ! obtained from traces
        real              :: off_init,off_inc,off_last  ! obtained from traces
        integer           :: icount,idropf,idropd       ! obtained from traces
        integer           :: ntraces                    ! obtained from traces

        integer                        :: hdr_sx,hdr_sy
        integer                        :: hdr_rx,hdr_ry

        character(len=FILENAME_LENGTH) :: path_inc_src
        character(len=FILENAME_LENGTH) :: path_inc_rec
        character(len=FILENAME_LENGTH) :: path_byt_src
        character(len=FILENAME_LENGTH) :: path_byt_rec

        character(len=40) :: errmsg,ext
        integer           :: nwin,itwin,ibwin,npick,ncorr,nfiles
        integer           :: ncards,ipn,lun,remember,iter,ifile
        integer           :: isfile,irfile
        integer           :: ikey_scorr,ikey_rcorr
        logical           :: converged,need_call_post_solve

        character(len=PC_DATACARD_LENGTH),pointer :: cards(:)

        type(latwin_struct)      ,pointer :: latwin  
        type(sortkeys_struct)    ,pointer :: sortkeys
        type(temptfile_struct)   ,pointer :: tracefile,stackfile,basefile
        type(statfile_container)          :: file(2)
        type(statshift_struct)   ,pointer :: statshift
        type(pathchoose_struct)  ,pointer :: dialog_src
        type(pathchoose_struct)  ,pointer :: dialog_rec
        type(increment_struct)            :: increment_xsrc,increment_ysrc
        type(increment_struct)            :: increment_xrec,increment_yrec
        type(increment_struct)            :: increment_xcmp,increment_ycmp
        type(increment_struct)            :: increment_offset

      end type stathelper_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      real   ,parameter,private :: DELTA  = 1.0
      integer,parameter,private :: NUMINC = 12
      integer,parameter,private :: NHEAD  = HDR_NOMINAL_SIZE

      integer,parameter,private :: IKEY_SRC  = 1
      integer,parameter,private :: IKEY_REC  = 2
      integer,parameter,private :: IKEY_CMP  = 3
      integer,parameter,private :: IKEY_SOFF = 4
      integer,parameter,private :: IKEY_AOFF = 5

      integer,parameter,private ::    HDR_SRC_NOPT = 4
      integer,parameter,private ::    HDR_REC_NOPT = 3
      integer,parameter,private :: CORR_FILES_NOPT = 2

      character(len=8),save,private ::    hdr_src_options (   HDR_SRC_NOPT)
      character(len=8),save,private ::    hdr_rec_options (   HDR_REC_NOPT)
      character(len=8),save,private :: corr_files_options (CORR_FILES_NOPT)

      data               hdr_src_options /'GROUP','SEQU','LNSP','GRID'/
      data               hdr_rec_options /        'SEQU','LNSP','GRID'/
      data            corr_files_options /        'TRC8','NONE'/


      contains


!!------------------------ stathelper private free -----------------------!!
!!------------------------ stathelper private free -----------------------!!
!!------------------------ stathelper private free -----------------------!!


      subroutine stathelper_private_free (obj)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                 ! arguments

      call sortkeys_delete           (obj%sortkeys)
      call temptfile_close           (obj%tracefile)
      call temptfile_close           (obj%stackfile)
      call temptfile_close           (obj%basefile)
      call statfile_delete           (obj%file(1)%statfile)
      call statfile_delete           (obj%file(2)%statfile)
      call statshift_close           (obj%statshift)
      call stathelper_remember_close (obj)

      call mem_free (obj%cards)
      return
      end subroutine stathelper_private_free


!!------------------------ stathelper define sort ---------------------------!!
!!------------------------ stathelper define sort ---------------------------!!
!!------------------------ stathelper define sort ---------------------------!!


      subroutine stathelper_sort_to_cmps (obj)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                 ! arguments

      call sortkeys_select_sort (obj%sortkeys,1)
      return
      end subroutine stathelper_sort_to_cmps



      subroutine stathelper_sort_to_file (obj,ifile)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                 ! arguments
      integer                ,intent(in)    :: ifile               ! arguments

      call sortkeys_select_sort (obj%sortkeys,ifile+1)
      return
      end subroutine stathelper_sort_to_file


!!-------------------------- get values ----------------------------------!!
!!-------------------------- get values ----------------------------------!!
!!-------------------------- get values ----------------------------------!!


      function stathelper_get_nfiles (obj) result (nfiles)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: nfiles         ! result
      nfiles = obj%nfiles
      return
      end function stathelper_get_nfiles



      function stathelper_get_nwin (obj) result (nwin)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: nwin           ! result
      nwin = obj%nwin
      return
      end function stathelper_get_nwin



      function stathelper_get_itwin (obj) result (itwin)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: itwin          ! result
      itwin = obj%itwin
      return
      end function stathelper_get_itwin



      function stathelper_get_ibwin (obj) result (ibwin)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: ibwin          ! result
      ibwin = obj%ibwin
      return
      end function stathelper_get_ibwin



      function stathelper_get_ncorr (obj) result (ncorr)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: ncorr          ! result
      ncorr = obj%ncorr
      return
      end function stathelper_get_ncorr


      function stathelper_file_is_source (obj,ifile) result (is_source)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: ifile          ! result
      logical                            :: is_source      ! result
      is_source = (ifile == obj%isfile .and. ifile > 0)
      return
      end function stathelper_file_is_source


      function stathelper_file_is_receiver (obj,ifile) result (is_receiver)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: ifile          ! result
      logical                            :: is_receiver    ! result
      is_receiver = (ifile == obj%irfile .and. ifile > 0)
      return
      end function stathelper_file_is_receiver


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function stathelper_get_ntraces (obj) result (ntraces)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: ntraces        ! result
      ntraces = obj%ntraces
      return
      end function stathelper_get_ntraces



      function stathelper_get_ncmp (obj) result (ncmp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: ncmp           ! result
      ncmp = obj%cx_tot * obj%cy_tot
      return
      end function stathelper_get_ncmp



      function stathelper_get_nxcmp (obj) result (nxcmp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: nxcmp          ! result
      nxcmp = obj%cx_tot
      return
      end function stathelper_get_nxcmp



      function stathelper_get_nycmp (obj) result (nycmp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: nycmp          ! result
      nycmp = obj%cy_tot
      return
      end function stathelper_get_nycmp



      function stathelper_get_noff (obj) result (noff)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                            :: noff           ! result
      noff = obj%off_tot
      return
      end function stathelper_get_noff



      function stathelper_get_ngp (obj,ifile) result (ngp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: ifile          ! arguments
      integer                            :: ngp            ! result
      ngp = obj%file(ifile)%ngp
      return
      end function stathelper_get_ngp


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function stathelper_get_irecord (obj,itrace) result (irecord)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: itrace         ! arguments
      integer                            :: irecord        ! result

      irecord = sortkeys_get_record (obj%sortkeys,itrace)
      return
      end function stathelper_get_irecord



      function stathelper_get_igp (obj,itrace) result (igp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: itrace         ! arguments
      integer                            :: igp            ! result

      igp = sortkeys_get_prioritized_key (obj%sortkeys,itrace,1)
      return
      end function stathelper_get_igp



      function stathelper_get_icorr (obj,itrace) result (icorr)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: itrace         ! arguments
      integer                            :: icorr          ! result

      icorr = sortkeys_get_prioritized_key (obj%sortkeys,itrace,2)
      return
      end function stathelper_get_icorr



      function stathelper_get_this_igp (obj,ifile,itrace) result (igp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: ifile,itrace   ! arguments
      integer                            :: igp            ! result
      logical                            :: is_source      ! local
      logical                            :: is_receiver    ! local

      is_source   = stathelper_file_is_source   (obj,ifile)
      is_receiver = stathelper_file_is_receiver (obj,ifile)
      if (is_source) then
           igp = sortkeys_get_original_key (obj%sortkeys,itrace,IKEY_SRC)
      else if (is_receiver) then
           igp = sortkeys_get_original_key (obj%sortkeys,itrace,IKEY_REC)
      else
           igp = 1
      end if
      return
      end function stathelper_get_this_igp



      function stathelper_get_icmp (obj,itrace) result (icmp)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: itrace         ! arguments
      integer                            :: icmp           ! result

      icmp = sortkeys_get_original_key (obj%sortkeys,itrace,IKEY_CMP)
      return
      end function stathelper_get_icmp



      function stathelper_get_isoff (obj,itrace) result (isoff)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: itrace         ! arguments
      integer                            :: isoff          ! result

      isoff = sortkeys_get_original_key (obj%sortkeys,itrace,IKEY_SOFF)
      return
      end function stathelper_get_isoff



      function stathelper_get_iaoff (obj,itrace) result (iaoff)
      implicit none
      type(stathelper_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: itrace         ! arguments
      integer                            :: iaoff          ! result

      iaoff = sortkeys_get_original_key (obj%sortkeys,itrace,IKEY_AOFF)
      return
      end function stathelper_get_iaoff



      function stathelper_get_ixcmp (obj,itrace) result (ixcmp)
      implicit none
      type(stathelper_struct),intent(in) :: obj                 ! arguments
      integer                ,intent(in) :: itrace              ! arguments
      integer                            :: ixcmp               ! result
      integer                            :: iycmp,icmp,nxcmp    ! local

      icmp  = stathelper_get_icmp  (obj,itrace)
      nxcmp = stathelper_get_nxcmp (obj)
      call mth_split_index (icmp,nxcmp,  ixcmp,iycmp)
      return
      end function stathelper_get_ixcmp



      function stathelper_get_iycmp (obj,itrace) result (iycmp)
      implicit none
      type(stathelper_struct),intent(in) :: obj                 ! arguments
      integer                ,intent(in) :: itrace              ! arguments
      integer                            :: iycmp               ! result
      integer                            :: ixcmp,icmp,nxcmp    ! local

      icmp  = stathelper_get_icmp  (obj,itrace)
      nxcmp = stathelper_get_nxcmp (obj)
      call mth_split_index (icmp,nxcmp,  ixcmp,iycmp)
      return
      end function stathelper_get_iycmp


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine stathelper_create (obj,procname)
      implicit none
      type(stathelper_struct),pointer :: obj            ! arguments
      character(len=*),intent(in)     :: procname       ! arguments

      allocate (obj)

      nullify (obj%cards)
      nullify (obj%sortkeys)
      nullify (obj%tracefile)
      nullify (obj%stackfile)
      nullify (obj%basefile)
      nullify (obj%file(1)%statfile)
      nullify (obj%file(2)%statfile)
      nullify (obj%statshift)
      nullify (obj%latwin) ! jpa
      nullify (obj%dialog_src) ! jpa
      nullify (obj%dialog_rec) ! jpa

      obj%remember = 0
      obj%procname = procname
      obj%errmsg   = trim(procname)//': FATAL ERROR'
      obj%ext      = '.'//procname

      call string_to_upper (obj%procname)
      call string_to_upper (obj%errmsg)
      call string_to_lower (obj%ext)

      call stathelper_private_free (obj)
      call latwin_create           (obj%latwin, no_mute = .true.)
      call pathchoose_create       (obj%dialog_src, 'path_src', obj%ext)
      call pathchoose_create       (obj%dialog_rec, 'path_rec', obj%ext)
      return
      end subroutine stathelper_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine stathelper_delete (obj)
      implicit none
      type(stathelper_struct),pointer :: obj       ! arguments

      call stathelper_private_free (obj)
      call latwin_delete           (obj%latwin)
      call pathchoose_delete       (obj%dialog_src)
      call pathchoose_delete       (obj%dialog_rec)

      deallocate(obj)
      return
      end subroutine stathelper_delete


!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!


      subroutine stathelper_initialize (obj)
      implicit none
      type(stathelper_struct),intent(inout) :: obj              ! arguments

      obj%hdr_flag      = 0
      obj%dead_end      = .true.
      obj%no_dead       = .false.
      obj%path_src      = PATHCHECK_EMPTY
      obj%path_rec      = PATHCHECK_EMPTY
      obj%hdr_src       = 'SEQU'
      obj%hdr_rec       = 'SEQU'
      obj%corr_files    = 'TRC8'
      obj%len_corr      = 100.0

      call stathelper_private_free (obj)
      call latwin_initialize       (obj%latwin)
      return
      end subroutine stathelper_initialize


!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!


      subroutine stathelper_update (obj,num_iter,max_static,converge,        &
                                    ra_src,ra_rec,hdr_corr_src,hdr_corr_rec, &
                                    error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj               ! arguments
      integer                ,intent(in)    :: num_iter          ! arguments
      real                   ,intent(in)    :: max_static        ! arguments
      real                   ,intent(in)    :: converge          ! arguments
      integer                ,intent(in)    :: ra_src,ra_rec     ! arguments
      character(len=*)       ,intent(in)    :: hdr_corr_src      ! arguments
      character(len=*)       ,intent(in)    :: hdr_corr_rec      ! arguments
      logical                ,intent(out)   :: error             ! arguments
      character(len=40)                     :: errmsg            ! local
      integer                               :: err,nwih,ndpt     ! local
      real                                  :: dtms              ! local
      integer                               :: hdr_src_nopt2     ! local
      integer                               :: hdr_rec_nopt2     ! local
      integer                               :: corr_files_nopt2  ! local
      logical                               :: ssolve,rsolve     ! local

!----------get started.

      obj%num_iter   = num_iter
      obj%max_static = max_static
      obj%converge   = converge
      obj%ra_src     = ra_src
      obj%ra_rec     = ra_rec
      obj%ipn        = pc_get_ipn()
      obj%lun        = pc_get_lun()
      error          = .false.

!----------read parameters.

      if (pathchoose_update(obj%dialog_src, obj%path_src)) return
      if (pathchoose_update(obj%dialog_rec, obj%path_rec)) return

      call pc_get_global ('nwih'    , nwih)
      call pc_get_global ('ndpt'    , ndpt)
      call pc_get_global ('dt'      , obj%dt)

      call pc_get  ('hdr_flag'    ,obj%hdr_flag      )
      call pc_get  ('dead_end'    ,obj%dead_end      )
      call pc_get  ('no_dead'     ,obj%no_dead       )
      call pc_get  ('path_src'    ,obj%path_src      )
      call pc_get  ('path_rec'    ,obj%path_rec      )
      call pc_get  ('hdr_src'     ,obj%hdr_src       )
      call pc_get  ('hdr_rec'     ,obj%hdr_rec       )
      call pc_get  ('corr_files'  ,obj%corr_files    )
      call pc_get  ('len_corr'    ,obj%len_corr      )

!----------miscellaneous verifications.

      if (obj%hdr_flag < 0 .or. obj%hdr_flag > nwih) then
           call pc_error ('STATHELPER: illegal value of HDR_FLAG')
           error = .true.
      end if

      if (obj%corr_files /= 'NONE') obj%corr_files = 'TRC8'

      dtms = 1000.0 * obj%dt

      obj%len_corr = max(obj%len_corr, obj%max_static, 2.0*dtms)

!----------process-specific adjustments.

      select case (obj%procname)
           case ('SISC')
                         hdr_src_nopt2    = hdr_src_nopt    - 1
                         hdr_rec_nopt2    = hdr_rec_nopt    - 1
                         corr_files_nopt2 = corr_files_nopt - 1
                         if (obj%hdr_src    == 'GRID') obj%hdr_src    = 'LNSP'
                         if (obj%hdr_rec    == 'GRID') obj%hdr_rec    = 'LNSP'
                         if (obj%corr_files == 'NONE') obj%corr_files = 'TRC8'
           case ('FISH')
                         hdr_src_nopt2    = hdr_src_nopt
                         hdr_rec_nopt2    = hdr_rec_nopt
                         corr_files_nopt2 = corr_files_nopt - 1
                         if (obj%corr_files == 'NONE') obj%corr_files = 'TRC8'
           case default
                         hdr_src_nopt2    = hdr_src_nopt
                         hdr_rec_nopt2    = hdr_rec_nopt
                         corr_files_nopt2 = corr_files_nopt
      end select

!----------verify and adjust source and receiver header word values.

      select case (obj%hdr_src)
           case ('GROUP')
                          obj%hdr_sx  = HDR_ORIGINAL_GROUP       ! 9
                          obj%hdr_sy  = 0
           case ('SEQU')
                          obj%hdr_sx  = HDR_SOURCE_GP            ! 46
                          obj%hdr_sy  = 0
           case ('LNSP')
                          obj%hdr_sx  = HDR_SOURCE_SHOTPOINT     ! 29
                          obj%hdr_sy  = HDR_SOURCE_LINE          ! 26
           case ('GRID')
                          obj%hdr_sx  = HDR_SOURCE_XGRID         ! 33
                          obj%hdr_sy  = HDR_SOURCE_YGRID         ! 34
           case default
                          obj%hdr_src = 'LNSP'
                          obj%hdr_sx  = HDR_SOURCE_SHOTPOINT     ! 29
                          obj%hdr_sy  = HDR_SOURCE_LINE          ! 26
      end select

      select case (obj%hdr_rec)
           case ('SEQU')
                          obj%hdr_rx  = HDR_RECEIVER_GP          ! 47
                          obj%hdr_ry  = 0
           case ('LNSP')
                          obj%hdr_rx  = HDR_RECEIVER_SHOTPOINT   ! 28
                          obj%hdr_ry  = HDR_RECEIVER_LINE        ! 27
           case ('GRID')
                          obj%hdr_rx  = HDR_RECEIVER_XGRID       ! 35
                          obj%hdr_ry  = HDR_RECEIVER_YGRID       ! 36
           case default
                          obj%hdr_rec = 'LNSP'
                          obj%hdr_rx  = HDR_RECEIVER_SHOTPOINT   ! 28
                          obj%hdr_ry  = HDR_RECEIVER_LINE        ! 27
      end select

!----------verify and adjust file names.

    call pathcheck ('path_src',obj%path_src,obj%ext,show=PATHCHECK_INFO_OUTPUT)
    call pathcheck ('path_rec',obj%path_rec,obj%ext,show=PATHCHECK_INFO_OUTPUT)

      ssolve = (obj%path_src /= PATHCHECK_EMPTY)
      rsolve = (obj%path_rec /= PATHCHECK_EMPTY)

      if (.not.ssolve .and. .not.rsolve) then
           call pc_error &
                   ('STATHELPER: PATH_SRC AND PATH_REC CANNOT BOTH BE MISSING')
           error = .true.
      else if (obj%path_src == obj%path_rec) then
           call pc_error &
                   ('STATHELPER: PATH_SRC AND PATH_REC CANNOT BE THE SAME')
           error = .true.
      end if

!----------verify correlation choices.

      if (obj%procname == 'SISC') then
           select case (hdr_corr_src)
                case ('REC' ) ; obj%ikey_scorr = IKEY_REC
                case ('CMP' ) ; obj%ikey_scorr = IKEY_CMP
                case ('SOFF') ; obj%ikey_scorr = IKEY_SOFF
                case ('AOFF') ; obj%ikey_scorr = IKEY_AOFF
                case default  ; obj%ikey_scorr = -999
           end select
           select case (hdr_corr_rec)
                case ('SRC' ) ; obj%ikey_rcorr = IKEY_SRC
                case ('CMP' ) ; obj%ikey_rcorr = IKEY_CMP
                case ('SOFF') ; obj%ikey_rcorr = IKEY_SOFF
                case ('AOFF') ; obj%ikey_rcorr = IKEY_AOFF
                case default  ; obj%ikey_rcorr = -999
           end select
      else
           select case (hdr_corr_src)
                case ('CMP' ) ; obj%ikey_scorr = IKEY_CMP
                case default  ; obj%ikey_scorr = -999
           end select
           select case (hdr_corr_rec)
                case ('CMP' ) ; obj%ikey_rcorr = IKEY_CMP
                case default  ; obj%ikey_rcorr = -999
           end select
      end if

      if (obj%ikey_scorr == -999) then
           call pc_error &
                ('STATHELPER: illegal value',hdr_corr_src,'for HDR_CORR_SRC')
           error = .true.
      end if

      if (obj%ikey_rcorr == -999) then
           call pc_error &
                ('STATHELPER: illegal value',hdr_corr_rec,'for HDR_CORR_REC')
           error = .true.
      end if

!----------write parameters.

 call pc_put_options_field ('hdr_src'   ,    hdr_src_options,    hdr_src_nopt2)
 call pc_put_options_field ('hdr_rec'   ,    hdr_rec_options,    hdr_rec_nopt2)
 call pc_put_options_field ('corr_files', corr_files_options, corr_files_nopt2)

      call pc_put  ('hdr_flag'    ,obj%hdr_flag      )
      call pc_put  ('dead_end'    ,obj%dead_end      )
      call pc_put  ('no_dead'     ,obj%no_dead       )
      call pc_put  ('path_src'    ,obj%path_src      )
      call pc_put  ('path_rec'    ,obj%path_rec      )
      call pc_put  ('hdr_src'     ,obj%hdr_src       )
      call pc_put  ('hdr_rec'     ,obj%hdr_rec       )
      call pc_put  ('corr_files'  ,obj%corr_files    )
      call pc_put  ('len_corr'    ,obj%len_corr      )

      call pc_put_gui_only  ('hdr_sx', obj%hdr_sx  )
      call pc_put_gui_only  ('hdr_sy', obj%hdr_sy  )
      call pc_put_gui_only  ('hdr_rx', obj%hdr_rx  )
      call pc_put_gui_only  ('hdr_ry', obj%hdr_ry  )

      call pc_put_global  ('numtr'       ,    1    )
      call pc_put_global  ('gathered'    , .false. )

      call pc_put_control ('ntapes'      ,    0    )
      call pc_put_control ('need_request', .true.  )
      call pc_put_control ('need_label'  , .true.  )
      call pc_put_control ('twosets'     , .false. )
      call pc_put_control ('nscratch'    ,    0    )    ! cannot be known.
      call pc_put_control ('nstore'      ,    0    )    ! cannot be known.
      call pc_put_control ('iftd'        , .true.  )
      call pc_put_control ('ndisk'       ,    0    )    ! cannot be known.
      call pc_put_control ('setup_only'  , .false. )

      call pc_put_sensitive_field_flag ('len_corr', obj%corr_files /= 'NONE')
      call pc_put_sensitive_field_flag ('no_dead' , obj%corr_files /= 'NONE')

!----------get ready to return.

      call latwin_update (obj%latwin, obj%nwin, obj%itwin, obj%ibwin)

      obj%npick  = nint(obj%max_static/dtms)*2 + 1     ! always odd.
      obj%ncorr  = nint(obj%len_corr  /dtms)*2 + 1     ! always odd.
      obj%nfiles = 0
      obj%isfile = 0
      obj%irfile = 0

      if (ssolve) then
           obj%nfiles = obj%nfiles + 1
           obj%isfile = obj%nfiles
      end if

      if (rsolve) then
           obj%nfiles = obj%nfiles + 1
           obj%irfile = obj%nfiles
      end if

      if (obj%corr_files == 'NONE') then
         obj%ncorr = obj%npick
         call pc_print ('STATHELPER: CORRELATION FUNCTIONS WILL NOT BE OUTPUT')
      end if

      call pc_print ('STATHELPER: MAXIMUM STATIC SHIFT IS', &
                                               obj%max_static,'MILLISECONDS')
      call pc_print ('STATHELPER: CORRELATION FUNCTIONS CONTAIN', &
                                               obj%ncorr,'VALUES')
      call pc_print ('STATHELPER: CORRELATION PICK RANGE CONTAINS', &
                                               obj%npick,'VALUES')
      call pc_print ('STATHELPER: TRACE WINDOWS WILL CONTAIN UP TO', &
                                               obj%nwin,'VALUES')
      call pc_print (' ')

      call stathelper_private_free (obj)

      if (pc_do_not_process_traces()) return

      !!!!! now we know error == .false.

!----------get correlation file and increment file names.

      obj%path_byt_src = obj%path_src
      obj%path_byt_rec = obj%path_rec

      select case (obj%corr_files)
           case ('TRC8')
              if (ssolve) call addext (obj%path_byt_src, 'trc8', .true.)
              if (rsolve) call addext (obj%path_byt_rec, 'trc8', .true.)
           case default
              obj%path_byt_src = PATHCHECK_EMPTY
              obj%path_byt_rec = PATHCHECK_EMPTY
      end select

      if (obj%procname == 'SISC') then
           if (ssolve) then
                obj%path_inc_src = obj%path_src
                call addext (obj%path_inc_src, 'inc', .true.)
           end if
           if (rsolve) then
                obj%path_inc_rec = obj%path_rec
                call addext (obj%path_inc_rec, 'inc', .true.)
           end if
      else
           obj%path_inc_src = PATHCHECK_EMPTY
           obj%path_inc_rec = PATHCHECK_EMPTY
      end if

!----------initialize counters.

      obj%icount  = 0
      obj%idropf  = 0
      obj%idropd  = 0
      obj%ntraces = 0
      obj%iter    = 0
      obj%ifile   = 0
      obj%need_call_post_solve = .false.

!----------get history cards for statics files.

      if (obj%procname == 'SISC') then
           call pc_alloc_process_cards (obj%cards,obj%ncards)
      else
           obj%ncards = pc_num_process_cards() + 1
           call mem_alloc (obj%cards,obj%ncards)
           call pc_get_process_cards (obj%cards,obj%ncards,errmsg)
           obj%ncards = pc_num_process_cards() + 1
           obj%cards(obj%ncards) = ' '
      end if

!----------initialize increment objects.

      call increment_init (obj%increment_xsrc  , DELTA, NUMINC)
      call increment_init (obj%increment_ysrc  , DELTA, NUMINC)
      call increment_init (obj%increment_xrec  , DELTA, NUMINC)
      call increment_init (obj%increment_yrec  , DELTA, NUMINC)
      call increment_init (obj%increment_xcmp  , DELTA, NUMINC)
      call increment_init (obj%increment_ycmp  , DELTA, NUMINC)
      call increment_init (obj%increment_offset, DELTA, NUMINC)

!----------create file for storing initial trace information.

      call stathelper_remember_open (obj,error)
      if (error) return

!----------create file for storing individual trace windows.

      call temptfile_open &
                  (obj%tracefile,trim(obj%procname)//'_trace_windows', &
                   0,obj%nwin,obj%lun,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary trace window file open error')
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if

!----------create file for storing traces for later shifting and output.

      if (.not.obj%dead_end) then
           call statshift_open (obj%statshift,                    &
                                trim(obj%procname)//'_traces',    &
                                nwih, ndpt, obj%dt,               &
                                obj%path_src, obj%path_rec,       &
                                obj%lun, err)
           if (err /= STATSHIFT_OK) then
                call pc_error ('STATHELPER: temporary trace file open error')
                call pc_error (obj%errmsg)
                error = .true.
                return
           end if
      end if
      call pc_print (' ')
      !!!! now we know error is still false.
      return
      end subroutine stathelper_update


!!-------------------- stathelper read shifted window -----------------------!!
!!-------------------- stathelper read shifted window -----------------------!!
!!-------------------- stathelper read shifted window -----------------------!!


      subroutine stathelper_read_shifted_window (obj,itrace,win,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: itrace             ! arguments
      real                   ,intent(out)   :: win(:)             ! arguments
      logical                ,intent(out)   :: error              ! arguments
      double precision                      :: dummy(1)           ! local
      real                                  :: scr(obj%nwin)      ! local
      real                                  :: shft,shft2         ! local
      integer                               :: ifile,igp,irecord  ! local
      integer                               :: err                ! local

      irecord = sortkeys_get_record (obj%sortkeys,itrace)
      call temptfile_read8 (obj%tracefile,irecord,dummy,scr,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary trace window file read error')
           call pc_error ('STATHELPER: on trace number',itrace)
           call pc_error ('STATHELPER: at iteration',obj%iter)
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if
      shft = 0.0
      do ifile = 1,obj%nfiles
           igp   = sortkeys_get_original_key &
                                   (obj%sortkeys,itrace,obj%file(ifile)%ikey)
           shft2 = statfile_get_static (obj%file(ifile)%statfile,igp)
           if (shft2 /= FNIL) shft = shft + shft2
      end do
      call statcc (shft,obj%nwin,scr,win)
      error = .false.
      return
      end subroutine stathelper_read_shifted_window


!!---------------- stathelper read and write stacked window -----------------!!
!!---------------- stathelper read and write stacked window -----------------!!
!!---------------- stathelper read and write stacked window -----------------!!


      subroutine stathelper_read_stacked_window (obj,icmp,ifold,win,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: icmp               ! arguments
      integer                ,intent(out)   :: ifold              ! arguments
      real                   ,intent(out)   :: win(:)             ! arguments
      logical                ,intent(out)   :: error              ! arguments
      double precision                      :: fold(1)            ! local
      integer                               :: err                ! local

      call temptfile_read8 (obj%stackfile,icmp,fold,win,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary stack window file read error')
           call pc_error ('STATHELPER: on CMP number',icmp)
           call pc_error ('STATHELPER: at iteration',obj%iter)
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if
      ifold = nint(fold(1))
      error = .false.
      return
      end subroutine stathelper_read_stacked_window


      subroutine stathelper_write_stacked_window (obj,icmp,ifold,win,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: icmp               ! arguments
      integer                ,intent(in)    :: ifold              ! arguments
      real                   ,intent(in)    :: win(:)             ! arguments
      logical                ,intent(out)   :: error              ! arguments
      double precision                      :: fold(1)            ! local
      integer                               :: err                ! local

      fold(1) = ifold
      call temptfile_write8 (obj%stackfile,icmp,fold,win,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary stack window file write error')
           call pc_error ('STATHELPER: on CMP number',icmp)
           call pc_error ('STATHELPER: at iteration',obj%iter)
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if
      error = .false.
      return
      end subroutine stathelper_write_stacked_window


!!---------------- stathelper read and write base window -----------------!!
!!---------------- stathelper read and write base window -----------------!!
!!---------------- stathelper read and write base window -----------------!!


      subroutine stathelper_read_base_window (obj,icmp,ifold,win,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: icmp               ! arguments
      integer                ,intent(out)   :: ifold              ! arguments
      real                   ,intent(out)   :: win(:)             ! arguments
      logical                ,intent(out)   :: error              ! arguments
      double precision                      :: fold(1)            ! local
      integer                               :: err                ! local

      call temptfile_read8 (obj%basefile,icmp,fold,win,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary base window file read error')
           call pc_error ('STATHELPER: on CMP number',icmp)
           call pc_error ('STATHELPER: at iteration',obj%iter)
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if
      ifold = nint(fold(1))
      error = .false.
      return
      end subroutine stathelper_read_base_window


      subroutine stathelper_write_base_window (obj,icmp,ifold,win,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: icmp               ! arguments
      integer                ,intent(in)    :: ifold              ! arguments
      real                   ,intent(in)    :: win(:)             ! arguments
      logical                ,intent(out)   :: error              ! arguments
      double precision                      :: fold(1)            ! local
      integer                               :: err                ! local

      fold(1) = ifold
      call temptfile_write8 (obj%basefile,icmp,fold,win,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary base window file write error')
           call pc_error ('STATHELPER: on CMP number',icmp)
           call pc_error ('STATHELPER: at iteration',obj%iter)
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if
      error = .false.
      return
      end subroutine stathelper_write_base_window


!!---------------------- stathelper input traces ----------------------------!!
!!---------------------- stathelper input traces ----------------------------!!
!!---------------------- stathelper input traces ----------------------------!!

! The input value of NTR must be >= 1 or NO_MORE_TRACES.
! The output value of NTR will be NEED_TRACES or NO_MORE_TRACES or FATAL_ERROR.


      subroutine stathelper_input_traces (obj,ntr,hd,tr)
      implicit none
      type(stathelper_struct),intent(inout) :: obj             ! arguments
      integer                ,intent(inout) :: ntr             ! arguments
      double precision       ,intent(in)    :: hd(:,:)         ! arguments
      real                   ,intent(in)    :: tr(:,:)         ! arguments
      integer                               :: i,err           ! local
      logical                               :: error           ! local

      if (ntr == NEED_TRACES .or. ntr == FATAL_ERROR) then
           ntr = FATAL_ERROR
           call pc_error ('STATHELPER: ILLEGAL VALUE OF NTR')
           call pc_error ('STATHELPER: NTR CANNOT BE BE NEED_TRACES')
           call pc_error ('STATHELPER: PROGRAMMING ERROR IN CALLING PROCESS')
           call pc_error ('STATHELPER: ERROR DURING TRACE INPUT')
           call pc_error (obj%errmsg)
           return
      end if

      if (ntr == NO_MORE_TRACES) then
           call stathelper_private_pre_solve (obj,error)
           obj%need_call_post_solve = .true.
           if (error) then
                ntr = FATAL_ERROR
                call pc_error ('STATHELPER: FATAL PRE-SOLUTION ERROR')
                call pc_error (obj%errmsg)
           else if (.not.obj%dead_end) then
                call statshift_rewind (obj%statshift)
           end if
           return
      end if

      do i = 1,ntr
           call stathelper_private_trace_input (obj,hd(1:,i),tr(1:,i),error)
           if (error) then
                ntr = FATAL_ERROR
                call pc_error ('STATHELPER: ERROR DURING TRACE INPUT')
                call pc_error (obj%errmsg)
                return
           end if
           if (.not.obj%dead_end) then
                call statshift_write (obj%statshift,hd(1:,i),tr(1:,i),err)
                if (err /= STATSHIFT_OK) then
                  ntr = FATAL_ERROR
                  call pc_error ('STATHELPER: temporary trace file write error')
                  call pc_error ('STATHELPER: ERROR DURING TRACE INPUT')
                  call pc_error (obj%errmsg)
                  return
                end if
           end if
      end do
      ntr = NEED_TRACES
      return
      end subroutine stathelper_input_traces


!!---------------------- stathelper output traces --------------------------!!
!!---------------------- stathelper output traces --------------------------!!
!!---------------------- stathelper output traces --------------------------!!

! The input value of NTR must be NEED_TRACES or FATAL_ERROR.
! The output value of NTR will be 1 or NO_MORE_TRACES or FATAL_ERROR.


      subroutine stathelper_output_traces (obj,ntr,hd,tr)
      implicit none
      type(stathelper_struct),intent(inout) :: obj             ! arguments
      integer                ,intent(inout) :: ntr             ! arguments
      double precision       ,intent(out)   :: hd(:,:)         ! arguments
      real                   ,intent(out)   :: tr(:,:)         ! arguments
      integer                               ::   err ! local
      logical                               :: error           ! local

      if (ntr /= NEED_TRACES .and. ntr /= FATAL_ERROR) then
           ntr = FATAL_ERROR
           call pc_error ('STATHELPER: ILLEGAL VALUE OF NTR')
           call pc_error ('STATHELPER: NTR MUST BE NEED_TRACES OR FATAL_ERROR')
           call pc_error ('STATHELPER: PROGRAMMING ERROR IN CALLING PROCESS')
           call pc_error ('STATHELPER: ERROR DURING SHIFTED TRACE OUTPUT')
           call pc_error (obj%errmsg)
           return
      end if

      if (obj%need_call_post_solve) then
            error = (ntr == FATAL_ERROR)
            call stathelper_private_post_solve (obj,error)
            obj%need_call_post_solve = .false.
      end if

      if (ntr == FATAL_ERROR) return

      if (obj%dead_end) then
           ntr = NO_MORE_TRACES
           return
      end if

      call statshift_read (obj%statshift,hd(1:,1),tr(1:,1),err)
      if (err == STATSHIFT_ERROR) then
           ntr = FATAL_ERROR
           call pc_error ('STATHELPER: temporary trace file read error')
           call pc_error ('STATHELPER: ERROR DURING SHIFTED TRACE OUTPUT')
           call statshift_close (obj%statshift)
           call pc_error (obj%errmsg)
           return
      end if
      if (err == STATSHIFT_EOF) then
           ntr = NO_MORE_TRACES
           call pc_print ('STATHELPER: FINISHED OUTPUTTING SHIFTED TRACES')
           call statshift_close (obj%statshift)
           call pc_print (trim(obj%procname)//': FINISHED')
           call pc_print (' ')
           return
      end if
      ntr = 1
      return
      end subroutine stathelper_output_traces


!!------------------ stathelper private remember i/o ------------------------!!
!!------------------ stathelper private remember i/o ------------------------!!
!!------------------ stathelper private remember i/o ------------------------!!


      subroutine stathelper_remember_open (obj,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj          ! arguments
      logical                ,intent(out)   :: error        ! args
      integer                               :: istat        ! local

      call getlun (obj%remember)

      if (obj%remember <= 0) then
           call pc_error ('STATHELPER: CANNOT GET LUN FOR REMEMBER FILE')
           call pc_error (obj%errmsg)
           obj%remember = 0
           error = .true.
           return
      end if

      open (obj%remember,status='scratch',form='unformatted', &
            access='sequential',iostat=istat)

      if (istat /= 0) then
           call pc_error ('STATHELPER: CANNOT OPEN REMEMBER FILE LUN', &
                                                    obj%remember)
           call pc_error (obj%errmsg)
           obj%remember = 0
           error = .true.
           return
      end if

      error = .false.
      return
      end subroutine stathelper_remember_open


!!--------------------------------------------------------------------------!!


      subroutine stathelper_remember_close (obj)
      implicit none
      type(stathelper_struct),intent(inout) :: obj          ! arguments

      if (obj%remember > 0) then
           close (obj%remember,status='delete')
           obj%remember = 0
      end if
      return
      end subroutine stathelper_remember_close


!!--------------------------------------------------------------------------!!


      subroutine stathelper_remember_rewind (obj)
      implicit none
      type(stathelper_struct),intent(inout) :: obj          ! arguments

      rewind obj%remember
      return
      end subroutine stathelper_remember_rewind


!!--------------------------------------------------------------------------!!


      subroutine stathelper_remember_write (obj,error,                &
                                            xcoord_src, ycoord_src,   &
                                            xcoord_rec, ycoord_rec,   &
                                            xcoord_cmp, ycoord_cmp,   &
                                            signed_offset)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                    ! args
      logical                ,intent(out)   :: error                  ! args
      real                   ,intent(in)    :: xcoord_src,ycoord_src  ! args
      real                   ,intent(in)    :: xcoord_rec,ycoord_rec  ! args
      real                   ,intent(in)    :: xcoord_cmp,ycoord_cmp  ! args
      real                   ,intent(in)    :: signed_offset          ! args
      integer                               :: istat                  ! local

      write (obj%remember,iostat=istat) xcoord_src, ycoord_src,   &
                                        xcoord_rec, ycoord_rec,   &
                                        xcoord_cmp, ycoord_cmp,   &
                                        signed_offset
      if (istat /= 0) then
           call pc_error ('STATHELPER: error writing to remember file')
           call pc_error (obj%errmsg)
           call stathelper_remember_close (obj)
           error = .true.
           return
      end if
      error = .false.
      return
      end subroutine stathelper_remember_write


!!--------------------------------------------------------------------------!!


      subroutine stathelper_remember_read  (obj,error,                &
                                            xcoord_src, ycoord_src,   &
                                            xcoord_rec, ycoord_rec,   &
                                            xcoord_cmp, ycoord_cmp,   &
                                            signed_offset)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                    ! args
      logical                ,intent(out)   :: error                  ! args
      real                   ,intent(out)   :: xcoord_src,ycoord_src  ! args
      real                   ,intent(out)   :: xcoord_rec,ycoord_rec  ! args
      real                   ,intent(out)   :: xcoord_cmp,ycoord_cmp  ! args
      real                   ,intent(out)   :: signed_offset          ! args
      integer                               :: istat                  ! local

      read (obj%remember,iostat=istat)  xcoord_src, ycoord_src,   &
                                        xcoord_rec, ycoord_rec,   &
                                        xcoord_cmp, ycoord_cmp,   &
                                        signed_offset
      if (istat /= 0) then
           call pc_error ('STATHELPER: error reading from remember file')
           call pc_error (obj%errmsg)
           call stathelper_remember_close (obj)
           error = .true.
           return
      end if
      error = .false.
      return
      end subroutine stathelper_remember_read 


!!------------------ stathelper private trace input -------------------------!!
!!------------------ stathelper private trace input -------------------------!!
!!------------------ stathelper private trace input -------------------------!!


      subroutine stathelper_private_trace_input (obj,hdi,tri,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj             ! arguments
      double precision,intent(in)    :: hdi(:)                 ! arguments
      real            ,intent(in)    :: tri(:)                 ! arguments
      logical         ,intent(out)   :: error                  ! arguments
      integer                        :: nlive,err              ! local
      double precision               :: dummy(1)               ! local
      real                           :: win(obj%nwin)          ! local
      real                           :: xcoord_src,ycoord_src  ! local
      real                           :: xcoord_rec,ycoord_rec  ! local
      real                           :: xcoord_cmp,ycoord_cmp  ! local
      real                           :: offset,signed_offset   ! local

!----------get trace window.

      obj%icount = obj%icount + 1

      if (obj%hdr_flag > 0) then
           if (hdi(obj%hdr_flag) /= 0.0) then
                obj%idropf = obj%idropf + 1
                error = .false.
                return
           end if
      end if

      call latwin_get_window (obj%latwin,hdi,tri,   win,nlive=nlive)

      if (nlive < obj%npick) then
            obj%idropd = obj%idropd + 1
            error = .false.
            return
      end if

!----------get header word values.

      xcoord_src = 0.0
      ycoord_src = 0.0
      xcoord_rec = 0.0
      ycoord_rec = 0.0
      xcoord_cmp = hdi(7)
      ycoord_cmp = hdi(8)
      offset     = hdi(6)

      if (obj%hdr_sx > 0) xcoord_src = hdi(obj%hdr_sx)
      if (obj%hdr_sy > 0) ycoord_src = hdi(obj%hdr_sy)
      if (obj%hdr_rx > 0) xcoord_rec = hdi(obj%hdr_rx)
      if (obj%hdr_ry > 0) ycoord_rec = hdi(obj%hdr_ry)

!----------get signed offset in grid units.

      signed_offset = &
             sqrt((hdi(HDR_RECEIVER_XGRID) - hdi(HDR_SOURCE_XGRID))**2 + &
                  (hdi(HDR_RECEIVER_YGRID) - hdi(HDR_SOURCE_YGRID))**2)

      if (hdi(HDR_RECEIVER_XGRID) < hdi(HDR_SOURCE_XGRID)) &
                    signed_offset = -signed_offset

!----------update limits.

      xcoord_src = DELTA * nint(xcoord_src / DELTA)
      ycoord_src = DELTA * nint(ycoord_src / DELTA)
      xcoord_rec = DELTA * nint(xcoord_rec / DELTA)
      ycoord_rec = DELTA * nint(ycoord_rec / DELTA)
      xcoord_cmp = DELTA * nint(xcoord_cmp / DELTA)
      ycoord_cmp = DELTA * nint(ycoord_cmp / DELTA)

  ! The above integerizing statements are needed to guarantee that the
  ! INTEGRATE_UPDATE and MTH_GET_INDICES routines are compatible when
  ! the header word values fall on the edge of a bin.

      call increment_update (obj%increment_xsrc  , xcoord_src)
      call increment_update (obj%increment_ysrc  , ycoord_src)
      call increment_update (obj%increment_xrec  , xcoord_rec)
      call increment_update (obj%increment_yrec  , ycoord_rec)
      call increment_update (obj%increment_xcmp  , xcoord_cmp)
      call increment_update (obj%increment_ycmp  , ycoord_cmp)
      call increment_update (obj%increment_offset, signed_offset)

!----------output trace headers to disk.

      obj%ntraces = obj%ntraces + 1

      call stathelper_remember_write (obj,error,                &
                                      xcoord_src, ycoord_src,   &
                                      xcoord_rec, ycoord_rec,   &
                                      xcoord_cmp, ycoord_cmp,   &
                                      signed_offset)
      if (error) return

!----------output trace window to disk.

      call temptfile_write8 (obj%tracefile,obj%ntraces,dummy,win,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary trace window file write error')
           call pc_error (obj%errmsg)
           error = .true.
           return
      end if

!----------finish up and return.

      error = .false.
      return
      end subroutine stathelper_private_trace_input


!!------------------- stathelper private pre solve ----------------------!!
!!------------------- stathelper private pre solve ----------------------!!
!!------------------- stathelper private pre solve ----------------------!!


      subroutine stathelper_private_pre_solve (obj,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                 ! arguments
      logical                ,intent(out)   :: error               ! arguments
      integer                        :: itrace      ,ifile ! local
      integer                        :: ix,iy,isrc,irec,icmp       ! local
      integer                        :: isoff,iaoff,nrun,err       ! local

      real                           :: xcoord_src,ycoord_src      ! local
      real                           :: xcoord_rec,ycoord_rec      ! local
      real                           :: xcoord_cmp,ycoord_cmp      ! local
      real                           :: signed_offset,dummy        ! local
      character(len=200)             :: line,line2                 ! local

!----------print header.

      line  = '++++++++++++++++++++'                           // &
              ' '//trim(obj%procname)//' PROCESSING SUMMARY '  // &
              '++++++++++++++++++++'
      line2 = '+++++++++++++++++++++++++++++++++++++++++++++'  // &
              '+++++++++++++++++++++++++++++++++++++++++++++'  // &
              '+++++++++++++++++++++++++++++++++++++++++++++'
      line2(len_trim(line)+1:) = ' '
      call pc_print (' ')
      call pc_print (line2)
      call pc_print (line)
      call pc_print (line2)
      call pc_print (' ')

!----------print initial information.

      call pc_print ('STATHELPER:',obj%icount,'input traces received')
      call pc_print ('STATHELPER:',obj%idropf,'flagged traces dropped')
      call pc_print ('STATHELPER:',obj%idropd,                         &
                         'traces dropped due to fewer than',obj%npick, &
                         'live values in correlation window')
      call pc_print ('STATHELPER:',obj%ntraces,                        &
                         'traces used for statics solution')
      if (obj%dead_end) then
           call pc_print ('STATHELPER: no traces will be output')
      else
           call pc_print ('STATHELPER:',obj%icount,                    &
                                      'shifted traces will be output')
      end if

!----------return if no traces.

      if (obj%ntraces == 0) then
           call pc_error ('STATHELPER: no traces to use for statics solution')
           error = .true.
           return
      end if

!----------get summary of input traces.

      call increment_result (obj%increment_xsrc, &
               dummy,dummy,obj%sx_inc,obj%sx_init,obj%sx_last,obj%sx_tot)

      call increment_result (obj%increment_ysrc, &
               dummy,dummy,obj%sy_inc,obj%sy_init,obj%sy_last,obj%sy_tot)

      call increment_result (obj%increment_xrec, &
               dummy,dummy,obj%rx_inc,obj%rx_init,obj%rx_last,obj%rx_tot)

      call increment_result (obj%increment_yrec, &
               dummy,dummy,obj%ry_inc,obj%ry_init,obj%ry_last,obj%ry_tot)

      call increment_result (obj%increment_xcmp, &
               dummy,dummy,obj%cx_inc,obj%cx_init,obj%cx_last,obj%cx_tot)

      call increment_result (obj%increment_ycmp, &
               dummy,dummy,obj%cy_inc,obj%cy_init,obj%cy_last,obj%cy_tot)

      call increment_result (obj%increment_offset, &
               dummy,dummy,obj%off_inc,obj%off_init,obj%off_last,obj%off_tot)

!----------print summary of input traces.

      write(obj%lun,*)' '
      write(obj%lun,*)'STATHELPER: ',obj%ntraces,' TRACES ENCOUNTERED:'
      write(obj%lun,*)' '
      write(obj%lun,*)'                   HWD     MINIMUM    MAXIMUM',  &
                                                  '  INCREMENT   TOTAL'
      write(obj%lun,3333)'X-SOURCE'  ,obj%hdr_sx, obj%sx_init , obj%sx_last , &
                                                  obj%sx_inc  , obj%sx_tot
      write(obj%lun,3333)'Y-SOURCE'  ,obj%hdr_sy, obj%sy_init , obj%sy_last , &
                                                  obj%sy_inc  , obj%sy_tot
      write(obj%lun,3333)'X-RECEIVER',obj%hdr_rx, obj%rx_init , obj%rx_last , &
                                                  obj%rx_inc  , obj%rx_tot
      write(obj%lun,3333)'Y-RECEIVER',obj%hdr_ry, obj%ry_init , obj%ry_last , &
                                                  obj%ry_inc  , obj%ry_tot
      write(obj%lun,3333)'X-CMP'     ,    7     , obj%cx_init , obj%cx_last , &
                                                  obj%cx_inc  , obj%cx_tot
      write(obj%lun,3333)'Y-CMP'     ,    8     , obj%cy_init , obj%cy_last , &
                                                  obj%cy_inc  , obj%cy_tot
      write(obj%lun,3333)'OFFSET'    ,    6     , obj%off_init, obj%off_last, &
                                                  obj%off_inc , obj%off_tot
      write(obj%lun,*)' '
      write(obj%lun,*)'                (the offset is in CMP grid units)'
      write(obj%lun,*)' '
3333  FORMAT ('      ',A12,1x,i3,1x,2F11.2,F9.2,I9)

!----------create the sortkeys object.

      call sortkeys_delete (obj%sortkeys)

                                !!!!!!!!!!  nrecords nkeys nsorts
                                !!!!!!!!!!      |      |    |
      call sortkeys_create (obj%sortkeys, obj%ntraces, 5,   3, error)
      if (error) then
           call pc_error ('STATHELPER: error creating SORTKEYS object')
           return
      end if

!----------read through the header word file to set the keys.

      call stathelper_remember_rewind (obj)

      do itrace = 1,obj%ntraces

        call stathelper_remember_read  (obj,error,                &
                                        xcoord_src, ycoord_src,   &
                                        xcoord_rec, ycoord_rec,   &
                                        xcoord_cmp, ycoord_cmp,   &
                                        signed_offset)
        if (error) return

        call mth_get_constrained_indices                                    &
                             (xcoord_src,obj%sx_init,obj%sx_inc,obj%sx_tot, &
                              ycoord_src,obj%sy_init,obj%sy_inc,obj%sy_tot, &
                              ix,iy,isrc)

        call mth_get_constrained_indices                                    &
                             (xcoord_rec,obj%rx_init,obj%rx_inc,obj%rx_tot, &
                              ycoord_rec,obj%ry_init,obj%ry_inc,obj%ry_tot, &
                              ix,iy,irec)

        call mth_get_constrained_indices                                    &
                             (xcoord_cmp,obj%cx_init,obj%cx_inc,obj%cx_tot, &
                              ycoord_cmp,obj%cy_init,obj%cy_inc,obj%cy_tot, &
                              ix,iy,icmp)

        call sortkeys_set_keys (obj%sortkeys,itrace,isrc,irec,icmp,isoff,iaoff)

      end do

      call stathelper_remember_close (obj)

      call sortkeys_define_sort (obj%sortkeys,1,IKEY_CMP,IKEY_AOFF)

!----------create the static file objects.

      obj%file(:)%ikey = 0
      obj%file(:)%ngp  = 0

      if (obj%isfile > 0) then
           ifile = obj%isfile
           nrun  = max(nint(obj%ra_src / obj%sx_inc),3)
           call statfile_create                                          &
                      (obj%file(ifile)%statfile,                         &
                       obj%procname,'SOURCE',                            &
                       obj%path_src,obj%path_byt_src,obj%path_inc_src,   &
                       obj%no_dead,obj%num_iter,obj%converge,obj%ncorr,  &
                       obj%ipn,obj%lun,obj%dt,                           &
                       obj%hdr_sx,obj%hdr_sy,                            &
                       obj%sx_init,obj%sy_init,obj%sx_inc,obj%sy_inc,    &
                       obj%sx_tot,obj%sy_tot,nrun,                       &
                       obj%cards,obj%ncards,error)
           if (error) return
           call sortkeys_define_sort &
                           (obj%sortkeys,ifile+1,IKEY_SRC,obj%ikey_scorr)
           obj%file(ifile)%ikey = IKEY_SRC
           obj%file(ifile)%ngp  = obj%sx_tot * obj%sy_tot
      end if

      if (obj%irfile > 0) then
           ifile = obj%irfile
           nrun  = max(nint(obj%ra_rec / obj%rx_inc),3)
           call statfile_create                                          &
                      (obj%file(ifile)%statfile,                         &
                       obj%procname,'RECEIVER',                          &
                       obj%path_rec,obj%path_byt_rec,obj%path_inc_rec,   &
                       obj%no_dead,obj%num_iter,obj%converge,obj%ncorr,  &
                       obj%ipn,obj%lun,obj%dt,                           &
                       obj%hdr_rx,obj%hdr_ry,                            &
                       obj%rx_init,obj%ry_init,obj%rx_inc,obj%ry_inc,    &
                       obj%rx_tot,obj%ry_tot,nrun,                       &
                       obj%cards,obj%ncards,error)
           if (error) return
           call sortkeys_define_sort &
                           (obj%sortkeys,ifile+1,IKEY_REC,obj%ikey_rcorr)
           obj%file(ifile)%ikey = IKEY_REC
           obj%file(ifile)%ngp  = obj%rx_tot * obj%ry_tot
      end if

!----------create files for storing stacked and base trace windows.

      if (obj%procname == 'FISH' .or. obj%procname == 'IMS') then
        call temptfile_open &
                  (obj%stackfile,trim(obj%procname)//'_stacked_windows', &
                   1,obj%nwin,obj%lun,err)
        if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary stack window file open error')
           call pc_error (obj%errmsg)
           error = .true.
           return
        end if
      end if

      if (obj%procname == 'FISH' .or. obj%procname == 'IMS') then
        call temptfile_open &
                  (obj%basefile,trim(obj%procname)//'_base_windows', &
                   1,obj%nwin,obj%lun,err)
        if (err /= TEMPTFILE_OK) then
           call pc_error ('STATHELPER: temporary base window file open error')
           call pc_error (obj%errmsg)
           error = .true.
           return
        end if
      end if

!----------finish up and return.

      error = .false.
      return
      end subroutine stathelper_private_pre_solve


!!--------------------- stathelper private post solve -------------------!!
!!--------------------- stathelper private post solve -------------------!!
!!--------------------- stathelper private post solve -------------------!!


      subroutine stathelper_private_post_solve (obj,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj          ! arguments
      logical                ,intent(in)    :: error        ! arguments
      character(len=200)                    :: line,line2   ! local
      integer                               :: ifile        ! local

!----------print error information.

      if (error) then
           call pc_error ('STATHELPER: error solving for file',obj%ifile)
           call pc_error ('STATHELPER: error in iteration',obj%iter)
      else
      end if

!----------close files.

      call temptfile_close (obj%tracefile)
      call temptfile_close (obj%stackfile)
      call temptfile_close (obj%basefile)

      do ifile = 1,obj%nfiles
           call statfile_close_files (obj%file(ifile)%statfile)
      end do

!----------print final information.

      if (error) then
           call pc_error (obj%errmsg)
      else if (obj%dead_end) then
           call pc_print ('STATHELPER: NOTHING PASSED TO NEXT CPS PROCESS')
      else
           call pc_print ('STATHELPER: BEGINNING TO OUTPUT SHIFTED TRACES')
      end if

!----------print footer.

      line  = '++++++++++++++++++++'                              // &
              ' END '//trim(obj%procname)//' PROCESSING SUMMARY ' // &
              '++++++++++++++++++++'
      line2 = '+++++++++++++++++++++++++++++++++++++++++++++'     // &
              '+++++++++++++++++++++++++++++++++++++++++++++'     // &
              '+++++++++++++++++++++++++++++++++++++++++++++'
      line2(len_trim(line)+1:) = ' '
      call pc_print (' ')
      call pc_print (line2)
      call pc_print (line)
      call pc_print (line2)
      call pc_print (' ')
      return
      end subroutine stathelper_private_post_solve


!!--------------------- stathelper begin iteration ------------------------!!
!!--------------------- stathelper begin iteration ------------------------!!
!!--------------------- stathelper begin iteration ------------------------!!


      subroutine stathelper_begin_iteration (obj,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj               ! arguments
      logical                ,intent(out)   :: error             ! arguments

      if(obj%iter == 0) call statfile_print_header (obj%lun)
      obj%iter      = obj%iter + 1
      obj%ifile     = 0
      obj%converged = .true.
      error         = .false.
      return
      end subroutine stathelper_begin_iteration


!!--------------------- stathelper end iteration ------------------------!!
!!--------------------- stathelper end iteration ------------------------!!
!!--------------------- stathelper end iteration ------------------------!!


      subroutine stathelper_end_iteration (obj,converged,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj               ! arguments
      logical                ,intent(out)   :: converged         ! arguments
      logical                ,intent(out)   :: error             ! arguments
      integer                               :: ifile             ! local
      logical                               :: error2            ! local

      converged = obj%converged
      if (converged .or. obj%iter >= obj%num_iter) then
           call pc_print (' ')
           if (obj%iter >= obj%num_iter) &
                          call pc_print ('STATHELPER: iterations finished')
           if (converged) call pc_print ('STATHELPER: converged')
           call pc_print (trim(obj%procname)//': final solution attained')
           call pc_print (' ')
      end if

      error = .false.
      do ifile = 1,obj%nfiles
           call statfile_save_files (obj%file(ifile)%statfile,converged,error2)
           if (error2) then
                call pc_error ('STATHELPER: error saving files for file',ifile)
                call pc_error ('STATHELPER: error after iteration',obj%iter)
                call pc_error (obj%errmsg)
                error = .true.
           end if
      end do
      return
      end subroutine stathelper_end_iteration


!!--------------------- stathelper begin file iteration ---------------------!!
!!--------------------- stathelper begin file iteration ---------------------!!
!!--------------------- stathelper begin file iteration ---------------------!!


      subroutine stathelper_begin_file_iteration (obj,ifile,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj               ! arguments
      integer                ,intent(in)    :: ifile             ! arguments
      logical                ,intent(out)   :: error             ! arguments

      obj%ifile = ifile
      call statfile_begin_file_iteration (obj%file(ifile)%statfile,error)
      if (error) then
           call pc_error ('STATHELPER: error before solving for file',ifile)
           call pc_error ('STATHELPER: error in iteration',obj%iter)
           call pc_error (obj%errmsg)
           return
      end if
      return
      end subroutine stathelper_begin_file_iteration


!!--------------------- stathelper end file iteration ---------------------!!
!!--------------------- stathelper end file iteration ---------------------!!
!!--------------------- stathelper end file iteration ---------------------!!


      subroutine stathelper_end_file_iteration (obj,ifile,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj               ! arguments
      integer                ,intent(in)    :: ifile             ! arguments
      logical                ,intent(out)   :: error             ! arguments
      logical                               :: converged         ! local

      call statfile_end_file_iteration &
                              (obj%file(ifile)%statfile,converged,error)
      if (ifile == 2) call pc_print (' ')
      if (error) then
           call pc_error ('STATHELPER: error after solving for file',ifile)
           call pc_error ('STATHELPER: error in iteration',obj%iter)
           call pc_error (obj%errmsg)
           obj%converged = .false.
      else if (.not.converged) then
           obj%converged = .false.
      end if
      return
      end subroutine stathelper_end_file_iteration


!!--------------------- stathelper corr ----------------------------------!!
!!--------------------- stathelper corr ----------------------------------!!
!!--------------------- stathelper corr ----------------------------------!!


      subroutine stathelper_corr (obj,win,ref,   corr,denom)
      implicit none
      type(stathelper_struct),intent(in)  :: obj                ! arguments
      real   ,                intent(in)  :: win (obj%nwin)     ! arguments
      real   ,                intent(in)  :: ref (obj%nwin)     ! arguments
      real   ,                intent(out) :: corr(obj%ncorr)    ! arguments
      real   ,                intent(out) :: denom              ! arguments

      call statutil_corr (win, ref, obj%nwin, corr, obj%ncorr, denom)
      return
      end subroutine stathelper_corr


!!--------------------- stathelper pick ----------------------------------!!
!!--------------------- stathelper pick ----------------------------------!!
!!--------------------- stathelper pick ----------------------------------!!


      subroutine stathelper_pick (obj,corr,ccmin,denom,   static,ccoef)
      implicit none
      type(stathelper_struct),intent(in)  :: obj               ! arguments
      real   ,                intent(in)  :: corr(obj%ncorr)   ! arguments
      real   ,                intent(in)  :: ccmin,denom       ! arguments
      real   ,                intent(out) :: static,ccoef      ! arguments

      call statutil_pick_enhanced &
                   (corr, obj%ncorr, obj%npick, ccmin, denom, static, ccoef)
      return
      end subroutine stathelper_pick


!!--------------------- stathelper report static -------------------------!!
!!--------------------- stathelper report static -------------------------!!
!!--------------------- stathelper report static -------------------------!!


      subroutine stathelper_report_static &
                      (obj,ifile,igp,corr,static,ccoef,nnt,nnc,error)
      implicit none
      type(stathelper_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: ifile,igp,nnt,nnc  ! arguments
      real                   ,intent(in)    :: corr(:)            ! arguments
      real                   ,intent(in)    :: static,ccoef       ! arguments
      logical                ,intent(out)   :: error              ! arguments

      call statfile_report_static &
              (obj%file(ifile)%statfile,igp,corr,static,ccoef,nnt,nnc,error)
      if (error) call pc_error (obj%errmsg)
      return
      end subroutine stathelper_report_static


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module stathelper_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

