!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- basetrace.f90 --------------------------------!!
!!---------------------------- basetrace.f90 --------------------------------!!
!!---------------------------- basetrace.f90 --------------------------------!!


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
! Name       : BASETRACE 
! Category   : math
! Written    : 2000-05-01   by: Tom Stoeckley
! Revised    : 2006-11-14   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Base trace calculator for CMP-type statics process modules.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is to be used by CMP-type statics process modules which
! need base traces to correlate with.  This primitive owns and manages all
! parameters needed for this functionality, and also manages the calculations
! required to return the appropriate base trace for a set of input traces.
!
! This primitive uses the parameter cache to read and write the parameters
! it needs, to report error messages, etc.
!
! This primitive also uses the CORDIP and SEMDIP primitives.
!
! To use this primitive from a process module named xxxx:
!
!    (1) basetrace_create     should be called from xxxx_create.
!    (2) basetrace_initialize should be called from xxxx_initialize.
!    (3) basetrace_update     should be called from xxxx_update.
!    (4) basetrace_build      should be called from xxxx.
!    (5) basetrace_wrapup     should be called from xxxx_wrapup.
!    (6) basetrace_delete     should be called from xxxx_delete.
!
! The purpose of this primitive is to encapsulate a standard method of
! specifying base traces for uniformity for the user, to encapsulate
! the code dealing with these base traces, and to encapsulate the screen
! layout and context-sensitive help so that they need not be repeated in
! the individual processes.  Any changes to this primitive can be made
! without affecting the processes which use it.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name       Description                             Action taken
! ----       -----------                             ------------
! NWIH       number of words in trace header         used but not changed
! NDPT       number of sample values in trace        used but not changed
! TSTRT      starting time on trace                  used but not changed
! DT         trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#        Description                                          Action taken
! ----        -----------                                          ------------
! HDR_X       header designating CMP X grid coordinate.             not changed
! HDR_Y       header designating CMP Y grid coordinate.             not changed
! 58 scratch  header in HDMIX and HDBASE containing X coordinate.   changed
! 59 scratch  header in HDMIX and HDBASE containing Y coordinate.   changed
! 60 scratch  header in HDMIX containing a flag given to the trace. changed
! 61 scratch  header in HDMIX containing a flag given to the trace. changed
!
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
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                                 o     i
!     call basetrace_create     (obj,procname)
!
!                                 b
!     call basetrace_initialize (obj)
!
!                             b      i         i       o
!     call basetrace_update (obj,max_static,num_iter,error,
!                                          hdr_x, x_init, x_inc, bins_x,
!                                          hdr_y, y_init, y_inc, bins_y)
!                                            o      o       o      o
!                                           opt    opt     opt    opt
!
!                                      b    o
!     call basetrace_begin_iteration (obj,error)
!     call basetrace_end_iteration   (obj,error)
!
!                         b    b     b    i   i    i   i    b      o      o
!  call basetrace_build (obj,hdmix,trmix,ntr,mid,twin,nwt,hdbase,trbase,error)
!
!                                 b
!     call basetrace_wrapup     (obj)
!     call basetrace_delete     (obj)
!
!
! type(basetrace_struct)  obj = pointer to the BASETRACE structure.
! character(len=*)   procname = name of statics process calling this primitive.
! real             max_static = maximum shift to allow for MSCTFB.
! integer            num_iter = total number of iterations.
! integer       hdr_x ,hdr_y  = header words for CMP X and Y grid coordinates.
! real          x_init,y_init = X and Y grid coords of center of any CMP bin.
! real          x_inc ,y_inc  = X and Y grid width of CMP bins.
! integer       bins_x,bins_y = number of bins to composite.
!
! integer NTR          = number of input traces to use for building base trace.
! double  HDMIX(NWIH,NTR) = input headers (first dim may exceed NWIH).
! real    TRMIX(NDPT,NTR) = input traces  (first dim may exceed NDPT).
! integer MID          = trace and header index (1<=MID<=NTR) of center trace.
! real    TWIN(2,NWT)  = top and bottom of correlation window in seconds,
!                         specified in pairs, for one or more windows.
! integer NWT          = number of correlation windows (one or more - no limit).
! double  HDBASE(nwih) = output header (must be pre-set upon input).
! real    TRBASE(ndpt) = output base trace.
! logical error        = error flag (true if an error occurred).
!
! Note: Normally MAX_STATIC is the maximum static shift in the calling process.
!
! BASETRACE_BUILD:
!  (1) builds a base trace from the input traces by using CORDIP or SEMDIP,
!       or by reading the next base trace from a file.
!  (2) HDBASE must be preset.
!  (3) The output base trace is located at the position given by the
!       (X,Y) coordinates preset by the user in HDBASE.
!  (4) optionally saves normalized unfiltered version of base trace to a file.
!  (5) returns un-normalized but optionally filtered base trace.
!
! BASETRACE_BEGIN_ITERATION:
! BASETRACE_END_ITERATION:
!  (1) These calls are used to decide whether to read base traces from a file
!       (method FILE... and PATH_BASE_IN specified) or to write them to a file
!       (PATH_BASE_OUT specified).
!  (2) Base trace files are opened when beginning iterations and closed when
!       ending iterations.
!  (3) Base traces are written to a file after each iteration.
!  (4) Traces are read from a file only when the iteration count is one (the
!       first iteration), or if the method is FILE.
!  (5) If both PATH_BASE_IN and PATH_BASE_OUT are specified, base traces are
!       not written out on the first iteration because they would be identical
!       to the input base traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!  1. This primitive is used by processes IMS and RTC, and potentially by
!     any other statics process which needs base traces of the type built
!     by this primitive.
!
!  2. The GUI_DEF section for a process which uses this primitive should
!     contain an INCLUDE line which says to include the GUI_DEF section
!     of this primitive.  The HELPSECTION for the parameters in this primitive
!     will also be made available to the process when this is done.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
! 17. 2006-11-14  Stoeckley  Replace pc_register_tab_group w HelpSection line.
! 16. 2006-08-15  D. Glover  Added NULLIFY statements for Intel compiler.
! 15. 2006-06-20  Stoeckley  Add pc_register_tab_group for SeisSpace.
!014. 2006-01-10  B. Menger   Removed Unused Variables.
! 13. 2001-12-10  Stoeckley  Make minor changes to the SEMDIP arguments
!                             to conform with improvements in SEMDIP.
! 12. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
! 11. 2001-01-10  Stoeckley  Fix uninitialized variable which occasionally
!                             prohibited setting WIN_LEN properly.
! 10. 2000-09-27  Stoeckley  Add documentation to the HelpSection for 2D data.
!  9. 2000-08-21  Stoeckley  Change to allow an output basetrace file to be
!                             a BYTE file; remove some unnecessary printouts
!                             which interrupted tables printed by IMS.
!  8. 2000-08-01  Stoeckley  Add intent(out) to the ERROR argument in
!                             BASETRACE_BUILD.
!  7. 2000-07-20  Stoeckley  Change INL and CRL parameter names to X and Y.
!  6. 2000-06-21  Stoeckley  Change SHIFT_MAX to MAX_STATIC for consistency
!                             with statics processes.
!  5. 2000-06-16  Stoeckley  Fix MSCTFB default to agree with documentation.
!  4. 2000-06-05  Stoeckley  Change printouts to identify the calling
!                             process; remove the EVERY parameter.
!  3. 2000-05-22  Stoeckley  Change to use the PERMTFILE primitive instead
!                             of the TRCIO primitive.
!  2. 2000-05-18  Stoeckley  Add methods FILE_THEN_CORR and FILE_THEN_SDIP.
!                             Replace parameter PATH_BASE with PATH_BASE_IN
!                             and PATH_BASE_OUT.  Remove unused parameter
!                             NUM_BASE.  Add arguments to basefile_create
!                             and basefile_update.  Add routines to be called
!                             at the beginning and end of each iteration.
!                             These updates allow this primitive to be used
!                             with both RTC and IMS.
!  1. 2000-05-01  Stoeckley  Initial version.
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


!-------------------------------------------------------------------------------
!<gui_def>
!                               FREQ_BEG=`FFFFFFF     FREQ_END=`FFFFFFF
!    METHOD=`CCCCCCCCCCCCCCC    MSCTFB=`FFFFFFFFFFF   WIN_LEN=`FFFFFFFFFFF 
!
!    `-----------------------------  `------------------------------
!       HDR_X=~~~~~~`IIIIII             HDR_Y=~~~~~~`IIIIII
!       X_INIT=~~~~~`FFFFFFFFFFF        Y_INIT=~~~~~`FFFFFFFFFFF
!       X_INC=~~~~~~`FFFFFFFFFFF        Y_INC=~~~~~~`FFFFFFFFFFF
!       BINS_X=~~~~~`IIIIIIII           BINS_Y=~~~~~`IIIIIIII
!       GAP_X=~~~~~~`IIIIIIII           GAP_Y=~~~~~~`IIIIIIII
!       DIP_X=~~~~~~`FFFFFFFFFFF        DIP_Y=~~~~~~`FFFFFFFFFFFF
!       BINS_INIT_X=`IIIIIIII           BINS_INIT_Y=`IIIIIIII
!       DIP_INC_X=~~`FFFFFFFFFFF        DIP_INC_Y=~~`FFFFFFFFFFFF
!    `-----------------------------  `------------------------------
!
! Select PATH_BASE_IN [PATH_BASE_IN ]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [path_base_in_info ]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! Select PATH_BASE_OUT[PATH_BASE_OUT]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [path_base_out_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS PATH_BASE_IN [/ML=128/XST]>
!<PARMS PATH_BASE_OUT[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!     tabgroup = Base Trace Parameters
!
!<Help KEYWORD="PATH_BASE_IN_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_BASE_IN. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_BASE_OUT_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_BASE_OUT. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATH_BASE_IN">
!<Tip> Choose PATH_BASE_IN using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATH_BASE_OUT">
!<Tip> Choose PATH_BASE_OUT using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Lowest frequency to use for computing correlations. </Tip>
! Default = 5
! Allowed = real > 0
!
! Bandpass filter is applied only to the base trace being correlated with.
! Output traces are not bandpass filtered.
!</Help>
!
!
!<Help KEYWORD="FREQ_END">
!<Tip> Highest frequency to use for computing correlations. </Tip>
! Default = 0.5*Nyquist
! Allowed = FREQ_BEG < real < Nyquist
!
! Bandpass filter is applied only to the base trace being correlated with.
! Output traces are not bandpass filtered.
!</Help>
!
!
!<Help KEYWORD="METHOD">
!<Tip> Method for building the base trace. </Tip>
! Default = CORR
! Allowed = CORR  (Correlate and shift stacked trace windows to form base)
! Allowed = SDIP  (Do semblance-based dip stack to form base)
! Allowed = FILE  (Use base traces from TROT file specified by PATH_BASE_IN)
! Allowed = FILE_THEN_CORR  (Use base traces from TROT file, then CORR method)
! Allowed = FILE_THEN_SDIP  (Use base traces from TROT file, then SDIP method)
!
! For the IMS process:
!   Method FILE is NOT an allowed option.
!   For method FILE_THEN_CORR, the base traces from the TROT file are used
!    for the first iteration, and method CORR is used to create base traces for
!    the rest of the iterations.
!   For method FILE_THEN_SDIP, the base traces from the TROT file are used
!    for the first iteration, and method SDIP is used to create base traces for
!    the rest of the iterations.
!
! For the RTC process:
!   Methods FILE_THEN_CORR and FILE_THEN_SDIP are NOT allowed options.
!   
!</Help>
!
!
!<Help KEYWORD="MSCTFB">
!<Tip> Maximum Shift in the Central Trace used to Form the Base. </Tip>
! Default = 10.0
! Allowed = real >= 0.0
!
! Maximum Shift, in ms, in the Central Trace used to Form the Base.
! MSCTFB cannot exceed MAX_STATIC.
! Used for METHOD = CORR and FILE_THEN_CORR only.
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the semblance window in seconds. </Tip>
! Default = 0.1
! Allowed = real > 0.0
!
! Used for METHOD = SDIP and FILE_THEN_SDIP only.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word designating CMP X grid coordinates. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word designating CMP Y grid coordinates. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="X_INIT">
!<Tip> Value of header word HDR_X for the center of any CMP. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Value of header word HDR_Y for the center of any CMP. </Tip>
! Default = 1.0
! Allowed = real
!
! For 2D data, this number is normally irrelevant.
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip>Increment of header word HDR_X between CMPs (X grid bin width).</Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip>Increment of header word HDR_Y between CMPs (Y grid bin width).</Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! For 2D data, this number is normally irrelevant.
!</Help>
!
!
!<Help KEYWORD="BINS_X">
!<Tip> Number of CMPs to stack in the X grid direction to form base. </Tip>
! Default = 5
! Allowed = odd int > 0
!</Help>
!
!
!<Help KEYWORD="BINS_Y">
!<Tip> Number of CMPs to stack in the Y grid direction to form base. </Tip>
! Default = 1
! Allowed = odd int > 0
!
! For 2D data, this number should be zero.
!</Help>
!
!
!<Help KEYWORD="GAP_X">
!<Tip> Number of CMPs to leave as a gap in the X grid direction. </Tip>
! Default = 0
! Allowed = 0 or odd int > 0
!
! Number of CMPs in the X grid direction to leave as a gap in the center of
!  the group of CMPs used for the base trace.
! GAP_X must be less than BINS_X.
! Not used if METHOD = FILE.
!</Help>
!
!
!<Help KEYWORD="GAP_Y">
!<Tip> Number of CMPs to leave as a gap in the Y grid direction. </Tip>
! Default = 0
! Allowed = 0 or odd int > 0
!
! Number of CMPs in the Y grid direction to leave as a gap in the center of
!  the group of CMPs used for the base trace.
! GAP_Y must be less than BINS_Y.
! Not used if METHOD = FILE.
! For 2D data, this number should be zero.
!</Help>
!
!
!<Help KEYWORD="BINS_INIT_X">
!<Tip> Number of CMPs in the X grid direction to use for an initial base. </Tip>
! Default = 1
! Allowed = odd int > 0
!
! BINS_INIT_X must not exceed BINS_X.
! Used for METHOD = CORR and FILE_THEN_CORR only.
!</Help>
!
!
!<Help KEYWORD="BINS_INIT_Y">
!<Tip> Number CMPs in the Y grid direction to use for an initial base. </Tip>
! Default = 1
! Allowed = odd int > 0
!
! BINS_INIT_Y must not exceed BINS_Y.
! Used for METHOD = CORR and FILE_THEN_CORR only.
! For 2D data, this number should be one.
!</Help>
!
!
!<Help KEYWORD="DIP_X">
!<Tip> Maximum dip in X grid direction for traces used to form the base. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! Maximum dip in X grid direction, in ms/trace, for traces used to form the
! base.  Non-zero values allow larger shifts for more distant traces and can
! help compensate for dips.
! Not used if METHOD = FILE.
!</Help>
!
!
!<Help KEYWORD="DIP_Y">
!<Tip> Maximum dip in Y grid direction for traces used to form the base. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! Maximum dip in Y grid direction, in ms/trace, for traces used to form the
! base.  Non-zero values allow larger shifts for more distant traces and can
! help compensate for dips.
! Not used if METHOD = FILE.
! For 2D data, this number should be zero.
!</Help>
!
!
!<Help KEYWORD="DIP_INC_X">
!<Tip> CMP X grid dip increment in milliseconds/trace. </Tip>
! Default = 10.0
! Allowed = real > 0.0
!
! Used for METHOD = SDIP and FILE_THEN_SDIP only.
! If DIP_X is zero, this number is irrelevant.
!</Help>
!
!
!<Help KEYWORD="DIP_INC_Y">
!<Tip> CMP Y grid dip increment in milliseconds/trace. </Tip>
! Default = 10.0
! Allowed = real > 0.0
!
! Used for METHOD = SDIP and FILE_THEN_SDIP only.
! If DIP_Y is zero, this number is irrelevant.
!</Help>
!
!
!<Help KEYWORD="PATH_BASE_IN">
!<Tip> Pathname for input TROT file containing base traces. </Tip>
! Default = NONE
! Allowed = char
!
! Used for METHOD = FILE and FILE_THEN_CORR and FILE_THEN_SDIP only.
!
! For the RTC process:
!   If METHOD = FILE, use base traces from the file PATH_BASE_IN.
!   Otherwise create base traces using the other specified parameters.
!
! For the IMS process:
!   If METHOD = FILE_THEN_CORR or FILE_THEN_SDIP, use this file only for the
!    first iteration.
!   Otherwise create base traces using the other specified parameters.
!</Help>
!
!
!<Help KEYWORD="PATH_BASE_OUT">
!<Tip> Pathname for output TROT file containing base traces. </Tip>
! Default = NONE
! Allowed = char
!
! Base traces are written to the file PATH_BASE_OUT unless the parameter
! PATH_BASE_OUT = NONE.  The base traces are output regardless of the
! specified METHOD (with restrictions specified below).
!
! For the RTC process:
!   If METHOD = FILE, base traces are not output because they would be
!    identical to the input base traces. 
!
! For the IMS process:
!   Base traces are written after each iteration, overwriting the previous
!    contents of the file.
!   If METHOD = FILE_THEN_CORR or FILE_THEN_SDIP, base traces are not output
!    after the first iteration because they would be identical to the input
!    base traces.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module basetrace_module
      use named_constants_module
      use pc_module
      use pathcheck_module
      use pathchoose_module
      use cordip_module
      use semdip_module
      use mth_module
      use mem_module
      use permtfile_module
      use fltr_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: BASETRACE_IDENT = &
       '$Id: basetrace.f90,v 1.17 2006/11/14 14:32:50 Stoeckley prod sps $'


      type,public :: basetrace_struct              

       private
       character(len=40) :: procname                      ! supplied parameter
       integer           :: num_iter                      ! supplied parameter

       integer           :: nwih,ndpt                     ! global parameters
       real              :: tstrt,dt                      ! global parameters

       real              :: freq_beg                      ! process parameters
       real              :: freq_end                      ! process parameters
       character(len=16) :: method                        ! process parameters
       real              :: msctfb,win_len                ! process parameters
       integer           :: hdr_x,hdr_y                   ! process parameters
       real              :: x_init,y_init                 ! process parameters
       real              :: x_inc,y_inc                   ! process parameters
       integer           :: bins_x,bins_y                 ! process parameters
       integer           :: gap_x,gap_y                   ! process parameters
       integer           :: bins_init_x,bins_init_y       ! process parameters
       real              :: dip_x,dip_y                   ! process parameters
       real              :: dip_inc_x,dip_inc_y           ! process parameters
       character(len=FILENAME_LENGTH)  :: path_base_in    ! process parameters
       character(len=FILENAME_LENGTH)  :: path_base_out   ! process parameters

       character(len=40)               :: errmsg          ! dependent
       type(permtfile_struct) ,pointer :: permtfile_in    ! dependent
       type(permtfile_struct) ,pointer :: permtfile_out   ! dependent
       integer                         :: nfilt,iter      ! dependent
       real                   ,pointer :: filt(:)         ! dependent
       type(pathchoose_struct),pointer :: dialog1         ! dependent
       type(pathchoose_struct),pointer :: dialog2         ! dependent

      end type basetrace_struct

      integer,parameter,private      :: SCRATCH_NHX = 58
      integer,parameter,private      :: SCRATCH_NHY = 59
      integer,parameter,private      :: SCRATCH_NHW = 60
      integer,parameter,private      :: SCRATCH_NHB = 61

      integer,parameter,private      :: method_rtc_nopt = 3
      integer,parameter,private      :: method_ims_nopt = 4
      character(len=16),private,save :: method_rtc_options (method_rtc_nopt)
      character(len=16),private,save :: method_ims_options (method_ims_nopt)

      data method_rtc_options /'CORR','SDIP','FILE'/
      data method_ims_options /'CORR','SDIP','FILE_THEN_CORR','FILE_THEN_SDIP'/

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine basetrace_create (obj,procname)
      implicit none
      type(basetrace_struct),pointer :: obj            ! arguments
      character(len=*),intent(in)    :: procname       ! arguments

      allocate (obj)

      nullify (obj%permtfile_in)
      nullify (obj%permtfile_out)
      nullify (obj%filt)
      nullify (obj%dialog1) ! jpa
      nullify (obj%dialog2) ! jpa

      obj%procname = procname
      obj%errmsg   = trim(procname)//': FATAL ERROR'

      call string_to_upper (obj%procname)
      call string_to_upper (obj%errmsg)

      call pathchoose_create (obj%dialog1, 'path_base_in' , '*')
      call pathchoose_create (obj%dialog2, 'path_base_out', 'base')
      return
      end subroutine basetrace_create


!!------------------------------ wrapup -----------------------------------!!
!!------------------------------ wrapup -----------------------------------!!
!!------------------------------ wrapup -----------------------------------!!


      subroutine basetrace_wrapup (obj)
      implicit none
      type(basetrace_struct),intent(inout) :: obj                 ! arguments

      call permtfile_close (obj%permtfile_in)
      call permtfile_close (obj%permtfile_out)
      call mem_free (obj%filt)
      return
      end subroutine basetrace_wrapup


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine basetrace_delete (obj)
      implicit none
      type(basetrace_struct),pointer :: obj       ! arguments

      call basetrace_wrapup  (obj)
      call pathchoose_delete (obj%dialog1)
      call pathchoose_delete (obj%dialog2)

      deallocate(obj)
      return
      end subroutine basetrace_delete


!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!


      subroutine basetrace_initialize (obj)
      implicit none
      type(basetrace_struct)  :: obj                 ! arguments

      obj%nwih          = 0
      obj%ndpt          = 0
      obj%tstrt         = 0.0
      obj%dt            = 0.0

      obj%freq_beg      = FNIL                ! was if1
      obj%freq_end      = FNIL                ! was if2
      obj%method        = 'CORR'
      obj%msctfb        = 10.0                ! was mxsb and rmlagb
      obj%win_len       = 0.1                 ! was tslc
      obj%hdr_x         = 7                   ! was nxhg
      obj%hdr_y         = 8                   ! was nyhg
      obj%x_init        = 1.0                 ! was xgv1
      obj%y_init        = 1.0                 ! was ygv1
      obj%x_inc         = 1.0                 ! was xginc
      obj%y_inc         = 1.0                 ! was yginc
      obj%bins_x        = 5                   ! was nxcdp
      obj%bins_y        = 1                   ! was nycdp
      obj%gap_x         = 0                   ! was nxgap
      obj%gap_y         = 0                   ! was nygap
      obj%bins_init_x   = 1                   ! was nxicb
      obj%bins_init_y   = 1                   ! was nyicb
      obj%dip_x         = 0.0                 ! was dipx
      obj%dip_y         = 0.0                 ! was dipy
      obj%dip_inc_x     = 10.0                ! was deltax
      obj%dip_inc_y     = 10.0                ! was deltay
      obj%path_base_in  = PATHCHECK_EMPTY     ! was basefile
      obj%path_base_out = PATHCHECK_EMPTY     ! was basefile

      obj%nfilt         = 0
      return
      end subroutine basetrace_initialize


!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!


      subroutine basetrace_update (obj, max_static, num_iter, error,     &
                                   hdr_x, x_init, x_inc, bins_x, &
                                   hdr_y, y_init, y_inc, bins_y)
      implicit none
      type(basetrace_struct),intent(inout) :: obj                 ! arguments
      real                  ,intent(in)    :: max_static          ! arguments
      integer               ,intent(in)    :: num_iter            ! arguments
      logical               ,intent(out)   :: error               ! arguments
      integer    ,optional  ,intent(out)   :: hdr_x ,hdr_y        ! arguments
      real       ,optional  ,intent(out)   :: x_init,y_init       ! arguments
      real       ,optional  ,intent(out)   :: x_inc ,y_inc        ! arguments
      integer    ,optional  ,intent(out)   :: bins_x,bins_y       ! arguments

      real                                 :: tstop               ! local
      integer                              :: indx,midp,nfilth    ! local
      real                                 :: tpi,frqh,frql       ! local
      real                                 :: aaa,bbb,ccc         ! local
      logical                              :: do_corr,do_sdip     ! local
      logical                              :: sense_msctfb        ! local
      logical                              :: sense_win_len       ! local
      logical                              :: sense_bins_x      ! local
      logical                              :: sense_bins_y      ! local
      logical                              :: sense_gap_x       ! local
      logical                              :: sense_gap_y       ! local
      logical                              :: sense_bins_init_x ! local
      logical                              :: sense_bins_init_y ! local
      logical                              :: sense_dip_x       ! local
      logical                              :: sense_dip_y       ! local
      logical                              :: sense_dip_inc_x   ! local
      logical                              :: sense_dip_inc_y   ! local
      logical                              :: sense_path_base_in  ! local
      logical                              :: sense_path_base_out ! local

      obj%num_iter = num_iter

!----------read parameters:

     if (pathchoose_update(obj%dialog1,obj%path_base_in )) return
     if (pathchoose_update(obj%dialog2,obj%path_base_out)) return

      call pc_get_global ('nwih'   , obj%nwih         )
      call pc_get_global ('ndpt'   , obj%ndpt         )
      call pc_get_global ('tstrt'  , obj%tstrt        )
      call pc_get_global ('dt'     , obj%dt           )

      call pc_get ('freq_beg'     , obj%freq_beg      )
      call pc_get ('freq_end'     , obj%freq_end      )
      call pc_get ('method'       , obj%method        )
      call pc_get ('msctfb'       , obj%msctfb        )
      call pc_get ('win_len'      , obj%win_len       )
      call pc_get ('hdr_x'        , obj%hdr_x         )
      call pc_get ('hdr_y'        , obj%hdr_y         )
      call pc_get ('x_init'       , obj%x_init        )
      call pc_get ('y_init'       , obj%y_init        )
      call pc_get ('x_inc'        , obj%x_inc         )
      call pc_get ('y_inc'        , obj%y_inc         )
      call pc_get ('bins_x'       , obj%bins_x        )
      call pc_get ('bins_y'       , obj%bins_y        )
      call pc_get ('gap_x'        , obj%gap_x         )
      call pc_get ('gap_y'        , obj%gap_y         )
      call pc_get ('bins_init_x'  , obj%bins_init_x   )
      call pc_get ('bins_init_y'  , obj%bins_init_y   )
      call pc_get ('dip_x'        , obj%dip_x         )
      call pc_get ('dip_y'        , obj%dip_y         )
      call pc_get ('dip_inc_x'    , obj%dip_inc_x     )
      call pc_get ('dip_inc_y'    , obj%dip_inc_y     )
      call pc_get ('path_base_in' , obj%path_base_in  )
      call pc_get ('path_base_out', obj%path_base_out )

!----------verify globals:

      if (obj%dt <= 0.0) then
           call pc_error ('BASETRACE: global DT not set')
           obj%dt = 0.004
      end if

      TSTOP = obj%TSTRT + (obj%NDPT-1)*obj%DT

      if (obj%nwih < HDR_NOMINAL_SIZE) then
           call pc_error ('BASETRACE: global NWIH not set')
           obj%nwih = HDR_NOMINAL_SIZE
      end if

      if (obj%ndpt <= 1) then
           call pc_error ('BASETRACE: global NDPT not set')
           obj%ndpt = 1
      end if

!----------verify parameters:

      if (obj%procname == 'RTC') then
           if (obj%method /= 'CORR' .and. obj%method /= 'SDIP' .and. &
               obj%method /= 'FILE') then
                call pc_error ('BASETRACE: invalid METHOD value',obj%method)
           end if
      else
           if (obj%method /= 'CORR' .and. obj%method /= 'SDIP' .and. &
               obj%method /= 'FILE_THEN_CORR' .and. &
               obj%method /= 'FILE_THEN_SDIP') then
                call pc_error ('BASETRACE: invalid METHOD value',obj%method)
           end if
      end if

      if (obj%hdr_x < 1 .or. obj%hdr_x > obj%nwih) then
           call pc_error ('BASETRACE: invalid HDR_X value',obj%hdr_x)
      end if

      if (obj%hdr_y < 1 .or. obj%hdr_y > obj%nwih) then
           call pc_error ('BASETRACE: invalid HDR_Y value',obj%hdr_y)
      end if

      if (obj%hdr_x == obj%hdr_y) then
           call pc_error ('BASETRACE: HDR_X and HDR_Y cannot be the same.')
      end if

      if (obj%freq_beg == FNIL) obj%freq_beg = 5.0
      if (obj%freq_end == FNIL) obj%freq_end = 0.25 / obj%dt

      tstop = obj%tstrt + (obj%ndpt - 1) * obj%dt

      call mth_constrain (obj%freq_beg,  0.0              , 0.5/obj%dt - 5.0 )
      call mth_constrain (obj%freq_end, obj%freq_beg + 5.0, 0.5/obj%dt       )
      call mth_constrain (obj%msctfb  ,  0.0              , max_static       )
      call mth_constrain (obj%win_len , obj%dt            , tstop - obj%tstrt)

      if (obj%x_inc <= 0.0) obj%x_inc = 1.0
      if (obj%y_inc <= 0.0) obj%y_inc = 1.0

      call mth_constrain_odd (obj%bins_x, 1, 99)
      call mth_constrain_odd (obj%bins_y, 1, 99)

      call mth_constrain_odd (obj%gap_x, 0, max(obj%bins_x - 2, 0))
      call mth_constrain_odd (obj%gap_y, 0, max(obj%bins_y - 2, 0))

      call mth_constrain_odd (obj%bins_init_x, 1, obj%bins_x)
      call mth_constrain_odd (obj%bins_init_y, 1, obj%bins_y)

      if (obj%dip_x < 0.0) obj%dip_x = 0.0
      if (obj%dip_y < 0.0) obj%dip_y = 0.0

      if (obj%dip_inc_x <= 0.0) obj%dip_inc_x = 10.0
      if (obj%dip_inc_y <= 0.0) obj%dip_inc_y = 10.0

      call pathcheck ('path_base_in', obj%path_base_in, &
                                   required = (obj%method(1:4) == 'FILE'), &
                                   show = PATHCHECK_INFO_INPUT)
      call pathcheck ('path_base_out', obj%path_base_out, &
                                   show = PATHCHECK_INFO_OUTPUT)

!----------return optional arguments:

      if (present(hdr_x )) hdr_x  = obj%hdr_x
      if (present(hdr_y )) hdr_y  = obj%hdr_y
      if (present(x_init)) x_init = obj%x_init
      if (present(y_init)) y_init = obj%y_init
      if (present(x_inc )) x_inc  = obj%x_inc
      if (present(y_inc )) y_inc  = obj%y_inc
      if (present(bins_x)) bins_x = obj%bins_x
      if (present(bins_y)) bins_y = obj%bins_y

!----------write parameters:

      if (obj%procname == 'RTC') then
           call pc_put_options_field &
                     ('method' ,method_rtc_options , method_rtc_nopt)
      else
           call pc_put_options_field &
                     ('method' ,method_ims_options , method_ims_nopt)
      end if

      call pc_put ('freq_beg'     , obj%freq_beg      )
      call pc_put ('freq_end'     , obj%freq_end      )
      call pc_put ('method'       , obj%method        )
      call pc_put ('msctfb'       , obj%msctfb        )
      call pc_put ('win_len'      , obj%win_len       )
      call pc_put ('hdr_x'        , obj%hdr_x         )
      call pc_put ('hdr_y'        , obj%hdr_y         )
      call pc_put ('x_init'       , obj%x_init        )
      call pc_put ('y_init'       , obj%y_init        )
      call pc_put ('x_inc'        , obj%x_inc         )
      call pc_put ('y_inc'        , obj%y_inc         )
      call pc_put ('bins_x'       , obj%bins_x        )
      call pc_put ('bins_y'       , obj%bins_y        )
      call pc_put ('gap_x'        , obj%gap_x         )
      call pc_put ('gap_y'        , obj%gap_y         )
      call pc_put ('bins_init_x'  , obj%bins_init_x   )
      call pc_put ('bins_init_y'  , obj%bins_init_y   )
      call pc_put ('dip_x'        , obj%dip_x         )
      call pc_put ('dip_y'        , obj%dip_y         )
      call pc_put ('dip_inc_x'    , obj%dip_inc_x     )
      call pc_put ('dip_inc_y'    , obj%dip_inc_y     )
      call pc_put ('path_base_in' , obj%path_base_in  )
      call pc_put ('path_base_out', obj%path_base_out )

      do_corr = (obj%method == 'CORR' .or. obj%method == 'FILE_THEN_CORR')
      do_sdip = (obj%method == 'SDIP' .or. obj%method == 'FILE_THEN_SDIP')

      sense_msctfb        = do_corr
      sense_win_len       = do_sdip
      sense_bins_x        = (obj%method /= 'FILE')
      sense_bins_y        = (obj%method /= 'FILE')
      sense_gap_x         = (obj%method /= 'FILE' .and. obj%bins_x > 1)
      sense_gap_y         = (obj%method /= 'FILE' .and. obj%bins_y > 1)
      sense_bins_init_x   = (do_corr              .and. obj%bins_x > 1)
      sense_bins_init_y   = (do_corr              .and. obj%bins_y > 1)
      sense_dip_x         = (obj%method /= 'FILE' .and. obj%bins_x > 1)
      sense_dip_y         = (obj%method /= 'FILE' .and. obj%bins_y > 1)
      sense_dip_inc_x     = (do_sdip              .and. obj%bins_x > 1)
      sense_dip_inc_y     = (do_sdip              .and. obj%bins_y > 1)
      sense_path_base_in  = (obj%method(1:4) == 'FILE')
      sense_path_base_out = (obj%method /= 'FILE' .or. obj%num_iter > 1)

 call pc_put_sensitive_field_flag ('msctfb'              , sense_msctfb)
 call pc_put_sensitive_field_flag ('win_len'             , sense_win_len)
 call pc_put_sensitive_field_flag ('bins_x'              , sense_bins_x)
 call pc_put_sensitive_field_flag ('bins_y'              , sense_bins_y)
 call pc_put_sensitive_field_flag ('gap_x'               , sense_gap_x)
 call pc_put_sensitive_field_flag ('gap_y'               , sense_gap_y)
 call pc_put_sensitive_field_flag ('bins_init_x'         , sense_bins_init_x)
 call pc_put_sensitive_field_flag ('bins_init_y'         , sense_bins_init_y)
 call pc_put_sensitive_field_flag ('dip_x'               , sense_dip_x)
 call pc_put_sensitive_field_flag ('dip_y'               , sense_dip_y)
 call pc_put_sensitive_field_flag ('dip_inc_x'           , sense_dip_inc_x)
 call pc_put_sensitive_field_flag ('dip_inc_y'           , sense_dip_inc_y)
 call pc_put_sensitive_field_flag ('path_base_in'        , sense_path_base_in)
 call pc_put_sensitive_field_flag ('path_base_out'       , sense_path_base_out)
 call pc_put_sensitive_field_flag ('select_path_base_in' , sense_path_base_in)
 call pc_put_sensitive_field_flag ('select_path_base_out', sense_path_base_out)
 call pc_put_sensitive_field_flag ('path_base_in_info'   , sense_path_base_in)
 call pc_put_sensitive_field_flag ('path_base_out_info'  , sense_path_base_out)

      if (obj%path_base_in /= PATHCHECK_EMPTY .and. &
          .not.sense_path_base_in) then
              call pc_info                                                  &
                 ('BASETRACE: PATH_BASE_IN will not be used with METHOD =', &
                  obj%method)
      end if

      if (obj%path_base_out /= PATHCHECK_EMPTY .and. &
          .not.sense_path_base_out) then
              call pc_info                                                   &
                 ('BASETRACE: PATH_BASE_OUT will not be used with METHOD =', &
                  obj%method)
      end if

!----------calculate dependencies:

      call permtfile_close (obj%permtfile_in)
      call permtfile_close (obj%permtfile_out)
      call mem_free (obj%filt)

      obj%nfilt = 0
      obj%iter  = 0
      error = .false.

      if (pc_do_not_process_traces()) return

!----------setup bandpass filter space if desired:
!----------length allows for 30 zero crossings in sinc function of highcut.

      if(obj%freq_beg > 0.0 .or. obj%freq_end < int(0.5/obj%dt)) then
         obj%nfilt = nint(7.5/obj%freq_end/obj%dt)*2 + 1
      else
         obj%nfilt = 0
      end if

!----------build bandpass filter as difference of two sinc functions.

      call mem_alloc (obj%filt, obj%nfilt)

      if(obj%nfilt > 0) then
         midp   = obj%nfilt/2 + 1
         nfilth = obj%nfilt/2
         obj%filt(midp) = obj%freq_end - obj%freq_beg
         tpi = 2.0 * PI
         frqh = obj%dt * obj%freq_end * tpi
         frql = obj%dt * obj%freq_beg * tpi
         do indx = 1,nfilth
           aaa = frqh * indx
           bbb = frql * indx
           ccc = obj%freq_end * sin(aaa)/aaa
           if(bbb /= 0.0) ccc = ccc - obj%freq_beg * sin(bbb)/bbb
           obj%filt(midp + indx) = ccc
           obj%filt(midp - indx) = ccc
         end do
      end if
      return
      end subroutine basetrace_update


!!--------------------- basetrace begin iteration ------------------------!!
!!--------------------- basetrace begin iteration ------------------------!!
!!--------------------- basetrace begin iteration ------------------------!!


      subroutine basetrace_begin_iteration (obj,error)
      implicit none
      type(basetrace_struct),intent(inout) :: obj                ! arguments
      logical               ,intent(out)   :: error              ! local
      integer                              :: err                ! local

      error = .false.
      obj%iter = obj%iter + 1

      if (obj%path_base_in /= PATHCHECK_EMPTY) then
           if (obj%iter == 1 .or. obj%method == 'FILE') then
           if (obj%method(1:4) == 'FILE') then
                call permtfile_open_read                          &
                             (obj%permtfile_in,obj%path_base_in,  &
                              obj%nwih,obj%ndpt,obj%tstrt,obj%dt, &
     !                        pc_get_lun(),err)
                              0,err)
                if (err /= PERMTFILE_OK) then
                     call pc_error &
                       ('BASETRACE: error opening input base trace file')
                     error = .true.
     !          else
     !               call pc_info &
     !                 ('BASETRACE: successfully opened input base trace file')
                end if
           end if
           end if
      end if

      if (obj%path_base_out /= PATHCHECK_EMPTY) then
           if (obj%iter > 1 .or. obj%method(1:4) /= 'FILE') then
                call permtfile_open_write                          &
                             (obj%permtfile_out,obj%path_base_out, &
                              obj%nwih,obj%ndpt,obj%tstrt,obj%dt,  &
     !                        pc_get_lun(),err,0)
                              0,err)
                if (err /= PERMTFILE_OK) then
                     call pc_error &
                            ('BASETRACE: error opening output base trace file')
                     error = .true.
     !          else
     !               call pc_info &
     !                ('BASETRACE: successfully opened output base trace file')
                end if
           end if
      end if

      if (error) then
           call pc_error ('BASETRACE: ERROR BEFORE ITERATION',obj%iter)
           call pc_error (obj%errmsg)
      end if
      return
      end subroutine basetrace_begin_iteration


!!--------------------- basetrace end iteration ------------------------!!
!!--------------------- basetrace end iteration ------------------------!!
!!--------------------- basetrace end iteration ------------------------!!


      subroutine basetrace_end_iteration (obj,error)
      implicit none
      type(basetrace_struct),intent(inout) :: obj                ! arguments
      logical               ,intent(out)   :: error              ! local
      integer                              :: err                ! local
      double precision                     :: hdum(obj%nwih)     ! local
      real                                 :: tdum(obj%ndpt)     ! local

      if (associated(obj%permtfile_in)) then
           call permtfile_read (obj%permtfile_in,hdum,tdum,err)
           if (err == PERMTFILE_OK) then
                call pc_warning &
                   ('BASETRACE: excess traces found on input base trace file')
           else if (err == PERMTFILE_ERROR) then
                call pc_warning &
                   ('BASETRACE: error reading EOF from input base trace file')
           end if
      end if

      call permtfile_close (obj%permtfile_in)
      call permtfile_close (obj%permtfile_out)
      error = .false.
      return
      end subroutine basetrace_end_iteration


!!------------------------ basetrace build --------------------------------!!
!!------------------------ basetrace build --------------------------------!!
!!------------------------ basetrace build --------------------------------!!


      subroutine basetrace_build &
                     (obj,hdmix,trmix,ntr,mid,twin,nwt,hdbase,trbase,error)
      implicit none
      type(basetrace_struct),intent(inout) :: obj                ! arguments
      double precision      ,intent(inout) :: hdmix(:,:)         ! arguments
      real                  ,intent(inout) :: trmix(:,:)         ! arguments
      integer               ,intent(in)    :: ntr,mid            ! arguments
      real                  ,intent(in)    :: twin(:,:)          ! arguments
      integer               ,intent(in)    :: nwt                ! arguments
      double precision      ,intent(inout) :: hdbase(:)          ! arguments
      real                  ,intent(out)   :: trbase(:)          ! arguments
      logical               ,intent(out)   :: error              ! arguments
      integer                              :: err,indx,midp      ! local
      double precision                     :: hdum(obj%nwih)     ! local
      real                        :: tdum(obj%ndpt + obj%nfilt)  ! local

!!!!!!!!!! put integerized coordinates into trace headers:

      do indx = 1,ntr
           hdmix(SCRATCH_NHX,indx) = &
                nint((hdmix(obj%hdr_x,indx) - obj%x_init) / obj%x_inc)
           hdmix(SCRATCH_NHY,indx) = &
                nint((hdmix(obj%hdr_y,indx) - obj%y_init) / obj%y_inc)
      end do

      hdbase(SCRATCH_NHX) = &
                nint((hdbase(obj%hdr_x) - obj%x_init) / obj%x_inc)
      hdbase(SCRATCH_NHY) = &
                nint((hdbase(obj%hdr_y) - obj%y_init) / obj%y_inc)

!!!!!!!!!! set flags in trace headers:

      call cordip_set_flags                                             &
                   (ntr, hdmix, mid,                                    &
                    SCRATCH_NHX, SCRATCH_NHY, SCRATCH_NHW, SCRATCH_NHB, &
                    obj%bins_x, obj%bins_y,                             &
                    obj%gap_x, obj%gap_y,                               &
                    obj%bins_init_x, obj%bins_init_y)

!!!!!!!!!! calculate the base trace:

      if (associated(obj%permtfile_in)) then

        call permtfile_read (obj%permtfile_in,hdum,trbase,err)
        if (err == PERMTFILE_EOF) then
             call pc_error ('BASETRACE: not enough traces on base trace file')
             call pc_error ('BASETRACE: ERROR DURING ITERATION',obj%iter)
             call pc_error (obj%errmsg)
             call permtfile_close (obj%permtfile_in)
             error = .true.
             return
        else if (err == PERMTFILE_ERROR) then
             call pc_error &
                      ('BASETRACE: error reading trace from base trace file')
             call pc_error ('BASETRACE: ERROR DURING ITERATION',obj%iter)
             call pc_error (obj%errmsg)
             call permtfile_close (obj%permtfile_in)
             error = .true.
             return
        end if

      else if (obj%method == 'CORR' .or. obj%method == 'FILE_THEN_CORR') then

        call cordip (ntr, hdmix, trmix, obj%ndpt, obj%tstrt, obj%dt,     &
                     SCRATCH_NHX, SCRATCH_NHY, SCRATCH_NHW, SCRATCH_NHB, &
                     obj%msctfb,                                         &
                     obj%dip_x, obj%dip_y,                               &
                     twin, nwt, hdbase, trbase)

      else ! if (obj%method == 'SDIP' .or. obj%method == 'FILE_THEN_SDIP') then

        call semdip (ntr, hdmix, trmix, obj%ndpt, obj%dt, obj%win_len,  &
                     SCRATCH_NHX, SCRATCH_NHY, SCRATCH_NHW,             &
                     obj%dip_x, obj%dip_y,                              &
                     obj%dip_inc_x, obj%dip_inc_y,                      &
                     hdbase, trbase)

      end if

!!!!!!!!!! save amplitude-normalized version of base trace to file:

      if (associated(obj%permtfile_out)) then
           call mth_amplitude_normalize (trbase,tdum,obj%ndpt)
           call permtfile_write (obj%permtfile_out,hdbase,tdum,err)
           if (err /= PERMTFILE_OK) then
                call pc_error &
                      ('BASETRACE: error writing trace to base trace file')
                call pc_error ('BASETRACE: ERROR DURING ITERATION',obj%iter)
                call pc_error (obj%errmsg)
                call permtfile_close (obj%permtfile_out)
                error = .true.
                return
           end if
      end if

!!!!!!!!!! filter the base trace before returning it:

      if(obj%nfilt > 0) then
         midp = obj%nfilt/2
         do indx = 1,obj%ndpt + obj%nfilt - 1
              tdum(indx) = 0.0
         end do
         do indx = 1,obj%ndpt
              tdum(indx + midp) = trbase(indx)
         end do
         call fltr_filterg &
                (obj%filt, obj%nfilt, tdum, obj%ndpt + obj%nfilt - 1, trbase)
      end if

!!!!!!!!!! finish up and return:

      error = .false.
      return
      end subroutine basetrace_build


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module basetrace_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

