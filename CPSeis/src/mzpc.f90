!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by S Cook on 2000-12-12. />

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
!                         C P S   P R O C E S S                  
!
! Name       : MZPC      (Minimum or Zero Phase Conversion)
! Category   : filters
! Written    : 1989-04-27   by: Bill Troutt
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Perform minimum or zero phase conversion of embedded signatures.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! MZPC will convert an embedded wavelet to minimum phase (MODE = MPC) or
! to zero phase (MODE = ZPC).  No assumption is made as to the phase of the 
! embedded wavelet in the input data.  A recorded (or extracted or modeled)
! signature is required.
!
!
! Minimum Phase Operation
!
! For Minimum phase conversion, each input trace is correlated with an operator
! that is formed by performing minimum-phase spiking deconvolution on the 
! recorded signature.  This replaces the embedded wavelet with its minimum 
! phase version.  (The reason this trick works is explained in the Algorithm
! Description Section).
!
!
! Zero Phase Operation
!
! For zero phase conversion, MZPC correlates the input traces with the recorded
! signature.  This replaces the embedded wavelet with its autocorrelation, 
! which is zero phase.
! 
!
! Signature Locations.
!
! If OPT_LOC = FILE, then a single signature is read from a TROT file and one 
! signature is used for conversion of all the input traces.  Input need not be
! gathered.
!
! If OPT_LOC = CHANNEL, then the signatures for each input gather are written 
! over one or more consecutive traces in each input gather.  The average of the
! signatures in each gather are used for conversion of the traces in that 
! gather.  Input must be gathered.
!
! If OPT_LOC = SWEEP, then the signature for each input gather is written on 
! one initial auxiliary trace in each input gather.  Input must be gathered.
!
!
! Signature Centering
!
! The location of zero-time for a signature determines whether a bulk shift 
! occurs when that signature is used for conversion.  Typically the bulk shift 
! can be minimized if the zero-time for the signature is associated with a 
! central high amplitude sample of the signature.  
!
! The TIM_FIRST parameter allows the signature to be centered so that MZPC will
! not cause a bulk shift.  Normally TIM_FIRST will be approximately
! -0.5 * length of signature.  When OPT_LOC=CHANNEL or OPT_LOC=SWEEP, it's up to
! you to specify an appropriate TIM_FIRST.  When OPT_LOC=FILE, the front end
! will initially set TIM_FIRST to match the TMIN value in the file (if the file
! exists when you build the job), but you may override that value.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Note on Amplitude Spectrum
!
! The purpose of MZPC is to replace the embedded wavelet with its minimum phase 
! or zero phase version.  MZPC may change the embedded wavelet's amplitude 
! spectrum, but it makes no effort to whiten the amplitude spectrum.  Normally
! the MPC mode will be followed, in the process sequence, by minimum phase 
! decon, while the ZPC mode will be followed by zero phase decon.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process if OPT_LOC = FILE.
! Process is a multiple-trace process if OPT_LOC /= FILE.
!
! This process requires traces to be input in gathers if OPT_LOC /= FILE.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
!
! This process outputs the same traces as it receives (possibly altered).
!
! Optionally, auxiliary traces are deleted.
!
! This process outputs traces with the same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! GATHERED  whether traces are a legitimate gather  used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                  Action taken
! ----    -----------                  ------------
!
! 1       Sequential Trace Count       Renumbered.
! 4       Trace number within gather   Renumbered if traces deleted.
! 25      LAV                          Reset
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY                    
! 
!     Date       Author      Description
!     ----       ------      -----------
!017. 2006-06-20  B. Menger   Removed Unused Variables.
! 16. 2001-02-15 Cook        Changed wrapped_up to skip_wrapup.
! 15. 2000-09-27 Cook        TIM_FIRST problem was fixed properly with last
!                             update, but user requested that there be even more
!                             similar in behavior to the DSIG GUI.  User now
!                             sees TIM_FIRST with same sign as in the disk file
!                             (only applies to FILE mode).  Also, some GUI
!                             elements now desensitized when in FILE mode.
! 14. 2000-09-15 Cook        When reading tim_first from file, sign was wrong.
! 13. 2000-09-05 Cook        Removed rcp support.  Changed to take tim_first
!                            value from the signature input file (TSTRT) if
!                            available (user can override).
! 12. 2000-08-10 Cook        (MAJOR) Converted to new CPS system.
! 11. 1998-12-14 Goodger     Begin using the fortran90 compiler.
! 10. 1995-11-13 Vunderink   Changed to calculate the number of words
!                            needed to save the process common block
! 9.  1992-10-03 Troutt      Remove code in setup that tried to RELEASE 
!                            (COS) or "rm" (UNICOS) FILE after read. There
!                            is no problem in UNICOS with multiple jobs
!                            reading the same input file.
! 8.  1992-06-04 Peterson    MZPCSCL, Check for possible divide by zero,
!                            then set divisor equal to 1.0 .
! 7.  1991-02-12 Peterson    UNICOS upgrades. RELEASE or ISHELL('rm
! 6.  1991-01-29 Troutt      For MODE=FILE, added option to process only  
!                            flagged traces (HF#).
! 5.  1990-11-16 Troutt      For MODE=FILE, call RELEASE after reading the
!                            file at setup time (so other jobs can use it)
! 4.  1990-10-31 Troutt      For MODE=FILE, call STREND after reading the
!                            file at setup time.  Also added Note about
!                            time shifting of results.
! 3.  1990-04-25 Howard      Make compatible with STRIN change.
! 2.  1989-08-21 Troutt      Add NWIN words to scratch ahead of existing
!                            arrays to circumvent OPERAND RANGE ERRORS in
!                            FILTGS.
! 1.  1989-04-27 Troutt      Original conversion from CONSEIS. 
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

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
! 
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS          
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
! Minimum Phase Algorithm
!
! When the signature has spiking decon applied to it the result has the phase 
! of the original signature minus the minimum phase associated with the 
! signature's amplitude spectrum.  Correlating is the same as convolving with a
! sign change of the phase.  So correlating traces with the deconned signature 
! adds the minimum phase minus the original signature phase to the trace phase,
! thus replacing the embedded wavelet phase with its minimum phase version.
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! Scratch calculations not included during conversion to new CPS system.
!
! Take care not to confuse similarly-named variables like TIM_SIG_BEG and
! TR_SIG_BEG.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS MZPC Process/NC=80>
!
!  Perform minimum or zero phase conversion of embedded signatures.
!
!
!  MODE=`CC
!
!
!  OPT_LOC=`CCCCCC
!
!
!  TIM_SIG_BEG= `FFFFFFFFFFF TIM_SIG_END= `FFFFFFFFFFF TIM_FIRST= `FFFFFFFFFFF
!
!
!  NUM_DEL= `IIIIIIII
!
!
!  For OPT_LOC == FILE:
!  HDR_FLAG= `IIIIII
!  PATHNAME= `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!
! For OPT_LOC == CHANNEL or SWEEP:
!  TR_SIG_BEG= `IIIIIIIIII
!  TR_SIG_END= `IIIIIIIIII
!
!
!<PARMS PATHNAME[/ML=120/XST]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to perfom minimum or zero phase conversion. </Tip>
! Default = MPC
! Allowed = MPC   (Perform minimum phase conversion.)
! Allowed = ZPC   (Perform zero phase conversion.)
!</Help>
!
!<Help KEYWORD="OPT_LOC">
!<Tip> Location of the recorded signature. </Tip>
! Default = FILE
! Allowed = FILE
! Allowed = CHANNEL
! Allowed = SWEEP
! If OPT_LOC = FILE, then a single signature is read from a TROT file and one 
! signature is used for conversion of all the input traces.  Input need not be
! gathered.
!
! If OPT_LOC = CHANNEL, then the signatures for each input gather are written 
! over one or more consecutive traces in each input gather.  The average of the
! signatures in each gather are used for conversion of the traces in that 
! gather.  Input must be gathered.
!
! If OPT_LOC = SWEEP, then the signature for each input gather is written on 
! one initial auxilliary trace in each input gather.  Input must be gathered.
!</Help>
!
!<Help KEYWORD="TIM_SIG_BEG">
!<Tip> Time in trace for start of signature, in seconds. </Tip>
! Default = TSTRT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_SIG_END">
!<Tip> Time in trace for end of signature, in seconds. </Tip>
! Default = TSTRT + 0.5
! Allowed = real > TIM_SIG_BEG
!</Help>
!
!<Help KEYWORD="TIM_FIRST">
!<Tip> Time of first sample in signature, in seconds. </Tip>
! Default = 0.0
! Allowed = real
! Time of first sample in signature is used for proper centering of the
! signature.  TIM_FIRST can be adjusted so that the conversion produces no bulk
! shift.  Normally TIM_FIRST will be approximately -0.5 * length of signature.
!</Help>
!
!<Help KEYWORD="NUM_DEL">
!<Tip> Delete NUM_DEL traces at the start of an input trace gather. </Tip>
! Default = 0
! Allowed = int >= 0
! Number of traces at the start of an input trace gather to delete.
!</Help>
!
!---------------------parameters for OPT_LOC = FILE-----------------------------
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces if OPT_LOC = FILE. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are converted.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are converted when OPT_LOC = FILE.  
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for file containing the recorded signature. </Tip>
! Default = NONE
! Allowed = char
! MZPC reads only one trace from this file, and uses it as the signature.  Any
! file format readable by TRIN is also readable by MZPC.  The DT global in this
! file must match DT in the current job.  The TMIN value in the file is used for
! setting an initial default for TIM_FIRST, although you may override that
! TIM_FIRST value when setting parameters for MZPC.
!</Help>
!
!-----------------parameters for OPT_LOC = CHANNEL or SWEEP---------------------
!
!<Help KEYWORD="TR_SIG_BEG">
!<Tip> Sequential number of first trace in gather containing a signature. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="TR_SIG_END">
!<Tip> Sequential number of last trace in gather containing a signature. </Tip>
! Default = 1
! Allowed = int>=TR_SIG_BEG
! If OPT_LOC = SWEEP, TR_SIG_END must be set equal to TR_SIG_BEG.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module mzpc_module

      use pc_module
      use mem_module
      use named_constants_module
      use pathcheck_module
      use cio_module

!      use finquire_module
!      use rcpfile_module
!      use tempname_module

      use ameq_module
      use fltr_module
      use lav_module
      use mth_module
      use opfilt_module
      use trcio_module

      implicit none
      private
      public :: mzpc_create
      public :: mzpc_initialize
      public :: mzpc_update
      public :: mzpc_delete
!<execute_only>
      public :: mzpc            ! main execution (trace processing) routine.
      public :: mzpc_wrapup
!</execute_only>

      character(len=100),public,save :: MZPC_IDENT = &
       '$Id: mzpc.f90,v 1.17 2006/06/20 13:12:01 Menger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer,parameter     :: MODE_NCHR         = 3
      integer,parameter     :: OPT_LOC_NCHR      = 7

      integer,parameter     :: MODE_NOPTIONS     = 2
      integer,parameter     :: OPT_LOC_NOPTIONS  = 3

      type,public :: mzpc_struct              
        private

        logical                    :: skip_wrapup     ! wrapup flag.

        logical                    :: gathered        ! global
        integer                    :: numtr           ! global
        integer                    :: nwih,ndpt       ! global.  
        real                       :: dt              ! global.

        character(len=MODE_NCHR)    :: mode           ! MPC, ZPC
        character(len=OPT_LOC_NCHR) :: opt_loc        ! FILE, CHANNEL, SWEEP

        character(len=FILENAME_LENGTH) :: pathname    ! if FILE
        integer                    :: hdr_flag        ! if FILE

        integer                    :: num_del         ! process parameters.

        real                       :: twin            ! process parameters.
        real                       :: bwin            ! process parameters.
        real                       :: tim_first       ! process parameters.

        integer                    :: tr_sig_beg      ! process parameters.
        integer                    :: tr_sig_end      ! process parameters.

        integer                    :: nwin,i1,ishft
        integer                    :: ntfl,ntout,ngrp

        double precision, pointer  :: hd_sigfile(:)
        real,pointer               :: tr_sigfile(:)

        real,pointer               :: scr_ndpt(:),wave(:),oper(:)
        real,pointer               :: scr2(:),cros(:),auto(:),siginv(:),sig(:)

      end type mzpc_struct


!!----------------------------- interfaces ---------------------------------!!
!!----------------------------- interfaces ---------------------------------!!
!!----------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(mzpc_struct),pointer,save :: object      ! needed for traps.

      character(len=FILENAME_LENGTH) :: pathname_prev  ! for checking in trap.

      character(len=MODE_NCHR),save :: mode_options(MODE_NOPTIONS)
      data mode_options/'MPC','ZPC'/

      character(len=OPT_LOC_NCHR),save :: opt_loc_options(OPT_LOC_NOPTIONS)
      data opt_loc_options/'FILE','CHANNEL','SWEEP'/


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine mzpc_create (obj)
      implicit none
      type(mzpc_struct),pointer :: obj       ! arguments

!-----allocate obj and nullify all pointers.

      allocate (obj)

      nullify (obj%hd_sigfile)
      nullify (obj%tr_sigfile)
      nullify (obj%scr_ndpt)
      nullify (obj%wave)
      nullify (obj%scr2)
      nullify (obj%oper)
      nullify (obj%cros)
      nullify (obj%auto)
      nullify (obj%siginv)
      nullify (obj%sig)

      call mzpc_initialize (obj)
      return
      end subroutine mzpc_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine mzpc_delete (obj)
      implicit none
      type(mzpc_struct),pointer :: obj       ! arguments

!<execute_only>
      call mzpc_wrapup (obj)
!</execute_only>

      call mem_free (obj%hd_sigfile)
      call mem_free (obj%tr_sigfile)
      call mem_free (obj%scr_ndpt)
      call mem_free (obj%wave)
      call mem_free (obj%scr2)
      call mem_free (obj%oper)
      call mem_free (obj%cros)
      call mem_free (obj%auto)
      call mem_free (obj%siginv)
      call mem_free (obj%sig)

      deallocate(obj)
      return
      end subroutine mzpc_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine mzpc_initialize (obj)
      implicit none
      type(mzpc_struct),intent(inout) :: obj       ! arguments

      real                            :: tstrt     ! local


! tstrt is needed to properly set defaults
      call pc_get_global ('tstrt', tstrt)
      obj%twin        = tstrt
      obj%bwin        = tstrt + .5
      obj%tim_first   = 0.0

      obj%gathered    = .false.
      obj%numtr       = 0
      obj%nwih        = 0
      obj%ndpt        = 0
      obj%dt          = 0

      obj%mode        = 'MPC'
      obj%opt_loc     = 'FILE'

      obj%hdr_flag    = 0
      obj%pathname    = PATHCHECK_EMPTY

      obj%num_del     = 0

      obj%tr_sig_beg  = 1
      obj%tr_sig_end  = 1

      obj%ishft       = 0

      obj%ntfl        = 0
      obj%ntout       = 0
      obj%ngrp        = 0


      call mzpc_update (obj)
      return
      end subroutine mzpc_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine mzpc_update (obj)
      implicit none
      type(mzpc_struct),intent(inout),target :: obj             ! arguments

      integer                         ::   istat ! local


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
      call pc_get_global ('numtr'   , obj%numtr)    ! max traces
      call pc_get_global ('gathered', obj%gathered) ! true if gathered
      call pc_get_global ('nwih'    , obj%nwih)     ! number of header words
      call pc_get_global ('ndpt'    , obj%ndpt)     ! number of trace samples
      call pc_get_global ('dt'      , obj%dt)       ! sample rate

      pathname_prev = obj%pathname

      call pc_get ('mode'    , obj%mode)
      call pc_get ('opt_loc' , obj%opt_loc)

      call pc_get ('hdr_flag', obj%hdr_flag)
      call pc_get ('pathname', obj%pathname, mzpc_pathname_trap)

      call pc_get ('num_del' , obj%num_del)

      call pc_get ('tim_sig_beg', obj%twin)
      call pc_get ('tim_sig_end', obj%bwin)
      call pc_get ('tim_first'  , obj%tim_first)

      call pc_get ('tr_sig_beg', obj%tr_sig_beg)
      call pc_get ('tr_sig_end', obj%tr_sig_end)

!
!-----check input parameters, initialize variables and tables here.
!
!-----check for min or zero phase conversion mode.  set logical variable.
      if (obj%mode /= 'MPC' .and. obj%mode /= 'ZPC') then 
        call pc_error ('MODE must be set to MPC or ZPC')
      end if 


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (obj%opt_loc == 'FILE') then
        call pc_put_sensitive_field_flag ('pathname'    , .true.)
        call pc_put_sensitive_field_flag ('tr_sig_beg'  , .false.)
        call pc_put_sensitive_field_flag ('tr_sig_end'  , .false.)
        call pc_put_sensitive_field_flag ('tim_sig_beg' , .false.)
        call pc_put_sensitive_field_flag ('tim_sig_end' , .false.)
      else
        call pc_put_sensitive_field_flag ('pathname'    , .false.)
        call pc_put_sensitive_field_flag ('tr_sig_beg'  , .true.)
        call pc_put_sensitive_field_flag ('tr_sig_end'  , .true.)
        call pc_put_sensitive_field_flag ('tim_sig_beg' , .true.)
        call pc_put_sensitive_field_flag ('tim_sig_end' , .true.)
      end if


      if(pc_verify_screen('screen1')) call mzpc_verify_screen()


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
      call pc_put_options_field &
        ('mode'    , mode_options    , mode_noptions)
      call pc_put_options_field &
        ('opt_loc', opt_loc_options, opt_loc_noptions)

      call pc_put ('mode'    , obj%mode)
      call pc_put ('opt_loc' , obj%opt_loc)

      call pc_put ('hdr_flag' ,obj%hdr_flag)
      call pc_put ('pathname', obj%pathname)

      call pc_put ('num_del' , obj%num_del)

      call pc_put ('tim_sig_beg', obj%twin)
      call pc_put ('tim_sig_end', obj%bwin)
      call pc_put ('tim_first'  , obj%tim_first)

      call pc_put ('tr_sig_beg', obj%tr_sig_beg)
      call pc_put ('tr_sig_end', obj%tr_sig_end)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free (obj%hd_sigfile)
      call mem_free (obj%tr_sigfile)

      call mem_free (obj%scr_ndpt)
      call mem_free (obj%wave)
      call mem_free (obj%scr2)
      call mem_free (obj%oper)
      call mem_free (obj%auto)
      call mem_free (obj%cros)
      call mem_free (obj%sig)
      call mem_free (obj%siginv)

!<execute_only>


      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!      if (pc_do_not_process_traces()) return

      allocate(obj%hd_sigfile(obj%nwih), stat=istat)
      if(istat /= 0) then
        call pc_error ('Error allocating HD_SIGFILE array')
        return
      end if

      allocate(obj%tr_sigfile(obj%ndpt), stat=istat)
      if(istat /= 0) then
        call pc_error ('Error allocating TR_SIGFILE array')
        return
      end if

!---- calculate nwin and allocate accordingly
      obj%i1 = nint(obj%twin/obj%dt + 1.0)
      obj%i1 = max0(1,obj%i1) 

      obj%nwin = nint((obj%bwin - obj%twin)/obj%dt + 1.0)
      if (mod(obj%nwin,2) == 0) obj%nwin = obj%nwin - 1      !force odd 

      call mem_alloc(obj%scr_ndpt,obj%ndpt)
      call mem_alloc(obj%wave    ,obj%nwin)
      call mem_alloc(obj%scr2    ,2*obj%nwin)
      call mem_alloc(obj%oper    ,obj%nwin)
      call mem_alloc(obj%auto    ,obj%nwin)
      call mem_alloc(obj%cros    ,obj%nwin)
      call mem_alloc(obj%sig     ,obj%nwin)
      call mem_alloc(obj%siginv  ,obj%nwin)

!-------------------------------------------------------------------------------
      if (pc_do_not_process_traces()) return   ! in case of allocation errors.
!-------------------------------------------------------------------------------

      obj%ishft = (obj%twin - obj%tim_first)/obj%dt

!---- need signature file for FILE mode
      if (obj%opt_loc == 'FILE') then 
        obj%ishft = obj%tim_first/obj%dt

        obj%sig(:obj%nwin) = 0.0
        call pc_print('Reading signature from ',obj%pathname)
        call mzpc_read_signature(obj,obj%pathname)

!------ put signature in scr1

        obj%wave(:obj%nwin) = obj%sig(obj%i1 : obj%nwin - 1 + obj%i1)

        if (obj%mode == 'MPC') then 
          call pc_print('Calculating operator (MPC mode)')
          call mzpc_mzpcdcn (obj%nwin, &
            obj%wave, obj%auto, obj%cros, obj%siginv, obj%scr2, obj%oper) 
        else 
          call pc_print('Calculating operator (ZPC mode)')
          call mzpc_mzpcscl (obj%nwin, obj%wave, obj%oper, istat) 
        end if 

      end if

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine mzpc_update

!-------------------------------------------------------------------------------
! Subroutine to read signature file (trot file)
!-------------------------------------------------------------------------------
      subroutine mzpc_read_signature(obj,filename)
      implicit none
      type(mzpc_struct)              :: obj       ! argument
      character(len=FILENAME_LENGTH) :: filename

      type(trcio_struct),pointer     :: sigfile             ! local
      integer                        :: istat               ! local

      nullify(sigfile);

!---- open as read-only scratch file
      sigfile => trcio_open(filename,'r',.false.)

!---- read it

      istat = trcio_read_trace(sigfile, obj%hd_sigfile, obj%tr_sigfile, 1)

      obj%sig = obj%tr_sigfile

!---- close it
      istat = trcio_close(sigfile,.false.)

      return
      end subroutine mzpc_read_signature

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!-------------------------------------------------------------------------------
! Subroutine to check all parameters not trapped elsewhere.
!-------------------------------------------------------------------------------
      subroutine mzpc_pathname_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword           ! arguments

      integer                       :: status            ! local
      type(trcio_struct),pointer    :: trcfile           ! local

      if (object%opt_loc /= 'FILE') then
        object%pathname = PATHCHECK_EMPTY
      else
        call pathcheck ('PATHNAME', object%pathname, status=status)
        if (status /= PATHCHECK_VALID) then
          call pc_error('Invalid syntax:  "', object%pathname, '"')
          return
        end if
        if (object%pathname == pathname_prev) return
        if (pc_get_update_state() == PC_BACKEND) return
        trcfile => trcio_open (object%pathname, 'r')
        if (.not.associated(trcfile)) then
          call pc_warning("Your signature file doesn't seem to exist yet.&
             &  Be sure its name is right and TIM_FIRST is set correctly.")
          return
        end if
        if (ameq(trcfile%dt, object%dt, 0.0001*object%dt)) then
          object%tim_first = trcfile%tmin
          call pc_info('TIM_FIRST set to signature file TMIN = ',  &
            trcfile%tmin, ' but you may override this value if necessary.')
        else
          call pc_error('DT in signature file = ', trcfile%dt, &
            ' differs from current DT global = ', object%dt)
          object%pathname = PATHCHECK_EMPTY
        end if
        status = trcio_close (trcfile)
      end if
      return
      end subroutine mzpc_pathname_trap

!-------------------------------------------------------------------------------
! Subroutine to check all parameters not trapped elsewhere.
!-------------------------------------------------------------------------------
      subroutine mzpc_verify_screen()
      implicit none

! gathered data sometimes required:
      if(object%opt_loc == 'CHANNEL') then
        if(.not. object%gathered) &
          call pc_error ('Gathered data needed when OPT_LOC == CHANNEL')
          return
      end if
      if(object%opt_loc == 'SWEEP') then
        if(.not. object%gathered) &
          call pc_error ('Gathered data needed when OPT_LOC == SWEEP')
          return
      end if

! FILE mode implies:
      if(object%opt_loc == 'FILE') then
        if(object%hdr_flag < 0 .or. object%hdr_flag > object%nwih) then 
          call pc_error ('HDR_FLAG not in range 0 to ', object%nwih)
          return
        end if

      end if

! sensible time ranges required
      if(object%twin == FNIL .or. object%bwin == FNIL) then
        call pc_error ('NIL value for TIM_SIG_BEG, TIM_SIG_END, or TIM_FIRST.')
        return
      end if

      if(object%twin >= object%bwin) then
        call pc_error ('TIM_SIG_BEG must be less than TIM_SIG_END.')
        return
      end if

      if(object%tim_first == FNIL) then
        call pc_error ('TIM_FIRST cannot be NIL.')
        return
      end if

! special check for SWEEP mode
      if(object%opt_loc == 'SWEEP') then
        if(object%tr_sig_beg /= object%tr_sig_end) then
          call pc_error ('For SWEEP mode, TR_SIG_BEG must equal TR_SIG_END.')
          return
        end if
      end if

! sensible traces must define signature location
      if(object%tr_sig_beg < 1) then
        call pc_error ('TR_SIG_BEG must be greater than 0.')
        return
      end if

      if(object%tr_sig_end < object%tr_sig_beg) then
        call pc_error ( &
          'TR_SIG_END must be greater than or equal to TR_SIG_BEG.')
        return
      end if

      return
      end subroutine mzpc_verify_screen


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine mzpc (obj,ntr,hd,tr)
      implicit none
      type(mzpc_struct),pointer :: obj               ! arguments
      integer          ,intent(inout) :: ntr         ! arguments
      double precision ,intent(inout) :: hd(:,:)     ! arguments
      real             ,intent(inout) :: tr(:,:)     ! arguments

      integer                         :: i,j,istat

!-------------------------------------------------------------------------------
! Bypass entirely if no more traces.
!-------------------------------------------------------------------------------
      if (ntr == NO_MORE_TRACES) go to 1000

      if(ntr - obj%num_del < 0) then
        call pc_error('Gather too small.')
        go to 999
      end if

!-------------------------------------------------------------------------------
! Signature for opt_loc==FILE is obtained in the update routine.
!-------------------------------------------------------------------------------
      if(obj%opt_loc /= 'FILE') then
        obj%ngrp = obj%ngrp + 1 
        obj%hdr_flag = 0               ! precaution
      else
        obj%tr_sig_beg = 0             ! precaution
        obj%tr_sig_end = 0             ! precaution
      end if

!-------------------------------------------------------------------------------
! Signature for opt_loc==SWEEP is obtained here.  No averaging required.
!-------------------------------------------------------------------------------
      if (obj%opt_loc == 'SWEEP') then 

        obj%wave(:obj%nwin) = tr(obj%i1 : obj%nwin-1+obj%i1, obj%tr_sig_beg) 

      end if 

!-------------------------------------------------------------------------------
! Signature for opt_loc==CHANNEL is obtained here.  Averaging may be required.
!-------------------------------------------------------------------------------
      if (obj%opt_loc == 'CHANNEL') then 

        do i = obj%tr_sig_beg, obj%tr_sig_end
          obj%wave(:obj%nwin) = &
            obj%wave(:obj%nwin) + tr(obj%i1:obj%nwin-1+obj%i1, i)
        end do

      end if 

!-------------------------------------------------------------------------------
! Compute operator from signature (opt_loc /= FILE).
!-------------------------------------------------------------------------------
      if(obj%opt_loc /= 'FILE') then

        if (obj%mode == 'MPC') then 
          call mzpc_mzpcdcn (obj%nwin, &
            obj%wave, obj%auto, obj%cros, obj%siginv, obj%scr2, obj%oper) 
        else 
          call mzpc_mzpcscl (obj%nwin, obj%wave, obj%oper, istat) 
        end if 

      end if

!-------------------------------------------------------------------------------
! DO loop to actually perform the correlation.
!-------------------------------------------------------------------------------
      do i = 1, ntr

!------ don't alter the signature traces
        if(i >= obj%tr_sig_beg .and. i <= obj%tr_sig_end) then
          cycle
        end if

!------ filter flagged traces only if flagging turned on (opt_loc==FILE only)
        if (obj%hdr_flag /= 0) then
          if (hd(obj%hdr_flag,i) == 0.0) cycle  
        end if 

        obj%ntfl = obj%ntfl + 1 

!------ need scratch copy of input trace
        obj%scr_ndpt(:obj%ndpt) = tr(:obj%ndpt,i)
!        tr(:obj%ndpt,i) = 0

!------ tr array is overwritten directly with this call:
        call fltr_filtrgs ( &
          obj%oper, obj%nwin, &
          obj%scr_ndpt, obj%ndpt, &
          tr(:obj%ndpt,i), obj%ndpt, &
          1, obj%ishft) 

      end do 

!-------------------------------------------------------------------------------
! Finally, take care of trace deletions and header value renumbering.  This
! loop is needed even if NUM_DEL is zero.
!-------------------------------------------------------------------------------
      j = 1

      do i=obj%num_del+1,ntr

!------ update seq trace number 
        obj%ntout = obj%ntout + 1
        hd(HDR_SEQUENCE,j) = obj%ntout

!------ update channel number
        hd(HDR_CURRENT_CHANNEL,j) = j

!------ move arrays
        if(i /= j) then
          hd(:obj%nwih,j) = hd(:obj%nwih,i) 
          tr(:obj%ndpt,j) = tr(:obj%ndpt,i) 
        end if

        j = j + 1

      end do

      ntr = ntr - obj%num_del

!-------------------------------------------------------------------------------
! Reset LAV, set ntr properly, and return.
!-------------------------------------------------------------------------------
      if(ntr<=0) then
        ntr = NEED_TRACES
      else
        call lav_set_hdr(hd, tr, obj%ndpt, ntr)
      end if

      return

!-------------------------------------------------------------------------------
! Fatal errors and ntr==NO_MORE_TRACES come here.
!-------------------------------------------------------------------------------
  999 ntr = FATAL_ERROR
 1000 call mzpc_wrapup (obj)

      return
      end subroutine mzpc

!-------------------------------------------------------------------------------
! Subroutine to normalize the signature for cross-correlation.
!-------------------------------------------------------------------------------
      subroutine mzpc_mzpcscl(nwin, sig, oper, istat)
      implicit none
      integer              :: nwin         ! # points in signature
      real                 :: sig(nwin)    ! input signature
      real, intent(out)    :: oper(nwin)   ! output operator
      integer, intent(out) :: istat        ! return status

! local
      integer :: iscale   
      real    :: scale 

      istat = 0

      iscale = mth_isamax(nwin,sig,1) 

      if (abs(sig(iscale)) == 0.0) then
        istat = 1
        call pc_error ('Signature is zero; cannot normalize.')
        call pc_error ('Correlation operator will be zeroed.')
        scale = 1.0 
      else 
        scale = 1./abs(sig(iscale)) 
      end if 

      oper(:nwin) = sig(:nwin)*scale 

      return
      end subroutine mzpc_mzpcscl 

!-------------------------------------------------------------------------------
! Subroutine to decon the signature.
!-------------------------------------------------------------------------------
      subroutine mzpc_mzpcdcn(nwin, sig, auto, cros, siginv, scr2, oper)
      implicit none

      integer        :: nwin            ! # of points to use on signature

      real           :: siginv(:)       ! array for min-phase sig. inverse
      real           :: cros(:)         ! array for cross-correlation
      real           :: scr2(:)         ! scratch array for opfilt
      real           :: auto(:)         ! array for autocorrelation

      real           :: sig(:)          ! input signature (window portion)
      real           :: oper(:)         ! final operator for trace correlation

!---- autocorrelate sig
      call fltr_filtrgs (sig, nwin, sig, nwin, auto, nwin, 1, 0)

      auto(1) = auto(1)*1.0001                   !load diagonal 
      auto(2:nwin) = auto(2:nwin)/auto(1)        !normalize autocorrelation 
      cros(2:nwin) = 0.0                         !form cross correlation 
      cros(1) = 1.0 
      auto(1) = 1.0 

!---- min phase inverse
      call opfilt (nwin, siginv, cros, scr2, auto) 

!---- flip for convolution 
      scr2(:nwin) = siginv(nwin:1:(-1))          

!---- decon
      call fltr_filtrgs (scr2, nwin, sig, nwin, oper, nwin, 1, 1 - nwin) 

!---- scale operator by inverse of its power
      oper(:nwin) = oper(:nwin) / dot_product(oper(:nwin),oper(:nwin)) 

      return
      end subroutine mzpc_mzpcdcn 

!</execute_only>

!-------------------------------------------------------------------------------
! Subroutine to strip off pathcheck junk.
!-------------------------------------------------------------------------------
      function mzpc_absolute_path(pathname) result(relpath)
      character(len=FILENAME_LENGTH), intent(in)   :: pathname
      character(len=FILENAME_LENGTH)               :: relpath

      integer                          :: i

      do i=1,FILENAME_LENGTH
        if (pathname(i:i) == ':') then
          relpath = pathname(i+1:FILENAME_LENGTH)
          return
        end if
      end do

      relpath = pathname

      end function mzpc_absolute_path


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine mzpc_wrapup (obj)
      implicit none
      type(mzpc_struct),intent(inout) :: obj       ! arguments
      integer                         :: i

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ( &
      '----------------------------- MZPC start ------------------------------')
      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ('MZPC summary, mode = ',obj%mode)

      if (obj%opt_loc == 'FILE' .and. associated(obj%wave)) then 
        call pc_print ('Number of OUTPUT traces         : ', obj%ntout)
        call pc_print ('Number of traces phase converted: ', obj%ntfl)

        call pc_print ('')
        call pc_print ('Signature printout (from file ',obj%pathname,'):')
        do i = 1,obj%nwin
          call pc_print ('',obj%wave(i))
        end do

      call pc_print ( &
      '-----------------------------------------------------------------------')

        call pc_print ('Correlation operator printout:')
        do i = 1,obj%nwin
          call pc_print ('',obj%oper(i))
        end do

      else 
        call pc_print ('Number of GROUPS phase converted: ', obj%ngrp)
      end if 

      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ( &
      '------------------------------ MZPC end -------------------------------')
      call pc_print ( &
      '-----------------------------------------------------------------------')

      return
      end subroutine mzpc_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module mzpc_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
