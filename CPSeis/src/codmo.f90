!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-06-27. />

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
! Name       : CODMO
! Category   : migrations
! Written    : 1990-02-14   by: Mike Howard
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Perform common offset DMO on 2D data.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! CODMO is a macro which performs 2D DMO on simple 2D datasets for which SRC_INC
! is a whole number multiple of REC_INC/2.  More complicated 2D data can be 
! accomodated by using the underlying processes that CODMO calls.  DMO for 3D 
! data is provided by the macro DMO3D.
!
! CODMO calls TSORT, RESTH, KDMO, GSTK, RESTH and MUTE.  In addition 
! to the CODMO parameters, the following internal parameters are calculated:
!
!
!      CMP_INC  =  .5 * REC_INC
!
!      OFF_INC  =  2 * DRED * SRC_INC
!
!      OFF_INIT =  OFF_NEAR + .5 * OFF_INC - .5 * REC_INC
!
!      OFF_MAX  =  OFF_NEAR + (NUM_CHANNELS - 1) * REC_INC
!
!      NUM_TR   =  2 * OFF_MAX/CMP_INC
!
!      NUM_OFF  =  NINT((OFF_MAX - OFF_INIT)/OFF_INC) + 1
!
!
! (Parameter values indicated by "*" below are set identical to CODMO input 
! parameter values.)
!  
!   
!   TSORT
!       MNTOD=*,     HDR_PRI  = 6, PRI_INIT = OFF_INIT,  PRI_INC  = OFF_INC,
!                    HDR_SEC  = 7, SEC_INIT = CMP_INIT*, SEC_INC  = CMP_INC     
!
!   RESTH
!       HDR_INL = 7, INL_TOT = CMP_TOT*, INL_INIT = CMP_INIT*, INL_INC = 1
!  
!   KDMO
!       COORD = 2D, BIN_WID = *, VEL_DMO = *, FREQ_MAX = *, OFF_MAX = OFF_MAX
!
!   GSTK
!       NTIM = NUM_TR, TVFSE = 0.0, TIM_BEG = TSTRT, TIM_END = end of trace,
!
!            HDR_OFF  = 6,            HDR_INL  = 7, 
!            OFF_INIT = OFF_INIT,     INL_INIT = CMP_INIT*,
!            OFF_INC  = OFF_INC,      INL_INC  = 1.0
!            OFF_TOT  = NUM_OFF       INL_TOT  = CMP_TOT*
!
!   RESTH
!       MODE = APPLY
!
!   MUTE
!       OPT_MUTE = REST_HEAD, LEN_TAPER = 0.0
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Input Data
!
! Input traces may be in either CMP or shot profile order, but must be NMO 
! corrected with horizontal dip velocities.
! 
!
! Special Cases
!
! Symmetric split spreads can be handled by setting NUM_CHANNELS to the number 
! of channels on either side of the spread.  Other cases can be handled if you 
! are willing to preceed CODMO with a SELECT to delete offsets larger than 
! OFF_NEAR+(NUM_CHANNELS-1)*REC_INC.
!
! The case where the receiver interval is 4 times the source interval can be 
! handled by setting
!       SRC_INC      = (source increment),
!       REC_INC      = (receiver increment)/2,
!       NUM_CHANNELS = (number of channels)*2,
!       DRED         = (desired DRED)      *2
!
!
! Missing or Dead Input Trace Option
!
! The OPT_MDT parameter allows the user to control processing of missing or 
! dead input traces.  Output traces corresponding to missing or dead input 
! traces are deleted (OPT_MDT = DEL), filled in (OPT_MDT = FILL) or killed 
! (OPT_MDT = KILL).  Traces affected by the FILL option will always have their
! mute headers set to the first and last non-zero samples. FILL option will 
! only output traces with non-zero samples. However, KILL option outputs
! all traces including dead and live traces. Users may need to fix up the 
! headers of those traces generated by FILL or KILL option, which have -1 
! in header word 32.  
!
!
! Muting Option
!
! If OPT_MUTE = YES, then the output trace mute header words are reset to the
! corresponding input trace values and the MUTE process is called to restore 
! the previous mute.
! If OPT_MUTE = NO, then the output trace mute header words are set to the 
! first and last non-zero samples.  The MUTE process is not called. 
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! Since CODMO contains a TSORT to constant offsets it can be run on data either
! in CMP order or shot profile order.  The input data should have NMO at the 
! horizontal dip velocity applied.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process outputs one trace at a time.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED          
!
! 
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 changed
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Renumbered.
! 2       Head mute                  Reset
! 3       Current gather number      Set
! 4       Number within gather       Set
! 64      Tail mute                  Reset 
! 
! See also underlying processes.
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author     Description
!     ----        ------     -----------
!026. 2006-06-20  B. Menger   Removed Unused Variables.
! 25.  2001-06-18 Chiu       Save the value of header word 8 and restore the
!                            value after DMO.  PRODUCTION.
! 24.  2001-04-04 Stoeckley  Fix bug where NTR was not reset to NEED_TRACES
!                             before looping back to TSORT to get another trace.
! 23.  2001-02-14 Chiu       Change wrapped_up to skip_wrapup.
! 22.  2000-07-23 Chiu       Use new coordinate for header 7 and 8.
! 21.  2000-06-14 Chiu       Add Gui.
! 20.  2000-03-21 Chiu       Remove PC_BACKEND check for pc_put_contol.
! 19.  2000-01-27 Chiu       Convert to new CPS.
! 18.  1998-11-10 Vunderink  Begin using the f90 compiler.
! 17.  1997-02-25 Vunderink  Added BBINW and LBINW parameters to GSTK.
! 16.  1997-02-03 Vunderink  Added BINSIZE parameter.
! 15.  1996-12-12 Goodger    Change default for MUTE taper to zero.
! 14.  1996-08-29 Cooper     Calculate NWTR as 2*MAXOFF/BIN
! 13.  1996-07-10 Goodger    Make the dcode parameters for GSTK for to 3   
!                            lines rather than two.  It is going over 80
!                            columns.
! 12.  1994-09-13 Troutt     Add code to regularize header word 17 in a 2-D
!                            sense so that it matches the regularized value
!                            in header word 7.
!                            Add code in setup to abort if the rotation
!                            angle is non-zero --- this process is strictly
!                            2-D.
! 11.  1994-02-11 Troutt     Add error checking to HPALLOC call.
! 10.  1991-12-13 Howard     Fix ORDER=COS to output dead traces for
!                            basements with missing traces.
! 9.   1991-07-24 Howard     Make compatible with OFR offset spacing.
! 8.   1991-02-22 Howard     Add basement compositing.
! 7.   1991-02-05 Howard     Regularize offsets and composite dupicates.
! 6.   1990-08-10 Peterson   Set to skip dead traces after TSORT.
! 5.   1990-06-19 Howard     Add call to MUTE OPT=0.  Delete traces not in
!                            original constant offset sections.
! 4.   1990-03-26 Howard     Change from SORT to TSORT. 
! 3.   1990-03-14 Howard     Fix bug if SIN not equal to RIN.
! 2.   1990-02-28 Howard     Output constant offset sections.
! 1.   1990-02-14 Howard     Original Version 
!    
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
! 
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS            
!
! This process uses two sets of trace and header arrays.
!
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! The design of the bandpass filter was changed. The KDMO output will not    
! match exactly as the output from the old CPS.
!
! NUM_OFF is changed to match the one from GSTK. This change may produce  
! a slightly different result when comparing with old CPS module. 
! 
! The LINE_WID, CRL_WID and OFF_WID parameters are removed in this version.
! The DMO output from the new CPS may not match exactly as the old CPS 
! module, because the new DMO output location may be off one CMP bin when  
! comparing with old CPS module. 
! 
! As a result of all these changes, users expect to see a slight difference
! in background signal between the new and old DMO outputs.   
! 
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS CODMO Process/NC=80>
!
!              Perform common offset DMO on 2D data.
!
! MNTOD=~~~`IIIIIIIIII   NTIM=~~~~`IIIIIIIII     NUM_CHANNELS=`IIIIIIIIII
!
! OFF_NEAR=`FFFFFFFFFF   REC_INC= `FFFFFFFFFFF   SRC_INC=~~~`FFFFFFFFFFF
!
! CMP_TOT= `FFFFFFFFFF   CMP_INIT=`FFFFFFFFFFF
!
! FREQ_MAX=`FFFFFFFFFF   VEL_DMO= `FFFFFFFFFFF   DRED=~~~~~~`IIIIIIIIII
!
! BIN_WID= `FFFFFFFFFF   OPT_MDT= `CCCC          OPT_MUTE=~~`CCC
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="MNTOD">
!<Tip> Maximum Number of Traces On Disk at once. </Tip>
! Default = 30000
! Allowed = int > 0
! MNTOD is the maximum number of traces to store on disk at once for the 
! sorting operation.
!</Help> 
!
!<Help KEYWORD="NTIM">
!<Tip> Number of Traces In Memory. </Tip>
! Default = 5000
! Allowed = int > 0
! Generally NTIM should be set large enough so that disk swapping is minimized,
! but small enough to avoid restrictions on memory availability or cost.
!
! For operation in conjunction with DMO, NTIM should be set to a minimum 
! of the number of bins in a rectangular array that might be populated by a DMO 
! broadcast operator for the largest offset.  Larger values may increase 
! efficiency, but at some point limits on memory availability and memory cost 
! will establish an upper limit on NTIM.
!</Help>
!
!<Help KEYWORD="NUM_CHANNELS">
!<Tip> Number of channels (receiver groups) in a shot profile. </Tip>
! Default = 320
! Allowed = 1 - 9999
!</Help>
!
!<Help KEYWORD="OFF_NEAR">
!<Tip> Near offset. </Tip>
! Default = 300
! Allowed = real > 0.0 
!</Help>
!
!<Help KEYWORD="REC_INC">
!<Tip> Receiver increment (distance between adjacent receiver groups). </Tip>
! Default = 12.5
! Allowed = real > 0.0 
!</Help>
!
!<Help KEYWORD="SRC_INC">
!<Tip> Source increment (distance between adjacent source locations). </Tip>
! Default = 12.5
! Allowed = real > 0.0 
!</Help>
!
!<Help KEYWORD="CMP_TOT">
!<Tip> Number of CMP locations on the line. </Tip>
! Default = 500
! Allowed = int > 0 
!</Help>
!
!<Help KEYWORD="CMP_INIT">
!<Tip> Value of header word 7 (inline grid coordinate) for first CMP. </Tip>
! Default = 1.0
! Allowed = real
! Value of header word 7 (inline grid coordinate) for first CMP in the line.
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency to be preserved in data, in Hz. </Tip>
! Default = 90
! Allowed = real > 0.0
! FREQ_MAX is used in the DMO operator calculation as the highest frequency to 
! preserve in the data.  Larger values of FREQ_MAX require significantly more 
! run-time.
!</Help>
!
!<Help KEYWORD="VEL_DMO">
!<Tip> Minimum propagation velocity in medium. </Tip>
! Default = 1500
! Allowed = real > 0.0
! VEL_DMO is used to calculate the maximum dip to be preserved in the data.
!</Help>
!
!<Help KEYWORD="DRED">
!<Tip> Fold reduction factor (new_fold = old_fold/DRED). </Tip>
! Default = 1
! Allowed = int > 0
! DRED > 1 composites adjacent offsets within CMP gathers.  This is done using 
! STK with FSE=0.5.   
!</Help>
!
!<Help KEYWORD="BIN_WID">
!<Tip> Stack bin width for DMO operator trace interval. </Tip>
! Default = REC_INC/2.0
! Allowed = n * REC_INC/2.0
! Setting BIN_WID (nominally the CMP interval) to twice the default value causes
! KDMO to spread the DMO operator over half as many traces as usual and to 
! generate operator wavelets of a lower frequency due to the anti-aliasing 
! condition.  The lower frequency wavelets may be helpful by filtering off 
! aliased energy in data that is poorly sampled when sorted into common offset 
! gathers.  The default value should be used except for poorly sampled data.
!</Help>
!
!<Help KEYWORD="OPT_MDT">
!<Tip> Option for handling Missing or Dead input Traces. </Tip>
! Default = DEL
! Allowed = DEL   
! Allowed = FILL
! Allowed = KILL
! Output traces corresponding to missing or dead input traces are deleted 
! (OPT_MDT = DEL), filled in (OPT_MDT = FILL) or killed (OPT_MDT = KILL).
! Traces affected by the FILL option will always have their mute headers set to
! the first and last non-zero samples.
!</Help>
!
!<Help KEYWORD="OPT_MUTE">
!<Tip> Whether to restore the previous mute. </Tip>
! Default = YES
! Allowed = YES/NO   
! If OPT_MUTE = YES, then the output trace mute header words are reset to the
! corresponding input trace values and the MUTE process is called to restore 
! the previous mute.
! If OPT_MUTE = NO, then the output trace mute header words are set to the 
! first and last non-zero samples.  The MUTE process is not called. 
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
!   1.  Omit COMP parameter and all functionality regarding OFR.
!
!   2.  Add OPT_MDT and OPT_MUTE functionality, same as DMO3D.
!    
!   3.  Omit SETWORD module, which is no longer required in TSORT. 
! 
!   4.  Omit ORDER  parameter. If the input is a common offset gather,  
!       the output from CODMO is a common offset gather. If the input 
!       is CMP gathers or other type of gathers, the output is CMP gather.
! 
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module codmo_module
      use pc_module
      use named_constants_module
      use grid_module           ! grid transformation.
      use string_module
      use tsort_module          ! calling tsort process internally.
      use resth_module          ! calling resth process internally.  
      use stk_module            ! calling stk process internally. 
      use kdmo_module           ! calling kdmo process internally. 
      use gstk_module           ! calling gstk process internally. 
      use mute_module           ! calling mute process internally. 
      use lav_module

      implicit none
      private
      public :: codmo_create     ! uses the parameter cache.
      public :: codmo_initialize
      public :: codmo_update     ! uses the parameter cache.
      public :: codmo_delete
!<execute_only>
      public :: codmo            ! main execution (trace processing) routine.
      public :: codmo_wrapup
!</execute_only>

      character(len=100),public,save :: codmo_IDENT = &
       '$Id: codmo.f90,v 1.26 2006/06/20 13:11:49 Menger prod sps $'
!!
!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: codmo_struct              
 
      private
      logical               :: skip_wrapup         ! wrapup flag.
      integer               :: ntim               ! process parameters 
      integer               :: mntod              ! process parameters  
      integer               :: cmp_tot            ! process parameters 
      integer               :: dred               ! process parameters 
      integer               :: num_channels       ! process parameters    
      real                  :: cmp_init           ! process parameters 
      real                  :: freq_max           ! process parameters 
      real                  :: off_near           ! process parameters  
      real                  :: rec_inc            ! process parameters  
      real                  :: src_inc            ! process parameters  
      real                  :: vel_dmo            ! process parameters
      real                  :: bin_wid            ! process parameters
      character (len=3)     :: opt_mute           ! process parameters   
      character (len=4)     :: opt_mdt            ! process parameters

      integer               :: ndpt               ! Common globals
      integer               :: nwih               ! Common globals
      real                  :: dt                 ! Common globals
      real                  :: tstrt              ! Common globals 
      type(grid_struct)     :: grid               ! Common globals

      logical               :: done               ! dependent variables
      integer               :: ifb                ! dependent variables
      integer               :: npan               ! dependent variables
      integer               :: nskip              ! dependent variables
      integer               :: ntr_input          ! dependent variables
      integer               :: print_lun          ! dependent variables

      real                  :: off_max            ! dependent variables
      real                  :: off_init           ! dependent variables
      real                  :: off_inc            ! dependent variables
      real                  :: cmp_inc            ! dependent variables
      real                  :: hd8_value          ! dependent variables
      logical               :: first_trace        ! dependent variables

      type(tsort_struct),pointer    :: tsort      ! dependent module 
      type(resth_struct),pointer    :: resth      ! dependent module 
      type(stk_struct),  pointer    :: stk        ! dependent module 
      type(kdmo_struct), pointer    :: kdmo       ! dependent module 
      type(gstk_struct), pointer    :: gstk       ! dependent module 
      type(mute_struct), pointer    :: mute       ! dependent module 

      end type codmo_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(codmo_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: opt_mdt_noptions = 3
      character(len=4),save :: opt_mdt_options(opt_mdt_noptions)
      data opt_mdt_options/'DEL ','FILL', 'KILL'/

      integer,parameter     :: opt_mute_noptions = 2
      character(len=3),save :: opt_mute_options(opt_mute_noptions)
      data opt_mute_options/'YES','NO '/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine codmo_create (obj)
      implicit none
      type(codmo_struct),pointer :: obj       ! arguments

      allocate (obj)


!! Nullify ALL POINTERS in your parameter structure as follows:
!! These might be pointers to arrays or pointers to the data structures
!! of internally called processes.
!!
      nullify  (obj%tsort)  
      nullify  (obj%resth)  
      nullify  (obj%stk)  
      nullify  (obj%kdmo)  
      nullify  (obj%gstk)  
      nullify  (obj%mute)  

      call codmo_initialize (obj)

      return
      end subroutine codmo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine codmo_delete (obj)
      implicit none
      type(codmo_struct),pointer :: obj       ! arguments

!<execute_only>
      call codmo_wrapup (obj)
!</execute_only>

!! deallocate ALL POINTERS in parameter structure 


      if (associated(obj%tsort))  call tsort_delete (obj%tsort)  
      if (associated(obj%resth))  call resth_delete (obj%resth)  
      if (associated(obj%stk))    call stk_delete   (obj%stk)  
      if (associated(obj%kdmo))   call kdmo_delete  (obj%kdmo)  
      if (associated(obj%gstk))   call gstk_delete  (obj%gstk)  
      if (associated(obj%mute))   call mute_delete  (obj%mute) 

      deallocate(obj)

      return
      end subroutine codmo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine codmo_initialize (obj)
      implicit none
      type(codmo_struct),intent(inout) :: obj       ! arguments

!! Initialize ALL NON-POINTER VARIABLES in the parameter structure

      obj%mntod         = 30000
      obj%ntim          = 5000
      obj%num_channels  = 320
      obj%off_near      = 300
      obj%rec_inc       = 12.5      
      obj%src_inc       = 12.5 
      obj%cmp_tot       = 500 
      obj%cmp_init      = 1.0 
      obj%freq_max      = 90.
      obj%vel_dmo       = 1500.
      obj%dred          = 1
      obj%bin_wid       = -1.0
      obj%opt_mdt       = 'DEL'           
      obj%opt_mute      = 'YES'   
  
      obj%print_lun = pc_get_lun()
      call grid_initialize(obj%grid)
      call codmo_update (obj)

      return
      end subroutine codmo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine codmo_update (obj)
      implicit none
      type(codmo_struct),intent(inout),target :: obj             ! arguments



      integer                    ::     j ! local
      logical                    :: gathered,iftd                ! local
      integer                    :: nstore, nscratch             ! local
      integer                    :: ntapes,ndisk                 ! local

      character(len=3)           :: need_label,need_request      ! local
      character(len=3)           :: twosets                      ! local
      character(len=3)           :: coord                        ! local
      character(len=60)          :: tmp_char                     ! local

      integer                    :: hdr_pri,hdr_sec ! local
      integer                    :: hdr_inL,hdr_gath,hdr_off     ! local    
      integer                    ::     napert,len_taper ! local
      real                       :: mscl          ! local
      real                       :: inL_inc ! local
      real                       :: inL_last                     ! local
      real                       :: off_last                     ! local
      real                       :: tim_beg,tim_end              ! local
      real                       :: dx21                         ! local
      real                       :: tvfse, xdh                   ! local
      real                       :: taper                        ! local
      character(len=8)           :: atod,print,opt_sort,fill     ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('nwih', obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt', obj%ndpt)  ! number of trace samples.
      call pc_get_global ('dt',   obj%dt)    ! trace sample interval (sec).
      call pc_get_global ('tstrt',obj%tstrt) ! time of 1st trace sample(sec).
      call pc_get_global ('grid', obj%grid)  

! grid transform data structure.

      call pc_get ('mntod',        obj%mntod)
      call pc_get ('ntim',         obj%ntim) 
      call pc_get ('num_channels', obj%num_channels)
      call pc_get ('off_near',     obj%off_near)
      call pc_get ('rec_inc',      obj%rec_inc)      
      call pc_get ('src_inc',      obj%src_inc) 
      call pc_get ('cmp_tot',      obj%cmp_tot) 
      call pc_get ('cmp_init',     obj%cmp_init) 
      call pc_get ('freq_max',     obj%freq_max)
      call pc_get ('vel_dmo',      obj%vel_dmo)
      call pc_get ('dred',         obj%dred)
      call pc_get ('bin_wid',      obj%bin_wid)
      call pc_get ('opt_mdt',      obj%opt_mdt)           
      call pc_get ('opt_mute',     obj%opt_mute) 

      call string_to_upper (obj%opt_mdt)
      call string_to_upper (obj%opt_mute)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

     if (obj%mntod <= 0) then
       call pc_warning('MNTOD MUST BE > 0')
       obj%mntod = 30000
      end if

      if(obj%ntim <= 0) then 
        call pc_error('NTIM MUST BE > 0')  
      end if
 
      if(obj%num_channels <= 0 .or. obj%num_channels > 9999) then
         call pc_error                                                 &
         ('Invalid in CODMO: NUM_CHANNELS MUST BE BETWEEN 1 & 9999')
      end if

      if(obj%off_near <= 0.) then
         call pc_error('Invalid in CODMO: OFF_NEAR MUST BE > 0')
      end if

      if(obj%rec_inc <= 0.) then
         call pc_error('Invalid in CODMO: REC_INC MUST BE > 0')
      end if

      if(obj%src_inc <= 0.) then
         call pc_error('Invalid in CODMO: SRC_INC MUST BE > 0')
      end if

      if(obj%cmp_tot <= 0) then
         call pc_error('Invalid in CODMO: CMP_TOT MUST BE > 0')
      end if

      if(obj%freq_max<0) then
         call pc_error                                                   &
         ('Invalid in CODMO: FREQ_MAX MUST BE >= ZERO')
      end if

      if(obj%freq_max > nint(0.5/obj%dt)) then
         obj%freq_max = (0.5/obj%dt)
         call pc_warning(' Reset FREQ_MAX in CODMO to Nyquist frequency ', &
                           (0.5/obj%dt)  )
       end if

       if(obj%vel_dmo <=0.) then
         call pc_error('Invalid in CODMO: VEL_DMO MUST BE > 0')
       end if

      if(obj%opt_mdt /='DEL' .and. obj%opt_mdt /='FILL'                      &
        .and. obj%opt_mdt /='KILL') then 
        call pc_error('Invalid in CODMO: OPT_MDT must be DEL, FILL or KILL')  
      end if

      if(obj%dred <= 0.) then
         call pc_error('Invalid in CODMO: DRED MUST BE > 0')
      end if

      if(obj%opt_mdt /='DEL' .and. obj%opt_mdt /='FILL'                      &
        .and. obj%opt_mdt /='KILL') then 
        call pc_error('OPT_MDT must be DEL, FILL or KILL')  
      end if

      if(obj%opt_mute /='YES' .and. obj%opt_mute /='NO') then 
        call pc_error('Invalid in CODMO: OPT_MUTE must be YES/NO')  
      end if
 
      if (obj%bin_wid == (-1.0)) then
         obj%bin_wid = 0.5*obj%rec_inc
      end if

      j = nint(obj%bin_wid/(0.5*obj%rec_inc))
      if (abs(obj%bin_wid-j*(0.5*obj%rec_inc)) > 1.e-10) then
         call pc_error                   &
         ('Invalid in CODMO: BIN_WID is not multiple of REC_INC/2.0')
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

     xdh = 0.75
     obj%cmp_inc  = 1
     obj%off_max = obj%off_near + (obj%num_channels - 1)*obj%rec_inc
     obj%off_inc = 2*obj%dred*obj%src_inc 
     obj%off_init = obj%off_near + 0.5*obj%off_inc - 0.5*obj%rec_inc
     obj%npan = nint((obj%off_max - obj%off_init)/obj%off_inc) 
     obj%npan = obj%npan+1
     napert   = nint(obj%off_max*xdh/obj%bin_wid/2.0)*2 + 1

     tim_beg = obj%tstrt 
     tim_end = (obj%ndpt - 1)*obj%dt 

!   set up parameters for processing modules

!   tsort

!      obj%mntod= *,    
!      hdr_pri  = 6,  pri_init = obj%off_init, pri_inc  = obj%off_inc, 
!      hdr_sec  = 7,  sec_init = cmp_init*,    sec_inc  = obj%cmp_inc

      atod     = 'NO'
      print    = 'DETAILED'
      opt_sort = 'CUSTOM'
      fill     = 'NO'

      hdr_pri  = 6
      hdr_sec  = 7 

      call pc_clear
      call pc_put_process ('mntod',     obj%mntod)       
      call pc_put_process ('ATOD',      atod)
      call pc_put_process ('OPT_PRINT', print)
      call pc_put_process ('OPT_SORT',  opt_sort)
      call pc_put_process ('FILL',      fill)

      call pc_put_process ('hdr_pri',   hdr_pri)     
      call pc_put_process ('pri_init',  obj%off_init)    
      call pc_put_process ('pri_inc',   obj%off_inc)        
 
      call pc_put_process ('hdr_sec',   hdr_sec)       
      call pc_put_process ('sec_init',  obj%cmp_init)    
      call pc_put_process ('sec_inc',   obj%cmp_inc)    

      call pc_put_process ('hdr_tert',   1)       
      call pc_put_process ('tert_init',  1.)    
      call pc_put_process ('tert_inc',   1.) 

      if (associated(obj%tsort)) then
          call tsort_update (obj%tsort)
      else
          call tsort_create (obj%tsort)
      end if
      call pc_restore

! end tsort 


! resth
!       hdr_inL = 7, inL_tot = cmp_tot*, inL_init = cmp_init*, inL_inc = 1
!       mode = apply

      hdr_inL = 7 
      inL_inc = 1.0
      inL_last = obj%cmp_init + (obj%cmp_tot-1)*inL_inc

      call pc_clear
      call pc_put_process ('mode',      'CALC')
      call pc_put_process ('hdr_inL',   hdr_inL)        
      call pc_put_process ('inL_tot',   obj%cmp_tot)     
      call pc_put_process ('inL_init',  obj%cmp_init)    
      call pc_put_process ('inL_inc',   inL_inc) 
      call pc_put_process ('inL_wid',   inL_inc)       
      call pc_put_process ('inL_last',  inL_last)

      call pc_put_process ('hdr_crl',  8)
      call pc_put_process ('crl_init', 1.0)  
      call pc_put_process ('crl_inc',  1.0) 
      call pc_put_process ('crl_wid',  1.0)
      call pc_put_process ('crl_last', 1.0)   
      call pc_put_process ('crl_tot',  1)

      if (associated(obj%resth)) then
          call resth_update (obj%resth)
      else
           call resth_create (obj%resth)
      end if
      call pc_restore

! end resth


! stk
      hdr_gath = 32 
      mscl     = 0.0

      call pc_clear
      call pc_put_process ('opt_input',   'SINGLE')
      call pc_put_process ('hdr_gath',     hdr_gath)           
      call pc_put_process ('mscl',         mscl)        
 
      if (associated(obj%stk)) then
          call stk_update (obj%stk)
      else
           call stk_create (obj%stk)
      end if
      call pc_restore

! end stk


! kdmo
!    coord = 2d, bin_wid = *, vel_dmo = *, freq_max = *, off_max = off_max

      coord = '2D'
      call pc_clear
      call pc_put_process ('bin_wid',    obj%bin_wid)
      call pc_put_process ('freq_max',   obj%freq_max)
      call pc_put_process ('coord',      coord)
      call pc_put_process ('off_max',    obj%off_max)
      call pc_put_process ('vel_dmo',    obj%vel_dmo)

      if (associated(obj%kdmo)) then
          call kdmo_update (obj%kdmo)
      else
          call kdmo_create (obj%kdmo)
      end if
      call pc_restore

!   end kdmo


!   gstk

      hdr_inL = 7 
      hdr_off  = 6
      inL_inc = 1.0
      inL_last = obj%cmp_init + (obj%cmp_tot-1)*inL_inc
      off_last = obj%off_init + (obj%npan-1)*obj%off_inc
      tvfse = 0.0

      write(obj%print_lun,935) obj%off_init,obj%off_inc,off_last, obj%npan
 935  format('off_init,off_inc,off_last, off_tot ', 3f12.3, 3x, i6)           

      call pc_clear

      call pc_put_process ('ntim',      obj%ntim) 
      call pc_put_process ('tvfse',     tvfse)
      call pc_put_process ('tim_beg',   tim_beg)
      call pc_put_process ('tim_end',   tim_end)
      call pc_put_process ('opt_mdt',   obj%opt_mdt)

      call pc_put_process ('hdr_inL',   hdr_off)             
      call pc_put_process ('inL_init',  obj%off_init)    
      call pc_put_process ('inL_inc',   obj%off_inc)
      call pc_put_process ('inL_wid',   obj%off_inc)
      call pc_put_process ('inL_last',  off_last)        
      call pc_put_process ('inL_tot',   obj%npan) 

      call pc_put_process ('hdr_crl',   hdr_inL)
      call pc_put_process ('crl_init',  obj%cmp_init)  
      call pc_put_process ('crl_inc',   inL_inc)
      call pc_put_process ('crl_wid',   inL_inc)
      call pc_put_process ('crl_last',  inL_last)    
      call pc_put_process ('crl_tot',   obj%cmp_tot)  
 
      call pc_put_process ('hdr_off',  8)
      call pc_put_process ('off_init', 1.0)  
      call pc_put_process ('off_inc',  1.0)
      call pc_put_process ('off_wid',  1.0) 
      call pc_put_process ('off_last', 1.0)   
      call pc_put_process ('off_tot',  1)

      if (associated(obj%gstk)) then
          call gstk_update (obj%gstk)
      else
           call gstk_create (obj%gstk)
      end if
      call pc_restore

!  end  gstk


! mute
!       opt_mute = rest_head, taper = 0.06

      taper = 0.06
      len_taper = nint(0.06/obj%dt) - 1
      tmp_char  = 'REST_HEAD'

      call pc_clear
      call pc_put_process ('OPT_MUTE',    tmp_char)
      call pc_put_process ('LEN_TAPER',   taper)
        
      if (associated(obj%mute)) then
          call mute_update (obj%mute)
      else
           call mute_create (obj%mute)
      end if
      call pc_restore

! end mute

!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('opt_mdt', opt_mdt_options,   &
                                   opt_mdt_noptions)

      call pc_put_options_field ('opt_mute', opt_mute_options,   &
                                   opt_mute_noptions)
      gathered = .false.

      call pc_put_global  ('numtr'       , napert)           ! if changed.
      call pc_put_global  ('gathered'    , gathered)         ! if changed.

      call pc_put ('mntod',        obj%mntod)
      call pc_put ('ntim',         obj%ntim) 
      call pc_put ('num_channels', obj%num_channels)
      call pc_put ('off_near',     obj%off_near)
      call pc_put ('rec_inc',      obj%rec_inc)      
      call pc_put ('src_inc',      obj%src_inc) 
      call pc_put ('cmp_tot',      obj%cmp_tot) 
      call pc_put ('cmp_init',     obj%cmp_init) 
      call pc_put ('freq_max',     obj%freq_max)
      call pc_put ('vel_dmo',      obj%vel_dmo)
      call pc_put ('dred',         obj%dred)
      call pc_put ('bin_wid',      obj%bin_wid)
      call pc_put ('opt_mdt',      obj%opt_mdt)           
      call pc_put ('opt_mute',     obj%opt_mute) 


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


        need_label   = 'YES'
        need_request = 'YES'
        twosets      = 'YES'
        iftd         = .false.
        ndisk        = 0
        ntapes       = 0 
 
        nstore       = obj%npan*obj%cmp_tot+1
        nscratch     = 1                                     ! scratch storage 

        call pc_put_control ('nstore',             nstore)
        call pc_put_control ('nscratch',         nscratch)
        call pc_put_control ('need_label',     need_label)
        call pc_put_control ('need_request', need_request)
        call pc_put_control ('twosets',           twosets)
 
!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false. 

      obj%ifb = 1                   ! initialize to show entry from above  
      obj%nskip = 0  
      obj%done = .false.
      obj%first_trace = .true.

!   make sure this is a 2-d job 

      dx21 = grid_get_dx21(obj%grid)
      if (dx21 /= 0.0) then 
        write(obj%print_lun, *) 'CODMO: **WARNING** This is 2-D DMO ',       & 
          'and cannot handle 3-D grids.' 
        write(obj%print_lun, *) '  You may use SETWORD to put artificial ',  & 
          '2-D coords. into headers 17 and 18.' 
        write(obj%print_lun, *) '  You would also have to change your',      & 
          ' rotation matrix accordingly.'
        call pc_error('FATAL_ERROR in routine CODMO ') 
        return 
      endif 

!! Allocate your permanent memory 


!   TSORT
      write (obj%print_lun, *) 'TSORT : '
      write (obj%print_lun, *)'  MNTOD = ',obj%mntod,'  HDR_PRI = ', hdr_pri, & 
      '  PRI_INIT = ',obj%off_init,'  PRI_INC  = ', obj%off_inc

       write (obj%print_lun, *) '  HDR_SEC = ',hdr_sec,                       &
      '  SEC_INIT = ',obj%cmp_init, '  SEC_INC = ',obj%cmp_inc 
!
!   RESTH
      write (obj%print_lun, *) ' RESTH : ' 
      write (obj%print_lun, *) '  HDR_INL = ', hdr_inL, '  INL_TOT = ',       &
        obj%cmp_tot,'  INL_INIT = ',  obj%cmp_init, '  INL_INC = ',           &
        inL_inc, '  INL_WID = ',inL_inc

!   KDMO
     write (obj%print_lun, *) ' KDMO : '  
     write (obj%print_lun, *) '  COORD = ',coord, '  BIN_WID = ',             &
       obj%bin_wid,'  VEL_DMO = ', obj%vel_dmo 
 
     write (obj%print_lun, *) ' FREQ_MAX = ',  obj%freq_max,                  &
       '  OFF_MAX = ', obj%off_max  

!   GSTK
     write (obj%print_lun, *) ' GSTK : '
     write (obj%print_lun, *) '  NTIM = ', obj%ntim, '  TVFSE =', tvfse,      &
     '  TIM_BEG = ', tim_beg,'  TIM_END =', tim_end

      write (obj%print_lun, *)      '  HDR_INL  =', hdr_off,                  &
     '  INL_INIT =', obj%off_init, '  INL_INC  =', obj%off_inc,               &
     '  INL_TOT  =', obj%npan,     '  INL_WID  =', obj%off_inc

      write (obj%print_lun, *)       '  HDR_CRL  =', hdr_inL,                 &
     '  CRL_INIT =', obj%cmp_init,  '  CRL_INC  =', inL_inc,                  &
     '  CRL_TOT  =', obj%cmp_tot,   '  CRL_WID  =', inL_inc
 
!   RESTH
      write (obj%print_lun, *) ' RESTH : '
      write (obj%print_lun, *) '  MODE = APPLY  '
!
!   MUTE
     write (obj%print_lun, *) ' MUTE : '
     write (obj%print_lun, *)'  LEN_TAPER =', len_taper,                      &
                             '  OPT_MUTE =', tmp_char                       

     if (pc_do_not_process_traces()) return  
                         
!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine codmo_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!! Upon input, NTR will have one of these values:
!!   NTR >= 1              means to process the input traces.
!!   NTR == NO_MORE_TRACES means there are no more imput traces.
!!   NTR == NEED_TRACES    means someone from below needs more traces.
!!   NTR == NEED_TRACES    might mean this is a trace-supplying process.
!!   NTR == NEED_TRACES    will not occur unless this process has a label.
!!
!! Upon output, NTR must have one of these values:
!!   NTR >= 1              if you are outputting traces.
!!   NTR == NO_MORE_TRACES if there are no more traces to output.
!!   NTR == FATAL_ERROR    if you have a fatal error.
!!   NTR == NEED_TRACES    if you need another trace before passing any out.
!!   NTR == NEED_TRACES    must not occur unless you specified that you
!!                           might need to request more traces.
!!
!<execute_only>

 
      subroutine codmo (obj,ntr,hdi, tri, hdo, tro)
      implicit none
      type(codmo_struct),intent(inout) :: obj                   ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hdi(:,:)               ! arguments
      real             ,intent(in)    :: tri(:,:)               ! arguments
      double precision ,intent(inout) :: hdo(:,:)               ! arguments
      real             ,intent(inout) :: tro(:,:)               ! arguments

      double precision                :: xorg, dx11             ! local
      integer                         :: k    ,ibas,ipan ! local
      real                            :: fmute,tmute            ! local

      if ( (ntr==NO_MORE_TRACES .and. obj%done)             &
       .or. ntr == FATAL_ERROR) then
         call codmo_wrapup (obj)
         write(obj%print_lun, *) 'CODMO: SKIPPED = ', obj%nskip,     &
          ' DEAD OR INPUT BAD TRACES.' 
         return
      end if

      if (obj%ifb < 0) go to 300 
         
      do k = 1, ntr 

!      if trace is dead, let's skip it ! 
        if (hdi(25,k) == 0.0) cycle  

!       reset the headers
        ibas = nint((hdi(7,k)-obj%cmp_init)/obj%cmp_inc) 
        hdi(7,k) = obj%cmp_init + ibas*obj%cmp_inc 
        xorg = grid_get_xorigin(obj%grid)
        dx11 = grid_get_dx11   (obj%grid)
        hdi(17,k) = xorg + dx11*hdi(7,k) 
        ipan = nint((hdi(6,k)-obj%off_init)/obj%off_inc) 
        hdi(6,k) = obj%off_init + ipan*obj%off_inc 
      end do 
 
  100 continue 

!.... store the initial value of header word

      if (obj%first_trace) then
         obj%hd8_value = hdi(8,1)
         call gstk_set_init_value(obj%gstk, obj%hd8_value)
         obj%first_trace = .false.
      end if

      call tsort (obj%tsort, ntr, hdi, tri, hdo, tro) 

      if (ntr == FATAL_ERROR) then 
        call pc_error('FATAL_ERROR in routine TSORT in CODMO ') 
        return
      end if 
      if (ntr == NEED_TRACES) go to 500 
      if (ntr == NO_MORE_TRACES) then
        if ( obj%dred > 1) then
          call stk (obj%stk, ntr, hdo, tro)      
        end if
        call kdmo (obj%kdmo, ntr,hdo, tro)
        ntr = NEED_TRACES
        go to 300
      end if


!      if trace is dead, let's skip it ! 
      if (ntr==1 .and. (hdo(25,ntr)<=0 .or. hdo(32,ntr)<0)) then 
        obj%nskip = obj%nskip + 1
        ntr = NEED_TRACES            !!!!! added 3/29/01 by Tom Stoeckley
        go to 100 
      end if 

      call resth (obj%resth, ntr, hdo, tro) 
      if (ntr == FATAL_ERROR) then
        call pc_error('FATAL_ERROR in routine RESTH in CODMO ') 
        return
      end if 
  101 continue 
      
      if ( obj%dred > 1) then
        call stk (obj%stk, ntr, hdo, tro) 
        if (ntr == FATAL_ERROR) then 
          call pc_error('FATAL_ERROR in routine STK in CODMO ') 
          return
        end if
        if (ntr == NEED_TRACES) go to 100
      end if
 
  200 continue

      call kdmo (obj%kdmo, ntr,hdo, tro) 
      if (ntr == FATAL_ERROR) then
        call pc_error('FATAL_ERROR in routine KDMO in CODMO ') 
        return
      end if
  
  300 continue 

      call gstk (obj%gstk, ntr, hdo, tro) 
      if (ntr == FATAL_ERROR) then
        call pc_error('FATAL_ERROR in routine GSTK in CODMO ') 
        return
      end if

      if (ntr == NEED_TRACES) then
         if ( obj%dred > 1) go to 101
         if ( obj%dred == 1) go to 100             
      else if (ntr == NO_MORE_TRACES) then
         obj%done = .true.
         go to 550
      end if 

!     resth does not restore front and tail mutes if hdo(32,ntr)< 0.0
      fmute = hdo(2,ntr)
      tmute = hdo(64,ntr)

      resth_mode = 'APPLY'
      call resth (obj%resth, ntr, hdo, tro) 
      if (ntr == FATAL_ERROR) then
        call pc_error('FATAL_ERROR in routine RESTH in CODMO ') 
        return
      end if
 
      if (hdo(32,ntr) <= 0.0 .and. obj%opt_mdt =='FILL') then
         hdo(2,ntr)  = fmute 
         hdo(64,ntr) = tmute 
      endif 
      if (obj%opt_mute == 'YES') then
         call mute (obj%mute, ntr, hdo, tro) 
      end if
      if (ntr == FATAL_ERROR) then
        call pc_error('FATAL_ERROR in routine  MUTE in CODMO ') 
        return
      end if

      call lav_set_hdr (hdo, tro, obj%ndpt, ntr)
 
!     exit down 
 550  continue
      obj%ifb = -1 
      return  
!     exit up 
  500 continue 
      obj%ifb = 1  
      return  

      end subroutine codmo

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine codmo_wrapup (obj)
      implicit none
      type(codmo_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

!!
!!  --> Put any required wrapup code here.
!!

      if (associated(obj%tsort))  call tsort_wrapup (obj%tsort)  
      if (associated(obj%resth))  call resth_wrapup (obj%resth)  
      if (associated(obj%stk))    call stk_wrapup   (obj%stk)  
      if (associated(obj%kdmo))   call kdmo_wrapup  (obj%kdmo)  
      if (associated(obj%gstk))   call gstk_wrapup  (obj%gstk)  
      if (associated(obj%mute))   call mute_wrapup  (obj%mute) 

      return
      end subroutine codmo_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module codmo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
