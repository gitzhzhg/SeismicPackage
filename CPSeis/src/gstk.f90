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
! Name       : GSTK     (Gatherless STacK)
! Category   : stacks
! Written    : 1986-10-02   by: Mike Howard
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Stack traces without requirement of sorting into stack gathers.
! Portability: No known limitations.
! Parallel   : NO
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! GSTK sets up a rectangular bin array based on HDR_INL, HDR_CRL and HDR_OFF,
! with the bins occupying memory that is swapped to disk.  Input traces 
! assigned to the same bin are stacked together into a single stacked trace.  
! Stack scaling is controlled by the TVFSE parameter, which functions the same 
! as in STK.
!
!
! 2D vs. 3D Operation
!
!       For 2D operation and for a 2D line from a 3D survey:
!              CRL_TOT should be set to 1, which causes GSTK to ignore the 
!              crossline direction. 
! 
!       For 3D operation, compositing offsets within GSTK:
!              generally INL_TOT > 1, CRL_TOT > 1 and OFF_TOT = 1.
!              (set OFF_INIT to 0.0)
!
!       For 3D common offset operation:
!              generally INL_TOT > 1, CRL_TOT > 1 and OFF_TOT = 1.
!              (set OFF_INIT to the nominal offset of the job)
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Memory Allocation
!
! Generally NTIM should be set large enough so that disk swapping is minimized,
! but small enough to control cost and avoid restrictions on memory 
! availability.
!
! For operation in conjunction with DMO, NTIM should be set to a minimum 
! of the number of bins in a rectangular array that might be populated by a DMO 
! broadcast operator for the largest offset within the job.  Larger values may 
! increase efficiency, but at some point limits on memory availability and 
! memory cost will establish an upper limit on NTIM.
!
!
! Source and Receiver Locations After DMO or Stack
!
! Even though GSTK calculates values for source and receiver surveyed 
! coordinate header words, source and receiver locations after DMO or stack are
! not well defined.
!
! If TVFSE paramter is greater than 0.0, the memory or disk size will be 
! doubled. For DMO stacking, TVFSE should be 0. 
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is an all-trace (loop-splitting) process.
!
! Traces may be input in any order.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process alters input traces.
!
! This process outputs one trace at a time.
!
! GSTK sets the offset header word (6) of output traces to the nominal value of
! their offset bin.
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
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GRID     grid transform                        used but not changed 
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 2       Head mute                  Reset
! 3       Current gather number      Set
! 4       Number within gather       Set
! 5       Fold                       Set
! 6       Offset                     Used and set
! 7       CMP x grid                 Used
! 8       CMP y grid                 Used
! 11      Source easting             Set
! 12      Source northing            Set
! 14      Receiver easting           Set
! 15      Receiver northing          Set
! 17      Midpoint easting           Set
! 18      Midpoint northing          Set
! 64      Tail mute                  Reset
!
! If headers 7 and 8 are used to set up bins, 17 and 18 are calculated using 
! the grid transform global and headers 11, 12, 14 and 15 are calculated from 
! 17, 18 and 6.
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author    Description
!     ----        ------    -----------
!014. 2006-06-20  B. Menger   Removed Unused Variables.
! 13. 2005-10-10  Stoeckley Added missing category STACKS.
! 12. 2002-03-07  Chiu      Change the NTIM default from 1 to 5000.
! 11. 2001-08-27  Chiu      Add "maxrecords" parameter to temptfile_open 
!                           to increase the file size if necessary.
! 10. 2001-06-18  Chiu      Replace all trcio calls by temptfile calls
!                           PRODUCTION.
! 9.  2001-05-14  Chiu      Fix frontend problem in pattern_stop2
! 8.  2001-03-21  Chiu      Add pc_get_update_state() /= PC_GUI in checking
!                           parameters.
! 7.  2001-02-14  Chiu      Change wrapped_up to skip_wrapup.
! 6.  2000-09-05  Chiu      Conform with stack creteria with front and end mute.
! 5.  2000-07-20  Chiu      Use new coordinate for header 7 and 8.      
! 4.  2000-06-14  Chiu      Add Gui.
! 3.  2000-03-21  Chiu      Remove PC_BACKEND check for pc_put_contol.
! 2.  2000-01-27  Chiu      header word 32 of filled in traces are set to -1.
! 1.  1999-12-07  Chiu      Convert into new CPS.
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
! This process uses a single set of trace and header arrays.
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
!    NTR == NEED_TRACES    if this process needs more traces.
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
!!  --> Description of this particular algorithm, related theory and
!!  --> relevant references.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS GSTK Process/NC=80>
!
!    Stack traces without requirement of sorting into stack gathers.
!
! NTIM=~~~`IIIIIIII      TVFSE=~~`IIIIIIII    
!
! TIM_BEG=`FFFFFFFFFFFF  TIM_END=`FFFFFFFFFFFF    
!
! OPT_MDT=`CCCC   
!
! `-----------------------------------------------------------------------
!  HDR_INL= `IIIIII
!
!  INL_INIT=`FFFFFFFFFFF    INL_INC=`FFFFFFFFFFF    INL_WID=`FFFFFFFFFFF
!  INL_LAST=`FFFFFFFFFFF    INL_TOT=`IIIIIIII  
!
!  HDR_CRL= `IIIIII
!
!  CRL_INIT=`FFFFFFFFFFF    CRL_INC=`FFFFFFFFFFF    CRL_WID=`FFFFFFFFFFF
!  CRL_LAST=`FFFFFFFFFFF    CRL_TOT=`IIIIIIII
!
!  HDR_OFF= `IIIIII
!  OFF_INIT=`FFFFFFFFFFF    OFF_INC=`FFFFFFFFFFF    OFF_WID=`FFFFFFFFFFF
!  OFF_LAST=`FFFFFFFFFFF    OFF_TOT=`IIIIIIII
!</gui_def>
!
!<HelpSection>
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
!<Help KEYWORD="TVFSE">
!<Tip> Time Varying Fold of Stack Exponential stack scaling method. </Tip>
! Default = 0.0
! Allowed = 0.0-1.0
! The Time Varying Fold of Stack Exponential method scales stacked traces by 
! dividing trace samples by the time varying number of live samples stacked 
! together raised to the TVFSE power.  TVFSE = 0.0 does no scaling.  This is an
! industry-standard method.
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Starting time for the stacked traces. </Tip>
! Default = TSTRT
! Allowed = real
! Stacked traces will be dead before TIM_BEG if TSTRT < TIM_BEG.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Ending time for the stacked traces. </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG
! Stacked traces will be dead after TIM_END if TIM_END < end of trace.
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
! FILL option should be used only when GSTK is a part of a DMO or other 
! migration process.
!</Help>
!
!<Help KEYWORD="HDR_INL">
!<Tip> Header word for inline coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="INL_INIT">
!<Tip> Value of HDR_INL for center of first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="INL_INC">
!<Tip> Increment between bins in inline direction. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="INL_WID">
!<Tip> Width of bins in inline direction. </Tip>
! Default = INL_INC
! Allowed = INL_INC >= real > 0.0
!</Help>
!
!<Help KEYWORD="INL_LAST">
!<Tip> Value of HDR_INL for center of last bin. </Tip>
! Default = 1.0
! Allowed = real >= INL_INIT
!</Help>
!
!<Help KEYWORD="INL_TOT">
!<Tip> Total number of bins in inline direction. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="HDR_CRL">
!<Tip> Header word for crossline coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="CRL_INIT">
!<Tip> Value of HDR_CRL for center of first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="CRL_INC">
!<Tip> Increment between bins in crossline direction. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="CRL_WID">
!<Tip> Width of bins in crossline direction. </Tip>
! Default = CRL_INC
! Allowed = CRL_INC >= real > 0.0
!</Help>
!
!<Help KEYWORD="CRL_LAST">
!<Tip> Value of HDR_CRL for center of last bin. </Tip>
! Default = 1.0
! Allowed = real >= CRL_INIT
!</Help>
!
!<Help KEYWORD="CRL_TOT">
!<Tip> Total number of bins in crossline direction. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word for offsets. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Value of HDR_OFF for center of first offset bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment between offset bins. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OFF_WID">
!<Tip> Width of offset bins. </Tip>
! Default = OFF_INC
! Allowed = OFF_INC >= real > 0.0
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Value of HDR_OFF for center of last offset bin. </Tip>
! Default = 1.0
! Allowed = real >= OFF_INIT
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offset bins. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
! Issues to deal with:
! 1.  Check on appropriate mute header values based on TIM_BEG and TIM_END. 
!     Should not reset NDPT.
! 2.  Please explain to me the sense behind the XXX_WID default value - CIB.
!     Should this kludge be removed??
! 3.  Is LINE_TOT = 1 a satisfactory flag for efficient binning for both 2D 
!     operation and one line out of a 3D survey?  Or should this changed?
! 4.  Be sure that GSTK sets header word 6 in output traces to the offset bin
!     center value.
! 5.  New grid global should be used to go from grid to surveyed headers and 
!     vice versa.
! 6.  TVFSE should use standard primitive.
! 7.  Look out for better way to handle disk swapping logic.  GSTK is now very 
!     slow.
! 8.  GSTK should not reset NDPT.
! 9.  Option for handling missing or dead input traces currently uses #PAR as a 
!     flag???  Should operate based on value of new OPT_MDT parameter.
!
!
!-------------------------------------------------------------------------------
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module gstk_module
      use pc_module
      use named_constants_module
      use grid_module             ! if you need the grid transformation.
      use string_module
      use mutehw_module
      use getsys_module
      use mth_module
      use lav_module
      use pattern_module
      use temptfile_module

      implicit none
      private
      public :: gstk_create     
      public :: gstk_initialize
      public :: gstk_update     
      public :: gstk_delete
!<execute_only>
      public :: gstk              ! main execution (trace processing) routine.
      public :: gstk_wrapup
      public :: gstk_set_init_value
!</execute_only>

      character(len=100),public,save :: gstk_IDENT = &
       '$Id: gstk.f90,v 1.14 2006/06/20 13:11:54 Menger prod sps $'

!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      real,parameter :: CONST = 1.e30
      integer, parameter :: MAX_LINE = 132 ! Maximum number of characters
                                           ! (columns) in one line

      type,public :: gstk_struct
 
        logical                    :: skip_wrapup       ! wrapup flag.
        integer                    :: ntim             ! process parameters. 
        real                       :: tim_beg          ! process parameters. 
        real                       :: tim_end          ! process parameters. 
        character(len=8)           :: opt_mdt          ! process parameters.
        real                       :: tvfse            ! process parameters.

        integer                    :: hdr_inL          ! process parameters.
        real                       :: inL_init         ! process parameters.
        real                       :: inL_inc          ! process parameters.
        real                       :: inL_wid          ! process parameters. 
        real                       :: inL_last         ! process parameters.
        integer                    :: inL_tot          ! process parameters.

        integer                    :: hdr_crl         ! process parameters. 
        real                       :: crl_init        ! process parameters. 
        real                       :: crl_inc         ! process parameters.
        real                       :: crl_wid         ! process parameters.
        real                       :: crl_last        ! process parameters.
        integer                    :: crl_tot         ! process parameters.

        integer                    :: hdr_off          ! process parameters.
        real                       :: off_init         ! process parameters.
        real                       :: off_inc          ! process parameters.
        real                       :: off_wid          ! process parameters.
        real                       :: off_last         ! process parameters. 
        integer                    :: off_tot          ! process parameters. 

        integer                    :: nwih,ndpt        ! globals.
        real                       :: dt,tstrt         ! globals.
        type(grid_struct)          :: grid             ! globals.

        character(len=160)         :: dafname,wtfname  ! dependent variables.
        integer                    :: nleft, nseq, nxy ! dependent variables. 
        integer                    :: nread, ninput    ! dependent variables. 
        integer           ,pointer :: nbfold(:)        ! dependent variables.
        integer           ,pointer :: nlive(:)         ! dependent variables.
        integer           ,pointer :: num(:)           ! dependent variables.
        real              ,pointer :: wsec(:,:)        ! dependent variables.
        real              ,pointer :: wtfold(:,:)      ! dependent variables.

        integer                    :: print_lun        ! dependent variables. 
        type(temptfile_struct),pointer :: dafile       ! dependent parameter.
        type(temptfile_struct),pointer :: wtfile       ! dependent parameter.

      end type gstk_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(gstk_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: opt_mdt_noptions = 3
      character(len=4),save :: opt_mdt_options(opt_mdt_noptions)
      data opt_mdt_options/'DEL','FILL', 'KILL'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine gstk_create (obj)
      implicit none
      type(gstk_struct),pointer :: obj       ! arguments

      allocate (obj)


!! Nullify ALL POINTERS in your parameter structure 
       
        nullify  (obj%nbfold)
        nullify  (obj%nlive)      
        nullify  (obj%num)         
        nullify  (obj%wsec)        
        nullify  (obj%wtfold)
        nullify  (obj%dafile)
        nullify  (obj%wtfile)      

      call gstk_initialize (obj)

      return
      end subroutine gstk_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine gstk_delete (obj)
      implicit none
      type(gstk_struct),pointer :: obj       ! arguments

!<execute_only>
      call gstk_wrapup (obj)
!</execute_only>

!! deallocate ALL POINTERS
    
        if (associated(obj%nbfold)) deallocate (obj%nbfold) 
        if (associated(obj%nlive))  deallocate (obj%nlive)    
        if (associated(obj%num))    deallocate (obj%num)        
        if (associated(obj%wsec))   deallocate (obj%wsec)      
        if (associated(obj%wtfold)) deallocate (obj%wtfold)

      deallocate(obj)

      return
      end subroutine gstk_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine gstk_initialize (obj)
      implicit none
      type(gstk_struct),intent(inout) :: obj       ! arguments

      call grid_initialize(obj%grid)

!! Initialize ALL NON-POINTER VARIABLES in your parameter structure

      obj%ntim      = 5000 
      obj%tvfse     = 0.0 
      obj%opt_mdt   = 'DEL'

      obj%hdr_inL   = 7 
      obj%inL_init  = 1.  
      obj%inL_inc   = 1.
      obj%inL_wid   = obj%inL_inc
      obj%inL_last  = 1
      obj%inL_tot   = 1

      obj%hdr_crl  = 8
      obj%crl_init = 1.
      obj%crl_inc  = 1.
      obj%crl_wid  = obj%crl_inc
      obj%crl_last = 1
      obj%crl_tot  = 1
 
      obj%hdr_off   = 6 
      obj%off_init  = 1. 
      obj%off_inc   = 1.
      obj%off_wid   = obj%off_inc 
      obj%off_last  = 1
      obj%off_tot   = 1

      obj%print_lun = pc_get_lun()
      call grid_initialize(obj%grid)

      call gstk_update (obj)

      return
      end subroutine gstk_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine gstk_update (obj)
      implicit none
      type(gstk_struct),intent(inout),target :: obj             ! arguments

!! Declare all required local variables 


      integer                    :: ier,j                              ! local
      logical                    :: gathered,iftd                      ! local
      integer                    :: nstore, nscratch                   ! local
      integer                    :: ntapes,ndisk                       ! local
      integer                    :: istatus                            ! local
      integer                    :: state                              ! local
      logical                    :: verify                             ! local
  
      character(len=3)           :: need_label,need_request            ! local
      character(len=3)           :: twosets                            ! local
      character(len=30)          :: seed                               ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('nwih'    , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'    , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('dt'      , obj%dt)    ! trace sample interval (sec).
      call pc_get_global ('tstrt'   , obj%tstrt) !time of 1st trace sample(sec).
      call pc_get_global ('grid'    , obj%grid)  ! grid transform structure.

      obj%tim_beg   = obj%tstrt 
      obj%tim_end   = obj%tstrt + (obj%ndpt - 1)*obj%dt

      call pc_get ('ntim'      , obj%ntim) 
      call pc_get ('tvfse'     , obj%tvfse)
      call pc_get ('tim_beg'   , obj%tim_beg)
      call pc_get ('tim_end'   , obj%tim_end)
      call pc_get ('opt_mdt'   , obj%opt_mdt)

      call pc_get ('hdr_inL'   , obj%hdr_inL)
      call pc_get ('inL_init'  , obj%inL_init)   
      call pc_get ('inL_inc'   , obj%inL_inc) 
      call pc_get ('inL_wid'   , obj%inL_wid)  
      call pc_get ('inL_last'  , obj%inL_last)   
      call pc_get ('inL_tot'   , obj%inL_tot)

      call pc_get ('hdr_crl'  , obj%hdr_crl)
      call pc_get ('crl_init' , obj%crl_init)  
      call pc_get ('crl_inc'  , obj%crl_inc) 
      call pc_get ('crl_wid'  , obj%crl_wid)   
      call pc_get ('crl_last' , obj%crl_last)   
      call pc_get ('crl_tot'  , obj%crl_tot)   

      call pc_get ('hdr_off'   , obj%hdr_off)
      call pc_get ('off_init'  , obj%off_init)  
      call pc_get ('off_inc'   , obj%off_inc) 
      call pc_get ('off_wid'   , obj%off_wid)    
      call pc_get ('off_last'  , obj%off_last)   
      call pc_get ('off_tot'   , obj%off_tot)    
 
      call string_to_upper (obj%opt_mdt)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%ntim <= 0) then 
        call pc_error('NTIM MUST BE > 0')  
      end if
 
      if(obj%tvfse < 0.0) then 
        call pc_error('TVFSE MUST BE > 0')  
      else if(obj%tvfse > 1.0) then 
        call pc_error('TVFSE MUST BE < 1.')  
      end if 

      if(obj%tim_end < obj%tim_beg) then 
        call pc_error('TIM_END MUST BE >= TIM_BEG')  
      end if

      if(obj%opt_mdt /='DEL' .and. obj%opt_mdt /='FILL'                      &
        .and. obj%opt_mdt /='KILL') then 
        call pc_error('OPT_MDT must be DEL, FILL or KILL')  
      end if

      if(obj%hdr_inL < 1) then 
        call pc_error('HDR_INL MUST BE >= 1')  
      else if(obj%hdr_inL > obj%nwih) then 
        call pc_error('HDR_INL MUST BE =< 64 ')  
      end if

      if(obj%inL_inc <= 0.0) then 
        call pc_error('INL_INC MUST BE > 0')  
      end if

      if(obj%inL_wid <= 0.0) then 
        call pc_error('INL_WID MUST BE > 0')  
      end if

      if(obj%inL_tot <= 0.0) then 
        call pc_error('INL_TOT MUST BE > 0 ')  
      end if

      if(obj%hdr_crl < 1) then 
        call pc_error('HDR_CRL MUST BE >= 1')  
      else if(obj%hdr_crl > obj%nwih) then 
        call pc_error('HDR_CRL MUST BE =< 64 ')  
      end if

      if(obj%crl_inc <= 0.0) then 
        call pc_error('CRL_INC MUST BE > 0')  
      end if

      if(obj%crl_wid <= 0.0) then 
        call pc_error('CRL_WID MUST BE > 0')  
      end if

      if(obj%crl_tot <= 0) then 
        call pc_error('CRL_TOT MUST BE > 0')  
      end if

      if(obj%hdr_off < 1) then 
        call pc_error('HDR_OFF MUST BE >= 1')  
      else if(obj%hdr_off > obj%nwih) then 
        call pc_error('HDR_OFF MUST BE =< 64 ')  
      end if

      if(obj%off_inc <= 0.0) then 
        call pc_error('OFF_INC MUST BE > 0 ')  
      end if

      if(obj%off_wid <= 0.0) then 
        call pc_error('OFF_WID MUST BE > 0 ')  
      end if

      if(obj%off_tot <= 0) then 
        call pc_error('OFF_TOT MUST BE > 0 ')  
      end if

       istatus = pattern_stop2('GSTK:', verify, &
       obj%inL_init, obj%inL_inc, obj%inL_last, obj%inL_tot, &
       'INL_INIT', 'INL_INC', 'INL_LAST', 'INL_TOT', &
       pc_verify_scalar('INL_INIT'), pc_verify_scalar('INL_INC'), &
       pc_verify_scalar('INL_LAST'), pc_verify_scalar('INL_TOT')) 

       istatus = pattern_stop2('GSTK:', verify, &
       obj%crl_init, obj%crl_inc, obj%crl_last, obj%crl_tot, &
       'CRL_INIT', 'CRL_INC', 'CRL_LAST', 'CRL_TOT', &
       pc_verify_scalar('CRL_INIT'), pc_verify_scalar('CRL_INC'), &
       pc_verify_scalar('CRL_LAST'), pc_verify_scalar('CRL_TOT')) 

       istatus = pattern_stop2('GSTK:', verify, &
       obj%off_init, obj%off_inc, obj%off_last, obj%off_tot, &
       'OFF_INIT', 'OFF_INC', 'OFF_LAST', 'OFF_TOT', &
       pc_verify_scalar('OFF_INIT'), pc_verify_scalar('OFF_INC'), &
       pc_verify_scalar('OFF_LAST'), pc_verify_scalar('OFF_TOT')) 
                                ! end trap

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('opt_mdt', opt_mdt_options,   &
                                   opt_mdt_noptions)

      gathered = .false.
      call pc_put_global ('gathered'  ,gathered)

      call pc_put ('ntim'      , obj%ntim) 
      call pc_put ('tvfse'     , obj%tvfse)
      call pc_put ('tim_beg'   , obj%tim_beg)
      call pc_put ('tim_end'   , obj%tim_end)
      call pc_put ('opt_mdt'   , obj%opt_mdt)

      call pc_put ('hdr_inL'   , obj%hdr_inL)
      call pc_put ('inL_init'  , obj%inL_init) 
      call pc_put ('inL_inc'   , obj%inL_inc)
      call pc_put ('inL_wid'   , obj%inL_wid)
      call pc_put ('inL_last'  , obj%inL_last) 
      call pc_put ('inL_tot'   , obj%inL_tot)

      call pc_put ('hdr_crl'  , obj%hdr_crl)
      call pc_put ('crl_init' , obj%crl_init)
      call pc_put ('crl_inc'  , obj%crl_inc) 
      call pc_put ('crl_wid'  , obj%crl_wid)
      call pc_put ('crl_last' , obj%crl_last) 
      call pc_put ('crl_tot'  , obj%crl_tot)

      call pc_put ('hdr_off'   , obj%hdr_off)
      call pc_put ('off_init'  , obj%off_init)
      call pc_put ('off_inc'   , obj%off_inc) 
      call pc_put ('off_wid'   , obj%off_wid) 
      call pc_put ('off_last'  , obj%off_last) 
      call pc_put ('off_tot'   , obj%off_tot)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!! Conditionally deallocate all arrays 
    
      if (associated(obj%nbfold)) deallocate (obj%nbfold) 
      if (associated(obj%nlive))  deallocate (obj%nlive)    
      if (associated(obj%num))    deallocate (obj%num)        
      if (associated(obj%wsec))   deallocate (obj%wsec)      
      if (associated(obj%wtfold)) deallocate (obj%wtfold)

      obj%nleft = 0 
      obj%nseq = 0 
      obj%nread = 0 
      obj%ninput = 0
 
        need_label   = 'YES'
        need_request = 'YES'
        twosets      = 'NO'
        iftd         = .false.
        ndisk        = 0
        ntapes       = 0 

        obj%nxy = obj%inL_tot*obj%crl_tot*obj%off_tot

        if( obj%tvfse > 0.0) then       ! permanent storage
          nstore = 2*obj%ndpt*obj%ntim + obj%ntim + 2*(obj%nxy+1)
        else 
          nstore = obj%ndpt*obj%ntim + obj%ntim + 2*(obj%nxy+1)
        end if 
        nscratch = 2048 + 2*obj%ntim + 2*obj%ndpt   ! scratch storage

        call pc_put_control ('nstore',             nstore)
        call pc_put_control ('nscratch',         nscratch)
        call pc_put_control ('need_label',     need_label)
        call pc_put_control ('need_request', need_request)
        call pc_put_control ('twosets',           twosets)
 
!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if (obj%inL_wid==obj%inL_inc .and. obj%crl_wid==obj%crl_inc  &
            .and. obj%off_wid==obj%off_inc) then 
        obj%inL_wid = abs(obj%inL_inc) + 1.0 
        obj%crl_wid = abs(obj%crl_inc) + 1.0 
        obj%off_wid = abs(obj%off_inc) + 1.0 
      endif 

      if (obj%crl_tot == 1) then
          obj%crl_inc = CONST
          obj%crl_wid = CONST*10.
      end if 
      if (obj%off_tot  == 1) then
          obj%off_inc  = CONST
          obj%off_wid  = CONST*10.
      end if 

      obj%inL_wid = 0.5*obj%inL_wid 
      obj%crl_wid = 0.5*obj%crl_wid 

!   Allocate your permanent memory
    
      allocate (obj%nbfold(obj%nxy+1),stat=ier)
      if (ier/=0) call pc_error ('Error creating GSTK:nbfold array')

      allocate (obj%nlive(obj%nxy+1),stat=ier)
      if (ier/=0) call pc_error ('Error creating GSTK:nlive array')
     
      allocate (obj%num(obj%ntim),stat=ier)
      if (ier/=0) call pc_error ('Error creating GSTK:num array')
      
      allocate (obj%wsec(obj%ndpt,obj%ntim),stat=ier)
      if (ier/=0) call pc_error ('Error creating GSTK:wsec array')
      
      if( obj%tvfse > 0.0) then
        allocate (obj%wtfold(obj%ndpt,obj%ntim),stat=ier)
        if (ier/=0) call pc_error ('Error creating GSTK:wtfold array')
      else
        allocate (obj%wtfold(1,1),stat=ier)
        if (ier/=0) call pc_error ('Error creating GSTK:wtfold array')
      end if

      obj%tim_beg = (obj%tim_beg-obj%tstrt)/obj%dt + 1
      obj%tim_end = (obj%tim_beg-obj%tstrt)/obj%dt + 1
      obj%tim_beg = min(obj%tim_beg,1.)
      obj%tim_end = max(obj%tim_end,real(obj%ndpt))
  
      obj%nbfold(:) = 0                            !clear fold of stack array
      obj%nlive(:)  = -1
      obj%num(1:obj%ntim) = (/(j,j=1,obj%ntim)/)
      obj%wsec(:,:) = 0.
      obj%wtfold(:,:) = 0.

      if (obj%nxy > obj%ntim) then  
        obj%num(1:obj%ntim) = obj%nxy + 1

!  Create a temporary file(s) to store data and optionally weight

        seed = 'gstk_dafile'
        call temptfile_open(obj%dafile, seed, obj%nwih, obj%ndpt,  &
         obj%print_lun, ier, maxrecords=obj%nxy)

!  Create a temporary file to store time varying weights

        if( obj%tvfse > 0.0) then
          seed = 'gstk_wtfile'
          call temptfile_open(obj%wtfile, seed, obj%nwih, obj%ndpt,  &
             obj%print_lun, ier, maxrecords=obj%nxy)
        end if   
      end if

      if (pc_do_not_process_traces()) return

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine gstk_update


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

      subroutine gstk (obj,ntr,hd,tr)
      implicit none
      type(gstk_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer                         :: jrec(1024)             ! local 
      integer                         :: nrec(1024)             ! local
      integer                         :: njv(obj%ntim)          ! local
      real                            :: dist(obj%ntim)         ! local
      real                            :: outbuf(obj%ndpt)       ! local
      real                            :: wtbuf(obj%ndpt)        ! local
      real                            :: rfold(obj%ndpt)        ! local



      logical                         :: found                  ! local
      integer                         :: iwt, ier               ! local
      integer                         :: j, nj, k, imute        ! local
      integer                         :: ifmute, ibmute         ! local
      integer                         :: kx,ky,kp,iof,iwa       ! local
      double precision                :: dummy(obj%nwih)        ! local

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call gstk_wrapup (obj)
      end if
 
      if (obj%nleft==0 .and. ntr>0) then 
        do k = 1, ntr 
          jrec(k) = 0 
!...  find nearest bin # 
          kx = nint((hd(obj%hdr_inL,k)-obj%inL_init)/obj%inL_inc)
          ky = nint((hd(obj%hdr_crl,k)-obj%crl_init)/obj%crl_inc) 
          kp = nint((hd(obj%hdr_off,k)-obj%off_init)/obj%off_inc)

          if (hd(25,k) == 0.0 .or. kx<0 .or. ky<0 .or. kp<0              &
            .or. kx>=obj%inL_tot .or. ky>=obj%crl_tot                    &
            .or. kp>=obj%off_tot                                         & 
            .or. abs(obj%inL_init+kx*obj%inL_inc-hd(obj%hdr_inL,k)) >    &
                obj%inL_wid                                              & 
            .or. abs(obj%crl_init+ky*obj%crl_inc-hd(obj%hdr_crl,k)) >    & 
                obj%crl_wid                                              &
            .or. abs(obj%off_init+kp*obj%off_inc-hd(obj%hdr_off,k))>     &
                obj%off_wid) cycle
  
!... jrec is record on disk,  nrec is record in workspace
 
          jrec(k) = (ky*obj%inL_tot + kx)*obj%off_tot + kp + 1 

          if ( hd(32,k) > 0.0 .and. jrec(k) > 0) then
            obj%nlive(jrec(k)) =  1
          end if
        end do 

      l2: do k = 1, ntr 
            if (jrec(k) == 0) cycle l2  
              do j = 1, obj%ntim 
                if (obj%num(j) /= jrec(k)) cycle  
                nrec(k) = j 
                cycle l2
              end do 
              nrec(k) = -1
       end do l2

        do k = 1, ntr 
          if (jrec(k) == 0) cycle 
 
!...  chech if already in memory 

          if (nrec(k)<=0 .or. obj%num(nrec(k))/=jrec(k)) then 

            obj%ninput = obj%ninput + 1

            if (obj%ninput > obj%ntim ) then
              found = .false.
              do j = 1, obj%ntim 
                if (obj%num(j) == jrec(k)) then  
                  nrec(k) = j 
                  found = .true.
                  exit 
                end if 
               end do
             else
               found = .true.
               nrec(k) = obj%ninput 
               obj%num(nrec(k)) = jrec(k)
             end if 

            if ( .not. found) then 

!...  not in memory, find farthest trace to remove 

              njv(1:obj%ntim) = (obj%num(1:obj%ntim)-1)/obj%off_tot
 
              dist(1:obj%ntim) = (obj%inL_init + obj%inL_inc                  &
               * (njv(1:obj%ntim)- njv(1:obj%ntim)/obj%inL_tot                &
               * obj%inL_tot) - hd(obj%hdr_inL,k))**2                         &
               + (obj%crl_init + obj%crl_inc                                  &
               * (njv(1:obj%ntim)/obj%inL_tot) - hd(obj%hdr_crl,k))**2 
                               
               nrec(k) = mth_ismax(obj%ntim,dist,1) 
               iof = 0 
               if (obj%nbfold(obj%num(nrec(k))) /= 0) then  
                 outbuf(1:obj%ndpt) = obj%wsec(1:obj%ndpt,nrec(k))
                 if (obj%tvfse > 0.0) then
                   wtbuf(1:obj%ndpt) = obj%wtfold(1:obj%ndpt,nrec(k))
                 end if
                 iof = 1
                 iwa = obj%num(nrec(k))  
               endif 
               obj%num(nrec(k)) = jrec(k) 

               if (obj%nbfold(jrec(k)) /= 0) then 
                 obj%nread = obj%nread + 1 

                 call temptfile_read(obj%dafile, jrec(k), dummy,    &
                     obj%wsec(:,nrec(k)), ier)

                 if (obj%tvfse > 0.0) then
                    call temptfile_read(obj%wtfile, jrec(k), dummy,    &
                     obj%wtfold(:,nrec(k)), ier)
   
                 end if
               else 
                 obj%wsec(1:obj%ndpt,nrec(k)) = 0.0 
                 if (obj%tvfse > 0.0) then
                   obj%wtfold(1:obj%ndpt,nrec(k)) = 0.0                   
                 end if
               endif
 
!...  write the trace to disk 

              if (iof == 1) then 
                 dummy = 0.  
                 call temptfile_write(obj%dafile, iwa, dummy,outbuf, ier) 
                 if (obj%tvfse > 0.0) then
                   call temptfile_write(obj%wtfile, iwa, dummy,wtbuf, ier) 
                 end if
              endif 
            endif

          end if
  
          ifmute = hd(2,k)
          ibmute = hd(64,k)
          obj%wsec(ifmute:ibmute,nrec(k)) = obj%wsec(ifmute:ibmute,nrec(k)) &
                                         +tr(ifmute:ibmute,k)          

!...  update the weight of tvfse scaling (optional). 

          iwt = 0
          if (obj%tvfse > 0.0) then
            if ( ifmute < ibmute ) then
              do j = ifmute, ibmute 
                 obj%wtfold(j,nrec(k)) = obj%wtfold(j,nrec(k)) + 1.
              end do
              iwt = 1
            end if
          else
            iwt = 1
          end if

          if ( iwt == 1) then
            obj%nbfold(jrec(k)) = obj%nbfold(jrec(k)) + 1
          end if

        end do
        ntr = NEED_TRACES 
        return  
      endif
      
 100  continue

!... output phase 

      obj%nleft = obj%nxy 

      if (obj%nseq >= obj%nleft) then 
        ntr = NO_MORE_TRACES
        write(obj%print_lun, *) '  gstk complete--# disk swaps =', obj%nread 
        if (obj%nxy > obj%ntim) then 
          call temptfile_close (obj%dafile)          
          if (obj%tvfse > 0.0) then
            call temptfile_close (obj%wtfile)
          end if
        end if
        return  
      endif 

      obj%nseq = obj%nseq + 1 
  
      if (obj%nlive(obj%nseq) <= 0 .and. obj%opt_mdt == 'DEL' ) go to 100

      if (obj%nlive(obj%nseq) <= 0 .and. obj%opt_mdt == 'KILL' ) then
         tr(1:obj%ndpt,1) = 0.0 
         imute = obj%ndpt 
         found = .true.
 
      else if (obj%nbfold(obj%nseq) > 0 .or. obj%opt_mdt == 'FILL') then

!... local the output trace from memory buffet.

        found = .false.
        do j = 1, obj%ntim 
          if (obj%num(j) == obj%nseq) then
             found = .true.
             outbuf(1:obj%ndpt) = obj%wsec(1:obj%ndpt,j)
             if (obj%tvfse > 0.0) then 
               wtbuf(1:obj%ndpt) = obj%wtfold(1:obj%ndpt,j)
             end if 
             exit
          end if
        end do


!... read for output trace from disk          

        if ( .not. found) then
          ! need to zero outbuf if trace is not in disk.
          outbuf = 0.0
          if (obj%nbfold(obj%nseq) > 0) then
             call temptfile_read(obj%dafile, obj%nseq, dummy,outbuf, ier)
            if (obj%tvfse > 0.0) then 
              wtbuf = 0.0
              call temptfile_read(obj%wtfile, obj%nseq, dummy, wtbuf, ier)
            end if
            obj%nread = obj%nread + 1 
          end if 
        end if

        tr(1:obj%ndpt,1) = outbuf(1:obj%ndpt)
        if (obj%tvfse > 0.0) then
          rfold(1:obj%ndpt) = wtbuf(1:obj%ndpt)
        end if  
       end if
!
!... apply tvfse scale 

       if( obj%tvfse > 0.0 ) then        ! scale form reciprocal of local fold
         where (rfold /= 0.0)
           rfold = 1. / rfold
         else where
           rfold = 1.
         end where
         tr(:,1) = tr(:,1) * rfold(:)**obj%tvfse           
       endif 

       do imute = 1, obj%ndpt
         if( tr(imute,1) /= 0.0) exit
       end do

!... update the necessary headers

      hd(1:obj%nwih,1) = 0. 
      hd(1,1) = obj%nseq 
      hd(2,1) = max(real(imute), obj%tim_beg)
      hd(32,1) = -1.
      if (obj%nlive(obj%nseq) > 0 ) hd(32,1) = obj%nbfold(obj%nseq)

      if (imute < obj%ndpt) then
        do j = obj%ndpt, imute, -1 
          if (tr(j,1) == 0.0) cycle  
          exit  
        end do 
        hd(64,1) = min(real(j), obj%tim_end)
      else 
        hd(64,1) = obj%ndpt  
      endif 

!      call mutehw (hd(:,1), tr(:,1), obj%ndpt, 0.0, 1) 

      if( hd(2,1) < obj%ndpt ) then
        call lav_set_hdr (hd, tr, obj%ndpt, 1)    
      end if

      ! dead trace 
      if ( hd(2,1) >= obj%ndpt) then
        if (obj%nlive(obj%nseq) <= 0 .and. obj%opt_mdt /= 'KILL') go to 100
        hd(25,1) = 0.0 
      end if

      hd(5,1) = obj%nbfold(obj%nseq) 

      nj = (obj%nseq - 1)/(obj%off_tot) 
      if (obj%off_tot > 1) then 
        hd(3,1) = nj + 1 
      else 
        hd(3,1) = (obj%nseq - 1)/(obj%inL_tot) + 1 
        hd(4,1) = obj%nseq - (hd(3,1)-1)*obj%inL_tot 
      endif 

      if (obj%crl_inc /= CONST) hd(obj%hdr_crl,1) =                         &
           obj%crl_init + obj%crl_inc*(nj/int(obj%inL_tot)) 

      hd(obj%hdr_inL,1) = obj%inL_init                                        &
            + obj%inL_inc*(nj - (nj/int(obj%inL_tot))*obj%inL_tot)            

      hd(obj%hdr_off,1) =                                                     &
            obj%off_init + obj%off_inc*(obj%nseq - 1 - nj*int(obj%off_tot)) 

      if (obj%hdr_inL==7 .or. obj%hdr_inL==8 .or. obj%hdr_crl==7             &
          .or. obj%hdr_crl==8) then 
        call grid_get_survey_coords(obj%grid, hd(7,1), hd(8,1),   &
                               hd(17,1), hd(18,1) )

      else if (obj%hdr_inL==17 .or. obj%hdr_inL==18 .or. obj%hdr_crl==17     &
          .or. obj%hdr_crl==18) then 
        call grid_get_grid_coords(obj%grid, hd(17,1), hd(18,1),   &
                               hd(7,1), hd(8,1) )
      endif 

      if (obj%hdr_off==6 .or. obj%hdr_crl==6 .or. obj%hdr_inL==6) then 
        hd(11,1) = hd(17,1) - 0.5*hd(6,1) 
        hd(12,1) = hd(18,1) 
        hd(14,1) = hd(17,1) + 0.5*hd(6,1) 
        hd(15,1) = hd(18,1) 
      endif 
        
      ntr = 1 
      return 

      end subroutine gstk


      subroutine gstk_set_init_value(obj, off_init) 
      implicit none
      type(gstk_struct),intent(inout) :: obj                   ! arguments
      real             ,intent(in)    :: off_init              ! arguments

      obj%off_init = off_init
      return 
      end subroutine gstk_set_init_value

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine gstk_wrapup (obj)
      implicit none
      type(gstk_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine gstk_wrapup

!</execute_only>



!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module gstk_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

