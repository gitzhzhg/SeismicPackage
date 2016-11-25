!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-07-19. />

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
! Name       : DRC      (2-D wave-equation Data ReConstruction)
! Category   : migrations
! Written    : 1999-04-16   by: Stephen Chiu and Bob Stolt
! Revised    : 2006-11-14   by: D. Glover
! Maturity   : production
! Purpose    : Reconstruct an offset from traces in the same and nearby CMPs.
! Parallel   : NO
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! DRC reconstructs one offset trace from a number of input offset traces.  This 
! implementation is Kirchhoff-like: To construct data at a particular midpoint 
! and new offset, data from a range of midpoints and existing offsets are 
! time-shifted, rescaled, summed and phase-shifted.
!
!
! Reference: 
!
! Robert Stolt, Data reconstruction and reflectivity mapping in 3-D, Conoco
! Research Report #2558-1-98, 1998.
! 
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
! Input data should be CMP gathers sorted in order of increasing CMP and   
! offsets. The input data is assumed to have spherical divergence and NMO  
! correction applied.  Input data should be single trace.
!
!
! Output Data
!
! Output from DRC is the reconstructed traces only.  Output is sorted in order 
! of increasing offset within a CMP.  Output data is single trace.
!
!
! Header Words
!
! If HDR_CMP = 7, then trace header words 7, 11, 14 and 17 will be modified by 
! DRC and others will remain unchanged.
!
! If HDR_CMP = 8, then trace header words 8, 12, 15 and 18 will be modified by
! DRC and others will remain unchanged.
!
!
! Mute
!
! Mute is not restored within DRC, normally it should be reapplied after DRC.
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
! Input data should be CMP gathers sorted in order of increasing offsets. The 
! input data is assumed to have spherical divergence and NMO correction 
! applied.  Input data should be single trace.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS             
!
! 
! This process outputs the reconstructed traces only.  Output is in single
! traces.
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
!
! 1       Sequential Trace Count     Renumbered.
! 3       Current gather             Renumbered.
! 4       Number within gather       Renumbered.
!
! If HDR_CMP = 7, then trace header words 7, 11, 14 and 17 will be modified by 
! DRC and others will remain unchanged.
!
! If HDR_CMP = 8, then trace header words 8, 12, 15 and 18 will be modified by
! DRC and others will remain unchanged.
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author        Description
!     ----       ------        -----------
!10. 2006-11-14  D. Glover     Added NULLIFY statements for Intel compiler.
! 9. 2006-06-06  Stoeckley     Add pc_register_array_names for SeisSpace.
! 8. 2006-01-10  B. Menger     Removed Unused Variables.
! 7.  2002-03-25 Chiu          Fix first CMP problem of zero traces
! 6.  2002-03-12 Chiu          Change CMP_TOT default from 1 to 100 and
!                              add error check on CMP_TOT
! 5.  2002-03-01 Chiu          Fix the excessive printout in RPT file
!                              and CI Burch rearranges GUI, add OFF_TOT
! 4.  2000-12-07 Chiu          Change wrapped_up to skip_wrapup.
! 3.  2000-07-21 Chiu          Update C.I Burch's new Doc. 
! 2.  2000-07-07 Chiu          Convert to new CPS.
! 1.  1999-04-16 Chiu          Original version  
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
!  THIS PROGRAM SHOULD BE COMPILED WITH -O2 OPTIMIZATION AND NO DEBUG.
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
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
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
!
!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS DRC Process/NC=80>
!               2-D wave-equation Data ReConstruction Process
!        Reconstruct an offset from traces in the same and nearby CMPs.
!
!  
!   HDR_CMP=~~~~~`IIIIIIIIII       CMP_TOT~~~~~~=`IIIIIIIIIII
!   DIST_CMP_IN =`FFFFFFFFFFF      DIST_CMP_OUT =`FFFFFFFFFFF 
!   TIM_BEG~~~~~=`FFFFFFFFFFF      TIM_END~~~~~~=`FFFFFFFFFFF   
!   NUM_OFF~~~~~=`IIIIIIIIIII      TYPE_GAIN~~~~=`CCCCCC
!
!
!      Input Offsets                  Output Offsets     
!   `----------------------        `--------------------------
!    OFF_INIT=`FFFFFFFFFFF          OFF_REC_INIT=`FFFFFFFFFFF
!    OFF_INC =`FFFFFFFFFFF          OFF_REC_INC =`FFFFFFFFFFF
!    OFF_LAST=`FFFFFFFFFFF          OFF_REC_LAST=`FFFFFFFFFFF
!    OFF_TOT =`IIIIIIIIIII          OFF_REC_TOT =`IIIIIIIIIII
!   `----------------------        `--------------------------
!  
!
!   TIMES       DIPS        VELS
!   `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!   `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!   `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!   `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!<PARMS TIMES_ARRAYSET[/XST/YST]>
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="HDR_CMP">
!<Tip> Header word designating CMPs in a 2D line. </Tip>
! Default = 7
! Allowed = 7, 8
! HDR_CMP should be either 7 or 8.
!</Help>
!
!<Help KEYWORD="CMP_TOT"> 
!<Tip> CMP_TOT should be >= to actual number of CMPs in a 2D line. </Tip>
! Default = 100
! Allowed = int > 0
! Maximum number of CMP used in memory allocation. 
!</Help>
!
!<Help KEYWORD="DIST_CMP_IN">
!<Tip> Physical distance associated with unit change in HDR_CMP. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! DIST_CMP_IN is the physical distance in feet or meters associated with a
! change in HDR_CMP of 1.0.  It is not necessarily the distance between  
! actual CMPs (which may be associated with a change of HDR_CMP of 2.0 
! or more).
!</Help>
! 
!<Help KEYWORD="DIST_CMP_OUT">
!<Tip> Physical distance of increment between CMPs for output data. </Tip>
! Default = DIST_CMP_IN
! Allowed = real > 0.0 
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Top of window, in seconds, for computing normalizing factor. </Tip>
! Default = TSTRT
! Allowed = real 
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Bottom of window, in seconds, for computing normalizing factor. </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG 
!</Help>
!
!<Help KEYWORD="TYPE_GAIN">
!<Tip> Type of gain calculation to use. </Tip>
! Default = OFFSET
! Allowed = OFFSET 
! Allowed = CMP  
! If TYPE_GAIN = OFFSET, then use one gain factor for each offset panel (this 
! choice should be used for regular spacing of input offsets.)
!
! If TYPE_GAIN = CMP, then use one gain factor for each CMP gather (this choice 
! should be used for irregular input offset spacing).
!</Help>
!
!<Help KEYWORD="NUM_OFF">
!<Tip> Number of input offsets used to construct one (new) output offset. </Tip>
! Default = 8
! Allowed = 1 < int < 30  
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Minimum offset in input data to use for reconstruction. </Tip>
! Default =  -
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Maximum offset in input data to use for reconstruction. </Tip>
! Default =  -
! Allowed = real > OFF_INIT
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Offset increment for input data. </Tip>
! Default =  -
! Allowed = real > 
! Normally this will be 2 * source increment for marine data and equal to 
! source increment for land split spread data.
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offsets in input data to use for reconstruction. </Tip>
! Default =  -
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="OFF_REC_TOT">
!<Tip> Number of output offsets to be reconstructed. </Tip>
! Default = 1
! Allowed = int > 0 
!</Help>
! 
!<Help KEYWORD="OFF_REC_INIT">
!<Tip> First output offset to be reconstructed. </Tip>
! Default = 1.0
! Allowed = real > 0 
!</Help>
!
!<Help KEYWORD="OFF_REC_INC">
!<Tip> Increment between output offsets to be reconstructed. </Tip>
! Default = 1.0
! Allowed = real > 0 
!</Help>
!
!<Help KEYWORD="OFF_REC_LAST">
!<Tip> Last output offset to be reconstructed. </Tip>
! Default = 1.0
! Allowed = real > 0 
!</Help>
!
!<Help KEYWORD="TIMES">
!<Tip> Array of times for specifying maximum dip and Vrms. </Tip>
! Default =  -
! Allowed = real (linked array)
! If only one time is entered then traces are assumed to have time-invariant 
! maximum dip and Vrms.
!</Help>
!
!<Help KEYWORD="DIPS">
!<Tip> Array of maximum dips at specified times. </Tip>
! Default =  -
! Allowed = real > 0 (linked array)
!</Help>
!
!<Help KEYWORD="VELS">
!<Tip> Array of Vrms at specified times. </Tip>
! Default =  -
! Allowed = real > 0 (linked array)
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
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module drc_module
      use pc_module
      use named_constants_module

      use pc_module
      use named_constants_module
      use getlun_module
      use pathcheck_module
      use string_module
      use lav_module
      use fft_module
      use pattern_module
      use grid_module
      use binsort_module
      Use sort_module
      Use interp_module

      implicit none
      private
      public :: drc_create
      public :: drc_initialize
      public :: drc_update
      public :: drc_delete
!<execute_only>
      public :: drc            ! main execution (trace processing) routine.
      public :: drc_wrapup
!</execute_only>


      character(len=100),public,save :: drc_IDENT = &
       '$Id: drc.f90,v 1.10 2006/11/14 14:32:52 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    integer, parameter :: ISMPL = 4
    integer, parameter :: LSQT = 10000
    real,    parameter :: EPS = 1.e-5 
    integer, parameter :: MAX_PTS = 200
                                          
    type,public :: drc_struct              
 
    private
    logical               :: skip_wrapup               ! wrapup flag.
    logical               :: gathered                 ! gathered flag.
    integer               :: nwih, ndpt               ! Gobal variables.
    real                  :: dt, tstrt                ! Gobal variables.

    character (len=8)     :: type_gain                ! Process parameter.
    integer               :: hdr_cmp                  ! Process parameter.
    integer               :: cmp_tot                  ! Process parameter. 
    real                  :: cmp_init                 ! Process parameter.
    real                  :: dist_cmp_in              ! Process parameter.
    real                  :: dist_cmp_out             ! Process parameter. 
    real                  :: tim_beg                  ! Process parameter.
    real                  :: tim_end                  ! Process parameter.
    integer               :: num_off                  ! Process parameter.
    integer               :: off_tot                  ! Process parameter.
    real                  :: off_init                 ! Process parameter.
    real                  :: off_last                 ! Process parameter.
    real                  :: off_inc                  ! Process parameter.
    integer               :: off_rec_tot              ! Process parameter.
    real                  :: off_rec_init             ! Process parameter.
    real                  :: off_rec_inc              ! Process parameter.
    real                  :: off_rec_last             ! Process parameter.
    real, dimension(MAX_PTS) :: times                 ! process parameter.
    real, dimension(MAX_PTS) :: dips                  ! Process parameter. 
    real, dimension(MAX_PTS) :: vels                  ! Process parameter.
  

    integer               :: imhdr                    ! dependent variables.
    integer               :: numin                    ! dependent variables.
    integer               :: numout                   ! dependent variables.
    integer               :: iflow                    ! dependent variables.
    logical               :: eo_data                  ! dependent variables.
    integer               :: koff_out                 ! dependent variables.
    integer               :: itstrt                   ! dependent variables.
    integer               :: lwin                     ! dependent variables.

    integer               :: nrefl                    ! dependent variables.
    integer               :: maxfold                  ! dependent variables.
    integer               :: itwin                    ! dependent variables.  
    integer               :: ibwin                    ! dependent variables. 
    integer               :: noin                     ! dependent variables.
    integer               :: nxd2                     ! dependent variables.
    integer               :: nnop                     ! dependent variables.
    integer               :: nvel                     ! dependent variables.
    integer               :: nfft1                    ! dependent variables.
    integer               :: nfft2                    ! dependent variables. 
    integer               :: ixo,ixi                  ! dependent variables.
    integer               :: ndpt_new                 ! dependent variables.
    integer               :: print_lun                ! dependent variables.
    integer               :: cmp_tot_inpt             ! dependent variables.
    
    real                  :: toloff                   ! dependent variables.
    real                  :: dtpi                     ! dependent variables.
    real                  :: xr                       ! dependent variables.
    real                  :: xmin_sep                 ! dependent variables.
    real                  :: xin_old,xl1              ! dependent variables.
    real                  :: scf0,xout                ! dependent variables.
    real                  :: cmpmax                   ! dependent variables.
    real                  :: off_extn                 ! dependent variables.

    real                  :: cmp_old                  ! dependent variables.
    
    double precision, pointer :: hdr_store(:,:,:)     ! dependent variables.

    integer,   pointer    :: hdr_cnt(:)               ! dependent variables.
    integer,   pointer    :: indx_save(:)             ! dependent variables. 

    real,      pointer    :: rmsamp_in(:)             ! dependent variables.
    real,      pointer    :: rmscnt_in(:)             ! dependent variables.
    real,      pointer    :: rmsamp_out(:)            ! dependent variables.
    real,      pointer    :: rmscnt_out(:)            ! dependent variables.
    real,      pointer    :: rmswt(:)                 ! dependent variables.

    real,      pointer    :: tr_save(:)               ! dependent variables.
    real,      pointer    :: tr_store(:,:,:)          ! dependent variables.
    real,      pointer    :: dip_ndpt(:)              ! dependent variables.
    real,      pointer    :: vel_ndpt(:)              ! dependent variables.
    real,      pointer    :: fil45(:)                 ! dependent variables.
    real,      pointer    :: offin(:)                 ! dependent variables.
    real,      pointer    :: offrc(:)                 ! dependent variables.
    real,      pointer    :: offmap(:)                ! dependent variables.
    real,      pointer    :: sqt(:)                   ! dependent variables.
    real,      pointer    :: xa_save(:)               ! dependent variables.

    type(grid_struct)        :: grid                  ! dependent variables.
    type(fft_struct),pointer :: fftrc_obj             ! dependent variables.
    type(fft_struct),pointer :: fftcr_obj             ! dependent variables. 
    type(binsort_struct),pointer :: binsort           ! dependent variables.

      end type drc_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(drc_struct),pointer,save :: object      ! needed for traps.

      integer,parameter      :: type_gain_noptions = 2
      character(len=8),save  :: type_gain_options(type_gain_noptions)  &
                                =  (/'OFFSET  ','CMP     '/)
      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine drc_create (obj)
      implicit none
      type(drc_struct),pointer :: obj       ! arguments

      allocate (obj)


!!  Nullify ALL POINTERS in your parameter structure 

      nullify  (obj%hdr_store)
      nullify  (obj%hdr_cnt)
      nullify  (obj%indx_save)
      nullify  (obj%tr_store)
      nullify  (obj%xa_save)

      nullify  (obj%rmsamp_in)
      nullify  (obj%rmscnt_in)
      nullify  (obj%rmsamp_out)
      nullify  (obj%rmscnt_out)
      nullify  (obj%rmswt)       

      nullify  (obj%tr_save)
      nullify  (obj%dip_ndpt)
      nullify  (obj%vel_ndpt)
      nullify  (obj%fil45)
      nullify  (obj%offin)
      nullify  (obj%offrc)
      nullify  (obj%offmap)
      nullify  (obj%sqt)
           
      nullify  (obj%fftrc_obj)
      nullify  (obj%fftcr_obj)
      nullify  (obj%binsort) ! jpa

      call drc_initialize (obj)
      return
      end subroutine drc_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine drc_delete (obj)
      implicit none
      type(drc_struct),pointer :: obj       ! arguments

!<execute_only>
      call drc_wrapup (obj)
!</execute_only>

!! Make sure ALL POINTERS in your parameter structure are deallocated

      if (associated(obj%hdr_store))   deallocate (obj%hdr_store)
      if (associated(obj%hdr_cnt))     deallocate (obj%hdr_cnt)
      if (associated(obj%indx_save))   deallocate (obj%indx_save)
      if (associated(obj%tr_store))    deallocate (obj%tr_store)
      if (associated(obj%xa_save))     deallocate (obj%xa_save)

      if (associated(obj%rmsamp_in))   deallocate (obj%rmsamp_in) 
      if (associated(obj%rmscnt_in))   deallocate (obj%rmscnt_in)
      if (associated(obj%rmsamp_out))  deallocate (obj%rmsamp_out)
      if (associated(obj%rmscnt_out))  deallocate (obj%rmscnt_out)
      if (associated(obj%rmswt))       deallocate (obj%rmswt)
      if (associated(obj%tr_save))     deallocate (obj%tr_save)
      if (associated(obj%dip_ndpt))    deallocate (obj%dip_ndpt)
      if (associated(obj%vel_ndpt))    deallocate (obj%vel_ndpt)
      if (associated(obj%fil45))       deallocate (obj%fil45)
      if (associated(obj%offin))       deallocate (obj%offin)
      if (associated(obj%offrc))       deallocate (obj%offrc)
      if (associated(obj%offmap))      deallocate (obj%offmap)
      if (associated(obj%sqt))         deallocate (obj%sqt)

      if (associated(obj%fftrc_obj))  call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj))  call fft_delete (obj%fftcr_obj)

      deallocate(obj)
      return
      end subroutine drc_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine drc_initialize (obj)
      implicit none
      type(drc_struct),intent(inout) :: obj       ! arguments

      call pc_get_global ('ndpt'  , obj%ndpt)    
      call pc_get_global ('dt'    , obj%dt)      
      call pc_get_global ('tstrt' , obj%tstrt)     
 
      obj%hdr_cmp      = 7               
      obj%cmp_tot      = 100
      obj%cmp_init     = 1.
      obj%dist_cmp_in  = 1.
      obj%dist_cmp_out = obj%dist_cmp_in
      obj%tim_beg      = obj%tstrt 
      obj%tim_end      = obj%tstrt + (obj%ndpt-1)*obj%dt
      obj%type_gain    = 'OFFSET'
      obj%num_off      = 8
      obj%off_tot      = 1
      obj%off_init     = 1.
      obj%off_last     = 1.
      obj%off_inc      = 1.
      obj%off_rec_tot  = 1
      obj%off_rec_init = 1.0
      obj%off_rec_inc  = 1.0
      obj%off_rec_last = 1.0
      obj%times        = 0.0
      obj%dips         = 0.0
      obj%vels         = 0.0
      obj%nvel         = 1
      obj%times(1)     = 1.
      obj%dips(1)      = 60.
      obj%vels(1)      = 1500

      obj%print_lun = pc_get_lun()
      call grid_initialize(obj%grid)

      call drc_update (obj)
      return
      end subroutine drc_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine drc_update (obj)
      implicit none
      type(drc_struct),intent(inout),target :: obj               ! arguments

      logical                    :: gathered                     ! local
      character(len=3)           :: need_label,need_request      ! local
      character(len=3)           :: twosets                      ! local
      integer                    :: ier,numtr,npts               ! local
      integer                    :: nstore, nscratch             ! local
      integer                    :: state                        ! local
      integer                    :: nvel_pts2, nvel_pts3         ! local
      integer                    ::     iw, nw, npanel ! local
      integer                    :: nxd, nx                      ! local
      real                       :: wh                           ! local

      logical                    :: verify                       ! local
      integer                    :: i_stat                       ! local

      object => obj                     ! needed for traps.
      obj%skip_wrapup = .true.          ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if
 
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("times_arrayset", (/  &
                                    "times",              &
                                    "dips ",              &
                                    "vels " /))

      nvel_pts2 = obj%nvel
      nvel_pts3 = obj%nvel

      call pc_get_global ('nwih'  , obj%nwih)   ! number of header words.
      call pc_get_global ('ndpt'  , obj%ndpt)   ! number of trace samples.
      call pc_get_global ('dt'    , obj%dt)     ! trace sample interval (sec).
      call pc_get_global ('tstrt' , obj%tstrt)  ! time of 1st sample (sec).
      call pc_get_global ('grid'  , obj%grid)   ! grid transform data structure.

      call pc_get_global  ('numtr'       , numtr)            
      call pc_get_global  ('gathered'    , gathered)

      call pc_get ('HDR_CMP',      obj%hdr_cmp)
      call pc_get ('CMP_TOT',      obj%cmp_tot)
      call pc_get ('DIST_CMP_IN',  obj%dist_cmp_in)
      call pc_get ('DIST_CMP_OUT', obj%dist_cmp_out)
      call pc_get ('TIM_BEG',      obj%tim_beg)
      call pc_get ('TIM_END',      obj%tim_end)
      call pc_get ('TYPE_GAIN',    obj%type_gain)

      call pc_get ('NUM_OFF',      obj%num_off)
      
      call pc_get ('OFF_TOT',      obj%off_tot)
      call pc_get ('OFF_INIT',     obj%off_init)
      call pc_get ('OFF_LAST',     obj%off_last)
      call pc_get ('OFF_INC',      obj%off_inc)

      call pc_get ('OFF_REC_TOT',  obj%off_rec_tot)
      call pc_get ('OFF_REC_INIT', obj%off_rec_init)
      call pc_get ('OFF_REC_INC',  obj%off_rec_inc)
      call pc_get ('OFF_REC_LAST', obj%off_rec_last)

      call pc_get ('TIMES',        obj%times,  obj%nvel)
      call pc_get ('DIPS',         obj%dips,   nvel_pts2)
      call pc_get ('VELS',         obj%vels,   nvel_pts3)

      call string_to_upper (obj%type_gain)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


    if (obj%hdr_cmp < 7 .or. obj%hdr_cmp > 8) then   
       call pc_error (' DRC: HDR_CMP is either 7 or 8' )
    end if 

    if (obj%cmp_tot <= 0 ) then   
       call pc_error (' DRC: CMP_TOT must be > 0' )
    end if 

    if (obj%dist_cmp_in <= 0.0) then    
      call pc_error (' DRC: DIST_CMP_IN must be > 0.0' )
    end if
  
    if (obj%dist_cmp_out <= 0.0) then   
      call pc_error (' DRC: DIST_CMP_OUT must be > 0.0' )
     end if 

    if (obj%tim_beg < 0.0) then   ! real 
      call pc_error (' DRC: TIM_BEG must be >= 0.0' )
    end if 

    if (obj%tim_end < obj%tim_beg) then   
      call pc_error (msg1 = 'DRC: TIM_END must be >= TIM_BEG ')
    end if 

    if (obj%tim_end > obj%dt*(obj%ndpt-1)) then   
      call pc_error (' DRC: TIM_END must be <= trace length of', &
         obj%dt*(obj%ndpt-1))
    end if 

     if(obj%type_gain /= 'OFFSET'.and. obj%type_gain /= 'CMP') then
        call pc_error('DRC: TYPE_GAIN MUST BE CMP OR OFFSET')
     end if
 
     if (obj%num_off < 1 .or. obj%num_off > 30) then    
       call pc_error (' DRC: NUM_OFF is out of the range 1-30 ')
     end if 

    i_stat = pattern_stop2('DRC:', verify, &
    obj%off_rec_init, obj%off_rec_inc, obj%off_rec_last, obj%off_rec_tot, &
   'OFF_REC_INIT', 'OFF_REC_INC', 'OFF_REC_LAST', 'OFF_REC_TOT', &
    pc_verify_scalar('OFF_REC_INIT'), pc_verify_scalar('OFF_REC_INC'), &
    pc_verify_scalar('OFF_REC_LAST'), pc_verify_scalar('OFF_REC_TOT')) 

    i_stat = pattern_stop2('kdmig:', verify, &
    obj%off_init, obj%off_inc, obj%off_last, obj%off_tot, &
    'off_init',   'off_inc',   'off_last',   'off_tot', &
    pc_verify_scalar('off_init'), pc_verify_scalar('off_inc'), &
    pc_verify_scalar('off_last'), pc_verify_scalar('off_tot'))

    if (obj%off_init < 0.0) then   
      call pc_error (' DRC: OFF_INIT must be >= 0.0' ) 
    end if

    if (obj%off_last < obj%off_init) then   
      call pc_error (' DRC: OFF_LAST must be >= OFF_INIT'  )
    end if 

    if (obj%off_tot <= 0) then    
      call pc_error (' DRC: OFF_TOT must be > 0 ') 
    end if 
      
    if (obj%off_inc <= 0.0) then    
      call pc_error (' DRC: OFF_INIT must be > 0.0' )
    end if 

    if (obj%off_rec_tot <= 0) then    
       call pc_error (' DRC: OFF_REC_TOT must be > 0 ') 
    end if 

    if (obj%off_rec_init < 0.0) then    
      call pc_error (' DRC: OFF_REC_INIT must be >= 0.0')
     end if 

    if (obj%off_rec_inc <= 0.0) then   
      call pc_error (' DRC: OFF_REC_INC must be > 0.0') 
    end if 

    if (obj%off_rec_last < obj%off_rec_inc) then         
      call pc_error (' DRC: OFF_REC_LAST must be >= OFF_REC_INIT'  )
    end if 

    if(obj%nvel /= nvel_pts2 .or.            &
      obj%nvel /= nvel_pts3) then
      call pc_error(' DRC: arrays not linked (TIMES, DIPS, VELS)')
    end if

    if (obj%times(1) < 0.0) then   ! real (linked array)
      call pc_error (' DRC: TIMES(1) must be >= 0.0')
    end if 

    if (obj%dips(1) < 0.0) then   ! real (linked array)
      call pc_error (' DRC: DIPS(1) must be >= 0.0' )
    end if 

    if (obj%vels(1) <= 0.0) then   ! real > 0 (linked array)
      call pc_error  (' DRC: VELS(1) must be > 0.0' ) 
    end if 

    if(numtr > 1) then
       call pc_error  (' DRC: Require a single trace input ')
    end if    

    if ( obj%nvel > MAX_PTS ) then
       call pc_error  (' DRC: See programmer - TIMES Array > limit of ', &
         MAX_PTS )
    end if  

 
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('TYPE_GAIN', type_gain_options,   &
                                  type_gain_noptions) 

      gathered = .false.
      call pc_put_global  ('gathered'    , gathered)         

      call pc_put ('HDR_CMP',      obj%hdr_cmp)
      call pc_put ('CMP_TOT',      obj%cmp_tot)
      call pc_put ('DIST_CMP_IN',  obj%dist_cmp_in)
      call pc_put ('DIST_CMP_OUT', obj%dist_cmp_out)
      call pc_put ('TIM_BEG',      obj%tim_beg)
      call pc_put ('TIM_END',      obj%tim_end)
      call pc_put ('TYPE_GAIN',    obj%type_gain)
      call pc_put ('NUM_OFF',      obj%num_off)

      call pc_put ('OFF_TOT',      obj%off_tot)
      call pc_put ('OFF_INIT',     obj%off_init)
      call pc_put ('OFF_LAST',     obj%off_last)
      call pc_put ('OFF_INC',      obj%off_inc)
      call pc_put ('OFF_REC_TOT',  obj%off_rec_tot)
      call pc_put ('OFF_REC_INIT', obj%off_rec_init)
      call pc_put ('OFF_REC_INC',  obj%off_rec_inc)
      call pc_put ('OFF_REC_LAST', obj%off_rec_last)

      call pc_put ('TIMES',        obj%times,  obj%nvel)
      call pc_put ('DIPS',         obj%dips,   obj%nvel)
      call pc_put ('VELS',         obj%vels,   obj%nvel)


!     set up control parameters.

      need_label   = 'YES'
      need_request = 'YES'
      twosets      = 'NO'
 
      obj%xr = (obj%num_off*2.0) * max(obj%off_rec_inc,obj%off_inc) 
      obj%xr = max(obj%xr, 30*obj%dist_cmp_out)
      nx = nint(obj%xr/obj%dist_cmp_out)
      nxd = 2*nx + 1
      obj%nxd2 = nxd + 2
      
      nstore = obj%ndpt*obj%off_rec_tot*(obj%nxd2+1) +                    &
       obj%nwih*obj%maxfold+(obj%nxd2+1) + (obj%nxd2+1)+ &
       obj%ndpt+obj%itstrt+1 + LSQT+1 + obj%nfft1/2+1 + &
       obj%off_rec_tot + obj%num_off+obj%off_rec_tot
      nscratch = obj%ndpt*4

      call pc_put_control ('nstore',             nstore)
      call pc_put_control ('nscratch',         nscratch)
      call pc_put_control ('need_label',     need_label)
      call pc_put_control ('need_request', need_request)
      call pc_put_control ('twosets',           twosets)
 
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! set a default value

      obj%cmp_tot_inpt = 0
      obj%dtpi  = sqrt(obj%dt/(2.*PI))
      obj%imhdr = 17
      if (obj%hdr_cmp .eq. 8) obj%imhdr = 18
 
      obj%iflow = 1
      obj%numin = 0
      obj%numout = 0
      obj%eo_data = .false.
      obj%koff_out = 0
      if (obj%tstrt > 0.0) then
        obj%itstrt = nint(obj%tstrt/obj%dt)
      else
        obj%itstrt = 0
      end if

      obj%nfft1 = fft_nfctr(obj%ndpt+ obj%itstrt)
      obj%nfft2 = obj%nfft1* ISMPL

!--------------------------------------------------------
!.... hardwire to window of 600 ms
 
      obj%lwin = nint(0.6/obj%dt)
      obj%lwin = min( obj%lwin, obj%ndpt)
!--------------------------------------------------------

!  set up reflectivity coeffs

      if( obj%num_off <= 1) then
        obj%nrefl =  1
      else if(obj%num_off > 1 .and. obj%num_off <= 3) then
        obj%nrefl =  2
      else
        obj%nrefl =  3
      end if
 
!....  initialize parameters

      obj%maxfold = 1.5*(obj%off_last-obj%off_init)/obj%off_inc

      if (obj%off_rec_tot > 2 ) then
         obj%off_extn = 2.*obj%off_rec_inc
      else 
         obj%off_extn = 1.
      end if
  
      obj%off_inc      = obj%off_inc*0.5
      obj%off_init     = obj%off_init * 0.5
      obj%off_last     = obj%off_last * 0.5
      obj%toloff       = obj%dist_cmp_out * 0.01
      obj%off_rec_init = obj%off_rec_init*0.5
      obj%off_rec_inc  = obj%off_rec_inc*0.5
      obj%itwin        = nint(obj%tim_beg/obj%dt+1)
      obj%ibwin        = nint(obj%tim_end/obj%dt+1)
      obj%itwin        = max(obj%itwin,1)
      obj%ibwin        = min(obj%ibwin,obj%ndpt)
 
      obj%noin = (obj%off_last - obj%off_init)/obj%off_inc + 1
      if (obj%off_rec_tot > 2 ) then
         obj%off_extn = 2.*obj%off_rec_inc
      else 
         obj%off_extn = 1.
      end if

 
!      obj%xr = max(abs(obj%off_last-obj%off_init),  &
!            abs(obj%off_last-obj%off_rec_init))       ! test land data
 
      obj%xr = (obj%num_off*2.0) * max(obj%off_rec_inc,obj%off_inc) 
      obj%xr = max(obj%xr, 30*obj%dist_cmp_out)
      nx = nint(obj%xr/obj%dist_cmp_out)
  
      nxd = 2*nx + 1
      obj%nxd2 = nxd + 2
      obj%nnop = obj%num_off + obj%off_rec_tot
      obj%xmin_sep= 0.5*obj%dist_cmp_in

!     write(obj%print_lun,'(a8,f12.2,2(a8,i6))')" obj%xr=",obj%xr," nxd2=", & 
!                    obj%nxd2 
!     do iv = 1, obj%nvel
!         write(obj%print_lun,'(i6,2(a8,e12.4))')iv," obj%dips=", & 
!                               obj%dips(iv),   
!    &      " vels=",obj%vels(iv)
!     end do
 
!      write(*,*) '  off_extn,maxfold = ', obj%off_extn, obj%maxfold 
!      write(obj%print_lun,957) obj%itwin,obj%ibwin,obj%xmin_sep
 957  format(' obj%itwin,obj%ibwin,xmin_sep',2i6,2f12.4)

      if (associated(obj%hdr_store))   deallocate (obj%hdr_store)
      if (associated(obj%hdr_cnt))     deallocate (obj%hdr_cnt)
      if (associated(obj%indx_save))   deallocate (obj%indx_save)
      if (associated(obj%tr_store))    deallocate (obj%tr_store)
      if (associated(obj%xa_save))     deallocate (obj%xa_save)

      if (associated(obj%rmsamp_in))   deallocate (obj%rmsamp_in) 
      if (associated(obj%rmscnt_in))   deallocate (obj%rmscnt_in)
      if (associated(obj%rmsamp_out))  deallocate (obj%rmsamp_out)
      if (associated(obj%rmscnt_out))  deallocate (obj%rmscnt_out)
      if (associated(obj%rmswt))       deallocate (obj%rmswt)
      if (associated(obj%tr_save))     deallocate (obj%tr_save)
      if (associated(obj%dip_ndpt))    deallocate (obj%dip_ndpt)
      if (associated(obj%vel_ndpt))    deallocate (obj%vel_ndpt)
      if (associated(obj%fil45))       deallocate (obj%fil45)
      if (associated(obj%offin))       deallocate (obj%offin)
      if (associated(obj%offrc))       deallocate (obj%offrc)
      if (associated(obj%offmap))      deallocate (obj%offmap)
      if (associated(obj%sqt))         deallocate (obj%sqt)
      if (associated(obj%fftrc_obj))   call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj))   call fft_delete (obj%fftcr_obj)

      if(obj%type_gain == 'OFFSET') then
         npts = obj%off_rec_tot + 1
      else
         npts = obj%cmp_tot + 1
      end if

      allocate (obj%tr_store(obj%ndpt,obj%off_rec_tot,0:obj%nxd2),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: tr_store array')

      allocate (obj%hdr_store(obj%nwih,obj%maxfold,0:obj%nxd2),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: hdr_store array')

      allocate (obj%hdr_cnt(0:obj%nxd2),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: hdr_cnt_in array')

      allocate (obj%indx_save(obj%maxfold),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: indx_save array')

      allocate (obj%xa_save(obj%maxfold),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: xa_save array')

      allocate (obj%rmsamp_in(npts),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: rmsamp_in array')

      allocate (obj%rmscnt_in(npts),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: rmscnt_in array')

      allocate (obj%rmsamp_out(npts),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: rmsamp_out array')

      allocate (obj%rmscnt_out(npts),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: rmsamp_out array')

      allocate (obj%rmswt(npts),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: rmswt array')

      allocate (obj%tr_save(obj%ndpt+obj%itstrt+1),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: tr_save array')

      allocate (obj%sqt(0:LSQT),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: sqt array')

      allocate (obj%dip_ndpt(0:obj%ndpt),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: dip_ndpt array')

      allocate (obj%vel_ndpt(0:obj%ndpt),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: vel_ndpt array')

      allocate (obj%fil45(obj%nfft1/2+1),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: fil45 array')

      allocate (obj%offin(obj%noin),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: offrc array')

      allocate (obj%offrc(obj%off_rec_tot),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: offrc array')

      allocate (obj%offmap(obj%num_off+obj%off_rec_tot),stat=ier)
      if (ier/=0) call pc_error ('Error creating DRC: fil45 array')

!.... initialize fft tables.
      ier =  fft_create (obj%fftrc_obj, -1, obj%nfft1, 'rtoc')
      if (ier/=0) call pc_error ('Error creating rtoc FFT1 object')
      ier =  fft_create (obj%fftcr_obj, 1, obj%nfft2, 'ctor')
      if (ier/=0) call pc_error ('Error creating ctor FFT2 object')


!     build a table of square roots of 0,1,2,...,LSQT
      call drc_sqt(obj)
 
!   Initial arrays to zeros 
 
      obj%tr_store = 0.0
      obj%hdr_store = 0. 
      obj%hdr_cnt = 0
      obj%rmsamp_in = 0.0
      obj%rmscnt_in = 0.0
      obj%rmsamp_out = 0.0
      obj%rmscnt_out = 0.0 

!    built 45 degree filter coefficients

      nw = obj%nfft1/2 + 1
      do iw = 1, nw 
        wh = 0.5*(iw-1) 
        obj%fil45(iw) = sqrt(wh) 
      end do 

!    initialize binsort primitives

      npanel  = 1
      call binsort_open(obj%binsort, npanel, 'DRC_tmp_file',        &
         obj%nwih, obj%ndpt, obj%print_lun, ier)
      
!.... compute the center of the offset range
 
      call drc_offset_init(obj)
 
!     build table of vels* times / 2.
!     (assumption here is that velocity doesn't change laterally.
!     to allow lateral changes, a table of vt/2 at each output
!     location would have to be constructed similar to that at tr_store).
 
      call drc_vel_dip(obj, obj%vels, obj%dips, obj%times, obj%nvel)

      if (pc_do_not_process_traces()) return

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine drc_update


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

      subroutine drc (obj,ntr,hd,tr)
      implicit none
      type(drc_struct),intent(inout)  :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
 
      integer   :: ixzero,napr, inc, i, j, j1,ixmax             ! local
      integer   ::     ix      , ixx, itmp,itr,itr1 ! local
      integer   :: ipos, mute      , ierr ! local
      integer   :: indx_map(obj%nnop), nmap, kloc  ! local
      integer   :: locn      , mtr, iflws ! local

      real      :: dxl, trt, cr, scf    , dxnow, xrnow, offmax ! local
      real      :: xtmp, hdr7, slope, b,c,d, off_dist           ! local
      real      :: offold, offnew, xin                          ! localcd
      real      :: offapr(obj%nxd2)                             ! local
      real      :: offtmp(2), offval(obj%num_off,obj%nnop)      ! local
      real      :: rtr(obj%nfft2+1), wrk(2*obj%ndpt)            ! local


      if ( obj%eo_data .and. obj%iflow == 2) then
          ntr = NEED_TRACES 
          call drc_out( obj, ntr, hd, tr)
!         recompute Lav of the trace
          if ( ntr == 1) then
             call lav_set_hdr (hd, tr, obj%ndpt, 1)
          end if

          if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
             call drc_wrapup (obj)
          end if
          return
      end if

 
!     obj%iflow = 1 -- input; obj%iflow = 2 -- output; obj%iflow = 3 -- eoj;
 
      if (ntr == NO_MORE_TRACES) then
         obj%eo_data = .true.

        if ( obj%iflow == 1) then

!         output the rest of traces in the memory buffer.
           do j = 0, obj%nxd2 
 
             if (obj%xout < obj%xin_old-obj%xr .or.        &
     &         obj%xout <= (obj%cmpmax+obj%toloff) ) then 
               if( obj%dist_cmp_in == obj%dist_cmp_out) then
                 hdr7 = nint(obj%cmp_init + (obj%xout-obj%xl1)/obj%dist_cmp_out)
               else
                 hdr7 = obj%cmp_init + (obj%xout-obj%xl1)/obj%dist_cmp_out
               end if
               do i = 1, obj%off_rec_tot
                  call drc_outo_disk(obj, hdr7, ierr)
                  if ( ierr == FATAL_ERROR) then
                    ntr = FATAL_ERROR
                    return
                  end if
               end do

             else
               exit
             end if
           end do

           obj%iflow = 2
          
           call drc_out( obj, ntr, hd, tr)

!          recompute Lav of the trace
           call lav_set_hdr (hd, tr, obj%ndpt, 1)
           return
        end if 
      end if 
 
        if( ntr > 1 .or. ntr <= 0) then
            write(obj%print_lun,'(a)')     & 
          &  " only one trace at a time allowed on input"
            ntr = FATAL_ERROR
            return
         end if

!....  process and store input trace

        if (obj%numin == 0) then
           obj%cmp_init = hd(obj%hdr_cmp,1)
           obj%xl1 = obj%cmp_init*obj%dist_cmp_in
           xin = obj%xl1
           obj%xin_old = xin
           obj%xout = obj%xl1 
           obj%cmpmax = obj%xl1
           obj%ixo = 0
           obj%ixi = 0
 
           obj%cmp_tot_inpt = obj%cmp_tot_inpt + 1
           obj%cmp_old = obj%cmp_init
        end if  
   
!       if offset is not within range, ignore trace and return upstream
 
        if(hd(6,1) < (obj%off_init*2. - obj%toloff) .or.                 &
     &     hd(6,1) > (obj%off_last*2. + obj%toloff))  then
           write(obj%print_lun,'(a,f12.3,a,f12.3,a)')                    &
     &      '  CMP = ',hd(obj%hdr_cmp,1),'  at Offset = ',hd(6,1),      &
     &      '  is out of range, trace not used '
            ntr = NEED_TRACES
            return 
         end if
 

         if ( obj%cmp_old /= hd(obj%hdr_cmp,1)) then
           obj%cmp_tot_inpt = obj%cmp_tot_inpt + 1
           obj%cmp_old = hd(obj%hdr_cmp,1)
           if( obj%cmp_tot_inpt > obj%cmp_tot ) then
             write(obj%print_lun,*)' ********************************* '
             write(obj%print_lun,*)' ******** FATAL_ERROR from DRC **** '
             write(obj%print_lun,'(a, i8,a,i8)')             &
     &       ' # of INPUT CMP = ',obj%cmp_tot_inpt,' > CMP_TOT = ', &
                                  obj%cmp_tot 
             write(obj%print_lun,*)' ******** FATAL_ERROR from DRC **** '
             write(obj%print_lun,*)' ********************************* '
             ntr = FATAL_ERROR
             return
           end if 
        end if

!       xin is location of current trace
 
         xin = hd(obj%hdr_cmp,1)*obj%dist_cmp_in
         obj%cmpmax = max(obj%cmpmax,xin)
         mute = nint(hd(2,1))
         mute = mute + obj%itstrt        ! pad zero to beginning of trace
 
!        calculate midpoint increment for this input trace
 
         dxnow = obj%dist_cmp_in

         if( abs(xin-obj%xin_old) > 3*obj%dist_cmp_in) then
            if (hd(obj%hdr_cmp,1)*obj%dist_cmp_in < obj%xl1) then
              write(obj%print_lun,'(a,f12.3,a,f12.3,a)')                    &
     &         '  CMP = ',hd(obj%hdr_cmp,1),'  at Offset = ',hd(6,1),       &
     &         '  is not in increasing order & trace is deleted '
              ntr = NEED_TRACES
              return 
            end if
          end if
 
          off_dist = abs(hd(6,1)) * 0.5
          xtmp = obj%off_init + int((off_dist-obj%off_init)/obj%off_inc) & 
               *obj%off_inc
          offold = xtmp + 0.5 * obj%off_inc
             
!         increment input trace number
          obj%numin = obj%numin + 1
  
          if(xin+obj%dist_cmp_in < obj%xin_old) then
             write(*,'(a,f12.2,a8,f12.2)')                            &
     &           " midpoint decreased from",obj%xin_old," to",xin
             ntr = FATAL_ERROR
             return
          end if
 
          obj%xin_old = xin

!         resample input trace and filter (if 45 deg filter is requested).
!         resampled trace goes into obj%rtr.
 
          if(obj%itstrt > 0) then
             obj%tr_save(1:obj%itstrt) = 0.0 
             obj%tr_save(obj%itstrt+1:obj%ndpt) = tr(1:obj%ndpt,1)
          else
             obj%tr_save(1:obj%ndpt) = tr(1:obj%ndpt,1)
          end if
 
          obj%ixi = nint((xin - obj%xl1)/obj%dist_cmp_out)
          obj%ixi = mod(obj%ixi,obj%nxd2)
   
!         write(*,*) '    '
!         write(*,*) " numin= ", obj%numin, " midpoint= ",xin,   &             
!           "offset= ",offold,  " cmp ", obj%ixi, " fold ",      &
!            obj%hdr_cnt(obj%ixi)               

!         look for a CMP gather ready to output
 
          obj%xout = obj%xl1 + obj%numout*obj%dist_cmp_out
 
          if (obj%xout < xin - obj%xr) then
             hdr7 = nint(obj%cmp_init + (obj%xout-obj%xl1)/obj%dist_cmp_out) 
             do j = 1, obj%off_rec_tot 
                 call drc_outo_disk(obj, hdr7, ierr)
                  if ( ierr == FATAL_ERROR) then
                    ntr = FATAL_ERROR
                    return
                  end if
             end do
          end if

!.... save header in the rolling gathers
 
          obj%hdr_cnt(obj%ixi) = min(obj%hdr_cnt(obj%ixi)+1, obj%maxfold)
          obj%hdr_store(1:obj%nwih,obj%hdr_cnt(obj%ixi),obj%ixi)   &
              = hd(1:obj%nwih,1)

          if( obj%type_gain == 'OFFSET') then
             locn = nint((offold-obj%off_init)/obj%off_inc) + 1
          else
             locn = nint((xin - obj%xl1)/obj%dist_cmp_out) + 1
          end if

!         compute and store input rms amplitudes.

          call drc_rmsamp(obj, obj%tr_save, mute, locn,     &
                  obj%rmsamp_in, obj%rmscnt_in)      
 
!         compute the mapping offset locations.

          call drc_noff( obj, offold, obj%offmap, indx_map, offval, nmap)
 
          do itr1 = 1, nmap
             kloc = indx_map(itr1)             ! location of mapping in
             offnew = obj%offmap(itr1) 
             xrnow = abs(offold-offnew)  

!            calculate overall scale factor for this input trace
 
             offmax = max(offold,offnew)
             obj%scf0 = obj%dtpi/offmax*dxnow
!            is offset increasing or decreasing?
 
             if(offnew < offold) then
               iflws = 1 
             else
               iflws = -1
             end if
 
             obj%ndpt_new = obj%ndpt + obj%itstrt
             rtr(1:obj%ndpt_new) = obj%tr_save(1:obj%ndpt_new)

             call drc_rs(obj, rtr, obj%ndpt_new, iflws)
 
!              broadcast input trace to all output traces within range.
 
             dxl =  obj%xout - xin
             dxl =  int(dxl/(obj%dist_cmp_out-1.e-10)) * obj%dist_cmp_out
             do  
               if (dxl > -xrnow) exit
               dxl = dxl + obj%dist_cmp_out
             end do
 
             ixzero = 1
             ixmax = 1
             offapr(1) = 0.
 
             napr = 0
             xtmp = -1.
             do 
               if (dxl >= xrnow) exit
               napr = napr + 1
               offapr(napr) = dxl
               if (abs(dxl) <= EPS) then
                  ixzero = napr
               end if
               if ( abs(dxl) > xtmp) then
                  xtmp = dxl
                  ixmax = napr
               end if
               dxl = dxl + obj%dist_cmp_out
             end do
 
             ixx = nint((xin - obj%xl1)/obj%dist_cmp_out)
             ixx = max(ixx, 0)
 
!          Broadcast the input trace with a particular offset.
 
             inc = sign(1, ixmax-ixzero)
             do itr =  ixzero, ixmax, inc
               dxl = offapr(itr)
               mtr = 1
               offtmp(1) = dxl
               if ( itr.ne.ixzero) then
                 ipos = itr - (itr-ixzero)*2
                 if ( ipos >= 1 .and. ipos <= napr) then
                   mtr = 2
                   offtmp(2) = offapr(ipos)
                 end if
               end if
 
!              calculate tr, c1, c2, c4, and scf
               call drc_trj(obj,offnew,offold,dxl,trt,cr,scf,slope, b,c,d)
                        
                wrk(1:obj%ndpt) = 0.0
 
!              map into different offset bins.

                call drc_xrc(obj,rtr,obj%ndpt,trt,cr,scf,slope,mute,  &
                    b,c,d,offnew,offold,offval(1,itr1), wrk)
  
                do j1 = 1, mtr
 
                  itmp= aint(offtmp(j1)/obj%dist_cmp_out)
                  ix= ixx + itmp
                  ix = mod(ix,obj%nxd2)
                  if (ix > obj%nxd2 .or. ix < 0) then
                    write(obj%print_lun,*) ' **** fatal error ', obj%ixo
                    ntr = FATAL_ERROR
                    return
                  end if 
                  obj%tr_store(1:obj%ndpt,kloc,ix)                  &
                      = obj%tr_store(1:obj%ndpt,kloc,ix)+ wrk(1:obj%ndpt)  
               end do
            end do
          end do

          if (obj%iflow.eq.1) then
!         return upstream get more traces
            ntr = NEED_TRACES
             return  
          else if (obj%numin.eq.0) then
            write(obj%print_lun,'(a)')" warning: no input traces found by drc!"
            ntr = FATAL_ERROR
          end if
 
      return
      end subroutine drc

!</execute_only>


      subroutine drc_outo_disk(obj, hdr7, ierr)  
!----------------------------------------------------------------------
!     drc_outo_disk  --  routine to output a trace to disk 
!     from the memory buffer
!     hdr7 -- the value of the header word 7 or 8
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj               ! arguments
      real , intent(in)              :: hdr7              ! arguments
      integer , intent(inout)        :: ierr              ! arguments

      double precision  :: hd(obj%nwih,1)                 ! local
      real              :: tr(obj%ndpt,1)                 ! local

      integer           ::                   mute, j  ! local
      integer           :: locn, ipanel, icdp, noff_in    ! local
      integer           :: ntr  ! local
      real              :: offnew                         ! local
  
      ierr = 1
      obj%koff_out = obj%koff_out + 1 
      icdp = obj%ixo
      noff_in = obj%hdr_cnt(obj%ixo) 

!     store the offset into asending order.

      if ( obj%koff_out == 1 .and. noff_in > 1 ) then
        obj%xa_save(1:noff_in) = obj%hdr_store(6,1:noff_in,icdp) 
        obj%indx_save = (/(j, j=1,noff_in)/)
        call sort_qkisort (noff_in, obj%xa_save, obj%indx_save)
       end if
 
      if (obj%ixo>obj%nxd2 .or. obj%ixo<0)    then
         write(pc_get_lun(),*) ' DRC: ixo is out of range' 
         ierr = FATAL_ERROR
      end if
 
!     move a trace header and trace into output buffer
  
      tr(1:obj%ndpt,1) = obj%tr_store(1:obj%ndpt,obj%koff_out, icdp)
      obj%tr_store(1:obj%ndpt,obj%koff_out, icdp) = 0.0
  
!      build trace header
 
      offnew = obj%offrc(obj%koff_out)*2. - 2*obj%off_inc*EPS 
      call drc_intp_hdr (obj, obj%hdr_cnt(obj%ixo), offnew,  icdp,   &
        hd, obj%xa_save, obj%indx_save)
 
      mute = nint(hd(2,1))
  
      if ( mute < 1 .or. mute > obj%ndpt ) then
         write(pc_get_lun(),*) ' DRC: mute problem in drc_outo_disk '
         ierr = FATAL_ERROR
      end if
  
      !.... reapply the front mute
      if ( mute < obj%ndpt ) then  
         tr(:mute,1) = 0.0 
         hd(6,1) = offnew 
         hd(5,1) = obj%off_rec_tot 

         if (obj%hdr_cmp == 7) then 
           hd(7,1) = hdr7 
           hd(8,1) = 0.0 
         else 
           hd(7,1) = 0.0 
           hd(8,1) = hdr7 
         end if
 
         if (obj%hdr_cmp==7 .or. obj%hdr_cmp==8) then  
            call grid_get_survey_coords(obj%grid, hd(7,1), hd(8,1),   &
               hd(17,1), hd(18,1) )

         else if (obj%hdr_cmp==17 .or. obj%hdr_cmp==18 ) then 
            call grid_get_grid_coords(obj%grid, hd(17,1), hd(18,1),   &
               hd(7,1), hd(8,1) )
         endif 

         if (obj%hdr_cmp == 7) then 
           hd(11,1) = hd(17,1) - 0.5*hd(6,1) 
           hd(12,1) = hd(18,1) 
           hd(14,1) = hd(17,1) + 0.5*hd(6,1) 
           hd(15,1) = hd(18,1) 
         else 
           hd(11,1) = hd(17,1)  
           hd(12,1) = hd(18,1) - 0.5*hd(6,1)
           hd(14,1) = hd(17,1)  
           hd(15,1) = hd(18,1) + 0.5*hd(6,1)
         end if
         call grid_get_survey_coords(obj%grid, hd(11,1), hd(12,1),   &
                 hd(33,1), hd(34,1) )
         call grid_get_survey_coords(obj%grid, hd(14,1), hd(15,1),   &
                 hd(35,1), hd(36,1) ) 
 
!.... truncate headers to integer 
 
         hd(2,1) = nint(hd(2,1)) 
         hd(5,1) = nint(hd(5,1)) 
         hd(9,1) = nint(hd(9,1)) 
         hd(10,1) = nint(hd(10,1))  
         hd(46,1) = nint(hd(46,1)) 
         hd(47,1) = nint(hd(47,1)) 
         hd(64,1) = nint(hd(64,1)) 
 
!...     type_gain = OFFSET -- one gain factor for the entire offset panel
 
         ipanel = 1
         hd(24,1) = ipanel

         if (obj%type_gain == 'OFFSET') then  
           locn =  obj%koff_out 
         else 
           locn = nint((obj%xout - obj%xl1)/obj%dist_cmp_out) + 1
         endif 

         call drc_rmsamp(obj, tr, mute, locn,                     &
             obj%rmsamp_out, obj%rmscnt_out)

!        store the output trace in binsort.

         ntr = 1
         call binsort (obj%binsort, ntr, hd, tr)

       end if
  
       if (obj%koff_out == obj%off_rec_tot) then 
!        reset hit counter for this output trace

         obj%koff_out = 0 
         obj%hdr_store(1:obj%nwih,1:obj%maxfold,obj%ixo) = 0.0 
         obj%hdr_cnt(obj%ixo) = 0 
         obj%numout = obj%numout + 1 
         obj%xout = obj%xout + obj%dist_cmp_out 
         obj%ixo = obj%ixo + 1 
         if (obj%ixo == obj%nxd2) obj%ixo = 0 
       endif 
      return  
      end subroutine drc_outo_disk 

 
      subroutine drc_intp_hdr(obj, noff_in, xnew, icdp, hd, xa, indx )
!----------------------------------------------------------------------
!    Linear interpolate traces headers.     
!       noff_in -- number of offsets in this cmp.
!       xnew    -- new offset to output.
!       icdp    -- CMP location.
!  xa & indx    -- offsets sorted in ascending order.
!        hd     -- output trace headers.
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj                ! arguments

      integer , intent(in) :: noff_in                      ! arguments
      real ,    intent(in) :: xnew                         ! arguments
      integer , intent(in) :: icdp                         ! arguments 
      integer,  intent(in) :: indx(obj%maxfold)            ! arguments
      real,     intent(in) :: xa(obj%maxfold)              ! arguments  
      double precision , intent(inout) :: hd(1:obj%nwih)   ! arguments

      integer       :: k, klo, khi  ! local
      real          :: h, frac, off_dist                   ! local
  
!     check for offset limit

      if (noff_in == 1) then
         off_dist = obj%hdr_store(6,1,icdp)
         if (xnew >= off_dist - obj%off_extn .and.      &
             xnew <= off_dist + obj%off_extn ) then
            hd(1:obj%nwih) = obj%hdr_store(1:obj%nwih,1,icdp)  
         else 
            hd(2) = 1.d0*obj%ndpt 
            hd(25) = 0.d0    
         endif 
         return

      else if (noff_in <= 0) then 
         hd(2) = 1.d0*obj%ndpt 
         hd(25) = 0.d0    
         return
      endif  
 
!     limit the output offset ranges

      if (xnew>=xa(indx(1)) - obj%off_extn .and.     &  
          xnew<=xa(indx(noff_in)) + obj%off_extn) then 
 
!...  binary search
 
        klo = 1 
        khi = noff_in 
    1   continue 
        if (khi - klo > 1) then 
          k = (khi + klo)/2 
          if (xa(indx(k)) > xnew) then 
            khi = k 
          else 
            klo = k 
          endif 
          go to 1 
        endif 
 
        klo = indx(klo) 
        khi = indx(khi) 
 
!      write(*,905) noff_in,klo,khi,xnew
  905   format(' noff_in,klo,khi,xnew ',3i6,f12.2) 
 
        h = xa(khi) - xa(klo) 
        if (abs(h) <= 1.e-6) then 
          hd(1:obj%nwih) = obj%hdr_store(1:obj%nwih,klo,icdp) 
        else 
 
!.... linear interpolate the headers
 
          frac = 1. - (xnew - xa(klo))/(xa(khi)-xa(klo))
          hd(1:obj%nwih) = frac*obj%hdr_store(1:obj%nwih,klo,icdp) +      &
                           (1.-frac)*obj%hdr_store(1:obj%nwih,khi,icdp)
        endif 
        if (hd(2) < 1.d0*obj%itstrt) hd(2) = obj%hdr_store(2,indx(1),icdp) 
        hd(2) = max(hd(2),1.d0*obj%itstrt) 
        hd(2) = min(hd(2),1.d0*obj%ndpt) 
      else  
        hd(2) = 1.d0*obj%ndpt 
        hd(25) = 0.0 
      endif 
 
      return  
      end subroutine drc_intp_hdr 


      subroutine drc_offset_init(obj) 

!     drc_offset_init  -- initialize the input and output ranges.

      implicit none
      type(drc_struct),intent(inout) :: obj               ! arguments

      integer :: j1, j2   ! local        
 
      obj%offin = obj%off_init + (/(j1,j1=0,obj%noin - 1)/)*obj%off_inc 
!
!....  compute the output offset array
!
      obj%offrc = obj%off_rec_init + (/(j2,j2=0,obj%off_rec_tot - 1)/)  &
                 *obj%off_rec_inc + obj%off_inc*EPS 
 
      return  
      end subroutine drc_offset_init 


 
      subroutine drc_noff(obj, offset, offmap, indx_map, offval, nmap) 
!----------------------------------------------------------------------
!     drc_noff  --  routine to map to output offset locations
!     offset -- offset of the input trace
!     offmap -- vector to contain locations of reconstructed offsets
!     indx_map -- vector to contain index locations of reconstructed
!     offval -- vector to contain input offsets
!     nmap -- number of elements in offmap
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj                   ! arguments

      integer , intent(out) :: nmap                           ! arguments
      real , intent(in)     :: offset                         ! arguments
      integer , intent(out) :: indx_map(obj%nnop)             ! arguments
      real , intent(out)    :: offmap(obj%nnop)               ! arguments
      real , intent(out)    :: offval(obj%num_off,obj%nnop)   ! arguments

      integer   :: ix, nx, j, j1                                  !  local
      integer   :: itmp(obj%noin + obj%off_rec_tot)           !  local
      real      :: diff(obj%noin + obj%off_rec_tot)           !  local
      real      :: xlimit                                     !  local
 
!.... find the offset range in the output bin
 
      do ix = 1, obj%off_rec_tot 
        diff(ix) = abs(obj%offrc(ix)-offset) 
      end do 
 
      itmp = (/(j, j=1,obj%off_rec_tot)/) 
      call sort_qkisort (obj%off_rec_tot,  diff, itmp)
 
      nx = min(nint(obj%num_off*obj%off_inc/obj%off_rec_inc),obj%off_rec_tot) 
 
      nmap = 0 
      xlimit = obj%num_off/2.*obj%off_inc 
 
      do ix = 1, nx 
        if (abs(offset - obj%offrc(itmp(ix))) > xlimit) cycle  
        nmap = nmap + 1 
        offmap(nmap) = obj%offrc(itmp(ix)) 
        indx_map(nmap) = itmp(ix) 
      end do 
 
!       write(obj%print_lun,'(a,i6,10(i6,f12.2))') 'nmap,indx_map,offmap ',  &
!           nmap,(indx_map(ix),offmap(ix), ix=1,nmap)
 
!.... find the offset range in the input bin
 
      do j = 1, nmap 
        do ix = 1, obj%noin 
          diff(ix) = abs(obj%offin(ix)-offmap(j)) 
        end do 
 
        itmp = (/(j1, j1=1,obj%noin)/) 
        call sort_qkisort (obj%noin,  diff, itmp)
 
        diff(:obj%num_off) = obj%offin(itmp(:obj%num_off)) 
 
        itmp = (/(j1, j1=1,obj%num_off)/) 
        call sort_qkisort (obj%num_off,  diff, itmp)

        offval(:obj%num_off,j) = diff(itmp(:obj%num_off)) 
 
!         write(obj%print_lun,'(a,10f12.2)') 'offset,offval ',         &
!           offset,(offval(ix,j), ix=1,obj%num_off)
 
      end do 
 
!....  reset to original offset values.

      offmap(:nmap) = offmap(:nmap) - obj%off_inc*EPS 
      return  
      end subroutine drc_noff 


      subroutine drc_rs(obj, rtr, nt, iflop)
!----------------------------------------------------------------------
!
!    drc_rs -- resample and apply a phase filter to input trace
!    rtr -- input trace
!    iflop- invoke 45 degree filter iff iflop = 1,
!                 -45 degree filter iff iflop = -1
!    rtr -- filtered trace
!
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj           ! arguments

      integer, intent(in)   :: nt                     ! arguments
      integer, intent(in)   :: iflop                  ! arguments
      real, intent(inout)   :: rtr(*)                 ! arguments
    
      integer     ::     nw, i1, i2, i ! local
      real        :: scfv        ! local
      real        :: wrk(nt*ISMPL)                    ! local
      complex     :: cfac, cfaccg                     ! local
      complex     :: ctr(obj%nfft2/2+1)               ! local
 
      scfv = 1./float(obj%nfft1) 
 
      cfac = cmplx(1.,-1.)/sqrt(obj%dt*obj%nfft1) 
      cfaccg = conjg(cfac) 
      nw = obj%nfft1/2 + 1 
      rtr(nt+1:obj%nfft1) = 0.0
      call fft_rc_transform(obj%fftrc_obj, rtr, ctr)
 
      
!    apply phase filter
 
      if (iflop == 1) then 
        ctr(:nw) = ctr(:nw) * cfac * obj%fil45(:nw) 
      else if (iflop == (-1)) then 
        ctr(:nw) = ctr(:nw) * cfaccg * obj%fil45(:nw) 
      endif 
 
!    resample data, pad with zeros in frequency domain

      if (obj%nfft2 > obj%nfft1) then 
        ctr(nw+1:obj%nfft2/2) = 0. 
      endif 
 
      call fft_cr_transform(obj%fftcr_obj, ctr, rtr, scfv)
      call drc_integ (nt*ISMPL, rtr, wrk) 
       if (obj%itstrt > 0) then 
        i1 = obj%itstrt*ISMPL + 1 
        i2 = nt*ISMPL 
        do i = i1, i2 
          rtr(i-i1) = rtr(i-1) 
        end do 
      end if

      return  
      end subroutine drc_rs 


      subroutine drc_trj(obj, hnew, hold, xr, t_ratio, cr, scf,  &
           slope, b, c, d)  

!----------------------------------------------------------------------
!
!   drc_trj -- calculate the travel-obj%times trajectory in x-t space
!     hnew -- new offset
!     hold -- old offset
!     xr   -- CMP displacement
!     t_ratio  -- ratio of old times (moveout corrected) to new times at
!            each midpoint displacement.
!     cr  -- mapping cutof parameter
!     slope  -- gradient of times/distance.
!     scf -- the times-independent portion of the scale factor that
!            multiplies each input trace.
!     b, c, d  -- mapping coefficients
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj            ! arguments

      real , intent(in)  :: hnew                       ! arguments
      real , intent(in)  :: hold                       ! arguments 
      real , intent(in)  :: xr                         ! arguments
      real , intent(out) :: t_ratio                    ! arguments
      real , intent(out) :: cr                         ! arguments
      real , intent(out) :: scf                        ! arguments
      real , intent(out) :: slope                      ! arguments
      real , intent(out) :: b                          ! arguments
      real , intent(out) :: c                          ! arguments
      real , intent(out) :: d                          ! arguments 
 

      real      :: dc  ! local
      real      :: hmax, hmin, xh2                     ! local
      double precision :: hr, hrsq, xrd, xtmp, trsq    ! local

      if (hnew == hold) then 
        t_ratio = 1. 
        b = 0. 
        c = 0. 
        d = 0. 
        cr = 0. 
        scf = 2.*obj%scf0 
        slope = 0.0 
        return  
      endif 
 
!...   calculate dimensionless constants
      hmax = max(hnew,hold) + obj%xmin_sep 
      hmin = min(hnew,hold) 
 
      hr = hmin/hmax 
      hrsq = hr*hr 
      xrd = 1. - xr*xr/(hmax*hmax) + hrsq 
      xtmp = sqrt(xrd*xrd - 4*hrsq) 
      trsq = xrd + xtmp 
 
      trsq = 2./trsq 
      t_ratio = sqrt(trsq) 
      b = trsq - 1. 
      c = 1. - hrsq*trsq 
      d = 1. - hrsq*trsq*trsq 
      cr = b/(d*hmax*hmax) 
      scf = obj%scf0*trsq*c/d**1.5 
      dc = d/c 
 
      if (hnew > hold) then 
        t_ratio = 1./t_ratio 
        scf = scf*(2. - dc) 
      else 
        cr = cr*trsq 
        scf = scf*(2. - dc/trsq) 
      endif 
 
      xh2 = xr/(hmax*hmax) 
      slope = abs(0.5*t_ratio*obj%dist_cmp_out*(xh2 + xh2*xrd/xtmp)) 
 
      return  
      end subroutine drc_trj 
 
 

      subroutine drc_xrc(obj, din, nt, t_ratio, cr, scf, slope, mute,  &
        b, c, d, hnew, hold, offval, dout)

!----------------------------------------------------------------------         
!     drc_xrc -- space-time data reconstruction module
!
!        din -- array of 2*obj%nx+1 input traces.
!        nt  -- number of sample in din
!        t_ratio  -- ratio tp/t of input to output tt for each input trace
!        cr  -- mapping cutoff parameter
!        scf -- the time-independent portion of the scale factor that
!               multiplies each input trace
!        slope  -- gradient of time/distance.
!        mute  -- front mute
!        b,c,d -- mapping coeffs
!        hnew  -- output mapping offset
!        hold -- input mapping offset
!        offval -- array to hold reconstructed offsets
!        dout  -- output trace after offset mapping
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj               ! arguments

      real, intent(in)       :: t_ratio                   ! arguments
      real, intent(in)       :: cr                        ! arguments
      real, intent(in)       :: scf                       ! arguments
      real, intent(in)       :: slope                     ! arguments
      integer, intent(in)    :: mute                      ! arguments 
      integer, intent(in)    :: nt                        ! arguments 
      real, intent(in)       :: b                         ! arguments
      real, intent(in)       :: c                         ! arguments
      real, intent(in)       :: d                         ! arguments
      real, intent(in)       :: hnew                      ! arguments
      real, intent(in)       :: hold                      ! arguments
      real, intent(in)       :: din(ISMPL*nt)             ! arguments
      real, intent(inout)    :: dout(nt)                  ! arguments
      real, intent(in)       :: offval(*)                 ! arguments

      integer    :: it, nte, itp     , itp0, itp1, itp2 ! local
      integer    ::       ist, ishif      , istart ! local         
      real       :: vtsq, c3, tre            ! local

      real       :: afi(nt)                               ! local
  
      if (obj%nrefl == 1) then 
        afi(:nt) = 1. 
      else 
        call drc_afi (obj, nt, t_ratio, cr, scf, mute,  b, c, d,      &
           hnew, hold, offval, afi) 
      endif 
 
      tre = t_ratio*ISMPL 
      nte = nt*ISMPL 
 
!   initiate time loop (maximum time to use is at c3=0 ( i.e.90 deg).
 
      istart = max( mute, obj%itstrt)
      it = max(1. ,istart/(tre + 1)) 
      it = min(it, nt) 
      ist = it 
      do it = ist, nt 
        itp0 = max(nint(tre*(it - 1 + obj%itstrt) + 1.),1) 
        itp = max(nint(tre*(it - 1 + obj%itstrt) + 1.),1)    &   
                  - obj%itstrt*ISMPL
        vtsq = obj%vel_ndpt(it)*obj%vel_ndpt(it) 
        c3 = obj%dip_ndpt(it) - vtsq*cr 
        if (it >= nt .or. itp >= nte .or. c3 <= 0.) exit  

!   add rescaled and time shifted input traces to output trace
 
        ishif = max(slope*itp0,1.) 
        itp1 = itp - ishif 
        itp2 = itp + ishif 
 
        if (itp1>=1 .and. itp2<=nte) dout(it) = dout(it) +     &
           (2*din(itp) - din(itp1) - din(itp2))*afi(it)*scf*   &
           obj%sqt(it+obj%itstrt)/float(ishif*ishif) 
      end do 
  
      return  
      end subroutine drc_xrc 

 
      subroutine drc_afi(obj, nt, t_ratio, cr, scf, mute, b, c, d,  &
           hnew, hold, offval, afi)
        
!----------------------------------------------------------------------
!     drc_afi -- compute the coeffs of afi
!        nt  -- number of sample
!        t_ratio  -- ratio tp/t of input to output tt for each input trace
!        cr  -- mapping cutoff parameter
!        scf -- the time-independent portion of the scale factor that
!               multiplies each input trace
!        mute  -- front mute
!        b,c,d -- mapping coeffs
!        hnew  -- output mapping offset
!        hold -- input mapping offset
!        offval -- array to hold reconstructed offsets
!        afi --  afi coeffs
!-----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj           ! arguments

      integer, intent(in)        :: nt                ! arguments
      integer , intent(in)       :: mute              ! arguments
      real , intent(in)          :: t_ratio           ! arguments
      real , intent(in)          :: cr                ! arguments
      real , intent(in)          :: scf               ! arguments
      real , intent(in)          :: b                 ! arguments
      real , intent(in)          :: c                 ! arguments
      real , intent(in)          :: d                 ! arguments
      real , intent(in)          :: hnew              ! arguments 
      real , intent(in)          :: hold              ! arguments
      real , intent(in)          :: offval(*)         ! arguments
      real , intent(inout)       :: afi(nt)           ! arguments

      integer   :: it          , jtp, jtp0, indx, i, j ! local
      integer   :: it0, ist, npts            , nwin ! local
      real      :: vtsq      , c3           ! local
      real      ::      xmin ! local
      real      :: a, rns     , tmp, si, hmax  ! local
      real      :: trsq, bfi, c1, c2, c11, c22          ! local
      real      :: xs, xe                               ! local
      real      :: time(nt)                             ! local
      real      :: twin(nt)                             ! local
      real      :: acoef(nt)                            ! local
      real      :: beta(obj%num_off, obj%nrefl)         ! local
      real      :: soln(obj%num_off, obj%nrefl)         ! local
 
       if (abs(c)<= tiny(c) .or. obj%num_off==1 .or.   &
          obj%lwin >= nt) then 
          afi(:nt) = 1. 
        return  
      endif 
 
      nwin = 0 
      it = min(obj%lwin, nt) 
      ist = it 
      trsq = t_ratio*t_ratio 
      if (t_ratio >= 1.) then 
        bfi = trsq*b 
        c1 = c*trsq*trsq 
        c2 = c 
      else 
        bfi = b 
        c1 = c*trsq 
        c2 = c/trsq 
      endif 
 
      if (t_ratio == 1.0) then 
        npts = nt 
      else 
        npts = obj%lwin 
      endif 
 
      l100: do it = ist, nt, npts 
        jtp = max(nint(t_ratio*(it - 1) + 1),1) 
        jtp = min(obj%ndpt,jtp) 
 
        it0 = it + obj%itstrt 
        jtp0 = max(nint(t_ratio*(it0 - 1) + 1),1) 
        vtsq = obj%vel_ndpt(it)*obj%vel_ndpt(it) 
        c3 = obj%dip_ndpt(it) - vtsq*cr 
        c11 = 1./(c1*vtsq) 
        c22 = 1./(c2*vtsq) 
 
        if (it >= nt .or. c3 <= 0.) exit  
 
!....   set up the beta matrix
 
        do j = 1, obj%nrefl 
          do i = 1, obj%num_off 
            hmax = max(hnew,offval(i)) + obj%xmin_sep 
            si = d - vtsq*bfi/(hmax*hmax) 
            if (si <= 0) cycle  l100 
            tmp = si*offval(i)*offval(i)*c11
            if ( tmp == 0.0) tmp = EPS 
            beta(i,j) = tmp**(j - 1)  
          end do 
        end do 
 
 
!... compute inverse of the matrix

         Rns = 1.e-10

         call drc_matinv(obj%num_off, obj%num_off, obj%nrefl,  beta, soln,  Rns)
  
        xmin = abs(offval(1)-hold) 
        indx = 1 
 
        do i = 2, obj%num_off 
          tmp = abs(offval(i)-hold) 
          if (tmp >= xmin) cycle  
          indx = i 
          xmin = tmp 
        end do 
 
        hmax = max(hnew,offval(indx)) + obj%xmin_sep 
        si = d - vtsq*bfi/(hmax*hmax) 
        tmp = si*hnew*hnew*c22 + tiny(tmp)
        a = 0 
        do j= 1, obj%nrefl
          a = a + tmp**(j-1)*soln(indx,j)
        end do
 
        nwin = nwin + 1 
        acoef(nwin) = a 
        time(nwin) = it 
 
      end do l100 
 
!     interpolate AFI coefficients.

      if (nwin == 1) then
         if (nint(time(1)) < nt) then
           nwin = nwin + 1
           time(nwin) = nt
           acoef(nwin) = acoef(1)
         else if (nint(time(1)) >= nt) then        
           time(1) = 1
           acoef(1) = acoef(1)
           nwin = nwin + 1
           time(nwin) = nt
           acoef(nwin) = acoef(1)
         end if
      else if ( nwin < 1) then
           time(1) = 1
           acoef(1) =  0.0
           nwin = nwin + 1
           time(nwin) = nt
           acoef(nwin) = 0.0
      end if
     
      xs = 1.
      xe = nt
      call interp_1d_var_lin_real     &
          (time, acoef, nwin, twin, afi, nt, xs, xe)
 
      return  
      end subroutine drc_afi 


      Subroutine drc_matinv(NF, NFM, ND,  A, B,  Rns)

!----------------------------------------------------------------------
!     drc_matinv Finds an inverse B to a NF*2 or NF*3 size matrix A
!
!        NFM   = Maximum size of data set
!        NF    = number of values in data set
!        ND    = size of model set (must be <= NF)
!                 Currently, ND = 2 or 3 only
!        A     = NF*ND matrix to invert
!        RNS   = initial diagonal load
!        B     = NF*ND inverse to A
!        RNS   = Final diagonal load
!----------------------------------------------------------------------      

      Implicit None

      integer , intent(in)    :: NF                    ! arguments
      integer , intent(in)    :: NFM                   ! arguments
      integer , intent(in)    :: ND                    ! arguments
      real , intent(in)       :: A(NFM,ND)             ! arguments
      real , intent(inout)    :: B(NFM,ND)             ! arguments
      real , intent(inout)    :: Rns                   ! arguments



      Integer, parameter      :: NDMAX=3

      real                    :: br(NDMAX,NDMAX)       ! local
      real                    :: ar(NDMAX,NDMAX)       ! local
      real                    :: arp(NDMAX,NDMAX)      ! local
      real                    ::     ss, sn, tr, det ! local
      real                    :: detin  ! local
      real                    :: rnsold, rpar          ! local
      Integer                 :: id, jd, mf            ! local
      Integer                 :: iter, jiter           ! local

!     Form ND*ND matrix  ar from A values
      Do id = 1, nd
         Do jd = 1, id
            ar(id,jd) = 0.
            Do mf = 1, NF
               ar(id,jd) = ar(id,jd) + a(mf,id)*a(mf,jd)
            End Do
         End Do
      End Do

!     Calculate trace of ND*ND matrix  ar and copy ar to arp
      tr = 0.
      Do id = 1, nd
         tr = tr + ar(id,id)
         Do jd = 1, id
            arp(id,jd) = ar(id,jd)
            arp(jd,id) = ar(id,jd)
         End Do
      End Do

!     Iterate until matrix is invertible
      rns = tr*rns/Nf
      iter = 0
      jiter = 0
      Do 
         
!        Add ratio of noise to signal to diagonal of ar
         Do id = 1, nd
            arp(id,id) = ar(id,id) + rns
         End Do
         rnsold = rns

!        Code for ND = 2
         If (nd.eq.2) Then

!           First find determinant of arp
            det = arp(1,1)*arp(2,2) - arp(2,1)*arp(2,1)

!           Check for stability
            If (det.ne.0.) Then
               iter = 1
               detin = 1./det

!              Calculate Inverse br of arp
               br(1,1) = arp(2,2)*detin
               br(2,1) = -arp(2,1)*detin
               br(2,2) = arp(1,1)*detin
               br(1,2) = br(2,1)
!              write(7,'(a6,3f12.3)')" br=",br(1,1),br(2,1),br(2,2)

!              Calculate B
            
               Do mf = 1, NF
                  b(mf,1) = a(mf,1)*br(1,1)+a(mf,2)*br(2,1)
                  b(mf,2) = a(mf,1)*br(2,1)+a(mf,2)*br(2,2)               
               End Do
               
!           If arp has no inverse, increase diagonal load
            Else
               sn = 2./(Nf+tr/(rns+1.e-6))               
               jiter = jiter+1
            End If
            
!        Code for ND = 3
         Elseif (Nd.eq.3) then

!           Calculate inverse br of arp
            br(1,1) = arp(2,2)*arp(3,3)-arp(3,2)*arp(3,2)
            br(2,1) = arp(3,2)*arp(1,3)-arp(2,1)*arp(3,3)
            br(3,1) = arp(2,1)*arp(3,2)-arp(3,1)*arp(2,2)
            br(2,2) = arp(1,1)*arp(3,3)-arp(3,1)*arp(3,1)
            br(3,2) = arp(2,1)*arp(3,1)-arp(1,1)*arp(3,2)
            br(3,3) = arp(2,2)*arp(1,1)-arp(1,2)*arp(1,2)
            det = arp(1,1)*br(1,1)+arp(2,1)*br(2,1)+arp(3,1)*br(3,1)

!           Check for stability
            If(det.ne.0.) Then
               iter = 1
               detin = 1./det
               br(1,1) = br(1,1)*detin
               br(2,1) = br(2,1)*detin
               br(3,1) = br(3,1)*detin
               br(2,2) = br(2,2)*detin
               br(3,2) = br(3,2)*detin
               br(3,3) = br(3,3)*detin
               br(1,2) = br(2,1)
               br(1,3) = br(3,1)
               br(2,3) = br(3,2)

!              Calculate B and D            
               Do mf = 1, NF
                 b(mf,1)=a(mf,1)*br(1,1)+a(mf,2)*br(2,1) & 
                         +a(mf,3)*br(3,1)
                 b(mf,2)=a(mf,1)*br(2,1)+a(mf,2)*br(2,2) &
                         +a(mf,3)*br(3,2)
                 b(mf,3)=a(mf,1)*br(3,1)+a(mf,2)*br(3,2) &
                         +a(mf,3)*br(3,3)               
               End Do

!           If arp has no inverse, increase diagonal load
            Else
               sn = 2./(Nf+tr/(rns+1.e-6))
               jiter = jiter+1
            End If
                     
         End IF  

            ss = (1. - sn*Nf)/tr
            rns = sn/ss
               
         rpar = abs(rns - rnsold)
         If(iter > 0 .or. jiter >= 5) Then
            Exit
         End If

      End Do

      Return
      end subroutine drc_matinv 


 
      subroutine drc_integ(nt, trace, wk) 

!-----------------------------------------------
!
!    drc_integ - perform double integration for anti-aliasing
!        nt  -- trace length
!        trace -- input trace.
!        wk -- working array.
!     output
!        trace -- trace after double integration.
!-----------------------------------------------
!
      implicit none

      integer , intent(in) :: nt                       ! arguments
      real , intent(inout) :: trace(nt)                ! arguments
      real , intent(inout) :: wk(nt)                   ! arguments

      integer :: i                                     ! local
      real :: xsum                                     ! local
 
      xsum = 0. 
      do i = 1, nt 
        xsum = xsum + trace(i) 
        wk(i) = xsum 
      end do 
 
      xsum = 0. 
      do i = nt, 1, -1 
        xsum = xsum + wk(i) 
        trace(i) = xsum 
      end do  
      return  
      end subroutine drc_integ 


      subroutine drc_vel_dip(obj, vels, dips, times, nvel) 
!
!----------------------------------------------------------------------
!
!     drc_vel_dip -- routine to generate vels * t/2 and dip_ndpt**2
!
!     vels   -- array rms velocity
!     dips   -- array reflector obj%dip_obj%ndpt velocity
!     times  -- obj%times corresponds to rms velocity
!     nvel   -- number of element in vector times
!     outputs:
!     obj%vel_ndpt is linearly interpolated between inputs.
!     obj%dip_ndpt is linearly interpolated between inputs.
!----------------------------------------------------------------------
!

      implicit none
      type(drc_struct),intent(inout) :: obj                  ! arguments

      integer , intent(in)    :: nvel                        ! arguments
      real , intent(in)       :: vels(nvel)                  ! arguments 
      real , intent(in)       :: dips(nvel)                  ! arguments
      real , intent(in)       :: times(nvel)                 ! arguments
 

      integer    :: j1, j2, j3, j4, j5    , iv, iti, itf !  local
      real       :: scf, dv, v, dp, p                        !  local

      scf = obj%dt*0.5 
      obj%vel_ndpt(0) = 0. 
      obj%dip_ndpt(0) = dips(1) 
 
      if (nvel == 1) then 
        obj%vel_ndpt(1:obj%ndpt) = vels(1)*scf*   &
             (/(j1+obj%itstrt, j1=1, obj%ndpt)/) 
        obj%dip_ndpt(1:obj%ndpt) = dips(1) 
      else 
         
!    interpolate velocity and dip to total trace length 

        itf = 0 
        do iv = 2, nvel 
          iti = itf + 1 
          itf = min(obj%ndpt,nint(times(iv)/obj%dt)) 
          if (itf <= iti) cycle  
          dv = (vels(iv)-vels(iv-1))/(itf - iti + 1)*scf 
          dp = (dips(iv)-dips(iv-1))/(itf - iti + 1) 
          v = vels(iv-1)*scf 
          p = dips(iv-1) 
          obj%vel_ndpt(iti:itf) = (v + dv*(/(j2,j2=0,itf - iti)/))*      &
                        (/(j3+obj%itstrt,j3=iti, itf)/) 
          obj%dip_ndpt(iti:itf) = p + dp*(/(j4,j4=0,itf - iti)/) 
        end do 
        if (itf < obj%ndpt) then 
          iti = itf + 1 
          itf = obj%ndpt 
          obj%vel_ndpt(iti:itf) = vels(nvel)*scf*          &
                            (/(j5+obj%itstrt,j5=iti, itf)/)
          obj%dip_ndpt(iti:itf) = dips(nvel) 
        endif 
      endif 
 

!  Compute cutoff angles   
 
       obj%dip_ndpt(0:obj%ndpt) = sin(PI*obj%dip_ndpt(0:obj%ndpt)/180.) 
       obj%dip_ndpt(0:obj%ndpt) = obj%dip_ndpt(0:obj%ndpt)  &
          *obj%dip_ndpt(0:obj%ndpt)
 
!      write(obj%print_lun,'(a)')" vel_ndpt"
!      write(obj%print_lun,'(5e12.4)')  &
!                      (obj%vel_ndpt(it),it=1,obj%ndpt-1,obj%ndpt/10)
!      write(obj%print_lun,'(a)')" obj%dip_ndpt"
!      write(obj%print_lun,'(5e12.4)')  &
!                      (obj%dip_ndpt(it),it=1,obj%ndpt-1,obj%ndpt/10)
 
      return  
      end subroutine drc_vel_dip


 
      subroutine drc_sqt(obj) 

!----------------------------------------------------------------------
!     drc_sqt  generate, store,  sqrt(x).  
!              square roots of the set of integers 0,1,2,...
!----------------------------------------------------------------------

      implicit none

      type(drc_struct),intent(inout) :: obj      ! arguments
      integer        :: ix                       ! local
      real           :: xn                       ! local
 
      xn = 1. 
      obj%sqt(0) = 0. 
      do ix = 1, LSQT 
        obj%sqt(ix) = sqrt(xn) 
        xn = xn + 1. 
      end do 
 
!      write(obj%print_lun,'(a)')" sqt"
!      write(obj%print_lun,'(8f10.3)')(sqt(ix),ix=1,LSQT/8)
 
      return  
      end subroutine drc_sqt 

 
      subroutine drc_rmsamp(obj, data,  mute, locn, rmsamp, rmscnt )
                           
!----------------------------------------------------------------------
!     drc_rmsamp  --  routine to compute rms amplitude
!        data -- input trace
!        mute -- front mute
!        locn -- cmp/offset location
!        rmsamp -- rms amplitude
!        rmscnt -- number of sample
!----------------------------------------------------------------------

      implicit none
      type(drc_struct),intent(inout) :: obj         ! arguments

      integer , intent(in)   :: mute                ! arguments
      integer , intent(in)   :: locn                ! arguments
      real ,    intent(in)   :: data(obj%ndpt)      ! arguments 
      real ,    intent(inout)   :: rmsamp(*)        ! arguments
      real ,    intent(inout)   :: rmscnt(*)        ! arguments


      real :: rms                                   ! local
 
      if (mute>=obj%ndpt - 1 .or. obj%ndpt<1) then  
        return  
      endif 
 
      rms = dot_product(data(obj%itwin:obj%ibwin),   &
                        data(obj%itwin:obj%ibwin)) 
      rms = sqrt(rms/(obj%ibwin - obj%itwin + 1)) 
      rmsamp(locn) = rmsamp(locn)+rms 
      rmscnt(locn) = rmscnt(locn) + 1
 
      return  
      end subroutine drc_rmsamp 


      subroutine drc_out( obj, ntr, hd, tr) 
!----------------------------------------------------------------------
!    drc_out -- output reconstructed traces
!----------------------------------------------------------------------

      implicit none

      type(drc_struct),intent(inout) :: obj 
                   ! arguments
      integer          ,intent(inout) :: ntr              ! arguments
      double precision ,intent(inout) :: hd(:,:)          ! arguments
      real             ,intent(inout) :: tr(:,:)          ! arguments

      integer,save         :: ifirst = 1

      integer              :: npts, locn, ierr            ! local
      real                 ::               offold ! local
  
      if ( ifirst == 1) then
         ifirst = 2
         ierr = 1

         if(obj%type_gain == 'OFFSET') then
           npts = obj%off_rec_tot + 1
         else
           npts = obj%cmp_tot + 1
         end if
         call drc_fill(obj%rmsamp_in, npts, ierr)
         call drc_fill(obj%rmscnt_in, npts, ierr)
         call drc_fill(obj%rmsamp_out, npts, ierr)
         call drc_fill(obj%rmscnt_out, npts, ierr)
         obj%rmsamp_in = obj%rmsamp_in/obj%rmscnt_in
         obj%rmsamp_out = obj%rmsamp_out/obj%rmscnt_out

          where (obj%rmsamp_in(:npts)==0.0 .or. obj%rmsamp_out(:npts)==0.0)  
            obj%rmswt(:npts) = 1. 
          elsewhere 
            obj%rmswt(:npts) = obj%rmsamp_in(:npts)/obj%rmsamp_out(:npts) 
          end where 

!         write (6, *) ' npts  ', npts 
!         write (6, 929) (obj%rmsamp_in(i),i=1,npts) 
  929    format(' rmsamp_in ',100(8e12.4,/)) 
!         write (6, 931) (obj%rmsamp_out(i),i=1,npts) 
  931    format(' rmsamp_out ',100(8e12.4,/))  
!         write (6, 937) (obj%rmswt(i),i=1,npts) 
  937    format(' rmswt ',100(8e12.4,/)) 

         if ( ierr == FATAL_ERROR) then
           ntr = FATAL_ERROR
           return
         end if

         if ( obj%iflow ==2 ) then
           ntr = NO_MORE_TRACES
         end if

       end if
        
       call binsort( obj%binsort, ntr, hd, tr) 
       if ( ntr == NO_MORE_TRACES ) then     ! end of disk file
          obj%iflow = 3
          call binsort_close(obj%binsort)
          return           
       end if

       if( obj%type_gain == 'OFFSET') then
          offold = hd(6,1)*0.5
          locn = (offold-obj%off_rec_init)/obj%off_rec_inc + 1
       else
          locn = hd(7,1) - obj%cmp_init + 1
       end if
 
       tr(:obj%ndpt,1) = tr(:obj%ndpt,1)*obj%rmswt(locn)

      return  
      end subroutine drc_out 
 
      subroutine drc_fill(xinput, nx, ierr) 
!----------------------------------------------------------------------
!     drc_fill --  linear interpolate missing values.
!        xinput -- input array
!        nx     -- number of values in the input array
!        ierr   -- error status
!----------------------------------------------------------------------

      implicit none

      integer      :: nx 
      real         :: xinput(nx) 
      integer      :: ierr

      integer                ::     npt, i !  local 
      real , dimension(nx)   :: y, x, xout      !  local
      real                   :: xs, xe          !  local
 
      npt = 0 
      xs = 1. 
      xe = nx 
      do i = 1, nx 
        if (xinput(i) <= 0.0) cycle  
        npt = npt + 1 
        y(npt) = xinput(i) 
        x(npt) = i 
      end do 
      if (npt <= 0)  then
         write(pc_get_lun(),*) ('drc_fill: Fatal error--input rms ampl = 0')
         ierr = FATAL_ERROR
      end if 
 
      call interp_1d_var_lin_real     &
          (x, y, npt, xout, xinput, nx, xs, xe)

      return  
      end subroutine drc_fill 


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine drc_wrapup (obj)
      implicit none
      type(drc_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine drc_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module drc_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

