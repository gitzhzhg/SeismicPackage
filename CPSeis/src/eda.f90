!<CPS_v1 type="PROCESS"/>

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
! Name       : EDA  (Edge Detection Attribute)
! Category   : filters
! Written    : 2000-01-11   by: Rob Meek
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Calculate edge detection attribute on 3D seismic volumes.
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
! Coherence is a measure of the extent to which the amplitude of a sample on a
! seismic trace resembles the amplitudes of other samples that are nearby in
! space and time.  Thus a region in a seismic data volume with a high coherence
! contains samples that are similar in amplitude whereas another region with 
! low coherence contains samples that are dissimilar in amplitude.  
!
! The EDA process uses a matrix singular value decomposition algorithm to 
! calculate coherence on a sample by sample basis for the entire input dataset.
! Replacing the original sample amplitudes in a data volume with the coherence 
! values associated with those samples results in a coherence volume which has
! the appearance of enhancing the edges of faults and other geological 
! features.  Such an "Edge Enhanced" data volume may make the existence and 
! location of geological features more obvious.
!
! EDA calculates coherence on a small subset volume of seismic data as follows:
!
!     1.  Read in and form the subset volume (for example, three traces each in 
!     the fast and slow directions and five trace samples).
!
!     2.  Form a matrix of sample amplitudes from the subset, where each column
!     is made up of the amplitudes at a given trace time with adjacent columns
!     corresponding to consecutive trace sample times.
!
!     3.  Calculate the singular value decomposition (SVD) of the matrix.
!
!     4.  The coherence value for the subset volume is given by:
!
!             (maximum eigenvalue)**PWR / (sum of (eigenvalues)**PWR),
!
!     where the eigenvalues are from the SVD calculation and PWR is a user 
!     specified parameter.
!
!     5.  Store this coherence value, go to the next subset volume and repeat 
!     steps 1 - 4 until the entire input volume has been processed.  Subset 
!     volumes overlap in space and time so that a separate SVD calculation is
!     made for each input data sample.
!
! The maximum coherence value is 1.0 which corresponds to the matrix elements 
! being being very similar in amplitude.  Lower coherence values indicate less 
! similarity in matrix element amplitude.
!     
! EDA expects to be fed a line at a time from gathr. The lines do not have
! to be all the same size. EDA will bin traces within a line. If this is
! not what you want, do your own infilling. EDA passes traces without
! alteration and outputs a file that contains the filtered data. It is not
! a parallel safe code, so do not include it in parallel jobs.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! EDA is intended for 3D migrated data volumes.
!
!
! Preprocessing
!
! If the input data are noisy, improved results may be obtained by running the 
! data through the CTAN process using the cosine of instantaneous phase 
! attribute prior to running EDA.  These complex trace attributes are 
! independent of trace amplitude and may produce a more noise-free result.
!
!
! Size of Data Subset Volume
!
! The size of the data subset volume used for the coherence calculation 
! determines the scale of the detail in the edge enhanced output volume.  
! Smaller subset volumes yield a finer grained detail and require less 
! run-time.  Larger subset volumes yield smoother detail and require more
! run-time.  However, small subset volumes may produce an output that is too
! jittery.
!
! NUM_TR = 3 and NUM_SAMP = 5 are typical values for a small subset volume.
!
!
! Power in Coherence Calculation
!
! The PWR parameter allows you to adjust the apparent contrast of the output
! volume.  When PWR is large, there will be a greater dynamic range between the
! largest and the smallest coherence values and when PWR is small, there will
! be a smaller dynamic range between the largest and the smallest coherence
! values.  PWR = 2.5 is a typical value.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! Process is a multiple-trace process.
! This process requires traces to be gathered.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process does not alter input traces.
! This process outputs the same traces as it receives.
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
! NWIH      number of words in trace header         used but not changed
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
! Hwd#    Description                Action taken
! ----    -----------                ------------
!  2      Head mute index            used
!         HDR_FAST                   used
!         HDR_SLOW                   used
! 25      LAV                        recalculated
! 64      Tail mute index            used
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date      Author       Description
!     ----      ------       -----------
! 9. 2006-06-12 B. Menger    Removed Unused Variables.
! 8. 2002-09-11 Stoeckley    Change to use the MTH module for binning.
! 7. 2001-08-08 Stoeckley    Change two RSVD arguments to zero for a six-fold
!                             speedup with identical results; move the RSVD
!                             subroutine to a new primitive with the same name;
!                             change the output scaling (including sign change
!                             to make discontinuities large rather than small
!                             amplitude).
! 6. 2001-07-26 RSDay        TIM_BEG,TIM_END default to trace settings
! 5. 2001-07-11 RSDay        Fixed origin shift of output samples
! 4. 2001-05-17 RSDay        Fixed bug, cmplx used when not needed.
!                            was: obj%a(m,n) = cmplx(obj%trc(i1,ix1,i),0.0) 
!                            now: obj%a(m,n) = obj%trc(i1,ix1,i)
!                            Bad number of elements in singular sum
! 3. 2001-05-03 RSDay        Can output filtered data to either an output file
!                            or to the trace stream.
! 2. 2001-03-28 RSDay        Enabled access to num_tr, num_samp fields
! 1. 2001-03-09 RSDay        Original - converted to New CPS
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   both      whether this process ever needs to request traces.
! NEED_LABEL     both      whether this process needs a label.     
! TWOSETS        both      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= fast_tot       if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
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

!<gui_def>
!<NS TTRIN Process/NC=80>
!          Read seismic traces from magnetic tape.
!
! O_FILE=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! HDR_FAST= `II  FAST_TOT=`IIII  FAST_INIT=`FFFFFFF FAST_INC=`FFFFFFF
! HDR_SLOW= `II  SLOW_TOT=`IIII  SLOW_INIT=`FFFFFFF SLOW_INC=`FFFFFFF
!
! TIM_BEG = `FFFFFFF TIM_END = `FFFFFFF   PWR=`FFFFFFF
!
! NUM_TR =`II     NUM_SAMP=`II     NSLICE=`II
!
!<PARMS O_FILE[/ML=96/XST]>
!</gui_def>
!

!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!<Help KEYWORD="FAST_LAST">
!<Tip> Last value of HDR_FAST for input data. </Tip>
! Default = 1000.0
! Allowed = real 
!</Help>
!<Help KEYWORD="SLOW_LAST">
!<Tip> Last value of HDR_SLOW for input data. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<HelpSection>
!
!
!<Help KEYWORD="O_FILE">
!<Tip> Output trace data file. </Tip>
! Default = "NONE"
! Allowed = 
! The filtered data will be output to this file. The filtered traces
! are passed to the next process if O_FILE=NONE, otherwise eda will pass
! traces transparently to the next process.
!</Help>
!
!<Help KEYWORD="NSLICE">
!<Tip> number of lines in computational window for edge detect. </Tip>
! Default = 3
! Allowed = int > 0
! number of lines in computational window for edge detect.
!</Help>
!
!<Help KEYWORD="HDR_SLOW">
!<Tip> Header word designating slowly changing coordinate of input data. </Tip>
! Default = 8
! Allowed = 1 - NWIH 
! HDR_SLOW should be the primary sort header word (changing slowly).
!</Help>
!
!<Help KEYWORD="SLOW_INIT">
!<Tip> First value of HDR_SLOW for input data. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="SLOW_INC">
!<Tip> Increment between HDR_SLOW values for input data. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!
!<Help KEYWORD="SLOW_TOT">
!<Tip> Total number of HDR_SLOW values in input data. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="HDR_FAST">
!<Tip> Header word designating rapidly changing coordinate of input data. </Tip>
! Default = 7
! Allowed = 1 - NWIH 
! HDR_FAST should be the secondary sort header word (changing rapidly).
!</Help>
!
!<Help KEYWORD="FAST_INIT">
!<Tip> First value of HDR_FAST for input data. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="FAST_INC">
!<Tip> Increment between HDR_FAST values for input data. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!
!<Help KEYWORD="FAST_TOT">
!<Tip> Total number of HDR_FAST values for input data. </Tip>
! Default = 1000
! Allowed = int < 0
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Time to start the coherence calculation. </Tip>
! Default = TSTRT
! Allowed = real
! Coherence calculation starts at the greater of TIM_BEG or head mute time.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Time to end the coherence calculation. </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG
! Coherence calculation ends at the smaller of TIM_END or tail mute time.
!</Help>
!
!<Help KEYWORD="PWR">
!<Tip> Power to use in the coherence calculation. </Tip>
! Default = 2.5
! Allowed = real > 0.0
! The coherence value for the subset volume is given by:
!
!          (maximum eigenvalue)**PWR / (sum of (eigenvalues)**PWR),
!
! where the eigenvalues are from the SVD calculation.
!</Help>
!
!<Help KEYWORD="NUM_SAMP">
!<Tip> Number of samples to use in the coherence calculation. </Tip>
! Default = 5
! Allowed = odd int >= 3
! The coherence calculation is performed on a data subset volume made up of 
! NUM_TR traces in both the fast-direction and the slow-direction and of length 
! NUM_SAMP samples in the time direction.
!</Help>
!
!<Help KEYWORD="NUM_TR">
!<Tip> Number of traces to use in the coherence calculation. </Tip>
! Default = 3
! Allowed = odd int >= 3
! The coherence calculation is performed on a data subset volume made up of 
! NUM_TR traces in both the fast-direction and the slow-direction and of length 
! NUM_SAMP samples in the time direction.
!</Help>
!
!</HelpSection>

!-------------------------------------------------------------------------------
!
!
!
! NOTES FOR CONVERSION PROGRAMMER

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! coher_svg.f
!
!  Rob Meek Feb 16, 2000
!
!  Eigenstructure coherency estimation
!  based on Gersztenkorn and Marfurt, 1999
!  This version though does the SVD on a rectangular matrix
!  rather than calculating the covariance matrix.
!  The subroutine ESD_CSVD is used to calculate the eigenvalues
!  currently this is a complex version.  I need to modify it
!  for real to make it save a little computation time.
!
! Parameters:
! nt= number of time samples
! nx= number of crosslines
! ny= number of inlines
! ns= number of samples to use in coherency estimate
! nr= number of traces in inline and crossline direction
!     to use in the estimate  (should allways be odd)
! pow=power to raise coherency by
! it1=first time sample to start coherency estimate
! it2=final time sample to end coherency estimate
!
!
!
! nr*nr must be greater than ns
!
! note: There will be edge effects, normally very coherent
! areas on the edges of the dataset
!
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------

      module eda_module   
      use pc_module
      use trcio_module
      use rsvd_module

      implicit none
      private

      character(len=100),public,save :: eda_ident = &
      '$Id: eda.f90,v 1.9 2006/06/12 13:03:50 Menger prod sps $'

      public :: eda_create     ! uses the parameter cache.
      public :: eda_initialize
      public :: eda_update     ! uses the parameter cache.
      public :: eda_delete
      public :: eda_wrapup
      public :: eda            ! main execution (trace processing) routine.

      type,public :: eda_struct

        private
        logical      :: skip_wrapup    !eda is no-op when true
        logical      :: fatal_error    !bad bad error detected
        integer      :: ipn
        integer      :: pmode          !processing mode
        integer      :: trcount        !count of processed traces
        integer      :: reject         !count of rejected traces
        integer      :: stdo
        character    :: o_file*96   !output file
        real         :: tim_beg     !start of time window
        real         :: tim_end     !end of time window
        integer      :: it1         !1st sample
        integer      :: it2         !last sample
        real         :: pwr
        integer      :: num_samp    !
        integer      :: num_tr
        integer      :: nslice
        integer      :: hdr_fast
        integer      :: hdr_slow
        integer      :: fast_tot
        integer      :: slow_tot
        double precision :: fast_inc
        double precision :: slow_inc
        double precision :: fast_init
        double precision :: slow_init
        integer      :: gcount      !group counter
        real,pointer    :: cov(:)
!       complex,pointer :: a(:,:)
!       complex,pointer :: u(:,:)
!       complex,pointer :: v(:,:)
        real,pointer :: a(:,:)
        real,pointer :: u(:,:)
        real,pointer :: v(:,:)
        real,pointer    :: hdr(:,:)
        real,pointer    :: trc(:,:,:)
        real,pointer    :: buff(:)
        type(trcio_struct),pointer :: fileo
        integer      :: ndpt
        integer      :: nwih
        real         :: tstrt
        real         :: dt
      end type eda_struct

      type(eda_struct),pointer,save :: object      ! needed for traps.

      contains


!!---------------------------- create -------------------------------------!!
!!---------------------------- create -------------------------------------!!
!!---------------------------- create -------------------------------------!!


      subroutine eda_create(obj)
      type(eda_struct),pointer :: obj       ! arguments
      allocate (obj)
      nullify(obj%cov)
      nullify(obj%a)
      nullify(obj%u)
      nullify(obj%v)
      nullify(obj%hdr)
      nullify(obj%trc)
      nullify(obj%buff)
      nullify(obj%fileo)
      call eda_initialize (obj)
      return
      end subroutine eda_create


!!-------------------------- initialize -----------------------------------!!
!!-------------------------- initialize -----------------------------------!!
!!-------------------------- initialize -----------------------------------!!


      subroutine eda_initialize(obj)
      type(eda_struct),pointer :: obj       ! arguments
      obj%pmode   = 0       !1=input, 2=output, 3 NO_MORE_TRACES
      obj%trcount = 0       !count of processed traces
      obj%reject  = 0       !count of rejected traces
      call pc_get_global ('tstrt', obj%tstrt)
      call pc_get_global ('dt'   , obj%dt)
      call pc_get_global ('ndpt' , obj%ndpt)
      obj%tim_beg = obj%tstrt
      obj%tim_end = obj%tstrt + (obj%ndpt-1)*obj%dt
      obj%it1     = 0
      obj%it2     = 0
      obj%gcount  = 0
      obj%pwr     = 2.5
      obj%num_tr  = 3
      obj%num_samp= 5
      obj%nslice  = obj%num_tr
      obj%o_file   = 'NONE'
      obj%hdr_fast = 7
      obj%hdr_slow = 8
      obj%fast_tot = 1000
      obj%slow_tot = 1
      obj%fast_inc = 1
      obj%slow_inc = 1
      obj%fast_init = 0
      obj%slow_init = 0

      obj%ndpt    = 0  ! will have to test later to make sure has been reset.
      obj%dt      = 0.0
      obj%tstrt   = 0.0
      obj%nwih    = 0
      call eda_update (obj)
      return
      end subroutine eda_initialize


!!--------------------------- update --------------------------------------!!
!!--------------------------- update --------------------------------------!!
!!--------------------------- update --------------------------------------!!


      subroutine eda_update(obj)
      type(eda_struct),intent(inout),target :: obj       ! arguments
      integer  :: i_err
      integer  :: nw1,nw2,nw3        
      integer  :: nto,numtr,nb1,nb2
      logical  :: isgathered,tf
      character:: rwmode*4,msg*80
      object => obj               ! needed for traps.
      obj%skip_wrapup =.true.
      obj%fatal_error= .false.
!
! Get globals, project and job data
      obj%ipn = pc_get_ipn()
      obj%stdo= pc_get_lun()
      call pc_get_global ('numtr'    , numtr)
      call pc_get_global ('gathered' , isgathered)

      call pc_get_global ('tstrt', obj%tstrt)
      call pc_get_global ('dt'   , obj%dt)
      call pc_get_global ('ndpt' , obj%ndpt)
      call pc_get_global ('nwih' , obj%nwih)

      call pc_get('tim_beg'    ,obj%tim_beg)
      call pc_get('tim_end'    ,obj%tim_end)
      call pc_get('pwr'        ,obj%pwr)
      call pc_get('num_tr'     ,obj%num_tr)
      call pc_get('num_samp'   ,obj%num_samp)
      call pc_get('nslice'     ,obj%nslice)
      call pc_get('o_file'     ,obj%o_file)
      call pc_get('hdr_slow'   ,obj%hdr_slow)
      call pc_get('hdr_fast'   ,obj%hdr_fast)
      call pc_get('slow_tot'   ,obj%slow_tot)
      call pc_get('fast_tot'   ,obj%fast_tot)
      call pc_get('slow_inc'   ,obj%slow_inc)
      call pc_get('fast_inc'   ,obj%fast_inc)
      call pc_get('slow_init'  ,obj%slow_init)
      call pc_get('fast_init'  ,obj%fast_init)

      if(obj%o_file==' ') obj%o_file='NONE'
      if(obj%o_file(1:3)=='non') obj%o_file='NONE'
      if(obj%o_file(1:2)=='NO') obj%o_file='NONE'
      if(obj%pwr<0) obj%pwr=1.0
      if(obj%tim_beg < obj%tstrt) obj%tim_beg=obj%tstrt
      if(obj%tim_end < obj%tim_beg) obj%tim_end=obj%tim_beg
      obj%it1 = 1+ nint((obj%tim_beg-obj%tstrt)/obj%dt)
      obj%it2 = 1+ nint((obj%tim_end-obj%tstrt)/obj%dt)
      obj%it1 = max(1,obj%it1)
      obj%it1 = min(obj%ndpt,obj%it1)
      obj%it2 = max(obj%it1,obj%it2)
      obj%it2 = min(obj%ndpt,obj%it2)
      obj%tim_beg = obj%tstrt + (obj%it1-1)*obj%dt
      obj%tim_end = obj%tstrt + (obj%it2-1)*obj%dt
!     obj%nslice = max(1,obj%nslice)
      obj%nslice = obj%num_tr
      obj%hdr_fast=max(1,obj%hdr_fast)
      obj%hdr_slow=max(1,obj%hdr_slow)
      obj%hdr_fast=min(obj%nwih,obj%hdr_fast)
      obj%hdr_slow=min(obj%nwih,obj%hdr_slow)
      obj%slow_tot=max(1,obj%slow_tot)
      obj%fast_tot=max(1,obj%fast_tot)
      if(obj%slow_inc==0.0) obj%slow_inc=1.0
      if(obj%fast_inc==0.0) obj%fast_inc=1.0

      call pc_put('tim_beg'    ,obj%tim_beg)
      call pc_put('tim_end'    ,obj%tim_end)
      call pc_put('pwr'        ,obj%pwr)
      call pc_put('num_tr'     ,obj%num_tr)
      call pc_put('num_samp'   ,obj%num_samp)
      call pc_put('nslice'     ,obj%nslice)
      call pc_put('o_file'     ,obj%o_file)
      call pc_put('hdr_slow'   ,obj%hdr_slow)
      call pc_put('hdr_fast'   ,obj%hdr_fast)
      call pc_put('slow_tot'   ,obj%slow_tot)
      call pc_put('fast_tot'   ,obj%fast_tot)
      call pc_put('slow_inc'   ,obj%slow_inc)
      call pc_put('fast_inc'   ,obj%fast_inc)
      call pc_put('slow_init'  ,obj%slow_init)
      call pc_put('fast_init'  ,obj%fast_init)

      call pc_put_sensitive_field_flag ('num_tr', .true.)
      call pc_put_sensitive_field_flag ('num_samp', .true.)
      call pc_put_sensitive_field_flag ('nslice', .false.)

      call pc_put_control('parallel_safe', .false.)
!     call pc_put_control ('gathered'    , .false.)
      call pc_put_control ('ntapes'      , 0)
      call pc_put_control ('iftd'        , .false.)       ! default false
      call pc_put_control ('ndisk'       , 0)
      call pc_put_control ('setup_only'  , .false.)       ! default .false.
      if(obj%o_file=='NONE') then
        call pc_put_global ('numtr'        , obj%fast_tot)
        call pc_put_control ('need_label'  , .true.)
        call pc_put_control ('need_request', .true.)       ! default false
        call pc_put_control ('twosets'     , .true.)       ! default false
        call pc_put_control ('gathered'    , .true.)
        nto = obj%it2 - obj%it1 + 1
        call pc_put_global ('ndpt'         , nto)
        call pc_put_global ('tstrt'        , obj%tim_beg)
!       write(msg,'(''nto='',I6)') nto
!       call pc_info(msg)
!       write(msg,'(''tim_beg='',F10.5)') obj%tim_beg
!       call pc_info(msg)
      else
        call pc_put_control ('need_label'  , .false.)
        call pc_put_control ('need_request', .false.)       ! default false
        call pc_put_control ('twosets'     , .false.)       ! default false
      endif

      if(.not.isgathered) then
        call pc_info('EDA: WARNING - data not gathered - insert gather!')
        call pc_info(msg)
      endif
      if(pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      nw3 = obj%nslice
      nw2 = obj%num_tr
      nw1 = obj%num_samp
      nto = obj%it2 - obj%it1 + 1
      allocate(obj%cov(nto),stat=i_err)
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 1')
        return
      endif
      obj%cov=0
      allocate(obj%a(nw2*nw3,nw1),stat=i_err) 
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 2')
        return
      endif
      allocate(obj%u(nw2*nw3,nw2*nw3),stat=i_err) 
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 3')
        return
      endif
      allocate(obj%v(nw1,nw1),stat=i_err) 
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 4')
        return
      endif
      allocate(obj%hdr(obj%nwih,obj%fast_tot),stat=i_err) 
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 5')
        return
      endif
      obj%hdr=0.0
      allocate(obj%trc(obj%ndpt,obj%fast_tot,obj%nslice),stat=i_err) 
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 6')
        return
      endif
      obj%trc=0.0
      allocate(obj%buff(obj%ndpt),stat=i_err)
      if(i_err /= 0) then
        obj%fatal_error= .true.
        call pc_error ('EDA_UPDATE: ERROR - 7')
        return
      endif
      call eda_print(obj)

      if(obj%o_file /= 'NONE') then
        nb1=32
        nb2=32
        tf=.false.
        rwmode = 'w'
        obj%fileo => trcio_open(obj%o_file,rwmode,tf, obj%nwih,nto,nb1,nb2)
        if(.not.associated(obj%fileo)) then
          obj%fatal_error= .true.
          call pc_error ('EDA_UPDATE: ERROR - failed open of output')
          return
        endif
        obj%fileo%tmin = obj%tim_beg
        obj%fileo%dt = obj%dt
      endif
 
      return
      end subroutine eda_update


!!--------------------------- eda print -----------------------------------!!
!!--------------------------- eda print -----------------------------------!!
!!--------------------------- eda print -----------------------------------!!


      subroutine eda_print (obj)
      implicit none
      type(eda_struct),intent(in) :: obj       ! arguments
      integer  :: i,j,k  
      real     :: val1,val2,lav
      real     :: val_min,val_max
      if(obj%stdo<0) return
      write(obj%stdo,*) 'eda_print: fast_tot=',obj%fast_tot
      write(obj%stdo,*) 'eda_print: slow_tot=',obj%slow_tot
      write(obj%stdo,*) 'eda_print: it2=',obj%it2,' it1=',obj%it1
      call eda_stats(obj%cov,val1,val2,lav)
      write(obj%stdo,*) 'eda_print: cov min=',val1,' cov max=',val2,' lav=',lav
      do i= 1,obj%nslice
        val_min = obj%trc(1,1,i)
        val_max = obj%trc(1,1,i)
        do j=1,obj%fast_tot
          do k = obj%it1,obj%it2
            val_min = min(val_min,obj%trc(k,j,i))
            val_max = max(val_max,obj%trc(k,j,i))
          enddo
        enddo
      write(obj%stdo,*) 'eda_print:',i,' trc min=',val_min,' trc max=',val_max
      enddo

      return
      end subroutine eda_print


!!----------------------------- eda stats ---------------------------------!!
!!----------------------------- eda stats ---------------------------------!!
!!----------------------------- eda stats ---------------------------------!!


      subroutine eda_stats (data,val_max,val_min,lav)
      implicit none
      real,intent(in)     :: data(:)
      real,intent(inout)  :: val_min,val_max,lav
      integer  :: i,nto
      nto = size(data)
      val_min = data(1)
      val_max = data(1)
      do i = 1,nto
        val_min = min(val_min,data(i))
        val_max = max(val_max,data(i))
      enddo
      lav = max(abs(val_min),abs(val_max))
      return
      end subroutine eda_stats


!!----------------------------- delete ------------------------------------!!
!!----------------------------- delete ------------------------------------!!
!!----------------------------- delete ------------------------------------!!


      subroutine eda_delete (obj)
      implicit none
      type(eda_struct),pointer :: obj       ! arguments
      integer i_err
      if (.not. associated(obj)) return
      call eda_wrapup (obj)
      deallocate(obj%cov, stat=i_err)
      deallocate(obj%a, stat=i_err)
      deallocate(obj%u, stat=i_err)
      deallocate(obj%v, stat=i_err)
      deallocate(obj%hdr, stat=i_err)
      deallocate(obj%trc, stat=i_err)
      deallocate(obj%buff, stat=i_err)
      deallocate(obj, stat=i_err)
      nullify(object)
      return
      end subroutine eda_delete


!!---------------------------- wrapup -------------------------------------!!
!!---------------------------- wrapup -------------------------------------!!
!!---------------------------- wrapup -------------------------------------!!


      subroutine eda_wrapup(obj)
      implicit none
      type(eda_struct),intent(inout) :: obj       ! arguments
      integer :: i_err

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.
      write(obj%stdo,*) 'EDA:(wrapup) traces scanned=',obj%trcount
      write(obj%stdo,*) 'EDA:(wrapup) traces rejected=',obj%reject
      if(obj%o_file /= 'NONE') then
        i_err = trcio_close(obj%fileo)
        write(obj%stdo,*) 'EDA:(wrapup) filtered data in ',trim(obj%o_file)
      else
        write(obj%stdo,*) 'EDA:(wrapup) filtered data passed to trace flow'
      endif
!
      return
      end subroutine eda_wrapup


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine eda(obj,ntr,hd,tr,hd2,tr2)
      implicit none
      type(eda_struct), pointer   :: obj       ! arguments
      integer,intent(inout)       :: ntr
      double precision,intent(in) :: hd(:,:)
      real,intent(in)             :: tr(:,:)
      double precision,intent(out),optional :: hd2(:,:)
      real,intent(out),optional             :: tr2(:,:)

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      double precision   :: hddp(64)
      integer :: nx
      integer :: ny
      integer :: nti
      integer :: nto,it1,it2
      integer :: ns   !number of samples
      integer :: nr   !number of lines
      real    :: pow
      integer :: i_err
      integer :: xbin,ybin,yold
      integer :: nr2, nrhalf    , nrm,nsm 
      integer :: j, i, m, ix, it, itime, n, i1, ix1, ip 
      real :: s (64)  !ns
      real :: sum , sum_max
      integer :: isum
      real :: val1,val2,lav
      character :: card*80
!
!  Check for object integrity
      if (.not.associated(obj)) then
        call pc_error('EDA: object pointer is bad')
        ntr = FATAL_ERROR
        return
      endif
      if(obj%skip_wrapup) return
      if(obj%fatal_error) return
!
! Check to see if any traces for output
      if(ntr == NO_MORE_TRACES) then
        obj%pmode=3
        return
      endif   !if(ntr == NO_MORE_TRACES) then
      if(ntr == NEED_TRACES) then
        if(obj%pmode==3) ntr=NO_MORE_TRACES
        return
      endif
 
      it1 = obj%it1
      it2 = obj%it2
      ns  = obj%num_samp
      nsm = ns/2
      nr  = obj%nslice
      nx  = obj%fast_tot
      ny  = obj%slow_tot
      nti = obj%ndpt
      nto = it2 - it1 + 1 
      nr2 = nr*nr 
      nrm = nr/2
      nrhalf = int(nr/2) 

!
! buffer the input cluster
!
      i_err = eda_bin_trace(obj,hd(:,1),tr(:,1), xbin,ybin)
      yold = ybin
      if(i_err <= 0) then
        obj%reject = obj%reject + ntr
        if(obj%o_file=='NONE') ntr = NEED_TRACES
        write(obj%stdo,*) 'EDA: reject - xbin=',xbin,' ybin=',ybin,' ntr=',ntr
        return
      endif
      obj%gcount = obj%gcount+1
      if(obj%gcount > obj%slow_tot) then
        call eda_wrapup(obj)
        if(obj%o_file=='NONE') then
          ntr = NO_MORE_TRACES
          obj%pmode=3
        endif
        return
      endif
      write (obj%stdo, *) 'EDA: line=', ybin ,ntr
 
        if (obj%gcount == 1) then 
          obj%trcount = obj%trcount + ntr
          call eda_new_group(obj)
          m = 0
          do i = 1, ntr
            i_err = eda_bin_trace(obj,hd(:,i),tr(:,i), xbin,ybin)
            if(i_err > 0) then
              m = m+1
              if(m==1) then
                yold = ybin
              else
                if(yold /= ybin) then
                  ntr = FATAL_ERROR
                  write(card,'(''EDA: bad ybin '',I6,I6)') yold,ybin
                  call pc_error(card)
                  return
                endif
              endif
              do j = nrhalf + 1, nr 
                obj%trc(:,xbin,j) = tr(:,i)
              end do 
              obj%hdr(:,xbin) = hd(:,i)
            endif
          end do 
          obj%reject = obj%reject + ntr-m
          if(m==0) then
            if(obj%o_file=='NONE') ntr = NEED_TRACES
            write (obj%stdo, *) 'EDA: no traces?, line=', ybin ,ntr
            return
          endif
          do j=1,nrhalf
            do i=1,nx
              obj%trc(1:nti,i,j)=obj%trc(1:nti,i,nr)
            end do
          end do
 
        else if (obj%gcount > ny - nrhalf) then 
          obj%trcount = obj%trcount + ntr
!         obj%trc(:nti,:nx,nr) = obj%trc(:nti,:nx,nr-1) 
          call eda_new_group(obj)
          m=0
          do i = 1, ntr 
            i_err = eda_bin_trace(obj,hd(:,i),tr(:,i), xbin,ybin)
            if(i_err > 0) then
              m = m+1
              obj%trc(:,xbin,nr) = tr(:,i)
              obj%hdr(:,xbin) = hd(:,i)
            endif
          end do 
          obj%reject = obj%reject + ntr-m
        else
          obj%trcount = obj%trcount + ntr
          call eda_new_group(obj)
          m = 0
          do i = 1, ntr 
            i_err = eda_bin_trace(obj,hd(:,i),tr(:,i), xbin,ybin)
            if(i_err > 0) then
              m = m+1
              if(m==1) then
                yold = ybin
              else
                if(yold /= ybin) then
                  ntr = FATAL_ERROR
                  write(card,'(''EDA: bad ybin '',I6,I6)') yold,ybin
                  call pc_error(card)
                  return
                endif
              endif
              obj%trc(:nti,xbin,nr) = tr(:nti,i)
              obj%hdr(:,xbin) = hd(:,i)
            endif
          end do 
          obj%reject = obj%reject + ntr-m
          if(m==0) then
            if(obj%o_file=='NONE') ntr = NEED_TRACES
            write (obj%stdo, *) 'EDA: no traces?, line=', ybin ,ntr
            return
          endif
        endif 
 
 
!       call eda_print(obj)
        pow = obj%pwr
        sum_max=0
        isum=0
        do ix = 1, nx 
          it = 1 
          do itime = it1, it2 
            m = 0 
!
! create data matrix (nr traces in x & y direction)
!
            do i = 1, nr  !line
              do j = 1, nr  !trace in line
                m = m + 1 
                do n = 1, ns 
                ! i1 = itime - 1 + n 
                  i1 = itime - nsm + n 
                ! ix1 = ix + j - 1 
                  ix1 = ix + j - nrm 
                  i1 = max0(1,i1) 
                  i1 = min0(nti,i1) 
                  ix1 = max0(1,ix1) 
                  ix1 = min0(nx,ix1) 
 
                  obj%a(m,n) = obj%trc(i1,ix1,i) 
!                 obj%a(m,n) = cmplx(obj%trc(i1,ix1,i),0.0) 
 
                end do 
              end do 
            end do 
 
 
            ip = 0 !make sure is initialized

!           call eda_svd(obj%a,nr2,ns,nr2,ns,s,obj%u,obj%v,i_err)
!           if(i_err /= 0) then
!            write(obj%stdo,*) 'EDA: eda_svd failed'
!            call eda_wrapup(obj)
!            return
!           endif

     !!     call eda_rsvd (obj%a, nr2, ns, nr2, ns, ip, &  ! removed 2001-08-01
     !!                    nr2, ns, s, obj%u, obj%v)       ! removed 2001-08-01
 
            call rsvd     (obj%a, nr2, ns, nr2, ns, ip, &  ! added   2001-08-01
                           0,   0,  s, obj%u, obj%v)       ! added   2001-08-01
 
            sum = 0.0 
            do i = 1, ns  !nr2   BUG - RSD
              sum = sum + s(i) **pow
            end do 
 
            if(sum /= 0) then
     !!      obj%cov(it) = s(1)**pow/sum*30000.0 - 20000.0 ! removed 2001-08-01
             obj%cov(it) = 1.0 - s(1)**pow/sum             ! added   2001-08-01
            else
             obj%cov(it) = 0.0
            endif
            it = it + 1 
 
 
          end do  !itime loop
 
          hddp(:) = obj%hdr(:,ix)
          hddp(1) = (obj%gcount-1)*obj%fast_tot + ix
          hddp(3) = obj%gcount
          call eda_stats(obj%cov,val1,val2,lav)
          hddp(25)= lav
          hddp(64)= nto
          if(obj%o_file /= 'NONE') then
            i_err=trcio_write_trace(obj%fileo,hddp,obj%cov)
          else
            ntr=ix
            hd2(1:obj%nwih,ix) = hddp(1:obj%nwih)
            tr2(1:nto,ix) = obj%cov(1:nto)
          endif
 
        end do  !ix loop
!
! shift lines down by 1
!
      obj%trc(:nti,:nx,:nr-1) = obj%trc(:nti,:nx,2:nr) 
 
      return  
      end subroutine eda


!!-------------------------- eda bin trace ---------------------------------!!
!!-------------------------- eda bin trace ---------------------------------!!
!!-------------------------- eda bin trace ---------------------------------!!

! status = -1 ERROR
!        =  0 REJECT TRACE
!           1 OK


      integer function eda_bin_trace(obj,hd,tr, xbin,ybin) result(status)
      type(eda_struct),intent(inout)   :: obj       ! arguments
      double precision,intent(in)      :: hd(:)
      real,intent(in)                  :: tr(:)
      integer,intent(out)              :: xbin,ybin
      status = -1      
      xbin = mth_bin_number (obj%fast_init, obj%fast_inc, hd(obj%hdr_fast))
      ybin = 1
      if(xbin <1 .or. xbin > obj%fast_tot) then
        status = 0
        return  
      endif
      if(obj%slow_tot > 1) then
        ybin = mth_bin_number (obj%slow_init, obj%slow_inc, hd(obj%hdr_slow))
        if(ybin <1 .or. ybin > obj%slow_tot) then
          status = 0
          return  
        endif
      endif
      status = 1
      return  
      end function eda_bin_trace


!!-------------------------- eda new group ---------------------------------!!
!!-------------------------- eda new group ---------------------------------!!
!!-------------------------- eda new group ---------------------------------!!

! Initialize arrays for new group


      subroutine eda_new_group(obj)
      type(eda_struct), intent(inout)   :: obj       ! arguments
      integer  :: i
      do i = 1,obj%fast_tot
         obj%trc(:,i,obj%nslice) = 0.0
         obj%hdr(:,i) = 0.0
         obj%hdr(obj%hdr_fast,i) &
                     = mth_bin_center (obj%fast_init, obj%fast_inc, i)
         obj%hdr(obj%hdr_slow,i) &
                     = mth_bin_center (obj%slow_init, obj%slow_inc, obj%gcount)
         obj%hdr(2,i) = 1.0
         obj%hdr(3,i) = obj%gcount
         if(obj%hdr_fast /= 7) obj%hdr(7,i) = i
         if(obj%hdr_slow /= 8) obj%hdr(8,i) = obj%gcount
         obj%hdr(64,i) = obj%it2 - obj%it1 + 1
      enddo
      return  
      end subroutine eda_new_group


!!----------------------------- eda svd -----------------------------------!!
!!----------------------------- eda svd -----------------------------------!!
!!----------------------------- eda svd -----------------------------------!!

! not used:


      subroutine eda_svd(a,mmax,nmax,m,n,s,u,v,i_err)
      real , intent(inout) :: a(mmax,nmax) 
      real , intent(inout) :: u(mmax,mmax) 
      real , intent(inout) :: v(nmax,nmax) 
      real , intent(inout) :: s(nmax) 
      integer, intent(inout) :: i_err
      integer , intent(in) :: mmax 
      integer , intent(in) :: nmax 
      integer , intent(in) :: m 
      integer , intent(in) :: n 
      real    :: work(1000)  
      integer :: lwork
      integer :: ie,itauq,itaup,iwork

      character  :: jobu,jobvt
      jobu ='N'
      jobvt='N'
      lwork=size(work)
!     call  sgesvd( jobu, jobvt, m, n, a, mmax, s, u, mmax, v, nmax,&
!      work, lwork, i_err )
!
!              Bidiagonalize R in A
!              (Workspace: need 4*N, prefer 3*N+2*N*NB)
!
       ie = 1
       itauq = ie + n
       itaup = itauq + n
       iwork = itaup + n
!      call sgebrd( m, n, a, m, s, work( ie ), work( itauq ),&
!                   work( itaup ), work( iwork ), lwork-iwork+1,&
!                   i_err )
!
!              Perform bidiagonal QR iteration, if desired, computing
!              left singular vectors in U and computing right singular
!              vectors in VT
!              (Workspace: need BDSPAC)
!
!       iwork = ie + n
!       ncvt=0
!       nru=0
!       call sbdsqr( 'U', n, ncvt, nru, 0, s, work( ie ), vt,&
!       nmax, u, mmax, dum, 1, work( iwork ), i_err )

      return  
      end subroutine eda_svd


!!----------------------------- eda rsvd ----------------------------------!!
!!----------------------------- eda rsvd ----------------------------------!!
!!----------------------------- eda rsvd ----------------------------------!!

! not used (moved to new primitive RSVD):


      subroutine eda_rsvd (a,mmax,nmax,m,n,ip,nu,nv,s,u,v)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER , INTENT(IN) :: MMAX 
      INTEGER , INTENT(IN) :: NMAX 
      INTEGER , INTENT(IN) :: M 
      INTEGER , INTENT(IN) :: N 
      INTEGER , INTENT(IN) :: IP 
      INTEGER , INTENT(IN) :: NU 
      INTEGER , INTENT(IN) :: NV 
      REAL , INTENT(INOUT) :: A(MMAX,NMAX) 
      REAL , INTENT(INOUT) :: S(NMAX) 
      REAL , INTENT(INOUT) :: U(MMAX,MMAX) 
      REAL , INTENT(INOUT) :: V(NMAX,NMAX) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NP, N1, K, K1, I, J, KK, LL, L, L1 
      REAL :: Q, R 
      REAL , DIMENSION(100) :: B, C, T 
      REAL :: ETA, TOL, Z, W, EPS, CS, SN, F, H, X, Y, G 
      INTEGER J1S(1) 
!-----------------------------------------------
!
!   Singular value decomposition of an  M by N  complex matrix  A,
!   where M .GT. N .  The singular values are stored in the vector
!   S. The first NU columns of the M by M unitary matrix U and the
!   first NV columns of the N by N unitary matrix V  that minimize
!   Det(A-USV*) are also computed.
!
!
!         P.A. Businger and G.H. Golub, "Singular Value Decomposition
!         of a Complex Matrix," Communications of the ACM, vol. 12,
!         pp. 564-565, October 1969.
!
!   This algorithm is reprinted by permission, Association for
!   Computing Machinery; copyright 1969.
!
      DATA ETA, TOL/ 1.2E-7, 2.4E-32/  
      NP = N + IP 
      N1 = N + 1 
!   Householder reduction
      C(1) = 0. 
      K = 1 
   10 CONTINUE 
      K1 = K + 1 
!   Elimination of A(I,K) , I=K+1,...,M
      Z = 0. 
      DO I=K,M
        Z=Z+A(I,K)**2
      ENDDO
      B(K) = 0. 
      IF (Z > TOL) THEN 
        Z = SQRT(Z) 
        B(K) = Z 
        W = ABS(A(K,K)) 
        Q = (1.,0.) 
        IF (W /= 0.) Q = A(K,K)/W 
        A(K,K) = Q*(Z + W) 
        IF (K /= NP) THEN 
          DO J=K1,NP
            Q=(0.,0.)
            DO I=K,M
              Q=Q+A(I,K)*A(I,J)
            ENDDO
            Q=Q/(Z*(Z+W))
            DO I=K,M
              A(I,J)=A(I,J)-Q*A(I,K)
            ENDDO
          ENDDO
!   Phase transformation
          Q = -A(K,K)/ABS(A(K,K)) 
          A(K,K1:NP) = Q*A(K,K1:NP) 
!   Elimination of A(K,J) , J=K+2,...,N
        ENDIF 
      ENDIF 
      IF (K == N) GO TO 140 
      Z = 0. 
      Z = SUM(A(K,K1:N)**2) 
      C(K1) = 0. 
      IF (Z > TOL) THEN 
        Z = SQRT(Z) 
        C(K1) = Z 
        W = ABS(A(K,K1)) 
        Q = (1.,0.) 
        IF (W /= 0.) Q = A(K,K1)/W 
        A(K,K1) = Q*(Z + W) 
        DO I=K1,M
        Q=(0.,0.)
          DO J=K1,N
            Q=Q+A(K,J)*A(I,J)
          ENDDO
          Q=Q/(Z*(Z+W))
          DO J=K1,N
            A(I,J)=A(I,J)-Q*A(K,J)
          ENDDO
        ENDDO
!   Phase transformation
        Q = -A(K,K1)/ABS(A(K,K1)) 
        A(K1:M,K1) = A(K1:M,K1)*Q 
      ENDIF 
      K = K1 
      GO TO 10 
!   Tolerance for negligible elements
  140 CONTINUE 
      EPS = 0. 
      DO K = 1, N 
        S(K) = B(K) 
        T(K) = C(K) 
        EPS = AMAX1(EPS,S(K)+T(K)) 
      END DO 
      EPS = EPS*ETA 
!   Initialization of U and V
      IF (NU /= 0) THEN 
        DO J = 1, NU 
          U(:M,J) = (0.,0.) 
          U(J,J) = (1.,0.) 
        END DO 
      ENDIF 
      IF (NV /= 0) THEN 
        DO J = 1, NV 
          V(:N,J) = (0.,0.) 
          V(J,J) = (1.,0.) 
        END DO 
      ENDIF 
!   QR diagonalization
      DO KK = 1, N 
        K = N1 - KK 
!   Test for split
  220   CONTINUE 
        DO LL = 1, K 
          L = K + 1 - LL 
          IF (ABS(T(L)) <= EPS) GO TO 290 
          IF (ABS(S(L-1)) > EPS) CYCLE  
          EXIT  
        END DO 
!   Cancellation of B(L)
        CS = 0. 
        SN = 1. 
        L1 = L - 1 
        DO I = L, K 
          F = SN*T(I) 
          T(I) = CS*T(I) 
          IF (ABS(F) <= EPS) EXIT  
          H = S(I) 
          W = SQRT(F*F + H*H) 
          S(I) = W 
          CS = H/W 
          SN = -F/W 
          IF (NU /= 0) THEN 
            DO J = 1, N 
              X = REAL(U(J,L1)) 
              Y = REAL(U(J,I)) 
              U(J,L1) = X*CS + Y*SN 
              U(J,I) = Y*CS - X*SN 
            END DO 
          ENDIF 
          IF (NP == N) CYCLE  
          DO J=N1,NP
            Q=A(L1,J)
            R=A(I,J)
            A(L1,J)=Q*CS+R*SN
            A(I,J)=R*CS-Q*SN
          ENDDO
        END DO 
!   Test for convergence
  290   CONTINUE 
        W = S(K) 
        IF (L == K) GO TO 360 
!   Origin shift
        X = S(L) 
        Y = S(K-1) 
        G = T(K-1) 
        H = T(K) 
        F = ((Y - W)*(Y + W) + (G - H)*(G + H))/(2.*H*Y) 
        G = SQRT(F*F + 1.) 
        IF (F < 0.) G = -G 
        F = ((X - W)*(X + W) + (Y/(F + G) - H)*H)/X 
!   QR step
        CS = 1. 
        SN = 1. 
        L1 = L + 1 
        DO I = L1, K 
          G = T(I) 
          Y = S(I) 
          H = SN*G 
          G = CS*G 
          W = SQRT(H*H + F*F) 
          T(I-1) = W 
          CS = F/W 
          SN = H/W 
          F = X*CS + G*SN 
          G = G*CS - X*SN 
          H = Y*SN 
          Y = Y*CS 
          IF (NV /= 0) THEN 
            DO J = 1, N 
              X = REAL(V(J,I-1)) 
              W = REAL(V(J,I)) 
              V(J,I-1) = X*CS + W*SN 
              V(J,I) = W*CS - X*SN 
            END DO 
          ENDIF 
          W = SQRT(H*H + F*F) 
          S(I-1) = W 
          CS = F/W 
          SN = H/W 
          F = CS*G + SN*Y 
          X = CS*Y - SN*G 
          IF (NU /= 0) THEN 
            DO J=1,N
              Y=U(J,I-1)
              W=U(J,I)
              U(J,I-1)=Y*CS+W*SN
              U(J,I)=W*CS-Y*SN
            ENDDO
          ENDIF 
          IF (N == NP) CYCLE  
          DO J=N1,NP
            Q=A(I-1,J)
            R=A(I,J)
            A(I-1,J)=Q*CS+R*SN
            A(I,J)=R*CS-Q*SN
          ENDDO
        END DO 
        T(L) = 0. 
        T(K) = F 
        S(K) = X 
        GO TO 220 
!   Convergence
  360   CONTINUE 
        IF (W >= 0.) CYCLE  
        S(K) = -W 
        IF (NV == 0) CYCLE  
        V(:N,K) = -V(:N,K) 
      END DO 
!   Sort singular values
      DO K = 1, N 
        G = -1. 
        J = K 
        IF (N - K + 1 > 0) THEN 
          J1S = MAXLOC(S(K:N)) - 1 + K 
          IF (S(J1S(1)) > G) THEN 
            G = S(J1S(1)) 
            J = J1S(1) 
          ENDIF 
        ENDIF 
        IF (J == K) CYCLE  
        S(J) = S(K) 
        S(K) = G 
        IF (NV /= 0) THEN 
          DO I=1,N
            Q=V(I,J)
            V(I,J)=V(I,K)
            V(I,K)=Q
          ENDDO
        ENDIF 
        IF (NU /= 0) THEN 
          DO I=1,N
            Q=U(I,J)
            U(I,J)=U(I,K)
            U(I,K)=Q
          ENDDO
        ENDIF 
        IF (N == NP) CYCLE  
        DO I=N1,NP
          Q=A(J,I)
          A(J,I)=A(K,I)
          A(K,I)=Q
        ENDDO
      END DO 
!   Back transformation
      IF (NU /= 0) THEN 
        DO KK = 1, N 
          K = N1 - KK 
          IF (B(K) == 0.) CYCLE  
          Q = -A(K,K)/ABS(A(K,K)) 
          U(K,:NU) = Q*U(K,:NU) 
          DO J = 1, NU 
            Q = SUM(A(K:M,K)*U(K:M,J)) 
            Q = Q/(ABS(A(K,K))*B(K)) 
            U(K:M,J) = U(K:M,J) - Q*A(K:M,K) 
          END DO 
        END DO 
      ENDIF 
      IF (NV /= 0) THEN 
        IF (N >= 2) THEN 
          DO KK = 2, N 
            K = N1 - KK 
            K1 = K + 1 
            IF (C(K1) == 0.) CYCLE  
            Q = -A(K,K1)/ABS(A(K,K1)) 
            V(K1,:NV) = Q*V(K1,:NV) 
            DO J = 1, NV 
              Q = SUM(A(K,K1:N)*V(K1:N,J)) 
              Q = Q/(ABS(A(K,K1))*C(K1)) 
              V(K1:N,J) = V(K1:N,J) - Q*A(K,K1:N) 
            END DO 
          END DO 
        ENDIF 
      ENDIF 
      return  
      end subroutine eda_rsvd 


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module eda_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

