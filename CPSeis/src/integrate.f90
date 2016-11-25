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
!!------------------------------- integrate.f90 ---------------------------------!!


!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S             
!
! Name       : integrate
! Category   : filters
! Written    : 2008-08-28   by: Bill Menger
! Revised    : 2008-08-28   by: Bill Menger
! Maturity   : beta
! Purpose    : Performs trace integration and then removes low frequency drift.
! Portability: No known limitations.
! Parallel   : Yes
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!  This is a direct copy of the traceint3d kernel from Steve Cook's program called
!  Traceint3d on the old COP Launcher.  The code is now inside of GeoCraft but is
!  added here for batch submittal if one has many volumes to integrate.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!  The operator length (in number of samples) is used for removing low frequency
!  drift.  The default should work (80 samples).  If the trace is less than 80
!  samples long, the process will use trace_length/8+1 for the window.
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
! No special requirements.
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
! This process outputs the same traces it receives but altered (integrated!)
! This process outputs traces with same gather status as the input traces.
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       --> standard actions taken
! GATHERED  whether traces are a legitimate gather  --> does not care
! NWIH      number of words in trace header         --> not changed
! NDPT      number of sample values in trace        --> not changed
! TSTRT     starting time on trace                  --> not changed
! DT        trace sample interval                   --> not changed
! GRID      grid transformation structure           --> not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   2     top mute                   changed
!  25     largest absolute value     changed
!  64     bottom mute                changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2008-08-28  Bill Menger       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS           
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
!
! This code does a running sum through the trace and then removes low frequency
! drift by removing drift over each chunk of "operator_length" length. 
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
! -- from the java... GeoCraft (TraceIntegrationImpl.java)
! 
! public static float[] integrate(IAreaOfInterest inputAOI, Trace trace, int oplen) {
! 
!     _oplen = oplen;
! 
!     // Get the input trace for the inline and xline.
!     float[] tvals = trace.getData();
! 
!     // no null traces are allowed
!     if (AreaOfInterestHelper.isInAreaOfInterest(inputAOI, trace) && tvals.length > 1) {
!       int firstLive = firstLive(tvals, 0.0001);
!       int lastLive = lastLive(tvals, 0.0001);
! 
!       if (Double.isNaN(tvals[0])) {
!         tvals[0] = Float.MIN_VALUE;
!       }
! 
!       // integrates trace also known as runSum
!       for (int i = 1; i < tvals.length; i++) {
! 
!         if (!Double.isNaN(tvals[i])) {
!           tvals[i] = tvals[i] + tvals[i - 1];
!         } else {
!           tvals[i] = tvals[i - 1];
!         }
! 
!       }
! 
!       // user will be required to enter an operator length (> 1) for the removal of low frequency
!       // drift
!       if (_oplen > 1) {
!         tvals = removeLowFreqDrift(tvals, _oplen);
!       }
! 
!       // re-zero to the orginal mute positions, if necessary
!       for (int i = 0; i < firstLive; i++) {
!         tvals[i] = Float.MIN_VALUE;
!       }
! 
!       for (int i = lastLive + 1; i < tvals.length; i++) {
!         tvals[i] = Float.MIN_VALUE;
!       }
!     }
! 
!     return tvals;
!   }
! 
! 
! public static final float[] removeLowFreqDrift(float[] tvals, int oplen) {
! 
!     _oplen = oplen;
! 
!     if (tvals.length < _oplen) {
!       throw new IllegalArgumentException("Trace is shorter than operator");
!     }
! 
!     int imax = tvals.length;
! 
!     // creation of smoothed version of trace
!     float[] smoother = new float[tvals.length];
!     smoother = smoothMeanBased(tvals, smoother, _oplen);
! 
!     // smoothed trace subtract from original trace to remove drift
!     for (int i = 0; i < imax; i++) {
!       tvals[i] = tvals[i] - smoother[i];
!     }
! 
!     return tvals;
!   }
!  /**
!    * Top mute picker.
!    * @param tvals is an array of trace values.
!    * @param threshold provides cutoff value for the trace samples.
!    * @return index of first value exceeding threshold, starting from the top.
!    */
! 
!   private static int firstLive(float[] tvals, double threshold) {
! 
!     int i;
!     int firstLive = 0;
! 
!     for (i = 0; i < tvals.length; i++) {
!       if (Math.abs(tvals[i]) > threshold) {
!         firstLive = i;
!         i = tvals.length + 1;
!       }
! 
!     }
! 
!     return firstLive;
!   }
! 
!   /**
!    * Bottom mute picker
!    * @param tvals is an array of trace values.
!    * @param threshold provides cutoff value for the trace samples.
!    * @return index of first value exceeding threshold, starting from the bottom
!    */
! 
!   private static int lastLive(float[] tvals, double threshold) {
! 
!     int i;
!     int lastLive = tvals.length - 1;
! 
!     for (i = tvals.length - 1; i >= 0; i--) {
!       if (Math.abs(tvals[i]) > threshold) {
!         lastLive = i;
!         i = -1;
!       }
!     }
! 
!     return lastLive;
!   }
! 
!   /**
!    * Smoothes a trace using an average-based function.
!    * @param in input trace values.
!    * @param out output trace values.
!    * @param windLen window length used to run function.
!    * @return array of smoothed trace values.
!    */
!   private static float[] smoothMeanBased(float[] in, float[] out, int windLen) {
! 
!     int i = 0;
!     int imax = windLen / 2;
! 
!     // first half of first window
!     while (i < imax) {
!       out[i] = in[i];
!       i++;
!     }
! 
!     /*
!      * calculates average for middle portion of array; the window length being the number of array
!      * values that are summed for the average.
!      */
!     imax = in.length - windLen / 2;
! 
!     while (i < imax) {
!       out[i] = calculateAverage(in, windLen, i);
!       i++;
!     }
! 
!     // last half of last window
!     imax = in.length;
! 
!     while (i < imax) {
!       out[i] = in[i];
!       i++;
!     }
! 
!     return out;
!   }
! 
!   /**
!    * Calculates average in order to smooth data.
!    * @param in array of trace values.
!    * @param window number of values to average.
!    * @param count keeps track of position in array.
!    * @return average.
!    */
!   private static float calculateAverage(float[] in, int window, int count) {
! 
!     float sum = 0;
!     int live = 0;
!     int j = count - window / 2;
!     int jmax = count + window / 2;
!     float v;
! 
!     while (j < jmax) {
! 
!       v = in[j];
!       if (!Double.isNaN(v)) {
!         sum += v;
!         live++;
!       }
!       j++;
! 
!     }
! 
!     if (live != 0) {
!       return sum / live;
!     } else {
!       return in[count];
!     }
! 
!   }
! 
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS integrate Process/NC=80>
! Window length (in number of samples) for smoothing 
! WINDOW_LENGTH=`III
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!<Help KEYWORD="WINDOW_LENGTH">
!<Tip> Number of samples in your smoothing window for removing drift.</Tip>
! Default = 80 
! Allowed >= 1 and <= trace length
! "Drift" is the low frequency ramp function you can inadvertently insert into
! the data when performing the integration (or running sum).  To remove it,
! the trace is smoothed using a sliding average operator of length = the value
! you specify in "window_length".
!</Help>
!</HelpSection>
!-------------------------------------------------------------------------------

module integrate_module
!!--------------------------- start of module ------------------------------!!
  use pc_module
  use named_constants_module
  use lav_module

  implicit none
  private
  public :: integrate_create
  public :: integrate_initialize
  public :: integrate_update
  public :: integrate_delete
  public :: integrate            ! main trace processing routine.
  public :: integrate_wrapup

  character(len=100),public,save :: integrate_IDENT = &
  '$Id: integrate.f90,v 1.18 2008/08/27 22:13:25 mengewm Exp $'

  !!---------------------- parameter structure -------------------------------!!


  type,public :: integrate_struct              
 
    private
    logical                    :: skip_wrapup      ! wrapup flag.
    integer                    :: ipn      ! process number.
    integer                    :: nwih     ! number of header words.
    integer                    :: ndpt     ! number of trace samples.
    integer                    :: window_length    ! process parameters.
  end type integrate_struct

  !!---------------------------- interfaces ---------------------------------!!

  !!--------------------------------- data -----------------------------------!!

  type(integrate_struct),pointer,save :: trap_object_pointer 

  contains

  !!----------------------------- create -------------------------------------!!

  subroutine integrate_create (obj)
    type(integrate_struct),pointer :: obj       ! arguments
    integer                        :: ierr      ! for error checking

    allocate (obj, stat=ierr)
    if (ierr /= 0) call pc_error ("Unable to allocate obj in integrate_create")

    call integrate_initialize (obj)
  end subroutine integrate_create

  !!------------------------------- delete -----------------------------------!!

  subroutine integrate_delete (obj)
    type(integrate_struct),pointer :: obj       ! arguments
    integer                   :: ierr      ! for error checking

    call integrate_wrapup (obj)

    deallocate(obj, stat=ierr)
    if (ierr /= 0) call pc_warning ("error deallocating obj in integrate_delete")
  end subroutine integrate_delete

  !!----------------------------- initialize ---------------------------------!!

  subroutine integrate_initialize (obj)
    type(integrate_struct),intent(inout) :: obj       ! arguments


    obj%nwih          = 0  
    obj%ndpt          = 0 
    obj%window_length = 80
    obj%skip_wrapup   = .false.
    obj%ipn           = 999

    call pc_get_global ('nwih', obj%nwih)
    call pc_get_global ('ndpt', obj%ndpt)

    call integrate_update (obj)
  end subroutine integrate_initialize


  !!------------------------- start of update --------------------------------!!


  subroutine integrate_update (obj)
    type(integrate_struct),intent(inout),target :: obj 

    trap_object_pointer => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.

    !!------------------------- read parameters --------------------------------!!

    obj%ipn = pc_get_ipn()
    call pc_get_global ('nwih'    , obj%nwih)
    call pc_get_global ('ndpt'    , obj%ndpt)

    call pc_get  ('window_length'    , obj%window_length,integrate_window_length_trap)

    !!----------------------- write parameters ---------------------------------!!

    !call pc_put_global  ('nwih'    , obj%nwih)
    !call pc_put ('test','This is a test parameter')
    call pc_put ('window_length',obj%window_length)

    call pc_put_control ('parallel_safe', .true.)

    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.     ! to run wrapup code after processing.
    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    !!------------------------- finish update ----------------------------------!!

  end subroutine integrate_update

  !!------------------------------- traps ------------------------------------!!

  subroutine integrate_window_length_trap (keyword)              ! scalar trap.
    character(len=*),intent(in) :: keyword           ! arguments
    if (trap_object_pointer%window_length < 1 .or. &
        trap_object_pointer%window_length > trap_object_pointer%ndpt) then
      call pc_error      ('window_length must be greater than 1 to remove drift.  '//&
                          'It must also be less than the number of samples per trace.')
      trap_object_pointer%window_length = min(trap_object_pointer%ndpt/8 + 1,  &
                                              trap_object_pointer%ndpt)
      trap_object_pointer%window_length = min(80,trap_object_pointer%window_length)
      call pc_jump_field (keyword)
    end if
  end subroutine integrate_window_length_trap

  !!--------------------------- main execution -------------------------------!!

  subroutine integrate (obj,ntr,hd,tr)
    type(integrate_struct),intent(inout) :: obj                    ! arguments
    integer          ,intent(inout) :: ntr                    ! arguments
    double precision ,intent(inout) :: hd(:,:)                ! arguments
    real             ,intent(inout) :: tr(:,:)                ! arguments
    !----------- local variables
    integer                         :: i
    real                            :: tr_smooth(size(tr(:,1)))
    integer                         :: ifirst, ilast,usable_length,winlen

    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      call integrate_wrapup (obj)
      return
    end if

    do i = 1, ntr 
      ifirst = integrate_FirstLiveSample(tr(:,i))
      ilast  =  integrate_LastLiveSample(tr(:,i))
      usable_length = ilast - ifirst + 1
      winlen = min(usable_length/8+1,obj%window_length)
      tr_smooth(:ifirst) = 0.0
      tr_smooth(ilast:)  = 0.0
      call integrate_RunningSum(tr(ifirst:ilast,i),tr(ifirst:ilast,i))
      call integrate_SmoothMeanBased(tr(ifirst:ilast,i),tr_smooth(ifirst:ilast),winlen)
      tr(:,i) = tr(:,i) - tr_smooth(:)
      if(obj%nwih >= HDR_TOP_MUTE)    hd(HDR_TOP_MUTE,i)    = integrate_FirstLiveSample(tr(:,i))
      if(obj%nwih >= HDR_BOTTOM_MUTE) hd(HDR_BOTTOM_MUTE,i) =  integrate_LastLiveSample(tr(:,i))
    end do
    call lav_set_hdr(hd,tr,obj%ndpt,ntr)
  end subroutine integrate

  function integrate_FirstLiveSample(tr) result (index_first_live)
    real, intent(in)  :: tr(:)
    integer           :: index_first_live
    do index_first_live = 1, size(tr)
     if(tr(index_first_live) /= 0.0 ) return
    end do
    index_first_live = size(tr)
  end function integrate_FirstLiveSample


  function integrate_LastLiveSample(tr) result (index_last_live)
    real, intent(in)  :: tr(:)
    integer           :: index_last_live
    do index_last_live = size(tr),1,-1
     if(tr(index_last_live) /= 0.0 ) return
    end do
    index_last_live = 1
  end function integrate_LastLiveSample

  subroutine integrate_RunningSum(tr_in,tr_out)
    real, intent(in)       ::  tr_in(:)
    real, intent(out)      :: tr_out(:)

    integer                :: i

    tr_out(1) = tr_in(1)
    do i = 2, size(tr_in)
      tr_out(i) = tr_out(i-1) + tr_in(i) 
    end do

  end subroutine integrate_RunningSum

  subroutine integrate_SmoothMeanBased(tr_in,tr_out,window_length)
    real, intent(in)       ::  tr_in(:)
    real, intent(out)      :: tr_out(:)
    integer,intent(in)     :: window_length

    integer                :: i,imin,imax

    tr_out(:window_length/2) = tr_in(:window_length/2) 
    do i = window_length/2+1,size(tr_in)-window_length/2
      imin =  i - window_length/2
      imax =  i + window_length/2
      tr_out(i) = SUM(tr_in(imin:imax))/count(tr_in(imin:imax).ne.0.0)
    end do
    tr_out(i:) = tr_in(i:)

 end subroutine integrate_SmoothMeanBased




  !!------------------------------- wrapup -----------------------------------!!


  subroutine integrate_wrapup (obj)
    type(integrate_struct),intent(inout) :: obj       ! arguments

    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.

  end subroutine integrate_wrapup

  !!----------------------------- end of module ------------------------------!!
end module integrate_module
