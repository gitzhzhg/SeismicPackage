!<CPS_v1 type="PROCESS"/>
!!------------------------------- res.f90 ---------------------------------!!
!!------------------------------- res.f90 ---------------------------------!!
!!------------------------------- res.f90 ---------------------------------!!

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
!                         C P S   P R O C E S S
!
! Name       : RES    (RESample)
! Category   : transforms
! Written    : 1986-07-01   by: John Sinton
! Revised    : 2000-12-07   by: Bob Baumel
! Maturity   : production   2001-04-30
! Purpose    : Resample seismic data by decimating or interpolating the trace
!              samples.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! RES resamples seismic trace data by decimating or interpolating the trace
! samples.
!
! If interpolation is done, the available methods are FFT, Bracewell (a 5 point
! operator) and linear interpolation.
!
! If decimation is done, the available methods are FFT, and simple decimation
! (keep every FCTR_RES sample).  An anti-alias filter is used ONLY if the FFT
! method is used in decimation.
!
! The resampling factor (FCTR_RES) may be any integer from 2 - 8 (except 7).
!
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Linear interpolation is the fastest, then Bracewell, and then FFT.  FFT
! interpolation is the most accurate, then Bracewell, and then linear.
! Inaccuracy results in attenuated high frequencies.
!
! If decimation is done by FFT, an anti-aliasing filter is applied.  If
! the DEC method is used (simple decimation), there is NO ANTI-ALIAS FILTER.
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
! No special requirements.
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs traces with same gather status as the input traces.
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      changed
! DT       trace sample interval                 changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 2       Head mute                  Reset
! 25      Largest Absolute Value     Reset
! 64      Tail mute                  Reset
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 31. 2001-04-30 Baumel     Change wrapped_up flag to skip_wrapup.
! 30. 2000-11-15 Baumel     Add Help for display-only screen parameters.
! 29. 2000-04-10 Baumel     Add gui_def to doc; also change named constant
!                           MUTEHW_NONE to preferred version MUTEHW_SET in
!                           MUTEHW calls.
! 28. 2000-02-24 Baumel     Standard handling of wrapped_up flag, even
!                           though wrapup doesn't do anything currently.
! 27. 2000-01-28 Baumel     New conventions for LAV and mute headers.
! 26. 2000-01-17 Baumel     Changed to longer versions of names: DT_IN,
!                           DT_OUT, NDPT_IN, NDPT_OUT.
! 25. 2000-01-14 Baumel     Fixed numerous errors in conversion.
! 24. 1999-12-29 Sharp      Made prolog the 1st line in the file, put all
!                           comment characters in the 1st column,
!                           clarified comments in programming notes.
! 23. 1999-10-08 Sharp      Convert old code to fortran90 module
! 22. 1998-11-03 Goodger    Begin using fortran90 compiler.
! 21. 1998-08-27 Vunderink  Modified to use mixed radix FFTs and removed
!                           power of 2 restriction for FFT interpolation.
! 20. 1995-01-31 Troutt     Fix two minor problems with FFT interpolation:
!                           a)   Change scale factor for FFT interpolation
!                           so that output amplitudes are comparable to
!                           those input. SCALE = 1/(npo2*2) instead of
!                           SCALE = 1/(npo2*ires).
!                           b)   Change indexing and count for CLEAR prior
!                           to inverse FFT so that input's Nyquist is not
!                           destroyed and adjust its amplitude in freq. by
!                           one half before reverse transform.
! 19. 1992-06-16 Troutt     Modify ANTI-ALIAS filter to not remove DC.
!                           Also change scale factor for FFT decimation
!                           so that output amplitudes are comparable to
!                           those input. SCALE = 1/(npo2*2) instead of
!                           SCALE = 1/(npo2*ires).
! 18. 1992-02-18 Troutt     Add handling for tail mute header word 64
!                           (call MUTEHW).
! 17. 1991-10-25 Troutt     Clean up documentation and parameter checks
!                           regarding legal values for IRES, especially
!                           noting that MODE=DEC with IRES not a power
!                           of 2 uses NO ANTI-ALIAS filter.
! 16. 1989-10-04 Hanson     Allow IRES to be any integer<8 if MODE=DECIM.
! 15. 1989-05-26 Adams      Add DTI,DTO to both history and online.
! 14. 1989-05-10 Sinton     Fixed anti-aliasing filter.
! 13. 1989-04-26 Adams      Change  loop 20, I1=IZHFN to I1=.9*IZFN
!                           &  FACTOR = 1.-(I-IZHFN)*RIZHFN
!                           to FACTOR = (I-I1)/(I2-I1)
! 12. 1989-04-21 Adams      Change SCALE for INT,FFT. From 2./ to 1./
!                           Add    SCALE to  DEC,FFT.
! 11. 1989-04-20 Adams      Changed NDPTO Calculation.
! 10. 1988-12-09 Sinton     Changed size of TRACE to NPO2*IRES+2.
! 9.  1988-10-28 Sinton     Fixed bug for MODE=INT, TYPE=FFT.
! 8.  1988-10-26 Sinton     Removed reference to NWPT in RESS!
! 7.  1988-09-26 Binkley    Conversion made for varying number of header
!                           words. Changes for number of words per trace
!                           (nwpt) and number of words in header (nwih)
!                           where appropriate.
! 6.  1988-08-31 Sinton     Added TYPE parameter (see parameters below).
! 5.  1988-08-17 Sinton     Changed to CPSPRT, added NCODE, and changed
!                           RCFFT2-CRFFT2 calls.
! 4.  1988-03-09 Sinton     Changed so that header word 2, mute time, is
!                           correctly handled.
! 3.  1986-12-04 Day        Several parameters saved to avoid problems
!                           with the BTREG compiler option in CONLIB.
!                           Corrected transposition of ndpto-->npdto.
! 2.  1986-07-14 Sinton     A call to DCODE with no "actual arguments"
!                           was added.
! 1.  1986-07-01 Sinton     Original Version
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!<gui_def>
!<NS RES Process/NC=80>
!
!   RES resamples seismic data by decimating or interpolating trace samples
!
!          MODE=~~~~`CCC       METHOD_DEC=`CCC    METHOD_INT=`CCC
!
!          FCTR_RES=`C
!
!          `-------------------------------------------
!            DT_IN=`XXXXXXXXXXX    DT_OUT=`XXXXXXXXXXX
!          `-------------------------------------------
!</gui_def>

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Determines whether to decimate or interpolate trace samples. </Tip>
!Default = DEC
!Allowed = DEC  (DECimate trace samples)
!Allowed = INT  (INTerpolate trace samples)
!
!If MODE = DEC, trace samples will be decimated by a factor of FCTR_RES.
!Decimating trace samples reduces their number and increases the sample
!interval.
!
!IF MODE = INT, trace samples will be interpolated by a factor of FCTR_RES.
!Interpolating trace samples increases their number and decreases the sample
!interval.
!</Help>
!
!<Help KEYWORD="METHOD_DEC">
!<Tip> Method used to perform decimation. </Tip>
!Default = FFT
!Allowed = FFT  (FFT Method)
!Allowed = DEC  (Simple decimation)
!
!DEC is simple decimation and is less accurate than FFT. The FFT method
!applies an anti-alias filter; the DEC method does not.
!</Help>
!
!<Help KEYWORD="METHOD_INT">
!<Tip> Method used to perform interpolation. </Tip>
!Default = BRA
!Allowed = BRA  (Bracewell method - five point operator)
!Allowed = FFT  (FFT Method)
!Allowed = LIN  (Linear Interpolation Method)
!
!FFT is most accurate, then Bracewell, then linear. (Inaccuracy
!results in attenuated high frequencies.)
!</Help>
!
!<Help KEYWORD="FCTR_RES">
!<Tip> Resampling factor for decimation or interpolation of trace samples.</Tip>
!Default = 2
!Allowed = 2,3,4,5,6,8 (integer)
!
!If MODE = DEC, then the number of output trace values will equal
!   NDPT_OUT = (NDPT_IN - 1) / FCTR_RES  +  1
!where NDPT_IN = the number of input trace samples.
!
!IF MODE = INT, then the number of output trace values will equal
!   NDPT_OUT = (NDPT_IN - 1) * FCTR_RES  +  1
!where NDPT_IN = the number of input trace samples.
!</Help>
!
!<Help TYPE="DISPLAY_ONLY" KEYWORD="DT_IN">
!<Tip> Sample rate (DT global) before resampling. </Tip>
!</Help>
!
!<Help TYPE="DISPLAY_ONLY" KEYWORD="DT_OUT">
!<Tip> Sample rate (DT global) after resampling. </Tip>
!</Help>
!
!</HelpSection>
!-----------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module res_module

      use pc_module
      use named_constants_module
      use fft_module
      use interp_module
      use mutehw_module
      use string_module
      use lav_module

      implicit none
      private
      public :: res_create
      public :: res_initialize
      public :: res_update
      public :: res_delete

!<execute_only>
      public :: res       ! main execution (trace processing) routine.
      public :: res_wrapup
!</execute_only>

      character(len=100),public,save :: res_ident = &
'$Id: res.f90,v 1.31 2001/04/26 17:33:38 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: res_struct

         private

         logical          :: skip_wrapup          ! wrapup flag

         integer          :: nwih                 ! globals
         integer          :: ndpt_in, ndpt_out    ! globals
         real             :: dt_in, dt_out        ! globals

         character(len=3) :: mode                 ! process parameters
         character(len=3) :: method_int           ! process parameters
         character(len=3) :: method_dec           ! process parameters
         integer          :: fctr_res             ! process parameters

         integer          :: nfft1, nfft2         ! dependent variables
         integer          :: nyq1, nyq2           ! dependent variables
         real   ,pointer  :: trtmp(:)             ! dependent variables
         real   ,pointer  :: ft(:)                ! dependent variables
         real   ,pointer  :: gt(:)                ! dependent variables
         real   ,pointer  :: filtr(:)             ! dependent variables
         complex,pointer  :: complex_arr(:)       ! dependent variables

         type(fft_struct),pointer :: CtoR_ptr     ! fft pointers
         type(fft_struct),pointer :: RtoC_ptr     ! fft pointers

      end type res_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(res_struct),pointer,save :: object      ! needed for traps.

      integer, parameter  :: mode_noptions = 2
      character(len=3)    :: mode_options(mode_noptions)              &
                             = (/ 'DEC', 'INT' /)

      integer, parameter  :: method_int_noptions = 3
      character(len=3)    :: method_int_options(method_int_noptions)  &
                             = (/ 'BRA', 'FFT', 'LIN' /)

      integer, parameter  :: method_dec_noptions = 2
      character(len=3)    :: method_dec_options(method_dec_noptions)  &
                             = (/ 'FFT', 'DEC' /)

      integer, parameter  :: fctr_res_noptions = 6
      integer             :: fctr_res_options(fctr_res_noptions)      &
                             = (/ 2, 3, 4, 5, 6, 8 /)

      contains
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine res_create (obj)
      implicit none
      type(res_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify  (obj%trtmp)
      nullify  (obj%ft)
      nullify  (obj%gt)
      nullify  (obj%filtr)
      nullify  (obj%complex_arr)
      nullify  (obj%CtoR_ptr)
      nullify  (obj%RtoC_ptr)

      call res_initialize (obj)

      return
      end subroutine res_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

    subroutine res_delete (obj)
    implicit none
    type(res_struct),pointer :: obj

!<execute_only>
    call res_wrapup (obj)
!</execute_only>

   if (associated(obj%trtmp))       deallocate (obj%trtmp)
   if (associated(obj%ft))          deallocate (obj%ft)
   if (associated(obj%gt))          deallocate (obj%gt)
   if (associated(obj%filtr))       deallocate (obj%filtr)
   if (associated(obj%complex_arr)) deallocate (obj%complex_arr)
   if (associated(obj%CtoR_ptr))    call fft_delete(obj%CtoR_ptr)
   if (associated(obj%RtoC_ptr))    call fft_delete(obj%RtoC_ptr)

   deallocate(obj)

   return
   end subroutine res_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine res_initialize (obj)
      implicit none
      type(res_struct),intent(inout) :: obj

         obj%nwih        = inil
         obj%ndpt_in     = inil
         obj%ndpt_out    = inil
         obj%dt_in       = fnil
         obj%dt_out      = fnil

         obj%mode        = 'DEC'
         obj%method_int  = 'BRA'
         obj%method_dec  = 'FFT'
         obj%fctr_res    =  2

         obj%nfft1       =  2
         obj%nfft2       =  2
         obj%nyq1        =  2
         obj%nyq2        =  2

         call res_update (obj)

      return
      end subroutine res_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

  subroutine res_update (obj)
  implicit none
  type(res_struct),intent(inout),target  ::  obj

  integer          :: nscra, nstore, j, i1, i2, ierr1, fft_err
  real             :: f1
  character(len=3) :: mode_prev, method_dec_prev

  object => obj
  obj%skip_wrapup = .true.     ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

  call pc_get_global ('NWIH' , obj%nwih)     ! number of header words.
  call pc_get_global ('NDPT' , obj%ndpt_in)  ! number of trace samples.
  call pc_get_global ('DT'   , obj%dt_in)    ! trace sample interval (sec).

  if (obj%nwih    == inil) call pc_error ("NWIH global hasn't been set.")
  if (obj%ndpt_in == inil) call pc_error ("NDPT global hasn't been set.")
  if (obj%dt_in   == fnil) call pc_error ("DT global hasn't been set.")

  mode_prev       = obj%mode
  method_dec_prev = obj%method_dec
  call pc_get ('MODE'      , obj%mode)
  call pc_get ('METHOD_INT', obj%method_int)
  call pc_get ('METHOD_DEC', obj%method_dec)
  call pc_get ('FCTR_RES'  , obj%fctr_res)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

  call string_to_upper(obj%mode)
  if      (obj%mode(1:1)=='I') then
    obj%mode='INT'
    call pc_put_sensitive_field_flag ('METHOD_INT', .true. )
    call pc_put_sensitive_field_flag ('METHOD_DEC', .false.)
  else if (obj%mode(1:1)=='D') then
    obj%mode='DEC'
    call pc_put_sensitive_field_flag ('METHOD_INT', .false.)
    call pc_put_sensitive_field_flag ('METHOD_DEC', .true. )
  else
    call pc_error ('MODE must be DEC or INT.')
    call pc_put_sensitive_field_flag ('METHOD_INT', .false.)
    call pc_put_sensitive_field_flag ('METHOD_DEC', .false.)
    call pc_jump_field ('MODE')
  end if

  call string_to_upper(obj%method_int)
  if      (obj%method_int(1:1)=='B') then
    obj%method_int ='BRA'
  else if (obj%method_int(1:1)=='F') then
    obj%method_int ='FFT'
  else if (obj%method_int(1:1)=='L') then
    obj%method_int ='LIN'
  else
    if (obj%mode=='INT') then
      call pc_error ('METHOD_INT must be BRA or FFT or LIN.')
      call pc_jump_field ('METHOD_INT')
    else
      obj%method_int = 'BRA'
    end if
  end if

  call string_to_upper(obj%method_dec)
  if      (obj%method_dec(1:1)=='F') then
    obj%method_dec ='FFT'
  else if (obj%method_dec(1:1)=='D') then
    obj%method_dec ='DEC'
    if (obj%mode == 'DEC') then
      if (method_dec_prev/='DEC' .or. mode_prev/='DEC') then
        call pc_warning('Warning - decimation WITHOUT anti-alias filter.')
      end if
    end if
  else
    if (obj%mode == 'DEC') then
      call pc_error ('METHOD_DEC must be FFT or DEC.')
      call pc_jump_field ('METHOD_DEC')
    else
      obj%method_dec = 'FFT'
    end if
  end if

  if (obj%fctr_res<2 .or. obj%fctr_res==7 .or. obj%fctr_res>8) then
    call pc_error ('FCTR_RES must be 2, 3, 4, 5, 6, or 8.')
    call pc_jump_field ('FCTR_RES')
  end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

!---Write option lists for combo boxes:
  call pc_put_options_field('MODE', mode_options, mode_noptions)
  call pc_put_options_field('METHOD_INT', method_int_options, &
                                          method_int_noptions)
  call pc_put_options_field('METHOD_DEC', method_dec_options, &
                                          method_dec_noptions)
  call pc_put_options_field('FCTR_RES', fctr_res_options, fctr_res_noptions)

!---Write process parameters to parameter cache:
  call pc_put  ('MODE'      , obj%mode)
  call pc_put  ('METHOD_INT', obj%method_int)
  call pc_put  ('METHOD_DEC', obj%method_dec)
  call pc_put  ('FCTR_RES'  , obj%fctr_res)

!---Determine output NDPT and DT:
  if      (obj%mode == 'INT') then
    obj%ndpt_out = (obj%ndpt_in - 1) * obj%fctr_res  +  1
    obj%dt_out   =  obj%dt_in / obj%fctr_res
  else if (obj%mode == 'DEC') then
    obj%ndpt_out = (obj%ndpt_in - 1) / obj%fctr_res  +  1
    obj%dt_out   =  obj%dt_in * obj%fctr_res
  end if

!---Print NDPT & DT values and put in parameter cache so appear in history:
  call pc_print ('NDPT_IN =',obj%ndpt_in,'   NDPT_OUT =',obj%ndpt_out)
  call pc_put ('NDPT_IN' , obj%ndpt_in)
  call pc_put ('NDPT_OUT', obj%ndpt_out)
  call pc_print ('DT_IN ='  ,obj%dt_in,  '   DT_OUT ='  ,obj%dt_out)
  call pc_put ('DT_IN'   , obj%dt_in)
  call pc_put ('DT_OUT'  , obj%dt_out)

!---Write altered globals to parameter cache:
  call pc_put_global  ('NDPT'        , obj%ndpt_out)
  call pc_put_global  ('DT'          , obj%dt_out)

!---Determine memory usage and write to parameter cache:
  if (obj%mode == 'INT') then
    obj%nfft1 = fft_nfctr (obj%ndpt_in)
    obj%nfft2 = obj%nfft1 * obj%fctr_res
  else
    obj%nfft2 = fft_nfctr (obj%ndpt_out)
    obj%nfft1 = obj%nfft2 * obj%fctr_res
  endif
  obj%nyq1 = obj%nfft1/2 + 1
  obj%nyq2 = obj%nfft2/2 + 1
  nscra = 0
  if (obj%mode == 'INT') then
     if (obj%method_int == 'FFT') then
         nstore = obj%nfft2  +  2*obj%nyq2                &
                  + fft_mem_usage (obj%nfft1, 'rtoc')     &
                  + fft_mem_usage (obj%nfft2, 'ctor')
     else if (obj%method_int == 'BRA') then
         nstore = (obj%ndpt_in + 5) * obj%fctr_res
     else
         nstore = (obj%ndpt_in + 2) * obj%fctr_res
     end if
  else if (obj%mode == 'DEC') then
     if (obj%method_dec == 'FFT') then
         nstore = obj%nfft1  +  2*obj%nyq1                &
                  + fft_mem_usage (obj%nfft1, 'rtoc')     &
                  + fft_mem_usage (obj%nfft2, 'ctor')
     else
         nstore = 0
     end if
  end if
  call pc_put_control ('NSCRATCH', nscra )
  call pc_put_control ('NSTORE'  , nstore)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

  if (associated(obj%trtmp))       deallocate (obj%trtmp)
  if (associated(obj%ft))          deallocate (obj%ft)
  if (associated(obj%gt))          deallocate (obj%gt)
  if (associated(obj%filtr))       deallocate (obj%filtr)
  if (associated(obj%complex_arr)) deallocate (obj%complex_arr)
  if (associated(obj%CtoR_ptr))    call fft_delete (obj%CtoR_ptr)
  if (associated(obj%RtoC_ptr))    call fft_delete (obj%RtoC_ptr)

!<execute_only>
  if (pc_do_not_process_traces()) return
  obj%skip_wrapup = .false.     ! needed for the wrapup routine.

    if (obj%mode == 'DEC') then
      if (obj%method_dec == 'FFT') then
        call pc_info ('** Decimation in RES will be done using FFT **')
        call pc_info ('** An anti-aliasing filter will be applied. **')
      else
       call pc_warning('**** WARNING ** WARNING ** WARNING ** WARNING ****')
       call pc_warning('**************************************************')
       call pc_warning('**************************************************')
       call pc_warning('** DECIMATION BY KEEPING EVERY FCTR_RESth VALUE **')
       call pc_warning('****                                          ****')
       call pc_warning('****      NO ANTI-ALIASING FILTER APPLIED     ****')
       call pc_warning('**************************************************')
       call pc_warning('**************************************************')
       call pc_warning('**************************************************')
       call pc_warning('**** WARNING ** WARNING ** WARNING ** WARNING ****')
      end if
    end if

!**************************************

    if (obj%mode == 'INT') then

        if (obj%method_int == 'FFT') then

          allocate (obj%trtmp(obj%nfft2), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating TRTMP array.')

          allocate (obj%complex_arr(obj%nyq2), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating COMPLEX_ARR array.')

        else if (obj%method_int == 'BRA') then

          allocate (obj%trtmp(obj%ndpt_in * obj%fctr_res), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating TRTMP array.')

          allocate (obj%ft(obj%fctr_res * 5), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating FT array.')

        else

          allocate (obj%trtmp(obj%ndpt_in * obj%fctr_res), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating TRTMP array.')

          allocate (obj%ft(obj%fctr_res), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating FT array.')

          allocate (obj%gt(obj%fctr_res), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating GT array.')

        end if

    else if (obj%mode == 'DEC') then

        if (obj%method_dec == 'FFT') then

          allocate (obj%trtmp(obj%nfft1), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating TRTMP array.')

          allocate (obj%complex_arr(obj%nyq1), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating COMPLEX_ARR array.')

          allocate (obj%filtr(obj%nyq2), stat=ierr1)
          if(ierr1/=0) call pc_error('Error allocating FILTR array.')

        end if

    end if


!************************************************

    if (obj%mode == 'INT') then

       if (obj%method_int == 'FFT') then

          fft_err = fft_create (obj%RtoC_ptr, -1, obj%nfft1, 'rtoc')
          if(fft_err/=0) call pc_error('Error creating real to complex FFT.')

          fft_err = fft_create (obj%CtoR_ptr,  1, obj%nfft2, 'ctor')
          if(fft_err/=0) call pc_error('Error creating complex to real FFT.')

          call pc_print('NFFT1 =', obj%nfft1, '   NFFT2 =', obj%nfft2)

       else if (obj%method_int == 'BRA') then

          call interp_1d_con_bw_real (obj%fctr_res, obj%ndpt_in, -1, obj%ft)

       else

          call interp_1d_con_lin_real  &
                (obj%fctr_res, obj%ndpt_in, -1, obj%ft, obj%gt)

       end if

    else if (obj%mode == 'DEC') then

       if (obj%method_dec == 'FFT') then

          fft_err = fft_create (obj%RtoC_ptr, -1, obj%nfft1, 'rtoc')
          if(fft_err/=0) call pc_error('Error creating real to complex FFT.')

          fft_err = fft_create (obj%CtoR_ptr,  1, obj%nfft2, 'ctor')
          if(fft_err/=0) call pc_error('Error creating complex to real FFT.')

          call pc_print('NFFT1 =', obj%nfft1, '   NFFT2 =', obj%nfft2)

          i1 = 0.9 * obj%nyq2
          i2 = obj%nyq2 - 2
          obj%filtr(1:i1) = 1.0
          if (i1 < i2) then
            f1 = 1.0 / (i2-i1)
            do j = i1+1, i2
              obj%filtr(j) = f1 * (i2-j)
            end do
          end if
          obj%filtr(i2+1:i2+2) = 0.

       end if

    end if
!************************************************
!</execute_only>

     return
  end subroutine res_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!<execute_only>

  subroutine res (obj,ntr,hd,tr)
    implicit none
    type(res_struct),intent(inout)  :: obj              ! arguments
    integer         ,intent(inout)  :: ntr              ! arguments
    double precision,intent(inout)  :: hd(:,:)          ! arguments
    real            ,intent(inout)  :: tr(:,:)          ! arguments

    integer                         :: IT               ! local
    logical                         :: dead             ! local

    if (ntr == NO_MORE_TRACES) then
      call res_wrapup(obj)
      return
    end if

    DO IT = 1, ntr
      call mutehw (hd(:,IT), tr(:,IT), obj%ndpt_in, 0.0, MUTEHW_SET)
      if (hd(HDR_LAV,IT) == 0.) then
        dead = .true.
        tr(:obj%ndpt_out,IT) = 0.
      else
        dead = .false.
      end if
      if (obj%mode == 'INT') then
!***********************************************
!****   Interpolate the trace.              ****
!***********************************************
        hd(HDR_TOP_MUTE,IT) = &
            (nint(hd(HDR_TOP_MUTE,IT))-1) * obj%fctr_res  +  1
        hd(HDR_BOTTOM_MUTE,IT) = &
            (nint(hd(HDR_BOTTOM_MUTE,IT))-1) * obj%fctr_res  +  1
        if (dead) goto 25
        if (obj%method_int == 'FFT') then
          obj%trtmp(:obj%ndpt_in) = tr(:obj%ndpt_in,IT)
          obj%trtmp(obj%ndpt_in+1:obj%nfft1) = 0.
          call fft_rc_transform (obj%RtoC_ptr, obj%trtmp, obj%complex_arr)
          obj%complex_arr(obj%nyq1) = 0.5 * obj%complex_arr(obj%nyq1)
          obj%complex_arr(obj%nyq1+1:obj%nyq2) = (0.,0.)
          call fft_cr_transform (obj%CtoR_ptr, obj%complex_arr, obj%trtmp)
          tr(:obj%ndpt_out,IT) = obj%trtmp(:obj%ndpt_out) / obj%nfft1
        else if (obj%method_int == 'BRA') then
          tr(obj%ndpt_in+1:obj%ndpt_in+2,IT) = 0.
          call interp_1d_con_bw_real (obj%fctr_res, obj%ndpt_in, 1,   &
                       obj%ft, tr(:,IT), obj%trtmp)
          tr(:obj%ndpt_out,IT) = obj%trtmp(:obj%ndpt_out)
        else
          tr(obj%ndpt_in+1,IT) = 0.
          call interp_1d_con_lin_real (obj%fctr_res, obj%ndpt_in, 1,  &
                       obj%ft, obj%gt, tr(:,IT), obj%trtmp)
          tr(:obj%ndpt_out,IT) = obj%trtmp(:obj%ndpt_out)
        end if

      else   ! if (obj%mode == 'DEC') then
!***********************************************
!****   Decimate the trace.                 ****
!***********************************************
        hd(HDR_TOP_MUTE,IT) = &
            (nint(hd(HDR_TOP_MUTE,IT))-1) / obj%fctr_res  +  1
        hd(HDR_BOTTOM_MUTE,IT) = &
            (nint(hd(HDR_BOTTOM_MUTE,IT))-1) / obj%fctr_res  +  1
        if (dead) goto 25
        if (obj%method_dec == 'FFT') then
          obj%trtmp(:obj%ndpt_in) = tr(:obj%ndpt_in,IT)
          obj%trtmp(obj%ndpt_in+1:obj%nfft1) = 0.
          call fft_rc_transform (obj%RtoC_ptr, obj%trtmp, obj%complex_arr)
          obj%complex_arr(:obj%nyq2)  &
               = obj%complex_arr(:obj%nyq2) * obj%filtr
          call fft_cr_transform (obj%CtoR_ptr, obj%complex_arr, obj%trtmp)
          tr(:obj%ndpt_out,IT) = obj%trtmp(:obj%ndpt_out) / obj%nfft1
        else       ! simple decimation
          tr(1:obj%ndpt_out,IT) = tr(1:obj%ndpt_in:obj%fctr_res,IT)
        end if
      end if
 25   call mutehw (hd(:,IT), tr(:,IT), obj%ndpt_out, 0.0, MUTEHW_SET)
    end do

    call lav_set_hdr (hd, tr, obj%ndpt_out, ntr)

    return
  end subroutine res
!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>
  subroutine res_wrapup(obj)
    implicit none
    type(res_struct),intent(inout) :: obj     ! arguments

    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.

    return
  end subroutine res_wrapup
!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module res_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
