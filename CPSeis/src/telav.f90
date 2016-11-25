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
! Name       : TELAV  (Trace Edit by Largest Absolute Value) [Previously ASFM]
! Category   : Amplitude_mod
! Written    : 1990-04-12   by: Mike Howard (median version)
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : Automatic trace editing based on trace largest absolute value.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process kills traces with LAV > FCTR_KIL * MEDIAN(LAV).  (LAV stands
! for Largest Absolute Value.) TELAV stores NTUME input traces on disk,
! determines their median LAV, then processes the traces stored followed by
! the rest of the dataset. The median LAV is reported for each NTUME traces
! that pass through so users can assess whether NTUME was large enough.
!
! Since the median is not sensitive to anomalous LAVs, NTUME can be a smaller 
! fraction of the traces and FCTR_KIL can be a smaller value than would be 
! required if using the average LAV.  TELAV prints information on the 
! median(LAV)s calculated so that you can judge whether NTUME was large
! enough to yield a stable median LAV.
!
! Normally TELAV uses the LAV values that are ALREADY PRESENT in HDR_LAV.
! (It is the responsibility of every process that may change a trace's LAV
! to recalculate it and reset HDR_LAV.)  When LAV_TIM_BEG is non-zero, a
! new LAV is calculated using the part of each input trace from LAV_TIM_BEG
! to the end of trace and stored in HDR_SCRATCH_30.  (Note that LAV_TIM_BEG
! is measured from the mute, in seconds.)
!
! When LAV_TIM_BEG is non-zero, the restricted-time LAV is used both for the
! median LAV calculation and for determining whether to kill an individual
! trace.
!
! When TELAV receives GATHERed data, NTUME is automatically adjusted upward
! to include the entire gather with the NTUMEth trace. This only happens
! before the median LAV has been calculated for the first time. After this,
! NTUME is reset to the value supplied by the user. The result is that TELAV
! might produce a slightly different answer for the same data that is gathered
! in different ways.
!
! For the final median LAV calculation, TELAV may use less than NTUME traces
! if fewer than NTUME traces are available.
!
! Historical Note:
! TELAV was previously known as ASFM.  ASFM was functionally equivalent to
! TELAV after 1990.  Prior to 1990, ASFM used an arithmetic mean (average)
! calculation rather than the median currently used.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                       ADVICE FOR USERS
!
! Testing for appropriate values of FCTR_KIL can be conveniently done by
! running several TELAV processes in the same job, with the FCTR_KIL values
! DECREASING from the first to the last process.  Number of traces killed for
! each value of FCTR_KIL are displayed in the .rpt file.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                   TRACE OUTPUT CHARACTERISTICS
!
! This process may alter input traces.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                 GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in header             used but not changed
! NDPT     number of data samples per trace      used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                 TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#          Description          Action taken
! ----          -----------          ------------
! 2             top mute             Used as reference time for LAV_TIM_BEG.
! 25            LAV                  Used by default when LAV_BEG_TIM is zero.
!                                    Reset to zero when trace is killed.
! 30 (scratch)  time-restricted LAV  Calculated when LAV_TIM_BEG > zero.
!                                    Reset to zero when trace is killed.
! 31 (scratch)  end of group flag    Set as needed for input parameter NTR.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                        REVISION HISTORY
!
!      Date        Author    Description
!      ----        -------   -----------
!028. 2006-04-25  B. Menger   Removed Unused Variables.
! 27.  2001-03-22  Stoeckley Change to collect at least NTUME live traces
!                             before calculating median.  Previously, although
!                             dead traces were ignored when calculating the
!                             median, if the first NTUME traces were all dead,
!                             the median was set to 0.  Also change allocation
!                             of WORK so it need not be reallocated later.
!                             Also replace TRCIO with TEMPTFILE, move sone
!                             redundant code into subroutines for clarity and
!                             easier maintenance, and add additional info to
!                             summary printout for better diagnosis.
! 26.  2001-02-15  Stoeckley Change wrapup flag.
! 25.  2000-09-05  O'Brien   Replaced native fortran i/o with trcio_module
! 24.  2000-04-04  O'Brien   Implemented EzGUI Layout
! 23.  2000-03-14  O'Brien   Fixed bug in write statement that used wrong
!                             variable for array subscript.
!                             Use lav_module when new LAVs are to be computed.
!                             Push all summary printouts into wrapup routine.
! 22.  2000-03-09  O'Brien   Added a summary print statement to telav_wrapup.
! 21.  2000-02-25  O'Brien   Made READ/WRITE error messages more specific.
!                             Cleaned up wrapup routine.
! 20.  2000-02-23  O'Brien   Updated document and help regarding GATHERed data
! 19.  2000-02-10  O'Brien   Added pc_put calls for control parameters
!                             NEED_REQUEST and NEED_LABEL.
!                             Allow work() to resize dynamically when a group
!                             of NTR traces pushes ncollected past NTUME
! 18.  2000-02-08  O'Brien   Put variable threshold into telav_struct 
! 17.  2000-02-03  O'Brien   General Description updated.
! 16.  2000-01-28  O'Brien   Implemented pc_put_options_field for OPT_PRINT
! 15.  2000-01-10  O'Brien   Fixed file access inconsistency when telav_create
!                             is imediately followed by tela_delete
! 14.  1999-12-29  O'Brien   Brought xml tags up to date
!                             Added RCS character ID variable
! 13.  1999-09-13  O'Brien   Updates for conformance with new pc_module
! 12.  1999-08-23  O'Brien   Changed header array to double precision
! 11.  1999-08-16  O'Brien   Changed state variable use per changes in pc_module
! 10.  1999-08-10  O'Brien   Removed min max args from pc_get calls.
!  9.  1999-08-05  O'Brien   Full f90 conversion.
!  8.  1998-11-24  Goodger   Begin using the fortran90 compiler.            
!  7.  1994-10-26  Troutt    Documentaion change only to clarify that DELAY
!                             is input in terms of seconds.
!  6.  1993-10-07  Troutt    Add new parameter DELAY for Nigeria 3D.  
!                             This allows for calculation of new LAV's in a 
!                             window below the mute and puts them in HW27.
!  5.  1993-09-22  Troutt    Set HW64 for killed traces.
!                             Fix array index for print inside of do 24.
!                             Change HW31 check back to 666 (use NINT).
!                             Add several comments for easier maintenance.
!  4.  1992-06-18  Peterson  Corrections:Specify to not pack the headers and
!                             data in STROTSI. (very large LAVs i.e. 1.9E+38 
!                             were being currupted by PACK21 and EXPAND21)
!                             For reentrancy, increase number of variables to
!                             save and restore to 17. Add MEDIAN calculation
!                             for the last WORK buffer full of LAVs.
!                             Use variable ONE=1 in CALL STRINI to be safe
!                             and for reentrancy (STRINI would change the 
!                             constant 1 to 0 at the end of the data.
!                             Change IF(HD(31,N).NE.666.) to 666.0 + or -.01
!  3.  1991-10-12  Howard    Add profile and receiver # to printout.
!  2.  1990-05-14  Howard    Fix bug if MAXONDSK># of traces in job.
!  1.  1990-04-12  Howard    Original Median Version (former versions used mean)
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                      PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                  SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                 SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == NEED_TRACES    if the process needs another trace before
!                            passing any out
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!               ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                        PROGRAMMING NOTES
!
! MODE starts out as 'COLLECT' and stays there until MAXONDSK traces are
!     read in (or NTR=NO_MORE_TRACES). The last trace in each input "group"
!     is flagged in HDR_SCRATCH_31. In this mode, input traces are written
!     to disk. Once MAXONDSK traces have been received, a median LAV is
!     determined and MODE changes to 'RELEASE'. Until then,
!     NTR=NEED_MORE_TRACES and FROMAB=.TRUE. at each RETURN.
! 
! MODE changes from 'COLLECT' to 'RELEASE' and the job reads traces from
!     disk, killing them as needed. HDR_SCRATCH_31 is used to determine
!     how many traces to pass down at once. In this mode, FROMAB=.FALSE.
! 
! MODE changes finally to 'PASS' when all of the traces written to disk
!     during 'COLLECT' have been passed down during 'RELEASE'. In this
!     mode, traces just pass directly through TELAV, and the median LAV is
!     periodically recomputed and reported.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TELAV Process/NC=80>
!
!         Automatic trace editing based on largest absolute value (LAV).
!
!             FCTR_KIL=`FFFFFFFFFFF           NTUME=`IIIIIIIIII
!
!            OPT_PRINT=`CC              LAV_TIM_BEG=`FFFFFFF    
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="FCTR_KIL">
!<Tip> Kill traces whose LAVs exceed FCTR_KIL times median(LAV). </Tip>
! Default = 10.0
! Allowed = real >1.0
! 
! This process kills traces with LAV > FCTR_KIL * MEDIAN(LAV). 
!</Help>
!
!
!<Help KEYWORD="NTUME">
!<Tip> Number of Traces to Use in the Median Estimate. </Tip>
! Default = 10000
! Allowed = int >3
! NTUME is the maximum number of traces to store on disk at once and use to
! estimate the median(LAV) for that block of traces. 
!
! For GATHERed data, NTUME is automatically adjusted upward to include the
! entire gather with the NTUMEth trace. This only happens before the median
! LAV has been calculated for the first time. After this, NTUME is reset to
! the value supplied by the user. The result is that TELAV might produce a
! slightly different answer for the same data that is gathered in different
! ways.
!</Help>
!
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Option whether to print detail on each trace killed. </Tip>
! Default = NO
! Allowed = YES/NO
! If OPT_PRINT = NO then mimimum detail is printed.  If OPT_PRINT = YES then 
! detail on each trace killed is printed.
!</Help>
!
!
!<Help KEYWORD="LAV_TIM_BEG">
!<Tip> Starting time for the restricted-time LAV calculation. </Tip>
! Default = 0.0
! Allowed = real >=0.0
! If LAV_TIM_BEG = 0.0 then the value in HDR_LAV is used by TELAV.
! If LAV_TIM_BEG > 0.0 then a new LAV is calculated for each trace for the
! time range from HDR_TOP_MUTE + LAV_TIM_BEG to end of trace.
!
! LAV_TIM_BEG is measured in seconds from the mute time.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module telav_module

      use pc_module
      use named_constants_module
      use median_module
      use getlun_module
      use string_module
      use lav_module
      use mem_module
      use temptfile_module
      use sizeof_module

      implicit none

      private
      public :: telav_create
      public :: telav_initialize
      public :: telav_update
      public :: telav_delete
!<execute_only>
      public :: telav      
      public :: telav_wrapup
!</execute_only>

      character(len=100),public,save :: TELAV_IDENT = &
'$Id: telav.f90,v 1.28 2006/04/25 13:24:20 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: telav_struct              
        private
        logical            :: skip_wrapup             ! wrapup flag.

        integer            :: ntume                   ! process parameters.
        real               :: fctr_kil                ! process parameters.
        real               :: lav_tim_beg             ! process parameters.
        logical            :: opt_print               ! process parameters.

        integer            :: nwih,ndpt               ! globals.  
        real               :: dt                      ! globals.  

        type(temptfile_struct),pointer :: temptfile   ! dependent variables
        integer            :: prt_lu                  ! dependent variables.
        integer            :: median_lu               ! dependent variables
        integer            :: trkill_lu               ! dependent variables
        integer            :: nadd,ihw                ! dependent variables.
        character(len=8)   :: mode                    ! dependent variables.
        logical            :: more,fromab             ! dependent variables.
        integer            :: ncollected              ! dependent variables.
        integer            :: nreleased               ! dependent variables.
        integer            :: ndead,nlive,nkill       ! dependent variables.
        integer            :: pdead,plive,pkill       ! dependent variables.
        real               :: amed                    ! dependent variables.
        real               :: amedmin,amedmax         ! dependent variables.
        real               :: threshold               ! dependent variables.
        real,pointer       :: work(:)                 ! dependent variables.
      end type telav_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(telav_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine telav_create (obj)
      implicit none
      type(telav_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify  (obj%work)
      nullify  (obj%temptfile) 

      call telav_initialize (obj)
      return
      end subroutine telav_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine telav_delete (obj)
      implicit none
      type(telav_struct),pointer :: obj       ! arguments

!<execute_only>
      call telav_wrapup(obj)
!</execute_only>

      call mem_free        (obj%work)
      call temptfile_close (obj%temptfile)
      deallocate(obj)

      return
      end subroutine telav_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine telav_initialize (obj)
      implicit none
      type(telav_struct),intent(inout) :: obj       ! arguments

      obj%ntume       = 10000
      obj%fctr_kil    = 10.0
      obj%lav_tim_beg = 0.0
      obj%opt_print   = .false.

      call telav_update (obj)
      return
      end subroutine telav_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine telav_update (obj)
      implicit none
      type(telav_struct),intent(inout),target :: obj              ! arguments
      integer                                 :: ier,numtr        ! local
      integer                                 :: nstore,ndisk     ! local

      object => obj                             ! needed for traps.
      obj%skip_wrapup = .true.


!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!


      obj%prt_lu = pc_get_lun()

      call pc_get_global ('NWIH' ,obj%nwih )
      call pc_get_global ('NDPT' ,obj%ndpt )
      call pc_get_global ('DT'   ,obj%dt   )
      call pc_get_global ('numtr',numtr    )

      call pc_get ('FCTR_KIL',   obj%fctr_kil,    telav_trap)
      call pc_get ('NTUME',      obj%ntume,       telav_trap)
      call pc_get ('OPT_PRINT',  obj%opt_print)
      call pc_get ('LAV_TIM_BEG',obj%lav_tim_beg, telav_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!



!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put  ('FCTR_KIL'   ,obj%fctr_kil   )
      call pc_put  ('NTUME'      ,obj%ntume      )
      call pc_put  ('OPT_PRINT'  ,obj%opt_print  )
      call pc_put  ('LAV_TIM_BEG',obj%lav_tim_beg)

      nstore = (obj%ntume+numtr)
      ndisk  = (obj%ntume+numtr) * obj%nwih * sizeof(0.0d0)  &
             + (obj%ntume+numtr) * obj%ndpt * sizeof(0.0)

      call pc_put_control ('NSTORE'      , nstore)
      call pc_put_control ('NDISK'       , ndisk )
      call pc_put_control ('NEED_REQUEST', .true.)
      call pc_put_control ('NEED_LABEL'  , .true.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call mem_free        (obj%work)
      call temptfile_close (obj%temptfile)

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! Initialize dependent parameters.

      obj%median_lu  =  0
      obj%trkill_lu  =  0
      obj%nadd       =  0
      obj%ihw        =  HDR_LAV
      obj%mode       = 'COLLECT'
      obj%more       = .true.
      obj%fromab     = .true.
      obj%ncollected =  0
      obj%nreleased  =  0
      obj%ndead      =  0        ! total dead input traces.
      obj%nlive      =  0        ! total live input traces.
      obj%nkill      =  0        ! total traces killed.
      obj%pdead      =  0        ! dead input traces within each group.
      obj%plive      =  0        ! live input traces within each group.
      obj%pkill      =  0        ! traces killed within each group.
      obj%amed       =  0.0      ! first median found.
      obj%amedmin    =  0.0      ! minimum median found.
      obj%amedmax    =  0.0      ! maximum median found.
      obj%threshold  = -1.0

! Handle the exception when new TLAV values are to be calculated.

      if (obj%lav_tim_beg > 0.0) then
        obj%ihw   = HDR_SCRATCH_30
        obj%nadd  = nint(obj%lav_tim_beg/obj%dt)
      endif

! Must set up a trace file for temprary trace and header storage.

      call temptfile_open (obj%temptfile, 'telav', obj%nwih, obj%ndpt, &
                           obj%prt_lu, ier)
      if (ier /= TEMPTFILE_OK) then
          call pc_error ("TELAV had trouble opening temporary trace file.")
      end if

! Get an i/o unit for median summary info.

      call getlun(obj%median_lu,ier)
      if (ier /= 0) then
        call pc_error ("TELAV failed to get an i/o unit for summary info.")
      else
        open (unit=obj%median_lu, status='SCRATCH', iostat=ier, &
              access='SEQUENTIAL', action='READWRITE')
        if (ier /= 0) then
          call pc_error ("TELAV had trouble opening file for summary info.")
        endif
      endif

! if needed, get an i/o unit for detailed trace kill info.

      if (obj%opt_print) then
        call getlun(obj%trkill_lu,ier)
        if (ier /= 0) then
          call pc_error ("TELAV failed to get an i/o unit for detailed info.")
        else
          open (unit=obj%trkill_lu, status='SCRATCH', iostat=ier, &
                access='SEQUENTIAL', action='READWRITE')
          if (ier /= 0) then
            call pc_error ("TELAV had trouble opening file for detailed info.")
          endif
        endif
      endif

! Allocate permanent memory here.

      call mem_alloc (obj%work, obj%ntume + numtr)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

!</execute_only>

      return
      end subroutine telav_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine telav_trap (keyword)
      implicit none
      character(len=*),intent(in)  :: keyword     ! argument
      real                         :: trace_len   ! local
 
      select case (keyword)

      case ('FCTR_KIL')
        if (object%fctr_kil < 1.0) then
          call pc_warning &
                 ('FCTR_KIL =',object%fctr_kil,' Must be greater than 1.0')
          call pc_warning ('Resetting FCTR_KIL to default: 10')
          object%fctr_kil = 10.0
        endif

      case ('NTUME')
        if (object%ntume <= 3) then
          call pc_warning ('NTUME =',object%ntume, ' Must be greater than 3')
          call pc_warning ('Resetting NTUME to default: 10000')
          object%ntume = 10000
        endif

      case ('LAV_TIM_BEG')
        trace_len = object%dt * (object%ndpt-1)
        if (object%lav_tim_beg > trace_len) then
          call pc_warning ('LAV_TIM_BEG is greater than trace end time')
          call pc_warning ('Resetting LAV_TIM_BEG to default: 0.0')
          object%lav_tim_beg = 0.0
        elseif (object%lav_tim_beg < 0.0) then
          call pc_warning ('LAV_TIM_BEG must not be negative.')
          call pc_warning ('Resetting LAV_TIM_BEG to default: 0.0')
          object%lav_tim_beg = 0.0
        endif

      end select
      return
      end subroutine telav_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine telav (obj,ntr,hd,tr)
      implicit none
      type(telav_struct),intent(inout) :: obj                ! arguments
      integer           ,intent(inout) :: ntr                ! arguments
      double precision  ,intent(inout) :: hd(:,:)            ! arguments
      real              ,intent(inout) :: tr(:,:)            ! arguments

      integer               :: mute, itr                     ! local
      integer               :: start_smp     , ier ! local

      real                  :: tlav                          ! local
      real,       parameter :: eogrp_flag = 999999.0         ! local


!!---------------------- start processing a set of traces ------------------!!
!!---------------------- start processing a set of traces ------------------!!
!!---------------------- start processing a set of traces ------------------!!


      if(obj%fromab .and. ntr == NO_MORE_TRACES) obj%more=.false.

!-------When DELAY is user defined, always compute TLAV in user defined window.

      if(obj%lav_tim_beg > 0. .and. obj%fromab .and. ntr > 0) then 

!----------Loop over the number of traces passed in.

        do itr = 1,ntr   

!----------Initialize the TLAV header word assuming no data in window.

          hd(obj%ihw,itr) = 0.0

!----------Compute the data window index boundaries.

          mute = max(nint(hd(HDR_TOP_MUTE,itr)),1)
          start_smp = mute+obj%nadd

!----------As needed, get new TLAV values.

          if(obj%ndpt >= start_smp) then
            tlav = lav( tr(start_smp:obj%ndpt,itr), obj%ndpt-start_smp+1 )
            hd(obj%ihw,itr) = tlav
          else
            hd(obj%ihw,itr) = 0.0
          endif

        enddo

      endif

!----------Branch to the mode dependent piece of code.

80    select case (obj%mode)


!!---------------------------- COLLECT mode --------------------------------!!
!!---------------------------- COLLECT mode --------------------------------!!
!!---------------------------- COLLECT mode --------------------------------!!


!----------First collect traces to disk.

      case ('COLLECT')

  !     if (obj%ncollected < obj%ntume .and. ntr > 0) then
        if (obj%plive < obj%ntume .and. ntr > 0) then
          do itr=1,ntr

!----------Update headers and counter.

            obj%ncollected = obj%ncollected + 1
            hd(HDR_SCRATCH_31,itr)=0.0
            if (itr == ntr) hd(HDR_SCRATCH_31,itr) = eogrp_flag

!----------Temporarily store headers and traces to disk.

            call temptfile_write (obj%temptfile, 0, hd(1:obj%nwih,itr), &
                                                    tr(1:obj%ndpt,itr), ier)
            if ( ier /= TEMPTFILE_OK ) then
              call pc_print &
                ('TELAV Error: Writing to temporary file at trace ', &
                      obj%ncollected)
              ntr = FATAL_ERROR
              call telav_wrapup(obj)
              return
            endif

!----------Accumulate info for the median calculation.

            call telav_add_trace_info (obj, hd(:,itr))

          enddo
        endif

   !    if (obj%ncollected >= obj%ntume .or. .not.obj%more) then
        if (obj%plive >= obj%ntume .or. .not.obj%more) then

!----------Either all traces are here, or at least enough to get a median.

          call telav_get_median (obj)

!----------Update variables as needed.

          obj%mode      = 'RELEASE'
          obj%fromab    = .false.
          call temptfile_rewind (obj%temptfile)
          goto 80  ! Must test against mode to process 'RELEASE' case.

        else

!----------Continue collecting traces.

          ntr = NEED_TRACES
          return 

        endif


!!---------------------------- RELEASE mode --------------------------------!!
!!---------------------------- RELEASE mode --------------------------------!!
!!---------------------------- RELEASE mode --------------------------------!!


!----------Now process the traces that were temporarily stored to disk.

      case ('RELEASE')

        if(obj%nreleased < obj%ncollected) then

          ntr = 0
          DO

            obj%nreleased = obj%nreleased + 1
            ntr = ntr+1
            itr = ntr

!----------Read a trace.

            call temptfile_read (obj%temptfile, 0, hd(1:obj%nwih,ntr), &
                                                   tr(1:obj%ndpt,ntr), ier)
            if (ier /= TEMPTFILE_OK ) then
              call pc_print &
                ('TELAV Error: Reading from temporary file at trace ', &
                 obj%nreleased)
              ntr = FATAL_ERROR
              call telav_wrapup(obj)
              return
            endif

!----------As needed, kill the trace and clear some headers.

            call telav_kill_trace (obj, hd(:,itr), tr(:,itr))

            if(hd(HDR_SCRATCH_31,itr) == eogrp_flag) EXIT

          ENDDO
          return

        endif

        if (obj%more) then
          obj%mode   = 'PASS'
          obj%fromab = .true.
          ntr = NEED_TRACES
        else
          ntr = NO_MORE_TRACES
        endif
        return


!!------------------------------- PASS mode --------------------------------!!
!!------------------------------- PASS mode --------------------------------!!
!!------------------------------- PASS mode --------------------------------!!


!----------Now handle the remainder of the incoming dataset.

      case ('PASS')

        if(obj%more) then

          if (.not. obj%fromab) then
            ntr = NEED_TRACES
            obj%fromab = .true.
            return
          endif

          do itr=1,ntr

!----------Gather info for updating median value.

            call telav_add_trace_info (obj, hd(:,itr))

!----------Kill traces as required.

            call telav_kill_trace (obj, hd(:,itr), tr(:,itr))

          enddo

!----------Every so often, report the median for the last NTUME traces.

          if (obj%plive >= obj%ntume) call telav_get_median (obj)
          return

        endif


!!--------------------- finish processing a set of traces ------------------!!
!!--------------------- finish processing a set of traces ------------------!!
!!--------------------- finish processing a set of traces ------------------!!


      endselect

      call telav_wrapup (obj)
      ntr = NO_MORE_TRACES
      obj%fromab=.false.
      return
      end subroutine telav


!!---------------------------- add trace info ----------------------------!!
!!---------------------------- add trace info ----------------------------!!
!!---------------------------- add trace info ----------------------------!!


      subroutine telav_add_trace_info (obj,hd)
      implicit none
      type(telav_struct),intent(inout) :: obj         ! arguments
      double precision  ,intent(in)    :: hd(:)       ! arguments

      if(hd(obj%ihw) > 0.0) then
           obj%nlive           = obj%nlive + 1
           obj%plive           = obj%plive + 1
           obj%work(obj%plive) = hd(obj%ihw)
      else
           obj%ndead           = obj%ndead + 1
           obj%pdead           = obj%pdead + 1
      endif
      return
      end subroutine telav_add_trace_info


!!---------------------------- get median --------------------------------!!
!!---------------------------- get median --------------------------------!!
!!---------------------------- get median --------------------------------!!


      subroutine telav_get_median (obj)
      implicit none
      type(telav_struct),intent(inout) :: obj                ! arguments
      real                             :: xmed               ! local

      if (obj%threshold < 0.0) then
           call median (obj%work, obj%plive, xmed)
           obj%amed      = xmed
           obj%amedmin   = xmed
           obj%amedmax   = xmed
           obj%threshold = obj%fctr_kil * obj%amed
      else
           call median (obj%work, obj%plive, xmed, obj%amed)
           obj%amedmin   = min(obj%amedmin, xmed)
           obj%amedmax   = max(obj%amedmax, xmed)
      end if

      write (obj%median_lu,*) xmed, obj%pdead, obj%plive, obj%pkill
      obj%pdead = 0
      obj%plive = 0
      obj%pkill = 0    ! pkill might be one trace group behind on the file.
      return
      end subroutine telav_get_median


!!------------------------------- kill trace -----------------------------!!
!!------------------------------- kill trace -----------------------------!!
!!------------------------------- kill trace -----------------------------!!


      subroutine telav_kill_trace (obj, hd, tr)
      implicit none
      type(telav_struct),intent(inout) :: obj         ! arguments
      double precision  ,intent(inout) :: hd(:)       ! arguments
      real              ,intent(inout) :: tr(:)       ! arguments

      if (hd(obj%ihw) > obj%threshold) then

          if (obj%opt_print) write (obj%trkill_lu,*)                 &
                                       nint(hd(HDR_SEQUENCE)),       &
                                       hd(obj%ihw),                  &
                                       nint(hd(HDR_ORIGINAL_GROUP)), &
                                       nint(hd(HDR_ORIGINAL_CHANNEL))

          tr(1:obj%ndpt)  = 0.0          ! zero the samples.
          hd(HDR_LAV)     = 0.0          ! dead LAV.
          hd(obj%ihw)     = 0.0          ! dead windowed LAV.
          obj%nkill       = obj%nkill+1  ! increment kill counter.
          obj%pkill       = obj%pkill+1  ! increment kill counter.

      endif
      return
      end subroutine telav_kill_trace


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


   subroutine telav_wrapup (obj)
   implicit none
   type(telav_struct),intent(inout) :: obj                     ! arguments
   logical                          :: opened                  ! local

   if (obj%skip_wrapup) return
   obj%skip_wrapup = .true.

   call pc_print(' ')
   call pc_print('**********************************************************')
   call pc_print('**************** TELAV PROCESSING SUMMARY ****************')
   call pc_print('**********************************************************')
   call pc_print(' ')

   if(obj%plive > 0) call telav_get_median (obj)
                     call telav_reporter   (obj)
                     call temptfile_close  (obj%temptfile)

   inquire(UNIT=obj%median_lu, OPENED=opened)
   if (opened) close (unit=obj%median_lu, status='DELETE')

   inquire(UNIT=obj%trkill_lu, OPENED=opened)
   if (opened) close (unit=obj%trkill_lu, status='DELETE')

   call pc_print(' ')
   call pc_print('**********************************************************')
   call pc_print('************** END TELAV PROCESSING SUMMARY **************')
   call pc_print('**********************************************************')
   call pc_print(' ')
   return
   end subroutine telav_wrapup



!!---------------------------- reporter -----------------------------------!!
!!---------------------------- reporter -----------------------------------!!
!!---------------------------- reporter -----------------------------------!!


      subroutine telav_reporter (obj)
      implicit none
      type(telav_struct)       :: obj                         ! arguments
      integer                  :: ier,pdead,plive,pkill,indx  ! local
      real                     :: xmed,lav                    ! local
      integer                  :: isequence,group,channel     ! local
      character(len=120)       :: f1,f2,f3,f4,f5,f6,f7,f8     ! local
      character(len=120)       :: g1,g2                       ! local

!----------Format statements.

f1="(8X,'                   total          live           dead               ')"
f2="(8X,'median value   input traces   input traces   input traces           ')"
f3="(8X,'   found         examined         used          ignored      killed ')"
f4="(8X,'------------   ------------   ------------   ------------   --------')"
f5="(1X,I4,4X,G13.6E2,3x,I8,2I15,I12)"
f6="(11X,'total input:  ',I8,2I15)"
f7="(11X,'total changes:',8X,2I15,I12)"
f8="(11X,'total output: ',I8,2I15)"

g1="(11x,'HDR_SEQUENCE',5x,'LAV',5x,'HDR_ORIGINAL_GROUP',  &
                               & 2x,'HDR_ORIGINAL_CHANNEL')"
g2="(1X,I7,3X,I10,4X,G13.6E2,I13,8X,I13)"

!----------Print Overall Summary Info.

      call pc_print ('input parameter FCTR_KIL    =',obj%fctr_kil   )
      call pc_print ('input parameter NTUME       =',obj%ntume      )
      call pc_print ('input parameter OPT_PRINT   =',obj%opt_print  )
      call pc_print ('input parameter LAV_TIM_BEG =',obj%lav_tim_beg)

      write(obj%prt_lu,*) ' '
      write(obj%prt_lu,*) &
        'TELAV killed ',obj%nkill,' traces using a median of ',obj%amed
      write(obj%prt_lu,*) &
        '                             and a threshold LAV of ',obj%threshold
      write(obj%prt_lu,*) ' '
      write(obj%prt_lu,*)'Minimum median value found: ',obj%amedmin
      write(obj%prt_lu,*)'Maximum median value found: ',obj%amedmax
      write(obj%prt_lu,*) ' '

!----------Print Median Summary Info.

      if (obj%median_lu > 0) then
        write(obj%prt_lu,*) 'SUMMARY OF MEDIAN VALUES FOUND:'
        write(obj%prt_lu,*) ' '
        write(obj%prt_lu,f1)
        write(obj%prt_lu,f2)
        write(obj%prt_lu,f3)
        write(obj%prt_lu,f4)

        rewind(obj%median_lu)

        indx = 0
        do
          read (obj%median_lu,*,iostat=ier) xmed,pdead,plive,pkill
          if(ier<0) exit  ! negative iostat means end of file/record
          indx = indx + 1
          write(obj%prt_lu,f5) indx, xmed, plive+pdead, plive, pdead, pkill
        enddo
        write(obj%prt_lu,f4)
        write(obj%prt_lu,f6) obj%nlive+obj%ndead, obj%nlive, obj%ndead
        write(obj%prt_lu,f7)                 -obj%nkill, obj%nkill, obj%nkill
        write(obj%prt_lu,f8) obj%nlive+obj%ndead, obj%nlive-obj%nkill, &
                                                    obj%ndead+obj%nkill
        write(obj%prt_lu,*) ' '
      endif

!----------Print Detailed Trace Kill Info.

      if(obj%opt_print) then

        write(obj%prt_lu,*) 'SUMMARY OF TRACES KILLED:'
        write(obj%prt_lu,*) ' '
        write(obj%prt_lu,g1)

        rewind (obj%trkill_lu)

        indx = 0
        do
          read (obj%trkill_lu,*,iostat=ier) isequence, lav, group, channel
          if(ier<0) exit  ! negative iostat means end of file/record
          indx = indx + 1
          write(obj%prt_lu,g2) indx, isequence, lav, group, channel
        enddo

        write(obj%prt_lu,g1)
        write(obj%prt_lu,*) ' '

      endif
      return
      end subroutine telav_reporter


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>

      end module telav_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
