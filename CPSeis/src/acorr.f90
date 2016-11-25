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
! Name       : acorr   (AutoCORRelation)
! Category   : diagnostics
! Written    : 1989-03-15   by: John Reed
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : computes the autocorrelations of windowed trace data.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! Computes and outputs the autocorrelations of trace windows in trace form.
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
!
! This process outputs traces with same gather status as the input traces.
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
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 2       Head mute index            Used and or reset
! 25      largest absolute value     recomputed
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 15. 2006-09-11  Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 14. 2001-04-26  Selzler    Changed wrapup logic to use skip_wrapup
! 13. 2000-10-03  Selzler    Corrected bug, linked array length initialization.
! 12. 2000-07-07  Selzler    Fixed problems found by CPS Fortran Code Review.
! 11. 2000-03-28  Selzler    Corrected bug in wrapup logic and parm rounding
! 10. 2000-02-02  Selzler    Added support for GUI and general cleanup
!  9. 1999-11-19  Selzler    Added RCS "Id" strings to tag executeable
!  8. 1999-09-13  Selzler    Updated skip_wrapup and print_lun usage
!  7. 1999-08-26  Selzler    Conversion to f90
!  6. 1998-11-24  Goodger    Begin using the fortran90 compiler.
!  5. 1989-07-27  Reed       Removed logic hanging autocorrelations at mutes
!  4. 1989-07-06  Reed       Clarification of documentation.
!  3. 1989-05-04  Baumel     Increase robustness: check filtrgs arguments in
!                            range, fix PUTP parameter count.
!                            Honor TSTRTY global.
!  2. 1989-04-12  Reed       Original version complete.
!  1. 1989-03-15  Reed       started writing the Cray code.
!
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

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
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
!    NTR == NO_MORE_TRACES means there are no more input traces.
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
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!   TR(tim_beg(I+K))=
!     SUM(K=tim_beg(I)-->tim_end(I)){TR(tim_beg(I))*TR(tim_beg(I+K))}
!
!   Autocorrelations are output with zero lag values at each tim_beg
!   value.  Positive lags follow for length len_ac.  No negative lags
!   are computed.
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS ACORR Process/NC=80>
!Computes the autocorrelations of trace windows in trace form.
!
!        LEN_AC= `FFFFFFFFFFF  NORM=~~~`CC 
!         
!            TIM_BEG     TIM_END
!            `FFFFFFFFFFF`FFFFFFFFFFF
!            `FFFFFFFFFFF`FFFFFFFFFFF
!            `FFFFFFFFFFF`FFFFFFFFFFF
!            `FFFFFFFFFFF`FFFFFFFFFFF
!            `FFFFFFFFFFF`FFFFFFFFFFF
!<PARMS TIM_BEG_ARRAYSET[/XST/YST]>
!</gui_def>
!<HelpSection>
!
!<Help KEYWORD="LEN_AC">
!<Tip> Length of computed autocorrelation in seconds. </Tip>
! Default = 0.4 seconds
! Allowed > 0.0 seconds
!</Help>

!<Help KEYWORD="NORM">
!<Tip> Normalize autocorrelation zero lags to 1.0. </Tip>
! Default = YES
! Allowed = YES  (Normalize autocorrelation zero lag to 1.0.)
! Allowed = NO   (Do NOT normalize.)
!</Help>

!<Help KEYWORD="TIM_BEG">
!<Tip> Array of beginning autocorrelation window times in seconds. </Tip>
! Default =  -
! allowed = real
!</Help>

!<Help KEYWORD="TIM_END">
!<Tip> Array of ending autocorrelation window times in seconds. </Tip>
! Default =  -
! Allowed = real
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module acorr_module
      use pc_module
      use named_constants_module
      use lav_module
      use mth_module
      use getlun_module
      use fltr_module
      implicit none
      private
      public :: acorr_create     ! uses the parameter cache.
      public :: acorr_initialize
      public :: acorr_update     ! uses the parameter cache.
      public :: acorr_delete

!<execute_only>

      public :: acorr            ! main execution (trace processing) routine.
      public :: acorr_wrapup

!</execute_only>

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer, parameter :: max_cnt = 10

      type,public :: acorr_struct
      private
        logical                    :: skip_wrapup      ! dependent parameter

        real                       :: len_ac           ! process parameter.
        character(len=3)           :: norm             ! process parameter.
        real,dimension(max_cnt)    :: tim_beg          ! process parameter.
        real,dimension(max_cnt)    :: tim_end          ! process parameter.

        integer                    :: ndpt             ! global
        real                       :: dt               ! global
        real                       :: tstrt            ! global

        integer,dimension(max_cnt) :: tim_beg_idx      ! dependent parameter.
        integer,dimension(max_cnt) :: tim_end_idx      ! dependent parameter.
        integer                    :: ac_cnt           ! dependent parameter
        integer                    :: tim_beg_cnt      ! dependent parameter
      end type acorr_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(acorr_struct),pointer,save :: object      ! needed for traps.

      integer :: print_lun = 0                        ! state variable
                 ! default = pc_get_lun()
                 ! valid = 0, do not print
                 !       > 0, Fortran print LUN for system output

      character(len=100),public :: acorr_ident = &
        "$Id: acorr.f90,v 1.15 2006/09/11 13:15:43 Stoeckley prod sps $"

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine acorr_create (obj)
      implicit none
      type(acorr_struct),pointer :: obj       ! arguments

      allocate (obj)

      call acorr_initialize (obj)

      return
      end subroutine acorr_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine acorr_delete (obj)
      implicit none
      type(acorr_struct),pointer :: obj       ! arguments

!<execute_only>
      call acorr_wrapup (obj)
!</execute_only>

      deallocate(obj)

      return
      end subroutine acorr_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine acorr_initialize (obj)
      implicit none
      type(acorr_struct),pointer :: obj       ! arguments

      obj%len_ac = 0.4
      obj%norm = 'YES'
      obj%tim_beg = 0.0
      obj%tim_end = 0.0
      obj%ndpt = 0
      obj%dt = 0.0
      obj%tstrt = 0.0

      obj%tim_beg_idx = 0
      obj%tim_end_idx = 0
      obj%ac_cnt = 0
      obj%tim_beg_cnt = 0

      print_lun = 0

      call acorr_update (obj)

      return
      end subroutine acorr_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine acorr_update (obj)
      implicit none
      type(acorr_struct),target :: obj                           ! arguments

      integer :: item       ! local
      integer :: state      ! local
      integer :: tim_beg_cnt2

      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

      state = pc_get_update_state()

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("tim_beg_arrayset", (/  &
                                    "tim_beg",              &
                                    "tim_end" /))

      call pc_get_global ('ndpt'  , obj%ndpt)
      call pc_get_global ('dt'  , obj%dt)
      call pc_get_global ('tstrt'  , obj%tstrt)

      call pc_get('len_ac', obj%len_ac)
      call pc_get('norm', obj%norm)
      call string_to_upper(obj%norm)
      tim_beg_cnt2 = obj%tim_beg_cnt
      call pc_get('tim_beg', obj%tim_beg, obj%tim_beg_cnt)
      call pc_get('tim_end', obj%tim_end, tim_beg_cnt2)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%dt <= 0.0) then
        call pc_error('DT must be greater than zero')
        return
      end if

      if(obj%len_ac <= 0.0) then
        call pc_error('LEN_AC must be greater than 0.0')
      end if

      if(obj%norm(1:1) == 'Y') then
        obj%norm = 'YES'
      else if(obj%norm(1:1) == 'N') then
        obj%norm = 'NO'
      else
        call pc_error('NORM must be either YES or NO')
        obj%norm = 'YES'
      end if

      obj%ac_cnt = NINT(obj%len_ac/obj%dt)

      if(0 /= mth_compare(obj%len_ac, obj%ac_cnt*obj%dt)) then
        call pc_warning('Rounding LEN_AC to nearest DT')
        obj%len_ac = obj%ac_cnt*obj%dt
      end if

      if(obj%tim_beg_cnt /= tim_beg_cnt2) then
        call pc_error('arrays not linked (TIM_BEG, TIM_END)')
      end if

      if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
        pc_verify_arrayset('tim_beg_arrayset')) then
        if(obj%tim_beg_cnt == 0) then
          call pc_error('One or more TIM_BEG/END pairs required')
        end if

        do item = 1, obj%tim_beg_cnt
          if(obj%tim_beg(item) >= obj%tim_end(item)) then
            call pc_error('TIM_BEG must be less than TIM_END for all items')
          end if

          obj%tim_beg_idx(item) = MAX(1, &
            NINT((obj%tim_beg(item)-obj%tstrt)/obj%dt)+1)

          if(0 /= mth_compare(obj%tim_beg(item), &
            obj%tstrt + (obj%tim_beg_idx(item)-1)*obj%dt)) then
            call pc_warning('Rounding TIM_BEG(', item, &
              ') to nearest DT')
            obj%tim_beg(item) = obj%tstrt + (obj%tim_beg_idx(item)-1)*obj%dt
          end if

          obj%tim_end_idx(item) = MIN(obj%ndpt, &
            NINT((obj%tim_end(item)-obj%tstrt)/obj%dt)+1)

          if(0 /= mth_compare(obj%tim_end(item), &
            obj%tstrt + (obj%tim_end_idx(item)-1)*obj%dt)) then
            call pc_warning('Rounding TIM_END(', item, &
              ') to nearest DT')
            obj%tim_end(item) = obj%tstrt + (obj%tim_end_idx(item)-1)*obj%dt
          end if

        END DO
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put('len_ac', obj%len_ac)
      call pc_put_options_field('norm', (/ "YES", "NO " /), 2)
      call pc_put('norm', obj%norm)
      call pc_put('tim_beg', obj%tim_beg, obj%tim_beg_cnt)
      call pc_put('tim_end', obj%tim_end, obj%tim_beg_cnt)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      print_lun = pc_get_lun()

      write(print_lun, *) '  ACORR PARAMETERS'
      write(print_lun, *) 'AUTOCORRELATION LENGTH= ', obj%len_ac

      IF (obj%norm == 'YES') write(print_lun, *) 'AUTOCORRELATIONS NORMED'

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine acorr_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine acorr (obj,ntr,hd,tr)
      implicit none
      type(acorr_struct)               :: obj                   ! arguments
      integer         ,intent(inout)  :: ntr                    ! arguments
      double precision,intent(inout)  :: hd(:,:)                ! arguments
      real            ,intent(inout)  :: tr(:,:)                ! arguments

      real, dimension(obj%ndpt) :: SCTR     ! local
      real :: ac1
      integer :: NTWIN1, LWIN, NACL1
      integer :: ntr_do, win_do

      ntr_loop: DO ntr_do = 1, ntr
        SCTR = 0.0

        DO win_do = 1, obj%tim_beg_cnt
          NTWIN1 = MAX(NINT(HD(2,ntr_do)),obj%tim_beg_idx(win_do))
          LWIN = obj%tim_end_idx(win_do) - NTWIN1 + 1

          IF (LWIN < 1) CYCLE
          NACL1 = MIN(obj%ac_cnt,LWIN - 1)

          CALL fltr_filtrgs (TR(NTWIN1:,ntr_do), LWIN, TR(NTWIN1:,ntr_do), &
            LWIN, SCTR(obj%tim_beg_idx(win_do)), NACL1 + 1, 0, 0)

          IF (obj%norm /= 'YES') CYCLE

          AC1 = SCTR(obj%tim_beg_idx(win_do))

          IF (AC1 == 0.) CYCLE

          AC1 = 1.0/AC1
          SCTR(obj%tim_beg_idx(win_do):NACL1+obj%tim_beg_idx(win_do)) = &
          SCTR(obj%tim_beg_idx(win_do):NACL1+obj%tim_beg_idx(win_do))*AC1
        END DO

        TR(:obj%ndpt,ntr_do) = SCTR(:obj%ndpt)
        HD(2,ntr_do) = 1

      END DO ntr_loop

      call lav_set_hdr(hd, tr, obj%ndpt, ntr)

      return
      end subroutine acorr

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine acorr_wrapup (obj)
      implicit none
      type(acorr_struct) :: obj       ! arguments

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine acorr_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module acorr_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
