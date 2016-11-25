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
!                        C P S   P R O C E S S
!
! Name       : STK  (STacK traces) [Also includes former STKF]
! Category   : Stacks
! Written    : 1988-05-24  by: Mike Howard
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Sum seismic traces with a common header word, then scale output.
! Portability: Yes iff input is gathered and OPT_OUT=TRACE.
! Parallel   : No.
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! STK stacks traces in a gather (normally a CMP gather) to a single stacked
! trace which is then scaled by a method determined by the scaling parameters.
!
! The headers of the stacked trace are equal to the headers of the first live
! trace in the stack gather, except for:
!     HD(1) = new sequential trace number.
!     HD(2) = minimum head mute time in stack gather.
!     HD(5) = fold of stack.
!     HD(25)= lav of stacked trace.
!     HD(64)= maximum tail mute time in stack gather.
!
! If OPT_OUT = TRACE then stacked traces are output in the trace output stream.
! If OPT_OUT = FILE then stacked traces are written to a designated TRCIO file
! and input traces pass through unchanged.
! The output file name can have any file-extension desired, but will be in the
! new CPS output file format.
!
! STK now includes the functionality of the former STKF process.
!
!-------------------------------------------------------------------------------
!</descript_doc>
!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS
!
! Traditional Conoco scaling used FSE = 0.5 and MSCL = 0.25.  TVFSE = 0.5
! is an industry-standard approach and should be very similar to the traditional
! Conoco results.
!
! Input traces must be sorted to desired sort order.  Typically this will be
! CMP order.
!-------------------------------------------------------------------------------
!</advice_doc>
!<trace_in_doc>
!-------------------------------------------------------------------------------
!                       TRACE INPUT REQUIREMENTS
!
! Process is either a single-trace or multiple-trace process.
!
! Process requires traces to be input in CMP or other stack sort order.
!-------------------------------------------------------------------------------
!</trace_in_doc>
!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! If OPT_OUT = TRACE then stacked traces are output in the trace output stream.
! If OPT_OUT = FILE then stacked traces are written to a designated TRCIO file
! and input traces pass through unchanged.
!-------------------------------------------------------------------------------
!</trace_out_doc>
!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     Number of trace values                used - not changed
! NWIH     Number of header values               used - not changed
! DT       Sample Rate                           used - not changed
! TSTRT    Start time for traces                 used - not changed
! GRID     Transform                             used - not changed
! NUMTR    Max number of traces input/output     may be changed on output:
!                                                NUMTR=1 for OPT_OUT='TRACE'!
!-------------------------------------------------------------------------------
!</global_doc>
!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! all     all headers                The headers of the stacked trace are equal
!                                    to the headers of the first live trace in
!                                    the stack gather, except for:
! HD(1) = new sequential trace number.
! HD(2) = minimum head mute time in stack gather.
! HD(5) = fold of stack.
! HD(25) = lav only on stacked traces.
! HD(64)= maximum tail mute time in stack gather.
!
! all     all headers                all headers are passed unchanged to next
!                                    process if OPT_OUT=='FILE'.
!-------------------------------------------------------------------------------
!</header_word_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                        REVISION HISTORY FOR STK
!
!     Date       Author      Description
! --- ---------- ----------- ---------------------------------------------------
!048. 2006-06-20  B. Menger   Removed Unused Variables.
! 47. 2005-01-31 Stoeckley   Add the STACKING_MODE parameter so that the
!                             "flatness stack" can be calculated.
! 46. 2004-05-13 Stoeckley   Fix scaling bugs for trimmed mean stack.
! 45. 2004-04-16 Stoeckley   Improve scaling for trimmed mean stack.
! 44. 2003-11-12 Stoeckley   Add trimmed mean stack capability to gathers.
! 43. 2003-06-17 Goodger     Fix permissions on file.
! 42. 2002-05-28 C. C. Burch Fixed handling optional arguments when called by
!                            processes
! 41. 2002-05-20 Goodger     Add information only field of gath_info.
! 40. 2002-05-08 C. C. Burch Set PCPS_RESEQUENCE_TRACES when in parallel mode
! 39. 2002-04-24 C. C. Burch Changes to accomodate NEED_TRACES for PCPS
!                            parallel mode for gather, not parallel for single
! 38. 2002-04-18 Ed Schmauch Accommodate changes in trcio_struct.
! 37. 2001-08-27 Goodger     Change trcio_write_history to trcio_write_history
!                            _cards.
! 36. 2001-06-18 Goodger     Remove jump to HDR_FLAG field. PRODUCTION.
! 35. 2001-02-13 Bill Menger Added skip_wrapup flag, remove wrapup_done flag.
! 34. 2000-09-20 Bill Menger Modified control logic.
! 33. 2000-09-05 Bill Menger Added delete of object on stk_delete.
! 32. 2000-08-17 Bill Menger Modified the trcio_open call to open with "w".
! 31. 2000-07-27 Bill Menger Removed the "cycle if sample==0" statements
! 30. 2000-07-26 Bill Menger Added stkscale primitive and added better gather
!                            logic, removed opt_input parameter.
! 29. 2000-05-08 Bill Menger continued with opt_input, using ONLY the gathered
!                            global to tell if OPT_INPUT=gather.  Also disabled
!                            GATH_INIT,etc if OPT_INPUT=gather.
! 28. 2000-03-29 Bill Menger disabled the OPT_INPUT field, set it automatically
!                            from the gathered and/or numtr globals, added the
!                            stk.gui file into the layout section.
! 27. 2000-02-24 Bill Menger Reset gath_inc and gath_init to 1.0 for default,
!                            removed some restrictive code in traps for both
!                            and fixed gath_inc trap to not allow 0.0 (because
!                            it will cause divide by zero later).
! 26. 2000-02-14 Bill Menger Modified pathcheck code.
! 25. 2000-02-02 Bill Menger Added combo boxes, pathcheck,fixed traps.
! 24. 2000-02-01 Bill Menger Changed dead trace detection from the old method
!                            of testing hd(64) > ndpt to new method of looking
!                            at the LAV (hd(25)).  Added lav_module dependency.
! 23. 2000-01-28 Bill Menger Modified FILE output from BYTE to TRCIO file.
! 22. 1999-09-15 Goodger   Parameter cache name changes.
! 21. 1999-09-15 Goodger   Parameter cache name changes.
! 20. 1999-08-30 B Troutt  Ready for TESTLIB - still using BYTE - not STROT.
! 19. 1999-08-23 B Troutt  Change all header variables to double precision.
! 18. 1999-08-16 B Troutt  Use BYTE instead of STROT for now so we can test.
! 17. 1999-07-28 B Troutt  Begin conversion to new CPS system.
! 16. 99/07/19: B Troutt; Change name of array from SUM to SUMT for
!                         Fortran 90.  SUM is a reserved word. This
!                         Change needed in order to make VAST work
!                         for beginning conversion to new CPS system.
! 15. 98/11/18  Goodger   Begin using the fortran90 compiler.
! 14. 92/05/07  B Troutt  No update. This version of STK put on system
!                         today after several weeks in [CPS.TEST].
! 13. 92/03/26  B Troutt  This version of STK honors the input mutes
!                         and discards values outside their range.
!                         The main change is the "IKILL" argument to
!                         MUTEHW.
! 12. 92/03/26  B Troutt  Add call to MUTEHW and tidy up tail mute
!                         logic. Note that updates 10,11 were never on
!                         the "system", but were accessed from
!                         [HOWARD.CONV].
!                         This version of STK uses input values even if
!                         they fall outside of the mute indeces.  This
!                         is the way STK has always worked: include all
!                         nonzero input values in the stacked trace,
!                         and retain the least severe mute indeces for
!                         the stacked trace headers.
! 11. 92/02/28  B Troutt  Add check for tail mute > NDPT.
! 10. 92/02/21  M Howard  Add tail mute.
! 9.  90/05/15  Peterson  Headers for bin come from first LIVE trace.
! 8.  90/02/15  M Howard  Fix bug is last trace in group is dead
! 7.  90/02/09  M Howard  Try to ignore dead traces from read errors.
! 6.  89/06/05  M Howard  Fix alternate return in SUBROUTINE statement
! 5.  89/06/05  M Howard  Add end-of-processing statistics.
! 4.  88/09/23  M Howard  NWPT and NWIH conversion. New SQRTFN
! 3.  88/08/27  M Howard  Add single trace input option.
! 2.  88/06/16  B Baumel  Fix bug in parameter saving statements.
! 1.  88/05/24  M Howard  Original version.
!
!
!                      REVISION HISTORY FOR STKF
!
!
!     Date     Author     Description
!     ----     ------     -----------
! 12. 98/12/14 Goodger    Begin using the fortran90 compiler.
! 11. 98/04/20 Day/GoodgerFix bug with trace length in call strot.
! 10. 95/07/24 Goodger    Increase stack trace limit to 15000.
!                         Added note 3.
! 9.  92/03/30 Troutt     Add tail mute logic as per STK. Call MUTEHW.
! 8.  91/08/13 Troutt     Fix REPP call for trace with zero header
!                         values. It was pointing to INPUT instead of
!                         ISAVE.
! 7.  91/03/27 Howard     Fix bug if 1st trace is live and 2nd is dead.
! 6.  90/05/21 Howard     Add FLAG header to SELECT traces for stack.
! 5.  90/05/15 Howard     Change to use headers from first LIVE trace.
! 4.  90/02/15 Howard     Fix bug if last trace in group is daad.
! 3.  90/02/09 Howard     Try to ignore dead traces from read errors.
! 2.  89/11/29 Howard     Increase stack trace limit to 10000.
! 1.  89/10/06 Howard     Original version.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                    SPECIAL COMPILING REQUIREMENTS
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>
!<calling_doc>
!-------------------------------------------------------------------------------
!                   SPECIFIC CALLING CHARACTERISTICS
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
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!</int_calling_doc>
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!-------------------------------------------------------------------------------
!</algorithm_doc>
!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!-------------------------------------------------------------------------------
!</programming_doc>
!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS STK Process/NC=80>
!
!                  Stack traces within gathers.
!
!STACKING_MODE=`CCCCCCCCCCCCCCCCCCCCCCCCCC
!
!GATH_INFO=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!HDR_FLAG=`II                       TRIM_PERCENT=`FFFFFFFFFFF
!
!FSE=~~~~~`FFFFFFFFFFF  MSCL=`FFFFFFFFFFF  TVFSE=`FFFFFFFFFFF
!
!HDR_GATH=`II      GATH_INIT=`FFFFFFF   GATH_INC=`FFFFFFFFFFF
!
!OPT_OUT=`CCCC
!
!PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!<PARMS PATHNAME[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="STACKING_MODE">
!<Tip> Type of stack to perform. </Tip>
! Default = NORMAL STACK
! Allowed = NORMAL STACK
! Allowed = FLATNESS STACK
!
! NORMAL STACK: The trace amplitudes are simply averaged together and scaled.
!
! FLATNESS STACK: This is a special-purpose stack which is simply a stack of
!                 the absolute values minus the absolute value of the normal
!                 stack.
!</Help>
!
!<Help KEYWORD="GATH_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Whether input traces are gathered. </Tip>
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! If HDR_FLAG = 0 then all traces are processed.  Otherwise only traces with
! a flag set in header word HDR_FLAG are processed.
!
!</Help>
!
!<Help KEYWORD="TRIM_PERCENT">
!<Tip> Percent of stack to trim (for trimmed mean stack). </Tip>
! Default = 0.0
! Allowed = 0.0 - 100.0
!
! If TRIM_PERCENT is greater than zero, each stacked sample point in the
! stacked trace will be calculated from the input points using a trimmed
! mean calculation.  A median stack is calculated if TRIM_PERCENT is 100.
!
! TRIM_PERCENT can be greater than zero only when traces are gathered.
! Execution time is much slower when TRIM_PERCENT is greater than zero.
!</Help>
!
!<Help KEYWORD="FSE">
!<Tip> Fold of stack exponential method for scaling stacked traces. </Tip>
! Default = 0.5
! Allowed = 0.0-1.0
!
! The Fold of Stack exponential method scales stacked traces by dividing trace
! samples by the number of live traces stacked together raised to the FSE
! power.  The FSE method is time-independent.  FSE = 0.0 does no scaling.
!
!</Help>
!
!<Help KEYWORD="MSCL">
!<Tip> Mute Scaling method for scaling stacked traces. </Tip>
! Default = 0.25
! Allowed = 0.0-1.0
!
! The Mute Scaling method scales stacked traces by multiplying by a complicated
! empirical formula (with no obvious justification) based on maximum fold and
! local time-dependent fold.  Mute scaling is intended to compensate for loss
! of fold due to muting.  MSCL = 0.0 does no scaling.
!
!        Scale(MSCL) = 1.0 + MSCL*(((maximum fold)/(actual fold)) - 1.0)
!
!</Help>
!
!<Help KEYWORD="TVFSE">
!<Tip> Time Varying Fold of Stack Exponential method. </Tip>
! Default = 0.0
! Allowed = 0.0-1.0
!
! The Time Varying Fold of Stack Exponential method scales stacked traces by
! dividing trace samples by the time varying number of live samples stacked
! together raised to the TVFSE power.  TVFSE = 0.0 does no scaling.  This is an
! industry-standard method.
!
! If TVFSE /= 0.0, then FSE and MSCL methods are disabled.
!
!</Help>
!
!<Help KEYWORD="HDR_GATH">
!<Tip> Header word on incoming traces designating stack gathers.</Tip>
! Default = 7
! Allowed = 1 - NWIH
!  This is only used when traces are coming in to stk singly.  If traces are
!  already gathered, there is no need for stk to internally gather on a
!  header word.
!</Help>
!
!<Help KEYWORD="GATH_INIT">
!<Tip> Value of stack gather header word for the first stack gather. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="GATH_INC">
!<Tip> Increment of stack gather header word between stack gathers. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OPT_OUT">
!<Tip> Option whether stacked traces are written to a file or passed out. </Tip>
! Default = TRACE
! Allowed = TRACE
! Allowed = FILE
! If OPT_OUT = TRACE then stacked traces are output in the trace output stream.
! If OPT_OUT = FILE then stacked traces are written to a designated STROT file
! and input traces pass through unchanged.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> File name of TRCIO file to hold stacked traces if OPT_OUT = FILE. </Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module stk_module
      use pc_module
      use cio_module
      use named_constants_module
      use string_module
      use mutehw_module
      use trcio_module
      use grid_module
      use pathcheck_module
      use lav_module
      use stkscale_module
      use mth_module
      use statutil_module

      implicit none

      private

      public :: stk_create     ! uses the parameter cache.
      public :: stk_initialize
      public :: stk_update     ! uses the parameter cache.
      public :: stk_delete
      public :: stk            ! main execution (trace processing) routine.
      public :: stk_wrapup

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

character(len=100),public,save :: stk_ident = &
"$Id: stk.f90,v 1.48 2006/06/20 13:12:10 Menger prod sps $"

      type,public :: stk_struct
        character(len=24)          :: stacking_mode    ! process parameter
        character(len=8)           :: opt_input        ! process parameter
        integer                    :: hdr_flag         ! process parameter
        character(len=8)           :: opt_out          ! process parameter
        character(len=FILENAME_LENGTH) :: pathname     ! process parameter
        integer                    :: hdr_gath         ! process parameter
        real                       :: gath_init        ! process parameter
        real                       :: gath_inc         ! process parameter
        real                       :: trim_percent     ! process parameter
        real                       :: fse              ! process parameter
        real                       :: mscl             ! process parameter
        real                       :: tvfse            ! process parameter

        real                       :: dt               ! global
        integer                    :: nwih             ! global
        integer                    :: ndpt             ! global
        integer                    :: numtr            ! global
        type(grid_struct)          :: grid             ! global
        real                       :: tstrt            ! global

        integer                    :: ntr_out          ! output trace count
        integer                    :: ntr_in           ! input trace count
        integer                    :: ntr_in_use       ! input traces used count
        integer                    :: ifold            ! nominal fold this stack
        integer                    :: maxfold          ! max nominal fold
        integer                    :: maxufold         ! max untrimmed fold
        logical                    :: firstliv         ! looking for 1st live
                                                       ! trace for current stack
        logical                    :: firstime         ! starting new stack
        logical                    :: lastout          ! the last stacked trace
                                                       ! has been output.
        logical                    :: skip_wrapup      ! wrapup completion flag
        real                       :: thisgrp          ! group or bin for
                                                       ! current stack

!       the following two arrays are two dimensional only because they may be
!       passed to another cps process.  They each contain a single vector.
        real,pointer               :: sumt(:,:)        ! array to hold current
                                                       ! stack trace
        double precision,pointer   :: hd_sumt(:,:)     ! array to hold current
                                                       ! stack header

        real,pointer               :: suma(:)          ! array to hold current
                                                       ! absolute-value stack
        real,pointer               :: fold(:)          ! array to hold local
                                                       ! fold of stack for each
                                                       ! sample in current stack
        type(trcio_struct),pointer :: file             ! trcio file structure
        logical                    :: gathered         ! global coming in.
      end type stk_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(stk_struct),pointer,save :: object      ! needed for traps.

      integer, parameter           :: num_opt_out = 2
      character(len=5),parameter   :: opt_out (num_opt_out) &
      = (/'TRACE','FILE '/)

      integer,parameter            :: null_grp=-2100000000  !no grp info yet

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine stk_create (obj)
      implicit none
      type(stk_struct),pointer :: obj       ! arguments

      allocate (obj)
                                       ! Nullify pointers in structure.
      nullify  (obj%sumt)              ! array of summed trace values.
      nullify  (obj%hd_sumt)           ! header for stack trace.
      nullify  (obj%suma)              ! array of summed trace absolute values.
      nullify  (obj%fold)              ! array of fold for each sample.
      nullify  (obj%file)              ! trcio file structure

      call stk_initialize (obj)

      return
      end subroutine stk_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine stk_delete (obj)
      implicit none
      type(stk_struct),pointer :: obj       ! arguments
      integer                  :: status    ! local

      if (associated(obj%sumt   ))    deallocate        (obj%sumt)
      if (associated(obj%hd_sumt))    deallocate        (obj%hd_sumt )
      if (associated(obj%suma   ))    deallocate        (obj%suma)
      if (associated(obj%fold   ))    deallocate        (obj%fold)
      if (associated(obj%file)) status = trcio_close (obj%file)
      deallocate(obj)
      return
      end subroutine stk_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine stk_initialize (obj)
      implicit none
      type(stk_struct),pointer :: obj       ! arguments

      obj%stacking_mode = 'NORMAL STACK'
      obj%hdr_flag   = 0
      obj%opt_out    = 'TRACE'
      obj%pathname   = PATHCHECK_EMPTY
      obj%hdr_gath   = 7
      obj%gath_init  = 1.0
      obj%gath_inc   = 1.0
      obj%trim_percent = 0.0
      obj%fse        = 0.5
      obj%mscl       = 0.25
      obj%tvfse      = 0.0
      obj%dt         = 0.0
      obj%tstrt      = 0.0
      obj%nwih       = 0          !initial globals with bogus values
      obj%ndpt       = 0          !so that you can check later when you
      obj%numtr      = -12        !get them to see if you got them

      obj%ntr_out    = 0
      obj%ntr_in     = 0
      obj%ntr_in_use = 0
      obj%ifold      = 0          !This initial value not used.
      obj%maxfold    = 0
      obj%maxufold   = 0
      obj%firstliv   = .true.
      obj%firstime   = .true.
      obj%lastout    = .false.
      obj%thisgrp    = null_grp   !Says no grp info yet
      obj%gathered   = .false.

      call stk_update (obj)

      return
      end subroutine stk_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine stk_update (obj)
      implicit none
      type(stk_struct),target :: obj                           ! arguments


      integer                    :: numtr                       ! local
      logical                    :: need_label                  ! local
      logical                    :: need_request                ! local
      logical                    :: parallel_safe               ! local
      logical                    :: two_set                     ! local
      integer                    :: nscratch                    ! local
      integer                    :: nstore                      ! local
      integer                    :: ier1, ier2, ier3            ! local
      integer                    :: status                      ! local
      logical                    :: fse_mscl_sense              ! local
      character(len=80)          :: gath_info

      object => obj         ! needed for traps.

      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
      call pc_get_global ('GRID',obj%grid)
      call pc_get_global ('TSTRT',obj%tstrt)
      call pc_get_global ('DT', obj%dt)
      if (obj%dt == 0) call pc_error ('STK cannot get global DT.')
      call pc_get_global ('NWIH'  , obj%nwih)
      if (obj%nwih == 0) call pc_error ('STK cannot get global NWIH.')
      call pc_get_global ('NDPT'  , obj%ndpt)
      if (obj%ndpt == 0) call pc_error ('STK cannot get global NDPT.')
      call pc_get_global ('NUMTR' , obj%numtr)
      if (obj%numtr == -12) then
        call pc_error ('STK cannot get global NUMTR.')
      endif
      call pc_get_global ('GATHERED', obj%gathered)

      call pc_get ('stacking_mode', obj%stacking_mode)
      call pc_get ('HDR_FLAG'   , obj%hdr_flag ,stk_hdr_flag_trap)

      if(obj%numtr == 1) then
        obj%opt_input = 'SINGLE'
        gath_info='Input data is single trace'
      else if (obj%gathered) then
        obj%opt_input = 'GATHER'
        gath_info='Input data is gathered'
      else
        gath_info=' '
        call pc_error('STK: Stack requires either single-trace or gathered &
        &input.  Traces are currently grouped but are not in functional &
        &gathers.  Please insert either GATHER or UNGATHER before STK.')
      end if

      if(obj%opt_input == 'GATHER') then
        call pc_put_sensitive_field_flag('HDR_GATH',.false.)
        call pc_put_sensitive_field_flag('GATH_INIT',.false.)
        call pc_put_sensitive_field_flag('GATH_INC',.false.)
      else
        call pc_get ('HDR_GATH'   , obj%hdr_gath ,stk_hdr_gath_trap)
        call pc_get ('GATH_INIT'  , obj%gath_init,stk_gath_init_trap)
        call pc_get ('GATH_INC'   , obj%gath_inc ,stk_gath_inc_trap)
      endif

      call pc_get ('TRIM_PERCENT', obj%trim_percent)
      call pc_get ('FSE'        , obj%fse      ,stk_fse_trap)
      call pc_get ('MSCL'       , obj%mscl     ,stk_mscl_trap)
      call pc_get ('TVFSE'      , obj%tvfse    ,stk_tvfse_trap)

      call stkscale_check(obj%fse,obj%mscl,obj%tvfse,fse_mscl_sense)
      call pc_put_sensitive_field_flag('FSE',fse_mscl_sense)
      call pc_put_sensitive_field_flag('MSCL',fse_mscl_sense)

      call pc_get ('OPT_OUT'    , obj%opt_out  ,stk_opt_out_trap)
      call pc_get ('PATHNAME'   , obj%pathname ,stk_pathname_trap)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      call mth_constrain (obj%trim_percent, 0.0, 100.0)
      if (obj%numtr > 1) then
           call pc_put_sensitive_field_flag ('trim_percent',.true.)
      else if (obj%trim_percent > 0.0) then
           call pc_error &
                  ('TRIM_PERCENT must be zero when traces are not gathered.')
           call pc_put_sensitive_field_flag ('trim_percent',.true.)
      else
           call pc_put_sensitive_field_flag ('trim_percent',.false.)
      end if

      if (obj%stacking_mode /= 'FLATNESS STACK') &
                                      obj%stacking_mode = 'NORMAL STACK'

!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!

      call pc_put_options_field('OPT_OUT',opt_out,num_opt_out)
      call pc_put_options_field('stacking_mode', (/'NORMAL STACK   ', &
                                                   'FLATNESS STACK '/))

      call pc_put ('stacking_mode', obj%stacking_mode)
      call pc_put ('OPT_OUT'    , obj%opt_out)
      call pc_put ('PATHNAME'   , obj%pathname)
      call pc_put ('HDR_FLAG'   , obj%hdr_flag)
      call pc_put ('HDR_GATH'   , obj%hdr_gath)
      call pc_put ('GATH_INIT'  , obj%gath_init)
      call pc_put ('GATH_INC'   , obj%gath_inc)
      call pc_put ('TRIM_PERCENT', obj%trim_percent)
      call pc_put ('FSE'        , obj%fse)
      call pc_put ('MSCL'       , obj%mscl)
      call pc_put ('TVFSE'      , obj%tvfse)
      call pc_put_gui_only('GATH_INFO',gath_info)

      nscratch = obj%nwih + obj%ndpt             ! hd_mute + rfold
      call pc_put_control ('NSCRATCH'   ,nscratch  )

      nstore   = obj%nwih + obj%ndpt + obj%ndpt  ! hd_sumt + sumt + fold
      call pc_put_control ('NSTORE'     ,nstore    )

!                    TRUTH TABLE for CONTROL LOGIC
!
!    +-------+---------+-------------+--------+----------+------------+-----+
!    |opt_out|opt_input|hdr_flag/=0.0|par safe|need_label|need_request|2 set|
!    +-------+---------+-------------+--------+----------+------------+-----+
!    | FILE  |    X    |     X       |   F    |    F     |    F       |  F  |
!    +-------+---------+-------------+--------+----------+------------+-----+
!    | TRACE | SINGLE  |     X       |   F    |    T     |    T       |  F  |
!    +-------+---------+-------------+--------+----------+------------+-----+
!    | TRACE | GATHER  |     T       |   T    |    F     |    T       |  T  |
!    +-------+---------+-------------+--------+----------+------------+-----+
!    | TRACE | GATHER  |     F       |   T    |    F     |    F       |  T  |
!    +-------+---------+-------------+--------+----------+------------+-----+
!    |             X = don't care, T=true, F=false.                         |
!    +-------+---------+-------------+--------+----------+------------+-----+
!

      ! if opt_out == FILE then
      ! we will pass traces through, creating the stacked data on a file.

      need_label    = .false.
      need_request  = .false.
      parallel_safe = .false.
      two_set       = .false.

      if(obj%opt_out == 'TRACE') then
        ! we will be gathering traces to create stacked traces for output to
        ! the rest of the job.
        if(obj%opt_input == 'SINGLE') then
          need_label     = .true.
          need_request   = .true.
        elseif(obj%opt_input == 'GATHER') then
          parallel_safe = .true.
          two_set=.true.
          if( obj%hdr_flag /= 0.0 ) need_request = .true.
        endif
      endif

      numtr = max (obj%numtr,1)
      if (obj%opt_out == 'TRACE') numtr = 1

      call pc_put_global ('NUMTR', numtr)
      call pc_put_control ('NEED_REQUEST', need_request)
      call pc_put_control ('NEED_LABEL'  , need_label)
      call pc_put_control ('TWOSETS'     , two_set)

      if(parallel_safe) then
        call pc_put_control('PARALLEL_SAFE'        ,.true.)
        call pc_put_control('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
        call pc_put_control('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
        call pc_put_control('PCPS_BUNCH_MODE'    ,'PCPS_BUNCH_TRACE_GROUPS')
        call pc_put_control('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
        call pc_put_control('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
        call pc_put_control('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')
        call pc_put_control('PCPS_RESEQUENCE_MODE','PCPS_RESEQUENCE_TRACES')
      endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!!-------------------------- end session trap ------------------------------!!
!!-------------------------- end session trap ------------------------------!!
!!-------------------------- end session trap ------------------------------!!


      if (associated(obj%sumt)) deallocate (obj%sumt)       ! for all pointers.
      if (associated(obj%suma)) deallocate (obj%suma)       ! for all pointers.
      if (associated(obj%fold)) deallocate (obj%fold)       ! for all pointers.
      if (associated(obj%hd_sumt)) deallocate (obj%hd_sumt) ! for all pointers.

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if(pc_get_update_state() .ne. PC_BACKEND) return

      if(obj%opt_out == 'FILE') then

        obj%file => trcio_open(obj%pathname,'w')
        if(.not. associated(obj%file)) then
          call pc_error('STK: cannot open file'//trim(obj%pathname)// &
          ' for write.')
          return
        endif
        obj%file%tmin           = obj%tstrt
        obj%file%dt             = obj%dt
        obj%file%nwih           = obj%nwih
        obj%file%num_values     = obj%ndpt
        obj%file%common%xorigin = grid_get_xorigin(obj%grid)
        obj%file%common%yorigin = grid_get_yorigin(obj%grid)
        obj%file%common%dx11    = grid_get_dx11(obj%grid)
        obj%file%common%dx12    = grid_get_dx12(obj%grid)
        obj%file%common%dx21    = grid_get_dx21(obj%grid)
        obj%file%common%dx22    = grid_get_dx22(obj%grid)
        status = trcio_writeheader(obj%file)
        if(status /= 0 ) call pc_error('STK: error writing TRCIO file header.')
        call trcio_write_history_cards(obj%file,'ALL')
      endif

      allocate(obj%sumt(obj%ndpt,1),stat=ier1)
      if (ier1 /= 0) call pc_error ('STK: error allocating sumt')
      allocate(obj%suma(obj%ndpt),stat=ier1)
      if (ier1 /= 0) call pc_error ('STK: error allocating suma')
      allocate(obj%fold(obj%ndpt),stat=ier2)
      if (ier2 /= 0) call pc_error ('STK: error allocating fold')
      allocate(obj%hd_sumt (obj%nwih,1),stat=ier3)
      if (ier3 /= 0) call pc_error ('STK: error allocating hd_sumt')


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine stk_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

subroutine stk_hdr_flag_trap(hdr_flag)
  character(len=*),intent(in) :: hdr_flag
  if (object%hdr_flag < 0 .or. object%hdr_flag > object%nwih) then
    call pc_error ('STK: HDR_FLAG must be in the range of 0 to NWIH.')
    call pc_jump_field(hdr_flag)
  endif
end subroutine stk_hdr_flag_trap

subroutine stk_fse_trap(fse)
  character(len=*),intent(in) :: fse
  if (object%fse  < 0.0 .or. object%fse  > 1.0) then
    call pc_error ('STK: FSE must be in range 0.0 to 1.0')
    call pc_jump_field(fse)
    return
  endif
  if (object%tvfse /= 0.0 .and. object%fse /= 0.0 ) then
!---    call pc_info    ('STK: Using FSE turns off TVFSE')
    object%tvfse = 0.0
  endif
end subroutine stk_fse_trap

subroutine stk_mscl_trap(mscl)
  character(len=*), intent(in) :: mscl
  if (object%mscl  < 0.0 .or. object%mscl  > 1.0) then
    call pc_error ('STK: MSCL must be in range 0.0 to 1.0')
    call pc_jump_field(mscl)
    return
  endif
  if (object%tvfse /= 0.0 .and. object%mscl /= 0.0 ) then
!---    call pc_info    ('STK: Using MSCL turns off TVFSE')
    object%tvfse = 0.0
  endif
end subroutine stk_mscl_trap

subroutine stk_tvfse_trap(tvfse)
  character(len=*),intent(in) :: tvfse
  if (object%tvfse  < 0.0 .or. object%tvfse  > 1.0) then
    call pc_error ('STK: TVFSE must be in range 0.0 to 1.0')
    call pc_jump_field(tvfse)
    return
  endif
  if (object%tvfse  > 0.0 .and. object%fse /= 0.0 .and. object%mscl /= 0) then
!---    call pc_info    ('STK: Using TVFSE turns off FSE and MSCL')
    object%fse  = 0.0
    object%mscl = 0.0
  elseif (object%tvfse >0.0 ) then
    if (object%fse /= 0.0 ) then
!---      call pc_info    ('STK: Using TVFSE turns off FSE')
      object%fse  = 0.0
    endif
    if (object%mscl /= 0.0 ) then
!---      call pc_info    ('STK: Using TVFSE turns off MSCL')
      object%mscl = 0.0
    endif
  endif
end subroutine stk_tvfse_trap

subroutine stk_hdr_gath_trap(hdr_gath)
  character(len=*) , intent(in) :: hdr_gath
  if (object%hdr_gath < 1 .or. object%hdr_gath > object%nwih) then
    call pc_error ('STK: HDR_GATH must be in the range of 1 to NWIH.')
    call pc_jump_field(hdr_gath)
  endif
end subroutine stk_hdr_gath_trap

subroutine stk_gath_init_trap(gath_init)
  character(len=*) ,intent(in) :: gath_init
end subroutine stk_gath_init_trap

subroutine stk_gath_inc_trap(gath_inc)
  character(len=*),intent(in) :: gath_inc
  if (object%gath_inc  <= 0.0 ) then
    call pc_error ('STK: GATH_INC must be > 0.0 ')
    call pc_jump_field(gath_inc)
  endif
end subroutine stk_gath_inc_trap

subroutine stk_opt_out_trap(opt_out)
  character(len=*),intent(in) :: opt_out
  call string_to_upper(object%opt_out)
  select case(object%opt_out)
    case('FILE')
      call pc_put_sensitive_field_flag('PATHNAME',.true.)
      if (len_trim(object%pathname) == 0) call pc_jump_field('PATHNAME')
    case('TRACE')
      call pc_put_sensitive_field_flag('PATHNAME',.false.)
    case default
      call pc_error ('STK: OPT_OUT must be either TRACE or FILE.')
      call pc_jump_field(opt_out)
  end select
end subroutine stk_opt_out_trap

subroutine stk_pathname_trap(pathname)
  character(len=*),intent(in)    :: pathname
  integer                        :: status

  if(object%opt_out == 'FILE')then
    call pathcheck(pathname,object%pathname,&
                 required=.true.,ext='trc',status=status)
    if(status /= PATH_VALID ) then
      call pc_error('STK: Enter a valid file name.')
      call pc_jump_field(pathname)
    endif
  endif

end subroutine stk_pathname_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine stk (obj,ntr,hd,tr, hd1,tr1 )
      implicit none
      type(stk_struct)               :: obj                    ! arguments
      integer    ,intent(inout)      :: ntr                    ! arguments
      real       ,intent(inout)      :: tr(:,:)                ! arguments
      double precision,intent(inout) :: hd(:,:)                ! arguments

! --- Note hd1, tr1 only needed for gather input mode
      real,            optional,intent(inout) :: tr1(:,:)      ! arguments
      double precision,optional,intent(inout) :: hd1(:,:)      ! arguments

      integer, parameter             :: ikill = 0              ! for mutehw call
      real,    parameter             :: rshft=0.0              ! for mutehw call

      integer                        :: j, k, live, flag_1     ! local
      integer                        :: mute1, mute2           ! local
      real                           :: grp, temp ,factor      ! local
      double precision               :: hd_mute(obj%nwih)      ! for mutehw call
      integer                        :: status, narray         ! local
      logical                        :: live_flag              ! local
      real         ,allocatable      :: array(:)               ! local
      integer      ,allocatable      :: mute1a(:)              ! local
      integer      ,allocatable      :: mute2a(:)              ! local


      if(obj%opt_input .eq. 'GATHER' .and. obj%trim_percent > 0.0) then

!-------GATHER ---- TRIM ---- GATHER ---- TRIM ---- GATHER ---- TRIM
!         |          |          |          |          |          |
!
        if(ntr<=0) then
          if(ntr == NO_MORE_TRACES) then    !we're finished
            call stk_wrapup(obj)
          else if(ntr == NEED_TRACES) then
            ntr=NEED_TRACES
          else
            ntr = FATAL_ERROR
          endif
          return
        endif

!       continue here for ntr>=1
        obj%ntr_in = obj%ntr_in + ntr  !counter for all input traces
        obj%sumt(:,1) = 0.0            !clear stack trace
        obj%suma(:) = 0.0              !clear absolute stack trace
        obj%fold(:) = 0.0              !clear fold of stack array
        obj%ifold = 0                  !nominal fold for stack trace

!       find 1st trace that will be used
        flag_1 = 0
        if (obj%hdr_flag == 0) then    !use 1st trace if not using flags
          flag_1 = 1
        else                           !look for 1st (flagged) trace
          do k = 1,ntr
            if (hd(obj%hdr_flag,k) /= 0.0) then
              flag_1 = k
              exit
            endif
          end do
        endif

        if (flag_1 == 0) then          !no traces to use in this gather
          ntr = NEED_TRACES            !request more traces
          RETURN
        endif

!       stacked trace gets first non-flagged trace header whether or not dead
        obj%hd_sumt(:,1) = hd(:,flag_1)

        allocate (array (ntr))    !!!! for trimmed mean values.
        allocate (mute1a(ntr))    !!!! for trimmed mean values.
        allocate (mute2a(ntr))    !!!! for trimmed mean values.

        mute1a(:) = obj%ndpt + 1
        mute2a(:) = 0

        do k = flag_1,ntr
!         check for non-flagged trace and skip over
          if (obj%hdr_flag /= 0 .and. hd(obj%hdr_flag,k) == 0.0) cycle

          obj%ntr_in_use = obj%ntr_in_use + 1

!         check for dead trace and skip over
          if (hd(hdr_lav,k) == 0.0 ) cycle

!         get mute header words and make sure they are okay
          mute1a(k) = nint(hd( 2,k))    !index of 1st "live" sample
          mute2a(k) = nint(hd(64,k))    !index of last "live" sample
          call mth_constrain (mute1a(k),         1, obj%ndpt)
          call mth_constrain (mute2a(k), mute1a(k), obj%ndpt)

          obj%ifold = obj%ifold + 1  !bump nominal fold

!         update mute header words of stacked trace
          if (obj%ifold == 1) then
            obj%hd_sumt( :,1) = hd(:,k)  !stacked trace gets first live header
            obj%hd_sumt( 2,1) = mute1a(k)
            obj%hd_sumt(64,1) = mute2a(k)
          else
            obj%hd_sumt( 2,1) = min(nint(obj%hd_sumt( 2,1)),mute1a(k))
            obj%hd_sumt(64,1) = max(nint(obj%hd_sumt(64,1)),mute2a(k))
          end if
        end do

        obj%maxufold = max(obj%maxufold,obj%ifold)       ! max untrimmed fold.
        factor       = (1.0 - obj%trim_percent / 100.0)  ! untrimmed fraction.
        obj%ifold    = max(nint(obj%ifold * factor), 1)  ! adjust fold for trim.

        do j = 1,obj%ndpt
          narray = 0
          do k = flag_1,ntr
            if (j < mute1a(k) .or. j > mute2a(k)) cycle
            narray = narray + 1
            array(narray) = tr(j,k)
          end do
          obj%fold(j) = max(narray * factor, 1.0)
          obj%sumt(j,1) = statutil_trimmed_mean (array,narray, &
                                                 obj%trim_percent,obj%suma(j))
          obj%sumt(j,1) = obj%sumt(j,1) * obj%fold(j)
          obj%suma(j)   = obj%suma(j)   * obj%fold(j)
        end do

        deallocate (array)       !!!! for trimmed mean values.
        deallocate (mute1a)      !!!! for trimmed mean values.
        deallocate (mute2a)      !!!! for trimmed mean values.

!       all (flagged) traces for this gather are in stack
        call stk_finish_stack(obj)
!       pass stacked trace to next process or put to a file

        if (obj%opt_out == 'FILE') then
          status = trcio_write_trace(obj%file,obj%hd_sumt(:,1),obj%sumt(:,1))
          if(status /= 0 ) then
            call pc_error('STK: Error writing a trace.')
            return
          endif

        else  !copy stack trace and header to input arrays for output
          if(present(hd1) .and. present(tr1)) then
            hd1(:,1) = obj%hd_sumt(:,1)     !note have set hd/tr
            tr1(:,1) = obj%sumt(:,1)
          else
            hd(:,1) = obj%hd_sumt(:,1)     !note have set hd/tr
            tr(:,1) = obj%sumt(:,1)
          endif
          ntr = 1
        endif

        RETURN
!
!         ^          ^          ^          ^          ^          ^
!         |          |          |          |          |          |
!-------GATHER ---- TRIM ---- GATHER ---- TRIM ---- GATHER ---- TRIM

      else if(obj%opt_input .eq. 'GATHER') then

!-------GATHER --- GATHER --- GATHER --- GATHER --- GATHER --- GATHER
!         |          |          |          |          |          |
!
        if(ntr<=0) then
          if(ntr == NO_MORE_TRACES) then    !we're finished
            call stk_wrapup(obj)
          else if(ntr == NEED_TRACES) then
            ntr=NEED_TRACES
          else
            ntr = FATAL_ERROR
          endif
          return
        endif

!       continue here for ntr>=1
        obj%firstliv = .true.          !looking for 1st live trace
        obj%ntr_in = obj%ntr_in + ntr  !counter for all input traces
        obj%sumt(:,1) = 0.0            !clear stack trace
        obj%suma(:) = 0.0              !clear absolute stack trace
        obj%fold(:) = 0.0              !clear fold of stack array
        obj%ifold = 0                  !nominal fold for stack trace

!       find 1st trace that will be used
        flag_1 = 0
        if (obj%hdr_flag == 0) then    !use 1st trace if not using flags
          flag_1 = 1
        else                           !look for 1st (flagged) trace
          do k = 1,ntr
            if (hd(obj%hdr_flag,k) /= 0.0) then
              flag_1 = k
              exit
            endif
          end do
        endif

        if (flag_1 == 0) then          !no traces to use in this gather
          ntr = NEED_TRACES            !request more traces
          RETURN
        endif

!       put 1st (flagged) trace into stack (data between mute indeces only)
!       make sure that mute header words are okay first
!       use a copy of the trace headers so that input headers are not altered
!       (in case we are passing intput traces to next process)

        obj%ntr_in_use = obj%ntr_in_use + 1 !traces used counter
        hd_mute(:) = hd(:,flag_1)
        call mutehw(hd_mute,tr(:,flag_1),obj%ndpt,rshft,ikill)
        mute1 = nint(hd_mute(2))   !index of 1st "live" sample
        mute2 = nint(hd_mute(64))  !index of last "live" sample

        !--- If trace is alive, lav is > 0.0, and live_flag is set.
        live_flag = hd_mute(hdr_lav) > 0.0

!       put header for 1st (flagged) trace into stack header
!       (whether or not trace may be dead)

        obj%hd_sumt(:,1) = hd_mute(:)
        if (live_flag ) then
                                          !at least one "live" sample
                                          !look for nonzero values
                                          !otherwise, stack already set to zero
          do j = mute1, mute2
!---            if (tr(j,flag_1) == 0.0) cycle
            obj%fold(j)   = 1.0
            obj%sumt(j,1) = tr(j,flag_1)
            obj%suma(j)   = abs(tr(j,flag_1))
            obj%ifold     = 1               !nominal fold of stack
          end do
        endif

!       if we have at least one nonzero value, we have found 1st live trace
        if (obj%ifold .eq. 1) obj%firstliv = .false.

!       now look at rest of gather

        do k = (flag_1 + 1),ntr
!         check for non-flagged trace and skip over
          if (obj%hdr_flag /= 0 .and. hd(obj%hdr_flag,k) == 0.0) cycle

          obj%ntr_in_use = obj%ntr_in_use + 1
          live = 0
!         put next (flagged) trace into stack (data between mute indeces only)
!         make sure that mute header words are okay first
!         use a copy of the trace headers so that input headers are not altered
!         (in case we are passing intput traces to next process)
          hd_mute(:) = hd(:,k)
          call mutehw(hd_mute,tr(:,k),obj%ndpt,rshft,ikill)
          mute1 = nint(hd_mute(2))     !index of 1st "live" sample
          mute2 = nint(hd_mute(64))    !index of last "live" sample

          !--- If trace is alive, lav is > 0.0, and live_flag is set.
          live_flag = hd_mute(hdr_lav) > 0.0
          if (.not. live_flag ) cycle  !trace is dead - skip it

!         look for nonzero values
!         update stack and local fold

          do j = mute1, mute2
!---            if (tr(j,k) == 0.0) cycle
            obj%fold(j)   = obj%fold(j) + 1.0
            obj%sumt(j,1) = obj%sumt(j,1) + tr(j,k)
            obj%suma(j)   = obj%suma(j)   + abs(tr(j,k))
            live          = 1
          end do

          if (live .eq. 1) then
            obj%ifold = obj%ifold + 1  !bump nominal fold
            if (obj%firstliv) then     !copy headers to stack.
              obj%hd_sumt(:,1) = hd_mute(:)
              obj%firstliv = .false.
            else                       !update mutes in stack.
              obj%hd_sumt(2,1) = min(obj%hd_sumt(2,1),hd_mute(2))
              obj%hd_sumt(64,1) = max(obj%hd_sumt(64,1),hd_mute(64))
            endif
          endif

        end do

!       all (flagged) traces for this gather are in stack
        call stk_finish_stack(obj)
!       pass stacked trace to next process or put to a file

        if (obj%opt_out == 'FILE') then
          status = trcio_write_trace(obj%file,obj%hd_sumt(:,1),obj%sumt(:,1))
          if(status /= 0 ) then
            call pc_error('STK: Error writing a trace.')
            return
          endif

        else  !copy stack trace and header to input arrays for output
          if(present(hd1) .and. present(tr1)) then
            hd1(:,1) = obj%hd_sumt(:,1)     !note have set hd/tr
            tr1(:,1) = obj%sumt(:,1)
          else
            hd(:,1) = obj%hd_sumt(:,1)     !note have set hd/tr
            tr(:,1) = obj%sumt(:,1)
          endif
          ntr = 1
        endif

        RETURN
!
!         ^          ^          ^          ^          ^          ^
!         |          |          |          |          |          |
!-------GATHER --- GATHER --- GATHER --- GATHER --- GATHER --- GATHER

      else

!-------SINGLE --- SINGLE --- SINGLE --- SINGLE --- SINGLE --- SINGLE
!         |          |          |          |          |          |

        if (ntr > 1) then  !shouldn't happen
          call pc_error ('STK encountered ntr>1 for single trace mode.')
          call stk_wrapup(obj)
          ntr = FATAL_ERROR
          RETURN

        else if (ntr == 1) then      !check to see if we use this trace
          obj%ntr_in = obj%ntr_in + 1        !input trace count
          if (obj%hdr_flag /=0 .and. hd(obj%hdr_flag,1) == 0.0) then
!           this trace not flagged, either discard it or pass it on
!
            if (obj%opt_out == 'TRACE') ntr = NEED_TRACES
            RETURN
          endif
        endif

!       Check to see if we have been "called from below" - i.e. that this
!       process has a label and last exited down.
!       This won't happen for Opt_Out=FILE - no label in that case.

        if (ntr == NEED_TRACES) then     !called from "below"
          if(obj%lastout) then           !last stack trace already passed out
            call stk_wrapup(obj)         !we're finished
            ntr = NO_MORE_TRACES
            RETURN

          else if(obj%thisgrp.eq.null_grp) then   !retrieve saved trace
            tr(:,1) = obj%sumt(:,1)
            hd(:,1) = obj%hd_sumt(:,1)
            ntr = 1                      !NOTICE ** setting ntr=1 **
            obj%firstime = .true.

          else
            RETURN                       !returning ntr=NEED_TRACES
          endif
        endif

  777   CONTINUE  !This is where we branch back to "re-input" the current
                  !input trace since it was in a different group
                  !This only happens for OPT_OUT=FILE

        if (ntr == 1 .and. obj%firstime) then  !start a new stack trace
          obj%thisgrp = nint((hd(obj%hdr_gath,1) - obj%gath_init)   &
                        / obj%gath_inc)  !calculate new gather number
          obj%firstliv = .true.          !looking for 1st live trace
          obj%sumt(:,1) = 0.0            !clear stack trace
          obj%suma(:) = 0.0              !clear absolute stack trace
          obj%fold(:)   = 0.0            !clear fold of stack array
          obj%ifold = 0                  !nominal fold for stack trace
          obj%ntr_in_use = obj%ntr_in_use + 1

!         put 1st (flagged) trace into stack (data between mute indeces only)
!         make sure that mute header words are okay first
!         use a copy of the trace headers so that input headers are not altered
!         (in case we are passing intput traces to next process)

          hd_mute(:) = hd(:,1)
          call mutehw(hd_mute,tr(:,1),obj%ndpt,rshft,ikill)
          mute1 = nint(hd_mute(2))       !index of 1st "live" sample
          mute2 = nint(hd_mute(64))      !index of last "live" sample

          !--- If trace is alive, lav is > 0.0, and live_flag is set.
          live_flag = hd_mute(hdr_lav) > 0.0

!         put header for 1st (flagged) trace into stack header
!         (whether or not trace may be dead)

          obj%hd_sumt(:,1) = hd_mute(:)

          if (live_flag        ) then    !at least one "live" sample
                                         !look for nonzero values
                                         !otherwise, stack already set to zero
            do j = mute1, mute2
!---              if (tr(j,1) == 0.0) cycle
              obj%fold(j)   = 1.0
              obj%sumt(j,1) = tr(j,1)
              obj%suma(j)   = abs(tr(j,1))
              obj%ifold     = 1          !nominal fold of stack
            end do
          endif

!         if we have at least one nonzero value, we have found 1st live trace
          if (obj%ifold .eq. 1) obj%firstliv = .false.
          obj%firstime = .false.
          if (obj%opt_out == 'TRACE') ntr = NEED_TRACES
          RETURN
        endif

        if (ntr == 1)  &         !calc. group number for this input trace
          grp = nint((hd(obj%hdr_gath,1) - obj%gath_init) / obj%gath_inc)

        if(ntr == 1 .and. grp == obj%thisgrp) then  !add to stack
          obj%ntr_in_use = obj%ntr_in_use + 1
          live = 0

!         put next (flagged) trace into stack (data between mute indices only)
!         make sure that mute header words are okay first
!         use a copy of the trace headers so that input headers are not altered
!         (in case we are passing intput traces to next process)

          hd_mute(:) = hd(:,1)
          call mutehw(hd_mute,tr(:,1),obj%ndpt,rshft,ikill)
          mute1 = nint(hd_mute(2))     !index of 1st "live" sample
          mute2 = nint(hd_mute(64))    !index of last "live" sample

          !--- If trace is alive, lav is > 0.0, and live_flag is set.
          live_flag = hd_mute(hdr_lav) > 0.0

          if (.not. live_flag ) then  !trace is dead - skip it
            if (obj%opt_out == 'TRACE') ntr = NEED_TRACES
            RETURN
          endif

!         look for nonzero values
!         update stack and local fold

          do j = mute1, mute2
!---            if (tr(j,1) == 0.0) cycle
            obj%fold(j)   = obj%fold(j) + 1.0
            obj%sumt(j,1) = obj%sumt(j,1) + tr(j,1)
            obj%suma(j)   = obj%suma(j)   + abs(tr(j,1))
            live          = 1
          end do

          if (live .eq. 1) then        !we added at least one value to stack
            obj%ifold = obj%ifold + 1  !bump nominal fold
            if (obj%firstliv) then     !copy headers to stack.
              obj%hd_sumt(:,1) = hd_mute(:)
              obj%firstliv = .false.
            else                       !update mutes in stack.
              obj%hd_sumt(2,1) = min(obj%hd_sumt(2,1),hd_mute(2))
              obj%hd_sumt(64,1) = max(obj%hd_sumt(64,1),hd_mute(64))
            endif
          endif

          if (obj%opt_out == 'TRACE') ntr = NEED_TRACES
          RETURN
        else      ! end of group or end of traces (ntr==NO_MORE_TRACES)

          if(ntr == NO_MORE_TRACES) obj%lastout = .true.
!         all (flagged) traces for this gather are in stack
          call stk_finish_stack(obj)

!         stack trace is ready to go "out" (to CPS stream or file)
          if (obj%opt_out == 'TRACE') then
!           swap stack with input

            do j = 1,obj%ndpt
              temp = obj%sumt(j,1)
              obj%sumt(j,1) = tr(j,1)
              tr(j,1) = temp
            end do

            do j = 1,obj%nwih
              temp = obj%hd_sumt(j,1)
              obj%hd_sumt(j,1) = hd(j,1)
              hd(j,1) = temp
            end do

            obj%firstliv = .true.!get ready to build next stack trace
            ntr = 1              !pass current stack to next process
            obj%thisgrp=null_grp !tell to retrieve saved trace on NEED_TRACES

          else  !opt_out = 'FILE'

            status = &
              trcio_write_trace(obj%file,obj%hd_sumt(:,1),obj%sumt(:,1))
            if(status /= 0 ) then
              call pc_error('STK: Error writing a trace.')
              return
            endif

            if (ntr == NO_MORE_TRACES) then  !we're finished
              call stk_wrapup(obj)
            else                             !we have an input trace pending
              obj%firstime = .true.          !which belongs to the next group
              GO TO 777
            endif
          endif

          RETURN
        endif
      endif
!         ^          ^          ^          ^          ^          ^
!         |          |          |          |          |          |
!-------SINGLE --- SINGLE --- SINGLE --- SINGLE --- SINGLE --- SINGLE

      RETURN
      end subroutine stk


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine stk_wrapup (obj)
      implicit none
      type(stk_struct) :: obj       ! arguments
      integer          :: status    ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

!     close STROT file if we have one.
      if (obj%opt_out == 'FILE') then
        if(associated(obj%file) ) status = trcio_close(obj%file)
      endif

! write to report file
      if (obj%trim_percent == 0.0) obj%maxufold = obj%maxfold
      call pc_print ('STK completed.')
      call pc_print ('STK:',obj%ntr_in,'traces input.')
      call pc_print ('STK:',obj%ntr_in_use,'traces used.')
      call pc_print ('STK:',obj%ntr_out,'stacked traces created.')
      call pc_print ('STK: Maximum  Trimmed  Fold of stack =',obj%maxfold)
      call pc_print ('STK: Maximum Untrimmed Fold of stack =',obj%maxufold)
      if (obj%opt_out == 'FILE') then
        call pc_print ('STK: Stacked traces saved in file',obj%pathname)
        status=cio_chmod(trim(obj%pathname),'rw-r--r--')
        if(status  /= cio_ok ) then
          call pc_info('STK: Unable to set file permissions on file: ' &
            //trim(obj%pathname)//'.')
        endif
      endif

      return
      end subroutine stk_wrapup


!!----------------------- other private subroutines ------------------------!!
!!----------------------- other private subroutines ------------------------!!
!!----------------------- other private subroutines ------------------------!!


  subroutine stk_finish_stack(obj)
    ! This routine is called when a stacked trace has been accumulated
    ! and is ready to be scaled for output.
    implicit none
    type(stk_struct)     :: obj                    ! arguments
    integer              :: j                      ! local

    if(obj%stacking_mode == 'FLATNESS STACK') then
      do j = 1, obj%ndpt
        obj%sumt(j,1) = obj%suma(j) - abs(obj%sumt(j,1))
      end do
    end if

    call stkscale(obj%hd_sumt(:,1),          &
                  obj%sumt(:,1),             &
                  obj%fold,                  &
                  obj%ndpt,                  &
                  obj%fse,obj%mscl,obj%tvfse,&
                  obj%ifold)
    call lav_set_hdr(obj%hd_sumt(:,1),obj%sumt(:,1),obj%ndpt)
    obj%maxfold = max(obj%maxfold,obj%ifold) !--- for report file(maxfold)
    obj%ntr_out = obj%ntr_out + 1            !--- stack trace counter(ntrc)
    obj%hd_sumt(1,1) = obj%ntr_out      !--- sequential trace number in output
  end subroutine stk_finish_stack


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module stk_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

