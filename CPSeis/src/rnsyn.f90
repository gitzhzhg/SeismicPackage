!<CPS_v1 type="PROCESS"/>
!!-------------------------- rnsyn.f90 -------------------------------------!!
!!-------------------------- rnsyn.f90 -------------------------------------!!
!!-------------------------- rnsyn.f90 -------------------------------------!!
 
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
! Name       : RNSYN
! Category   : Synthetics
! Written    : 1988-09-26   by: Bob Baumel
! Revised    : 2000-12-08   by: Tom Stoeckley
! Maturity   : production   2001-04-30
! Purpose    : Generate random number traces for testing.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  RNSYN can generate large numbers of traces very rapidly by creating only 
!  ONE such trace at setup time and then passing out multiple copies of that 
!  trace (with only headers varying).  Alternatively, it can put different 
!  random values in each trace.  It can also pass out only dead traces, assuming
!  that values will be filled in later.
!
!  The random numbers generated in this process module are the same on all
!  platforms.
!
!-------------------------------------------------------------------------------
!</descript_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  Trace sample values are Gaussian distributed pseudo-random values with mean 
!  zero and standard deviation SDEV.
!
!  OPT_TR=SAME is very fast because only ONE random trace is generated at setup
!  time; this trace is then passed out over and over, with only the headers
!  varying.  OPT_TR=DIFF is slower, but should be used if you need different 
!  random values in each trace.
!
!  With OPT_TR=DEAD, the traces contain only zero values; however, the mute 
!  header words (2 and 64) are NOT set as for a dead trace (instead they 
!  contain 1 and NDPT).  Thus, it is assumed that you will somehow supply live 
!  values to these traces.
!
!  Traces are passed out with the "Line" header word varying least rapidly and
!  the "Offset" header word varying most rapidly.  The "CMP" header can be
!  thought of as actual CMP if you wish to think of your synthetics as CMP
!  gathers, or as shotpoint if you wish to regard them as shot profiles.
!
!  RNSYN passes out its traces one at a time.
!
!-------------------------------------------------------------------------------
!</advice_doc>

 
!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! NO input traces -- RNSYN is a trace supplying process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

 
!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs one trace at a time.
! This is a trace-supplying process.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

 
!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Set to 1
! GATHERED  whether traces are gathered          Set to .false.
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>
 
 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Set
! 2       Head mute                  Set to 1
! 3       Current gather number      Set
! 4       No. within current gather  Set
! 25      Largest absolute value     Set
! 64      Tail mute                  Set to NDPT
!         HDR_LINE (default 8)       Set
!         HDR_CMP  (default 7)       Set
!         HDR_OFF  (default 6)       Set
! All other headers set to zero.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
! 13.  2001-04-30  Stoeckley  Change wrapup flag.
! 12.  2000-11-15  Stoeckley  Add missing required documentation sections.
! 11.  2000-04-07  Stoeckley  Removed random number code to the MTH primitive;
!                              add GUI definition section.
! 10.  2000-03-10  Stoeckley  Added code to set LAV header word; removed
!                              unnecessary code; made random number
!                              calculations more robust (per Bob Baumel).
!  9.  2000-01-17  Vunderink  Removed extra comments at beginning of file and
!                              added RCS "Id" string
!  8.  1999-10-26  Baumel     Fix trapping & defaults for lines, cmps, offsets.
!  7.  1999-09-13  Vunderink  Made parameter cache named constants changes
!  6.  1999-06-24  Vunderink  Converted from old CPS system.
!  5.  1998-11-03  Goodger    Begin using fortran90 compiler.             
!  4.  1994-04-05  Baumel     Bug fix: Make sure last trace value is set.
!  3.  1994-02-14  Baumel     Change random values from uniform to Gaussian;
!                              add OPT and SDEV parameters.
!  2.  1992-04-06  Troutt     Set tail mute (hw64) to NDPT.
!  1.  1988-09-26  Baumel     Original Version
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE      very small   amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR ignored on input because this is a trace supplying process.
!
! Upon output, NTR will have one of these values:
!    NTR = 1               if this process is outputting traces.
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
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS RNSYN Process/NC=80>
!               RaNdom SYNthetic trace generator
!
!               OPT_TR=`CCCCCCC     SDEV=`FFFFFF
!
! HDR_LINE= `IIIIII
! LINE_INIT=`FFFFFF LINE_INC=`FFFFFF LINE_LAST=`FFFFFF LINE_TOT=`IIIIII
!
! HDR_CMP=~~`IIIIII
! CMP_INIT= `FFFFFF CMP_INC= `FFFFFF CMP_LAST= `FFFFFF CMP_TOT= `IIIIII
!
! HDR_OFF=~~`IIIIII
! OFF_INIT= `FFFFFF OFF_INC= `FFFFFF OFF_LAST= `FFFFFF OFF_TOT= `IIIIII
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_TR">
!<Tip> Option for samples of generated traces. </Tip>
! Default = SAME   
! Allowed = SAME  -> All traces have identical samples (headers vary).
! Allowed = DIFF  -> Each trace is different.
! Allowed = DEAD  -> All traces are dead.
!
! Choice of OPT_TR value determines whether all traces have identical samples
! (SAME), each trace has different samples (DIFF), or all traces are dead 
! (DEAD).  OPT_TR choice does not affect trace headers.
!</Help>
!
!<Help KEYWORD="SDEV">
!<Tip> Standard deviation of the random trace values. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="HDR_LINE">
!<Tip> Header word designating lines. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="LINE_INIT">
!<Tip> Initial value for line header word. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="LINE_INC">
!<Tip> Increment for line header word. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="LINE_LAST">
!<Tip> Last value of line header word value. </Tip>
! Default = 0.0
! Allowed = real>=LINE_INIT
!</Help>
!
!<Help KEYWORD="LINE_TOT">
!<Tip> Total number of line header word values. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="HDR_CMP">
!<Tip> Header word designating CMPs. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="CMP_INIT">
!<Tip> Initial value for CMP header word. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="CMP_INC">
!<Tip> Increment for CMP header word. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="CMP_LAST">
!<Tip> Last value of CMP header word value. </Tip>
! Default = 0.0
! Allowed = real>=CMP_INIT
!</Help>
!
!<Help KEYWORD="CMP_TOT">
!<Tip> Total number of CMP header word values. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word designating offsets. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Initial value for offset header word. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment for offset header word. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Last value of offset header word value. </Tip>
! Default = 0.0
! Allowed = real>=OFF_INIT
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offset header word values. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module rnsyn_module
      use pc_module
      use named_constants_module
      use string_module
      use lav_module
      use mth_module
      implicit none
      private
      public :: rnsyn_create     ! uses the parameter cache.
      public :: rnsyn_initialize ! uses the parameter cache.
      public :: rnsyn_update     ! uses the parameter cache.
      public :: rnsyn_delete
!<execute_only>
      public :: rnsyn            ! main execution (trace processing) routine.
      public :: rnsyn_wrapup
!</execute_only>

      character(len=100),public,save :: rnsyn_ident = &
       '$Id: rnsyn.f90,v 1.13 2001/04/27 20:46:53 sps prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: rnsyn_struct              
 
      private
      logical           :: skip_wrapup    !wrapup flag.
      character(len=8)  :: opt_tr         !process parameter
      real              :: sdev           !process parameter
      integer           :: hdr_line       !process parameter
      real              :: line_init      !process parameter
      real              :: line_inc       !process parameter
      real              :: line_last      !process parameter
      integer           :: line_tot       !process parameter
      integer           :: hdr_cmp        !process parameter
      real              :: cmp_init       !process parameter
      real              :: cmp_inc        !process parameter
      real              :: cmp_last       !process parameter
      integer           :: cmp_tot        !process parameter
      integer           :: hdr_off        !process parameter
      real              :: off_init       !process parameter
      real              :: off_inc        !process parameter
      real              :: off_last       !process parameter
      integer           :: off_tot        !process parameter
 
      integer           :: ndpt           !global
      integer           :: nwih           !global
 
      integer           :: nopt_tr_menu   !option menu
      character(len=4)  :: opt_tr_menu(3) !option menu

      real,pointer      :: trstore(:)     !dependent variable
      integer           :: ilin           !dependent variable
      integer           :: ibas           !dependent variable
      integer           :: ioff           !dependent variable
      integer           :: ngrp           !dependent variable
      integer           :: ntot           !dependent variable
      real              :: lav            !dependent variable

      end type rnsyn_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(rnsyn_struct),pointer,save :: object      ! needed for traps.

      logical,private,parameter :: homegrown = .true.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine rnsyn_create (obj)
      implicit none
      type(rnsyn_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%trstore)

      call rnsyn_initialize (obj)
      return
      end subroutine rnsyn_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine rnsyn_delete (obj)
      implicit none
      type(rnsyn_struct),pointer :: obj       ! arguments

      if (associated(obj%trstore)) deallocate (obj%trstore)

      deallocate(obj)
      return
      end subroutine rnsyn_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine rnsyn_initialize (obj)
      implicit none
      type(rnsyn_struct),pointer :: obj       ! arguments


      obj%opt_tr         = 'SAME'
      obj%sdev           = 1.0
      obj%hdr_line       = 8
      obj%line_init      = 0.0
      obj%line_inc       = 1.0
      obj%line_last      = 0.0
      obj%line_tot       = 1
      obj%hdr_cmp        = 7
      obj%cmp_init       = 0.0
      obj%cmp_inc        = 1.0
      obj%cmp_last       = 0.0
      obj%cmp_tot        = 1
      obj%hdr_off        = 6
      obj%off_init       = 0.0
      obj%off_inc        = 1.0
      obj%off_last       = 0.0
      obj%off_tot        = 1

      obj%ndpt           = inil
      obj%nwih           = inil

      obj%nopt_tr_menu   = 3
      obj%opt_tr_menu(1) = 'SAME'
      obj%opt_tr_menu(2) = 'DIFF'
      obj%opt_tr_menu(3) = 'DEAD'

      obj%ilin           = 1
      obj%ibas           = 1
      obj%ioff           = 0
      obj%ngrp           = 1
      obj%ntot           = 0
      obj%lav            = 0.0

      call rnsyn_update (obj)
      return
      end subroutine rnsyn_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine rnsyn_update (obj)
      implicit none
      type(rnsyn_struct),target :: obj                          ! arguments

      integer                    :: nstore                      ! local
      character(len=8)           :: opt_tr_loc                  ! local
      real                       :: sdev_loc                    ! local
      real                       :: line_last_loc               ! local
      real                       :: cmp_last_loc                ! local
      real                       :: off_last_loc                ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!

      opt_tr_loc    = obj%opt_tr
      sdev_loc      = obj%sdev
      line_last_loc = obj%line_last
      cmp_last_loc  = obj%cmp_last
      off_last_loc  = obj%off_last

      call pc_get_global ('NDPT', obj%ndpt)
      call pc_get_global ('NWIH', obj%nwih)
 
      call pc_get ('OPT_TR'    ,obj%opt_tr    )
      call pc_get ('SDEV'      ,obj%sdev      )
      call pc_get ('HDR_LINE'  ,obj%hdr_line  ,rnsyn_hdr_trap   )
      call pc_get ('LINE_INIT' ,obj%line_init )
      call pc_get ('LINE_INC'  ,obj%line_inc  )
      call pc_get ('LINE_LAST' ,obj%line_last )
      call pc_get ('LINE_TOT'  ,obj%line_tot  )
      call pc_get ('HDR_CMP'   ,obj%hdr_cmp   ,rnsyn_hdr_trap   )
      call pc_get ('CMP_INIT'  ,obj%cmp_init  )
      call pc_get ('CMP_INC'   ,obj%cmp_inc   )
      call pc_get ('CMP_LAST'  ,obj%cmp_last  )
      call pc_get ('CMP_TOT'   ,obj%cmp_tot   )
      call pc_get ('HDR_OFF'   ,obj%hdr_off   ,rnsyn_hdr_trap   )
      call pc_get ('OFF_INIT'  ,obj%off_init  )
      call pc_get ('OFF_INC'   ,obj%off_inc   )
      call pc_get ('OFF_LAST'  ,obj%off_last  )
      call pc_get ('OFF_TOT'   ,obj%off_tot   )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%ndpt == inil) call pc_error ("NDPT global hasn't been set.")
      if (obj%nwih == inil) call pc_error ("NWIH global hasn't been set.")

!  Verify proper value for OPT_TR and possibly reset SDEV
      call string_to_upper (obj%opt_tr)
      if (obj%opt_tr(1:2) == 'DE') then
        obj%opt_tr = 'DEAD'
        if (obj%opt_tr /= opt_tr_loc) obj%sdev   = 0.0
      elseif (obj%opt_tr(1:2) == 'DI') then
        obj%opt_tr = 'DIFF'
        if (obj%opt_tr /= opt_tr_loc .and. obj%sdev == 0.0) obj%sdev = 1.0
      elseif (obj%opt_tr(1:1) == 'S') then
        obj%opt_tr = 'SAME'
        if (obj%opt_tr /= opt_tr_loc .and. obj%sdev == 0.0) obj%sdev = 1.0
      else
        call pc_error('OPT_TR must be SAME, DIFF, or DEAD.')
      end if

!  Verify proper value for SDEV and possibly reset OPT_TR
      obj%sdev = abs(obj%sdev)
      if (obj%sdev /= sdev_loc) then
        if (obj%sdev == 0.0) then
          obj%opt_tr = 'DEAD'
        elseif (obj%opt_tr == 'DEAD') then
          obj%opt_tr = 'SAME'
          call pc_info('OPT_TR changed to SAME')
        end if
      end if

!  Verify LINE parameters
      if (obj%line_last /= line_last_loc) then
        if (obj%line_inc /= 0.0) then
          obj%line_tot =   &
           max (nint((obj%line_last - obj%line_init)/obj%line_inc) + 1, 1)
          obj%line_last = obj%line_init + (obj%line_tot - 1) * obj%line_inc
        else
          call pc_error('LINE_INC must be non-zero to compute LINE_TOT.')
        end if
      else
        obj%line_last = obj%line_init + (obj%line_tot - 1) * obj%line_inc
      end if

!  Verify CMP parameters
      if (obj%cmp_last /= cmp_last_loc) then
        if (obj%cmp_inc /= 0.0) then
          obj%cmp_tot =   &
           max (nint((obj%cmp_last - obj%cmp_init)/obj%cmp_inc) + 1, 1)
          obj%cmp_last = obj%cmp_init + (obj%cmp_tot - 1) * obj%cmp_inc
        else
          call pc_error('CMP_INC must be non-zero to compute CMP_TOT.')
        end if
      else
        obj%cmp_last = obj%cmp_init + (obj%cmp_tot - 1) * obj%cmp_inc
      end if

!  Verify OFFSET parameters
      if (obj%off_last /= off_last_loc) then
        if (obj%off_inc /= 0.0) then
          obj%off_tot =   &
           max (nint((obj%off_last - obj%off_init)/obj%off_inc) + 1, 1)
          obj%off_last = obj%off_init + (obj%off_tot - 1) * obj%off_inc
        else
          call pc_error('OFF_INC must be non-zero to compute OFF_TOT.')
        end if
      else
        obj%off_last = obj%off_init + (obj%off_tot - 1) * obj%off_inc
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put_options_field ('OPT_TR' ,obj%opt_tr_menu ,obj%nopt_tr_menu)

      call pc_put ('OPT_TR'    ,obj%opt_tr   )
      call pc_put ('SDEV'      ,obj%sdev     )
      call pc_put ('HDR_LINE'  ,obj%hdr_line )
      call pc_put ('LINE_INIT' ,obj%line_init)
      call pc_put ('LINE_INC'  ,obj%line_inc )
      call pc_put ('LINE_LAST' ,obj%line_last)
      call pc_put ('LINE_TOT'  ,obj%line_tot )
      call pc_put ('HDR_CMP'   ,obj%hdr_cmp  )
      call pc_put ('CMP_INIT'  ,obj%cmp_init )
      call pc_put ('CMP_INC'   ,obj%cmp_inc  )
      call pc_put ('CMP_LAST'  ,obj%cmp_last )
      call pc_put ('CMP_TOT'   ,obj%cmp_tot  )
      call pc_put ('HDR_OFF'   ,obj%hdr_off  )
      call pc_put ('OFF_INIT'  ,obj%off_init )
      call pc_put ('OFF_INC'   ,obj%off_inc  )
      call pc_put ('OFF_LAST'  ,obj%off_last )
      call pc_put ('OFF_TOT'   ,obj%off_tot  )
 
      if (obj%opt_tr == 'SAME') then
         nstore  = obj%ndpt
      else
         nstore  = 0
      endif
 
      call pc_put_global  ('NUMTR'      , 1         )
      call pc_put_global  ('GATHERED'   , .false.   )

      call pc_put_control ('NEED_LABEL' , .true.    )
      call pc_put_control ('NSCRATCH'   , 0         )
      call pc_put_control ('NSTORE'     , nstore    )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call mth_ranset (1,homegrown)         ! initialize random number seed.
      if (obj%opt_tr == 'SAME') then
         allocate(obj%trstore(obj%ndpt))
         call rnsyn_rand (obj%trstore, obj%ndpt, obj%sdev)
         obj%lav = lav(obj%trstore, obj%ndpt)
      endif

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine rnsyn_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine rnsyn_hdr_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      select case (keyword)
        case ('HDR_LINE')
          if (object%hdr_line < 1 .or. object%hdr_line > object%nwih) then
            call pc_error ('HDR_LINE must be between 1 and',object%nwih)
          endif

        case ('HDR_CMP')
          if (object%hdr_cmp < 1 .or. object%hdr_cmp > object%nwih) then
            call pc_error ('HDR_CMP must be between 1 and',object%nwih)
          endif

        case ('HDR_OFF')
          if (object%hdr_off < 1 .or. object%hdr_off > object%nwih) then
            call pc_error ('HDR_OFF must be between 1 and',object%nwih)
          endif
      end select
      return
      end subroutine rnsyn_hdr_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine rnsyn (obj,ntr,hd,tr)
      implicit none
      type(rnsyn_struct)                 :: obj                    ! arguments
      integer         ,intent(inout)     :: ntr                    ! arguments
      double precision,intent(inout)     :: hd(:,:)                ! arguments
      real            ,intent(inout)     :: tr(:,:)                ! arguments
 
      if (obj%ioff < obj%off_tot) then
         obj%ioff = obj%ioff + 1
      else
         if (obj%ibas < obj%cmp_tot) then
            obj%ibas = obj%ibas + 1
         else
            if (obj%ilin < obj%line_tot) then
               obj%ilin = obj%ilin + 1
               obj%ibas = 1
            else
               ntr = NO_MORE_TRACES
               return              ! return here if no more traces to pass out
            endif
         endif
         obj%ngrp = obj%ngrp + 1
         obj%ioff = 1
      endif

      if (obj%opt_tr == 'SAME') then
         tr(1:obj%ndpt,1) = obj%trstore
      elseif (obj%opt_tr == 'DIFF') then
         call rnsyn_rand (tr(1:,1), obj%ndpt, obj%sdev)
         obj%lav = lav(tr(1:,1), obj%ndpt)
      else
         tr(1:obj%ndpt,1) = 0.0
      endif

      obj%ntot           = obj%ntot + 1
      hd(1:obj%nwih,1)   = 0.0
      hd(1,1)            = obj%ntot
      hd(2,1)            = 1.0
      hd(3,1)            = obj%ngrp
      hd(4,1)            = obj%ioff
      hd(25,1)           = obj%lav
      hd(64,1)           = obj%ndpt
      hd(obj%hdr_off,1)  = obj%off_init  + (obj%ioff - 1) * obj%off_inc
      hd(obj%hdr_cmp,1)  = obj%cmp_init  + (obj%ibas - 1) * obj%cmp_inc
      hd(obj%hdr_line,1) = obj%line_init + (obj%ilin - 1) * obj%line_inc
      ntr = 1
      return
      end subroutine rnsyn

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine rnsyn_wrapup (obj)
      implicit none
      type(rnsyn_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine rnsyn_wrapup

!</execute_only>


!!----------------------------- rnsyn rand ----------------------------------!!
!!----------------------------- rnsyn rand ----------------------------------!!
!!----------------------------- rnsyn rand ----------------------------------!!
 

!<execute_only>

      subroutine rnsyn_rand (trace, ndpt, sdev)
      implicit none
      real   ,intent(inout)     :: trace(:)               ! argument
      integer,intent(in)        :: ndpt                   ! argument
      real   ,intent(in)        :: sdev                   ! argument
      integer                   :: j                      ! local
 
      trace(ndpt) = 0.0
      do j = 1, ndpt - 1, 2
         call mth_gauss_ranf (sdev, trace(j), trace(j+1), homegrown)
      enddo
      return
      end subroutine rnsyn_rand

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module rnsyn_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

