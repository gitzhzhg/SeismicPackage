!<CPS_v1 type="PROCESS"/>
!!----------------------------- gather.f90 --------------------------------!!
!!----------------------------- gather.f90 --------------------------------!!
!!----------------------------- gather.f90 --------------------------------!!


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
! Name       : GATHER
! Category   : sorts
! Written    : 1999-09-10   by: Tom Stoeckley
! Revised    : 2002-07-18   by: Tom Stoeckley
! Maturity   : production   2002-07-29
! Purpose    : A simple process to gather traces.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! The GATHER process takes input traces and outputs them in gathers (ensembles)
! as defined by header word 3.  A new gather will begin each time the value of
! header word 3, rounded to a whole number, changes.
!
! Alternatively, the GATHER process can redefine trace gathers based on
! any other header word, or by changing the bin centers or bin increment.
! Therefore, header words 3 and 4 will be reset unless HDR_GATHER = 3,
! GATHER_INIT = 1, and GATHER_INC = 1.  Header word 3 will be set according
! to the following equation:
!
!     hdr(3) = 1 + nint ( (hdr(HDR_GATHER) - GATHER_INIT) / GATHER_INC )
!
! Whether or not the trace gathers are redefined, the parameter NUM_TR_MAX,
! which is the maximum number of traces any gather will have, must be
! specified.
!
! Input traces may be either gathered or ungathered.
!
! Traces must be sorted to the desired order prior to being input to GATHER
! since GATHER only creates ensembles of traces, it cannot reorder traces.
!
! GATHER can be called either from a processing system or a process module.
! There is a companion process called UNGATHER for ungathering traces.
!
! If a process module requires input traces to be gathered, it can simply
! contain code like the following:
!
!      call pc_get_global  ('gathered', gathered)
! 
!      if (.not.gathered) then
!           call pc_error ('this process must be preceded by a gather')
!      end if
!
!-------------------------------------------------------------------------------
!</descript_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS            
!
! Process is a multiple-trace process.
!
! Input traces can be ungathered, or gathered in any unspecified manner.
! This algorithm works even if the input traces are already gathered on
! header word 3, but it would be more efficient to eliminate calling this
! process in such circumstances unless you are regathering traces based
! on a different header word or with a different bin center or increment.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>
 

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS          
!
! This process does not alter input traces.
! This process outputs trace gathers.
!
! Output traces will be gathered on header word 3.  Each time the value of
! header word 3 (rounded to an integer) changes, a new gather will begin.
!
! If there are more consecutive input traces with the same value of header
! word 3 than are specified with the NUM_TR_MAX parameter, a fatal error
! will be generated.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

 
!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED       
!
! Name       Description                            Action taken
! ----       -----------                            ------------
! NWIH       number of words in trace header        used but not changed
! NDPT       number of sample values in trace       used but not changed
! NUMTR      max number of traces input/output      changed
! GATHERED   whether traces are gathered on hwd 3   set to true
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED      
!
! Hwd#        Description      Action taken
! ----        -----------      ------------
! 3           Current gather   Used if HDR_GATHER is 3, and maybe changed.
! 4           Trace counter    Maybe changed (input value not used).
! HDR_GATHER  Gather header    Used to group traces into gathers (not changed).
!
! If HDR_GATHER is set to 3, GATHER_INIT is set to 1, and GATHER_INC is set
! to 1 (all default values), no header words will be changed.  Otherwise,
! header words 3 and 4 will be reset.
!
!-------------------------------------------------------------------------------
!</header_word_doc>
 

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
! 14. 2002-07-29  Stoeckley  Change to use the MTH module for binning.
! 13. 2001-10-11  Stoeckley  Add parameters to gather on any header word.
! 12. 2001-05-16  Stoeckley  Change wrapup flag.
! 11. 2000-11-15  Stoeckley  Add missing context-sensitive help.
! 10. 2000-10-06  Stoeckley  Add missing required documentation sections.
!  9. 2000-05-08  Stoeckley  Fix wrapup printout error in number of gathers
!                             output.
!  8. 2000-04-10  Stoeckley  Change to be setup only when gather is not needed.
!  7. 2000-03-30  Stoeckley  Change to abort if the number of traces in any
!                             gather exceeds NUM_TR_MAX (this required some
!                             logic changes); add GUI definition section.
!  6. 2000-02-04  Stoeckley  Add diagnostic printouts at wrapup time.
!  5. 2000-01-28  Stoeckley  Add checks on input global values.
!  4. 2000-01-06  Stoeckley  Terminate building a gather when NTR reaches
!                             the minimum of NUM_TR_MAX and SIZE(TRO,2).
!                             Previously terminated only based on SIZE(TRO,2).
!                             Also add wrapup flag.
!  3. 1999-12-21  Stoeckley  Incorporate documentation supplied by Chuck I.
!                             Burch, and change parameter name from TPG to
!                             NUM_TR_MAX.  Also fixed a bug.
!  2. 1999-11-17  Stoeckley  Add ident string for RCS.
!  1. 1999-09-10  Stoeckley  Initial version.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! SETUP_ONLY is set to true if the gather could have been omitted.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == NEED_TRACES    if this process needs more traces.
!    NTR == FATAL_ERROR    if this process has an error.
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
!<NS GATHER Process/NC=72>
!
!                A Simple Process to Gather the Traces
!
!                        HDR_GATHER = `II
!                        GATHER_INIT= `FFFFFFFFF
!                        GATHER_INC = `FFFFFFFFF
!
!                        NUM_TR_MAX = `IIIIIIIIII
!
!           This GATHER will abort if NUM_TR_MAX is too small.
!
! [msg1]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! [msg2]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! [msg3]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_GATHER">
!<Tip> Header word designating desired trace gathers. </Tip>
! Default = 3
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="GATHER_INIT">
!<Tip> Value of HDR_GATHER for the first (or any) trace gather. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin designating a trace gather.
! This value does not have to correspond to the first actual trace gather.
!
! Header word 3 will be set to 1 for the gather which is centered on
! the value GATHER_INIT.
!</Help>
!
!
!<Help KEYWORD="GATHER_INC">
!<Tip> Increment of HDR_GATHER between 2D lines. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This value must be the bin increment (or width) designating a trace gather
!</Help>
!
!
!<Help KEYWORD="NUM_TR_MAX">
!<Tip> Maximum number of traces to expect in any one gather. </Tip>
! Default = 0
! Allowed = int >= 1
!
! This process will intentionally abort if more than NUM_TR_MAX traces
! are encountered in the same gather.
!</Help>
!
!
!<Help KEYWORD="MSG1">
!<Tip> An informational or error message. </Tip>
!</Help>
!
!
!<Help KEYWORD="MSG2">
!<Tip> An informational or error message. </Tip>
!</Help>
!
!
!<Help KEYWORD="MSG3">
!<Tip> An informational or error message. </Tip>
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module gather_module
      use pc_module
      use named_constants_module
      use mth_module
      implicit none
      private
      public :: gather_create     ! uses the parameter cache.
      public :: gather_initialize
      public :: gather_update     ! uses the parameter cache.
      public :: gather_delete
      public :: gather            ! main execution (trace processing) routine.
      public :: gather_wrapup
 
      character(len=100),public,save :: GATHER_IDENT = &
       '$Id: gather.f90,v 1.14 2002/07/26 20:39:59 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: gather_struct
           private
           logical          :: skip_wrapup         ! wrapup flag.
           integer          :: hdr_gather          ! process parameter.
           double precision :: gather_init         ! process parameter.
           double precision :: gather_inc          ! process parameter.
           integer          :: num_tr_max          ! process parameter.
           integer          :: nwih                ! global parameter.
           integer          :: ndpt                ! global parameter.
           logical          :: gathered            ! global parameter.
           integer          :: ntrkeep             ! dependent variable.
           integer          :: kounti,kounto       ! dependent variable.
           integer          :: ntot,nfull          ! dependent variable.
           integer          :: nmin,nmax           ! dependent variable.
           logical          :: reset               ! dependent variable.
           integer          :: last3,last4         ! dependent variable.
      end type gather_struct


!!------------------------- interfaces -----------------------------------!!
!!------------------------- interfaces -----------------------------------!!
!!------------------------- interfaces -----------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
 
 
      type(gather_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine gather_create (obj)
      implicit none
      type(gather_struct),pointer :: obj       ! arguments

      allocate (obj)
      call gather_initialize (obj)
      return
      end subroutine gather_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
 
 
      subroutine gather_delete (obj)
      implicit none
      type(gather_struct),pointer :: obj       ! arguments
 
      call gather_wrapup (obj)

      deallocate(obj)
      return
      end subroutine gather_delete
 
 
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
 
 
      subroutine gather_initialize (obj)
      implicit none
      type(gather_struct),intent(inout) :: obj       ! arguments
 
      obj%hdr_gather  = 3
      obj%gather_init = 1.0
      obj%gather_inc  = 1.0
      obj%num_tr_max  = 0
      obj%nwih        = 0
      obj%ndpt        = 0
      obj%gathered    = .false.
      obj%ntrkeep     = 0
      obj%kounti      = 0
      obj%kounto      = 0
      obj%ntot        = 0
      obj%nfull       = 0
      obj%nmin        = 0
      obj%nmax        = 0
      obj%reset       = .false.
      obj%last3       = 0
      obj%last4       = 0
      call gather_update (obj)
      return
      end subroutine gather_initialize
 
 
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine gather_update (obj)
      implicit none
      type(gather_struct),intent(inout),target :: obj             ! arguments
      integer                                  :: numtr           ! local
      character(len=80)                        :: msg1,msg2,msg3  ! local
      logical                                  :: sense,whoops    ! local

      object => obj                ! needed for traps.
      obj%skip_wrapup = .true.     ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global  ('nwih'       , obj%nwih)
      call pc_get_global  ('ndpt'       , obj%ndpt)
      call pc_get_global  ('numtr'      , numtr)
      call pc_get_global  ('gathered'   , obj%gathered)

      call pc_get         ('hdr_gather' , obj%hdr_gather)
      call pc_get         ('gather_init', obj%gather_init)
      call pc_get         ('gather_inc' , obj%gather_inc)
      call pc_get         ('num_tr_max' , obj%num_tr_max)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      msg1      = ' '
      msg2      = ' '
      msg3      = ' '
      sense     = .true.
      whoops    = .false.
      obj%reset = .false.

      if (obj%hdr_gather  /=   3) obj%reset = .true.
      if (obj%gather_init /= 1.0) obj%reset = .true.
      if (obj%gather_inc  /= 1.0) obj%reset = .true.

      if (obj%hdr_gather <= 0 .or. obj%hdr_gather > obj%nwih) then
           whoops = .true.
           msg1   = 'HDR_GATHER must be between 1 and NWIH'
           call pc_error (msg1)
      end if

      if (obj%gather_inc <= 0.0) then
           whoops = .true.
           msg2   = 'GATHER_INC must be greater than zero.'
           call pc_error (msg2)
      end if

      if (obj%num_tr_max <= 0) then
           whoops = .true.
           msg3   = 'NUM_TR_MAX must be greater than zero.'
           call pc_error (msg3)
      end if

      if (whoops) then
           continue
      else if (obj%gathered .and. .not.obj%reset) then
           obj%num_tr_max = numtr
           sense = .false.
           msg1 = 'This GATHER is not needed since traces are already gathered.'
           msg2 = 'NUM_TR_MAX has been reset to the value of NUMTR.'
           msg3 = 'This GATHER will be a setup-only process.'
           call pc_info (msg1)
           call pc_info (msg2)
           call pc_info (msg3)
      else if (obj%reset) then
           msg1 = 'Header words 3 and 4 will be reset.'
           call pc_print (msg1)
      else
           msg1 = 'No header words will be changed.'
           call pc_print (msg1)
      end if


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put         ('hdr_gather'  , obj%hdr_gather)
      call pc_put         ('gather_init' , obj%gather_init)
      call pc_put         ('gather_inc'  , obj%gather_inc)
      call pc_put         ('num_tr_max'  , obj%num_tr_max)

      call pc_put_global  ('numtr'       , obj%num_tr_max)  ! not an error.
      call pc_put_global  ('gathered'    , .true.)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)
      call pc_put_control ('setup_only'  , .not.sense)

      call pc_put_gui_only ('msg1', msg1)
      call pc_put_gui_only ('msg2', msg2)
      call pc_put_gui_only ('msg3', msg3)

      call pc_put_sensitive_field_flag ('num_tr_max', sense)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.    ! to run wrapup code after processing.

      obj%ntrkeep = 0
      obj%kounti  = 0
      obj%kounto  = 0
      obj%ntot    = 0
      obj%nfull   = 0
      obj%nmin    = 0
      obj%nmax    = 0
      obj%last3   = 0
      obj%last4   = 0


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine gather_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!-------------------------- reset headers --------------------------------!!
!!-------------------------- reset headers --------------------------------!!
!!-------------------------- reset headers --------------------------------!!


      subroutine gather_reset_headers (obj,hdi)
      implicit none
      type(gather_struct),intent(inout)   :: obj                ! arguments
      double precision   ,intent(inout)   :: hdi(:)             ! arguments
      integer                             :: igather            ! local

      igather = mth_bin_number  &
                    (obj%gather_init, obj%gather_inc, hdi(obj%hdr_gather))
      if(size(hdi) < 3 ) return

      if (obj%last4 == 0) then               ! this is the very first trace.
           obj%last3 = igather
           obj%last4 = 1
      else if (igather == obj%last3) then    ! we are still in the same gather.
           obj%last4 = obj%last4 + 1
      else                                   ! we are starting a new gather.
           obj%last3 = igather
           obj%last4 = 1
      end if

      hdi(3) = obj%last3
      hdi(4) = obj%last4
      return
      end subroutine gather_reset_headers


!!----------------------- main execution -----------------------------------!!
!!----------------------- main execution -----------------------------------!!
!!----------------------- main execution -----------------------------------!!


      subroutine gather (obj, ntr,hdi,tri,hdo,tro)
      implicit none
      type(gather_struct),intent(inout)   :: obj                ! arguments
      integer            ,intent(inout)   :: ntr                ! arguments
      double precision   ,intent(inout)   :: hdi(:,:)           ! arguments
      real               ,intent(in)      :: tri(:,:)           ! arguments
      double precision   ,intent(inout)   :: hdo(:,:)           ! arguments
      real               ,intent(inout)   :: tro(:,:)           ! arguments
      logical                             :: ok_to_add_trace    ! local
      integer                             :: valuei,valueo      ! local
      integer                             :: indx               ! local
      if (ntr >= 1) then

           obj%ntrkeep = ntr
           obj%kounti  = 0

           if (obj%reset) then
                do indx = 1,ntr
                     call gather_reset_headers (obj,hdi(1:,indx))
                end do
           end if

      else if (ntr == NO_MORE_TRACES) then

           obj%ntrkeep = NO_MORE_TRACES
           obj%kounti  = 0
           ntr         = obj%kounto           ! passing out last gather.
           obj%kounto  = 0
           call gather_update_statistics (obj,ntr)
           call gather_wrapup (obj, 'after all traces were gathered')
           return

      else    ! if (ntr == NEED_TRACES) then

           if (obj%ntrkeep == NO_MORE_TRACES) then
                ntr = NO_MORE_TRACES
                call gather_wrapup (obj, 'after all traces were gathered')
                return
           end if

      end if

      do
           if (obj%kounti == obj%ntrkeep) then
                ntr = NEED_TRACES
                !call gather_dump_struct(obj)
                return
           end if

           if (obj%kounto == 0) then
                ok_to_add_trace = .true.
           else
                valuei = nint(hdi(3, obj%kounti + 1))
                valueo = nint(hdo(3, obj%kounto    ))
                ok_to_add_trace = (valuei == valueo)
           end if

           if (ok_to_add_trace) then
                obj%kounti = obj%kounti + 1
                obj%kounto = obj%kounto + 1
                if (obj%kounto > obj%num_tr_max) then
                      call pc_error ('GATHER received gather number',   &
                                     nint(hdi(3,obj%kounti)),           &
                                     'larger than NUM_TR_MAX =',        &
                                     obj%num_tr_max,'.')
                      ntr = FATAL_ERROR
                      call gather_wrapup (obj, 'with errors')
                      return
                end if
                hdo(1:obj%nwih, obj%kounto) = hdi(1:obj%nwih, obj%kounti)
                tro(1:obj%ndpt, obj%kounto) = tri(1:obj%ndpt, obj%kounti)
           else
                ntr = obj%kounto                ! passing out gather.
                obj%kounto = 0
                exit
           end if
      end do

      call gather_update_statistics (obj,ntr)
      return
      end subroutine gather

      subroutine gather_dump_struct(obj)
      implicit none
      type(gather_struct),intent(inout)   :: obj                ! arguments
        write(6,'(A,L1)')'skip_wrapup: ',obj%skip_wrapup
        write(6,'(A,I7)')'hdr_gather : ',obj%hdr_gather
        write(6,'(A,F11.3)')'gather_init: ',obj%gather_init
        write(6,'(A,F11.3)')'gather_inc : ',obj%gather_inc
        write(6,'(A,I7)')'num_tr_max : ',obj%num_tr_max
        write(6,'(A,I7)')'nwih       : ',obj%nwih
        write(6,'(A,I7)')'ndpt       : ',obj%ndpt
        write(6,'(A,L1)')'gathered   : ',obj%gathered
        write(6,'(A,I7)')'ntrkeep    : ',obj%ntrkeep
        write(6,'(A,I7)')'kounti     : ',obj%kounti
        write(6,'(A,I7)')'kounto     : ',obj%kounto
        write(6,'(A,I7)')'ntot       : ',obj%ntot
        write(6,'(A,I7)')'nfull      : ',obj%nfull
        write(6,'(A,I7)')'nmin       : ',obj%nmin
        write(6,'(A,I7)')'nmax       : ',obj%nmax
        write(6,'(A,L1)')'reset      : ',obj%reset
        write(6,'(A,I7)')'last3      : ',obj%last3
        write(6,'(A,I7)')'last4      : ',obj%last4
      end subroutine gather_dump_struct
 
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
 
 
      subroutine gather_wrapup (obj,msg)
      implicit none
      type(gather_struct)      ,intent(inout) :: obj       ! arguments
      character(len=*),optional,intent(in)    :: msg       ! arguments
 
      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.
      if (pc_get_update_state() /= PC_EXECUTE) return

      call pc_print ('')
      if (present(msg)) then
           call pc_print ('GATHER is finished ('//trim(msg)//').')
      else
           call pc_print ('GATHER is finished.')
      end if
      if (.not.obj%gathered) then
           call pc_print ('Number of trace ensembles output:', obj%ntot)
           call pc_print ('Number of trace ensembles with',  &
                            obj%num_tr_max, 'traces:', obj%nfull)
           call pc_print ('Number of trace ensembles with fewer than',  &
                            obj%num_tr_max, 'traces:', obj%ntot - obj%nfull)
           call pc_print ('Minimum output trace ensemble size:', obj%nmin)
           call pc_print ('Maximum output trace ensemble size:', obj%nmax)
      end if
      call pc_print ('')
      return
      end subroutine gather_wrapup
 

!!------------------------ update statistics -------------------------------!!
!!------------------------ update statistics -------------------------------!!
!!------------------------ update statistics -------------------------------!!
 
 
      subroutine gather_update_statistics (obj,ntr)
      implicit none
      type(gather_struct)      ,intent(inout) :: obj       ! arguments
      integer                  ,intent(in)    :: ntr       ! arguments
 
      if (ntr > 0) then
           if (obj%ntot == 0) then
                                      obj%nmax  = ntr
                                      obj%nmin  = ntr
           end if
                                      obj%ntot  = obj%ntot  + 1
           if (ntr == obj%num_tr_max) obj%nfull = obj%nfull + 1
                                      obj%nmin  = min(obj%nmin,ntr)
                                      obj%nmax  = max(obj%nmax,ntr)
      end if
      return
      end subroutine gather_update_statistics
 

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module gather_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

