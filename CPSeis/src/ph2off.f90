!<CPS_v1 type="PROCESS"/>
!<-- This documentation header was last revised by SChiu on 2003-04-23. />

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
! Name       : PH2OFF    
! Category   : transforms 
! Written    : 2003-05-06   by: Stephen Chiu
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Convert Tau-Ph gathers to Time-Offset gathers
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! This process is equivalent to PHXH module in APSE. It converts 
! Tau-Ph(Ph is P in offset domain) gathers to Time-Offset gathers  
!
! P-values (input) and offsets (output) are BOTH kept in header word 6.
! P-values have units of MICROSECONDS per unit distance and offsets  
! replace P-values on output.   
!
! If P values are used from the offset header (header 6), P values must
! monotonically increase or decrease. If the P values are not used from the
! header, user needs to define the range of P values.
!
! PH2OFF accepts a CPS velocity file and allows only one of the velocity type:
! 'VTNM', 'VTRM','VTAV'and 'VTIN'.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in gathers with P values 
! monotonically increasing or decreasing if offset header values are used.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
!
! This process outputs trace gathers.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used and possibly increased
! GATHERED  whether traces are a legitimate gather  used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used and not changed
! TSTRT     starting time on trace                  used and not changed
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
!
! 1       Sequential Trace Count     Renumbered.
! 2       head mute                  not used 
! 3       Current gather             used
! 4       trace in current gather    set
! 6       offset or P-value          used and set
! 25      LAV                        reset
! 64      tail mute                  not used 
!

!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author       Description
!     ----       ------       -----------
!003. 2006-10-16 D. Glover    Added NULLIFY statements for Intel compiler.
!002. 2006-01-10 B. Menger    Removed Unused Variables.
! 1  2005-01-31  S. Chiu      Original version
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
! no special requirements.
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
!<gui_def>
!<NS PH2OFF Process/NC=80>
!
!    [/C]Convert Tau-Ph gathers to Time-Offset gathers
!
!    P_OFFSET_HDR=`CCC
!
!    P_INIT~~=`FFFFFFFF         OFF_INIT=`FFFFFFFF
!    P_INC~~~=`FFFFFFFF         OFF_INC= `FFFFFFFF
!    P_LAST~~=`FFFFFFFF         OFF_LAST=`FFFFFFFF
!    P_TOT~~~=`IIIIIII          OFF_TOT= `IIIIIII
!            
! Select PATH_VEL [PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [PATH_VEL_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS PATH_VEL[/ML=128/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!----------------------input and output trace parameters------------------------
!
!<Help KEYWORD="P_OFFSET_HDR">
!<Tip> Whether to use P values from offset header or user defined.</Tip>
! Default = YES
! Allowed = YES/NO
! If P_OFFSET_HDR = YES, then use P values from header word 6. 
! If P_OFFSET_HDR = NO,  then user needs to define P-value ranges.
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> First output offset. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> output offset increment. </Tip>
! Default = 25.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Last output offset. </Tip>
! Default = OFF_INC * (NUMTR-1)
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of output offsets.  </Tip>
! Default = NUMTR
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="P_INIT">
!<Tip> First input P value. </Tip>
! Default = 0.0
! Allowed = real
!
! Note: P-value units are MICROseconds/distance.
!</Help>
!
!<Help KEYWORD="P_INC">
!<Tip> Increment of input p-values </Tip>
! Default = 5.0
! Allowed = real >= 0.0
!
!<Help KEYWORD="P_LAST">
!<Tip> Last input P value. </Tip>
! Default = P_INC * (NUMTR-1)
! Allowed = real
!
! Note: P-value units are MICROseconds/distance.
!</Help>
!
!<Help KEYWORD="P_TOT">
!<Tip> Total number of input P-values. </Tip>
! Default = NUMTR
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> Select velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="PATH_VEL_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_VEL. </Tip>
!</Help>
!
!-------------------------------------------------------------------------------
!
!</HelpSection>


!-------------------------------------------------------------------------------
!
! NOTES FOR CONVERSION PROGRAMMER
!
!
!
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ph2off_module

      use bandps_module
      use cio_module

      use lav_module

      use mutehw_module
      use named_constants_module
      use pattern_module
      use pc_module

      use velio_module
      use nmo_module
      use pathcheck_module
      use pathchoose_module

      implicit none

      private
      public :: ph2off_create
      public :: ph2off_initialize
      public :: ph2off_update
      public :: ph2off_delete

!<execute_only>

      public :: ph2off           ! main trace processing routine.
      public :: ph2off_wrapup

!</execute_only>

      character(len=100),public,save :: PH2OFF_IDENT =  &
      '$Id: ph2off.f90,v 1.3 2006/10/17 13:45:46 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

!---- the usual parameter structure:

      INTEGER, PARAMETER :: RAW_VF_MAX = 100
      INTEGER, PARAMETER :: MAX_LINE = 132

      type,public :: ph2off_struct

        private

        logical              :: skip_wrapup    ! dependent variable
        integer              :: istat          ! dependent variable

        logical              :: p_offset_hdr   ! User parameters
        real                 :: off_init       ! User parameters
        real                 :: off_inc        ! User parameters
        real                 :: off_last       ! User parameters
        integer              :: off_tot        ! User parameters

        real                 :: p_init         ! User parameters
        real                 :: p_inc          ! User parameters
        real                 :: p_last         ! User parameters
        integer              :: p_tot          ! User parameters

        character (len=filename_length) :: path_vel   ! velocity file name
        type(pathchoose_struct),pointer :: dialog_vel

        integer              :: ndpt           ! Global
        real                 :: tstrt          ! Global
        integer              :: numtr          ! Global
        integer              :: nwih           ! Global
        real                 :: dt             ! Global
        logical              :: gathered       ! Global

!------ more dependent variables

        integer                     :: sequence
        logical                     :: velinv
        type(velio_struct),pointer  :: velio ! dependent module
        type(nmo_struct), pointer   :: nmo   ! dependent module

      end type ph2off_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(ph2off_struct),pointer,save :: object      ! needed for traps

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine ph2off_create (obj)
      implicit none
      type(ph2off_struct),pointer :: obj       ! arguments

!---- allocate the structure

      allocate (obj)

!---- nullify all pointers

      nullify (obj%velio)
      nullify (obj%nmo)
      nullify (obj%dialog_vel) ! jpa

      call pathchoose_create (obj%dialog_vel, 'path_vel',  '*')

      call ph2off_initialize (obj)

      return
      end subroutine ph2off_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine ph2off_delete (obj)
      implicit none
      type(ph2off_struct),pointer :: obj       ! arguments

!<execute_only>
      call ph2off_wrapup (obj)
!</execute_only>

      if (associated(obj%velio))    call velio_close(obj%velio)
      if (associated(obj%nmo))      call nmo_delete(obj%nmo)

      if ( associated ( obj%dialog_vel ) )  &
           call pathchoose_delete ( obj%dialog_vel )

      deallocate(obj)

      return
      end subroutine ph2off_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine ph2off_initialize (obj)
      implicit none
      type(ph2off_struct),pointer :: obj       ! arguments

!---- get global values in initialization
      obj%ndpt = FNIL
      call pc_get_global ('NDPT', obj%ndpt)
      if ( obj%ndpt == FNIL ) then
        call pc_error('PH2OFF: Global param NDPT has not been properly set.')
      endif


      obj%tstrt = FNIL
      call pc_get_global ('TSTRT', obj%tstrt)
      if ( obj%tstrt == FNIL ) then
        call pc_error('PH2OFF: Global param TSTRT has not been properly set.')
      endif


      obj%dt = FNIL
      call pc_get_global ('DT', obj%dt)
      if ( obj%dt == FNIL ) then
        call pc_error('PH2OFF: Global param DT has not been properly set.')
      endif

      obj%numtr = FNIL
      call pc_get_global ('NUMTR', obj%numtr)
      if ( obj%numtr == FNIL ) then
        call pc_error('PH2OFF: Global param NUMTR has not been properly set.')
      endif

!---- initialize process variables

      obj%path_vel = pathcheck_empty  ! velocity file


      obj%p_offset_hdr = .true.

      obj%off_init = 0.0
      obj%off_inc  = 25
      obj%off_tot  = obj%numtr
      obj%off_last = obj%off_inc * (obj%numtr-1)

      obj%p_init   = 0.0
      obj%p_inc    = 5.0
      obj%p_tot    = obj%numtr
      obj%p_last   = obj%p_inc * (obj%numtr-1)

      call ph2off_update (obj)

      return
      end subroutine ph2off_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine ph2off_update (obj)
      implicit none
      type(ph2off_struct),intent(inout),target :: obj

      integer     :: j,nstore,nscratch              ! Local
      integer     :: update_state                   ! Local
      integer     :: ier, maxtrc                    ! Local

      character(len=7)           :: opt_nmo          ! process parameter
      character(len=6)           :: opt_interp       ! process parameter
      real                       :: doppler          ! process parameter.
      character(len=4)           :: opt_nmo_res      ! process parameter
      integer                    :: order_mo         ! process parameter.
      logical                    :: opt_demult       ! process parameter.

      integer :: raw_pt_cnt                            ! local
      real,dimension(RAW_VF_MAX) :: raw_vel_func       ! local
      real,dimension(RAW_VF_MAX) :: raw_time_func      ! local

      real,dimension(RAW_VF_MAX) :: raw_x_coord        ! local
      real,dimension(RAW_VF_MAX) :: raw_y_coord        ! local

      integer                    :: vf_x_hdr           ! local
      integer                    :: vf_y_hdr           ! local
      integer                    :: vf_bin_cnt         ! local
      character(len=4*MAX_LINE)  :: msg                ! local
      CHARACTER(len=8)           :: vf_name            ! local
      CHARACTER(len=4)           :: vf_type            ! local       
      real                       :: SIGN_GETV, EXP_GETV

      object => obj         ! needed for traps.
      update_state = pc_get_update_state()

      obj%skip_wrapup = .true.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

    if ( pathchoose_update ( obj%dialog_vel,   obj%path_vel   ) ) return

!---- retrieve globals
      call pc_get_global ('NDPT'    , obj%ndpt    )
      call pc_get_global ('NWIH'    , obj%nwih    )
      call pc_get_global ('DT'      , obj%dt      )
      call pc_get_global ('TSTRT'   , obj%tstrt   )
      call pc_get_global ('GATHERED', obj%gathered)
      call pc_get_global ('NUMTR'   , obj%numtr)

      if(.not. obj%gathered) then
        call pc_error('PH2OFF: This process requires gathered data.')
      endif

      if ( obj%ndpt <= 0 ) then
        call pc_error('PH2OFF: Global parameter NDPT <= 0. Nothing to do.')
      endif

      if ( obj%nwih /= HDR_NOMINAL_SIZE ) then
        call pc_error('PH2OFF: Global parameter NWIH /= HDR_NOMINAL_SIZE.')
      endif

      if ( obj%dt <= 0.0 ) then
        call pc_error('PH2OFF: Global parameter DT <= 0.0.')
      endif

!---- retrieve user paramerters

      call pc_get ('OFF_INIT'       , obj%off_init   , ph2off_trap)
      call pc_get ('OFF_INC'        , obj%off_inc    , ph2off_trap)
      call pc_get ('OFF_LAST'       , obj%off_last   , ph2off_trap)
      call pc_get ('OFF_TOT'        , obj%off_tot    , ph2off_trap)

      call pc_get ('P_OFFSET_HDR'   , obj%p_offset_hdr)

      call pc_get ('P_INIT'         , obj%p_init     , ph2off_trap)
      call pc_get ('P_INC'          , obj%p_inc      , ph2off_trap)
      call pc_get ('P_LAST'         , obj%p_last     , ph2off_trap)
      call pc_get ('P_TOT'          , obj%p_tot      , ph2off_trap)

      call pc_get ('PATH_VEL'       , obj%path_vel )

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (.not. obj%gathered) then
        call pc_error ('This process must be preceded by a GATHER')
      end if
      if (obj%numtr < 2) then
        call pc_error ('PH2OFF: Must have more than one trace in gather.')
      end if

      ier = pattern_stop2('PH2OFF:', .true.,                        &
         obj%off_init, obj%off_inc, obj%off_last, obj%off_tot,      &
         'OFF_INIT', 'OFF_INC', 'OFF_LAST', 'OFF_TOT',              &
         pc_verify_scalar('off_init'), pc_verify_scalar('off_inc'), &
         pc_verify_scalar('off_last'), pc_verify_scalar('off_tot'), &
         inc_min=1.0)


      ier = pattern_stop2('PH2OFF:', .true.,                       &
        obj%p_init, obj%p_inc, obj%p_last, obj%p_tot,              &
        'P_INIT', 'P_INC', 'P_LAST', 'P_TOT',                      &
        pc_verify_scalar('p_init'), pc_verify_scalar('p_inc'),     &
        pc_verify_scalar('p_last'), pc_verify_scalar('p_tot'),     &
        inc_min=0.0)


      if ( pc_verify_scalar('path_vel') ) then
        call pathcheck (KEYWORD='path_vel', PATHNAME=obj%path_vel, &
                        EXT='', REQUIRED=.true., SCREEN='PH2OFF',  &
                        show=PATHCHECK_INFO_INPUT)
      endif

!  Run the traps needed when GUI users assert the parameters are "OK".
      if ( pc_verify_end() ) then
         if ( obj%path_vel /= pathcheck_empty ) then
           call pathcheck (KEYWORD='path_vel', PATHNAME=obj%path_vel, &
                           EXT='', REQUIRED=.true., SCREEN='PH2OFF')
         endif
      end if

      if (obj%p_offset_hdr) then
         call pc_put_sensitive_field_flag   ('P_INIT',   .false.)
         call pc_put_sensitive_field_flag   ('P_INC ' ,  .false.)
         call pc_put_sensitive_field_flag   ('P_LAST',   .false.)
         call pc_put_sensitive_field_flag   ('P_TOT ',   .false.)
      else
         call pc_put_sensitive_field_flag   ('P_INIT',   .true.)
         call pc_put_sensitive_field_flag   ('P_INC ' ,  .true.)
         call pc_put_sensitive_field_flag   ('P_LAST',   .true.)
         call pc_put_sensitive_field_flag   ('P_TOT ',   .true.)  
      end if 

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! nmo
        
      opt_nmo = 'STK_VEL'
      opt_interp = 'LINEAR'
      doppler = -1
      opt_nmo_res = 'NONE'
      order_mo = 2
      opt_demult = .false.

      call pc_clear

      call pc_put_process('pathname',       obj%path_vel)
      call pc_put_process('opt_nmo',        opt_nmo)     
      call pc_put_process('opt_interp',     opt_interp)
      call pc_put_process('doppler',        doppler)
      call pc_put_process('opt_nmo_res',    opt_nmo_res)
      call pc_put_process('order_mo',       order_mo)
      call pc_put_process('opt_demult',     opt_demult)
   
      if (associated(obj%nmo)) then
         call nmo_update (obj%nmo)
      else
         call nmo_create (obj%nmo)
      end if
      call pc_restore

! end nmo

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      maxtrc = max(obj%numtr, obj%off_tot, obj%p_tot)

      call pc_put_global ('NUMTR', maxtrc)      ! maximum number of traces.

      call pc_put ('OFF_INIT'      , obj%off_init  )
      call pc_put ('OFF_INC'       , obj%off_inc   )
      call pc_put ('OFF_LAST'      , obj%off_last  )
      call pc_put ('OFF_TOT'       , obj%off_tot   )

      call pc_put ('P_OFFSET_HDR'  , obj%p_offset_hdr)
      call pc_put ('P_INIT'        , obj%p_init    )
      call pc_put ('P_INC'         , obj%p_inc     )
      call pc_put ('P_LAST'        , obj%p_last    )
      call pc_put ('P_TOT'         , obj%p_tot     )
      call pc_put ('PATH_VEL'      , obj%path_vel   )

!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!

 
!---- convert bytes to words

      nscratch = obj%ndpt
      nstore = obj%ndpt

      call pc_put_control ('NSTORE'  , nstore  )
      call pc_put_control ('NSCRATCH', nscratch)

      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!    read the velocity type from the velocity file.

     if(obj%path_vel /= PATHCHECK_EMPTY) then

        call velio_open_read(obj%velio, obj%path_vel, vf_bin_cnt, &
          ier, msg, vf_x_hdr, vf_y_hdr, SIGN_GETV, EXP_GETV)

        j = 1
        call velio_read_velfun(obj%velio, &
            raw_x_coord(j), raw_y_coord(j), &
            raw_pt_cnt, raw_time_func, raw_vel_func, ier, msg, &
            vf_name, vf_type)
            
        call velio_close (obj%velio)

       if (vf_type=='VTNM' .or. vf_type=='VTRM' .or. vf_type=='VTAV') then
          obj%velinv = .false.
       else if (vf_type=='VTIN') then
          obj%velinv = .true.
       else
          call pc_error ('PH2OFF: Does not support this velocity type =', &
           vf_type)
          call pc_error ('PH2OFF: Requires velocity in time ')
       end if

     end if

     ! reset p back to microsecond per distance unit

      obj%sequence = 0
      obj%p_init =  obj%p_init * 1.e-6    
      obj%p_inc  =  obj%p_inc * 1.e-6   

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine ph2off_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
   
      subroutine ph2off_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

      integer             ::      warning ! local


      warning = 0

!---- there may be a duplicate error check for this, but this is friendlier

       keyword_select: select case (keyword)

        case ('OFF_TOT') keyword_select
          if (object%off_tot == FNIL) then
            call pc_error ('PH2OFF: OFF_TOT field cannot be blank.')
          endif

        case ('P_INC') keyword_select
            if (object%p_inc == FNIL) then
              call pc_error ('PH2OFF: P_INC field cannot be blank.')
            endif

        case ('P_TOT') keyword_select
            if (object%p_tot == INIL) then
              call pc_error ('PH2OFF: P_TOT field cannot be blank.')
            endif

      end select keyword_select

      return
      end subroutine ph2off_trap


!-------------------------------------------------------------------------------
! Subroutine to handle all the GUI sensitivity settings during update.
!-------------------------------------------------------------------------------

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine ph2off (obj,ntr,hd,tr)
      implicit none
      type(ph2off_struct)               :: obj             ! arguments
      integer,         intent(inout)  :: ntr               ! arguments
      double precision,intent(inout)  :: hd(:,:)           ! arguments
      real,            intent(inout)  :: tr(:,:)           ! arguments

      double precision   :: hd_tmp(obj%nwih, ntr)

      integer  :: i,j,k, npa,npb      ! local

      integer  :: ntr1                                     ! local

      real     :: sa(obj%numtr,obj%ndpt), p_val(obj%numtr) ! local
      real     :: xh(obj%numtr,obj%ndpt), vel(obj%ndpt,1)  ! local
      real     ::       h    , dxh, sini, cosi, fact ! local

!-------------------------------------------------------------------------------
     if (ntr == NO_MORE_TRACES) then
       call ph2off_wrapup (obj)
       return
     endif

    if (ntr == 1) then
      call pc_warning (msg1 = 'PH2OFF: Gather number ',              &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),        &
                       msg2 = ' has only one trace; not processed.')
      return
    end if
    !
    if (ntr > obj%numtr) then
      call pc_warning (msg1 = 'PH2OFF: Gather number ',                &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),          &
                       msg2 = ' too big - processing only NUMTR traces;&
                              & gather size was ',                     &
                       var2 = ntr )
      ntr = obj%numtr
    end if

    !   store headers

    hd_tmp(1:obj%nwih, 1:ntr) = hd(1:obj%nwih, 1:ntr)

    ntr1 = 1

    call nmo(obj%nmo, ntr1, hd, vel(:,:ntr1))

   ! Copy input p values to work array

     do j=1,obj%ndpt
       sa(1:obj%p_tot,j) = tr(j,1:obj%p_tot)
     enddo

     do i = 1, ntr
       if (obj%p_offset_hdr) then
          p_val(i) = real(hd(HDR_OFFSET, i))* 1.e-6
       else 
          p_val(i) = obj%p_init + obj%p_inc * (i-1)         
       end if
     end do

   ! Compute offset as a function of time for each Ph trace

     do i=1, ntr
  
        xh(i,1) = 0.0
        do j=2,obj%ndpt

           sini = p_val(i)*vel(j,ntr1) 
           if (abs(sini).lt.1.0) then
              cosi = sqrt(1.-sini*sini)
              if (obj%velinv) then         
                 !  interval velocity type
                 dxh = vel(j,ntr1)*obj%dt*sini/cosi  
                 xh(i,j) = xh(i,j-1) + dxh
              else
                 !  rms velocity type
                 xh(i,j) = vel(j,ntr1)*obj%dt*(j-1)*sini/cosi  
              end if
           else
              xh(i,j:obj%ndpt) = -99999.0
              exit
           end if
         end do
      end do


! Loop and interpolate across P traces

      tr(1,1:obj%off_tot) = 0.0
      do i=2,obj%ndpt
         do j=1,obj%off_tot
            h = obj%off_init + obj%off_inc*(j-1)

          ! Find first and last live p traces
            npa = 0
            npb = 0
            do k=1,obj%p_tot
               if (xh(k,i).ne.-99999.0) then
                 if (npa.eq.0) npa = k
                 if (k.gt.npb) npb = k
               endif
            end do
          ! Check for out of bounds
            if (npa.eq.npb) then
              tr(i,j) = 0.0
            else if (h.lt.xh(npa,i) .or. h.gt.xh(npb,i)) then
              tr(i,j) = 0.0
            else       

          ! Find bounding values

              do k=npa+1,npb
                if (h.gt.xh(k-1,i) .and. h.lt.xh(k,i)) then
                  fact = (h - xh(k-1,i))/(xh(k,i)-xh(k-1,i))
                  tr(i,j) = (1.-fact)*sa(k-1,i) + fact*sa(k,i)
                  exit
                else if (h.eq.xh(k-1,i)) then
                  tr(i,j) = sa(k-1,i)
                  exit
                else if (h.eq.xh(k,i)) then
                  tr(i,j) = sa(k,i)
                  exit
                endif
              enddo
            endif
         enddo
      enddo

      ntr = obj%off_tot

      !  reset header values

      do j = 1, ntr

        obj%sequence              = obj%sequence + 1
        hd(1:obj%nwih, j)         = hd_tmp(1:obj%nwih, 1)
    
        hd(HDR_SEQUENCE,j)        = obj%sequence
        hd(HDR_TOP_MUTE,j)        = 1
        hd(HDR_BOTTOM_MUTE,j)     = obj%ndpt
        hd(HDR_CURRENT_CHANNEL,j) = j
        hd(HDR_OFFSET,j)          = obj%off_init + obj%off_inc*(j-1)

        hd(HDR_SOURCE_XLOC,j)     = hd(HDR_MIDPOINT_XLOC,j)  &
                                     - 0.5*obj%off_inc*j
        hd(HDR_SOURCE_YLOC,j)     = hd(HDR_MIDPOINT_YLOC,j)
        hd(HDR_RECEIVER_XLOC,j)   = hd(HDR_MIDPOINT_XLOC,j)  &
                                     + 0.5*obj%off_inc*j
        hd(HDR_RECEIVER_YLOC,j)   = hd(HDR_MIDPOINT_YLOC,j)
      end do

    !
    ! - Set LAV header
    !
    call lav_set_hdr (hd, tr, obj%ndpt, ntr)

    return
    !
  end subroutine ph2off
 

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine ph2off_wrapup (obj)
      implicit none
      type(ph2off_struct) :: obj       ! arguments



!---- conditional return
      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine ph2off_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module ph2off_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
