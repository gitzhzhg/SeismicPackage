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
! Name       : HVEL  (Horizon based Velocity analysis)
! Category   : velocity_analysis
! Written    : 1990-05-18   by: John Reed
! Revised    : 2006-11-14   by: D. Glover
! Maturity   : production
! Purpose    : Horizon based semblance velocity analysis for 2D and 3D.
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
! For each input CMP, HVEL performs a semblance stacking velocity analysis in a
! window centered at the time associated with the specified horizon.  For each
! CMP a single trace is output whose sample values are semblance values for 
! different velocity scans.  These output traces can be displayed in CBYT and 
! the stacking velocity picked. If the number of traces within a CMP gather 
! is less than 4, the output semblance values are set to zero. 
!
! The horizon may be specified in two ways:  either as a constant time or by a
! pickfile created with the CBYT General Picking Option.
! 
! 
! Output Trace Header Words
!
! Header words in the output traces are identical to the header words in the 
! first trace of the CMP except:
!
!     HW(1)  is reset to the new sequential trace number
!     HW(2)  is set to the index of the first live sample
!     HW(5)  is the fold for the CMP
!     HW(25) is the LAV of the output trace samples
!     HW(64) is set to the index of the last live sample
!
!
! Output Trace Globals
!
! Time on the output traces is to be interpreted as scan velocity/1000. 
! 
!     DT    = (specified scan velocity increment)/1000.
!
!     TSTRT = (velocity of first scan)/1000.
!
!     NDPT  = (total number of velocity scans)
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Input traces must be gathered, in CMP order and with no NMO correction 
! applied.
!
! Output traces may be displayed in CBYT, where the semblance display may be 
! picked with the General Picking Option.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in gathers.
!
! Process requires traces to be input in CMP sort order with no NMO correction
! applied.
!
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
! This process outputs one trace at a time.
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
! NDPT      number of sample values in trace        changed
! TSTRT     starting time on trace                  changed 
! DT        trace sample interval                   changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
!
!     HW(1)  is reset to the new sequential trace number
!     HW(2)  is set to the index of the first live sample
!     HW(5)  is the fold for the CMP
!     HW(25) is the LAV of the output trace samples
!     HW(64) is set to the index of the last live sample
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author     Description
!     ----       ------     -----------
! 16. 2006-11-14 D. Glover  Added NULLIFY statements for Intel compiler.
! 15. 2006-01-10 B. Menger  Removed Unused Variables.
! 14. 2002-02-04 Stoeckley  Replace STATIO1 with STATIO call; move location
!                            of DIO_CLOSE call; change DIO_OBJ from structure
!                            variable to local variable.
! 13. 2001-02-15 Chiu       Convert to new CPS.
! 12. 1998-12-11 Goodger    Begin using the fortran90 compiler.    
! 11. 1997-06-19 Vunderink  Sort into increasing horizon basements
! 10. 1997-05-19 Goodger    Change file parameter from hollerith to 
!                           character.
! 9.  1994-03-09 Reed       Added option to read CVM model horizons
! 8.  1991-07-24 Reed       Added file option for horizon
! 7.  1991-06-04 Howard     Add NOBL directive on DO 21 loop.
! 6.  1990-09-05 Reed       Improved semblance calculation
! 5.  1990-06-01 Reed       Converted to system spline subroutines
!                           corrected,storage allocation, added NORM
!                           option.
! 4.  1990-05-25 Reed       Fixed a bug in semblance calculation
! 3.  1990-05-24 Reed       Fixed a bug in window timing
! 2.  1990-05-23 Reed       Put on system, checks O.K. on synthetic.
! 1.  1990-05-18 Reed       Started writing program, modified from LVEL 
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR  = 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS HVEL Process/NC=80>
! Horizon based semblance velocity analysis for 2D and 3D.
!
!   WIN_LEN= `FFFFFFFFFFF   
!
!   NORMALIZE= `CCC    OPT_PRINT= `CCC     OPT_HOR=~~~`CCCCCCCCC
!
!   VEL_BEG= `FFFFFFFFFFF              
!   VEL_END= `FFFFFFFFFFF      
!   VEL_INC= `FFFFFFFFFFF      
!
!   PATH_PICK=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!   CONS_TIME=`FFFFFFFFFFFF      
!
!<PARMS PATH_PICK[/ML=128/XST]>
!
!</gui_def>
!
!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Window length for semblance calculation, in seconds. </Tip>
! Default = 0.08
! Allowed = real > 0.0 
!</Help>
!
!<Help KEYWORD="VEL_BEG">
!<Tip> Stacking velocity for first (lowest) velocity scan. </Tip>
! Default = 1500.0
! Allowed = real > 0.0 
!</Help>
!
!<Help KEYWORD="VEL_END">
!<Tip> Stacking velocity for last (highest) velocity scan. </Tip>
! Default = 5000.0
! Allowed = real > VEL_BEG 
!</Help>
!
!<Help KEYWORD="VEL_INC">
!<Tip> Increment for stacking velocity scans. </Tip>
! Default = 20.0
! Allowed = real > VEL_BEG 
!</Help>
!
!<Help KEYWORD="NORMALIZE">
!<Tip> Whether to normalize semblance values to 1.0 on output traces. </Tip>
! Default = YES
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Whether to print semblance vs. velocity values in .rpt file. </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="OPT_HOR">
!<Tip> Whether to specify a horizon with a constant time or a pickfile. </Tip>
! Default = PICKFILE
! Allowed = PICKFILE
! Allowed = CONS_TIME
!</Help>
!
!<Help KEYWORD="CONS_TIME">
!<Tip> Constant time for horizon in seconds. </Tip>
! Default = -
! Allowed = real
!</Help>
!
!<Help KEYWORD="PATH_PICK">
!<Tip> Pathname for horizon pickfile. </Tip>
! Default = -
! Allowed = char
! PATH_PICK is the pathname for the horizon pickfile picked with the CBYT 
! General Picking Option.
!</Help>
!
!</HelpSection>

!-------------------------------------------------------------------------------
!
! NOTES FOR CONVERSION PROGRAMMER
!
!1.  Omit HDR_CMP_INL and HDR_CMP_CRL unnecessary.  
!
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module hvel_module

      use pc_module
      use named_constants_module
      use getlun_module
      use statio_module
      use dio_module
      use mutehw_module
      use pathcheck_module
      use lav_module
      use interp_module
      use sort_module

      implicit none
      private
      public :: hvel_create
      public :: hvel_initialize
      public :: hvel_update
      public :: hvel_delete
!<execute_only>
      public :: hvel            ! main execution (trace processing) routine.
      public :: hvel_wrapup
!</execute_only>


      character(len=100),public,save :: hvel_IDENT = &
'$Id: hvel.f90,v 1.16 2006/11/14 14:32:54 Glover prod sps $'

!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    type,public :: hvel_struct              
 
    private
    logical                          :: skip_wrapup    ! wrapup flag
    logical                          :: gathered       ! gathered flag
    character (len=FILENAME_LENGTH)  :: path_pick      ! Process parameter
    character (len=9)                :: opt_hor        ! Process parameter
    character (len=3)                :: normalize      ! Process parameter
    character (len=3)                :: opt_print      ! Process parameter
    real                             :: cons_time      ! Process parameter
    real                             :: vel_beg        ! Process parameter 
    real                             :: vel_end        ! Process parameter 
    real                             :: vel_inc        ! Process parameter 
    real                             :: win_len        ! Process parameter 

    integer                          :: ndpt           ! globals
    integer                          :: nwih           ! globals
    real                             :: dt             ! globals
    real                             :: tstrt          ! globals

    integer                          :: ntr_sum        ! dependent variables
    integer                          :: hdr_cmp_crl    ! dependent variables
    integer                          :: hdr_cmp_inl    ! dependent variables
    integer                          :: nx, ny         ! dependent variables
    integer                          :: nwin, nvel     ! dependent variables
    integer                          :: print_lun      ! dependent variables
    real                             :: x0, y0         ! dependent variables
    real                             :: xinc, yinc     ! dependent variables

    real              ,pointer       :: t_interp(:,:)  ! dependent variables

    end type hvel_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(hvel_struct),pointer,save :: object      ! needed for traps.

      integer,parameter      :: opt_hor_noptions = 2
      character(len=9),save  :: opt_hor_options(opt_hor_noptions)  &
                                =  (/'PICKFILE ','CONS_TIME'/)

      integer,parameter      :: normalize_noptions = 2
      character(len=3),save  :: normalize_options(normalize_noptions)  &
                                =  (/'YES','NO '/)

      integer,parameter      :: opt_print_noptions = 2
      character(len=3),save  :: opt_print_options(opt_print_noptions)  &
                                =  (/'YES','NO '/)

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine hvel_create (obj)
      implicit none
      type(hvel_struct),pointer :: obj        ! arguments

      allocate (obj)

      nullify  (obj%t_interp)

      call hvel_initialize (obj)
      return
      end subroutine hvel_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine hvel_delete (obj)
      implicit none
      type(hvel_struct),pointer :: obj       ! arguments

!<execute_only>
      call hvel_wrapup (obj)
!</execute_only>

!! Make sure ALL POINTERS in your parameter structure are deallocated

      if (associated(obj%t_interp )) deallocate        (obj%t_interp)

      deallocate(obj)
      return
      end subroutine hvel_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine hvel_initialize (obj)
      implicit none
      type(hvel_struct),intent(inout) :: obj       ! arguments

!! Initialize ALL NON-POINTER VARIABLES in your parameter structure

      obj%win_len   = 0.08
      obj%vel_beg   = 1500.0
      obj%vel_end   = 5000.0
      obj%vel_inc   = 20.0
      obj%normalize = 'YES'
      obj%opt_print = 'NO'
      obj%opt_hor   = 'PICKFILE'
      obj%cons_time = FNIL
      obj%path_pick = PATHCHECK_EMPTY

      obj%print_lun = pc_get_lun()

      call hvel_update (obj)
      return
      end subroutine hvel_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine hvel_update (obj)
      implicit none
      type(hvel_struct),intent(inout),target :: obj        ! arguments

      integer                   :: numtr                   ! local
 
      character(len=8)          :: stattype                ! local
      integer                   :: nhx2,nhy2               ! local
      real            ,pointer  :: pstatics(:)             ! local

      integer                   :: status                  ! local
      character(len=120)        :: msg                     ! local
      type(dio_struct),pointer  :: dio_obj                 ! local

      nullify (dio_obj) ! jpa

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    numtr = inil
    call pc_get_global('numtr'   ,numtr)        
    call pc_get_global('gathered',obj%gathered) 
    call pc_get_global('ndpt'   , obj%ndpt)         
    call pc_get_global('dt'     , obj%dt)           
    call pc_get_global('tstrt'  , obj%tstrt)        
    call pc_get_global('nwih'   , obj%nwih) 

    if (numtr == inil) call pc_error ("NUMTR global hasn't been set.")
    if (obj%nwih == inil) call pc_error ("NWIH global hasn't been set.")
    if (obj%ndpt == inil) call pc_error ("NDPT global hasn't been set.")
    if (obj%tstrt == fnil) call pc_error ("TSTRT global hasn't been set.")
    if (obj%dt   == fnil) call pc_error ("DT global hasn't been set.")

    call pc_get ('WIN_LEN',   obj%win_len)
    call pc_get ('VEL_BEG',   obj%vel_beg)
    call pc_get ('VEL_END',   obj%vel_end)
    call pc_get ('VEL_INC',   obj%vel_inc)
    call pc_get ('NORMALIZE', obj%normalize)
    call pc_get ('OPT_PRINT', obj%opt_print)
    call pc_get ('OPT_HOR',   obj%opt_hor)
    call pc_get ('CONS_TIME', obj%cons_time)
    call pc_get ('PATH_PICK', obj%path_pick)

    call string_to_upper (obj%normalize)
    call string_to_upper (obj%opt_print)


    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!


    if (obj%win_len <= 0.0) then    
      call pc_error (' HVEL error: WIN_LEN must be > 0 ' )
    end if 

    if (obj%vel_beg <= 0.0) then    
      call pc_error (' HVEL error: VEL_BEG must be > 0 ' )
    end if

    if (obj%vel_end < obj%vel_beg) then    
      call pc_error (' HVEL error: VEL_END must be >= VEL_BEG ' )
    end if

    if (obj%vel_inc <= 0.0) then    
      call pc_error (' HVEL error: VEL_INC must be > 0 ' )
    end if 


    if (obj%opt_hor == 'PICKFILE ') then

      call pc_put_sensitive_field_flag ('CONS_TIME', .false.)
      call pc_put_sensitive_field_flag ('PATH_PICK', .true.)

      if ( obj%path_pick == PATHCHECK_EMPTY) then
         call pc_error (' HVEL error: Require PICKFILE  ')
      end if

      if (obj%path_pick /= PATHCHECK_EMPTY) then
         call pathcheck ('PATH_PICK', obj%path_pick, status=status)
         call dio_open_read (dio_obj, obj%path_pick, status, msg)
         call dio_close     (dio_obj)
        ! status = 0 -- open successful
        if ( status /= 0) call pc_error (msg)
      end if

    else 

      call pc_put_sensitive_field_flag ('CONS_TIME', .true.)
      call pc_put_sensitive_field_flag ('PATH_PICK', .false.)

      if (obj%cons_time <= 0.0 ) then   
         call pc_error (' HVEL error: CONS_TIME must be > 0  ')  
      end if

    end if

    if ( .not. obj%gathered) then
        call pc_error (' HVEL error: Require input to be gathered ')  
    end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('OPT_HOR', opt_hor_options,   &
                                  opt_hor_noptions) 

      call pc_put_options_field ('NORMALIZE', normalize_options,   &
                                  normalize_noptions)

      call pc_put_options_field ('OPT_PRINT', opt_print_options,   &
                                  opt_print_noptions)

    call pc_put ('WIN_LEN',   obj%win_len)
    call pc_put ('VEL_BEG',   obj%vel_beg)
    call pc_put ('VEL_END',   obj%vel_end)
    call pc_put ('VEL_INC',   obj%vel_inc)
    call pc_put ('NORMALIZE', obj%normalize)
    call pc_put ('OPT_PRINT', obj%opt_print)
    call pc_put ('OPT_HOR',   obj%opt_hor)

    call pc_put ('CONS_TIME', obj%cons_time)
    call pc_put ('PATH_PICK', obj%path_pick)

    obj%nwin = int(obj%win_len/obj%dt) 
    obj%nvel = int((obj%vel_end - obj%vel_beg)/obj%vel_inc) + 1

    call pc_put_global('ndpt'   , obj%nvel)         
    call pc_put_global('dt'     , obj%vel_inc/1000.0 )           
    call pc_put_global('tstrt'  , obj%vel_beg/1000.0)        

    call pc_put_global ('numtr'   , 1)
    call pc_put_global ('gathered', .false. ) 


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!! Conditionally deallocate all dependent arrays 

     if (associated(obj%t_interp)) deallocate (obj%t_interp)

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.

      obj%ntr_sum = 0
      obj%nx = 1
      obj%ny = 1
      obj%hdr_cmp_inl = 7 
      obj%hdr_cmp_crl = 8

!  Allocate your permanent memory 

      if (obj%opt_hor == 'PICKFILE ' .and.                        &
          obj%path_pick /= PATHCHECK_EMPTY) then
         nullify  (pstatics) 

         call statio_read_file(obj%path_pick,stattype,            &
         obj%hdr_cmp_inl, obj%hdr_cmp_crl,                        &  
         nhx2,nhy2, obj%x0,obj%y0,obj%xinc,obj%yinc,              &
         obj%nx,obj%ny, pstatics, status, msg) 

         if (obj%hdr_cmp_inl < 1 ) then
           call pc_error (' HVEL error: HDR_CMP_INL must be >= 1 ')
         end if 
         if (obj%hdr_cmp_inl > obj%nwih ) then
            call pc_error (' HVEL error: HDR_CMP_INL must be <= ',obj%nwih)
         end if

         if (obj%hdr_cmp_crl < 1 ) then
           call pc_error (' HVEL error: HDR_CMP_CRL must be >= 1 ')
         end if
         if (obj%hdr_cmp_crl > obj%nwih ) then
           call pc_error (' HVEL error: HDR_CMP_CRL must be <= ',obj%nwih)
         end if

         allocate(obj%t_interp(obj%nx,obj%ny), stat=status)
         if (status /= 0) call pc_error  &
            ('error allocating TVALUE to',obj%nx*obj%ny,'array elements')
 
         call hvel_interp_pick(obj, pstatics)

      end if

     if (pc_do_not_process_traces()) return   ! in case of allocation errors.


!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine hvel_update


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

!<execute_only>

      subroutine hvel (obj,ntr,hd,tr)
      implicit none
      type(hvel_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer        :: ix, iy, iv, it, j, indx                 ! local
      real           :: scr(obj%ndpt), yy(obj%ndpt)             ! local
      real           :: tzero, czero, vi, v2, xpwr              ! local
      real           :: x, x2, t0, tx, xindex, d, trm, trp      ! local
      real           :: s1, amx, amn                            ! local

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call hvel_wrapup (obj)
      end if

      if (ntr == 0) then 
        return  
      endif 
 
      if (ntr < 4) then 
        obj%ntr_sum = obj%ntr_sum + 1                            
        tr(1:obj%ndpt,1) = 0.0 
        hd(1,1) = obj%ntr_sum 
        hd(2,1) = 1
        hd(5,1) = ntr
        hd(25,1) = 0.0
        hd(64,1) = obj%nvel
        ntr = 1 
        return  
      endif 

!     get window time

      if (obj%opt_hor == 'PICKFILE ') then
         ix = (hd(obj%hdr_cmp_inl,1) - obj%x0)/obj%xinc + 1
         iy = (hd(obj%hdr_cmp_crl,1) - obj%y0)/obj%yinc + 1
         ix = max(1, min(ix, obj%nx))
         iy = max(1, min(iy, obj%ny))
         czero = obj%t_interp(ix,iy)
      else
         czero = obj%cons_time
      end if
 
      if (obj%opt_print == 'YES') write (obj%print_lun, *)    &
         'LOCATION VALUE = ', hd(obj%hdr_cmp_inl,1),'   ',    &
         hd(obj%hdr_cmp_crl,1), '   TIME = ', czero
!
      tzero = czero - float(obj%nwin/2)*obj%dt 

!     start of semblance calculation - velocity loop

      do iv = 1, obj%nvel 
        vi = obj%vel_beg + (iv - 1)*obj%vel_inc  
        v2 = vi**2 

        xpwr = 0.0 
        scr(1:obj%nwin) = 0.0 

        do ix = 1, ntr                !     offset loop
          x = hd(6,ix) 
          x2 = x**2 

          do it = 1, obj%nwin         !     time loop  
            t0 = tzero + float(it - 1)*obj%dt 
            tx = sqrt(t0**2 + x2/v2) 

!     compute delta-t, move sample from trace & accumulate

            xindex = tx/obj%dt 
            indx = xindex 
            indx = max(indx,1) 
            indx = min(indx,obj%ndpt) 
            d = xindex - indx 
            trm = (1.0 - d)*tr(indx,ix) 
            trp = d*tr(indx+1,ix) 
            scr(it) = scr(it) + trm + trp 
            xpwr = xpwr + trm**2 + trp**2 
          end do 
        end do 

!     compute semblance

        s1 = 0.0 
        s1 = sum(scr(:obj%nwin)**2) 
        if (xpwr <= 0.0) then 
          yy(iv) = 0.0 
        else 
          yy(iv) = s1/xpwr/float(ntr) 
        endif 
        if (obj%opt_print /= 'YES') cycle  
        write (obj%print_lun, *) 'VI=', vi, '   SEMBLANCE=', yy(iv) 
      end do 

!      output

      tr(1:obj%ndpt,1) = 0.0  
      tr(1:obj%nvel,1) = yy(1:obj%nvel) 
      if (obj%normalize == 'YES') then 

!       shift and scale output amplitudes

        amx = 0.0 
        amn = 100.0 
        do j = 1, obj%nvel 
          amn = min(abs(tr(j,1)),amn) 
          amx = max(abs(tr(j,1)),amx) 
        end do 
        amx = amx - amn 
        if (amx > 0.0) then 
          tr(:obj%nvel,1) = (tr(:obj%nvel,1)-amn)/amx 
        endif 
      endif 
 
      obj%ntr_sum = obj%ntr_sum + 1 
      hd(1,1) = obj%ntr_sum 
      hd(2,1) = 1
      hd(5,1) = ntr
      hd(64,1) = obj%nvel 
      ntr = 1 

      call lav_set_hdr (hd, tr, obj%ndpt, 1)

      return
      end subroutine hvel


      subroutine hvel_interp_pick(obj, tvalue)

!.... Read PICK file and interpolate the missing values
!.... Assume the time value is in msec.

      implicit none
      type(hvel_struct),intent(inout) :: obj              ! arguments
      real, intent(inout)  :: tvalue(obj%nx,obj%ny)       ! arguments

      real              :: loc(max(obj%nx,obj%ny))        ! local
      real              :: val(max(obj%nx,obj%ny))        ! local
      real              :: loc_sort(max(obj%nx,obj%ny))   ! local
      real              :: val_sort(max(obj%nx,obj%ny))   ! local
      real              :: xp(max(obj%nx,obj%ny))         ! local
      real              :: yp(max(obj%nx,obj%ny))         ! local
      real              :: start, end                     ! local

      integer           :: itmp(max(obj%nx,obj%ny))       ! local
      integer           :: npt, i, j, k  ! local
           
!...  interpolate inline picks first

      start = obj%x0
      end = start + (obj%nx-1)*obj%xinc
      do j = 1, obj%ny
        npt = 0
        do i = 1, obj%nx
            if ( tvalue(i,j) /= FNIL) then
              npt = npt + 1
              loc(npt) = obj%x0 + (i-1)* obj%xinc
              val(npt) = tvalue(i,j)/1000.    ! convert time to sec
            end if
         end do
         if ( npt > .25*obj%nx .and. npt > 1) then

           itmp = (/(k, k=1, npt)/) 
           call sort_qkisort (npt, loc, itmp)
           do k = 1, npt
             loc_sort(k) = loc(itmp(k))
             val_sort(k) = val(itmp(k))
           end do

           call interp_1d_var_lin_real (loc_sort, val_sort, npt,  &
              xp, yp, obj%nx, start, end)
           obj%t_interp(1:obj%nx, j) = yp(1:obj%nx)
         end if
      end do


!...  interpolate xline picks first

      start = obj%y0
      end = start + (obj%ny-1)*obj%yinc
      do i = 1, obj%nx
        npt = 0
        do j = 1, obj%ny
            if ( tvalue(i,j) /= FNIL) then
              npt = npt + 1
              loc(npt) = obj%y0 + (j-1)* obj%yinc
              val(npt) = tvalue(i,j)/1000.   ! convert time to sec
            end if
         end do
         if ( npt > .25*obj%ny .and. npt > 1) then

           itmp = (/(k, k=1, npt)/) 
           call sort_qkisort (npt, loc, itmp)
           do k = 1, npt
             loc_sort(k) = loc(itmp(k))
             val_sort(k) = val(itmp(k))
           end do

           call interp_1d_var_lin_real (loc_sort, val_sort, npt,  &
              xp, yp, obj%ny, start, end)
           obj%t_interp(i,1:obj%ny) = yp(1:obj%ny)
         end if
      end do
      
      end subroutine hvel_interp_pick


!</execute_only>




!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine hvel_wrapup (obj)
      implicit none
      type(hvel_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine hvel_wrapup

!</execute_only>


!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module hvel_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

