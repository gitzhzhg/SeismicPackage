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
!                      C P S   P R O C E S S                  
!
! Name       : NORM (Normalize Traces)
! Category   : amplitude_mod
! Written    : 1986-07-29   by: Richard S. Day, Doug Hanson
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Applies a multiplicative scale factor to traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                       GENERAL DESCRIPTION                   
!
! 
! NORM scales input traces by a multiplicative scale factor that is calculated 
! using a method determined by MODE.
! 
! If MODE = LAV,   scale each trace individually to make its LAV equal to VALUE.
!
! If MODE = SCALE, multiply each trace individually by the factor VALUE.
!
! If MODE = MAX,   scale each ensemble to make its maximum LAV equal to VALUE.
!
! If MODE = L1,    scale each ensemble to make its L1 norm equal to VALUE.
!
! If MODE = L2,    scale each ensemble to make its L2 norm equal to VALUE.
!
! If MODE = MED,   scale each ensemble to make its median absolute value equal
!                  to VALUE.
!
! MODEs MAX, L1, L2, and MED calculate a single scale factor for each input 
! ensemble of traces and then apply that scale factor to all the traces in the 
! ensemble.  MODEs LAV and SCALE calculate and apply a single scale factor 
! for each input trace regardless of the ensemble size.    
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS                      
!
! In MODEs MAX, L1, L2, and MED, the traces must be input in functional
! ensembles.  The user must insure that traces are input with the desired 
! grouping.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                   TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace or multiple-trace process.
!
! This process requires traces to be input in gathers for MODEs MAX, L1, L2, 
! and MED.
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                  TRACE OUTPUT CHARACTERISTICS             
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs traces with same gather status as the input traces.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                 GLOBAL PARAMETERS USED OR CHANGED          
!
! 
! Name     Description                       Action taken
! ----     -----------                       ------------
! NDPT     number of sample values in trace   used but not changed
! TSTRT    starting time on trace             used but not changed
! DT       trace sample interval              used but not changed

! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#           Description                Action taken
! ----           -----------                ------------
! 25             LAV                        set if trace data is modified
! HDR_FLAG       Flagword for scaling       used but not changed
! HDR_FLAG_CALC  Flagword for calculation   used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY                    
! 
!     Date       Author    Description
!     ----       ------    -----------
!020. 2006-06-12  B. Menger   Removed Unused Variables.
! 19. 2002-08-29 Stoeckley Add HDR_SCALE parameter.
! 18. 2001-04-26 Stoeckley Fix some incorrect XML tags.
! 17. 2000-12-08 Stoeckley Change wrapup flag.
! 16. 2000-07-26 Coleman   Added GUI and set header word 25 to new LAV
! 15. 2000-03-10 Coleman   Added support for combo boxes and textbox sensitivity
! 14. 1999-12-10 Coleman   Converted to new system
! 13. 1998-11-20 Vunderink Begin using the f90 compiler.
! 12. 1998-09-10 Vunderink Added header flag HFC# parameter.
! 11. 1998-08-18 Vunderink Added header flag HF# parameter.
! 10. 1996-07-15 K.Goodger Declare AMP parameter as type integer.  F90
!                           conversion.
!  9. 1991-08-06 B.Troutt  Comment out call to ASSIGN to avoid unresolved 
!                           external (not needed for UNICOS anyway).
!  8. 1990-04-25 M.Howard  Fix to use median of NON-ZERO values and to use
!                           norm of whole qroup of traces in window.
!  7. 1990-03-04 M.Howard  Add MED option.
!  6. 1989-07-19 H.Ball    Change all APUTWA to PUTWA
!  5. 1989-06-19 D.Hanson  Change printout format from 10 groups-Line to 198.
!  4. 1988-10-21 H.Ball    NWIH and NWPT  Conversion
!  3. 1988-04-10 D.Hanson  Include only non zero values in L1 and L2
!                           norms, add SCALE option.
!  2. 1987-03-27 D.Hanson  Modified to scale groups of traces
!                           with several different norms.
!  1. 1986-07-29           Added MEMPRT to the setup  .
!    
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                     PORTABILITY LIMITATIONS               
!
! No known limitations.
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                   SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
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
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if there is a fatal error in this process.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                 ALTERNATE INTERNAL CALLING METHODS          
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                         PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS NORM Process/NC=80>
!
!                                Normalize Traces
!                Applies a multiplicative scale factor to traces
!
!                          MODE =~~~~~~~~~~`CCCCC
!
!                          VALUE =~~~~~~~~~`FFFFFFFFF
!
!                          TIM_BEG =~~~~~~~`FFFFFFFFF
!
!                          TIM_END =~~~~~~~`FFFFFFFFF
!
!                          TR_BEG =~~~~~~~~`IIIIIIIII
!
!                          TR_END =~~~~~~~~`IIIIIIIII
!
!                          HDR_FLAG =~~~~~~`IIIIIIIII
!
!                          HDR_FLAG_CALC = `IIIIIIIII
!
!                          HDR_SCALE =~~~~~`IIIIIIIII
!
!                          PRINT =~~~~~~~~~`CCC
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Method to use for scaling traces. </Tip>
! Default = LAV  
! Allowed = LAV
! Allowed = SCALE
! Allowed = MAX
! Allowed = L1
! Allowed = L2
! Allowed = MED
! If MODE = LAV, scale each trace individually to make its LAV equal to VALUE.
!
! If MODE = SCALE, multiply each trace individually by the factor VALUE.
!
! If MODE = MAX, scale each ensemble to make its maximum LAV equal to VALUE.
!
! If MODE = L1, scale each ensemble to make its L1 norm equal to VALUE.
!
! If MODE = L2, scale each ensemble to make its L2 norm equal to VALUE.
!
! If MODE = MED, scale each ensemble to make its median absolute value equal to
! VALUE.
!</Help>
!
!<Help KEYWORD="VALUE">
!<Tip> Amplitude parameter to use in scaling traces. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Top of trace window to use in computing scale factor, in seconds. </Tip>
! Default = TSTRT
! Allowed = real 
! If TIM_BEG < TSTRT, then TSTRT is used.  If TIM_BEG > time of last sample,
! then the time of the last sample is used.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Bottom of window to use in computing scale factor, in seconds. </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG
! If TIM_END < TSTRT, then TSTRT is used.  If TIM_END > time of last sample,
! then the time of the last sample is used.
!</Help>
!
!<Help KEYWORD="TR_BEG">
!<Tip> First trace in ensemble to use in computing scale factor. </Tip>
! Default = 1
! Allowed = int > 0
! If TR_BEG > last trace, then the last trace is used.  TR_BEG is ignored if
! MODE = LAV or SCALE.
!</Help>
!
!<Help KEYWORD="TR_END">
!<Tip> Last trace in ensemble to use in computing scale factor. </Tip>
! Default = 0
! Allowed = int >= TR_BEG or == 0
! If TR_END = 0 or > last trace, then the last trace is used.  TR_END is ignored
! if MODE = LAV or SCALE.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting traces flagged for scaling. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are scaled.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are scaled and used in the calculation of
! the scale factor.  HDR_FLAG and HDR_FLAG_CALC must not both be non-zero.
!</Help>
!
!<Help KEYWORD="HDR_FLAG_CALC">
!<Tip> Header word denoting traces flagged to use for calculation. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_FLAG_CALC = 0, then all traces are used in calculation of the scale
! factor.  Otherwise, only traces with a flag set in header word HDR_FLAG_CALC
! are used in the calculation.  HDR_FLAG and HDR_FLAG_CALC must not both be
! non-zero.  HDR_FLAG_CALC is ignored if MODE = LAV or SCALE.
!</Help>
!
!<Help KEYWORD="HDR_SCALE">
!<Tip> Header word in which the scale factor used is placed. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_SCALE > 0, then the scale factor used to scale that trace is placed
! into that header word.
!</Help>
!
!<Help KEYWORD="PRINT">
!<Tip> Option whether to print scale factors. </Tip>
! Default = NO
! Allowed = YES/NO 
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
! NOTES FOR CONVERSION PROGRAMMER
!


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module norm_module
    use pc_module
    use named_constants_module
    use string_module
    use getlun_module
    use mth_module
    use median_module
    use lav_module

    implicit none
    private
    public  :: norm_create       ! uses the parameter cache.
    public  :: norm_initialize
    public  :: norm_update       ! uses the parameter cache.
    public  :: norm_delete
    public  :: norm_dump_object
    public  :: norm              ! main execution (trace processing) routine.
    private :: norm_calc_lav
    private :: norm_calc_scale
    private :: norm_calc_l1
    private :: norm_calc_l2
    private :: norm_calc_max
    private :: norm_calc_med
    private :: norm_apply_scale
    public  :: norm_wrapup


    character(len=100),public,save :: norm_IDENT = &
     '$Id: norm.f90,v 1.20 2006/06/12 13:03:53 Menger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    integer, parameter      :: LRESULTS = 6
    type,public :: norm_struct            
 
        private
        logical                     :: skip_wrapup      ! wrapup flag.
        character(len=5)            :: mode             ! pc - LAV,SCALE,MAX,
                                                        !      L1,L2,MED
        logical                     :: print            ! pc - YES/NO 
        integer                     :: hdr_flag         ! pc - 0 - NWIH
        integer                     :: hdr_flag_calc    ! pc - 0 - NWIH
        integer                     :: hdr_scale        ! pc - 0 - NWIH
        integer                     :: tr_beg           ! pc - int > 0
        integer                     :: tr_end           ! pc - int >= 0
        real                        :: tim_beg          ! pc - real 
        real                        :: tim_end          ! pc - real > TIM_BEG
        real                        :: value            ! pc - real > 0.0
        integer                     :: ndpt             ! global
        integer                     :: nwih             ! global
        real                        :: dt               ! global
        real                        :: tstrt            ! global
        integer                     :: idpt1            ! dependent variable
        integer                     :: ndpt1            ! dependent variable
        integer                     :: mgroups          ! dependent variable
        integer                     :: mtraces          ! dependent variable
        integer                     :: ngroups          ! dependent variable
        integer                     :: ntraces          ! dependent variable
        integer                     :: nresults         ! dependent variable
        integer                     :: tmp_lun          ! dependent variable
        real   ,dimension(LRESULTS) :: resscal          ! dependent variable
        integer,dimension(LRESULTS) :: restrcs          ! dependent variable

    end type norm_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

    integer,parameter :: MAX_LINE  = 132
    integer,save      :: print_lun =   6


contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine norm_create (obj)
    implicit none
    type(norm_struct),pointer :: obj       ! arguments

    allocate (obj)

    call norm_initialize (obj)

    return
end subroutine norm_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine norm_delete (obj)
    implicit none
    type(norm_struct),pointer :: obj       ! arguments

    call norm_wrapup (obj)

    deallocate(obj)

    return
end subroutine norm_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine norm_initialize (obj)
    implicit none
    type(norm_struct),intent(inout) :: obj

    call pc_get_global( 'ndpt' , obj%ndpt  )
    call pc_get_global( 'tstrt', obj%tstrt )
    call pc_get_global( 'dt'   , obj%dt    )

    obj%value         = 1.0 
    obj%tim_beg       = obj%tstrt 
    obj%tim_end       = (obj%ndpt - 1) * obj%dt + obj%tstrt 
    obj%tr_beg        = 1 
    obj%tr_end        = 0 
    obj%hdr_flag      = 0 
    obj%hdr_flag_calc = 0 
    obj%hdr_scale     = 0 
    obj%mode          = 'LAV' 
    obj%print         = .false. 
    obj%idpt1         = 1
    obj%ndpt1         = obj%ndpt
    obj%mgroups       = 1 
    obj%mtraces       = 1 
    obj%ngroups       = 0 
    obj%ntraces       = 0 
    obj%nresults      = 0
    obj%restrcs       = 0
    obj%resscal       = 0.0

    call norm_update (obj)

    return
end subroutine norm_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine norm_update (obj)
    implicit none
    type(norm_struct),intent(inout),target :: obj            ! arguments

    integer                 :: idpt2        ! local
    integer                 :: ierr         ! local
    integer                 :: nscratch     ! local
    character(len=MAX_LINE) :: msg          ! local

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get( 'MODE'         , obj%mode          )
    call pc_get( 'VALUE'        , obj%value         )
    call pc_get( 'TIM_BEG'      , obj%tim_beg       )
    call pc_get( 'TIM_END'      , obj%tim_end       )
    call pc_get( 'TR_BEG'       , obj%tr_beg        )
    call pc_get( 'TR_END'       , obj%tr_end        )
    call pc_get( 'HDR_FLAG'     , obj%hdr_flag      )
    call pc_get( 'HDR_FLAG_CALC', obj%hdr_flag_calc )
    call pc_get( 'HDR_SCALE'    , obj%hdr_scale     )
    call pc_get( 'PRINT'        , obj%print         )

    call string_to_upper( obj%mode )

    call pc_get_global( 'ndpt' , obj%ndpt  )
    call pc_get_global( 'nwih' , obj%nwih  )
    call pc_get_global( 'tstrt', obj%tstrt )
    call pc_get_global( 'dt'   , obj%dt    )

!!------------------------- verify parameters --------------------------!!
!!------------------------- verify parameters --------------------------!!
!!------------------------- verify parameters --------------------------!!


!
!---Check MODE: string = 'LAV', 'SCALE', 'MAX', 'L1', 'L2', or 'MED'
!

    nscratch = 0

    if     ( obj%mode(1:2) == 'LA' ) then
        obj%mode = 'LAV'
    else if( obj%mode(1:2) == 'L1' ) then
        obj%mode = 'L1'
    else if( obj%mode(1:2) == 'L2' ) then
        obj%mode = 'L2'
    else if( obj%mode(1:2) == 'MA' ) then
        obj%mode = 'MAX'
    else if( obj%mode(1:2) == 'ME' ) then
        obj%mode = 'MED'
        nscratch = (obj%tr_end - obj%tr_beg + 1) * obj%ndpt1
    else if( obj%mode(1:1) == 'S'  ) then
        obj%mode = 'SCALE'
    else
        call pc_error( msg1 = 'Bad value MODE - ' // obj%mode )
        obj%mode = 'LAV'
    endif
!
!---Check VALUE: real > 0.0
!
    if( obj%value <= 0.0 ) then 
        call pc_error( msg1 = 'Bad value VALUE',   &
                       var1 = obj%value )
    endif
!
!---Check TIM_END: real > tim_beg
!
    if( obj%tim_end <= obj%tim_beg ) then
        call pc_error( msg1 = 'Bad value TIM_END',   &
                       var1 = obj%tim_end )
    endif
!
!---Check HDR_FLAG: 0 <= int < nwih
!
    if( (obj%hdr_flag < 0) .or. (obj%hdr_flag > obj%nwih) ) then
        call pc_error( msg1 = 'Bad value HDR_FLAG',   &
                       var1 = obj%hdr_flag )
    endif
!
!---Ignore hdr_flag_calc, tr_beg, & tr_end if mode is LAV or SCALE
!
    if( obj%mode /= 'LAV' .and. obj%mode /= 'SCALE' ) then
!
!-------Check HDR_FLAG_CALC: 0 <= int < nwih
!
        if( (obj%hdr_flag_calc<0).or.(obj%hdr_flag_calc>obj%nwih) ) then
            call pc_error( msg1 = 'Bad value HDR_FLAG_CALC',   &
                           var1 = obj%hdr_flag_calc )
        endif
!
!-------Check TR_BEG: int > 0
!
        if( obj%tr_beg <= 0 ) then
            call pc_error( msg1 = 'Bad value TR_BEG',   &
                           var1 = obj%tr_beg )
        endif
!
!-------Check TR_END: int >= 0 
!
        if( (obj%tr_end < obj%tr_beg) .and. (obj%tr_end /= 0) ) then
            call pc_error( msg1 = 'Bad value TR_END',   &
                           var1 = obj%tr_end )
        endif

    endif
!
!---Insure that either hdr_flag or hdr_flag_calc or both are zero
!
    if( (obj%hdr_flag /= 0) .and. (obj%hdr_flag_calc /= 0) ) then 
        call pc_error( 'both hdr_flag and hdr_flag_calc are non-zero' )
        return  
    endif 
!
!---Check HDR_SCALE: 0 <= int < nwih
!
    if( (obj%hdr_scale < 0) .or. (obj%hdr_scale > obj%nwih) ) then
        call pc_error( msg1 = 'Bad value HDR_SCALE',   &
                       var1 = obj%hdr_scale )
    endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


    call pc_put_options_field( 'PRINT', (/ 'YES'  , 'NO '   /), 2 )
    call pc_put_options_field( 'MODE' , (/ 'LAV  ', 'SCALE', 'MAX  ',      &
                                           'L1   ', 'L2   ', 'MED  ' /), 6 )

    call pc_put( 'MODE'         , obj%mode          )
    call pc_put( 'VALUE'        , obj%value         )
    call pc_put( 'TIM_BEG'      , obj%tim_beg       )
    call pc_put( 'TIM_END'      , obj%tim_end       )
    call pc_put( 'TR_BEG'       , obj%tr_beg        )
    call pc_put( 'TR_END'       , obj%tr_end        )
    call pc_put( 'HDR_FLAG'     , obj%hdr_flag      )
    call pc_put( 'HDR_FLAG_CALC', obj%hdr_flag_calc )
    call pc_put( 'HDR_SCALE'    , obj%hdr_scale     )
    call pc_put( 'PRINT'        , obj%print         )

    if( obj%mode == 'LAV' .or. obj%mode == 'SCALE' ) then
        call pc_put_sensitive_field_flag( 'TR_BEG'       , .false. )
        call pc_put_sensitive_field_flag( 'TR_END'       , .false. )
        call pc_put_sensitive_field_flag( 'HDR_FLAG_CALC', .false. )
    else
        call pc_put_sensitive_field_flag( 'TR_BEG'       , .true. )
        call pc_put_sensitive_field_flag( 'TR_END'       , .true. )
        call pc_put_sensitive_field_flag( 'HDR_FLAG_CALC', .true. )
    endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.

    obj%idpt1    = max0( nint( (obj%tim_beg - obj%tstrt) / obj%dt )+1, 1 ) 
    obj%idpt1    = min0( obj%idpt1, obj%ndpt )
    idpt2        = min0( nint( (obj%tim_end - obj%tstrt) / obj%dt )+1, obj%ndpt)
    idpt2        = max0( obj%idpt1, idpt2 )
    obj%ndpt1    = idpt2 - obj%idpt1 + 1 

!  SET UP WORD ADDRESSABLE FILE WHICH CONTAINS HEADER WORD INFO 
!  FOR SORTING AND ORDERING DATA 

    print_lun = pc_get_lun()

    if( obj%print ) then
        call getlun( obj%tmp_lun, ierr )

        if( ierr == 0 ) then
            open( unit=obj%tmp_lun, action='READWRITE', iostat=ierr, &
                  status='SCRATCH', recl=MAX_LINE )
            if( ierr /= 0 ) then
                write( msg, '("iostat = ",I4,", opening temp dump file")' ) ierr
                call pc_error( msg )
                return
            endif
        else
            call pc_error( 'getlun failed attempting to open temp dump file' )
            return
        endif
    
        if( obj%mgroups == 1 ) then
            write(obj%tmp_lun,*) 'NUMBER OF TRACES AND SCALE FACTOR PER GROUP'
            write(obj%tmp_lun,*) ' '
            write(obj%tmp_lun,'(" FIRST FIRST", 6("           SCALE    "))' )
            write(obj%tmp_lun,'(" GROUP TRACE", 6("   NTR     FACTOR   "))' )
        endif
    endif


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine norm_update


!!--------------------------- norm_dump_object ---------------------------!!
!!--------------------------- norm_dump_object ---------------------------!!
!!--------------------------- norm_dump_object ---------------------------!!


subroutine norm_dump_object( obj )
    !
    type(norm_struct), intent(in) :: obj       ! arguments
    !
    ! - Begin norm_dump_object
    !
    write( print_lun, '( /"***** BEGIN NORM OBJECT DUMP ", 51("*") )' )
    write( print_lun, '( "    skip_wrapup   = ", l1   )' ) obj%skip_wrapup
    write( print_lun, '( "    mode          = ", a5   )' ) obj%mode
    write( print_lun, '( "    print         = ", l1   )' ) obj%print
    write( print_lun, '( "    hdr_flag      = ", i4   )' ) obj%hdr_flag
    write( print_lun, '( "    hdr_flag_calc = ", i4   )' ) obj%hdr_flag_calc
    write( print_lun, '( "    hdr_scale     = ", i4   )' ) obj%hdr_scale
    write( print_lun, '( "    tr_beg        = ", i4   )' ) obj%tr_beg
    write( print_lun, '( "    tr_end        = ", i4   )' ) obj%tr_end
    write( print_lun, '( "    tim_beg       = ", f8.3 )' ) obj%tim_beg
    write( print_lun, '( "    tim_end       = ", f8.3 )' ) obj%tim_end
    write( print_lun, '( "    value         = ", f8.3 )' ) obj%value
    write( print_lun, '( "    ndpt          = ", i4   )' ) obj%ndpt
    write( print_lun, '( "    nwih          = ", i4   )' ) obj%nwih
    write( print_lun, '( "    dt            = ", f8.3 )' ) obj%dt
    write( print_lun, '( "    tstrt         = ", f8.3 )' ) obj%tstrt
    write( print_lun, '( "    idpt1         = ", i4   )' ) obj%idpt1
    write( print_lun, '( "    ndpt1         = ", i4   )' ) obj%ndpt1
    write( print_lun, '( "    mgroups       = ", i4   )' ) obj%mgroups
    write( print_lun, '( "    mtraces       = ", i4   )' ) obj%mtraces
    write( print_lun, '( "    ngroups       = ", i4   )' ) obj%ngroups
    write( print_lun, '( "    ntraces       = ", i4   )' ) obj%ntraces
    write( print_lun, '( "    nresults      = ", i4   )' ) obj%nresults
    write( print_lun, '( "    tmp_lun       = ", i4   )' ) obj%tmp_lun
    write( print_lun, '( "    restrcs       = ", i4, 6i8 / 16x, 7i8   )' ) &
                          obj%restrcs
    write( print_lun, '( "    resscal       = ", 7f8.3   / 20x, 7f8.3 )' ) &
                          obj%resscal
    write( print_lun, '( "***** END NORM OBJECT DUMP ", 53("*") / )' )
    !
end subroutine norm_dump_object


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


subroutine norm( obj, ntr, hd, tr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(inout) :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data


    integer :: ierr  ! local

!-------------------------------------------------------------------------------

    ! wrapup and abort if there are no traces to process
    if( ntr <= 0 ) then
         call norm_wrapup( obj )
         return
    end if
! 
! NOISE SPIKES SHOULD BE REMOVED PRIOR TO CALLING NORM 
! 
    select case( obj%mode )
        case( 'LAV' )
            call norm_calc_lav( obj, ntr, hd, tr )
        case( 'SCALE' )
            call norm_calc_scale( obj, ntr, hd, tr )
        case( 'L1' )
            call norm_calc_l1( obj, ntr, hd, tr )
        case( 'L2' )
            call norm_calc_l2( obj, ntr, hd, tr )
        case( 'MAX' )
            call norm_calc_max( obj, ntr, hd, tr )
        case( 'MED' )
            call norm_calc_med( obj, ntr, hd, tr, ierr )
            if( ierr /= 0 ) then
                ntr = FATAL_ERROR
                return
            endif
    end select
        
    return  
end subroutine norm


!!------------------------ do lav normalization ----------------------------!!
!!------------------------ do lav normalization ----------------------------!!
!!------------------------ do lav normalization ----------------------------!!


subroutine norm_calc_lav( obj, ntr, hd, tr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    
! local variables

    integer      :: imx
    integer      :: jtrc      ! loop index for traces
    real         :: scale

!-------------------------------------------------------------------------------

    do jtrc = 1, ntr 
        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        endif

        imx   = mth_isamax( obj%ndpt1, tr(obj%idpt1:,jtrc), 1 ) 
        scale = abs( tr(imx+obj%idpt1-1,jtrc) ) 

        call  norm_apply_scale( obj, 1, hd(1:,jtrc:), tr(1:,jtrc:), scale )
    end do 
    return
end subroutine norm_calc_lav


!!------------------------ do scale normalization ----------------------------!!
!!------------------------ do scale normalization ----------------------------!!
!!------------------------ do scale normalization ----------------------------!!


subroutine norm_calc_scale( obj, ntr, hd, tr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    
! local variables

    integer      :: jtrc      ! loop index for traces
    real         :: scale

!-------------------------------------------------------------------------------

    do jtrc = 1, ntr 
        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        endif

        scale = 1.0
        call  norm_apply_scale( obj, 1, hd(1:,jtrc:), tr(1:,jtrc:), scale )
    end do 
    return
end subroutine norm_calc_scale


!!------------------------- do L1 normalization ----------------------------!!
!!------------------------- do L1 normalization ----------------------------!!
!!------------------------- do L1 normalization ----------------------------!!


subroutine norm_calc_l1( obj, ntr, hd, tr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    
! local variables

    integer      :: idpt      ! loop index for data points
    integer      :: jtrc      ! loop index for traces
    integer      :: jtrc1
    integer      :: jtrc2
    integer      :: nsum 
    real         :: scale
    real         :: sum 

!-------------------------------------------------------------------------------

    jtrc1 = obj%tr_beg 
    jtrc1 = min0( ntr, jtrc1 ) 
    jtrc2 = obj%tr_end 
    if( jtrc2 == 0 ) jtrc2 = ntr 
    jtrc2 = min0( ntr, jtrc2 ) 
    nsum  = 0 
    sum   = 0. 
    scale = 0.

!  SCALE EACH TRACE IN GROUP BY L1 NORM OF DATA WITHIN WINDOW 

    do jtrc = jtrc1, jtrc2 
        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        else if( obj%hdr_flag_calc /= 0 ) then 
            if( nint( hd(obj%hdr_flag_calc,jtrc) ) == 0 ) cycle
        endif 

        do idpt = obj%idpt1, obj%ndpt1 + obj%idpt1 - 1 
            if( tr(idpt,jtrc) /= 0.0 ) then  
                sum = sum + abs( tr(idpt,jtrc) ) 
                nsum = nsum + 1 
            endif
        end do 
    end do 

    if( nsum > 0 ) scale = sum / nsum 

    call  norm_apply_scale( obj, ntr, hd, tr, scale )

    return
end subroutine norm_calc_l1


!!------------------------- do L2 normalization ----------------------------!!
!!------------------------- do L2 normalization ----------------------------!!
!!------------------------- do L2 normalization ----------------------------!!


subroutine norm_calc_l2( obj, ntr, hd, tr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    
! local variables

    integer      :: idpt      ! loop index for data points
    integer      :: jtrc      ! loop index for traces
    integer      :: jtrc1
    integer      :: jtrc2
    integer      :: nsum 
    real         :: scale
    real         :: sum 

!-------------------------------------------------------------------------------

    jtrc1 = obj%tr_beg 
    jtrc1 = min0( ntr, jtrc1 ) 
    jtrc2 = obj%tr_end 
    if( jtrc2 == 0 ) jtrc2 = ntr 
    jtrc2 = min0( ntr, jtrc2 ) 
    nsum  = 0 
    sum   = 0. 
    scale = 0.0 

!  SCALE EACH TRACE IN GROUP BY L2 NORM OF DATA WITHIN WINDOW 

    do jtrc = jtrc1, jtrc2 
        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        else if( obj%hdr_flag_calc /= 0 ) then 
            if( nint( hd(obj%hdr_flag_calc,jtrc) ) == 0 ) cycle
        endif 

        do idpt = obj%idpt1, obj%ndpt1 + obj%idpt1 - 1 
            if( tr(idpt,jtrc) /= 0.0 ) then  
                sum  = sum + tr(idpt,jtrc) ** 2 
                nsum = nsum + 1 
            endif
        end do 
    end do 

    if( nsum > 0 ) scale = sqrt( sum / nsum ) 

    call  norm_apply_scale( obj, ntr, hd, tr, scale )

    return
end subroutine norm_calc_l2


!!------------------------- do max normalization ---------------------------!!
!!------------------------- do max normalization ---------------------------!!
!!------------------------- do max normalization ---------------------------!!


subroutine norm_calc_max( obj, ntr, hd, tr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    
! local variables



    integer      :: imx

    integer      :: jtrc      ! loop index for traces
    integer      :: jtrc1
    integer      :: jtrc2
    real         :: scale
    real         :: scale0 

!-------------------------------------------------------------------------------

    jtrc1 = obj%tr_beg 
    jtrc1 = min0( ntr, jtrc1 ) 
    jtrc2 = obj%tr_end 
    if( jtrc2 == 0 ) jtrc2 = ntr 
    jtrc2 = min0( ntr, jtrc2 ) 
    scale = 0.0 

!  SCALE EACH TRACE IN GROUP BY ABSOLUTE MAXIMUM OF DATA WITHIN WINDOW 

    do jtrc = jtrc1, jtrc2 
        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        else if( obj%hdr_flag_calc /= 0 ) then 
            if( nint( hd(obj%hdr_flag_calc,jtrc) ) == 0 ) cycle
        endif 

        imx    = mth_isamax( obj%ndpt1, tr(obj%idpt1:,jtrc), 1 )
        scale0 = abs( tr(imx+obj%idpt1-1, jtrc ) ) 
        scale  = amax1( scale0, scale ) 
    end do 

    call  norm_apply_scale( obj, ntr, hd, tr, scale )

    return
end subroutine norm_calc_max


!!----------------------- do median normalization --------------------------!!
!!----------------------- do median normalization --------------------------!!
!!----------------------- do median normalization --------------------------!!


subroutine norm_calc_med( obj, ntr, hd, tr, ierr )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    integer          ,intent(out)   :: ierr     ! argument: completion code
    
! local variables

    real,pointer :: buf(:)
    integer      :: idpt      ! loop index for data points
    integer      :: jtrc      ! loop index for traces
    integer      :: jtrc1
    integer      :: jtrc2
    integer      :: nsum 
    real         :: scale

!-------------------------------------------------------------------------------

    jtrc1 = obj%tr_beg 
    jtrc1 = min0( ntr, jtrc1 ) 
    jtrc2 = obj%tr_end 
    if( jtrc2 == 0 ) jtrc2 = ntr 
    jtrc2 = min0( ntr, jtrc2 ) 
    nsum  = 0 
    scale = 0.0 

    allocate( buf(obj%ndpt1*(jtrc2-jtrc1+1)), stat=ierr )
    if( ierr /= 0 ) then
        call pc_error( 'memory allocation error in subroutine norm_calc_med' )
        return
    end if

    do jtrc = jtrc1, jtrc2 
        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        else if( obj%hdr_flag_calc /= 0 ) then 
            if( nint( hd(obj%hdr_flag_calc,jtrc) ) == 0 ) cycle
        endif 


        do idpt = obj%idpt1, obj%ndpt1 + obj%idpt1 - 1 
            if( tr(idpt,jtrc) /= 0.0 ) then  
                nsum      = nsum + 1 
                buf(nsum) = abs( tr(idpt,jtrc) ) 
            endif
        end do 
    end do 

    call median( buf, nsum, scale ) 

    deallocate( buf )

    call  norm_apply_scale( obj, ntr, hd, tr, scale )

    return
end subroutine norm_calc_med


!!----------------------------- apply scale --------------------------------!!
!!----------------------------- apply scale --------------------------------!!
!!----------------------------- apply scale --------------------------------!!


subroutine norm_apply_scale( obj, ntrc, hd, tr, scale )
    implicit none
    type(norm_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(in)    :: ntrc     ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    real             ,intent(inout) :: scale    ! argument: scale facter
    
! local variables

    integer      :: i
    integer      :: iflag
    integer      :: jtrc      ! loop index for traces

!-------------------------------------------------------------------------------

    iflag = 0

    if( scale /= 0.0 ) scale = obj%value / scale 

    do jtrc = 1, ntrc 
        if( obj%hdr_scale /= 0 ) hd(obj%hdr_scale,jtrc) = scale

        if( obj%hdr_flag /= 0 ) then 
            if( nint( hd(obj%hdr_flag,jtrc) ) == 0 ) cycle
        endif 

        iflag = 1 
        obj%ntraces         = obj%ntraces + 1 
        tr(1:obj%ndpt,jtrc) = tr(1:obj%ndpt,jtrc) * scale

        call lav_set_hdr( hd(:,jtrc), tr(:,jtrc), obj%ndpt )
    end do 

    if( iflag == 0 ) return
    
    obj%ngroups = obj%ngroups + 1 

    if( obj%print ) then  
        obj%nresults = obj%nresults + 1
        obj%resscal(obj%nresults) = scale
        obj%restrcs(obj%nresults) = ntrc
        if( obj%nresults == LRESULTS ) then
            write( obj%tmp_lun,'(2i6,6(i6,e14.5))' ) obj%mgroups, obj%mtraces, &
                         ( obj%restrcs(i), obj%resscal(i), i = 1, obj%nresults )
            obj%nresults = 0
            obj%mgroups  = obj%ngroups + 1
            obj%mtraces  = obj%ntraces + 1
        endif
    endif

    return
end subroutine norm_apply_scale


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


subroutine norm_wrapup (obj)
    implicit none
    type(norm_struct),intent(inout) :: obj       ! arguments

    ! local variables

    integer                 :: i     ! local - loop index
    integer                 :: ierr  ! local - return condition code
    character(len=MAX_LINE) :: line  ! local - line buffer

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    if (pc_do_not_process_traces()) return

    write( print_lun, '( " ", 70("*") )' )
    write( print_lun, '( " -------> BEGIN NORM PROCESS <------- " )' )

    write( print_lun, '( "NUMBER OF TRACES NORMALIZED = ", i7 )' ) obj%ntraces
    write( print_lun, '( "NUMBER OF GROUPS NORMALIZED = ", i7 )' ) obj%ngroups

    ! dump temporary file to the print file

    if( obj%print ) then
        if( obj%nresults > 0 ) then
            write( obj%tmp_lun, '(2i6,6(i6,e14.5))' )obj%mgroups, obj%mtraces, &
                         ( obj%restrcs(i), obj%resscal(i), i = 1, obj%nresults )
        endif

        rewind obj%tmp_lun

        do
            read(  obj%tmp_lun, '( A132 )', iostat=ierr ) line
            if( ierr /= 0 ) exit
            write( print_lun  , '( A132 )'              ) line 
        end do
    endif

    write( print_lun, '( " -------> END NORM PROCESS <------- " )' )
    write( print_lun, '( " ", 70("*") )' )

    return
end subroutine norm_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


    end module norm_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

