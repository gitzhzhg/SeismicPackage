!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-08-31. />

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
! Name       : CLIP    (CLIP noise spikes.)  [Formerly known as CHOP]
! Category   : amplitude_mod
! Written    : 1988-10-13   by: D. Binkley
! Revised    : 2000-12-08   by: Tom Stoeckley
! Maturity   : production   2001-04-26
! Purpose    : Clip or zero samples exceeding a specified absolute amplitude.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! CLIP identifies sample amplitudes that exceed a user specified absolute 
! amplitude (AMPL_MAX) and optionally zeros IEEE NAN and INF values.
!
!   If (OPT_CLIP = CLIP), CLIP reduces those samples to AMPL_MAX, while 
!       maintaining polarity.
!
!   If (OPT_CLIP = ZERO), CLIP zeros those samples.
!
! Users can control the traces subject to the CLIP process by putting the 
! SELECT process in the job ahead of CLIP and setting the flag header word
! parameter HDR_FLAG.
! 
!
! CLEAN Option
!
! The CLEAN option identifies samples that are "NAN" or "INF" IEEE values. 
! NAN (Not A Number) designates the result of an IEEE arithmetic operation that
! is undefined, such as 0/0.  INF (INFinity) designates a value that is larger
! than can be represented by IEEE arithmetic at the precision of the platform
! CPS is running on (typically 32 bits).  IEEE arithmetic is used by Intel and
! SUN platforms and certain others.
!
!
!     If OPT_CLEAN = ZERO, then samples identified as NANs or INFs are set to 
!     zero.
!
!     If OPT_CLEAN = KILL, then traces containing samples identified as NANs 
!     or INFs are killed.
!
!     If OPT_CLEAN = NONE, then no action is taken on traces containing 
!     samples identified as NANs or INFs.
!
! The OPT_CLEAN options do not affect the operation of OPT_CLIP = CLIP or 
! ZERO.
! 
!
! Reporting 
!
! CLIP will always print the following information in the .rpt file:
!
!     1.  Sequential trace number (header word 1) of the first and last trace
!     containing a NAN or an INF.
!     2.  Total number of traces cleaned (containing NAN or INF)
!     3.  Total number of clips or zeros resulting from OPT_CLIP operation.
!
! If OPT_PRINT = YES, then CLIP will, in addition, print the following 
! information for EACH affected trace:
!
!     1.  Sequential trace number (header word 1) of the affected trace.
!     that trace.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
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
! NWIH     number of words in header             used but not changed
! NDPT     number of data points in trace        used but not changed
! 
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#      Description                Action taken
! ----      -----------                ------------
!
! 25        LAV                        set if trace data is modified
! HDR_FLAG  Flagword for clipping      used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
!
!     Date       Author     Description
!     ----       ------     -----------
! 11. 2001-04-26 Stoeckley  Change wrapup flag.
! 10. 2000-11-27 Selzler    Added clean support for IEEE NAN and INF
! 9.  2000-05-16 Coleman    Added GUI and set header word 25 to new LAV
! 8.  2000-04-17 Coleman    Added RCS Ident string
! 7.  2000-03-10 Coleman    Added support for combo boxes
! 6.  1999-10-14 Coleman    Converted from old system
! 5.  1998-11-25 Goodger    Begin using the fortran90 compiler.          
! 4.  1994-01-03 Troutt     Increase format for chopped trace numbers to
!                           8-digits (was 5).
! 3.  1991-08-30 Cooper     Fix some variables that should be static instead
!                           stack
! 2.  1990-03-21 Troutt     Fix program to handle multi-trace input
!                           (add second dimensions to HD and TR).
!                           Also move ICHOP array to storage, where it 
!                           won't get corrupted and fix logic for 
!                           printing chopped trace numbers.
! 1.  1988-10-13 Binkley    Original version. 
!    
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
!
!<gui_def>
!<NS CLIP Process/NC=80>
!
!                               CLIP noise spikes
!         Clip or zero samples exceeding a specified absolute amplitude
!
!                            OPT_CLIP =~~~~~~~`CCCC
!
!                            HDR_FLAG =~~~`IIIIIIIII
!
!                            AMPL_MAX =~~~`FFFFFFFFF
!
!                            OPT_CLEAN =~~`CCC
!
!                            OPT_PRINT =~~`CCC
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
!<pm>
! If HDR_FLAG = 0, then all traces are processed.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are processed.  
!</pm>
!</Help>
!
!<Help KEYWORD="OPT_CLIP">
!<Tip> Whether to clip or zero samples exceeding AMPL_MAX abs. amplitude. </Tip>
! Default = CLIP
! Allowed = CLIP
! Allowed = ZERO
! Clipped samples are reduced to AMPL_MAX absolute amplitude, while 
! maintaining polarity.
!</Help>
!
!<Help KEYWORD="AMPL_MAX">
!<Tip> CLIP will clip or zero samples exceeding AMPL_MAX abs. amplitude. </Tip>
! Default = 10.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OPT_CLEAN">
!<Tip> Action to take if NANs or INFs are detected. </Tip>
! Default = ZERO
! Allowed = ZERO   (Zero samples identified as NANs or INFs.)
! Allowed = KILL   (Kill traces containing samples identified as NANs or INFs.)
! Allowed = NONE   (Take no action if samples are identified as NANs or INFs.)
! The OPT_CLEAN options do not affect the operation of OPT_CLIP = CLIP or 
! ZERO.
!</Help>
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Whether to print information on each affected trace. </Tip>
! Default = NO
! Allowed = YES/NO 
! If OPT_PRINT = YES, then CLIP will print the following information for EACH 
! affected trace:
!
!     1.  Sequential trace number (header word 1) of the affected trace.
!     2.  Total number of NANs detected on that trace.
!     3.  Total number of INFs detected on that trace.
!     4.  Total number of clips or zeros resulting from OPT_CLIP operation on 
!     that trace.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module clip_module

    use pc_module
    use string_module
    use named_constants_module
    use getlun_module
    use clean_module
    use lav_module

    implicit none

    private
    public :: clip_create     ! uses the parameter cache.
    public :: clip_initialize
    public :: clip_update     ! uses the parameter cache.
    public :: clip_delete

!<execute_only>

    public :: clip            ! main execution (trace processing) routine.
    public :: clip_wrapup

!</execute_only>


    character(len=100),public,save :: clip_IDENT = &
     '$Id: clip.f90,v 1.11 2001/04/25 19:48:57 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    integer,parameter :: LCLIP = 10
    
    type,public :: clip_struct              
 
        private
        logical                     :: skip_wrapup  ! wrap up flag
        integer                     :: hdr_flag     ! process parameter
        character(len=4)            :: opt_clip     ! process parameter
        real                        :: ampl_max     ! process parameter
        character(len=4)            :: opt_clean    ! process parameter
        logical                     :: opt_print    ! process parameter
        integer                     :: nwih         ! global
        integer                     :: ndpt         ! global
        integer                     :: ntrc_input   ! dependent variable
        integer                     :: ntrc_tested  ! dependent variable
        integer                     :: ntrc_chnged  ! dependent variable
        integer                     :: ntrc_cleaned ! dependent variable
        integer                     :: nclip        ! dependent variable
        integer                     :: iclip(LCLIP) ! dependent variable
        integer                     :: tmp_lun      ! dependent variable

    end type clip_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

    integer,parameter :: MIN_NWIH  =  64   ! minimum words in header
    integer,parameter :: MIN_NDPT  =   0   ! minimum data points in trace
    integer,parameter :: MAX_LINE  = 132   ! maximum characters per line
    integer           :: print_lun =   0   ! lun for the printer
    character(len=40) :: outfmt1   = '( 1X, "ALTERED TRACES: ", 10(1X,I8) )'


contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine clip_create( obj )
    implicit none
    type(clip_struct),pointer :: obj

    allocate( obj )

    call clip_initialize( obj )

    return
end subroutine clip_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine clip_delete( obj )
    implicit none
    type(clip_struct),pointer :: obj

!<execute_only>
    call clip_wrapup( obj )
!</execute_only>

    deallocate( obj )

    return
end subroutine clip_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine clip_initialize( obj )
    implicit none
    type(clip_struct),intent(out) :: obj      ! argument: process data

    obj%opt_clip    = 'CLIP'
    obj%opt_print   = .false.
    obj%ampl_max    = 10.0
    obj%opt_clean   = 'ZERO'
    obj%hdr_flag    =  0
    obj%ntrc_input  =  0     ! # of traces input
    obj%ntrc_tested =  0     ! # of traces tested
    obj%ntrc_chnged =  0     ! # of traces changed
    obj%ntrc_cleaned=  0     ! # of traces changed
    obj%nclip       =  0     ! # of entries in the iclip array
    obj%iclip       =  0     ! clear iclip array
    obj%tmp_lun     = -1     ! lun for temporary file
    obj%nwih        = -1     ! check later to make sure it's been reset
    obj%ndpt        = -1     ! check later to make sure it's been reset


    call clip_update( obj )

    return
end subroutine clip_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine clip_update( obj )
    implicit none
    type(clip_struct),intent(inout) :: obj      ! argument: process data

    integer                  :: ierr    ! local, return condition code
    character(len=MAX_LINE)  :: msg     ! local, message string

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get_global( 'nwih', obj%nwih )  ! number of header words.
    call pc_get_global( 'ndpt', obj%ndpt )  ! number of trace samples.

    call pc_get( 'hdr_flag' , obj%hdr_flag  )
    call pc_get( 'opt_clip' , obj%opt_clip  )
    call pc_get( 'ampl_max' , obj%ampl_max  )
    call pc_get( 'opt_clean', obj%opt_clean )
    call pc_get( 'opt_print', obj%opt_print )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    call string_to_upper( obj%opt_clip )
    select case( obj%opt_clip(1:1) )
        case( 'C' )
            obj%opt_clip = 'CLIP'
        case( 'Z' )
            obj%opt_clip = 'ZERO'
        case default
            call pc_error( 'OPT_CLIP must be "CLIP" or "ZERO"' )
            obj%opt_clip = 'CLIP'
    end select
    
    if( obj%ampl_max <= 0.0 ) then
        call pc_error( 'AMPL_MAX must be greater than zero' )
        obj%ampl_max = 10.0
    endif

    call string_to_upper( obj%opt_clean )
    select case( obj%opt_clean(1:1) )
        case( 'Z' )
            obj%opt_clean = 'ZERO'
        case( 'K' )
            obj%opt_clean = 'KILL'
        case( 'N' )
            obj%opt_clean = 'NONE'
        case default
            call pc_error( 'OPT_CLEAN must be "ZERO", "KILL" or "NONE"' )
            obj%opt_clean = 'ZERO'
    end select
    
    if( (obj%hdr_flag < 0) .or. (obj%hdr_flag > obj%nwih) ) then
        call pc_error( 'HDR_FLAG is < 0 or > NWIH' )
        obj%hdr_flag = 0
    endif

    if( obj%nwih < MIN_NWIH ) then
        call pc_error( 'NWIH is less than MIN_NWIH' )
    endif

    if( obj%ndpt < MIN_NDPT ) then
        call pc_error( 'NDPT is less than MIN_NDPT' )
    endif

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

    call pc_put_options_field( 'opt_clip' , (/ 'CLIP', 'ZERO' /), 2 )
    call pc_put_options_field( 'OPT_PRINT', (/ 'YES' , 'NO '  /), 2 )
    call pc_put_options_field( 'opt_clean' , (/ 'ZERO', 'KILL', 'NONE' /), 3 )

    call pc_put( 'hdr_flag' , obj%hdr_flag  )
    call pc_put( 'opt_clip' , obj%opt_clip  )
    call pc_put( 'ampl_max' , obj%ampl_max  )
    call pc_put( 'opt_clean', obj%opt_clean )
    call pc_put( 'opt_print', obj%opt_print )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

    print_lun = pc_get_lun()
    call getlun( obj%tmp_lun, ierr )

    if( ierr == 0 ) then
        open( unit=obj%tmp_lun, action='READWRITE', iostat=ierr, &
              status='SCRATCH', recl=MAX_LINE )
        if( ierr /= 0 ) then
            write( msg, '("iostat = ", I4, ", opening temp dump file")' ) ierr
            call pc_error( msg )
            return
        endif
    else
        call pc_error( 'getlun failed attempting to open temp dump file' )
        return
    endif

    write( obj%tmp_lun, '( " ", 70("*") )' )
    write( obj%tmp_lun, '( " -------> BEGIN CLIP PROCESS <------- " )' )

!</execute_only>

    return
end subroutine clip_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

subroutine clip( obj, ntr, hd, tr )
    implicit none
    type(clip_struct),intent(inout) :: obj      ! argument: process data
    integer          ,intent(inout) :: ntr      ! argument: number of traces
    double precision ,intent(inout) :: hd(:,:)  ! argument: trace headers
    real             ,intent(inout) :: tr(:,:)  ! argument: trace data
    
! local variables

    integer  :: idpt    ! loop index for data points
    integer  :: jtrc    ! loop index for traces
    integer  :: zeroed  ! count of points zeroed by clean
    logical  :: chnged  ! trace has been changed flag
    logical  :: killed  ! trace has been killed flag

!-------------------------------------------------------------------------------

    ! wrapup and abort if there are no traces to process
    if( ntr <= 0 ) then
         call clip_wrapup( obj )
         return
    end if

    ! process ntr traces

    do jtrc = 1, ntr
        obj%ntrc_input =obj%ntrc_input + 1

        if( obj%hdr_flag > 0 ) then
            if( hd(obj%hdr_flag,jtrc) == 0.0 ) cycle
        endif
        
        obj%ntrc_tested =obj%ntrc_tested + 1
        chnged = .false.
        killed = .false.
        zeroed = 0

        if( obj%opt_clean == 'ZERO' ) then
            zeroed = clean_zero(tr(:obj%ndpt, jtrc))

            if(zeroed > 0) then
                obj%ntrc_cleaned =obj%ntrc_cleaned + 1
            end if
        else if( obj%opt_clean == 'KILL' ) then
            killed = clean_kill(tr(:obj%ndpt, jtrc))

            if(killed) then
                obj%ntrc_cleaned =obj%ntrc_cleaned + 1
            end if
        end if

        if(.not. killed) then
            if( obj%opt_clip == 'CLIP' ) then
                do idpt = 1, obj%ndpt
                    if( abs(tr(idpt,jtrc)) > obj%ampl_max ) then
                        tr(idpt,jtrc) = sign(obj%ampl_max,tr(idpt,jtrc))
                        chnged = .true.
                    endif
                end do
            else
                do idpt = 1, obj%ndpt
                    if( abs(tr(idpt,jtrc)) > obj%ampl_max ) then
                        tr(idpt,jtrc) = 0.0
                        chnged = .true.
                    endif
                end do
            endif

            if( chnged ) then
                obj%ntrc_chnged =obj%ntrc_chnged + 1
            end if
        endif

        if( chnged .or. killed .or. zeroed > 0 ) then
            if(killed) then
                hd(25,jtrc) = 0.0
            else
                call lav_set_hdr( hd(:,jtrc), tr(:,jtrc), obj%ndpt )
            end if

            if( obj%opt_print ) then
                obj%nclip = obj%nclip + 1
                obj%iclip(obj%nclip) = obj%ntrc_input
                if( obj%nclip == LCLIP ) then
                    write( obj%tmp_lun, outfmt1) obj%iclip
                    obj%nclip = 0
                endif
            endif
        endif

    end do

    return
end subroutine clip

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

subroutine clip_wrapup( obj )
    implicit none
    type(clip_struct),intent(inout) :: obj      ! argument: process data

    ! local variables

    integer                 :: i     ! local - loop index
    integer                 :: ierr  ! local - return condition code
    character(len=MAX_LINE) :: line  ! local - line buffer

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    if( pc_do_not_process_traces() ) return

    if( obj%opt_print .and. (obj%nclip > 0) ) then
        write( obj%tmp_lun, outfmt1 ) ( obj%iclip(i), i=1,obj%nclip )
    endif

    write( obj%tmp_lun, '( 10X, I10, " TRACES INPUT  " )' ) obj%ntrc_input
    write( obj%tmp_lun, '( 10X, I10, " TRACES TESTED " )' ) obj%ntrc_tested
    write( obj%tmp_lun, '( 10X, I10, " TRACES CLEANED" )' ) obj%ntrc_cleaned
    if( obj%opt_clip == 'CLIP' ) then
        write( obj%tmp_lun, '( 10X, I10, " TRACES CLIPPED" )' ) obj%ntrc_chnged
    else
        write( obj%tmp_lun, '( 10X, I10, " TRACES ZEROED " )' ) obj%ntrc_chnged
    endif

    write( obj%tmp_lun, '( " -------> END CLIP PROCESS <------- " )' )
    write( obj%tmp_lun, '( " ", 70("*") )' )

    ! dump temporary file to the print file

    rewind obj%tmp_lun

    do
        read(  obj%tmp_lun, '( A132 )', iostat=ierr ) line
        if( ierr /= 0 ) exit
        write( print_lun  , '( A132 )'              ) line 
    end do

    return
end subroutine clip_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module clip_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

