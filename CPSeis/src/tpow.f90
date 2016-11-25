!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 1999-06-15. />
!
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
! Name       : TPOW  (Scale traces by Time raised to a POWer)
! Category   : amplitude_mod
! Written    : 1986-07-01   by: Richard S. Day
! Revised    : 2000-12-08   by: Tom Stoeckley
! Maturity   : production   2001-06-04
! Purpose    : Scale trace values by a T**PWR * EXP(BETA*T) factor.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                   GENERAL DESCRIPTION                    
!
! 
! TPOW scales trace values by T**PWR, EXP(BETA*T) or a combination
! of both.  Trace value at time T is multiplied by (T**PWR) * EXP(BETA*T).  This
! is a simple expansion that can be undone by changing the signs of PWR and 
! BETA.
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                   ADVICE FOR USERS                       
!
! 
! If BETA = 0, then scale factor is simply T**PWR, conversely, if PWR = 0, then
! scale factor is calculated by EXP(BETA*T).  If PWR is non-zero, the scale
! factor will be 1.0 at all non-positive times.  Scale factors will be constant
! from beginning of trace to TIM_BEG and from TIM_END to end of trace. 
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                 TRACE INPUT REQUIREMENTS                 
!
! TPOW is a single-trace process.
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!               TRACE OUTPUT CHARACTERISTICS               
!
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!             GLOBAL PARAMETERS USED OR CHANGED            
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! IPN      process number                        used (must not be changed)
! MAXTR    max number of traces input/output     used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       Trace sample interval                 used but not changed
!
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!             TRACE HEADER WORDS USED OR CHANGED           
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!  25     LAV                        Set
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                    REVISION HISTORY                      
!
!     Date        Author       Description
!     ----        ------       -----------
!  17.2001-06-04  Stoeckley    Change wrapup flag.
!  16.2000-07-20  Coleman      Added GUI and set header word 25 to new LAV
!  15.2000-04-17  Coleman      Added RCS Ident string
!  14.2000-02-22  Coleman      Put the new documentation into the source file.
!  13.2000-02-07  Goodger      Incorporated latest newdoc.  Put all revision
!                              history entries into proper format.
!  12.1999-10-20  Coleman      Converted from old system.
!  11.1998-11-12  Day          Fortran 90 pointers replace cray pointers.
!                              GET_NPARM used for parameter computation.
!  10.1997-11-11  Vunderink    Added option for parameters to vary with 
!                              offset.
!  9. 1989-09-06  Baumel       and Javaid Durrani- added option
!                              to apply exponential gain.
!  8. 1988-09-23  Ball         NWIH and LTR Conversion  
!  7. 1988-04-22  Baumel       Add CPSPRT calls
!  6. 1987-04-08  Hanson       NCODE for history records
!  5. 1986-07-03  Baumel       changed default for TEND
!  4. 1986-07-16  Howard       Store PPOW
!  3. 1986-07-25  Baumel       Allow negative value of ALFT
!                              (to undo a previous TPOW)
!  2. 1986-07-29  Day          Added Memprt to set up
!  1. 1986-07-01  Day          Original Version   
!
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                  PORTABILITY LIMITATIONS                 
!
! No known limitations.
!
!
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!              SPECIAL COMPILING REQUIREMENTS              
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!             SPECIFIC CALLING CHARACTERISTICS             
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
!            ALTERNATE INTERNAL CALLING METHODS           
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!           ALGORITHM DESCRIPTION FOR DEVELOPERS          
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                    PROGRAMMING NOTES                    
!
!<pm>
!</pm>
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS TPOW Process/NC=80>
!
!                    Scale traces by Time raised to a POWer
!              Scale trace values by a T**PWR * EXP(BETA*T) factor
!
!                              PWR =~~~~~`FFFFFFFFF
!
!                              BETA =~~~~`FFFFFFFFF
!
!                              TIM_BEG = `FFFFFFFFF
!
!                              TIM_END = `FFFFFFFFF
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="PWR">
!<Tip> Exponent to use in the T**PWR factor. </Tip>
! Default = 2.0      
! Allowed = real   
! If PWR /= 0.0, trace value at time (T) is multiplied by (T**PWR)*EXP(BETA*t).
! If PWR = 0.0, trace value at time (T) is multiplied by EXP(BETA*T) 
!</Help>
!
!<Help KEYWORD="BETA">
!<Tip> Coefficient used in the EXP(BETA*T) factor. </Tip>
! Default = 0.0
! Allowed = real   
! If BETA = 0.0, trace value at time (T) is multiplied by T**PWR.
! If BETA/= 0.0, trace value at time (T) is multiplied by (T**PWR)*EXP(BETA*t).
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Beginnig time of trace to start scaling.</Tip>
! Default = TSTRT
! Allowed = TSTRT <= TIM_BEG < end of trace
! If TIM_BEG is greater than the TSTRT global (time of first trace sample), the
! scale factor will be constant from TSTRT to TIM_BEG.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Ending time of trace to stop scaling. </Tip>
! Default = (NDPT-1)DT
! Allowed = TIM_BEG < TIM_END < (NDPT-1)DT
! If TIM_END < (NDPT-1)DT, scale factor is constant from TIM_END to end of 
! trace.
!</Help>
!</HelpSection>
!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
! NOTES FOR CONVERSION PROGRAMMER
! 
! After polling users, Chuck I. Burch recommends that we disable the offset
! dependence in the old TPOW.

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module tpow_module
          
          

    use pc_module
    use named_constants_module
    use lav_module

    implicit none

    private
    public :: tpow_create     ! uses the parameter cache.
    public :: tpow_initialize
    public :: tpow_update     ! uses the parameter cache.
    public :: tpow_delete

!<execute_only>

    public :: tpow            ! main execution (trace processing) routine.
    public :: tpow_wrapup

!</execute_only>


    character(len=100),public,save :: tpow_IDENT = &
     '$Id: tpow.f90,v 1.17 2001/05/31 13:51:22 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    type,public :: tpow_struct              
 
        private
        logical                 :: skip_wrapup      ! wrap up flag.
        real                    :: pwr              ! process parameter
        real                    :: beta             ! process parameter
        real                    :: tim_beg          ! process parameter
        real                    :: tim_end          ! process parameter
        integer                 :: ndpt             ! global  
        real                    :: tstrt            ! global  
        real                    :: dt               ! global  
        real,pointer            :: ppow(:)          ! dependent variable

    end type tpow_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine tpow_create( obj )
    implicit none
    type(tpow_struct),pointer :: obj

    allocate( obj )

    nullify( obj%ppow )

    call tpow_initialize( obj )

    return
end subroutine tpow_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine tpow_delete( obj )
    implicit none
    type(tpow_struct),pointer :: obj

!<execute_only>
    call tpow_wrapup( obj )
!</execute_only>

    if( associated( obj%ppow ) ) deallocate( obj%ppow )
    deallocate( obj )

    return
end subroutine tpow_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine tpow_initialize( obj )
    implicit none
    type(tpow_struct),intent(inout) :: obj

    real :: x

    obj%ndpt  = 0          ! test later to ensure it has been reset.
    obj%dt    = -1.0       ! test later to ensure it has been reset.
    obj%tstrt = -HUGE(x)   ! test later to ensure it has been reset.

    call pc_get_global( 'ndpt' , obj%ndpt  )
    call pc_get_global( 'tstrt', obj%tstrt )
    call pc_get_global( 'dt'   , obj%dt    )

    obj%pwr      = 2.0
    obj%beta     = 0.0
    obj%tim_beg  = obj%tstrt
    obj%tim_end  = obj%tstrt + (obj%ndpt - 1) * obj%dt

    call tpow_update( obj )

    return
end subroutine tpow_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine tpow_update( obj )
    implicit none
    type(tpow_struct),intent(inout) :: obj

     integer :: i         ! local - loop index
     integer :: i1        ! local - first loop index value
     integer :: ibeg      ! local - index corresponding to tim_beg
     integer :: iend      ! local - index corresponding to tim_end
     integer :: izero     ! local - index (floor) corresponding to time = zero
     integer :: ierr      ! local - staus code
     real    :: tim_last  ! local - time at last sample
     real    :: t         ! local - time
     real    :: x1        ! local - test value
     real    :: x2        ! local - test value
     real    :: y1        ! local - test value
     real    :: y2        ! local - test value
     real    :: lhuge     ! local - log( HUGE(x) )
     real    :: ltiny     ! local - log( TINY(x) )

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get_global( 'ndpt' , obj%ndpt  )  ! number of data point per trace
    call pc_get_global( 'tstrt', obj%tstrt )  ! start time (time at 1st sample)
    call pc_get_global( 'dt'   , obj%dt    )  ! time between samples

    call pc_get( 'pwr'    , obj%pwr     )
    call pc_get( 'beta'   , obj%beta    )
    call pc_get( 'tim_beg', obj%tim_beg )
    call pc_get( 'tim_end', obj%tim_end )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    if( obj%ndpt  <  0 ) call pc_error( 'NDPT is less than 0' )
    if( obj%dt    <= 0 ) call pc_error( 'DT is less than or equal to zero' )
    if( obj%tstrt == -HUGE(x1) ) call pc_error( 'TSTRT is invalid' )

    tim_last = obj%tstrt + obj%dt * (obj%ndpt - 1)

    if( (obj%tim_beg < obj%tstrt) .or. (obj%tim_beg >= tim_last) ) then
        call pc_error( 'TIM_BEG is out of range' )
        obj%tim_beg = obj%tstrt
    endif

    if( (obj%tim_end <= obj%tim_beg) .or. (obj%tim_end > tim_last) ) then
        call pc_error( 'TIM_END is out of range' )
        obj%tim_end = tim_last
    endif
        
    ! NOTE: obj%pwr & obj%beta can be any real; however, let's check to make
    ! sure that they won't cause an overflow or underflow.

    lhuge = log( HUGE(x1) )
    ltiny = log( TINY(x1) )

    t  = max( obj%dt, obj%tim_beg )
    x1 = obj%pwr  * log( t )
    x2 = obj%pwr  * log( obj%tim_end )
    y1 = obj%beta * t
    y2 = obj%beta * obj%tim_end

    if( (x1 > lhuge) .or. (x1 < ltiny) .or. &
        (x2 > lhuge) .or. (x2 < ltiny) ) then
        call pc_error( 'The value of PWR will cause an overflow or underflow' )
        obj%pwr = 2.0
    endif

    if( (y1 > lhuge) .or. (y1 < ltiny) .or. &
        (y2 > lhuge) .or. (y2 < ltiny) ) then
        call pc_error( 'The value of BETA will cause an overflow or underflow' )
        obj%beta = 0.0
    endif

    if( (x1+y1 > lhuge) .or. (x1+y1 < ltiny) .or. &
        (x2+y2 > lhuge) .or. (x2+y2 < ltiny) ) then
        call pc_error( 'The parameters will cause an overflow or underflow' )
        obj%pwr  = 2.0
        obj%beta = 0.0
    endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


    call pc_put( 'pwr'    , obj%pwr     )
    call pc_put( 'beta'   , obj%beta    )
    call pc_put( 'tim_beg', obj%tim_beg )
    call pc_put( 'tim_end', obj%tim_end )

    call pc_put_control( "nstore", obj%ndpt )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    if( associated(obj%ppow) ) deallocate(obj%ppow)

!<execute_only>


    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

    allocate( obj%ppow(obj%ndpt), stat=ierr )
    if( ierr /= 0 ) then
        call pc_error( 'Unable to allocate PPOW array' )
        return
    endif

    ibeg  = max( nint( (obj%tim_beg - obj%tstrt) / obj%dt + 1.0 ), 1        )
    iend  = min( nint( (obj%tim_end - obj%tstrt) / obj%dt + 1.0 ), obj%ndpt )
    izero = floor( - obj%tstrt / obj%dt + 0.001 ) + 1

    if( iend < ibeg ) then
        call pc_error( 'Bad window' )
        return
    endif

    if( (izero >= iend) .or. ((obj%pwr==0.0) .and. (obj%beta==0.0)) ) then 
        obj%ppow = 1.0
    else
        i1 = max( ibeg, izero+1 )
        if( obj%pwr == 0.0 ) then
            do i = i1, iend
                t = obj%tstrt + obj%dt * (i - 1)
                obj%ppow(i) = exp( obj%beta * t )
            end do
        else if( obj%beta == 0.0 ) then
            do i = i1, iend
                t = obj%tstrt + obj%dt * (i - 1)
                obj%ppow(i) = t ** obj%pwr
            end do
        else
            do i = i1, iend
                t = obj%tstrt + obj%dt * (i - 1)
                obj%ppow(i) = t ** obj%pwr * exp( obj%beta * t )
            end do
        endif

        if( iend  <  obj%ndpt ) obj%ppow(iend+1:obj%ndpt) = obj%ppow(iend)
        if( ibeg  >  1        ) obj%ppow(     1:ibeg-1  ) = obj%ppow(i1)
        if( izero >= 1        ) obj%ppow(     1:izero   ) = 1.0
    endif

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine tpow_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

subroutine tpow( obj, ntr, hd, tr )
    implicit none
    type(tpow_struct),intent(inout)  :: obj               ! argument
    integer          ,intent(in)     :: ntr               ! argument
    double precision ,intent(inout)  :: hd(:,:)           ! argument
    real             ,intent(inout)  :: tr(:,:)           ! argument

    integer                          :: jtrc              ! local loop index

!-------------------------------------------------------------------------------

    if( ntr <= 0 ) then
        call tpow_wrapup( obj )
        return
    end if

    do jtrc = 1, ntr
        tr(1:obj%ndpt,jtrc) = tr(1:obj%ndpt,jtrc) * obj%ppow
    end do
        
    call lav_set_hdr( hd, tr, obj%ndpt, ntr )

    return
end subroutine tpow

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

subroutine tpow_wrapup( obj )
    implicit none
    type(tpow_struct),intent(inout) :: obj

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    return
end subroutine tpow_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module tpow_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

