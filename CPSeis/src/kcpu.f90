!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- kcpu.f90 --------------------------------!!
!!------------------------------- kcpu.f90 --------------------------------!!
!!------------------------------- kcpu.f90 --------------------------------!!

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
!                        C P S   P R I M I T I V E        
!
! Name       : kcpu 
! Category   : migrations
! Written    : 2003-05-22   by: Douglas Hanson
! Revised    : 2006-10-10   by: Douglas Hanson Change some cpu timers.
! Maturity   : beta
! Purpose    : basic cpu count functions and modules
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! basic cpu count functions and modules
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent (in)    = value required upon INPUT.
!   o = intent (out)   = value set by the routine upon OUTPUT.
!   b = intent (inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
!  8  2006-10-10  Douglas Hanson Change some cpu timers.
!  7  2006-08-29  Douglas Hanson Add counter descriptions.
!  6  2006-03-30  Douglas Hanson Add kray counters.
!  5  2006-03-28  Douglas Hanson Add ktable counters.  
!  4  2006-03-16  Douglas Hanson Add kray counter.
!  3  2005-10-13  Douglas Hanson Add map counters.
!  2  2004-03-15  Douglas Hanson Add kcpu_add.
!  1  2003-06-04  Douglas Hanson Extract from kmig.
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
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!
module kcpu_module
  !
  ! travel time table computation routines
  !
  use cpucount_module
  use pcpsx_module
  !
  implicit  none
  !
  private
  !
  ! subroutines
  !
  public :: kcpu_create               ! create cpu structure
  public :: kcpu_delete               ! delete cpu structure
  public :: kcpu_print                ! print  cpu structure
  public :: kcpu_reduce               ! reduce cpu structure
  public :: kcpu_clear                ! clear  cpu structure
  public :: kcpu_add                  ! add    cpu structure
  public :: kcpu                      ! update cpu structure
  !
  ! functions
  !
  type, public :: kcpu_struct
    !
    !private
    !
    type ( cpucount_struct ),  pointer :: c  ! cpucount structure
    !
  end type kcpu_struct
  !
  character(len=20), save, public :: c_kmig_total          & 
                                    = 'kmig_total          '
  character(len=20), save, public :: c_kmig_input          &
                                    = 'kmig_input          '
  character(len=20), save, public :: c_kmig_output         &
                                    = 'kmig_output         '
  character(len=20), save, public :: c_kmig_table          &
                                    = 'kmig_table          '
  character(len=20), save, public :: c_kmig_image          &
                                    = 'kmig_image          '
  character(len=20), save, public :: c_kmig_prep           &
                                    = 'kmig_prep           '
  character(len=20), save, public :: c_kmig_post           &
                                    = 'kmig_post           '
  character(len=20), save, public :: c_kmig_loop           &
                                    = 'kmig_loop           '
  character(len=20), save, public :: c_kalias_apply        &
                                    = 'kalias_apply        '
  character(len=20), save, public :: c_kio_swap_traces     &
                                    = 'kio_swap_traces     '
  character(len=20), save, public :: c_kio_swap_vel        &
                                    = 'kio_swap_vel        '
  character(len=20), save, public :: c_ksum_image_vel      &
                                    = 'ksum_image_vel      '
  character(len=20), save, public :: c_ksum_image_loc      &
                                    = 'ksum_image_loc      '
  character(len=20), save, public :: c_ksum_image_coef     &
                                    = 'ksum_image_coef     '
  character(len=20), save, public :: c_ksum_image_coef_z   &
                                    = 'ksum_image_coef_z   '
  character(len=20), save, public :: c_ksum_map_compute    &
                                    = 'ksum_map_compute    '
  character(len=20), save, public :: c_ksum_map_gradient   &
                                    = 'ksum_map_gradient   '
  character(len=20), save, public :: c_ksum_paz_interp     &
                                    = 'ksum_paz_interp     '
  character(len=20), save, public :: c_ksum_mute_turn      &
                                    = 'ksum_mute_turn      '
  character(len=20), save, public :: c_ksum_doppler_apply  &
                                    = 'ksum_doppler_apply  '
  character(len=20), save, public :: c_ksum_map_apply      &
                                    = 'ksum_map_apply      '
  character(len=20), save, public :: c_ksum_map_sum        &
                                    = 'ksum_map_sum        '
  character(len=20), save, public :: c_ktable_open_tables  &
                                    = 'ktable_open_tables  '
  character(len=20), save, public :: c_ktable_total        &
                                    = 'ktable_total        '
  character(len=20), save, public :: c_ktable_compute_1    &
                                    = 'ktable_compute_1    '
  character(len=20), save, public :: c_ktable_compute_2    &
                                    = 'ktable_compute_2    '
  character(len=20), save, public :: c_ktable_compute_3    &
                                    = 'ktable_compute_3    '
  character(len=20), save, public :: c_ktable_compute_4    &
                                    = 'ktable_compute_4    '
  character(len=20), save, public :: c_ktable_compute_5    &
                                    = 'ktable_compute_5    '
  character(len=20), save, public :: c_ktable_compute_6    &
                                    = 'ktable_compute_6    '
  character(len=20), save, public :: c_ktable_compute_7    &
                                    = 'ktable_compute_7    '
  character(len=20), save, public :: c_ktable_compute_8    &
                                    = 'ktable_compute_8    '
  character(len=20), save, public :: c_ktable_compute_9    &
                                    = 'ktable_compute_9    '
  character(len=20), save, public :: c_kray_compute_inc    &
                                    = 'kray_compute_inc    '
  character(len=20), save, public :: c_kray_compute_1      &
                                    = 'kray_compute_1      '
  character(len=20), save, public :: c_kray_compute_2      &
                                    = 'kray_compute_2      '
  character(len=20), save, public :: c_kray_compute_3      &
                                    = 'kray_compute_3      '
  character(len=20), save, public :: c_kray_compute_4      &
                                    = 'kray_compute_4      '
  character(len=20), save, public :: c_kray_compute_5      &
                                    = 'kray_compute_5      '
  character(len=20), save, public :: c_kray_compute_6      &
                                    = 'kray_compute_6      '
  character(len=20), save, public :: c_kray_compute_7      &
                                    = 'kray_compute_7      '
  character(len=20), save, public :: c_kray_compute_8      &
                                    = 'kray_compute_8      '
  character(len=20), save, public :: c_kray_compute_9      &
                                    = 'kray_compute_9      '
  !
  integer,           save, public :: kcpu_kmig_total          = 0
  integer,           save, public :: kcpu_kmig_input          = 0
  integer,           save, public :: kcpu_kmig_output         = 0
  integer,           save, public :: kcpu_kmig_table          = 0
  integer,           save, public :: kcpu_kmig_image          = 0
  integer,           save, public :: kcpu_kmig_prep           = 0
  integer,           save, public :: kcpu_kmig_post           = 0
  integer,           save, public :: kcpu_kmig_loop           = 0
  integer,           save, public :: kcpu_kalias_apply        = 0
  integer,           save, public :: kcpu_kio_swap_traces     = 0
  integer,           save, public :: kcpu_kio_swap_vel        = 0
  integer,           save, public :: kcpu_ksum_image_vel      = 0
  integer,           save, public :: kcpu_ksum_image_loc      = 0
  integer,           save, public :: kcpu_ksum_image_coef     = 0
  integer,           save, public :: kcpu_ksum_image_coef_z   = 0
  integer,           save, public :: kcpu_ksum_map_compute    = 0
  integer,           save, public :: kcpu_ksum_map_gradient   = 0
  integer,           save, public :: kcpu_ksum_paz_interp     = 0
  integer,           save, public :: kcpu_ksum_mute_turn      = 0
  integer,           save, public :: kcpu_ksum_doppler_apply  = 0
  integer,           save, public :: kcpu_ksum_map_apply      = 0
  integer,           save, public :: kcpu_ksum_map_sum        = 0
  integer,           save, public :: kcpu_ktable_open_tables  = 0
  integer,           save, public :: kcpu_ktable_total        = 0
  integer,           save, public :: kcpu_ktable_compute_1    = 0
  integer,           save, public :: kcpu_ktable_compute_2    = 0
  integer,           save, public :: kcpu_ktable_compute_3    = 0
  integer,           save, public :: kcpu_ktable_compute_4    = 0
  integer,           save, public :: kcpu_ktable_compute_5    = 0
  integer,           save, public :: kcpu_ktable_compute_6    = 0
  integer,           save, public :: kcpu_ktable_compute_7    = 0
  integer,           save, public :: kcpu_ktable_compute_8    = 0
  integer,           save, public :: kcpu_ktable_compute_9    = 0
  integer,           save, public :: kcpu_kray_compute_inc    = 0
  integer,           save, public :: kcpu_kray_compute_1      = 0
  integer,           save, public :: kcpu_kray_compute_2      = 0
  integer,           save, public :: kcpu_kray_compute_3      = 0
  integer,           save, public :: kcpu_kray_compute_4      = 0
  integer,           save, public :: kcpu_kray_compute_5      = 0
  integer,           save, public :: kcpu_kray_compute_6      = 0
  integer,           save, public :: kcpu_kray_compute_7      = 0
  integer,           save, public :: kcpu_kray_compute_8      = 0
  integer,           save, public :: kcpu_kray_compute_9      = 0
  !
  ! rcs identifier string
  character(len=100),public,save :: kcpu_ident = &
  '$Id: kcpu.f90,v 1.8 2006/10/11 13:01:26 Hanson beta sps $'
  !
  contains
  !
  subroutine kcpu_create ( o, i_err )
    !
    ! setup the cpucount structure
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    integer,                  intent(inout) :: i_err ! 0 O.K. -1 err
    !
    ! - Local variables
    !
    i_err = 0
    !
    allocate ( o )
    !
    call cpucount_create ( o%c, 100 )
    !
call cpucount_add ( o%c, c_kmig_total,         kcpu_kmig_total,         i_err )
call cpucount_add ( o%c, c_kmig_input,         kcpu_kmig_input,         i_err )
call cpucount_add ( o%c, c_kmig_output,        kcpu_kmig_output,        i_err )
call cpucount_add ( o%c, c_kmig_table,         kcpu_kmig_table,         i_err )
call cpucount_add ( o%c, c_kmig_image,         kcpu_kmig_image,         i_err )
call cpucount_add ( o%c, c_kmig_prep,          kcpu_kmig_prep,          i_err )
call cpucount_add ( o%c, c_kmig_post,          kcpu_kmig_post,          i_err )
call cpucount_add ( o%c, c_kmig_loop,          kcpu_kmig_loop,          i_err )
call cpucount_add ( o%c, c_kalias_apply,       kcpu_kalias_apply,       i_err )
call cpucount_add ( o%c, c_kio_swap_traces,    kcpu_kio_swap_traces,    i_err )
call cpucount_add ( o%c, c_kio_swap_vel,       kcpu_kio_swap_vel,       i_err )
call cpucount_add ( o%c, c_ksum_image_loc,     kcpu_ksum_image_loc,     i_err )
call cpucount_add ( o%c, c_ksum_image_coef,    kcpu_ksum_image_coef,    i_err )
call cpucount_add ( o%c, c_ksum_image_coef_z,  kcpu_ksum_image_coef_z,  i_err )
call cpucount_add ( o%c, c_ksum_image_vel,     kcpu_ksum_image_vel,     i_err )
call cpucount_add ( o%c, c_ksum_map_compute,   kcpu_ksum_map_compute,   i_err )
call cpucount_add ( o%c, c_ksum_map_gradient,  kcpu_ksum_map_gradient,  i_err )
call cpucount_add ( o%c, c_ksum_paz_interp,    kcpu_ksum_paz_interp,    i_err )
call cpucount_add ( o%c, c_ksum_mute_turn,     kcpu_ksum_mute_turn,     i_err )
call cpucount_add ( o%c, c_ksum_doppler_apply, kcpu_ksum_doppler_apply, i_err )
call cpucount_add ( o%c, c_ksum_map_apply,     kcpu_ksum_map_apply,     i_err )
call cpucount_add ( o%c, c_ksum_map_sum,       kcpu_ksum_map_sum,       i_err )
call cpucount_add ( o%c, c_ktable_open_tables, kcpu_ktable_open_tables, i_err )
call cpucount_add ( o%c, c_ktable_total,       kcpu_ktable_total,       i_err )
call cpucount_add ( o%c, c_ktable_compute_1,   kcpu_ktable_compute_1,   i_err )
call cpucount_add ( o%c, c_ktable_compute_2,   kcpu_ktable_compute_2,   i_err )
call cpucount_add ( o%c, c_ktable_compute_3,   kcpu_ktable_compute_3,   i_err )
call cpucount_add ( o%c, c_ktable_compute_4,   kcpu_ktable_compute_4,   i_err )
call cpucount_add ( o%c, c_ktable_compute_5,   kcpu_ktable_compute_5,   i_err )
call cpucount_add ( o%c, c_ktable_compute_6,   kcpu_ktable_compute_6,   i_err )
call cpucount_add ( o%c, c_ktable_compute_7,   kcpu_ktable_compute_7,   i_err )
call cpucount_add ( o%c, c_ktable_compute_8,   kcpu_ktable_compute_8,   i_err )
call cpucount_add ( o%c, c_ktable_compute_9,   kcpu_ktable_compute_9,   i_err )
call cpucount_add ( o%c, c_kray_compute_inc,   kcpu_kray_compute_inc,   i_err )
call cpucount_add ( o%c, c_kray_compute_1,     kcpu_kray_compute_1,     i_err )
call cpucount_add ( o%c, c_kray_compute_2,     kcpu_kray_compute_2,     i_err )
call cpucount_add ( o%c, c_kray_compute_3,     kcpu_kray_compute_3,     i_err )
call cpucount_add ( o%c, c_kray_compute_4,     kcpu_kray_compute_4,     i_err )
call cpucount_add ( o%c, c_kray_compute_5,     kcpu_kray_compute_5,     i_err )
call cpucount_add ( o%c, c_kray_compute_6,     kcpu_kray_compute_6,     i_err )
call cpucount_add ( o%c, c_kray_compute_7,     kcpu_kray_compute_7,     i_err )
call cpucount_add ( o%c, c_kray_compute_8,     kcpu_kray_compute_8,     i_err )
call cpucount_add ( o%c, c_kray_compute_9,     kcpu_kray_compute_9,     i_err )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call cpucount_print ( o%c, ' kcpu_create ' )
    !
    return
    !
  end subroutine kcpu_create 
  !
  subroutine kcpu_delete ( o )
    !
    ! setup the cpucount structure
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    !
    call cpucount_delete ( o%c )
    !
    deallocate ( o )
    !
    return
    !
  end subroutine kcpu_delete 
  !
  subroutine kcpu_print ( o, c_title )
    !
    ! setup the cpucount structure
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    character(len=*),         intent(in   ) :: c_title
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /, " ktable_compute_1 = ktable_compute_tables total ", &
    & /, " ktable_compute_2 = ktable_open_tables ", &
    & /, " ktable_compute_3 = ktime_compute_table ", &
    & /, " ktable_compute_4 = ktime_compute_table write ", &
    & /, " ktable_compute_5 = ktime_compute_table pcpsx_sum_all_reduce ", &
    & /, " ktable_compute_6 = ktable_compute_table_1 total ", &
    & /, " ktable_compute_7 = ktable_compute_table_1 - read ", &
    & /, " ktable_compute_8 = ktable_compute_table_1 - compute ", &
    & /, " ktable_compute_9 = ktable_compute_table_1 - compute + write ", &
    & /, " kray_compute_1   = kray_compute total ", &
    & /, " kray_compute_2   = kray_compute_table total ", &
    & /, " kray_compute_3   = kray_shoot_macro  ", &
    & /, " kray_compute_4   = kray_plot_rays  ", &
    & /, " kray_compute_5   = kray_tub_to_grid  ", &
    & /, " kray_compute_6   = kray_add  ", &
    & /, " kray_compute_7   = kray_out  ", &
    & /, " kray_compute_inc = kray_compute_inc total " &
    & )'
    !
    call cpucount_print ( o%c, c_title )
    !
    return
    !
  end subroutine kcpu_print
  !
  subroutine kcpu_reduce ( o )
    !
    ! setup the cpucount structure
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    !
    call cpucount_reduce ( o%c )
    !
    return
    !
  end subroutine kcpu_reduce
  !
  subroutine kcpu_clear ( o, l0 )
    !
    ! clear the cpucount structure
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    logical,                  intent(in   ) :: l0
    !
    call cpucount_clear ( o%c, l0 )
    !
    return
    !
  end subroutine kcpu_clear
  !
  subroutine kcpu_add ( o, c_title, index, i_err )
    !
    ! add a flag
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    character(len=*),         intent(in   ) :: c_title
    integer,                  intent(inout) :: index
    integer,                  intent(inout) :: i_err
    !
    call cpucount_add ( o%c, c_title, index, i_err )
    !
    return
    !
  end subroutine kcpu_add
  !
  subroutine kcpu ( o, i1, i2 )
    !
    ! setup the cpucount structure
    !
    type ( kcpu_struct ),     pointer       :: o ! kcpu structure
    integer,                  intent(in   ) :: i1
    integer,                  intent(in   ) :: i2
    !
    call cpucount ( o%c, i1, i2 )
    !
    return
    !
  end subroutine kcpu
  !
end module kcpu_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
