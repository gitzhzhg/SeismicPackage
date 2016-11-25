!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- MIGFUN.f90 --------------------------------!!
!!------------------------------- MIGFUN.f90 --------------------------------!!
!!------------------------------- MIGFUN.f90 --------------------------------!!

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
! Name       : MIGFUN 
! Category   : migrations
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2007-01-11   by: Douglas Hanson Remove print.
! Maturity   : beta         
! Purpose    : various migration utilities.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! various migration utilities.
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
!   i =intent (in)    = value required upon INPUT.
!   o =intent (out)   = value set by the routine upon OUTPUT.
!   b =intent (inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                  o                i     b      o
!                hello = MIGFUN     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    MIGFUN_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    MIGFUN_ZZZ (bbbb, indx, value)
!
!
! character(len=*)           aaaa(*) =    --> description 
! character(len=8),pointer   bbbb(:) =    --> description 
! double precision           cvar    =    --> description
! character(len=*)           msg     =    --> description 
! integer                    hello   =    --> description
! integer         ,optional  indx    =    --> description
! double precision,optional  value   =    --> description
!
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
!  14 2007-01-11  Douglas Hanson Remove print.
!  13 2006-11-16  Douglas Hanson Fix prints.
!  12 2006-10-17  Douglas Hanson Modify prints.
!  11 2003-09-30  Douglas Hanson Fix migfun_sr_location 
!  10 2003-08-15  Douglas Hanson Add migfun_sr_location 
!   9 2003-07-16  R.S.Day        Fixed logic bug for ym_mig in 
!                                migfun_output_headers_sr 
!   8 2003-06-05  Bill Done      In module procedure migfun_output_headers(),
!                                truncate values of header words associated
!                                with the x grid and y grid midpoint values.
!   7 2003-03-14  Douglas Hanson Set hdr_offset in migfun_output_header_sr.
!   6 2003-02-04  Douglas Hanson Add migfun_output_header_sr.
!   5 2001-07-10  Douglas Hanson Fix azimuth=0, negative offset. PRODUCTION.
!   4 2001-03-21  Douglas Hanson Fix mutes in migfun_output_headers
!   3 2001-01-10  Douglas Hanson Set HEADER_GVS_MODIFIER, migfun_output_headers 
!   2 2000-08-25  Douglas Hanson cpsfcr
!   1 1999-12-01  Douglas Hanson Initial version.
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


module migfun_module
  ! utilities for imaging module headers
  use pc_module
  use named_constants_module
  use lav_module
  use matfun_module

  implicit none

  ! set default to all routines public
  public

  ! set default to all routines private
  ! private

  ! subroutines
  public :: migfun_mute_apply
  public :: migfun_mute_set
  public :: migfun_fos_compute
  public :: migfun_fos_apply
  public :: migfun_get_scale
  public :: migfun_check_x_header_word
  public :: migfun_check_xy_header_words
  public :: migfun_output_headers
  public :: migfun_output_headers_sr
  public :: migfun_xy_offset
  public :: migfun_trace_location
  public :: migfun_trace_offset
  public :: migfun_set_range
  public :: migfun_sr_location 

  ! functions
  public :: migfun_offset           ! return offset

  ! interfaces

! rcs identifier string
  character(len=100),public,save :: migfun_ident = &
       '$Id: migfun.f90,v 1.14 2007/01/12 11:50:37 Hanson beta sps $'

  contains

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_mute_apply ( mh_inp, hd_inp, nt_inp, tr_inp )
!  reapply mutes

    integer   mh_inp
    double precision hd_inp ( mh_inp )

    integer   nt_inp
    real      tr_inp ( nt_inp )

    integer   it_mute, nt_mute

!  reapply top mute
      it_mute = 1
      nt_mute = max ( 0, min ( nt_inp, nint ( hd_inp ( 02 ) ) - 1 ) )
      tr_inp ( it_mute:it_mute+nt_mute-1 ) = 0.

!  reapply bottom mute
      it_mute = max ( 1, min ( nt_inp, nint ( hd_inp ( 64 ) ) + 1 ) )
      nt_mute = max ( 0, min ( nt_inp, nt_inp-it_mute+1 ) )
      tr_inp ( it_mute:it_mute+nt_mute-1 ) = 0.

    return
  end subroutine migfun_mute_apply

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_mute_set ( m_hd, hd, n_tr, tr )
!  set the top and bottom mute indexes to the first nonzero value location

    integer   m_hd, n_tr
    double precision hd ( m_hd )
    real      tr ( n_tr )

    integer   i_tr

!  set mute
      do i_tr = 1 , n_tr

        hd ( 2 ) = i_tr
        if ( tr ( i_tr ) .ne. 0. ) go to 1

      end do    ! do i_tr = 1 , n_tr

      hd ( 2 ) = n_tr + 1

    1 continue

!  set tail mute
      do i_tr = n_tr , 1 , -1

        hd ( 64 ) = i_tr
        if ( tr ( i_tr ) .ne. 0. ) go to 2

      end do    ! do i_tr = n_tr , 1 , -1

      hd ( 64 ) = 0

    2 continue

    return
  end subroutine migfun_mute_set

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_fos_compute ( &
                                  nt_inp, t0_inp, dt_inp, &
                                  nt_fos, t0_fos, dt_fos, jt_fos &
                                )
! compute the fos grid

    integer   nt_inp
    real      t0_inp, dt_inp

    integer   nt_fos
    real      t0_fos, dt_fos
    integer   jt_fos

    real      t1_fos

      nt_fos = max ( 0, min ( nt_inp, nt_fos ) )
      t0_fos = t0_inp
      t1_fos = ( nt_inp - 1 ) * dt_inp + t0_fos
      dt_fos = dt_inp
      jt_fos = nt_inp - 1
      if ( nt_fos .eq. 0 ) return

      if ( nt_fos .gt. 1 ) then

        jt_fos = ( nt_inp - 1 ) / max ( 1, nt_fos-1 )
    1 continue
        dt_fos = dt_inp * jt_fos
        t1_fos = max ( 0, nt_fos-1 ) * dt_fos + t0_fos
        if ( t1_fos .le. ( nt_inp-1 )*dt_inp+t0_inp ) then
          jt_fos = jt_fos + 1
          go to 1
         end if

       end if    ! if ( nt_fos .le. 1 ) then

    return
  end subroutine migfun_fos_compute

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_fos_apply ( &
                                jt_fos, at_fos, &
                                nt_fos, tr_fos, &
                                nt_mig, tr_mig &
                              )
!  scale trace by fold of stack correction

    integer   jt_fos
    real      at_fos

    integer   nt_fos
    real      tr_fos ( nt_fos )

    integer   nt_mig
    real      tr_mig ( nt_mig )

    integer   it_mig
    integer   it_fos_1, it_fos_2
    real      ft_fos_1, ft_fos_2
    real      t0_fos, dt_fos

    real      tr_scale
    real      eps_fos

      eps_fos = 1.e-6

      if ( nt_fos .eq. 0 .or. abs ( at_fos ) .lt. .001 ) then
!  do nothing for nt_fos = 0 or at_fos = 0.

       else if ( nt_fos .eq. 1 ) then

        if ( tr_fos ( 1 ) .gt. 0. ) tr_mig = tr_mig / tr_fos ( 1 )**at_fos

       else if ( nt_fos .gt. 1 ) then    ! if ( nt_fos .eq. 0 ) then

        t0_fos = 1. + eps_fos
        dt_fos = 1. / jt_fos

        do it_mig = 1 , nt_mig

          it_fos_1 = min ( nt_fos, max ( 1, int ( t0_fos ) ) )
          it_fos_2 = min ( nt_fos, it_fos_1+1 )
          ft_fos_2 = max ( 0., min ( 1., t0_fos-it_fos_1 ) )
          ft_fos_1 = 1. - ft_fos_2
          t0_fos   = t0_fos + dt_fos
          tr_scale = ft_fos_1 * tr_fos ( it_fos_1 ) &
     &             + ft_fos_2 * tr_fos ( it_fos_2 )
          if ( tr_scale .gt. 0. ) tr_scale = ( 1. / tr_scale ) ** at_fos
          tr_mig ( it_mig ) = tr_scale * tr_mig ( it_mig )

        end do    ! do it_mig = 1 , nt_mig

       end if    ! if ( nt_fos .eq. 0 ) then

    return
  end subroutine migfun_fos_apply

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_get_scale ( &
                                c_title, lu_out, grid_obj, hx_mig, x0_scl, &
                                i_err &
                              )
! compute a scale factor relating header word hx_mig to distance units
! hx_mig must be 
! hdr_midpoint_xgrid, hdr_midpoint_ygrid
! hdr_midpoint_xloc,  hdr_midpoint_yloc

      character (len=*) :: c_title
    integer   lu_out
      type ( grid_struct )                      :: grid_obj             ! global
    integer   hx_mig
    real      x0_scl
    integer   i_err

! initalize the error flag to o.k.
      i_err = 0

! check the value of the header word for validity
      call migfun_check_x_header_word ( &
     & c_title, lu_out, hx_mig, i_err )

! using inline grid units
           if ( hx_mig .eq. hdr_midpoint_xgrid ) then

        x0_scl = grid_get_xgrid_width ( grid_obj )

! using crossline grid units
      else if ( hx_mig .eq. hdr_midpoint_ygrid ) then

        x0_scl = grid_get_ygrid_width ( grid_obj )

! using distance units
       else if ( hx_mig .eq. hdr_midpoint_xloc ) then    

        x0_scl = 1.

      else if ( hx_mig .eq. hdr_midpoint_yloc ) then    

        x0_scl = 1.

      end if

     !if ( lu_out .ge. 0 ) &
     !write ( lu_out,' ( &
     !& /, '' migfun_get_scale '', a &
     !& /,'' header word='', i8, '' scale='', g10.4 &
     !& )' ) &
     !c_title ( 1:len_trim ( c_title ) ), &
     !hx_mig, x0_scl

    return
  end subroutine migfun_get_scale

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_check_x_header_word ( c_title, lu_out, hx_mig, i_err )
! check to be sure
! hx_mig is
! hdr_midpoint_xgrid, hdr_midpoint_ygrid
! hdr_midpoint_xloc,  hdr_midpoint_yloc

      character (len=*) :: c_title
    integer   lu_out
    integer   hx_mig
    integer   i_err

      i_err = 0

! using inline grid units
           if ( hx_mig .eq. hdr_midpoint_xgrid ) then

! using crossline grid units
      else if ( hx_mig .eq. hdr_midpoint_ygrid ) then

! using distance units
       else if ( hx_mig .eq. hdr_midpoint_xloc ) then    

      else if ( hx_mig .eq. hdr_midpoint_yloc ) then    

      else

        i_err = -1

        call pc_error ( ' migfun_check_x_header_word error ' )

        if ( lu_out .le. 0 ) write ( lu_out, ' ( &
     & /, '' migfun_check_x_header_word error '', a &
     & /,'' header word ='', i8, &
     & /, '' should be ='', i8, '' or '', i8, &
     & '' or '', i8, '' or '', i8 &
     & )' ) &
     &  c_title ( 1:len_trim ( c_title ) ), hx_mig, &
     & hdr_midpoint_xgrid, hdr_midpoint_ygrid, &
     & hdr_midpoint_xloc,  hdr_midpoint_yloc

      end if

    return
  end subroutine migfun_check_x_header_word

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_check_xy_header_words ( &
                                            c_title, lu_out, hx_mig, hy_mig, &
                                            i_err &
                                          )
! check to be sure
! hx_mig, hy_mig are a pair of 
! hdr_midpoint_xgrid, hdr_midpoint_ygrid
! hdr_midpoint_xloc,  hdr_midpoint_yloc

      character (len=*) :: c_title
    integer   lu_out
    integer   hx_mig
    integer   hy_mig
    integer   i_err

      i_err = 0

! using x,y grid units
           if ( hx_mig .eq. hdr_midpoint_xgrid &
     &   .and. hy_mig .eq. hdr_midpoint_ygrid ) then

! using y,x grid units
      else if ( hx_mig .eq. hdr_midpoint_ygrid &
     &   .and. hy_mig .eq. hdr_midpoint_xgrid ) then

! using x,y distance units
      else if ( hx_mig .eq. hdr_midpoint_xloc &
     &   .and. hy_mig .eq. hdr_midpoint_yloc ) then    

! using y,x distance units
      else if ( hx_mig .eq. hdr_midpoint_yloc &
     &   .and. hy_mig .eq. hdr_midpoint_xloc ) then    

      else

        i_err = -1

        call pc_error ( ' migfun_check_xy_header_words error ' )

        if ( lu_out .le. 0 ) write ( lu_out, ' ( &
     & /, '' migfun_check_xy_header_words error '', a &
     & /, '' you must select x and y header words '', &
     & /, '' that are both grid units or both distance units. '', &
     & /,'' x header word ='', i8, '' y header word ='', i8, &
     & /, '' should be '', i8, '' and '', i8, '' or '', i8, '' and '', i8 &
     & /, ''        or '', i8, '' and '', i8, '' or '', i8, '' and '', i8 &
     & )' ) &
     &  c_title ( 1:len_trim ( c_title ) ), &
     & hx_mig, hy_mig, &
     & hdr_midpoint_xgrid, hdr_midpoint_ygrid, &
     & hdr_midpoint_xloc,  hdr_midpoint_yloc, &
     & hdr_midpoint_ygrid, hdr_midpoint_xgrid, &
     & hdr_midpoint_yloc, hdr_midpoint_xloc

      end if

    return
  end subroutine migfun_check_xy_header_words

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_output_headers (  &
                                      grid_obj, &
                                      trace_number, &
                                      group_number, &
                                      trace_group, &
                                      hx_mig, xm_mig, x0_scl, &
                                      hy_mig, ym_mig, y0_scl, &
                                      om_mig, azimuth, &
                                      top_mute, bot_mute, &
                                      nt_fos, at_fos, tr_fos, &
                                      i_velocity_scale, velocity_scale, &
                                      mh_mig, hd_mig, &
                                      nt_mig, tr_mig,  &
                                      i_err &
                                    )
!  output next trace

      type ( grid_struct )                      :: grid_obj             ! global

    integer   trace_number
    integer   group_number
    integer   trace_group

    integer   hx_mig
    real      xm_mig, x0_scl

    integer   hy_mig
    real      ym_mig, y0_scl

    real      om_mig, azimuth

    integer   top_mute, bot_mute

    integer   nt_fos
    real      at_fos
    real      tr_fos ( : )

    integer   i_velocity_scale
    real      velocity_scale

    integer   mh_mig
    double precision hd_mig ( mh_mig )

    integer   nt_mig
    real      tr_mig ( nt_mig )

    integer   i_err

    real      tr_ave
    real      xo_mig, yo_mig
    integer   it_mig

      i_err = 0

! set the top mute to the index of the first non zero amplitude
      if_top_mute : if ( top_mute .eq. -1) then

        hd_mig ( hdr_top_mute ) = nt_mig

        do_it_mig_1 : do it_mig = 1 , nt_mig

          if_tr_mig_1 : if ( tr_mig (it_mig) .ne. 0.) then

            hd_mig ( hdr_top_mute )    = it_mig

            go to 1

          end if if_tr_mig_1 ! if ( tr_mig (it_mig) .ne. 0.) then

        end do do_it_mig_1 ! do it_mig = 1 , nt_mig

    1 continue

      end if if_top_mute ! if ( top_mute .eq. -1) then

! set the bot mute to the index of the last non zero amplitude
      if_bot_mute : if ( bot_mute .eq. -1) then

        hd_mig ( hdr_bottom_mute ) = 1

        do_it_mig_2 : do it_mig = nt_mig , 1, -1

          if_tr_mig_2 : if ( tr_mig (it_mig) .ne. 0. ) then

            hd_mig ( hdr_bottom_mute )    = it_mig

            go to 2

          end if if_tr_mig_2 ! if ( tr_mig (it_mig) .ne. 0. ) then

        end do do_it_mig_2 ! do it_mig = nt_mig, 1, -1

    2 continue

      end if if_bot_mute ! if ( bot_mute .eq. -1) then

! set the top mute
      if ( top_mute .ge. 1 ) hd_mig ( hdr_top_mute )    = top_mute ! top mute

! set the bot mute
      if ( bot_mute .ge. 1 ) hd_mig ( hdr_bottom_mute ) = bot_mute ! bot mute

!  set output header words
      hd_mig ( hdr_sequence )        = trace_number    ! output trace number
      hd_mig ( hdr_current_group )   = group_number    ! group number
      hd_mig ( hdr_current_channel ) = trace_group     ! trace number in group

!  set the x,y, offset output header words for the image location
      hd_mig ( hx_mig )              = xm_mig / x0_scl ! x location
      hd_mig ( hy_mig )              = ym_mig / y0_scl ! y location
      hd_mig ( hdr_offset )          = om_mig          ! offset

! get the offsets in the two directions
      call migfun_xy_offset ( &
                                offset  = om_mig,  &
                                azimuth = azimuth, &
                                x_hdr   = hx_mig,  &
                                x_off   = xo_mig,  &
                                y_hdr   = hy_mig,  &
                                y_off   = yo_mig,  &
                                i_err   = i_err    &
                                   )

           if (i_err .ne. 0) then
             call pc_error(' migfun_output_headers error')
           return
           end if 

! set the midpoint header words
           if ( hx_mig .eq. hdr_midpoint_xgrid &
          .and. hy_mig .eq. hdr_midpoint_ygrid ) then

        hd_mig ( hdr_midpoint_xloc ) = grid_get_xsurvey_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_xgrid ), &
        hd_mig ( hdr_midpoint_ygrid ) )
        hd_mig ( hdr_midpoint_yloc ) = grid_get_ysurvey_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_xgrid ), &
        hd_mig ( hdr_midpoint_ygrid ) )

      else if ( hx_mig .eq. hdr_midpoint_ygrid &
          .and. hy_mig .eq. hdr_midpoint_xgrid ) then

        hd_mig ( hdr_midpoint_xloc ) = grid_get_xsurvey_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_ygrid ), &
        hd_mig ( hdr_midpoint_xgrid ) )
        hd_mig ( hdr_midpoint_yloc ) = grid_get_ysurvey_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_ygrid ), &
        hd_mig ( hdr_midpoint_xgrid ) )

      else if ( hx_mig .eq. hdr_midpoint_xloc &
          .and. hy_mig .eq. hdr_midpoint_yloc ) then

        hd_mig ( hdr_midpoint_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_xloc ), &
        hd_mig ( hdr_midpoint_yloc ) )
        hd_mig ( hdr_midpoint_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_xloc ), &
        hd_mig ( hdr_midpoint_yloc ) )

      else if ( hx_mig .eq. hdr_midpoint_yloc &
          .and. hy_mig .eq. hdr_midpoint_xloc ) then

        hd_mig ( hdr_midpoint_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_yloc ), &
        hd_mig ( hdr_midpoint_xloc ) )
        hd_mig ( hdr_midpoint_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
        hd_mig ( hdr_midpoint_yloc ), &
        hd_mig ( hdr_midpoint_xloc ) )

      end if     ! if ( hx_mig .eq. hdr_midpoint_xgrid

! set the source, reciever header words
      hd_mig ( hdr_source_xloc   ) = hd_mig ( hdr_midpoint_xloc ) + xo_mig * .5
      hd_mig ( hdr_receiver_xloc ) = hd_mig ( hdr_midpoint_xloc ) - xo_mig * .5
      hd_mig ( hdr_source_yloc   ) = hd_mig ( hdr_midpoint_yloc ) + yo_mig * .5
      hd_mig ( hdr_receiver_yloc ) = hd_mig ( hdr_midpoint_yloc ) - yo_mig * .5

      hd_mig ( hdr_source_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
      hd_mig ( hdr_source_xloc ), &
      hd_mig ( hdr_source_yloc )  )

      hd_mig ( hdr_source_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
      hd_mig ( hdr_source_xloc ), &
      hd_mig ( hdr_source_yloc )  )

      hd_mig ( hdr_receiver_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
      hd_mig ( hdr_receiver_xloc ), &
      hd_mig ( hdr_receiver_yloc )  )

      hd_mig ( hdr_receiver_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
      hd_mig ( hdr_receiver_xloc ), &
      hd_mig ( hdr_receiver_yloc )  )

! panel value
      if ( i_velocity_scale .gt. 0 ) then
        hd_mig ( hdr_panel        ) = i_velocity_scale    ! velocity panel
        hd_mig ( hdr_gvs_modifier ) = velocity_scale    ! velocity scaler
      end if    ! if ( i_velocity_scale .gt. 0 ) then

! fold of stack counter
      if ( nt_fos .gt. 0 ) then
        tr_ave = matfun_average ( nt_fos, tr_fos )       ! average fold
        hd_mig ( hdr_fold )    = tr_ave                  ! average fold
        hd_mig ( hdr_user_49 ) = at_fos                  ! fold of stack scaler
      end if ! if ( nt_fos .gt. 0 ) then

      hd_mig ( hdr_lav )     = lav ( tr_mig ( 1:nt_mig ), nt_mig )  ! max amp

!   round midpoint grid values to nearest integer
    hd_mig ( hdr_midpoint_xgrid ) = anint ( hd_mig ( hdr_midpoint_xgrid ) )
    hd_mig ( hdr_midpoint_ygrid ) = anint ( hd_mig ( hdr_midpoint_ygrid ) )

    return

  end subroutine migfun_output_headers
  !
  subroutine migfun_output_headers_sr ( &
                                        grid_obj, &
                                        trace_number, &
                                        group_number, &
                                        trace_group, &
                                        hx_mig, xs_mig, xr_mig, x0_scl, &
                                        hy_mig, ys_mig, yr_mig, y0_scl, &
                                        top_mute, bot_mute, &
                                        mh_mig, hd_mig, &
                                        nt_mig, tr_mig  &
                                      )
    !
    !  set output trace headers using source, receiver positions
    !
    type ( grid_struct )                      :: grid_obj             ! global
    !
    integer,                   intent (in   ) :: trace_number
    integer,                   intent (in   ) :: group_number
    integer,                   intent (in   ) :: trace_group
    !
    integer,                   intent (in   ) :: hx_mig
    real,                      intent (in   ) :: xs_mig
    real,                      intent (in   ) :: xr_mig
    real,                      intent (in   ) :: x0_scl
    !
    integer,                   intent (in   ) :: hy_mig
    real,                      intent (in   ) :: ys_mig
    real,                      intent (in   ) :: yr_mig
    real,                      intent (in   ) :: y0_scl
    !
    integer,                   intent (in   ) :: top_mute
    integer,                   intent (in   ) :: bot_mute
    !
    integer,                   intent (in   ) :: mh_mig
    double precision,          intent (inout) :: hd_mig ( : )
    !
    integer,                   intent (in   ) :: nt_mig
    real,                      intent (in   ) :: tr_mig ( : )
    !
    ! Local variables
    !
    real                                      :: om_mig
    real                                      :: xo_mig
    real                                      :: yo_mig
    real                                      :: xm_mig
    real                                      :: ym_mig
    integer                                   :: it_mig
    integer                                   :: i_err
    real                                      :: rx_scl
    real                                      :: ry_scl
    !
    i_err = 0
    !
    ! get the scale to go from hx_mig to distance units, rx_scl
    !
    call migfun_get_scale ( 'migfun_output_headers', -1,  grid_obj, &
                            hx_mig, rx_scl, i_err )
    !
    xxif_error_x : if ( i_err .ne. 0 ) then
      !
      call pc_error(' migfun_output_headers error')
      !
      return
      !
    end if xxif_error_x
    !
    ! get the scale to go from hy_mig to distance units, ry_scl
    !
    call migfun_get_scale ( 'migfun_output_headers', -1,  grid_obj, &
                            hy_mig, ry_scl, i_err )
    !
    xxif_error_y : if ( i_err .ne. 0 ) then
      !
      call pc_error(' migfun_output_headers error')
      !
      return
      !
    end if xxif_error_y
    !
    ! set the top mute to the index of the first non zero amplitude
    !
    if_top_mute : if ( top_mute .eq. -1 ) then
      !
      hd_mig ( hdr_top_mute ) = nt_mig
      !
      do_it_mig_1 : do it_mig = 1 , nt_mig
        !
        if_tr_mig_1 : if ( tr_mig (it_mig) .ne. 0.) then
          !
          hd_mig ( hdr_top_mute )    = it_mig
          !
          go to 1
          !
        end if if_tr_mig_1 ! if ( tr_mig (it_mig) .ne. 0.) then
        !
      end do do_it_mig_1 ! do it_mig = 1 , nt_mig
      !
    1 continue
      !
    end if if_top_mute ! if ( top_mute .eq. -1) then
    !
    ! set the bot mute to the index of the last non zero amplitude
    !
    if_bot_mute : if ( bot_mute .eq. -1) then
      !
      hd_mig ( hdr_bottom_mute ) = 1
      !
      do_it_mig_2 : do it_mig = nt_mig , 1, -1
        !
        if_tr_mig_2 : if ( tr_mig (it_mig) .ne. 0. ) then
          !
          hd_mig ( hdr_bottom_mute )    = it_mig
          !
          go to 2
          !
        end if if_tr_mig_2 ! if ( tr_mig (it_mig) .ne. 0. ) then
        !
      end do do_it_mig_2 ! do it_mig = nt_mig, 1, -1
      !
    2 continue
      !
    end if if_bot_mute ! if ( bot_mute .eq. -1) then
    !
    ! set the top mute
    !
    if ( top_mute .ge. 1 ) hd_mig ( hdr_top_mute )    = top_mute ! top mute
    !
    ! set the bot mute
    !
    if ( bot_mute .ge. 1 ) hd_mig ( hdr_bottom_mute ) = bot_mute ! bot mute
    !
    !  set output header words
    !
    hd_mig ( hdr_sequence )        = trace_number    ! output trace number
    hd_mig ( hdr_current_group )   = group_number    ! group number
    hd_mig ( hdr_current_channel ) = trace_group     ! trace number in group
    !
    !  set the x,y, offset output header words for the image location
    !
    xm_mig = ( xs_mig + xr_mig ) * .5
    !
    ym_mig = ( ys_mig + yr_mig ) * .5
    !
    xo_mig = ( xs_mig - xr_mig ) * rx_scl / x0_scl 
    !
    yo_mig = ( ys_mig - yr_mig ) * ry_scl / y0_scl 
    !
    hd_mig ( hx_mig )              = xm_mig / x0_scl ! x location
    hd_mig ( hy_mig )              = ym_mig / y0_scl ! y location
    hd_mig ( hdr_offset )          = om_mig          ! offset
    !
!print'(" h=",i4," s=",g10.4," r=",g10.4," m=",g10.4," o=",g10.4," c=",g10.4)',&
!hx_mig, xs_mig, xr_mig, xm_mig, om_mig, x0_scl
!print'(" rx=",g10.4," ry=",g10.4," xo=",g10.4," yo=",g10.4)', &
!rx_scl, ry_scl, xo_mig, yo_mig
    !
!print'(" h grid mid=",i8," src=",i8," rec=",i8)',&
!hdr_midpoint_xgrid, hdr_source_xgrid, hdr_receiver_xgrid
    !
!print'(" h loc  mid=",i8," src=",i8," rec=",i8)',&
!hdr_midpoint_xloc, hdr_source_xloc, hdr_receiver_xloc
    !
    ! set the midpoint header words
    !
    xxif_hx_mig : &
    if ( hx_mig .eq. hdr_midpoint_xgrid &
   .and. hy_mig .eq. hdr_midpoint_ygrid ) then
      !
      hd_mig ( hdr_midpoint_xloc ) = grid_get_xsurvey_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_xgrid ), &
      hd_mig ( hdr_midpoint_ygrid ) )
      hd_mig ( hdr_midpoint_yloc ) = grid_get_ysurvey_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_xgrid ), &
      hd_mig ( hdr_midpoint_ygrid ) )
      !
    else if ( hx_mig .eq. hdr_midpoint_ygrid &
        .and. hy_mig .eq. hdr_midpoint_xgrid ) then
      !
      hd_mig ( hdr_midpoint_xloc ) = grid_get_xsurvey_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_ygrid ), &
      hd_mig ( hdr_midpoint_xgrid ) )
      hd_mig ( hdr_midpoint_yloc ) = grid_get_ysurvey_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_ygrid ), &
      hd_mig ( hdr_midpoint_xgrid ) )
      !
    else if ( hx_mig .eq. hdr_midpoint_xloc &
        .and. hy_mig .eq. hdr_midpoint_yloc ) then
      !
      hd_mig ( hdr_midpoint_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_xloc ), &
      hd_mig ( hdr_midpoint_yloc ) )
      hd_mig ( hdr_midpoint_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_xloc ), &
      hd_mig ( hdr_midpoint_yloc ) )
      !
    else if ( hx_mig .eq. hdr_midpoint_yloc &
        .and. hy_mig .eq. hdr_midpoint_xloc ) then
      !
      hd_mig ( hdr_midpoint_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_yloc ), &
      hd_mig ( hdr_midpoint_xloc ) )
      hd_mig ( hdr_midpoint_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
      hd_mig ( hdr_midpoint_yloc ), &
      hd_mig ( hdr_midpoint_xloc ) )
      !
    end if xxif_hx_mig 
    !
    ! set the source, reciever header words
    !
    hd_mig ( hdr_source_xloc   ) = hd_mig ( hdr_midpoint_xloc ) + xo_mig * .5
    hd_mig ( hdr_receiver_xloc ) = hd_mig ( hdr_midpoint_xloc ) - xo_mig * .5
    hd_mig ( hdr_source_yloc   ) = hd_mig ( hdr_midpoint_yloc ) + yo_mig * .5
    hd_mig ( hdr_receiver_yloc ) = hd_mig ( hdr_midpoint_yloc ) - yo_mig * .5
    !
    !print'(" x loc  mid=",g10.4," src=",g10.4," rec=",g10.4)',&
    !hd_mig ( hdr_midpoint_xloc ), &
    !hd_mig ( hdr_source_yloc   ), &
    !hd_mig ( hdr_receiver_yloc )
    !
    hd_mig ( hdr_source_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
    hd_mig ( hdr_source_xloc ), &
    hd_mig ( hdr_source_yloc )  )
    !
    hd_mig ( hdr_source_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
    hd_mig ( hdr_source_xloc ), &
    hd_mig ( hdr_source_yloc )  )
    !
    hd_mig ( hdr_receiver_xgrid ) = grid_get_xgrid_coord ( grid_obj, &
    hd_mig ( hdr_receiver_xloc ), &
    hd_mig ( hdr_receiver_yloc )  )
    !
    hd_mig ( hdr_receiver_ygrid ) = grid_get_ygrid_coord ( grid_obj, &
    hd_mig ( hdr_receiver_xloc ), &
    hd_mig ( hdr_receiver_yloc )  )
    !
    hd_mig ( hdr_offset) = sqrt ( &
    ( hd_mig ( hdr_source_xloc ) - hd_mig ( hdr_receiver_xloc ) ) ** 2 + &
    ( hd_mig ( hdr_source_yloc ) - hd_mig ( hdr_receiver_yloc ) ) ** 2 )
    !
    !print'(" x grid mid=",g10.4," src=",g10.4," rec=",g10.4)',&
    !hd_mig ( hdr_midpoint_xgrid ), &
    !hd_mig ( hdr_source_ygrid   ), &
    !hd_mig ( hdr_receiver_ygrid )
    !
    hd_mig ( hdr_lav )     = lav ( tr_mig ( 1:nt_mig ), nt_mig )  ! max amp
    hd_mig ( hdr_lav )     = lav ( tr_mig ( 1:nt_mig ), nt_mig )  ! max amp
    !
    return
    !
  end subroutine migfun_output_headers_sr
  !
  subroutine migfun_xy_offset ( &
                                offset, azimuth, &
                                x_hdr, x_off, &
                                y_hdr, y_off, &
                                i_err &
                              )
! given two direction header words, x_hdr and y_hdr, and the offset and azimuth
! determine the offset in each direction
! x_hdr, y_hdr must be a combination of :
! hdr_midpoint_xgrid, hdr_midpoint_ygrid, 
! hdr_midpoint_xloc, hdr_midpoint_yloc, 
! hdr_midpoint_xgrid, hdr_midpoint_ygrid, 
! hdr_midpoint_xloc, hdr_midpoint_yloc, 
  real,    intent (in   ) :: offset  ! total offset
  real,    intent (in   ) :: azimuth ! azimuth in radians 0=xgrid, pi/2=ygrid
  integer, intent (in   ) :: x_hdr   ! x header word
  real,    intent (  out) :: x_off   ! x offset
  integer, intent (in   ) :: y_hdr   ! y header word
  real,    intent (  out) :: y_off   ! y offset
  integer, intent (  out) :: i_err   ! error flag = 0 o.k., /= 0 error

! initialize the offsets to 0
    x_off = 0.
    y_off = 0.

! check that the header words are an allowed combination
    call migfun_check_xy_header_words ( &
    & 'migfun_xy_offset ', pc_get_lun(), x_hdr, y_hdr, i_err )
    if (i_err .ne. 0) return

! compute the direction offsets
    if ( x_hdr .eq. hdr_midpoint_xgrid &
   .and. y_hdr .eq. hdr_midpoint_ygrid ) then

      x_off = offset * cos ( azimuth )
      y_off = offset * sin ( azimuth )

    else if ( x_hdr .eq. hdr_midpoint_ygrid &
        .and. y_hdr .eq. hdr_midpoint_xgrid ) then

      x_off = offset * sin ( azimuth )
      y_off = offset * cos ( azimuth )

    else if ( x_hdr .eq. hdr_midpoint_xloc &
        .and. y_hdr .eq. hdr_midpoint_yloc ) then

      x_off = offset * cos ( azimuth )
      y_off = offset * sin ( azimuth )

    else if ( x_hdr .eq. hdr_midpoint_yloc &
        .and. y_hdr .eq. hdr_midpoint_xloc ) then

      x_off = offset * sin ( azimuth )
      y_off = offset * cos ( azimuth )

    end if     ! if ( x_hdr .eq. hdr_midpoint_xgrid

  end subroutine migfun_xy_offset 

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_trace_location ( &
                                     hd_inp, x0_mig, dx_mig, x0_scl, &
                                     ix_inp, x0_inp &
                                   )
    double precision hd_inp

    real      x0_mig, dx_mig, x0_scl

    integer   ix_inp
    real      x0_inp

!  get the input trace location
      x0_inp = hd_inp * x0_scl

!  get trace location
      ix_inp = nint (  ( x0_inp-x0_mig )/dx_mig ) + 1

    return
  end subroutine migfun_trace_location

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_trace_offset ( &
                                   n_dim, &
                                   h0_inp, h0_ixy, &
                                   hx_mig, x0_scl, hx_inp, &
                                   hy_mig, y0_scl, hy_inp, &
                                   mh_inp, hd_inp &
                                 )
    !  migfun compute the trace offset by determining the trace azimuth from
    !  the source and receiver positons and using that azimuth with
    !  the total trace offset from header word 6.  If the source and
    !
    integer   n_dim
    !
    real      h0_inp, h0_ixy
    !
    integer   hx_mig
    real      x0_scl, hx_inp
    !
    integer   hy_mig
    real      y0_scl, hy_inp
    !
    integer   mh_inp
    double precision hd_inp ( mh_inp )
    !
    real      xy_angle
    !
    hx_inp = migfun_offset ( hx_mig, x0_scl, mh_inp, hd_inp )   ! x offset
    hy_inp = migfun_offset ( hy_mig, y0_scl, mh_inp, hd_inp )   ! y offset
    !
    h0_ixy = sqrt ( hx_inp**2+hy_inp**2 )                       ! xy offset
    h0_inp = hd_inp ( hdr_offset )                              ! total offset
    !
    xy_angle = matfun_atan2 ( hy_inp, hx_inp ) ! angle from the x axis
    if ( n_dim .eq. 2 ) xy_angle = 0.       ! for 2D put offset in X direction
    if ( n_dim .eq. 2 .and. hx_inp .lt. 0. ) xy_angle = - pi
    !
    hx_inp = cos ( xy_angle ) * abs ( h0_inp )             ! x offset
    hy_inp = sin ( xy_angle ) * abs ( h0_inp )             ! y offset
    if ( n_dim .eq. 2 ) hy_inp = 0. ! for 2D put offset in X direction
    !
    !write ( pc_get_lun(),' ( &
    !& /,'' migfun_trace_offset '' &
    !& /, '' n_dim='',i2, '' xy_angle='',g10.4, &
    !& /, '' hx_inp='',g10.4,'' hy_inp='',g10.4, &
    !& /, '' h0_inp='',g10.4,'' h0_ixy='',g10.4 &
    !& )' ) &
    !n_dim, xy_angle*90./asin ( 1. ), &
    !hx_inp,hy_inp,h0_inp,h0_ixy
    !
    return
    !
  end subroutine migfun_trace_offset

!23456789012345678901234567890123456789012345678901234567890123456789012
  real function migfun_offset ( hx_mig, x0_scl, mh_inp, hd_inp )
    !  return the offset in the hx_mig direction
    ! hx_mig must be 
    ! hdr_midpoint_xgrid, hdr_midpoint_ygrid
    ! hdr_midpoint_xloc,  hdr_midpoint_yloc
    !
    integer   hx_mig
    real      x0_scl, hx_inp
    !
    integer   mh_inp
    double precision hd_inp ( mh_inp )
    !
    integer   i_err
    !
    ! check the value of the header word for validity
    !
    call migfun_check_x_header_word ( &
    'migfun_offset', pc_get_lun(), hx_mig, i_err )
    !
    ! get the trace offset
    !
    if_x_grid : if (  hx_mig .eq. hdr_midpoint_xgrid ) then
      !
      hx_inp = ( hd_inp ( hdr_source_xgrid ) &
               - hd_inp ( hdr_receiver_xgrid ) ) * x0_scl
      !
    else if ( hx_mig .eq. hdr_midpoint_ygrid ) then
      !
      hx_inp = ( hd_inp ( hdr_source_ygrid ) &
               - hd_inp ( hdr_receiver_ygrid ) ) * x0_scl
      !
    else if ( hx_mig .eq. hdr_midpoint_xloc ) then    
      !
      hx_inp = ( hd_inp ( hdr_source_xloc ) &
               - hd_inp ( hdr_receiver_xloc ) )
      !
    else if ( hx_mig .eq. hdr_midpoint_yloc ) then    
      !
      hx_inp = ( hd_inp ( hdr_source_yloc ) &
               - hd_inp ( hdr_receiver_yloc ) )
      !
    end if if_x_grid
    !
    migfun_offset = hx_inp
    !
    return
    !
  end function migfun_offset

!23456789012345678901234567890123456789012345678901234567890123456789012
  subroutine migfun_set_range (  nx_mig, x0_mig, dx_mig, x1_mig )
!  make sure the image increment is positive and min is less than max
    integer   nx_mig
    real      x0_mig, dx_mig, x1_mig

    real      x_temp

      x1_mig = ( nx_mig - 1 ) * dx_mig + x0_mig

      if (  dx_mig .lt. 0 ) then

        x_temp = x1_mig
        x1_mig = x0_mig
        x0_mig = x1_mig
        dx_mig = abs ( dx_mig )

      end if    ! if (  dx_mig .lt. 0 ) then

    return
  end subroutine migfun_set_range
  !
  subroutine migfun_sr_location ( &
                                  grid_obj, &
                                  hx_mig, xs_mig, xr_mig, x0_scl, &
                                  hy_mig, ys_mig, yr_mig, y0_scl, &
                                  hd_mig &
                                )
    !
    ! return the trace source and reciever locations 
    ! in hx_mig units scaled by x0_scl, y0_scl
    !
    type ( grid_struct )                      :: grid_obj             ! global
    !
    integer,                   intent (in   ) :: hx_mig
    real,                      intent (inout) :: xs_mig
    real,                      intent (inout) :: xr_mig
    real,                      intent (in   ) :: x0_scl
    !
    integer,                   intent (in   ) :: hy_mig
    real,                      intent (inout) :: ys_mig
    real,                      intent (inout) :: yr_mig
    real,                      intent (in   ) :: y0_scl
    !
    double precision,          intent (in   ) :: hd_mig ( : )
    !
    ! Local variables
    !
    integer                                   :: i_err
    !
    i_err = 0
    !
    ! make sure the header words are correct
    !
    call migfun_check_xy_header_words ( &
    ' migfun_sr_location ', pc_get_lun(), hx_mig, hy_mig, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! set get the source reciever locations 
    !
    xxif_hx_mig : &
    if ( hx_mig .eq. hdr_midpoint_xgrid &
   .and. hy_mig .eq. hdr_midpoint_ygrid ) then
      !
      xs_mig = hd_mig ( hdr_source_xgrid   )
      !
      xr_mig = hd_mig ( hdr_receiver_xgrid )
      !
      ys_mig = hd_mig ( hdr_source_ygrid   )
      !
      yr_mig = hd_mig ( hdr_receiver_ygrid )
      !
    else if ( hx_mig .eq. hdr_midpoint_ygrid &
        .and. hy_mig .eq. hdr_midpoint_xgrid ) then
      !
      xs_mig = hd_mig ( hdr_source_ygrid   )
      !
      xr_mig = hd_mig ( hdr_receiver_ygrid )
      !
      ys_mig = hd_mig ( hdr_source_xgrid   )
      !
      yr_mig = hd_mig ( hdr_receiver_xgrid )
      !
    else if ( hx_mig .eq. hdr_midpoint_xloc &
        .and. hy_mig .eq. hdr_midpoint_yloc ) then
      !
      xs_mig = hd_mig ( hdr_source_xloc   )
      !
      xr_mig = hd_mig ( hdr_receiver_xloc )
      !
      ys_mig = hd_mig ( hdr_source_yloc   )
      !
      yr_mig = hd_mig ( hdr_receiver_yloc )
      !
    else if ( hx_mig .eq. hdr_midpoint_yloc &
        .and. hy_mig .eq. hdr_midpoint_xloc ) then
      !
      xs_mig = hd_mig ( hdr_source_yloc   )
      !
      xr_mig = hd_mig ( hdr_receiver_yloc )
      !
      ys_mig = hd_mig ( hdr_source_xloc   )
      !
      yr_mig = hd_mig ( hdr_receiver_xloc )
      !
    end if xxif_hx_mig 
    !
    xs_mig = xs_mig * x0_scl
    !
    xr_mig = xr_mig * x0_scl
    !
    ys_mig = ys_mig * y0_scl
    !
    yr_mig = yr_mig * y0_scl
    !
    return
    !
999 continue
    !
    call pc_error ( ' migfun_sr_location error ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine migfun_sr_location 
  !
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

end module migfun_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
