!<CPS_v1 type="PROCESS"/>
!!------------------------------- slab.f90 ---------------------------------!!
!!------------------------------- slab.f90 ---------------------------------!!
!!------------------------------- slab.f90 ---------------------------------!!


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
! Name       : SLAB
! Category   : miscellaneous
! Written    : 1996-03-01   by: Bob Baumel
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Replaces trace sample amplitudes by a statistic within windows.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! For each input trace, within each user defined window, SLAB calculates a 
! statistic based on the samples within that window and replaces all the trace
! sample values in the window with the value of the statistic.  Available 
! statistics are median, LAV, average (mean), and RMS.  
!
! Windows have length WIN_LEN seconds with an increment of WIN_INC seconds. 
! Adjacent windows may abut but may not overlap.  The first window starts at
! time TIM_BEG seconds.  The last window on the trace may not be the full
! specified length.
! 
! Trace samples above the top of the first window, between windows, and
! below the last window, are reset to zero.
!
! In locating windows and calculating statistics SLAB ignores the mute header
! words.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
! Statistics computed by SLAB can be extracted using the SLICE process.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NDPT      number of sample values in trace        used but not changed 
! TSTRT     starting time on trace                  used but not changed
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
! 2       Head mute index            Reset to top of first window
! 25      LAV                        Recalculated
! 64      Tail mute index            Reset to bottom of last window
! 
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author       Description
!     ----       ------       -----------
! 7.  2006-09-11 Stoeckley    Add call to pc_register_array_names for SeisSpace.
! 6.  2001-06-27 Stoeckley    Converted from old system. PRODUCTION.
! 5.  1999-03-24 Baumel       Move from newlib to conlib.
! 4.  1999-03-08 Baumel       Modify for F90 compiler.
! 3.  1998-10-14 Baumel       Add ISTAT=3 and 4 options.
! 2.  1996-03-04 Baumel       Add TSTEP and ISTAT parms.
! 1.  1996-03-01 Baumel       Original version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
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
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
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
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS SLAB Process/NC=80>
!
!        Replace trace sample amplitudes by a statistic within windows.
!
!    OPT_STAT=`CCC       TIM_BEG=`FFFFFFFF         WIN_INIT WIN_LAST
!                                                  `XXXXXXXX`XXXXXXXX
!    TSTRT=`XXXXXXXX     WIN_LEN=`FFFFFFFF         `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!    TSTOP=`XXXXXXXX     WIN_INC=`FFFFFFFF         `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!                        NUM_WIN=`XXXXXXXX         `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!                                                  `XXXXXXXX`XXXXXXXX
!
!<PARMS WIN_INIT_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_STAT">
!<Tip> Statistic to use for slab sample replacement. </Tip>
! Default = MED
! Allowed = MED  (Median of sample absolute values.)
! Allowed = AVE  (Average, or arithmetic mean, of sample absolute values.)
! Allowed = LAV  (Largest sample absolute value.)
! Allowed = RMS  (Square root of mean square of sample absolute values.)
!
! For each input trace, within each user defined window, SLAB calculates a 
! statistic based on the samples within that window and replaces all the trace
! sample values in the window with the value of the statistic. 
!</Help>
!
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Time for top of first slab window, in seconds. </Tip>
! Default = TSTRT
! Allowed = real >= TSTRT 
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of slab windows, in seconds. </Tip>
! Default = 0.2
! Allowed = real >= DT
!</Help>
!
!
!<Help KEYWORD="WIN_INC">
!<Tip> Increment between slab windows, in seconds. </Tip>
! Default = WIN_LEN
! Allowed = real >= WIN_LEN
!</Help>
!
!
!<Help KEYWORD="NUM_WIN">
!<Tip> Number of slab windows (informational only). </Tip>
!</Help>
!
!
!<Help KEYWORD="TSTRT">
!<Tip> Starting time on trace (informational only). </Tip>
!</Help>
!
!
!<Help KEYWORD="TSTOP">
!<Tip> Ending time on trace (informational only). </Tip>
!</Help>
!
!
!<Help KEYWORD="WIN_INIT">
!<Tip> Top of each of slab window, in seconds (informational only). </Tip>
!</Help>
!
!
!<Help KEYWORD="WIN_LAST">
!<Tip> Bottom of each of slab window, in seconds (informational only). </Tip>
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module slab_module
      use pc_module
      use named_constants_module
      use mth_module
      use mem_module
      use median_module
      implicit none
      private
      public :: slab_create
      public :: slab_initialize
      public :: slab_update
      public :: slab_delete
!<execute_only>
      public :: slab
      public :: slab_wrapup
!</execute_only>


      character(len=100),public,save :: SLAB_IDENT = &
'$Id: slab.f90,v 1.7 2006/09/11 13:15:51 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: slab_struct              
 
        private
        logical                    :: skip_wrapup      ! wrapup flag.
        integer                    :: ndpt             ! global parameter.
        real                       :: tstrt            ! global parameter.
        real                       :: dt               ! global parameter.
        character(len=3)           :: opt_stat         ! process parameters.
        real                       :: tim_beg          ! process parameters.
        real                       :: win_len          ! process parameters.
        real                       :: win_inc          ! process parameters.
        integer                    :: num_win          ! dependent variables.
        integer,pointer            :: istart  (:)      ! dependent variables.
        integer,pointer            :: istop   (:)      ! dependent variables.
        real   ,pointer            :: win_init(:)      ! dependent variables.
        real   ,pointer            :: win_last(:)      ! dependent variables.

      end type slab_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(slab_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: opt_stat_noptions = 4
      character(len=3),save :: opt_stat_options(opt_stat_noptions)
   
      data opt_stat_options/'MED','AVE','LAV','RMS'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine slab_create (obj)
      implicit none
      type(slab_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%istart)
      nullify (obj%istop)
      nullify (obj%win_init)
      nullify (obj%win_last)

      call slab_initialize (obj)
      return
      end subroutine slab_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine slab_delete (obj)
      implicit none
      type(slab_struct),pointer :: obj       ! arguments

!<execute_only>
      call slab_wrapup (obj)
!</execute_only>

      call mem_free (obj%istart)
      call mem_free (obj%istop)
      call mem_free (obj%win_init)
      call mem_free (obj%win_last)

      deallocate(obj)
      return
      end subroutine slab_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine slab_initialize (obj)
      implicit none
      type(slab_struct),intent(inout) :: obj       ! arguments

      obj%ndpt     = INIL
      obj%tstrt    = FNIL
      obj%dt       = FNIL
      obj%opt_stat = 'MED'
      obj%tim_beg  = FNIL
      obj%win_len  = FNIL
      obj%win_inc  = FNIL

      call slab_update (obj)
      return
      end subroutine slab_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine slab_update (obj)
      implicit none
      type(slab_struct),intent(inout),target :: obj             ! arguments
      integer                                :: iwin            ! local  
      real                                   :: tstop           ! local
      real                                   :: t1,t2           ! local
      integer                                :: i1,i2,lun       ! local  
      integer                                :: itim_beg        ! local  
      integer                                :: iwin_len        ! local  
      integer                                :: iwin_inc        ! local  
      integer                                :: keep_ndpt       ! local  
      real                                   :: keep_tstrt      ! local  
      real                                   :: keep_dt         ! local  
      real                                   :: keep_tim_beg    ! local  
      real                                   :: keep_win_len    ! local  
      real                                   :: keep_win_inc    ! local  

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("win_init_arrayset", (/  &
                                    "win_init",              &
                                    "win_last" /))

      keep_ndpt    = obj%ndpt  
      keep_tstrt   = obj%tstrt 
      keep_dt      = obj%dt  
      keep_tim_beg = obj%tim_beg  
      keep_win_len = obj%win_len  
      keep_win_inc = obj%win_inc  

      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt) 

      call pc_get  ('opt_stat'   , obj%opt_stat)
      call pc_get  ('tim_beg'    , obj%tim_beg)
      call pc_get  ('win_len'    , obj%win_len)
      call pc_get  ('win_inc'    , obj%win_inc)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      select case (obj%opt_stat(1:1))
          case ('M')   ; obj%opt_stat = 'MED'
          case ('m')   ; obj%opt_stat = 'MED'
          case ('A')   ; obj%opt_stat = 'AVE'
          case ('a')   ; obj%opt_stat = 'AVE'
          case ('L')   ; obj%opt_stat = 'LAV'
          case ('l')   ; obj%opt_stat = 'LAV'
          case ('R')   ; obj%opt_stat = 'RMS'
          case ('r')   ; obj%opt_stat = 'RMS'
          case default ; obj%opt_stat = 'MED'
      end select

      if (obj%tim_beg == FNIL) obj%tim_beg = obj%tstrt
      if (obj%win_len == FNIL) obj%win_len = 0.2
      if (obj%win_inc == FNIL) obj%win_inc = obj%win_len

      if (obj%ndpt    /= keep_ndpt    .or. &
          obj%tstrt   /= keep_tstrt   .or. &
          obj%dt      /= keep_dt      .or. &
          obj%tim_beg /= keep_tim_beg .or. &
          obj%win_len /= keep_win_len .or. &
          obj%win_inc /= keep_win_inc) then

           itim_beg = 1 + nint((obj%tim_beg - obj%tstrt) / obj%dt)
           iwin_len =     nint( obj%win_len              / obj%dt)
           iwin_inc =     nint( obj%win_inc              / obj%dt)

           call mth_constrain (iwin_len,        1, obj%ndpt)
           call mth_constrain (iwin_inc, iwin_len, obj%ndpt)
           call mth_constrain (itim_beg,        1, obj%ndpt)

           obj%tim_beg = obj%tstrt + (itim_beg - 1) * obj%dt
           obj%win_len =              iwin_len      * obj%dt
           obj%win_inc =              iwin_inc      * obj%dt

           obj%num_win = (obj%ndpt - itim_beg) / iwin_inc + 1

           call mth_constrain (obj%num_win, 1, obj%ndpt)

           call mem_alloc (obj%istart  , obj%num_win)
           call mem_alloc (obj%istop   , obj%num_win)
           call mem_alloc (obj%win_init, obj%num_win)
           call mem_alloc (obj%win_last, obj%num_win)

           do iwin = 1,obj%num_win
                i1 = itim_beg + (iwin-1) * iwin_inc
                i2 = i1 + iwin_len - 1
                call mth_constrain (i1,  1, obj%ndpt)
                call mth_constrain (i2, i1, obj%ndpt)
                t1 = obj%tstrt + (i1 - 1) * obj%dt
                t2 = obj%tstrt + (i2 - 1) * obj%dt
                obj%win_init  (iwin) = t1
                obj%win_last  (iwin) = t2
                obj%istart(iwin) = i1
                obj%istop (iwin) = i2
           end do

      end if

      if (pc_get_update_state() /= PC_GUI) then
           lun = pc_get_lun()
           do iwin = 1,obj%num_win
                write (lun,1000) iwin,obj%win_init(iwin),obj%win_last(iwin), &
                                 obj%istart(iwin),obj%istop(iwin)
1000            format ('SLAB: window:',I6,  &
                               '   trace times:',F6.3,' to',F6.3, &
                               '   trace indices:',I6,' to',I6)
           end do
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


   call pc_put_options_field ('opt_stat', opt_stat_options, opt_stat_noptions)

      call pc_put  ('opt_stat'   , obj%opt_stat)
      call pc_put  ('tim_beg'    , obj%tim_beg, ndec=3)
      call pc_put  ('win_len'    , obj%win_len, ndec=3)
      call pc_put  ('win_inc'    , obj%win_inc, ndec=3)

      tstop = obj%tstrt + (obj%ndpt - 1) * obj%dt

      call pc_put_gui_only ('num_win', obj%num_win)
      call pc_put_gui_only ('tstrt'  , obj%tstrt)
      call pc_put_gui_only ('tstop'  , tstop)

      call pc_put_gui_only ('win_init', obj%win_init, obj%num_win, ndec=3)
      call pc_put_gui_only ('win_last', obj%win_last, obj%num_win, ndec=3)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.


!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine slab_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>


      subroutine slab (obj,ntr,hd,tr)
      implicit none
      type(slab_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      integer                         :: itr                    ! local

      do itr = 1,ntr
           call slab_solve (obj,hd(1:,itr),tr(1:,itr))
      end do

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call slab_wrapup (obj)
      end if
      return
      end subroutine slab


!!------------------------------- solve ------------------------------------!!
!!------------------------------- solve ------------------------------------!!
!!------------------------------- solve ------------------------------------!!


      subroutine slab_solve (obj,hd,tr)
      implicit none
      type(slab_struct),intent(inout) :: obj                    ! arguments
      double precision ,intent(inout) :: hd(:)                  ! arguments
      real             ,intent(inout) :: tr(:)                  ! arguments
      real                            :: lav,sum,answer,value   ! local
      integer                         :: iwin,i1,i2,nsamp,indx  ! local
      real                            :: trnew(obj%ndpt)        ! local

      trnew(:) = 0.0
      lav      = 0.0
      do iwin = 1,obj%num_win

           sum = 0.0
           i1  = obj%istart(iwin)
           i2  = obj%istop (iwin)
           do indx = i1,i2

                value = tr(indx)
                select case (obj%opt_stat)
                    case ('MED') ; sum = sum + value
                    case ('AVE') ; sum = sum + value
                    case ('LAV') ; sum = max(sum, abs(value))
                    case ('RMS') ; sum = sum + value * value
                end select

           end do
           nsamp = i2 - i1 + 1

           select case (obj%opt_stat)
               case ('MED') ; call median (tr(i1:), nsamp, answer, sum/nsamp)
               case ('AVE') ; answer = sum / nsamp
               case ('LAV') ; answer = sum
               case ('RMS') ; answer = sqrt(sum / nsamp)
           end select

           trnew(i1:i2) = answer
           lav          = max(lav,abs(answer))

      end do

      hd( 2)         = obj%istart(1)
      hd(25)         = lav
      hd(64)         = obj%istop(obj%num_win)
      tr(1:obj%ndpt) = trnew(:)
      return
      end subroutine slab_solve


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine slab_wrapup (obj)
      implicit none
      type(slab_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine slab_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>

      end module slab_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

