!<CPS_v1 type="PROCESS"/>
!!------------------------------- vtrin.f90 ---------------------------------!!
!!------------------------------- vtrin.f90 ---------------------------------!!
!!------------------------------- vtrin.f90 ---------------------------------!!
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
!                         C P S   P R O C E S S
!
! Name       : VTRIN
! Category   : IO
! Written    : 2004-01-26   by: R.S.Day
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Input a velocity file as input traces
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! The user specifies an input file name for a velocity model. VTRIN extracts
! velocity records from the file and feeds them to the processing flow as
! seismic traces one record at a time. The input file can be any format
! recognized by the CPS modgrid primitive.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! The input file has to currently be a time or depth ordered file. A file that
! contains depth or time slices is not legitimate.
! 
! Do not confuse the line and record number from the path_vel file with survey
! line and shot information. Velocity files normally specify what CPS header
! words are consistent with the records in the file.
!
! Normally the positions of records in the velocity file are set in terms
! of either survey coordinates (ie. xloc,yloc=17,18) or grid coordinates
! (i.e. xgrid,ygrid=7,8) . If the velocity file geometry is set in terms of
! the xgrid,ygrid headers, then the grid transform will be used to set the
! xloc,yloc headers. If xloc,yloc are used in the velocity file, then 7,8 
! will be set by vtrin from the grid transform.
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
!
! No input traces needed. VTRIN is a trace source process.
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!
! This is a trace-supplying process.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! NDPT      number of sample values in trace        set from path_vel
! TSTRT     starting time on trace                  set from path_vel
! DT        trace sample interval                   set from path_vel
! GATHERED                                          false
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
!
! HDR     Description                Action taken
! ----    -----------                ------------
! 1       Sequence counter           To number of the output record
! 2       Top mute                   Set by mute module
!25       LAV                        Set by LAV module
! ?       hdx                        Set from info in path_vel
! ?       hdy                        Set from info in path_vel
!48       in line position           Set from info in path_vel
!49       line position              Set from info in path_vel
!50       trace no                   Input file record in line number
!51       line no                    Input file line number
!64       Bottom mute                Set by mute module
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2007-11-29  Stoeckley      Remove unused reference to memman.
!  6. 2006-06-12  B. Menger      Removed Unused Variables.
!  5  2005-01-31  R.S.Day        Quit when fatal error detected. Check if
!                                headers are properly set.
!  4  2004-03-02  R.S.Day        buffer larger chunks of CPSVEL files to reduce
!                                overhead of slow ascii reads.
!  3  2004-02-16  R.S.Day        Added velocity clip and scale parameters.
!  2  2004-02-05  R.S.Day        Added parameter to scale the output DT.
!  1  2004-01-26  R.S.Day        Original version.
!-----------------------------------------------------------------------
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
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!
!<NS VTRIN-VELOCITY FILE TO TRACES/NC=80>
!
!  SELECT PATH_VEL [PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!  BIN_HDR_X=`C          BIN_HDR_Y=`C
!
!  TR_MAX=`IIIIIII       SKIP_INIT=`IIIIIII
!  NUM_DO=`IIIIIII       NUM_SKIP~=`IIIIIII
!
!  Settings from PATH_VEL 
!  pv_ndpt=`IIIII   pv_tstrt=`FFFFFFFF  pv_dt=`FFFFFFFF
!  file_type=`AAAAAAAAAAA
!
!  DEP_SCALE=`FFFFFFFF
!
!  VEL_SCALE=`FFFFFFF    VEL_CLIP_MIN=`FFFFFFF     VEL_CLIP_MAX=`FFFFFFF
!<PARMS PATH_VEL[/ML=104/XST]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="BIN_HDR_X">
!<Tip> Use this header when scanning trace files for bin info</Tip>
! Default = 7
! Allowed = 17
! Allowed = 37
! Vtrin scans segy and cps trace files to figure out if the traces
! fall into a regular bin pattern. It will use bin_hdr_x as the header
! with x bin information
!</Help>
!
!<Help KEYWORD="BIN_HDR_Y">
!<Tip> Use this header when scanning trace files for bin info </Tip>
! Default = 8
! Allowed = 18
! Vtrin scans segy and cps trace files to figure out if the traces
! fall into a regular bin pattern. It will use bin_hdr_y as the header
! with y bin information
!</Help>
!
!<Help KEYWORD="FILE_TYPE">
!<Tip> The classification of path_vel according to modgrid</Tip>
! Default = UNKNOWN
! Allowed = Character
! Informative - The file type according to the modgrid module 
!</Help>
!
!<Help KEYWORD="DEP_SCALE">
!<Tip> Scale the  time-depth axis for the output traces</Tip>
! Default = 1.0
! Allowed = Real>0
! The output tstrt and dt will be scaled by this factor.
! If the sample rate in the PATH_VEL file is PV_DT then
! the DT global coming out of VTRIN will be DT= PV_DT/DEP_SCALE
!</Help>
!
!<Help KEYWORD="VEL_SCALE">
!<Tip> Scale the  output velocity trace values</Tip>
! Default = 1.0
! Allowed = Real
!  tr(i) = min(vel_clip_max, max(velocity,velocity(i)*vel_cale))
!</Help>
!
!<Help KEYWORD="VEL_CLIP_MIN">
!<Tip> Clip the  output velocity trace values</Tip>
! Default = 0.0
! Allowed = Real
!  tr(i) = min(vel_clip_max, max(velocity,velocity(i)*vel_cale))
!</Help>
!
!<Help KEYWORD="VEL_CLIP_MAX">
!<Tip> Clip the  output velocity trace values</Tip>
! Default = 25000.0
! Allowed = Real
!  tr(i) = min(vel_clip_max, max(velocity,velocity(i)*vel_cale))
!</Help>
!
!<Help KEYWORD="PV_DT">
!<Tip> The record sample rate in the path_vel file</Tip>
! Default = 1.
! Allowed = Real
! Informative - The T or Z bin size in a path_vel record 
!</Help>
!
!<Help KEYWORD="PV_NDPT">
!<Tip> The number of samples in a record of the file path_vel</Tip>
! Default = 1
! Allowed = Integer > 0
! Informative - The number of samples in a path_vel record
!</Help>
!
!<Help KEYWORD="PV_TSTRT">
!<Tip> The record origin for the input file path_vel</Tip>
! Default = 0.
! Allowed = 
! Informative - The T or Z origin in a path_vel record
!</Help>
!
!
!<Help KEYWORD="NUM_DO">
!<Tip> Controls skip pattern for input records</Tip>
! Default = 1
! Allowed = >0
! Read num_do records before skipping num_skip record
!</Help>
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Controls skip pattern for input records</Tip>
! Default = 0
! Allowed = 
! Read num_do records before skipping num_skip record
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> Name of a file with velocity data</Tip>
! Default = NONE
! Allowed = Character
! Any file with velocity information. The file should not be
! in time or depth slice order
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> File selection dialogue</Tip>
! Default = 
! Allowed = 
!
!</Help>
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Skip this many velocity records in the input file</Tip>
! Default = 0
! Allowed = >0
! Skip this many velocity records in the input file.
!</Help>
!
!<Help KEYWORD="TR_MAX">
!<Tip> Maximum number of records to output</Tip>
! Default = 99999
! Allowed = >0
! The maximum number of records to output to the processing stream
!</Help>
!</HelpSection>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


   module vtrin_module
   !
      use pc_module
      use named_constants_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use modgrid_module
      use lav_module
      use mutehw_module
      use velio_module


! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: vtrin_create
      public :: vtrin_initialize
      public :: vtrin_update
      public :: vtrin_delete
      public :: vtrin            ! main trace processing routine.
      public :: vtrin_wrapup

  ! rcs identifier string
  character(len=100),public,save :: VTRIN_IDENT = &
  '$Id: vtrin.f90,v 1.7 2007/11/30 13:55:20 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: vtrin_struct

        private

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        logical                    :: gathered ! 
        real                       :: tstrt    ! time of 1st trace sample(sec)
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        logical                    :: skip_wrapup      ! wrapup flag.
        logical                    :: fatal_error 

        type(pathchoose_struct), pointer :: select_path_vel
        character(len=16)          :: file_type
        character(len=2)           :: bin_hdr_x
        character(len=2)           :: bin_hdr_y
        integer                    :: iscanx
        integer                    :: iscany
        integer                    :: pv_ndpt
        real                       :: pv_tstrt
        real                       :: pv_dt
        real                       :: dep_scale
        real                       :: vel_scale
        real                       :: vel_clip_min
        real                       :: vel_clip_max
        character(len=16)          :: ftype
        integer                    :: num_do
        integer                    :: num_skip
        character(len=104)         :: path_vel
        integer                    :: skip_init
        integer                    :: tr_max

        integer                    :: ocnt   !output record counter
        integer                    :: omax   !max possible records
        integer                    :: ngrp   !group counter for ndo
        integer                    :: rig    !record in group counter
        type(modgrid_struct),pointer :: mobj
        integer                    :: ihd(2) !in-line,cross-line header
        integer                    :: ng(3)
        real                       :: og(3)
        real                       :: dg(3)
        integer                    :: traces_per_line
        integer                    :: num_lines

      end type vtrin_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(vtrin_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine vtrin_create (obj)
      type(vtrin_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
        call pc_error ("vtrin_create: ERROR, Unable to allocate obj ")
        obj%fatal_error = .true.
        return
      endif


      nullify ( obj%select_path_vel )
      nullify ( obj%mobj )
      call pathchoose_create ( obj%select_path_vel,  'path_vel',  '*vo'  )

      call vtrin_initialize (obj)
      end subroutine vtrin_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine vtrin_delete (obj)
      type(vtrin_struct),pointer:: obj       ! arguments
      integer                   :: ierr      ! for error checking

      if ( associated ( obj%select_path_vel ) )  &
       call pathchoose_delete ( obj%select_path_vel )

      call vtrin_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      if (associated(obj%mobj)) call modgrid_delete (obj%mobj)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in vtrin_delete")
      end subroutine vtrin_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine vtrin_initialize (obj)
      type(vtrin_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%bin_hdr_x = '7'
      obj%bin_hdr_y = '8'
      obj%file_type = 'UNKNOWN'
      obj%pv_ndpt   = 1
      obj%pv_tstrt  = 0.0
      obj%pv_dt     = 1.0
      obj%dep_scale = 1.0
      obj%vel_scale = 1.0
      obj%vel_clip_min = 0.0
      obj%vel_clip_max = 25000.0
      obj%num_do    = 1
      obj%num_skip  = 0
      obj%path_vel  = pathcheck_empty
      obj%skip_init = 0
      obj%tr_max    = 999999
      obj% ocnt = 0
      obj% omax = 1
      obj% ngrp = 1
      obj% rig  = 1

      obj%fatal_error = .false.
!
      obj%traces_per_line= 0
      obj%num_lines      = 0
      obj%ihd(1:2)       = 0
     !call pc_get_global ('ndpt' , obj%pv_ndpt)
     !call pc_get_global ('tstrt', obj%pv_tstrt)
     !call pc_get_global ('dt'   , obj%pv_dt)
      obj%numtr=1

      call vtrin_update (obj)
      end subroutine vtrin_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine vtrin_update (obj)
      type(vtrin_struct),intent(inout),target :: obj             ! arguments
      character(len=104) :: path_local


      integer,save       :: first_call=1
      character(len=104) :: msg

! --> Insert code to declare all required local variables.

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.
      obj%fatal_error = .false.
      path_local = obj%path_vel
      if(pathchoose_update(obj%select_path_vel, obj%path_vel      ) ) return

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

! --> Delete any of the globals below that are not needed:

      obj%gathered = .false.
      obj%ipn = pc_get_ipn()

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('BIN_HDR_X', obj%bin_hdr_x)
      call pc_get('BIN_HDR_Y', obj%bin_hdr_y)
      call pc_get('FILE_TYPE', obj%file_type)
      call pc_get('PV_NDPT'  , obj%pv_ndpt)
      call pc_get('PV_TSTRT' , obj%pv_tstrt)
      call pc_get('PV_DT'    , obj%pv_dt)
      call pc_get('DEP_SCALE', obj%dep_scale)
      call pc_get('VEL_SCALE', obj%vel_scale)
      call pc_get('VEL_CLIP_MIN', obj%vel_clip_min)
      call pc_get('VEL_CLIP_MAX', obj%vel_clip_max)
      call pc_get('NUM_DO   ', obj%num_do)
      call pc_get('NUM_SKIP ', obj%num_skip)
      call pc_get('PATH_VEL ', obj%path_vel)
      call pc_get('SKIP_INIT', obj%skip_init)
      call pc_get('TR_MAX   ', obj%tr_max)

      obj%skip_init = max(0,obj%skip_init)
      if(.not.associated(obj%mobj) .or. &
         path_local /= obj%path_vel) then
        call vtrin_get_vinfo(obj, 0) !get info and delete modgrid obj
        first_call=0
      endif
      if(obj%dep_scale==0) obj%dep_scale=1.0
     !write(msg,'(A,i6)') 'vtrin: ndpt=',obj%ndpt
     !call pc_info(msg)

      obj%skip_init = min(obj%omax-1,obj%skip_init)
      obj%tr_max    = min(obj%omax-obj%skip_init,obj%tr_max)
      obj%num_do    = max(1,obj%num_do)
      obj%num_skip  = max(0,obj%num_skip)


      call pc_put_options_field('BIN_HDR_X', (/' 7', '17', '33'/) )
      call pc_put_options_field('BIN_HDR_Y', (/' 8', '18', '34'/) )

     !call pc_put_sensitive_field_flag ('num_do'   , .false.)
     !call pc_put_sensitive_field_flag ('num_skip' , .false.)
      call pc_put_sensitive_field_flag ('pv_ndpt'  , .false.)
      call pc_put_sensitive_field_flag ('pv_tstrt' , .false.)
      call pc_put_sensitive_field_flag ('pv_dt'    , .false.)
      call pc_put_sensitive_field_flag ('file_type', .false.)

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('gathered'     , .false.)

      call pc_put('BIN_HDR_X', obj%bin_hdr_x)
      call pc_put('BIN_HDR_Y', obj%bin_hdr_y)
      call pc_put('FILE_TYPE', obj%file_type)
      call pc_put('PV_DT    ', obj%pv_dt)
      call pc_put('DEP_SCALE', obj%dep_scale)
      call pc_put('VEL_SCALE', obj%vel_scale)
      call pc_put('VEL_CLIP_MIN', obj%vel_clip_min)
      call pc_put('VEL_CLIP_MAX', obj%vel_clip_max)
      call pc_put('PV_TSTRT ', obj%pv_tstrt)
      call pc_put('PV_NDPT  ', obj%pv_ndpt)
      call pc_put('NUM_DO   ', obj%num_do)
      call pc_put('NUM_SKIP ', obj%num_skip)
      call pc_put('PATH_VEL ', obj%path_vel)
      call pc_put('SKIP_INIT', obj%skip_init)
      call pc_put('TR_MAX   ', obj%tr_max)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.


      if (pc_do_not_process_traces()) then
        return   ! in case of allocation errors.
      endif

      write(msg,'(A)') 'vtrin_update:2 calling vtrin_get_info'
      call pc_info(msg)
      call vtrin_get_vinfo(obj, 0) !get info and keep modgrid obj
      if(.not. obj%fatal_error) then
        call modgrid_print(obj%mobj,lunprint)
      endif

      return
      end subroutine vtrin_update

      subroutine vtrin_get_vinfo(obj, delete_mod)
      type(vtrin_struct),intent(inout):: obj             ! arguments
      integer,intent(in) :: delete_mod
      character(len=96)  :: dfile
      character(len=8)   :: wtype
      character(len=16)  :: ftype
      double precision   :: dfsize
     !character(len=16)  :: vel_type
      integer            :: i_err
      integer            :: rank
      integer            :: hd(3),ng(3)
      real               :: og(3),dg(3)
      integer            :: ix,iy,iz
      integer            :: i

      if(obj%path_vel==pathcheck_empty) return
      if(obj%path_vel==' ') return
      if(associated(obj%mobj)) call modgrid_delete(obj%mobj)
      ftype = modgrid_ftype(obj%path_vel,lunprint,dfsize)
      if(ftype=='CPSVEL' .and. dfsize> 100000000) then
        call pc_info('vtrin_get_vinfo: scanning big CPSVEL input file&
        & - go get a coffee')
      endif
      !vel_type=' ' !do no conversion
      i_err = modgrid_rddesc(obj%mobj,&
       obj%path_vel,lunprint,dfile,wtype,ftype,obj%iscanx, obj%iscany)
      if(i_err /=0) then
        call pc_info('vtrin_get_vinfo: ERROR, modgrid_rddesc problem')
        obj%fatal_error = .true.
        return
      endif
      hd=-1
      obj%file_type = ftype
      do i = 1,min(3,rank)
        call modgrid_get_griddesc(obj%mobj,i, hd(i),ng(i),og(i),dg(i))
      enddo
      i_err = modgrid_xyz_order(obj%mobj,ix,iy,iz)
      if(i_err /=0) then
        call pc_info('vtrin_get_vinfo: ERROR, modgrid_xyz_order problem')
        obj%fatal_error = .true.
      endif
      !
      obj%traces_per_line = ng(ix)
      obj%num_lines = ng(iy)
      obj%ihd(1) = hd(ix)
      obj%ihd(2) = hd(iy)
      if(obj%ihd(1)<0 .or. obj%ihd(2)<0) then
        call pc_info('vtrin_get_vinfo: ERROR, headers not set?')
        obj%fatal_error = .true.
      endif
      if(obj%fatal_error) return
      if(ix > iy) then
        obj%traces_per_line = ng(iy)
        obj%num_lines = ng(ix)
        obj%ihd(1) = hd(iy)
        obj%ihd(2) = hd(ix)
      endif
      obj%omax = ng(ix)*ng(iy)
      obj%ng   = ng
      obj%og   = og
      obj%dg   = dg
      obj%iscanx = 7
      obj%iscany = 8
      read(obj%bin_hdr_x,'(I2)') obj%iscanx
      read(obj%bin_hdr_y,'(I2)') obj%iscany

      obj%ndpt  = ng(iz)
      obj%dt    = dg(iz)/obj%dep_scale
      obj%tstrt = og(iz)/obj%dep_scale

      obj%pv_ndpt = ng(iz)
      obj%pv_dt   = dg(iz)
      obj%pv_tstrt= og(iz)
      if(iz/=1) then
        call pc_info('vtrin_get_vinfo: ERROR file is not trace ordered')
        obj%fatal_error = .true.
        return
      endif
     !print *,'traces_per_line=',obj%traces_per_line
     !print *,'num_lines=',obj%num_lines
      if(delete_mod/=0) then
        if (associated(obj%mobj)) call modgrid_delete (obj%mobj)
      endif
      return
      end subroutine vtrin_get_vinfo

      subroutine vtrin (obj,ntr,hd,tr)
      type(vtrin_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer   :: rnum
      integer   :: i_err

      tr(:,1) = 0.0
      hd(:,1) = 0.0
      if(obj%fatal_error) then
        call pc_info('vtrin: check your setup ERRORs. ')
       ntr = FATAL_ERROR
      endif
      if(ntr==NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call vtrin_wrapup (obj)
        return
      end if
      if(obj%ocnt < obj%tr_max .and. obj%ocnt<= obj%omax) then
        ! get a velocity trace
        ntr = 1
        !rnum = obj%skip_init + obj%ocnt + 1
        rnum = vtrin_next_rec_to_get(obj)
        i_err = vtrin_get_vrecord(obj,rnum,hd(:,1) , tr(:,1))
        if(i_err/=0) then
          ntr=FATAL_ERROR
          ntr=NO_MORE_TRACES
          return
        endif
      else
        !done with outputing traces
        ntr = NO_MORE_TRACES
      endif

      return
      end subroutine vtrin

      integer function vtrin_get_vrecord(obj, rnum, hd, tr) result(status)
      type(vtrin_struct),intent(inout):: obj       ! arguments
      integer,intent(in)             :: rnum
      real,intent(inout)             :: tr(:)
      double precision,intent(inout) :: hd(:)
      !
      type(modgrid_struct),pointer   :: mobj
      character(len=16)              :: ftype
      integer   :: j1,j2
      integer   :: sslice,nslice
      integer   :: npts
      real      :: x1,x2
      integer   :: i_err
      double precision :: xloc,yloc,xgrid,ygrid
      status = -1
      hd(:) = 0.0
      tr(:) = 0.0
      mobj => obj%mobj
      if(.not. associated(mobj)) then
        call pc_info("vtrin_get_vrecord: error mobj not allocated")
        obj%fatal_error = .true.
        return
      endif
      ! check type of input file
      ftype = modgrid_ftype(mobj)
      !convert trace number to line and cross line position
      !rnum = (j2-1)*n_cross_line + j1
      ! j1 = trace index within line
      ! j2 = index of line.( The input slice to retrieve)
      if(rnum > obj%omax) return
      j2 = 1 + (rnum-1)/obj%traces_per_line
      j2 = max(j2,1)
      j2 = min(j2,obj%num_lines)
      j1 = rnum  - (j2-1)*obj%traces_per_line
      j1 = max(j1,1)
      j1 = min(j1,obj%traces_per_line)

      sslice=j2 
      nslice=1
      !                               start and end slices
      if(.not. modgrid_is_in_mem(mobj,j2,j2)) then
        if(modgrid_size(mobj)  < 10000000) then
          ! read in a reasonable chunk of data each time
          nslice = obj%num_lines
        endif
        if(ftype=='CPSVEL') nslice = min(1000,obj%num_lines -j2 + 1)
       !print *,' j2=',j2,' nslice=',nslice,' rnum=',rnum
        i_err = modgrid_rd_data(mobj,lunprint,j2,nslice)
        if(i_err /=0) then
          call pc_info("vtrin_get_vrecord: error in modgrid_rd_data call")
          return
        endif
      endif

      !
      ! Set output header words
      hd(HDR_SEQUENCE) = obj%ocnt + 1
      hd(HDR_FOLD)   = 1
      hd(HDR_CURRENT_CHANNEL) = j1 !cps 4
      hd(HDR_CURRENT_GROUP)   = j2 !cps 3
    ! hd(HDR_MIDPOINT_SHOTPOINT)= j1
    ! hd(HDR_MIDPOINT_LINE)   = j2
      x1 = obj%og(2) + (j1-1)*obj%dg(2)
      x2 = obj%og(3) + (j2-1)*obj%dg(3)


      hd(obj%ihd(1)) = x1
      hd(obj%ihd(2)) = x2
      if(obj%ihd(1)==HDR_MIDPOINT_XGRID .or.&
         obj%ihd(2)==HDR_MIDPOINT_XGRID) then
        xgrid = x1
        ygrid = x2
        if(obj%ihd(2)==HDR_MIDPOINT_XGRID) then
          xgrid = x2
          ygrid = x1
        endif
        call grid_get_survey_coords (obj%grid, xgrid, ygrid, xloc , yloc )
        hd(HDR_MIDPOINT_XLOC) = xloc
        hd(HDR_MIDPOINT_YLOC) = yloc
      endif
      if(obj%ihd(1)==HDR_MIDPOINT_XLOC .or.&
         obj%ihd(2)==HDR_MIDPOINT_XLOC) then
        xloc = x1
        yloc = x2
        if(obj%ihd(2)==HDR_MIDPOINT_XLOC) then
          xloc = x2
          yloc = x1
        endif
        call grid_get_grid_coords (obj%grid,xloc, yloc,  xgrid, ygrid)
        hd(HDR_MIDPOINT_XGRID) = xgrid
        hd(HDR_MIDPOINT_YGRID) = ygrid
      endif
      if(ftype == 'SEGY' .or. ftype=='TRCIO') then
        if(obj%ocnt<5) then
        call pc_info('vtrin_get_vrecord: input trace headers not used')
        endif
      endif

      hd(HDR_USER_48) = x1
      hd(HDR_USER_49) = x2
      hd(HDR_USER_50) = j1
      hd(HDR_USER_51) = j2
      !
      ! Get the appropriate record from the slice in memory
      ! call modgrid_set_modspec_zgrid(mobj,nz,oz,dz)
      npts = obj%ng(1) !may be changed by modgrid_get_data
      i_err = modgrid_get_data(mobj,tr,npts=npts,&
              stride=1, u=1,v=j1,w=sslice)
      if(i_err < 0) then
        call pc_info("vtrin_get_vrecord: error in modgrid_get_data call")
        print *,' vtrin_get_vrecord: j1=',j1
        print *,' vtrin_get_vrecord: sslice=',sslice
        print *,' vtrin_get_vrecord: npts=',npts
        call modgrid_print(obj%mobj,lunprint)
        return
      endif
      do j1 = 1,npts
        tr(j1) = tr(j1) * obj%vel_scale
        tr(j1) = max(tr(j1),obj%vel_clip_min)
        tr(j1) = min(tr(j1),obj%vel_clip_max)
      enddo
      !
      ! Make sure lav and mutes are set      
      call mutehw(hd(:),tr(:),npts,0.0,0)
      call lav_set_hdr (hd(:),tr(:),npts)
      !
      ! increment the counter of output records
      obj%ocnt = obj%ocnt + 1
      status = 0
      return
      end function vtrin_get_vrecord
 
      integer function vtrin_next_rec_to_get(obj) result(rnum)
      type(vtrin_struct),intent(inout):: obj       ! arguments
      integer   ::  ngrp    !current trace group
      integer   ::  rig     !current trace in group
      ! calculate the next record to obtain 
      ! from the current state of the do-skip counters
       rig  = max(1,obj%rig)
       ngrp = max(1,obj%ngrp)
      !rnum = obj%skip_init + obj%ocnt + 1
       rnum = obj%skip_init + (ngrp-1)*(obj%num_do+ obj%num_skip) + rig
       ! update the counters
       rig = rig + 1
       if(rig > obj%num_do) then
         ngrp = ngrp + 1
         rig  = 1
       endif
       obj%ngrp = ngrp
       obj%rig  = rig
        
      return
      end function vtrin_next_rec_to_get

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
      subroutine vtrin_wrapup (obj)
      type(vtrin_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine vtrin_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module vtrin_module

