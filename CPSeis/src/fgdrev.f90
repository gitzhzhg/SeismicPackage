!<CPS_v1 type="PROCESS"/>
!!------------------------------- fgdrev.f90 ---------------------------------!!
!!------------------------------- fgdrev.f90 ---------------------------------!!
!!------------------------------- fgdrev.f90 ---------------------------------!!


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
! Name       : FGDREV             (reverse FGD)
! Category   : headers
! Written    : 2000-08-28   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Create a field geometry file (.jd) from trace headers.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! FGDREV creates a field geometry file from CPS trace headers.
! See the information on TRACE INPUT REQUIREMENTS for the correct way to
! sort the traces before passing them to this process.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!              CHARACTERISTICS OF THE CREATED FIELD GEOMETRY FILE
!
! Field geometry file header:
!
!  (1) VE is taken from the VE process parameter.
!  (2) REF is taken from the DATUM process parameter.
!  (3) FIXDIST is always set to zero.
!  (4) CHAINING is always set to NONE.
!  (5) The grid transform is taken from the GRID global parameter.
!
! Line Description (LD) cards:
!
!  (1) The LD cards contain only the seismic lines which are occupied by at
!      least one source or receiver, in the order of increasing line number.
!
!  (2) Each seismic line on the LD cards contains flag locations beginning
!      at the first shotpoint occupied by a source or receiver, to the last
!      shotpoint occupied by a source or receiver, in equal shotpoint
!      increments, in the order of increasing shotpoint.  The shotpoint
!      increment will be a multiple of 0.5 shotpoints, between 0.5 and 6.0.
!      Smaller increments will be lumped together.  If there is a need for
!      increments smaller than 0.5 or larger than 6.0, contact the programmer
!      to make some simple code changes.
!
!  (3) If the source and receiver line numbers overlap, the LD cards have
!      the same shotpoint increment for every line.  If they do not overlap,
!      all of the source lines have the same shotpoint increment, and all of
!      the receiver lines have the same shotpoint increment.  In either case,
!      the shotpoint increment is the minimum increment found from all of
!      the source and/or receiver lines.
!
!  (4) The following columns are set from trace headers, with unoccupied
!      locations set to nil:
!        SP#  XLOC  YLOC  ELEV  HD  TUH  LINE
!
!  (5) The following columns are set to nil:
!        DIST  TR  TS  XSD  YSD  ELSD
!
!  (6) No attempt is made to deal with conflicting coordinates for a given
!      shotpoint and line number.  In other words, skids are not accounted
!      for, and it is possible that some of the coordinates put into the
!      LD cards might be skidded coordinates.
!
!  (7) If sources are skidded with respect to the receivers, it would be a
!      good idea to use SETWORD to add a constant to the source (or receiver)
!      lines in the trace headers so that the source and receiver lines do
!      not overlap.  This will allow the sources and receivers to both get
!      their correct coordinates.  This, however, will not resolve the
!      problem of two sources at the same location, or two receivers at the
!      same location, having different coordinates.
!
! Receiver Pattern (RP) cards:
!
!  (1) A separate receiver pattern (consisting of one or more RP cards) is
!      created for each shot profile.  No attempt is made to combine equivalent
!      receiver patterns from different shot profiles.
!
!  (2) If the shot profile is sorted first by receiver line and then by
!      receiver shotpoint, one or more separate RP cards will be created for
!      each receiver line in the receiver pattern for that shot profile.
!
!  (3) If the shot profile is sorted first by receiver shotpoint and then
!      by receiver line, one or more separate RP cards will be created for
!      each receiver shotpoint in the receiver pattern for that shot profile.
!
!  (4) For more complicated ordering of receiver shotpoints and line numbers
!      within a shot profile, quite a few separate RP cards will be created
!      for the receiver pattern for that shot profile.
!
!  (5) No attempt is made to minimize the number of RP cards in a receiver
!      pattern based on a regular rectangular pattern of receiver shotpoints
!      and line numbers.
!
!  (6) The following columns are set from trace headers, with unoccupied
!      locations set to zero:
!        PAT#  FLAG  SP#  LINE#  #X  XINC  #Y  YINC
!
!  (7) The following columns are set to zero:
!        XSD  YSD  ELSD
!
! Profile Pattern (PP) cards:
!
!  (1) A separate profile pattern card is created for each shot profile.
!      No attempt is made to combine profile pattern cards for consecutive
!      shot profiles.
!
!  (2) The following columns are set from trace headers, with unoccupied
!      locations set to nil:
!        (source)SP#  (source)LINE#  (receiver)SP#  (receiver)LINE#
!        PAT#  ELEV  HD  TUH  GRP#
!
!  (3) The following columns are set to nil:
!        XSD  YSD  HOLD  SRC  REC
!
! Zero Trace (ZT) cards:
!
!  (4) No ZT cards are generated.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a single-trace process.
! Traces can be input one or more at a time.
!
! Traces must be sorted into shot profiles in the order that they would
! be input if trace headers were to be applied using the field geometry
! file created by this process.  This usually means that the traces should
! be in the original data acquisition order, although that is not strictly
! required.
!
! In order to keep the number of receiver pattern cards acceptably small,
! it is important that each shot profile be sorted into a regular pattern
! of receiver shotpoints and line numbers.  It is recommended that each
! shot profile be sorted in order of increasing (or decreasing) receiver
! line number, and that each receiver line be sorted in order of increasing
! (or decreasing) receiver shotpoint.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! Traces are passed out in the same ensembles and order as input.
! The traces are not changed in any way.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! GRID     grid transformation structure      used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! No trace headers are changed.
! The following minimum set of trace headers are used:
!
!              9      original shot profile number.
!            11,12    source surveyed easting and northing coordinates.
!             13      source elevation.
!            14,15    receiver surveyed easting and northing coordinates.
!             16      receiver elevation.
!             20      source hole depth.
!             26      source line number.
!             27      receiver line number.
!             28      receiver shotpoint.
!             29      source shotpoint.
!             44      source uphole time.
!
! Header word 9 is used only to distinguish between two consecutive shot
! profiles with the same source location.  If all of the traces in the same
! shot profile do not have the same value of header word 9, that shot profile
! will be split into several (smaller) shot profiles.  If two consecutive
! shot profiles have the same source location, header word 9 must be different
! for each shot profile in order to keep them from being combined into a
! single (larger) shot profile.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date       Author     Description
!     ----       ------     -----------
!010. 2006-10-16 D. Glover  Added NULLIFY statements for Intel compiler.
!009. 2006-01-10 B. Menger  Removed Unused Variables.
!  8. 2005-10-10 Stoeckley  Change temporary files to use TEMPTFILE instead
!                            of Fortran I/O to allow larger file sizes.
!  7. 2005-03-07 Stoeckley  Fix bug whereby source and receiver lines were
!                            sometimes combined when they did not overlap.
!  6. 2001-03-22 Stoeckley  Fix problem of sometimes not writing LD card header.
!  5. 2001-02-27 Stoeckley  Fix typo which had assigned source X location to
!                            source Y location; remove unoccupied extrapolated
!                            portions of seismic lines from LD cards.
!  4. 2001-02-26 Stoeckley  Remove unoccupied seismic lines from LD cards to
!                            reduce file size.
!  3. 2000-12-08 Stoeckley  Change wrapup flag.
!  2. 2000-11-27 Stoeckley  Add ability to save history cards to JD file.
!  1. 2000-08-28 Stoeckley  Initial version.
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.       
! NSTORE          >0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
!    Upon input, NTR must have one of these values:
!      NTR >= 1              means to process the input traces.
!      NTR == NO_MORE_TRACES means no more input traces.      
!
!    Upon output, NTR will have one of these values:
!      NTR >= 1              if this process is outputting traces.
!      NTR == NO_MORE_TRACES if there are no more traces to output.
!      NTR == FATAL_ERROR    if this process has a fatal error.
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
!<NS FGDREV Process/NC=80>
!                 Create Field Geometry File from Trace Headers
!                               (reverse FGD)
!
! VE~~~~~~=`FFFFFFFF (elevation velocity)
! DATUM~~~=`FFFFFFFF (reference elevation)
!
! PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS PATHNAME[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="VE">
!<Tip> Velocity for datum correction. </Tip>
! Default = 8000
! Allowed = real > 0.0
! VE is the velocity used in the datum correction which may be either an
! elevation correction or an up-hole correction.
!</Help>
!
!
!<Help KEYWORD="DATUM">
!<Tip> Datum elevation (reference elevation for datum correction). </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the .jd file written by this process. </Tip>
! Default = from job_data
! Allowed = char
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module fgdrev_module
      use pc_module
      use named_constants_module
      use grid_module
      use pathcheck_module
      use geomio_module
      use geomdata_module
      use increment_module
      use mem_module
      use temptfile_module
      implicit none
      private
      public :: fgdrev_create
      public :: fgdrev_initialize
      public :: fgdrev_update
      public :: fgdrev_delete
      public :: fgdrev
      public :: fgdrev_wrapup

      character(len=100),public,save :: FGDREV_IDENT = &
'$Id: fgdrev.f90,v 1.10 2006/10/17 13:45:44 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: fgdrev_struct              
 
        private
        logical                        :: skip_wrapup     ! wrapup flag.
        type(grid_struct)              :: grid            ! globals.
        real                           :: ve              ! process parameters
        real                           :: datum           ! process parameters
        character(len=FILENAME_LENGTH) :: pathname        ! process parameters
        integer                        :: lun
        type(geomdata_struct),pointer  :: geomdata
        type(temptfile_struct),pointer :: temptfile
        type(increment_struct)         :: source_line  
        type(increment_struct)         :: source_shot  
        type(increment_struct)         :: receiver_line
        type(increment_struct)         :: receiver_shot
        type(increment_struct)         :: any_line  
        type(increment_struct)         :: any_shot  
        integer                        :: ntraces
        character(len=80),pointer      :: hist(:)
        integer                        :: nhist

      end type fgdrev_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(fgdrev_struct),pointer,save :: object     ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine fgdrev_create (obj)
      implicit none
      type(fgdrev_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%hist)
      nullify (obj%temptfile)
      nullify (obj%geomdata) ! jpa

      call geomdata_create (obj%geomdata)

      call fgdrev_initialize (obj)
      return
      end subroutine fgdrev_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine fgdrev_delete (obj)
      implicit none
      type(fgdrev_struct),pointer :: obj       ! arguments

      call fgdrev_wrapup (obj)

      call temptfile_close (obj%temptfile)

      call geomdata_delete (obj%geomdata)

      if (associated(obj%hist)) deallocate (obj%hist)
      deallocate(obj)
      return
      end subroutine fgdrev_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine fgdrev_initialize (obj)
      implicit none
      type(fgdrev_struct),intent(inout) :: obj       ! arguments

      call geomdata_clear (obj%geomdata)

      obj%ve           = 8000.0
      obj%datum        = 0.0
      obj%pathname     = PATHCHECK_EMPTY 

      call fgdrev_update (obj)
      return
      end subroutine fgdrev_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine fgdrev_update (obj)
      implicit none
      type(fgdrev_struct),intent(inout),target :: obj           ! arguments
      integer                                  :: err           ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      obj%lun = pc_get_lun()

      call pc_get_global ('grid'      , obj%grid)

      call pc_get        ('ve'        , obj%ve        )
      call pc_get        ('datum'     , obj%datum     )
      call pc_get        ('pathname'  , obj%pathname  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%ve <= 0.0) call pc_error ('VE must be > 0')

      call pathcheck ('pathname', obj%pathname, 'jd', required = .true.)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put         ('ve'        , obj%ve        )
      call pc_put         ('datum'     , obj%datum     )
      call pc_put         ('pathname'  , obj%pathname  )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call temptfile_close (obj%temptfile)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.       ! to run wrapup code after processing.

      call pc_alloc_process_cards (obj%hist,obj%nhist)

      call increment_init (obj%source_line   , 1.0 , 12)
      call increment_init (obj%source_shot   , 0.5 , 12)
      call increment_init (obj%receiver_line , 1.0 , 12)
      call increment_init (obj%receiver_shot , 0.5 , 12)
      call increment_init (obj%any_line      , 1.0 , 12)
      call increment_init (obj%any_shot      , 0.5 , 12)

      call temptfile_open (obj%temptfile, 'header', 0, 13, obj%lun, err)

      if (err /= TEMPTFILE_OK) then
           call pc_error ('FGDREV: error opening temporary header file')
           call temptfile_close (obj%temptfile)
      endif

      obj%ntraces = 0


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine fgdrev_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine fgdrev (obj,ntr,hd,tr)
      implicit none
      type(fgdrev_struct),intent(inout) :: obj               ! arguments
      integer            ,intent(inout) :: ntr               ! arguments
      double precision   ,intent(inout) :: hd(:,:)           ! arguments
      real               ,intent(inout) :: tr(:,:)           ! arguments

      integer              :: i,err,ntr2                         ! local
      real                 :: source_xloc,receiver_xloc          ! local
      real                 :: source_yloc,receiver_yloc          ! local
      real                 :: source_elev,receiver_elev          ! local
      real                 :: source_shot,receiver_shot          ! local
      integer              :: source_line,receiver_line          ! local
      real                 :: source_llll,receiver_llll          ! local
      real                 :: source_depth,source_uptime         ! local
      integer              :: group                              ! local
      double precision     :: temp_hd(1)                         ! local
      real                 :: temp_tr(77)                        ! local


      ntr2 = ntr
      do i = 1,ntr2

           source_xloc   =      hd(HDR_SOURCE_XLOC       ,i)
           receiver_xloc =      hd(HDR_RECEIVER_XLOC     ,i)
           source_yloc   =      hd(HDR_SOURCE_YLOC       ,i)
           receiver_yloc =      hd(HDR_RECEIVER_YLOC     ,i)
           source_elev   =      hd(HDR_SOURCE_ELEV       ,i)
           receiver_elev =      hd(HDR_RECEIVER_ELEV     ,i)
           source_shot   =      hd(HDR_SOURCE_SHOTPOINT  ,i)
           receiver_shot =      hd(HDR_RECEIVER_SHOTPOINT,i)
           source_llll   =      hd(HDR_SOURCE_LINE       ,i)
           receiver_llll =      hd(HDR_RECEIVER_LINE     ,i)
           source_line   = nint(hd(HDR_SOURCE_LINE       ,i))
           receiver_line = nint(hd(HDR_RECEIVER_LINE     ,i))
           source_depth  =      hd(HDR_SOURCE_DEPTH      ,i)
           source_uptime =      hd(HDR_SOURCE_UPTIME     ,i)
           group         = nint(hd(HDR_ORIGINAL_GROUP    ,i))

           temp_tr( 1) = source_xloc
           temp_tr( 2) = receiver_xloc
           temp_tr( 3) = source_yloc 
           temp_tr( 4) = receiver_yloc
           temp_tr( 5) = source_elev
           temp_tr( 6) = receiver_elev
           temp_tr( 7) = source_shot 
           temp_tr( 8) = receiver_shot
           temp_tr( 9) = source_line
           temp_tr(10) = receiver_line
           temp_tr(11) = source_depth
           temp_tr(12) = source_uptime
           temp_tr(13) = group       

           call temptfile_write (obj%temptfile, 0, temp_hd, temp_tr, err)

           if (err /= TEMPTFILE_OK) then
                call pc_error &
                       ('FGDREV: error writing to temporary trace header file')
                ntr = FATAL_ERROR
                exit
           end if

           call increment_update (obj%source_line  , source_llll  )
           call increment_update (obj%source_shot  , source_shot  )
           call increment_update (obj%receiver_line, receiver_llll)
           call increment_update (obj%receiver_shot, receiver_shot)
           call increment_update (obj%any_line     , source_llll  )
           call increment_update (obj%any_line     , receiver_llll)
           call increment_update (obj%any_shot     , source_shot  )
           call increment_update (obj%any_shot     , receiver_shot)

           obj%ntraces = obj%ntraces + 1

      end do

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call fgdrev_wrapup (obj,ntr)
      end if
      return
      end subroutine fgdrev


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine fgdrev_wrapup (obj,ntr)
      implicit none
      type(fgdrev_struct),intent(inout) :: obj             ! arguments
      integer,optional   ,intent(inout) :: ntr             ! arguments
      logical                           :: error           ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (pc_get_update_state() /= PC_EXECUTE) return

      if (present(ntr)) then
           error = (ntr == FATAL_ERROR)
      else
           error = .false.
      end if

      call fgdrev_work (obj,error)

      if (present(ntr) .and. error) ntr = FATAL_ERROR
      return
      end subroutine fgdrev_wrapup


!!------------------------------- work -------------------------------------!!
!!------------------------------- work -------------------------------------!!
!!------------------------------- work -------------------------------------!!


      subroutine fgdrev_work (obj,error)
      implicit none
      type(fgdrev_struct),intent(inout) :: obj                      ! arguments
      logical            ,intent(inout) :: error                    ! arguments

      real    :: source_llll_inc  , source_llll_min  , source_llll_max   ! local
      real    :: source_shot_inc  , source_shot_min  , source_shot_max   ! local
      integer :: source_line_inc  , source_line_min  , source_line_max   ! local
      real    :: receiver_llll_inc, receiver_llll_min, receiver_llll_max ! local
      real    :: receiver_shot_inc, receiver_shot_min, receiver_shot_max ! local
      integer :: receiver_line_inc, receiver_line_min, receiver_line_max ! local
      real    :: any_llll_inc     , any_llll_min     , any_llll_max      ! local
      real    :: any_shot_inc     , any_shot_min     , any_shot_max      ! local
      integer :: any_line_inc     , any_line_min     , any_line_max      ! local

      integer :: num_source_lines  , num_source_shots                    ! local
      integer :: num_receiver_lines, num_receiver_shots                  ! local
      integer :: num_any_lines     , num_any_shots                       ! local

      integer                           :: iline,ishot              ! local
      real                              :: xmin,xmax                ! local
      integer                           :: nld,nrp,npp,nld2,kount   ! local
      integer                           :: ild,irp,ipp              ! local
      integer                           :: ild_keep,group_keep      ! local
      integer                           :: nchan,maxchan,itrace     ! local
      type(temptfile_struct),pointer    :: temptfile_rp             ! local
      type(temptfile_struct),pointer    :: temptfile_pp             ! local
      integer                           :: iflag                    ! local
      integer                           :: nx,ixinc,ny,iyinc        ! local
      integer                           :: status,err               ! local
      character(len=80)                 :: msg                      ! local
      logical                           :: combine                  ! local
      logical                           :: save_rp_card             ! local

      real            ,parameter        :: FIXDIST  = 0.0           ! local
      character(len=4),parameter        :: CHAINING = 'NONE'        ! local
      real            ,parameter        :: DIST     = FNIL          ! local
      real            ,parameter        :: TR       = FNIL          ! local
      real            ,parameter        :: TS       = FNIL          ! local
      real            ,parameter        :: XSD      = FNIL          ! local
      real            ,parameter        :: YSD      = FNIL          ! local
      real            ,parameter        :: ELSD     = FNIL          ! local
      integer         ,parameter        :: NZT1     = 0             ! local
      integer         ,parameter        :: NZT2     = 0             ! local
      integer         ,parameter        :: NZT3     = 0             ! local
      integer         ,parameter        :: NZT4     = 0             ! local
      real            ,parameter        :: XSD2     = FNIL          ! local
      real            ,parameter        :: YSD2     = FNIL          ! local
      integer         ,parameter        :: HOLD     = INIL          ! local
      integer         ,parameter        :: IS       = INIL          ! local
      integer         ,parameter        :: IR       = INIL          ! local
      integer         ,parameter        :: IFLAGX   = 1             ! local
      integer         ,parameter        :: IFLAGY   = 2             ! local
      real            ,parameter        :: XSD1     = 0.0           ! local
      real            ,parameter        :: YSD1     = 0.0           ! local
      real            ,parameter        :: ELSD1    = 0.0           ! local

      type(geomio_struct)   ,pointer    :: geomio                   ! local
      real                  ,pointer    :: xloc(:),yloc(:)          ! local
      real                  ,pointer    :: shot(:)                  ! local
      real                  ,pointer    :: elev(:),depth(:),tuh(:)  ! local
      integer               ,pointer    :: line(:)                  ! local
      logical               ,pointer    :: occupied(:)              ! local
      integer               ,pointer    :: minld(:),maxld(:)        ! local
      real                              :: rpshot                   ! local
      integer                           :: rpline,rpishot           ! local
      character(len=4)                  :: kflag                    ! local

      real             :: source_xloc,receiver_xloc          ! local
      real             :: source_yloc,receiver_yloc          ! local
      real             :: source_elev,receiver_elev          ! local
      real             :: source_shot,receiver_shot          ! local
      integer          :: source_line,receiver_line          ! local

      real             :: source_depth,source_uptime         ! local
      integer          :: group                              ! local
      double precision :: temp_hd(1)                         ! local
      real             :: temp_tr(77)                        ! local

      integer ::     ild_source       , ild_receiver            ! local
      integer ::     first_source_ild , first_receiver_ild      ! local
      integer ::     last_source_ild  , last_receiver_ild       ! local
      integer ::     first_line, last_line, nrange ,irange      ! local

!------------print header.

      write (obj%lun,   *) ' '
      write (obj%lun,5000) '++ FGDREV PROCESSING SUMMARY ++'
      write (obj%lun,   *) ' '

!------------nullify pointers.

      nullify (geomio)
      nullify (shot)
      nullify (xloc)
      nullify (yloc)
      nullify (elev)
      nullify (depth)
      nullify (tuh)
      nullify (line)
      nullify (occupied)
      nullify (minld)
      nullify (maxld)
      nullify (temptfile_rp)
      nullify (temptfile_pp)

!------------initial printouts.

      write (obj%lun,*) 'FGDREV: number of traces processed: ',obj%ntraces
      write (obj%lun,*) ' '

      if (error) then
           write (obj%lun,*) &
                   'FGDREV: attempting to create a field geometry file'
           write (obj%lun,*) &
                   'FGDREV: even though a previous fatal error has occurred.'
           write (obj%lun,*) ' '
      end if

      if (obj%ntraces == 0) then
           msg = 'no traces available for creating a field geometry file'
           go to 999
      end if

!------------get increment results.

      call increment_result (obj%source_line       , xmin,xmax,          &
                                 source_llll_inc   , source_llll_min,    &
                                 source_llll_max   , num_source_lines)

      call increment_result (obj%source_shot       , xmin,xmax,          &
                                 source_shot_inc   , source_shot_min,    &
                                 source_shot_max   , num_source_shots)

      call increment_result (obj%receiver_line     , xmin,xmax,          &
                                 receiver_llll_inc , receiver_llll_min,  &
                                 receiver_llll_max , num_receiver_lines)

      call increment_result (obj%receiver_shot     , xmin,xmax,          &
                                 receiver_shot_inc , receiver_shot_min,  &
                                 receiver_shot_max , num_receiver_shots)

      call increment_result (obj%any_line          , xmin,xmax,          &
                                 any_llll_inc      , any_llll_min,       &
                                 any_llll_max      , num_any_lines)

      call increment_result (obj%any_shot          , xmin,xmax,          &
                                 any_shot_inc      , any_shot_min,       &
                                 any_shot_max      , num_any_shots)

      source_line_min   = nint(source_llll_min)
      source_line_max   = nint(source_llll_max)
      source_line_inc   = nint(source_llll_inc)
      receiver_line_min = nint(receiver_llll_min)
      receiver_line_max = nint(receiver_llll_max)
      receiver_line_inc = nint(receiver_llll_inc)
      any_line_min      = nint(any_llll_min)
      any_line_max      = nint(any_llll_max)
      any_line_inc      = nint(any_llll_inc)

 !!!  combine = (source_line_max   >= receiver_line_min .or. &
 !!!             receiver_line_max >=   source_line_min)

      combine = .not. (source_line_max   < receiver_line_min .or. &
                       receiver_line_max <   source_line_min)

      if (combine) then
           nld                = num_any_lines * num_any_shots
           first_source_ild   = 1
           last_source_ild    = nld
           first_receiver_ild = 1
           last_receiver_ild  = nld
      else
           nld                = num_source_lines * num_source_shots +  &
                                num_receiver_lines * num_receiver_shots
           first_source_ild   = 1
           last_source_ild    = num_source_lines * num_source_shots
           first_receiver_ild = num_source_lines * num_source_shots + 1
           last_receiver_ild  = nld
      end if

!------------print summary.

      write (obj%lun,1000)

      write (obj%lun,3000) 'source lines:       ',             &
                            source_line_min, source_line_max,  &
                            source_line_inc, num_source_lines

      write (obj%lun,3000) 'receiver lines:     ',                 &
                            receiver_line_min, receiver_line_max,  &
                            receiver_line_inc, num_receiver_lines

      write (obj%lun,3000) 'all lines:          ',       &
                            any_line_min, any_line_max,  &
                            any_line_inc, num_any_lines

      write (obj%lun,2000) 'source shotpoints:  ',             &
                            source_shot_min, source_shot_max,  &
                            source_shot_inc, num_source_shots

      write (obj%lun,2000) 'receiver shotpoints:',                 &
                            receiver_shot_min, receiver_shot_max,  &
                            receiver_shot_inc, num_receiver_shots

      write (obj%lun,2000) 'all shotpoints:     ',       &
                            any_shot_min, any_shot_max,  &
                            any_shot_inc, num_any_shots

      write (obj%lun,*) ' '

 1000 format ('         ',22x,'minimum  maximum  increment  number')
 2000 format (' FGDREV: ',a20,1x,f8.1,1x,f8.1,1x,f8.1,1x,i8)
 3000 format (' FGDREV: ',a20,1x,i6  ,3x,i6  ,3x,i6  ,3x,i8)

!------------combine source and receiver lines if necessary.

      if (combine) then
           source_line_min    = any_line_min
           source_line_max    = any_line_max
           source_line_inc    = any_line_inc
           num_source_lines   = num_any_lines

           source_shot_min    = any_shot_min
           source_shot_max    = any_shot_max
           source_shot_inc    = any_shot_inc
           num_source_shots   = num_any_shots

           receiver_line_min  = any_line_min
           receiver_line_max  = any_line_max
           receiver_line_inc  = any_line_inc
           num_receiver_lines = num_any_lines

           receiver_shot_min  = any_shot_min
           receiver_shot_max  = any_shot_max
           receiver_shot_inc  = any_shot_inc
           num_receiver_shots = num_any_shots
      end if

!------------get line number range to test for occupied lines and their length.

      first_line = min (source_line_min, receiver_line_min)
      last_line  = max (source_line_max, receiver_line_max)
      nrange     = last_line - first_line + 1

        ! index irange will be (line(ild) - first_line + 1).

!------------allocate arrays.

      msg = 'error allocating arrays'

      call mem_alloc (shot     ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (xloc     ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (yloc     ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (elev     ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (depth    ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (tuh      ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (line     ,nld    ,status) ; if (status /= 0) go to 999
      call mem_alloc (occupied ,nrange ,status) ; if (status /= 0) go to 999
      call mem_alloc (minld    ,nrange ,status) ; if (status /= 0) go to 999
      call mem_alloc (maxld    ,nrange ,status) ; if (status /= 0) go to 999

      msg = 'unspecified message - programming error'

!------------initialize LD arrays.

      shot    (:) = FNIL      ! will get reset immediately below.
      xloc    (:) = FNIL
      yloc    (:) = FNIL
      elev    (:) = FNIL
      depth   (:) = FNIL
      tuh     (:) = FNIL
      line    (:) = INIL      ! will get reset immediately below.
      occupied(:) = .false.
      minld   (:) = 0
      maxld   (:) = 0

      do iline = 1,num_source_lines
      do ishot = 1,num_source_shots

           ild = first_source_ild + (ishot - 1) +  &
                                    (iline - 1) * num_source_shots

           if (ild < first_source_ild .or. ild > last_source_ild) then
                call pc_error ('FGDREV: programming error with source ILD')
                stop
           end if

           shot(ild) = source_shot_min + (ishot - 1) * source_shot_inc
           line(ild) = source_line_min + (iline - 1) * source_line_inc

      end do
      end do

      do iline = 1,num_receiver_lines
      do ishot = 1,num_receiver_shots

           ild = first_receiver_ild + (ishot - 1) +  &
                                      (iline - 1) * num_receiver_shots

           if (ild < first_receiver_ild .or. ild > last_receiver_ild) then
                call pc_error ('FGDREV: programming error with receiver ILD')
                stop
           end if

           shot(ild) = receiver_shot_min + (ishot - 1) * receiver_shot_inc
           line(ild) = receiver_line_min + (iline - 1) * receiver_line_inc

      end do
      end do

!------------start reading through temporary trace header file.

      call temptfile_open (temptfile_rp, 'rp', 0, 8, obj%lun, err)

      if (err /= TEMPTFILE_OK) then
           call pc_error ('FGDREV: error opening temporary RP file')
           msg = 'unsuccessful attempt to open temporary RP card file'
           go to 999
      endif

      call temptfile_open (temptfile_pp, 'pp', 0, 7, obj%lun, err)

      if (err /= TEMPTFILE_OK) then
           call pc_error ('FGDREV: error opening temporary PP file')
           msg = 'unsuccessful attempt to open temporary PP card file'
           go to 999
      endif

      call temptfile_rewind (obj%temptfile)

      nrp        = 0
      npp        = 0
      ild_keep   = INIL
      group_keep = INIL
      nchan      = 0
      maxchan    = 0
      nx         = 0
      ny         = 0

      do itrace = 1,obj%ntraces

           call temptfile_read (obj%temptfile, 0, temp_hd, temp_tr, err)

           if (err /= TEMPTFILE_OK) then
                msg = 'error reading from temporary trace header file'
                go to 999
           end if

           source_xloc    =      temp_tr( 1)
           receiver_xloc  =      temp_tr( 2)
           source_yloc    =      temp_tr( 3)
           receiver_yloc  =      temp_tr( 4)
           source_elev    =      temp_tr( 5)
           receiver_elev  =      temp_tr( 6)
           source_shot    =      temp_tr( 7)
           receiver_shot  =      temp_tr( 8)
           source_line    =      temp_tr( 9)
           receiver_line  =      temp_tr(10)
           source_depth   =      temp_tr(11)
           source_uptime  =      temp_tr(12)
           group          = nint(temp_tr(13))

!------------update source LD information.

           iline = 1 +      (source_line - source_line_min) / source_line_inc
           ishot = 1 + nint((source_shot - source_shot_min) / source_shot_inc)

           if (iline < 1 .or. iline > num_source_lines) then
                msg = 'error getting source line index from trace header file'
                go to 999
           end if

           if (ishot < 1 .or. ishot > num_source_shots) then
                msg = 'error getting source shot index from trace header file'
                go to 999
           end if

           ild = first_source_ild + (ishot - 1) +  &
                                    (iline - 1) * num_source_shots

           if (ild < first_source_ild .or. ild > last_source_ild) then
                msg = 'error getting source ILD from trace header file'
                go to 999
           end if

           if (nint((shot(ild) - source_shot) / source_shot_inc) /= 0) then
                msg = 'error matching source shotpoint'
                go to 999
           end if

           if (line(ild) /= source_line) then
                msg = 'error matching source line'
                go to 999
           end if

!!                                shot    (ild) = source_shot   ! already set.
!!                                line    (ild) = source_line   ! already set.
           if (xloc(ild) == FNIL) xloc    (ild) = source_xloc
           if (yloc(ild) == FNIL) yloc    (ild) = source_yloc
           if (elev(ild) == FNIL) elev    (ild) = source_elev
                                  depth   (ild) = source_depth
                                  tuh     (ild) = source_uptime

!!   Not overwriting xloc, yloc, and elev guarantees that receiver values
!!   will prevail at locations occupied by both sources and receivers.
!!   This allows for the possibility in the future to determine source
!!   skids for the PP cards.

           ild_source = ild      ! source LD card number.

           irange = line(ild) - first_line + 1
           if (occupied(irange)) then
                minld   (irange) = min(minld(irange),ild)
                maxld   (irange) = max(maxld(irange),ild)
           else
                occupied(irange) = .true.
                minld   (irange) = ild
                maxld   (irange) = ild
           end if

!------------update receiver LD information.

     iline = 1 +      (receiver_line - receiver_line_min) / receiver_line_inc
     ishot = 1 + nint((receiver_shot - receiver_shot_min) / receiver_shot_inc)

           if (iline < 1 .or. iline > num_receiver_lines) then
                msg = 'error getting receiver line index from trace header file'
                go to 999
           end if

           if (ishot < 1 .or. ishot > num_receiver_shots) then
                msg = 'error getting receiver shot index from trace header file'
                go to 999
           end if

           ild = first_receiver_ild + (ishot - 1) +  &
                                      (iline - 1) * num_receiver_shots

           if (ild < first_receiver_ild .or. ild > last_receiver_ild) then
                msg = 'error getting receiver ILD from trace header file'
                go to 999
           end if

           if (nint((shot(ild) - receiver_shot) / receiver_shot_inc) /= 0) then
                msg = 'error matching receiver shotpoint'
                go to 999
           end if

           if (line(ild) /= receiver_line) then
                msg = 'error matching receiver line'
                go to 999
           end if

!!         shot    (ild) = receiver_shot                     ! already set.
!!         line    (ild) = receiver_line                     ! already set.
           xloc    (ild) = receiver_xloc           
           yloc    (ild) = receiver_yloc           
           elev    (ild) = receiver_elev           

           ild_receiver = ild      ! receiver LD card number (not used).

           irange = line(ild) - first_line + 1
           if (occupied(irange)) then
                minld   (irange) = min(minld(irange),ild)
                maxld   (irange) = max(maxld(irange),ild)
           else
                occupied(irange) = .true.
                minld   (irange) = ild
                maxld   (irange) = ild
           end if

!------------check whether we are starting a new shot profile.

           if (ild_source /= ild_keep .or. group /= group_keep) then

!------------write previous RP card to temporary RP card file.

                if (nx > 0 .and. ny > 0) then
                     temp_tr(1) = npp
                     temp_tr(2) = iflag
                     temp_tr(3) = rpshot
                     temp_tr(4) = rpline
                     temp_tr(5) = nx    
                     temp_tr(6) = ixinc
                     temp_tr(7) = ny    
                     temp_tr(8) = iyinc

                     call temptfile_write &
                                  (temptfile_rp, 0, temp_hd, temp_tr, err)

                     if (err /= TEMPTFILE_OK) then
                          msg = 'error writing to temporary RP card file'
                          go to 999
                     end if

                     nrp = nrp + 1
                     nx  = 0
                     ny  = 0
                end if

!------------write PP card to temporary PP card file.

                temp_tr(1) = source_shot
                temp_tr(2) = source_line
                temp_tr(3) = receiver_shot
                temp_tr(4) = receiver_line
                temp_tr(5) = source_elev
                temp_tr(6) = source_depth
                temp_tr(7) = source_uptime

                call temptfile_write (temptfile_pp, 0, temp_hd, temp_tr, err)

                if (err /= TEMPTFILE_OK) then
                     msg = 'error writing to temporary PP card file'
                     go to 999
                end if

!------------end check whether we are starting a new shot profile.

                npp        = npp + 1
                ild_keep   = ild_source
                group_keep = group
                nchan      = 0
                nx         = 0
                ny         = 0

           end if

!------------add next channel to the current RP card.

           nchan = nchan + 1
           maxchan = max(nchan,maxchan)

           ishot = 1 + nint((receiver_shot - receiver_shot_min) &
                                      / receiver_shot_inc)

           save_rp_card = .false.

           if (nx == 0 .and. ny == 0) then

                iflag   = IFLAGX
                rpshot  = receiver_shot         ! shotpoint.
                rpline  = receiver_line         ! line number.
                rpishot = ishot                 ! shot index.
                nx      = 1
                ixinc   = 1
                ny      = 1
                iyinc   = 1

           else if (nx == 1 .and. ny == 1) then

                if (receiver_line == rpline) then
                     nx    = 2
                     ixinc = ishot - rpishot
                else if (receiver_shot == rpshot) then
                     iflag = IFLAGY
                     ny    = 2
                     iyinc = receiver_line - rpline
                else
                     save_rp_card = .true.
                end if

           else if (nx > 1) then

                if (receiver_line == rpline .and. &
                    ishot - rpishot == nx * ixinc) then
                     nx = nx + 1
                else
                     save_rp_card = .true.
                end if

           else   !   if (ny > 1) then

                if (ishot == rpishot .and. &
                    receiver_line - rpline == ny * iyinc) then
                     ny = ny + 1
                else
                     save_rp_card = .true.
                end if

           end if

!------------write next RP card to temporary RP card file if necessary.

           if (save_rp_card) then
                temp_tr(1) = npp
                temp_tr(2) = iflag
                temp_tr(3) = rpshot
                temp_tr(4) = rpline
                temp_tr(5) = nx    
                temp_tr(6) = ixinc
                temp_tr(7) = ny    
                temp_tr(8) = iyinc

                call temptfile_write (temptfile_rp, 0, temp_hd, temp_tr, err)

                if (err /= TEMPTFILE_OK) then
                     msg = 'error writing to temporary RP card file'
                     go to 999
                end if

                nrp     = nrp + 1
                iflag   = IFLAGX
                rpshot  = receiver_shot         ! shotpoint.
                rpline  = receiver_line         ! line number.
                rpishot = ishot                 ! shot index.
                nx      = 1
                ixinc   = 1
                ny      = 1
                iyinc   = 1
           end if

!------------finish reading through temporary trace header file.

      end do

!------------write last RP card to temporary RP card file.

      if (nchan > 0) then

           temp_tr(1) = npp
           temp_tr(2) = iflag
           temp_tr(3) = rpshot
           temp_tr(4) = rpline
           temp_tr(5) = nx    
           temp_tr(6) = ixinc
           temp_tr(7) = ny    
           temp_tr(8) = iyinc

           call temptfile_write (temptfile_rp, 0, temp_hd, temp_tr, err)

           if (err /= TEMPTFILE_OK) then
                msg = 'error writing to temporary RP card file'
                go to 999
           end if

           nrp = nrp + 1
      end if

!------------count the LD cards of occupied lines.

      nld2 = 0
      do irange = 1,nrange
        if (occupied(irange)) nld2 = nld2 + maxld(irange) - minld(irange) + 1
      end do

!------------print additional summary.

      write (obj%lun,*) ' '
      write (obj%lun,*) 'FGDREV: maximum number of channels = ',maxchan
      write (obj%lun,*) ' '
      write (obj%lun,*) 'FGDREV: creating ',nld2,' LD cards'
      write (obj%lun,*) 'FGDREV: creating ',nrp ,' RP cards'
      write (obj%lun,*) 'FGDREV: creating ',npp ,' PP cards'
      write (obj%lun,*) 'FGDREV: creating ',NZT1,' ZT1 cards'
      write (obj%lun,*) 'FGDREV: creating ',NZT2,' ZT2 cards'
      write (obj%lun,*) 'FGDREV: creating ',NZT3,' ZT3 cards'
      write (obj%lun,*) 'FGDREV: creating ',NZT4,' ZT4 cards'
      write (obj%lun,*) ' '

!------------open field geometry file.

      call geomio_open_write (geomio,obj%pathname,err,msg,                &
                              obj%ve,obj%datum,FIXDIST,obj%grid,          &
                              CHAINING,nld2,nrp,npp,NZT1,NZT2,NZT3,NZT4,  &
                              obj%hist,obj%nhist,'FGDREV')
 !!!                          obj%hist,obj%nhist,'FGDREV',GEOMIO_OLDCPS)
 !!!                          obj%hist,obj%nhist,'FGDREV',GEOMIO_ASCII)
 !!!                          obj%hist,obj%nhist,'FGDREV',GEOMIO_BINARY)

      if (err /= GEOMIO_OK) go to 999

!------------save LD cards to field geometry file.

      kount = 0
      do irange = 1,nrange
           if (.not.occupied(irange)) cycle     ! skip the unoccupied lines.
           do ild = minld(irange),maxld(irange)
                kount = kount + 1
                call geomio_write_ld_card (geomio,kount,err,msg,            &
                        shot(ild),DIST,xloc(ild),yloc(ild),elev(ild),       &
                        depth(ild),tuh(ild),TR,TS,XSD,YSD,ELSD,line(ild))
                if (err /= GEOMIO_OK) go to 999
           end do
      end do
      if (kount /= nld2) then
           msg = 'error eliminating unoccupied lines - programming error'
           go to 999
      end if

!------------save RP cards to field geometry file.

      call temptfile_rewind (temptfile_rp)

      do irp  = 1,nrp

           call temptfile_read (temptfile_rp, 0, temp_hd, temp_tr, err)

           if (err /= TEMPTFILE_OK) then
                msg = 'error reading from temporary RP card file'
                go to 999
           end if

           ipp           = nint(temp_tr(1))
           iflag         = nint(temp_tr(2))
           receiver_shot =      temp_tr(3)
           receiver_line = nint(temp_tr(4))
           nx            = nint(temp_tr(5))
           ixinc         = nint(temp_tr(6))
           ny            = nint(temp_tr(7))
           iyinc         = nint(temp_tr(8))

           kflag = 'X'
           if (iflag == IFLAGY) kflag = 'Y'

           call geomio_write_rp_card (geomio,irp,err,msg,  &
                   ipp,kflag,receiver_shot,receiver_line,  &
                   nx,ixinc,ny,iyinc,XSD1,YSD1,ELSD1)

           if (err /= GEOMIO_OK) go to 999

      end do

!------------save PP cards to field geometry file.

      call temptfile_rewind (temptfile_pp)

      do ipp = 1,npp

           call temptfile_read (temptfile_pp, 0, temp_hd, temp_tr, err)

           if (err /= TEMPTFILE_OK) then
                msg = 'error reading from temporary PP card file'
                go to 999
           end if

           source_shot   =      temp_tr(1)
           source_line   = nint(temp_tr(2))
           receiver_shot =      temp_tr(3)
           receiver_line = nint(temp_tr(4))
           source_elev   =      temp_tr(5)
           source_depth  =      temp_tr(6)
           source_uptime =      temp_tr(7)

           call geomio_write_pp_card (geomio,ipp,err,msg,                  &
                  source_shot, source_line, receiver_shot, receiver_line,  &
                  ipp,XSD2,YSD2,HOLD,                                      &
                  source_elev, source_depth, source_uptime, IS,IR,ipp)

           if (err /= GEOMIO_OK) go to 999

      end do

!------------successful return.

      call pc_print ('FGDREV: successful completion')
      error = .false.
      go to 888

!------------error return.

999   continue

      call pc_error ('FGDREV: ',msg)
      call pc_error ('FGDREV: unsuccessful completion')
      error = .true.

!------------all returns come to here.

888   continue

      if (associated(geomio)) call geomio_close (geomio)

      call temptfile_close (obj%temptfile)
      call temptfile_close (temptfile_rp)
      call temptfile_close (temptfile_pp)

      call mem_free (shot)
      call mem_free (xloc)
      call mem_free (yloc)
      call mem_free (elev)
      call mem_free (depth)
      call mem_free (tuh)
      call mem_free (line)
      call mem_free (occupied)
      call mem_free (minld)
      call mem_free (maxld)

!------------print footer.

      write (obj%lun,   *) ' '
      write (obj%lun,5000) ' END FGDREV PROCESSING SUMMARY '
      write (obj%lun,   *) ' '

5000  format (1x,'+++++++++++++++++++++++++++++++',  &
                 '+++++++++++++++++++++++++++++++',  &
                 '+++++++++++++++++++++++++++++++'/  &
              1x,'+++++++++++++++++++++++++++++++',  &
                           a31,                      &
                 '+++++++++++++++++++++++++++++++'/  &
              1x,'+++++++++++++++++++++++++++++++',  &
                 '+++++++++++++++++++++++++++++++',  &
                 '+++++++++++++++++++++++++++++++')
      return
      end subroutine fgdrev_work


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module fgdrev_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

