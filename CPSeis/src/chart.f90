!<CPS_v1 type="PROCESS"/>
!!------------------------------- chart.f90 ---------------------------------!!
!!------------------------------- chart.f90 ---------------------------------!!
!!------------------------------- chart.f90 ---------------------------------!!


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
! Name       : CHART    (generate ground position or stacking chart)
! Category   : headers
! Written    : 1989-12-31   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Generate a ground position or stacking chart from trace headers.
! Portability: No known limitations, but see portability issues below.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! CHART will produce a GROUND POSITION CHART if HDR_LEN=47 or 35, otherwise it
! will produce a STACKING CHART.  Normally the header words are set as follows.
!
!
!                                 COORDS      HDR_LEN    HDR_WID (3-D)
!                                 ------        ---       ---------
!     Stacking chart             surveyed       17           18
!     Stacking chart               grid          7            8
!     Ground position chart     sequential      47         0,26,27,38
!     Ground position chart        grid         35           36
!
! HDR_WID must be set to 0 for a 2D chart.
!
! For ground position charts, header word 46 is also used when 47 is specified
! and 33 is used when 35 is specified.  Headers 26,27,38 are source, receiver,
! and CMP line numbers respectively.
! 
! Traces that fall outside the range of the specified header word values will 
! not be plotted.  You can use this fact to restrict your chart to a subset of 
! your data.
!
! CHART generates the chart from information in the trace headers.
! The traces do not have to be in any particular order.
!
! 3D Charts
!
! For 3-D data, you will get a sequential ground position chart for all your 
! lines at once if you specify HDR_LEN=47 and HDR_WID=0.  However, you will 
! normally have to call CHART separately to get a stacking chart, or grid 
! ground position chart, for each HDR_WID value.
!
!
! Chart Annotation
!
! The entire shot profile number (header word 9) is printed vertically 
! immediately preceding the first location (source or receiver or CMP) for that
! shot profile.
!
! Receivers (or CMP's) and sources are plotted using the following symbols.  
! The symbols for the receivers are listed with highest overriding priority 
! first.
!
!                       RECEIVER (OR CMP) SYMBOLS
!
!     Dead receiver                           x   (X every fifth column)
!     Reverse polarity receiver               +   (if header 25 is negative)
!     Beginning (first) receiver in group     a
!     End (last) receiver in group            e
!     Multiple live receivers                 m   (M every fifth column)
!     One live receiver                       .   (: every fifth column)
!
!                           SOURCE SYMBOLS
!
!     Source with no receivers at all         S   (For SPACING=DOUBLE,
!     Source with receivers forward           F   plotted to the left
!     Source with receivers reverse           R   if there is a receiver
!     Source with receivers on both sides     B   at the same position)
!
!             ADDITIONAL SOURCE SYMBOLS for SPACING=SINGLE
!
!     Source and dead receiver                D   (plotted over x or X)
!     Source and reverse polarity receiver    P   (plotted over +)
!     Source and beginning receiver           U   (plotted over a)
!     Source and end receiver                 V   (plotted over e)
!     Source and multiple live receivers      T   (plotted over m or M)
!     Source and one live receiver            L   (plotted over . or :)
!
! Note that reverse polarity receivers are identified with + only if header
! word 25 is negative.  Since this is a violation of the standard for header
! word 25 (largest absolute value on the trace), this will normally not be
! the case.  However, if CHART is called internally from FGD, which has the
! ability to reverse the polarity of a trace based upon the field geometry
! description, FGD will set header 25 negative for internal calls to CHART.
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
! No special requirements.
! Traces can be in any order.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process does not alter input traces.
! This process outputs the same traces as it receives.
! This process outputs traces with same gather status as the input traces.
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
! NWIH     number of words in trace header       used but not changed
! GRID     grid transformation structure         used but not changed
! 
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! 
! See details in General Description.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
! 31. 2006-06-20  B. Menger    Removed Unused Variables.
! 30. 2001-02-13  Stoeckley    Change wrapup flag.
! 29. 2000-10-19  Stoeckley    Add missing context-sensitive help.
! 28. 2000-10-06  Stoeckley    Add required missing documentation sections.
! 27. 2000-08-24  Stoeckley    Change defaults for LEN_LAST and LEN_TOT to
!                               999; add printouts of min/max values.
! 26. 2000-05-02  Stoeckley    Fix problem which caused aborts on linux and
!                               extremely slow operation on sun: Changed
!                               dimension of obj%h from 8 to 80, which matches
!                               dimension in called subroutines (obj_h).
!                               See portability issues below.
! 25. 2000-04-28  Stoeckley    Fix error in getting SURVEY_UNITS.
! 24. 2000-04-10  Stoeckley    Add GUI definition section; remove the MODE
!                               parameter.
! 23. 2000-02-15  Stoeckley    Improve detection and error reporting of
!                               missing globals.
! 22. 2000-02-04  Stoeckley    Improved use of wrapup flag to keep from
!                               executing wrapup code from front end.
! 21. 2000-01-24  Stoeckley    Converted from old system.  Plotting option
!                               not yet implemented.
! 20. 1998-12-16  K. Goodger   Begin using the fortran90 compiler.  
! 19. 1993-04-21  K. Goodger   Insert leading zeros if necessary to 
!                               insure a 5 character job name for conplot.
! 18. 1993-04-20  K. Goodger   Recompile with static option.           
! 17. 1992-05-07  K. Goodger   Remove documentation on DEV parameter as
!                               this option has not been completed and is
!                               available only through FGD.
! 16. 1991-10-28  K. Goodger   Get info from common blocks from FGD if
!                               CHART run as separate process and want plot.
!                               Get NQS to pass to CONPLOT for vector file name.
! 15. 1991-09-23  D Peterson   Comment out CALL JDGH code in IF (JD) THEN
! 14. 1991-09-23  D Peterson   Clear rest of CBUF character array after
!                               writing to it. UNICOS writes aprox. 277
!                               characters and the rest to 640 is garbage.
! 13. 1991-04-16  K. Goodger   Change lower case chars to upper if OPS.
!                               Calculate plot length.
! 12. 1991-02-26  Goodger      Add option to draw vertical lines every
!                               20 CDP's on plotter option.
! 11. 1991-02-20  Goodger      Add option to output charts to plotter.
! 10. 1990-08-27  Stoeckley    Change how shotpoints and line numbers are
!                               printed when information is not available
!                               in trace headers.
!  9. 1990-07-10  Stoeckley    Add legend to chart.
!  8. 1990-07-09  Stoeckley    Add beginning and end receiver flag, add
!                               MODE=HEADERS2, modify some of the symbols
!                               to use the extra column when SPACING=DOUBLE,
!                               and eliminate subroutine CHARTA.
!  7. 1990-07-05  Stoeckley    Add English-metric flag, make some print
!                               format changes, add SPACING parameter, and
!                               add End Receiver flag.
!  6. 1990-05-25  Stoeckley    Fix reverse polarity ident when MODE=FGD.
!  5. 1990-05-16  Stoeckley    Add option to generate chart from FGD.
!                               Also add reverse polarity symbol.
!  4. 1990-05-04  Stoeckley    Fix bug in calculation for length of line.
!                               Also improve calculation of shotpoints.
!  3. 1990-04-23  Stoeckley    Add option to generate chart from the JD.
!                               Change plot width from 104 to 80 columns.
!  2. 1990-03-12  Stoeckley    Minor chart format improvements.
!  1. 1989-12-13  Stoeckley    Original version converted from Conseis. 
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                            PORTABILITY ISSUES
!
! This primitive was originally written with the data structure array H(:)
! typed as character(len=8).  This array was then used only in the subroutines
! CHART_ADD and CHART_PRINT and declared there either as len=80 or len=640,
! depending on whether the chart was to be printed or plotted.  (Currently
! only printing works and not plotting.)
!
! In the old Cray Fortran-77 code, the CHART_ADD and CHART_PRINT subroutines
! were each replaced with two identical subroutines with the only difference
! being the character length 80 or 640.  In Fortran-90, the length can be
! passed as an argument, making it possible to use the same subroutine for
! both lengths.
!
! However, it turns out that on the sun, CHART ran EXTREMELY slow, and
! on linux, CHART ran quite slow with the Portland Group compiler, but
! fast with the Absoft compiler.  It also turned out that CHART aborted
! under some circumstances with both linux compilers.
!
! I then tried declaring array H(:) to be len=80 instead of 8 (and reduced
! the allocated dimension by a factor of 8).  Now all three compilers work,
! and the sun code sped up by a large factor to be comparable to the Portland
! Group code.  (This change made the formal argument length the same as the
! actual length - for printing but not plotting).
!
! Then I tried coding the formal parameter (in CHART_ADD only) to len=* instead
! of using a passed-in argument which was set to 80.  This sped up the sun code
! to the same speed as the Absoft code, but had no effect on the Portland
! Group code.
!
! Therefore I am leaving things hardwired to len=80 and len=*, which is fine
! for printing but not plotting.  When plotting capability is added, the best
! thing to do will probably to have pointers to two arrays H(:) in the data
! structure, with lengths 80 and 640, and to allocate and use the correct
! one.  Then either the two subroutines can be called with the correct array
! and the length argument (as currently), or each subroutine can be made into
! two subroutines with the two lengths.
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
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!
! This process always outputs the same NTR as it received upon input.
! This process never outputs NTR == FATAL_ERROR.
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
!                            PROGRAMMING NOTES                 
! 
!  1. To change the width of the plot, replace thc current width (80)
!     by the desired width everywhere it occurs.  Make sure the desired
!     width is evenly divisible by 8.  It is preferable for this width
!     also to be evenly divisible by 5 (or by 10 for SPACING=DOUBLE).
!     The only statements that should have to be changed are:
!                DATA IWIDTH/80/
!                CHARACTER*80 H(*)
!                format statements containing A80 or 80X
!                memory requirements in documentation:  80*(#X+6)-8
!
!  2. It would be good to use the entire width of the IBM laser printer
!     paper for the plot.  At 15 characters-inch, there are probably
!     about 164 columns available on the IBM laser printer, although
!     only 132 columns are actually plotted, possibly because the
!     program that sends the file to the IBM laser printer only sends
!     the first 132 characters of each line.  Note that the limit
!     is 132 columns on the Geophysics printer, and (for ease of view
!     using EVV) 132 columns at one time in EVV.
!
!  3. Examples of plot widths:
!                                #words-plot   plot width   #chars-line
!     Early version of CHART         13           104           152
!     Probable IBM laser maximum     14.5         116           164
!     Geophysics and EVV maximum     10.5          84           132
!     Current version of CHART       10            80           130
!     CONP version of CHART          80           640           688
!
!     Notes:  (1) As the program is now written, it is necessary for
!                 the #words-plot (at 8 chars-word) to be a whole
!                 number.
!             (2) It is preferable for the plot width to be divisible
!                 by 5 (or by 10 for SPACING=DOUBLE), so that every
!                 fifth column (which contains special characters to
!                 allow it to be more easily followed) will always
!                 correspond to a group number divisible by 5.
!
!  4. Currently the DEVICE must be PRINTER only.  Code lines labelled
!     "! temporary" are to be removed when this restriction is lifted.
!     Code lines commented out with "!!!" (three exclamation points) are
!     code from the old CPS system and will have to be reactivated or
!     rewritten when the restriction is lifted.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS CHART Process/NC=70>
!
! Generate a Ground Position Chart or a Stacking Chart from Traces
!
!               SPACING=~~`CCCCCCC    DEVICE=`CCCCCCC
!
! PROF_INIT=`IIIIIIII PROF_LAST=`IIIIIIII     SURVEY_UNITS=`XXXXXXXX
!
! HDR_LEN=~~`IIIIII (7 17 47 35)
!
! LEN_INIT= `FFFFFF LEN_INC=`FFFFFF LEN_LAST=`FFFFFF LEN_TOT=`IIIIII
!
! HDR_WID=~~`IIIIII (8 18 0 26 27 38 36)
!
! WID_INIT= `FFFFFF WID_INC=`FFFFFF  (only one Y bin is plotted)
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="SURVEY_UNITS">
!<Tip> Units (feet or meters) used in the surveyed coordinate system. </Tip>
!</Help>
!
!<Help KEYWORD="SPACING">
!<Tip> Whether to use single or double column spacing. </Tip>
! Default = SINGLE
! Allowed = SINGLE  (Less wrap-around, harder to read.)
! Allowed = DOUBLE  (More wrap-around, easier to read.)  
!</Help>
!
!<Help KEYWORD="DEVICE">
!<Tip> Device on which to put the chart (printer or plotter). </Tip>
! Default = PRINTER
! Allowed = PRINTER or OPS or CONP
! Currently only the PRINTER choice is available.
!</Help>
!
!<Help KEYWORD="PROF_INIT">
!<Tip> Value of header word 9 for first shot profile to plot. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="PROF_LAST">
!<Tip> Value of header word 9 for last shot profile to plot. </Tip>
! Default = 999
! Allowed = int > SP_INIT
!</Help>
!
!<Help KEYWORD="HDR_LEN">
!<Tip> Header word labeling plot length coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!                               COORDS     HDR_LEN     HDR_WID
!                               ------     -------     -------
!     Stacking chart           surveyed      17        18 or 0
!     Stacking chart             grid         7         8 or 0
!     Ground position chart   sequential     47       0,26,27,38
!     Ground position chart      grid        35          36
!
! HDR_WID must be set to 0 for a 2D chart.
!
! For ground position charts, header word 46 is also used when 47 is specified
! and 33 is used when 35 is specified.  Headers 26,27,38 are source, receiver,
! and CMP line numbers respectively.
!</Help>
!
!<Help KEYWORD="HDR_WID">
!<Tip> Header word labeling plot width coordinate. </Tip>
! Default = 8
! Allowed = 0 - NWIH
!                               COORDS     HDR_LEN     HDR_WID
!                               ------     -------     -------
!     Stacking chart           surveyed      17        18 or 0
!     Stacking chart             grid         7         8 or 0
!     Ground position chart   sequential     47       0,26,27,38
!     Ground position chart      grid        35          36
!
! HDR_WID must be set to 0 for a 2D chart.
!
! For ground position charts, header word 46 is also used when 47 is specified
! and 33 is used when 35 is specified.  Headers 26,27,38 are source, receiver,
! and CMP line numbers respectively.
!</Help>
!
!<Help KEYWORD="LEN_INIT">
!<Tip> Initial value for plot length header word to plot. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="WID_INIT">
!<Tip> Only value for plot width header word to plot. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="LEN_INC">
!<Tip> Increment for plot length header word. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="WID_INC">
!<Tip> Increment for plot width header word. </Tip>
! Default = 1.0
! Allowed = real>0.0
! Only one value of HDR_WID is plotted
!</Help>
!
!<Help KEYWORD="LEN_LAST">
!<Tip> Last value of plot length header word to plot. </Tip>
! Default = 999.0
! Allowed = real>=LEN_INIT
!</Help>
!
!<Help KEYWORD="LEN_TOT">
!<Tip> Total number of plot length header word values to plot. </Tip>
! Default = 999
! Allowed = int>0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module chart_module
      use pc_module
      use named_constants_module
      use grid_module 
      implicit none
      private
      public :: chart_create
      public :: chart_initialize
      public :: chart_update
      public :: chart_delete
!<execute_only>
      public :: chart
      public :: chart_wrapup
!</execute_only>

      character(len=100),public,save :: chart_IDENT = &
       '$Id: chart.f90,v 1.31 2006/06/20 13:11:49 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: chart_struct              
 
        private
        logical                  :: skip_wrapup      ! wrapup flag
        character(len=8)         :: survey_units     ! project data
        character(len=8)         :: spacing          ! process parameters
        character(len=8)         :: device           ! process parameters
        integer                  :: prof_init        ! process parameters
        integer                  :: prof_last        ! process parameters
        integer                  :: hdr_len          ! process parameters
        real                     :: len_init         ! process parameters
        real                     :: len_inc          ! process parameters
        real                     :: len_last         ! process parameters
        integer                  :: len_tot          ! process parameters
        integer                  :: hdr_wid          ! process parameters
        real                     :: wid_init         ! process parameters
        real                     :: wid_inc          ! process parameters
        real                     :: xwidth           ! dependent parameters
        integer                  :: ng               ! dependent parameters
        integer                  :: nhs              ! dependent parameters
        real                     :: splo             ! dependent parameters
        real                     :: spup             ! dependent parameters
        integer                  :: iblo             ! dependent parameters
        integer                  :: ibup             ! dependent parameters
        real                     :: offmin           ! dependent parameters
        real                     :: offmax           ! dependent parameters
        integer                  :: maxchan          ! dependent parameters
        integer                  :: kount            ! dependent parameters
        integer                  :: iuse             ! dependent parameters
        integer                  :: idead            ! dependent parameters
!       character(len=8),pointer :: h      (:)       ! dependent parameters
!       character(len=1),pointer :: h      (:)       ! dependent parameters
        character(len=80),pointer :: h      (:)       ! dependent parameters
                                       ! sometimes h is len=80 and len=640.
        integer         ,pointer :: ifirst (:)       ! dependent parameters
        integer         ,pointer :: isource(:)       ! dependent parameters
        integer         ,pointer :: live   (:)       ! dependent parameters
        integer         ,pointer :: nfold  (:)       ! dependent parameters
        real            ,pointer :: sp     (:)       ! dependent parameters
        integer         ,pointer :: ichan1 (:)       ! dependent parameters
        integer         ,pointer :: ichan2 (:)       ! dependent parameters
        character(len=8),pointer :: frflag (:)       ! dependent parameters
        integer                  :: iwidth           ! dependent parameters
        integer                  :: iwidth2          ! dependent parameters
        integer                  :: ispace           ! dependent parameters
        integer                  :: ipt              ! dependent parameters
        real                     :: scal             ! dependent parameters
        real                     :: gmin,gmax        ! dependent parameters
        real                     :: xmin,xmax,x      ! dependent parameters
        real                     :: ymin,ymax,y      ! dependent parameters
        integer                  :: ian              ! dependent parameters

      end type chart_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(chart_struct),pointer,save :: object      ! needed for traps.

      real   ,parameter     :: csize = 0.05

      integer,parameter     :: spacing_noptions = 2
      integer,parameter     :: device_noptions  = 3
      character(len=8),save :: spacing_options (spacing_noptions)
      character(len=8),save :: device_options  (device_noptions)
   
      data spacing_options /'SINGLE','DOUBLE'/
      data device_options  /'PRINTER','OPS','CONP'/
   
      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine chart_create (obj)
      implicit none
      type(chart_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%h      )
      nullify (obj%ifirst )
      nullify (obj%isource)
      nullify (obj%live   )
      nullify (obj%nfold  )
      nullify (obj%sp     )
      nullify (obj%ichan1 )
      nullify (obj%ichan2 )
      nullify (obj%frflag )

      call chart_initialize (obj)
      return
      end subroutine chart_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine chart_delete (obj)
      implicit none
      type(chart_struct),pointer :: obj       ! arguments

!<execute_only>
      call chart_wrapup (obj)
!</execute_only>

      if (associated(obj%h      )) deallocate (obj%h      )
      if (associated(obj%ifirst )) deallocate (obj%ifirst )
      if (associated(obj%isource)) deallocate (obj%isource)
      if (associated(obj%live   )) deallocate (obj%live   )
      if (associated(obj%nfold  )) deallocate (obj%nfold  )
      if (associated(obj%sp     )) deallocate (obj%sp     )
      if (associated(obj%ichan1 )) deallocate (obj%ichan1 )
      if (associated(obj%ichan2 )) deallocate (obj%ichan2 )
      if (associated(obj%frflag )) deallocate (obj%frflag )

      deallocate(obj)
      return
      end subroutine chart_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine chart_initialize (obj)
      implicit none
      type(chart_struct),intent(inout) :: obj                ! arguments

      obj%survey_units = "METERS"
      obj%spacing      = "SINGLE"
      obj%device       = "PRINTER"
      obj%prof_init    = 1.0
      obj%prof_last    = 999.0
      obj%hdr_len      = 7
      obj%len_init     = 1.0
      obj%len_inc      = 1.0
      obj%len_last     = 999.0
      obj%len_tot      = 999
      obj%hdr_wid      = 0
      obj%wid_init     = 1.0
      obj%wid_inc      = 1.0

      call chart_update (obj)
      return
      end subroutine chart_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine chart_update (obj)
      implicit none
      type(chart_struct),intent(inout),target :: obj          ! arguments
      integer           :: len_last                           ! local
      integer           :: nstore,nscratch,need,nwih ! local
      type(grid_struct) :: grid                               ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      nwih = -1
      call grid_initialize (grid)

      len_last = obj%len_last

      call pc_get_pdata  ('survey_units', obj%survey_units)
      call pc_get_global ('nwih'        , nwih)
      call pc_get_global ('grid'        , grid)

      call pc_get ('spacing'  , obj%spacing  )
      call pc_get ('device'   , obj%device   )
      call pc_get ('prof_init', obj%prof_init)
      call pc_get ('prof_last', obj%prof_last)
      call pc_get ('hdr_len'  , obj%hdr_len  )
      call pc_get ('len_init' , obj%len_init )
      call pc_get ('len_inc'  , obj%len_inc  )
      call pc_get ('len_last' , obj%len_last )
      call pc_get ('len_tot'  , obj%len_tot  )
      call pc_get ('hdr_wid'  , obj%hdr_wid  )
      call pc_get ('wid_init' , obj%wid_init )
      call pc_get ('wid_inc'  , obj%wid_inc  )

      if (obj%len_last /= len_last .and. obj%len_inc /= 0.0) then
           obj%len_tot = nint((obj%len_last - obj%len_init)/obj%len_inc) + 1
      end if
      obj%len_last = obj%len_init + (obj%len_tot - 1) * obj%len_inc

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (nwih <= 0) then
           call pc_error ('NWIH not found in the parameter cache')
           nwih = HDR_NOMINAL_SIZE   ! to allow code to work correctly.
      end if

      if (.not.pc_global_keyword_present('grid')) then
           call pc_warning ('grid transform not found in the parameter cache')
           call pc_warning ('identity transform used instead')
      end if

      if (obj%spacing /= "SINGLE" .and. obj%spacing /= "DOUBLE") then
           call pc_error ('invalid option for SPACING')
      end if
      if (obj%device /= "PRINTER" .and. obj%device /= "OPS" .and.  &
          obj%device /= 'CONP') then
           call pc_error ('invalid option for DEVICE')
      end if
      if (obj%device /= "PRINTER") then                            ! temporary
           call pc_error ('currently DEVICE can be PRINTER only')  ! temporary
      end if                                                       ! temporary
      obj%ng = nint(obj%prof_last - obj%prof_init + 1.0)
      if (obj%ng < 1) then
           call pc_error ('PROF_LAST must be greater than PROF_INIT')
      end if
      if (obj%len_tot < 1) then
           call pc_error ('LEN_TOT must be greater than zero')
      end if
      if (obj%hdr_len < 1 .or. obj%hdr_len > nwih) then
           call pc_error ('HDR_LEN is out of range')
      end if
      if (obj%hdr_wid < 0 .or. obj%hdr_wid > nwih) then
           call pc_error ('HDR_WID is out of range')
      end if
      if (obj%len_inc <= 0.0) then
           call pc_error ('LEN_INC mut be greater than zero')
      end if
      if (obj%len_last < obj%len_init) then
           call pc_error ('LEN_LAST must be greater than LEN_INIT')
      end if
      if (obj%wid_inc <= 0.0) then
           call pc_error ('WID_INC mut be greater than zero')
      end if

!----------SET VALUES RELATING TO PLOT WIDTH.

      if (obj%spacing == "SINGLE") obj%ispace = 1
      if (obj%spacing == "DOUBLE") obj%ispace = 2
      obj%iwidth = 80
      if (obj%device /= "PRINTER") obj%iwidth = 640
      obj%iwidth2 = obj%iwidth/obj%ispace

!----------CALCULATE STORAGE.

      need     = obj%iwidth*(obj%len_tot+6)/8    ! number of 8-byte characters.
      nstore   = need + 5*obj%ng + 3*obj%len_tot
      nscratch = 0


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('spacing', spacing_options, spacing_noptions)
      call pc_put_options_field ('device' , device_options , device_noptions)

      call pc_put ('spacing'  , obj%spacing     )
      call pc_put ('device'   , obj%device      )
      call pc_put ('prof_init', obj%prof_init   )
      call pc_put ('prof_last', obj%prof_last   )
      call pc_put ('hdr_len'  , obj%hdr_len     )
      call pc_put ('len_init' , obj%len_init , 7)
      call pc_put ('len_inc'  , obj%len_inc  , 7)
      call pc_put ('len_last' , obj%len_last , 7)
      call pc_put ('len_tot'  , obj%len_tot     )
      call pc_put ('hdr_wid'  , obj%hdr_wid     )
      call pc_put ('wid_init' , obj%wid_init , 7)
      call pc_put ('wid_inc'  , obj%wid_inc  , 7)

      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)

      call pc_put_gui_only ('survey_units', obj%survey_units)
 

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%h      )) deallocate (obj%h      )
      if (associated(obj%ifirst )) deallocate (obj%ifirst )
      if (associated(obj%isource)) deallocate (obj%isource)
      if (associated(obj%live   )) deallocate (obj%live   )
      if (associated(obj%nfold  )) deallocate (obj%nfold  )
      if (associated(obj%sp     )) deallocate (obj%sp     )
      if (associated(obj%ichan1 )) deallocate (obj%ichan1 )
      if (associated(obj%ichan2 )) deallocate (obj%ichan2 )
      if (associated(obj%frflag )) deallocate (obj%frflag )

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.    ! to run wrapup code after processing.

!----------SET UP STORAGE.

      allocate (obj%h      (need/8))
!     allocate (obj%h      (8*need))
!     allocate (obj%h      (need  ))
      allocate (obj%ifirst (obj%ng))
      allocate (obj%isource(obj%ng))
      allocate (obj%ichan1 (obj%ng))
      allocate (obj%ichan2 (obj%ng))
      allocate (obj%frflag (obj%ng))
      allocate (obj%live   (obj%len_tot))
      allocate (obj%nfold  (obj%len_tot))
      allocate (obj%sp     (obj%len_tot))

!----------INITIALIZE VARIABLES.

      obj%h      (:) = ' '
      obj%ifirst (:) = 9999999
      obj%isource(:) = 0
      obj%ichan1 (:) = 0
      obj%ichan2 (:) = 0
      obj%frflag (:) = "S"
      obj%live   (:) = 0
      obj%nfold  (:) = 0
      obj%sp     (:) = 0.0
      obj%xwidth     =  grid_get_xgrid_width(grid)
      obj%offmin     =  9999999.0
      obj%offmax     = -9999999.0
      obj%maxchan    = 0
      obj%kount      = 0
      obj%iuse       = 0
      obj%idead      = 0
      obj%splo       =  9999999.0
      obj%spup       = -9999999.0
      obj%iblo       =  9999999
      obj%ibup       = -9999999
      obj%nhs        = 0
      if (obj%hdr_len == 35) obj%nhs = 33
      if (obj%hdr_len == 47) obj%nhs = 46
      obj%gmin       =  1.0e30
      obj%gmax       = -1.0e30
      obj%xmin       =  1.0e30
      obj%xmax       = -1.0e30
      obj%ymin       =  1.0e30
      obj%ymax       = -1.0e30
      if (obj%hdr_wid == 0) obj%ymin = 0.0
      if (obj%hdr_wid == 0) obj%ymax = 0.0

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine chart_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine chart (obj,ntr,hd,tr)
      implicit none
      type(chart_struct),intent(inout) :: obj                ! arguments
      integer           ,intent(in)    :: ntr                ! arguments
      double precision  ,intent(in)    :: hd(:,:)            ! arguments
      real              ,intent(in)    :: tr(:,:)            ! arguments
      integer                          :: i                  ! local
!     character(len=8),pointer         :: htemp(:)    ! absoft work-around
!     character(len=1),pointer         :: htemp(:)    ! absoft work-around
      character(len=80),pointer        :: htemp(:)    ! absoft work-around

      if (ntr > 0) then
           do i=1,ntr
!!!!!!       call chart_add (obj, hd(1:,i), obj%h)   ! absoft does not like.
             htemp => obj%h                          ! absoft work-around.
             call chart_add (obj, hd(1:,i), htemp)   ! absoft work-around.
           end do
      else
           call chart_wrapup (obj)
      end if
      return
      end subroutine chart

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine chart_wrapup (obj)
      implicit none
      type(chart_struct),intent(inout) :: obj         ! arguments

!     character(len=8),pointer         :: htemp(:)    ! absoft work-around
!     character(len=1),pointer         :: htemp(:)    ! absoft work-around
      character(len=80),pointer        :: htemp(:)    ! absoft work-around

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.
      if (pc_get_update_state() /= PC_EXECUTE) return

!----------INITIALIZE PLOT.

      if (obj%device /= "PRINTER") then
! !!!        INITIALIZE PLOT
  !!!   if (obj%device == "CONP") then
  !!!     iunit=icpname
  !!!     ilf=iunit
  !!!     icpname=icpname+256
  !!!     card(1:16)=jobcrd(28:43)
  !!!     itemp1=0
  !!!     itemp2=0
  !!!     itemp3=0
  !!!     itemp4=0
  !!!     call mvc(acct,10,itemp1,1,8)
  !!!     call mvc(acct,18,itemp2,1,4)
  !!!     call mvc(card,1,itemp3,1,8)
  !!!     call mvc(card,9,itemp4,1,8)
  !!!     nloc='POPLOT'l
  !!!     ncpy=1
  !!!     call mvc(jobcrd,12,jntmp,1,8)
  !!!     istat=getenv("QSUB_REQID"l,nqs,5)
  !!!     kntchrt=kntchrt+1
  !!!     ctemp(1:1)=char(kntchrt)
  !!!     kk=index(nqs,'.')
  !!!     if (kk > 0 .and. kk <= 5) then
  !!!       ctmp=nqs
  !!!       nc=kk-1
  !!!       nqs=' '
  !!!       i1=6-nc
  !!!       call mvc(ctmp,1,nqs,i1,nc)
  !!!       do jj=1,i1-1
  !!!         nqs(jj:jj)='0'
  !!!       enddo
  !!!     endif
  !!!     call mvc(ctemp,1,nqs,6,1)
  !!!     nqs(7:8)=' Z'
  !!!
! !!!                  NODE  CHARGE CODE     ROUTE INFO
  !!!     call conpsetx(nloc,itemp1,itemp2,itemp3,itemp4,ncpy,jntmp,nqs)
  !!!     call conpfnt(19)
  !!!   else
  !!!     iunit=iopname
  !!!     ilf=iunit
  !!!     iopname=iopname+256
! !!!            INITIALIZE
  !!!     call xsetup(0,0.0,0.0,200.0,200.0,0.0,0.0,1199.0,39.5,-10,1)
  !!!   endif
  !!!   xo=0.0
  !!!   yo=0.0
  !!!   obj%scal=200.0
  !!!   obj%x=1.0*obj%scal
  !!!   obj%y=1.0*obj%scal
  !!!   call xsymbol (obj%x,obj%y,jobcrd,80,0.2,15,2,90.0)
  !!!   obj%x=obj%x+.5*obj%scal
  !!!   call xsymbol (obj%x,obj%y,prjcrd,80,0.2,15,2,90.0)
  !!!   if (obj%device == "CONP")call conpfnt(0)
      endif

!----------PRINT OR PLOT THE CHART.

!!!!! call chart_print (obj,obj%h)   ! absoft does not like.
      htemp => obj%h                 ! absoft work-around.
      call chart_print (obj,htemp)   ! absoft work-around.

!----------FINISH UP PRINTING.

      if (obj%device == "PRINTER") then
        call pc_print ('CHART:  MINIMUM AND MAXIMUM OFFSETS = ',obj%offmin, &
                          ' and ',obj%offmax)
        call pc_print ('CHART:  MAXIMUM NUMBER OF CHANNELS (TRACES PER&
                          & SHOT PROFILE) = ',obj%maxchan)
        call pc_print ('CHART:  NUMBER OF TRACES INPUT TO CHART = ',obj%kount)
        call pc_print ('CHART:  NUMBER OF LIVE TRACES IN CHART = ', &
                          obj%iuse-obj%idead)
        call pc_print ('CHART:  NUMBER OF DEAD TRACES IN CHART = ',obj%idead)
        call pc_print ('CHART:  TOTAL NUMBER OF TRACES IN CHART = ',obj%iuse)
        call pc_print ('CHART:  NUMBER OF TRACES SKIPPED (NOT PLOTTED)= ', &
                          obj%kount-obj%iuse)
        if (obj%kount > 0) then
          call pc_print ('CHART:  for header word = 9 :')
          call pc_print &
               ('CHART:    minimum and maximum requested shot profiles =', &
                             obj%prof_init,'',obj%prof_last)
          call pc_print &
               ('CHART:    minimum and maximum encountered shot profiles =', &
                             obj%gmin,'',obj%gmax)
          call pc_print ('CHART:  for HDR_LEN =',obj%hdr_len,':')
          call pc_print &
               ('CHART:    minimum and maximum requested HDR_LEN values =', &
                             obj%len_init,'',obj%len_last)
          call pc_print &
               ('CHART:    minimum and maximum encountered HDR_LEN values =', &
                             obj%xmin,'',obj%xmax)
          if (obj%hdr_wid > 0) then
            call pc_print ('CHART:  for HDR_WID =',obj%hdr_wid,':')
            call pc_print &
               ('CHART:    requested HDR_WID value =', obj%wid_init)
            call pc_print &
               ('CHART:    minimum and maximum encountered HDR_WID values =', &
                             obj%ymin,'',obj%ymax)
          end if
        end if

        call pc_print ('CHART:  WE ARE FINISHED')
        call pc_print (' ')

!----------FINISH UP PLOTTING.

      else
  !!!   write(cbuf,9001)obj%offmin,obj%offmax
  !!! 9001   format(' CHART: MINIMUM AND MAXIMUM OFFSETS = ',2f10.2)
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
  !!!   write(cbuf,9002)obj%maxchan
  !!! 9002   format(' CHART:  MAXIMUM NUMBER OF CHANNELS (TRACES PER SHOT PRO&
  !!!&file) = ',I5)
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
  !!!   write(cbuf,9003)obj%iuse-obj%idead
  !!! 9003   format('CHART:  NUMBER OF LIVE TRACES IN CHART = ',i10)
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
  !!!   write(cbuf,9004)obj%idead
  !!! 9004    format('CHART:  NUMBER OF DEAD TRACES IN CHART = ',i10)
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
  !!!   write(cbuf,9005)obj%iuse
  !!! 9005    format('CHART:  TOTAL NUMBER OF TRACES IN CHART = 'i10)
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
  !!!   write(cbuf,9006)obj%kount-obj%iuse
  !!! 9006   format('CHART:  NUMBER OF TRACES SKIPPED (NOT PLOTTED)=',i10)
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
  !!!   write(cbuf,'(A)') 'CHART:  WE ARE FINISHED'
! !!! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
  !!!   cbuf(270:640)=' '
  !!!   obj%x=obj%x+.1*obj%scal
  !!!   call xsymbol (obj%x,obj%y,cbuf,obj%iwidth,csize,15,2,90.0)
! !!!      THIS IS THE LAST THING PLOTTED - CALL XSETUP TO SET PLOT LENGTH
  !!!   flen=obj%x/obj%scal+5.0
  !!!   call xsetup(0,0.0,0.0,200.0,200.0,0.0,0.0,flen,39.5,-10,1)
  !!!   call xfinish(2)
      endif
      return
      end subroutine chart_wrapup

!</execute_only>


!!------------------------------- chart add ------------------------------!!
!!------------------------------- chart add ------------------------------!!
!!------------------------------- chart add ------------------------------!!


!  ADD ONE TRACE TO CHART.
!  FOR STACKING CHART:        obj%LIVE (*) = LIVE FOLD
!  FOR STACKING CHART:        obj%NFOLD(*) = TOTAL FOLD
!  FOR GROUND POSITION CHART: obj%LIVE (*) = LINE NUMBER
!  FOR GROUND POSITION CHART: obj%NFOLD(*) = PSEUDO FOLD


      subroutine chart_add (obj, hd, obj_h)
      implicit none
      type(chart_struct)       ,intent(inout) :: obj           ! arguments
      double precision         ,intent(in)    :: hd(:)         ! arguments
!!!   character(len=obj%iwidth),intent(inout) :: obj_h(*)      ! arguments
      character(len=*)         ,intent(inout) :: obj_h(:)      ! arguments
      integer                                 :: iy,ix,is,ig   ! local
!     integer                                 :: icol,isx      ! local
      integer                                 :: icol          ! local
      character(len=1)                        :: single        ! local

!----------UPDATE LIMITS.

      obj%gmin = min(obj%gmin,real(hd(9)))
      obj%gmax = max(obj%gmax,real(hd(9)))
      obj%xmin = min(obj%xmin,real(hd(obj%hdr_len)))
      obj%xmax = max(obj%xmax,real(hd(obj%hdr_len)))
      if (obj%hdr_wid > 0) then
        obj%ymin = min(obj%ymin,real(hd(obj%hdr_wid)))
        obj%ymax = max(obj%ymax,real(hd(obj%hdr_wid)))
      end if

!----------PROCESS A TRACE.

      obj%kount = obj%kount + 1
      if (obj%hdr_wid > 0) then
        iy = 1 + nint((hd(obj%hdr_wid) - obj%wid_init)/obj%wid_inc)
        if (iy /= 1) return
      endif
      ix = 1 + nint((hd(obj%hdr_len) - obj%len_init)/obj%len_inc)
      if (ix < 1 .or. ix > obj%len_tot) return
      is = ix
      if (obj%nhs > 0) then
        is = 1 + nint((hd(obj%nhs) - obj%len_init)/obj%len_inc)
        if (is < 1 .or. is > obj%len_tot) return
      endif
      ig = 1 + nint(hd(9) - obj%prof_init)
      if (ig < 1 .or. ig > obj%ng) return
      obj%iuse = obj%iuse + 1
      if (hd(25) == 0.) obj%idead = obj%idead + 1

!----------ADD RECEIVER (OR CMP) LOCATION TO CHART.
!          NO SOURCES HAVE BEEN ADDED YET.
!          EVERY FIFTH COLUMN WILL BE ALTERED LATER.

      icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
      single = obj_h(ix+6)(icol:icol)
      if (obj%device /= "OPS") then
        if (single /= 'x') then
          if (hd(25) == 0.) then
            single = 'x'                           !  x overrides +
          else if (hd(25) < 0.) then
            single = '+'                           !  + overrides . or m
          else if (single == ' ') then
            single = '.'
          else if (single /= '+') then
            single = 'm'
          endif
          obj_h(ix+6)(icol:icol) = single
        endif
      else
        if (single /= 'X') then
          if (hd(25) == 0.) then
            single = 'X'                           !  X overrides +
          else if (hd(25) < 0.) then
            single = '+'                           !  + overrides . or M
          else if (single == ' ') then
            single = '.'
          else if (single /= '+') then
            single = 'M'
          endif
          obj_h(ix+6)(icol:icol) = single
        endif
      endif

!----------UPDATE MIN AND MAX VALUES.

      obj%ifirst (ig) = min(ix,is,obj%ifirst(ig))
      obj%isource(ig) = is
      obj%maxchan = max(nint(hd(10)),obj%maxchan)
      obj%offmin  = min(real(hd( 6)),obj%offmin)
      obj%offmax  = max(real(hd( 6)),obj%offmax)
      if (obj%nhs == 0) then
        obj%sp   (ix) = obj%sp   (ix) + hd(37)
        obj%nfold(ix) = obj%nfold(ix) + 1
        if (hd(25) /= 0.0) obj%live(ix) = obj%live(ix) + 1
      else
        obj%sp   (ix) = obj%sp   (ix) + hd(28)
        obj%sp   (is) = obj%sp   (is) + hd(29)
        obj%live (ix) = obj%live (ix) + hd(27)
        obj%live (is) = obj%live (is) + hd(26)
        obj%nfold(ix) = obj%nfold(ix) + 1
        obj%nfold(is) = obj%nfold(is) + 1
      endif

!----------UPDATE FIRST AND LAST TRACE FLAGS, AND FORWARD/REVERSE FLAG.

      if (obj%ichan1(ig) == 0) obj%ichan1(ig) = ix
      obj%ichan2(ig) = ix
      if (ix > is) then
        if (obj%frflag(ig) == 'S') obj%frflag(ig) = 'F'
        if (obj%frflag(ig) == 'R') obj%frflag(ig) = 'B'
      else if (ix < is) then
        if (obj%frflag(ig) == 'S') obj%frflag(ig) = 'R'
        if (obj%frflag(ig) == 'F') obj%frflag(ig) = 'B'
      endif
      return 
      end subroutine chart_add


!!------------------------------ chart print --------------------------------!!
!!------------------------------ chart print --------------------------------!!
!!------------------------------ chart print --------------------------------!!

!  PRINT THE STACKING (OR GROUND POSITION) CHART.


      subroutine chart_print (obj, obj_h)
      implicit none
      type(chart_struct)       ,intent(inout) :: obj           ! arguments
      character(len=obj%iwidth),intent(inout) :: obj_h(*)      ! arguments
      integer :: ig, is, icol, ix, ixlo, ixup                  ! local
      integer :: j, k, itotal, maxfold, maxlive                ! local
      integer :: ilive, livelo, liveup, itrace, lun            ! local
      real    :: ratio, ggpp, xx1, bsmt, xlen                  ! local
      character(len=6)   :: buf                                ! local
      character(len=1)   :: single                             ! local
      character(len=4)   :: msg                                ! local
      character(len=10)  :: spbuf                              ! local
      character(len=640) :: cbuf                               ! local

!----------PRINT HEADER.

      lun = pc_get_lun()
      if (obj%device == "PRINTER") then
        write (lun, 1001)
        if (obj%nhs == 0) then
          write (lun, 1002)
        else
          write (lun, 1003)
        endif
        write (lun, 1004)
 1001   format('1',77('+'))
 1002   format(' ',26('+'),'     STACKING CHART      ',26('+'))
 1003   format(' ',26('+'),'  GROUND POSITION CHART  ',26('+'))
 1004   format(' ',77('+'))
        if (obj%nhs == 0) then
          write (lun, 1005) obj%hdr_len, obj%len_init, obj%len_inc, &
                            obj%hdr_wid, obj%wid_init, obj%wid_inc
        else
          write (lun, 1006) obj%hdr_len, obj%len_init, obj%len_inc, &
                            obj%nhs, obj%hdr_wid, obj%wid_init, obj%wid_inc
        endif
 1005   format('0    X CMP HEADER ',i3,'  FIRST X BIN CENTER',f10.2,&
          '   X BIN SIZE AND INCREMENT ',f10.2,/,'0    Y CMP HEADER ',i3,&
          '        Y BIN CENTER',f10.2,'   Y BIN SIZE               ',f10.2,/)
 1006   format('0    X RECEIVER HEADER ',i3,'  FIRST X BIN CENTER',f10.2,&
          '   X BIN SIZE AND INCREMENT ',f10.2,/,'     X SOURCE   HEADER ',i3,/&
          ,'0    Y RECEIVER HEADER ',i3,'        Y BIN CENTER',f10.2,&
          '   Y BIN SIZE               ',f10.2,/)

!----------PLOT HEADER.

      else
        if (obj%nhs == 0) then
          write (cbuf, 1008)
 1008     format('   STACKING CHART ')
        else
          write (cbuf, 1009)
 1009     format('   GROUND POSITION CHART ')
        endif
        obj%x = obj%x + 0.2*obj%scal
 !!!    call xsymbol (obj%x, obj%y, cbuf, 100, csize, 15, 2, 90.0)
        if (obj%nhs == 0) then
          write (cbuf, 9005) obj%hdr_len, obj%len_init, obj%len_inc
          obj%x = obj%x + 0.2*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, 100, csize, 15, 2, 90.0)
          write (cbuf, 90051) obj%hdr_wid, obj%wid_init, obj%wid_inc
        endif
        if (obj%nhs /= 0) then
          write (cbuf, 9006) obj%hdr_len, obj%len_init, obj%len_inc
          obj%x = obj%x + 0.2*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, 100, csize, 15, 2, 90.0)
          write (cbuf, 90061) obj%nhs
          obj%x = obj%x + 0.2*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, 100, csize, 15, 2, 90.0)
          write (cbuf, 90062) obj%hdr_wid, obj%wid_init, obj%wid_inc
        endif

 9005   format('     X CMP HEADER ',i3,'  FIRST X BIN CENTER',f10.2,&
          '   X BIN SIZE AND INCREMENT ',f10.2)
90051   format('     Y CMP HEADER ',i3,'        Y BIN CENTER',f10.2,&
          '   Y BIN SIZE               ',f10.2)
 9006   format('     X RECEIVER HEADER ',i3,'  FIRST X BIN CENTER',f10.2,&
          '   X BIN SIZE AND INCREMENT ',f10.2)
90061   format('     X SOURCE   HEADER ',i3)
90062   format('     Y RECEIVER HEADER ',i3,'        Y BIN CENTER',f10.2,&
          'Y BIN SIZE               ',f10.2)

        obj%x = obj%x + 0.2*obj%scal
 !!!    call xsymbol (obj%x, obj%y, cbuf, 100, csize, 15, 2, 90.0)
      endif

!----------ADD BEGINNING AND END TRACE FLAGS TO CHART.

      if (obj%device /= "OPS") then
        do ig = 1, obj%ng
          is = obj%isource(ig)
          if (is == 0) cycle
          icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
          ix = obj%ichan2(ig)
          if (ix > 0) then
            single = obj_h(ix+6)(icol:icol)
            if (single/='x' .and. single/='+') single = 'e'
            obj_h(ix+6)(icol:icol) = single
          endif
          ix = obj%ichan1(ig)
          if (ix <= 0) cycle
          single = obj_h(ix+6)(icol:icol)
          if (single/='x' .and. single/='+') single = 'a'
          obj_h(ix+6)(icol:icol) = single
        end do
      else
        do ig = 1, obj%ng
          is = obj%isource(ig)
          if (is == 0) cycle
          icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
          ix = obj%ichan2(ig)
          if (ix > 0) then
            single = obj_h(ix+6)(icol:icol)
            if (single/='X' .and. single/='+') single = 'E'
            obj_h(ix+6)(icol:icol) = single
          endif
          ix = obj%ichan1(ig)
          if (ix <= 0) cycle
          single = obj_h(ix+6)(icol:icol)
          if (single/='X' .and. single/='+') single = 'A'
          obj_h(ix+6)(icol:icol) = single
        end do
      endif

!----------ADD THE SOURCES TO THE CHART.

      if (obj%nhs > 0) then
        if (obj%ispace == 1) then
          if (obj%device /= "OPS") then
            do ig = 1, obj%ng
              is = obj%isource(ig)
              if (is == 0) cycle
              icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
              single = obj_h(is+6)(icol:icol)
              select case (single)
                case (' ')  ; single = obj%frflag(ig)
                case ('.')  ; single = 'L'
                case ('x')  ; single = 'D'
                case ('+')  ; single = 'P'
                case ('m')  ; single = 'T'
                case ('a')  ; single = 'U'
                case ('e')  ; single = 'V'
              end select
              obj_h(is+6)(icol:icol) = single
            end do
          else
            do ig = 1, obj%ng
              is = obj%isource(ig)
              if (is == 0) cycle
              icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
              single = obj_h(is+6)(icol:icol)
              select case (single)
                case (' ')  ; single = obj%frflag(ig)
                case ('.')  ; single = 'L'
                case ('X')  ; single = 'D'
                case ('+')  ; single = 'P'
                case ('M')  ; single = 'T'
                case ('A')  ; single = 'U'
                case ('E')  ; single = 'V'
              end select
              obj_h(is+6)(icol:icol) = single
            end do
          endif
        else
          do ig = 1, obj%ng
            is = obj%isource(ig)
            if (is == 0) cycle
            icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
            single = obj_h(is+6)(icol:icol)
            if (single /= ' ') icol = icol - 1
            obj_h(is+6)(icol:icol) = obj%frflag(ig)
          end do
        endif
      endif

!----------FIND FIRST AND LAST ROWS OF CHART.

      do ix = 1, obj%len_tot
        if (obj_h(ix+6) /= ' ') go to 36
      end do
      call pc_print ('CHART:  ENTIRE CHART IS BLANK')
      return 
   36 continue
      ixlo = ix
      do ix = obj%len_tot, 1, -1
        if (obj_h(ix+6) /= ' ') go to 38
      end do
      call pc_print ('CHART:  ENTIRE CHART IS BLANK')
      return 
   38 continue
      ixup = ix

!----------ALTER THE CHARACTERS FOR EVERY FIFTH COLUMN.

      do icol = 5*obj%ispace, obj%iwidth, 5*obj%ispace
        do ix = ixlo, ixup
          single = obj_h(ix+6)(icol:icol)
          select case (single)
            case ('.')  ; single = ':'
            case ('x')  ; single = 'X'
            case ('m')  ; single = 'M'
          end select
          obj_h(ix+6)(icol:icol) = single
        end do
      end do

!----------ADD GROUP NUMBERS TO CHART.

      do ig = 1, obj%ng
        if (obj%ifirst(ig) > ixup) cycle
        icol = obj%ispace*mod(ig - 1,obj%iwidth2) + obj%ispace
        write (buf, '(I6)') ig
        do j = 1, 6
          if (buf(j:j) == ' ') cycle
          obj_h(j+obj%ifirst(ig)-1)(icol:icol) = buf(j:j)
        end do
      end do

!----------PRINT BEGINNING OF GROUND POSITION CHART.

      if (obj%nhs /= 0) then
        if (obj%device == "PRINTER") then
          write (lun, 3001)
        else
          write (cbuf, 9007)
! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
          cbuf(270:640) = ' '
          obj%x = obj%x + 0.1*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, obj%iwidth, csize, 15, 2, 90.0)
          write (cbuf, 9008)
! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
          cbuf(270:640) = ' '
          obj%x = obj%x + 0.1*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, obj%iwidth, csize, 15, 2, 90.0)
        endif
 3001   format('     GROUND      SHOT ',80x,'  LINE'/,'   POSITION     POINT ',&
          80x,'NUMBER')
 9007   format('     GROUND      SHOT ')
 9008   format('   POSITION     POINT ')
        if (obj%device == "PRINTER") then
          do ix = ixlo - 6, ixlo - 1
            if (obj_h(ix+6)==' ' .and. ix/=ixlo-1) cycle
            write (lun, 4000) obj_h(ix+6)
          end do
        else
          do ix = ixlo - 6, ixlo - 1
            if (obj_h(ix+6)==' ' .and. ix/=ixlo-1) cycle
            obj%x = obj%x + 0.1*obj%scal
 !!!        call xsymbol (obj%x,obj%y+22*0.1*obj%scal,obj_h(ix+6), &
 !!!                                 obj%iwidth,csize,15,2,90.0)
          end do
        endif
 4000   format(22x,a80)

!----------PRINT REST OF GROUND POSITION CHART.

        if (obj%ibup > obj%iblo) &
                  ratio = (obj%spup - obj%splo)/(obj%ibup - obj%iblo)
        do ix = ixlo, ixup
          buf = ' '
          spbuf = ' '
          if (obj%nfold(ix) > 0) then
            write (spbuf, '(F10.2)') obj%sp(ix)/obj%nfold(ix)
            write (buf, '(I6)') obj%live(ix)/obj%nfold(ix)
          else if (obj%ibup > obj%iblo) then
            write (spbuf, '(F10.2)') obj%splo + (2*ix - obj%iblo)*ratio
          endif
          ggpp = obj%len_init + (ix - 1)*obj%len_inc
          if (obj%device == "PRINTER") then
            write (lun, 5001) ggpp, spbuf, obj_h(ix+6), buf
          else
            obj%x = obj%x + 0.1*obj%scal
            write (cbuf, '(1X,F10.2,A10)') ggpp, spbuf
 !!!        call xsymbol(obj%x,obj%y            ,cbuf   ,    21,csize,15,2,90.0)
 !!!        call xsymbol(obj%x,obj%y+22*0.1*obj%scal &
 !!!                                    ,obj_h(ix+6),obj%iwidth,csize,15,2,90.0)
            k = amod(ggpp,20.0)
            if (k == 0) then
              xx1 = obj%x + 0.7*csize*obj%scal
 !!!          call xline (xx1, obj%y, xx1, 7400.0, 0.005, 15, 201)
            endif
!CC     WRITE(CBUF,'(A6)')BUF
!CC     CALL XSYMBOL (obj%X,(obj%Y+(22+IWIDTH)*CSIZE*SCAL),CBUF,6,CSIZE,15,2,
!CC  X                90.0)
          endif
 5001     format(1x,f10.2,a10,1x,a80,a6)
        end do

!----------FINAL PRINTS FOR GROUND POSITION CHART.

        if (obj%device == "PRINTER") then
          write (lun, 3001)
          write (lun, 8007)
 8007     format(40x,'Dead receiver                           x or X'/,40x,&
            'Reverse polarity receiver               +     '/,40x,&
            'Beginning (first) receiver in group     a     '/,40x,&
            'End (last) receiver in group            e     '/,40x,&
            'Multiple live receivers                 m or M'/,40x,&
            'One live receiver                       . or :'/)
          write (lun, 8008)
 8008     format(40x,'Source with receivers forward           F'/,40x,&
            'Source with receivers reverse           R'/,40x,&
            'Source with receivers on both sides     B'/)
          if (obj%ispace == 1) write (lun, 8009)
 8009     format(40x,'Source and dead receiver                D',&
            '   (plotted over x or X)'/,40x,&
            'Source and reverse polarity receiver    P','   (plotted over +)'/,&
            40x,'Source and beginning receiver           U',&
            '   (plotted over a)'/,40x,&
            'Source and end receiver                 V','   (plotted over e)'/,&
            40x,'Source and multiple live receivers      T',&
            '   (plotted over m or M)'/,40x,&
            'Source and one live receiver            L',&
            '   (plotted over . or :)'/)
        endif
        return
      endif

!----------PRINT BEGINNING OF STACKING CHART.

      msg = ' '
      if (obj%hdr_len == 17 .or. obj%hdr_len == 18) msg = 'DIST'
      if (obj%hdr_len ==  7 .or. obj%hdr_len ==  8) msg = 'GRID'
      if (obj%device == "PRINTER") then
        write (lun, 3003) msg
 3003   format('  LIVE      SHOT  STACKED',80x,2x,' CMP  FOLD  TOTAL  LIVE'/,&
          '   TR#     POINT  TR#',80x,6x,a4,'      #TRACES  FOLD')
        do ix = ixlo - 6, ixlo - 1
          if (obj_h(ix+6)==' ' .and. ix/=ixlo-1) cycle
          write (lun, 4000) obj_h(ix+6)
        end do
      else
        obj%x = obj%x + 0.2*obj%scal
        write (cbuf, '(A)') '  LIVE      SHOT  STACKED'
 !!!    call xsymbol (obj%x, obj%y, cbuf, 25, csize, 15, 2, 90.0)
        write (cbuf, '(A)') ' CMP  FOLD  TOTAL  LIVE'
 !!!    call xsymbol (obj%x, 7260.0, cbuf, 23, csize, 15, 2, 90.0)
        obj%x = obj%x + 0.1*obj%scal
        write (cbuf, '(A)') '   TR#     POINT  TR#'
 !!!    call xsymbol (obj%x, obj%y, cbuf, 21, csize, 15, 2, 90.0)
        write (cbuf, '(6X,A4)') msg              ! GRID OR DIST
 !!!    call xsymbol (obj%x, 7200.0, cbuf, 10, csize, 15, 2, 90.0)
        write (cbuf, '(A)') '      #TRACES  FOLD'
 !!!    call xsymbol (obj%x, 7300.0, cbuf, 19, csize, 15, 2, 90.0)
      endif

!----------PRINT REST OF STACKING CHART.

      itotal = 0
      maxfold = 0
      maxlive = 0
      ilive = 0
      livelo = 0
      liveup = 0
      do ix = ixlo, ixup
        itrace = ix - ixlo + 1
        itotal = itotal + obj%nfold(ix)
        maxfold = max(obj%nfold(ix),maxfold)
        bsmt = obj%len_init + (ix - 1)*obj%len_inc
        buf = ' '
        spbuf = ' '
        if (obj%nfold(ix) > 0) write (spbuf,'(F10.2)') obj%sp(ix)/obj%nfold(ix)
        if (obj%live(ix) > 0) then
          if (livelo == 0) livelo = ix
          liveup = ix
          maxlive = max(obj%live(ix),maxlive)
          ilive = ilive + 1
          write (buf, '(I5,1X)') ilive
        endif

        if (obj%device == "PRINTER") then
          write (lun, 5002) &
              buf, spbuf, itrace, obj_h(ix+6), bsmt, obj%nfold(ix), itotal, &
              obj%live(ix)
 5002     format(1x,a5,a10,i5,1x,a80,f10.2,i5,i7,i6)
        else
          obj%x = obj%x + 0.1*obj%scal
          write (cbuf, '(1X,A5,A10,I5)') buf, spbuf, itrace
 !!!      call xsymbol (obj%x,obj%y            ,cbuf   ,    21,csize,15,2,90.0)
 !!!      call xsymbol (obj%x,obj%y+22*0.1*obj%scal,obj_h(ix+6),obj%iwidth,&
 !!!                        csize,15,2,90.0)
          k = mod(itrace,20)
          if (k == 0) then
            xx1 = obj%x + 0.7*csize*obj%scal
 !!!        call xline (xx1, obj%y, xx1, 7600.0, 0.005, 15, 201)
          endif
          write (cbuf, '(F10.2,I5,I7,I6)') &
                            bsmt, obj%nfold(ix), itotal, obj%live(ix)
 !!!      call xsymbol (obj%x, 7200.0, cbuf, 28, csize, 15, 2, 90.0)
        endif
      end do

      xlen = (liveup - livelo)*obj%len_inc
      if (obj%hdr_len == 7) xlen = xlen*obj%xwidth

!----------FINAL PRINT.

      if (obj%device == "PRINTER") then
        write (lun, 3003) msg
        write (lun, 7007)
 7007   format(40x,'Dead trace                           x or X'/,40x,&
          'Reverse polarity trace               +     '/,40x,&
          'Beginning (first) trace in group     a     '/,40x,&
          'End (last) trace in group            e     '/,40x,&
          'Multiple live traces                 m or M'/,40x,&
          'One live trace                       . or :'/)
        write (lun, *) 'CHART:  MAXIMUM FOLD = ', maxfold, &
          '  (MAXIMUM LIVE FOLD = ', maxlive, ')'
        if (obj%survey_units == 'METERS') then
             write (lun, 7000) xlen/1000., 'KILOMETERS'
        else
             write (lun, 7000) xlen/5280., 'MILES     '
        end if
 7000   format(' CHART:  LENGTH OF LINE = ',f11.3,1x,a10)

!----------FINAL PLOT.

      else
        obj%x = obj%x + 0.1*obj%scal
        write (cbuf, '(A)') '  LIVE      SHOT  STACKED'
 !!!    call xsymbol (obj%x, obj%y, cbuf, 25, csize, 15, 2, 90.0)
        write (cbuf, '(A)') ' CMP  FOLD  TOTAL  LIVE'
 !!!    call xsymbol (obj%x, 7260.0, cbuf, 23, csize, 15, 2, 90.0)
        obj%x = obj%x + 0.2*obj%scal
        write (cbuf, '(A)') '   TR#     POINT  TR#'
 !!!    call xsymbol (obj%x, obj%y, cbuf, 21, csize, 15, 2, 90.0)
        write (cbuf, '(6X,A4)') msg              ! GRID OR DIST
 !!!    call xsymbol (obj%x, 7200.0, cbuf, 10, csize, 15, 2, 90.0)
        write (cbuf, '(A)') '      #TRACES  FOLD'
 !!!    call xsymbol (obj%x, 7300.0, cbuf, 19, csize, 15, 2, 90.0)
        write (cbuf, 9001) maxfold, maxlive
 9001   format('CHART:  MAXIMUM FOLD = ',i6,' MAXIMUM LIVE FOLD = ',i6)
        obj%x = obj%x + 0.1*obj%scal
 !!!    call xsymbol (obj%x, obj%y, cbuf, 80, csize, 15, 2, 90.0)
        if (obj%survey_units == 'METERS') then
          write (cbuf, 7000) xlen/1000., 'KILOMETERS'
! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
          cbuf(270:640) = ' '
          obj%x = obj%x + 0.1*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, obj%iwidth, csize, 15, 2, 90.0)
        else
          write (cbuf, 7000) xlen/5280., 'MILES '
! Clear end of CBUF that cft WRITE damages,  D. Peterson 19 Sept. 1991
          cbuf(270:640) = ' '
          obj%x = obj%x + 0.1*obj%scal
 !!!      call xsymbol (obj%x, obj%y, cbuf, obj%iwidth, csize, 15, 2, 90.0)
        endif
      endif
      return 
      end subroutine chart_print


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module chart_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

