!<CPS_v1 type="PROCESS"/>
!!------------------------------- DMAP3D.f90 ---------------------------------!!
!!------------------------------- DMAP3D.f90 ---------------------------------!!
!!------------------------------- DMAP3D.f90 ---------------------------------!!
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
!                          C P S   P R O C E S S
!
! Name       : DMAP3D    Data Mapping in 3D
! Category   : migrations
! Written    : 1999-09-08   by: Bob Stolt
! Revised    : 2007-12-13   by: Bill Menger
! Maturity   : beta
! Purpose    : Data Mapping of 3-D data.
! Portability: No known limitations.
! Parallel   : YES
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! DMAP3D is a generalized 3D data mapping algorithm that creates new data
! gathers from old.  Data from one or more input trace gathers are mapped to
! one or more output trace gathers where both input and output gathers are
! two-dimensional arrays of traces extensive in the inline and the crossline
! directions.
!
! This program operates with a shift-sum operation similar to a DMO or
! Kirchhoff prestack migration.  However, while most Conoco programs scatter
! data from each input trace to all output traces, this program gathers data
! from all input traces to each output trace.  Though theoretically equivalent,
! this way allows amplitude weighting of the input traces according to distance
! to nearest neighbors.
!
! Before an output trace can be made, all contributing input traces must be in
! memory.  The amount of memory allocated to the input traces is specified by
! the user.
!
! Data mapping algorithms tend to lose fidelity at small times and small offset
! changes.  This algorithm incorporates some measures to maintain robustness
! under most circumstances, and provides the user with some control over the
! process.  The design philosophy is "do no evil" which, loosely interpreted,
! means to preserve amplitude and waveform of flat events insofar as possible.
!
! Data mapping may produce slightly different images depending on the number
! of PE used in the job. The differences are mainly due to minor background  
! noise changes and do not change the signal continuity and usually limit  
! a few traces in the first three inlines
!  
! Input Modes
!
! DMAP3D can operate in three input modes: common source, common receiver and
! common offset, where the input mode name indicates the type of input trace
! gather expected.  Each mode maps information from the input trace gathers to
! output trace gathers that are created by the process.  Common source mode
! maps existing sources to new sources, common receiver mode maps existing
! receivers to new receivers, common offset mode maps existing offsets to new
! offsets.  (DMAP3D cannot map existing midpoints to new midpoints.)
!
!
! Output Modes
!
! Output from DMAP3D consists only of new traces created by the data mapping
! operation.  There are three output modes: common source, common receiver and
! common offset, where the output mode name indicates the type of output trace
! gather.  (The output mode need not be the same as the input mode.)
!
!
! Specifying Output Gathers
!
! Users define the output gathers with four coordinates.  Output trace
! locations are specified by the coordinates E1 and E2.  The locations of the
! element defining the gathers (source, receiver or offset depending on the
! output mode) are specified by the coordinates G1 and G2.  Details are shown
! in the table.
!
!
!    |   Output Mode   |  G Coordinates   |       E Coordinates        |
!    |-----------------------------------------------------------------|
!    |Common Source    |Source location   |Receiver - Source distance  |
!    |Common Receiver  |Receiver location |Source - Receiver distance  |
!    |Common Offset    |Half-offset       |Midpoint location           |
!    ------------------------------------------------------------------|
!
! For example, in common offset output mode, the G coordinates are the
! x-component and the y-component of half-offset and the E coordinates are the
! x-component and the y-component of individual trace midpoint location.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Of the three input modes (common source, common receiver and common offset)
! common offset mode is likely to be somewhat better behaved than the others
! because its operator has a less radical shape.
!
! Even though the algorithm attempts to preserve amplitudes, it is likely that
! output traces generated by the mapping operation will have amplitudes
! noticably different from nearby input traces.  Consequently it will usually
! be desirable to use DMAP3D to map the entire desired dataset rather than using
! some raw original traces and some mapped traces.
!
! All parameters pertaining to distance should have values with units of the
! surveyed coordinates (feet or meters).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                          TRACE INPUT REQUIREMENTS
!
! Process is an all-trace (loop-splitting) process.
!
!    1.  Input traces should be sorted to common source, common receiver or
!    common offset order.  This order should agree with the choice made in the
!    INTYPE parameter.
!
!    2.  Traces should be input one at a time, not gathered.
!
!    3.  Traces should be NMO corrected and normally should have decon applied
!    to insure that wavelets are compact.
!
!    4.  All traces in a logical gather should have the same (integer) value of
!    header word 3.  If multiple logical gathers are input, they should be
!    sorted in increasing order of header word 3.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                        TRACE OUTPUT CHARACTERISTICS
!
! This process outputs trace gathers generated by the data mapping operation.
! Generally, they will be different gathers than those input.
!
! One trace is output at a time.  Input traces are not passed through.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                      GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NUMTR    max number of traces input/output     changed
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 2       Top Mute                   Used to define mapping limits
! 3       Current Gather Number      Renumbered, used to identify gathers
! 4       Current Gather Element     Renumbered
! 6       Offset                     recomputed
! 11      Source Xloc                Used, recomputed
! 12      Source Yloc                Used, recomputed
! 14      Receiver Xloc              Used, recomputed
! 15      Receiver Yloc              Used, recomputed
! 17      Midpoint Xloc              recomputed
! 18      Midpoint Yloc              recomputed
! 25      LAV                        recomputed
! 64      Bottom Mute                Used to define mapping limits
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 16. 2007-12-13  Bill Menger  Changed format statement again on line 2914.
! 15. 2007-12-04  Bill Menger  changed format statement on print line 2857.
! 14. 2006-12-04  D. Glover    Added NULLIFY statements for Intel compiler
!                              Removed Unused Variables
! 13. 2005-10-10  Goodger      Change intent(out) to intent(inout) for
!                              absoft 9.0 compiler.
! 12. 2005-01-31  S.Chiu       Change document and modify subdivision of
!                              output grid.
! 11. 2002-10-25  S.Chiu       Change AFMAX default to 10000.
! 10. 2002-10-22  S.Chiu       Add time-varying scale option.
!  9. 2002-08-16  S.Chiu       Add RMS scale option.
!  8. 2002-08-12  S.Chiu       Fix parallel IO problem.
!  7. 2002-07-17  S.Chiu       Fix the subdivision of output grid.
!  6. 2002-07-16  S.Chiu       Modified front-end interface and debugged. 
!  5. 2001-10-26  Goodger      Rename labels beginning with if to get around
!                              intel compiler bug.
!  4. 2001-07-13  Stolt        General algorithm tuning and debugging
!  3. 2001-03-30  Stoeckley    Slight modifications to conform with template.
!  2. 2000-12-31  Madiba       Modifications to interface with front end
!  1. 1999-08-24  Bob Stolt    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                           PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                         SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      SPECIFIC CALLING CHARACTERISTICS
!
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
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
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                     ALTERNATE INTERNAL CALLING METHODS
!
!  Alternate create routine DMAP3D_cr2 to run under wcps
!
!------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!------------------------------------------------------------------------------
!                    ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!  This program implements equation (62) of Generalized Data Mapping (1999)
!  It is meant to be a robust algorithm able to handle small times, small
!  offset changes, etc.
!
!                             DMAP3D Program Flow
!
! DMAP3D maps data from one or more input gathers to one or more output gathers
! Both input and output gathers are user-defined, two-dimensional arrays of
! traces.  Currently, they are limited to constant-offset, constant-source, or
! constant-receiver arrays.  (It is not possible to map from a constant-
! midpoint array or CMP gather, because the algorithm cannot generate midpoints
! not represented in the input data.)  The program does not require input and
! output gathers to be of the same type.
!
! The user defines a four-dimensional hypercube of desired output data.  Two
! coordinates define trace location within an output gather, and two more
! define the gather location.  For example, in a constant-offset gather, the
! two midpoint coordinates define the trace location, and the two offset
! coordinates define the gather.
!
! The program examines all input traces, and retains those inside the mapping
! aperture for at least one of the output traces.  The retained traces are
! buffered in processor memory.  For a single output trace, mapping aperture
! tends to be fairly small, and shouldn't overtax memory.  However, as the
! number of output traces increase, the number of retained input traces
! increases also, putting an upper bound on the number of output traces that
! can be generated in one pass of the program.  My thought was that a large
! output trace region could be divided into subcubes and parceled out to
! multiple processors, though I have not written the code to do that.
!
! Once all traces are input, the program does some pre-processing.  A Voronoi
! diagram is generated for each input gather.  Each input trace is handed a
! list of its nearest neighbors, plus the coordinates of the vertices of its
! Voronoi cell (the polygonal region closer to the input trace than to any of
! its neighbors).  The Voronoi diagram serves two purposes.  The area of its
! Voronoi cell becomes the weight factor for each input trace in the mapping
! operation.  Second, the variation in mapping properties across each cell
! gives a measure of how much anti-aliasing an input trace requires.  The
! Voronoi diagram gives the program the information it needs to handle both
! random and systematic variations from a regular input geometry.
!
! I don't think the Voronoi diagram generation is particularly costly, though I
! confess I paid little attention to efficiency.  There are some algorithms and
! software in the literature, but I ended up losing patience trying to decipher
! them and ended up writing my own.  If this step proves to be a bottleneck,
! I'm sure it could be improved.
!
! Once the Voronoi diagram is generated and its parameters stored, each input
! trace can be treated independently of all the others.  It would be possible
! to do the Voronoi diagram generation in a separate program, store the
! results, then do the mapping operation in a second pass.  This would result
! in a data flow more like our current-generation Kirchhoff 3-D prestack
! migrations.  Since I've kept both steps in a single program, the program
! necessarily does a gather rather than a scatter operation, building one
! output trace at a time.  (There is at least one advantage to this mode:  The
! user can, relatively cheaply, test program operation by generating a single
! output trace.)
!
! Given the input traces and the Voronoi info, the program goes into mapping
! mode.  For each trace in the output hypercube, the program cycles over input
! traces and computes its contribution to the output trace. To guarantee proper
! normalization, the program also computes the response to a theoretical flat
! event and divides the output trace by this result.  Once all contributions to
! the output trace have been made, an inverse filter is applied.  Periodically,
! the program shifts to output mode and empties the output trace buffer.
!
! There are several anti-aliasing strategies available in the program.  These
! include NOAA, NEARAA, QNAA, and LINAA.  As one might expect, NOAA doesn't do
! any antialiasing.  It just performs a single-valued mapping using the mapping
! properties at the input trace location.  This might be adequate for well-
! sampled input data.  However, the flip side to a small mapping aperture is
! that mapping properties can vary extremely rapidly, changing significantly
! over a trace spacing interval.  NOAA pays no attention to these changes,
! consequently may in many cases give an unsatisfactory result.
!
! The other modes pay varying degrees of attention to changes in mapping
! properties between traces.  The idea behind all of them is that aliasing (and
! some other mapping problems) occur because the input data is not sampled
! densely enough.  To correct this, the input trace is either effectively
! replicated over the Voronoi cell (NEARAA and QNAA options) or graded linearly
! with adjacent input traces (LINAA option).  The result in any of these cases
! is a multi-valued mapping:  A single traveltime on the input trace maps to a
! range of traveltimes on the output trace.  (The strategy I've employed is
! simple but not cheap.  There are some references to a similar strategy in the
! SEP reports, but I don't know whether/where it might be in use.  I haven't
! made comparisons, but I expect it to be more expensive than using multiple
! filtered copies of the input traces.)
!
! In the QNAA option, the range of mapping properties is computed over the
! Voronoi cell, and a multi-valued mapping applied.  A potential problem with
! this method is that if mapping properties exhibit a large nonlinear variation
! across the cell, inaccuracies may result.  In extreme cases, the method could
! miss an event entirely.
!
! The NEARAA option employs a more elaborate scheme.  If mapping properties
! vary too rapidly across the cell, the cell is subdivided into smaller cells
! until variations are small and linear enough.  Each subdivision contributes
! its own mapping to the output trace.  This method should be pretty robust,
! though practicality limits the number of subdivisions.  Its expense depends
! on whether only a few, or perhaps many, Voronoi cells need subdivision.
!
! The LINAA option uses, instead of the Voronoi cell, the associated Delaunay
! triangles.  (The vertices of these triangles are the input trace and two
! adjacent neighbors.)  Mapping amplitude is weighted within the triangle from
! 1 at the input trace vertex to 0 at the adjacent trace vertices.  Mechanics
! are similar to the NEARAA option, though the area to process per input trace
! is approximately three times as large.  Since the cells are larger, mapping
! properties will vary more, and typically more subdivision may be required.
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
!<NS DMAP3D Process/NC=80>
!                             Data Mapping in 3D
!
!
!                   --- Output Geometry Specification ---
!  `--------------------------------------------------------------------------
!
!    OUT_OFFSET=`FFFFFFFFF   OUT_AZIMUTH=`FFFFFFFFFFF
!   
!    SCALE_RMS =`CCCCCCCC    WIN_INC~~~~=`FFFFFFFFFFF
!
!    E1INI~~=`FFFFFFFFF  E2INI~~=`FFFFFFFFF
!    DE1~~~~=`FFFFFFFFF  DE2~~~~=`FFFFFFFFF
!    E1_LAST=`FFFFFFFFF  E2_LAST=`FFFFFFFFF
!    NE1~~~~=`IIIIIIIII  NE2~~~~=`IIIIIIIII
!  `--------------------------------------------------------------------------
!
!                           --- Algorithm Controls ---
!  `--------------------------------------------------------------------------
!    VEL~~~=`FFFFFFFFF  MNTOD=`IIIIIIIII  NTIM =`IIIIIIIII  NNMAX=`IIIIIIIII  
!    TTRMAX=`FFFFFFFFF  LFMAX=`IIIIIIIII  AFMAX=`FFFFFFFFF  VCMAX=`FFFFFFFFF  
!    TPRANG=`FFFFFFFFF  ITMIN=`IIIIIIIII  NSUBT=`FFFFFFFFF  HMIN =`FFFFFFFFF            
!  `--------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OUT_OFFSET">
!<Tip> Output offset. </Tip>
! Default = 0.
! Allowed = real >= 0
!</Help>
!
!<Help KEYWORD="OUT_AZIMUTH">
!<Tip> Output AZIMUTH. </Tip>
! Default = 0.
! Allowed = real 
!</Help>
!
!<Help KEYWORD="SCALE_RMS">
!<Tip> Apply RMS gain factor to the output trace. </Tip>
! Default = NO
! Allowed = CONSTANT (A constant gain for the entire output trace)
! Allowed = VARYING  (Time-varying gains for the output trace)
!
! A gain factor is computed from the median of all input traces  
! (RMS amplitudes within a window or entire trace) that contribute 
! to a output trace
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<tip> Time increment for time-varying gain window, in seconds. </tip>
!  Default = WIN_LEN
!  Allowed = real>=DT
!  Time increment for time-varying gain window, in seconds.
!</Help>
!
!<Help KEYWORD="E1ini">
!<Tip> First output x-coordinate gather element location (Header 7). </Tip>
! Default = 1.
! Allowed = real
!
!First of two (x,y) coordinates.
!For common offset gathers, E1ini is the first midpoint to output
!</Help>
!
!
!<Help KEYWORD="E2ini">
!<Tip> First output y-coordinate gather element location (Header 8). </Tip>
! Default = 1.
! Allowed = real
!
!Second of two (x,y) coordinates.
!For common offset gathers, E2ini is the first midpoint to output
!</Help>
!
!
!<Help KEYWORD="NE1">
!<Tip> Number of elements (traces) per output gather in direction 1. </Tip>
! Default = 1
! Allowed = Pos Int
!
! On output, E1 = E1ini + DE1 * (ie - 1), ie = 1,...,NE1
!</Help>
!
!
!<Help KEYWORD="NE2">
!<Tip> Number of elements (traces) per output gather in direction 2. </Tip>
! Default = 1
! Allowed = Pos Int
!
! On output, E2 = E2ini + DE2 * (ie - 1), ie = 1,...,NE2
!</Help>
!
!
!<Help KEYWORD="DE1">
!<Tip> Distance between output gather elements (traces) in direction 1. </Tip>
! Default = 1.
! Allowed = real
!
! On output, E1 = E1ini + DE1 * (ie - 1), ie = 1,...,NE1
!</Help>
!
!
!<Help KEYWORD="DE2">
!<Tip> Distance between output gather elements (traces) in direction 2. </Tip>
! Default = 1.
! Allowed = real
!
! On output, E2 = E2ini + DE2 * (ie - 1), ie = 1,...,NE2
!</Help>
!
!
!<Help KEYWORD="E1_LAST">
!<Tip> Last value of elements (traces) per output gather in direction 1. </Tip>
! Default = 1
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="E2_LAST">
!<Tip> Last value of elements (traces) per output gather in direction 2. </Tip>
! Default = 1
! Allowed = real
!</Help>
!
!<Help KEYWORD="VEL">
!<Tip> Rms Velocity. </Tip>
! Default = 5000.
! Allowed = pos real
!
! Even though data mapping is "velocity independent", velocity is in fact
! needed to determine the physical limits of the gathering operation.
! VEL should really be an array, but for now its just a constant.
!
! VEL should have units of feet or meters per second, according the the
! surveyed coordinates units.
!</Help>
!
!
!<Help KEYWORD="MNTOD">
!<Tip> Maximum number of input traces on disk within a subcube. </Tip>
! Default = 20000
! Allowed = Pos Int 
!
! All the input traces which contribute to an output subcube must be 
! included. MNTOD should be set large enough to include all traces.
!</Help>
!
!<Help KEYWORD="NTIM">
!<Tip> Number of Traces In Memory. </Tip>
! Default = 10000
! Allowed = int > 0
! Generally NTIM should be set large enough so that disk swapping is minimized,
! but small enough to avoid restrictions on memory availability or cost.
!</Help>
!
!<Help KEYWORD="NNMAX">
!<Tip> Maximum number of neighbors per input trace. </Tip>
! Default = 12
! Allowed = Pos Int >= 9
!
! DMAP3D allocates an area to each input trace by finding it's natural neighbors
! and forming a polygon from the perpendicular bisectors of the line to each
! neighbor.  NNMAX is the maximum number of neighbors per input trace to store
! in memory. For a regular 2D grid, NNMAX = 8 would do.  For irregular
! geometries, a given trace could have many neighbors.  Limiting NNMAX too
! small could cause the area allocated to a trace to be underestimated, which
! in turn can affect output amplitudes.
!</Help>
!
!<Help KEYWORD="VCMAX">
!<Tip> Maximum Voronoi Cell size relative to average spacings. </Tip>
! Default = 5.
! Allowed >= 1.
!
! DMAP3D allocates an area to each input trace by finding it's natural neighbors
! and forming a polygon from the perpendicular bisectors of the line to each
! neighbor.  This polygon is called a "Voronoi cell".  In sparsely sampled
! regions or on the data boundary, the polygon may become large, or even
! unclosed.  VCMAX sets the maximum size of a Voronoi cell relative to average
! distances between neighbors.  Cells larger than this are truncated by adding
! a "virtual neighbor" to the input trace.
!</Help>
!
!
!<Help KEYWORD="LFMAX">
!<Tip> Maximum inverse filter size. </Tip>
! Default = 50
! Allowed = Pos Int
!
! Best not messed with.  Size of array to hold inverse filter.  The larger
! the inverse filter, the longer it will take the program to apply it.
!</Help>
!
!
!<Help KEYWORD="TTRMAX">
!<Tip> Maximum allowed traveltime ratio between input and output traces. </Tip>
! Default = 3.
! Allowed = >= 1.
!
! Puts an upper limit on the traveltime ratio, which is similar to a doppler
! mute in NMO.  Making this too small could affect the fidelity of the output
! especially at small times.  If too large, the program will do unnecessary
! computation and could run into some numerical difficulties which might
! affect quality.
!</Help>
!
!
!<Help KEYWORD="AFMAX">
!<Tip> Max allowed Amplitude ratio between input and reference traces.</Tip>
! Default = 10000.
! Allowed = >= 1.
!
! The amplitude ratio is one at normal incidence, and increases with angle of
! incidence.  It can get very large at the edge of the mapping aperture.  For
! infinitely dense sampling, that wouldn't matter, but it can lead to problems
! in practice.  The reference trace is taken from the center of the output   
! grid and the amplitue of the output reference trace is the median mapping
! amplitude of all traces contributing to this reference output trace.  
! This can be used to despike the amplitudes of the output data. 
! This parameter sets a limit on the ratio. 
!</Help>
!
!
!<Help KEYWORD="TPRANG">
!<Tip> Puts a taper at the edge of the physical aperture. </Tip>
! Default = 60.
! Allowed = pos real < 90.
!
! Angle of incidence at which to start tapering the input.  As with AFMAX,
! this helps keep things reasonable at the edge of the physical aperture.
!</Help>
!
!
!<Help KEYWORD="HMIN">
!<Tip> Minimum offset for offset mapping. </Tip>
! Default = max( X_GRID_DIST, Y_GRID_DIST)
! Allowed = real > 0
!
! Minimum offset is used to prevent numerical stability in the mapping.  
!</Help>
!
!
!<Help KEYWORD="ITMIN">
!<Tip> Minimum time sample to worry about. </Tip>
! Default = 10
! Allowed = pos int <= NDPT
!
! It is most difficult to maintain good numerical quality at small times.
! By making ITMIN larger, computation time is reduced.
!</Help>
!
!
!<Help KEYWORD="NSUBT">
!<Tip> Maximum number of subintervals to stack. </Tip>
! Default = 25
! Allowed = pos int >= 1
!
! Every input trace represents a range of input values based on distance to
! nearest neighbors. If the program decides to subdivide a trace interval
! (effectively duplicating an input trace) it starts building a stack of
! subintervals until it is satisfied with sampling accuracy in each.  NSUBT
! is the maximum number of subintervals allowed in the stack, and places
! a limit on the exuberance of the subdividing algorithm.  A smaller number
! might in some instances reduce cost, while too small a number will reduce
! accuracy.  Normally, the default should be fine.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module DMAP3D_module
      use pc_module
      use named_constants_module
      use getlun_module
      use pcps_module
      use pcpsx_module
      use cio_module
      use trcio_module
      use pp_module
      use grid_module
      use pattern_module
      use median_module
      use interp_module       
      use exptilde_module

      implicit none
      private

      public :: DMAP3D_create
      public :: DMAP3D_initialize
      public :: DMAP3D_update
      public :: DMAP3D_delete
!<execute_only>
      public :: DMAP3D
      public :: DMAP3D_wrapup
  !   public :: DMAP3D_initgraphics    ! for NT version only.
  !   public :: DMAP3D_plotvc          ! for NT version only.
  !   public :: DMAP3D_s               ! for NT version only.
!</execute_only>


      character(len=100),public,save :: DMAP3D_IDENT = &
'$Id: dmap3d.f90,v 1.16 2007/12/14 15:05:47 Menger beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


  type,public :: DMAP3D_struct

  private
  logical   :: skip_wrapup         ! wrapup flag

!----------input parameters.

  !Input gather type, output gather type, anti-aliasing strategy, save workfile
  character(len=8)  :: InType, OuType, AAType, SaveWF

  !very general description of output gathers
  real              :: out_offset
  real              :: out_azimuth
  real              :: e1ini,e2ini,g1ini,g2ini !initial values
  real              :: de1,de2,dg1,dg2         !increments
  real              :: e1_last,e2_last         !increments
  integer           :: ne1,ne2,ng1,ng2         !how many?

  real              :: dg1lb,dg1ub,dg2lb,dg2ub  !input gthr screen
  real              :: vel          !this version uses a constant velocity
  integer           :: MNTOD        !input buffer size
  integer           :: ntim               ! process parameters.
  integer           :: Nnmax        !maximum number of nearest neighbors
  real              :: Vcmax        !maximum voronoi cell size
  integer           :: Itmin        !mapping minimum time threshhold
  real              :: Ttrmax       !maximum traveltime ratio
  real              :: Hmin         !minimum normal offset
  integer           :: Nsubt        !maximum number of subtriangles
  real              :: tprang       !dip angle at which to begin taper
  real              :: Afmax        !maximum allowed amplitude factor
  real              :: einbin       !bin sizes
  integer           :: inhdr        !header word specifying gather number
  integer           :: lfmax        !maximum antialiasing filter size
  integer           :: wfsize       !unused
  Character(len=8)  :: PrNbr, PrItr
  Integer           :: MapPr
  Integer           :: min_samp
  Character(len=8)  :: scale_rms
  real              :: win_inc      ! process parameter

  integer           :: ndpt,nwih
  real              :: dt, tstrt
  real(8)           :: xwidth, ywidth, xorigin, yorigin
  real(8)           :: dx11, dx12, dx21, dx22

!----------Added to include these menu items on the GUI front-end.

  integer           :: naatype_menu
  character(len=4)  :: aatype_menu(4)

  integer           :: nintype_menu
  character(len=4)  :: intype_menu(4)

  integer           :: noutype_menu
  character(len=4)  :: outype_menu(5)

  integer           :: nsavewf_menu
  character(len=4)  :: savewf_menu(2)

  integer           :: nprnbr_menu
  Character(len=4)  :: prnbr_menu(2)

  integer           :: npritr_menu
  Character(len=4)  :: pritr_menu(2)

  real              :: Bigness(2)

!----------other parameters.

  real              :: eg2sr(4,4), sr2eg(4,4)
  real              :: eg2mh(4,4), mh2eg(4,4)
  real              :: ego2egi(4,4)
  real              :: target_slope, tol_slope
  integer           :: idmax
  real              :: dmax
  integer           :: ndead, ninwf, ninti, ninhi, nmap
  integer           :: Iflow
  integer           :: done, luscr
  integer           :: mycpu, rootcpu, numcpu
  integer           :: Ntrin, Ntrout, Ntrib, Ntotout
  integer           :: Ig1,Ig2,Ie1,Ie2
  integer           :: Nin, Nav, Ndo, Ngin, Ngin1, Ngin2
  real              :: xinmin(2), xinmax(2)
  integer           :: ticount
  integer           :: closest
  integer           :: file_index
  Character(len=filename_length) :: file_name, root_name, pathname_dir

  !Subcube parameters
  integer           :: nsc(5)      !Nominal # of subcubes in each direction
  integer           :: lnsc(5)     !Nominal subcube length in each direction
  integer           :: rmsc(5)    !remaining subcube elements
  integer           :: jmsc, rmjm  !special direction
  integer           :: isc(4)  !internal subcube coords
  integer           :: msc(4)  !subcube identifiers in each direction
  integer           :: scnum   !subcube id #
  integer           :: ne1sc, ne2sc, ng1sc, ng2sc  !size of current subcube
  real              :: e1isc, e2isc, g1isc, g2isc  !loc of current subcube
  integer           :: ie1sc, ie2sc, ig1sc, ig2sc
  integer           :: ntotoutsc, ntroutsc
  integer           :: pe_out ! which pe has the next trace
  integer, pointer  :: negsc(:,:,:)  !4*numcpu array of subcube locations

  !----------storage.
   Real, pointer     :: Din(:,:)       !Input Data
   Real, pointer     :: HDin(:,:)      !Input Headers
   
!  double precision, pointer  :: HDin(:,:)      !Input Headers

  Real, pointer     :: Dout(:,:)      !Output Data

  !Dimensioned (2,MNTOD)
  Real,Pointer       :: Sloc(:,:)     !Source loc
  Real,Pointer       :: Rloc(:,:)     !Receiver loc
  Real,Pointer       :: H(:,:)        !Offset coords
  Real,Pointer       :: X(:,:)        !Midpoint coords

  !Dimensioned (MNTOD)
  Integer,Pointer    :: Hm(:)         !Head Mute
  Integer,Pointer    :: Tm(:)         !Tail Mute
  Integer,Pointer    :: Mask(:)       !Indicates which positions to use
  Integer,Pointer    :: Same(:)       !Number of locations at same coords
  Integer,Pointer    :: Nnn(:)        !Number of Natural Neighbors
  Integer,Pointer    :: Gbin(:)       !Gather bin numbers

  !Dimensioned (NNMAX,MNTOD)
  Integer,Pointer    :: In(:,:)       !Index of Natural Neighbors

  !Dimensioned (2,NNMAX,MNTOD)
  Real,Pointer       :: DX(:,:,:)    !Relative coords of each NN
  Real,Pointer       :: Vc(:,:,:)    !Voronoi cell nodes

  !Grid structure
  Type (grid_struct) grob

  type ( trcio_struct ), pointer :: image_obj   ! trcio image file io structure
  type ( trcio_struct ), pointer :: storetr_obj ! trcio temp trace io structure

  character(len=filename_length) :: image_fname   ! image file name
  character(len=filename_length) :: storetr_fname   ! temporary trace file name

  character(len=filename_length) :: job_name ! job name

  integer                        :: ntr_disk
  Real,Pointer                   :: din_tmp(:,:)
  double precision,Pointer       :: hd_tmp(:,:)

  logical,Pointer                :: in_disk(:)   ! trace in disk y/n 
  integer,Pointer                :: in_mem(:)    ! position of trace in memory 
  integer,Pointer                :: loc_mem(:)   ! location of trace in memory
  integer                        :: itr_mem       
  logical                        :: use_disk     ! use disk y/n
  integer                        :: winlen     
  integer                        :: namp_store 
  logical                        :: calc_med_scale
  Real,Pointer                   :: amp_store(:)
  real                           :: amp_median   
  end type DMAP3D_struct


   ! Need to check this out for modification
   Integer, Parameter, Private   :: Ipr = 0        !Used in debugging
   Integer, Parameter, Private   :: nskip = 500    !Used in debugging
   Real,    Parameter, Private   :: AREA_MULTI = 5 !Increase area size
 
   Integer, Private              :: Iupr           !Print Unit


!!------------------------------ data -----------------------------------!!
!!------------------------------ data -----------------------------------!!
!!------------------------------ data -----------------------------------!!


      type(DMAP3D_struct),pointer,save :: object    ! needed for traps.

    integer    ,parameter :: scale_rms_opt_n = 3
    character(len=8),save :: scale_rms_opt_c(scale_rms_opt_n)         &
                = (/ 'NO      ', 'CONSTANT', 'VARYING ' /)

      contains


!!-------------------------- create -------------------------------------!!
!!-------------------------- create -------------------------------------!!
!!-------------------------- create -------------------------------------!!


      subroutine DMAP3D_create (obj)
      implicit none
      type(DMAP3D_struct),pointer :: obj     ! arguments

      allocate (obj)

      nullify(obj%negsc)
      nullify(obj%Din)
      nullify(obj%HDin)
      nullify(obj%Dout)
      nullify(obj%Sloc)
      nullify(obj%Rloc)
      nullify(obj%H)
      nullify(obj%X)
      nullify(obj%Hm)
      nullify(obj%Tm)
      nullify(obj%Mask)
      nullify(obj%Same)
      nullify(obj%NNN)
      nullify(obj%Gbin)
      nullify(obj%In)
      nullify(obj%Dx)
      nullify(obj%Vc)
      nullify(obj%image_obj) ! jpa
      nullify(obj%storetr_obj) ! jpa
      nullify(obj%din_tmp)
      nullify(obj%hd_tmp)
      nullify(obj%in_disk)
      nullify(obj%in_mem)
      nullify(obj%loc_mem)
      nullify(obj%amp_store)
                  
      call DMAP3D_initialize (obj)
    
      return
      end subroutine DMAP3D_create


!!---------------------------- delete -----------------------------------!!
!!---------------------------- delete -----------------------------------!!
!!---------------------------- delete -----------------------------------!!


      subroutine DMAP3D_delete (obj)
      implicit none
      type(DMAP3D_struct),pointer :: obj     ! arguments

!<execute_only>
      call DMAP3D_wrapup (obj)
!</execute_only>

      if (associated(obj%Din  )) deallocate (obj%Din  )
      if (associated(obj%HDin )) deallocate (obj%HDin )

      if (associated(obj%in_disk  )) deallocate (obj%in_disk )
      if (associated(obj%in_mem )) deallocate (obj%in_mem )
      if (associated(obj%loc_mem )) deallocate (obj%loc_mem )
      if (associated(obj%amp_store)) deallocate (obj%amp_store )
       
      if (associated(obj%din_tmp)) deallocate (obj%din_tmp)
      if (associated(obj%hd_tmp )) deallocate (obj%hd_tmp)
      if (associated(obj%dout )) deallocate (obj%dout )
      if (associated(obj%Sloc )) deallocate (obj%Sloc )
      if (associated(obj%Rloc )) deallocate (obj%Rloc )
      if (associated(obj%H    )) deallocate (obj%H    )
      if (associated(obj%X    )) deallocate (obj%X    )
      if (associated(obj%Gbin )) deallocate (obj%Gbin )
      if (associated(obj%Hm   )) deallocate (obj%Hm   )
      if (associated(obj%Tm   )) deallocate (obj%Tm   )
      if (associated(obj%Mask )) deallocate (obj%Mask )
      if (associated(obj%Same )) deallocate (obj%Same )
      if (associated(obj%Nnn  )) deallocate (obj%Nnn  )
      if (associated(obj%In   )) deallocate (obj%In   )
      if (associated(obj%Dx   )) deallocate (obj%Dx   )
      if (associated(obj%Vc   )) deallocate (obj%Vc   )
      if (associated(obj%negsc)) deallocate (obj%negsc)

      deallocate(obj)
      return
      end subroutine DMAP3D_delete


!!--------------------------- initialize ---------------------------------!!
!!--------------------------- initialize ---------------------------------!!
!!--------------------------- initialize ---------------------------------!!


      subroutine DMAP3D_initialize (obj)
      implicit none
      type(DMAP3D_struct),intent(inout) :: obj  !argument

      iupr = pc_get_lun()        !Assign the print unit

      obj%InType       = 'COG'   !Input data configuration type
      obj%OuType       = 'COG'   !Output data configuration type
      obj%AAType       = 'NEAR'  !Anti-Aliasing type
      obj%SaveWF       = 'NO'    !Save work files at end of job
      obj%PrNbr        = 'NO'    !Print neighbor list
      obj%PrItr        = 'NO'    !Print input trace list

!Include these menu items on the GUI front-end.


      obj%nAATYPE_menu  = 4
      obj%AATYPE_menu(1) = 'NEAR'
      obj%AATYPE_menu(2) = 'LIN'
      obj%AATYPE_menu(3) = 'NONE'
      obj%AATYPE_menu(4) = 'QN'

      obj%nINTYPE_menu  = 4
      obj%INTYPE_menu(1) = 'COG'
      obj%INTYPE_menu(2) = 'CSG'
      obj%INTYPE_menu(3) = 'CRG'
      obj%INTYPE_menu(4) = 'MAN'

      obj%nOUTYPE_menu  = 4
      obj%OUTYPE_menu(1) = 'COG'
      obj%OUTYPE_menu(2) = 'CSG'
      obj%OUTYPE_menu(3) = 'CRG'
      obj%OUTYPE_menu(4) = 'CAZ'
      obj%OUTYPE_menu(5) = 'MAN'

      obj%nSAVEWF_menu = 2
      obj%SAVEWF_menu(1) = 'YES'
      obj%SAVEWF_menu(2) = 'NO'

      obj%nPrNbr_menu   = 2
      obj%PrNbr_menu(1) = 'YES'
      obj%PrNbr_menu(2) = 'NO'

      obj%nPrItr_menu   = 2
      obj%PrItr_menu(1) = 'YES'
      obj%PrItr_menu(2) = 'NO'

!End modified

      obj%eg2sr   = 0.    !e-g to source-receiver transform matrix
      obj%sr2eg   = 0.    !source-receiver to e-g transform matrix
      obj%eg2mh   = 0.    !e-g to midpoint-offset transform matrix
      obj%mh2eg   = 0.    !midpoint-offset to e-g transform matrix
      obj%ego2egi = 0.
      obj%target_slope = 0.
      obj%tol_slope    = 1.

      obj%out_offset  = 0.
      obj%out_azimuth = 0.
      obj%scale_rms   = 'NO      '
      obj%win_inc     = 0.5
      obj%namp_store  = 0
      obj%amp_median  = 0.0
    
      obj%G1ini   = 0.
      obj%G2ini   = 0.
      obj%NG1   = 1     !Number of output gathers in direction 1
      obj%NG2   = 1     !Number of output gathers in direction 2
      obj%DG1   = 1.    !Distance between gathers in direction 1
      obj%DG2   = 1.    !Distance between gathers in direction 2

      obj%E1ini   = 1.
      obj%E2ini   = 1.
      obj%NE1   = 1     !Number of gather elements in direction 1
      obj%NE2   = 1     !Number of gather elements in direction 2
      obj%DE1   = 1.    !Distance between elements in direction 1
      obj%DE2   = 1.    !Distance between elements in direction 2
      obj%E1_LAST = 1.
      obj%E2_LAST = 1.


      obj%Dg1lb   = - 10000.
      obj%Dg1ub   = + 10000.
      obj%Dg2lb   = - 10000.
      obj%Dg2ub   = + 10000.

      obj%MNTOD   = 20000
      obj%ntim    = 10000
      obj%Nnmax   = 12
      obj%Vcmax   = 5.

      call pc_get_global ('grid', obj%grob)
      call grid_get_widths(obj%grob,obj%xwidth,obj%ywidth)      !x-y grid size.
      obj%Hmin = 1.0 * max(obj%xwidth,obj%ywidth)             

      obj%Einbin  = 0.25
      obj%inhdr   = 3
      obj%Itmin   = 10
      obj%vel     = 5000.
      obj%Nsubt   = 25     !Maximum number of subtriangles allowed in memory
      obj%Tprang  = 60.    !Incident angle to begin tapering
      obj%Ttrmax  = 3.     !Maximum traveltime ratio
      obj%Afmax   = 10000. !Maximum amplitude ratio
      obj%Lfmax   = 16     !Maximum antialias filter length
      obj%Wfsize  = 100

      obj%Iflow   = 1     !determines mode of program flow
      obj%IG1 = 0     !index of last gather output in 1st direction
      obj%IG2 = 0     !index of last gather output in 2nd direction
      obj%IE1 = 0     !index of last element output in 1st direction
      obj%IE2 = 0     !index of last element output in 2nd direction

      obj%ne1sc = 0   !subcube parameters
      obj%ne2sc = 0
      obj%ng1sc = 0
      obj%ng2sc = 0
      obj%Nsc  = 0
      obj%lnsc = 0
      obj%rmsc = 0
      obj%jmsc = 0
      obj%rmjm = 0
      obj%isc  = 0
      obj%msc  = 0
      obj%scnum = 0
      obj%ntotoutsc = 0
      obj%ntroutsc = 0
      obj%pe_out   = 0    ! which pe has the next output trace


      obj%Nin     = 0     !last number of input traces
      obj%Ntrib   = 0     !number of traces in input buffer
      obj%Ntrin   = 0     !total number of input traces
      obj%Ntrout  = 0     !total number of output traces
      obj%Ngin    = 0     !Number of gathers input
      obj%Ngin1   = 0
      obj%Ngin2   = 0

      obj%xinmin  = 0.
      obj%xinmax  = 0.

      obj%prnbr   = "NO"
      obj%pritr   = "NO"
      obj%Ndo     = 0
      obj%Nav     = 0
      obj%Ntotout = 0

      obj%nwih    = 0
      obj%ndpt    = 0
      obj%dt      = 0.
      obj%tstrt   = 0.

      obj%luscr   = 0     !Unit number for scratch i/o
      obj%done    = 0     !Signal cpu is done with output

      obj%MapPr   = 0

      !Summary statistics
      obj%dmax    = 0.
      obj%idmax   = 0
      obj%ndead   = 0
      obj%ninwf   = 0
      obj%ninti   = 0
      obj%ninhi   = 0
      obj%nmap    = 0

!Initializing the multiprocessor component of the structure

      obj%mycpu   = 0
      obj%rootcpu = 0
      obj%numcpu  = 1

      call DMAP3D_update (obj)
      return
      end subroutine DMAP3D_initialize


!!----------------------- start of update --------------------------------!!
!!----------------------- start of update --------------------------------!!
!!----------------------- start of update --------------------------------!!


      subroutine DMAP3D_update (obj)
      implicit none
      type(DMAP3D_struct),intent(inout),target :: obj        ! arguments

      logical           :: need_label, need_request       ! local

      integer           :: numtr                          ! local
      integer           :: nscratch                       ! local
      integer           :: nstore                         ! local
      integer           :: ier(23), istatus               ! local
      real              ::          ego2sr(4,4) ! local

      logical           :: verify                         ! local
      integer           :: state, i_stat                  ! local

!Modified by turning MPI related stuff off.
!     integer           :: cmpi_i_pel,cmpi_n_pel          ! local

      integer           :: my_cpu, root_cpu, num_cpu      ! local


     integer           :: i_err                           ! error flag

     real              :: tmax
          
      object => obj                  ! needed for traps.
      obj%skip_wrapup = .true.       ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if 

!Multi-processor stuff
!After initializing them, we are now updating the multi-processor
!variables to 0, 0 and 1 respectively.

      root_cpu = 0
      num_cpu = pcpsx_n_pel()
      my_cpu  = pcpsx_i_pel()

      obj%mycpu   = my_cpu
      obj%rootcpu = root_cpu
      obj%numcpu  = num_cpu

!!----------------------- read parameters --------------------------------!!
!!----------------------- read parameters --------------------------------!!
!!----------------------- read parameters --------------------------------!!


      call pc_get_global ('numtr', numtr)     ! maximum number of traces.
      call pc_get_global ('nwih' , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt' , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('dt'   , obj%dt)    ! trace sample interval (sec).
      call pc_get_global ('tstrt', obj%tstrt) ! time of 1st trace sample (sec).
      call pc_get_global ('grid' , obj%grob)   ! grid transformation structure.
      
      call grid_get_widths(obj%grob,obj%xwidth,obj%ywidth)      !x-y grid size.
      call grid_get_origin(obj%grob,obj%xorigin,obj%yorigin)  !x-y grid origin.
      call grid_get_dx(obj%grob,obj%dx11,obj%dx21,obj%dx12,obj%dx22)
            
!     keyword     parameter      trap (optional)
!       |    |         |
!      call pc_get ('intype'  , obj%intype,      DMAP3D_ityp_trap)
!      call pc_get ('outype'  , obj%outype,      DMAP3D_otyp_trap)

      call pc_get ('E1ini'    , obj%E1ini      )
      call pc_get ('E2ini'    , obj%E2ini      )
      call pc_get ('NE1 '     , obj%NE1 ,      DMAP3D_NE1_trap)
      call pc_get ('NE2 '     , obj%NE2 ,      DMAP3D_NE2_trap)
      call pc_get ('dE1 '     , obj%DE1 ,      DMAP3D_DE1_trap)
      call pc_get ('dE2 '     , obj%DE2 ,      DMAP3D_DE2_trap)
      call pc_get ('E1_LAST ' , obj%E1_LAST    )
      call pc_get ('E2_LAST ' , obj%E2_LAST    )

      call pc_get ('out_offset'   , obj%out_offset)
      call pc_get ('out_azimuth'  , obj%out_azimuth)
      call pc_get ('scale_rms'    , obj%scale_rms)
      call pc_get ('win_inc'      , obj%win_inc )
            
 !     call pc_get ('G1ini'   , obj%G1ini      )
 !     call pc_get ('G2ini'   , obj%G2ini      )
 !     call pc_get ('NG1 '    , obj%NG1 ,      DMAP3D_NG1_trap)
 !     call pc_get ('NG2 '    , obj%NG2 ,      DMAP3D_NG2_trap)
 !     call pc_get ('dG1 '    , obj%DG1 ,      DMAP3D_DG1_trap)
 !     call pc_get ('dG2 '    , obj%DG2 ,      DMAP3D_DG2_trap)

!      call pc_get ('dg1lb'   , obj%dg1lb      )
!      call pc_get ('dg1ub'   , obj%dg1ub      )
!      call pc_get ('dg2lb'   , obj%dg2lb      )
!      call pc_get ('dg2ub'   , obj%dg2ub      )

      call pc_get ('vel '    , obj%vel    ,      DMAP3D_vel_trap )
      call pc_get ('MNTOD'   , obj%MNTOD ,       DMAP3D_MNTOD_trap)
      call pc_get ('ntim'    , obj%ntim ) 
      call pc_get ('Nnmax'   , obj%Nnmax  ,      DMAP3D_nnmx_trap)
      call pc_get ('Vcmax'   , obj%Vcmax  ,      DMAP3D_vcmx_trap)
      call pc_get ('Itmin'   , obj%Itmin  ,      DMAP3D_itmn_trap)
      call pc_get ('Ttrmax'  , obj%Ttrmax ,      DMAP3D_ttrm_trap)

      call pc_get ('Hmin'    , obj%Hmin   ,      DMAP3D_hmin_trap)

      call pc_get ('Nsubt'   , obj%Nsubt  ,      DMAP3D_nsub_trap)
      call pc_get ('tprang'  , obj%tprang ,      DMAP3D_tpra_trap)
      call pc_get ('Afmax'   , obj%Afmax  ,      DMAP3D_afma_trap)

!      call pc_get ('InHdr'   , obj%InHdr  ,      DMAP3D_ihdr_trap)

      call pc_get ('Lfmax'   , obj%Lfmax  ,      DMAP3D_lfmx_trap)
      call pc_get ('Wfsize'  , obj%Wfsize ,      DMAP3D_wfsz_trap)

!      call pc_get ('PrNbr'   , obj%PrNbr      )
!      call pc_get ('PrItr'   , obj%PrItr      )

!      call pc_get ('Einbin'  , obj%Einbin ,      DMAP3D_ebin_trap)
!      call pc_get ('AAtype'  , obj%AAtype     )
!      call pc_get ('SaveWF'  , obj%SaveWF     )
!      call pc_get ('MapPr'   , obj%MapPr      )


!!------------------------ verify parameters -----------------------------!!
!!------------------------ verify parameters -----------------------------!!
!!------------------------ verify parameters -----------------------------!!

      !Normalize the velocity
      ! obj%vel = obj%vel * obj%dt * .5
      
      call string_to_upper (obj%intype)
      call string_to_upper (obj%outype)
      call string_to_upper (obj%aatype)
      call string_to_upper (obj%savewf)
      call string_to_upper (obj%PrNbr)
      call string_to_upper (obj%PrItr)
 
      call string_to_upper (obj%scale_rms)
 
     if (obj%scale_rms(1:2) == 'NO' .or. obj%scale_rms(1:2) == 'CO') then     
       call pc_put_sensitive_field_flag   ('WIN_INC',   .false.)
     else 
       call pc_put_sensitive_field_flag   ('WIN_INC',   .true.)
     end if     

      if(obj%ntim <= 0) then 
        call pc_error('NTIM MUST BE > 0')  
      end if

      if( obj%win_inc < obj%dt ) then
        call pc_error('Invalid value: WIN_INC must be >=', obj%win_inc )
        obj%win_inc = 0.5
      endif
      
      tmax = obj%tstrt + (obj%ndpt-1)*obj%dt
      
      if( obj%win_inc > tmax ) then
        call pc_error('Invalid value: WIN_INC must be <=', tmax)
        obj%win_inc = 0.5
      endif
      
       i_stat = pattern_stop2('DMAP3D:', verify, &
        obj%e1ini, obj%de1, obj%e1_last, obj%ne1, &
           'e1ini',   'de1',   'e1_last',   'ne1', &
        pc_verify_scalar('e1ini'), pc_verify_scalar('de1'), &
        pc_verify_scalar('e1_last'), pc_verify_scalar('ne1'))

       i_stat = pattern_stop2('DMAP3D:', verify, &
        obj%e2ini, obj%de2, obj%e2_last, obj%ne2, &
           'e2ini',   'de2',   'e2_last',   'ne2', &
        pc_verify_scalar('e2ini'),   pc_verify_scalar('de2'), &
        pc_verify_scalar('e2_last'), pc_verify_scalar('ne2'))

!!----------------------- call processes internally ----------------------!!
!!----------------------- call processes internally ----------------------!!
!!----------------------- call processes internally ----------------------!!



!!----------------------- write parameters -------------------------------!!
!!----------------------- write parameters -------------------------------!!
!!----------------------- write parameters -------------------------------!!


!      call pc_put_options_field ('AATYPE', obj%aatype_menu, obj%naatype_menu)
!      call pc_put_options_field ('INTYPE', obj%intype_menu, obj%nintype_menu)
!      call pc_put_options_field ('OUTYPE', obj%outype_menu, obj%noutype_menu)
!      call pc_put_options_field ('SAVEWF', obj%savewf_menu, obj%nsavewf_menu)

!      call pc_put_options_field ('PRNBR' , obj%prnbr_menu , obj%nprnbr_menu)
!      call pc_put_options_field ('PRITR',  obj%pritr_menu , obj%npritr_menu)

!      call pc_put ('intype'  , obj%intype )
!      call pc_put ('outype'  , obj%outype )
!      call pc_put ('savewf'  , obj%savewf )

      call pc_put_options_field ('scale_rms',scale_rms_opt_c,scale_rms_opt_n)

      call pc_put ('E1ini'   , obj%E1ini  )
      call pc_put ('E2ini'   , obj%E2ini  )
      call pc_put ('NE1 '    , obj%NE1    )
      call pc_put ('NE2 '    , obj%NE2    )
      call pc_put ('DE1 '    , obj%DE1    )
      call pc_put ('DE2 '    , obj%DE2    )
      call pc_put ('E1_LAST' , obj%E1_LAST)
      call pc_put ('E2_LAST' , obj%E2_LAST)

      call pc_put ('out_offset'   , obj%out_offset)
      call pc_put ('out_azimuth'  , obj%out_azimuth)
      call pc_put ('scale_rms'    , obj%scale_rms)
      call pc_put ('win_inc'      , obj%win_inc   )
            
!      call pc_put ('G1ini'   , obj%G1ini  )
!      call pc_put ('G2ini'   , obj%G2ini  )
!      call pc_put ('dG1 '    , obj%DG1    )
!      call pc_put ('dG2 '    , obj%DG2    )
!      call pc_put ('NG1 '    , obj%NG1    )
!      call pc_put ('NG2 '    , obj%NG2    )

!      call pc_put ('dg1lb'   , obj%dg1lb  )
!      call pc_put ('dg1ub'   , obj%dg1ub  )
!      call pc_put ('dg2lb'   , obj%dg2lb  )
!      call pc_put ('dg2ub'   , obj%dg2ub  )

      call pc_put ('vel '    , obj%vel    )
      call pc_put ('MNTOD'   , obj%MNTOD )
      call pc_put ('ntim'    , obj%ntim)
      call pc_put ('nnmax'   , obj%nnmax  )
      call pc_put ('vcmax'   , obj%vcmax  )
      call pc_put ('itmin'   , obj%itmin  )
      call pc_put ('ttrmax'  , obj%ttrmax )

      call pc_put ('hmin'    , obj%hmin   )
      
      call pc_put ('Nsubt'   , obj%Nsubt  )
      call pc_put ('tprang'  , obj%tprang )
      call pc_put ('afmax'   , obj%afmax  )

!      call pc_put ('inhdr'   , obj%inhdr  )

      call pc_put ('lfmax'   , obj%lfmax  )
      call pc_put ('wfsize'  , obj%wfsize )
 
!      call pc_put ('pritr'   , obj%pritr  )
!      call pc_put ('prnbr'   , obj%prnbr  )

!      call pc_put ('einbin'  , obj%einbin )
!      call pc_put ('aatype'  , obj%aatype )
!      call pc_put ('mappr'   , obj%mappr  )

      need_label   =  .true.
      need_request =  .true.
      nscratch     =  10000
      nstore       =  obj%ndpt + obj%MNTOD*(5*obj%Nnmax + 15)

      call pc_put_control ("NEED_LABEL"  ,   need_label  )
      call pc_put_control ("NEED_REQUEST",   need_request)
      call pc_put_control ("NSCRATCH"    ,   nscratch    )
      call pc_put_control ("NSTORE"      ,   nstore      )

! dwh 05-21-01
! test single pe parallel output
! add the following 3 controls for single pe test

    call pc_put_control ( 'parallel_safe' ,         .true.                )
    call pc_put_control ( 'pcps_send_mode' ,        'PCPS_BOSS_EXECS'     )
    call pc_put_control ( 'pcps_generator_mode' ,   'PCPS_TRACE_GEN'      )

!dwh-05-21-01
!remove the following controls 8 for single pe test
!dwh call pc_put_control ("parallel_safe",  .true.      )
!dwh call pc_put_control ("pcps_send_mode",       'PCPS_BOSS_EXECS' )
!dwh call pc_put_control ("pcps_generator_mode",  'PCPS_TRACE_GEN')
!dwh call pc_put_control ("pcps_receive_mode",    'PCPS_RECEIVE_GROUP' )
!dwh call pc_put_control ("pcps_send_eof_mode",   'PCPS_SEND_ALL_EOF' )
!dwh call pc_put_control ("pcps_alt_send_mode",   'PCPS_SEND_ALL_AVAIL' )
!dwh call pc_put_control ("pcps_alt_receive_mode",'PCPS_RECEIVE_GROUP' )
!dwh call pc_put_control ("pcps_bunch_mode",      'PCPS_BUNCH_TRACES' )


!!--------------------- prepare for execution ----------------------------!!
!!--------------------- prepare for execution ----------------------------!!
!!--------------------- prepare for execution ----------------------------!!


      if (associated(obj%Din  )) deallocate (obj%Din  )
      if (associated(obj%HDin )) deallocate (obj%HDin )
      if (associated(obj%in_disk  )) deallocate (obj%in_disk )
      if (associated(obj%in_mem ))   deallocate (obj%in_mem )
      if (associated(obj%loc_mem ))  deallocate (obj%loc_mem )
      if (associated(obj%amp_store ))  deallocate (obj%amp_store )
       
      if (associated(obj%din_tmp)) deallocate (obj%din_tmp)
      if (associated(obj%hd_tmp )) deallocate (obj%hd_tmp)
      if (associated(obj%dout )) deallocate (obj%dout )
      if (associated(obj%Sloc )) deallocate (obj%Sloc )
      if (associated(obj%Rloc )) deallocate (obj%Rloc )
      if (associated(obj%H    )) deallocate (obj%H    )
      if (associated(obj%X    )) deallocate (obj%X    )
      if (associated(obj%Gbin )) deallocate (obj%Gbin )
      if (associated(obj%Hm   )) deallocate (obj%Hm   )
      if (associated(obj%Tm   )) deallocate (obj%Tm   )
      if (associated(obj%Mask )) deallocate (obj%Mask )
      if (associated(obj%Same )) deallocate (obj%Same )
      if (associated(obj%Nnn  )) deallocate (obj%Nnn  )
      if (associated(obj%In   )) deallocate (obj%In   )
      if (associated(obj%Dx   )) deallocate (obj%Dx   )
      if (associated(obj%Vc   )) deallocate (obj%Vc   )
      if (associated(obj%negsc)) deallocate (obj%negsc)
      
      obj%G1ini = obj%out_offset * cos(obj%out_azimuth*3.141593/180.)
      obj%G2ini = obj%out_offset * sin(obj%out_azimuth*3.141593/180.)      
      obj%G1ini = 0.5 * obj%G1ini/obj%xwidth
      obj%G2ini = 0.5 * obj%G2ini/obj%ywidth

      obj%NG1 = 1
      obj%NG2 = 1
      obj%DG1 = 10.
      obj%DG2 = 10.

      write(iupr,'(2(a,f12.3))') ' G1ini= ', obj%G1ini, &
                          '        G2ini= ', obj%G2ini                          
 

      obj%Iflow   = 1    !determines mode of program flow
      obj%IG1 = 0    !index of last gather output in 1st direction
      obj%IG2 = 0    !index of last gather output in 2nd direction
      obj%IE1 = 0    !index of last element output in 1st direction
      obj%IE2 = 0    !index of last element output in 2nd direction
      obj%ne1sc = 0
      obj%ne2sc = 0
      obj%ng1sc = 0
      obj%ng2sc = 0
      obj%isc = 0

      obj%Nin     = 0    !last number of input traces
      obj%Ntrin   = 0
      obj%Ntrout  = 0
      obj%Ngin    = 0    !Number of gathers input
      obj%Ngin1   = 0
      obj%Ngin2   = 0
      obj%xinmin  = 0.
      obj%xinmax  = 0.
      obj%Ntotout = obj%NE1*obj%NE2*obj%NG1*obj%NG2 !Number of traces to output
      
!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      !---------------Define output file name--------------------------
      !call pc_get_jdata ('pathname_dir', obj%pathname_dir )
      !obj%root_name = 'stolt'
      !obj%file_index = pc_get_ipn()
      !obj%file_name = trim( obj%pathname_dir ) // trim( obj%root_name )
      !call pp_create_file_name ( base_name = obj%file_name, &
      !                           file_name = obj%file_name, &
      !                           i_pn      = obj%file_index &
      !                         )
      !write(iupr,'(2a)')"Output File ", obj%file_name

      !obj%trcio_obj => trcio_open( filename=obj%file_name,     &
      !                             io_mode='w',                &
      !                             scratch=.false.,            &
      !                             nwih=obj%nwih,              &
      !                             ndpt=obj%ndpt,              &
      !                             nbits=32,                   &
      !                             nbitshd=64)

      !----------------Define Output Subcube---------------------------
            
      obj%winlen = obj%win_inc/obj%dt
 
      !obj%scnum = max(0,obj%mycpu-1)
      obj%scnum = obj%mycpu
      Call DMAP3D_Subdiv(obj,istatus)
      if (Istatus.ne.0) then
           call pc_error('bad output grid size or numcpu')
      end if
      Call DMAP3D_FindHypCubLoc(obj)

      obj%ntotoutsc = obj%ne1sc*obj%ne2sc*obj%ng1sc*obj%ng2sc

      If(obj%mycpu == obj%rootcpu) then
         Write(iupr,'(a,i6,a4,i6)')"Multiprocessing with processor "  &
            ,obj%mycpu," of ",obj%numcpu
         write(iupr,'(a10,4i6)')"Cube Size= ",obj%ne1,obj%ne2,obj%ng1,obj%ng2
         write(iupr,'(a10,i6)')"numcpu= ",obj%numcpu
         write(iupr,'(a,5i6)') "num subcubes/dir ",obj%nsc
         write(iupr,'(a,5i6)') "subcube lengths  ",obj%lnsc
         write(iupr,'(a,5i6)') "remainders       ",obj%rmsc
         write(iupr,'(2(a,i6))')"special direction= ",obj%jmsc,   &
             " special remainder= ",obj%rmjm
         write(iupr,'(a14,5i6)')"SubCube Size= ",obj%ne1sc,obj%ne2sc,   &
            obj%ng1sc,obj%ng2sc,obj%ntotoutsc
         write(iupr,'(a14,4i6)')"SubCube Loc = ",obj%ie1sc,obj%ie2sc,   &
            obj%ig1sc,obj%ig2sc
         write(iupr,'(a14,4e12.4)')"SubCube Cord= ",obj%e1isc,obj%e2isc,   &
            obj%g1isc,obj%g2isc
         write(iupr,'(a14,4i6)')"SubCube Id's= ",obj%msc
         write(iupr,'(a,2f12.4)')"grid widths= ",obj%xwidth,obj%ywidth
         write(iupr,*) '  obj%Hmin ', obj%Hmin

! testing

         if(obj%nsubt == 999) write(iupr,*) ' AMO weights applies '   
 
       End If


!-------- Input Gather Transform Matrices -------------------------------

      !make sure intype is capitalized
      call string_to_upper(obj%intype)

!Build offset gather transformation matrix

      if (obj%intype(1:2) == 'CO' ) then

           obj%intype = 'COG'

           obj%sr2eg(1,1) =   .5/obj%xwidth
           obj%sr2eg(1,2) =   0.
           obj%sr2eg(1,3) =   .5/obj%xwidth
           obj%sr2eg(1,4) =   0.
           obj%sr2eg(2,1) = - 0.
           obj%sr2eg(2,2) =   .5/obj%ywidth
           obj%sr2eg(2,3) = - 0.
           obj%sr2eg(2,4) =   .5/obj%ywidth
           obj%sr2eg(3,1) =   .5/obj%xwidth
           obj%sr2eg(3,2) =   0.
           obj%sr2eg(3,3) = - .5/obj%xwidth
           obj%sr2eg(3,4) = - 0.
           obj%sr2eg(4,1) = - 0.
           obj%sr2eg(4,2) =   .5/obj%ywidth
           obj%sr2eg(4,3) =   0.
           obj%sr2eg(4,4) = - .5/obj%ywidth
           obj%target_slope = 0.

!Build source gather transformation matrix

      elseif (obj%intype(1:2) == 'CS' ) then

           obj%intype = 'CSG'

           obj%sr2eg(1,1) = - 1./obj%xwidth
           obj%sr2eg(1,2) = - 0.
           obj%sr2eg(1,3) =   1./obj%xwidth
           obj%sr2eg(1,4) =   0.
           obj%sr2eg(2,1) =   0.
           obj%sr2eg(2,2) = - 1./obj%ywidth
           obj%sr2eg(2,3) = - 0.
           obj%sr2eg(2,4) =   1./obj%ywidth
           obj%sr2eg(3,1) =   1./obj%xwidth
           obj%sr2eg(3,2) =   0.
           obj%sr2eg(3,3) =   0.
           obj%sr2eg(3,4) =   0.
           obj%sr2eg(4,1) = - 0.
           obj%sr2eg(4,2) =   1./obj%ywidth
           obj%sr2eg(4,3) =   0.
           obj%sr2eg(4,4) =   0.
           obj%target_slope = -1.

!Build receiver gather transformation matrix

      elseif (obj%intype(1:2) == 'CR'  &
         .or. obj%intype(1:2) == 'CG' ) then

           obj%intype = 'CRG'

           obj%sr2eg(1,1) =   1./obj%xwidth
           obj%sr2eg(1,2) =   0.
           obj%sr2eg(1,3) = - 1./obj%xwidth
           obj%sr2eg(1,4) = - 0.
           obj%sr2eg(2,1) = - 0.
           obj%sr2eg(2,2) =   1./obj%ywidth
           obj%sr2eg(2,3) =   0.
           obj%sr2eg(2,4) = - 1./obj%ywidth
           obj%sr2eg(3,1) =   0.
           obj%sr2eg(3,2) =   0.
           obj%sr2eg(3,3) =   1./obj%xwidth
           obj%sr2eg(3,4) =   0.
           obj%sr2eg(4,1) =   0.
           obj%sr2eg(4,2) =   0.
           obj%sr2eg(4,3) = - 0.
           obj%sr2eg(4,4) =   1./obj%ywidth
           obj%target_slope = 1.

!Manual mode not operational

      elseif (obj%intype(1:1) == 'M' ) then

           obj%intype = 'MAN'

           call pc_info('Manual mode selected in DMAP3D')
           call pc_error('Manual mode not operational')

      else
           call pc_error('mode must be COG,CSG,CRG, or MAN')
      end if

!transform matrix from m-h to e-g

      obj%mh2eg(1,1) = obj%sr2eg(1,1) + obj%sr2eg(1,3)
      obj%mh2eg(1,2) = obj%sr2eg(1,2) + obj%sr2eg(1,4)
      obj%mh2eg(2,1) = obj%sr2eg(2,1) + obj%sr2eg(2,3)
      obj%mh2eg(2,2) = obj%sr2eg(2,2) + obj%sr2eg(2,4)
      obj%mh2eg(1,3) = obj%sr2eg(1,1) - obj%sr2eg(1,3)
      obj%mh2eg(1,4) = obj%sr2eg(1,2) - obj%sr2eg(1,4)
      obj%mh2eg(2,3) = obj%sr2eg(2,1) - obj%sr2eg(2,3)
      obj%mh2eg(2,4) = obj%sr2eg(2,2) - obj%sr2eg(2,4)
      obj%mh2eg(3,1) = obj%sr2eg(3,1) + obj%sr2eg(3,3)
      obj%mh2eg(3,2) = obj%sr2eg(3,2) + obj%sr2eg(3,4)
      obj%mh2eg(4,1) = obj%sr2eg(4,1) + obj%sr2eg(4,3)
      obj%mh2eg(4,2) = obj%sr2eg(4,2) + obj%sr2eg(4,4)
      obj%mh2eg(3,3) = obj%sr2eg(3,1) - obj%sr2eg(3,3)
      obj%mh2eg(3,4) = obj%sr2eg(3,2) - obj%sr2eg(3,4)
      obj%mh2eg(4,3) = obj%sr2eg(4,1) - obj%sr2eg(4,3)
      obj%mh2eg(4,4) = obj%sr2eg(4,2) - obj%sr2eg(4,4)

!transform matrix from e-g to s-r

      obj%eg2sr = obj%sr2eg
      call DMAP3D_util_inv4(obj%eg2sr, Istatus)

      if (Istatus.ne.0) then
           call pc_error('transform matrix sr2eg is singular')
      end if

!transform matrix from e-g to m-h

      obj%eg2mh = obj%mh2eg
      call DMAP3D_util_inv4(obj%eg2mh, Istatus)

      if (Istatus.ne.0) then
           call pc_error('transform matrix mh2eg is singular')
      end if

!--------- Output Gather Transform Matrix

      !Make sure outtype is uppper case
      call string_to_upper(obj%outype)

!Build sr2ego and store in ego2sr

      if (obj%outype(1:2) == 'CO' ) then

           obj%outype = 'COG'

           ego2sr(1,1) =  .5 / obj%xwidth
           ego2sr(1,2) =   0.
           ego2sr(1,3) =  .5 / obj%xwidth
           ego2sr(1,4) =   0.
           ego2sr(2,1) = - 0.
           ego2sr(2,2) =  .5 / obj%ywidth
           ego2sr(2,3) = - 0.
           ego2sr(2,4) =  .5 / obj%ywidth
           ego2sr(3,1) =  .5 / obj%xwidth
           ego2sr(3,2) =   0.
           ego2sr(3,3) = -.5 / obj%xwidth
           ego2sr(3,4) = - 0.
           ego2sr(4,1) = - 0.
           ego2sr(4,2) =  .5 / obj%ywidth
           ego2sr(4,3) =   0.
           ego2sr(4,4) = -.5 / obj%ywidth

!transform matrix for Azimuth Gathers

      elseif (obj%outype(1:2) == 'CA' ) then

           obj%outype = 'CAZ'

           ego2sr(1,1) =  .5 / obj%xwidth
           ego2sr(1,2) =   0.
           ego2sr(1,3) =  .5 / obj%xwidth
           ego2sr(1,4) =   0.
           ego2sr(2,1) = - 0.
           ego2sr(2,2) =  .5 / obj%ywidth
           ego2sr(2,3) = - 0.
           ego2sr(2,4) =  .5 / obj%ywidth
           ego2sr(3,1) =  .5 / obj%xwidth
           ego2sr(3,2) =   0.
           ego2sr(3,3) = -.5 / obj%xwidth
           ego2sr(3,4) = - 0.
           ego2sr(4,1) = - 0.
           ego2sr(4,2) =  .5 / obj%ywidth
           ego2sr(4,3) =   0.
           ego2sr(4,4) = -.5 / obj%ywidth

!Transform matrix for source gathers
      elseif(obj%outype(1:2) == 'CS' ) then

           obj%outype = 'CSG'

           ego2sr(1,1) = - 1. / obj%xwidth
           ego2sr(1,2) = - 0.
           ego2sr(1,3) =   1. / obj%xwidth
           ego2sr(1,4) =   0.

           ego2sr(2,1) =   0.
           ego2sr(2,2) = - 1. / obj%ywidth
           ego2sr(2,3) = - 0.
           ego2sr(2,4) =   1. / obj%ywidth

           ego2sr(3,1) =   1. / obj%xwidth
           ego2sr(3,2) =   0.
           ego2sr(3,3) =   0.
           ego2sr(3,4) =   0.

           ego2sr(4,1) = - 0.
           ego2sr(4,2) =   1. / obj%ywidth
           ego2sr(4,3) =   0.
           ego2sr(4,4) =   0.

!Transform matrix for receiver gathers

      elseif (obj%outype(1:2) == 'CR'  &
         .or. obj%outype(1:2) == 'CG' ) then

           obj%outype = 'CRG'

           ego2sr(1,1) =   1. / obj%xwidth
           ego2sr(1,2) =   0.
           ego2sr(1,3) = - 1. / obj%xwidth
           ego2sr(1,4) = - 0.

           ego2sr(2,1) = - 0.
           ego2sr(2,2) =   1. / obj%ywidth
           ego2sr(2,3) =   0.
           ego2sr(2,4) = - 1. / obj%ywidth

           ego2sr(3,1) =   0.
           ego2sr(3,2) =   0.
           ego2sr(3,3) =   1. / obj%xwidth
           ego2sr(3,4) =   0.

           ego2sr(4,1) =   0.
           ego2sr(4,2) =   0.
           ego2sr(4,3) = - 0.
           ego2sr(4,4) =   1. / obj%ywidth

!Manual mode not currently supported

      elseif (obj%outype(1:1) == 'M' ) then

           obj%outype = 'MAN'

           call pc_info('Manual mode selected in DMAP3D')
           call pc_error('Manual mode not operational')

      else
           call pc_error('mode must be COG,CMP,CSG,CRG, or MAN')
      end if

!invert sr2ego to get ego2sr

      call DMAP3D_util_inv4(ego2sr, Istatus)
      if (Istatus /= 0) then
           call pc_error('transform matrix sr2eg is singular')
      end if

      !multiply obj%sr2eg by ego2sr to get ego2egi
      obj%ego2egi = MatMul(obj%sr2eg,ego2sr)

      if(obj%mycpu==obj%rootcpu) then
         write(iupr,'("ego2egi",4f14.4)')obj%ego2egi
      end if
      ier = 0

!---------- Allocate stored arrays --------------------------------------

      allocate(obj%Din  (obj%ndpt,obj%Ntim),     stat=ier(18))
      allocate(obj%HDin (obj%nwih,obj%MNTOD),    stat=ier(19))
      
      allocate(obj%in_disk  (obj%MNTOD),         stat=ier(20))
      allocate(obj%in_mem   (obj%MNTOD),         stat=ier(21))
      allocate(obj%loc_mem  (obj%Ntim),          stat=ier(22))
      allocate(obj%amp_store(obj%MNTOD),         stat=ier(23))
              
      allocate(obj%din_tmp (obj%ndpt,3),         stat=ier(1))
      allocate(obj%hd_tmp  (obj%nwih,3),         stat=ier(2))      
      allocate(obj%dout    (obj%ndpt,2),         stat=ier(3))      

      allocate(obj%Sloc (2,obj%MNTOD       ),    stat=ier(4))
      allocate(obj%Rloc (2,obj%MNTOD       ),    stat=ier(5))
      allocate(obj%H    (2,obj%MNTOD       ),    stat=ier(6))
      allocate(obj%X    (2,obj%MNTOD       ),    stat=ier(7))
      allocate(obj%Gbin (  obj%MNTOD       ),    stat=ier(8))

      allocate(obj%Hm   (  obj%MNTOD       ),    stat=ier(9))
      allocate(obj%Tm   (  obj%MNTOD       ),    stat=ier(10))
      allocate(obj%Mask (  obj%MNTOD       ),    stat=ier(11))
      allocate(obj%Same (  obj%MNTOD       ),    stat=ier(12))
      allocate(obj%Nnn  (  obj%MNTOD       ),    stat=ier(13))
      allocate(obj%In   (  obj%NNmax,obj%MNTOD),   stat=ier(14))
      allocate(obj%Dx   (2,obj%NNmax,obj%MNTOD), stat=ier(15))
      allocate(obj%Vc   (2,obj%NNmax,obj%MNTOD), stat=ier(16))
      allocate(obj%negsc(4,2,0:obj%numcpu-1),    stat=ier(17))

      if (Sum(ier) /= 0) then
           call pc_error('DMAP3D Storage Allocation Failed')
      end if
 
      obj%Sloc  = 0
      obj%Rloc  = 0
      obj%H     = 0
      obj%X     = 0
      obj%Gbin  = 0
      obj%Hm    = 0
      obj%Tm    = 0
      obj%Mask  = 0
      obj%Same  = 0
      obj%Nnn   = 0
      obj%In    = 0
      obj%Dx    = 0
      obj%Vc    = 0
      obj%negsc = 0

      !Normalize the velocity here!!
      obj%Vel = obj%Vel * obj%dt * .5

      !Number of available trace slots
      obj%Nav = obj%MNTOD

      !Number of minimum sample to mapping ( default to 120 ms)

      obj%min_samp = .120/obj%dt
      
      !Clear output buffer
      obj%dout = 0.    
      
      ! get the job name

      call pc_get_jdata ('pathname_dir', obj%pathname_dir )
      call pc_get_jdata ( 'jobname',     obj%job_name )
            
      !
      !  get a unique file name, including path, job, ipn and pe
      !

      obj%file_name  = "~/cpstemp/" // trim ( obj%job_name )      
      obj%file_name  = trim ( obj%file_name ) // '_dmap_3d'  
      call exptilde ( obj%file_name)
      !
      !
      !  get a unique file name, including path, job, ipn and pe
      !
      call pp_create_file_name (                            &
                                 base_name = obj%file_name,    &
                                 file_name = obj%file_name,    &
                                 i_pn      = pc_get_ipn(),  &
                                 i_worker  = dmap3d_i_pel() &
                               )
      
      ! add a '.trc' suffix to the image file name
      
      obj%image_fname    = trim (obj%file_name) // '_image.trc'
      obj%storetr_fname  = trim (obj%file_name) // '_storetr.trc'

      write(iupr,*) 'ipe = ',dmap3d_i_pel(), '  file = ', obj%image_fname  

      ! open the image trace file
 
      call dmap3d_open_trace_file ( &
                                    io_obj    = obj%image_obj,       &
                                    file_name =  obj%image_fname,    &
                                    file_stat = 'w+',           &
                                    hd_len    = obj%nwih,       &
                                    tr_len    = obj%ndpt,       &
                                    tr_min    = obj%tstrt,      &
                                    tr_inc    = obj%dt,         &
                                    scratch   = .true.,         & 
                                    i_err     = i_err           &
                                   )                               
                  
      !
      xxif_open_error : if ( i_err .ne. 0 ) then
        !
        call pc_error ( ' error in dmad3d_update during dmap3d_open_trace_file')
        return
        !
      end if xxif_open_error
      !
      ! zero the image trace file
      !
      call dmap3d_zero_trace_file (                            &
                                    io_obj   = obj%image_obj,     &
                                    n_rec    = obj%ntotoutsc,  &
                                    hd_len   = obj%nwih,       &
                                    tr_len   = obj%ndpt,       &
                                    i_err    = i_err           &
                                  )
      
       if ( i_err .ne. 0 ) then
          call pc_error           &
            ( ' error in dmad3d_update during dmap3d_zero_trace_file')
          return              
       end if 
      
      obj%itr_mem = 0
      obj%in_disk = .false.
      obj%in_mem  = -9999
      
      obj%ntr_disk = obj%MNTOD

      if ( obj%MNTOD > obj%ntim) then
        obj%use_disk  = .true.
        obj%in_disk = .true.
      end if        

      !
      ! open the storetr trace file
      !
     
      if (obj%use_disk) then 
        call dmap3d_open_trace_file ( &
                                    io_obj    = obj%storetr_obj,     &
                                    file_name = obj%storetr_fname,    &
                                    file_stat = 'w+',           &
                                    hd_len    = obj%nwih,       &
                                    tr_len    = obj%ndpt,       &
                                    tr_min    = obj%tstrt,      &
                                    tr_inc    = obj%dt,         &
                                    scratch   = .true.,         &
                                    i_err     = i_err           &
                                   )
      
        write(iupr,*) 'ipe = ',dmap3d_i_pel(), '  file = ', obj%storetr_fname

        if ( i_err .ne. 0 ) then          
          call pc_error  &
              ( ' error in dmad3d_update during dmap3d_open_trace_file')
          return        
        end if 

      end if
      
      !Find the starting locations of all subcubes and store in obj%negsc
      Call DMAP3D_FindAllHypCubLoc(obj)
    
!</execute_only>


!!----------------------- finish update ----------------------------------!!
!!----------------------- finish update ----------------------------------!!
!!----------------------- finish update ----------------------------------!!

      return
      end subroutine DMAP3D_update


!!----------------------------- traps ------------------------------------!!
!!----------------------------- traps ------------------------------------!!
!!----------------------------- traps ------------------------------------!!


   !------- ityp trap -----------------------------------------------------
      subroutine DMAP3D_ityp_trap(keyword)
      implicit none
      character(len=*),intent(in)      :: keyword
      real      :: deg_to_rad = .0174533
      integer is

      is = 0

      call string_to_upper(object%intype)

      if    (object%intype(1:2) == 'CO' ) then
             object%intype = 'COG'

      elseif(object%intype(1:2) == 'CS' ) then
           object%intype = 'CSG'

      elseif(object%intype(1:2) == 'CR'  &
          .or. object%intype(1:2) == 'CG' ) then
           object%intype = 'CRG'

      elseif(object%intype(1:1) == 'M' ) then
           object%intype = 'MAN'
           call pc_info('Manual mode selected in DMAP3D')
           call pc_error('Manual mode not operational')
      else
           call pc_error('mode must be COG,CSG,CRG, or MAN')
      end if
      end subroutine DMAP3D_ityp_trap

  !------- otyp trap -----------------------------------------------------
      Subroutine DMAP3D_otyp_trap(keyword)
      implicit none
      character(len=*),intent(in)      :: keyword
      real      :: deg_to_rad = .0174533
      integer is

      is = 0

      call string_to_upper(object%outype)

      if     (object%outype(1:2) == 'CO' ) then
             object%outype = 'COG'

      elseif (object%outype(1:2) == 'CS' ) then
           object%outype = 'CSG'

      elseif (object%outype(1:2) == 'CR'  &
          .or. object%outype(1:2) == 'CG' ) then
           object%outype = 'CRG'

      elseif (object%outype(1:2) == 'CA') then
           object%outype = 'CAZ'

      elseif (object%outype(1:1) == 'M' ) then
           object%outype = 'MAN'
           call pc_info('Manual mode selected in DMAP3D')
           call pc_error('Manual mode not operational')
      else
           call pc_error('mode must be COG,CSG,CRG, or MAN')
      end if
      end subroutine DMAP3D_otyp_trap

  !------- ne1 trap -----------------------------------------------------
      subroutine DMAP3D_NE1_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%NE1 < 1) then
           object%NE1 = 1
           call pc_error('Number of traces per gather must be > 0')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_NE1_trap

  !------- ne2 trap -----------------------------------------------------
      subroutine DMAP3D_NE2_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%NE2 < 1) then
           object%NE2 = 1
           call pc_error('Number of traces per gather must be > 0')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_NE2_trap

  !------- ng1 trap -----------------------------------------------------
      subroutine DMAP3D_NG1_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%NG1 < 1) then
           object%NG1 = 1
           call pc_error('Number gathers must be > 0')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_NG1_trap

   !------- ng2 trap -----------------------------------------------------
      subroutine DMAP3D_NG2_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%NG2 < 1) then
           object%NG2 = 1
           call pc_error('Number gathers must be > 0')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_NG2_trap

   !------- de1  trap -----------------------------------------------------
      subroutine DMAP3D_DE1_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%DE1 == 0.) then
           call pc_error('Output trace spacing cannot be zero')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_DE1_trap

   !------- de2  trap -----------------------------------------------------
      subroutine DMAP3D_DE2_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%DE2 == 0.) then
           call pc_error('Output trace spacing cannot be zero')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_DE2_trap

   !------- dg1  trap -----------------------------------------------------
      subroutine DMAP3D_DG1_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%DG1 == 0.) then
           call pc_error('Output gather spacing cannot be zero')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_DG1_trap

   !------- dg2  trap -----------------------------------------------------
      subroutine DMAP3D_DG2_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%DG2 == 0.) then
           call pc_error('Output gather spacing cannot be zero')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_DG2_trap

   !------- vel  trap -----------------------------------------------------
      subroutine DMAP3D_vel_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Vel <= 0.) then
           call pc_error('Velocity must be positive')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_vel_trap

   !------- MNTODtrap -----------------------------------------------------
      subroutine DMAP3D_MNTOD_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword
      integer    :: Ntot_out 
      integer    :: num_cpus
      integer    :: grid_size
      integer    :: npadd
      integer    :: ntr_need
                    
      call pc_get_jdata ( 'num_cpus', num_cpus )
      Ntot_out = object%NE1*object%NE2*object%NG1*object%NG2/num_cpus
      grid_size = sqrt(1.0*Ntot_out)+1
      npadd = object%out_offset/min(object%xwidth,object%ywidth)
      ntr_need = 2*((grid_size+2*npadd+8)**2)
      
      If (object%MNTOD < ntr_need) then
           call pc_info('MNTOD may be too small, estimated MNTOD = ', &
                         ntr_need, ',   MNTOD = ', object%MNTOD)
           call pc_info('num_cpus= ', num_cpus)

           write(iupr,*) 'Input trace buffer (MNTOD)may be too small'
           write(iupr,*)'object%MNTOD,num_cpus,ntr_need ', &
             object%MNTOD,num_cpus,ntr_need

           if (object%MNTOD < 1)     &
              call pc_error( 'MNTOD must be > 0')  
      end if

      end subroutine DMAP3D_MNTOD_trap

   !------- nnmx trap -----------------------------------------------------
      subroutine DMAP3D_nnmx_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Nnmax < 9) then
           object%Nnmax = 12
           call pc_error('Max Number of Neighbors too small')
           call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_nnmx_trap

   !------- vcmx trap -----------------------------------------------------
      subroutine DMAP3D_vcmx_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Vcmax < 1.) then
         object%Nnmax = 1.
         call pc_error('Max Voronoi Cell Size Too Small: must be > = 1')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_vcmx_trap

   !------- itmn trap -----------------------------------------------------
      subroutine DMAP3D_itmn_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Itmin <= 0) then
         call pc_error('Minimum time value cannot be negative')
         call pc_jump_field (keyword)
      elseif(object%Itmin < 10) then
         call pc_info('Minimum time value sure is small')
      end if
      end subroutine DMAP3D_itmn_trap

   !------- ttrm trap -----------------------------------------------------
      subroutine DMAP3D_ttrm_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Ttrmax <= 1.) then
         call pc_error('Maximum traveltime ratio must be > 1')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_ttrm_trap


   !------- nsub trap -----------------------------------------------------
      subroutine DMAP3D_nsub_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Nsubt < 1) then
         call pc_error('Subinterval stack size must be >= 1')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_nsub_trap

   !------- tpra trap -----------------------------------------------------
      subroutine DMAP3D_tpra_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Tprang <= 0. .or. object%tprang >= 90.) then
         call pc_error('Taper angle must be between 0 and 90')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_tpra_trap

  !------- afma trap -----------------------------------------------------
      subroutine DMAP3D_afma_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Afmax < 1.) then
         call pc_error('Maximum amplitude factor must be >= 1')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_afma_trap

  !------- ebin trap -----------------------------------------------------
      subroutine DMAP3D_ebin_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Einbin <= 0. ) then
         call pc_error('Element bin size must be positive')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_ebin_trap

!------- ihdr trap -----------------------------------------------------
      subroutine DMAP3D_ihdr_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%inhdr <= 0 .or. object%inhdr>64 ) then
         call pc_error('Inhdr must be a header word number')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_ihdr_trap


  !------- lfmx trap -----------------------------------------------------
      subroutine DMAP3D_lfmx_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Lfmax <1) then
         call pc_error('antialias filter length must be at least 1')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_lfmx_trap


  !------- wfsz trap -----------------------------------------------------
      subroutine DMAP3D_wfsz_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Wfsize < 10) then
         call pc_error('Weight factor array size must be at least 10')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_wfsz_trap

  !------- hmin trap -----------------------------------------------------
      subroutine DMAP3D_hmin_trap (keyword)
      implicit none
      character(len=*),intent(in)   :: keyword

      If (object%Hmin <= 0.) then
         call pc_error('minimum offset separation must be > 0.')
         call pc_jump_field (keyword)
      end if
      end subroutine DMAP3D_hmin_trap


!!------------------------- main execution -------------------------------!!
!!------------------------- main execution -------------------------------!!
!!------------------------- main execution -------------------------------!!


!<execute_only>

      Subroutine DMAP3D (obj,Ntr,Hd,tr)

      implicit none
      type(DMAP3D_struct),intent(inout)       :: obj        ! arguments
      integer         ,intent(inout)       :: Ntr        ! arguments
      double precision,intent(inout)       :: Hd(:,:)    ! arguments
      real            ,intent(inout)       :: tr(:,:)    ! arguments

      integer     :: Istatus       ! local
      integer     :: Ntrm          ! local
      integer     :: itr           ! local



      !Counters include
      !obj%ntrin  -- total number of traces input
      !obj%ntrib  -- number of input traces in buffer
      !obj%ntrout -- total number of traces output
      !obj%nin    -- number of input traces this pass
      !obj%nav    -- space available in input buffer

!-------------------------Begin execution----------------------------------

      if ( ntr==0 ) then
         write(iupr,*) ' end_of_data1 ', pcpsx_i_pel(),obj%mycpu,obj%iflow, &
           obj%ntrin, obj%ntrib, obj%nav
     end if
 
!     Set error status indicator
      Istatus = 0

!      if ( Istatus == 0) stop    

!     Find size of trace and header arrays
      Ntrm = Size(tr,2)

!     This Do Loop enables more than one task to be performed per call
      Do

!        Print where things stand this cycle
!         if(obj%iflow>1) then

         if(obj%iflow == 1) then
           if ( mod(obj%ntrin,nskip*10) == 1) then  
             write(iupr,'(3(a8,i8),2(a6,i8),a8,2f12.2)') &
              " Ntrin= ",obj%ntrin," Ntrout= ", &
              obj%Ntrout," Iflow= ",obj%Iflow," Ntr= ",Ntr," Nav= ",obj%Nav, &
              " h7&8= ", Hd(7:8,ntr)
           end if 
          end if

!         Iflow directs which task to perform this cycle
      Select Case (obj%Iflow)


!   ------------------------   Case Iflow = 1 -------------------------
!   Input Mode:  Reads traces into input buffer until buffer is full or
!      until all traces are read, then shifts to trace loc comp mode

     Case (1)

!        store number of input traces
        obj%Nin = Ntr
        itr = 1

!        Check whether all traces are input
        If(obj%Nin.le.0) then

            Write(iupr,'("Input Midpoint Range",2f12.0," to",2f12.0)')  &
                   obj%xinmin,obj%xinmax

            If((obj%Nav == obj%MNTOD .or. obj%ntrib <= 0) .and. &
                obj%numcpu == 1) then

               !Input buffer is empty.  Switch to wrapup mode
               obj%iflow = 7
               write(iupr,'(a)')"No traces within aperture.  Wrapping up."
            Else

               !Traces to process.  Set mode to Comp
               obj%iflow = 2

               write(iupr,'(a4,i4,a,i6)')"cpu ",obj%mycpu,      &
                            " All traces are input. # accepted= ",obj%ntrib

            End If

        !Inactivate root cpu
        !Elseif (obj%numcpu > 1 .and. obj%mycpu == obj%rootcpu) then
            !return

        Else

            !Add new traces to input buffer
            Call DMAP3D_Addin( obj, hd, tr, Istatus )

!           Error return
            if(Istatus.ne.0) then
               write(iupr,*)" Error on trace input"

               write(*,*)" Error on input (Addin)- iflow, ntr,Istatus ", &
                   obj%Iflow, ntr,Istatus

               Ntr = FATAL_ERROR
               exit
            end if

!           Increment Trace Counters
            obj%Ntrin = obj%Ntrin + obj%Nin

            !Go get more traces
            Ntr = NEED_TRACES
            exit

        End If

!   ----------------------- Case Iflow = 2 ----------------------------
!     Trace location comp mode:  Computes a Voronoi diagram,
!     then shifts to data comp mode

     Case(2)

        Call DMAP3D_getNN(obj, Istatus)

        If(Istatus .ne. 0) Then
            write(iupr,*) "Problem generating Voronoi diagram"
            Ntr = FATAL_ERROR
            exit
        End If

        obj%Iflow = 3
     
        Call DMAP3D_med_amp(obj, Istatus)

!   -------------------------- Case Iflow = 3 ------------------------
!     Computation Mode -- Computes an output trace from an input gather

     Case(3)

!        Compute an Output Trace

        itr = 0
        obj%dout  = 0.

        Call DMAP3D_mkout(obj,itr)

!        Set mode to Output
        obj%Iflow = 6

!  --------------------------  Case Iflow = 4 ------------------------
!    Compute next output trace location.

     Case(4)

        !next step will be Comp mode
        obj%Iflow = 3

!       Increment the gather element counter
        obj%Isc(1) = obj%Isc(1) + 1

        If(obj%Isc(1).ge.obj%NE1sc) then

            obj%Isc(1) = 0
            obj%Isc(2) = obj%Isc(2) + 1

!           An output gather is completed
            If(obj%Isc(2).ge.obj%NE2sc) Then

!              Reset the gather element counter and inc 1st gath counter
               obj%Isc(2) = 0
               obj%Isc(3) = obj%Isc(3) + 1

               if(obj%Isc(3).ge.obj%NG1sc) then

                   obj%Isc(3) = 0
                   obj%Isc(4) = obj%Isc(4) + 1

!                  If all gathers are output, clear gather, etc
                   If(obj%Isc(4).ge.obj%NG2sc) Then

                      obj%Isc(4) = 0
                      Ntr = 1

                   !End gather if-blocks
                   End If
               End If

            !End Gather Element If-Blocks
            End If
        End If
!
!     ------------------------ Case Iflow = 5 --------------------------
!     Collection Mode

     Case(5)

! the new version cycles over global values, ntrout
        xxif_flush_trace : if(obj%ntrout < obj%ntotout) then


            !Put out an output trace
            ntr = 1
            Call DMAP3D_flush(obj,hd,tr,1,ntr,Istatus)

!!            write(*,*) ' dmap3d_flush ',obj%ntrout, obj%ntotout, hd(33:36,1)

             !Error return
            If(Istatus .ne. 0) Then

!                 write(iupr,*)" Problem in flush."
!                 ntr = FATAL_ERROR
!                 exit

               ntr = 1
               write(*,*) ' dmap3d_flush_problem ',obj%ntrout,  &
                            obj%ntotout, hd(33:36,1)

            End if

            ! check the global trace counter, ntrout
            ! to decide if we need to output more traces
            if (obj%ntrout < obj%ntotout) then
               itr       = 0        !reset output trace counter
               obj%dout  = 0.       !clear output trace buffer
               obj%iflow = 5        !output the next trace
            else
               write(iupr,'(a,3i8)')" Exiting 1 ",obj%ntroutsc,   &
                                    obj%ntotoutsc,obj%mycpu
               obj%iflow = 7        !finished

               Istatus = 0
               call pcpsx_sync_workers(Istatus)   
               If(Istatus .ne. 0) Then
                  ! print *, " Problem in pcpsx_sync_workers."
                  write(iupr,*) " Problem in pcpsx_sync_workers."
               end if

            end if

            Return

        else xxif_flush_trace
            write(iupr,'(a,3i8)')" Exiting 2 ",obj%ntroutsc,   &
                                  obj%ntotoutsc,obj%mycpu
            obj%iflow = 7

        end if xxif_flush_trace

!     ------------------------ Case Iflow = 6 --------------------------
!     Output Mode

     Case(6)

        if(obj%ntroutsc < obj%ntotoutsc) then

            !Put out an output trace
            ntr = 1
            Call DMAP3D_putout(obj,hd,tr,1,ntr,Istatus)

             !Error return
            If(Istatus .ne. 0) Then
               write(iupr,*)" Problem in putout."
               ntr = FATAL_ERROR
               exit
            End if

            if (obj%ntroutsc < obj%ntotoutsc) then
               itr       = 0        !reset output trace counter
               obj%dout  = 0.       !clear output trace buffer
               obj%iflow = 4        !Update output trace mode
            else

! all image traces have been constructed
! reset the local, ntroutsc, and global, ntrout, trace counters to 0
! and set the trace flow flag, iflow, to 5 to flush the traces from disk

               write(iupr,'(a,3i8)')" Exiting 3 ",obj%ntroutsc,   &
                                    obj%ntotoutsc,obj%mycpu
               obj%iflow = 5        ! flush traces
               obj%isc      = 0     ! count each direction
               obj%ntroutsc = 0     ! local output trace counter
               obj%ntrout   = 0     ! global output trace counter

               !print *, ' exiting 3 - computation done  cpu =  ', obj%mycpu
               write(iupr,*) ' exiting 3 - computation done  cpu =  ', obj%mycpu

               Istatus = 0
               call pcpsx_sync_workers(Istatus)    
               If(Istatus .ne. 0) Then
                  ! print *, " Problem in pcpsx_sync_workers."
                  write(iupr,*) " Problem in pcpsx_sync_workers."
               end if

            end if

!do not return here, construct the next output trace
! and put it to the image disk file
!dwh            Return

        else
            write(iupr,'(a,3i8)')" Exiting 4 ",obj%ntroutsc,   &
                                 obj%ntotoutsc,obj%mycpu
            obj%iflow = 7

        end if

!   ------------------------ Case Iflow = 7 --------------------------
!     Wrapup Mode

     Case(7)

        Ntr = NO_MORE_TRACES
        exit

     Case Default

        ntr = FATAL_ERROR
        exit

     End Select

     End Do

     If(Ntr == NO_MORE_TRACES .or. Ntr == FATAL_ERROR) then
         call DMAP3D_wrapup (obj)         
         write(*,*) ' end_of_data2 ', pcpsx_i_pel(),obj%iflow, obj%ntrin, ntr 
     Endif
     
     return
     
     end subroutine DMAP3D


!!--------------------------- DMAP3D addin -----------------------------------!!
!!--------------------------- DMAP3D addin -----------------------------------!!
!!--------------------------- DMAP3D addin -----------------------------------!!


! **************************************************************************
!
!     DMAP3DAddin -- Discards unneeded traces from input buffer and adds
!    new ones
!


!     Passed Parameters -- headers and traces
!  hd     -- input trace headers
!  trc     -- input traces
!
!     Passed Parameters -- obj elements
!  Nin     -- Number of traces ready to input
!  Nwih     -- Header length
!  Sloc,Rloc  -- Source and Receiver Locs
!  G,E     -- Gather and Gather Element Coords
!  Gbin     -- Gather number
!  Hm,Tm     -- Head and Tail Mutes
!  Din     -- Input trace buffer
!  Ndpt     -- Buffer trace length
!  Nav     -- Available slots in input buffer
!  Mask     -- Array of slot indicator values.
!         Mask(in)=0 if in'th slot in buffer is available
!  Ntrib     -- Number of traces in current input gather
!  MNTOD     -- Number of slots in input buffer
!
!     Other passed parameters
!  Istatus    -- Status Indicator.  If routine decides that input
!   buffer is too small, it is set to 1
!
!     Local Parameters
!  Jn     -- Current trace number in input buffer
!  in     -- Current input trace number
!  k     -- Index within current trace
!  xsi,ysi    -- Current source coordinate
!  xri,yri    -- Current receiver coordinate
!  nb1,nb2    -- Bin numbers for current gather
!  Gn1,Gn2    -- G value of new set of traces
!  num_cpu    -- number of cpus working on this job
!  my_cpu     -- number of the current cpu.
!
! **********************************************************************

      Subroutine  DMAP3D_Addin( obj,hd,trc, Istatus )

!      Use Global
      Implicit None

!     Passed Parameters
      type (DMAP3D_struct) obj
      real trc(:,:)
      double precision hd(:,:)

      integer Istatus

!     Local Parameters
      Real xsi, ysi, xri, yri
      Real gn1, gn2
      Real tdmaxi, tdmaxo
      Integer In, jn,    nb1

      real   :: sdist, rdist

!     Set Status
      Istatus = 0

!     Test for new gather
      xsi =  hd(33,1) * obj%xwidth
      ysi =  hd(34,1) * obj%ywidth
      xri =  hd(35,1) * obj%xwidth
      yri =  hd(36,1) * obj%ywidth

!      nb1 = nint(hd(obj%inhdr,1))

!    Hardwire to a constant
      nb1 = 1

      If(obj%Ntrin == 0) then

        !First gather, first group -- define gather
        obj%Ngin1 = nb1
        obj%Ngin  = 0

        write(iupr,*)"HANDLING FIRST GATHER!!"
        obj%xinmin(1) = .5 * (xsi + xri)
        obj%xinmin(2) = .5 * (ysi + yri)
        obj%xinmax = obj%xinmin
        write(iupr,'(2(a4,2f12.1),i8)')"s= ", xsi, ysi," r= ",xri,yri, nb1
        write(iupr,'(a,4f12.0)')"field coords= ",hd(33:34,1),hd(35:36,1)
  !      write(iupr,'(a,4f12.0)')"field coords= ",hd(11:12,1),hd(14:15,1)

      Elseif(nb1 .ne. obj%Ngin1 ) then

        write(iupr,'(2(a16,2f12.1),2i8)')"new gather. s= ", &
          xsi/obj%xwidth, ysi/obj%ywidth, &
          " r= ",xri/obj%xwidth,yri/obj%ywidth, nb1,obj%ntrin

        !New gather
        obj%Ngin1 = nb1
        obj%Ngin  = obj%Ngin + 1

      End if

      !If no space is available, the buffer is too small
      If (obj%Nav.le.obj%Nin) then
      
        write(iupr,*)' ******************************************** '
        write(iupr,'(//,a)') " MNTOD is Too Small; increase MNTOD."
        write(iupr,*)' DMAP3D: cpu,Nav,MNTOD ', pcpsx_i_pel(),obj%Nav,obj%MNTOD 
        write(iupr,'(//,a)') ' '    
        write(iupr,*)' ******************************************** '
        Istatus = 1
        Return
      End If

!     Place New Traces in buffer
      tdmaxi = 0.
      tdmaxo = 0.
      Do in = 1, obj%Nin
        jn = obj%MNTOD - obj%Nav + 1

        !get source and receiver loc from field coords (inactive)
        !obj%Sloc(1,jn) =  hd(11,in)
        !obj%Sloc(2,jn) =  hd(12,in)!
        !obj%Rloc(1,jn) =  hd(14,in)
        !obj%Rloc(2,jn) =  hd(15,in)

        !get source and receiver loc from grid coords
        obj%sloc(1,jn) =  hd(33,in)*obj%xwidth
        obj%sloc(2,jn) =  hd(34,in)*obj%ywidth
        obj%rloc(1,jn) =  hd(35,in)*obj%xwidth
        obj%rloc(2,jn) =  hd(36,in)*obj%ywidth
!
        gn1  =   obj%sr2eg(3,1)*obj%Sloc(1,jn) +  &
                 obj%sr2eg(3,2)*obj%Sloc(2,jn) +  &
                 obj%sr2eg(3,3)*obj%Rloc(1,jn) +  &
                 obj%sr2eg(3,4)*obj%Rloc(2,jn)
        gn2  =   obj%sr2eg(4,1)*obj%Sloc(1,jn) +  &
                 obj%sr2eg(4,2)*obj%Sloc(2,jn) +  &
                 obj%sr2eg(4,3)*obj%Rloc(1,jn) +  &
                 obj%sr2eg(4,4)*obj%Rloc(2,jn)
!
        !Use midpoint as the Voroni coordinate

        sdist = obj%Sloc(1,jn)**2 + obj%Sloc(2,jn)**2
        rdist = obj%rloc(1,jn)**2 + obj%rloc(2,jn)**2

        obj%X(1:2,jn) = .5*(obj%Sloc(1:2,jn)+obj%Rloc(1:2,jn))
        obj%H(1:2,jn) = .5*(obj%Sloc(1:2,jn)-obj%Rloc(1:2,jn))

        !update min, max input midpoint
        obj%xinmin(1) = Min(obj%xinmin(1),obj%x(1,jn))
        obj%xinmin(2) = Min(obj%xinmin(2),obj%x(2,jn))
        obj%xinmax(1) = Max(obj%xinmax(1),obj%x(1,jn))
        obj%xinmax(2) = Max(obj%xinmax(2),obj%x(2,jn))

        obj%HM(jn) =  hd( 2,in)        !Head Mute
        obj%Tm(jn) =  hd(64,in)        !Tail Mute


        if (.not. DMAP3D_Addin_Accept(obj,obj%x(1:2,jn),obj%h(1:2,jn))) cycle

!     write(iupr,*) ' current subc ', obj%mycpu, obj%Nin, obj%nav, obj%ntrib,  &
!               hd(7,in), hd(8,in)

      !  Transfer trace and header

!!        obj%Din(1:obj%ndpt,jn) = Trc(1:obj%ndpt,in)  

        obj%HDin(1:obj%nwih,jn) = Hd(1:obj%nwih,in)   

      call dmap3d_put_trace (obj, jn, hd(:,in:), trc(:,in:), Istatus)
              
      If (Istatus .ne. 0) then
        write(iupr,'(a, 3i7)')' Dmap3d_put_trace has problem, index = ', &
                                pcpsx_i_pel(),jn,in  
        Return
      End If

      if ( jn > obj%MNTOD ) then
         write(iupr,*) ' jn > obj%MNTOD in routine ADD_IN '    !  
         write(iupr,*) ' jn > obj%MNTOD in routine ADD_IN '    ! 
         Return
      end if 

        !set mask to "occupied"
        obj%mask(jn) = -1

        !set the gather id for this trace
        obj%gbin(jn) = obj%ngin + 1

        !calculate maximum value of input trace
 !       dmaxi = 0.
 !        do i = 1, obj%ndpt
 !          dmaxi = Max(dmaxi,abs(trc(i,in)))
 !       end do
 !       tdmaxi = Max(tdmaxi,dmaxi)      !maximum input value
     
        tdmaxi =  hd(25,in)

        !Print properties of input trace
        if(obj%pritr(1:1)=="Y") then
           if(mod(jn-1,nskip)==0) then
              write(iupr,'(4x,a4,4x,a4,8x,a1,11x,a1,2(10x,a2),8x,a5)')  &
                 "ntrin","indi","x","y","hx","hy","maxin"
           end if
           write(iupr,'(2i8,4f12.1,e12.4)')  &
                obj%ntrin,jn,obj%x(1:2,jn),obj%h(1:2,jn),tdmaxi
        end if

      !  decrement number of available slots
        obj%Nav = obj%Nav - 1

        !increment the slot counter
        obj%ntrib = obj%ntrib + 1

      End Do

      End Subroutine DMAP3D_Addin


!!--------------------------- DMAP3D addin accept ----------------------------!!
!!--------------------------- DMAP3D addin accept ----------------------------!!
!!--------------------------- DMAP3D addin accept ----------------------------!!


!******************************************************************************
!
!     Screen input traces.  Reject traces outside the aperture of all output
!     traces.
!     For Constant-Offset Gathers, the screening process is simple.
!     Given an input and an output offset, the mapping aperture is
!     the parallelogram with vertices hi+hf,hi-hf,-hi-hf,-hi+hf.
!     The rectangular box enclosing this parallelogram is tested
!     against the box of output traces.  If the boxes overlap, the
!     trace is accepted.  This does result in some traces being accepted that
!     are ultimately unused.
!     For other gather types, input traces are compared against each output
!     trace individually in a much slower process.  If it proves a bottleneck,
!     no doubt a faster process analogous to the constant-offset one could be
!     implemented.
!
!******************************************************************************

   Logical Function DMAP3D_Addin_Accept(obj,xi,hi)

   Implicit None
   Type (DMAP3D_struct)    :: obj
   Real, Intent(IN)    :: xi(2)       !midpoint of trace to test
   Real, Intent(IN)    :: hi(2)         !offset of trace to test

   real      :: e1l, e2l, g1l, g2l
   real      :: g1g, g2g
   real      :: ego(4), egoi(4), mh(4), mhi(4), egi(4)
   real      :: de1, de2
   integer     :: ie1, ie2, ig1, ig2   
   integer     :: ne1, ne2, ng1, ng2
   real      :: hif, xhi, xhf, dx(2), hf(2), dg(2)
   real      :: dxl(2), dxg(2), xa(2), xb(2), xc(2), xd(2)         
   real      :: dt, xmin(2), xmax(2)



   real      :: e1_last, e2_last
   integer   :: overlap
   integer,save   :: in_tot = 0
   
   integer   :: my_cpu
   
   !executables
   DMAP3D_addin_accept = .false.

   !generalized coordinates of input trace
   mhi(1:2) = xi
   mhi(3:4) = hi
   egi = matmul(obj%mh2eg,mhi)

   overlap = obj%vcmax*3

   !Output trace coordinate ranges
      
   de1 = obj%de1
   e1l = obj%e1isc - overlap*obj%de1    

   de2 = obj%de2
   e2l = obj%e2isc - overlap*obj%de2

   e1l = max( obj%E1ini, e1l)
   e2l = max( obj%E2ini, e2l) 

   g1l = obj%g1isc
   g1g = obj%g1isc + (obj%ng1sc-1)*obj%dg1
   g2l = obj%g2isc
   g2g = obj%g2isc + (obj%ng2sc-1)*obj%dg2

   ne1 = obj%ne1sc + overlap*2
   ne2 = obj%ne2sc + overlap*2
   ng1 = min(obj%ng1sc,2)
   ng2 = min(obj%ng2sc,2)

   my_cpu  = pcpsx_i_pel()

   e1_last = e1l + (ne1-1) * obj%de1 
   e2_last = e2l + (ne2-1) * obj%de2
   in_tot = in_tot + 1


   !Constant offset output
   if(obj%outype(1:2) == 'CO') then

      !set output offsets
      ego(4) = g2l
      Do ig2 = 1, ng2

         ego(3) = g1l
         Do ig1 = 1, ng1

            !lower bound of output rectangle
            ego(1) = e1l
            ego(2) = e2l
            egoi = matmul(obj%ego2egi,ego)
            mh   = matmul(obj%eg2mh,egoi)
            dxl = xi - mh(1:2)
            hf  = mh(3:4)

            !upper bound of output rectangle
            ego(1) = e1l + (ne1-1) * obj%de1
            ego(2) = e2l + (ne2-1) * obj%de2
            egoi = matmul(obj%ego2egi,ego)
            mh   = matmul(obj%eg2mh,egoi)
            dxg = xi - mh(1:2)

  
!!  write(*,*) ' current subc ', obj%mycpu, my_cpu, obj%nav,obj%ntrib,  &
!!               xi,mh(1:2),e1l,e1_last,e2l,e2_last, ne1,ne2 

            !arrange dx(1) in increasing order
            if(dxl(1) > dxg(1)) then
               dt = dxl(1)
               dxl(1) = dxg(1)
               dxg(1) = dt
            end if

            !arrange dx(2) in increasing order
            if(dxl(2) > dxg(2)) then
               dt = dxl(2)
               dxl(2) = dxg(2)
               dxg(2) = dt
            end if

            !find box around aperture parallelogram

            xa = hi + hf       
            xb = hi - hf
            xc = - xa
            xd = - xb

            xmin(1) = min(xa(1),xb(1),xc(1),xd(1))
            xmin(2) = mIN(xa(2),xb(2),xc(2),xd(2))
            xmax(1) = Max(xa(1),xb(1),xc(1),xd(1))
            xmax(2) = Max(xa(2),xb(2),xc(2),xd(2))

 ! ***   testing   reduce the  aperture
 !            xa = hf
 !            xb = hi 
 !           xe = hi - hf       ! **   testing
 !           xf = -xe           ! **   testing
 !           xmin(1) = min(xa(1),xb(1),xc(1),xd(1),xe(1),xf(1))
 !           xmin(2) = min(xa(2),xb(2),xc(2),xd(2),xe(2),xf(2))
 !           xmax(1) = Max(xa(1),xb(1),xc(1),xd(1),xe(1),xf(1))
 !           xmax(2) = Max(xa(2),xb(2),xc(2),xd(2),xe(2),xf(2))
 ! ***   testing

            !Does input box overlap output box?
            if(dxl(1) > xmax(1) .or. dxl(2) > xmax(2) .or.  &
               dxg(1) < xmin(1) .or. dxg(2) < xmin(2) ) then

               !No overlap
               Cycle

            else

               !Input and output boxes overlap.  Accept this input point
               DMAP3D_addin_accept = .true.

               return

            end if

         End Do         !end ig1 goop
         ego(4) = g2g
      End Do         !end ig2 loop

      !to get to here, no output point lies inside aperture
      DMAP3D_addin_accept = .false.
      return

   end if

   !At least one output trace location must lie within aperture
   ego(4) = g2l
   Do ig2 = 1, ng2

      ego(3) = g1l
      Do ig1 = 1, ng1

         ego(2) = e2l - de2
         do ie2 = 1, ne2
           ego(2) = ego(2) + de2
           ego(1) = e1l - de1
           do ie1 = 1, ne1
              ego(1) = ego(1) + de1
              egoi = matmul(obj%ego2egi,ego)

              !first test: distance between input and output gather
              dg = egoi(3:4) - egi(3:4)
              if(dg(1) < obj%Dg1lb .or.  &
                dg(1) > obj%Dg1ub .or.  &
                dg(2) < obj%Dg2lb .or.  &
                dg(2) > obj%Dg2ub ) then
                Cycle
              end if

              !2nd test: is midpoint difference within parallelogram?
              mh  = matmul(obj%eg2mh,egoi)
              hf  = mh(3:4)
              dx  = xi - mh(1:2)

              hif = abs(hi(1)*hf(2) - hi(2)*hf(1))
              xhi = abs(dx(1)*hi(2) - dx(2)*hi(1))

              if (xhi > hif) then
                  Cycle
              end if
              xhf = abs(dx(1)*hf(2) - dx(2)*hf(1))

              if (xhf > hif) then
                  Cycle
              end if

              !All tests passed.  Accept this input location
              DMAP3D_addin_accept = .true.

  if ( obj%nav <= 5) then 
    write(iupr,*) ' current subc_2 ', obj%mycpu, my_cpu, obj%nav,obj%ntrib, &
               e1l,e1_last,e2l,e2_last, ne1,ne2    
  end if

              return
           end do        !end ie1 loop
        End Do         !end ie2 loop
        ego(3) = g1g
      End Do         !end ig1 goop
      ego(4) = g2g
   End Do         !end ig2 loop

   End Function DMAP3D_addin_accept


!!--------------------------- DMAP3D mkout flt ----------------------------!!
!!--------------------------- DMAP3D mkout flt ----------------------------!!
!!--------------------------- DMAP3D mkout flt ----------------------------!!


!******************************************************************************
!
!     fflt applies filters proportional to /freq/ and /time/ to the input data.
!     4 Pi   4 k    n/2-1 2 Pi
!     The kth filter element is  ---- * (--*(-1)  + Sum   j*Cos[---- j k]
!      N*N   N      j=1   N
!     with N = 12 and -N/2 <= k <= N/2.  The filter is symmetric with 7 non-
!     zero elements.
!
!     Input
!  tri      -- the input trace
!     Output
!  tro      -- the output trace
!
!******************************************************************************

      Subroutine DMAP3D_mkout_flt(tri,tro,Aout,dmaxi,dmaxo)

      Implicit None
      Real, Intent(In) :: tri(:)
      Real, Intent(Out) :: tro(:)
      Real, Intent(In) :: Aout(:,:)
      Real, Intent(Out) :: dmaxi, dmaxo

      Integer  :: n, i          
      Integer  ::     ntf, ltf, ifr, itf 
      Real  :: bo, bn, dbdt, afr, epa

      Real,Parameter :: flt1 = 1.5708,     flt2 = -0.651366
      Real,Parameter :: flt3 = -0.087267,  flt4 = -0.046767

      integer   :: dummy
      
      !Size of input and output arrays
      n   = size(tri)
      ntf = size(Aout,2)

      !Frequency filter
      tro = tri * flt1
      tro(2:n-1) = tro(2:n-1) + flt2 * (tri(1:n-2)  + tri(3:n))
      tro(4:n-3) = tro(4:n-3) + flt3 * (tri(1:n-6)  + tri(7:n))
      tro(6:n-5) = tro(6:n-5) + flt4 * (tri(1:n-10) + tri(11:n))

      tro(1) = tro(1) + flt2 * tri(2) + flt3 * tri(4) + flt4 * tri(6)
      tro(2) = tro(2)        + flt3 * tri(5) + flt4 * tri(7)
      tro(3) = tro(3)        + flt3 * tri(6) + flt4 * tri(8)
      tro(4) = tro(4)          + flt4 * tri(9)
      tro(5) = tro(5)          + flt4 * tri(10)

      tro(n-4) = tro(n-4)      + flt4 * tri(n-9)
      tro(n-3) = tro(n-3)      + flt4 * tri(n-8)
      tro(n-2) = tro(n-2)    + flt3*tri(n-5) + flt4 * tri(n-7)
      tro(n-1) = tro(n-1)    + flt3*tri(n-4) + flt4 * tri(n-6)
      tro(n)   = tro(n)   + flt2*tri(n-1) + flt3*tri(n-3) + flt4 * tri(n-5)

      !Time filter -- Theoretically, this should be multiplication by i-1.
      !Instead, this uses the inverse of the impulse response of the
      !the mapping operator, measured at ntf points ltf/2,3ltf/2,...

    dummy = 0
      
    if ( dummy == 1 ) then
      ltf = n/ntf
      epa = .001 * MaxVal(aout(1,1:ntf))
      bo = ltf/2
      bn = 1./(aout(1,2) + epa)
      dbdt  = 1.
      ifr = ltf + ltf/2
      itf = 2
      afr = bn +(1-ifr)*dbdt

      Do i = 1,n

          tro(i) = tro(i) * afr      !multiply trace by amplitude factor

          if (i > ifr) then
          itf = itf + 1
         ifr = ifr + ltf
         if(itf == ntf) ifr = ifr + ltf
            bo = bn
            bn = 1./(aout(1,itf) + epa)
           dbdt  = (bn - bo)/ltf
         end if
         afr = afr + dbdt          !update amplitude factor
      End Do

    else 
    
      Do i = 1,n
        afr = i
        tro(i) = tro(i) * afr      !multiply trace by amplitude factor
      end do

    end if    

      dmaxi = 0.
      dmaxo = 0.

      End Subroutine DMAP3D_mkout_flt


!!--------------------------- DMAP3D putout ---------------------------------!!
!!--------------------------- DMAP3D putout ---------------------------------!!
!!--------------------------- DMAP3D putout ---------------------------------!!


!******************************************************************************
!
!  Putout gets traces from scratch, places them in trace array
!  Passed Parameters
!     obj%ntotout    -- Total number of traces to output
!     obj%ntrout     -- Number of traces output so far
!     obj%ndpt      -- Trace length
!     hd      -- Output array of trace headers
!     tr      -- Output array of traces
!     itr      -- First output trace this pass
!     ndo      -- Number of traces output this pass
!     Istatus      -- Error status indicator
!  Other Parameters
!     ntrm      -- Maximum number of traces per pass
!
!
!******************************************************************************

   Subroutine DMAP3D_putout(obj,hd,tr,itr,ndo,Istatus)

   Implicit None
   Type (DMAP3D_struct) :: obj
   Real   :: tr(:,:)
   Double Precision :: hd(:,:)
   Integer  :: itr, ndo, Istatus

   Integer  :: ntrm, ntr

   real     ::                            dmot   
   integer  :: j       


   Integer  :: ie1, ie2, ig1, ig2        
   real     :: ego(4), egi(4), sr(4)
   integer  :: i_err                           ! error flag
   
   integer, save  :: pre_close = 1

      ntrm = size(tr,2)
      istatus = 0

      if(obj%ntroutsc == obj%mappr - 1) then
         dmot = maxval(obj%dout(1:obj%ndpt,itr))
         write(iupr,'(a6,i6,2(a7,e14.6))')   &
                   "out= ",obj%mappr," dmot= ",dmot  
      end if

!     write(iupr,'(a,5i6,e12.4)') &
!         " putout. obj%ntrout,obj%ntotout,obj%ntroutsc,itr,ndo", &
!         obj%ntrout,obj%ntotout,obj%ntroutsc, itr,ndo

      Do ntr = 1, ndo
        if(ntr > ntrm .or. obj%ntroutsc >= obj%ntotoutsc) Exit

        !absolute trace number
        obj%ntroutsc = obj%ntroutsc + 1

        ie1 = obj%ie1sc + obj%isc(1)
        ie2 = obj%ie2sc + obj%isc(2)
        ig1 = obj%ig1sc + obj%isc(3)
        ig2 = obj%ig2sc + obj%isc(4)
        obj%ntrout  =  1 + &
                       ie1 + &
                       obj%ne1 * (ie2 +  &
                       obj%ne2 * (ig1 +  &
                       obj%ng1 * (ig2 )))

        tr(1:obj%ndpt,ntr) = obj%dout(1:obj%ndpt,ntr+itr-1)

         if (ntr+itr-1 > 1 ) then 
           write(*,*) ' fatal error itr1 > 1 of dout ',itr, ntr+itr-1
           stop
         end if


        !Calculate the e-g output coordinates

        ego(1) = ie1 * obj%de1 + obj%e1ini
        ego(2) = ie2 * obj%de2 + obj%e2ini
        ego(3) = ig1 * obj%dg1 + obj%g1ini
        ego(4) = ig2 * obj%dg2 + obj%g2ini

        !Transform output location to input e-g coordinates
        egi = Matmul(obj%ego2egi,ego)

        !transform output trace location to source-receiver coords
        sr = MatMul(obj%eg2sr,egi)

        !Default header values from closest input trace

       if ( obj%closest < 1) then          ! 

          obj%closest = pre_close

       else if( obj%closest > obj%MNTOD ) then 

          obj%closest = obj%MNTOD         

       else 
          pre_close = obj%closest
       end if
        
       Hd(1:obj%nwih,ntr)  = obj%hdin(1:obj%nwih,obj%closest)
        
!        call dmap3d_get_trace (obj, obj%closest, Hd(:,ntr:), & 
!             obj%din_tmp(:,1:), i_err)    

        Hd(1,ntr)      = obj%ntrout
        Hd(3,ntr)      = ig1 + obj%ng1*ig2

        Hd(33,ntr)  = sr(1)/obj%xwidth  !output source grid xpos
        Hd(34,ntr)  = sr(2)/obj%ywidth  !output source grid ypos
        Hd(35,ntr)  = sr(3)/obj%xwidth  !output receiver grid xpos
        Hd(36,ntr)  = sr(4)/obj%ywidth  !output receiver grid ypos

        Hd(6,ntr )  = Sqrt((sr(1)-sr(3))**2 + (sr(2)-sr(4))**2)  !offset
        Hd(7:8,ntr) = .5*(Hd(33:34,ntr)+Hd(35:36,ntr)) !midpoint grid pos

        !field coordinates
        hd(11,ntr) = obj%xorigin + Hd(33,ntr) * obj%dx11 + Hd(34,ntr) * obj%dx12
        hd(12,ntr) = obj%yorigin + Hd(33,ntr) * obj%dx21 + Hd(34,ntr) * obj%dx22
        hd(14,ntr) = obj%xorigin + Hd(35,ntr) * obj%dx11 + Hd(36,ntr) * obj%dx12
        hd(15,ntr) = obj%yorigin + Hd(35,ntr) * obj%dx21 + Hd(36,ntr) * obj%dx22


!        write(iupr,*)'obj%xorigin,  sr(1),  obj%dx11, sr(2), obj%dx12 ', &
!                      obj%xorigin,  sr(1),  obj%dx11, sr(2), obj%dx12,   &
!                      obj%dx21, obj%dx22

!        write(iupr,*)'obj%yorigin, sr(3), obj%dx21, sr(4), obj%dx22 ', &
!           obj%yorigin, sr(3), obj%dx21, sr(4), obj%dx22

        hd(17,ntr) = .5 * (hd(11,ntr) + hd(14,ntr))
        hd(18,ntr) = .5 * (hd(12,ntr) + hd(15,ntr))

        hd(25,ntr) = 0.
        do j = 1, obj%ndpt
           if(hd(25,ntr) < abs(tr(j,ntr))) then
              hd(25,ntr) = abs(tr(j,ntr))
           end if
        end do

        !maximum value in output data set
        if(obj%dmax < hd(25,ntr)) then
            obj%dmax = hd(25,ntr)
            obj%idmax = obj%ntrout
        end if
                                           
        if ( mod(obj%ntroutsc, nskip) == 1)  then
          write(iupr,'(a8,2i6,6f12.2,f12.2)')"putout ",obj%ntroutsc,  &
            obj%mycpu,hd(7:8,ntr),hd(33:36,ntr),hd(6,ntr)
        end if 

        !write the output trace to disc.
        !istatus = trcio_write_trace ( file=obj%trcio_obj,            &
        !                              hd=hd(1:obj%nwih,ntr),         &
        !                              tr=tr(1:obj%ndpt,ntr),         &
        !                              tnum=obj%ntrout )

        !if(istatus .ne. 0) then
        !    write(iupr,'(a,i6)')"problem writing trace # ",obj%ntrout
        !end if
        !
        ! write this trace to record obj%ntroutsc, using pe dmap3d_i_pel
        !
        call dmap3d_write_trace_file (                          &
                                     io_obj   = obj%image_obj,     & 
                                     irecord  = obj%ntroutsc,   &
                                     hd_buf   = hd(:,ntr:),     &
                                     tr_buf   = tr(:,ntr:),     &
                                     i_err    = i_err          &
                                   )
        !
        xxif_write_error : if ( i_err .ne. 0 ) then
          !

       call pc_error ( ' error in dmad3d_putout during dmap3d_write_trace_file')
          return
          !
        end if xxif_write_error
        !

      End Do

   End Subroutine DMAP3D_putout


!!--------------------------- DMAP3D flush ---------------------------------!!
!!--------------------------- DMAP3D flush ---------------------------------!!
!!--------------------------- DMAP3D flush ---------------------------------!!


!******************************************************************************
!
!  flush outputs traces from the image disk file
!  Passed Parameters
!     obj%ntotout    -- Total number of traces to output
!     obj%ntrout     -- Number of traces output so far
!     obj%ndpt      -- Trace length
!     hd      -- Output array of trace headers
!     tr      -- Output array of traces
!     itr      -- First output trace this pass
!     ndo      -- Number of traces output this pass
!     Istatus      -- Error status indicator
!  Other Parameters
!     ntrm      -- Maximum number of traces per pass
!
!
!******************************************************************************

   Subroutine DMAP3D_flush(obj,hd,tr,itr,ndo,Istatus)

   Implicit None
   Type (DMAP3D_struct) :: obj
   Real   :: tr(:,:)
   Double Precision :: hd(:,:)
   Integer  :: itr, ndo, Istatus

   Integer  :: ntrm, ntr





   Integer  ::                               icpu  

   integer           :: pe_current ! the current output pe

      ntrm = size(tr,2)
      istatus = 0

      if ( mod(obj%ntrout, nskip) == 1) then

         write(iupr,'(a,6i10)')" flush_rout.", &
          obj%mycpu,obj%ntrout,obj%ntotout,itr,ndo

      end if
      
      Do ntr = 1, ndo
        if(ntr > ntrm .or. obj%ntrout >= obj%ntotout) Exit

        pe_current = obj%pe_out

        !increment the global output trace counter ntrout
        obj%ntrout  =  obj%ntrout + 1

        obj%pe_out = - 1
        Do icpu = 0, obj%numcpu - 1
           if ( obj%ie1 >= obj%negsc(1,1,icpu) .and. &
                obj%ie1 <= obj%negsc(1,2,icpu) .and. &
                obj%ie2 >= obj%negsc(2,1,icpu) .and. &
                obj%ie2 <= obj%negsc(2,2,icpu) .and. &
                obj%ig1 >= obj%negsc(3,1,icpu) .and. &
                obj%ig1 <= obj%negsc(3,2,icpu) .and. &
                obj%ig2 >= obj%negsc(4,1,icpu) .and. &
                obj%ig2 <= obj%negsc(4,2,icpu) ) Then

                obj%pe_out = icpu
                exit
            end if
        end do

        if(obj%pe_out < 0 .or. obj%pe_out >= obj%numcpu) then

!            istatus = - 1

             write(iupr,*) obj%pe_out,obj%mycpu, " problem in flush "
             write(iupr,*) ' flush_problem ', obj%ie1,obj%ie2,obj%ig1,obj%ig2

             Do icpu = 0, obj%numcpu - 1
                write(iupr,*) ' flush2 ', icpu,  &
                   obj%negsc(1,1,icpu),obj%negsc(1,2,icpu), &
                   obj%negsc(2,1,icpu), obj%negsc(2,2,icpu), &
                   obj%negsc(3,1,icpu),obj%negsc(3,2,icpu), &
                   obj%negsc(4,1,icpu),obj%negsc(4,2,icpu)
           end do

 
            call pc_error (' error in dmap3d_flush pe_out=',obj%pe_out,  &
              ' invalid output pe index ')

           return
         end if

         !broadcast the value of the next output pe,pe_out from the current one
         !call pcpsx_broadcast ( pe_current, obj%pe_out )

         ! if this is the output pe increment the local output trace counter,
         ! ntroutsc and read this output trace from disk

        if ( obj%mycpu == obj%pe_out ) Then
          !
          ! increment the local output trace counter, ntroutsc
          !
          obj%ntroutsc = obj%ntroutsc + 1
          !
          ! read this trace from record obj%ntroutsc, using pe dmap3d_i_pel
          !
          istatus = 0
          call dmap3d_read_trace_file (                           &
                                       io_obj   = obj%image_obj,  & 
                                       irecord  = obj%ntroutsc,   &
                                       hd_buf   = hd(:,ntr:),     &
                                       tr_buf   = tr(:,ntr:),     &
                                       i_err    = istatus         & 
                                      )
        end if 

        !
        ! broadcast the read error flag, istatus, from the output pe, pe_out
        ! to the other pes              
        ! check for a read error
        !

        xxif_read_error : if ( istatus .ne. 0 ) then
          
          write(iupr,*) ' error in dmad3d_flush in dmap3d_read_trace_file' 
          call pc_error ( ' error in dmad3d_flush in dmap3d_read_trace_file')
          call pcpsx_broadcast ( obj%pe_out, istatus )
          return
          
        end if xxif_read_error

        !
        ! broadcast the header and trace array from th eoutput pe to the others
        ! all samples in the first n-1 dimensions will be broadcast
        ! the 1 is how many of the last dimension get broadcast
        !
  !        call pcpsx_broadcast ( obj%pe_out, 1, hd(:,ntr:) )
  !        call pcpsx_broadcast ( obj%pe_out, 1, tr(:,ntr:) )

        if(obj%pe_out.ne.0) then                              ! chuck change
          if(pcps_current_worker_num.eq.obj%pe_out) then
           call pcpsx_send_data(0,1,hd(:,ntr:ntr),21)
           call pcpsx_send_data(0,1,tr(:,ntr:ntr),22)

  !           write(iupr,*)' send  ', pcps_current_worker_num, &
  !              obj%pe_out, dmap3d_i_pel(), &
  !              obj%ntrout, obj%ntroutsc, & 
  !              obj%ie1, & 
  !              obj%ie2, &  
  !              obj%ig1, & 
  !              obj%ig2, real(hd(7:8,ntr))

          else if(pcps_current_worker_num == 0) then

           call pcpsx_receive_data(obj%pe_out,1,hd(:,ntr:ntr),21)
           call pcpsx_receive_data(obj%pe_out,1,tr(:,ntr:ntr),22)

  !           write(iupr,*)' receive  ', pcps_current_worker_num, &
  !              obj%pe_out, dmap3d_i_pel(), &
  !              obj%ntrout, obj%ntroutsc, & 
  !              obj%ie1, & 
  !              obj%ie2, &  
  !              obj%ig1, & 
  !              obj%ig2, real(hd(7:8,ntr))

          endif

        endif

        !increment the directional counters
        obj%ie1 = obj%ie1 + 1
        if(obj%ie1 >= obj%ne1) then
            obj%ie1 = 0
            obj%ie2 = obj%ie2 + 1
            if(obj%ie2 >= obj%ne2) then
               obj%ne2 = 0
               obj%ng1 = obj%ng1 + 1
               if(obj%ig1 >= obj%ng1) then
                   obj%ig1 = 0
                   obj%ig2 = obj%ig2 + 1
                end if
             end if
        end if
 
      End Do

   End Subroutine DMAP3D_flush


  subroutine DMAP3D_med_amp(obj, Istatus)
   Implicit None

   !Passed Parameters

   Type (DMAP3D_Struct),intent(inout)  ::  obj    
   Integer,intent(Out)  :: Istatus      !Status Word

   integer              :: itr 

   obj%Isc(1) = obj%NE1sc/2
   obj%Isc(2) = obj%NE2sc/2

   Istatus = 0
   itr = 0
   obj%calc_med_scale = .true.

   Call DMAP3D_mkout(obj,itr)  

   call median (obj%amp_store, obj%namp_store, obj%amp_median)

   write(iupr,910)   obj%amp_median
 910 format(' Median mapping amplitude of reference trace =  ',E16.6)

   obj%calc_med_scale = .false.
   obj%Isc(1:2) = 0
   obj%Dout = 0.0 

  End Subroutine DMAP3D_med_amp 


!!--------------------------- DMAP3D getNN ---------------------------------!!
!!--------------------------- DMAP3D getNN ---------------------------------!!
!!--------------------------- DMAP3D getNN ---------------------------------!!


!**************************************************************************
!
!GetNN Finds the natural (or nearest) neighbors of a set of x-y locations.
!
!The natural neighbors of a location are nearest in the sense that the inside
!of the polygon defined by the perpendicular bisectors of the line segments
!from the location to its natural neighbors is closer to that location than
!to any other location in the set.  Thus, the natural neighbors define an
!area around a location that is closest to that location.
!
!This algorithm is straightforward, brute force, and hopefully robust. All I
!do is cycle through all possible neighbors, building and evolving a set of
!potential naturals as I go along.
!
!Locations closer together than some minimum distance are considered to be at
!the same location, and are given the same set of nearest neighbors.  They
!become cohabiters (referred to below as "twinkies"), not neighbors.
!
!This routine may discard some legitimate neighbors if necessary to avoid
!overflow.  A maximum number of allowed neighbors is set by the calling
!program when the parameter structure obj is defined.  Currently, this
!algorithm discards the furthest neighbor if necessary to keep within the
!maximum.
!
!GetNN also restricts the maximum Voronoi cell size.  This is most useful
!around the boundaries of the data where it is desirable to truncate cells
!at some reasonable distance from the edge, but it also limits the distance
!cells are allowed to cover large holes within the data.
!
!The number of operations goes with the square of the number of locations.  I
!haven't really read the literature, but I expect that faster algorithms are
!out there.  A "sweepline" algorithm by a fellow named Fortune at Bell labs
!is supposed to be "as good as any".  Even my algorithm could potentially be
!speeded up by sorting the data in the x and y directions and develping
!an index that allows cycling only through locations in a given region.
!However, I'm not inclined to do anything more until this algorithm is proven
!to be too slow.
!
!Inputs include
! obj%MNTOD    -- The number of locations in the set
! obj%Einbin    -- Two points closer than this are considered to be at
!   the same location.
! obj%Vcmax    -- Sets the maximum Voronoi cell size relative to an
!   average separation between neighbors.
! obj%Mask(ntr)    -- Mask = 0 for locations to ignore
! obj%Gbin(ntr)     -- Gather id numbers.  To be neighbors, locations must
!   be in the same gather.
! obj%x(2,ntr)    -- the x-y locations
!
!Outputs include
! obj%Nnn(ntr)    -- number of natural neighbors to each location
! obj%In(nnn,ntr)  -- indices of natural neighbors for each location
! obj%dx(2,nnn,ntr)-- x-y distances from location to each natural neighbor
! obj%Same(ntr)    -- number of points at the same location.
!
!Last Update 99/11/24
!--------------------------------------------------------------------------

Subroutine DMAP3D_getNN(obj, Istatus)

Implicit None

!Passed Parameters

Type (DMAP3D_Struct),intent(inout)  ::  obj    !Location property structure
Integer,intent(Out)  :: Istatus      !Status Word

!Local Parameters
Integer       :: Ntr      !Number of locations in set
Real          :: Vcmax      !Ratio of maximum Voronoi cell size
                            !to avg distance between neighbors
Real          :: BIG      !A very large number
Integer       :: itr
Integer       :: jtr
Integer       :: ktr
Integer       :: inn
Integer       :: jnn
Integer       :: lnn      !Counters
Integer       :: Nbig      !Number of unclosed Voronoi Cells
Real          :: de(2), rsq   !Distance between loc and a neighbor
Real          :: rmsq      !Minimum separation distance squared

Real          :: rav1, rav2   !Average separation between neighbors
Integer       :: nrav      !Number of positions in average

Real          :: TooBig(2)    !Voronoi Cell Size Limit
Logical       :: skipcctest   !Trigger for VC CC Test
Logical       :: addme      !add a neighbor

Integer, parameter   :: itrpr=0

logical              :: lprnt        !
real                 :: vcmax1, vcmax2

!Define local parameters in terms of passed parameters
Ntr   = obj%MNTOD - obj%Nav
Vcmax = obj%Vcmax

!Define large Real
BIG = Huge(1.0)*.5

!Initializations
Istatus    = 0       !Set status integer
obj%Nnn    = 0       !Set number of neighbors to none
obj%Same   = 0       !Set number of shared locations (twinkies)

!!
obj%Vc    = Huge(1.0)      !Set Voronoi polygon locations to infinity

!!
Nbig    = 0       !Number of unclosed Voronoi cells
rav1    = 0.       !Average x-separation between neighbors
rav2    = 0.       !Average y-separation between neighbors
Nrav    = 0       !Number of neighbors in average

write(iupr,*)" Voronoi Cell Generation: # of nodes is Ntr = ", Ntr,   &
             " on Processor ",obj%mycpu
write(iupr,*)" "

!Square of minimum separation
rmsq = obj%Einbin*obj%Einbin*obj%xwidth*obj%ywidth

If (ntr == 0) then

   !Input aperture is zero.  Nothing to do
   Return
Elseif (ntr == 1) then

   !Only one input trace.  Surround it with a rectangle of one grid spacing
   obj%nnn(1) = 4
   obj%in(:,1) = -1                 !All neighbors are imaginary

   !neighbor positions
   obj%dx(1,1,1) =   obj%xwidth
   obj%dx(2,1,1) =   obj%ywidth
   obj%dx(1,2,1) = - obj%xwidth
   obj%dx(2,2,1) =   obj%ywidth
   obj%dx(1,3,1) = - obj%xwidth
   obj%dx(2,3,1) = - obj%ywidth
   obj%dx(1,4,1) =   obj%xwidth
   obj%dx(2,4,1) = - obj%ywidth

   !Voronoi Cell Node positions
   Call DMAP3D_getNN_getVC(obj%dx(1:2,1,1),  &
     obj%dx(1:2,2,1),obj%Vc(1:2,1,1))
   Call DMAP3D_getNN_getVC(obj%dx(1:2,2,1),  &
     obj%dx(1:2,3,1),obj%Vc(1:2,2,1))
   Call DMAP3D_getNN_getVC(obj%dx(1:2,3,1),  &
     obj%dx(1:2,4,1),obj%Vc(1:2,3,1))
   Call DMAP3D_getNN_getVC(obj%dx(1:2,4,1),  &
     obj%dx(1:2,1,1),obj%Vc(1:2,4,1))

   Return

End if

!Loop over Locations
Itr_Loop_1: Do itr = 1, Ntr

     !This proces is only for traces with Mask <> 0

     If(obj%Mask(itr) == 0 ) CYCLE

      if(itr == itrpr) then
        write(iupr,'(a4,i4,2(a4,2f12.2),a6,2i6)')"tr# ",itr,   &
        " x= ",obj%x(1:2,itr)," h= ",obj%h(1:2,itr)," bin= ",obj%gbin(itr)
      end if

     !If location is shared (has twinkies), share neighbors
     If(obj%Same(itr) < 0) then

       jtr         = - obj%Same(itr)
       obj%Nnn(itr)        =   obj%Nnn (jtr)
       Do inn = 1, obj%Nnn(itr)
          obj%In(inn,  itr ) =   obj%In(inn,  jtr )
          obj%dx(1,inn, itr ) = - obj%dx(1,inn, jtr )
          obj%dx(2,inn, itr ) = - obj%dx(2,inn, jtr )
          obj%Vc(1,inn, itr ) =   obj%Vc(1,inn, jtr )
          obj%Vc(2,inn, itr ) =   obj%Vc(2,inn, jtr )
       End Do

        !Debug print statement
        if(itr == itrpr) then
           write(iupr,*) itr," same as ", jtr
        end if

       CYCLE
     end If

     !Test all other locations
     skipcctest = .false.

     vcmax1 = (0.5*obj%Vcmax *obj%xwidth)   
     vcmax2 = (0.5*obj%Vcmax *obj%ywidth)

     If (itr < Ntr) then
 
           jtr_Loop: Do jtr = 1, Ntr      

!!         jtr_Loop: Do jtr = itr+1, Ntr     ! ** original


         !Neighbors must lie in same Gbin
         If(obj%Mask(jtr)    == 0 .or. jtr == itr .or.  &
            obj%Gbin(jtr) .ne. obj%Gbin(itr) )       CYCLE

         !Distance to test location
         de(1) = obj%x(1,jtr) - obj%x(1,itr)
         de(2) = obj%x(2,jtr) - obj%x(2,itr)

         rsq = de(1)*de(1) + de(2)*de(2)

         !Debug print statement
         If(itr == itrpr) then
           write(iupr,*)"itr jtr  = ", itr, jtr
           write(iupr,*)"rsq rmsq = ", rsq, rmsq
           write(iupr,*)" "
         endif

        !If distance is less than minimum, treat locations as identical twinkie
         If(rsq.lt.rmsq) then

            obj%Same(itr) = obj%Same(itr) + 1
            obj%Same(jtr) = - itr

            if(itr == itrpr) then
              write(iupr,*) itr," same as ", jtr
            end if

           CYCLE
         End if

         if ( abs(de(1)) > vcmax1 .or. abs(de(2)) > vcmax2 ) cycle
 
         Call DMAP3D_getNN_addNN(obj,itr,jtr,de,rmsq,skipcctest)

         !Debug print
         if (itr == itrpr) then
            write(iupr,'(2(a4,i6,a4,2e12.4))')(" In = ",obj%In(inn,itr),  &
             " Vc = ", obj%Vc(1:2,inn,itr), inn = 1,obj%Nnn(itr))
         end if

       !End loop over test locations
       End Do Jtr_Loop

     End If

     !Loop over neighbors of itr
     skipcctest = .true.
     inn_loop: Do inn = 1, obj%Nnn(itr)

       !jtr is a neighbor of itr
       jtr  = obj%In(inn,itr)

       if (jtr < 1 .or. jtr > Ntr) CYCLE

       !calculate average distance to neighbors
       nrav = nrav + 1
       rav1 = rav1 + abs(obj%dx(1,inn,itr))
       rav2 = rav2 + abs(obj%dx(2,inn,itr))

       !Reciprocity: if jtr neighbors itr, then itr neighbors jtr
       if(jtr > itr) then
          de(1)= - obj%dx(1,inn,itr)
          de(2)= - obj%dx(2,inn,itr)

          call DMAP3D_getNN_addNN(obj,jtr,itr,de,rmsq,skipcctest)

          If(itr ==itrpr) then
              write(iupr,'(2(a4,i6,a4,2e12.4))')" In = ", obj%In(inn,itr), &
                " Vc = ", obj%Vc(1:2,inn,itr)
          endif

       end if

       !Delaunay triangles must match too (degeneracies can cause problems)
       if(obj%Vc(1,inn,itr) < BIG ) then

          !Next neighbor of itr must be a neighbor of jtr as well
          jnn = 1 + Mod(inn,obj%Nnn(itr))

          !itr,jtr,ktr form vertices of a Delaunay triangle
          ktr = obj%In(jnn,itr)

          !ktr should neighbor jtr
          addme = .true.
          do lnn = 1, obj%Nnn(jtr)
             if (ktr == obj%In(lnn,jtr)) then
               addme = .false.
               EXIT
             end if
          end do

          !if ktr does not neighbor jtr, make it so
          if(addme) then
             de(1) = obj%x(1,ktr) - obj%x(1,jtr)
             de(2) = obj%x(2,ktr) - obj%x(2,jtr)

             call DMAP3D_getNN_addNN(obj,jtr,ktr,de,rmsq,skipcctest)
          end if

          !jtr should neighbor ktr
          addme = .true.
          do lnn = 1, obj%Nnn(ktr)
             if (jtr == obj%In(lnn,ktr)) then
               addme = .false.
               EXIT
             end if
          end do

          !if jtr does not neighbor ktr, make it so
          if(addme) then
             de(1) = obj%x(1,jtr) - obj%x(1,ktr)
             de(2) = obj%x(2,jtr) - obj%x(2,ktr)

             call DMAP3D_getNN_addNN(obj,ktr,jtr,de,rmsq,skipcctest)
          end if
       end if

     !End loop over neighbors of itr
     End Do inn_loop

!End loop over locations itr
End Do Itr_Loop_1


!Average distances to neighbors
rav1 = rav1/nrav
rav2 = rav2/nrav

TooBig(1) = rav1 * Vcmax
TooBig(2) = rav2 * Vcmax

!!  write(iupr,'(a12,5f14.3)')"TooBig= ",TooBig, rav1, rav2, vcmax1, vcmax2

!calculate Number of Too Big Voronoi Nodes.

Itr_Loop_2: Do itr = 1, Ntr

   !This proces is only for traces with Mask <> 0
   If(obj%Mask(itr) == 0 .or. obj%Same(itr) < 0) CYCLE

    lprnt = .false.

   do inn = 1, obj%Nnn(itr)
      !
      !write(iupr,*)"itr inn obj%Vc(1,inn,itr) = ", itr, inn, obj%Vc(1,inn,itr)
      !write(iupr,*)"itr inn obj%Vc(2,inn,itr) = ", itr, inn, obj%Vc(2,inn,itr)
      !write(iupr,*)" "
      !

      if(Abs(obj%Vc(1,inn,itr)) >= TooBig(1) .or.   &
         Abs(obj%Vc(2,inn,itr)) >= TooBig(2) ) then 
         Nbig = Nbig + 1      
         lprnt = .true.                
      end if 
   end Do

   if (lprnt) then
! *** newadd 
      write(11,990) obj%x(1,itr)/obj%xwidth, obj%x(2,itr)/obj%ywidth    
 990  format(2f12.2)
      do inn = 1, obj%Nnn(itr)
        write(11,*) obj%Vc(1,inn,itr), obj%Vc(2,inn,itr)  
      end do
   end if

!!   write(iupr,*)" "
!!   write(iupr,*)"itr and # of unclosed Voronoi cells is Nbig = ", itr, Nbig

!End loop over locations
End Do Itr_Loop_2

Call DMAP3D_getNN_closeVC(obj,Nbig,TooBig,rmsq)

 if(obj%prnbr(1:1) =="Y") Then   
   write(iupr,*)
   write(iupr,*)"Neighbor List"
   Do itr = 1, ntr
      write(iupr,'(2e12.4,3x,i5,2f12.2)') & 
           obj%x(1:2,itr), itr,obj%x(1,itr)/obj%xwidth, &  
           obj%x(2,itr)/obj%ywidth  
      do inn=1,obj%Nnn(itr)
        write(iupr,'(2e12.4, 3x,i5)') obj%Vc(1:2,inn,itr)+obj%x(1:2,itr), inn
      end do
   End Do

  end if


End Subroutine DMAP3D_getNN



!!--------------------------- DMAP3D getNN addNN -----------------------------!!
!!--------------------------- DMAP3D getNN addNN -----------------------------!!
!!--------------------------- DMAP3D getNN addNN -----------------------------!!


!-------------------------------------------------------------------------
!
!AddNN Tests a point jtr to see if it is a natural neighbor of point itr, by
!  comparing it with the current neighbor list of itr. If the point is
!  a natural neighbor, it is added to the list.  The list is then scanned to
!  see whether some points should be removed from the list.
!
!The neighbor list for each location is stored in courterclockwise order
!  (2 is ccw of 1, etc.)  The algorithm locates the position of jtr in this
!  order, builds a line from jtr perpendicular to a line from jtr to itr,
!  and calculates the intersection points of this line with perpendicular
!  lines from the two neighbors bracketing jtr in the neighbor list.
!  If jtr is a natural neighbor, the second intersection point will be
!  counterclockwise of the first.
!
!Once a point is added, it is possible that some points in the list no longer
!  qualify as neighbors and should be removed.
!  This algorithm removes points if the intersection points on their
!  perpendicular lines are no longer in counterclockwise order.
!
!  Inputs
!     obj   -- A structure of derived type header containing the current
!    neighbor list for all points.
!     itr   -- The index number in obj of the point to update
!     jtr   -- A test point which may or may not belong in itr's neighbor
!    list
!     de    -- A 2-D vector pointing from itr to jtr.
!     rmsq  -- Square of minimum separation distance.  If jtr is closer
!        to a neighbor than this, it will not be added to the list.
!
!  Outputs
!     The following structure elements in obj may be updated:
!     Nnn(itr)      -- Number of neighbors for itr
!     In(inn,itr)    -- Index numbers of neighbors to itr
!     Dx(i,inn,itr)  -- vectors from itr to each neighbor
!     Vc(i,inn,itr)  -- vectors from itr to intersection points
!
!The set of perpendicular intersection points define the vertices of a
!polygon surrounding itr.  If you shrink this polygon to half-size, you get
!itr's Voronoi cell, enclosing the points closer to itr than to any of its
!neighbors.
!
!---------------------------------------------------------------------------

Subroutine DMAP3D_getNN_addNN(obj,itr,jtr,de,rmsq,skipcctest)

   Implicit None
   type (DMAP3D_struct),intent(inout) :: obj
   integer      :: itr,jtr
   Real       :: de(2),rmsq
   Logical      :: skipcctest

   Integer      :: inn, jnn, knn, lnn   
   Real       :: rsq         

   Real       :: det   
   Real       :: vca(2), vcb(2)   

   Real       :: Big, rm     , rsqi 
   Integer      ::                   irm   
   Logical      :: neighbor

   !Define a really big number
   Big = Huge(1.0)*.5

   !Define rsq as squared distance from itr to jtr
   rsq = de(1)*de(1) + de(2)*de(2)

   !How many neighbors have been found for location itr?
   Select Case( obj%Nnn(itr) )

   Case (0)  !Itr has no neighbors (yet)

      !the first test location is a neighbor til proved otherwise
      obj%Nnn(itr)    = 1
      obj%In(1,itr)   = jtr
      obj%dx(1,1,itr) = de(1)
      obj%dx(2,1,itr) = de(2)

      !write(iupr,'(a12,2f12.4)')"dx1 dx2 = ",obj%dx(1,1,itr),obj%dx(2,1,itr)

   Case (1)    !Itr has one neighbor so far.

      !Points too close together are treated as identical or twinkies
      If((de(1)-obj%dx(1,1,itr))*(de(1)-obj%dx(1,1,itr))+ &
        (de(2)-obj%dx(2,1,itr))*(de(2)-obj%dx(2,1,itr)) > rmsq ) then
        !check determinant for order
        det = de(1)*obj%dx(2,1,itr)-de(2)*obj%dx(1,1,itr)

        If(det == 0. .and. de(1)*obj%dx(1,1,itr)+de(2)*obj%dx(2,1,itr) > 0.) &
        then

           !The two points are aligned.  The closer stays
           rsqi = obj%dx(1,1,itr)*obj%dx(1,1,itr) +  &
           obj%dx(2,1,itr)*obj%dx(2,1,itr)
           if (rsq < rsqi) then
              obj%In(1,itr)   = jtr
              obj%dx(1,1,itr) = de(1)
              obj%dx(2,1,itr) = de(2)
           end if

        Else
           !Order is immaterial for two points.  Put new one second
           obj%Nnn(itr) = 2
           obj%In(2,itr)   = jtr
           obj%dx(1,2,itr) = de(1)
           obj%dx(2,2,itr) = de(2)

           !Calculate Voronoi Nodes (One will be at infinity)
           Call DMAP3D_getNN_getVC(obj%dx(1:2,1,itr),  &
           obj%dx(1:2,2,itr),obj%Vc(1:2,1,itr))

           Call DMAP3D_getNN_getVC(obj%dx(1:2,2,itr),  &
           obj%dx(1:2,1,itr),obj%Vc(1:2,2,itr))
        End If
      End If


   Case Default    !At least 2 neighbors already

      !If the neighbor buffer is full, discard the furthest neighbor

 !!     If (obj%Nnn(itr) >= Size(obj%In,1) ) then

      If (obj%Nnn(itr) >= obj%nnmax ) then   

        !The furthest neighbor will be the irm'th.
        rm = 0.
        irm = 0

        Do inn = 1, obj%Nnn(itr)
           rsqi = obj%dx(1,inn,itr)*obj%dx(1,inn,itr) +  &
           obj%dx(2,inn,itr)*obj%dx(2,inn,itr)

           if(rm < rsqi) then
              rm = rsqi
              irm = inn
           end if
        End Do

        !Inform user of dropped point
        !write(iupr,'(a40,3i6,f12.2)') &
        ! " Buffer Full.  AddNN Dropping Point ", irm,itr,obj%In(irm,itr),rm
        !write(iupr,*)" "

        !Reduce Number of Points by one
        obj%Nnn(itr) = obj%Nnn(itr) - 1

        !inn is point before furthest neighbor
        if(irm == 1 ) then
           inn = obj%Nnn(itr)
        else
           inn = irm - 1
        end if
      !
        !Drop point irm from list
        if (irm <= obj%Nnn(itr)) then

           Do knn = irm, obj%Nnn(itr)
              obj%In( knn, itr )  = obj%In(  knn+1, itr )
              obj%dx(1,knn, itr ) = obj%dx(1,knn+1, itr )
              obj%dx(2,knn, itr ) = obj%dx(2,knn+1, itr )
              obj%Vc(1,knn, itr ) = obj%Vc(1,knn+1, itr )
              obj%Vc(2,knn, itr ) = obj%Vc(2,knn+1, itr )
           End Do
        else
           irm = 1
        End If

        !Calculate VC between points before and after discarded point.
        Call DMAP3D_getNN_getVC(obj%dx(1:2,inn,itr),   &
               obj%dx(1:2,irm,itr),obj%Vc(1:2,inn,itr))

      End If

      !Should jtr be in the neighbor list?
      Call DMAP3D_getNN_testN(obj,itr,de,rmsq,neighbor,lnn,jnn, &
      vca,vcb,skipcctest)

      If(neighbor ) then

        if(jnn - lnn == 2 .or. jnn - lnn == 2 - obj%Nnn(itr) ) then

           !test neighbor is aligned with neighbor lnn
           !Replace inn with jtr

           inn = lnn + 1
           if(inn > obj%Nnn(itr)) inn = 1
           obj%In(  inn,itr) = jtr
           obj%dx(1,inn,itr) = de(1)
           obj%dx(2,inn,itr) = de(2)

           obj%Vc(1,inn,itr) = vcb(1)
           obj%Vc(2,inn,itr) = vcb(2)
           obj%Vc(1,lnn,itr) = vca(1)
           obj%Vc(2,lnn,itr) = vca(2)

           Call DMAP3D_getNN_subN(obj,itr)

        else

           !Add jtr to itr's neighbor list
           obj%Vc(1,lnn, itr ) = vca(1)
           obj%Vc(2,lnn, itr ) = vca(2)

           !Shift neighbors to make room for new neighbor in list
           obj%Nnn(itr)  = obj%Nnn(itr) + 1
           Do knn = obj%Nnn(itr),jnn+1,-1
              obj%In( knn, itr )  = obj%In(  knn-1, itr )
              obj%dx(1,knn, itr ) = obj%dx(1,knn-1, itr )
              obj%dx(2,knn, itr ) = obj%dx(2,knn-1, itr )
              obj%Vc(1,knn, itr ) = obj%Vc(1,knn-1, itr )
              obj%Vc(2,knn, itr ) = obj%Vc(2,knn-1, itr )
           End Do

           !Add new neighbor to list
           obj%In(jnn,   itr ) = jtr
           obj%dx(1,jnn, itr ) = de(1)
           obj%dx(2,jnn, itr ) = de(2)
           obj%Vc(1,jnn, itr ) = vcb(1)
           obj%Vc(2,jnn, itr ) = vcb(2)

           !Look for neighbors to discard from list
           Call DMAP3D_getNN_subN(obj,itr)

        end if

      End If

   End Select

End Subroutine DMAP3D_getNN_addNN


!!--------------------------- DMAP3D getNN subN -----------------------------!!
!!--------------------------- DMAP3D getNN subN -----------------------------!!
!!--------------------------- DMAP3D getNN subN -----------------------------!!


!---------------------------------------------------------------------------
!
!   Subn examines the perpendicular intersection points of the neighbors of
!   itr.  If any are not in counterclockwise order, then the associated
!   neighbor is not a natural neighbor and is removed from itr's neighbor
!   list.
!
!---------------------------------------------------------------------------

Subroutine DMAP3D_getNN_subN(obj,itr)

   Implicit None
   Type (DMAP3D_struct) :: obj
   Integer      :: itr

   Integer      :: Nok, inn, jnn, knn
   Real       :: Big, dvc

   Big = Huge(1.0)*.5

   !loop over neighbors
   nok = 0
   inn = 0

   Do
      inn = inn + 1
      nok = nok + 1
      if(inn > obj%Nnn(itr)) inn = 1

      !Exit once all points have been examined.
      if(nok > obj%Nnn(itr)) Exit

      !cyclic wraparound
      if(inn == 1 ) then
  jnn = obj%Nnn(itr)
      else
  jnn = inn - 1
      end if

      !Don't remove neighbors adjacent to a 180 deg + gap
      if(obj%Vc(1,jnn,itr) >= Big .or. obj%Vc(1,inn,itr) >= Big) Cycle

      !Adjacent vc points must be counterclockwise
      dvc = obj%Vc(1,jnn,itr)*obj%Vc(2,inn,itr)  &
   - obj%Vc(2,jnn,itr)*obj%Vc(1,inn,itr)

      if( dvc .lt. 0.) then

  !Debug print
 ! if (itr == ipr) then
 !    write(iupr,'(a10,i6,g12.4)')" dropping ",obj%In(inn,itr),dvc

 !    if(obj%In(inn,itr) == 15 ) then

 ! write(iupr,'(2i6,4e12.4)')inn,jnn,obj%Vc(1,jnn,itr),&
 ! obj%Vc(2,jnn,itr),obj%Vc(1,inn,itr),obj%Vc(2,inn,itr)
 !    end if
 ! end if

  !Drop point inn from list
  obj%Nnn(itr) = obj%Nnn(itr) - 1

  If(inn <= obj%Nnn(itr)) then
     Do knn = inn, obj%Nnn(itr)
        obj%In(knn,   itr ) = obj%In(knn+1,   itr )
        obj%dx(1,knn, itr ) = obj%dx(1,knn+1, itr )
        obj%dx(2,knn, itr ) = obj%dx(2,knn+1, itr )
        obj%Vc(1,knn, itr ) = obj%Vc(1,knn+1, itr )
        obj%Vc(2,knn, itr ) = obj%Vc(2,knn+1, itr )
     End Do
  Else
     inn = 1
  End If

  !jnn is the point before inn
  if(inn == 1 ) then
     jnn = obj%Nnn(itr)
  else
     jnn = inn - 1
  end if

  !Get the intersection point between jnn and inn
  Call DMAP3D_getNN_getVC(obj%dx(1:2,jnn,itr),   &
         obj%dx(1:2,inn,itr),obj%vc(1:2,jnn,itr))
  inn = jnn - 1
  nok = 0

      end if

   End Do

End Subroutine DMAP3D_getNN_subN


!!--------------------------- DMAP3D getNN closeVC -------------------------!!
!!--------------------------- DMAP3D getNN closeVC -------------------------!!
!!--------------------------- DMAP3D getNN closeVC -------------------------!!


!---------------------------------------------------------------------------
!
!  Close removes Voronoi Nodes that are too far away by adding virtual
!  neighbors to the diagram.
!
!  Inputs:
!     obj      -- a structure of defined type DMAP3D_struct
!     Nbig     -- number of too big Voronoi Nodes
!     TooBig   -- Bigness Threshhold
!     rmsq     -- square of minimum separation distance.  If a virtual
!    neighbor is this close to a neighbor already in the
!    list, it will not be added.
!
!  Outputs
!     elements Nnn, de, Vc of obj are modified
!
!     If process is SUCCESSFUL, Nbig will be set to ZERO!!.
!
!--------------------------------------------------------------------------

Subroutine DMAP3D_getNN_closeVC(obj,Nbig,TooBig,rmsq)

   Implicit None

   !Passed Parameters
   Type (DMAP3D_struct) :: obj
   Integer  :: Nbig
   Real   :: TooBig(2), rmsq

   !Local Parameters
   Integer  :: Ntr
   Real   :: BIG, rto
   Real   :: de(2), tbl(2)
!
   Real   :: ve(2,Nbig)
   Real   :: gbin(Nbig)
!
   Real   :: rsqi, rsqj, rsqv
   integer  :: nvn, ivn, inn, jnn, itr, itry
   integer  :: auto_nbig, icount, ind_neigh

   Integer,Parameter :: VIRTUAL_NODE = -1

   auto_Nbig = Nbig

   !write(iupr,*)"On Entrance, Nbig auto_Nbig = ", auto_Nbig, Nbig
   !write(iupr,*)" "
!
   !write(iupr,*)"TooBig = ",TooBig(1), TooBig(2)
   !write(iupr,*)" "

   If(Nbig == 0) Return

   Ntr = obj%MNTOD - obj%Nav          
   
   Big = Huge(1.0)*.5
   tbl = Big

   !More than one cycle of this process may be required.
   Do itry = 1, 5

      !First Pass.  Put a Virtual Neighbor near every TooBig
      nvn = 0

      Do itr = 1, Ntr
        If(obj%Mask(itr) == 0 .or. obj%Same(itr) < 0 ) CYCLE

        Do inn = 1, obj%Nnn(itr)

          if (inn < obj%Nnn(itr)) then
            jnn = inn + 1
          else
            jnn = 1
          end if

        !Too Big and Really Big are treated differently
        If (obj%Vc(1,inn,itr) >= BIG ) then

        !Really big.  Cell extends to infinity
         nvn = nvn + 1
!
         ve(1,nvn) = obj%x(1,itr)-.5*(obj%dx(2,inn,itr)-obj%dx(2,jnn,itr))
         ve(2,nvn) = obj%x(2,itr)+.5*(obj%dx(1,inn,itr)-obj%dx(1,jnn,itr))
!
         gbin(nvn) = obj%Gbin(itr)
         gbin(nvn) = obj%Gbin(itr)
!
        !If(Nvn.eq.auto_Nbig) EXIT

        !!Commenting Out for Now since it fills up the report
        !  write(iupr,'(" Nvn= ",2i6,4e12.4)')  &
        !  nvn,itr,ve(1,nvn),ve(2,nvn)

        Elseif(Abs(obj%Vc(1,inn,itr)) >= tbl(1) .or. &
         Abs(obj%Vc(2,inn,itr)) >= tbl(2) ) Then

         nvn = nvn + 1

!
         !Cell is larger than maximum allowed size
         rsqi = obj%dx(1,inn,itr)*obj%dx(1,inn,itr) +  &
         obj%dx(2,inn,itr)*obj%dx(2,inn,itr)
         rsqj = obj%dx(1,jnn,itr)*obj%dx(1,jnn,itr) +  &
         obj%dx(2,jnn,itr)*obj%dx(2,jnn,itr)

!
         rsqv = obj%Vc(1,inn,itr)*obj%Vc(1,inn,itr) +   &
         obj%Vc(2,inn,itr)*obj%Vc(2,inn,itr)

         rto = Sqrt(Sqrt(rsqi*rsqj)/rsqv)
 
!
         ve(1,nvn) = obj%x(1,itr) + obj%Vc(1,inn,itr)*rto
         ve(2,nvn) = obj%x(2,itr) + obj%Vc(2,inn,itr)*rto
         gbin(nvn) = obj%Gbin(itr)
         gbin(nvn) = obj%Gbin(itr)
!
         If(Nvn.eq.auto_Nbig) EXIT
!
        !!Commenting this out for Now!!
        !   write(*,'(" Nvn= ",2i6,4e12.4)')  &
        !      nvn,itr,ve(1,nvn),ve(2,nvn)

         End If
        End Do

       If(Nvn.eq.auto_Nbig) EXIT

      End Do

      !Reset Nbig
      Nbig = 0
      tbl = TooBig

      !2nd Pass.  Add the virtual neighbors to the diagram, and recalc Nbig
      icount = 0
      Do itr = 1, Ntr

       If(obj%Mask(itr) == 0 .or. obj%Same(itr) < 0 ) CYCLE

       !Loop over virtual neighbors
       Do ivn = 1, nvn

        !Virtual neighbor must belong to same group
        if(obj%Gbin(itr).ne.gbin(ivn)) CYCLE

        !Distance to test location
        de(1) = ve(1,ivn) - obj%x(1,itr)
        de(2) = ve(2,ivn) - obj%x(2,itr)

        !Add virtual neighbor ivn
        Call DMAP3D_getNN_addNN(obj,itr,VIRTUAL_NODE,de,rmsq,.false.)

      End Do

      !Update Nbig
      Do inn = 1, obj%Nnn(itr)

       !!Added for testing
       ind_neigh = obj%In(inn,itr)

       if(Abs(obj%Vc(1,inn,itr)) >= tbl(1) .or. &
         Abs(obj%Vc(2,inn,itr)) >= tbl(2)    ) then
  
         Nbig = Nbig + 1
       End If
      End Do
!
      !Commment this out for now.
      !write(iupr,'(i6,2x,10i6,/(8x,10i6))')itr,(obj%In(inn,itr), &
      !   inn=1,obj%Nnn(itr))

     End Do
!
     If (Nbig == 0) Exit

   End Do

   If(Nbig == 0) then
     !write(iupr,*)"CLOSEVC WAS SUCECESSFUL !"
   Else
     !write(iupr,*)"UNSUCCESSFUL CLOSEVC !"
   Endif

End Subroutine DMAP3D_getNN_closeVC

!!--------------------------- DMAP3D getNN ClsVC ----------------------------!!
!!--------------------------- DMAP3D getNN ClsVC ----------------------------!!
!!--------------------------- DMAP3D getNN ClsVC ----------------------------!!


!---------------------------------------------------------------------------
!
!  Close removes Voronoi Nodes that are too far away by adding virtual
!  neighbors to the diagram.
!
!  Inputs:
!     obj      -- a structure of defined type DMAP3D_struct
!     Nbig     -- number of too big Voronoi Nodes
!     TooBig   -- Bigness Threshhold
!     rmsq     -- square of minimum separation distance.  If a virtual
!    neighbor is this close to a neighbor already in the
!    list, it will not be added.
!
!  Outputs
!     elements Nnn, de, Vc of obj are modified
!
!     If process is SUCCESSFUL, Nbig will be set to ZERO!!.
!
!--------------------------------------------------------------------------

Subroutine DMAP3D_getNN_ClsVC(obj,Nbig,TooBig,rmsq)

   Implicit None

   !Passed Parameters
   Type (DMAP3D_struct) :: obj
   Integer  :: Nbig
   Real   :: TooBig(2), rmsq

   !Local Parameters
   Integer  :: Ntr
   Real   :: BIG   
   Real   :: de(2), tbl(2)
!
   Real   :: ve(2,4*Nbig+4)
   Real   :: gbin(4*Nbig+4)
!

   integer  :: nvn, ivn, inn, jnn, itr, itry
   integer  :: auto_nbig, icount, ind_neigh

   Integer,Parameter :: VIRTUAL_NODE = -1

   auto_Nbig = Nbig

!   write(iupr,*)"On Entrance, Nbig auto_Nbig = ", auto_Nbig, Nbig
!   write(iupr,*)" "

!   write(iupr,*)"TooBig = ",TooBig(1), TooBig(2)
!   write(iupr,*)" "

   If(Nbig == 0) Return

   Ntr = obj%MNTOD

   Big = Huge(1.0)*.5
   tbl = Big

   !More than one cycle of this process may be required.
   Itry_Loop: Do itry = 1, 5

      !First Pass.  Put a Virtual Neighbor near every TooBig
      nvn = 0

      First_Itr_Loop: Do itr = 1, Ntr

        If(obj%Mask(itr) == 0 .or. obj%Same(itr) < 0 ) CYCLE

        If(obj%nnn(itr) == 0) then

            !no neighbors.  place four virtual neighbors around point
            de(1) = obj%xwidth*obj%de1
            de(2) = obj%ywidth*obj%de2
            nvn = nvn + 1
            ve(1,nvn) = obj%x(1,itr) + de(1)
            ve(2,nvn) = obj%x(2,itr)
            gbin(nvn) = obj%gbin(itr)
            nvn = nvn + 1
            ve(1,nvn) = obj%x(1,itr)
            ve(2,nvn) = obj%x(2,itr) + de(2)
            gbin(nvn) = obj%gbin(itr)
            nvn = nvn + 1
            ve(1,nvn) = obj%x(1,itr) - de(1)
            ve(2,nvn) = obj%x(2,itr)
            gbin(nvn) = obj%gbin(itr)
            nvn = nvn + 1
            ve(1,nvn) = obj%x(1,itr)
            ve(2,nvn) = obj%x(2,itr) - de(2)
            gbin(nvn) = obj%gbin(itr)
            nvn = nvn + 1

        elseif(obj%nnn(itr) == 1) then

            !only one neighbor. Place 3 virtual neighbors around point
            de = obj%dx(:,1,itr)
            nvn = nvn + 1
            ve(1,nvn) = obj%x(1,itr) - de(2)
            ve(2,nvn) = obj%x(2,itr) + de(1)
            gbin(nvn) = obj%gbin(itr)
            nvn = nvn + 1
            ve(:,nvn) = obj%x(:,itr) - de
            gbin(nvn) = obj%gbin(itr)
            nvn = nvn + 1
            ve(1,nvn) = obj%x(1,itr) + de(2)
            ve(2,nvn) = obj%x(2,itr) - de(1)
            gbin(nvn) = obj%gbin(itr)

        else

           Inn_Loop: Do inn = 1, obj%Nnn(itr)

              jnn = 1 + mod(inn,obj%nnn(itr))

              !Too Big and Really Big are treated differently
              If (obj%Vc(1,inn,itr) >= BIG ) then

                  !Really big.  Add at least two virtual neighbors
                  nvn = nvn + 1
                  ve(1,nvn) = obj%x(1,itr) - obj%dx(2,inn,itr)
                  ve(2,nvn) = obj%x(2,itr) + obj%dx(1,inn,itr)
                  gbin(nvn) = obj%gbin(itr)
                  nvn = nvn + 1
                  ve(1,nvn) = obj%x(1,itr) - obj%dx(2,jnn,itr)
                  ve(2,nvn) = obj%x(2,itr) + obj%dx(1,jnn,itr)
                  gbin(nvn) = obj%gbin(itr)

                  !if the two new neighbors are >90deg apart, add a third
                  if (obj%dx(2,inn,itr)*obj%dx(2,jnn,itr) + &
                      obj%dx(1,inn,itr)*obj%dx(1,jnn,itr) < 0.) Then
                     nvn = nvn + 1
                     ve(1,nvn) = obj%x(1,itr) - .5*(ve(2,nvn-2)-ve(2,nvn-1))
                     ve(2,nvn) = obj%x(2,itr) + .5*(ve(1,nvn-2)-ve(1,nvn-1))
                     gbin(nvn) = obj%gbin(itr)
                  end if


              Elseif(Abs(obj%Vc(1,inn,itr)) >= tbl(1) .or. &
                 Abs(obj%Vc(2,inn,itr)) >= tbl(2) ) Then

                 nvn = nvn + 1
                 ve(1,nvn) = obj%x(1,itr) -  &
                           .5*(obj%dx(2,inn,itr)-obj%dx(2,jnn,itr))
                 ve(2,nvn) = obj%x(2,itr) +  &
                           .5*(obj%dx(1,inn,itr)-obj%dx(1,jnn,itr))
                 gbin(nvn) = obj%gbin(itr)

              End If
           End Do Inn_Loop

        End If

        If(Nvn.eq.auto_Nbig) EXIT

      End Do First_Itr_Loop

      !Reset Nbig
      Nbig = 0
      tbl = TooBig

      !2nd Pass.  Add the virtual neighbors to the diagram, and recalc Nbig
      icount = 0
      Second_Itr_Loop: Do itr = 1, Ntr

         If(obj%Mask(itr) == 0 .or. obj%Same(itr) < 0 ) CYCLE

        !Loop over virtual neighbors
        Do ivn = 1, nvn

           !Virtual neighbor must belong to same group
           if(obj%Gbin(itr).ne.gbin(ivn)) CYCLE

           !Distance to test location
           de(1) = ve(1,ivn) - obj%x(1,itr)
           de(2) = ve(2,ivn) - obj%x(2,itr)

           !Add virtual neighbor ivn
           Call DMAP3D_getNN_addNN(obj,itr,VIRTUAL_NODE,de,rmsq,.false.)

        End Do

        !Update Nbig
        Do inn = 1, obj%Nnn(itr)

           !!Added for testing
           ind_neigh = obj%In(inn,itr)

           if(Abs(obj%Vc(1,inn,itr)) >= tbl(1) .or. &
              Abs(obj%Vc(2,inn,itr)) >= tbl(2)    ) then

              Nbig = Nbig + 1
           End If
        End Do

      End Do Second_Itr_Loop
!
      If (Nbig == 0) Exit

   End Do Itry_Loop

   If(Nbig == 0) then
     !write(iupr,*)"ClsVC WAS SUCECESSFUL !"
   Else
     !write(iupr,*)"UNSUCCESSFUL ClsVC !"
   Endif

End Subroutine DMAP3D_getNN_ClsVC



!!--------------------------- DMAP3D getNN getVC -----------------------------!!
!!--------------------------- DMAP3D getNN getVC -----------------------------!!
!!--------------------------- DMAP3D getNN getVC -----------------------------!!


!--------------------------------------------------------------------------
!
!  GetVc finds the intersection point of vectors normal to dea and deb.
!  The coords of this point divided by 2 define a node on the boundary of
!  the Voronoi cell around the origin.
!
!  If the second vector is clockwise of the first, this routine returns
!  HUGE(1.) instead of the intersection point.
!
!--------------------------------------------------------------------------
Subroutine DMAP3D_getNN_getVC(dea,deb,Vc)

Implicit None

Real     :: dea(2), deb(2), Vc(2)
Real     :: det, ca, Bigger

Bigger = Huge(1.0)


!Look at cross product of the two vectors
det = dea(1)*deb(2) - dea(2)*deb(1)

If(det <= 0.) Then

      !2nd vector is not < 180 deg counter clockwise of first
      Vc(1) = Bigger
      Vc(2) = Bigger

Else

      !Find intersection point
      ca =  ((deb(1)-dea(1))*deb(1) + (deb(2)-dea(2))*deb(2))/det
      Vc(1) = dea(1) - dea(2) * ca
      Vc(2) = dea(2) + dea(1) * ca

End If

!!Commenting Out for Now since it fills up the report
   !write(iupr,'(a8,4f12.4,/8x,4e12.4)')" GetVc ",dea,deb,det,ca,vc

End Subroutine DMAP3D_getNN_getVC


!!--------------------------- DMAP3D getNN testN -----------------------------!!
!!--------------------------- DMAP3D getNN testN -----------------------------!!
!!--------------------------- DMAP3D getNN testN -----------------------------!!


!---------------------------------------------------------------------------
!
!  TestN decides whether a point is a natural neighbor to itr
!  Inputs
!    obj   -- data structure containing neighbor info
!    de    -- 2-D distance from itr to test neighbor
!    rmsq  -- square of minimum separation distance.
!       If the test neighbor is closer than rmsq to a neighbor in the
!       list, the test will return neighbor = .false.
!
!  Outputs
!    neighbor -- .true. if test point is natural neighbor
!    lnn,jnn  -- indices of neighbors clockwise, counterclockwise, of test
!   neighbor, if neighbor is .true.  (If test neighbor is
!   aligned with neighbor inn, then lnn = jnn = inn)
!    vca,vcb  -- intersection of the perpendicular to DE with those for
!   neighbors on either side, provided neighbor = .true.
!
!---------------------------------------------------------------------------
Subroutine DMAP3D_getNN_testN(obj,itr,de,rmsq,neighbor,lnn,jnn,vca,vcb,&
         skipcctest)

Implicit None

!Passed Parameters
Type (DMAP3D_struct)    :: obj
Integer,Intent(In)    :: itr
Real, Intent(In)    :: de(2), rmsq
Logical, Intent(Out)    :: neighbor
Integer, Intent(Out)    :: lnn, jnn
Real,  Intent(Out)    :: vca(2), vcb(2)

!Local Parameters
Integer      :: inn
Real      :: rsq, rsqi   
Real      :: det, detl
Real      :: Big
Logical      :: skipcctest

   !Distance squared to test neighbor
   rsq = de(1)*de(1) + de(2)*de(2)

   !define large number
   Big = Huge(1.0)*.5

   !Default output values
   lnn = 0
   jnn = 0
   neighbor = .false.
   vca = 0.
   vcb = 0.

   !Loop over current neighbors.

   !First look at last neighbor in list.
   lnn = obj%Nnn(itr)

   !Points too close together are treated as twinkies, not neighbors
   If((de(1)-obj%dx(1,lnn,itr))*(de(1)-obj%dx(1,lnn,itr))+ &
      (de(2)-obj%dx(2,lnn,itr))*(de(2)-obj%dx(2,lnn,itr)) < rmsq ) then

      Return
   End If

   !The cross product of test neighbor with last neighbor in list
   detl = de(1)*obj%dx(2,lnn,itr)-de(2)*obj%dx(1,lnn,itr)

   !If test neighbor aligns with last neighbor, it must be closer
   if(detl == 0..and. &
      de(1)*obj%dx(1,lnn,itr)+de(2)*obj%dx(2,lnn,itr) > 0.) then

      !test neighbor is same direction as last neighbor
      rsqi = obj%dx(1,lnn,itr)*obj%dx(1,lnn,itr) +  &
      obj%dx(2,lnn,itr)*obj%dx(2,lnn,itr)

      !test neighbor is aligned and shorter than last neighbor
      if (rsq < rsqi) then
        neighbor = .true.
        jnn = 1
        lnn = lnn - 1

        Call DMAP3D_getNN_getVC(de,  obj%dx(1:2,1,itr),vcb)
        Call DMAP3D_getNN_getVC(obj%dx(1:2,lnn,itr),de,    vca)
      end if

      Return

   End If

   !Loop over current list of neighbors for location itr
   Do inn = 1, obj%Nnn(itr)

      !Points too close together are treated as twinkies, not neighbors
      If((de(1)-obj%dx(1,inn,itr))*(de(1)-obj%dx(1,inn,itr))+ &
         (de(2)-obj%dx(2,inn,itr))*(de(2)-obj%dx(2,inn,itr)) < rmsq ) then

         return
      End If

      !If det > (<) 0, jtr is cw (ccw) of neighbor inn
      det = de(1)*obj%dx(2,inn,itr)-de(2)*obj%dx(1,inn,itr)
!

      !Debug Print Statement
    !  if(itr == ipr ) then
      !write(iupr,'(a8,i6,2(a8,g12.4))')"lnn= ",lnn," detl= ",detl," det= ",det
      !write(iupr,'(i6,4g12.5)')inn,de,obj%dx(1,inn,itr),obj%dx(2,inn,itr)
    !  end if

      If ( det == 0. .and.  &
         de(1)*obj%dx(1,inn,itr)+de(2)*obj%dx(2,inn,itr) > 0.) Then

        !Test neighbor is same direction as neighbor. One must go
        rsqi = obj%dx(1,inn,itr)*obj%dx(1,inn,itr) +  &
        obj%dx(2,inn,itr)*obj%dx(2,inn,itr)

        if (rsq < rsqi) then

           neighbor = .true.
           if(inn < obj%Nnn(itr)) then
              jnn = inn + 1
           else
              jnn = 1
           end if

           Call DMAP3D_getNN_getVC(de,  obj%dx(1:2,jnn,itr),vcb)
           Call DMAP3D_getNN_getVC(obj%dx(1:2,lnn,itr),de,     vca)
        end if

        Return

      !Place the test location between the last cw and first ccw neighbors
      Elseif ( (obj%Vc(1,lnn,itr)>= BIG .and. det*detl >= 0.) &
         .or. ( detl < 0. .and. det > 0.) ) Then

        !Calclulate VC points with neighbors on either side
        Call DMAP3D_getNN_getVC(de,  obj%dx(1:2,inn,itr),vcb)

        Call DMAP3D_getNN_getVC(obj%dx(1:2,lnn,itr),de,      vca)

        !Debug print statement
        !if(itr.eq.ipr) write(iupr,'(a8,4e12.4)')' vc ', vca,vcb

        !If vcb is counterclockwise of vca, point is neighbor
        if(vcb(1) >= BIG .or. vca(1) >= BIG   .or.   &
           vca(1)*vcb(2) - vca(2)*vcb(1) > 0. .or.   &
           skipcctest ) then

           jnn = inn
           neighbor = .true.
        end if

        Return

      End If

      detl = det
      lnn  = inn

   !End Loop over neighbor List
   End Do

End Subroutine DMAP3D_getNN_testN


!!--------------------------- DMAP3D mkout -----------------------------------!!
!!--------------------------- DMAP3D mkout -----------------------------------!!
!!--------------------------- DMAP3D mkout -----------------------------------!!


!***************************************************************************
!
!   DMAP3D_MkOut  -- Performs the data mapping between input and output traces.
!
!        For an output trace, MkOut should scan the input traces for
!        those whose associated area (Voronoi cell?)lies partly or
!        completely within the contributing (parallelogram) region.
!
!        For those that contribute, compute an amplitude and a range
!        of traveltime ratios.
!        Map the input trace onto the output trace.
!
!        It will be necessary in some instances to divide the area
!        around an input trace into smaller areas, effectively
!        replicating the trace in each sub-area.
!
!  Inputs
!     obj   -- The DMAP3D parameter stucture
!
!  Outputs
!     itr   -- Output trace number
!     obj%dout   -- Output trace
!
!  Subroutines Called
!     DMAP3D_mkout_NOAA     DMAP3D_mkout_NEARAA
!     DMAP3D_mkout_LINAA    DMAP3D_mkout_QNAA
!     DMAP3D_mkout_PAR      DMAP3D_mkout_FindAppCtr
!
!
!***************************************************************************
      Subroutine DMAP3D_mkout( obj, itr )

      Implicit None

!     Passed Parameters
      type(DMAP3D_struct)       :: obj
      Integer        :: itr

      !Parameters
      Integer, Parameter      :: Niter = 4

      !local variables
      real        :: ego(4), egi(4)       , sr(4), go(2) 
      real        :: xi(2), xf(2), hf(2)
      real        :: hif                  , hifa, hifb, hifc  


      integer     :: indi                          

!
      real        :: tfti(obj%MNTOD)
      real        :: wf(obj%MNTOD)
 
!
      real        :: hi(2,obj%MNTOD)
      real        :: dxm(2,obj%MNTOD)

      integer     ::                      Ntrib  
      real        ::      vdti         , ttrmin, ttrmax   

      real        :: timax(obj%MNTOD)
      real        :: scale_tr(obj%MNTOD)
      
      real        :: ahf2, ahf

      real        :: hmn2, hfr       , stpr 

 




      real        :: dxm_new(2),hi_new(2),BigBig








      real,parameter       :: threshhold = .00001


      integer     ::      iinb, iinc 

      integer        :: iin(3)
      real           :: tc(2), thr, trlen
      Logical        :: Inside
      integer        :: ntrout
      real           :: mh(4), gci(4), dg(2)
      real           :: dmax(3)
      integer        :: incount     
      !
   
      integer     :: dummy   

      !set some parameters
      thr     = float(obj%ndpt)*.10
      trlen   = obj%ndpt
      vdti    = 1./obj%vel    !Inverse of Vel*Dt/2
      ttrmax  = obj%Ttrmax    !Maximum allowed traveltime ratio
      ttrmin  = 1./Ttrmax    !Minimum allowed traveltime ratio
      stpr    = Sin(obj%tprang*.0174539)  !Sin of maximum dip
      BIGBIG = Huge(1.0)
      incount = 0

!     Increment the subcube output trace counter ntrout
      Ntrout = obj%isc(1) + obj%NE1sc*(      &
               obj%Isc(2) + obj%NE2sc*(      &
               obj%Isc(3) + obj%NG1sc*       &
               obj%Isc(4)    )) + 1

      !write(iupr,'(2(a,i6))')"Processor ",obj%mycpu,  &
       !                      " Working on Ntroutsc = ", Ntrout
      !write(iupr,*)" "
      !write(iupr,'("452",12i6)')obj%in(:,452)
      !write(iupr,'("565",12i6)')obj%in(:,565)

      !Increment the local output trace counter itr
      itr = itr + 1

      !If input aperture is zero, there is nothing to calculate

      if(obj%MNTOD == obj%nav) then   
         obj%Dout(:,itr) = 0.0                 ! *** newadd 
         Return
      end if

      !Calculate output trace location ego in output e-g coordinates
      ! ego is location of output trace in the output coordinate system.
      ego(1) = obj%Isc(1) * obj%dE1 + obj%E1isc
      ego(2) = obj%Isc(2) * obj%dE2 + obj%E2isc
      go(1)  = obj%Isc(3) * obj%dG1 + obj%G1isc
      go(2)  = obj%Isc(4) * obj%dG2 + obj%G2isc
      if(obj%outype(1:2).ne.'CA') then
         ego(3) = go(1)
         ego(4) = go(2)
      else
         ego(3) = go(1) * Cos(.0174533*go(2))
         ego(4) = go(1) * Sin(.0174533*go(2))
      end if

!!!       write(*,*) ' isc ', obj%Isc(1), obj%Isc(2)

      !Transform output location ego to input e-g coordinates egi
      !egi is ego as represented in the input coordinate system.!
      egi = Matmul(obj%ego2egi,ego)

      !transform output trace location ego to source-receiver coords sr
      sr = Matmul(obj%eg2sr,egi)

      !transform output trace location sr to midpoint-offset coords xf,hf
      xf = (sr(1:2)+sr(3:4))*.5
      hf = (sr(1:2)-sr(3:4))*.5

      !write(iupr,'(a14,i6,a6,4f10.2)')" Output itr= ",itr, " egi= ",egi

      !write(iupr,'(a14,i6,a6,4f10.2,2(a5,2f10.2))')" Output Ntrout= ",Ntrout,&
      !" egi= ",egi, " xf= ",xf," hf= ",hf                ! *** newadd ***

      !This algorithm fails if hf = 0.
      !if hf magnitude is below the minimum, increase it
!
      ahf2 = hf(1)*hf(1) + hf(2)*hf(2)   !amplitude squared of hf
      hmn2 = obj%Hmin*obj%Hmin*2.   !Minimum squared amplitude
!
      if(ahf2 == 0. ) then
        hf(1) = obj%Hmin
        hf(2) = obj%Hmin
        ahf   = obj%Hmin
      elseif(ahf2 < hmn2) then
        hfr = obj%Hmin/sqrt(ahf2)
        hf(1) = hf(1)*hfr
        hf(2) = hf(2)*hfr
        ahf   = obj%Hmin
      else
         ahf   = sqrt(ahf2)
      end if

      !Set mask
      wf  = 0.

      !Find Input Traces That are INSIDE Mapping Aperture, and Compute
      ! Traveltime Ratio, etc.
      
      Ntrib = obj%MNTOD- obj%Nav   !# of contributing traces

      !Loop over input traces
      IndiLoop:  Do indi = 1, Ntrib

        !transform input trace location to midpoint-offset coords
        xi = obj%x(1:2,indi)
        hi(1:2,indi) = obj%h(1:2,indi)
        dxm(1:2,indi) = xi - xf

        !generalized coordinates of input trace
        mh(1:2) = xi
        mh(3:4) = hi(1:2,indi)
        gci = matmul(obj%mh2eg,mh)

        !Gather distance between input and output traces
        dg = egi(3:4) - gci(3:4)

        !Distance between input and output gather must be within bounds
        if(dg(1) < obj%Dg1lb .or. dg(1) > obj%Dg1ub .or.  &
           dg(2) < obj%Dg2lb .or. dg(2) > obj%Dg2ub) CYCLE

        hi_new  = hi(1:2,indi)
        dxm_new = dxm(1:2,indi)

        !Cross product of hi with hf defines aperture
        hif  = (hi(1,indi)*hf(2)  - hi(2,indi)*hf(1))

        !Algorithm fails near hi cross hf = 0.
        !If aperture is too small, enlarge it slightly by adding
        !a small increment to hi perpendicular to hf.

        if(hif == 0. ) then

           !No aperture at all.
           hfr = obj%Hmin/ahf
           hi(1,indi) = hi(1,indi) - hf(2)*hfr
           hi(2,indi) = hi(2,indi) + hf(1)*hfr
           hif  = (hi(1,indi)*hf(2)  - hi(2,indi)*hf(1))
           
        elseif (abs(hif) < obj%Hmin * ahf) then

           !The component of hi normal to hf is too small
           hfr = Sign((obj%Hmin * ahf - abs(hif))/(ahf*ahf),hif)
           hi(1,indi) = hi(1,indi) + hf(2)*hfr
           hi(2,indi) = hi(2,indi) - hf(1)*hfr
           hif  = (hi(1,indi)*hf(2)  - hi(2,indi)*hf(1))

        end if

        !use reciprocity to keep hi x hf from changing sign
        !if (hif < 0.) hi(1:2,indi) = - hi(1:2,indi)

        !If trace is inside aperture, calculate tfti, timax
        Call DMAP3D_Mkout_Par(obj, hi(1:2,indi),hf,dxm(1:2,indi),trlen,vdti, &
            tfti(indi),timax(indi))

        if (tfti(indi) > ttrmin .and. tfti(indi) < ttrmax) then
           wf(indi) = 1.
           incount = incount + 1
        end if

      End Do IndiLoop    !Input trace loop

      obj%closest = 1    
      
      !Find aperture center
      Call DMAP3D_MkOut_FindAppCtr(obj,xf,Ntrib,Inside,Iin,Tc)

       if ( obj%closest > obj%MNTOD) then        
          write(*,*) 'mkout ', obj%closest, Iin, Inside, &
            obj%ntrout,obj%ntotout,obj%ntroutsc
       end if
      
      !If center is not found, output is zero
      If(.Not. Inside) Then

        !(((((((((((((((((((((((((( Print Statement ))))))))))))))))))))))))))

       ! write (iupr,'(a,2i8)')    &
       !    " No aperture found for output point ", ntrout,obj%mycpu
         
       ! write(iupr,'(a14,i6,a6,4f10.2,2(a5,2f10.2))')" Output itr= ",itr, &
       ! " egi= ",egi, " xf= ",xf," hf= ",hf
      
        obj%Dout(1:obj%ndpt,itr) = 0.0
        obj%ndead = obj%ndead + 1

        Return

      End If
  
  dummy = 0
  if ( dummy == 1) then

      !-------------- Added 8/30/01 by RHS -------------------------------------
      !The three input locations surrounding the output location have been 
      !found. If mapping properties vary too drastically among these three 
      !locations, the program may give up and just interpolate the output trace
      !from these three input traces.  The following if-statement prevents 
      !interpolation only when any of the three input locations are virtual.

          If(Iin(2) > 0      .and. Iin(3) > 0 .and.       &
             Iin(2) <= ntrib .and. Iin(3) <= ntrib) then

           !Calculate the cross product of hi and hf for the three nodes
             hifa = hi(1,iin(1))*hf(2) - hi(2,iin(1))*hf(1)
             hifb = hi(1,iin(2))*hf(2) - hi(2,iin(2))*hf(1)
             hifc = hi(1,iin(3))*hf(2) - hi(2,iin(3))*hf(1)

             !The three nodes must be within aperture and
             !must have the same sign hi cross hf and
             !their maximum traveltimes must exceed the threshhold
             if( wf(Iin(1)) == 0. .or. timax(Iin(1)) < thr .or.    &

               wf(Iin(2)) == 0. .or. timax(Iin(2)) < thr .or.    &
               wf(Iin(3)) == 0. .or. timax(Iin(3)) < thr .or.  &
               hifa*hifb   < 0. .or. hifa*hifc     < 0.) then

              !Virtual nodes are replicants of primary node
              !(These if-statements are actually unnecessary with the 8/30/01
              !if-block in place, but I will leave them in for now -- RHS)
              
                if(iin(2) > 0) then
                   iinb = iin(2)
                else
                   iinb = iin(1)
                end if
                if(iin(3) > 0) then
                   iinb = iin(3)
                else
                   iinb = iin(1)
                end if


              !Aperture is too small to reconstruct.  Interpolate instead

 !**********************  check ****************************** 

                  obj%Dout(:,itr) = obj%Dout(:,itr)         +     &
                     obj%din_tmp(:,1) * (1. - tc(1))        +     &
                     obj%din_tmp(:,2) * (tc(1)-tc(2))       +     &
                     obj%din_tmp(:,3) *  tc(2) 
!**********************  check ******************************

 !!               dmax(1) = MaxVal(obj%Din(:,iin(1)))
 !!               dmax(2) = MaxVal(obj%Din(:,iinb))
 !!               dmax(3) = MaxVal(obj%Din(:,iinc))

                dmax(1) = MaxVal(obj%din_tmp(:,iin(1)))
                dmax(2) = MaxVal(obj%din_tmp(:,iinb))
                dmax(3) = MaxVal(obj%din_tmp(:,iinc))

              !(((((((((((((((((((((((((( Print Statement ))))))))))))))))))))
                 write(iupr,'(a,5i8,3e12.4)')                               &
                   "Interpolating ",obj%mycpu,ntrout,Iin,dmax
                 if(wf(Iin(1)) == 0. .or. wf(Iin(2)) == 0. .or.  &
                    wf(Iin(3)) == 0.) then
                    write(iupr,'(a,3f12.4)')                       &
                   " weight factors ",wf(iin(1)),wf(iin(2)),wf(iin(3))
                    obj%ninwf = obj%ninwf + 1
                  end if

                  if( timax(iin(1)) < thr .or. timax(iin(2)) < thr .or.      &
                     timax(iin(3)) < thr ) then
                     write(iupr,'(a,4f11.2)')                 &
                     " timax= ",timax(Iin(1)),timax(Iin(2)),timax(Iin(3)), thr
                          obj%ninti = obj%ninti + 1
                      end if
                      if( hifa*hifb < 0. .or. hifa*hifc < 0. ) then
                           write(iupr,'(a,3e12.4)')"hif= ",hifa,hifb,hifc 
                           obj%ninhi = obj%ninhi + 1
                      end if                     

                      Return         

                  End If

          !This terminates the if-block added 8/30/01
          End If

 end if
  
      obj%nmap = obj%nmap + 1
      
      !Next step depends on anti-aliasing strategy
      If    (obj%aatype(1:2) == 'NO')  Then

         !Perform data mapping with no anti-aliasing
         Call DMAP3D_Mkout_NoAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax)

      ElseIf(obj%aatype(1:1) == 'L')   Then

         !Perform data mapping with a linear anti-aliasing strategy
          Call DMAP3D_Mkout_LinAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax)

      Elseif(obj%aatype(1:2) == 'NE') Then

         !Perform data mapping with a nearest neighbor anti-aliasing strategy
          Call DMAP3D_Mkout_NearAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax, &
          scale_tr)

      Elseif(obj%aatype(1:2) == 'QN') Then

         !Perform data mapping with a nearest neighbor anti-aliasing strategy
          Call DMAP3D_Mkout_QNAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax)

      End If   !AA strategy if-block
      
     ! Write(iupr,'(a6,i6,a5,i5,a8,2i10,a5,3i6)')"trace ",ntrout, &
     !          " cpu ",obj%mycpu," incount ",incount,obj%ticount," iin ",iin
               
  !    Write(iupr,'(a6,i6,a5,i5,a8,2i10,a12,2f12.1,e12.4)')"trace ",ntrout, &
  !             " cpu ",obj%mycpu," incount ",incount,obj%ticount, &
  !             " ego(1:2) ",ego(1)/obj%xwidth, ego(2)/obj%ywidth
             

      End Subroutine DMAP3D_mkout


!!--------------------------- DMAP3D mkout par -------------------------------!!
!!--------------------------- DMAP3D mkout par -------------------------------!!
!!--------------------------- DMAP3D mkout par -------------------------------!!


!******************************************************************************
!
!  DMAP3D_Mkout_Par calculates traveltime ratio and max traveltime
!
!******************************************************************************
   Subroutine DMAP3D_Mkout_Par(obj,hi,hf,dxm,trlen,vdti,tfti,timax)
   

   Implicit None
   
   type (DMAP3D_struct) :: obj
      
   Real, Intent(In)  :: hi(2)       !Input Offset
   Real, Intent(In)  :: hf(2)      !Output Offset
   Real, Intent(In)  :: dxm(2)     !Differential midpoint
   Real, Intent(In)  :: trlen      !Maximum trace length
   Real, Intent(In)  :: vdti   !inverse dimensionless velocity
   Real, Intent(Out) :: tfti          !Traveltime Ratio
   Real, Intent(Out) :: timax        !Maximum traveltime

   real hif, hidx, hfdx, ahif
   real dci, dcf, dcic, dcfc
   real dh(2), dhsq, vdtp, tmr, Small
   
   integer, save  :: dummy =0                               
   
   vdtp = 1.2732395/vdti    !Constant vdtp = 4/(Pi*c*dt/2)
   Small = Epsilon(1.0)*10.     !Define a small number

   !test if new location is inside aperture.
   hif = hi(1)*hf(2)  - hi(2)*hf(1)
   hidx = hi(1)*dxm(2) - hi(2)*dxm(1)
   hfdx = hf(1)*dxm(2) - hf(2)*dxm(1)
   ahif = abs(hif)*.999999
   If (Abs(hidx) >= ahif .or. Abs(hfdx) >= ahif) Then
      tfti = 0.
      timax = 0.
      return
   End if

   !new location is inside aperture. Calculate tfti, timax
   
   dci  =  hfdx / hif      !nu sub i dimensionless coordinate
   dcf  =  hidx / hif      !nu sub f dimensionless coordinate
   dcic  = 1. - dci*dci
   dcfc  = 1. - dcf*dcf

   !traveltime ratio
   tfti = sqrt(dcic/dcfc)

   !maximum time
   dh(1) = dci*dcfc*hf(1) - dcf*dcic*hi(1)
   dh(2) = dci*dcfc*hf(2) - dcf*dcic*hi(2)
   dhsq = (dh(1)*dh(1) + dh(2)*dh(2))
   if (dhsq <= Small) then
      timax = trlen
   else
      tmr = dcfc * Sqrt(dcic/dhsq)
      timax = vdti * ahif * tmr * (1. + vdtp * tmr)
   end if

   timax = min(timax,trlen)

   !write(iupr,'("par",7f10.4)')hi,hf,dxm,tfti,timax

   End Subroutine DMAP3D_Mkout_Par


!!--------------------------- DMAP3D mkout FindAppCtr ------------------------!!
!!--------------------------- DMAP3D mkout FindAppCtr ------------------------!!
!!--------------------------- DMAP3D mkout FindAppCtr ------------------------!!


!******************************************************************************
!
!  FindAppCtr finds the triangle enclosing the aperture center or output point
!     Inputs are
!  obj   -- The DMC structure
!  Ntrib    -- number of input traces to do
!  xf   -- output midpoint
!     Outputs are
!  Inside   -- .False. if no enclosing triangle is found
!       .True. if enclosing triangle is found
!  Iin   -- Indices of enclosing triangle
!  Tc   -- Internal coordinates of aperture center
!
!******************************************************************************

      Subroutine DMAP3D_MkOut_FindAppCtr(obj,xf,Ntrib,Inside,Iin,Tc)

      !Passed Parameters
      Type(DMAP3D_struct)        :: obj
      Real, Intent(In)        :: xf(2)   !The output or center point
      Integer, Intent(In)     :: Ntrib   !Number of input traces in this gather

      Logical, Intent(Out)    :: Inside   !Results of search (see above)
      Integer, Intent(Out)    :: Iin(3)   !Indices of enclosing triangle
      Real, Intent(Out)       :: Tc(2)    !center point in internal coords

      !local variables
      Integer        :: indi, indb, indc
      Integer        :: ndtri, idtri, jdtri
      Real           :: x1(2), x2(2), x3(2), rsq(3)
      Integer, Parameter   :: indpr = 0

      Inside = .False.
      obj%closest = 1

      if(indi == indpr) then
         write(iupr,'(a,2e14.6,2i6)')"Finding aperture Center for ",    &
                                      xf, ntrib, indpr
      end if

      !Look for the aperture center.
      !If absent, output trace is zero.
      !If too narrow, output trace is just an interpolated input trace
      Do indi = 1, Ntrib

        !number of Delaunay triangles for node indi
        ndtri = obj%nnn(indi)

        !midpoint coordinate of node indi
        x1 = obj%x(1:2,indi)

        if(indi == indpr ) then
         write(iupr,'(a,2e14.6,i6)')" pos of indpr = ",x1,ndtri
        end if

        !Define the Delaunay triangles with node indi
        Do idtri = 1, ndtri

           !second triangle vertex has index indb
           indb = obj%in(idtri,indi)
           if(indi == indpr ) then
               write(iupr,'("Indi= ",i6," idtri= ",i6," indb= ",i6)')   &
                  indi,idtri,indb
           end if
           !if(indb < 1 .or. indb > Ntrib) Cycle

           !third triangle vertex has index indc
           jdtri = 1 + Mod(idtri,ndtri)
           indc = obj%in(jdtri,indi)
           if(indi == indpr ) then
               write(iupr,'("Indi= ",i6," jdtri= ",i6," indc= ",i6)')&
                            indi,jdtri,indc
           end if
           !if(indc < 1 .or. indc > Ntrib) Cycle

           !midpoint coordinates of 2dn and 3rd vertices
           x2 = obj%x(:,indi) + obj%dx(:,idtri,indi)
           x3 = obj%x(:,indi) + obj%dx(:,jdtri,indi)

           if(indi == indpr) then
               write(iupr,'("ind= ",2i5," pos= ",4e13.6)')indb,indc,x2,x3
           end if

           !Is output point within this triangle?
           Inside = DMAP3D_util_tstInsideTri(xf,x1,x2,x3,tc)

           if(indi == indpr) then
               write(iupr,'("tc=",2e12.4)')tc
           end if

           If (.not. Inside) Cycle
           !Output point is inside triangle

           !(((((((((((((((((((((((((( Print Statement ))))))))))))))))))))
           if(indi == indpr) then
              write(iupr,'(a,3i6,2f12.1)') &
                 "Output point found inside triangle ",indi,indb,indc,xf
              write(iupr,'(a8,6f12.1)')"locs = ",x1,x2,x3
           end if

           !Triangle is found.  Search no more
           IIn(1) = indi
           Iin(2) = indb
           Iin(3) = indc

           !Store index of closest trace
           rsq(1) = (xf(1)-x1(1))*(xf(1)-x1(1)) + (xf(2)-x1(2))*(xf(2)-x1(2))
           rsq(2) = (xf(1)-x2(1))*(xf(1)-x2(1)) + (xf(2)-x2(2))*(xf(2)-x2(2))
           rsq(3) = (xf(1)-x3(1))*(xf(1)-x3(1)) + (xf(2)-x3(2))*(xf(2)-x3(2))
           if(rsq(3) < rsq(2) .and. rsq(3) < rsq(1)) then
               obj%closest = indc
           elseif (rsq(2) < rsq(1)) then
               obj%closest = indb
           else
               obj%closest = indi
           end if
           Exit

        End Do

        !If triangle has been found, Discontinue search
        If (Inside) then
            Exit
        end if

      End Do

      End Subroutine DMAP3D_MkOut_FindAppCtr

!!--------------------------- DMAP3D mkout NearAA ---------------------------!!
!!--------------------------- DMAP3D mkout NearAA ---------------------------!!
!!--------------------------- DMAP3D mkout NearAA ---------------------------!!


!******************************************************************************
!
!  NearAA does a data mapping with a nearest neighbor anti-aliasing strategy
!     Inputs are
!  obj   -- The DMC structure
!  itr   -- output trace number
!  Ntrib    -- number of input traces to do
!  hi   -- array of input offsets
!  dxm   -- array of differential midpoints
!  hf   -- output offset
!  iin   -- indices of triangle enclosing aperture center
!  wf   -- weight function (set to 1 for traces within aperture)
!  tfti   -- array of traveltime ratios
!  timax   -- array of maximum traveltimes
!
!******************************************************************************

      Subroutine DMAP3D_Mkout_NearAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti, &
                                     timax, scale_tr)

      Implicit None

      integer, parameter  :: itrpr=0
      integer, parameter  :: NTR_STORE = 5

!     Passed Parameters
      Type(DMAP3D_struct)     :: obj
      Integer, Intent(In)     :: itr
      Integer, Intent(In)     :: Ntrib
      real,    Intent(InOut)  :: hi(2,obj%MNTOD)
      real,    Intent(InOut)  :: dxm(2,obj%MNTOD)
      real,    Intent(InOut)  :: wf(obj%MNTOD)     !Weight fn at triangle node
      real,    Intent(In)     :: hf(2)
      Integer, Intent(In)     :: Iin(3)
      real,    Intent(InOut)  :: tfti(obj%MNTOD)   !ttr at triangle node
      real,    Intent(InOut)  :: timax(obj%MNTOD)
      real,    Intent(InOut)  :: scale_tr(obj%MNTOD)
      !local variables
      integer     :: indi, ndtri, idtri, jdtri, inda, indb   
      integer     :: ind(3)
      real        :: hiv(2,obj%nnmax+1),dxv(2,obj%nnmax+1)
      real        :: trv(obj%nnmax+1), tmv(obj%nnmax+1)
      real        :: xvr(2), dxa(2), dxb(2), ad, add, ca, cb
      real        :: hif, hifc
      real        :: Biggest
      real        :: dhiv(2), dhit(2), ddh(2), ddhok(2)

      integer     :: niter, iter, Istatus
      real        :: trlen, vdti, ttrmin, ttrmax
      integer     :: mute(2)

      real        :: dout(obj%ndpt), dfo(obj%ndpt), dmaxi, dmaxo        

      real        :: aout(0:5,10)
      real        :: rmsamp_in(obj%ndpt), rmsamp_out(obj%ndpt)
      real        :: rmsco_in,  rmsco_out      
      real        :: trstore(obj%ndpt,NTR_STORE)
      real        :: dist, min_dist(NTR_STORE)
      integer     :: indx_max(1), numtrc      
      integer     :: np_trc, istart, iend, iw, j  

      !set some parameters
      Biggest = Huge(1.0)
      Niter   = 4
      vdti    = 1./obj%vel        !Inverse of Vel*Dt/2
      ttrmax  = obj%ttrmax        !Maximum allowed traveltime ratio
      ttrmin  = 1./ttrmax        !Minimum allowed traveltime ratio
      trlen   = ttrmax * obj%ndpt       !Maximum trace length
      Aout    = 0.
      dout    = 0.
      trstore   = 0.
      obj%ticount = 0
      np_trc = 0

      !((((((((((((((((((((((((Print Statement)))))))))))))))))))))))))))
      if(obj%ntroutsc == itrpr - 1) then
         write(iupr,'(a12,2i6,a4,2e12.4)')"ntroutsc= ",obj%ntroutsc,    &
                                          obj%ntrout," hf ",hf
         !write(iupr,'("Ind",10x,"dxm",21x,"hi",22x,"tr",10x,"tm")')
      end if

      do iw = 1, ntr_store
         min_dist(iw) = Huge(1.0) - iw
      end do

      !Loop over input traces
      Indi_Loop: Do indi = 1, Ntrib

        !Start with nodes within aperture
        if(wf(indi) < .99999) cycle

!     retrieve the input trace from disk

        if (indi > obj%MNTOD) then          
          write(*,*) 'nearaa > ', indi, obj%ntrout,obj%ntotout, &
               obj%ntroutsc
        end if
       
        if (indi < 1) then          
          write(*,*) 'nearaa < ', indi, obj%ntrout,obj%ntotout, &
             obj%ntroutsc
        end if
              
        if ( indi > obj%MNTOD ) then    
         write(*,*) ' indi > obj%MNTOD in routine near_aa ', indi,obj%MNTOD    
         write(*,*) ' indi > obj%MNTOD in routine near_aa ', indi,obj%MNTOD   
         stop
        end if 

        call dmap3d_get_trace (obj, indi, obj%hd_tmp(:,1:),  &
             obj%din_tmp(:,1:), Istatus)     
     
!       compute the rms gain factor

        if (obj%scale_rms(1:2) /= 'NO') then

          dist = dxm(1,indi)*dxm(1,indi) + dxm(2,indi)*dxm(2,indi)
          indx_max = maxloc(min_dist)          
  
          if (dist < min_dist(indx_max(1)) ) then
            trstore(1:obj%ndpt,indx_max(1)) = obj%din_tmp(1:obj%ndpt,1) 
            min_dist(indx_max(1)) = dist
          end if

        end if

        !((((((((((((((((((((((((Print Statement)))))))))))))))))))))))))))
        if(obj%ntroutsc == itrpr - 1) then
            write(iupr,'(a5,i4,a4,2f12.4,a3,2f12.4)')"indi ",indi, &
                              " dxm",dxm(1:2,indi)," hi",hi(1:2,indi)
        end if

        !compute hi cross hf for this input trace
        hifc = hi(1,indi)*hf(2) - hi(2,indi)*hf(1)

        !number of Delaunay triangles for node indi
        ndtri = obj%nnn(indi)

        !Calculate parameters for Nodes of Voronoi cell surrounding indi
        Idtri_Loop1: Do idtri = 1, ndtri

           inda = obj%in(idtri,indi)      !first Delaunay node

           if(obj%ntroutsc == itrpr - 1) then
            write(iupr,'(2(a6,i6),a4,2f14.2)')"idtri ",idtri, &
                              " inda ",inda," vc ",obj%vc(:,idtri,indi)
           end if

 !           write(iupr,'(a10,2(i4,3x,2f12.4),2f12.4)')'voro1 ', &
 !           idtri,obj%vc(1:2,idtri,indi), &
 !           inda,dxm(1:2,inda),hi(1:2,inda)

           !Default for node coordinates
           dxv(1:2,idtri) = Biggest
           hiv(1:2,idtri) = Biggest

           !If indi is on gather boundary, node may not be valid
           if(abs(obj%vc(1,idtri,indi))==Biggest .or.         &
              abs(obj%vc(2,idtri,indi))==Biggest ) Cycle

           !Differential midpoint coordinate of node
           dxv(1:2,idtri) = dxm(1:2,indi) + .5*obj%vc(1:2,idtri,indi)
           xvr = dxv(1:2,idtri)- dxm(1:2,indi)

           !ideal change in offset to node
           dhit = xvr*obj%target_slope
           ddhok(1) = abs(xvr(1)*obj%tol_slope)
           ddhok(2) = abs(xvr(2)*obj%tol_slope)

           !Obtain node offset coordinate by interpolating Delaunay Triangle
           jdtri = 1 + Mod(idtri,ndtri)

           indb = obj%in(jdtri,indi)     !second Delaunay node

           if (inda < 1 .or. inda > Ntrib .or. indb < 1 .or. indb > Ntrib) then

              !virtual node.  Give the Voronoi node the target offset
              hiv(:,idtri) = hi(:,indi) + dhit

!            write(iupr,'(a10,2(i4,3x,2f12.4),2f12.4)')'virtual1 ', &
!                indi,hi(:,indi), idtri,hiv(:,idtri), dhit
   

           else         !Real node.  Calculate offset from nearby Delaunay pts.

              !node-to-node distances and triangle area
              dxa = dxm(1:2,inda) - dxm(1:2,indi)
              dxb = dxm(1:2,indb) - dxm(1:2,indi)
              ad = dxa(1)*dxb(2) - dxa(2)*dxb(1)     !twice triangle area

              if( ad == 0. ) Then
                 dxv(1:2,idtri) = Biggest
                 Cycle    !Area can't be zero
              end if

              add = 1./ad
              ca = ((dxb(2)-dxa(2))*xvr(1) - (dxb(1)-dxa(1))*xvr(2))*add
              cb = (dxa(1)*xvr(2) - dxa(2)*xvr(1))*add

              !Calculated change in offset to node
              dhiv = (hi(1:2,inda)-hi(1:2,indi))*ca +  &
                     (hi(1:2,indb)-hi(1:2,inda))*cb

              !deviation from ideal
              ddh = dhiv - dhit

              !X-Offset at node idtri
              if (abs(ddh(1)) > ddhok(1)) then
                  dhit(1) = dhit(1) + Sign(ddhok(1),ddh(1))
                  hiv(1,idtri) = hi(1,indi) + dhit(1)
              else
                  hiv(1,idtri) = hi(1,indi) + dhiv(1)
              end if

              !Y-offset at node idtri
              if (abs(ddh(2)) > ddhok(2)) then
                  dhit(2) = dhit(2) + Sign(ddhok(2),ddh(2))
                  hiv(2,idtri) = hi(2,indi) + dhit(2)
                  if(obj%ntroutsc == itrpr - 1) then
                     write(iupr,'(a,2i6,3f12.5)')"clipping offset change", &
                                          indi,idtri,dhiv(2),dhit(2),xvr(2)
                  end if
              else
                  hiv(2,idtri) = hi(2,indi) + dhiv(2)
              end if

            end if

           !Do not allow hi Cross hf to change sign within cell
           hif = hiv(1,idtri)*hf(2) - hiv(2,idtri)*hf(1)
           If(hif*hifc < 0.) Then
               hiv(:,idtri) = hi(:,indi)
               hif = hifc
               if(obj%ntroutsc == itrpr - 1) then
                  write(iupr,'(a,2i6,3f12.5)')"nulling offset change", &
                                       indi,idtri,hiv(:,idtri)
               end if
           End If

           !This loop ensures node is inside aperture
           Do iter = 1, niter

              !Calculate traveltime ratio and max traveltime at node
              Call DMAP3D_mkout_par(obj, hiv(1:2,idtri),hf,dxv(1:2,idtri),&
                  trlen,vdti,trv(idtri),tmv(idtri))

              !If node is inside aperture, exit iter loop
              if (trv(idtri)>ttrmin .and. trv(idtri)<ttrmax) exit

              !Move node coordinates halfway to indi
              dxv(1:2,idtri) = .5*(dxv(1:2,idtri)+dxm(1:2,indi))
              hiv(1:2,idtri) = .5*(hiv(1:2,idtri)+hi (1:2,indi))

           End Do
           
           !If node is still outside aperture, delete it
           if (trv(idtri)<ttrmin .or. trv(idtri)>ttrmax) then
              dxv(1:2,idtri) = Biggest
              hiv(1:2,idtri) = Biggest
           end if

           !((((((((((((((((((((((((Print Statement)))))))))))))))))))))))))))
           if(obj%ntroutsc == itrpr - 1) then
               write(iupr,'(i4,2(a4,2f10.1),a2,2f12.4)')idtri, " dxv", &
               dxv(1:2,idtri)," hiv",hiv(1:2,idtri)," t",trv(idtri),tmv(idtri)
           end if

        End Do Idtri_Loop1           !idtri do-loop

        !set mutes
        mute(1) = obj%hm(indi)
        mute(2) = obj%tm(indi)

        !Cell center
        idtri = ndtri + 1
        dxv(1:2,idtri) = dxm(1:2,indi)
        hiv(1:2,idtri) = hi (1:2,indi)
        trv(idtri) = tfti(indi)
        tmv(idtri) = timax(indi)

        ind(1) = ndtri+1       

        !Calc contribution from each triangle in Voronoi cell
        
        Idtri_Loop2: Do idtri = 1, ndtri

           !Node must lie within aperture
           if(dxv(1,idtri)==Biggest) Cycle

           !triangle is defined by nodes ind(1), ind(2), ind(3)
           jdtri = 1 + Mod(idtri,ndtri)
           ind(2) = idtri
           ind(3) = jdtri

           !Second Node must lie within aperture
           if(dxv(1,jdtri)==Biggest) Cycle

           !(((((((((((((((((((((((((( Print Statement ))))))))))))))))))))
           if(obj%ntroutsc == itrpr - 1) then
            write(iupr,'(a28,3i4,a3,3f8.2)')"calling makemap.ind= " &
                ,ind,"tmv",tmv(ndtri+1),tmv(idtri),tmv(jdtri)
           end if

           !calculate the weight factor contribution from this triangle
       
       if ( Istatus .ne. 0) then     
          write(*,*) ' Warning ** get_trace problem in routine near_aa ',  &
              pcpsx_i_pel(), Istatus, indi,obj%MNTOD,Istatus,idtri,  &
              obj%ntrout,obj%ntotout,obj%ntroutsc
          cycle
       end if 
            
            call DMAP3D_mkout_MakeVST(obj,hf,hiv,dxv,ind,trv,tmv, &
            mute, obj%din_tmp(:,1), obj%hd_tmp(:,1), dout,Aout)
            
 !          call DMAP3D_mkout_MakeVST(obj,hf,hiv,dxv,ind,trv,tmv, &
 !           mute,obj%din(1:obj%ndpt,indi),  &
 !           obj%hdin(1:obj%nwih,indi), dout,Aout)

        End Do Idtri_Loop2

      End Do Indi_Loop   !Input Trace Loop

!      if(obj%ntroutsc == obj%mappr - 1) then
!         dmo = maxval(dout)
!         dmot = maxval(obj%dout(:,itr))
!         write(iupr,'(a6,i6,2(a7,e14.6))')"out= ",obj%mappr," dmot= ",dmot,  &
!                                          " dmo= ",dmo
!      end if

      !Filter output
      Call DMAP3D_mkout_flt(Dout,Dfo,Aout,dmaxi,dmaxo)

      !Add result to output trace
      obj%dout(:,itr) = obj%dout(:,itr) + dfo

      if (obj%scale_rms(1:2) /= 'NO' ) then
       
        do istart = 1, obj%ndpt
          if( obj%dout(istart,itr) /= 0.0) exit
        end do
     
        do iend = obj%ndpt, istart, -1 
          if (obj%dout(iend,itr) == 0.0) cycle  
          exit  
        end do 
    
        if (obj%scale_rms(1:2) == 'CO') then

          do j = 1, NTR_STORE 
            call dmap3d_rms_const(trstore(:,j), istart, iend, scale_tr(j))
          end do
          call median (scale_tr, NTR_STORE, rmsco_in)

          call dmap3d_rms_const(obj%dout(:,itr), istart, iend, rmsco_out)
       
          if (rmsco_out == 0.0) rmsamp_out = 1.0

!!          write(iupr,*)' rmsamp ',obj%ntroutsc, np_trc, rmsco_in, &
!!            rmsco_out, rmsco_in/rmsco_out, itr
       
          obj%dout(1:obj%ndpt,itr) =  &
          obj%dout(1:obj%ndpt,itr) * rmsco_in/rmsco_out

        else

           numtrc = NTR_STORE
           call dmap3d_rms_vary(numtrc, trstore, istart, iend, obj%winlen, &
                obj%ndpt, rmsamp_in)

           numtrc = 1
           call dmap3d_rms_vary(numtrc, obj%dout(:,itr:), istart, iend,  &
                obj%winlen, obj%ndpt, rmsamp_out)

           rmsamp_out = rmsamp_out + 1.e-20

          obj%dout(1:obj%ndpt,itr) =  &
          obj%dout(1:obj%ndpt,itr)*rmsamp_in(1:obj%ndpt)/rmsamp_out(1:obj%ndpt)
      
        end if

      end if 
      
!      if(obj%ntroutsc == obj%mappr - 1) then
!         dmo = maxval(dfo)
!         dmot = maxval(obj%dout(:,itr))
!         write(iupr,'(a6,i6,2(a8,e14.6))')   &
!                   "out= ",obj%mappr," dmaxi= ",dmaxi," dmaxo= ",dmaxo
!         write(iupr,'(a6,i6,2(a7,e14.6))')   &
!                   "out= ",obj%mappr," dfo= ",dmo," dmot= ",dmot
!      end if

      !(((((((((((((((((((((((((     Print Statements )))))))))))))))))))))))))
!  !  write(iupr,'(a8,i6,a8,2e12.4)')"itr=!",itr,"!dmax=!",dmaxi,!dmaxo!
!  !  write(iupr,'(a10,5x,a2,5(9x,a2))')"jto!itout",!!&
!  !  !  !  !  !  !  !  !  !  !  !  !  !"A0","A1","A2","A3","A4","A5"
!  !  Do!jto!=!1,!10
!  !  !  itout!=!jto!*!((obj%ndpt+9)/10)!-!obj%ndpt/20
!  !  !  write(iupr,'(2i5,6f11.5)')jto,itout,Aout(0:5,jto)

      End Subroutine DMAP3D_Mkout_NearAA



!!--------------------------- DMAP3D mkout MakeVST --------------------------!!
!!--------------------------- DMAP3D mkout MakeVST --------------------------!!
!!--------------------------- DMAP3D mkout MakeVST --------------------------!!


! *****************************************************************************
!
!     DMAP3D_mkout_MakeVST generates a traveltime ratio weighting function,
!     assuming that the ratio varies linearly within a Voronoi subtriangle.
!     The traveltime ratios at the triangle vertices are input.
!     Returned is a weight function wttr(ttr) proportional to the density of
!     traveltime ratio ttr, for equally spaced traveltime ratios within the
!     range of ratios encountered.
!
!     If the properties do not vary linearly within the triangle, the
!     triangle is subdivided into two triangles, this process continuing
!     until linearity is reached or the program gets tired.
!
!     The routine keeps track of the subtriangles by means of two stacks. The
!     first stack is a list of properties (ttr,timax,hi,dxm, and WF) calculated
!     at each subtriangle node or vertex.  When a triangle divides, a new node
!     or vertex is added to the node stack.  Another stack (Nd) lists the nodes
!     that belong to each subtriangle. When a subtriangle divides, the node
!     and triangle stacks increases their length by one.
!
!     When the contribution of a subtriangle to the weight function has been
!     made, the length of the triangle stack is decreased by 1.  When the
!     length of the triangle stack drops to zero, the routine is done.
!
!     Logic for the routine is
!     1.  Initialize Stack
!
!     2.  Do (Process Stack)
!
!     2.1   If Stack is not full
!     2.1.1    Test nonlinarity of last subtriangle in stack
!     2.1.2    If nonlinearity is significant then
!     2.1.2.1   Divide last subtriangle in two
!     2.1.2.2   Increment stack
!     2.1.2.3   Cycle Process stack loop (2.)
!     2.1.2    End nonlinearity if-block
!     2.1   End If Stack is not full
!
!     2.2   If maximum traveltime in subtriangle is large enough
!     2.2.1    Calculate weight factor polynomial for subtriangle
!     2.2.2    Add to weight factor polynomial for cell
!     2.2   End maximum traveltime if-block
!
!     2.3   Remove last subtriangle from stack
!     2.4   If stack is empty, Exit Process stack loop (2.)
!
!     2.  End Do (Process Stack)
!
!     Inputs
!  obj  -- the dmc data structure
!  hf  -- output offset
!  hi,dxm  -- input offsets and midpoints at triangle vertices
!  ind(3)  -- indices of the three input triangle vertices
!  ttr  -- array of traveltime ratios at triangle vertices
!  timax  -- array of maximum traveltimes at triangle vertices.
!  mute  -- head and tail mutes for input trace
!  din  -- input trace
!     Outputs
!  dout  -- output trace
!  Aout  -- impulse response at selected time intervals
!     Subroutines called
!  DMAP3D_mkout_MakeDST_Nwf
!
! *****************************************************************************

  Subroutine DMAP3D_mkout_MakeVST(obj,hf,hi,dxm,ind,ttr,timax,mute,  &
          din,hdin,dout,Aout)

   Implicit None
   type (DMAP3D_struct) :: obj
   real, Intent(In) :: timax(:)
   real, Intent(In) :: hf(2)
   real, Intent(In) :: hi(:,:),dxm(:,:)
   real, Intent(In) :: ttr(:)
   integer, Intent(In) :: ind(3)
   integer, Intent(In) :: Mute(2)
   real, Intent(InOut) :: Din(:)
   double precision, Intent(In)    :: hdin(:)
   real, Intent(InOut) :: Dout(:)
   real, Intent(Inout) :: Aout(0:5,10)

   integer  :: np_trc=0   

   Real, Parameter :: third = 1./3.
   Integer, Parameter :: tri_tot = 256, tri_open = 12
   real   :: ttrs(tri_tot), his(2,tri_tot)
   real   :: dxms(2,tri_tot),tims(tri_tot)
   integer  :: nd(3,tri_open)



   real   ::                                      ttl, ttg, ttm  
   integer  ::      i,it1,it2,it3, it4, its, ir(3) 
   real   ::                  hif, hifmin, ahif, area  
   real   :: hidx, hfdx, dcis, dcfs, dcic, dcfc, vdti
   real   :: dxba(2), dxcb(2), dhba(2), dhcb(2), dba(2), dcb(2)
   real   :: wnd, wna(2), amp               

   integer  :: mtiml  


   real   ::      tt12, tt23, tt13       
   real   :: dtm1, dtm2, dtm3
   real   :: dxm1(2), dxm2(2), dxm3(2)
   real   :: dx1sq, dx2sq, dx3sq, dxmsq
   real   :: dxf(2), tc(2)
   Integer  ::     inm, inp 

   Real   :: hic(2), dxmc(2)   
   real   :: dtmx, maxdtmax, maxtims, maxttst


   Logical  :: InsideAll, InsideThis



   real   :: trval(3)   

   real ttrmin2, ttrmax2, trlen, stpr

   integer, save  ::        ncnt=0  
   real   :: tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8


    
   !set taper
   stpr = Sin(obj%tprang*.0174539)

   !set hifmin
   hifmin = obj%hmin * obj%hmin * (hf(1)*hf(1)+hf(2)*hf(2))

   !Is aperture center inside triangle?
   dxf = 0.
   InsideAll = DMAP3D_Util_TstInsideTri(dxf,dxm(1:2,ind(1)),dxm(1:2,ind(2)),  &
    dxm(1:2,ind(3)),tc)

   if( obj%ntroutsc == obj%mappr - 1 .and. InsideAll ) Then
      write(iupr,'(a,3i6)')" Aperture center inside triangle ",ind
   end if

   if(.not. InsideAll) then

      !Unless aperture center is inside triangle, max tt must be >= itmin
      if(  obj%itmin > timax(ind(1)) .and.  &
           obj%itmin > timax(ind(2)) .and.  &
           obj%itmin > timax(ind(3))) Return

      end if

      !inverse of vdt
      vdti = 1./obj%vel

      trlen = obj%ndpt*1.1
      
      !extremal ttr sq
      ttrmax2 = obj%ttrmax*obj%ttrmax
      ttrmin2 = 1./ttrmax2

      !Maximum change in max traveltime allowed in subtriangle
      maxdtmax = 50.

      !Start the traveltime stack (contains ttr, hi,dxm for each node)
      Do i = 1,3
         ttrs(i)   = ttr(    ind(i))
         his(1:2,i)  = hi (1:2,ind(i))
         dxms(1:2,i) = dxm(1:2,ind(i))
         tims(i)   = min(timax(ind(i)),trlen)   !Added 12/01/00
      End Do
        
      it4 = 4        !next loc in tt stack

      !Debug print statement

      If (obj%ntroutsc == obj%mappr - 1) then
          write(iupr,'(a3,5x,a3,2(7x,a4),3x,a4,9x,a2)')  &
            "  i","ttr","dxm1","dxm2","tmax","hi"
         Do i = 1,3
            write(iupr,'(i5,f9.4,2f9.2,3f9.0)')  &
              ind(i),ttr(ind(i)),dxm(1:2,ind(i)),timax(ind(i)),his(:,i)
         End Do
      End If

      !Start the triangle stack (contains vertices of all subtriangles)
      nd(1:3,1) = (/1,2,3/)
      its = 1        !number of triangles in stack

      !2.  Subtriangle stack loop (will continue until triangle stack is empty)
      Do

         !2.1  If stack is full, triangle cannot divide
         If (it4 < tri_tot .and. its < tri_open - 1) Then

           !2.1.1   Change in max traveltime
           dtm1 = Abs(tims(nd(1,its))-tims(nd(2,its)))
           dtm2 = Abs(tims(nd(2,its))-tims(nd(3,its)))
           dtm3 = Abs(tims(nd(3,its))-tims(nd(1,its)))
           dtmx = Max(dtm1, dtm2, dtm3)
           maxtims = Max(tims(nd(1,its)),tims(nd(2,its)),tims(nd(3,its)))

           !Change in midpoint
           dxm1 = dxms(1:2,nd(1,its))-dxms(1:2,nd(2,its))
           dxm2 = dxms(1:2,nd(2,its))-dxms(1:2,nd(3,its))
           dxm3 = dxms(1:2,nd(3,its))-dxms(1:2,nd(1,its))
           dx1sq = dxm1(1)*dxm1(1)+dxm1(2)*dxm1(2)
           dx2sq = dxm2(1)*dxm2(1)+dxm2(2)*dxm2(2)
           dx3sq = dxm3(1)*dxm3(1)+dxm3(2)*dxm3(2)
           dxmsq = max(dx1sq,dx2sq,dx3sq)

           !Is aperture center inside this triangle?
           if(InsideAll) then
              InsideThis = DMAP3D_Util_TstInsideTri(dxf,dxms(1:2,nd(1,its)), &
               dxms(1:2,nd(2,its)), dxms(1:2,nd(3,its)),tc)

              if(InsideThis) then

                  if(obj%ntroutsc == obj%mappr - 1) then
                     write(iupr,'(a,3i6)') &
                       " Aperture center inside triangle ",nd(1:3,its)
                  end if

                 maxtims = trlen
                 dtm1 = Abs(tims(nd(1,its))-trlen)
                 dtm2 = Abs(tims(nd(2,its))-trlen)
                 dtm3 = Abs(tims(nd(3,its))-trlen)
                 dtmx = Max(dtm1, dtm2, dtm3)
              end if

           end if

           maxttst = 50 + .25*maxtims 
                                         
           !2.1.2   Test whether to subdivide
           
 !          If (dtmx > maxttst)   &
 !          write(*,*) ' dtmx > maxttst ', dtmx, maxttst
                          
           If (dtmx > maxttst .and. obj%Nsubt /= 999 ) Then !**new check *****
                        
              !2.1.2.1  Divide subtriangle in two

              !Bisect side with largest change in midpoint
              if (dx1sq == dxmsq) then
                 inm = 1
                 inp = 2
              elseif (dx2sq == dxmsq) then
                 inm = 2
                 inp = 3
              else
                 inm = 3
                 inp = 1
              end if

              !Properties at new node
              his(1:2,it4)  = .5*(his(1:2,nd(inm,its))+his(1:2,nd(inp,its)))
              dxms(1:2,it4) = .5*(dxms(1:2,nd(inm,its))+dxms(1:2,nd(inp,its)))
              call DMAP3D_Mkout_par(obj, his(1:2,it4),hf,dxms(1:2,it4),  &
                   trlen,vdti,ttrs(it4),tims(it4))

              !Debug print statement
              if(obj%ntroutsc == obj%mappr - 1) then
                  write(iupr,'(i5,f9.4,2f9.4,f9.4,2f9.0,3i4)') it4,ttrs(it4), &
                    dxms(1,it4),dxms(2,it4),tims(it4),his(:,it4),inm, its
              end if

              !Update triangle stack
              nd(1:3,its+1)  = nd(1:3,its)
              nd(inm,its+1)  = it4
              nd(1+mod(inm,3),its) = it4

              !2.1.2.  Increment stack lengths
              it4 = it4 + 1       !Increments number of nodes
              its = its + 1   !Increments number of triangles
              
              !2.1.2.3 Cycle the process-stack loop 2.
              Cycle

         End If           !Subdivision test if-block 2.1.2

      Else

         !if (it4 >= tri_tot) then
          !  write(iupr,'(i6,a,3i6,a10,i6)')it4," triangles inside ",ind,   &
          !    " level= ",its
          !           write(iupr,'("dxms",6f12.4)')dxms(1:2,nd(1:3,its))
          !  write(iupr,'("tims",3f12.1)')tims(nd(1:3,its))
         !end if

      End If        !End stack-full if-block 2.1


      !Ready to process subtriangle

      !2.2  Check whether maximum time is large enough to process
      mtiml =nint(max(tims(nd(1,its)),tims(nd(2,its)),tims(nd(3,its))))

       !Debug print statement
       if(obj%ntroutsc == obj%mappr - 1) then
          !write(iupr,'(a4,6i5,5f9.1)') "Prc",  &
          !it4, its, nd(1:3,its), mtiml,tims(nd(1:3,its)),dtmx,maxttst
       end if

      if(mtiml > obj%itmin) then

        !2.2.1   Calculate weight-factor polynomial for this subtriangle

        !Vertices of current subtriangle, sorted in increasing traveltime
        If (ttrs(nd(1,its)) <= ttrs(nd(2,its)) ) then
           if (ttrs(nd(2,its)) <= ttrs(nd(3,its))) then
              ir = (/1,2,3/)
           elseif (ttrs(nd(1,its)) <= ttrs(nd(3,its))) then
              ir = (/1,3,2/)
           else
              ir = (/3,1,2/)
           end if
        Else
           if(ttrs(nd(1,its)) <= ttrs(nd(3,its))) then
              ir = (/2,1,3/)
           elseif (ttrs(nd(2,its)) <= ttrs(nd(3,its))) then
              ir = (/2,3,1/)
           else
              ir = (/3,2,1/)
           end if
        End If

        it1 = nd(ir(1),its)     !Vertex with smallest traveltime ratio
        it2 = nd(ir(2),its)
        it3 = nd(ir(3),its)     !Vertex with largest traveltime ratio

        !Coordinates of triangle center
        hic  = (his(1:2,it1)  + his(1:2,it2)  + his(1:2,it3))*.333333
        dxmc = (dxms(1:2,it1) + dxms(1:2,it2) + dxms(1:2,it3))*.333333

        !Cross products
        hif  =  hic(1)*hf(2)   - hic(2)*hf(1)   
           
        hidx =  hic(1)*dxmc(2) - hic(2)*dxmc(1)
        hfdx =  hf(1) *dxmc(2) - hf(2) *dxmc(1)
        
        ahif =  Abs(hif)                       

        !aperture check
        if(abs(hidx)>=ahif .or. abs(hfdx)>=ahif) then
           amp = 0.
           wna = 0.
        else
        
           dcis   =  hfdx / hif
           dcfs   =  hidx / hif
           dcic  = 1. - dcis*dcis
           dcfc  = 1. - dcfs*dcfs
               
           !Calculate weight factors.
           dxba = dxms(1:2,it2)-dxms(1:2,it1)  !dxm between b and a
           dxcb = dxms(1:2,it3)-dxms(1:2,it2)  !dxm between c and b
           dhba = his (1:2,it2)-his (1:2,it1)  !dxh between b and a
           dhcb = his (1:2,it3)-his (1:2,it2)  !dxh between c and b

           !Triangle area
           Area = .5 * Abs(dxba(1)*dxcb(2)-dxba(2)*dxcb(1))
           
           !Triangle area must be smaller than parallelogram area
           ahif = Max(ahif, .25*Area)
           hif  = Sign(ahif, hif)

           !Calculate the Dj matrices (eq 37)
           dba  = dxba + dcis * dhba
           dcb  = dxcb + dcis * dhcb

           !Normalized wave numbers
           wnd    =    1./(dcfc * hif)
           wna(1) =   (dcis*hf(2)*dcfc - dcfs*hic(2)*dcic)*wnd
           wna(2) = - (dcis*hf(1)*dcfc - dcfs*hic(1)*dcic)*wnd
           
           !amplitude factor
        
            Amp = .159155 /ahif * Abs( Min(AREA_MULTI * Area, (  &
             abs((1.+dcis*dcis)/(dcic * dcfc)  *  &
             (dba(1)*dcb(2) - dba(2)*dcb(1))) - &
             (wna(1)*dhba(1) + wna(2)*dhba(2)) * &
             (hic(2)*dcb(1) - hic(1)*dcb(2))   + &
             (wna(1)*dhcb(1) + wna(2)*dhcb(2)) * &
             (hic(2)*dba(1) - hic(1)*dba(2))     &
             )  )  )

        
            if (obj%calc_med_scale) then
              obj%namp_store = obj%namp_store + 1
              obj%amp_store(obj%namp_store) = amp
              if ( obj%namp_store > obj%MNTOD ) stop
           end if

            if ( amp > obj%amp_median*obj%Afmax ) then
                 amp = obj%amp_median
            end if 

!********   AMO amplitude

!!          Amp1 = Abs(.159155 /ahif * (1.+dcis*dcis)/(dcic * dcfc) )

!*******************************************************************

 !!        if ( np_trc == 0 ) then

        if ( np_trc == -1 ) then

           tmp1 = .159155 /ahif
           tmp2 =  obj%Afmax * Area
           
           tmp3 = (1.+dcis*dcis)/(dcic * dcfc)
           tmp4 = (dba(1)*dcb(2) - dba(2)*dcb(1))
           
           tmp5 = (wna(1)*dhba(1) + wna(2)*dhba(2))
           tmp6 = (hic(2)*dcb(1) - hic(1)*dcb(2))
           
           tmp7 = (wna(1)*dhcb(1) + wna(2)*dhcb(2))
           tmp8 = (hic(2)*dba(1) - hic(1)*dba(2))
                   
      !     write(iupr,933) ahif, area/ahif*100, amp/amp1, amp*1000
 933         format('ahif,area,ratio1+2,amp ',e12.4, 10f12.3)

     !      write(iupr,946) hic,hf,dxmc
 946       format(' h1 ',10e12.4)
  
     !      write(iupr,947) dxms(1:2,it1),dxms(1:2,it2),dxms(1:2,it3)
 947       format(' h2 ',10e12.4)
                                                        
       !    write(iupr,948)  hif,hidx,hfdx,dcis,dcfs,wnd,wna(1),wna(2)
 948         format(' hidx1 ',10e12.4)
 
       !    write(iupr,949) dxba,dxcb,dhba,dhcb,dba,dcb,hic
 949         format(' hidx2 ',10e12.5)
             
      !      write(iupr,950) ahif,area,dcic, dcfc,wnd,ttrs(it3)/ttrs(it1),amp
 950         format('ahif,area,ic,fc,wnd,t3/t1,amp ',2f10.1,4f10.6,e12.5)
             
       !    write(iupr,955) tmp1, tmp2, tmp3*tmp1,tmp1*tmp3*tmp4,tmp5*tmp6,&
       !      tmp7*tmp8, (-tmp5*tmp6+tmp7*tmp8)*tmp1, &
       !      abs((-tmp5*tmp6+tmp7*tmp8)/(tmp3*tmp4))
           
 955       format('ahif ', 7e12.4, f10.2)
!           write(*,*) ' '
           
        end if 
             
        End if

        !weight factor function calculation. First calc extremal ttrs
        ttl = ttrs(it1)
        ttg = ttrs(it3)
        ttm = ttrs(it2)

        !traveltime ratio differences
        tt13 = ttg - ttl
        tt12 = ttm - ttl
        tt23 = ttg - ttm

        !do the map
        trval = (/ttl,ttm,ttg/)
        
        if ( amp > 0.0) then
           call DMAP3D_mkout_Nwf(obj,Din,hdin,Dout,trval,amp,mtiml,stpr, &
              Mute,Aout)
        end if
         
        !obj%ticount = obj%ticount + 1
        !if(obj%ntroutsc == obj%mappr - 1) then
         !dmo = maxval(dout)
         !write(iupr,'(a6,3i6)')"ind= ",ind
         !write(iupr,'(a1,4x,a4,12x,a3,21x,a4,10x,a4)')   &
         !           "i","ttrs","his","dxms","tims"
         !do i=1,it4
         !  write(iupr,'(i4,f9.4,5f12.1)')i,ttrs(i),his(:,i),dxms(:,i),tims(i)
         !end do
         !write(iupr,'(a8,3e14.6)')"offcp ",hif,hidx,hfdx
         !write(iupr,'(a8,4e14.6)')"coord ",dcis, dcfs, dcic, dcfc
         !write(iupr,'("dxdh",4e14.6)')dxba,dxcb,dhba,dhcb
         !write(iupr,'(a8,4e14.6)')"w     ",wnd,wna,area
         !write(iupr,'(a8,4e14.6)')"d     ",dba, dcb
         !write(iupr,'(a4,5i5,f9.4,a5,2e14.6)')"nwf ",it1,it2,it3, &
         !                obj%ticount,mtiml,ttm," amp ",amp, dmo
        !end if
          
      End If     !End of maximum traveltim if-block 2.2

      !2.3  Decrement the triangle stack. If it is empty, quit
      its = its - 1

      !2.4  If stack is empty, quit
      if (its < 1) Exit

   End do            !end of stack do-loop 2.

   end subroutine DMAP3D_mkout_MakeVST

!!--------------------------- DMAP3D mkout Nwf ------------------------------!!
!!--------------------------- DMAP3D mkout Nwf ------------------------------!!
!!--------------------------- DMAP3D mkout Nwf ------------------------------!!


! *****************************************************************************
!
!  DMAP3D_Mkout_Nwf creates an amplitude weight function using the nearest
!  neighbor AA option and calls the data mapping operation
!  The mapping is generally multi-valued, in that a single input time point
!  maps to a range of output time points, the range increasing with traveltime.
!  Among other things, the multi-valued map eliminates aliasing artifacts.
!  Inputs
!     obj      -- the dmc parameter structure
!     tfti(3)  -- Min, Max, Med traveltime ratio for this mapping
!     Amp      -- Mapping amplitude
!     mtim     -- maximum traveltime
!     stpr     -- taper parameter
!     Mute     -- Head and tail mutes for input trace
!     Din      -- input trace
!  Outputs
!     Dout     -- Output trace
!
! *****************************************************************************
       
        
Subroutine DMAP3D_Mkout_Nwf(obj,Din,hdin,Dout,tfti,Amp,mtim,stpr,Mute,Aout)

Implicit None
Type (DMAP3D_struct)    :: obj
Integer, Intent(In)    :: mtim       !maximum traveltime
real, Intent(In)    :: tfti(3)    !ttr's at triangle nodes    

!**************************
real, Intent(Inout)    :: Amp        !mapping amplitude
  !  real, Intent(In)    :: Amp        !mapping amplitude

real, Intent(In)    :: stpr    !taper function
integer, Intent(In)    :: Mute(2)
real, Intent(In)    :: Din(:)
double precision, Intent(In)    :: hdin(:)
real, Intent(InOut)    :: Dout(obj%ndpt)
real, Intent(InOut)    :: Aout(0:5,10)

Integer      :: ndo, ido, jdo
Integer      ::        mtimx 
Real      :: dtrl, dtrr, dtrt     !traveltime ratio intervals
real      :: adrv(2)
Real      :: trr, tr1, tr2
real      :: dmaxin

real      :: wf(obj%lfmax,obj%lfmax)
real      :: wfn, wfo, wft
integer      :: Istatus, npts 


!(((((((((((((((((((((((((   Debug Print Statement    )))))))))))))))))))))))))
!write(iupr,'(a6,i4,a7,2i4,a7,2f9.4,a7,e14.8)')" ndo= ",ndo,  &
!   " timx= ",mtimx, mtimn," tfti= ",tfti(1),tfti(3)," amp= ",wf(1,1)

!    initial wf to zero

     wf = 0.0

!Executables
Istatus = 0

  !   dmaxin = maxval(din)

  if ( hdin(25) <= 1.0e-25) then
     dmaxin = 0.0
  else
     dmaxin = hdin(25)    
  end if

mtimx = mtim

!Is there anything to map?
If(mtimx < obj%itmin .or. dmaxin == 0.) Return

!traveltime ratio intervals

dtrl = tfti(2) - tfti(1)
dtrr = tfti(3) - tfti(2)
dtrt = tfti(3) - tfti(1)

if (dtrl < 0. .or. dtrr < 0. ) then
   write(iupr,*) "negative traveltime interval in mapn", tfti
   return
end if

!Mapping amplitude slopes
           
   
!! if(dtrl > 0. ) then                  

if(dtrl > 0.00075 ) then    
   Adrv(1) = .5*Amp/(dtrl*dtrt)     
else
   Adrv(1) = 0.
end if
  

!!   if(dtrr > 0.0 ) then          

if(dtrr > 0.00075 ) then     
   Adrv(2) = .5*Amp/(dtrr*dtrt)
else
   Adrv(2) = 0.
end if

!Mtimx cannot be longer than the trace length
Mtimx = Min(Mtimx,Mute(2),1+Int((obj%ndpt-1.5)/tfti(3)))

!Each input time value maps to from 1 to Ndo output time values, the
!number increasing approximately linearly with traveltime.
Ndo = int(tfti(3)*mtimx) - int(tfti(1)*mtimx) + 1


!traveltime range and integrated amplitude
wft = .5*Amp
wf(1,1) = wft

  !! Do ido = 2, min(ndo,obj%lfmax)

Do ido = 2, obj%lfmax    

   tr2 = 0.
   trr = dtrt/ido            !ttr interval
   wfn = 0.

   npts = 0

   do jdo = 1, ido
      tr1 = tr2       !First tfti to use
      tr2 = tr2 + trr      !Last tfti to use
      wfo = wfn
      if (tr2 <= dtrl) then
        wfn = tr2 * tr2 * Adrv(1)
      else
        wfn = wft - (dtrt-tr2)*(dtrt-tr2) * Adrv(2)
      end if
      wf(jdo,ido) = wfn - wfo

      npts = npts + 1

   end do

!   if ( maxval(abs(wf(1:npts,ido))) >= 100.) then
!     write(*,*) obj%lfmax,npts,wft,dtrl*dtrt,dtrr*dtrt, Adrv(1:2) 
!     write(*,'(a4,3(8e12.4,/))') ' wf ',wf(1:npts,ido)
!   end if
   
end do


Call DMAP3D_Mkout_Map(mtimx,stpr,mute,tfti,obj%lfmax,wf,obj%ndpt,din,dout, &
                      Aout,obj%min_samp, obj%amp_median, obj%Afmax)

End subroutine DMAP3D_mkout_Nwf


! *****************************************************************************
!
!  This routine performs the actual multi-valued mapping
!
! *****************************************************************************
Subroutine DMAP3D_Mkout_Map(mtimx,stpr,mute,tfti,lfm,wf,ndpt,din,dout,Aout, &
                            min_samp, amp_median, Afmax)

Implicit None
Integer, Intent(InOut) :: mtimx         !maximum input tt  
real,  Intent(In) :: stpr       !taper
integer, intent(in) :: mute(2)      !head and tail mutes
Real,  Intent(In) :: tfti(3)   !min, med, max tt ratio
Integer, Intent(In) :: lfm          !max mapping size
Real,  Intent(In) :: wf(lfm,lfm)       !mapping amplitudes
Integer, Intent(In) :: ndpt       !trace length
Real,  Intent(In) :: Din(ndpt)       !Input trace
real,  Intent(InOut) :: Dout(ndpt)      !Output trace
real,  Intent(InOut) :: Aout(0:5,10)         !Impulse response
Integer, Intent(In) :: min_samp
real,    Intent(In) :: amp_median, Afmax

Integer   :: mtimn      !time to start taper
Integer   :: ndo     !maximum mapping multiplicity
Integer   :: it1, it2        !first time to use
Integer   :: its    !max time for a single valued mapping
Integer   :: itm      !max time for an lfm-valued mapping
Integer   :: itf1, itf2      , ido  !mapping multiplicity
Integer   :: itoinc          
Integer   :: it   
Real   :: tpr, dtpr       !taper
Real   :: wft             !total mapping amplitude
Real   :: rto             !amplitude skewness ratio
Real   :: dil(ndpt)       !local copy of input trace


   
!For impulse response estimation, trace is divided into ten intervals
itoinc = (ndpt+9)/10

 wft = wf(1,1)    
 rto = (tfti(2)-tfti(1))/(tfti(3)-tfti(1))

!Mtimx and Mtimn cannot be longer than the trace length

Mtimn = Mtimx * stpr
Mtimx = Min(Mtimx,Mute(2),1+Int((ndpt-1.5)/tfti(3)))
Mtimn = Min(Mtimn,Mtimx)
it1   = mute(1)

!Each input time value maps to from 1 to Ndo output time values, the
!number increasing approximately linearly with traveltime.
Ndo = int(tfti(3)*mtimx) - int(tfti(1)*mtimx) + 1
   
!Find maximum time for a single-valued mapping
If (Ndo == 1) then

   its = mtimx

Else
   its = (mtimx+ndo/2) / ndo
   itf1 = 1 + int((its-1)*tfti(1))
   itf2 = 1 + int((its-1)*tfti(3))
   ido = itf2 - itf1 + 1
   
   If(ido > 1) then
     if(its > it1) then
      Do it = its - 1, it1, -1
       itf1 = 1 + int((it-1)*tfti(1))
       itf2 = 1 + int((it-1)*tfti(3))
       ido = itf2 - itf1 + 1
              
       if (ido <= 1) then
         its = it
         Exit
       end if
      End Do
     else
      its = it1
     end if
     
   Else if(ido <= 1) then
     Do it = its, mtimx
       itf1 = 1 + int((it-1)*tfti(1))
       itf2 = 1 + int((it-1)*tfti(3))
       ido = itf2 - itf1 + 1
       
       if (ido > 1) then
         its = it - 1
         Exit
       end if
     End Do
   End if

End if

!Maximum filter length may be exceeded at large times.
If (Ndo > lfm ) then

   !Find maximum time for which filter length will not be exceeded
   itm = (mtimx * lfm)/ndo
   itm = Min(itm,mtimx)
   itf1 = 1 + int((itm-1)*tfti(1))
   itf2 = 1 + int((itm-1)*tfti(3))
   ido = itf2 - itf1 + 1
   If(ido > lfm) then
     if(itm > it1) then
       Do it = itm - 1, it1, -1
         itf1 = 1 + int((it-1)*tfti(1))
         itf2 = 1 + int((it-1)*tfti(3))
         ido = itf2 - itf1 + 1
         if (ido <= lfm) then
           itm = it
           Exit
         end if
       End Do
     else
       itm = it1
     end if

   Else if(ido <= lfm) then
     Do it = itm, mtimx
       itf1 = 1 + int((it-1)*tfti(1))
       itf2 = 1 + int((it-1)*tfti(3))
       ido = itf2 - itf1 + 1
       if (ido > lfm) then
         itm = it - 1
         Exit
       end if
     End Do
   End if
Else
   itm = mtimx
end if

!(((((((((((((((((((((((((   Debug Print Statement    )))))))))))))))))))))))))
!write(iupr,'(a6,i4,a7,2i4,a7,2f9.4,a7,e14.8)')" ndo= ",ndo,  &
!   " timx= ",mtimx, mtimn," tfti= ",tfti(1),tfti(3)," amp= ",wf(1,1)


!Calculate effect of mapping on flat event
!itout = - itoinc/2
!do jto = 1, 10
!   itout = itout + itoinc
!   if(itout < it1) Cycle
!   if(itout > mtimx) Exit
!   itf1 = 1 + int((itout-1)*tfti(1)) - Itout
!   itf2 = 1 + int((itout-1)*tfti(3)) - Itout
!   ido = itf2 - itf1 + 1
!   if (ido > lfm) then
!      itfm = 1 + int((itout-1)*tfti(2)) - Itout
!      itf1 = itfm - int(Float((itfm-itf1)*lfm)/Float(ido))
!      itf2 = itf1 + lfm - 1
!      ido = lfm
!   end if
!   it = itf1
!   Do jdo = 1, ido
!      itf = abs(it)
!      it = it + 1
!      if(itf < 6) Aout(itf,jto) = Aout(itf,jto) + wf(jdo,ido)
!   End Do

       
   !((((((((((((((((((((((   Print Statement     )))))))))))))))))))))))))))
!   write(iupr,'("Aout",i4,6e12.4)')jto,Aout(0:5,jto)
!end do

!Taper the end of the input trace

dil(1:mtimn) = din(1:mtimn)
if(mtimx > mtimn) then
   tpr = 1.
   dtpr = 1./(mtimx-mtimn+1)
   Do it = mtimn+1, mtimx
      tpr = tpr - dtpr
      Dil(it) = Din(it) * tpr
   End Do
End if
            
!Perform the mapping.  First single valued
   
if ((its-it1) > min_samp ) then   
  
  Do it = it1, its
   itf1 = 1 + int((it-1)*tfti(2))   
   if ( itf1 > ndpt .or. itf1 < mute(1)) Exit     
   dout(itf1) = dout(itf1) + dil(it)*wft      
  End Do

end if 


If(its >= mtimx) Return

!Next multi-valued up to lfm

it2 = Min(itm,mtimx)
     
if ((it2-(its+1)) > min_samp ) then
 
  Do it = its+1, it2
   itf1 = 1 + int((it-1)*tfti(1))
   itf2 = 1 + int((it-1)*tfti(3))
   
 
   if ( itf1 >= ndpt ) cycle
   if ( itf2 > ndpt ) itf2 = ndpt      

   ido = 1 + itf2 - itf1
   ido = min(ido, lfm)
        
   itf2 = min(ndpt, (itf1+ido-1) )   
   ido = 1 + itf2 - itf1

   dout(itf1:itf2) = dout(itf1:itf2) + dil(it) * wf(1:ido,ido)
   
  End Do
end if
  
if(it2 >= mtimx) Return

!Finally, lfm valued

if ((mtimx-it2) > min_samp ) then

  Do it = it2, mtimx
   itf1 = 1 + int((it-1)*tfti(2)-rto*(lfm-1))
   itf2 = itf1 + lfm - 1

   if ( itf1 >= ndpt ) cycle
   if ( itf2 > ndpt ) itf2 = ndpt      !***  check

   
   ido = 1 + itf2 - itf1
   ido = min(ido, lfm)
        
   itf2 = min(ndpt, (itf1+ido-1) )   ! *** newadd
   ido = 1 + itf2 - itf1

   dout(itf1:itf2) = dout(itf1:itf2) + dil(it) * wf(1:ido,ido)
 
  End Do


end if

End Subroutine DMAP3D_Mkout_Map


!!--------------------------- DMAP3D mkout Lwf ------------------------------!!
!!--------------------------- DMAP3D mkout Lwf ------------------------------!!
!!--------------------------- DMAP3D mkout Lwf ------------------------------!!


! *****************************************************************************
!
!  DMAP3D_Mkout_Lwf creates an amplitude weight function using the linear AA
!  option and calls the data mapping operation
!  The mapping is generally multi-valued, in that a single input time point
!  maps to a range of output time points, the range increasing with traveltime.
!  Among other things, the multi-valued map eliminates aliasing artifacts.
!  Inputs
!     obj      -- the dmc parameter structure
!     Din      -- the input trace
!     tfti(3)  -- Minimum, Median and maximum traveltime ratio for this mapping
!     Adrv     -- set of first and second derivatives for the ttr weight fn.
!    The weight function is for tfti(1) < t < tfti(2)
!    W(t) = (t-tfti(1))*(2.*Adrv(1)+3.*(t-tfti(1))*Adrv(3)),
!    and for tfti(2) < t < tfti(3)
!    W(t) = (tfti(3)-t)*(2.*Adrv(2)+3.*(tfti(3)-t)*Adrv(4)),
!     lstk     -- number of weight function polynomials
!     mtim     -- set of maximum traveltimes
!     stpr     -- taper parameter
!  Outputs
!     obj%dout      -- Output trace
!
! *****************************************************************************
Subroutine DMAP3D_MkOut_Lwf(obj,Din,Dout,tfti,Adrv,mtim,stpr,Mute,Aout)

Implicit None
Type (DMAP3D_struct)    :: obj
Integer, Intent(In)    :: mtim       !maximum traveltime
real, Intent(In)    :: tfti(3)         !ttr's at triangle nodes
real, Intent(In)    :: Adrv(4)        !mapping amplitude
real, Intent(In)    :: stpr    !taper function
integer, Intent(In)    :: Mute(2)
real, Intent(In)    :: Din(*)
real, Intent(InOut)    :: Dout(*)
real, Intent(InOut)    :: Aout(0:5,10)

Integer      :: ndo, ido, jdo
Integer      :: mtimx

real      :: dmaxin

real      :: tr1, tr2, trb, trr
real      :: wf(obj%lfmax, obj%lfmax)
real      :: wft, wfo, wfn
integer      :: Istatus

real      :: dtrl, dtrr, dtrt

!Executables
Istatus = 0

mtimx = mtim
dmaxin = maxval(din(1:obj%ndpt))

!Is there anything to map?
If(mtimx < obj%itmin .or. dmaxin == 0.) Return

!traveltime ratio intervals
dtrl = tfti(2) - tfti(1)
dtrr = tfti(3) - tfti(2)
dtrt = tfti(3) - tfti(1)
if (dtrl < 0. .or. dtrr < 0. ) then
!   write(iupr,*) "negative traveltime interval in mapn", tfti
   return
end if

!Mtimx cannot be longer than the trace length
Mtimx = Min(Mtimx,Mute(2),1+Int((obj%ndpt-1.5)/tfti(3)))

!Each input time value maps to from 1 to Ndo output time values, the
!number increasing approximately linearly with traveltime.
Ndo = nint(tfti(3)*mtimx) - nint(tfti(1)*mtimx) + 1

!total integrated output
wft = dtrl*dtrl*(Adrv(1) + Adrv(3)*dtrl) + &
      dtrr*dtrr*(Adrv(2) + Adrv(4)*dtrr)

!traveltime range and integrated amplitude
wf(1,1) = wft
Do ido = 2, min(ndo,obj%lfmax)
   tr2 = 0.
   trr = dtrt/ido            !ttr interval
   wfn = 0.
   do jdo = 1, ido
      tr1 = tr2       !First tfti to use
      tr2 = tr2 + trr      !Last tfti to use
      wfo = wfn
      if (tr2 <= dtrl) then
  wfn = tr2 * tr2 * (Adrv(1) + tr2 * Adrv(3))
      else
  trb = dtrt - tr2
  wfn = wft - trb * trb * (Adrv(2) + trb * Adrv(4))
      end if
      wf(jdo,ido) = wfn - wfo
   end do
end do

Call DMAP3D_Mkout_Map(mtimx,stpr,mute,tfti,obj%lfmax,wf,obj%ndpt,din,dout, &
                      Aout,obj%min_samp, obj%amp_median, obj%Afmax)

End subroutine DMAP3D_MkOut_Lwf



!!--------------------------- DMAP3D mkout Qwf ------------------------------!!
!!--------------------------- DMAP3D mkout Qwf ------------------------------!!
!!--------------------------- DMAP3D mkout Qwf ------------------------------!!


! *****************************************************************************
!
!  DMAP3D_Mkout_Qwf creates an amplitude weight function using a Nearest
!  Neighbor AA option and calls the data mapping operation
!  The mapping is generally multi-valued, in that a single input time point
!  maps to a range of output time points, the range increasing with traveltime.
!  Among other things, the multi-valued map eliminates aliasing artifacts.
!  Inputs
!     obj      -- the dmc parameter structure
!     tfti(2)  -- Min, Max traveltime ratio for this mapping
!     Amp      -- Mapping amplitude
!     mtim     -- maximum traveltime
!     stpr     -- taper parameter
!     Mute     -- Head and tail mutes for input trace
!     Din      -- input trace
!  Outputs
!     Dout     -- Output trace
!
! *****************************************************************************
Subroutine DMAP3D_Mkout_Qwf(obj,Din,Dout,tfti,Amp,mtim,stpr,Mute,Aout)

Implicit None
Type (DMAP3D_struct)    :: obj
Integer, Intent(In)    :: mtim       !maximum traveltime
real, Intent(In)    :: tfti(3)         !ttr's at triangle nodes
real, Intent(In)    :: Amp        !mapping amplitude
real, Intent(In)    :: stpr    !taper function
integer, Intent(In)    :: Mute(2)
real, Intent(In)    :: Din(*)
real, Intent(InOut)    :: Dout(*)
real, Intent(InOut)    :: Aout(0:5,10)

Integer      :: ido
Integer      :: mtimx
Real      :: dtrt       !traveltime ratio interval

real      :: dmaxin
real      :: wf(obj%lfmax,obj%lfmax), wft
integer      :: Istatus

!Set some values
Istatus = 0

dmaxin = maxval(din(1:obj%ndpt))
mtimx = mtim

!Is there anything to map?
If(mtimx < obj%itmin .or. dmaxin == 0.) Return

!traveltime ratio intervals
dtrt = tfti(2) - tfti(1)
if (dtrt < 0. ) then
!   write(iupr,*) "negative traveltime interval in mapq", tfti
   return
end if

!traveltime range and integrated amplitude
wft = .5*Amp
Do ido = 1, obj%lfmax
   wf(1:ido,ido) = wft/ido
End Do
 

 Call DMAP3D_Mkout_Map(mtimx,stpr,mute,tfti,obj%lfmax,wf,obj%ndpt,din,dout, &
                       Aout, obj%min_samp, obj%amp_median, obj%Afmax)

End subroutine DMAP3D_mkout_Qwf


! *****************************************************************************
!
!  This routine performs the actual multi-valued mapping
!
! *****************************************************************************
Subroutine DMAP3D_Mkout_Map2(mtimx,stpr,mute,tfti,lfm,wf,ndpt,din,dout,Aout)

Implicit None
Integer, Intent(InOut) :: mtimx         !maximum input tt
real,  Intent(In) :: stpr       !taper
integer, intent(in) :: mute(2)      !head and tail mutes
Real,  Intent(In) :: tfti(3)   !min, med, max tt ratio
Integer, Intent(In) :: lfm          !max mapping size
Real,  Intent(In) :: wf(lfm,lfm)       !mapping amplitudes
Integer, Intent(In) :: ndpt       !trace length
Real,  Intent(In) :: Din(ndpt)       !Input trace
real,  Intent(InOut) :: Dout(ndpt)      !Output trace
real,  Intent(InOut) :: Aout(0:5,10)         !Impulse response

Integer   :: mtimn      !time to start taper
Integer   :: ndo     !maximum mapping multiplicity
Integer   :: it1, it2        !first time to use
Integer   :: its    !max time for a single valued mapping
Integer   :: itm      !max time for an lfm-valued mapping
Integer   :: itf1, itf2, itfm, ido, jdo   !mapping multiplicity
Integer   :: itoinc, itout, jto
Integer   :: it, itf
Real   :: tpr, dtpr      !taper
Real   :: wft          !total mapping amplitude
Real   :: rto         !amplitude skewness ratio
Real   :: dil(ndpt)       !local copy of input trace
Real   :: b, bi, tin     , ti1, tis 
Integer     :: itn, itp, istd, itd1, itds

!For impulse response estimation, trace is divided into ten intervals
itoinc = (ndpt+9)/10

wft = wf(1,1)
rto = (tfti(2)-tfti(1))/(tfti(3)-tfti(1))

!Mtimx and Mtimn cannot be longer than the trace length
Mtimn = Mtimx * stpr
Mtimx = Min(Mtimx,Mute(2),1+Int((ndpt-1.5)/tfti(3)))
Mtimn = Min(Mtimn,Mtimx)
it1   = mute(1)

!Each input time value maps to from 1 to Ndo output time values, the
!number increasing approximately linearly with traveltime.
Ndo = nint(tfti(3)*mtimx) - nint(tfti(1)*mtimx) + 1

!Find maximum time for a single-valued mapping
If (Ndo == 1) then

   its = mtimx

Else
   its = (mtimx+ndo/2) / ndo
   itf1 = 1 + Nint((its-1)*tfti(1))
   itf2 = 1 + Nint((its-1)*tfti(3))
   ido = itf2 - itf1 + 1
   If(ido > 1) then
      if(its > it1) then
  Do it = its - 1, it1, -1
     itf1 = 1 + Nint((it-1)*tfti(1))
     itf2 = 1 + Nint((it-1)*tfti(3))
     ido = itf2 - itf1 + 1
     if (ido <= 1) then
        its = it
        Exit
     end if
  End Do
      else
  its = it1
      end if
   Elseif(ido <= 1) then
      Do it = its, mtimx
  itf1 = 1 + Nint((it-1)*tfti(1))
  itf2 = 1 + Nint((it-1)*tfti(3))
  ido = itf2 - itf1 + 1
  if (ido > 1) then
     its = it - 1
     Exit
  end if
      End Do
   End if

End if

!Maximum filter length may be exceeded at large times.
If (Ndo > lfm ) then

   !Find maximum time for which filter length will not be exceeded
   itm = (mtimx * lfm)/ndo
   itm = Min(itm,mtimx)
   itf1 = 1 + Nint((itm-1)*tfti(1))
   itf2 = 1 + Nint((itm-1)*tfti(3))
   ido = itf2 - itf1 + 1
   If(ido > lfm) then
      if(itm > it1) then
  Do it = itm - 1, it1, -1
     itf1 = 1 + Nint((it-1)*tfti(1))
     itf2 = 1 + Nint((it-1)*tfti(3))
     ido = itf2 - itf1 + 1
     if (ido <= lfm) then
        itm = it
        Exit
     end if
  End Do
      else
  itm = it1
      end if

   Elseif(ido <= lfm) then
      Do it = itm, mtimx
  itf1 = 1 + Nint((it-1)*tfti(1))
  itf2 = 1 + Nint((it-1)*tfti(3))
  ido = itf2 - itf1 + 1
  if (ido > lfm) then
     itm = it - 1
     Exit
  end if
      End Do
   End if
Else
   itm = mtimx
end if

!(((((((((((((((((((((((((   Debug Print Statement    )))))))))))))))))))))))))
!write(iupr,'(a6,i4,a7,2i4,a7,2f9.4,a7,e14.8)')" ndo= ",ndo,  &
!   " timx= ",mtimx, mtimn," tfti= ",tfti(1),tfti(3)," amp= ",wf(1,1)

!Calculate effect of mapping on flat event
itout = - itoinc/2
do jto = 1, 10
   itout = itout + itoinc
   if(itout < it1) Cycle
   if(itout > mtimx) Exit
   itf1 = 1 + Nint((itout-1)*tfti(1)) - Itout
   itf2 = 1 + Nint((itout-1)*tfti(3)) - Itout
   ido = itf2 - itf1 + 1
   if (ido > lfm) then
      itfm = 1 + Nint((itout-1)*tfti(2)) - Itout
      itf1 = itfm - Nint(Float((itfm-itf1)*lfm)/Float(ido))
      itf2 = itf1 + lfm - 1
      ido = lfm
   end if
   it = itf1
   Do jdo = 1, ido
      itf = abs(it)
      it = it + 1
      if(itf < 6) Aout(itf,jto) = Aout(itf,jto) + wf(jdo,ido)
   End Do

   !((((((((((((((((((((((   Print Statement     )))))))))))))))))))))))))))
!   write(iupr,'("Aout",i4,6e12.4)')jto,Aout(0:5,jto)
end do

!Taper the end of the input trace
dil(1:mtimn) = din(1:mtimn)
if(mtimx > mtimn) then
   tpr = 1.
   dtpr = 1./(mtimx-mtimn+1)
   Do it = mtimn+1, mtimx
      tpr = tpr - dtpr
      Dil(it) = Din(it) * tpr
   End Do
End if

!Perform the mapping.  First single valued
b = tfti(2) - 1.     !proportional to distance between ti and tf
ti1 = Float(it1-1)   !lower bound to ti
tis = Float(its-1)   !upper bound to ti
itd1 = nint(ti1*b)   !number of time samples between ti and tf at lower bound
itds = nint(tis*b)   !number of time samples between ti and tf at upper bound

!if distance between ti and tf is constant over interval, do a block mpy/add
if (itds == itd1) then
   dout(it1+itds:its+itds) = dout(it1+itds:its+itds) + wft * dil(it1:its)
else

   !distance varies over interval.  subdivide
   bi = abs(1./b)   !subinterval size
   itp = it1   !time at bottom of subinterval

   !distance between ti and tf may be positive or negative
   if(b < 0.) then
      istd = -1
   else
      istd = 1
   end if

   !loop over subintervals, do block mpy/add in each subinterval
   tin = bi * (float(abs(itd1)) - .5)
   Do it = itd1, itds, istd
      tin = tin + bi
      itn = min(int(tin) + 1,its)   !subinterval upper boundary
      dout(itp+it:itn+it) = dout(itp+it:itn+it) + wft * dil(itp:itn)
      itp = itn
   End Do

end if

If(its >= mtimx) Return

!Next multi-valued up to lfm
it2 = Min(itm,mtimx)
Do it = its+1, it2
   itf1 = 1 + Nint((it-1)*tfti(1))
   itf2 = 1 + Nint((it-1)*tfti(3))
   ido = 1 + itf2 - itf1
   dout(itf1:itf2) = dout(itf1:itf2) + dil(it)*wf(1:ido,ido)
End Do
if(it2 >= mtimx) Return

!Finally, lfm valued
Do it = it2, mtimx
   itf1 = 1 + nint((it-1)*tfti(2)-rto*(lfm-1))
   itf2 = itf1 + lfm - 1
   dout(itf1:itf2) = dout(itf1:itf2) + dil(it)*wf(1:ido,ido)
End Do

End Subroutine DMAP3D_Mkout_Map2
!!--------------------------- DMAP3D mkout map sumover ----------------------!!
!!--------------------------- DMAP3D mkout map sumover ----------------------!!
!!--------------------------- DMAP3D mkout map sumover ----------------------!!


!***************************************************************************
!
!  SUMOVER integrates a peicewise polynomial function over an interval
!
!  Input
!     ti,tf    -- integration interval
!     lstk     -- Number of interval end points in master function
!     tstk     -- A list of interval end points
!     wstk     -- A set of polynomial coefficients for each interval
!     wi       -- integrals over intervals
!
!  Output
!     wsum     -- Result of the integration
!
!  Local Parameters
!     lwf      -- Number of polynomial coefficients in cwf
!     istk,iwf -- counters
!     i1,i2    -- position of integration interval end points in tstk
!     wfd      -- {1,1/2,1/3,...}
!     ta,tb    -- interval boundaries
!     tpa,tpb  -- interval boundaries to some power
!
!  Subroutines called
!     DMAP3D_util_place
!
!***************************************************************************
   Subroutine DMAP3D_mkout_map_sumover(ti,tf,lstk,tstk,wstk,wi,wsum)

   Implicit None

   !Passed parameters
   Real,Intent(In)       :: ti, tf
   Integer,Intent(In)       :: lstk
   Real, Intent(In)       :: tstk(:),wstk(:,:),wi(:)
   Real, Intent(Out)       :: wsum

   !Local parameters

   Integer        :: i1, i2
   Real         :: ta     , tb   

   !Executables
   wsum = 0.

   if(tf <= tstk(1) .or. ti >= tstk(lstk)) Return

   !Find place for ti, tf in stack
   Call DMAP3D_util_place(ti,tstk,lstk,i1)
   Call DMAP3D_util_place(tf,tstk,lstk,i2)

   !reset i1 and i2 to last nodes before ti and tf
   i1 = max(i1-1,1)
   i2 = min(i2-1,lstk)

   !incremental coordinates
   ta = max(ti,tstk(1))    - tstk(i1)
   tb = min(tf,tstk(lstk)) - tstk(i2)

   wsum = wi(i2) - wi(i1)  &
      + tb*(wstk(1,i2)+tb*(.5*wstk(2,i2)+.3333333*tb*wstk(3,i2))) &
      - ta*(wstk(1,i1)+ta*(.5*wstk(2,i1)+.3333333*ta*wstk(3,i1)))

   End Subroutine DMAP3D_mkout_map_sumover

!******************************************************************************
!
!     Subdiv -- Subdivides the output hypercube
!     Output trace i in the output hypercube has coordinates (ie1,ie2,ig1,ig2)
!     where i = ie1 + ne1 * (ie2 + ne2 * (ig1 + ng1 * ig2)),
!     with i=0,...ne1*ne2*ng1*ng2-1, ie1=0,...ne1-1, etc.
!     The output hypercube is divided into ncpu subcubes, each assigned to a
!     cpu.  In three of the dimensions, the number of subcubes is fixed.  In
!     the fourth, the number of subcubes is slightly variable.
!     Outputs are object elements
!        nsc(1:5) --number of subcubes in each dimension
!        lnsc(1:5)--length of subcubes in each dimension
!        rmsc(1:5)--remainder: first rmsc subcubes will have length lnsc+1
!        jmsc     --the special direction
!        rmjm     --special remainder:rmjm=numcpu - nsc(1)*nsc(2)*nsc(3)*nsc(4)
!     The fifth dimension in nsc,lnsc,rmsc holds a second value for the special
!     dimension.  In general, nsc(1)*nsc(2)*nsc(3)*nsc(4)<numcpu.  To assign
!     a subcell to every available processor, nsc(jmsc) varies slightly.
!     Specifically, For the first rmjm values of the other three subcell
!     coordinates, nsc(jmsc) -> nsc(jmsc)+1, and lnsc(jmsc), rmsc(jmsc) are
!     adjusted accordingly.
!
!
!******************************************************************************
Subroutine DMAP3D_Subdiv(obj,istatus)

implicit none
type (DMAP3D_struct) obj
integer, intent(out)    :: istatus

!locals
integer           :: ntot   !Number of output traces
Real              :: topp  !floating point estimate of traces per processor
integer           :: nde   !Number of dimensions in hypercube of lenght > 1
real              :: size  !floating point estimate of subcell length

real              :: size_eg(4)  !floating point estimate of subcell length

real              :: ssc(4)!estimate of length in each dimension
real              :: rl(4) !approximate number of subcells in each dimension
integer           :: npa   !approximate number of subcubes
integer           :: npera !# subcubes per jmsc value
integer           :: j
integer           :: nscad
integer           :: neg(4)
integer           :: lnmn, jm
integer           :: numproc

if( obj%numcpu <= 0 .or. &
    obj%ne1    <= 0 .or. &
    obj%ne2    <= 0 .or. &
    obj%ng1    <= 0 .or. &
    obj%ng2    <= 0 )   Then
    Istatus = 1
    write(iupr,*)"Faulty input parameters to SubDiv"
    Return
else
    istatus = 0
end if

!total number of output traces
ntot = obj%ne1*obj%ne2*obj%ng1*obj%ng2     !Number of output traces

!single output trace--one subcube
if(ntot == 1) then
   obj%nsc  = 1   !one subcube in each direction
   obj%lnsc = 1   !subcube length is one
   obj%rmsc = 0   !no remainder
   obj%jmsc = 1   !special direction might as well be one
   obj%rmjm = 0   !no special remainder
   return
end if

neg(1) = obj%ne1
neg(2) = obj%ne2
neg(3) = obj%ng1
neg(4) = obj%ng2

!Number of working processors
!numproc = max(1,obj%numcpu-1)
numproc = obj%numcpu

if(numproc > ntot) then
   write(iupr,*)"More processors than output points"
   istatus = 1
   Return
elseif(numproc <=0) then
   write(iupr,*)"No processors assigned"
   Istatus = 1
   return
end if

!Number of traces per processor
topp = Float(ntot)/Float(numproc)

!Effective dimensionality of hypercube
nde = 0

!!!     Do j = 1, 4         ! ***** newchange
 
Do j = 1, 2                  
   if(neg(j)>1) nde = nde + 1
End Do

!Number of traces/subcube/dim
! size = topp**(1./float(nde))

   size = topp

   call DMAP3D_adjust_size(neg(1), neg(2), size, size_eg(1), size_eg(2))
   call DMAP3D_adjust_size(neg(3), neg(4), size, size_eg(3), size_eg(4))
      
!First estimate of size, number of subcubes
npa = 1                                !# processors assigned so far
Do j = 1, 4
   ssc(j)       = min(float(neg(j)),size_eg(j))    !# traces/subcube in dim j
   rl(j)        = float(neg(j))/ssc(j)       !# subcubes in dimension j
   obj%nsc(j)   = int(rl(j))                 !integer # subcubes
   obj%lnsc(j)  = neg(j)/obj%nsc(j)               !length of subcube in dim j
   obj%rmsc(j)  = neg(j) - obj%lnsc(j)*obj%nsc(j) !remainder
   npa          = npa * obj%nsc(j)                !# subcubes

!   write(*,*) obj%mycpu, j,neg(j),size_eg(j),ssc(j),rl(j), &
!              obj%nsc(j),obj%lnsc(j), obj%rmsc(j),npa, numproc
 
End do

if ( numproc - npa < min(obj%nsc(1),obj%nsc(2)) ) then   
   numproc = min( npa, obj%numcpu) 
end if
 
!npa cannot be greater than the number of available processors
Do
   if (npa <= numproc ) Exit

   !find direction with smallest subcube size
   lnmn = ntot
   Do j = 1, 4
      if (obj%nsc(j) == 1 ) cycle
      if (lnmn > obj%lnsc(j)) then
         lnmn = obj%lnsc(j)
         jm   = j
      end if
   End Do

   !Reduce the number of subcubes in this direction by one
   npa = npa / obj%nsc(jm)
   obj%nsc( jm) = obj%nsc(jm) -1
   obj%lnsc(jm) = neg(jm)/obj%nsc(jm)
   obj%rmsc(jm) = neg(jm) - obj%lnsc(jm)*obj%nsc(jm)
   npa = npa * obj%nsc(jm)

End Do

!Find largest subcube dimension (first pass)
obj%jmsc = 1                                  !Largest dimension
Do j = 2,4
   if (obj%lnsc(j) > obj%lnsc(obj%jmsc)) obj%jmsc=j
End Do

!Find size of hypercolumns normal to longest direction
npera = 1                              !# subcubes orthogonal to jmsc
Do j = 1,4
   if (j==obj%jmsc) cycle
   npera = npera * obj%nsc(j)
End Do

!Add hypercolumns of subcubes as necessary
Nscad = (numproc - npa)/npera     !# of hypercolumns of subcubes to add
If(nscad > 0) then
   obj%nsc(obj%jmsc)  = obj%nsc(obj%jmsc) + nscad
   obj%lnsc(obj%jmsc) = neg(obj%jmsc)/obj%nsc(obj%jmsc)
   obj%rmsc(obj%jmsc) = neg(obj%jmsc) - obj%lnsc(obj%jmsc)*obj%nsc(obj%jmsc)
   npa    = npera * obj%nsc(obj%jmsc)
end if

!Find largest subcube dimension (second pass)
obj%jmsc = 1                                  !Largest dimension
Do j = 2,4
   if (obj%lnsc(j) > obj%lnsc(obj%jmsc)) obj%jmsc=j
End Do

!Find size of hypercolumns normal to longest direction
npera = 1                              !# subcubes orthogonal to jmsc
Do j = 1,4
   if (j==obj%jmsc) cycle
   npera = npera * obj%nsc(j)
End Do

!Add hypercolumns of subcubes as necessary
Nscad = (numproc - npa)/npera     !# of hypercolumns of subcubes to add
If(nscad > 0) then
   obj%nsc(obj%jmsc)  = obj%nsc(obj%jmsc) + nscad
   obj%lnsc(obj%jmsc) = neg(obj%jmsc)/obj%nsc(obj%jmsc)
   obj%rmsc(obj%jmsc) = neg(obj%jmsc) - obj%lnsc(obj%jmsc)*obj%nsc(obj%jmsc)
   npa    = npera * obj%nsc(obj%jmsc)
end if

!remaining subcubes
obj%rmjm     = numproc    - npa                  !# subcubes still to add
obj%nsc(5)   = obj%nsc(obj%jmsc) + 1
obj%lnsc(5)  = neg(obj%jmsc)/obj%nsc(5)  !length of last subcubes
obj%rmsc(5) = neg(obj%jmsc) - obj%lnsc(5)*obj%nsc(5)

End Subroutine DMAP3D_Subdiv

  Subroutine DMAP3D_adjust_size( neg1, neg2, size, size1, size2)

!  Adjust the side of the rectangle if required.

   implicit none

   integer, intent(in)    :: neg1
   integer, intent(in)    :: neg2
   real,    intent(in)    :: size 
   real,    intent(inout) :: size1
   real,    intent(inout) :: size2
   
   real                   :: r 
   

    if ( neg1 > neg2 ) then
      r = float(neg1)/neg2
      size1 = sqrt(r*size)
      size2 = size1/r
    else if ( neg2 > neg1 ) then
      r = float(neg2)/neg1
      size2 = sqrt(r*size)
      size1 = size2/r
    else 
      size1 = sqrt(size)
      size2 = size1    
   end if
   
   return

  End Subroutine DMAP3D_adjust_size   
!******************************************************************************
!
!  FindHypCub calculates the coordinates (ie1,ie2,ig1,ig2) in the full
!  hypercube corresponding to subcube (msc1,msc2,msc3,msc4)
!
!******************************************************************************
Subroutine DMAP3D_FindHypCubLoc(obj)

Implicit None
Type (DMAP3D_Struct) obj

Integer j, oscn, ieg(4), lnn(4), nsc, lnsc, rmsc, mr, ms

!Given subcube number, calculate msc(1:4)
mr = obj%scnum
Do j = 1, 4
   If(j == obj%jmsc) Cycle
   ms = mr/obj%nsc(j)
   obj%msc(j) = mr - ms * obj%nsc(j)
   mr = ms
End Do
obj%msc(obj%jmsc) = mr

!given msc(1:4), compute hypercube coordinates in all but the special direction
oscn = 0
Do j = 4,1,-1
   if (j == obj%jmsc) Cycle

   if(obj%msc(j) < obj%rmsc(j)) then
      lnn(j) = obj%lnsc(j) + 1
      ieg(j) =obj%msc(j)*lnn(j)
   else
      lnn(j) = obj%lnsc(j)
      ieg(j) = obj%rmsc(j)*(obj%lnsc(j)+1) +   &
                       (obj%msc(j)-obj%rmsc(j))*obj%lnsc(j)
   end if

   !Counter orthogonal to special direction
   oscn = obj%msc(j) + obj%nsc(j) * oscn
End Do

!In the special direction, the number of subcubes varies slightly
j = obj%jmsc
if( oscn < obj%rmjm ) then

   !increase number of subcubes by one
   nsc = obj%nsc(5)
   lnsc = obj%lnsc(5)
   rmsc = obj%rmsc(5)
else
   nsc = obj%nsc(j)
   lnsc = obj%lnsc(j)
   rmsc = obj%rmsc(j)
end if

!calculate hypercube coordinates in the special direction
if(obj%msc(j) < rmsc) then
   lnn(j) = lnsc + 1
   ieg(j) = obj%msc(j)*lnn(j)
else
   lnn(j) = lnsc
   ieg(j) = rmsc*(lnsc+1) + (obj%msc(j)-rmsc)*lnsc

end if

!move hypercube coordinates into obj
obj%ie1sc = ieg(1)
obj%ie2sc = ieg(2)
obj%ig1sc = ieg(3)
obj%ig2sc = ieg(4)

!Current subcube size
obj%ne1sc = lnn(1)
obj%ne2sc = lnn(2)
obj%ng1sc = lnn(3)
obj%ng2sc = lnn(4)

!Coordinate of current element
obj%e1isc = obj%e1ini + obj%ie1sc * obj%de1
obj%e2isc = obj%e2ini + obj%ie2sc * obj%de2
obj%g1isc = obj%g1ini + obj%ig1sc * obj%dg1
obj%g2isc = obj%g2ini + obj%ig2sc * obj%dg2

End subroutine DMAP3D_FindHypCubLoc

!******************************************************************************
!
!  FindAllHypCubLoc calculates the coordinates (ie1,ie2,ig1,ig2) in the full
!  hypercube corresponding to subcube (msc1,msc2,msc3,msc4)
!
!******************************************************************************
Subroutine DMAP3D_FindAllHypCubLoc(obj)

Implicit None
Type (DMAP3D_Struct) obj

Integer j, oscn, ieg(4), lnn(4), nsc, lnsc, rmsc, mr, ms, icpu

if(obj%mycpu==obj%rootcpu) write(iupr,*)" Subcube boundary locations"


icpu_loop: do icpu = 0, obj%numcpu - 1
   !Given subcube number, calculate msc(1:4)
   mr = icpu
   Do j = 1, 4
      If(j == obj%jmsc) Cycle
      ms = mr/obj%nsc(j)
      obj%msc(j) = mr - ms * obj%nsc(j)
      mr = ms
   End Do
   obj%msc(obj%jmsc) = mr

   !given msc(1:4), compute hypercube coords in all but the special direction
   oscn = 0
   Do j = 4,1,-1
      if (j == obj%jmsc) Cycle

      if(obj%msc(j) < obj%rmsc(j)) then
         lnn(j) = obj%lnsc(j) + 1
         ieg(j) =obj%msc(j)*lnn(j)
      else
         lnn(j) = obj%lnsc(j)
         ieg(j) = obj%rmsc(j)*(obj%lnsc(j)+1) +   &
                          (obj%msc(j)-obj%rmsc(j))*obj%lnsc(j)
      end if

      !Counter orthogonal to special direction
      oscn = obj%msc(j) + obj%nsc(j) * oscn
   End Do

   !In the special direction, the number of subcubes varies slightly
   j = obj%jmsc
   if( oscn < obj%rmjm ) then

      !increase number of subcubes by one
      nsc = obj%nsc(5)
      lnsc = obj%lnsc(5)
      rmsc = obj%rmsc(5)
   else
      nsc = obj%nsc(j)
      lnsc = obj%lnsc(j)
      rmsc = obj%rmsc(j)
   end if

   !calculate hypercube coordinates in the special direction
   if(obj%msc(j) < rmsc) then
      lnn(j) = lnsc + 1
      ieg(j) = obj%msc(j)*lnn(j)
   else
      lnn(j) = lnsc
      ieg(j) = rmsc*(lnsc+1) + (obj%msc(j)-rmsc)*lnsc

   end if

   !move first and last subcube coordinates into negsc
   Do j=1,4
      obj%negsc(j,1,icpu) = ieg(j)
      obj%negsc(j,2,icpu) = ieg(j) + lnn(j) - 1
   end do

   if(obj%mycpu == obj%rootcpu) then
      write(iupr,'(i4,4i7,2x,4i7)') icpu,obj%negsc(:,:,icpu)
   end if


End Do icpu_loop

End subroutine DMAP3D_FindAllHypCubLoc


!!--------------------------- DMAP3D util place -----------------------------!!
!!--------------------------- DMAP3D util place -----------------------------!!
!!--------------------------- DMAP3D util place -----------------------------!!


!***************************************************************************
!
!  Utility routine -- Find where a real number fits in an ordered list
!  Inputs
!     rnum     -- Number to be tested
!     rlist    -- Ordered list of real numbers
!     nlist    -- Number of numbers in list
!
!  Output
!     place    -- index of first number in list >= rnum
!
!***************************************************************************
   Subroutine DMAP3D_util_place ( rnum, rlist, nlist, place)

   Implicit None
   Real, Intent(IN) :: rnum, rlist(:)
   Integer, Intent(IN) :: nlist

   Integer, Intent(OUT) :: place

   Integer  :: ilist

   place = nlist + 1
   Do ilist = 1, nlist
      if (rnum <= rlist(ilist)) Then
  place = ilist
  Exit
      end if
   End Do

   End Subroutine DMAP3D_util_place


!!--------------------------- DMAP3D util findiv ----------------------------!!
!!--------------------------- DMAP3D util findiv ----------------------------!!
!!--------------------------- DMAP3D util findiv ----------------------------!!


!***************************************************************************
!
!  Routine to find location in integrated function.
!  Rather than solving a cubic equation, it uses an iterative solution.
!
!  Inputs
!     lstk     -- number of interval bdys in piecewise polynomial
!     tstk     -- ttr's at interval bdys
!     wstk     -- polynomial within intervals
!     wi       -- integrated polynomial
!     wiv      -- desired value of polynomial integral
!
!  Output
!     tval     -- ttr at wiv
!     itv      -- First interval boundary number after tval
!
!  Subroutines Called
!     DMAP3D_mkout_map_sumover
!
!***************************************************************************
   Subroutine DMAP3D_util_findiv(lstk,tstk,wstk,wi,wiv,tval,itv)

   Implicit None
   Integer, Intent(In)    :: lstk
   Real, Intent(In)    :: tstk(:),wi(:),wstk(:,:),wiv
   Real, Intent(Out)    :: tval
   Integer, Intent(Out)    :: itv

   Integer     :: istk, itry
   Real      ::             wps,wpo,wpt,eps  
   Real      :: wl, wg, tl, tg

   eps = wi(lstk)*.001

   Do istk = 1, lstk-1

      wpo = wi(istk)      !wpo is integral to start of current interval
      wps = wi(istk+1)      !wps is integral to end of current interval

      If( Abs(wps-wiv) < eps ) then

  tval = tstk(istk+1)
  itv  = istk + 1

      Elseif( wps > wiv ) then

  !desired value is inside the istk'th interval
  itv  = istk + 1
  wl   = wpo
  wg   = wps
  tl   = tstk(istk)
  tg   = tstk(itv)
  itry = 0

  !!write(iupr,'("t= ",2f10.5," w= ",3f10.5)')tl,tg,wl,wg,wiv

  Do
     itry = itry + 1
     tval = .5*(tl + tg)

     Call DMAP3D_mkout_map_sumover(tstk(1),tval,lstk,tstk,wstk,wi,wpt)

     !!write(iupr,'("itry",i6," t= ",f10.5," w= ",f10.5," dw= ",f10.5)')&
     !!        itry,   tval,        wpt,    (wiv-wpt)/wiv

     if(abs(wpt-wiv) < eps .or. itry > 10) EXIT

     if(wiv > wpt) then
        wl = wpt
        tl = tval
     else
        wg = wpt
        tg = tval
     end if

     !write(iupr,'("tl tg = ",2f10.5," wl wg = ",2f10.5)')tl,tg,wl,wg
     !write(iupr,*)" "

  End Do

  if(itry>10) then
     write(iupr,*)"itry > 10 and Trouble finding value at wiv = ",wiv
     write(iupr,*)" "
  end if

      else

  CYCLE

      end if

      EXIT

   End Do

   !write(iupr,*)"Leaving DMAP3D_util_findiv with itv tval = ",itv,tval
   !write(iupr,*)" "

   End Subroutine DMAP3D_util_findiv


!!--------------------------- DMAP3D util TstInsideTri ----------------------!!
!!--------------------------- DMAP3D util TstInsideTri ----------------------!!
!!--------------------------- DMAP3D util TstInsideTri ----------------------!!


! *****************************************************************************
! Tests whether a point lies within a triangle.  If so, returns the
! internal coordinate of the point.
! The internal coordinates are defined relative to the triangle vertices
! p1, p2, and p3.  A point xtst is given in terms of the internal coordinates
! ca and cb as
!        xtst = p1 + ca * (p2-p1) + cb * (p3-p2)
! At the first vertex p1, ca = 0 and cb = 0.  At p2, ca = 1 and cb = 0.
! At p3, ca = 1 and cb = 1.  The inside of the triangle is the region
!        0 <= ca <= 1.,   0 <= cb <= ca
! The internal coordinates of xtst are given as
!  ca = (p3(2)-p2(2))*(xtst(1)-p1(1)) - (p3(1)-p2(1))*(xtst(2)-p1(2))
!    -------------------------------------------------------------
!      (p2(1)-p1(1))*(p3(2)-p1(2)) - (p2(2)-p1(2))*(p3(1)-p1(1))
!
!  cb = (p2(1)-p1(1))*(xtst(2)-p1(2)) - (p2(2)-p1(2))*(xtst(1)-p1(1))
!    -------------------------------------------------------------
!      (p2(1)-p1(1))*(p3(2)-p1(2)) - (p2(2)-p1(2))*(p3(1)-p1(1))
!
! *****************************************************************************
   Logical Function DMAP3D_util_TstInsideTri(xtst,p1,p2,p3,tc)

   Implicit None
   Real, Intent(In) :: xtst(2)   !test point coordinates
   Real, Intent(In) :: p1(2),p2(2),p3(2)  !coordinates of triangle vertices
   Real, Intent(Out) :: tc(2)     !test point in internal coordinates
   Real   :: xr(2),ar(2),br(2),ca,cb,ad

   DMAP3D_util_TstInsideTri = .false.

   !coordinates relative to first triangle vertex
   xr = xtst - p1
   ar = p2   - p1
   br = p3   - p1

   !ad,the internal coordinate denominator, is proportional to triangle area
   ad = ar(1)*br(2) - ar(2)*br(1)

   !if the triangle has no area, nothing is inside it.
   if(ad == 0.) return

   !ca is the first internal coordinate numerator
   ca = (br(2)-ar(2))*xr(1) - (br(1)-ar(1))*xr(2)

   !first internal coordinate must be between 0 and 1.
   if ((ca < 0. .and. ca < ad) .or. (ca > 0. .and. ca > ad)) Return

   !cb is the numerator of the second internal coordinate
   cb = ar(1)*xr(2) - ar(2)*xr(1)

   !second internal coordinate must be between 0 and the first coord
   if ((cb < 0. .and. cb < ca) .or. (cb > 0. .and. cb > ca)) Return

   !xtst is inside.  return the internal coordinates of xtst
   tc(1) = ca/ad
   tc(2) = cb/ad
   DMAP3D_util_TstInsideTri = .true.

   End Function DMAP3D_util_TstInsideTri


!!--------------------------- DMAP3D util inv4 ------------------------------!!
!!--------------------------- DMAP3D util inv4 ------------------------------!!
!!--------------------------- DMAP3D util inv4 ------------------------------!!


!**************************************************************************
!
!  Utility routine -- Inverts a 4*4 matrix
!
!**************************************************************************
      SUBROUTINE DMAP3D_util_inv4(a, is)

      implicit none

      integer n, is
      parameter (n=4)
      REAL a(n,n)

      INTEGER i,icol,irow,j,k,l,ll,indxc(n),indxr(n),ipiv(n)
      REAL big,dum,pivinv

!     Set status integer
      is = 0

      do  j=1,n
  ipiv(j)=0
      end do

      do  i=1,n
  big=0.
  do j=1,n

     if(ipiv(j).ne.1)then
        do k=1,n
    if (ipiv(k).eq.0) then
       if (abs(a(j,k)).ge.big)then
   big=abs(a(j,k))
   irow=j
   icol=k
       endif
    else if (ipiv(k).gt.1) then
       is = 1
       return
    endif
        end do
     endif

  end do

  ipiv(icol)=ipiv(icol)+1
  if (irow.ne.icol) then
     do l=1,n
        dum=a(irow,l)
        a(irow,l)=a(icol,l)
        a(icol,l)=dum
     end do
  endif

  indxr(i)=irow
  indxc(i)=icol
  if (a(icol,icol).eq.0.) then
     is = 1
     return
  end if

  pivinv=1./a(icol,icol)
  a(icol,icol)=1.
  do l=1,n
     a(icol,l)=a(icol,l)*pivinv
  end do

  do  ll=1,n
     if(ll.ne.icol)then
        dum=a(ll,icol)
        a(ll,icol)=0.
        do l=1,n
   a(ll,l)=a(ll,l)-a(icol,l)*dum
        end do
     endif
  end do

      end do

      do l=n,1,-1
  if(indxr(l).ne.indxc(l))then
     do k=1,n
        dum=a(k,indxr(l))
        a(k,indxr(l))=a(k,indxc(l))
        a(k,indxc(l))=dum
     end do
  endif
      end do

      return
      END Subroutine DMAP3D_util_inv4

      subroutine dmap3d_rms_const(data, istart, iend, rmsamp)
                           
!----------------------------------------------------------------------
!     dmap3d_rms_const  --  routine to compute rms amplitude
!        data -- input trace
!        istart, iend -- starting and ending of trace samples
!        rmsamp -- rms amplitude
!----------------------------------------------------------------------

      implicit none

      real ,    intent(in)    :: data(:)             ! arguments
      integer , intent(in)    :: istart              ! arguments
      integer , intent(in)    :: iend                ! arguments       
      real ,    intent(inout) :: rmsamp              ! arguments
 
      if (istart > iend .or. iend < 1 ) then  
        return  
      endif 
 
      rmsamp = dot_product(data(istart:iend),   &
                           data(istart:iend)) 
      rmsamp = sqrt(rmsamp/(iend - istart + 1))  
 
      return  
      end subroutine dmap3d_rms_const 

      subroutine dmap3d_rms_vary(numtrc, data, istart, iend, winlen, &
                                 ndpt, rmsamp)
                           
!----------------------------------------------------------------------
!     dmap3d_rms_const  --  routine to compute rms amplitude
!        numtrc -- number of input traces
!        data -- input trace
!        istart, iend -- starting and ending of trace samples
!        winlen -- length of the window
!        ndpt   -- number of sample in the trace
!        rmsamp -- rms amplitude
!       
!----------------------------------------------------------------------

      implicit none

      integer , intent(in)    :: numtrc              ! arguments
      real ,    intent(in)    :: data(:,:)           ! arguments
      integer , intent(in)    :: istart              ! arguments
      integer , intent(in)    :: iend                ! arguments
      integer , intent(in)    :: winlen              ! arguments
      integer , intent(in)    :: ndpt                ! arguments   
      real ,    intent(inout) :: rmsamp(:)           ! arguments
 
      real      :: amp_win(numtrc, ndpt)
      real      :: amp(ndpt), loc_win(ndpt)
      real      :: t(ndpt)      
      integer   :: npts, nwin, iw, iw1, i1, i2, j
       
      if (istart > iend .or. iend < 1 ) then  
        return  
      endif 
      
      amp_win = 0.0
      npts = iend - istart + 1
 
!     compute rms gain factor

      nwin = 0
      do j = 1, numtrc

        i1 = istart
        do iw = 1, npts 
          i2 = i1 + winlen - 1
          if ( i2 > iend) exit
          iw1 = iw 
          amp_win(j,iw) = dot_product(data(i1:i2,j), data(i1:i2,j))
          amp_win(j,iw) = sqrt( amp_win(j,iw)/winlen )
          i1 = i1 + winlen/2         

!          write(*,*) 'numtrc,j,iw,i1,i2,amp ', numtrc,j,iw,i1,i2,amp_win(j,iw)

        end do
        nwin = max(nwin, iw1)
      end do

      if ( numtrc > 1) then
        do iw = 1, nwin 
          call median (amp_win(:,iw), numtrc, amp(iw+1))
        end do
      else
        do iw = 1, nwin
          amp(iw+1) = amp_win(1,iw)  
        end do
      end if
 
      i1 = istart      
      do iw = 1, nwin
        i2 = i1 + winlen - 1
        iw1 = iw + 1 
        loc_win(iw1) = (i1+i2)/2.  
        i1 = i1 + winlen/2         
      end do
      
      amp(1)         = amp(2)
      amp(iw1+1)     = amp(iw1)
      loc_win(1)     = 1
      loc_win(iw1+1) = ndpt


!     interpolate the rms amplitude


      npts = iw1+1                
      call interp_1d_var_lin_real (loc_win, amp, npts, &
        t, rmsamp, ndpt, float(1), float(ndpt))
       
!      do iw = 1, npts
!         write(*,910) iw, loc_win(iw),amp(iw)
! 910     format(' iw, loc_win,amp_win ', i6, 2f12.5)
!      end do

!      do iw = 1, ndpt, 20
!         write(*,920) iw, t(iw),rmsamp(iw)
! 920     format(' iw, t, rmsamp       ', i6, 2f12.5)
!      end do

      return  
      end subroutine dmap3d_rms_vary 
 
!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine DMAP3D_wrapup (obj)
      implicit none
      type(DMAP3D_struct),intent(inout)  :: obj   ! arguments
      integer i_err
      logical remove

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      !Print maximum value output
      write(iupr,'("")')
      write(iupr,'("Summary Statistics for CPU ",i6)') obj%mycpu
      Write(iupr,'(a7,i6,a8,i6,a9,e14.6)') &
         " # out= ",obj%ntroutsc," trc # ",obj%idmax," max val ",obj%dmax
      write(iupr,'(a9,i6,a16,3i6,a8,i6)')"mapped = ",obj%nmap, &
         " interpolated= ",obj%ninwf,obj%ninti,obj%ninhi," dead= ",obj%ndead
      write(iupr,'("")')

      !Save or remove work file
      if(obj%savewf(1:1)=='Y') then
         remove = .false.
      else
         remove = .true.
      end if

      !status = trcio_close(file=obj%trcio_obj,remove=.false.)
      !if(status .ne. 0) write(iupr,'(2a,i6)')obj%file_name, &
      !   " closing status = ",status
 
      call dmap3d_close_trace_file ( &
                                     io_obj   = obj%image_obj,     &
                                     l_remove = remove,         &
                                     i_err    = i_err           &
                                    )

      xxif_close_error : if ( i_err .ne. 0 ) then
         call pc_error  &
         ( ' error in dmad3d_wrapup during dmap3d_close_trace_file' )
         return
      end if xxif_close_error

      if (obj%use_disk) then
        remove = .true.
        call dmap3d_close_trace_file ( &
                                     io_obj   = obj%storetr_obj,     &
                                     l_remove = remove,         &
                                     i_err    = i_err           &
                                    )

        if ( i_err .ne. 0 ) then
          call pc_error  &
          ( ' error in dmad3d_wrapup during dmap3d_close_trace_file' )
          return
        end if 
      
      end if 

      return
      end subroutine DMAP3D_wrapup

! the following added by dwh for test single pe || output

  subroutine dmap3d_open_trace_file ( &
                                    io_obj,               &
                                    file_name, file_stat, &
                                    hd_len, tr_len, tr_min, tr_inc, &
                                    scratch, i_err )
         
    type ( trcio_struct ), pointer   :: io_obj


    character(len=*), intent (in   ) :: file_name    ! file name
    character(len=*), intent (in   ) :: file_stat    ! file status
    integer,          intent (in   ) :: hd_len       ! header length
    integer,          intent (in   ) :: tr_len       ! trace length
    real,             intent (in   ) :: tr_min       ! trace min
    real,             intent (in   ) :: tr_inc       ! trace increment
    integer,          intent (inout) :: i_err        ! error flag
    logical,          intent (in   ) :: scratch      ! scratch file status 
    
    integer                   :: extsize
                
    ! initialize the error flag, i_err
    
    i_err = 0
        
    extsize = 2147482624
    i_err = cio_set_file_ext_size(extsize)
    if(i_err /= cio_ok) then
      call pc_error('DMAP3D: Error setting file extent size.')
    endif
    
    io_obj  => trcio_open(                              &
                           filename = file_name,        &
                           io_mode  = file_stat,        &     
                           scratch  = scratch,          &
                           nwih     = hd_len,           &
                           ndpt     = tr_len,           &
                           nbits    = 32,               &
                           nbitshd  = 64                &
                         )
                           
    io_obj%dt   = tr_inc
    io_obj%tmin = tr_min            
    io_obj%tmax = ( tr_len - 1 ) * tr_inc + tr_min

    
    if ( i_err .ne. 0 ) then
      call pc_error ( ' error in dmap3d_open_trace file pe= ', dmap3d_i_pel() )
      return
    end if
    
    return
    !
  end subroutine dmap3d_open_trace_file
  

      
  subroutine dmap3d_close_trace_file ( &
                                       io_obj,      &
                                       l_remove,    &
                                       i_err        &
                                     )
    !
    ! close and remove a trace file
    !
    type ( trcio_struct ), pointer   :: io_obj
    logical,          intent (in   ) :: l_remove     ! remove flag
    integer,          intent (inout) :: i_err        ! error flag
    
    ! initialize the error flag, i_err
    
    i_err = 0
      
      call trcio_headerdump ( file=io_obj )
      
      i_err = trcio_close ( file=io_obj, remove=l_remove )
      
      if ( i_err .lt. 0 ) then
        call pc_error (' error in dmap3d_close_trace file pe= ',dmap3d_i_pel())
        return
      end if
      
    return

  end subroutine dmap3d_close_trace_file
  !
  subroutine dmap3d_zero_trace_file ( &
                                   io_obj, n_rec, hd_len, tr_len, i_err ) 
 
    !
    ! zero a trace file
    !
    type ( trcio_struct ), pointer   :: io_obj

    integer,          intent (in   ) :: n_rec        ! number of records
    integer,          intent (in   ) :: hd_len       ! header length
    integer,          intent (in   ) :: tr_len       ! trace length
    integer,          intent (inout) :: i_err        ! error flag
    !
    double precision                 :: hd_buf ( hd_len, 1 ) ! header array
    real                             :: tr_buf ( tr_len, 1 ) ! trace array

    integer                          :: i_rec        ! record index
    !
    ! initialize the error flag, i_err
    !
    i_err = 0

      hd_buf ( 1:hd_len, 1) = 0.
      tr_buf ( 1:tr_len, 1 ) = 0.
      
      do_i_rec : do i_rec = 1 , n_rec
        
         call dmap3d_write_trace_file ( &
                                        io_obj,     &
                                        i_rec,      &
                                        hd_buf,     &
                                        tr_buf,     &
                                        i_err       &
                                       )
 
        
        if ( i_err .ne. 0 ) then
           call pc_error ( ' error in dmap3d_zero_trace file pe= ',  &
                           dmap3d_i_pel() )
           return
        end if
       
      end do do_i_rec
      
    return
    
  end subroutine dmap3d_zero_trace_file
  

  subroutine dmap3d_read_trace_file ( &
       io_obj, irecord, hd_buf, tr_buf, i_err )
 
    !
    ! read a trace file
    !
    type ( trcio_struct ), pointer   :: io_obj

    integer,          intent (in   ) :: irecord        ! disk record index
    double precision, intent (inout)   :: hd_buf(:, :) ! header buffer
    real,             intent (inout)   :: tr_buf(:, :) ! trace buffer
    integer,          intent (inout) :: i_err        ! error flag
    integer           itr 
    
    ! initialize the error flag, i_err
    

    i_err = 0
    itr = 1

    i_err = trcio_read_trace(                                &
                                  file = io_obj,             &
                                  hd   = hd_buf(:, itr),     &
                                  tr   = tr_buf(:, itr),     &
                                  tnum = irecord             &            
                             )
                                        
     if ( i_err .lt. 0 ) then
       call pc_error ( ' error in dmap3d_read_trace file pe= ', dmap3d_i_pel()) 
     end if 
     
    return

  end subroutine dmap3d_read_trace_file
  
  
  subroutine dmap3d_write_trace_file ( &
       io_obj, irecord, hd_buf, tr_buf, i_err )
                                        
    !
    ! write a trace file
    !
    type ( trcio_struct ), pointer   :: io_obj

    integer,          intent (in   ) :: irecord      ! disk record index
    double precision, intent (in   ) :: hd_buf(:, :) ! header buffer
    real,             intent (in   ) :: tr_buf(:, :) ! trace buffer
    integer,          intent (inout) :: i_err        ! error flag
    integer           itr
    !
    ! initialize the error flag, i_err
    !

    i_err = 0
    itr = 1
        
    i_err = trcio_write_trace(                            &
                               file = io_obj,             &
                               hd   = hd_buf(:, itr),     &
                               tr   = tr_buf(:, itr),     &
                               tnum = irecord )   
        
    if ( i_err .lt. 0 ) then
       call pc_error ('error in dmap3d_write_trace file pe= ',dmap3d_i_pel())
    end if
        
    return

  end subroutine dmap3d_write_trace_file

  subroutine dmap3d_put_trace ( obj, &
       irecord, hd_buf, tr_buf, i_err )
                                               
    ! write a trace file
    
    implicit none
     type (DMAP3D_struct) obj 

    integer,          intent (in   ) :: irecord        ! disk record index
    double precision, intent (in   ) :: hd_buf(:, :) ! header buffer
    real,             intent (in   ) :: tr_buf(:, :) ! trace buffer
    integer,          intent (inout) :: i_err        ! error flag
    integer           itr 
    
    
    ! initialize the error flag, i_err
    
    i_err = 0
    itr = 1
        
    if ( irecord <= obj%Ntim) then
       obj%din(1:obj%ndpt, irecord) = tr_buf(1:obj%ndpt, itr )
       obj%in_disk(irecord) = .false.
       obj%in_mem(irecord)  = irecord
       obj%loc_mem(irecord) = irecord

!       write(*,*) ' mem1 ', irecord, obj%ndpt

    end if
 
    if (obj%use_disk) then
      i_err = trcio_write_trace(                            &
                               file = obj%storetr_obj,      &
                               hd   = hd_buf( :, itr ),     &
                               tr   = tr_buf( :, itr ),     &
                               tnum = irecord )   
        
      if ( i_err .lt. 0 ) then
         call pc_error ('error in dmap3d_write_trace file pe= ',dmap3d_i_pel())
      end if

    end if
     
    return

  end subroutine dmap3d_put_trace
  
  subroutine dmap3d_get_trace ( obj, &
        irecord, hd_buf, tr_buf, i_err )
 
    !
    ! read a trace file
    !
    implicit none
    type (DMAP3D_struct) obj 

    integer,          intent (in   ) :: irecord        ! disk record index
    double precision, intent (inout) :: hd_buf(:, :) ! header buffer
    real,             intent (inout) :: tr_buf(:, :) ! trace buffer
    integer,          intent (inout) :: i_err        ! error flag    

    integer                          :: ipos, loc_disk
    
    ! initialize the error flag, i_err
    
    i_err = 0
        
    if (obj%in_disk(irecord) ) then
    
        i_err = trcio_read_trace(                            &
                                  file = obj%storetr_obj,    &
                                  hd   = hd_buf( :, 1 ),     &
                                  tr   = tr_buf( :, 1 ),     &
                                  tnum = irecord               )

        obj%itr_mem = obj%itr_mem + 1
        ipos = mod( obj%itr_mem, obj%ntim)
        if ( ipos == 0) then
           ipos = obj%ntim 
           obj%itr_mem = 0
        end if
         
        obj%din(1:obj%ndpt, ipos) = tr_buf(1:obj%ndpt, 1)

        obj%in_mem(irecord) = ipos
        loc_disk = obj%loc_mem(ipos)
        obj%loc_mem(ipos) = irecord
 
        obj%in_disk(irecord)= .false. 
        obj%in_disk(loc_disk)= .true.
                                       
     else 
        ipos = obj%in_mem(irecord)
        tr_buf(1:obj%ndpt, 1) = obj%din(1:obj%ndpt, ipos)
        hd_buf(1:obj%nwih, 1) = obj%hdin(1:obj%nwih,irecord)

     end if 
      
     if ( i_err .lt. 0 ) then
       call pc_error ( ' error in dmap3d_read_trace file pe= ', dmap3d_i_pel()) 
     end if 
     
    return

  end subroutine dmap3d_get_trace
  
!
  integer function dmap3d_i_pel ( )
    !
    !  current worker index
    !
    dmap3d_i_pel = pcpsx_i_pel()
    !
    return
    !
  end function dmap3d_i_pel

!
  integer function dmap3d_n_pel ( )
    !
    !  number of workers
    !
    integer :: num_cpus
    !
    xxif_process_traces : if ( .not. pc_do_not_process_traces() ) then
      !
      dmap3d_n_pel = pcpsx_n_pel()
      !
    else xxif_process_traces
      !
      call pc_get_jdata ( 'num_cpus', num_cpus )
      !
      dmap3d_n_pel = num_cpus
      !
    end if xxif_process_traces
    !
    return
    !
  end function dmap3d_n_pel



!!--------------------------- DMAP3D mkout NoAA ------------------------------!!
!!--------------------------- DMAP3D mkout NoAA ------------------------------!!
!!--------------------------- DMAP3D mkout NoAA ------------------------------!!


!******************************************************************************
!
!  NoAA performs a data mapping without worrying about aliasing or sampling.
!     If input traces are too far apart, it gives up and does a simple
!     interpolation.
!     Inputs are
!  obj   -- The DMC structure
!  itr   -- output trace number
!  Ntrib    -- number of input traces to do
!  hi   -- array of input offsets
!  dxm   -- array of differential midpoints
!  hf   -- output offset
!  wf   -- weight function (set to 1 for traces within aperture)
!  tfti   -- array of traveltime ratios
!  timax   -- array of maximum traveltimes
!
!******************************************************************************

      Subroutine DMAP3D_Mkout_NoAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax)

     Implicit None

!     Passed Parameters
      Type(DMAP3D_struct)     :: obj
      Integer, Intent(In)     :: itr
      Integer, Intent(In)     :: Ntrib
      real,    Intent(In)     :: hi(2,obj%MNTOD)
      real,    Intent(In)     :: dxm(2,obj%MNTOD)
      real,    Intent(In)     :: wf(obj%MNTOD)     !Weight fn at triangle node
      real,    Intent(In)     :: hf(2)
      real,    Intent(In)     :: tfti(obj%MNTOD)   !ttr at triangle node
      real,    Intent(In)     :: timax(obj%MNTOD)
      integer, Intent(In)     :: Iin(3)

      !local variables
      integer        :: indi, jndi, ndtri, idtri, jdtri
      real        :: hif, hidx, hfdx
      real        :: dci, dcf, dcic, dcfc
      real        :: sxx,sxy,syy,sh11,sh12,sh21,sh22
      real        :: dxn(2,obj%nnmax),dh11,dh12,dh21,dh22
      real        :: dhi1, dhi2   !change in offset bet 2 neighbors
      real        :: det, detin
      real        :: Biggest
      real        :: stpr, Ar, Wnd, Wna(2), Amp, Dti, ttrm(2)
      integer        :: lstk
      real        :: tstk(3)
      Integer        :: mtim, Istatus 

      integer        :: mute(2)

      real        :: dout(obj%ndpt), dfo(obj%ndpt), dmaxi, dmaxo

      real        :: aout(0:5,10)
      
      !set some parameters
      stpr    = Sin(obj%tprang*.0174539)      !Sin of maximum dip
      Biggest = Huge(1.0)

      Aout    = 0.
      dout(1:obj%ndpt) = 0.

      !No antialiasing.  Calculate contribution from each node
      !and add it to the output trace.
      Do indi = 1, Ntrib

  !Node must be within aperture
  if(wf(indi) == 0.) Cycle

  !Node coordinates
  hif  =  hi(1,indi)*hf(2) - hi(2,indi)*hf(1)
  hidx =  hi(1,indi)*dxm(2,indi) - hi(2,indi)*dxm(1,indi)
  hfdx =  hf(1)*dxm(2,indi) - hf(2)*dxm(1,indi)
  dci   =  hfdx / hif
  dcf   =  hidx / hif
  dcic  = 1. - dci*dci
  dcfc  = 1. - dcf*dcf

  !Calculate cell boundary and average offset derivatives
  ndtri = obj%nnn(indi)
  sxx   = 0.       !Sum of dx**2
  sxy   = 0.       !Sum of dx*dy
  syy   = 0.       !Sum of dy**2
  sh11   = 0.       !Sum of dhx*dx
  sh12   = 0.       !Sum of dhx*dy
  sh21   = 0.       !Sum of dhy*dx
  sh22   = 0.       !Sum of dhy*dy

  !Loop over neighbors
  Do idtri = 1, ndtri

     jndi  = obj%in(idtri,indi)        !neighbor index
     if(jndi < 1 .or. jndi > obj%MNTOD) cycle !must be real neighbor
     dxn(1:2,idtri)   = obj%dx(1:2,idtri,jndi)  !distance to neighbor
     dhi1 = hi(1,jndi)-hi(1,indi)
     dhi2 = hi(2,jndi)-hi(2,indi)

     sxx   = sxx + dxn(1,idtri)*dxn(1,idtri)
     sxy   = sxy + dxn(1,idtri)*dxn(2,idtri)
     syy   = syy + dxn(2,idtri)*dxn(2,idtri)
     sh11  = sh11  + dxn(1,idtri)*dhi1
     sh12  = sh12  + dxn(2,idtri)*dhi1
     sh21  = sh21  + dxn(1,idtri)*dhi2
     sh22  = sh22  + dxn(2,idtri)*dhi2

     !(((((((((((((((((((((((((( Print Statement ))))))))))))))))))))
!      write(iupr,'(7e11.4)')sxx,sxy,syy,sh21,sh22,dxn(1:2,idtri)

  End Do

  !calculate the determinant of the sxx's
  det = sxx*syy - sxy*sxy
  if(det <= epsilon(1.0)) CYCLE
  detin = 1./det

  !offset derivatives
  dh11   = (sh11*syy - sh12*sxy) * detin       !dhx/dx
  dh12   = (sh12*sxx - sh11*sxy) * detin       !dhx/dy
  dh21   = (sh21*syy - sh22*sxy) * detin       !dhy/dx
  dh22   = (sh22*sxx - sh21*sxy) * detin       !dhy/dy

  Ar = 0.
  !Next calculate area of Voronoi cell
  Do idtri = 1, ndtri
     jdtri = 1 + Mod(idtri,ndtri)

     if (abs(obj%vc(1,idtri,indi)) == Biggest .or. &
  abs(obj%vc(2,idtri,indi)) == Biggest .or. &
  abs(obj%vc(1,jdtri,indi)) == Biggest .or. &
  abs(obj%vc(2,jdtri,indi)) == Biggest) CYCLE

     Ar = Ar + Abs(obj%vc(1,idtri,indi)*obj%vc(2,jdtri,indi)  &
      -   obj%vc(2,idtri,indi)*obj%vc(1,jdtri,indi))
  End Do
  Ar = Ar *.25

  !Now calculate amplitude
  wnd = 1./(dcfc*hif)
  wna(1) = wnd*(dci*hf(2)*dcfc - dcf*hi(2,indi)*dcic)
  wna(2) = wnd*(dci*hf(1)*dcfc - dcf*hi(1,indi)*dcic)

  amp = Min( obj%afmax,Abs( (1.+dci*dci)/dcic       &
      * ( (1.+dci*dh11)*(1.+dci*dh22)-dci*dci*dh12*dh21 )     &
      + ( wna(1)*dh11+wna(2)*dh21 )        &
      * ( hi(1,indi)*(1.+dci*dh22)-hi(2,indi)*dci*dh12 )      &
      + ( wna(1)*dh12+wna(2)*dh22 )        &
      * ( hi(2,indi)*(1.+dci*dh11)-hi(1,indi)*dci*dh21 ) ) )

  !(((((((((((((((((((((((((( Print Statement )))))))))))))))))))))))
!   if(indi==indi)Write(iupr,'(7f11.5)')dci,dcf,dh11,dh12,dh21,dh22,amp

  amp = Ar *.1591549 * Abs(wnd) * Amp

  !Set parameters for mapping operation
  dti = .1/float(obj%ndpt)
  ttrm(1) = tfti(indi) - dti
  ttrm(2) = tfti(indi) + dti
  lstk = 1
  tstk(1:3) = tfti(indi)
  mtim = nint(timax(indi))
  mute(1) = obj%hm(indi)
  mute(2) = obj%tm(indi)

  !The three traces surrounding the output point are given the unit map
  if(indi == iin(1) .or. indi == iin(2) .or. indi == iin(3)) then
     tstk(1:3) = 1.
     mtim = obj%ndpt
  end if

  !Add input trace to output trace

  call dmap3d_get_trace (obj, indi, obj%hd_tmp(:,1:),  &
       obj%din_tmp(:,1:), Istatus)
  
  Call DMAP3D_Mkout_Nwf(obj,  obj%din_tmp(:,1), &
        obj%hd_tmp(:,1), Dout,  &
        tstk,Amp,mtim,stpr,Mute,Aout)

!!  Call DMAP3D_Mkout_Nwf(obj,obj%din(1:obj%ndpt,indi), &
!!        obj%hdin(1:obj%nwih,indi),Dout,  &
!!        tstk,Amp,mtim,stpr,Mute,Aout)

      End Do

      !Filter output
      Call DMAP3D_mkout_flt(Dout,Dfo,Aout,dmaxi,dmaxo)

      !Add result to output trace
      obj%dout(:,itr) = obj%dout(:,itr) + dfo

      !(((((((((((((((((((((((((     Print Statements )))))))))))))))))))))))))
!  !! write(iupr,'(a8,i6,a8,2e12.4)')"itr=!",itr,"!dmax=!",dmaxi,!dmaxo!
!  !! write(iupr,'(a10,5x,a2,5(9x,a2))')"jto!itout",!!&
!  !  !  !  !  !  !  !  !  !  !  !  !! "A0","A1","A2","A3","A4","A5"
!  !! Do!jto!=!1,!10
!  !  !!itout!=!jto!*!((obj%ndpt+9)/10)!-!obj%ndpt/20
!  !  !!write(iupr,'(2i5,6f11.5)')jto,itout,Aout(0:5,jto)
!  !! end!do

      End Subroutine DMAP3D_Mkout_NoAA


!!--------------------------- DMAP3D mkout QNAA ------------------------------!!
!!--------------------------- DMAP3D mkout QNAA ------------------------------!!
!!--------------------------- DMAP3D mkout QNAA ------------------------------!!


!******************************************************************************
!
!  QNAA performs a data mapping with quick nearest neighbor antialiasing
!     Inputs are
!  obj   -- The DMC structure
!  itr   -- output trace number
!  Ntrib    -- number of input traces to do
!  hi   -- array of input offsets
!  dxm   -- array of differential midpoints
!  hf   -- output offset
!  wf   -- weight function (set to 1 for traces within aperture)
!  tfti   -- array of traveltime ratios
!  timax   -- array of maximum traveltimes
!
!******************************************************************************

      Subroutine DMAP3D_Mkout_QNAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax)

     Implicit None

!     Passed Parameters
      Type(DMAP3D_struct)       :: obj
      Integer, Intent(In)     :: itr
      Integer, Intent(In)     :: Ntrib
      real,    Intent(In)     :: hi(2,obj%MNTOD)
      real,    Intent(In)     :: dxm(2,obj%MNTOD)
      real,    Intent(In)     :: wf(obj%MNTOD)     !Weight fn at triangle node
      real,    Intent(In)     :: hf(2)
      real,    Intent(In)     :: tfti(obj%MNTOD)   !ttr at triangle node
      real,    Intent(In)     :: timax(obj%MNTOD)
      integer, Intent(In)     :: Iin(3)

      !local variables
      integer        :: indi, jndi, ndtri, idtri, jdtri
      real        :: hif, hidx, hfdx
      real        :: dci, dcf, dcic, dcfc
      real        :: sxx,sxy,syy,sh11,sh12,sh21,sh22
      real        :: dxn(2,obj%nnmax),dh11,dh12,dh21,dh22
      real        :: dhi1, dhi2   !change in offset bet 2 neighbors
      real        :: det, detin
      real        :: Biggest
      real        :: stpr, Ar, Wnd, Wna(2), Amp        

      real        :: tstk(3)
      Integer        :: mtim, Istatus

      integer        :: mute(2)

      real        :: xf(2),p1(2),p2(2),p3(2),tc(2)
      real        :: ttrmax, ttrmin, ttl, ttg, ttm
      logical        :: insidemaybe, insideone,insidethis

      real        :: dout(obj%ndpt), din(obj%ndpt)
      real        :: dmaxi, dmaxo

      real        :: aout(0:5,10)
      
      !set some parameters
      stpr    = Sin(obj%tprang*.0174539)      !Sin of maximum dip
      Biggest = Huge(1.0)
      ttrmax  = obj%ttrmax        !Maximum allowed traveltime ratio
      ttrmin  = 1./ttrmax        !Minimum allowed traveltime ratio

      Aout    = 0.
      dout    = 0.

      !Calculate contribution from each node and add it to the output trace.
      Do indi = 1, Ntrib

  !Node must be within aperture
  if(wf(indi) == 0.) Cycle

  !Is the aperture center nearby?
  if(indi == iin(1) .or. indi == iin(2) .or. indi == iin(3)) then
     insidemaybe = .true.
!      write(iupr,*)" aperture center nearby ",indi
  else
     insidemaybe = .false.
  end if
  insideone = .false.

  !Node coordinates
  xf = dxm(1:2,indi)
  hif  =  hi(1,indi)*hf(2) - hi(2,indi)*hf(1)
  hidx =  hi(1,indi)*dxm(2,indi) - hi(2,indi)*dxm(1,indi)
  hfdx =  hf(1)*dxm(2,indi) - hf(2)*dxm(1,indi)
  dci   =  hfdx / hif
  dcf   =  hidx / hif
  dcic  = 1. - dci*dci
  dcfc  = 1. - dcf*dcf

  !Calculate cell boundary and average offset derivatives
  ndtri = obj%nnn(indi)
  sxx   = 0.       !Sum of dx**2
  sxy   = 0.       !Sum of dx*dy
  syy   = 0.       !Sum of dy**2
  sh11   = 0.       !Sum of dhx*dx
  sh12   = 0.       !Sum of dhx*dy
  sh21   = 0.       !Sum of dhy*dx
  sh22   = 0.       !Sum of dhy*dy

  !Loop over neighbors
  Do idtri = 1, ndtri

     jndi  = obj%in(idtri,indi)        !neighbor index
     if(jndi < 1 .or. jndi > obj%MNTOD) cycle !must be real neighbor
     dxn(1:2,idtri)   = obj%dx(1:2,idtri,jndi)  !distance to neighbor
     dhi1 = hi(1,jndi)-hi(1,indi)
     dhi2 = hi(2,jndi)-hi(2,indi)

     sxx   = sxx + dxn(1,idtri)*dxn(1,idtri)
     sxy   = sxy + dxn(1,idtri)*dxn(2,idtri)
     syy   = syy + dxn(2,idtri)*dxn(2,idtri)
     sh11  = sh11  + dxn(1,idtri)*dhi1
     sh12  = sh12  + dxn(2,idtri)*dhi1
     sh21  = sh21  + dxn(1,idtri)*dhi2
     sh22  = sh22  + dxn(2,idtri)*dhi2

  End Do

  !calculate the determinant of the sxx's
  det = sxx*syy - sxy*sxy
  if(det <= epsilon(1.0)) CYCLE
  detin = 1./det

  !offset derivatives
  dh11   = (sh11*syy - sh12*sxy) * detin       !dhx/dx
  dh12   = (sh12*sxx - sh11*sxy) * detin       !dhx/dy
  dh21   = (sh21*syy - sh22*sxy) * detin       !dhy/dx
  dh22   = (sh22*sxx - sh21*sxy) * detin       !dhy/dy

  Ar = 0.
  !Next calculate area of Voronoi cell
  Do idtri = 1, ndtri
     jdtri = 1 + Mod(idtri,ndtri)

     if (abs(obj%vc(1,idtri,indi)) == Biggest .or. &
  abs(obj%vc(2,idtri,indi)) == Biggest .or. &
  abs(obj%vc(1,jdtri,indi)) == Biggest .or. &
  abs(obj%vc(2,jdtri,indi)) == Biggest) CYCLE

     Ar = Ar + Abs(obj%vc(1,idtri,indi)*obj%vc(2,jdtri,indi)  &
      -   obj%vc(2,idtri,indi)*obj%vc(1,jdtri,indi))

     !does aperture center fall in this triangle?
     if(insidemaybe) then
        p1 = 0.
        p2 = -obj%vc(1:2,idtri,indi)*.5
        p3 = -obj%vc(1:2,jdtri,indi)*.5
        insidethis = DMAP3D_util_tstInsideTri(xf,p1,p2,p3,tc)
        if(insidethis) then
    insideone = .true.
!     write(iupr,'(a8,3i5,6f9.2)')"inside",indi,idtri,jdtri,xf,p2,p3
        else
!     write(iupr,'(a8,3i5,6f9.2)')"outside",indi,idtri,jdtri,xf,p2,p3
        end if
     end if

  End Do
  Ar = Ar *.25

  !If aperture center falls near this trace, adjust ttr and tm
  if(insideone) then
     ttm  = 1.
     mtim = obj%ndpt
  else
     ttm  = tfti(indi)
     mtim = nint(timax(indi))
  end if

  !Now calculate amplitude
  wnd = 1./(dcfc*hif)
  wna(1) = wnd*(dci*hf(2)*dcfc - dcf*hi(2,indi)*dcic)
  wna(2) = wnd*(dci*hf(1)*dcfc - dcf*hi(1,indi)*dcic)

  amp = Min( obj%afmax,Abs( (1.+dci*dci)/dcic       &
      * ( (1.+dci*dh11)*(1.+dci*dh22)-dci*dci*dh12*dh21 )     &
      + ( wna(1)*dh11+wna(2)*dh21 )        &
      * ( hi(1,indi)*(1.+dci*dh22)-hi(2,indi)*dci*dh12 )      &
      + ( wna(1)*dh12+wna(2)*dh22 )        &
      * ( hi(2,indi)*(1.+dci*dh11)-hi(1,indi)*dci*dh21 ) ) )

  !(((((((((((((((((((((((((( Print Statement )))))))))))))))))))))))
!   if(indi==indi)Write(iupr,'(7f11.5)')dci,dcf,dh11,dh12,dh21,dh22,amp

  !mapping amplitude
  amp = Ar *.1591549 * Abs(wnd) * Amp

  !ttl/ttg is largest/smallest ttr not larger/smaller than tfti(indi)
  ttl = ttrmin - .1
  ttg = ttrmax + .1
  Do idtri = 1, ndtri

     jndi  = obj%in(idtri,indi)        !neighbor index
     if(jndi < 1 .or. jndi > obj%MNTOD) cycle !must be real neighbor
     if(ttl < tfti(jndi) .and. tfti(jndi) < ttm) then
        ttl = tfti(jndi)
     elseif (ttg > tfti(jndi) .and. tfti(jndi) > ttm) then
        ttg = tfti(jndi)
     end if

  End Do
  if(ttl < ttrmin) ttl = ttm
  if(ttg > ttrmax) ttg = ttm

!   write(iupr,'(i4,a,3f10.4)')indi," ttr interval= ", ttl, ttm, ttg

  !traveltime interval is half the distance to nearest neighbor
  tstk(1) = .5*(ttm+ttl)
  tstk(2) = ttm
  tstk(3) = .5*(ttm+ttg)

  !Set mutes
  mute(1) = obj%hm(indi)
  mute(2) = obj%tm(indi)

  !Add input trace to output trace 

  call dmap3d_get_trace (obj, indi, obj%hd_tmp(:,1:),  &
       obj%din_tmp(:,1:), Istatus)
       
!!  Din = obj%din(1:obj%ndpt,indi)

  Call DMAP3D_Mkout_Qwf(obj,obj%din_tmp(:,1),Dout,tstk,Amp,mtim,stpr,Mute,Aout)

      End Do

      !Filter output
      Call DMAP3D_mkout_flt(Dout,Din,Aout,dmaxi,dmaxo)

      !Add result to output trace
      obj%dout(:,itr) = obj%dout(:,itr) + Din

      !(((((((((((((((((((((((((     Print Statements )))))))))))))))))))))))))
!  !! write(iupr,'(a8,i6,a8,2e12.4)')"itr=!",itr,"!dmax=!",dmaxi,!dmaxo!
!  !! write(iupr,'(a10,5x,a2,5(9x,a2))')"jto!itout",!!&
!  !  !  !  !  !  !  !  !  !  !  !  !! "A0","A1","A2","A3","A4","A5"
!  !! Do!jto!=!1,!10
!  !  !!itout!=!jto!*!((obj%ndpt+9)/10)!-!obj%ndpt/20
!  !  !!write(iupr,'(2i5,6f11.5)')jto,itout,Aout(0:5,jto)
!  !! end!do

      End Subroutine DMAP3D_Mkout_QNAA


!!--------------------------- DMAP3D mkout LinAA ----------------------------!!
!!--------------------------- DMAP3D mkout LinAA ----------------------------!!
!!--------------------------- DMAP3D mkout LinAA ----------------------------!!


!******************************************************************************
!
!  LinAA performs a data mapping with a linear anti-aliasing strategy.
!     This is the most expensive option.
!     Inputs are
!  obj   -- The DMC structure
!  itr   -- output trace number
!  Ntrib    -- number of input traces to do
!  hi   -- array of input offsets
!  dxm   -- array of differential midpoints
!  hf   -- output offset
!  iin   -- indices of triangle enclosing aperture center
!  wf   -- weight function (set to 1 for traces within aperture)
!  tfti   -- array of traveltime ratios
!  timax   -- array of maximum traveltimes
!
!******************************************************************************

      Subroutine DMAP3D_Mkout_LinAA(obj,itr,Ntrib,hi,dxm,wf,hf,Iin,tfti,timax)

      Implicit None

!     Passed Parameters
      Type(DMAP3D_struct)       :: obj
      Integer, Intent(In)     :: itr
      Integer, Intent(In)     :: Ntrib
      real,    Intent(InOut)  :: hi(2,obj%MNTOD)
      real,    Intent(InOut)  :: dxm(2,obj%MNTOD)
      real,    Intent(InOut)  :: wf(obj%MNTOD)     !Weight fn at triangle node
      real,    Intent(In)     :: hf(2)
      Integer, Intent(In)     :: Iin(3)
      real,    Intent(InOut)  :: tfti(obj%MNTOD)   !ttr at triangle node
      real,    Intent(InOut)  :: timax(obj%MNTOD)

      !local variables
      integer     :: indi, ndtri, idtri, jdtri, indb, indc, indj
      integer     :: ind(3)
      real        :: hif, hifc
      real        :: Biggest

      real        :: dsq, dsqm, dx1, dx2
      real        :: wff
      integer     :: niter, iter       
      real        :: trlen, vdti, ttrmin, ttrmax

      integer     :: mute(2)

      real        :: dout(obj%ndpt), dfo(obj%ndpt), dmaxi, dmaxo

      real        :: aout(0:5,10)

      Integer     :: Istatus 

      !set some parameters
      Biggest = Huge(1.0)
      Niter   = 4
      vdti    = 1./obj%vel        !Inverse of Vel*Dt/2
      ttrmax  = obj%ttrmax        !Maximum allowed traveltime ratio
      ttrmin  = 1./ttrmax        !Minimum allowed traveltime ratio
      trlen   = ttrmax * obj%ndpt       !Maximum trace length

      aout = 0.
      dout = 0.

      !((((((((((((((((((((((((Print Statement)))))))))))))))))))))))))))
!      write(iupr,'("Ind",10x,"dxm",21x,"hi",22x,"tr",10x,"tm")')

      !For triangles partly inside aperture, find nodes adjacent to aperture
      !and extrapolate them inside.
      Do indi = 1, Ntrib

        !Start with nodes within aperture
        if(wf(indi) < .999999) cycle

        !((((((((((((((((((((((((Print Statement)))))))))))))))))))))))))))
         !   write(iupr,'(i4,4f12.4)')indi,dxm(1:2,indi),hi(1:2,indi)

        !number of Delaunay triangles for node indi
        ndtri = obj%nnn(indi)

        !Look for outside nodes adjacent to indi
        Do idtri = 1, ndtri
           indb = obj%in(idtri,indi)
           if(indb < 1 .or. indb > Ntrib .or. wf(indb) > 0. ) Cycle

           !node indb is outside aperture.  Try to move it inside, and
           !assign it a reduced weight according to how far it moved.
           !Extrapolate in direction of inside node indc nearest to indb.
           !(Indc may or may not be indi.)
           dsqm = Biggest
           do jdtri = 1, obj%nnn(indb)
              indj = obj%in(jdtri,indb)
              if(indj < 0 .or. wf(indj) == 0.) cycle

              dx1 = dxm(1,indb)-dxm(1,indj)
              dx2 = dxm(2,indb)-dxm(2,indj)
              dsq = dx1*dx1+dx2*dx2
              if (dsq > dsqm ) cycle
              dsqm = dsq
              indc = indj

           end do

           !move indb closer to indc until it is inside aperture
           wff = 1.
           Do iter = 1, Niter

              !each iteration moves half the remaining from indb to indc
              wff = wff * 2.
              hi(1:2,indb)  = .5*(hi(1:2,indb)+hi(1:2,indc))   !new hi
              dxm(1:2,indb) = .5*(dxm(1:2,indb)+dxm(1:2,indc))   !new dxm

              !If point is within aperture, calc tfti, timax
              Call DMAP3D_Mkout_Par (obj, hi(1:2,indb),hf,dxm(1:2,indb), &
                  trlen,vdti,tfti(indb),timax(indb))

              !Traveltime ratio must be within bounds
              if(tfti(indb) > ttrmin .and. tfti(indb) < ttrmax) then

                 !Node is accepted. Weight the node by how far it moved.
                 wf(indb)  = 1./wff

                   !(((((((((((((((((((((((((( Print Statement ))))))))))))))
                  !  !  ! if(itr!==!1)!then
                  !  !  !  !write(iupr,'(a10,i6,6f10.1)')"Adding!  !",indb,!!&
                  !  !  !  obj%x(1:2,indb),hi(1:2,indb),dxm(1:2,indb)
                  !  !  !  !write(iupr,'(2f10.5,f12.5,f9.4,i8)')dci,dcf,!  !&
                  !  !  !  !  !tfti(indb),timax(indb),iter
                  !  !  ! end!if!

                  Exit

              end if            !ttr if-block

           End Do            !iter do-loop

        End Do            !idtri do-loop

      End Do             !indi do-loop

      !Contribution to output trace from each included node.
      Do indi = 1, Ntrib

        !Only nodes within aperture
        if(wf(indi) == 0.) CYCLE

        !Calculate hi cross hf for this input trace
        hifc = hi(1,indi)*hf(2) - hi(2,indi)*hf(1)

        !number of Delaunay triangles
        ndtri = obj%nnn(indi)

        !first node of triangle is input trace
        ind(1) = indi

        !Mutes
        mute(1) = obj%hm(indi)
        mute(2) = obj%tm(indi)

        !Calc contribution from each Delaunay triangle
        Do idtri = 1, ndtri

           !triangle is defined by nodes ind(1), ind(2), ind(3)
           jdtri = 1 + Mod(idtri,ndtri)
           ind(2) = obj%in(idtri,indi)
           ind(3) = obj%in(jdtri,indi)

           !All three nodes must lie within aperture
           if(ind(2) < 1 .or. ind(2) > Ntrib .or.      &
              ind(3) < 1 .or. ind(3) > Ntrib .or.      &
              ind(1) == ind(2) .or. ind(1) == ind(3) .or.  &
              ind(2) == ind(3) )       CYCLE
           if (wf(ind(2)) == 0. .or. wf(ind(3)) == 0.)     CYCLE

           !hi cross hf must not change sign within a triangle
           hif = hi(1,ind(2))*hf(2) - hi(2,ind(2))*hf(1)
           if(hifc*hif < 0.) CYCLE
           hif = hi(1,ind(3))*hf(2) - hi(2,ind(3))*hf(1)
           if(hifc*hif < 0.) CYCLE

           !(((((((((((((((((((((((((( Print Statement ))))))))))))))))))))
            !! write(iupr,'(a20,3i4,3f8.4,3f8.1)')"makewf.!ind,ttr,tmv=!"!&
            !  !!,ind,tfti(ind(1)),tfti(ind(2)),tfti(ind(3)),!!&
            !  !  timax(ind(1)),timax(ind(2)),timax(ind(3))

           !calculate the weight factor contribution from this triangle

           call dmap3d_get_trace (obj, indi, obj%hd_tmp(:,1:),  &
              obj%din_tmp(:,1:), Istatus)
       
           call DMAP3D_mkout_MakeDST(obj,hf,hi,dxm,ind,tfti,timax,wf,mute,&  
               obj%din_tmp(:,1),dout,Aout)     

!           call DMAP3D_mkout_MakeDST(obj,hf,hi,dxm,ind,tfti,timax,wf,mute,&
!               obj%din(1:obj%ndpt,indi),dout,Aout)

        End Do

      End Do   !Input Trace Loop

      !Filter output
      Call DMAP3D_mkout_flt(Dout,Dfo,Aout,dmaxi,dmaxo)

      !Add result to output trace
      obj%dout(:,itr) = obj%dout(:,itr) + dfo(1:obj%ndpt)

      !(((((((((((((((((((((((((     Print Statements )))))))))))))))))))))))))
      !  !  write(iupr,'(a8,i6,a8,2e12.4)')"itr=!",itr,"!dmax=!",dmaxi,!dmaxo!
      !  !  write(iupr,'(a10,5x,a2,5(9x,a2))')"jto!itout",!!&
      !  !  !  !  !  !  !  !  !  !  !  !  !  !"A0","A1","A2","A3","A4","A5"
      !  !  Do!jto!=!1,!10
      !  !  !  itout!=!jto!*!((obj%ndpt+9)/10)!-!obj%ndpt/20
      !  !  !  write(iupr,'(2i5,6f11.5)')jto,itout,Aout(0:5,jto)
      !  !  end!do

      End Subroutine DMAP3D_Mkout_LinAA


!!--------------------------- DMAP3D mkout MakeDST --------------------------!!
!!--------------------------- DMAP3D mkout MakeDST --------------------------!!
!!--------------------------- DMAP3D mkout MakeDST --------------------------!!


! *****************************************************************************
!
!     DMAP3D_mkout_MakeDST generates a traveltime ratio weighting function,
!     assuming that the ratio varies linearly within a Delaunay triangle.
!     The traveltime ratios at the triangle vertices are input.
!     Returned is a weight function wttr(ttr) proportional to the density of
!     traveltime ratio ttr, for equally spaced traveltime ratios within the
!     range of ratios encountered.
!
!     If the properties do not vary linearly within the triangle, the
!     triangle is subdivided into two triangles, this process continuing
!     until linearity is reached or the program gets tired.
!
!     The routine keeps track of the subtriangles by means of two stacks. The
!     first stack is a list of properties (ttr,timax,hi,dxm, and WF) calculated
!     at each subtriangle node or vertex.  When a triangle divides, a new node
!     or vertex is added to the node stack.  Another stack (Nd) lists the nodes
!     that belong to each subtriangle. When a subtriangle divides, the node
!     and triangle stacks increases their length by one.
!
!     When the contribution of a subtriangle to the weight function has been
!     made, the length of the triangle stack is decreased by 1.  When the
!     length of the triangle stack drops to zero, the routine is done.
!
!     Logic for the routine is
!     1.  Initialize Stack
!
!     2.  Do (Process Stack)
!
!     2.1   If Stack is not full
!     2.1.1    Test nonlinarity of last subtriangle in stack
!     2.1.2    If nonlinearity is significant then
!     2.1.2.1   Divide last subtriangle in two
!     2.1.2.2   Increment stack
!     2.1.2.3   Cycle Process stack loop (2.)
!     2.1.2    End nonlinearity if-block
!     2.1   End If Stack is not full
!
!     2.2   If maximum traveltime in subtriangle is large enough
!     2.2.1    Calculate weight factor polynomial for subtriangle
!     2.2.2    Add to weight factor polynomial for cell
!     2.2   End maximum traveltime if-block
!
!     2.3   Remove last subtriangle from stack
!     2.4   If stack is empty, Exit Process stack loop (2.)
!
!     2.  End Do (Process Stack)
!
!     Inputs
!  obj  -- the dmc data structure
!  hf  -- output offset
!  hi,dxm  -- input offsets and midpoints at triangle vertices
!  ind(3)  -- indices of the three input triangle vertices
!  ttr  -- array of traveltime ratios at triangle vertices
!  wv  -- array of weights at triangle vertices (1 unless
!      vertex has been moved).
!  timax  -- array of maximum traveltimes at triangle vertices.
!  mute  -- head and tail mutes for current input trace
!  din  -- current input trace
!     Outputs
!  dout  -- current output trace
!  Aout  -- impulse response at selected time intervals
!     Subroutines called
!  DMAP3D_Util_TstInsideTri
!  DMAP3D_Mkout_Par
!  DMAP3D_Mkout_Lwf
!
! *****************************************************************************

  Subroutine DMAP3D_mkout_MakeDST(obj,hf,hi,dxm,ind,ttr,timax,wv,mute,&
          din,dout,Aout)

   Implicit None
   type (DMAP3D_struct) :: obj
   real, Intent(In) :: timax(:)
   real, Intent(In) :: hf(2)
   real, Intent(In) :: hi(:,:),dxm(:,:)
   real, Intent(In) :: ttr(:)
   real, Intent(In) :: wv(:)
   integer, Intent(In) :: ind(3)
   integer, Intent(In) :: Mute(2)
   real, Intent(In) :: Din(*)
   real, Intent(InOut) :: Dout(*)
   real, Intent(Inout) :: Aout(0:5,10)

   Real, Parameter :: third = 1./3.
   Integer, Parameter :: tri_tot = 256, tri_open = 12
   real   :: ttrs(tri_tot), his(2,tri_tot)
   real   :: dxms(2,tri_tot),wvs(tri_tot),tims(tri_tot)
   integer  :: nd(3,tri_open)

   real   ::                                      ttl, ttg, ttm  
   integer  ::      i,it1,it2,it3, it4, its, ir(3) 
   real   :: wa, wb, wc     , hif, ahif, area 
   real   :: hidx, hfdx, dcis, dcfs, dcic, dcfc, vdti
   real   :: dxba(2), dxcb(2), dhba(2), dhcb(2), dba(2), dcb(2)
   real   :: wnd, wna(2), amp               

   integer  :: mtiml  


   real   :: cwfd,tt12, tt23, tt13       
   real   :: dtm1, dtm2, dtm3
   real   :: dxm1(2), dxm2(2), dxm3(2)
   real   :: dx1sq, dx2sq, dx3sq, dxmsq
   real   :: dxf(2), tc(2)
   Integer  ::     inm, inp 

   Real   :: hic(2), dxmc(2)   
   real   :: dtmx, maxdtmax, maxtims, maxttst


   Logical  :: InsideAll, InsideThis



   real   :: trval(3), Adrv(4)
   real, Parameter :: sixth = 1./6

   real ttrmin2, ttrmax2, trlen, stpr

   !set taper
   stpr = Sin(obj%tprang*.0174539)

   !Is aperture center inside triangle?
   dxf = 0.
   InsideAll = DMAP3D_Util_TstInsideTri(dxf,dxm(1:2,ind(1)),dxm(1:2,ind(2)),  &
    dxm(1:2,ind(3)),tc)

!   if(InsideAll) write(iupr,'(a,3i6)')" Aperture center inside triangle ",ind

   if(.not. InsideAll) then

      !Unless aperture center is inside triangle, max tt must be >= itmin
      if(obj%itmin > timax(ind(1)) .and.  &
  obj%itmin > timax(ind(2)) .and.  &
  obj%itmin > timax(ind(3))) Return

   end if

   !inverse of vdt
   vdti = 1./obj%vel
   trlen = obj%ndpt*1.1

   !extremal ttr sq
   ttrmax2 = obj%ttrmax*obj%ttrmax
   ttrmin2 = 1./ttrmax2

   !Maximum change in max traveltime allowed in subtriangle
   maxdtmax = 50.

   !Start the traveltime stack (contains ttr, hi,dxm and wvs for each node)
   Do i = 1,3
      ttrs(i)   = ttr(    ind(i))
      his(1:2,i)  = hi (1:2,ind(i))
      dxms(1:2,i) = dxm(1:2,ind(i))
      tims(i)   = min(timax(ind(i)),trlen)   !Added 12/01/00
   End Do

   !Assign weight factors to triangle nodes (not used in AATYPE=NEAR mode)
   If(obj%aatype(1:1)=="L") then
      wvs(1) =     wv(ind(1))
      wvs(2) = 1. - wv(ind(2))
      wvs(3) = 1. - wv(ind(3))
   Else
      wvs(1:3) = 1.
   End If

   it4 = 4        !next loc in tt stack

   !Debug print statement

!      write(iupr,'(a3,5x,a3,2(7x,a4),3x,a4,5x,a3)')   &
!     "  i","ttr","dxm1","dxm2","tmax","wvs"
!      Do i = 1,3
!   write(iupr,'(i5,f9.4,2f9.4,2f9.4)')  &
!     ind(i),ttr(ind(i)),dxm(1:2,ind(i)),timax(ind(i)),wvs(i)
!      End Do


   !Start the triangle stack (contains vertices of all subtriangles)
   nd(1:3,1) = (/1,2,3/)
   its = 1        !number of triangles in stack

   !2.  Subtriangle stack loop (will continue until triangle stack is empty)
   Do

      !2.1  If stack is full, triangle cannot divide
      If (it4 < tri_tot .and. its < tri_open - 1) Then

  !2.1.1   Change in max traveltime
  dtm1 = Abs(tims(nd(1,its))-tims(nd(2,its)))
  dtm2 = Abs(tims(nd(2,its))-tims(nd(3,its)))
  dtm3 = Abs(tims(nd(3,its))-tims(nd(1,its)))
  dtmx = Max(dtm1, dtm2, dtm3)
  maxtims = Max(tims(nd(1,its)),tims(nd(2,its)),tims(nd(3,its)))

  !Change in midpoint
  dxm1 = dxms(1:2,nd(1,its))-dxms(1:2,nd(2,its))
  dxm2 = dxms(1:2,nd(2,its))-dxms(1:2,nd(3,its))
  dxm3 = dxms(1:2,nd(3,its))-dxms(1:2,nd(1,its))
  dx1sq = dxm1(1)*dxm1(1)+dxm1(2)*dxm1(2)
  dx2sq = dxm2(1)*dxm2(1)+dxm2(2)*dxm2(2)
  dx3sq = dxm3(1)*dxm3(1)+dxm3(2)*dxm3(2)
  dxmsq = max(dx1sq,dx2sq,dx3sq)

  !Is aperture center inside this triangle?
  if(InsideAll) then
     InsideThis = DMAP3D_Util_TstInsideTri(dxf,dxms(1:2,nd(1,its)),   &
      dxms(1:2,nd(2,its)), dxms(1:2,nd(3,its)),tc)

     if(InsideThis) then
!  write(iupr,'(a,3i6)') &
!     " Aperture center inside triangle ",nd(1:3,its)

        maxtims = trlen
        dtm1 = Abs(tims(nd(1,its))-trlen)
        dtm2 = Abs(tims(nd(2,its))-trlen)
        dtm3 = Abs(tims(nd(3,its))-trlen)
        dtmx = Max(dtm1, dtm2, dtm3)
     end if

  end if

  maxttst = 50 + .25*maxtims
      
  !2.1.2   Test whether to subdivide
  If (dtmx > maxttst) Then
     
     !2.1.2.1  Divide subtriangle in two

     !Bisect side with largest change in midpoint
     if (dx1sq == dxmsq) then
        inm = 1
        inp = 2
     elseif (dx2sq == dxmsq) then
        inm = 2
        inp = 3
     else
        inm = 3
        inp = 1
     end if

     !Properties at new node
     wvs(it4)   = .5*(wvs(nd(inm,its))+wvs(nd(inp,its)))
     his(1:2,it4)  = .5*(his(1:2,nd(inm,its))+his(1:2,nd(inp,its)))
     dxms(1:2,it4) = .5*(dxms(1:2,nd(inm,its))+dxms(1:2,nd(inp,its)))
     call DMAP3D_Mkout_par(obj, his(1:2,it4),hf,dxms(1:2,it4),  &
          trlen,vdti,ttrs(it4),tims(it4))

     !Debug print statement
!  !  !  !  write(iupr,'(i5,f9.4,2f9.4,2f9.4,3i4)')!it4,ttrs(it4),!&
!  !  !  !  !!dxms(1,it4),dxms(2,it4),tims(it4),wvs(it4),&
!  !  !  !  !!inm,its!  !

     !Update triangle stack
     nd(1:3,its+1)  = nd(1:3,its)
     nd(inm,its+1)  = it4
     nd(1+mod(inm,3),its) = it4

     !2.1.2.  Increment stack lengths
     it4 = it4 + 1       !Increments number of nodes
     its = its + 1   !Increments number of triangles

     !2.1.2.3 Cycle the process-stack loop 2.
     Cycle

  End If           !Subdivision test if-block 2.1.2

      Else

!  !  !  if!(it4!>=!tri_tot)!then
!  !  !  !  write(iupr,'(i6,a,3i6,a10,i6)')it4,"!triangles!inside!",ind,!  &
!  !  !  !  !  "!level=!",its
!  !  !  !  write(iupr,'("dxms",6f12.4)')dxms(1:2,nd(1:3,its))
!  !  !  !  write(iupr,'("tims",3f12.1)')tims(nd(1:3,its))
!  !  !  end!if

      End If        !End stack-full if-block 2.1


      !Ready to process subtriangle

      !2.2  Check whether maximum time is large enough to process
      mtiml =nint(max(tims(nd(1,its)),tims(nd(2,its)),tims(nd(3,its))))

      !Debug print statement

!   write(iupr,'(a4,6i5,5f9.1)') "Prc",  &
!   it4, its, nd(1:3,its), mtiml,tims(nd(1:3,its)),dtmx,maxttst

      if(mtiml > obj%itmin ) then

  !2.2.1   Calculate weight-factor polynomial for this subtriangle

  !Vertices of current subtriangle, sorted in increasing traveltime
  If (ttrs(nd(1,its)) <= ttrs(nd(2,its)) ) then
     if (ttrs(nd(2,its)) <= ttrs(nd(3,its))) then
        ir = (/1,2,3/)
     elseif (ttrs(nd(1,its)) <= ttrs(nd(3,its))) then
        ir = (/1,3,2/)
     else
        ir = (/3,1,2/)
     end if
  Else
     if(ttrs(nd(1,its)) <= ttrs(nd(3,its))) then
        ir = (/2,1,3/)
     elseif (ttrs(nd(2,its)) <= ttrs(nd(3,its))) then
        ir = (/2,3,1/)
     else
        ir = (/3,2,1/)
     end if
  End If

  it1 = nd(ir(1),its)     !Vertex with smallest traveltime ratio
  it2 = nd(ir(2),its)
  it3 = nd(ir(3),its)     !Vertex with largest traveltime ratio

  !Coordinates of triangle center
  hic  = (his(1:2,it1)  + his(1:2,it2)  + his(1:2,it3))*.333333
  dxmc = (dxms(1:2,it1) + dxms(1:2,it2) + dxms(1:2,it3))*.333333

  !Cross Products
  hif  =  hic(1)*hf(2) - hic(2)*hf(1)
  hidx =  hic(1)*dxmc(2) - hic(2)*dxmc(1)
  hfdx =  hf(1)*dxmc(2) - hf(2)*dxmc(1)
  ahif =  Abs(hif)

  !aperture check
  if(abs(hidx)>=ahif .or. abs(hfdx)>=ahif) then
     amp = 0.
     wna = 0.
  else

     dcis   =  hfdx / hif
     dcfs   =  hidx / hif
     dcic  = 1. - dcis*dcis
     dcfc  = 1. - dcfs*dcfs

     !Calculate weight factors.
     dxba = dxms(1:2,it2)-dxms(1:2,it1)  !dxm between b and a
     dxcb = dxms(1:2,it3)-dxms(1:2,it2)  !dxm between c and b
     dhba = his (1:2,it2)-his (1:2,it1)  !dxh between b and a
     dhcb = his (1:2,it3)-his (1:2,it2)  !dxh between c and b

     !Triangle area
     Area = .5 * Abs(dxba(1)*dxcb(2)-dxba(2)*dxcb(1))

     !Triangle area must be smaller than parallelogram area
     ahif = Max(ahif, .25*Area)
     hif  = Sign(ahif, hif)

     !Calculate the Dj matrices (eq 37)
     dba  = dxba + dcis * dhba
     dcb  = dxcb + dcis * dhcb

     !Normalized wave numbers
     wnd    =    1./(dcfc * dcfc * hif)
     wna(1) =   (dcis*hf(2)*dcfc - dcfs*hic(2)*dcic)*wnd
     wna(2) = - (dcis*hf(1)*dcfc - dcfs*hic(1)*dcic)*wnd

     !amplitude factor
     Amp = .159155 /ahif * Abs( Min( obj%Afmax * Area, (                      &
       (1.+dcis*dcis)/(dcic * dcfc) * (dba(1)*dcb(2) - dba(2)*dcb(1))      -  &
       (wna(1)*dhba(1) + wna(2)*dhba(2)) * (hic(2)*dcb(1) - hic(1)*dcb(2)) +  &
       (wna(1)*dhcb(1) + wna(2)*dhcb(2)) * (hic(2)*dba(1) - hic(1)*dba(2))    &
       )  )  )

  End if

  !weight factor function calculation. First calc extremal ttrs
  ttl = ttrs(it1)
  ttg = ttrs(it3)
  ttm = ttrs(it2)

  !weight function at vertices
  wa = wvs(it1)
  wb = wvs(it2)
  wc = wvs(it3)

  !traveltime ratio differences
  tt13 = ttg - ttl
  tt12 = ttm - ttl
  tt23 = ttg - ttm

  !Calculate weight factor polynomial for this subtriangle

  !coefficients for interval (ttl,ttm)
  if(ttl < ttm) then

     cwfd   = amp / (tt12*tt12*tt13*tt13)
     !coefficient of (t-ttl)**2
     Adrv(3) = cwfd * ((wb-wa)*tt13 + (wc-wa)*tt12) * sixth
     !coefficient of (t-ttl)
     Adrv(1) = cwfd * wa * tt12 * tt13 * .5

  end if

  !coefficients for interval (ttm,ttg)
  if (ttm < ttg ) then

     cwfd   = amp / (tt23*tt23*tt13*tt13)
     !coefficient of (ttg-t)**2
     Adrv(4) = cwfd * ((wb-wc)*tt13 + (wa-wc)*tt23) * sixth
     !coefficient of (ttg-t)
     Adrv(2) = cwfd * wc * tt23 * tt13 * .5
     !start of polynomial range

  end if

  trval = (/ttl,ttm,ttg/)
  call DMAP3D_MkOut_Lwf(obj,Din,Dout,trval,Adrv,mtiml,stpr,Mute,Aout)

      End If     !End of maximum traveltim if-block 2.2

      !2.3  Decrement the triangle stack. If it is empty, quit
      its = its - 1

      !2.4  If stack is empty, quit
      if (its < 1) Exit

   End do            !end of stack do-loop 2.

   end subroutine DMAP3D_mkout_MakeDST


!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module DMAP3D_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

