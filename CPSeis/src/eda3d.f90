!<CPS_v1 type="PROCESS"/>
!!------------------------------- eda3d.f90 ---------------------------------!!
!!------------------------------- eda3d.f90 ---------------------------------!!
!!------------------------------- eda3d.f90 ---------------------------------!!


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
! Name       : EDA3D          (3D Edge Detection Attributes)
! Category   : filters
! Written    : 2001-08-17   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Calculate edge detection attributes from 3D data volume.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! EDA3D replaces the input traces by edge detection attribute traces.
! The edge detection attribute is large at locations where a discontinuity
! (such as a fault) is found in the input data.
!
! Edge detection is usually run on 3D migrated data and viewed in time slices.
!
! EDA3D is functionally similar to SDIP3D, except that EDA3D generates edge
! detection traces instead of mixed traces.  In both processes, a 3D moving
! gather is maintained using the MGATHER3D primitive.  Also in both processes,
! the SDIPUTIL primitive is used to do the dip search and calculate the
! output traces.
!
! This EDA3D process is equivalent to the original EDA process, except that
! this new process has the following additional capabilities:
!
!  (1) This new process can do dip searches prior to calculating the edge
!      detection attributes along the dominant dips.
!
!  (2) This new process does not require a GATHER in front of the process.
!
!  (3) This new process can be made to run significantly faster (with with
!      a lossof resolution) by setting the NSKIP parameter to increment the
!      edge detection calculation by moving more than one trace sample at
!      a time.
!
!  (4) This new process can also calculate a different type of edge
!      detection calculation which is faster but not well evaluated.
!
!  (5) This new process can also output several types of diagnostic traces.
!
!  (6) This new process shares most of its code with the SDIP3D and SDIP
!      processes, improving the maintainability of all three processes.
!
!-------------------------------------------------------------------------------
!                           DETAILS OF OPERATION
!
! 1. Within each window, (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) traces,
!    centered on the input trace, are used to perform a semblance dip search
!    to determine which dip has the maximum semblance.  A total of
!    (2*DIP_MAX_X + 1) times (2*DIP_MAX_Y + 1) dips are tested.  The dip with
!    the maximum semblance is referred to as the dominant dip.  Each window
!    length is WIN_LEN seconds long, and the windows move down the trace one
!    sample at a time.
!
!
! 2. Using new smaller windows, a total of (2*NUM_TR_MIX_X + 1) times
!    (2*NUM_TR_MIX_Y + 1) traces, centered on the input trace, are used to
!    obtain coherency values to use for calculating the edge detection
!    attributes.  These coherency values are calculated along the dominant
!    dip previously found.  Each of these new smaller windows is WIN_NSAMP
!    trace samples long, and the windows move down the trace one sample at
!    a time.
!
!     (a) For Robert Meek's edge detection, the coherency values are obtained
!         using a matrix singular value decomposition algorithm.
!
!     (b) For semblance edge detection, the coherency values are simply
!         obtained from semblance values.
!
!
! 3. The output trace is formed from the coherency values, sample by sample,
!    using the following equation:
!
!      TR_OUT =   1 - COHERENCY**PWR_EDGE     for Meek edge detection.
!      TR_OUT =  (1 - COHERENCY)**PWR_EDGE    for semblance edge detection.
!
!    where:
!
!      TR_OUT    = Output trace.
!      COHERENCY = Local coherency value measured along the dominant dip.
!      PWR_EDGE  = Power to raise the coherency value.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!  1. The bin centers and bin widths are used to identify the locations
!     of traces to use for dip searching and calculating the edge detection
!     attributes.  This insures that the correct traces are used in both
!     the X and Y directions, even if the starting X coordinates of different
!     lines are not the same or there are missing traces.  The following
!     binning parameters:
!                        X_INIT       Y_INIT
!                        X_INC        Y_INC
!     should match the actual inline and crossline spacing of the input
!     traces.  If two or more traces fall into the same bin, only one
!     trace in the bin will be used for dip searching and edge detection
!     calculations.
!
!
!  4. Example of use of the following dip parameters:
!                        DIP_X         DIP_Y
!                        DIP_INC_X     DIP_INC_Y
!
!     Using DIP_X = 10 and DIP_INC_X = 2:
!            five negative X-dips (-10,-8,-6,-4,-2),
!            one zero X-dip,
!            and five positive X-dips (2,4,6,8,10) will be tested.
!
!     Using DIP_Y = 6 and DIP_INC_Y = 3:
!            two negative Y-dips (-6,-3),
!            one zero Y-dip,
!            and two positive Y-dips (3,6) will be tested.
!
!     WARNING: Computer time is proportional to the product of the number
!     of dips tested in each direction!
!
!
!  6. The input traces should be sorted to either inline or crossline
!     order.  The inline direction corresponds to traces in the same
!     Y bin, and with X bins incrementing by one bin per trace.  The
!     crossline direction corresponds to traces in the same X bin, and
!     with Y bins incrementing by one bin per trace.  The trace spacing
!     should be such that there is generally one trace in each bin,
!     although some bins can be empty.
!
!
!  7. Each output trace is an edge detection attribute version of each
!     input trace.  Traces are output in the same order as they are input.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a multiple-trace process.
! Process requires traces to be input with the primary sort in the Y
! direction and the secondary sort in the X direction.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
! This process normally outputs one trace at a time.
!
! For the gather output options, the entire moving gather associated with
! each input trace is output at one time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! GATHERED  whether traces are a legitimate gather  set to false
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
! For the gather output options, NUMTR is set > 1 and GATHERED to true.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#          Description                Action taken
! ----          -----------                ------------
! 2             Head mute index            used but not changed
! 25            LAV                        reset
! 64            Tail mute index            used but not changed
! HDR_X         X coordinate               used but not changed
! HDR_Y         Y coordinate               used but not changed
! 58,59,60,61   Scratch                    used
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!006. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!005. 2006-01-10  B. Menger  Removed Unused Variables.
!  4. 2002-03-04  Stoeckley  Set limits on number of displayed digits in GUI
!                             for floating point numbers to correct an
!                             occasional GUI display problem.
!  3. 2001-12-13  Stoeckley  Remove the WIN_NSKIP parameter (is now always 1);
!                             change the DIP_INC_X and DIP_INC_Y parameters
!                             to be informational only (now automatically
!                             calculated to protect against aliasing); change
!                             the PWR parameter to PWR_EDGE (old name will
!                             still work upon input); add MAX_Y_BINS parameter;
!                             put output diagnostics onto separate trace files
!                             so that they can all be obtained at the same
!                             time; improve the dip search algorithm.
!  2. 2001-08-24  Stoeckley  Add options for speeding up dip searches.
!  1. 2001-08-17  Stoeckley  Initial version, made by altering the SDIP3D
!                            process and including (in the SDIPUTIL primitive)
!                            Rob Meek's edge detection algorithm from the
!                            original EDA process.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES           
!
! No known limitations, but see comments in SDIP.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
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
!  NTR == 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
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
!<NS EDA3D Process/NC=80>
!                       3D Edge Detection Attribute
!
! OPT_OUTPUT=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCC     QUICK_DIP_WEIGHTS=`KKK
!                                               QUICK_DIP_SEARCH =`KKK
! PWR_EDGE~~=`FFFFFF [/L]Power of coherency for edge detection.
! WIN_LEN~~~=`FFFFFF [/L]Semblance window length in seconds for dip search.
! WIN_NSAMP =`II     [/L]Number of trace samples in edge detection window.
!
! `-------------------- `--------------------
!  HDR_X~~~~~~~=`II      HDR_Y~~~~~~~=`II     [/L]Header word.
!  X_INIT~~~~~~=`FFFFF   Y_INIT~~~~~~=`FFFFF  [/L]First (or any) bin center.
!  X_INC~~~~~~~=`FFFFF   Y_INC~~~~~~~=`FFFFF  [/L]Bin increment.
!  MAX_X_BINS~~=`IIIII   MAX_Y_BINS~~=`IIIII  [/L]Maximum approximate number of bins (traces).
!
!  DIP_MAX_X~~~=`FFFFF   DIP_MAX_Y~~~=`FFFFF  [/L]Maximum dip (ms/trace).
!  dip_inc_x~~~=`XXXXX   dip_inc_y~~~=`XXXXX  [/L]Dip increment (ms/trace).
!  nxdips~~~~~~=`XXXXX   nydips~~~~~~=`XXXXX  [/L]Total number of dips.
!
!  NUM_TR_DIP_X=`II      NUM_TR_DIP_Y=`II     [/L]#Traces each side for dip search.
!  nxgather~~~~=`XX      nygather~~~~=`XX     [/L]Total #traces for dip search.
!
!  NUM_TR_MIX_X=`II      NUM_TR_MIX_Y=`II     [/L]#Traces each side for edge detection.
!  nxmix~~~~~~~=`XX      nymix~~~~~~~=`XX     [/L]Total #traces for edge detection.
! `-------------------- `--------------------
!
!<include sdiputil.f90>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="nxdips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested in X direction. </Tip>
!</Help>
!
!<Help KEYWORD="nydips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested in Y direction. </Tip>
!</Help>
!
!
!<Help KEYWORD="nxgather" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in X direction for dip search. </Tip>
!</Help>
!
!<Help KEYWORD="nygather" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in Y direction for dip search. </Tip>
!</Help>
!
!
!<Help KEYWORD="nxmix" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in X direction to use for edge detection. </Tip>
!</Help>
!
!<Help KEYWORD="nymix" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in Y direction to use for edge detection. </Tip>
!</Help>
!
!
!
!<Help KEYWORD="OPT_OUTPUT">
!<Tip> Type of traces to output to the next process. </Tip>
! Default = MEEK EDGE DETECTION
! Allowed = MEEK EDGE DETECTION      (Meek edge detection attribute)
! Allowed = SEMBLANCE EDGE DETECTION (semblance edge detection attribute)
! Allowed = MIXED TRACES             (trace mixed along dips)
! Allowed = semblance values         (semblance values found during dip search)
! Allowed = dip_x values             (X dips found during the dip search)
! Allowed = dip_y values             (Y dips found during the dip search)
! Allowed = original gathers         (original moving gathers for dip search)
! Allowed = flattened gathers        (flattened moving gathers from dip search)
! Allowed = middle trace in gather   (middle trace in each moving gather)
!
! MEEK EDGE DETECTION:
! SEMBLANCE EDGE DETECTION:
!
!   Each input trace is replaced by a trace which contains an edge detection
!   attribute for each trace sample.  The edge detection attribute is large
!   at locations where a discontinuity (such as a fault) is found in the
!   input data.
!
!   MEEK edge detection is the algorithm available in the older EDA process.
!   MEEK edge detection has higher resolution than SEMBLANCE edge detection,
!   and also probably has other superior characteristics, but takes longer to
!   calculate.
!
! MIXED TRACES:
!
!   Each input trace is replaced by a mixed trace.  This is the same result
!   which is output by the SDIP and SDIP3D processes.
!
! semblance values:
! dip_x values:
! dip_y values:
! 
!   Each input trace is replaced by a trace which contains a semblance value
!   (between 0 and 1) or a dip value (in ms/trace) found from the dip search.
!
! original gathers:  
! flattened gathers:
!
!   Each input trace is replaced by the entire moving gather of traces used
!   to do the dip search.
!
! middle trace in gather: 
!
!   Each input trace is replaced by the middle trace in the moving gather
!   of traces used for the dip search.  Normally this trace will be the
!   same as the input trace, allowing the user to verify the accuracy of
!   the code which assembles the moving gathers.
!</Help>
!
!
!<Help KEYWORD="QUICK_DIP_WEIGHTS">
!<Tip> Option whether to decimate the traces used for the dip search. </Tip>
! Default = YES
! Allowed = YES  use a subset of the traces in moving gather for dip search.
! Allowed = NO   use all traces in moving gather for dip search.
!
! If QUICK_DIP_WEIGHTS = YES, only the traces along the two orthogonal axes
! through the middle of the moving gather, plus all additional traces to be
! subsequently used in the edge detection calculation, are used in the dip
! search.
!
! For example, using a moving gather containing 7 x 7 traces total, with plans
! to use 3 x 3 traces in the middle of the moving gather to calculate the
! edge detection attribute:
!
!   QUICK_DIP_WEIGHTS = NO will use all 49 traces for the dip search.
!   QUICK_DIP_WEIGHTS = YES will use only 17 traces for the dip search.
!
! Using QUICK_DIP_WEIGHTS = YES and/or QUICK_DIP_SEARCH = YES will speed up
! the dip search, which dominates the timing when dip searching is being done,
! with probably little loss of quality.
!</Help>
!
!
!<Help KEYWORD="QUICK_DIP_SEARCH">
!<Tip> Option whether to decimate the dip searches being performed. </Tip>
! Default = YES
! Allowed = YES  use a subset of combinations of X and Y dips for dip search.
! Allowed = NO   use all combinations of X and Y dips for dip search.
! 
! If QUICK_DIP_SEARCH = YES, only the dips in the X and Y orthogonal
! directions, but not dips in other directions, are used in the dip search.
!
! For example, testing 7 dips in the X direction and 7 dips in the Y direction: 
!
!   QUICK_DIP_SEARCH = NO will use all 49 dip combinations in the dip search.
!   QUICK_DIP_SEARCH = YES will use only 14 dips in the dip search.
!
! Using QUICK_DIP_WEIGHTS = YES and/or QUICK_DIP_SEARCH = YES will speed up
! the dip search, which dominates the timing when dip searching is being done,
! with probably little loss of quality.
!</Help>
!
!
!<Help KEYWORD="PWR_EDGE">
!<Tip> Raise local coherency to the PWR_EDGE power for edge detection. </Tip>
! Default = 2.0
! Allowed = real >= 1.0
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the time window, in sec, for the semblance calculation. </Tip>
! Default = 0.1
! Allowed = real > 0.0
!
! WIN_LEN is used for dip search calculations prior to generating edge
! detection attributes.
!</Help>
!
!
!<Help KEYWORD="WIN_NSAMP">
!<Tip> Length of the time window, in trace samples, for edge detection. </Tip>
! Default = 5
! Allowed = integer >= 1
!
! (2*NUM_TR_MIX_X + 1) times (2*NUM_TR_MIX_Y + 1) must be greater than
! WIN_NSAMP for MEEK edge detection.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX_X">
!<Tip> Maximum dip in X direction to use in the dip search calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! Dips are in ms/trace in the X direction.
!
! A total of (2*DIP_X/DIP_INC_X + 1) dips are tested in the X direction,
! from -DIP_MAX_X to +DIP_MAX_X.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX_Y">
!<Tip> Maximum dip in Y direction to use in the dip search calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! Dips are in ms/trace in the Y direction.
!
! A total of (2*DIP_X/DIP_INC_Y + 1) dips are tested in the Y direction,
! from -DIP_MAX_Y to +DIP_MAX_Y.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!</Help>
!
!
!<Help KEYWORD="dip_inc_x" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment in X direction to use in the dip search calculation. </Tip>
!
! Dip increments are in ms/trace in the X direction.
!
! A total of (2*DIP_X/DIP_INC_X + 1) dips are tested in the X direction,
! from -DIP_MAX_X to +DIP_MAX_X.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!
! This value is calculated from the Nyquist frequency and the NUM_TR_DIP_X
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="dip_inc_y" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment in Y direction to use in the dip search calculation. </Tip>
!
! Dip increments are in ms/trace in the Y direction.
!
! A total of (2*DIP_X/DIP_INC_Y + 1) dips are tested in the Y direction,
! from -DIP_MAX_Y to +DIP_MAX_Y.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!
! This value is calculated from the Nyquist frequency and the NUM_TR_DIP_Y
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_DIP_X">
!<Tip> Number of traces on each side in X direction for dip testing. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of traces on each side of the center trace.
! A total of (2*NUM_TR_DIP_X + 1) traces are used in the dip search calculation
! in the X direction.
!
! A total of (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) traces are used
! altogether in the dip search calculation.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_DIP_Y">
!<Tip> Number of traces on each side in Y direction for dip testing. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of traces on each side of the center trace.
! A total of (2*NUM_TR_DIP_Y + 1) traces are used in the dip search calculation
! in the Y direction.
!
! A total of (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) traces are used
! altogether in the dip search calculation.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_MIX_X">
!<Tip> Number of traces on each side in X direction for edge detection. </Tip>
! Default = 1
! Allowed = integer >= 0 and =< NUM_TR_DIP_X
!
! This is the number of traces on each side of the center trace.
!
! A total of (2*NUM_TR_MIX_X + 1) times (2*NUM_TR_MIX_Y + 1) traces are
! used for edge detection in the X direction.
!
! (2*NUM_TR_MIX_X + 1) times (2*NUM_TR_MIX_Y + 1) must be greater than
! WIN_NSAMP for MEEK edge detection.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_MIX_Y">
!<Tip> Number of traces on each side in Y direction for edge detection. </Tip>
! Default = 1
! Allowed = integer >= 0 and =< NUM_TR_DIP_X
!
! This is the number of traces on each side of the center trace.
!
! A total of (2*NUM_TR_MIX_Y + 1) times (2*NUM_TR_MIX_Y + 1) traces are
! used for edge detection in the Y direction.
!
! (2*NUM_TR_MIX_X + 1) times (2*NUM_TR_MIX_Y + 1) must be greater than
! WIN_NSAMP for MEEK edge detection.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="X_INIT">
!<Tip> X coordinate of the first (or any) trace in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin measured in the X direction.
! This value does not have to correspond to the first actual trace in the
! X direction, since this could in fact vary from line to line.
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Y coordinate of the first (or any) trace in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin measured in the Y direction.
! This value does not have to correspond to the first actual trace (or
! first actual line) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip> Increment of HDR_X between bin centers in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the bin increment (or width) in the X direction.
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y between bin centers in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the bin increment (or width) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="MAX_X_BINS">
!<Tip> Maximum number of X bins (traces) in any one line. </Tip>
! Default = blank
! Allowed = integer > 0
!
! This value must be specified.  The maximum number of traces which will
! be stored on disk at any one time will be slightly greater than MAX_X_BINS
! times (2*NUM_TR_DIP_Y + 1).
!</Help>
!
!
!<Help KEYWORD="MAX_Y_BINS">
!<Tip> Maximum number of Y bins (lines). </Tip>
! Default = blank
! Allowed = integer > 0 (or blank if not requesting diagnostic traces)
!
! This value must be specified if you are requesting any diagnostic output
! files (see separate screen).
!
! This parameter is needed only for reserving disk space for the requested
! files, and needs to be approximate only.  But if the files will be
! very large, the parameter is important because the number of available
! file extents may otherwise be insufficient.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module eda3d_module
      use pc_module
      use named_constants_module
      use mgather3d_module
      use sdiputil_module
      use tweights_module
      implicit none
      private
      public :: eda3d_create
      public :: eda3d_initialize
      public :: eda3d_update
      public :: eda3d_delete
!<execute_only>
      public :: eda3d            ! main execution (trace processing) routine.
      public :: eda3d_wrapup
!</execute_only>


      character(len=100),public,save :: EDA3D_IDENT = &
'$Id: eda3d.f90,v 1.6 2006/09/18 13:32:46 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: eda3d_struct

        private
        logical             :: skip_wrapup               ! wrapup flag
        integer             :: nwih,ndpt                 ! globals
        real                :: tstrt,dt                  ! globals

        character(len=28)   :: opt_output                ! process parameters
        real                :: pwr_edge                  ! process parameters
        real                :: pwr_semb                  ! not in gui
        logical             :: opt_semb                  ! not in gui
        real                :: fctr_mix                  ! not in gui
        character(len=8)    :: opt_taper                 ! not in gui
        real                :: win_len                   ! process parameters
        integer             :: win_nsamp                 ! process parameters
        real                :: dip_max_x                 ! process parameters
        real                :: dip_max_y                 ! process parameters
        integer             :: num_tr_dip_x              ! process parameters
        integer             :: num_tr_dip_y              ! process parameters
        integer             :: num_tr_mix_x              ! process parameters
        integer             :: num_tr_mix_y              ! process parameters
        integer             :: hdr_x                     ! process parameters
        integer             :: hdr_y                     ! process parameters
        real                :: x_init                    ! process parameters
        real                :: y_init                    ! process parameters
        real                :: x_inc                     ! process parameters
        real                :: y_inc                     ! process parameters
        integer             :: max_x_bins                ! process parameters
        integer             :: max_y_bins                ! process parameters
        logical             :: quick_dip_weights         ! process parameters
        logical             :: quick_dip_search          ! process parameters

        integer                        :: ngather        ! dependent
        type(mgather3d_struct),pointer :: mgather3d      ! dependent
        type(sdiputil_struct) ,pointer :: sdiputil       ! dependent

      end type eda3d_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(eda3d_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: SCRATCH_NSX = HDR_SCRATCH_58
      integer,parameter     :: SCRATCH_NSY = HDR_SCRATCH_59
      integer,parameter     :: SCRATCH_NSW = HDR_SCRATCH_60
      integer,parameter     :: SCRATCH_NSF = HDR_SCRATCH_61

      integer,parameter      :: opt_output_nopt = 9

      character(len=28),save :: opt_output_options (opt_output_nopt)

      data opt_output_options /'MEEK EDGE DETECTION',           &
                               'SEMBLANCE EDGE DETECTION',      &
                               'MIXED TRACES',                  &
                               'semblance values',              &
                               'dip_x values',                  &
                               'dip_y values',                  &
                               'original gathers',              &
                               'flattened gathers',             &
                               'middle trace in gather'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine eda3d_create (obj)
      implicit none
      type(eda3d_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%mgather3d)
      nullify (obj%sdiputil) ! jpa

      call sdiputil_create  (obj%sdiputil)
      call eda3d_initialize (obj)
      return
      end subroutine eda3d_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine eda3d_delete (obj)
      implicit none
      type(eda3d_struct),pointer :: obj       ! arguments

!<execute_only>
      call eda3d_wrapup (obj)
!</execute_only>
      call sdiputil_delete (obj%sdiputil)

      deallocate(obj)
      return
      end subroutine eda3d_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine eda3d_initialize (obj)
      implicit none
      type(eda3d_struct),intent(inout) :: obj       ! arguments

      obj%opt_output         = 'MEEK EDGE DETECTION'
      obj%pwr_edge           = 2.0
      obj%pwr_semb           = 0.5        ! not in gui.
      obj%opt_semb           = .false.    ! not in gui.
      obj%fctr_mix           = 0.5        ! not in gui.
      obj%opt_taper          = 'NONE'     ! not in gui.
      obj%win_len            = 0.10
      obj%win_nsamp          = 5
      obj%dip_max_x          = 10.0
      obj%dip_max_y          = 10.0
      obj%num_tr_dip_x       = 3
      obj%num_tr_dip_y       = 3
      obj%num_tr_mix_x       = 1
      obj%num_tr_mix_y       = 1
      obj%hdr_x              = 7
      obj%hdr_y              = 8
      obj%x_init             = 1.0
      obj%y_init             = 1.0
      obj%x_inc              = 1.0
      obj%y_inc              = 1.0
      obj%max_x_bins         = INIL
      obj%max_y_bins         = INIL
      obj%quick_dip_weights  = .true.
      obj%quick_dip_search   = .true.

      call sdiputil_initialize (obj%sdiputil)
      call eda3d_update        (obj)
      return
      end subroutine eda3d_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine eda3d_update (obj)
      implicit none
      type(eda3d_struct),intent(inout),target :: obj               ! arguments
      integer                     ::       lun ! local
      integer                     :: nstore,nscratch               ! local
      real,allocatable            :: xweights(:)                   ! local
      real,allocatable            :: yweights(:)                   ! local
      integer                     :: nxgather,nxmix,nxdips         ! local
      integer                     :: nygather,nymix,nydips         ! local
      real                        :: dip_inc_x,dip_inc_y           ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      lun = pc_get_lun()

      call pc_get_global ('nwih'              , obj%nwih)
      call pc_get_global ('ndpt'              , obj%ndpt)
      call pc_get_global ('tstrt'             , obj%tstrt)
      call pc_get_global ('dt'                , obj%dt)

      call pc_get        ('opt_output'        , obj%opt_output        )
      call pc_get        ('pwr_edge'          , obj%pwr_edge          )
      call pc_get        ('win_len'           , obj%win_len           )
      call pc_get        ('win_nsamp'         , obj%win_nsamp         )
      call pc_get        ('dip_max_x'         , obj%dip_max_x         )
      call pc_get        ('dip_max_y'         , obj%dip_max_y         )
      call pc_get        ('num_tr_dip_x'      , obj%num_tr_dip_x      )
      call pc_get        ('num_tr_dip_y'      , obj%num_tr_dip_y      )
      call pc_get        ('num_tr_mix_x'      , obj%num_tr_mix_x      )
      call pc_get        ('num_tr_mix_y'      , obj%num_tr_mix_y      )
      call pc_get        ('hdr_x'             , obj%hdr_x             )
      call pc_get        ('hdr_y'             , obj%hdr_y             )
      call pc_get        ('x_init'            , obj%x_init            )
      call pc_get        ('y_init'            , obj%y_init            )
      call pc_get        ('x_inc'             , obj%x_inc             )
      call pc_get        ('y_inc'             , obj%y_inc             )
      call pc_get        ('max_x_bins'        , obj%max_x_bins        )
      call pc_get        ('max_y_bins'        , obj%max_y_bins        )
      call pc_get        ('quick_dip_weights' , obj%quick_dip_weights )
      call pc_get        ('quick_dip_search'  , obj%quick_dip_search  )

      call pc_get ('pwr', obj%pwr_edge)    ! for backward compatibility.


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call sdiputil_update (obj%sdiputil, obj%opt_output,                 &
                 obj%pwr_edge, obj%pwr_semb, obj%opt_semb, obj%fctr_mix,  &
                 obj%win_len          , obj%win_nsamp       ,             &
                 obj%quick_dip_weights, obj%quick_dip_search,             &
                 obj%dip_max_x        , obj%dip_max_y       ,             &
                 obj%num_tr_dip_x     , obj%num_tr_dip_y    ,             &
                 obj%num_tr_mix_x     , obj%num_tr_mix_y    ,             &
                 obj%hdr_x            , obj%hdr_y           ,             &
                 obj%x_inc            , obj%y_inc           ,             &
                 obj%max_x_bins       , obj%max_y_bins      ,             &
                 nxgather             , nygather            ,             &
                 nxmix                , nymix               ,             &
                 nxdips               , nydips              ,             &
                 dip_inc_x            , dip_inc_y)

      obj%ngather = nxgather * nygather


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


 call pc_put_options_field ('opt_output', opt_output_options, opt_output_nopt)

!     call pc_put_global ('gathered'   , .false.)    ! set by sdiputil.
!     call pc_put_global ('numtr'      ,       1)    ! set by sdiputil.

      call pc_put        ('opt_output'        , obj%opt_output          )
      call pc_put        ('pwr_edge'          , obj%pwr_edge          ,7)
      call pc_put        ('win_len'           , obj%win_len           ,7)
      call pc_put        ('win_nsamp'         , obj%win_nsamp           )
      call pc_put        ('dip_max_x'         , obj%dip_max_x         ,6)
      call pc_put        ('dip_max_y'         , obj%dip_max_y         ,6)
      call pc_put        ('num_tr_dip_x'      , obj%num_tr_dip_x        )
      call pc_put        ('num_tr_dip_y'      , obj%num_tr_dip_y        )
      call pc_put        ('num_tr_mix_x'      , obj%num_tr_mix_x        )
      call pc_put        ('num_tr_mix_y'      , obj%num_tr_mix_y        )
      call pc_put        ('hdr_x'             , obj%hdr_x               )
      call pc_put        ('hdr_y'             , obj%hdr_y               )
      call pc_put        ('x_init'            , obj%x_init            ,6)
      call pc_put        ('y_init'            , obj%y_init            ,6)
      call pc_put        ('x_inc'             , obj%x_inc             ,6)
      call pc_put        ('y_inc'             , obj%y_inc             ,6)
      call pc_put        ('max_x_bins'        , obj%max_x_bins          )
      call pc_put        ('max_y_bins'        , obj%max_y_bins          )
      call pc_put        ('quick_dip_weights' , obj%quick_dip_weights   )
      call pc_put        ('quick_dip_search'  , obj%quick_dip_search    )

      call pc_put_gui_only ('dip_inc_x' , dip_inc_x      ,6,2)
      call pc_put_gui_only ('dip_inc_y' , dip_inc_y      ,6,2)
      call pc_put_gui_only ('nxdips'    , nxdips         )
      call pc_put_gui_only ('nydips'    , nydips         )
      call pc_put_gui_only ('nxgather'  , nxgather       )
      call pc_put_gui_only ('nygather'  , nygather       )
      call pc_put_gui_only ('nxmix'     , nxmix          )
      call pc_put_gui_only ('nymix'     , nymix          )

      nscratch = mgather3d_scratch(obj%nwih,obj%ndpt,nxgather,nygather) &
                   + obj%ngather * (obj%nwih + obj%ndpt)
      nstore   = mgather3d_store &
                      (obj%nwih,obj%ndpt,nxgather,nygather,obj%max_x_bins)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call mgather3d_delete (obj%mgather3d)

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call pc_print ('EDA3D: max number of traces used in X direction =', &
                                   nxgather,'(for dip search)')
      call pc_print ('EDA3D: max number of traces used in Y direction =', &
                                   nygather,'(for dip search)')
      call pc_print ('EDA3D: max number of traces used in X direction =', &
                                   nxmix,'(for edge detection)')
      call pc_print ('EDA3D: max number of traces used in Y direction =', &
                                   nymix,'(for edge detection)')

      allocate (xweights(nxgather))
      allocate (yweights(nygather))

      call tweights_calculate ('NONE', xweights, nxgather, nxmix)
      call tweights_calculate ('NONE', yweights, nygather, nymix)

      write(lun,*) 'EDA3D: X weights = ',xweights
      write(lun,*) 'EDA3D: Y weights = ',yweights

      call mgather3d_create (obj%mgather3d, lun, obj%nwih, obj%ndpt,     &
                             obj%hdr_x, obj%hdr_y,                       &
                             obj%x_init, obj%y_init,                     &
                             obj%x_inc, obj%y_inc,                       &
                             nxgather, nygather,                         &
                             xweights, yweights,                         &
                             SCRATCH_NSX, SCRATCH_NSY,                   &
                             SCRATCH_NSW, SCRATCH_NSF, obj%max_x_bins,   &
                             'EDA3D')

      deallocate (xweights)
      deallocate (yweights)

      call sdiputil_prepare (obj%sdiputil,SCRATCH_NSX,SCRATCH_NSY,SCRATCH_NSW)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

!</execute_only>

      return
      end subroutine eda3d_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine eda3d (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(eda3d_struct),intent(inout) :: obj                  ! arguments
      integer           ,intent(inout) :: ntr                  ! arguments
      double precision  ,intent(inout) :: hdi(:,:)             ! arguments
      real              ,intent(in)    :: tri(:,:)             ! arguments
      double precision  ,intent(out)   :: hdo(:,:)             ! arguments
      real              ,intent(out)   :: tro(:,:)             ! arguments
      double precision       :: hdmove(obj%nwih,obj%ngather)   ! local
      real                   :: trmove(obj%ndpt,obj%ngather)   ! local
      integer                 :: mid                           ! local
      real                    :: xmid,ymid                     ! local

!----------get the mix gather.

      call mgather3d (obj%mgather3d,ntr,hdi,tri,hdmove,trmove)

      if (ntr == NEED_TRACES) return

      if (ntr == FATAL_ERROR) then
           call pc_error ('EDA3D: FATAL ERROR IN MGATHER3D')
           call eda3d_wrapup (obj)
           return
      end if

      if (ntr == NO_MORE_TRACES) then
           call eda3d_wrapup (obj)
           return
      end if

!----------get output trace.

      mid  = (ntr+1)/2
      xmid = hdmove(SCRATCH_NSX,mid)
      ymid = hdmove(SCRATCH_NSY,mid)

      call sdiputil_solve (obj%sdiputil,ntr,hdmove,trmove,xmid,ymid,hdo,tro)

      if (ntr == FATAL_ERROR) then
           call pc_error ('EDA3D: FATAL ERROR IN SDIPUTIL')
           call eda3d_wrapup (obj)
      end if
      return
      end subroutine eda3d


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine eda3d_wrapup (obj)
      implicit none
      type(eda3d_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print         (' ')
      call pc_print         ('EDA3D: WRAPUP')
      call mgather3d_delete (obj%mgather3d)
      call sdiputil_wrapup  (obj%sdiputil)
      call pc_print         ('EDA3D: FINISHED')
      call pc_print         (' ')
      return
      end subroutine eda3d_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>

      end module eda3d_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

