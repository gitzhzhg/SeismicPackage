!<CPS_v1 type="PROCESS"/>
!!------------------------------ cnearts.f90 --------------------------------!!
!!------------------------------ cnearts.f90 --------------------------------!!
!!------------------------------ cnearts.f90 --------------------------------!!


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
! Name       : CNEARTS
! Category   : filters
! Written    : 2003-11-07   by: Bill Done
! Revised    : 2007-01-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Create and insert missing near offset traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Multiple attenuation algorithms currently under development require that
! near offset traces be present. Unforunately, there is usually a gap between
! the source and nearest receiver. In marine seismic, especially, that gap
! can be large.
!
! Module CNEARTS creates the missing traces by estimating them from traces
! taken from other shots. Requiring gathered shot record input, CNEARTS collects
! shot gathers. Every trace from each shot read is assigned a cmp index. A
! missing trace from a previous shot is estimated by collecting traces from
! later shots with the same cmp index as the missing trace. The number of
! traces collected to estimate the missing trace is a user specified
! parameter. Once the necessary traces have been collected, they have
! differential moveout applied and are then combined to form the output
! composite trace. The original traces and created near offset traces are
! written to the output when all required near offset traces have been created.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Data Preparation: data should be in shot gathers.
!   Data must be gathered prior to processing by CNEARTS. In the GATHER module,
!   set parameter NUM_TR_MAX equal to or greater than the number of traces in
!   the input shot gather. CNEARTS will abort if the data is not gathered.
!
! Additional Parameter Information:
!   CNEARTS parameter MAX_NUM_TRC_GATHER should be equal to or greater than
!   NUM_TR_MAX specified in the GATHER module. If any input record trace count
!   exceeds MAX_NUM_TRC_GATHER, CNEARTS will abort.
!
!   Parameter MAX_NEAR_OFFSET should be set to a value equal to or larger than
!   the largest source-to-near-trace offset expected for the swath. The number
!   of fill traces is NOT a parameter specified by the user. That number is
!   computed for each shot that is input to CNEARTS.
!   Parameters MAX_NEAR_OFFSET and MAX_NUM_TRC_GATHER are used to determine
!   the maximum size of the output gathers.
!
!   Parameter RCV_GRP_INTRVAL should be specified from geometry information
!   in units (feet or meters) consistent with the offset units in the data
!   trace headers.
!
!   Parameter SRC_RCVR_SEP_LIMIT is specified in units of receiver group
!   interval. It controls how different in offset an actual trace (from
!   another or the same shot) can be and still be considered for use in
!   creating a missing near trace. A very large number here indicates the
!   user wants any trace to be used for creating the near traces, regardless
!   of offset from that missing trace. This parameter becomes important if
!   there are significant gaps in the shots. Large values will pull traces
!   from far down the cable to create missing near traces. Small values will
!   not, resulting in a roll out and roll in effect on the created near traces
!   at the edges of the gap. Recent changes limit the size of this parameter.
!
!   The DELTA_SOURCE parameter is also specified in units of receiver group
!   interval. It specifies how many receiver groups the shot advances between
!   adjacent shots.
!
!   Option OPT_TRC_EXTRAP="Stack Nearest by Midpoint" uses an integer
!   common midpoint index to assign existing traces for the creation of
!   the missing near traces. Set OPT_COMPUTE_CMPIDX to "YES" to compute
!   that number (starts at 1 and increases in increments of 1). If the
!   traces header already contains a cmp index, set OPT_COMPUTE_CMPIDX to "NO"
!   and indicate the header word in parameter HDR_CMP_INDEX.
!
!   Parameter HDR_CABLE_NUM_LOC is used to indicate which trace header word
!   contains the cable number. This was added to enable processing a swath
!   of marine seismic data.
!
!   Option OPT_DEBUG_PRINT=YES is normally used by the developer of this
!   module to print out large amounts of debug information. Users should
!   set it to YES with caution. Processing long lines will produce HUGE
!   report files.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process requires traces to be input in valid gathers, which are assumed
! to be shot gathers. It is further assumed that this data comes from a marine
! seismic acquisition sceme: single end spread with the shot pulling the 
! receivers down the line. The data present can be from a swath of data, but
! the data must be sorted in shot order with all traces for one source array,
! cable pair in contiguous. Right now the software detects a change in the cable
! number, NOT a change in which shot. So the sort currently can't be to one
! source array, cable pair followed by the other source array, same cable pair.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! When the first shot gather is available, the module computes how many traces
! must be inserted, based on the distance from the source to the nearest
! receiver. Shot gathers are stored in memory when they are available. When
! enough traces have been collected from later shots to estimate all of the
! missing traces in the oldest shot, its missing traces are created, and the
! shot gather for this oldest shot is then output. The size of the gather
! increases by the number of traces estimated.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! The following parameters are used by this process:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       changed
! GATHERED  whether traces are a legitimate gather  used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! The headers for original input traces are not changed except for HDR_SEQUENCE,
! which is renumbered because of the trace insertions. The created traces start
! with a copy of the header for the trace nearest the shot. Important header
! entries generated for the created traces are listed below.
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!    1    HDR_SEQUENCE               Renumbered in all traces.
!    2    HDR_TOP_MUTE               Minimum top mute saved for created traces.
!    3    HDR_CURRENT_GROUP          Checked for gather number.
!    ?    HDR_CMP_INDEX              Set in created traces for header word
!                                    specified by parameter HDR_CMP_INDEX
!                                    if cmp index is not computed. Default
!                                    value is Hwd# 7 (HDR_MIDPOINT_XGRID).
!    ?    HDR_CABLE_NUM_LOC          Used by parameter HDR_CABLE_NUM_LOC as
!                                    the header word containing the cable
!                                    number associated with the trace.
!                                    Default is Hwd# 27 (HDR_RECEIVER_LINE).
!    ?    HDR_SHOT_INDEX             Used by parameter HDR_SHOT_INDEX as
!                                    the header word containing the shot
!                                    index for each shot on the line.
!                                    Default is Hwd# 3 (HDR_CURRENT_GROUP).
!    4    HDR_CURRENT_CHANNEL        Renumbered in all traces.
!    6    HDR_OFFSET                 Set to zero in created traces.
!    7    HDR_MIDPOINT_XGRID         Set to zero in created traces.
!    8    HDR_MIDPOINT_YGRID         Set to zero in created traces.
!   10    HDR_ORIGINAL_CHANNEL       Set in created traces
!   11    HDR_SOURCE_XLOC            Used to compute source separation.
!   12    HDR_SOURCE_YLOC            Used to compute source separation.
!   14    HDR_RECEIVER_XLOC          In created traces, set to value of
!                                    nearest actual trace.
!   15    HDR_RECEIVER_YLOC          In created traces, set to value of
!                                    nearest actual trace.
!   17    HDR_MIDPOINT_XLOC          Set to zero in created traces.
!   18    HDR_MIDPOINT_YLOC          Set to zero in created traces.
!   19    HDR_MIDPOINT_ELEV          Set to zero in created traces.
!   25    LAV                        Calculated for created traces
!   30    HDR_SCRATCH_30             Temp storage for cmp index, if computed.
!   31    HDR_SCRATCH_31             Computed shot index stored here.
!   32    HDR_SCRATCH_32             Temp storage for offset.
!   35    HDR_RECEIVER_XGRID         In created traces, set to value of
!                                    nearest actual trace.
!   36    HDR_RECEIVER_YGRID         In created traces, set to value of
!                                    nearest actual trace.
!   37    HDR_MIDPOINT_SHOTPOINT     Set to zero in created traces.
!   48    HDR_USER_48                Flag indicating this is created trace.
!   50    HDR_USER_50                Contains cmp index, ideal computation.
!   51    HDR_USER_51                Contains cmp index, realistic computation,
!                                    value not rounded to integer.
!   52    HDR_USER_52                Contains cmp index, realistic computation
!                                    value rounded to integer.
!   64    HDR_BOTTOM_MUTE            Max bottom mute saved for created traces.
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 20. 2007-01-03  Stoeckley  Moved a return statement to a later location in
!                             the update routine so that the pc_put calls will
!                             always be made for all parameters, to allow the
!                             SeisSpace gui to be built.
!019. 2006-06-22  D. Glover  Added NULLIFY statements for Intel compiler.
!018. 2006-01-10  B. Menger  Removed Unused Variables.
! 17. 2005-01-31  Bill Done  Add code to initialize composite arrays to zero
!                            when reconstructing after a change in the number
!                            of fill traces. Add code in subroutine
!                            cnearts_composite_diffnmo_stack to zero the
!                            output composite array if there are no traces
!                            available to composite.
! 16. 2004-05-11  Bill Done  Changes in module nmoprim_module call sequences
!                            for its subroutines nmoprim_create and
!                            nmoprim_apply necessitated changes in call
!                            sequences of several cnearts subroutines.
! 15. 2004-05-04  Bill Done  Eliminate member src_to_near_rcvr from
!                            cnearts_struct. It was duplicating what element
!                            near_offset_this_shot does. Parameter
!                            near_offset_this_shot was not being initialized in
!                            cnearts_do_first_shot_setup. Eliminate code that
!                            discards a shot that is too close to the previous
!                            shot. That is now being handled by reg2d. In
!                            subroutine cnearts_composite_oldest_shot(), set
!                            ntr to number of traces in shot to be composited.
! 14. 2004-03-30  Bill Done  In midpoint stacking option, print info about
!                            number of fill traces and dead fill traces when
!                            shot is ready to output.
! 13. 2004-03-22  Bill Done  Major change to allow variation in number of
!                            fill traces shot-to-shot on the same cable.
!                            Parameter MAX_NUM_FILL_TRC is now MAX_NEAR_OFFSET,
!                            from which the maximum number of fill traces on
!                            a swath is determined. Two new gui elements have
!                            been added: HDR_SHOT_INDEX which specifies the
!                            trace header word to be used for the shot index
!                            and DELTA_SHOT_INDEX to specify the change in
!                            the shot index value between adjacent shots.
!                            CNEARTS now discards shots that are less than
!                            half the predicted distance from the previous
!                            shot. 
! 12. 2004-03-16  Bill Done  Insert code to detect and abort if any one trace
!                            ensembles are encountered. Limit maximum value of
!                            SRC_RCVR_SEP_LIMIT to 50. Add code to force a
!                            minimum value of 2 for parameter NUM_TRC_TO_STACK
!                            for extrapolation options Stack Nearest by
!                            Midpoint and Stack Nearest Same Shot. Rename
!                            some members of supporting structs. Compute cmp
!                            index taking change in source location and actual
!                            near trace offset into account.
! 11. 2004-02-11  Bill Done  Fix an error in the Stack Nearest Same Shot option
!                            introduced when the swath processing capability
!                            was added.
! 10. 2004-01-22  Bill Done  Fix shot tracker error mechanism. Adjust
!                            computation of number of trackers. Add internal
!                            parameter to control action if shot tracker error
!                            occurs. Rename some trace header and sample
!                            arrays. Initialize post moveout debugging arrays
!                            to zero.
!  9. 2004-01-21  Bill Done  Add capability to process a swath of marine data,
!                            not just a single source-cable pair. This meant
!                            introducing a new parameter HDR_CABLE_NUM_LOC,
!                            which indicates which trace header word indicates
!                            the cable number associated with the data.
!  8. 2003-12-16  Bill Done  Rename parameter OPT_DEBUG to OPT_DEBUG_PRINT.
!                            Introduce parameter OPT_DEBUG_OUTPUT to enable
!                            the output of diagnostic traces, pre- and
!                            post-moveout. Eliminate use of memman primitive.
!  7. 2003-12-12  Bill Done  Fix computation of max_trckr_index, the number
!                            slots in the shot tracker. Add documentation for
!                            "ADVICE FOR USERS" section. When working with
!                            offset values from headers, add code to take the
!                            absolute value of the input offset, then restore
!                            the proper sign for the extrapolated traces later.
!  6. 2003-12-01  Bill Done  Change extrapolation option "Copy Nearest Trace"
!                            to "Stack Nearest From Same Shot" and implement
!                            that option. In option "Stack Nearest by Midpoint"
!                            add subroutine to composite headers and use it
!                            to set top and bottom mute.
!  5. 2003-11-21  Bill Done  Modify how HDR_CURRENT_CHANNEL and
!                            HDR_ORIGINAL_CHANNEL are renumbered. Also,
!                            set or initialize to zero the various
!                            HDR_MIDPOINT_? header values. Change default
!                            value of OPT_DEBUG to 'NO'. Fix parameter
!                            sensitization on gui.
!  4. 2003-11-07  Bill Done  Fix parameter verification problem where
!                            verification of OPT_TRC_EXTRAP interferes
!                            with NUM_TRC_TO_STACK. Fix cmp index value
!                            in trace headers for insert dead trace option.
!  3. 2003-11-05  Bill Done  Implement "Copy Nearest Trace" option.
!                            This option creates the missing near offset
!                            traces by copying the nearest trace from the
!                            current shot, applying partial nmo.
!  2. 2003-10-27  Bill Done  Incorporate changes required by changes in
!                            nmoprim. Write code for handling full shot
!                            tracker when processing a new shot. Introduce
!                            del_cmp_index to track the change between
!                            adjadent cmp's if using getting the cmp index
!                            from a header word.
!  1. 2003-10-23  Bill Done  Initial version.
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
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST    true     whether this process ever needs to request traces.
! NEED_LABEL      true     whether this process needs a label.
! TWOSETS         true     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE    ?       whether this process can be in a parallelized loop.
!
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
! Currently, the algorithm used to build the missing trace, also called the
! composite trace, simply applies differential moveout to the traces from
! other shots sharing the same cmp index as the target missing trace. After
! differential moveout has been applied, these "compositing" traces are then
! summed. Dividing this summed trace by the number of compositing traces gives
! the composite trace.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! It is assumed that the input to this module is gathers of shots. The module
! does verify whether the input is gathered. Input shot gathers are saved in
! memory. When a new shot gather is available, subroutine cnearts() is called
! with ntr>0. The traces in that new shot are assigned a common midpoint (cmp)
! index. The midpoint between the source and farthest offset trace for the
! first shot on a line is cmp index 1.
!
! The new shot is then assigned to a slot in the shot_trckr(:) array. If
! successfully assigned a slot, the headers and trace samples for that shot
! are copied to the saveShotPtr member of the shot_tracker_struct.
!
! The number of missing traces to be estimated for that shot is then
! computed for the new shot. This number (element num_fill_trkr in the
! shot struct) determines how many composite trace structs are created for
! that shot.
!
! Missing traces for the new shot are represented by the cmposTrcPtr member
! of the shot_tracker_struct. When a shot is assigned to the shot_trckr(:)
! array, its cmposTrcPtr member is initialized. The cmposTrcPtr contains
! the scalar num_total_trc which defines how much memory has been allocated
! in the header and sample arrays (trc_hdr(:,:) and trc_smp(:,:), respectively)
! to hold the traces needed to estimate a missing trace and the estimate of
! the missing trace itself, num_total_trc = NUM_TRC_TO_STACK + 1. Member
! num_saved_trc counts how many compositing traces are currently held in
! the structure, cmp_index specifies what cmp index is assigned to the midpoint
! for this missing trace, and nom_offset gives the nominal source-to-receiver
! offset for that missing trace.

!
! The traces for that new shot are then scanned, comparing the cmp index for
! a trace with those of previous shots still in memory. The new trace's cmp
! index is compared against the cmp_index member of the cmposTrcPtr member of
! saved shots. When a match is found, the new input trace is copied into the
! appropriate cmposTrcPtr. When that cmposTrcPtr is full (num_saved_trc =
! num_total_trc - 1) for all of the missing traces for the oldest shot in
! memory, those missing traces are composited. Once composited, that oldest
! shot and the estimates of its missing near offset traces are then copied
! to the output header and sample arrays. The position in the shot_trckr(:)
! array held by that oldest shot is then reset, making it available to hold
! another shot.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS cnearts Process/NC=80>
!
!CNEARTS - Create NEAR offset TraceS
!
! `----------------------------------------------------------------------------
! | Data Properties
! |
! | OPT_TRC_EXTRAP~~ = `CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! | 
! | NUM_TRC_TO_STACK~~~= `III
! | RCV_GRP_INTRVAL~~~~= `FFFFFFFFF
! |
! | DELTA_SOURCE~~~~~~~= `FFFF     SRC_RCVR_SEP_LIMIT~= `IIII
! |
! | MAX_NUM_TRC_GATHER = `IIII       MAX_NEAR_OFFSET~~= `FFFF
! |
! | OPT_COMPUTE_CMPIDX = `CCCC       HDR_CMP_INDEX~~~~= `III
! |
! | HDR_CABLE_NUM_LOC = `III  HDR_SHOT_INDEX = `III  DELTA_SHOT_INDEX = `III
! |
! | Select PATH_VEL [PATH_VEL] = `QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! |                   [PATH_VEL_INFO] =`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! |
! | OPT_DEBUG_PRINT = `CCCC          OPT_DEBUG_OUTPUT = `CCCC
! |
! `----------------------------------------------------------------------------
!
!<PARMS PATH_VEL[/ML=128/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_TRC_EXTRAP">
!<Tip> Trace extrapolation algorithm.</Tip>
! Default = "Stack Nearest by Midpoint"
! Allowed = "Stack Nearest by Midpoint"
!           "Stack Nearest From Same Shot"
!           "Insert Dead Traces"
! Specifies the method used to compute the extrapolated near offset traces.
! If OPT_TRC_EXTRAP = "Stack Nearest by Midpoint", then NUM_TRC_TO_STACK
! traces having the same midpoint as the trace being extrapolated (obtained
! from nearby shots) are combined to form the extrapolated trace.
! If OPT_TRC_EXTRAP = "Stack Nearest From Same Shot", then the closest
! single trace from the same shot as the trace being created is copied.
! If OPT_TRC_EXTRAP = "Insert Dead Traces", then a single trace having
! all zero data samples is generated for the extrapolated trace.
!</Help>
!
!<Help KEYWORD="NUM_TRC_TO_STACK">
!<Tip> Number of traces stacked to generate each missing near trace.</Tip>
! Default = 3 if OPT_TRC_EXTRAP = "Stack Nearest by Midpoint" or
! "Stack Nearest From Same Shot".
! Allowed = any "small" positive integer greater than 1, up to and including
! 20. Specifies the number of traces stacked to generate each missing near
! offset trace. Only used if OPT_TRC_EXTRAP = "Stack Nearest by Midpoint" or
! "Stack Nearest From Same Shot".
!</Help>
!
!<Help KEYWORD="MAX_NUM_TRC_GATHER">
!<Tip> Maximum number of traces in an INPUT gather, excluding fill traces.</Tip>
! Default = None
! Allowed = any positive integer.
! The input to this module must be gathered. This parameter specifies
! the maximum number of traces that will be in any INPUT gather. This count
! does NOT have to include the missing traces that will be added to each
! shot. Module aborts if this number is exceeded by any input gather.
!</Help>
!
!<Help KEYWORD="MAX_NEAR_OFFSET">
!<Tip> Maximum source-to-near-trace offset for swath.</Tip>
! Default = None
! Allowed = any positive integer.
! Specifies the maximum source-to-near-trace offset for the entire swath
! being processed. It should take into account possible variations in the
! offset introduced by acquisition geometry changes.
!
! This value and MAX_NUM_TRC_GATHER are used to determine maximum output
! record size.
!</Help>
!
!<Help KEYWORD="OPT_COMPUTE_CMPIDX">
!<Tip> Option to compute cmp index.</Tip>
! Default = YES
! Allowed = YES/NO
! If OPT_COMPUTE_CMPIDX = YES, this module computes the integer cmp index
! using a starting value of 1 for the line and information from trace headers
! on the location of the source and receiver for each trace.
! If OPT_COMPUTE_CMPIDX = NO, this module uses the header word specified by
! parameter HDR_CMP_INDEX, which should be an integer incrementing or
! decrementing by one between adjacent cmp's.
!</Help>
!
!<Help KEYWORD="HDR_CMP_INDEX">
!<Tip> Specifies header word containing integer cmp index for trace.</Tip>
! Default = 7
! Allowed = integer specifying any valid trace header word.
! If OPT_COMPUTE_CMPIDX = NO, the header word specified by HDR_CMP_INDEX
! will be used as the cmp index. It is assumed to be an integer that
! increments or decrements by 1 between adjacent cmp's.
! Not used if OPT_COMPUTE_CMPIDX = YES.
!</Help>
!
!<Help KEYWORD="HDR_CABLE_NUM_LOC">
!<Tip> Specifies header word containing cable number.</Tip>
! Default = 27
! Allowed = integer specifying any valid trace header word.
! When CNEARTS was modified to process a swath, it requires knowledge of
! the cable number associated with each trace. This parameter specifies
! the location in the trace header containing the cable number. This parameter
! is only important when using the "Stack Nearest by Midpoint" extrapolation
! option. For the other options it is insensitive.
!</Help>
!
!<Help KEYWORD="HDR_SHOT_INDEX">
!<Tip> Specifies header word containing the shot index.</Tip>
! Default = 3
! Allowed = integer specifying any valid trace header word.
! CNEARTS computes a new shot index, taking into account initial shot number
! on the line, the change or delta between adjacent shot numbers, and the
! cumulative distance along the line for the shot. This parameter indicates
! which header word should be used to get the input data's shot index.
!</Help>
!
!<Help KEYWORD="DELTA_SHOT_INDEX">
!<Tip> Specifies change in shot index value between adjacent shots.</Tip>
! Default = 2
! Allowed = integer
! This number represents the change in the shot index value between two
! adjacent shots and assumes that this represents the case where there
! are not skipped or missing shots.
!</Help>
!
!<Help KEYWORD="DELTA_SOURCE">
!<Tip> Separation between consecutive src locations, in group intervals.</Tip>
! Default = 1
! Allowed = any positive integer.
! Specifies the separation between consecutive shots, measured in receiver
! group intervals. This parameter affects the memory required to hold previous
! shots in memory. The smaller DELTA_SOURCE is, the larger the memory
! requirement. Since DELTA_SOURCE represents a characteristic of the 
! acquisition, the user should not set it larger than the physical value in
! an attempt to save memory. That will result in run time problems. If the
! user is unsure of the physical separation between consecutive sources, using
! a value of 1 possibly requires more memory than necessary, but is safe.
!</Help>
!
!<Help KEYWORD="SRC_RCVR_SEP_LIMIT">
!<Tip> Maximum difference in offset when selecting traces to composite.</Tip>
! Default = 50
! Allowed = any positive integer less than or equal to 50.
! Specifies the maximum offset difference, in receiver group intervals,
! that will be allowed when selecting a trace for compositing a missing
! trace. For example, if RCV_GRP_INTRVAL = 25 and SRC_RCVR_SEP_LIMIT = 20,
! traces to be composited for a missing trace with a nominal offset of
! 150 meters from the source, must have a source-to-receiver offset of
! 150 + 20*25 = 650 meters or less.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> Select stacking velocity file. </Tip>
!Shows file selection dialog to select file containing stacking velocities.
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> Velocity path name. </Tip>
! Default = None
! Allowed = Any valid stacking velocity file name
! Full path to a file containing stacking velocity functions for the data.
!</Help>
!
!<Help KEYWORD="PATH_VEL_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_VEL. </Tip>
!</Help>
!
!<Help KEYWORD="RCV_GRP_INTRVAL">
!<Tip> Inline separation between centers of two adjacent receiver groups.</Tip>
! Default = None
! Allowed = Positive value
! For two adjacent receiver groups on a cable, this parameter specifies
! the separation between the centers of those groups. It is assumed to
! be the same for all adjacent receiver pairs on all cables.
!</Help>
!
!<Help KEYWORD="OPT_DEBUG_PRINT">
!<Tip> Control printing of debug information.</Tip>
! Default = NO
! Allowed = YES/NO
! If yes, print debug information.
!</Help>
!
!<Help KEYWORD="OPT_DEBUG_OUTPUT">
!<Tip> Control output of traces for debug information.</Tip>
! Default = NO
! Allowed = YES/NO
! If yes, output the debug traces, meaning that instead of outputting the
! extrapolated traces in each shot, the NUM_TRC_TO_STACK traces used to
! compute each of the missing traces to be extrapolated are written out.
! This is done for these traces before and after the application of
! partial moveout.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


  module cnearts_module
    use pc_module
    use named_constants_module
    use string_module
    use nmoprim_module
    use lav_module             ! for updating lav after creating traces
    use grid_module            ! if you need the grid transformation.
    use pathchoose_module      ! if you use file name parameters.
    use pathcheck_module       ! if you use file name parameters.

    implicit none
    private
    public :: cnearts_create
    public :: cnearts_delete
    public :: cnearts_initialize
    public :: cnearts_nullify
    public :: cnearts_update
    public :: cnearts            ! main trace processing routine.
    public :: cnearts_wrapup

!   trap subroutines
    private :: cnearts_gathered_trap
    private :: cnearts_opt_trc_extrap_trap
    private :: cnearts_num_trc_to_stack_trap
    private :: cnearts_max_num_trc_gather_trap
    private :: cnearts_max_num_fill_trc_trap
    private :: cnearts_max_near_offset_trap
    private :: cnearts_delta_source_trap
    private :: cnearts_src_rcvr_sep_limit_trap
    private :: cnearts_path_vel_trap
    private :: cnearts_rcv_grp_intrval_trap
    private :: cnearts_opt_debug_print_trap
    private :: cnearts_opt_debug_output_trap
    private :: cnearts_opt_compute_cmpidx_trap
    private :: cnearts_hdr_cmp_index_trap
    private :: cnearts_end

!   private subroutines
    private :: cnearts_opt_stack_near_trc_same
    private :: cnearts_opt_insert_dead_trc
    private :: cnearts_opt_stack_midpt_trc
    private :: cnearts_init_tracker
    private :: cnearts_reset_tracker
    private :: cnearts_save_shot
    private :: cnearts_save_transition_shot
    private :: cnearts_load_compositing_trcs
    private :: cnearts_map_cmp_to_trc_idx
    private :: cnearts_composite_oldest_shot
    private :: cnearts_test_compositing
    private :: cnearts_composite_diffnmo_stack
    private :: cnearts_composite_trace_headers
    private :: cnearts_do_diff_nmo
    private :: cnearts_output_ready_shot
    private :: cnearts_flush_shots_in_memory
    private :: cnearts_copy_shot_to_out_array
    private :: cnearts_output_auxtrc_tr1isnear
    private :: cnearts_output_auxtrc_tr1isfar
    private :: cnearts_set_header_values
    private :: cnearts_shot_tracker_cnstrct
    private :: cnearts_comp_trc_array_cnstrct
    private :: cnearts_comp_trc_cnstrct
    private :: cnearts_postmo_trc_cnstrct
    private :: cnearts_save_trc_cnstrct
    private :: cnearts_retain_trc_cnstrct
    private :: cnearts_shot_tracker_destrct
    private :: cnearts_comp_trc_array_destrct
    private :: cnearts_comp_trc_destrct
    private :: cnearts_postmo_trc_destrct
    private :: cnearts_save_trc_destrct
    private :: cnearts_retain_trc_destrct
    private :: cnearts_assign_default_value
    private :: cnearts_print_trace_info
    private :: cnearts_compute_cmp_index
    private :: cnearts_print_cmp_index_info
    private :: cnearts_init_default_struct
    private :: cnearts_do_first_shot_setup

!   private functions
    private :: cnearts_point2point_dist
    private :: cnearts_get_near_offset
    private :: cnearts_is_trace_1_near
    private :: cnearts_get_srcrcvr_dist
    private :: cnearts_compute_numfill
    private :: cnearts_shot_grp_advance
    private :: cnearts_get_new_tracker_index
    private :: cnearts_get_new_tracker_index0
    private :: cnearts_get_prev_shot_idx
    private :: cnearts_get_next_shot_idx
    private :: cnearts_assign_comp_trc
    private :: cnearts_insert_comp_trc
    private :: cnearts_get_num_shots_tracked
    private :: cnearts_check_cmp_increment
    private :: cnearts_has_cable_changed
    private :: cnearts_data_sanity_check

    character(len=100),public,save :: CNEARTS_IDENT = &
'$Id: cnearts.f90,v 1.20 2007/01/03 14:01:36 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    ! retained trace struct to hold first shot of next cable until shots
    ! of current cable can be flushed to output
    type,private :: retain_trc_struct
      ! number of retained traces
      integer                   :: ret_saved_trc     
      ! retained trace headers
      double precision, pointer :: ret_trc_hdr(:,:)  
      ! retained trace samples
      real,             pointer :: ret_trc_smp(:,:)  
    end type retain_trc_struct

    ! composite trace struct holds num_trc_to_stack traces that are to
    ! be composited and the composite trace they form
    type,private :: comp_trc_struct
      ! total num traces in composite trace struct
      integer                          :: comp_num_total_trc    
      ! total saved traces currently in composite trace struct
      integer                          :: comp_num_saved_trc    
      ! cmp index for this composite trace 
      integer                          :: comp_cmp_index    
      ! offset for this composite trace
      real                             :: comp_nom_offset   
      ! header array for compositing traces and composite
      double precision,        pointer :: comp_trc_hdr(:,:) 
      ! sample array for compositing traces and composite
      real,                    pointer :: comp_trc_smp(:,:) 
      ! struct to hold post moveout uncomposited traces, used in debug mode
      type(retain_trc_struct), pointer :: postmoTrcPtr  
    end type comp_trc_struct

    ! save shot struct holds the headers and samples of an input shot while
    ! traces for use in extrapolating missing near traces for it are
    ! gathered from later shots
    type,private :: save_shot_struct
      ! saved shot header array
      double precision, pointer :: save_trc_hdr(:,:)  
      ! saved shot sample array
      real,             pointer :: save_trc_smp(:,:)  
    end type save_shot_struct

    ! shot tracker struct holds shots in memory as traces for extrapolating
    ! the missing near traces for each are gathered. the shot tracker can
    ! hold max_trckr_index shots. when ready, the oldest shot in the tracker
    ! is prepared for output and written. its slot is then reinitialized
    ! and used to hold the next available shot.
    type,private :: shot_tracker_struct
      ! true if tracker in use
      logical                        :: in_use          
      ! true if ready to comp.
      logical                        :: ready_to_output 
      ! num fill trc this shot
      integer                        :: num_fill_trkr   
      ! num in trc this shot
      integer                        :: num_trc_trkr    
      ! map cmp idx to trace
      integer                        :: mapCmp2Trc      
      ! array of composite trace structs, one for each trace to be inserted
      ! in the near offset gap
      type(comp_trc_struct), pointer :: cmposTrcPtr(:)  
      ! save shot struct to hold the shots in memory while traces are
      ! gathered from later shots for compositing
      type(save_shot_struct), pointer:: saveShotPtr     
    end type shot_tracker_struct

    type,public :: cnearts_struct

      private
      logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

      integer                    :: ipn      ! process number.
      logical                    :: gathered ! whether properly gathered.
      integer                    :: nwih     ! number of header words.
      integer                    :: ndpt     ! number of trace samples.
      real                       :: tstrt    ! time of 1st trace sample (sec).
      real                       :: dt       ! trace sample interval (sec).
      type(grid_struct)          :: grid     ! grid transform.

!     elements on the gui
      integer                        :: num_trc_to_stack
      integer                        :: max_num_trc_gather
      real                           :: max_near_offset
      real                           :: delta_source
      integer                        :: src_rcvr_sep_limit
      integer                        :: hdr_cmp_index
      integer                        :: hdr_cable_num_loc
      integer                        :: hdr_shot_index
      integer                        :: delta_shot_index
      real                           :: rcv_grp_intrval
      logical                        :: opt_compute_cmpidx
      logical                        :: opt_debug_print
      logical                        :: opt_debug_output
      character(len=28)              :: opt_trc_extrap
      character(len=filename_length) :: path_vel

!     other elements
      integer                        :: gather_hdr_word_num
      integer                        :: max_num_fill_trc
      integer                        :: max_num_trc_to_stack
      integer                        :: min_num_trc_to_stack
      integer                        :: num_fill_1st
      integer                        :: save_seq_count
      integer                        :: del_orig_chan
      integer                        :: del_curr_chan
      integer                        :: lastCmpStartIndex
      double precision               :: del_cmp_index
      real                           :: near_offset_this_shot
      double precision               :: cumulative_delta_src
      double precision               :: shot_num_1st_online
      logical                        :: trc_1_is_near
      logical                        :: firstShotThisCable
      logical                        :: secondShotThisCable

!     shot tracker related elements
      type(shot_tracker_struct), pointer :: shot_trckr(:)
      integer                            :: max_trckr_index
      integer                            :: index_to_oldest_shot
      integer                            :: index_to_newest_shot
      logical                            :: abort_on_trckr_error

!     swath processing related elements
      logical                            :: haveTransitionShot
      type(retain_trc_struct), pointer   :: transitionShotPtr
      

!     velocity related parameters
      type(pathchoose_struct),pointer    :: path_vel_dialog
      type(nmoprim_struct),   pointer    :: nmoprim
      character(len=7)                   :: opt_nmo
      character(len=6)                   :: opt_interp
      character(len=4)                   :: opt_nmo_res
      real                               :: doppler
      integer                            :: order_mo
      integer                            :: nhx
      integer                            :: nhy
      real                               :: vel_bias
      real                               :: vel_scale

    end type cnearts_struct

!   declarations for parameter defaults for default_obj
    integer,           parameter       :: dflt_num_trc_to_stack     = 3
    integer,           parameter       :: dflt_max_num_trc_to_stack = 20
    integer,           parameter       :: dflt_min_num_trc_to_stack = 2
    integer,           parameter       :: dflt_max_num_trc_gather   = 500
    integer,           parameter       :: dflt_max_near_offset      = 0
    real,              parameter       :: dflt_delta_source         = 3.0
    integer,           parameter       :: dflt_src_rcvr_sep_limit   = 50
    integer,           parameter       :: dflt_gather_hdr_word_num  = 3
    integer,           parameter       :: dflt_hdr_cable_num_loc    = 27
    integer,           parameter       :: dflt_hdr_shot_index       = 3
    integer,           parameter       :: dflt_delta_shot_index     = 2
    integer,           parameter       :: dflt_hdr_cmp_index        = &
                                              HDR_MIDPOINT_XGRID
    integer,           parameter       :: dflt_lastCmpStartIndex    = 1
    integer,           parameter       :: dflt_max_trckr_index      = 50
    integer,           parameter       :: dflt_index_to_oldest_shot = 0
    integer,           parameter       :: dflt_index_to_newest_shot = 0
    logical,           parameter       :: dflt_abort_on_trckr_error = .true.
    real,              parameter       :: dflt_rcv_grp_intrval      = 0.0
    logical,           parameter       :: dflt_opt_debug_print      = .false.
    logical,           parameter       :: dflt_opt_debug_output     = .false.
    logical,           parameter       :: dflt_opt_compute_cmpidx   = .true.
    real,              parameter       :: dflt_vel_bias             = 0.0
    real,              parameter       :: dflt_vel_scale            = 1.0
    real,              parameter       :: dflt_doppler              = 1.7
    integer,           parameter       :: dflt_order_mo             = NMOPRIM_2
    character(len=7),  parameter       :: dflt_opt_nmo              = 'PARTIAL'
    character(len=6),  parameter       :: dflt_opt_interp           = 'LINEAR'
    character(len=4),  parameter       :: dflt_opt_nmo_res          = 'FFT4'
    character(len=25), parameter       :: dflt_opt_trc_extrap = &
                                              'Stack Nearest by Midpoint'

!   other parameter declarations
    integer,           parameter       :: flag_cnearts_trace        = -100
    integer,           parameter       :: ASSIGN_STAT_ASSIGNED      = 0
    integer,           parameter       :: ASSIGN_STAT_FLUSH         = 1
    integer,           parameter       :: ASSIGN_STAT_ERROR         = 2

!   declare and initialize type to contain default eagc parameter values
    type, private :: cnearts_default_struct
      logical           :: opt_debug_print
      logical           :: opt_debug_output
      logical           :: opt_compute_cmpidx
      integer           :: num_trc_to_stack
      integer           :: max_num_trc_to_stack
      integer           :: min_num_trc_to_stack
      integer           :: max_num_trc_gather
      real              :: max_near_offset
      real              :: delta_source
      integer           :: src_rcvr_sep_limit
      integer           :: gather_hdr_word_num
      integer           :: hdr_cable_num_loc
      integer           :: hdr_shot_index
      integer           :: delta_shot_index
      integer           :: hdr_cmp_index
      integer           :: lastCmpStartIndex
      real              :: rcv_grp_intrval
      character(len=25) :: opt_trc_extrap

      integer           :: max_trckr_index
      integer           :: index_to_oldest_shot
      integer           :: index_to_newest_shot
      logical           :: abort_on_trckr_error

      character(len=7)  :: opt_nmo
      character(len=6)  :: opt_interp
      character(len=4)  :: opt_nmo_res
      real              :: vel_bias
      real              :: vel_scale
      real              :: doppler
      integer           :: order_mo

    end type cnearts_default_struct




!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


    interface assignment(=)
      module procedure cnearts_assign_default_value
    end interface


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(cnearts_struct),pointer,save :: object    ! needed for traps.

!   declaration of parameter defaults struct
    type(cnearts_default_struct), save :: default_obj

!   declarations for list contents
    integer,    parameter :: n_opt_trc_extrap = 3
    character(len=28),save :: c_opt_trc_extrap(n_opt_trc_extrap) &
                = (/ 'Stack Nearest by Midpoint   ', &
                     'Stack Nearest From Same Shot', &
                     'Insert Dead Traces          ' /)

    contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


    subroutine cnearts_create (obj)
      type(cnearts_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in cnearts_create")

      nullify (obj%path_vel_dialog)
      nullify (obj%nmoprim)
      nullify (obj%shot_trckr) ! jpa
      nullify (obj%transitionShotPtr) ! jpa

      call pathchoose_create (obj%path_vel_dialog, 'PATH_VEL', 'vel')

!     initialize the default struct
      call cnearts_init_default_struct(default_obj)

      call cnearts_initialize (obj)

    end subroutine cnearts_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


    subroutine cnearts_delete (obj)
      type(cnearts_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call cnearts_wrapup (obj)

      if (associated(obj%nmoprim)) call nmoprim_delete (obj%nmoprim)

      if ( associated (obj%path_vel_dialog))  &
        call pathchoose_delete (obj%path_vel_dialog)

      call cnearts_retain_trc_destrct(obj%transitionShotPtr)

      call cnearts_shot_tracker_destrct(obj%shot_trckr, obj%max_trckr_index)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in               &
       &cnearts_delete")

    end subroutine cnearts_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


    subroutine cnearts_initialize (obj)
      type(cnearts_struct),intent(inout) :: obj       ! arguments

!     initialize module parameters to defaults
      obj = default_obj

      call cnearts_update (obj)
    end subroutine cnearts_initialize


    subroutine cnearts_nullify (obj)
      type(cnearts_struct), pointer :: obj       ! arguments

      nullify ( obj%path_vel_dialog )
      nullify ( obj%shot_trckr )
      nullify ( obj%transitionShotPtr )

      obj%path_vel       = pathcheck_empty ! stacking velocity file

    end subroutine cnearts_nullify


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


    subroutine cnearts_update (obj)
      type(cnearts_struct),intent(inout),target :: obj             ! arguments

!     declare local variables

      integer         :: lcl_numtr
      integer         :: action,terpmode
      integer         :: sampmode
      logical         :: tracemute, error
      character(len=200) :: dbmsg

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update (obj%path_vel_dialog, obj%path_vel)) return

      obj%ipn = pc_get_ipn()


      call pc_get_global ('numtr'   , lcl_numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

! The following code has been moved from this location to a location after
! the pc_put calls because the CPS wrappers for SeisSpace require that the
! pc_put calls are always made for all parameters.  This is because SeisSpace
! uses the parameters in the parameter cache to learn how to build the gui.
! Without the parameters in the parameter cache the first time this module's
! update routine is called, the SeisSpace gui will contain no parameters.

  !   ! check that input data is gathered
  !   if (.not. obj%gathered) then
  !     call cnearts_gathered_trap
  !     return
  !   end if

      call pc_get('OPT_TRC_EXTRAP', obj%opt_trc_extrap   ,             &
        cnearts_opt_trc_extrap_trap)
      call pc_get('NUM_TRC_TO_STACK', obj%num_trc_to_stack,                &
        cnearts_num_trc_to_stack_trap)
      call pc_get('MAX_NUM_TRC_GATHER', obj%max_num_trc_gather,            &
        cnearts_max_num_trc_gather_trap)
      call pc_get('MAX_NEAR_OFFSET', obj%max_near_offset,                &
        cnearts_max_near_offset_trap)
      call pc_get('DELTA_SOURCE', obj%delta_source,            &
        cnearts_delta_source_trap)
      call pc_get('SRC_RCVR_SEP_LIMIT', obj%src_rcvr_sep_limit,            &
        cnearts_src_rcvr_sep_limit_trap)
      call pc_get('RCV_GRP_INTRVAL', obj%rcv_grp_intrval   ,             &
        cnearts_rcv_grp_intrval_trap)
      call pc_get('PATH_VEL', obj%path_vel   ,             &
        cnearts_path_vel_trap)
      call pc_get('OPT_DEBUG_PRINT', obj%opt_debug_print   ,             &
        cnearts_opt_debug_print_trap)
      call pc_get('OPT_DEBUG_OUTPUT', obj%opt_debug_output   ,             &
        cnearts_opt_debug_output_trap)
      call pc_get('OPT_COMPUTE_CMPIDX', obj%opt_compute_cmpidx,  &
        cnearts_opt_compute_cmpidx_trap)
      call pc_get('HDR_CMP_INDEX', obj%hdr_cmp_index,  &
        cnearts_hdr_cmp_index_trap)
      call pc_get('HDR_CABLE_NUM_LOC', obj%hdr_cable_num_loc,  &
        cnearts_hdr_cable_num_loc_trap)
      call pc_get('HDR_SHOT_INDEX', obj%hdr_shot_index,  &
        cnearts_hdr_shot_index_trap)
      call pc_get('DELTA_SHOT_INDEX', obj%delta_shot_index,  &
        cnearts_delta_shot_index_trap)

!      *** end trap ***

      call pc_call_end_trap(cnearts_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


!     verify valid path name to velocities
!     call pathcheck('PATH_VEL', obj%path_vel, 'vel', required=.true., &
!                    status=errorStatus, show=PATHCHECK_INFO_INPUT)
!     if (errorStatus /= PATHCHECK_VALID) then
!       call pc_error('valid PATHNAME is required for velocity info')
!     end if
      if (pc_get_update_state() /= PC_GUI) then    
        if (obj%path_vel == pathcheck_empty) then
          call pc_error ('path_vel is not found' )
        end if
      end if


      if (pc_get_update_state() == PC_BACKEND .or. &
          pc_get_update_state() == PC_FRONTEND) then
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!



! --> Delete any of the globals below that have not changed:

      if (obj%opt_debug_output) then
!       output debug mode: output original gather, stacked fill traces,
!                          and prestack compositing traces for each fill
!                          trace (pre and post partial nmo)
        call pc_put_global ('numtr', obj%max_num_trc_gather + &
             obj%max_num_fill_trc*(2*(obj%max_num_trc_to_stack+1) + 1))
      else
!       standard mode: output original gather and fill traces
        call pc_put_global ('numtr', &
                 obj%max_num_trc_gather + obj%max_num_fill_trc)
      end if
!TODO does gathered status need to be changed to false?
      call pc_put_global ('gathered', obj%gathered)

      call pc_put('OPT_TRC_EXTRAP', obj%opt_trc_extrap)
      call pc_put('NUM_TRC_TO_STACK', obj%num_trc_to_stack)
      call pc_put('MAX_NUM_TRC_GATHER', obj%max_num_trc_gather)
      call pc_put('MAX_NEAR_OFFSET', obj%max_near_offset)
      call pc_put('DELTA_SOURCE', obj%delta_source)
      call pc_put('SRC_RCVR_SEP_LIMIT', obj%src_rcvr_sep_limit)
      call pc_put('RCV_GRP_INTRVAL', obj%rcv_grp_intrval)
      call pc_put('PATH_VEL', obj%path_vel)
      call pc_put('OPT_DEBUG_PRINT', obj%opt_debug_print)
      call pc_put('OPT_DEBUG_OUTPUT', obj%opt_debug_output)
      call pc_put('OPT_COMPUTE_CMPIDX', obj%opt_compute_cmpidx)
      call pc_put('HDR_CMP_INDEX', obj%hdr_cmp_index)
      call pc_put('HDR_CABLE_NUM_LOC', obj%hdr_cable_num_loc)
      call pc_put('HDR_SHOT_INDEX', obj%hdr_shot_index)
      call pc_put('DELTA_SHOT_INDEX', obj%delta_shot_index)


! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('twosets'      , .true.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)

      call pc_put_options_field('OPT_TRC_EXTRAP', c_opt_trc_extrap, &
                              n_opt_trc_extrap)

! The following code has been moved to this location from the commented-out
! location above because SeisSpace requires that the pc_put calls are always
! made for all parameters.  This is because SeisSpace uses the parameters
! in the parameter cache to learn how to build the gui.  Without the
! parameters in the parameter cache the first time this module's update
! routine is called, the SeisSpace gui will contain no parameters.

      ! check that input data is gathered
      if (.not. obj%gathered) then
        call cnearts_gathered_trap
        return
      end if


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%nmoprim)) call nmoprim_delete (obj%nmoprim)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      select case (obj%opt_nmo)
        case ('FORWARD') ; action = NMOPRIM_FORWARD
        case ('REVERSE') ; action = NMOPRIM_REVERSE
        case ('STK_VEL') ; action = NMOPRIM_VNMO
        case ('DIX_VEL') ; action = NMOPRIM_VINT
        case ('PARTIAL') ; action = NMOPRIM_PARTIAL
        case default     ; action = NMOPRIM_FORWARD
      end select

      select case (obj%opt_nmo_res)
        case ('NONE') ; terpmode = NMOPRIM_LINEAR
        case ('FFT2') ; terpmode = NMOPRIM_FFT2
        case ('FFT4') ; terpmode = NMOPRIM_FFT4
        case ('FFT8') ; terpmode = NMOPRIM_FFT8
        case ('CUB4') ; terpmode = NMOPRIM_CUBIC
        case default  ; terpmode = NMOPRIM_FFT4
      end select

      select case (obj%opt_interp)
        case ('LINEAR') ; sampmode = NMOPRIM_LSAMP
        case ('SPLINE') ; sampmode = NMOPRIM_SSAMP
        case default    ; sampmode = NMOPRIM_LSAMP
      end select

      tracemute = .true.

      call nmoprim_create (obj%nmoprim, lunprint, obj%nwih, obj%ndpt,      &
                           obj%tstrt, obj%dt, action, obj%order_mo,        &
                           terpmode, obj%doppler, tracemute, obj%path_vel, &
                           "", obj%vel_bias, obj%vel_scale, sampmode,      &
                           obj%nhx, obj%nhy, error, dbmsg)

      if (error) call pc_error (dbmsg)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!     initialize in prep for trace processing


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    end subroutine cnearts_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! *** Trap for variable GATHERED ***

    subroutine cnearts_gathered_trap
      call pc_error('Input data to cnearts module must be gathered.&
                    & Use gather module prior to cnearts.')
    end subroutine cnearts_gathered_trap

! *** Trap for variable OPT_TRC_EXTRAP ***

    subroutine cnearts_opt_trc_extrap_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%opt_trc_extrap == c_opt_trc_extrap(1)) then
!       option: stack nearest by midpoint
        call pc_put_sensitive_field_flag('NUM_TRC_TO_STACK', .true.)
        call pc_put_sensitive_field_flag('OPT_COMPUTE_CMPIDX', .true.)
        call pc_put_sensitive_field_flag('HDR_CABLE_NUM_LOC', .true.)
        if (.not. object%opt_compute_cmpidx) then
          call pc_put_sensitive_field_flag('HDR_CMP_INDEX', .true.)
        end if
        call pc_put_sensitive_field_flag('OPT_DEBUG_OUTPUT', .true.)
        if (object%num_trc_to_stack < default_obj%min_num_trc_to_stack .or. &
            object%num_trc_to_stack > default_obj%max_num_trc_to_stack) then
          object%num_trc_to_stack = default_obj%num_trc_to_stack
          call pc_put('NUM_TRC_TO_STACK', object%num_trc_to_stack)
        end if
      else if (object%opt_trc_extrap == c_opt_trc_extrap(2)) then
!       option: copy nearest trace
        call pc_put_sensitive_field_flag('OPT_COMPUTE_CMPIDX', .false.)
        call pc_put_sensitive_field_flag('HDR_CMP_INDEX', .false.)
        call pc_put_sensitive_field_flag('NUM_TRC_TO_STACK', .true.)
        call pc_put_sensitive_field_flag('HDR_CABLE_NUM_LOC', .false.)
        call pc_put_sensitive_field_flag('OPT_DEBUG_OUTPUT', .false.)
        object%opt_debug_output = .false.
        call pc_put('OPT_DEBUG_OUTPUT', object%opt_debug_output)
        if (object%num_trc_to_stack < default_obj%min_num_trc_to_stack .or. &
            object%num_trc_to_stack > default_obj%max_num_trc_to_stack) then
          object%num_trc_to_stack = default_obj%num_trc_to_stack
          call pc_put('NUM_TRC_TO_STACK', object%num_trc_to_stack)
        end if
      else
!       option: insert dead traces
        call pc_put_sensitive_field_flag('OPT_COMPUTE_CMPIDX', .false.)
        call pc_put_sensitive_field_flag('HDR_CMP_INDEX', .false.)
        call pc_put_sensitive_field_flag('HDR_CABLE_NUM_LOC', .false.)
        call pc_put_sensitive_field_flag('OPT_DEBUG_OUTPUT', .false.)
        object%opt_debug_output = .false.
        call pc_put('OPT_DEBUG_OUTPUT', object%opt_debug_output)
        object%num_trc_to_stack = 0
        call pc_put('NUM_TRC_TO_STACK', object%num_trc_to_stack)
        call pc_put_sensitive_field_flag('NUM_TRC_TO_STACK', .false.)
      end if

      return
    end subroutine cnearts_opt_trc_extrap_trap

! *** Trap for variable NUM_TRC_TO_STACK ***

    subroutine cnearts_num_trc_to_stack_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      character(len=200) :: dbmsg

      if (object%opt_trc_extrap == c_opt_trc_extrap(1)) then
        if (object%num_trc_to_stack < default_obj%min_num_trc_to_stack .or. &
            object%num_trc_to_stack > default_obj%max_num_trc_to_stack) then
          write(dbmsg,*)'NUM_TRC_TO_STACK = ', object%num_trc_to_stack, &
                        ' must be > ',default_obj%min_num_trc_to_stack-1, &
                        ' and < ', dflt_max_num_trc_to_stack
          call pc_error(dbmsg)
          call pc_error('in Stack Nearest by Midpoint mode')
          object%num_trc_to_stack = default_obj%num_trc_to_stack
          call pc_put('NUM_TRC_TO_STACK', object%num_trc_to_stack)
        end if
      else if (object%opt_trc_extrap == c_opt_trc_extrap(2)) then
        if (object%num_trc_to_stack < default_obj%min_num_trc_to_stack .or. &
            object%num_trc_to_stack > default_obj%max_num_trc_to_stack) then
          write(dbmsg,*)'NUM_TRC_TO_STACK = ', object%num_trc_to_stack, &
                        ' must be > ',default_obj%min_num_trc_to_stack-1, &
                        ' and < ', dflt_max_num_trc_to_stack
          call pc_error(dbmsg)
          call pc_error('in Stack Nearest From Same Shot mode')
          object%num_trc_to_stack = default_obj%num_trc_to_stack
          call pc_put('NUM_TRC_TO_STACK', object%num_trc_to_stack)
        end if
      else if (object%opt_trc_extrap == c_opt_trc_extrap(3)) then
        if (object%num_trc_to_stack /= 0) then
          call pc_error('NUM_TRC_TO_STACK = ', object%num_trc_to_stack, &
                        ' must be 0')
          call pc_error('in Insert Dead Traces mode')
          object%num_trc_to_stack = 0
          call pc_put('NUM_TRC_TO_STACK', object%num_trc_to_stack)
        end if
      else
        call pc_error('Unknown extrapolation option')
      end if

      return
    end subroutine cnearts_num_trc_to_stack_trap

! *** Trap for variable max_num_trc_gather ***

    subroutine cnearts_max_num_trc_gather_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%max_num_trc_gather < 0) then
        call pc_error('MAX_NUM_TRC_GATHER must be > 0')
        object%max_num_trc_gather = default_obj%max_num_trc_gather
        call pc_put('MAX_NUM_TRC_GATHER', object%max_num_trc_gather)
      end if

      return
    end subroutine cnearts_max_num_trc_gather_trap

! *** Trap for variable max_near_offset ***

    subroutine cnearts_max_near_offset_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%max_near_offset < 0) then
        call pc_error('MAX_NEAR_OFFSET must be > 0')
      else
        if (object%rcv_grp_intrval > 0.0) then
          object%max_num_fill_trc = &
              ceiling(object%max_near_offset/object%rcv_grp_intrval)
        else
          object%max_num_fill_trc = 0
        end if
      end if

      return
    end subroutine cnearts_max_near_offset_trap

! *** Trap for variable max_num_fill_trc ***

    subroutine cnearts_max_num_fill_trc_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%max_num_fill_trc < 0) then
        call pc_error('MAX_NUM_FILL_TRC must be > 0')
      end if

      return
    end subroutine cnearts_max_num_fill_trc_trap

! *** Trap for variable delta_source ***

    subroutine cnearts_delta_source_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%delta_source <= 0) then
        call pc_error('DELTA_SOURCE must be > 0')
        object%delta_source = dflt_delta_source
        call pc_put('DELTA_SOURCE', object%delta_source)
      end if

      return
    end subroutine cnearts_delta_source_trap

! *** Trap for variable src_rcvr_sep_limit ***

    subroutine cnearts_src_rcvr_sep_limit_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%src_rcvr_sep_limit <= 0) then
        call pc_error('SRC_RCVR_SEP_LIMIT must be > 0')
      end if
      if (object%src_rcvr_sep_limit > dflt_src_rcvr_sep_limit) then
        call pc_error('SRC_RCVR_SEP_LIMIT must be < ',dflt_src_rcvr_sep_limit)
      end if

      return
    end subroutine cnearts_src_rcvr_sep_limit_trap

! *** Trap for variable PATH_VEL ***

    subroutine cnearts_path_vel_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
    end subroutine cnearts_path_vel_trap

! *** Trap for variable RCV_GRP_INTRVAL ***

    subroutine cnearts_rcv_grp_intrval_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%rcv_grp_intrval > 0.0) then
        object%max_num_fill_trc = &
            object%max_near_offset/object%rcv_grp_intrval
      else
        object%max_num_fill_trc = 0
      end if

      return
    end subroutine cnearts_rcv_grp_intrval_trap

! *** Trap for variable OPT_DEBUG_PRINT ***

    subroutine cnearts_opt_debug_print_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
    end subroutine cnearts_opt_debug_print_trap

! *** Trap for variable OPT_DEBUG_OUTPUT ***

    subroutine cnearts_opt_debug_output_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
    end subroutine cnearts_opt_debug_output_trap

! *** Trap for variable OPT_COMPUTE_CMPIDX ***

    subroutine cnearts_opt_compute_cmpidx_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%opt_compute_cmpidx) then
!       option to compute cmp index is true, so disable parameter HDR_CMP_INDEX
        call pc_put_sensitive_field_flag('HDR_CMP_INDEX', .false.)
      else
!       option to compute cmp index is false, so enable parameter HDR_CMP_INDEX
!       if in stack nearest by midpoint extrapolation mode
        if (object%opt_trc_extrap == c_opt_trc_extrap(1)) then
          call pc_put_sensitive_field_flag('HDR_CMP_INDEX', .true.)
        end if
      end if

      return
    end subroutine cnearts_opt_compute_cmpidx_trap

! *** Trap for variable HDR_CMP_INDEX ***

    subroutine cnearts_hdr_cmp_index_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
    end subroutine cnearts_hdr_cmp_index_trap

! *** Trap for variable HDR_CABLE_NUM_LOC ***

    subroutine cnearts_hdr_cable_num_loc_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%hdr_cable_num_loc < 1) then
        call pc_error('HDR_CABLE_NUM_LOC must be a valid header index')
        object%hdr_cable_num_loc = dflt_hdr_cable_num_loc
        call pc_put('HDR_CABLE_NUM_LOC', object%hdr_cable_num_loc)
      end if

      return

    end subroutine cnearts_hdr_cable_num_loc_trap

! *** Trap for variable HDR_SHOT_INDEX ***

    subroutine cnearts_hdr_shot_index_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%hdr_shot_index < 1) then
        call pc_error('HDR_SHOT_INDEX must be a valid header index')
        object%hdr_shot_index = dflt_hdr_shot_index
        call pc_put('HDR_SHOT_INDEX', object%hdr_shot_index)
      end if

      return

    end subroutine cnearts_hdr_shot_index_trap

! *** Trap for variable DELTA_SHOT_INDEX ***

    subroutine cnearts_delta_shot_index_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if (object%delta_shot_index == 0) then
        call pc_error('DELTA_SHOT_INDEX must not be zero')
        object%delta_shot_index = dflt_delta_shot_index
        call pc_put('DELTA_SHOT_INDEX', object%delta_shot_index)
      end if

      return

    end subroutine cnearts_delta_shot_index_trap

! *** End Trap ***

    subroutine cnearts_end
      implicit none

! --> Insert code to validate data input
      return
    end subroutine cnearts_end



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


    subroutine cnearts (obj,ntr,hdi,tri,hdo,tro)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer             , intent(inout) :: ntr                ! arguments
      double precision   ,  intent(inout) :: hdi(:,:)           ! arguments
      real               ,  intent(inout) :: tri(:,:)           ! arguments
      double precision   ,  intent(inout) :: hdo(:,:)           ! arguments
      real               ,  intent(inout) :: tro(:,:)           ! arguments

!     local variables
      logical, save :: firstEntryAbsolute = .true.
      logical       :: cableHasChanged
      integer       :: error_status
      real          :: deltaSrc = 0.0, deltaRcvr   
      double precision, save :: srcPrevX, srcPrevY
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts(): enter, actual ntr in = ',ntr)
      end if

!     check for the very, very, absolute first entry into this routine
      if (firstEntryAbsolute) then

!       in this first major if block, do some initialization. this block
!       is only entered on the first trip into this subroutine at the
!       start of processing.

!       disable further entry here
        firstEntryAbsolute = .false.

!       set flags for access to first and second entries for data
!       on this cable
        obj%firstShotThisCable = .true.
        obj%secondShotThisCable = .false.

!       initialize flag indicating a shot that is the first shot
!       on the next source-cable dataset is in memory
        obj%haveTransitionShot = .false.

!       save the sequence header item from the very first header
        obj%save_seq_count = hdi(HDR_SEQUENCE,1) - 1

!       if in stack nearest by midpoint extrapolation mode, will need
!       tmp space to hold a shot at end of a cable
        if (string_upper_compare(obj%opt_trc_extrap, &
                                 c_opt_trc_extrap(1))) then
!         now allocate space to save shot headers and samples on
!         a transition from one source-cable dataset to another
          call cnearts_retain_trc_cnstrct(obj, obj%transitionShotPtr, &
                                          error_status)
          if (error_status /= 0) then
            call pc_error('Failed to allocate memory for transition shot')
            call cnearts_wrapup(obj)
            return
          end if
        end if

      end if

!     check for too large an input gather
      if (.not. cnearts_data_sanity_check(obj, ntr, hdi)) then
        call pc_error('CNEARTS: failed data sanity check. Abort')
        ntr = FATAL_ERROR
        call cnearts_wrapup(obj)
        return
      end if

!     the following is the second major if block. it performs operations
!     that are independent of which extrapolation option is being used,
!     such as printing a summary of the first and second shots on a cable.
!     this if block depends on the input value of ntr: ntr>0 indicates a
!     new shot is available; ntr==NEED_TRACES indicates the master routine
!     is requesting traces from this module, if any; ntr==NO_MORE_TRACES
!     indicates there are no more traces for this module, so wrapup.
      if (ntr > 0) then

!       yup, there are traces available in this call

!       check for second entry for shots on this cable, to print info about
!       shot to shot spacing
        if (obj%secondShotThisCable) then

!         compute separation from previous shot and current shot
          deltaSrc = cnearts_point2point_dist( &
                         hdi(HDR_SOURCE_XLOC,1), hdi(HDR_SOURCE_YLOC,1), &
                         srcPrevX, srcPrevY)

          call pc_print('')
          call pc_print('CNEARTS Summary:')
          call pc_print('  Input Data: second record ', &
                        hdi(obj%hdr_shot_index,1))
          if (obj%opt_debug_print) then
            call cnearts_print_trace_info(hdi, 1)
            call cnearts_print_trace_info(hdi, ntr)
          end if
          call pc_print('')
          call pc_print('    Delta src location (actual): ',deltaSrc)
          call pc_print('    Cable #: ',int(hdi(obj%hdr_cable_num_loc,1)))
          call pc_print('End CNEARTS Summary:')
          call pc_print('')

!         block further entry into "first shot" if block for shots on this cable
          obj%firstShotThisCable = .false.

!         block further entry into this if block while on this cable
          obj%secondShotThisCable = .false.

        end if

!       check for first entry for shots on this cable, to get info
!       about data set
        if (obj%firstShotThisCable) then

!         do the setup for this first shot on cable
          if (obj%opt_debug_print) then
            call pc_print('cnearts(): do setup for first shot')
          end if
          call cnearts_do_first_shot_setup(obj, ntr, hdi, tri, deltaRcvr, &
                                           srcPrevX, srcPrevY)

        end if

!       check for one trace per ensemble error
        if (ntr == 1) then
          call pc_error('CNEARTS received a gather with 1 trace')
          ntr = FATAL_ERROR
          call cnearts_wrapup(obj)
          return
        end if

!       now determine if there has been a cable change
        if (obj%opt_debug_print) then
          call pc_print('cnearts(): checking for cable change')
        end if
        cableHasChanged = cnearts_has_cable_changed(obj, hdi)

        if (cableHasChanged) then

!         this shot is on a new cable
          obj%haveTransitionShot = .true.

          if (obj%opt_debug_print) then
            call pc_print('cnearts(): setting haveTransitionShot TRUE')
          end if

        else

!         still on same cable

          if (obj%opt_debug_print) then
            call pc_print('cnearts(): get src sep and do cmp indices')
          end if

!         there are new traces, compute change in source location and then
!         assign cmp indices to the new shot's traces

!         compute distance from previous source location to current location.
!         since srcPrevX and srcPrevY were set to current source location, the
!         deltaSrc for the first shot will be 0.
          deltaSrc = cnearts_point2point_dist( &
                         hdi(HDR_SOURCE_XLOC,1), hdi(HDR_SOURCE_YLOC,1), &
                         srcPrevX, srcPrevY)

!         now update the previous shot location values to current
          srcPrevX = hdi(HDR_SOURCE_XLOC,1)
          srcPrevY = hdi(HDR_SOURCE_YLOC,1)

!         compute source-to-near-receiver offset for this source
          obj%near_offset_this_shot =    &
              cnearts_get_near_offset(ntr, hdi, obj%trc_1_is_near)

          if (obj%opt_compute_cmpidx) then

!           update the cumulative distance the sources have
!           moved down the line
            obj%cumulative_delta_src = obj%cumulative_delta_src + deltaSrc

!           compute a common midpoint index for the traces in this shot
            call cnearts_compute_cmp_index(ntr, deltaSrc, obj, hdi)
          end if

          if (obj%opt_debug_print) then
!           print cmp index info
            call cnearts_print_cmp_index_info(obj, ntr, hdi, deltaSrc)
          end if

        end if

      end if

!     this is the last major if block in this subroutine, branching
!     based on the extrapolation mode. inside each option branch are
!     further branches based on the value of ntr.

!     process traces based on trace extrapolation mode
      if (string_upper_compare(obj%opt_trc_extrap, &
                               c_opt_trc_extrap(1))) then

! extrapolation option: stack nearest traces by common midpoint index

        if (obj%haveTransitionShot) then

!         have detected that cable number has changed on last shot input

          if (ntr >= 1) then
!           this new shot is on a different cable than previous shot

!           save this shot for later processing after current shots
!           on current cable are flushed from memory
            call cnearts_save_transition_shot(obj, ntr, hdi, tri)

!           begin flushing shots for current cable
            if (obj%opt_debug_print) then
              call pc_print('  cnearts(): have transition shot <------------ &
                            &begin flushing shots for this cable')
            end if

          end if

!         flush next shot in memory
          call cnearts_flush_shots_in_memory(obj, ntr, hdo, tro, error_status)
          if (error_status /= 0) then
              ntr = FATAL_ERROR
              return
          end if

!         return value of ntr from previous subroutine call indicates:
!           1) ntr > 0, there are traces to output this cycle
!           2) ntr == NO_MORE_TRACES, there are no more traces to output
!              from cnearts

          if ( ntr > 0) then

!           shot ready to flush, so return
            return

          else if (ntr == NO_MORE_TRACES) then   ! ntr has been set just above

!           no more shots to flush on this cable. set up for next cable,
!           the first shot of which was previously save to temp memory

!           indicate we're no longer in transition shot state
            obj%haveTransitionShot = .false.

!           do data consistency check. there should be no shots in tracker.
            if (obj%index_to_newest_shot /= 0) then
              call pc_print('cnearts(): tracker has shots at start of new &
                            &cable')
              ntr = FATAL_ERROR
              call cnearts_wrapup(obj)
              return
            end if

!           transfer the first shot of the next cable to the input
!           arrays, which allows us to use code originally setup to
!           process just a single source-cable dataset. this set ntr
!           to be the number of traces in that shot (i.e., ntr >= 1).
            call cnearts_get_transition_shot(obj, ntr, hdi, tri)

!           do the setup for this first shot on cable
            call cnearts_do_first_shot_setup(obj, ntr, hdi, tri, deltaRcvr, &
                                             srcPrevX, srcPrevY)

!           compute distance from previous source location to current location.
!           since srcPrevX and srcPrevY were set to current source location, the
!           deltaSrc for the first shot will be 0.
            deltaSrc = cnearts_point2point_dist( &
                           hdi(HDR_SOURCE_XLOC,1), hdi(HDR_SOURCE_YLOC,1), &
                           srcPrevX, srcPrevY)

!           now update the previous shot location values to current
            srcPrevX = hdi(HDR_SOURCE_XLOC,1)
            srcPrevY = hdi(HDR_SOURCE_YLOC,1)

            if (obj%opt_compute_cmpidx) then

!             update the cumulative distance the sources have
!             moved down the line
              obj%cumulative_delta_src = obj%cumulative_delta_src + deltaSrc

!             compute a common midpoint index for the traces in this shot
              call cnearts_compute_cmp_index(ntr, deltaSrc, obj, hdi)
            end if

            if (obj%opt_debug_print) then
!             print cmp index info
              call cnearts_print_cmp_index_info(obj, ntr, hdi, deltaSrc)
            end if

          end if

        end if    !  end of if (obj%haveTransitionShot) 

        if (ntr >= 1) then

          if (obj%opt_debug_print) then
            write(dbmsg,*)'cnearts(): nearest midpt opt: ntr >=1, ntr = ', &
                          ntr,' <------------ have new shot'
            call pc_print(dbmsg)
          end if

!         new traces to process
          call cnearts_opt_stack_midpt_trc(obj,ntr,hdi,tri,hdo,tro,error_status)
          if (error_status /= 0) then
            call pc_print('cnearts(): error from cnearts_opt_stack_midpt_trc()&
                          &, aborting')
            ntr = FATAL_ERROR
            call cnearts_wrapup(obj)
            return
          end if

!         return value of ntr from previous subroutine call indicates:
!           1) ntr > 0, there are traces to output this cycle
!           2) ntr == NO_MORE_TRACES, there are no more traces to output
!              from cnearts
!           3) ntr == NEED_TRACES, there are no traces ready to output
!              from cnearts and more are needed

        else if (ntr == NO_MORE_TRACES) then

          if (obj%opt_debug_print) then
            call pc_print('  cnearts(): nearest midpt opt, &
                          &<------------ called with ntr==NO_MORE_TRACES')
          end if

!         no more traces to process. clear any shots still in memory (if
!         any).
          call cnearts_flush_shots_in_memory(obj, ntr, hdo, tro, error_status)
          if (error_status /= 0) then
              ntr = FATAL_ERROR
              return
          end if

!         return value of ntr from previous subroutine call indicates:
!           1) ntr > 0, there are traces to output this cycle
!           2) ntr == NO_MORE_TRACES, there are no more traces to output
!              from cnearts

          if (ntr == NO_MORE_TRACES) then

!           there are no more traces, so wrapup and then pass that ntr
!           value on
            call cnearts_wrapup(obj)

          end if

        else if (ntr == NEED_TRACES) then

          if (obj%opt_debug_print) then
            call pc_print('  cnearts(): nearest midpt opt: ntr == NEED_TRACES &
                          &<------------ request traces')
          end if

!         later process needs traces. check shots in memory for
!         any that have all traces required for compositing.
          call cnearts_output_ready_shot(obj, ntr, hdo, tro, error_status)
          if (error_status /= 0) then
              ntr = FATAL_ERROR
              return
          end if

!         return value of ntr indicates:
!           1) ntr > 0, there are traces to output this cycle (and could
!              be more for future cycles) or
!           2) ntr == NO_MORE_TRACES, there are no traces ready to output
!              from cnearts

          if (ntr == NO_MORE_TRACES) then

!           there are no shots ready to output, so pass request for traces
!           up the line
            ntr = NEED_TRACES

          end if

        end if    ! end of if (ntr >= 1),(ntr == NO_MORE_TRACES), 
                  ! (ntr == NEED_TRACES) branch

      else if (string_upper_compare(obj%opt_trc_extrap, &
                                    c_opt_trc_extrap(2))) then

! extrapolation method: Stack Nearest From Same Shot

        if (ntr >= 1) then

!         new traces to process
          call cnearts_opt_stack_near_trc_same(obj,ntr,hdi,tri,hdo,tro, &
                                               error_status)
          if (error_status /= 0) then
              ntr = FATAL_ERROR
              return
          end if

!         set ntr to indicate there are traces ready for output
          ntr = ntr+obj%num_fill_1st

        else if (ntr == NO_MORE_TRACES) then

!         no more traces to process and none in holding, so wrap up

          if (obj%opt_debug_print) then
            call pc_print('  cnearts(): same shot opt: ntr == NO_MORE_TRACES &
                          &<------------ wrap up and return')
          end if

          call cnearts_wrapup(obj)

        else if (ntr == NEED_TRACES) then

!         later process needs traces, but this extrapolation option retains
!         no traces, so allow subroutine to return, requesting more traces

          if (obj%opt_debug_print) then
            call pc_print('  cnearts(): same shot opt: ntr == NEED_TRACES &
                          &<------------ request traces')
          end if


        end if

      else if (string_upper_compare(obj%opt_trc_extrap, &
                               c_opt_trc_extrap(3))) then

! extrapolation option: insert dead traces

        if (ntr >= 1) then

!         new traces to process
          call cnearts_opt_insert_dead_trc(obj,ntr,hdi,tri,hdo,tro)

!         set ntr to indicate there are traces ready for output
          ntr = ntr+obj%num_fill_1st

        else if (ntr == NO_MORE_TRACES) then

!         no more traces to process and none in holding, so wrap up

          if (obj%opt_debug_print) then
            call pc_print('  cnearts(): dead trace opt: ntr == NO_MORE_TRACES &
                          &<------------ wrap up and return')
          end if

          call cnearts_wrapup(obj)

        else if (ntr == NEED_TRACES) then

!         later process needs traces, but this extrapolation option retains
!         no traces, so allow subroutine to return, requesting more traces

          if (obj%opt_debug_print) then
            call pc_print('  cnearts(): dead trace opt: ntr == NEED_TRACES &
                          &<------------ request traces')
          end if

        end if

      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts(): leave, actual ntr out = ',ntr)
      end if

    end subroutine cnearts


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


    subroutine cnearts_wrapup (obj)
      type(cnearts_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (pc_get_update_state() /= PC_EXECUTE) return

    end subroutine cnearts_wrapup


!!--------------------------- extrapolation --------------------------------!!
!!--------------------------- extrapolation --------------------------------!!
!!--------------------------- extrapolation --------------------------------!!


    subroutine cnearts_opt_stack_near_trc_same (obj,ntr,hdi,tri,hdo,tro, &
                                                err_ret_val)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer             , intent(in)    :: ntr                ! arguments
      double precision   ,  intent(in)    :: hdi(:,:)           ! arguments
      real               ,  intent(in)    :: tri(:,:)           ! arguments
      double precision   ,  intent(inout) :: hdo(:,:)           ! arguments
      real               ,  intent(inout) :: tro(:,:)           ! arguments
      integer             , intent(out)   :: err_ret_val        ! arguments

      double precision :: hdtmp(1:obj%nwih)
      real    :: trtmp(1:obj%ndpt)
      real    :: trsum(1:obj%ndpt)
      integer :: ktrc, knumstak, cmp_index_hdr_word
      real    :: xcoord, ycoord
      real    :: offset, offnew, offsetsave
      integer :: mtop, mbot, mtopsave, mbotsave
      logical :: errstatus
      character(len=200) :: msg

!     print current group number
      call pc_print('CNEARTS processing shot ', int(hdi(obj%hdr_shot_index,1)))

!     set default status return value
      err_ret_val = 0

!     point to header word for cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

!     copy data from input arrays to output arrays and create missing traces
      select case (obj%trc_1_is_near)
      case (.true.)

!       copy the original traces to output arrays
        hdo(1:obj%nwih,1+obj%num_fill_1st:ntr+obj%num_fill_1st) = &
            hdi(1:obj%nwih,1:ntr)
        tro(1:obj%ndpt,1+obj%num_fill_1st:ntr+obj%num_fill_1st) = &
            tri(1:obj%ndpt,1:ntr)

!       create the missing traces
        do ktrc = obj%num_fill_1st, 1, -1

!         copy header of nearest trace to this fill trace header
          hdo(1:obj%nwih,ktrc) = hdi(1:obj%nwih,1)

!         initialize trace sum array
          trsum = 0.0

!         compute offset for this fill trace
          offsetsave  = abs(abs(hdi(HDR_OFFSET,1)) -  &
                        (obj%num_fill_1st + 1 - ktrc) * obj%rcv_grp_intrval)

!         initialize mute parameters for tracking min/max mute
          mtopsave = 999999
          mbotsave = 0

!         sum nearest traces from this shot for this missing trace
          do knumstak = 1, obj%num_trc_to_stack

!           copy trace to temp trace
            hdtmp(1:obj%nwih) = hdi(1:obj%nwih,knumstak)
            trtmp(1:obj%ndpt) = tri(1:obj%ndpt,knumstak)

!           initialize parameters needed for partial nmo
            xcoord  = hdtmp(obj%nhx)
            ycoord  = hdtmp(obj%nhy)
            offset  = abs(hdtmp(HDR_OFFSET))
            mtop    = hdtmp(HDR_TOP_MUTE)
            mbot    = hdtmp(HDR_BOTTOM_MUTE)
            offnew  = abs(offsetsave)

!           apply partial nmo
            call nmoprim_apply (obj%nmoprim, xcoord, ycoord, offset, offnew, &
                                trtmp(1:obj%ndpt), mtop, mbot, errstatus, msg)
            if (errstatus) then
                err_ret_val = 1
                return
            end if

!           sum this temp trace to temp trace
            trsum(1:obj%ndpt) = trsum(1:obj%ndpt) + trtmp(1:obj%ndpt)

!           update saved values for mute top and bottom
            mtopsave = min(mtop,mtopsave)
            mbotsave = max(mbot,mbotsave)

          end do

!         normalize sum trace and save to output
          tro(1:obj%ndpt,ktrc) = trsum(1:obj%ndpt)/obj%num_trc_to_stack

!         update header values
!         save offset
          hdo(HDR_SCRATCH_32,ktrc) = offsetsave

!         save mute values to header
          hdo(HDR_TOP_MUTE,ktrc) = mtopsave
          hdo(HDR_BOTTOM_MUTE,ktrc) = mbotsave

!         set the output cmp index
          hdo(cmp_index_hdr_word,ktrc) = &
              hdi(cmp_index_hdr_word,1) + obj%num_fill_1st + 1 - ktrc

!         reset HDR_MIDPOINT header values to 0
          hdo(HDR_MIDPOINT_XLOC,ktrc) = 0.0
          hdo(HDR_MIDPOINT_YLOC,ktrc) = 0.0
          hdo(HDR_MIDPOINT_ELEV,ktrc) = 0.0
          hdo(HDR_MIDPOINT_XGRID,ktrc) = 0.0
          hdo(HDR_MIDPOINT_YGRID,ktrc) = 0.0
          hdo(HDR_MIDPOINT_SHOTPOINT,ktrc) = 0.0

        end do

!       update amplitude info in header for the new traces
        call lav_set_hdr (hdo(1:obj%nwih,1:obj%num_fill_1st), &
                          tro(1:obj%ndpt,1:obj%num_fill_1st), &
                          obj%ndpt, obj%num_fill_1st)

      case (.false.)

!       copy the original traces
        hdo(1:obj%nwih,1:ntr) = hdi(1:obj%nwih,1:ntr)
        tro(1:obj%ndpt,1:ntr) = tri(1:obj%ndpt,1:ntr)

!       create the missing traces
        do ktrc = 1, obj%num_fill_1st

!         copy header of nearest trace to this fill trace header
          hdo(1:obj%nwih,ntr+ktrc) = hdi(1:obj%nwih,ntr)

!         initialize trace sum array
          trsum = 0.0

!         compute offset for this fill trace
          offsetsave  = abs(abs(hdi(HDR_OFFSET,ntr)) - &
                            ktrc * obj%rcv_grp_intrval)

!         initialize mute parameters for tracking min/max mute
          mtopsave = 999999
          mbotsave = 0

!         sum nearest traces from this shot for this missing trace
          do knumstak = 1, obj%num_trc_to_stack

!           copy trace to temp trace
            hdtmp(1:obj%nwih) = hdi(1:obj%nwih,ntr-knumstak+1)
            trtmp(1:obj%ndpt) = tri(1:obj%ndpt,ntr-knumstak+1)

!           initialize parameters needed for partial nmo
            xcoord  = hdtmp(obj%nhx)
            ycoord  = hdtmp(obj%nhy)
            offset  = abs(hdtmp(HDR_OFFSET))
            mtop    = hdtmp(HDR_TOP_MUTE)
            mbot    = hdtmp(HDR_BOTTOM_MUTE)
            offnew  = abs(offsetsave)

!           apply partial nmo
            call nmoprim_apply (obj%nmoprim, xcoord, ycoord, offset, offnew, &
                                trtmp(1:obj%ndpt), mtop, mbot, errstatus, msg)
            if (errstatus) then
                err_ret_val = 1
                return
            end if

!           sum this temp trace to temp trace
            trsum(1:obj%ndpt) = trsum(1:obj%ndpt) + trtmp(1:obj%ndpt)

!           update saved values for mute top and bottom
            mtopsave = min(mtop,mtopsave)
            mbotsave = max(mbot,mbotsave)

          end do

!         normalize sum trace and save to output
          tro(1:obj%ndpt,ntr+ktrc) = trsum(1:obj%ndpt)/obj%num_trc_to_stack

!         update header values
!         save offset
          hdo(HDR_SCRATCH_32,ntr+ktrc) = offsetsave

!         save mute values to header
          hdo(HDR_TOP_MUTE,ntr+ktrc) = mtopsave
          hdo(HDR_BOTTOM_MUTE,ntr+ktrc) = mbotsave

!         set the output cmp index
          hdo(cmp_index_hdr_word,ntr+ktrc) = hdi(cmp_index_hdr_word,ntr) + ktrc

!         reset HDR_MIDPOINT header values to 0
          hdo(HDR_MIDPOINT_XLOC,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_YLOC,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_ELEV,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_XGRID,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_YGRID,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_SHOTPOINT,ntr+ktrc) = 0.0

        end do

!       update amplitude info in header for the new traces
        call lav_set_hdr (hdo(1:obj%nwih,ntr+1:ntr+obj%num_fill_1st), &
                          tro(1:obj%ndpt,ntr+1:ntr+obj%num_fill_1st), &
                          obj%ndpt, obj%num_fill_1st)

      end select

!     set header values in new headers
      call cnearts_set_header_values(obj, ntr, obj%num_fill_1st, hdo)

    end subroutine cnearts_opt_stack_near_trc_same


    subroutine cnearts_opt_insert_dead_trc (obj,ntr,hdi,tri,hdo,tro)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer             , intent(in)    :: ntr                ! arguments
      double precision   ,  intent(in)    :: hdi(:,:)           ! arguments
      real               ,  intent(in)    :: tri(:,:)           ! arguments
      double precision   ,  intent(inout) :: hdo(:,:)           ! arguments
      real               ,  intent(inout) :: tro(:,:)           ! arguments

      integer :: ktrc, cmp_index_hdr_word

!     print current group number
      call pc_print('CNEARTS processing shot ', int(hdi(obj%hdr_shot_index,1)))

!     point to header word for cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

      select case (obj%trc_1_is_near)
      case (.true.)
        hdo(1:obj%nwih,1:obj%num_fill_1st) = hdi(1:obj%nwih,1:obj%num_fill_1st)
        hdo(1:obj%nwih,1+obj%num_fill_1st:ntr+obj%num_fill_1st) = &
            hdi(1:obj%nwih,1:ntr)
        tro(1:obj%ndpt,1:obj%num_fill_1st) = 0.0
        tro(1:obj%ndpt,1+obj%num_fill_1st:ntr+obj%num_fill_1st) = &
            tri(1:obj%ndpt,1:ntr)

!       update some header values
        do ktrc = obj%num_fill_1st, 1, -1

!         set the output cmp index
          hdo(cmp_index_hdr_word,ktrc) = &
              hdi(cmp_index_hdr_word,1) + obj%num_fill_1st + 1 - ktrc

!         reset HDR_MIDPOINT header values to 0
          hdo(HDR_MIDPOINT_XLOC,ktrc) = 0.0
          hdo(HDR_MIDPOINT_YLOC,ktrc) = 0.0
          hdo(HDR_MIDPOINT_ELEV,ktrc) = 0.0
          hdo(HDR_MIDPOINT_XGRID,ktrc) = 0.0
          hdo(HDR_MIDPOINT_YGRID,ktrc) = 0.0
          hdo(HDR_MIDPOINT_SHOTPOINT,ktrc) = 0.0

        end do

!       update amplitude info in header for the new traces
        call lav_set_hdr (hdo(1:obj%nwih,1:obj%num_fill_1st), &
                          tro(1:obj%ndpt,1:obj%num_fill_1st), &
                          obj%ndpt, obj%num_fill_1st)

      case (.false.)
        hdo(1:obj%nwih,1:ntr) = hdi(1:obj%nwih,1:ntr)
        hdo(1:obj%nwih,ntr+1:ntr+obj%num_fill_1st) = &
            hdi(1:obj%nwih,ntr+1-obj%num_fill_1st:ntr)
        tro(1:obj%ndpt,1:ntr) = tri(1:obj%ndpt,1:ntr)
        tro(1:obj%ndpt,ntr+1:ntr+obj%num_fill_1st) = 0.0

!       update some header values
        do ktrc = 1, obj%num_fill_1st

!         set the output cmp index
          hdo(cmp_index_hdr_word,ntr+ktrc) = hdi(cmp_index_hdr_word,ntr) + ktrc

!         reset HDR_MIDPOINT header values to 0
          hdo(HDR_MIDPOINT_XLOC,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_YLOC,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_ELEV,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_XGRID,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_YGRID,ntr+ktrc) = 0.0
          hdo(HDR_MIDPOINT_SHOTPOINT,ntr+ktrc) = 0.0

        end do

!       update amplitude info in header for the new traces
        call lav_set_hdr (hdo(1:obj%nwih,ntr+1:ntr+obj%num_fill_1st), &
                          tro(1:obj%ndpt,ntr+1:ntr+obj%num_fill_1st), &
                          obj%ndpt, obj%num_fill_1st)

      end select

!     set header values in new headers
      call cnearts_set_header_values(obj, ntr, obj%num_fill_1st, hdo)

    end subroutine cnearts_opt_insert_dead_trc


    subroutine cnearts_opt_stack_midpt_trc(obj,ntr,hdi,tri,hdo,tro,errstat)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer,              intent(inout) :: ntr                ! arguments
      double precision,     intent(in)    :: hdi(:,:)           ! arguments
      real,                 intent(in)    :: tri(:,:)           ! arguments
      double precision,     intent(inout) :: hdo(:,:)           ! arguments
      real,                 intent(inout) :: tro(:,:)           ! arguments
      integer,              intent(out)   :: errstat            ! arguments

!     local variables
      integer ::    newShotIndx, astatus   
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_opt_stack_midpt_trc(): enter')
      end if

!     set default error status return
      errstat = 0

! KLUDGE: added to prevent SIGSEGV error when cnearts starts processing
!         2nd shot and OPT_DEBUG_PRINT is NO
      call pc_print('CNEARTS processing shot ', int(hdi(obj%hdr_shot_index,1)))

!     check shot tracker for slot to save this shot. start with index to
!     oldest and search up (modulo obj%max_trckr_index) to next empty spot.
      newShotIndx = cnearts_get_new_tracker_index(obj)

      if (newShotIndx == 0) then

!       error has occurred in assigning new shot to tracker, tracker
!       is full. determine if a shot should be flushed from tracker
!       or abort.

        if (obj%abort_on_trckr_error) then
!         internal parameter abort_on_trckr_error set to abort on
!         detection of a shot tracker error rather than flush the
!         oldes shot and attempt to continue
          call cnearts_wrapup(obj)
          call pc_error('Overflow of shot tracker buffer. Abort.')
          errstat = 1
          ntr = FATAL_ERROR
          return
        end if

!       didn't find an empty slot for new shot. attempt to flush one from
!       memory.
        call cnearts_flush_shots_in_memory(obj, ntr, hdo, tro, errstat)
        if (errstat /= 0) then
          errstat = 1
          ntr = FATAL_ERROR
          return
        end if

!       return value of ntr indicates:
!         1) ntr > 0, there are traces to output this cycle
!         2) ntr == NO_MORE_TRACES, there are no more traces to output
!            from cnearts

        if (ntr == NO_MORE_TRACES) then

!         there are no more traces to output but no space was available
!         in the tracker. this is a strange, supposedly unattainable
!         state, so wrapup and then pass on a fatal error notification.
          call cnearts_wrapup(obj)
          call pc_error('Overflow of shot tracker buffer.')
          call pc_error('Unable to flush oldest shot from shot tracker.')
          errstat = 1
          ntr = FATAL_ERROR
          return

        else

!         output arrays now set with oldest shot to output and slot
!         in tracker should now be available. attempt assignment of current
!         shot again.
          call pc_warning('WARNING: shot tracker error forced shot flush')
          newShotIndx = cnearts_get_new_tracker_index(obj)

          if (newShotIndx == 0) then

!           failed second attempt to assign current shot to tracker. there
!           should have been an empty slot after flushing the oldest shot.
!           declare an error, wrap-up, and give up.
            call pc_error('Failure to assign shot to tracker, second attempt.')
            errstat = 1
            ntr = FATAL_ERROR
            call cnearts_wrapup(obj)
            return

          else

!           successfully assigned shot. print warning of circumstances.
            call pc_warning('Assigned shot to tracker, second attempt')

          end if
        end if
      end if

!     if here, got index to slot in tracker for new shot

      if (obj%opt_debug_print) then
        call pc_print('  Assign new shot to index ', newShotIndx)
      end if

!     if obj%index_to_oldest_shot is still 0, this is first shot
!     assigned to the tracker, so initialize obj%index_to_oldest_shot
      if (obj%index_to_oldest_shot == 0) then

!       this is first shot going into tracker. it must have newShotIndx = 1
        if (newShotIndx == 1) then
!         initialize obj%index_to_oldest_shot
          obj%index_to_oldest_shot = newShotIndx
          if (obj%opt_debug_print) then
            call pc_print('  Initialize oldest shot index to ', newShotIndx)
          end if
        else
!         inconsistent newShotIndx for first shot
          call pc_error('First shot in tracker got index = ', newShotIndx)
          errstat = 1
          ntr = FATAL_ERROR
          return
        end if
      end if
!     found empty slot, initialize, and copy in shot data
      obj%index_to_newest_shot = newShotIndx
      call cnearts_init_tracker(ntr, obj, hdi, tri, errstat)
      if (errstat /= 0) then
        return
      end if

!     scan traces in new shot, entering each into shot tracker for
!     previous shots. not necessary if there is only one shot in tracker
!     array.
      if (cnearts_get_num_shots_tracked(obj) > 1) then

        if (obj%opt_debug_print) then
          write(dbmsg,*)'cnearts_opt_stack_midpt_trc(): ', &
                        cnearts_get_num_shots_tracked(obj), &
                        ' shots in tracker'
          call pc_print(dbmsg)
          write(dbmsg,*)'    Index to oldest shot = ',obj%index_to_oldest_shot
          call pc_print(dbmsg)
          write(dbmsg,*)'    Index to newest shot = ',obj%index_to_newest_shot
          call pc_print(dbmsg)
          write(dbmsg,*)'    Max shot trckr index = ',obj%max_trckr_index
          call pc_print(dbmsg)
        end if

!       scan new shot and get traces from it moved into shot tracker
!       compositing arrays until all midpoints of all other shots have
!       received a trace
        call cnearts_load_compositing_trcs(obj, astatus)
        if (astatus == ASSIGN_STAT_ASSIGNED) then

!         traces were assigned to midpoints of oldest previous shot's missing
!         traces

!         look at oldest shot in shot tracker and do compositing of all
!         fill traces ("oldshottrkr%num_fill_trkr of them) if it is ready
!         (all compositing traces have been loaded for all fill traces).
!         subr cnearts_output_ready_shot returns ntr>0 if there are traces to
!         write, ntr==NEED_TRACES if more input is needed, or ntr==FATAL_ERROR
!         if there was a bad error.
          call cnearts_output_ready_shot(obj, ntr, hdo, tro, errstat)
          if (errstat /= 0) then
              ntr = FATAL_ERROR
              return
          end if
          if (obj%opt_debug_print) then
            if (ntr == NEED_TRACES) then
              call pc_print('  Return of ntr = NEED_TRACES from &
                            &cnearts_output_ready_shot()')
            else if (ntr > 0) then
              call pc_print('  Return of ntr > 0 from &
                            &cnearts_output_ready_shot()')
            else if (ntr == FATAL_ERROR) then
              call pc_print('  Return of ntr == FATAL_ERROR from &
                            &cnearts_output_ready_shot()')
              errstat = 1
            end if
          end if
        else if (astatus == ASSIGN_STAT_FLUSH) then

!         traces were NOT assigned to midpoints of oldest previous shot's
!         missing traces. probably shot gap, so force oldest shot to be
!         flushed from memory.
          call cnearts_flush_shots_in_memory(obj, ntr, hdo, tro, errstat)
          if (errstat /= 0) then
              errstat = 1
              ntr = FATAL_ERROR
              return
          end if

!         return value of ntr indicates:
!           1) ntr > 0, there are traces to output this cycle
!           2) ntr == NO_MORE_TRACES, there are no more traces to output
!              from cnearts

          if (ntr == NO_MORE_TRACES) then
!           there are no more traces, so wrapup and then pass that ntr
!           value on
            call cnearts_wrapup(obj)
          end if

        else
          errstat = 1
          ntr = FATAL_ERROR
          return
        end if

      else

!       there is only 1 shot assigned to the tracker now. just set
!       ntr to NEED_TRACES to request another shot on the next pass
!       through cnearts.
        if (obj%opt_debug_print) then
          call pc_print('cnearts_opt_stack_midpt_trc(): only 1 shot in tracker')
          write(dbmsg,*)'    Index to oldest shot = ',obj%index_to_oldest_shot
          call pc_print(dbmsg)
          write(dbmsg,*)'    Index to newest shot = ',obj%index_to_newest_shot
          call pc_print(dbmsg)
          write(dbmsg,*)'    Max shot trckr index = ',obj%max_trckr_index
          call pc_print(dbmsg)
        end if
        ntr = NEED_TRACES
      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_opt_stack_midpt_trc(): leave')
      end if

    end subroutine cnearts_opt_stack_midpt_trc


    subroutine cnearts_init_tracker(numTrc, obj, hdi, tri, errstat)
      integer,              intent(in)    :: numTrc
      type(cnearts_struct), intent(inout) :: obj
      double precision,     intent(in)    :: hdi(:,:)
      real,                 intent(in)    :: tri(:,:)
      integer,              intent(out)   :: errstat

      type(shot_tracker_struct), pointer :: stptr
      integer :: k, neartrcindex, newShotIndx, numCompTrcs
      integer :: cmp_index_hdr_word
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_init_tracker(): enter')
      end if

!     set local var to new shot index and point at that shot
      newShotIndx = obj%index_to_newest_shot
      stptr => obj%shot_trckr(newShotIndx)

!     reset the tracker contents
      call cnearts_reset_tracker(obj, stptr, errstat)
      if (errstat /= 0) then
        return
      end if

!     set number of traces in shot and flag this slot as in use
      stptr%num_trc_trkr = numTrc
      stptr%in_use = .true.

!     copy the shot headers and data to tracker
      call cnearts_save_shot(obj, newShotIndx, hdi, tri)

!     get the header index for the trace nearest the source
      select case(obj%trc_1_is_near)
      case(.true.)
        neartrcindex = 1
      case(.false.)
        neartrcindex = numTrc
      end select

!     set location for cmp index in header
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

!     set the cmp index associated with the composite trace groups.
!     this puts the cmp index farthest from the shot in composTrcPtr(1).
!     that is the lowest numbered cmp index of this num_fill cmp indices.
      do k = 1, stptr%num_fill_trkr

!       set the cmp index and offset
        stptr%cmposTrcPtr(k)%comp_cmp_index = &
            hdi(cmp_index_hdr_word,neartrcindex) +  k*obj%del_cmp_index
        stptr%cmposTrcPtr(k)%comp_nom_offset = &
            sign(abs(hdi(HDR_OFFSET,neartrcindex)) -  k * obj%rcv_grp_intrval, &
                 hdi(HDR_OFFSET,neartrcindex))

!       copy header of nearest trace to the composite trace header. this
!       sets many of the header values.
        numCompTrcs = stptr%cmposTrcPtr(k)%comp_num_total_trc
        stptr%cmposTrcPtr(k)%comp_trc_hdr(:,numCompTrcs) = hdi(:,neartrcindex)

      end do

      if (obj%opt_debug_print) then
        write(dbmsg,*)' New shot trace ', &
            int(stptr%saveShotPtr%save_trc_hdr(HDR_CURRENT_CHANNEL,1)), &
            ' trc index = 1 cmp index = ', &
            int(stptr%saveShotPtr%save_trc_hdr(cmp_index_hdr_word,1)), &
            ', trace ', &
            int(stptr%saveShotPtr%save_trc_hdr(HDR_CURRENT_CHANNEL,  &
                stptr%num_trc_trkr)), &
            ', trc index = ',stptr%num_trc_trkr,' cmp index = ', &
            int(stptr%saveShotPtr%save_trc_hdr(cmp_index_hdr_word,  &
                stptr%num_trc_trkr))
        call pc_print(dbmsg)
        write(dbmsg,*)' Composite 1 gets cmp index ', &
          int(stptr%cmposTrcPtr(1)%comp_cmp_index),' Composite ',  &
          stptr%num_fill_trkr,' gets cmp index ',  &
          stptr%cmposTrcPtr(stptr%num_fill_trkr)%comp_cmp_index, &
          ' nom_offset = ',  &
          stptr%cmposTrcPtr(stptr%num_fill_trkr)%comp_nom_offset
        call pc_print(dbmsg)
      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_init_tracker(): leave')
      end if

    end subroutine cnearts_init_tracker


    subroutine cnearts_reset_tracker(obj, trkr, errstat)
      type(cnearts_struct),      intent(in)    :: obj
      type(shot_tracker_struct), intent(inout) :: trkr
      integer,                   intent(out)   :: errstat

      integer            :: k, prev_num_fill   
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_reset_tracker(): enter')
      end if

      ! set error status
      errstat = 0

      ! retain the previous value for number of fill traces in this
      ! shot tracker struct
      prev_num_fill = trkr%num_fill_trkr

      ! reset the scalars
      trkr%in_use = .false.
      trkr%ready_to_output = .false.
      trkr%num_fill_trkr = cnearts_compute_numfill(obj%near_offset_this_shot, &
                               obj%rcv_grp_intrval)
      if (obj%opt_debug_print) then
        write(dbmsg,*)'cnearts_reset_tracker(): old num_fill = ',prev_num_fill
        call pc_print(dbmsg)
        write(dbmsg,*)'cnearts_reset_tracker(): near offset = ', &
                      obj%near_offset_this_shot,' group interval = ', &
                      obj%rcv_grp_intrval,' --> num fill = ',trkr%num_fill_trkr
        call pc_print(dbmsg)
      end if
      if (trkr%num_fill_trkr > obj%max_num_fill_trc) then
        write(dbmsg,*)'cnearts_reset_tracker(): number of fill traces = ', &
                      trkr%num_fill_trkr,' exceeds max fill traces = ', &
                      obj%max_num_fill_trc
        call pc_print(dbmsg)
        errstat = 1
      end if
      trkr%num_trc_trkr = 0

      if (trkr%num_fill_trkr == prev_num_fill) then

        ! NO change in number of fill traces

        ! reset the composite trace structs
        do k = 1, trkr%num_fill_trkr
          trkr%cmposTrcPtr(k)%comp_num_total_trc = obj%num_trc_to_stack + 1
          trkr%cmposTrcPtr(k)%comp_num_saved_trc = 0
          trkr%cmposTrcPtr(k)%comp_cmp_index = 0
          trkr%cmposTrcPtr(k)%comp_trc_hdr = 0.0
          trkr%cmposTrcPtr(k)%comp_trc_smp = 0.0
          if (obj%opt_debug_output) then
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_saved_trc = 0
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_trc_hdr = 0.0
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_trc_smp = 0.0
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_trc_hdr(HDR_USER_48,:) = &
                flag_cnearts_trace
          end if
        end do

        if (obj%opt_debug_print) then
          write(dbmsg,*)' shot ', &
                        trkr%saveShotPtr%save_trc_hdr(obj%hdr_shot_index,1), &
                        ' num_fill_trkr: previous = ',prev_num_fill, &
                        ' new = ',trkr%num_fill_trkr, &
                        ' <---- number fill traces unchanged'
          call pc_print(dbmsg)
        end if

      else

        ! CHANGE in number of fill traces

        ! destroy existing composite trace structs
        call cnearts_comp_trc_array_destrct(trkr%cmposTrcPtr, prev_num_fill)

        ! now allocate a new set of composite trace structs
        call cnearts_comp_trc_array_cnstrct(obj, trkr%num_fill_trkr, &
                 trkr%cmposTrcPtr, errstat)
        if (errstat /= 0) then
          call pc_error('Failed to reallocate composite trace array ')
          errstat = 2
          return
        end if

        ! reset the composite trace structs
        do k = 1, trkr%num_fill_trkr
          trkr%cmposTrcPtr(k)%comp_num_total_trc = obj%num_trc_to_stack + 1
          trkr%cmposTrcPtr(k)%comp_num_saved_trc = 0
          trkr%cmposTrcPtr(k)%comp_cmp_index = 0
          trkr%cmposTrcPtr(k)%comp_trc_hdr = 0.0
          trkr%cmposTrcPtr(k)%comp_trc_smp = 0.0
          if (obj%opt_debug_output) then
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_saved_trc = 0
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_trc_hdr = 0.0
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_trc_smp = 0.0
            trkr%cmposTrcPtr(k)%postmoTrcPtr%ret_trc_hdr(HDR_USER_48,:) = &
                flag_cnearts_trace
          end if
        end do

        if (obj%opt_debug_print) then
          write(dbmsg,*)' shot ', &
                        trkr%saveShotPtr%save_trc_hdr(obj%hdr_shot_index,1), &
                        ' num_fill_trkr: previous = ',prev_num_fill, &
                        ' new = ',trkr%num_fill_trkr, &
                        ' <---- number fill traces CHANGED'
          call pc_print(dbmsg)
        end if
      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_reset_tracker(): leave')
      end if

    end subroutine cnearts_reset_tracker


    subroutine cnearts_save_shot(obj, newShotIndx, hdi, tri)
      type(cnearts_struct), intent(in) :: obj
      integer,              intent(in) :: newShotIndx
      double precision,     intent(in) :: hdi(:,:)
      real,                 intent(in) :: tri(:,:)

      type(shot_tracker_struct), pointer :: stptr
      integer :: nwih, ndpt, numTrc

      if (obj%opt_debug_print) then
        call pc_print('cnearts_save_shot(): enter')
      end if

      stptr => obj%shot_trckr(newShotIndx)
      numTrc = stptr%num_trc_trkr
      nwih = obj%nwih
      ndpt = obj%ndpt

      if (obj%opt_debug_print) then
        call pc_print('  Save shot with num traces = ',numTrc, &
                      ' num samples = ',ndpt)
      end if

!     copy header array
      stptr%saveShotPtr%save_trc_hdr(1:nwih,1:numTrc) = hdi(1:nwih,1:numTrc)

!     copy trace sample array
      stptr%saveShotPtr%save_trc_smp(1:ndpt,1:numTrc) = tri(1:ndpt,1:numTrc)

      if (obj%opt_debug_print) then
        call pc_print('cnearts_save_shot(): leave')
      end if

    end subroutine cnearts_save_shot


    subroutine cnearts_get_transition_shot(obj, numTrc, hdr, trc)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(inout) :: numTrc
      double precision,     intent(inout) :: hdr(:,:)
      real,                 intent(inout) :: trc(:,:)

      integer :: nwih, ndpt

      if (obj%opt_debug_print) then
        call pc_print('cnearts_get_transition_shot(): enter')
      end if

      nwih = obj%nwih
      ndpt = obj%ndpt

!     get number of traces saved
      numTrc = obj%transitionShotPtr%ret_saved_trc
      if (obj%opt_debug_print) then
        call pc_print('  Get transition shot with num traces = ',numTrc)
      end if

!     copy saved header array to input header array
      hdr(1:nwih,1:numTrc) = obj%transitionShotPtr%ret_trc_hdr(1:nwih,1:numTrc)

!     copy saved trace sample array to input data array
      trc(1:ndpt,1:numTrc) = obj%transitionShotPtr%ret_trc_smp(1:ndpt,1:numTrc)

      if (obj%opt_debug_print) then
        call pc_print('cnearts_get_transition_shot(): leave')
      end if

    end subroutine cnearts_get_transition_shot


    subroutine cnearts_save_transition_shot(obj, numTrc, hdr, trc)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(in)    :: numTrc
      double precision,     intent(in)    :: hdr(:,:)
      real,                 intent(in)    :: trc(:,:)

      integer :: nwih, ndpt

      if (obj%opt_debug_print) then
        call pc_print('cnearts_save_transition_shot(): enter')
      end if

      nwih = obj%nwih
      ndpt = obj%ndpt

      if (obj%opt_debug_print) then
        call pc_print('  Save transition shot with num traces = ',numTrc)
      end if

!     save number of traces
      obj%transitionShotPtr%ret_saved_trc = numTrc

!     copy header array
      obj%transitionShotPtr%ret_trc_hdr(1:nwih,1:numTrc) = hdr(1:nwih,1:numTrc)

!     copy trace sample array
      obj%transitionShotPtr%ret_trc_smp(1:ndpt,1:numTrc) = trc(1:ndpt,1:numTrc)

      if (obj%opt_debug_print) then
        call pc_print('cnearts_save_transition_shot(): leave')
      end if

    end subroutine cnearts_save_transition_shot


    subroutine cnearts_load_compositing_trcs(obj, assignStatus)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(out)   :: assignStatus

!     local vars
      integer :: kshot, ktrc
      integer :: numTrkrShots, cmpIdx
      integer :: nearTrcIdx, farTrcIdx, trcIdxInc, startTrcIdx, endTrcIdx
      integer :: prevShotIdx, oldShotIdx, newShotIdx
      integer :: numTrcAssigned, numTrcFull, numTrcOutOfRange
      integer :: numTrcAssignedOldShot, numTrcFullOldShot
      integer :: numTrcOutOfRangeOldShot, numTotalOldShot, numTotal
      double precision,          pointer :: hdrptr(:)
      real,                      pointer :: smpptr(:)
      type(shot_tracker_struct), pointer :: prevShotTrkr
      type(shot_tracker_struct), pointer :: newShotTrkr
      character(len=200)                 :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_load_compositing_trcs(): enter')
      end if

!     get number of shots currently in tracker
      numTrkrShots = cnearts_get_num_shots_tracked(obj)
      if (numTrkrShots < 2) then
        call pc_error('cnearts_load_compositing_trcs() called with < 2 shots')
        assignStatus = ASSIGN_STAT_ERROR
        return
      end if

!     fill out the entries in the new shot trace index to lowest cmp index
!     from previous shot array
      call cnearts_map_cmp_to_trc_idx(obj, numTrkrShots)

!     get index for trace nearest shot and increment (+ or -1) to next trace
      select case(obj%trc_1_is_near)
      case (.true.)
        nearTrcIdx = 1
        farTrcIdx = obj%shot_trckr(newShotIdx)%num_trc_trkr
        trcIdxInc = 1
      case (.false.)
        nearTrcIdx = obj%shot_trckr(newShotIdx)%num_trc_trkr
        farTrcIdx = 1
        trcIdxInc = -1
      end select

      if (obj%opt_debug_print) then
!       tmp debug
        call pc_print('  cnearts_load_compositing_trcs(): got trace increment')
      end if

!     define local vars as index to oldest & newest shots in obj
      oldShotIdx = obj%index_to_oldest_shot
      newShotIdx = obj%index_to_newest_shot
      newShotTrkr => obj%shot_trckr(newShotIdx)
      if (obj%opt_debug_print) then
!       tmp debug
        call pc_print('  cnearts_load_compositing_trcs(): got shot indices')
      end if

!     cycle through shots, oldest to newest, assigning traces from new shot
!     to composite objects of previous shots
      prevShotIdx = oldShotIdx
      numTrcAssigned = 0
      numTrcAssignedOldShot = 0
      numTrcFull = 0
      numTrcOutOfRange = 0
      numTrcOutOfRangeOldShot = 0
      numTotal = 0
      numTotalOldShot = 0
      do kshot = 1, numTrkrShots-1

!       point at tracker for current previous shot
        prevShotTrkr => obj%shot_trckr(prevShotIdx)
        if (obj%opt_debug_print) then
!         tmp debug
          call pc_print('  cnearts_load_compositing_trcs(): prevShotTrkr =>')
        end if

!       get trace index to new shot from map as starting location for
!       finding traces to assign to current shot's composite trace object
        startTrcIdx = prevShotTrkr%mapCmp2Trc

        if (obj%opt_debug_print) then
          write(dbmsg,*)' for prevShotIdx = ',prevShotIdx, &
                        ' look for matching input trace beginning at ', &
                        ' trcIdx = ',prevShotTrkr%mapCmp2Trc
          call pc_print(dbmsg)
        end if

        endTrcIdx = startTrcIdx-trcIdxInc*(prevShotTrkr%num_fill_trkr - 1)
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Scan ktrc from ',startTrcIdx,' to ', &
                        endTrcIdx,', step ',-trcIdxInc
          call pc_print(dbmsg)
        end if

        do ktrc = startTrcIdx, endTrcIdx, -trcIdxInc

          if (ktrc < 1 .or.   &
              ktrc > obj%shot_trckr(newShotIdx)%num_trc_trkr) then
            if (obj%opt_debug_print) then
              write(dbmsg,*)' Exiting ktrc loop at ktrc = ',ktrc
              call pc_print(dbmsg)
            end if
            
            exit  ! exit the ktrc loop
          end if

!         point to header and samples for this trace
          hdrptr => obj%shot_trckr(newShotIdx)%saveShotPtr%save_trc_hdr(:,ktrc)
          smpptr => obj%shot_trckr(newShotIdx)%saveShotPtr%save_trc_smp(:,ktrc)

!         count this a a trace attempted to assigned
          numTotal = numTotal + 1
          if (prevShotIdx == oldShotIdx) then
            numTotalOldShot = numTotalOldShot + 1
          end if

!         attempt to assign the current hdr and samples in appropriate composite
          cmpIdx = cnearts_assign_comp_trc(obj, hdrptr, smpptr, prevShotTrkr)
          if (cmpIdx > 0) then

!           trace successfully assigned, bump counter
            numTrcAssigned = numTrcAssigned + 1
            if (prevShotIdx == oldShotIdx) then
              numTrcAssignedOldShot = numTrcAssignedOldShot + 1
            end if

            if (obj%opt_debug_print) then
              write(dbmsg,*)' Assign trace ', &
              int(hdrptr(HDR_CURRENT_CHANNEL)),' trc index = ',ktrc, &
              ' of shot ',newShotIdx, &
              '(cmp ',int(hdrptr(HDR_USER_50)),',',  &
              int(hdrptr(HDR_USER_52)),')', &
              ' to shot ',prevShotIdx,' cmp index ',cmpIdx, &
              ' <---- Assign trace'
              call pc_print(dbmsg)
            end if

          else if (cmpIdx == 0) then

!           this trace matches cmp index for this composite, but it's
!           full, so go to next trace

!           trace successfully assigned, bump counter
            numTrcFull = numTrcFull + 1
            if (prevShotIdx == oldShotIdx) then
              numTrcFullOldShot = numTrcFullOldShot + 1
            end if

            if (obj%opt_debug_print) then
              write(dbmsg,*)' Assign trace ',ktrc,' of shot ', &
              newShotIdx,' to shot ',prevShotIdx,' <---- Composite full'
              call pc_print(dbmsg)
            end if
          else if (cmpIdx == -1) then

!           this trace matches cmp index for this composite, but it's
!           out of allowed offset range, so go to next trace

!           trace successfully assigned, bump counter
            numTrcOutOfRange = numTrcOutOfRange + 1
            if (prevShotIdx == oldShotIdx) then
              numTrcOutOfRangeOldShot = numTrcOutOfRangeOldShot + 1
            end if

            if (obj%opt_debug_print) then
              write(dbmsg,*)' Assign trace ',ktrc,' of shot ', &
              newShotIdx,' to shot ',prevShotIdx,' <---- Out of Range'
              call pc_print(dbmsg)
            end if
          else

!           error occurred computing insert trace index
!           call pc_error('Error in cnearts_assign_comp_trc()')
!           assignStatus = ASSIGN_STAT_ERROR
!           return

!           this trace not assigned to a previous shot
            if (obj%opt_debug_print) then
              write(dbmsg,*)' Trace ',ktrc,' of shot ', &
              newShotIdx,' not assigned <---- NOT assigned'
              call pc_print(dbmsg)
            end if

          end if

        end do

!       advance shot index to move to next shot in tracker
        prevShotIdx = cnearts_get_next_shot_idx(prevShotIdx, obj)
      end do

!     set output trace assignment status code
      if (numTrcAssignedOldShot > 0) then
        assignStatus = ASSIGN_STAT_ASSIGNED
      else 
        assignStatus = ASSIGN_STAT_FLUSH
      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_load_compositing_trcs(): leave')
      end if

    end subroutine cnearts_load_compositing_trcs


    subroutine cnearts_map_cmp_to_trc_idx(obj, numTrkrShots)
      type(cnearts_struct), intent(in) :: obj
      integer,              intent(in) :: numTrkrShots

      integer :: cmp_index_hdr_word
      integer :: ktrc, nearTrcIdx, farTrcIdx, trcIdxInc
      integer :: saveTrcIdx, startTrcIdx
      integer :: kshot, oldShotIdx, newShotIdx, shotIdx
      integer ::            pshotLowCmpIdx 
      type(shot_tracker_struct), pointer :: prevShotTrkr, newShotTrkr
      double precision,          pointer :: newhdrptr(:,:)
      character(len=200)                 :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_map_cmp_to_trc_idx(): enter')
      end if

!     define local vars as index to oldest & newest shots in obj
      oldShotIdx = obj%index_to_oldest_shot
      newShotIdx = obj%index_to_newest_shot
      newShotTrkr => obj%shot_trckr(newShotIdx)
      newhdrptr => newShotTrkr%saveShotPtr%save_trc_hdr

!     get index for trace nearest shot and increment (+ or -1) to next trace
      select case(obj%trc_1_is_near)
      case (.true.)
        nearTrcIdx = 1
        farTrcIdx = obj%shot_trckr(newShotIdx)%num_trc_trkr
        trcIdxInc = 1
      case (.false.)
        nearTrcIdx = obj%shot_trckr(newShotIdx)%num_trc_trkr
        farTrcIdx = 1
        trcIdxInc = -1
      end select

!     set location of header word with cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

      shotIdx = cnearts_get_prev_shot_idx(newShotIdx, obj)
      startTrcIdx = nearTrcIdx
      do kshot = numTrkrShots-1, 1, -1

!       point at tracker for previous shot
        prevShotTrkr => obj%shot_trckr(shotIdx)
        pshotLowCmpIdx = prevShotTrkr%cmposTrcPtr(1)%comp_cmp_index

!       initialize the map cmp index to new shot trace index component
!       of this previous tracker
        prevShotTrkr%mapCmp2Trc = 0

!       scan through traces to match their cmp index to smallest cmp index
!       of previous shot
        do ktrc = startTrcIdx, farTrcIdx, trcIdxInc
!         save this trace index
          saveTrcIdx = ktrc

          if (obj%opt_debug_print) then
            write(dbmsg,*)' testing for mapTrc2Cmp index for shotIdx = ', &
                          shotIdx,' trace ', &
                          int(newhdrptr(HDR_CURRENT_CHANNEL,ktrc)), &
                          ' trc index =',ktrc,' with cmp index = ', &
                          int(newhdrptr(cmp_index_hdr_word,ktrc)), &
                          ' vs ',pshotLowCmpIdx
            call pc_print(dbmsg)
          end if
          if (newhdrptr(cmp_index_hdr_word,ktrc) == pshotLowCmpIdx) then

!           cmp index of new shot trace matches low cmp index of current
!           previous shot. save it in map.
            prevShotTrkr%mapCmp2Trc = ktrc
            startTrcIdx = ktrc + trcIdxInc

            if (obj%opt_debug_print) then
              write(dbmsg,*)' assign mapTrc2Cmp = ', &
                            prevShotTrkr%mapCmp2Trc, &
                            ' for shotIdx = ',shotIdx, &
                            ' with cmp index = ',pshotLowCmpIdx
              call pc_print(dbmsg)
            end if

            exit    ! exit trace loop
          end if
        end do

!       check if trace loop went to last trace
        if (saveTrcIdx == farTrcIdx) then
!         for current shot, trace loop terminated without matching a cmp
!         index. this means this and all other older previous shots do
!         not have composite traces with cmp indices matching any cmp
!         indices on this new shot's traces. so we're through filling map.
          if (obj%opt_debug_print) then
            write(dbmsg,*)' no cmp index match for shot = ',shotIdx, &
                          ' or older shots'
            call pc_print(dbmsg)
          end if
          exit    ! exit the shot loop

        else

!         get actual index to next previous shot
          shotIdx = cnearts_get_prev_shot_idx(shotIdx, obj)

        end if
      end do

      if (obj%opt_debug_print) then
        call pc_print('cnearts_map_cmp_to_trc_idx(): leave')
      end if

    end subroutine cnearts_map_cmp_to_trc_idx


    subroutine cnearts_composite_oldest_shot(obj, ntr, forceComp, errorstatus)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(inout) :: ntr
      logical,              intent(in)    :: forceComp
      integer,              intent(out)   :: errorstatus

      integer :: kfill


      type(shot_tracker_struct), pointer :: prTrckr
      type(comp_trc_struct),     pointer :: prCmpPtr
      character(len=200)                 :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_composite_oldest_shot(): enter')
      end if

!     initialize default error return value
      errorstatus = 0

!     point to oldest shot in tracker
      if (obj%index_to_oldest_shot == 0) then
        if (obj%opt_debug_print) then
          call pc_print('cnearts_composite_oldest_shot(): no shots in tracker')
        end if
        ntr = NO_MORE_TRACES
        return
      end if
      prTrckr => obj%shot_trckr(obj%index_to_oldest_shot)

      if (.not. forceComp) then

!       not in force composition mode, so determine if this shot is ready
!       to be composited.

!       scan the num_fill composite trace structs and see if all are ready
!       to composite
        do kfill = 1, prTrckr%num_fill_trkr

!         point to kfill'th composite trace struct
          prCmpPtr => prTrckr%cmposTrcPtr(kfill)

!         bail out early if there are no traces here to composite
          if (prCmpPtr%comp_num_saved_trc <    &
              prCmpPtr%comp_num_total_trc - 1) then

            if (obj%opt_debug_print) then
              call pc_print('cnearts_composite_oldest_shot(): leave, no comp')
            end if
            return    ! leave subroutine now
          end if
        end do
      end if

!     if here, all missing traces have necessary compositing traces to
!     go ahead and compute the composite for each missing trace
!     OR
!     compositing is being forced because there will be no more input
!     data or there is a hole in the data forcing incomplete shots out.
      ntr = prTrckr%num_trc_trkr
      if (obj%opt_debug_print) then
        write(dbmsg,*)'    Ready to composite shot ', &
                      prTrckr%saveShotPtr%save_trc_hdr(obj%hdr_shot_index,1), &
                      ' with ntr = ',ntr, &
                      ' and num_fill = ',prTrckr%num_fill_trkr
        call pc_print(dbmsg)
      end if
      do kfill = 1, prTrckr%num_fill_trkr

!       point to kfill'th composite trace struct
        prCmpPtr => prTrckr%cmposTrcPtr(kfill)

!       now composite the traces
!       call cnearts_test_compositing(obj, prCmpPtr)
        call cnearts_composite_diffnmo_stack(obj, prCmpPtr, errorstatus)
        if (errorstatus /= 0) then
            return
        end if
        call cnearts_composite_trace_headers(obj, prCmpPtr)

!       mark this shot as ready to output
        prTrckr%ready_to_output = .true.

      end do

      if (obj%opt_debug_print) then
        write(dbmsg,*)' Composited cmp indices ', &
            prTrckr%cmposTrcPtr(1)%comp_cmp_index,' to ', &
            prTrckr%cmposTrcPtr(prTrckr%num_fill_trkr)%comp_cmp_index, &
            ' <-------- composited shot ', &
            int(prTrckr%saveShotPtr%save_trc_hdr(obj%hdr_shot_index,1))
        call pc_print(dbmsg)
      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_composite_oldest_shot(): leave')
      end if

    end subroutine cnearts_composite_oldest_shot


    subroutine cnearts_test_compositing(obj, compObj)
      type(cnearts_struct),  intent(in)    :: obj
      type(comp_trc_struct), intent(inout) :: compObj

      integer  :: ktrc, base, idx0, idx1
      integer  :: cmp_index_hdr_word

!     header of composite trace was initialized when shot was saved

!     point to header location containing cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

!     initialize composite output trace to zero
      compObj%comp_trc_smp(:,compObj%comp_num_total_trc) = 0.0

!     for test purposes, build a stair step trace, last stair being the
!     cmp index
      base = 1
      do ktrc = 1, compObj%comp_num_saved_trc
        idx0 = (base-1)*50 + 1 + 300*(ktrc-1)
        idx1 = idx0 + 50
        compObj%comp_trc_smp(idx0:idx1,compObj%comp_num_total_trc) = &
            1. + compObj%comp_trc_hdr(HDR_ORIGINAL_GROUP,ktrc)/10000.
      end do
      base = 2
      do ktrc = 1, compObj%comp_num_saved_trc
        idx0 = (base-1)*50 + 1 + 300*(ktrc-1)
        idx1 = idx0 + 50
        compObj%comp_trc_smp(idx0:idx1,compObj%comp_num_total_trc) = &
            1. + compObj%comp_trc_hdr(HDR_ORIGINAL_CHANNEL,ktrc)/1000.
      end do
      base = 3
      do ktrc = 1, compObj%comp_num_saved_trc
        idx0 = (base-1)*50 + 1 + 300*(ktrc-1)
        idx1 = idx0 + 50
        compObj%comp_trc_smp(idx0:idx1,compObj%comp_num_total_trc) = &
            1. + compObj%comp_trc_hdr(cmp_index_hdr_word,ktrc)/1000.
      end do
      base = 4
      do ktrc = 1, compObj%comp_num_saved_trc
        idx0 = (base-1)*50 + 1 + 300*(ktrc-1)
        idx1 = idx0 + 50
        compObj%comp_trc_smp(idx0:idx1,compObj%comp_num_total_trc) = &
            1. + real(compObj%comp_cmp_index)/1000.
      end do
      base = 5
      do ktrc = 1, compObj%comp_num_saved_trc
        idx0 = (base-1)*50 + 1 + 300*(ktrc-1)
        idx1 = idx0 + 50
        compObj%comp_trc_smp(idx0:idx1,compObj%comp_num_total_trc) = &
            1. + compObj%comp_trc_hdr(HDR_OFFSET,ktrc)/100000.
      end do

    end subroutine cnearts_test_compositing


    subroutine cnearts_composite_diffnmo_stack(obj, compObj, err_ret_val)
      type(cnearts_struct),  intent(inout) :: obj
      type(comp_trc_struct), intent(inout) :: compObj
      integer,               intent(out)   :: err_ret_val

      integer          :: ktrc
      double precision :: hdtmp(1:obj%nwih)
      real             :: trtmp(1:obj%ndpt)
      real             :: trsum(1:obj%ndpt)
      integer          :: errstat


!     header of composite trace was initialized when shot was saved

!     initialize default error return value
      err_ret_val = 0

!     if there are compositing traces, do the compositing
      if (compObj%comp_num_saved_trc > 0) then

!       initialize composite output trace samples to zero
        trsum(:) = 0.0

        do ktrc = 1, compObj%comp_num_saved_trc

!         move the ktrc'th header and samples to temps
          hdtmp = compObj%comp_trc_hdr(:,ktrc)
          trtmp = compObj%comp_trc_smp(:,ktrc)

!         do differential moveout on compositing traces
          call cnearts_do_diff_nmo(obj, hdtmp, trtmp, compObj%comp_nom_offset, &
                                   errstat)
          if (errstat /= 0) then
              err_ret_val = 1
              return
          end if

!         sum all of the compositing traces into the composite output trace
          trsum(:) = trsum(:) + trtmp(:)

!         if in debug outut mode, save the prestack, post moveout trace
          if (obj%opt_debug_output) then
            compObj%postmoTrcPtr%ret_trc_hdr(:,ktrc) = hdtmp(:)
            compObj%postmoTrcPtr%ret_trc_smp(:,ktrc) = trtmp(:)
          end if

        end do

!       normalize the composite output trace by the number of traces summed
        compObj%comp_trc_smp(:,compObj%comp_num_total_trc) = &
            trsum(:) * (1.0/compObj%comp_num_saved_trc)

      else

!       there are no traces to composite at this cmp index for this shot
        compObj%comp_trc_smp(:,compObj%comp_num_total_trc) = 0.0

        if (obj%opt_debug_print) then
          call pc_print(' cnearts_composite_diffnmo_stack(): no traces to &
                        &composite at ',compObj%comp_cmp_index)
        end if

      end if

    end subroutine cnearts_composite_diffnmo_stack


    subroutine cnearts_composite_trace_headers(obj, compObj)
      type(cnearts_struct),  intent(inout) :: obj
      type(comp_trc_struct), intent(inout) :: compObj

      integer :: ktrc
      double precision :: mtop, mbot

!     header of composite trace was initialized when shot was saved

!     if there are compositing traces, do the compositing of header values
      if (compObj%comp_num_saved_trc > 0) then

!       extract header info for compositing
        mtop = 999999999.
        mbot = 0.
        do ktrc = 1, compObj%comp_num_saved_trc
          mtop = min(mtop, compObj%comp_trc_hdr(HDR_TOP_MUTE,ktrc))
          mbot = max(mbot, compObj%comp_trc_hdr(HDR_BOTTOM_MUTE,ktrc))
        end do

!       insert into composite header
        compObj%comp_trc_hdr(HDR_TOP_MUTE,compObj%comp_num_total_trc) = mtop
        compObj%comp_trc_hdr(HDR_BOTTOM_MUTE,compObj%comp_num_total_trc) = mbot

      end if

    end subroutine cnearts_composite_trace_headers


    subroutine cnearts_do_diff_nmo(obj, hdtmp, trtmp, nom_offset, err_ret_val)
      type(cnearts_struct), intent(inout) :: obj
      double precision,     intent(inout) :: hdtmp(:)
      real,                 intent(inout) :: trtmp(:)
      real,                 intent(in)    :: nom_offset
      integer,              intent(out)   :: err_ret_val

      real     :: xcoord, ycoord
      real     :: offset, offnew
      integer  :: mtop, mbot
      logical  :: errstatus
      character(len=200) :: msg

!     initialize default error return status value
      err_ret_val = 0

      xcoord  = hdtmp(obj%nhx)
      ycoord  = hdtmp(obj%nhy)
      offset  = abs(hdtmp(HDR_OFFSET))
      offnew  = abs(nom_offset)
      mtop    = hdtmp(HDR_TOP_MUTE)
      mbot    = hdtmp(HDR_BOTTOM_MUTE)

      call nmoprim_apply (obj%nmoprim, xcoord, ycoord, offset, offnew, &
                          trtmp(:), mtop, mbot, errstatus, msg)
      if (errstatus) then
          err_ret_val = 1
          return
      end if

!     update header values (but not offset, it will be done in
!     subroutine cnearts_set_header_values)
      hdtmp(HDR_OFFSET) = offnew
      hdtmp(HDR_SCRATCH_32) = offnew
      hdtmp(HDR_TOP_MUTE) = mtop
      hdtmp(HDR_BOTTOM_MUTE) = mbot

    end subroutine cnearts_do_diff_nmo


    subroutine cnearts_output_ready_shot(obj, ntr, hdo, tro, err_ret_val)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(inout) :: ntr
      double precision,     intent(inout) :: hdo(:,:)
      real,                 intent(inout) :: tro(:,:)
      integer,              intent(out)   :: err_ret_val

      type(save_shot_struct), pointer :: shotPtr
      integer            :: errorstatus
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_output_ready_shot(): enter')
      end if

!     set default error status value
      err_ret_val = 0

      call cnearts_composite_oldest_shot(obj, ntr, .false., errorstatus)
      if (errorstatus /= 0) then
          err_ret_val = 1
          return
      end if
      if (ntr == NO_MORE_TRACES) then
        if (obj%opt_debug_print) then
          call pc_print('cnearts_output_ready_shot(): no more traces')
        end if
        return
      end if
      if (obj%shot_trckr(obj%index_to_oldest_shot)%ready_to_output) then

!       oldest shot is composited so copy to output arrays
        call cnearts_copy_shot_to_out_array(obj, ntr, hdo, tro)

!       shot successfully readied for output
!       NOTE: ntr is now set to number of traces in output

!       mark this tracker ready for future use
        obj%shot_trckr(obj%index_to_oldest_shot)%in_use = .false.

        if (obj%opt_debug_print) then
          write(dbmsg,*)' Shot ',int(hdo(obj%hdr_shot_index,1)), &
              ' trkr index ',obj%index_to_oldest_shot, &
              ' now ready for output <------------ shot ready'
          call pc_print(dbmsg)
        end if

!       move up index to oldest shot
        if (cnearts_get_num_shots_tracked(obj) > 1) then
!         there will still be shots in tracker after this one written
          obj%index_to_oldest_shot = &
              cnearts_get_next_shot_idx(obj%index_to_oldest_shot, obj)
        else
!         tracker will have no shots after this one written, so initialize
!         tracker indices to unused state.
          obj%index_to_oldest_shot = 0
          obj%index_to_newest_shot = 0
        end if

      else

!       oldest shot is not yet composited
        ntr = NEED_TRACES
        if (obj%opt_debug_print) then
          shotPtr => obj%shot_trckr(obj%index_to_oldest_shot)%saveShotPtr
          write(dbmsg,*)' Shot ', &
              int(shotPtr%save_trc_hdr(obj%hdr_shot_index,1)), &
              ' trkr index ',obj%index_to_oldest_shot, &
              ' NOT ready for output'
          call pc_print(dbmsg)
        end if
      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_output_ready_shot(): leave')
      end if

    end subroutine cnearts_output_ready_shot


    subroutine cnearts_flush_shots_in_memory(obj, ntr, hdo, tro, errstatus)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(inout) :: ntr
      double precision,     intent(inout) :: hdo(:,:)
      real,                 intent(inout) :: tro(:,:)
      integer,              intent(out)   :: errstatus

      integer            :: err_ret_val
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_flush_shots_in_memory(): enter')
      end if

!     set default error status
      errstatus = 0

      if (cnearts_get_num_shots_tracked(obj) > 0) then

        call cnearts_composite_oldest_shot(obj, ntr, .true., err_ret_val)
        if (err_ret_val /= 0) then
            errstatus = 1
            return
        end if
        if (ntr == NO_MORE_TRACES) then
          if (obj%opt_debug_print) then
            call pc_print('cnearts_flush_shots_in_memory(): did no &
                          &compositing, no shots in tracker')
            return
          end if
        end if

!       oldest shot is composited so copy to output arrays
        call cnearts_copy_shot_to_out_array(obj, ntr, hdo, tro)

        if (ntr == FATAL_ERROR) then

!         error occurred during copy to output arrays
          call pc_error('Copying to output arrays, &
                        &cnearts_flush_shots_in_memory()')
          errstatus = 1
          return

        else

!         shot successfully readied for output
!         ntr is now set to number of traces in output

!         mark this tracker ready for future use
          obj%shot_trckr(obj%index_to_oldest_shot)%in_use = .false.

          if (obj%opt_debug_print) then
            write(dbmsg,*)' Shot ',int(hdo(obj%hdr_shot_index,1)), &
                ' trkr index ',obj%index_to_oldest_shot, &
                ' now ready for output <------------ shot ready, flushing'
            call pc_print(dbmsg)
          end if

!         move up index to oldest shot
          if (cnearts_get_num_shots_tracked(obj) > 1) then
!           there will still be shots in tracker after this one written
            obj%index_to_oldest_shot = &
                cnearts_get_next_shot_idx(obj%index_to_oldest_shot, obj)
          else
!           tracker will have no shots after this one written, so initialize
!           tracker indices to unused state.
            obj%index_to_oldest_shot = 0
            obj%index_to_newest_shot = 0
          end if

        end if

      else

!       there are no more shots left in tracker. set ntr to NO_MORE_TRACES
!       and return

        if (obj%opt_debug_print) then
          call pc_print('cnearts_flush_shots_in_memory(): no shots in tracker')
        end if
        ntr = NO_MORE_TRACES

      end if

      if (obj%opt_debug_print) then
        call pc_print('cnearts_flush_shots_in_memory(): leave')
      end if

    end subroutine cnearts_flush_shots_in_memory


    subroutine cnearts_copy_shot_to_out_array(obj, ntro, hdo, tro)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer,              intent(out)   :: ntro               ! arguments
      double precision,     intent(inout) :: hdo(:,:)           ! arguments
      real,                 intent(inout) :: tro(:,:)           ! arguments

      integer                            :: ntr
      integer                            :: ktrc, numFill, numDead
      integer                            :: shotIndex
      integer                            :: idxCompTrc
      integer                            :: cmp_index_hdr_word
      type(shot_tracker_struct), pointer :: prTrckr
      type(comp_trc_struct),     pointer :: prCmpPtr(:)
      double precision,          pointer :: hdri(:,:)
      real,                      pointer :: smpi(:,:)
      character(len=200)                 :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_copy_shot_to_out_array(): enter')
      end if

!     initialize dead trace counter to zero (counts dead extrapolated traces)
      numDead = 0

!     point to oldest shot in tracker and its composite trace object.
!     set number of fill traces and number of total input traces.
      prTrckr => obj%shot_trckr(obj%index_to_oldest_shot)
      prCmpPtr => prTrckr%cmposTrcPtr
      numFill = prTrckr%num_fill_trkr
      ntr = prTrckr%num_trc_trkr

!     point to this shot's header and data
      hdri => prTrckr%saveShotPtr%save_trc_hdr
      smpi => prTrckr%saveShotPtr%save_trc_smp

!     point to header location containing cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

      select case (obj%trc_1_is_near)
      case (.true.)

!       copy input headers and trace samples to output arrays
        hdo(1:obj%nwih,1+numFill:ntr+numFill) = hdri(1:obj%nwih,1:ntr)
        tro(1:obj%ndpt,1+numFill:ntr+numFill) = smpi(1:obj%ndpt,1:ntr)

        if (obj%opt_debug_print) then
          write(dbmsg,*)' Near input trace 1 cmp index = ', &
              int(hdri(cmp_index_hdr_word,1))
          call pc_print(dbmsg)
        end if

!       copy composited headers and trace samples to output arrays.
!       NOTE: prCmpPtr(1) has the smallest cmp index, so it comes
!             immediately before input trace 1, which is now at trace
!             count numFill+1.
        do ktrc = 1, numFill
          idxCompTrc = prCmpPtr(numFill+1-ktrc)%comp_num_total_trc
          hdo(1:obj%nwih,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(1:obj%nwih,idxCompTrc)
          tro(1:obj%ndpt,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_smp(1:obj%ndpt,idxCompTrc)

          if (obj%opt_debug_print) then
            write(dbmsg,*)' Copy fill trace ',ktrc,' cmp index = ', &
                prCmpPtr(numFill+1-ktrc)%comp_cmp_index
            call pc_print(dbmsg)
          end if

!         count dead traces in composited output traces
          if (prCmpPtr(numFill+1-ktrc)%comp_num_saved_trc == 0) then
            numDead = numDead + 1
          end if

!         set cmp index and all HDR_MIDPOINT header values
          hdo(cmp_index_hdr_word,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_cmp_index
          hdo(HDR_MIDPOINT_XLOC,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(HDR_MIDPOINT_XLOC,1)
          hdo(HDR_MIDPOINT_YLOC,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(HDR_MIDPOINT_YLOC,1)
          hdo(HDR_MIDPOINT_ELEV,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(HDR_MIDPOINT_ELEV,1)
          hdo(HDR_MIDPOINT_XGRID,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(HDR_MIDPOINT_XGRID,1)
          hdo(HDR_MIDPOINT_YGRID,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(HDR_MIDPOINT_YGRID,1)
          hdo(HDR_MIDPOINT_SHOTPOINT,ktrc) = &
              prCmpPtr(numFill+1-ktrc)%comp_trc_hdr(HDR_MIDPOINT_ELEV,1)

        end do

        if (obj%opt_debug_output) then
          call cnearts_output_auxtrc_tr1isnear(obj, cmp_index_hdr_word, &
                                               prTrckr, hdo, tro)
        end if

!       get shot index
        shotIndex = prTrckr%saveShotPtr%save_trc_hdr(obj%hdr_shot_index,1)

!       update amplitude info in header for the new traces
        call lav_set_hdr (hdo(1:obj%nwih,1:numFill), &
                          tro(1:obj%ndpt,1:numFill), &
                          obj%ndpt, numFill)

      case (.false.)

!       copy input headers and trace samples to output arrays
        hdo(1:obj%nwih,1:ntr) = hdri(1:obj%nwih,1:ntr)
        tro(1:obj%ndpt,1:ntr) = smpi(1:obj%ndpt,1:ntr)

        if (obj%opt_debug_print) then
          write(dbmsg,*)' Near input trace ',ntr,' cmp index = ', &
              int(hdri(cmp_index_hdr_word,ntr))
          call pc_print(dbmsg)
        end if

!       copy composited headers and trace samples to output arrays
!       NOTE: prCmpPtr(1) has the smallest cmp index, so it comes
!             immediately after input trace ntr, which is now at trace
!             count ntr.
        do ktrc = 1, numFill
          idxCompTrc = prCmpPtr(ktrc)%comp_num_total_trc
          hdo(1:obj%nwih,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(1:obj%nwih,idxCompTrc)
          tro(1:obj%ndpt,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_smp(1:obj%ndpt,idxCompTrc)

!         count dead traces in composited output traces
          if (prCmpPtr(ktrc)%comp_num_saved_trc == 0) then
            numDead = numDead + 1
          end if

!         set cmp index and all HDR_MIDPOINT header values
          hdo(cmp_index_hdr_word,ntr+ktrc) = prCmpPtr(ktrc)%comp_cmp_index
          hdo(HDR_MIDPOINT_XLOC,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(HDR_MIDPOINT_XLOC,1)
          hdo(HDR_MIDPOINT_YLOC,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(HDR_MIDPOINT_YLOC,1)
          hdo(HDR_MIDPOINT_ELEV,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(HDR_MIDPOINT_ELEV,1)
          hdo(HDR_MIDPOINT_XGRID,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(HDR_MIDPOINT_XGRID,1)
          hdo(HDR_MIDPOINT_YGRID,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(HDR_MIDPOINT_YGRID,1)
          hdo(HDR_MIDPOINT_SHOTPOINT,ntr+ktrc) = &
              prCmpPtr(ktrc)%comp_trc_hdr(HDR_MIDPOINT_ELEV,1)

        end do

        if (obj%opt_debug_output) then
          call cnearts_output_auxtrc_tr1isfar(obj, cmp_index_hdr_word, &
                                              prTrckr, hdo, tro)
        end if

!       get shot index
        shotIndex = prTrckr%saveShotPtr%save_trc_hdr(obj%hdr_shot_index,ntr)

!       update amplitude info in header for the new traces
        call lav_set_hdr (hdo(1:obj%nwih,ntr+1:ntr+numFill), &
                          tro(1:obj%ndpt,ntr+1:ntr+numFill), &
                          obj%ndpt, numFill)

        if (obj%opt_debug_print) then
          write(dbmsg,*)' Copy fill traces 1 to ',numFill, &
              ' cmp index = ',prCmpPtr(1)%comp_cmp_index,' to ', &
              prCmpPtr(numFill)%comp_cmp_index
          call pc_print(dbmsg)
        end if

      end select

!     set header values in new headers
      call cnearts_set_header_values(obj, ntr, numFill, hdo)

!     set new number of output traces
      if (obj%opt_debug_output) then
        ntro = ntr + numFill*(2*(obj%num_trc_to_stack + 1) + 1)
      else
        ntro = ntr + numFill
      end if

!     print out info about this shot
      write(dbmsg,*)' ---->CNEARTS shot ',shotIndex,' ready to output: ', &
                    ntr,' input traces, ',numFill,' fill traces, ', &
                    numDead,' dead fill traces'
      call pc_print(dbmsg)

      if (obj%opt_debug_print) then
        call pc_print('cnearts_copy_shot_to_out_array(): leave, ntro = ',ntro)
      end if

    end subroutine cnearts_copy_shot_to_out_array


    subroutine cnearts_output_auxtrc_tr1isnear(obj, cmp_index_hdr_word,&
                                              prTrckr, hdo, tro)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer,              intent(in)    :: cmp_index_hdr_word ! arguments
      type(shot_tracker_struct), intent(in)    :: prTrckr
      double precision,     intent(inout) :: hdo(:,:)           ! arguments
      real,                 intent(inout) :: tro(:,:)           ! arguments

      integer                            :: ntr, numFill, nextra
      integer                            :: idxFillTrc, ktrc
      integer                            :: insertIdx, idxCompTrc
      type(comp_trc_struct),     pointer :: prCmpPtr(:)
      real                               :: deadtrc(obj%ndpt)
      character(len=200)                 :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_output_auxtrc_tr1isnear(): enter')
      end if

!     set number of fill traces and number of total input traces.
      prCmpPtr => prTrckr%cmposTrcPtr
      numFill = prTrckr%num_fill_trkr
      ntr = prTrckr%num_trc_trkr

!     move the output header and sample arrays up far enough
!     to allow space for the auxiliary debug output traces
      nextra = numFill*2*(obj%num_trc_to_stack + 1)
      do ktrc = ntr+numFill, 1, -1
        hdo(1:obj%nwih,ktrc+nextra) = hdo(1:obj%nwih,ktrc)
        tro(1:obj%ndpt,ktrc+nextra) = tro(1:obj%ndpt,ktrc)
      end do

!     build dead trace
      deadtrc(:) = 0.0

      do idxFillTrc = numFill, 1, -1

!       get index for composited trace (the num_total_trc'th
!       trace in the composite structure)
        idxCompTrc = prCmpPtr(idxFillTrc)%comp_num_total_trc

!       set starting insert point
        insertIdx = 1 + (numFill - idxFillTrc)*(2*(obj%num_trc_to_stack + 1))

!       insert pre-moveout traces
        hdo(1:obj%nwih,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%comp_trc_hdr(1:obj%nwih,1:obj%num_trc_to_stack)
        tro(1:obj%ndpt,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%comp_trc_smp(1:obj%ndpt,1:obj%num_trc_to_stack)
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert  pre-moveout traces at index = ', &
                        insertIdx,' to ',insertIdx-1+obj%num_trc_to_stack
          call pc_print(dbmsg)
        end if

!       bump insert point
        insertIdx = insertIdx + obj%num_trc_to_stack

!       insert dead trace with header from fill trace
        hdo(1:obj%nwih,insertIdx) = &
            prCmpPtr(idxFillTrc)%comp_trc_hdr(1:obj%nwih,idxCompTrc)
        hdo(cmp_index_hdr_word,insertIdx) = 0
        tro(1:obj%ndpt,insertIdx) = deadtrc
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert dead trace at index = ',insertIdx
          call pc_print(dbmsg)
        end if

!       bump insert point
        insertIdx = insertIdx + 1

!       insert post-moveout traces
        hdo(1:obj%nwih,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%postmoTrcPtr%   &
            ret_trc_hdr(1:obj%nwih,1:obj%num_trc_to_stack)
        tro(1:obj%ndpt,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%postmoTrcPtr%   &
            ret_trc_smp(1:obj%ndpt,1:obj%num_trc_to_stack)
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert post-moveout traces at index = ', &
                        insertIdx,' to ',insertIdx-1+obj%num_trc_to_stack
          call pc_print(dbmsg)
        end if

!       bump insert point
        insertIdx = insertIdx + obj%num_trc_to_stack

!       insert dead trace with header from fill trace
        hdo(1:obj%nwih,insertIdx) = &
            prCmpPtr(idxFillTrc)%comp_trc_hdr(1:obj%nwih,idxCompTrc)
        hdo(cmp_index_hdr_word,insertIdx) = 0
        tro(1:obj%ndpt,insertIdx) = deadtrc
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert dead trace at index = ',insertIdx
          call pc_print(dbmsg)
        end if

      end do

      if (obj%opt_debug_print) then
        call pc_print('cnearts_output_auxtrc_tr1isnear(): leave')
      end if

    end subroutine cnearts_output_auxtrc_tr1isnear


    subroutine cnearts_output_auxtrc_tr1isfar(obj, cmp_index_hdr_word,&
                                              prTrckr, hdo, tro)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer,              intent(in)    :: cmp_index_hdr_word ! arguments
      type(shot_tracker_struct), intent(in)    :: prTrckr
      double precision,     intent(inout) :: hdo(:,:)           ! arguments
      real,                 intent(inout) :: tro(:,:)           ! arguments

      integer                            :: ntr, numFill
      integer                            :: idxFillTrc
      integer                            :: insertIdx, idxCompTrc
      type(comp_trc_struct),     pointer :: prCmpPtr(:)
      real                               :: deadtrc(obj%ndpt)
      character(len=200)                 :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_output_auxtrc_tr1isfar(): enter')
      end if

!     set number of fill traces and number of total input traces.
      prCmpPtr => prTrckr%cmposTrcPtr
      numFill = prTrckr%num_fill_trkr
      ntr = prTrckr%num_trc_trkr

!     build dead trace
      deadtrc(:) = 0.0

      do idxFillTrc = 1, numFill

!       get index for composited trace (the num_total_trc'th
!       trace in the composite structure)
        idxCompTrc = prCmpPtr(idxFillTrc)%comp_num_total_trc

!       set starting insert point
        insertIdx = ntr + numFill + 1 + &
                    (idxFillTrc - 1)*(2*(obj%num_trc_to_stack + 1))

!       insert dead trace with header from fill trace
        hdo(1:obj%nwih,insertIdx) = &
            prCmpPtr(idxFillTrc)%comp_trc_hdr(1:obj%nwih,idxCompTrc)
        hdo(cmp_index_hdr_word,insertIdx) = 0
        tro(1:obj%ndpt,insertIdx) = deadtrc
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert dead trace at index = ',insertIdx
          call pc_print(dbmsg)
        end if

!       bump insert point
        insertIdx = insertIdx + 1

!       insert pre-moveout traces
        hdo(1:obj%nwih,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%comp_trc_hdr(1:obj%nwih,1:obj%num_trc_to_stack)
        tro(1:obj%ndpt,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%comp_trc_smp(1:obj%ndpt,1:obj%num_trc_to_stack)
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert  pre-moveout traces at index = ', &
                        insertIdx,' to ',insertIdx-1+obj%num_trc_to_stack
          call pc_print(dbmsg)
        end if

!       bump insert point
        insertIdx = insertIdx + obj%num_trc_to_stack

!       insert dead trace with header from fill trace
        hdo(1:obj%nwih,insertIdx) = &
            prCmpPtr(idxFillTrc)%comp_trc_hdr(1:obj%nwih,idxCompTrc)
        hdo(cmp_index_hdr_word,insertIdx) = 0
        tro(1:obj%ndpt,insertIdx) = deadtrc
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert dead trace at index = ',insertIdx
          call pc_print(dbmsg)
        end if

!       insert post-moveout traces
        insertIdx = insertIdx + 1
        hdo(1:obj%nwih,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%postmoTrcPtr%   &
            ret_trc_hdr(1:obj%nwih,1:obj%num_trc_to_stack)
        tro(1:obj%ndpt,insertIdx:insertIdx-1+obj%num_trc_to_stack) = &
            prCmpPtr(idxFillTrc)%postmoTrcPtr%   &
            ret_trc_smp(1:obj%ndpt,1:obj%num_trc_to_stack)
        if (obj%opt_debug_print) then
          write(dbmsg,*)' Insert post-moveout traces at index = ', &
                        insertIdx,' to ',insertIdx-1+obj%num_trc_to_stack
          call pc_print(dbmsg)
        end if

      end do

      if (obj%opt_debug_print) then
        call pc_print('cnearts_output_auxtrc_tr1isfar(): leave')
      end if

    end subroutine cnearts_output_auxtrc_tr1isfar


!!------------------------------ headers -----------------------------------!!
!!------------------------------ headers -----------------------------------!!
!!------------------------------ headers -----------------------------------!!


    subroutine cnearts_set_header_values(obj, ntr, numfill, hdo)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer             , intent(in)    :: ntr                ! arguments
      integer             , intent(in)    :: numfill            ! arguments
      double precision   ,  intent(inout) :: hdo(:,:)           ! arguments

!     local variables
      integer             :: k
      double precision    :: offset_from_near_trc

      select case (obj%trc_1_is_near)
      case (.true.)

        do k = 1, numfill

!         set original channel number for created traces to 0
          hdo(HDR_ORIGINAL_CHANNEL,1+numfill-k) = 0.0

!         compute offset, giving result the sign of the original near trace
          offset_from_near_trc = k * obj%rcv_grp_intrval
          hdo(HDR_OFFSET,1+numfill-k) = hdo(HDR_OFFSET,1+numfill) - &
              sign(offset_from_near_trc, hdo(HDR_OFFSET,1+numfill))


!         set header flag that this is a cnearts created trace
          hdo(HDR_USER_48,1+numfill-k) = flag_cnearts_trace

        end do

!       for now, copy receiver location and grid from just the nearest actual
!       trace
!TODO put in real values here eventually
        hdo(HDR_RECEIVER_XLOC,1:numfill) = &
                hdo(HDR_RECEIVER_XLOC,1+numfill)
        hdo(HDR_RECEIVER_YLOC,1:numfill) = &
                hdo(HDR_RECEIVER_YLOC,1+numfill)
        hdo(HDR_RECEIVER_XGRID,1:numfill) = &
                hdo(HDR_RECEIVER_XGRID,1+numfill)
        hdo(HDR_RECEIVER_YGRID,1:numfill) = &
                hdo(HDR_RECEIVER_YGRID,1+numfill)

      case (.false.)

        do k = 1, numfill

!         set original channel number for created traces to 0
          hdo(HDR_ORIGINAL_CHANNEL,ntr+k) = 0.0

!         compute offset, giving result the sign of the original near trace
          offset_from_near_trc = k * obj%rcv_grp_intrval
          hdo(HDR_OFFSET,ntr+k) = hdo(HDR_OFFSET,ntr) - &
              sign(offset_from_near_trc, hdo(HDR_OFFSET,ntr))

!         set header flag that this is a cnearts created trace
          hdo(HDR_USER_48,ntr+k) = flag_cnearts_trace

        end do

!       for now, copy receiver location and grid from just the nearest actual
!       trace
!TODO put in real values here eventually
        hdo(HDR_RECEIVER_XLOC,ntr+1:ntr+numfill) = &
                hdo(HDR_RECEIVER_XLOC,ntr)
        hdo(HDR_RECEIVER_YLOC,ntr+1:ntr+numfill) = &
                hdo(HDR_RECEIVER_YLOC,ntr)
        hdo(HDR_RECEIVER_XGRID,ntr+1:ntr+numfill) = &
                hdo(HDR_RECEIVER_XGRID,ntr)
        hdo(HDR_RECEIVER_YGRID,ntr+1:ntr+numfill) = &
                hdo(HDR_RECEIVER_YGRID,ntr)

      end select

!     now set the HDR_SEQUENCE entry for all trace headers. also renumber
!     HDR_CURRENT_CHANNEL to begin at 1
      do k = 1, ntr+numfill
        hdo(HDR_SEQUENCE,k) = obj%save_seq_count + k
        hdo(HDR_CURRENT_CHANNEL,k) = k
      end do
      obj%save_seq_count = obj%save_seq_count + ntr+numfill

    end subroutine cnearts_set_header_values


!!------------------------- memory allocation ------------------------------!!
!!------------------------- memory allocation ------------------------------!!
!!------------------------- memory allocation ------------------------------!!


    subroutine cnearts_shot_tracker_cnstrct(obj, numfill, ierr)
      type(cnearts_struct), intent(inout) :: obj
      integer,              intent(in)    :: numfill
      integer,              intent(out)   :: ierr

      integer      :: kshot   

      allocate(obj%shot_trckr(obj%max_trckr_index), stat=ierr)

      if (ierr == 0) then

!        now allocate the structs that will hold the traces for compositing
         do kshot = 1, obj%max_trckr_index

!          initialize scalars and pointers
           obj%shot_trckr(kshot)%in_use = .false.
           obj%shot_trckr(kshot)%ready_to_output = .false.
           obj%shot_trckr(kshot)%num_fill_trkr = numfill
           obj%shot_trckr(kshot)%num_trc_trkr = 0
           nullify(obj%shot_trckr(kshot)%cmposTrcPtr)
           nullify(obj%shot_trckr(kshot)%saveShotPtr)

           call cnearts_comp_trc_array_cnstrct(obj, numfill, &
                    obj%shot_trckr(kshot)%cmposTrcPtr, ierr)

!          allocate  saveShotPtr object to hold saved shot traces
           allocate(obj%shot_trckr(kshot)%saveShotPtr, stat=ierr)
           if (ierr /= 0) then
             call pc_error('Failed to allocate save shot object&
                           &, shot tracker ', kshot)
             return
           end if

!          now allocate space for save shot headers and samples
           call cnearts_save_trc_cnstrct(obj, &
                  obj%shot_trckr(kshot)%saveShotPtr, ierr)
           if (ierr == 0) then
             obj%shot_trckr(kshot)%num_trc_trkr = obj%max_num_trc_gather
           else
             call pc_error('Failed to allocate memory for save shot traces&
                           &, shot tracker ', kshot)
             return
           end if
         end do
      end if

      return

    end subroutine cnearts_shot_tracker_cnstrct


    subroutine cnearts_comp_trc_array_cnstrct(obj, num_fill, cmposTrcPtr, ierr)

      type(cnearts_struct), intent(in)  :: obj
      integer,              intent(in)  :: num_fill
      type(comp_trc_struct),   pointer  :: cmposTrcPtr(:)
      integer,              intent(out) :: ierr

      integer :: kfilltrc

!     allocate num_fill objects to hold compositing traces
      allocate(cmposTrcPtr(num_fill), stat=ierr)
      if (ierr /= 0) then
        call pc_error('Failed to allocate composite trace object ')
        ierr = 1
        return
      end if

!     now allocate space for compositing headers and samples
      do kfilltrc = 1, num_fill
        call cnearts_comp_trc_cnstrct(obj, cmposTrcPtr(kfilltrc), ierr)
        if (ierr /= 0) then
          call pc_error('Failed to allocate memory for composite trace ', &
                        kfilltrc, ', shot tracker ')
          ierr = 2
          return
        end if
      end do

      ierr = 0
      return

    end subroutine cnearts_comp_trc_array_cnstrct


    subroutine cnearts_comp_trc_cnstrct(obj, compTrcObj, ierr)
      type(cnearts_struct),  intent(in)    :: obj
      type(comp_trc_struct), intent(inout) :: compTrcObj
      integer,               intent(out)   :: ierr

!     nullify pointers in cmposTrcPtr object
      nullify(compTrcObj%comp_trc_hdr)
      nullify(compTrcObj%comp_trc_smp)
      nullify(compTrcObj%postmoTrcPtr)

!     get memory for trace header
      allocate(compTrcObj%comp_trc_hdr(obj%nwih, 1+obj%num_trc_to_stack),&
               stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for composite object &
                      &headers failed')
        return
      end if

!     get memory for trace samples
      allocate(compTrcObj%comp_trc_smp(obj%ndpt, 1+obj%num_trc_to_stack), &
               stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for composite object &
                      &samples failed')
        return
      end if

!     if required, get memory for post moveout data
      if (obj%opt_debug_output) then
        allocate(compTrcObj%postmoTrcPtr, stat=ierr)
        call cnearts_postmo_trc_cnstrct(obj, compTrcObj%postmoTrcPtr, ierr)
        if (ierr /= 0) then
          call pc_error('Failed to allocate memory for post moveout &
                        &trace object')
          return
        end if
      end if

!     successfully got memory for compositing traces. set scalars.
      compTrcObj%comp_num_total_trc = obj%num_trc_to_stack + 1
      compTrcObj%comp_num_saved_trc = 0
      compTrcObj%comp_nom_offset = 0.0

    end subroutine cnearts_comp_trc_cnstrct


    subroutine cnearts_postmo_trc_cnstrct(obj, postmoTrcObj, ierr)
      type(cnearts_struct),    intent(in)    :: obj
      type(retain_trc_struct), intent(inout) :: postmoTrcObj
      integer,                 intent(out)   :: ierr

!     nullify pointers in cmposTrcPtr object
      nullify(postmoTrcObj%ret_trc_hdr)
      nullify(postmoTrcObj%ret_trc_smp)

!     get memory for trace header
      allocate(postmoTrcObj%ret_trc_hdr(obj%nwih, 1+obj%num_trc_to_stack),&
               stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for post moveout object &
                      &headers failed')
        return
      end if

!     get memory for trace samples
      allocate(postmoTrcObj%ret_trc_smp(obj%ndpt, 1+obj%num_trc_to_stack),&
               stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for post moveout object &
                      &samples failed')
        return
      end if


!     successfully got memory for post move out traces. set scalar.
      postmoTrcObj%ret_saved_trc = obj%num_trc_to_stack

    end subroutine cnearts_postmo_trc_cnstrct


    subroutine cnearts_save_trc_cnstrct(obj, saveTrcObj, ierr)
      type(cnearts_struct),   intent(in)    :: obj
      type(save_shot_struct), intent(inout) :: saveTrcObj
      integer,                intent(out)   :: ierr

!     local variables
      integer :: num_trc

!     set local var to max number of traces allowed per input gather
      num_trc = obj%max_num_trc_gather

!     nullify pointers in saveTrcObj objects
      nullify(saveTrcObj%save_trc_hdr)
      nullify(saveTrcObj%save_trc_smp)

!     get memory for trace header
      allocate(saveTrcObj%save_trc_hdr(obj%nwih, num_trc), stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for saved shot headers failed')
        return
      end if

!     get memory for trace samples
      allocate(saveTrcObj%save_trc_smp(obj%ndpt, num_trc), stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for saved shot samples failed')
        return
      end if

    end subroutine cnearts_save_trc_cnstrct


    subroutine cnearts_retain_trc_cnstrct(obj, retainTrcObj, ierr)
      type(cnearts_struct),    intent(in)  :: obj
      type(retain_trc_struct), pointer     :: retainTrcObj
      integer,                 intent(out) :: ierr

!     local variables
      integer :: num_trc

!     allocate the retain_trc_struct object
      allocate(retainTrcObj, stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for retained trace object failed')
        return
      end if

!     set local var to max number of traces allowed per input gather
      num_trc = obj%max_num_trc_gather

!     initialize trace count
      retainTrcObj%ret_saved_trc = 0

!     nullify pointers in retainTrcObj objects
      nullify(retainTrcObj%ret_trc_hdr)
      nullify(retainTrcObj%ret_trc_smp)

!     get memory for trace header
      allocate(retainTrcObj%ret_trc_hdr(obj%nwih, num_trc), stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for retained shot headers failed')
        return
      end if

!     get memory for trace samples
      allocate(retainTrcObj%ret_trc_smp(obj%ndpt, num_trc), stat=ierr)
      if (ierr /= 0) then
!       allocation failed
        call pc_error('Memory allocation for retained shot samples failed')
        return
      end if

    end subroutine cnearts_retain_trc_cnstrct


    subroutine cnearts_shot_tracker_destrct(shot_trckr, numtrckr)
      type(shot_tracker_struct), pointer :: shot_trckr(:)
      integer,                intent(in) :: numtrckr

      integer :: k, numfill

      if ( associated (shot_trckr)) then
        do k = 1, numtrckr
          numfill = shot_trckr(k)%num_fill_trkr
          if (associated(shot_trckr(k)%cmposTrcPtr)) then
            call cnearts_comp_trc_array_destrct(shot_trckr(k)%cmposTrcPtr, &
                                                numfill)
          end if
          if (associated(shot_trckr(k)%saveShotPtr)) then
            call cnearts_save_trc_destrct(shot_trckr(k)%saveShotPtr)
          end if
        end do
        deallocate(shot_trckr)
        nullify(shot_trckr)
      end if

    end subroutine cnearts_shot_tracker_destrct


    subroutine cnearts_comp_trc_array_destrct(compTrc, numfill)
      type(comp_trc_struct), pointer :: compTrc(:)
      integer,            intent(in) :: numfill

      integer :: k

      if (associated (compTrc) ) then
        do k = 1, numfill
          call cnearts_comp_trc_destrct(compTrc(k))
        end do
        deallocate(compTrc)
        nullify(compTrc)
      end if
      
    end subroutine cnearts_comp_trc_array_destrct


    subroutine cnearts_comp_trc_destrct(compTrcObj)
      type(comp_trc_struct), intent(inout) :: compTrcObj

      if ( associated (compTrcObj%comp_trc_hdr) ) then
        deallocate(compTrcObj%comp_trc_hdr)
        nullify(compTrcObj%comp_trc_hdr)
      end if
      if (associated(compTrcObj%comp_trc_smp)) then
        deallocate(compTrcObj%comp_trc_smp)
        nullify(compTrcObj%comp_trc_smp)
      end if
      if (associated(compTrcObj%postmoTrcPtr)) then
        call cnearts_postmo_trc_destrct(compTrcObj%postmoTrcPtr)
      end if

    end subroutine cnearts_comp_trc_destrct


    subroutine cnearts_postmo_trc_destrct(postmoTrc)
      type(retain_trc_struct), pointer :: postmoTrc

      if (associated (postmoTrc) ) then
        if (associated (postmoTrc%ret_trc_hdr) ) then
          deallocate(postmoTrc%ret_trc_hdr)
          nullify(postmoTrc%ret_trc_hdr)
        end if
        if (associated(postmoTrc%ret_trc_smp)) then
          deallocate(postmoTrc%ret_trc_smp)
          nullify(postmoTrc%ret_trc_smp)
        end if
        deallocate(postmoTrc)
        nullify(postmoTrc)
      end if

    end subroutine cnearts_postmo_trc_destrct


    subroutine cnearts_save_trc_destrct(saveTrcPtr)
      type(save_shot_struct), pointer :: saveTrcPtr

      if (associated(saveTrcPtr)) then
        if (associated (saveTrcPtr%save_trc_hdr)) then
          deallocate(saveTrcPtr%save_trc_hdr)
          nullify(saveTrcPtr%save_trc_hdr)
        end if
        if (associated(saveTrcPtr%save_trc_smp)) then
          deallocate(saveTrcPtr%save_trc_smp)
          nullify(saveTrcPtr%save_trc_smp)
        end if
        deallocate(saveTrcPtr)
        nullify(saveTrcPtr)
      end if
      
    end subroutine cnearts_save_trc_destrct


    subroutine cnearts_retain_trc_destrct(retainTrcPtr)
      type(retain_trc_struct), pointer :: retainTrcPtr

      if (associated(retainTrcPtr)) then
        if (associated (retainTrcPtr%ret_trc_hdr)) then
          deallocate(retainTrcPtr%ret_trc_hdr)
          nullify(retainTrcPtr%ret_trc_hdr)
        end if
        if (associated(retainTrcPtr%ret_trc_smp)) then
          deallocate(retainTrcPtr%ret_trc_smp)
          nullify(retainTrcPtr%ret_trc_smp)
        end if
        deallocate(retainTrcPtr)
        nullify(retainTrcPtr)
      end if
      
    end subroutine cnearts_retain_trc_destrct


!!----------------------------- functions ----------------------------------!!
!!----------------------------- functions ----------------------------------!!
!!----------------------------- functions ----------------------------------!!


    function cnearts_point2point_dist(pt2x, pt2y, pt1x, pt1y)
      double precision, intent(in)  :: pt2x, pt2y, pt1x, pt1y
      real                          :: cnearts_point2point_dist

      cnearts_point2point_dist = sqrt((pt2x - pt1x)**2 + (pt2y - pt1y)**2)

    end function cnearts_point2point_dist

    function cnearts_get_near_offset(num_tr, trc_hdr, near_trc_is_1)
      integer         , intent(in)  :: num_tr          ! arguments
      double precision, intent(in)  :: trc_hdr(:,:)    ! arguments
      logical         , intent(in)  :: near_trc_is_1   ! arguments
      real                          :: cnearts_get_near_offset

      integer :: neartrc

!     set index to nearest trace
      if (near_trc_is_1) then
        neartrc = 1
      else
        neartrc = num_tr
      end if

!     compute offset to nearest trace
      cnearts_get_near_offset = cnearts_get_srcrcvr_dist(trc_hdr(:,neartrc))

    end function cnearts_get_near_offset

    function cnearts_is_trace_1_near(num_tr, trc_hdr)
      integer         , intent(in)  :: num_tr          ! arguments
      double precision, intent(in)  :: trc_hdr(:,:)    ! arguments
      logical                       :: cnearts_is_trace_1_near

      real :: offset1, offset2

!     get distance from source to receiver for first and last trace using
!     the headers for each
      offset1 = cnearts_get_srcrcvr_dist(trc_hdr(:,1))
      offset2 = cnearts_get_srcrcvr_dist(trc_hdr(:,num_tr))

!     set return true if trace 1 is nearest to the source
      if (offset1 < offset2) then
        cnearts_is_trace_1_near = .true.
      else
        cnearts_is_trace_1_near = .false.
      end if

    end function cnearts_is_trace_1_near

    function cnearts_get_srcrcvr_dist(trchdr)
      double precision    , intent(in) :: trchdr(:)    ! arguments
      real                             :: cnearts_get_srcrcvr_dist

      cnearts_get_srcrcvr_dist = sqrt( &
        (trchdr(HDR_SOURCE_XLOC) - trchdr(HDR_RECEIVER_XLOC))**2 + &
        (trchdr(HDR_SOURCE_YLOC) - trchdr(HDR_RECEIVER_YLOC))**2 )

    end function cnearts_get_srcrcvr_dist

    function cnearts_compute_numfill(src_nearrcvr_offset, rcvr_grp_int)
      real, intent(in)  :: src_nearrcvr_offset, rcvr_grp_int
      integer           :: cnearts_compute_numfill

      integer :: m

      m = src_nearrcvr_offset/rcvr_grp_int
      if (abs(src_nearrcvr_offset - m*rcvr_grp_int) <= &
          abs(src_nearrcvr_offset - (m+1)*rcvr_grp_int)) then
        cnearts_compute_numfill = m
      else
        cnearts_compute_numfill = m + 1
      end if

    end function cnearts_compute_numfill

    function cnearts_shot_grp_advance(deltaSrc, deltaRcvr)
      real,                 intent(in)    :: deltaSrc, deltaRcvr
      integer                             :: cnearts_shot_grp_advance

      real :: temp             

!     compute how many receiver interval (deltaRcvr) were moved in the
!     last source move (deltaSrc) and round to nearest integer, not
!     truncate
      temp = int(deltaSrc/deltaRcvr)
      if (abs(deltaSrc-(temp+1)*deltaRcvr) < abs(deltaSrc-temp*deltaRcvr)) then
        cnearts_shot_grp_advance = temp + 1
      else
        cnearts_shot_grp_advance = temp
      end if

    end function cnearts_shot_grp_advance


    function cnearts_get_new_tracker_index(obj)
      type(cnearts_struct), intent(in) :: obj
      integer                          :: cnearts_get_new_tracker_index

      integer :: k, shotIdx

      if (obj%opt_debug_print) then
        call pc_print('cnearts_get_new_tracker_index(): enter')
      end if

!     check shot tracker for slot to save this shot. start with index to
!     newest and search up (modulo obj%max_trckr_index) to next empty spot.
      shotIdx = obj%index_to_newest_shot

!     scan over all trackers
      if (obj%opt_debug_print) then
        call pc_print(' obj%index_to_oldest_shot = ',obj%index_to_oldest_shot)
        call pc_print(' obj%max_trckr_index = ',obj%max_trckr_index)
      end if
      do k = 1, obj%max_trckr_index

!       get next shot index in tracker
        shotIdx = cnearts_get_next_shot_idx(shotIdx, obj)
        if (obj%opt_debug_print) then
          call pc_print('  checking tracker at index = ',shotIdx)
        end if

!       check to see if this is an empty tracker
        if (.not. obj%shot_trckr(shotIdx)%in_use) then
          cnearts_get_new_tracker_index = shotIdx
          if (obj%opt_debug_print) then
            call pc_print('  found tracker at index = ',shotIdx)
            call pc_print('cnearts_get_new_tracker_index(): leave')
          end if
          return
        end if
      end do

!     if here, didn't find an empty tracker, so signal error
      cnearts_get_new_tracker_index = 0

      if (obj%opt_debug_print) then
        call pc_print('  did not find a tracker <------------ tracker error')
        call pc_print('cnearts_get_new_tracker_index(): leave')
      end if

    end function cnearts_get_new_tracker_index


    function cnearts_get_new_tracker_index0(obj)
      type(cnearts_struct), intent(in) :: obj
      integer                          :: cnearts_get_new_tracker_index0

      integer :: k, kmodulo

!     check shot tracker for slot to save this shot. start with index to
!     oldest and search up (modulo obj%max_trckr_index) to next empty spot.

!     scan over all trackers
      do k = 0, obj%max_trckr_index - 1

!       kmodulo will go from obj%index_to_oldest_shot up, modulo 
!       obj%max_trckr_index
        kmodulo = mod(k-1+obj%index_to_oldest_shot, obj%max_trckr_index) + 1

!       check for empty tracker
        if (.not. obj%shot_trckr(kmodulo)%in_use) then
          cnearts_get_new_tracker_index0 = kmodulo
          return
        end if
      end do

!     if here, didn't find an empty tracker, so signal error
      cnearts_get_new_tracker_index0 = 0

    end function cnearts_get_new_tracker_index0


    function cnearts_get_prev_shot_idx(currShotIdx, obj)
      integer,              intent(in) :: currShotIdx
      type(cnearts_struct), intent(in) :: obj
      integer                          :: cnearts_get_prev_shot_idx

      cnearts_get_prev_shot_idx = currShotIdx - 1
      if (cnearts_get_prev_shot_idx <= 0) then
!       wrap around to end of shot tracker array
        cnearts_get_prev_shot_idx = obj%max_trckr_index
      end if
      
    end function cnearts_get_prev_shot_idx


    function cnearts_get_next_shot_idx(currShotIdx, obj)
      integer,              intent(in) :: currShotIdx
      type(cnearts_struct), intent(in) :: obj
      integer                          :: cnearts_get_next_shot_idx

      cnearts_get_next_shot_idx = currShotIdx + 1
      if (cnearts_get_next_shot_idx > obj%max_trckr_index) then
!       wrap around to beginning of shot tracker array
        cnearts_get_next_shot_idx = 1
      end if
      
    end function cnearts_get_next_shot_idx


    function cnearts_assign_comp_trc(obj, hdrptr, smpptr, prTrckr)
      type(cnearts_struct), intent(in)   :: obj
      double precision,          pointer :: hdrptr(:)
      real,                      pointer :: smpptr(:)
      type(shot_tracker_struct), pointer :: prTrckr
      integer                            :: cmp_index_hdr_word
      integer                            :: cnearts_assign_comp_trc

      integer :: k, nextCompIdx
      type(comp_trc_struct), pointer :: prCmpPtr
      character(len=200) :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_assign_comp_trc(): enter')
      end if

!     point to header location containing cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

      cnearts_assign_comp_trc = -2
      do k = 1, prTrckr%num_fill_trkr
        prCmpPtr => prTrckr%cmposTrcPtr(k)
        if (hdrptr(cmp_index_hdr_word) == prCmpPtr%comp_cmp_index) then
!         this trace cmp index matches that for this composite buffer

          if (prCmpPtr%comp_num_saved_trc ==    &
              prCmpPtr%comp_num_total_trc - 1) then

!           composite trace buffer full (the num_total_trc'th trace is
!           for the composite trace)
            cnearts_assign_comp_trc = 0
            if (obj%opt_debug_print) then
              write(dbmsg,*)' New trace ', &
                  int(hdrptr(HDR_CURRENT_CHANNEL)), &
                  ' cmp index = ', &
                  int(hdrptr(cmp_index_hdr_word)),&
                  ' matches comp obj index ',prCmpPtr%comp_cmp_index, &
                  ' but buffer full'
              call pc_print(dbmsg)
            end if
            exit    ! through, exit loop

          else if (abs(hdrptr(HDR_OFFSET)) > prCmpPtr%comp_nom_offset + &
                   obj%rcv_grp_intrval * obj%src_rcvr_sep_limit) then

!           the candidate trace for compositing is offset too far from
!           the trace to be created
            cnearts_assign_comp_trc = -1
            if (obj%opt_debug_print) then
              write(dbmsg,*)' New trace ', &
                  int(hdrptr(HDR_CURRENT_CHANNEL)), &
                  ' cmp index = ', &
                  int(hdrptr(cmp_index_hdr_word)),&
                  ' matches comp obj index ',prCmpPtr%comp_cmp_index, &
                  ' but offset ',abs(hdrptr(HDR_OFFSET)),' outside range ', &
                  prCmpPtr%comp_nom_offset + &
                  obj%rcv_grp_intrval * obj%src_rcvr_sep_limit
              call pc_print(dbmsg)
            end if
            exit    ! through, exit loop

          else

!           there is room in this composite buffer for this trace

            nextCompIdx = prTrckr%cmposTrcPtr(k)%comp_num_saved_trc + 1
            prTrckr%cmposTrcPtr(k)%comp_trc_hdr(:,nextCompIdx) = hdrptr(:)
            prTrckr%cmposTrcPtr(k)%comp_trc_smp(:,nextCompIdx) = smpptr(:)
            prTrckr%cmposTrcPtr(k)%comp_num_saved_trc = nextCompIdx
            if (obj%opt_debug_print) then
              write(dbmsg,*)' New trace ', &
                  int(hdrptr(HDR_CURRENT_CHANNEL)),' cmp index = ', &
                  int(hdrptr(cmp_index_hdr_word)),&
                  ' matches comp obj index ',prCmpPtr%comp_cmp_index, &
                  ' and offset ',abs(hdrptr(HDR_OFFSET)),' inside range ', &
                      prCmpPtr%comp_nom_offset + &
                      obj%rcv_grp_intrval * obj%src_rcvr_sep_limit
              call pc_print(dbmsg)
            end if

            cnearts_assign_comp_trc = prTrckr%cmposTrcPtr(k)%comp_cmp_index
            exit    ! through, exit loop

          end if
        else

          if (obj%opt_debug_print) then
            write(dbmsg,*)' New trace cmp index = ', &
                int(hdrptr(cmp_index_hdr_word)),&
                ' does not match comp obj index ',prCmpPtr%comp_cmp_index
            call pc_print(dbmsg)
          end if

        end if
      end do

      if (obj%opt_debug_print) then
        call pc_print('cnearts_assign_comp_trc(): leave')
      end if

    end function cnearts_assign_comp_trc


    function cnearts_insert_comp_trc(obj, hdrptr, smpptr, prTrckr)
      type(cnearts_struct), intent(in)   :: obj
      double precision,          pointer :: hdrptr(:)
      real,                      pointer :: smpptr(:)
      type(shot_tracker_struct), pointer :: prTrckr
      integer                            :: cnearts_insert_comp_trc

      integer :: k, nextCompIdx
      integer :: cmp_index_hdr_word
      type(comp_trc_struct), pointer :: prCmpPtr
      character(len=200)             :: dbmsg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_insert_comp_trc(): enter')
      end if

!     point to header location containing cmp index
      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

      cnearts_insert_comp_trc = -1
      do k = 1, prTrckr%num_fill_trkr
        prCmpPtr => prTrckr%cmposTrcPtr(k)
        if (hdrptr(cmp_index_hdr_word) == prCmpPtr%comp_cmp_index) then
!         this trace cmp index matches that for this composite buffer

          if (prCmpPtr%comp_num_saved_trc ==    &
              prCmpPtr%comp_num_total_trc - 1 .or. &
              abs(hdrptr(HDR_OFFSET)) > prCmpPtr%comp_nom_offset + &
              obj%rcv_grp_intrval * obj%src_rcvr_sep_limit) then

!           composite trace buffer full (the num_total_trc'th trace is
!           for the composite trace)
!             OR
!           the candidate trace for compositing is offset too far from
!           the trace to be created
            cnearts_insert_comp_trc = 0
            if (obj%opt_debug_print) then
              if (prCmpPtr%comp_num_saved_trc ==    &
                  prCmpPtr%comp_num_total_trc - 1) then
                write(dbmsg,*)' New trace cmp index = ', &
                    int(hdrptr(cmp_index_hdr_word)),&
                    ' matches comp obj index ',prCmpPtr%comp_cmp_index, &
                    ' but buffer full'
              else
                if (abs(hdrptr(HDR_OFFSET)) > prCmpPtr%comp_nom_offset + &
                    obj%rcv_grp_intrval * obj%src_rcvr_sep_limit) then
                  write(dbmsg,*)' New trace cmp index = ', &
                      int(hdrptr(cmp_index_hdr_word)),&
                      ' matches comp obj index ',prCmpPtr%comp_cmp_index, &
                      ' but offset ',abs(hdrptr(HDR_OFFSET)), &
                      ' outside range ',prCmpPtr%comp_nom_offset + &
                      obj%rcv_grp_intrval * obj%src_rcvr_sep_limit
                end if
              end if
              call pc_print(dbmsg)
            end if
            exit    ! through, exit loop

          else

!           there is room in this composite buffer for this trace

            nextCompIdx = prTrckr%cmposTrcPtr(k)%comp_num_saved_trc + 1
            prTrckr%cmposTrcPtr(k)%comp_trc_hdr(:,nextCompIdx) = hdrptr(:)
            prTrckr%cmposTrcPtr(k)%comp_trc_smp(:,nextCompIdx) = smpptr(:)
            prTrckr%cmposTrcPtr(k)%comp_num_saved_trc = nextCompIdx
            if (obj%opt_debug_print) then
              write(dbmsg,*)' New trace cmp index = ', &
                  int(hdrptr(cmp_index_hdr_word)),&
                  ' matches comp obj index ',prCmpPtr%comp_cmp_index, &
                  ' and offset ',abs(hdrptr(HDR_OFFSET)),' inside range ', &
                      prCmpPtr%comp_nom_offset + &
                      obj%rcv_grp_intrval * obj%src_rcvr_sep_limit
              call pc_print(dbmsg)
            end if

            cnearts_insert_comp_trc = prTrckr%cmposTrcPtr(k)%comp_cmp_index
            exit    ! through, exit loop

          end if
        else

!         if (obj%opt_debug_print) then
!           write(dbmsg,*)' New trace cmp index = ', &
!               int(hdrptr(cmp_index_hdr_word)),&
!               ' does not match comp obj index ',prCmpPtr%cmp_index
!           call pc_print(dbmsg)
!         end if

        end if
      end do

      if (obj%opt_debug_print) then
        call pc_print('cnearts_insert_comp_trc(): leave')
      end if

    end function cnearts_insert_comp_trc


    function cnearts_get_num_shots_tracked(obj)
      type(cnearts_struct), intent(in) :: obj
      integer                          :: cnearts_get_num_shots_tracked

      if (obj%index_to_newest_shot == 0) then
!       if obj%index_to_newest_shot is 0, there are no shots in tracker
        cnearts_get_num_shots_tracked = 0
      else if (obj%index_to_newest_shot >= obj%index_to_oldest_shot) then
!       oldest shot index is below newest shot index in tracker, so
!       take difference to get number of shots in tracker
        cnearts_get_num_shots_tracked = obj%index_to_newest_shot - &
            obj%index_to_oldest_shot + 1
      else
!       oldest shot index is above newest shot index in tracker, so
!       number of shots in tracker is total minus the number of indices
!       between the old and new index (and not including the old and
!       new indices)
        cnearts_get_num_shots_tracked = obj%max_trckr_index + &
            obj%index_to_newest_shot -  obj%index_to_oldest_shot + 1
      end if

    end function cnearts_get_num_shots_tracked


    function cnearts_check_cmp_increment(delcmpindex)
      double precision, intent(in) :: delcmpindex
      logical                      :: cnearts_check_cmp_increment

      integer          :: int_delta
      double precision :: dp_delta

      int_delta = delcmpindex
      dp_delta = int_delta

      if ((dp_delta == delcmpindex) .and. &
          (int_delta == 1 .or. int_delta == -1)) then 
        cnearts_check_cmp_increment = .true.
      else
        cnearts_check_cmp_increment = .false.
      end if

    end function cnearts_check_cmp_increment


    function cnearts_has_cable_changed(obj, hdi)
      type(cnearts_struct), intent(in)    :: obj                ! arguments
      double precision   ,  intent(inout) :: hdi(:,:)           ! arguments

      logical                      :: cnearts_has_cable_changed

      integer :: hdrindex
      type(save_shot_struct), pointer:: newestTrackedShot
      character(len=200)             :: dbmsg

!     check if there are any shots in tracker. if not, return now with false.
      if (obj%index_to_newest_shot == 0) then
        cnearts_has_cable_changed = .false.
        return
      end if

!     set index to input cable (line) number
      hdrindex = obj%hdr_cable_num_loc

!     point at new shot in shot tracker. this would be the shot entered
!     on the most recent call to cnearts before this current call.
      newestTrackedShot => obj%shot_trckr(obj%index_to_newest_shot)%saveShotPtr

      if (hdi(hdrindex,1) == newestTrackedShot%save_trc_hdr(hdrindex,1)) then
!       still on the same cable
        cnearts_has_cable_changed = .false.
        if (obj%opt_debug_print) then
          write(dbmsg,*)' New shot on same cable number = ', &
                        int(hdi(hdrindex,1))
          call pc_print(dbmsg)
        end if
      else
!       this new shot on different cable than previous
        cnearts_has_cable_changed = .true.
        if (obj%opt_debug_print) then
          write(dbmsg,*)' New shot on new cable number = ', &
                        int(hdi(hdrindex,1)),' old cable number = ' &
                        ,int(newestTrackedShot%save_trc_hdr(hdrindex,1)), &
                        '  <------------ Cable transition detected'
          call pc_print(dbmsg)
        end if
      end if

    end function cnearts_has_cable_changed


    function cnearts_data_sanity_check(obj, ntr, hdi)
      type(cnearts_struct), intent(in)    :: obj          ! arguments
      integer,              intent(in)    :: ntr          ! arguments
      double precision   ,  intent(inout) :: hdi(:,:)     ! arguments

      logical                      :: cnearts_data_sanity_check

      cnearts_data_sanity_check = .true.  ! means data passes check

      if (ntr > obj%max_num_trc_gather) then
        call pc_error('CNEARTS received gather number ',nint(hdi(3,1)), &
                      ' larger than MAX_NUM_TRC_GATHER = ', &
                      obj%max_num_trc_gather)
        cnearts_data_sanity_check = .false.  ! fails
      end if
      if (ntr == 1) then
        call pc_error('CNEARTS received a gather with 1 trace')
        cnearts_data_sanity_check = .false.  ! fails
      end if

    end function cnearts_data_sanity_check


!!----------------------------- assignment ---------------------------------!!
!!----------------------------- assignment ---------------------------------!!
!!----------------------------- assignment ---------------------------------!!

    subroutine cnearts_assign_default_value(obj, dflt_obj)
      type(cnearts_struct),         intent(inout) :: obj
      type(cnearts_default_struct), intent(in)    :: dflt_obj

!     do assignments from default object to parameter object
      obj%num_trc_to_stack     = dflt_obj%num_trc_to_stack
      obj%max_num_trc_to_stack = dflt_obj%max_num_trc_to_stack
      obj%min_num_trc_to_stack = dflt_obj%min_num_trc_to_stack
      obj%max_num_trc_gather   = dflt_obj%max_num_trc_gather
      obj%max_near_offset      = dflt_obj%max_near_offset
      obj%delta_source         = dflt_obj%delta_source
      obj%src_rcvr_sep_limit   = dflt_obj%src_rcvr_sep_limit
      obj%gather_hdr_word_num  = dflt_obj%gather_hdr_word_num
      obj%hdr_cable_num_loc    = dflt_obj%hdr_cable_num_loc
      obj%hdr_shot_index       = dflt_obj%hdr_shot_index
      obj%delta_shot_index     = dflt_obj%delta_shot_index
      obj%hdr_cmp_index        = dflt_obj%hdr_cmp_index
      obj%rcv_grp_intrval      = dflt_obj%rcv_grp_intrval
      obj%opt_debug_print      = dflt_obj%opt_debug_print
      obj%opt_debug_output     = dflt_obj%opt_debug_output
      obj%opt_compute_cmpidx   = dflt_obj%opt_compute_cmpidx
      obj%opt_trc_extrap       = dflt_obj%opt_trc_extrap
      obj%max_trckr_index      = dflt_obj%max_trckr_index
      obj%index_to_oldest_shot = dflt_obj%index_to_oldest_shot
      obj%index_to_newest_shot = dflt_obj%index_to_newest_shot
      obj%abort_on_trckr_error = dflt_obj%abort_on_trckr_error
      obj%doppler              = dflt_obj%doppler
      obj%order_mo             = dflt_obj%order_mo
      obj%opt_nmo              = dflt_obj%opt_nmo
      obj%opt_interp           = dflt_obj%opt_interp
      obj%opt_nmo_res          = dflt_obj%opt_nmo_res
      obj%vel_bias             = dflt_obj%vel_bias
      obj%vel_scale            = dflt_obj%vel_scale

!     do other default assignments
      obj%path_vel             = PATHCHECK_EMPTY
      obj%firstShotThisCable   = .false.
      obj%secondShotThisCable  = .false.
      obj%num_fill_1st         = -1
      obj%cumulative_delta_src = 0.0
  
      return
  
    end subroutine cnearts_assign_default_value


!!--------------------------- miscellaneous --------------------------------!!
!!--------------------------- miscellaneous --------------------------------!!
!!--------------------------- miscellaneous --------------------------------!!


    subroutine cnearts_print_trace_info(hdr, trcnum)
      double precision, intent(in) :: hdr(:,:)
      integer,          intent(in) :: trcnum

!     print stats for trace "trcnum" from trace header array "hdr"

      call pc_print('  Trace ',trcnum,':')
      call pc_print('    Current channel: ',hdr(HDR_CURRENT_CHANNEL,trcnum))
      call pc_print('    Original channel: ',hdr(HDR_ORIGINAL_CHANNEL,trcnum))
      call pc_print('    Offset: ',abs(hdr(HDR_OFFSET,trcnum)))
      call pc_print('    Source X:   ',hdr(HDR_SOURCE_XLOC,trcnum))
      call pc_print('    Source Y:   ',hdr(HDR_SOURCE_YLOC,trcnum))
      call pc_print('    Receiver X: ',hdr(HDR_RECEIVER_XLOC,trcnum))
      call pc_print('    Receiver Y: ',hdr(HDR_RECEIVER_YLOC,trcnum))

    end subroutine cnearts_print_trace_info


    subroutine cnearts_compute_cmp_index(ntrc, deltaSrc, obj, hdr)
      integer,              intent(in)    :: ntrc
      real,                 intent(in)    :: deltaSrc
      type(cnearts_struct), intent(inout) :: obj
      double precision,     intent(inout) :: hdr(:,:)

      integer          :: k
      real             :: deltaRcvr
      double precision :: slope_dbg, intercept_dbg, k_dbg

      if (obj%opt_debug_print) then
        call pc_print('cnearts_compute_cmp_index(): enter')
      end if

!     this routine assumes that deltaSrc has been set to 0 for first shot.

!     the first midpoint index on a line is dflt_lastCmpStartIndex

!     get nominal spacing between adjacent receivers
      deltaRcvr = obj%rcv_grp_intrval

!     compute adjustment to starting cmp index for this shot. this is
!     based on the number of receiver group intervals current shot has
!     moved since last shot. the adjustment is double that because the
!     cmp interval is half the receiver interval, so the cmp index
!     advances 2 for each receiver group advance between shot positions.
      obj%lastCmpStartIndex = obj%lastCmpStartIndex + &
                       2 * cnearts_shot_grp_advance(deltaSrc, deltaRcvr)

!     debug: testing new cmp index computation
      slope_dbg = 1./deltaRcvr
      intercept_dbg = real(ntrc) + slope_dbg*(obj%cumulative_delta_src + &
                      obj%near_offset_this_shot)

!     this occurs at start up, for the first shot in

      select case(obj%trc_1_is_near)
      case (.true.)

        do k = ntrc, 1, -1

!         put ideal cmp index in HDR_USER_50
          hdr(HDR_USER_50,k) = obj%lastCmpStartIndex + ntrc - k

!         put computed cmp index in HDR_USER_51 and rounded value
!         in HDR_USER_52
          k_dbg = obj%cumulative_delta_src - obj%near_offset_this_shot -  &
                  real((k - 1))*deltaRcvr
          hdr(HDR_USER_51,k) = slope_dbg*k_dbg + intercept_dbg
          hdr(HDR_USER_52,k) = dnint(hdr(HDR_USER_51,k))

!         copy rounded, computed value to HDR_SCRATCH_30
          hdr(HDR_SCRATCH_30,k) = hdr(HDR_USER_52,k)  ! rounded, computed cmp
!         hdr(HDR_SCRATCH_30,k) = hdr(HDR_USER_50,k)  ! ideal cmp

!         save cumulative source distance divided by the ideal
!         source moveup distance in HDR_SCRATCH_62
          hdr(HDR_SCRATCH_62,k) = &
              obj%cumulative_delta_src/(obj%delta_source*obj%rcv_grp_intrval)

!         compute a group number based on initial shot
!         number, the delta between adjacent shot numbers, and the
!         distance between this shot and the previous shot
          hdr(HDR_SCRATCH_31,k) = obj%shot_num_1st_online + &
              real(obj%delta_shot_index) * dnint(hdr(HDR_SCRATCH_62,k))
        end do

      case (.false.)

        do k = 1, ntrc

!         put ideal cmp index in HDR_USER_50
          hdr(HDR_USER_50,k) = obj%lastCmpStartIndex + k - 1

!         put computed cmp index in HDR_USER_51 and rounded value
!         in HDR_USER_52
          k_dbg = obj%cumulative_delta_src - obj%near_offset_this_shot -  &
                  real((ntrc - k))*deltaRcvr
          hdr(HDR_USER_51,k) = slope_dbg*k_dbg + intercept_dbg
          hdr(HDR_USER_52,k) = dnint(hdr(HDR_USER_51,k))

!         copy rounded, computed value to HDR_SCRATCH_30
          hdr(HDR_SCRATCH_30,k) = hdr(HDR_USER_52,k)  ! rounded, computed cmp
!         hdr(HDR_SCRATCH_30,k) = hdr(HDR_USER_50,k)  ! ideal cmp

!         save cumulative source distance divided by the ideal
!         source moveup distance in HDR_SCRATCH_62
          hdr(HDR_SCRATCH_62,k) = &
              obj%cumulative_delta_src/(obj%delta_source*obj%rcv_grp_intrval)

!         compute a group number based on initial shot
!         number, the delta between adjacent shot numbers, and the
!         distance between this shot and the previous shot
          hdr(HDR_SCRATCH_31,k) = obj%shot_num_1st_online + &
              real(obj%delta_shot_index) * dnint(hdr(HDR_SCRATCH_62,k))
        end do
      end select

      if (obj%opt_debug_print) then
        call pc_print('cnearts_compute_cmp_index(): leave')
      end if

    end subroutine cnearts_compute_cmp_index


    subroutine cnearts_print_cmp_index_info(obj, ntrc, hdr, deltaSrc)
      type(cnearts_struct), intent(in) :: obj
      integer,              intent(in) :: ntrc
      double precision,     intent(in) :: hdr(:,:)
      real,                 intent(in) :: deltaSrc

      integer            :: cmp_index_hdr_word
      integer            :: startIndexCMP, finalIndexCMP
      real               :: near_offset
      character(len=200) :: dbmsg

      if (obj%opt_compute_cmpidx) then
        cmp_index_hdr_word = HDR_SCRATCH_30
      else
        cmp_index_hdr_word = obj%hdr_cmp_index
      end if

      select case(obj%trc_1_is_near)
      case (.true.)
        startIndexCMP = hdr(cmp_index_hdr_word,ntrc)
        finalIndexCMP = hdr(cmp_index_hdr_word,1)
      case (.false.)
        startIndexCMP = hdr(cmp_index_hdr_word,1)
        finalIndexCMP = hdr(cmp_index_hdr_word,ntrc)
      end select
      near_offset = cnearts_get_near_offset(ntrc, hdr, obj%trc_1_is_near)
      write(dbmsg,*)'Source ',int(hdr(obj%hdr_shot_index,1)),  &
        'near offset=',obj%near_offset_this_shot,' delS=',deltaSrc,' is ',  &
        cnearts_shot_grp_advance(deltaSrc,obj%rcv_grp_intrval),  &
        ' groups; cmp start=',startIndexCMP,' end=',finalIndexCMP
      call pc_print(dbmsg)

    end subroutine cnearts_print_cmp_index_info

    subroutine cnearts_init_default_struct(dfltobj)
      type(cnearts_default_struct), intent(inout) :: dfltobj

!     because fortran 90 doesn't allow initialization of elements of
!     a user defined type, do that here in a subroutine 

      dfltobj%opt_debug_print     = dflt_opt_debug_print
      dfltobj%opt_debug_output    = dflt_opt_debug_output
      dfltobj%opt_compute_cmpidx  = dflt_opt_compute_cmpidx
      dfltobj%num_trc_to_stack    = dflt_num_trc_to_stack
      dfltobj%max_num_trc_to_stack= dflt_max_num_trc_to_stack
      dfltobj%min_num_trc_to_stack= dflt_min_num_trc_to_stack
      dfltobj%max_num_trc_gather  = dflt_max_num_trc_gather
      dfltobj%max_near_offset     = dflt_max_near_offset
      dfltobj%delta_source        = dflt_delta_source
      dfltobj%src_rcvr_sep_limit  = dflt_src_rcvr_sep_limit
      dfltobj%gather_hdr_word_num = dflt_gather_hdr_word_num
      dfltobj%hdr_cable_num_loc   = dflt_hdr_cable_num_loc
      dfltobj%hdr_shot_index      = dflt_hdr_shot_index
      dfltobj%delta_shot_index    = dflt_delta_shot_index
      dfltobj%hdr_cmp_index       = dflt_hdr_cmp_index
      dfltobj%lastCmpStartIndex   = dflt_lastCmpStartIndex
      dfltobj%rcv_grp_intrval     = dflt_rcv_grp_intrval
      dfltobj%opt_trc_extrap      = dflt_opt_trc_extrap

      dfltobj%max_trckr_index     = dflt_max_trckr_index
      dfltobj%index_to_oldest_shot= dflt_index_to_oldest_shot
      dfltobj%index_to_newest_shot= dflt_index_to_newest_shot
      dfltobj%abort_on_trckr_error= dflt_abort_on_trckr_error

      dfltobj%opt_nmo             = dflt_opt_nmo
      dfltobj%opt_interp          = dflt_opt_interp
      dfltobj%opt_nmo_res         = dflt_opt_nmo_res
      dfltobj%vel_bias            = dflt_vel_bias
      dfltobj%vel_scale           = dflt_vel_scale
      dfltobj%doppler             = dflt_doppler
      dfltobj%order_mo            = dflt_order_mo

    end subroutine cnearts_init_default_struct


    subroutine cnearts_do_first_shot_setup(obj, ntr, hdi, tri, deltaRcvr, &
                                           srcPrevX, srcPrevY)
      type(cnearts_struct), intent(inout) :: obj                ! arguments
      integer,              intent(inout) :: ntr                ! arguments
      double precision,     intent(inout) :: hdi(:,:)           ! arguments
      real,                 intent(inout) :: tri(:,:)           ! arguments
      real,                 intent(inout) :: deltaRcvr          ! arguments
      double precision,     intent(inout) :: srcPrevX, srcPrevY

      integer     ::             error_status 
      integer     :: previous_num_fill, previous_max_trckr_idx

      if (obj%opt_debug_print) then
        call pc_print('cnearts_do_first_shot_setup(): enter')
      end if

!     compute number of traces to fill based on first shot and nearest
!     receiver

!     determine if trace 1 is nearest trace to source
      obj%trc_1_is_near = cnearts_is_trace_1_near(ntr, hdi)

!     get offset from source to nearest receiver
      obj%near_offset_this_shot = cnearts_get_near_offset(ntr, hdi, &
                                                          obj%trc_1_is_near)
      if (obj%near_offset_this_shot <= 0.0) then
        if (obj%opt_debug_print) then
          call pc_print('cnearts_do_first_shot_setup(): leave')
        end if
        call pc_error('Source to near receiver has negative or zero offset')
        ntr = FATAL_ERROR
        call cnearts_wrapup(obj)
        return
      end if

!     initialize cumulative source distance to zero
      obj%cumulative_delta_src = 0.0

!     compute difference between the first two receivers' original channel
!     numbers. repeat for current channel number.
      select case (obj%trc_1_is_near)
      case (.true.)

        obj%del_orig_chan = hdi(HDR_ORIGINAL_CHANNEL,2) - &
                            hdi(HDR_ORIGINAL_CHANNEL,1)
        obj%del_curr_chan = hdi(HDR_CURRENT_CHANNEL,2) - &
                            hdi(HDR_CURRENT_CHANNEL,1)
      case (.false.)

        obj%del_orig_chan = hdi(HDR_ORIGINAL_CHANNEL,ntr) - &
                            hdi(HDR_ORIGINAL_CHANNEL,ntr-1)
        obj%del_curr_chan = hdi(HDR_CURRENT_CHANNEL,ntr) - &
                            hdi(HDR_CURRENT_CHANNEL,ntr-1)
      end select

!     set starting value for cmp index (only if computing the index)
!     and the delta for the cmp index
      if (obj%opt_compute_cmpidx) then
        obj%lastCmpStartIndex = default_obj%lastCmpStartIndex
        obj%del_cmp_index = 1.0
      else
        select case (obj%trc_1_is_near)
        case (.true.)
          obj%del_cmp_index = hdi(obj%hdr_cmp_index,1) - &
                                hdi(obj%hdr_cmp_index,2)
        case (.false.)
          obj%del_cmp_index = hdi(obj%hdr_cmp_index,ntr) - &
                                hdi(obj%hdr_cmp_index,ntr-1)
        end select
      end if

!     check for valid obj%del_cmp_index (1 or -1, not 0, integer)
      if (.not. cnearts_check_cmp_increment(obj%del_cmp_index)) then

!       failed
        call pc_error('User specified header word ', obj%hdr_cmp_index)
        call pc_error(' for cmp index results in cmp index increment of ', &
                      obj%del_cmp_index)
        if (obj%opt_debug_print) then
          call pc_print('cnearts_do_first_shot_setup(): leave')
        end if
        call pc_error(' instead of 1 or -1 and integer.')
        ntr = FATAL_ERROR
        call cnearts_wrapup(obj)
        return
        
      end if

!     compute the number of traces to be filled for this shot
      previous_num_fill = obj%num_fill_1st
      obj%num_fill_1st = cnearts_compute_numfill(obj%near_offset_this_shot, &
                                             obj%rcv_grp_intrval)
      if (obj%num_fill_1st > obj%max_num_fill_trc) then
        if (obj%opt_debug_print) then
          call pc_print('cnearts_do_first_shot_setup(): leave')
        end if
        call pc_error('Number of fill traces ', obj%num_fill_1st, &
                      ' exceeds specified maximum ', obj%max_num_fill_trc)
        ntr = FATAL_ERROR
        call cnearts_wrapup(obj)
        return
      end if

!     set max size of shot tracker array. the factors representing how this
!     number is determined are:
!         max_num_fill_trc/(2*delta_source) + 1: this represents
!             the number of shots it takes for successive cables to work
!             across the gap to be filled, at two cmp's per group interval
!             between consecutive shots
!         num_trc_to_stack: each additional shot past those for the first
!             factor provides one additional trace for summing
!         1:  the first shot doesn't contribute any traces to any other shot
      previous_max_trckr_idx = obj%max_trckr_index
      obj%max_trckr_index = 2*(obj%max_num_fill_trc/(2.0*obj%delta_source) + &
                            1 + obj%num_trc_to_stack + 1)

      if (string_upper_compare(obj%opt_trc_extrap, &
                               c_opt_trc_extrap(1))) then

!       in "Stack Nearest by Midpoint" mode, allocate memory
!       for the shot tracker objects
        if (associated(obj%shot_trckr)) then
          if (obj%num_fill_1st /= previous_num_fill) then
            if (obj%opt_debug_print) then
              call pc_print('cnearts_do_first_shot_setup(): destroy tracker &
                            &for num_fill = ',previous_num_fill)
            end if
            call cnearts_shot_tracker_destrct(obj%shot_trckr, &
                                              previous_max_trckr_idx)
          end if
        end if
        if (obj%opt_debug_print) then
          call pc_print('cnearts_do_first_shot_setup(): create new tracker &
                        &for num_fill = ',obj%num_fill_1st)
        end if
        call cnearts_shot_tracker_cnstrct(obj, obj%num_fill_1st, error_status)
        if (error_status /= 0) then
          if (obj%opt_debug_print) then
            call pc_print('cnearts_do_first_shot_setup(): leave')
          end if
          call pc_error('Failed allocation of shot tracker objects')
          ntr = FATAL_ERROR
          call cnearts_wrapup(obj)
          return
        end if

      end if

!     compute actual separation from adjacent receivers
      deltaRcvr = cnearts_point2point_dist( &
                      hdi(HDR_RECEIVER_XLOC,2), hdi(HDR_RECEIVER_YLOC,2), &
                      hdi(HDR_RECEIVER_XLOC,1), hdi(HDR_RECEIVER_YLOC,1))

      call pc_print('')
      call pc_print('CNEARTS Summary:')
      call pc_print('  Input Data: first record ',hdi(obj%hdr_shot_index,1))
      if (obj%opt_debug_print) then
        call cnearts_print_trace_info(hdi, 1)
        call cnearts_print_trace_info(hdi, ntr)
      end if
      call pc_print('')
      call pc_print('  Src-to-near rcvr distance: ',obj%near_offset_this_shot)
      call pc_print('  Receiver group interval: ',obj%rcv_grp_intrval)
      call pc_print('  Max near offset: ',obj%max_near_offset)
      call pc_print('  Max number fill traces: ',obj%max_num_fill_trc)
      call pc_print('  Actual receiver group interval: ',deltaRcvr)
      call pc_print('  Number fill traces, first shot: ',obj%num_fill_1st)
      if (obj%trc_1_is_near) then
        call pc_print('  First trace is the near trace')
      else
        call pc_print('  First trace is NOT the near trace')
      end if
      call pc_print('  Max number of trackers: ',obj%max_trckr_index)
      call pc_print('  Original channel number delta: ',obj%del_orig_chan)
      call pc_print('  Current channel number delta: ',obj%del_curr_chan)
      call pc_print('  Header word for cmp index: ',obj%hdr_cmp_index)
      call pc_print('  Increment for cmp index: ',int(obj%del_cmp_index))
      call pc_print('  Cable #: ',int(hdi(obj%hdr_cable_num_loc,1)))
      call pc_print('  Cable # header location: ',obj%hdr_cable_num_loc)
      call pc_print('  Shot index header location: ',obj%hdr_shot_index)
      call pc_print('  Delta shot index: ',obj%delta_shot_index)
      call pc_print('  Number words in header: ',obj%nwih)
      call pc_print('End CNEARTS Summary:')
      call pc_print('')

!     save this first shot's shot number
      obj%shot_num_1st_online = hdi(obj%hdr_shot_index,1)

!     save source location for use in next shot
      srcPrevX = hdi(HDR_SOURCE_XLOC,1)
      srcPrevY = hdi(HDR_SOURCE_YLOC,1)

!     enable second entry flag, but DO NOT disable the firstShotThisCable
!     flag. That will be done at the end of the secondShotThisCable if
!     block. The firstShotThisCable flag needs to be true throughout
!     the processing steps for the first shot.
      obj%secondShotThisCable = .true.

      if (obj%opt_debug_print) then
        call pc_print('cnearts_do_first_shot_setup(): leave')
      end if

    end subroutine cnearts_do_first_shot_setup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


  end module cnearts_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

