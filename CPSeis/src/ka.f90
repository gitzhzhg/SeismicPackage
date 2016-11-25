!<CPS_v1 type="PROCESS"/>
!!----------------------------- ka.f90 ---------------------------------!!
!!----------------------------- ka.f90 ---------------------------------!!
!!----------------------------- ka.f90 ---------------------------------!!


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
! Name       : ka                               (Kombo Analysis)
! Category   : migrations
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2007-01-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Analyze and edit a combo of input data volumes for tomography.
! Portability: No known limitations, but see note below.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process prepares data for tomography by analyzing and editing a
! combination of 3D data input volumes which are in trace format.
!
! Possible input and output traces at each CMP are the following:
!
!   input   81  The residual NMO velocity ratio       (BETA attribute).
!   input   88  The structural dip in the X direction (XDIP attribute).
!   input   89  The structural dip in the Y direction (YDIP attribute).
!   input   82  The RMS amplitude stacked trace for this CMP.
!   input   83  The maximum absolute amplitude stacked trace for this CMP.
!   input   71  The semblance calculated along the residual NMO curve.
!   input   90  The semblance calculated along the structural dip.
!   input   51  The input average velocity.
!   output  91  The weight volume                (calculated by this process).
!   output  84  The GAMMA volume                 (calculated by this process).
!   output  85  The output average velocity         (edited average velocity).
!   output 181  The residual NMO velocity ratio       (edited BETA attribute).
!   output 188  The structural dip in the X direction (edited XDIP attribute).
!   output 189  The structural dip in the Y direction (edited YDIP attribute).
!
!    Header word HDR_IDENT identifies the type of information each trace
!    contains.  These header word values are listed above.
!
!    In this documentation the BETA or XDIP or YDIP trace (or the data volume
!    in which this trace resides) is called the ATTRIBUTE trace or the
!    ATTRIBUTE volume.
!
!    The output ATTRIBUTE trace might contain nil values unless
!    the nil values are replaced before or after smoothing.
!
!    The output weight volume might contain nil or zero values.
!
!    The output average velocity volume will never contain nil values or
!    values <= zero.
!
!    Not all input traces are required; only those which are needed to
!    prepare the requested output traces are needed.  The GUI lists the
!    required input traces dynamically depending on the current state
!    of the process parameters.
!
! This process performs the following steps in the indicated order:
!
!    (1) Calculates the weight volume to use for editing and smoothing.
!    (2) Applies amplitude and semblance threshholds to the ATTRIBUTE volume.
!    (3) Optionally flattens the weight and ATTRIBUTE volumes.
!    (4) Removes anomalous values from the ATTRIBUTE volume.
!    (5) Optionally replaces nil values in the ATTRIBUTE volume.
!    (6) Smooths the ATTRIBUTE volume.
!    (7) Optionally replaces nil values in the ATTRIBUTE volume.
!    (8) Unflattens the weight and ATTRIBUTE volumes if they had been flattened.
!  Also performs the following steps on BETA and VAV (if both are input):
!    (9) Edits the input average velocities using the edited BETA volume.
!   (10) Applies velocity constraints to the edited BETA volume.
!   (11) Optionally replaces nil values in the new edited BETA volume.
!   (12) Edits input average velocities again using the new edited BETA volume.
!   (13) Always replaces nil values in the new edited average velocity volume.
!
! Notes regarding the above steps:
!
!    Weights:     The weight volume (optionally used for removing anomalous
!      (1)        values and for smoothing) is calculated from the following
!                 input traces:
!                      RMS amplitude stacked trace.
!                      maximum absolute amplitude stacked trace.
!                      semblance calculated along the residual NMO curve.
!                      semblance calculated along the structural dip.
!
!    Threshholds: Values in the ATTRIBUTE trace are set to nil where
!      (2)        the values in the following input traces are smaller
!                 than the specified threshholds:
!                      RMS amplitude stacked trace.
!                      maximum absolute amplitude stacked trace.
!                      semblance calculated along the residual NMO curve.
!                      semblance calculated along the structural dip.
!
!    Flattening:  New regions after shifting (at top and/or bottom of trace)
!      (3) (8)    are set to NIL.
!
!    Anomalies:   Values in the ATTRIBUTE trace are set to nil where they
!      (4)        differ from the mean in the surrounding 3D data window by
!                 more than the specified standard deviations.
!
!    Smoothing:   Values in the ATTRIBUTE trace are reset to the mean of all
!      (6)        non-nil values in the surrounding 3D data window.
!
!    Constraints: Values in the edited BETA trace are set to nil where the
!      (10)       ratios between the input and edited average or interval
!                 velocities are outside of the specified constraints.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Normal trace flow:
!
!    (1) Run ABRA to get the following 3D data volumes:
!
!           81  The residual NMO velocity ratio (BETA attribute).
!           82  The RMS amplitude stacked trace for this CMP.
!           83  The maximum absolute amplitude stacked trace for this CMP.
!           71  The semblance calculated along the residual NMO curve.
!            
!    (2) Run SDIP3D or EDA3D to get the following 3D data volumes:
!
!           88  The structural dip in the X direction (XDIP attribute).
!           89  The structural dip in the Y direction (YDIP attribute).
!           90  The semblance calculated along the structural dip.
!
!    (3) Also prepare the following migration velocity 3D data volume:
!
!           51  The input average velocity.
!
!    (4) Optionally run MASKER to mask unwanted portions of the above data
!        volumes, and choose the flattening option, for input into ka.
!        You can mask the top, bottom, or internal portions of the traces by
!        running MASKER one or more times to set the masked regions to nil.
!
!    (5) Optionally run KASTATS to gather statistics on the following 3D
!        data volumes for help in setting threshholds in KA:
!
!           82  The RMS amplitude stacked trace for this CMP.
!           83  The maximum absolute amplitude stacked trace for this CMP.
!           71  The semblance calculated along the residual NMO curve.
!           90  The semblance calculated along the structural dip.
!
!    (6) Run KA which takes any of the above input data volumes and
!        produces the following output data volumes:
!
!           91  The weight volume                (calculated by this process).
!           85  The output average velocity         (edited average velocity).
!          181  The residual NMO velocity ratio       (edited BETA attribute).
!          188  The structural dip in the X direction (edited XDIP attribute).
!          189  The structural dip in the Y direction (edited YDIP attribute).
!
!    (7) Optionally run MASKER again one or more times to mask unwanted
!        portions of the above data volumes which were output from KA.
!
!    (8) Run SLICER to shrink the 3D data volumes to 2D horizon slices,
!        with each horizon slice represented by a single sample point on
!        the traces.  The following new data volume (an additional trace)
!        will be created:
!
!            1  The time or depth of the horizon (last trace in gather).
!
!    (9) Run DABRA to further process the following 2D horizon slices:
!
!           91  The weight volume                     (calculated by KA).
!           85  The output average velocity               (edited by KA).
!          181  The residual NMO velocity ratio       (edited BETA attribute).
!          188  The structural dip in the X direction (edited XDIP attribute).
!          189  The structural dip in the Y direction (edited YDIP attribute).
!            1  The time or depth of the horizon (last trace in gather).
!
!   (10) Use the output from DABRA to run tomography.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process is a multiple-trace process.
!
! This process requires traces to be input with the primary sort in the Y
! direction and the secondary sort in the X direction.
!
! This process must receive input traces in gathers.
! Each input gather corresponds to a single CMP location.
!
! The traces in the input gather must contain certain information listed
! in the GENERAL DESCRIPTION section above.  Each type of information must
! be contained in a single trace.
!
! If two or more input traces have the same ident, the first one will be used.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs a gather of traces at a time.
! Each output gather corresponds to a single CMP location.
! Each output gather corresponds to each input gather, in the same order.
!
! The traces in the output gather will contain certain information listed
! in the GENERAL DESCRIPTION section above.  Each type of information will
! be contained in a single trace.
!
! If PASS_THROUGH is true, all input traces will be output, followed by the
! requested output traces, all in the same gather.  If PASS_THROUGH is false,
! the input traces will not be output.
!
!          WARNING: SINCE OUTPUT TRACES MIGHT CONTAIN NIL VALUES,
!                   THESE TRACES MUST NOT BE COMPRESSED.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used and reset.
! NWIH      number of words in trace header         used but not changed.
! NDPT      number of sample values in trace        used but not changed.
! TSTRT     starting time on trace                  used but not changed.
! DT        trace sample interval                   used but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#            Description               Action taken
! ----            -----------               ------------
! HDR_IDENT       trace identification      used to identify traces.
! HDR_FLATTENING  flattening time           used to flatten data.
! HDR_X           X coordinate              used to identify CMP location.
! HDR_Y           Y coordinate              used to identify CMP location.
! 25              largest absolute value    reset as necessary.
! 58              scratch header            used by the multgather primitive.
! 59              scratch header            used by the multgather primitive.
! 60              scratch header            used by the multgather primitive.
! 61              scratch header            used by the multgather primitive.
!
! Additional header words containing the X and Y coordinates of the
! traces may be used.  These header words are specified in the velocity
! files which are input.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 13. 2007-01-03  Stoeckley  Replace pc_register_tab_group w HelpSection line.
! 12. 2006-06-20  Stoeckley  Add calls to pc_register_tab_group for SeisSpace.
! 11. 2006-03-16 Bill Menger Made Parallel
! 10. 2006-01-10 Bill Menger Removed Unused Variables.
!  9. 2005-10-10  Goodger    Change arguments tri and hdi in the pre-process
!                            routine from intent out to intent inout to
!                            satisfy to absoft f90 compiler.
!  8. 2005-01-10  Stoeckley  Add USE_TAPERED_SMOOTHING & TAPERED_WEIGHT_PERCENT.
!  7. 2004-01-23  Stoeckley  Fix memory access bug when weights are not used.
!  6. 2003-10-20  Stoeckley  Change the way INTERP_WEIGHTS works; add
!                             REPLACE_NILS_VELCON and MAX_INTERP_LENGTH_VELCON.
!  5. 2003-10-02  Stoeckley  Add nullify calls before the memman_nullify calls
!                             to satisfy Portland Group compiler.
!  4. 2003-09-29  Stoeckley  Fix GAMMA = sqrt(BETA+1) which was upside down;
!                             fix bug with MAX_INTERP_LENGTH parameters; use
!                             abrakadabra to return identification string.
!  3. 2003-08-22  Stoeckley  Fix bug introduced in revision 2 for setting dead
!                             input traces to nil; remove requirement that VAV
!                             must always be input when BETA is edited; add
!                             parameter TRACE_SAMPLE_UNIT.
!  2. 2003-08-20  Stoeckley  Set dead input traces to nil; remove normalization
!                             of weights; change defaults for threshhold
!                             parameters; fix calculation of standard deviation.
!  1. 2003-06-19  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! The Portland Group compiler aborts on memman_nullify calls unless the
! pointer has previously been nullified.  Therefore the extra nullify
! statements have been added.  This is a Portland Group compiler bug,
! since it is legitimate to pass an undefined pointer to a subroutine
! which is expecting a pointer.  The only thing that would be illegimate
! in such a case would be for the subroutine to use the ASSOCIATED or
! DEALLOCATE statement on the pointer unless the ALLOCATE or NULLIFY
! statement is used first.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
! PCPS_BOSS_EXEC_MODE      'PCPS_BOSS_DISTRIBUTES'
! PCPS_SEND_MODE           'PCPS_SEND_ALL'
! PCPS_ALT_SEND_MODE       'PCPS_SEND_ALL'
! PCPS_SEND_EOF_MODE       'PCPS_SEND_ALL_EOF'
! PCPS_RECEIVE_MODE        'PCPS_RECEIVE_GATHER'
! PCPS_ALT_RECEIVE_MODE    'PCPS_RECEIVE_GATHER'
! PCPS_GENERATOR_MODE      'PCPS_TRACE_GEN'
! PCPS_ALT_GENERATOR_MODE  'PCPS_TRACE_GEN'
! PCPS_BUNCH_TRACE_MODE    'PCPS_BUNCH_TRACE_GROUPS'
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
!
!<NS Input and Output/NC=80>
!
!                            Kombo Analysis
!  NUM_CPUS~~~~=`II
!  [/L]Input Menu  [msg_in]`XXXXXXXXXXXXXXXXXXX
! `-----------------------------------------------------------------------------
!  HDR_IDENT~~~~~~~~=`II    [/L]Header word containing trace identification.
!  HDR_FLATTENING~~~=`II    [/L]Header word containing flattening time (sec).
!  TRACE_SAMPLE_UNIT=`CCCCC [/L]Whether traces are in time or depth.
!
!       [MSG01]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG02]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG03]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG04]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG05]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG06]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG07]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       [MSG08]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `-----------------------------------------------------------------------------
!
!  [/L]Output Menu [msg_out]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `-----------------------------------------------------------------------------
!  PASS_THROUGH~~~=`KK  [/L]Output all original input volumes.
!
!  OUTPUT_BETA~~~~=`KK  [/L]Output edited BETA attribute volume.      [/L][msg_o_beta]`XXXXXXXX
!  OUTPUT_XDIP~~~~=`KK  [/L]Output edited XDIP attribute volume.      [/L][msg_o_xdip]`XXXXXXXX
!  OUTPUT_YDIP~~~~=`KK  [/L]Output edited YDIP attribute volume.      [/L][msg_o_ydip]`XXXXXXXX
!  OUTPUT_WEIGHT~~=`KK  [/L]Output weight volume.                     [/L][msg_o_weight]`XXXXXXXX
!  OUTPUT_GAMMA~~~=`KK  [/L]Output GAMMA volume.                      [/L][msg_o_gamma]`XXXXXXXX
!  OUTPUT_VAV~~~~~=`KK  [/L]Output modified average velocity volume.  [/L][msg_o_vav]`XXXXXXXX
! `-----------------------------------------------------------------------------
!
!<NS 3D Data Volume/NC=80>
!
!           This a description of the input and output 3D data volume.
!
!
!  `------------------- `-------------------
!   HDR_X~~~~~~=`II      HDR_Y~~~~~~=`II     [/L]Header words containing X and Y coordinates.
!   X_INIT~~~~~=`FFFFF   Y_INIT~~~~~=`FFFFF  [/L]First (or any) bin center.
!   X_INC~~~~~~=`FFFFF   Y_INC~~~~~~=`FFFFF  [/L]Bin increment.
!   MAX_X_BINS =`IIIII                       [/L]Maximum approximate number of bins (traces) in X direction.
!  `------------------- `-------------------
!
!<NS 3D Analysis Window/NC=80>
!
!     This 3D analysis window is used for anomaly editing and for smoothing.
!
!
!  WIN_LEN =`FFFFFFFFFF  [/L]Window length in seconds for validation/smoothing.
!  nt~~~~~~=`XXXXXXXXXX  [/L]#Samples in window for validation/smoothing.
!
!  `------------------- `-------------------
!   NUM_TR_X~~~=`II      NUM_TR_Y~~~=`II     [/L]#Traces each side for validation/smoothing.
!   nx~~~~~~~~~=`XX      ny~~~~~~~~~=`XX     [/L]Total #traces for validation/smoothing.
!  `------------------- `-------------------
!
!<NS Velocity Constraint Editing/NC=80>
!
!           Apply velocity constraints to the BETA attribute volume.
! BETA values are set to nil if velocity changes fall outside the constraints.
!     Nil BETA values are optionally replaced after applying constraints.
!
!  APPLY_VAV_CONSTRAINTS~~=`KK  [/L]Apply average velocity constraints.
!  APPLY_VINT_CONSTRAINTS =`KK  [/L]Apply interval velocity constraints.
!
!                  VAV_TIME VAV_MIN_PERCENT VAV_MAX_PERCENT
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!
!                  VINT_TIMEVINT_MIN_PERCENTVINT_MAX_PERCENT
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `FFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!
! [/L]After applying velocity constraints:
!  `--------------------------------------------------------------------------
!    REPLACE_NILS_VELCON =`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!    MAX_INTERP_LENGTH_VELCON~~=`FFFFF  [/L]Maximum length of nil attribute values to interpolate (seconds).
!  `--------------------------------------------------------------------------
!
!<PARMS VAV_TIME_ARRAYSET [/XST/YST]>
!<PARMS VINT_TIME_ARRAYSET[/XST/YST]>
!
!<NS Threshhold Editing/NC=80>
!
!       Apply threshholds to the BETA or XDIP or YDIP attribute volume.
!         Attribute values are set to nil if threshholds are not met.
!
!  APPLY_RMS_AMPL_THRESH ~~ =`KK  [/L]Apply RMS amplitude threshholds.
!  APPLY_MAX_AMPL_THRESH ~~ =`KK  [/L]Apply maximum absolute amplitude threshholds.
!  APPLY_GATHER_SEMB_THRESH =`KK  [/L]Apply gather semblance threshholds.
!  APPLY_STRUCT_SEMB_THRESH =`KK  [/L]Apply structural semblance threshholds.
!
!  THRESH_TIMERMS_AMPL_THRESHMAX_AMPL_THRESHGATHER_SEMB_THRESHSTRUCT_SEMB_THRESH
!  `FFFFFFFFFF`FFFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFFFFFF `FFFFFFFFFFFFFFF~~
!  `FFFFFFFFFF`FFFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFFFFFF `FFFFFFFFFFFFFFF~~
!  `FFFFFFFFFF`FFFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFFFFFF `FFFFFFFFFFFFFFF~~
!  `FFFFFFFFFF`FFFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFFFFFF `FFFFFFFFFFFFFFF~~
!  `FFFFFFFFFF`FFFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFFFFFF `FFFFFFFFFFFFFFF~~
!
!<PARMS THRESH_TIME_ARRAYSET[/XST/YST]>
!
!<NS Weights/NC=80>
!
!       This weight volume is used for anomaly editing and for smoothing.
!
!  USE_RMS_AMPL_WEIGHTING ~~ =`KK  [/L]Use RMS amplitude for weighting.
!  USE_MAX_AMPL_WEIGHTING ~~ =`KK  [/L]Use maximum absolute amplitude for weighting.
!  USE_GATHER_SEMB_WEIGHTING =`KK  [/L]Use gather semblance for weighting.
!  USE_STRUCT_SEMB_WEIGHTING =`KK  [/L]Use structural semblance for weighting.
!
!<NS Anomaly Editing/NC=80>
!
!      Apply anomaly editing to the BETA or XDIP or YDIP attribute volume.
!           Attribute values are set to nil if they are anomalous.
!
!  REMOVE_BAD_VALUES ~~~ =`KK    [/L]Remove anomalous values based on standard deviation.
!  USE_WEIGHTED_EDIT ~~~ =`KK    [/L]Use weight volume for standard deviation calculation.
!  EDITING_PERCENT_VALID =`FFFF  [/L]Percent of 3D analysis window required to contain valid picks.
!
!              STDEV_TIMESTDEV
!              `FFFFFFFFF`FFFF
!              `FFFFFFFFF`FFFF
!              `FFFFFFFFF`FFFF
!              `FFFFFFFFF`FFFF
!              `FFFFFFFFF`FFFF
!
!<PARMS STDEV_TIME_ARRAYSET[/XST/YST]>
!
!<NS Smoothing/NC=80>
!
!  Smooth the BETA or XDIP or YDIP attribute volume after editing is finished.
!            The editing will have set some attribute values to nil.
!    Nil attribute values are optionally replaced before or after smoothing.
!
! [/L]Before smoothing:
!  `--------------------------------------------------------------------------
!    REPLACE_NILS_BEFORE =`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!    MAX_INTERP_LENGTH_BEFORE =`FFFFF  [/L]Maximum length of nil attribute values to interpolate (seconds).
!    INTERP_WEIGHTS_BEFORE~~~~=`KK     [/L]Interpolate weights where attribute values were nil.
!  `--------------------------------------------------------------------------
!
! [/L]Smoothing:
!  `--------------------------------------------------------------------------
!    APPLY_SMOOTHING ~~~~~~~~=`KK    [/L]Smooth the edited attribute volume.
!    USE_WEIGHTED_SMOOTHING~~=`KK    [/L]Use weight volume for smoothing.
!    USE_DISTANCE_SMOOTHING~~=`KK    [/L]Use inverse distance for smoothing.
!    USE_TAPERED_SMOOTHING~~~=`KK    [/L]Use tapered edges for smoothing.
!    CENTER_WEIGHT_PERCENT~~~=`FFFF  [/L]Percent of center weight for inverse distance smoothing.
!    TAPERED_WEIGHT_PERCENT~~=`FFFF  [/L]Percent of smoothing distance to taper.
!    SMOOTHING_PERCENT_VALID =`FFFF  [/L]Percent of 3D analysis window required to contain valid picks.
!  `--------------------------------------------------------------------------
!
! [/L]After smoothing:
!  `--------------------------------------------------------------------------
!    REPLACE_NILS_AFTER =`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!    MAX_INTERP_LENGTH_AFTER~~=`FFFFF  [/L]Maximum length of nil attribute values to interpolate (seconds).
!    INTERP_WEIGHTS_AFTER~~~~~=`KK     [/L]Interpolate weights where attribute values were nil.
!  `--------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!           tabgroup = Threshhold Editing
!
!<Help KEYWORD="APPLY_RMS_AMPL_THRESH">
!<Tip> Whether to apply RMS amplitude threshholds. </Tip>
! Default = YES
! Allowed = YES or NO
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! The RMS_AMPL_THRESH array will not be used if this parameter is NO.
!</Help>
!
!
!<Help KEYWORD="APPLY_MAX_AMPL_THRESH">
!<Tip> Whether to apply maximum absolute amplitude threshholds. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! The MAX_AMPL_THRESH array will not be used if this parameter is NO.
!</Help>
!
!
!<Help KEYWORD="APPLY_GATHER_SEMB_THRESH">
!<Tip> Whether to apply gather semblance threshholds. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! The GATHER_SEMB_THRESH array will not be used if this parameter is NO.
!</Help>
!
!
!<Help KEYWORD="APPLY_STRUCT_SEMB_THRESH">
!<Tip> Whether to apply structural semblance threshholds. </Tip>
! Default = YES
! Allowed = YES or NO
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! The STRUCT_SEMB_THRESH array will not be used if this parameter is NO.
!</Help>
!
!           tabgroup = Input and Output
!
!<Help KEYWORD="NUM_CPUS">
!<Tip> This is a global and you cannot set it. </Tip>
! Default = 1
! Allowed >= 1
!</Help>
!!
!<Help KEYWORD="HDR_IDENT">
!<Tip> Header word containing trace identification. </Tip>
! Default = 48
! Allowed = 1-NWIH   (normally 48-55 or 65-NWIH)
!
! The following identification integers must reside in the specified
! header word of each trace:
!
!  input    81  The residual NMO velocity ratio       (BETA attribute).
!  input    88  The structural dip in the X direction (XDIP attribute).
!  input    89  The structural dip in the Y direction (YDIP attribute).
!  input    82  The RMS amplitude stacked trace for this CMP.
!  input    83  The maximum absolute amplitude stacked trace for this CMP.
!  input    71  The semblance calculated along the residual NMO curve.
!  input    90  The semblance calculated along the structural dip.
!  input    51  The input average velocity.
!  output   91  The weight volume                (calculated by this process).
!  output   84  The GAMMA volume                 (calculated by this process).
!  output   85  The output average velocity         (edited average velocity).
!  output  181  The residual NMO velocity ratio       (edited BETA attribute).
!  output  188  The structural dip in the X direction (edited XDIP attribute).
!  output  189  The structural dip in the Y direction (edited YDIP attribute).
!</Help>
!
!
!<Help KEYWORD="HDR_FLATTENING">
!<Tip> Header word containing flattening time (seconds). </Tip>
! Default = 0
! Allowed = 0-NWIH   (normally 0 or 48-55 or 65-NWIH)
!
! Each WEIGHT and ATTRIBUTE trace will be flattened using the flattening
! time in the specified trace header word.
!
! The traces are flattened before anomaly editing and smoothing.
! The traces are unflattened after anomaly editing and smoothing.
!
! The traces are flattened by shifting the flattening time to zero.
! The shifts are performed by shifting the traces by whole sample intervals
! so that no interpolation between trace samples will be done.
!
! New trace values shifted in from off the end of the trace will be set to NIL.
!</Help>
!
!
!<Help KEYWORD="TRACE_SAMPLE_UNIT">
!<Tip> Whether traces are in time or depth. </Tip>
! Default = DEPTH
! Allowed = TIME or DEPTH
!
! This parameter is needed only when calculating interval velocities from
! an average velocity trace.  The algorithm is different depending on whether
! the trace sample interval corresponds to constant time steps or constant
! depth steps.
!
! This parameter is used only when applying interval velocity constraints
! to the BETA attribute.
!</Help>
!
!           tabgroup = 3D Data Volume
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X coordinate. </Tip>
! Default = 7
! Allowed = 1-NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y coordinate. </Tip>
! Default = 8
! Allowed = 1-NWIH
!</Help>
!
!            tabgroup = Anomaly Editing
!
!<Help KEYWORD="STDEV_TIME">
!<Tip> Time on trace for specifying standard deviation. </Tip>
! Default = 0.0
! Allowed = valid time on trace (seconds)
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! This is the time on the trace where the user wishes to specify STDEV.
!
! The times must be specified in ascending order.
!
! Interpolations will be performed between the specified times, and flat
! extrapolations will be performed above the first specified time and below
! the last specified time.
!</Help>
!
!
!<Help KEYWORD="STDEV">
!<Tip> Maximum number of standard deviations to allow. </Tip>
! Default = 3.0
! Allowed = 0.0 or greater
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! Each value in the attribute volume is compared with the mean value
! calculated from a 3D analysis window centered at that value.  A value
! will be set to nil if it differs from the mean value by more than
! the specified number of standard deviations.
!</Help>
!
!            tabgroup = Threshhold Editing
!
!<Help KEYWORD="THRESH_TIME">
!<Tip> Time on trace for specifying threshholds. </Tip>
! Default = 0.0
! Allowed = valid time on trace (seconds)
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! This is the time on the trace where the user wishes to specify the following
! parameters:
!               RMS_AMPL_THRESH
!               MAX_AMPL_THRESH
!               GATHER_SEMB_THRESH
!               STRUCT_SEMB_THRESH
!
! The times must be specified in ascending order.
!
! Interpolations will be performed between the specified times, and flat
! extrapolations will be performed above the first specified time and below
! the last specified time.
!</Help>
!
!
!<Help KEYWORD="RMS_AMPL_THRESH">
!<Tip> RMS amplitude threshhold. </Tip>
! Default = 0.8
! Allowed >= 0.0
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! RMS amplitude values should be those which were calculated from the CMP
! gather before the traces were stacked.
!
! RMS amplitude values smaller than the specified threshhold will cause the
! corresponding value in the attribute volume to be set to nil.
!</Help>
!
!
!<Help KEYWORD="MAX_AMPL_THRESH">
!<Tip> Maximum absolute amplitude threshhold. </Tip>
! Default = 1.2
! Allowed >= 0.0
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! Maximum absolute amplitude values should be those which were calculated
! from the CMP gather before the traces were stacked.
!
! Maximum amplitude values smaller than the specified threshhold will cause
! the corresponding value in the attribute volume to be set to nil.
!
! Absolute values of the MAX_AMPL_THRESH array values will be used.
!</Help>
!
!
!<Help KEYWORD="GATHER_SEMB_THRESH">
!<Tip> Gather semblance threshhold. </Tip>
! Default = 0.4
! Allowed >= 0.0
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! Gather semblance values should be those which were obtained while the input
! BETA data volume was being calculated.
!
! Gather semblance values smaller than the specified threshhold will cause
! the corresponding value in the attribute volume to be set to nil.
!</Help>
!
!
!<Help KEYWORD="STRUCT_SEMB_THRESH">
!<Tip> Structure semblance threshhold. </Tip>
! Default = 0.4
! Allowed >= 0.0
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! Structural semblance values should be those which were obtained while the
! input XDIP and YDIP data volumes were being calculated.
!
! Structural semblance values smaller than the specified threshhold will cause
! the corresponding value in the attribute volume to be set to nil.
!
! Negative or NIL structural semblance values will cause
! the corresponding value in the attribute volume to be set to nil.
!</Help>
!
!           tabgroup = Velocity Constraint Editing
!
!<Help KEYWORD="APPLY_VAV_CONSTRAINTS">
!<Tip> Whether to apply average velocity constraints. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to edit the BETA attribute volume.
!
! The VAV_MIN_PERCENT and VAV_MAX_PERCENT arrays will not be used if this
! parameter is NO.
!</Help>
!
!
!<Help KEYWORD="APPLY_VINT_CONSTRAINTS">
!<Tip> Whether to apply interval velocity constraints. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to edit the BETA attribute volume.
!
! The VINT_MIN_PERCENT and VINT_MAX_PERCENT arrays will not be used if this
! parameter is NO.
!</Help>
!
!           tabgroup = Velocity Constraint Editing
!
!<Help KEYWORD="VAV_TIME">
!<Tip> Time on trace for specifying average velocity contraints. </Tip>
! Default = 0.0
! Allowed = valid time on trace (seconds)
!
! This parameter is used to edit the BETA attribute volume.
!
! This is the time on the trace where the user wishes to specify the following
! parameters:
!               VAV_MIN_PERCENT
!               VAV_MAX_PERCENT
!
! The times must be specified in ascending order.
!
! Interpolations will be performed between the specified times, and flat
! extrapolations will be performed above the first specified time and below
! the last specified time.
!</Help>
!
!
!<Help KEYWORD="VINT_TIME">
!<Tip> Time on trace for specifying interval velocity contraints. </Tip>
! Default = 0.0
! Allowed = valid time on trace (seconds)
!
! This parameter is used to edit the BETA attribute volume.
!
! This is the time on the trace where the user wishes to specify the following
! parameters:
!               VINT_MIN_PERCENT
!               VINT_MAX_PERCENT
!
! The times must be specified in ascending order.
!
! Interpolations will be performed between the specified times, and flat
! extrapolations will be performed above the first specified time and below
! the last specified time.
!</Help>
!
!
!<Help KEYWORD="VAV_MIN_PERCENT">
!<Tip> Minimum percentage of average velocity to accept. </Tip>
! Default = 75.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to edit the BETA attribute volume.
!
! This array is not used if APPLY_VAV_CONSTRAINTS is NO.
!
! Average velocities smaller than the specified threshhold will cause the
! corresponding value in the BETA attribute volume to be set to nil.
!</Help>
!
!
!<Help KEYWORD="VAV_MAX_PERCENT">
!<Tip> Maximum percentage of average velocity to accept. </Tip>
! Default = 125.0
! Allowed = 100.0 or greater
!
! This parameter is used to edit the BETA attribute volume.
!
! This array is not used if APPLY_VAV_CONSTRAINTS is NO.
!
! Average velocities larger than the specified threshhold will cause the
! corresponding value in the BETA attribute volume to be set to nil.
!</Help>
!
!
!<Help KEYWORD="VINT_MIN_PERCENT">
!<Tip> Minimum percentage of interval velocity to accept. </Tip>
! Default = 75.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to edit the BETA attribute volume.
!
! This array is not used if APPLY_VINT_CONSTRAINTS is NO.
!
! Interval velocities smaller than the specified threshhold will cause the
! corresponding value in the BETA attribute volume to be set to nil.
!</Help>
!
!
!<Help KEYWORD="VINT_MAX_PERCENT">
!<Tip> Maximum percentage of interval velocity to accept. </Tip>
! Default = 125.0
! Allowed = 100.0 or greater
!
! This parameter is used to edit the BETA attribute volume.
!
! This array is not used if APPLY_VINT_CONSTRAINTS is NO.
!
! Interval velocities larger than the specified threshhold will cause the
! corresponding value in the BETA attribute volume to be set to nil.
!</Help>
!
!           tabgroup = 3D Data Volume
!
!<Help KEYWORD="MAX_X_BINS">
!<Tip> Maximum number of X bins in any one Y-line. </Tip>
! Default = blank
! Allowed = integer > 0
!
! This value must be specified.  The maximum number of traces which will
! be stored on disk at any one time will be slightly greater than MAX_X_BINS
! times (2*NUM_TR_Y + 1) times the number of input traces used at each CMP.
!</Help>
!
!           tabgroup = Input and Output
!
!<Help KEYWORD="msg_o_beta" TYPE="DISPLAY_ONLY">
!<Tip> Trace identification found in header word specified by HDR_IDENT. </Tip>
!</Help>
!
!<Help KEYWORD="msg_o_xdip" TYPE="DISPLAY_ONLY">
!<Tip> Trace identification found in header word specified by HDR_IDENT. </Tip>
!</Help>
!
!<Help KEYWORD="msg_o_ydip" TYPE="DISPLAY_ONLY">
!<Tip> Trace identification found in header word specified by HDR_IDENT. </Tip>
!</Help>
!
!<Help KEYWORD="msg_o_weight" TYPE="DISPLAY_ONLY">
!<Tip> Trace identification found in header word specified by HDR_IDENT. </Tip>
!</Help>
!
!<Help KEYWORD="msg_o_gamma" TYPE="DISPLAY_ONLY">
!<Tip> Trace identification found in header word specified by HDR_IDENT. </Tip>
!</Help>
!
!<Help KEYWORD="msg_o_vav" TYPE="DISPLAY_ONLY">
!<Tip> Trace identification found in header word specified by HDR_IDENT. </Tip>
!</Help>
!
!           tabgroup = Input and Output
!
!<Help KEYWORD="msg_in" TYPE="DISPLAY_ONLY">
!<Tip> Number of input traces for each CMP location. </Tip>
!</Help>
!
!<Help KEYWORD="msg_out" TYPE="DISPLAY_ONLY">
!<Tip> Number of output traces for each CMP location. </Tip>
!</Help>
!
!           tabgroup = Input and Output
!
!<Help KEYWORD="MSG01" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG02" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG03" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG04" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG05" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG06" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG07" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!<Help KEYWORD="MSG08" TYPE="DISPLAY_ONLY">
!<Tip> Information about required input traces. </Tip>
!</Help>
!
!           tabgroup = Analysis Window
!
!<Help KEYWORD="NT" TYPE="DISPLAY_ONLY">
!<Tip> Total number sample points (in time) in 3D analysis window. </Tip>
!
! The 3D analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute volume.
!
! NT is calculated from WIN_LEN.
!</Help>
!
!
!<Help KEYWORD="NX" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in X direction in 3D analysis window. </Tip>
!
! The 3D analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute volume.
!
! NX is calculated from NUM_TR_X.
!</Help>
!
!
!<Help KEYWORD="NY" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces in Y direction in 3D analysis window. </Tip>
!
! The 3D analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute volume.
!
! NY is calculated from NUM_TR_Y.
!</Help>
!
!           tabgroup = Analysis Window
!
!<Help KEYWORD="NUM_TR_X">
!<Tip> Number of traces on each side in X dir in 3D analysis window. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of traces on each side of the center trace in the
! 3D analysis window.
!
! The 3D analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute volume.
!
! The 3D analysis window is specified with the following three parameters:
!                WIN_LEN     (size in the time direction is NT)
!                NUM_TR_X    (size in the  X   direction is NX)
!                NUM_TR_Y    (size in the  Y   direction is NY)
!</Help>
!
!
!<Help KEYWORD="NUM_TR_Y">
!<Tip> Number of traces on each side in Y dir in 3D analysis window. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of traces on each side of the center trace in the
! 3D analysis window.
!
! The 3D analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute volume.
!
! The 3D analysis window is specified with the following three parameters:
!                WIN_LEN     (size in the time direction is NT)
!                NUM_TR_X    (size in the  X   direction is NX)
!                NUM_TR_Y    (size in the  Y   direction is NY)
!</Help>
!
!           tabgroup = Input and Output
!
!<Help KEYWORD="PASS_THROUGH">
!<Tip> Whether to output all of the original input volumes. </Tip>
! Default = YES
! Allowed = YES or NO
!
! If YES, all of the original (unmodified) input traces will be output along
! with the requested output traces.  This choice is useful if you wish to
! follow one instance of KA with another in the same job.
!
! If NO, the original input traces will not be output.  Depending on the
! number of input traces in a gather, and the parameter choices, this can
! be a significant time savings.
!</Help>
!
!
!<Help KEYWORD="OUTPUT_BETA">
!<Tip> Whether to output an edited BETA attribute volume. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The output BETA attribute volume will be the edited and smoothed version
! of the input BETA attribute volume.
!
! BETA = (input average velocity / output average velocity)**2 - 1
!
! sqrt(BETA + 1) = input average velocity / output average velocity.
!
! The output edited BETA attribute volume might contain nil values unless
! REPLACE_NILS_AFTER and REPLACE_NILS_VELCON say to replace nil
! values.
!</Help>
!
!
!<Help KEYWORD="OUTPUT_XDIP">
!<Tip> Whether to output an edited XDIP attribute volume. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The output XDIP attribute volume will be the edited and smoothed version
! of the input XDIP attribute volume.
!
! The output edited XDIP attribute volume might contain nil values unless
! REPLACE_NILS_AFTER says to replace nil values.
!</Help>
!
!
!<Help KEYWORD="OUTPUT_YDIP">
!<Tip> Whether to output an edited YDIP attribute volume. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The output YDIP attribute volume will be the edited and smoothed version
! of the input YDIP attribute volume.
!
! The output edited YDIP attribute volume might contain nil values unless
! REPLACE_NILS_AFTER says to replace nil values.
!</Help>
!
!
!<Help KEYWORD="OUTPUT_WEIGHT">
!<Tip> Whether to output a weight volume. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The weight volume is calculated based on the following four parameters:
!                USE_RMS_AMPL_WEIGHTING
!                USE_MAX_AMPL_WEIGHTING
!                USE_GATHER_SEMB_WEIGHTING
!                USE_STRUCT_SEMB_WEIGHTING
!
! The weight volume will never contain any nil values.
!</Help>
!
!
!<Help KEYWORD="OUTPUT_GAMMA">
!<Tip> Whether to output a GAMMA volume. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This volume will be calculated from the edited BETA volume.
!
! GAMMA = sqrt(BETA + 1) = input average velocity / output average velocity.
!
! The output GAMMA volume will never contain nil or zero values.
!</Help>
!
!
!<Help KEYWORD="OUTPUT_VAV">
!<Tip> Whether to output a modified average velocity volume. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This volume will be calculated from the input average velocity volume
! and the edited BETA volume.
!
! The output modified average velocity volume will never contain nil or
! zero values.
!</Help>
!
!            tabgroup = Anomaly Editing
!
!<Help KEYWORD="REMOVE_BAD_VALUES">
!<Tip> Whether to remove anomalous values based on standard deviation. </Tip>
! Default = YES
! Allowed = YES or NO
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! The USE_WEIGHTED_EDIT parameter and the STDEV array will not be used if
! this parameter is NO.
!</Help>
!
!
!<Help KEYWORD="USE_WEIGHTED_EDIT">
!<Tip> Whether to use weight volume for standard deviation calculation. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! Constant weights will be used if this parameter is NO.
!
! The weight volume is calculated based on the following four parameters:
!                USE_RMS_AMPL_WEIGHTING
!                USE_MAX_AMPL_WEIGHTING
!                USE_GATHER_SEMB_WEIGHTING
!                USE_STRUCT_SEMB_WEIGHTING
!</Help>
!
!            tabgroup = Weights
!
!<Help KEYWORD="USE_RMS_AMPL_WEIGHTING">
!<Tip> Whether to use RMS amplitude for weighting. </Tip>
! Default = YES
! Allowed = YES or NO
!
! This parameter is used to calculate the weight volume which is used
! to edit and smooth the selected BETA or XDIP or YDIP attribute volume.
!
! If you choose YES, an RMS amplitude trace volume must be input.
!
! Zero, negative, and NIL values in any RMS amplitude trace will be
! interpreted as zero.
!</Help>
!
!
!<Help KEYWORD="USE_MAX_AMPL_WEIGHTING">
!<Tip> Whether to use maximum absolute amplitude for weighting. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to calculate the weight volume which is used
! to edit and smooth the selected BETA or XDIP or YDIP attribute volume.
!
! If you choose YES, a maximum absolute amplitude trace volume must be input.
!
! Zero and NIL values in any maximum absolute amplitude trace will be
! interpreted as zero.  Negative values will be interpreted as if they
! are positive.
!</Help>
!
!
!<Help KEYWORD="USE_GATHER_SEMB_WEIGHTING">
!<Tip> Whether to use gather semblance for weighting. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to calculate the weight volume which is used
! to edit and smooth the selected BETA or XDIP or YDIP attribute volume.
!
! If you choose YES, a gather semblance trace volume must be input.
!
! Zero, negative, and NIL values in any gather semblance trace will be
! interpreted as zero.
!</Help>
!
!
!<Help KEYWORD="USE_STRUCT_SEMB_WEIGHTING">
!<Tip> Whether to use structural semblance for weighting. </Tip>
! Default = YES
! Allowed = YES or NO
!
! This parameter is used to calculate the weight volume which is used
! to edit and smooth the selected BETA or XDIP or YDIP attribute volume.
!
! If you choose YES, a structural semblance trace volume must be input.
!
! Zero, negative, and NIL values in any structural semblance trace will be
! interpreted as zero.
!</Help>
!
!            tabgroup = Smoothing
!
!<Help KEYWORD="USE_WEIGHTED_SMOOTHING">
!<Tip> Whether to use the weight volume for smoothing. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! volume.
!
! The weight volume is calculated based on the following four parameters:
!                USE_RMS_AMPL_WEIGHTING
!                USE_MAX_AMPL_WEIGHTING
!                USE_GATHER_SEMB_WEIGHTING
!                USE_STRUCT_SEMB_WEIGHTING
!</Help>
!
!
!<Help KEYWORD="USE_DISTANCE_SMOOTHING">
!<Tip> Whether to use inverse distance for smoothing. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! volume.
!
! NO causes a stronger smoothing by weighting all traces the same, regardless
! of their distances from the middle trace.
!</Help>
!
!
!<Help KEYWORD="USE_TAPERED_SMOOTHING">
!<Tip> Whether to use a taper at the edge of the smoothing area. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! volume.
!
! NO causes a stronger smoothing by weighting all traces the same, regardless
! of their distances from the middle trace.
!</Help>
!
!
!<Help KEYWORD="CENTER_WEIGHT_PERCENT">
!<Tip> Percent of center weight to use for inverse distance smoothing. </Tip>
! Default = 20.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to set the weight of the center trace when inverse
! distance weights are used for smoothing.  If this parameter were not used,
! the center trace would always get 100% of the weight because the inverse
! distance would be infinite.
!
! A smaller percent weights the center trace less, causing a stronger smoothing.
! A larger percent weights the center trace more, causing a weaker smoothing.
!
! This parameter is not used if USE_DISTANCE_SMOOTHING is NO.
!</Help>
!
!
!<Help KEYWORD="TAPERED_WEIGHT_PERCENT">
!<Tip> Percent of the distance to taper for tapered smoothing. </Tip>
! Default = 20.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to set the size of the tapered area at the edge of
! the smoothing area.  for example, if this parameter is set to 20, the
! outer 20% of the smoothing distance will be tapered (lower weights grading
! to zero) and the inner 80% will have constant weights.
!
! A smaller percent produces a narrower taper, causing a stronger smoothing.
! A larger percent produces a wider taper, causing a weaker smoothing.
!
! This parameter is not used if USE_TAPERED_SMOOTHING is NO.
!</Help>
!
!            tabgroup = Anomaly Editing
!
!<Help KEYWORD="EDITING_PERCENT_VALID">
!<Tip> Percent of window required to contain valid picks for editing. </Tip>
! Default = 60.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! The 3D analysis window is specified with the following three parameters:
!                WIN_LEN     (size in the time direction is NT)
!                NUM_TR_X    (size in the  X   direction is NX)
!                NUM_TR_Y    (size in the  Y   direction is NY)
!</Help>
!
!            tabgroup = Smoothing
!
!<Help KEYWORD="SMOOTHING_PERCENT_VALID">
!<Tip> Percent of window required to contain valid picks for smoothing. </Tip>
! Default = 60.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! volume.
!
! The 3D analysis window is specified with the following three parameters:
!                WIN_LEN     (size in the time direction is NT)
!                NUM_TR_X    (size in the  X   direction is NX)
!                NUM_TR_Y    (size in the  Y   direction is NY)
!</Help>
!
!           tabgroup = Analysis Window
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length in time of the 3D analysis window (seconds). </Tip>
! Default = 0.1
! Allowed = real > 0.0
!
! The 3D analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute volume.
!
! The 3D analysis window is specified with the following three parameters:
!                WIN_LEN     (size in the time direction is NT)
!                NUM_TR_X    (size in the  X   direction is NX)
!                NUM_TR_Y    (size in the  Y   direction is NY)
!</Help>
!
!            tabgroup = Smoothing
!
!<Help KEYWORD="REPLACE_NILS_BEFORE">
!<Tip> Whether and how to replace nil attribute values before smoothing. </Tip>
! Default = do not replace nils
! Allowed = use linear interpolation (to replace nils) (vertical interpolation)
! Allowed = use scaled interpolation (to replace nils) (vertical interpolation)
! Allowed = replace nils with zeroes                   (vertical interpolation)
! Allowed = linear interpolation between weight peaks  (vertical interpolation)
! Allowed = scaled interpolation between weight peaks  (vertical interpolation)
! Allowed = do not replace nils
!
! This parameter is used before smoothing the selected BETA or XDIP or
! YDIP attribute volume.
!
! Linear interpolation simply replaces nils with linearly interpolated values.
!
! Scaled interpolation is used only when editing the BETA attribute.
! Scaled interpolation is replaced by linear interpolation when editing
! the XDIP or YDIP attribute.
!
! Scaled interpolation does a linear interpolation of the SQRT(BETA+1)
! values instead of a linear interpolation of the BETA values directly.
! SQRT(BETA+1) = GAMMA = old average velocity / new average velocity.
!
! Interpolation between weight peaks uses information from the weight volume,
! instead of the locations of nil attribute values, to determine where the
! attribute values will be replaced by interpolated values.  The attribute
! values are interpolated between local maximum peaks on the weight volume.
! No interpolation is performed between locations where the attribute value
! is nil at one or both of the bracketing peaks of the weight volume.
!</Help>
!
!
!<Help KEYWORD="REPLACE_NILS_AFTER">
!<Tip> Whether and how to replace nil attribute values after smoothing. </Tip>
! Default = do not replace nils
! Allowed = use linear interpolation (to replace nils) (vertical interpolation)
! Allowed = use scaled interpolation (to replace nils) (vertical interpolation)
! Allowed = replace nils with zeroes                   (vertical interpolation)
! Allowed = linear interpolation between weight peaks  (vertical interpolation)
! Allowed = scaled interpolation between weight peaks  (vertical interpolation)
! Allowed = do not replace nils
!
! This parameter is used after smoothing the selected BETA or XDIP or
! YDIP attribute volume.
!
! Linear interpolation simply replaces nils with linearly interpolated values.
!
! Scaled interpolation is used only when editing the BETA attribute.
! Scaled interpolation is replaced by linear interpolation when editing
! the XDIP or YDIP attribute.
!
! Scaled interpolation does a linear interpolation of the SQRT(BETA+1)
! values instead of a linear interpolation of the BETA values directly.
! SQRT(BETA+1) = GAMMA = old average velocity / new average velocity.
!
! Interpolation between weight peaks uses information from the weight volume,
! instead of the locations of nil attribute values, to determine where the
! attribute values will be replaced by interpolated values.  The attribute
! values are interpolated between local maximum peaks on the weight volume.
! No interpolation is performed between locations where the attribute value
! is nil at one or both of the bracketing peaks of the weight volume.
!</Help>
!
!        tabgroup = Velocity Constraint Editing
!
!<Help KEYWORD="REPLACE_NILS_VELCON">
!<Tip> Whether and how to replace nil BETA values after constraints. </Tip>
! Default = do not replace nils
! Allowed = use linear interpolation (to replace nils) (vertical interpolation)
! Allowed = use scaled interpolation (to replace nils) (vertical interpolation)
! Allowed = replace nils with zeroes                   (vertical interpolation)
! Allowed = linear interpolation between weight peaks  (vertical interpolation)
! Allowed = scaled interpolation between weight peaks  (vertical interpolation)
! Allowed = do not replace nils
!
! This parameter is used after applying velocity constraints to the BETA
! attribute volume.
!
! Linear interpolation simply replaces nils with linearly interpolated values.
!
! Scaled interpolation does a linear interpolation of the SQRT(BETA+1)
! values instead of a linear interpolation of the BETA values directly.
! SQRT(BETA+1) = GAMMA = old average velocity / new average velocity.
!
! Interpolation between weight peaks uses information from the weight volume,
! instead of the locations of nil BETA values, to determine where the
! BETA values will be replaced by interpolated values.  The BETA values
! are interpolated between local maximum peaks on the weight volume.
! No interpolation is performed between locations where the BETA value
! is nil at one or both of the bracketing peaks of the weight volume.
!</Help>
!
!            tabgroup = Smoothing
!
!<Help KEYWORD="APPLY_SMOOTHING">
!<Tip> Whether to smooth the BETA or XDIP or YDIP attribute volume. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The attribute volume will be smoothed over the 3D analysis window.
!</Help>
!
!            tabgroup = Smoothing
!
!<Help KEYWORD="MAX_INTERP_LENGTH_BEFORE">
!<Tip> Maximum length of nil attribute values to interpolate (seconds). </Tip>
! Default = 0.0 
! Allowed = 0.0 - trace length
!
! Any nil-filled gaps with a length greater than the specified time length
! will not be interpolated when using linear or scaled interpolation before
! smoothing.
!
! If this parameter is >= trace length or zero, nil-filled gaps of all
! lengths will be interpolated.
!
! This parameter is used only if REPLACE_NILS_BEFORE is set to one of the
! following choices:
!           use linear interpolation
!           use scaled interpolation
!           linear interpolation between weight peaks
!           scaled interpolation between weight peaks
!</Help>
!
!
!<Help KEYWORD="MAX_INTERP_LENGTH_AFTER">
!<Tip> Maximum length of nil attribute values to interpolate (seconds). </Tip>
! Default = 0.0 
! Allowed = 0.0 - trace length
!
! Any nil-filled gaps with a length greater than the specified time length
! will not be interpolated when using linear or scaled interpolation after
! smoothing.
!
! If this parameter is >= trace length or zero, nil-filled gaps of all
! lengths will be interpolated.
!
! This parameter is used only if REPLACE_NILS_AFTER is set to one of the
! following choices:
!           use linear interpolation
!           use scaled interpolation
!           linear interpolation between weight peaks
!           scaled interpolation between weight peaks
!</Help>
!
!           tabgroup = Velocity Constraint Editing
!
!<Help KEYWORD="MAX_INTERP_LENGTH_VELCON">
!<Tip> Maximum length of nil BETA values to interpolate (seconds). </Tip>
! Default = 0.0 
! Allowed = 0.0 - trace length
!
! Any nil-filled gaps with a length greater than the specified time length
! will not be interpolated when using linear or scaled interpolation after
! smoothing.
!
! If this parameter is >= trace length or zero, nil-filled gaps of all
! lengths will be interpolated.
!
! This parameter is used only if REPLACE_NILS_VELCON is set to one of the
! following choices:
!           use linear interpolation
!           use scaled interpolation
!           linear interpolation between weight peaks
!           scaled interpolation between weight peaks
!</Help>
!
!            tabgroup = Smoothing
!
!<Help KEYWORD="INTERP_WEIGHTS_BEFORE">
!<Tip>Interpolate weights where attribute values are interpolated. </Tip>
! Default = NO
! Allowed = YES or NO
!
! If this parameter is YES, the weights will be replaced by linearly
! interpolated values in the regions where the attribute values were
! nil before interpolation.
!</Help>
!
!
!<Help KEYWORD="INTERP_WEIGHTS_AFTER">
!<Tip>Interpolate weights where attribute values are interpolated. </Tip>
! Default = NO
! Allowed = YES or NO
!
! If this parameter is YES, the weights will be replaced by linearly
! interpolated values in the regions where the attribute values were
! nil before interpolation.
!</Help>
!
!           tabgroup = 3D Data Volume
!
!<Help KEYWORD="X_INIT">
!<Tip> X coordinate of the first (or any) trace in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any CMP bin measured in the X direction.
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
! This value must be the center of any CMP bin measured in the Y direction.
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
! This value must be the CMP bin increment (or width) in the X direction.
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y between bin centers in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the CMP bin increment (or width) in the Y direction.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ka_module
      use pc_module
      use named_constants_module
      use abrakadabra_module
      use kadabra_module
      use memman_module
      use string_module
      use mth_module
      use multgather_module
      use lav_module
      use terputil_module
      use pcps_module

      implicit none
      private
      public :: ka_create
      public :: ka_initialize
      public :: ka_update
      public :: ka_delete
      public :: ka            ! main trace processing routine.
      public :: ka_wrapup

      character(len=100),public,save :: KA_IDENT = &
'$Id: ka.f90,v 1.13 2007/01/03 14:01:41 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: ka_struct

        private
        logical            :: skip_wrapup                 ! wrapup flag.

        integer            :: numtr_input                 ! global
        integer            :: numtr_in_and_out            ! global
        integer            :: numtr_process               ! global
        integer            :: numtr_output                ! global
        integer            :: nwih                        ! global
        integer            :: ndpt                        ! global
        real               :: tstrt                       ! global
        real               :: tstop                       ! global
        real               :: dt                          ! global
        integer            :: num_cpus                    ! global

        logical            :: apply_rms_ampl_thresh       ! process parameter
        logical            :: apply_max_ampl_thresh       ! process parameter
        logical            :: apply_gather_semb_thresh    ! process parameter
        logical            :: apply_struct_semb_thresh    ! process parameter
        logical            :: apply_vav_constraints       ! process parameter
        logical            :: apply_vint_constraints      ! process parameter

        integer            :: hdr_ident                   ! process parameter
        integer            :: hdr_flattening              ! process parameter
        integer            :: hdr_x                       ! process parameter
        integer            :: hdr_y                       ! process parameter
        real               :: x_init                      ! process parameter
        real               :: y_init                      ! process parameter
        real               :: x_inc                       ! process parameter
        real               :: y_inc                       ! process parameter
        integer            :: max_x_bins                  ! process parameter

        integer            :: num_tr_x                    ! process parameter
        integer            :: num_tr_y                    ! process parameter
        real               :: win_len                     ! process parameter

        logical            :: pass_through                ! process parameter
        logical            :: output_beta                 ! process parameter
        logical            :: output_xdip                 ! process parameter
        logical            :: output_ydip                 ! process parameter
        logical            :: output_weight               ! process parameter
        logical            :: output_gamma                ! process parameter
        logical            :: output_vav                  ! process parameter

        logical            :: use_weighted_edit           ! process parameter
        logical            :: remove_bad_values           ! process parameter
        logical            :: apply_smoothing             ! process parameter
        logical            :: use_rms_ampl_weighting      ! process parameter
        logical            :: use_max_ampl_weighting      ! process parameter
        logical            :: use_gather_semb_weighting   ! process parameter
        logical            :: use_struct_semb_weighting   ! process parameter
        logical            :: use_weighted_smoothing      ! process parameter
        logical            :: use_distance_smoothing      ! process parameter
        logical            :: use_tapered_smoothing       ! process parameter

        real               :: center_weight_percent       ! process parameter
        real               :: tapered_weight_percent      ! process parameter
        real               :: editing_percent_valid       ! process parameter
        real               :: smoothing_percent_valid     ! process parameter
        character(len=60)  :: replace_nils_before         ! process parameter
        character(len=60)  :: replace_nils_after          ! process parameter
        character(len=40)  :: replace_nils_velcon         ! process parameter
        character(len=8)   :: trace_sample_unit           ! process parameter
        real               :: max_interp_length_before    ! process parameter
        real               :: max_interp_length_after     ! process parameter
        real               :: max_interp_length_velcon    ! process parameter
        logical            :: interp_weights_before       ! process parameter
        logical            :: interp_weights_after        ! process parameter

        integer            :: num_thresh                  ! process parameter
        real,pointer       :: thresh_time(:)              ! process parameter
        real,pointer       :: rms_ampl_thresh(:)          ! process parameter
        real,pointer       :: max_ampl_thresh(:)          ! process parameter
        real,pointer       :: gather_semb_thresh(:)       ! process parameter
        real,pointer       :: struct_semb_thresh(:)       ! process parameter

        integer            :: num_stdev                   ! process parameter
        real,pointer       :: stdev_time(:)               ! process parameter
        real,pointer       :: stdev(:)                    ! process parameter

        integer            :: num_vav                     ! process parameter
        real,pointer       :: vav_time(:)                 ! process parameter
        real,pointer       :: vav_min_percent(:)          ! process parameter
        real,pointer       :: vav_max_percent(:)          ! process parameter

        integer            :: num_vint                    ! process parameter
        real,pointer       :: vint_time(:)                ! process parameter
        real,pointer       :: vint_min_percent(:)         ! process parameter
        real,pointer       :: vint_max_percent(:)         ! process parameter

        integer            :: nt,nx,ny,ngather,mid

        integer            :: kounti              ! input gather counter
        integer            :: kounto              ! output gather counter
        integer            :: max_interp_points_before
        integer            :: max_interp_points_after
        integer            :: max_interp_points_velcon

        integer            :: itr_input_beta      ! input trace index
        integer            :: itr_input_xdip      ! input trace index
        integer            :: itr_input_ydip      ! input trace index
        integer            :: itr_rms_ampl        ! input trace index
        integer            :: itr_max_ampl        ! input trace index
        integer            :: itr_gather_semb     ! input trace index
        integer            :: itr_struct_semb     ! input trace index
        integer            :: itr_input_vav       ! input trace index

        integer            :: itr_process_beta    ! processing trace index
        integer            :: itr_process_xdip    ! processing trace index
        integer            :: itr_process_ydip    ! processing trace index
        integer            :: itr_process_weight  ! processing trace index
        integer            :: itr_process_vav     ! processing trace index

        integer            :: itr_output_beta     ! output trace index
        integer            :: itr_output_xdip     ! output trace index
        integer            :: itr_output_ydip     ! output trace index
        integer            :: itr_output_weight   ! output trace index
        integer            :: itr_output_gamma    ! output trace index
        integer            :: itr_output_vav      ! output trace index

        real,pointer                    :: rms_ampl_fillout(:)
        real,pointer                    :: max_ampl_fillout(:)
        real,pointer                    :: gather_semb_fillout(:)
        real,pointer                    :: struct_semb_fillout(:)
        real,pointer                    :: stdev_fillout(:)
        real,pointer                    :: vav_min_fillout(:)
        real,pointer                    :: vav_max_fillout(:)
        real,pointer                    :: vint_min_fillout(:)
        real,pointer                    :: vint_max_fillout(:)
        real,pointer                    :: dweights(:)
        real,pointer                    :: tweights(:)

        logical                         :: need_beta  
        logical                         :: need_xdip  
        logical                         :: need_ydip  
        logical                         :: need_vav  
        logical                         :: need_rms_ampl  
        logical                         :: need_max_ampl 
        logical                         :: need_gather_semb 
        logical                         :: need_struct_semb

        logical                         :: use_or_output_beta
        logical                         :: use_or_output_xdip
        logical                         :: use_or_output_ydip
        logical                         :: use_or_output_weight
        logical                         :: use_or_output_vav

        logical                         :: apply_any_threshhold
        type(multgather_struct),pointer :: multgather

        double precision,pointer        :: hdsave(:,:,:)   ! for parallel
        real            ,pointer        :: trsave(:,:,:)   ! for parallel
        integer                         :: counter_saved   ! for parallel
        integer                         :: ntr_saved       ! for parallel
        logical                         :: no_more_in      ! for parallel

      end type ka_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                ,save :: lunprint  ! unit number for printing.
      type(ka_struct),pointer,save :: object    ! needed for traps.

      character(30),parameter,private :: SUFFIX1 = ' must be input'
      character(30),parameter,private :: SUFFIX2 = ' will be output'

      character(10),save,private :: MSG_ID_OUTPUT_BETA   ! output ident.
      character(10),save,private :: MSG_ID_OUTPUT_XDIP   ! output ident.
      character(10),save,private :: MSG_ID_OUTPUT_YDIP   ! output ident.
      character(10),save,private :: MSG_ID_WEIGHT        ! output ident.
      character(10),save,private :: MSG_ID_GAMMA         ! output ident.
      character(10),save,private :: MSG_ID_OUTPUT_VAV    ! output ident.

      character(80),save,private :: MSG_INPUT_BETA       ! input message.
      character(80),save,private :: MSG_INPUT_XDIP       ! input message.
      character(80),save,private :: MSG_INPUT_YDIP       ! input message.
      character(80),save,private :: MSG_RMS_AMPL         ! input message.
      character(80),save,private :: MSG_MAX_AMPL         ! input message.
      character(80),save,private :: MSG_GATHER_SEMB      ! input message.
      character(80),save,private :: MSG_STRUCT_SEMB      ! input message.
      character(80),save,private :: MSG_INPUT_VAV        ! input message.

      character(80),save,private :: MSG_OUTPUT_BETA      ! output message.
      character(80),save,private :: MSG_OUTPUT_XDIP      ! output message.
      character(80),save,private :: MSG_OUTPUT_YDIP      ! output message.
      character(80),save,private :: MSG_WEIGHT           ! output message.
      character(80),save,private :: MSG_GAMMA            ! output message.
      character(80),save,private :: MSG_OUTPUT_VAV       ! output message.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine ka_create (obj)
      type(ka_struct),pointer :: obj       ! arguments
      integer                 :: ierr      ! for error checking

      MSG_ID_OUTPUT_BETA = abrakadabra_idstring (ABRAKADABRA_OUTPUT_BETA)
      MSG_ID_OUTPUT_XDIP = abrakadabra_idstring (ABRAKADABRA_OUTPUT_XDIP)
      MSG_ID_OUTPUT_YDIP = abrakadabra_idstring (ABRAKADABRA_OUTPUT_YDIP)
      MSG_ID_WEIGHT      = abrakadabra_idstring (ABRAKADABRA_WEIGHT     )
      MSG_ID_GAMMA       = abrakadabra_idstring (ABRAKADABRA_GAMMA      )
      MSG_ID_OUTPUT_VAV  = abrakadabra_idstring (ABRAKADABRA_OUTPUT_VAV )

      MSG_INPUT_BETA     = abrakadabra_message  (ABRAKADABRA_INPUT_BETA )
      MSG_INPUT_XDIP     = abrakadabra_message  (ABRAKADABRA_INPUT_XDIP )
      MSG_INPUT_YDIP     = abrakadabra_message  (ABRAKADABRA_INPUT_YDIP )
      MSG_RMS_AMPL       = abrakadabra_message  (ABRAKADABRA_RMS_AMPL   ) 
      MSG_MAX_AMPL       = abrakadabra_message  (ABRAKADABRA_MAX_AMPL   )
      MSG_GATHER_SEMB    = abrakadabra_message  (ABRAKADABRA_GATHER_SEMB)
      MSG_STRUCT_SEMB    = abrakadabra_message  (ABRAKADABRA_STRUCT_SEMB)
      MSG_INPUT_VAV      = abrakadabra_message  (ABRAKADABRA_INPUT_VAV  )

      MSG_OUTPUT_BETA    = abrakadabra_message  (ABRAKADABRA_INPUT_BETA )
      MSG_OUTPUT_XDIP    = abrakadabra_message  (ABRAKADABRA_INPUT_XDIP )
      MSG_OUTPUT_YDIP    = abrakadabra_message  (ABRAKADABRA_INPUT_YDIP )
      MSG_WEIGHT         = abrakadabra_message  (ABRAKADABRA_WEIGHT     )  
      MSG_GAMMA          = abrakadabra_message  (ABRAKADABRA_GAMMA      )    
      MSG_OUTPUT_VAV     = abrakadabra_message  (ABRAKADABRA_OUTPUT_VAV )

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
           call pc_error ("Unable to allocate obj in ka_create")
           return
      end if

      nullify (obj%thresh_time        )
      nullify (obj%rms_ampl_thresh    )
      nullify (obj%max_ampl_thresh    )
      nullify (obj%gather_semb_thresh )
      nullify (obj%struct_semb_thresh )

      nullify (obj%stdev_time         )
      nullify (obj%stdev              )

      nullify (obj%vav_time           )
      nullify (obj%vav_min_percent    )
      nullify (obj%vav_max_percent    )

      nullify (obj%vint_time          )
      nullify (obj%vint_min_percent   )
      nullify (obj%vint_max_percent   )

      nullify (obj%multgather)

      nullify (obj%rms_ampl_fillout   )
      nullify (obj%max_ampl_fillout   )
      nullify (obj%gather_semb_fillout)
      nullify (obj%struct_semb_fillout)
      nullify (obj%stdev_fillout      )
      nullify (obj%vav_min_fillout    )
      nullify (obj%vav_max_fillout    )
      nullify (obj%vint_min_fillout   )
      nullify (obj%vint_max_fillout   )
      nullify (obj%dweights           )
      nullify (obj%tweights           )

      nullify (obj%hdsave)    ! for parallel
      nullify (obj%trsave)    ! for parallel

!!! The above nullify statements must precede the memman_nullify calls
!!! for the Portland Group Compiler.

      call memman_nullify (obj%thresh_time        , "ka_thresh_time")
      call memman_nullify (obj%rms_ampl_thresh    , "ka_rms_ampl_thresh")
      call memman_nullify (obj%max_ampl_thresh    , "ka_max_ampl_thresh")
      call memman_nullify (obj%gather_semb_thresh , "ka_gather_semb_thresh")
      call memman_nullify (obj%struct_semb_thresh , "ka_struct_semb_thresh")

      call memman_nullify (obj%stdev_time         , "ka_stdev_time")
      call memman_nullify (obj%stdev              , "ka_stdev")

      call memman_nullify (obj%vav_time           , "ka_vav_time")
      call memman_nullify (obj%vav_min_percent    , "ka_vav_min_percent")
      call memman_nullify (obj%vav_max_percent    , "ka_vav_max_percent")

      call memman_nullify (obj%vint_time          , "ka_vint_time")
      call memman_nullify (obj%vint_min_percent   , "ka_vint_min_percent")
      call memman_nullify (obj%vint_max_percent   , "ka_vint_max_percent")

      nullify (obj%multgather)

      call memman_nullify (obj%rms_ampl_fillout   , "ka_rms_ampl_fillout")
      call memman_nullify (obj%max_ampl_fillout   , "ka_max_ampl_fillout")
      call memman_nullify (obj%gather_semb_fillout, "ka_gather_semb_fillout")
      call memman_nullify (obj%struct_semb_fillout, "ka_struct_semb_fillout")
      call memman_nullify (obj%stdev_fillout      , "ka_stdev_fillout")
      call memman_nullify (obj%vav_min_fillout    , "ka_vav_min_fillout")
      call memman_nullify (obj%vav_max_fillout    , "ka_vav_max_fillout")
      call memman_nullify (obj%vint_min_fillout   , "ka_vint_min_fillout")
      call memman_nullify (obj%vint_max_fillout   , "ka_vint_max_fillout")
      call memman_nullify (obj%dweights           , "ka_dweights")
      call memman_nullify (obj%tweights           , "ka_tweights")

      call memman_nullify (obj%hdsave, "ka_hdsave")   ! for parallel
      call memman_nullify (obj%trsave, "ka_trsave")   ! for parallel

      call ka_initialize (obj)
      end subroutine ka_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ka_delete (obj)
      type(ka_struct),pointer :: obj            ! arguments
      integer                 :: ierr           ! for error checking

      call ka_wrapup (obj)

      call memman_free (obj%thresh_time)
      call memman_free (obj%rms_ampl_thresh)
      call memman_free (obj%max_ampl_thresh)
      call memman_free (obj%gather_semb_thresh)
      call memman_free (obj%struct_semb_thresh)

      call memman_free (obj%stdev_time)
      call memman_free (obj%stdev)

      call memman_free (obj%vav_time)
      call memman_free (obj%vav_min_percent)
      call memman_free (obj%vav_max_percent)

      call memman_free (obj%vint_time)
      call memman_free (obj%vint_min_percent)
      call memman_free (obj%vint_max_percent)

      call multgather_delete (obj%multgather)

      call memman_free (obj%rms_ampl_fillout   )
      call memman_free (obj%max_ampl_fillout   )
      call memman_free (obj%gather_semb_fillout)
      call memman_free (obj%struct_semb_fillout)
      call memman_free (obj%stdev_fillout      )
      call memman_free (obj%vav_min_fillout    )
      call memman_free (obj%vav_max_fillout    )
      call memman_free (obj%vint_min_fillout   )
      call memman_free (obj%vint_max_fillout   )
      call memman_free (obj%dweights           )
      call memman_free (obj%tweights           )

      call memman_free (obj%hdsave)   ! for parallel
      call memman_free (obj%trsave)   ! for parallel

      deallocate(obj, stat=ierr)
      if (ierr /= 0) &
             call pc_warning ("error deallocating obj in ka_delete")
      end subroutine ka_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine ka_initialize (obj)
      type(ka_struct),intent(inout) :: obj            ! arguments
      integer                       :: ierr           ! local

      obj%apply_rms_ampl_thresh      = .true.
      obj%apply_max_ampl_thresh      = .false.
      obj%apply_gather_semb_thresh   = .false.
      obj%apply_struct_semb_thresh   = .true.
      obj%apply_vav_constraints      = .false.
      obj%apply_vint_constraints     = .false.

      obj%num_cpus                   = 1
      obj%hdr_ident                  = 48
      obj%hdr_flattening             = 0
      obj%hdr_x                      = 7
      obj%hdr_y                      = 8
      obj%x_init                     = 1.0
      obj%y_init                     = 1.0
      obj%x_inc                      = 1.0
      obj%y_inc                      = 1.0
      obj%max_x_bins                 = INIL

      obj%num_tr_x                   = 2
      obj%num_tr_y                   = 2
      obj%win_len                    = 0.1

      obj%pass_through               = .true.
      obj%output_beta                = .true.
      obj%output_xdip                = .false.
      obj%output_ydip                = .false.
      obj%output_weight              = .true.
      obj%output_gamma               = .false.
      obj%output_vav                 = .false.

      obj%use_weighted_edit          = .false.
      obj%remove_bad_values          = .true.
      obj%apply_smoothing            = .true.
      obj%use_rms_ampl_weighting     = .true.
      obj%use_max_ampl_weighting     = .false.
      obj%use_gather_semb_weighting  = .false.
      obj%use_struct_semb_weighting  = .true.
      obj%use_weighted_smoothing     = .false.
      obj%use_distance_smoothing     = .false.
      obj%use_tapered_smoothing      = .false.

      obj%center_weight_percent      = KADABRA_CENTER_PERCENT
      obj%tapered_weight_percent     = KADABRA_TAPERED_PERCENT
      obj%editing_percent_valid      = KADABRA_PERCENT_VALID
      obj%smoothing_percent_valid    = KADABRA_PERCENT_VALID
      obj%replace_nils_before        = 'do not replace nils'
      obj%replace_nils_after         = 'do not replace nils'
      obj%replace_nils_velcon        = 'do not replace nils'
      obj%trace_sample_unit          = 'depth'
      obj%max_interp_length_before   = 0.0
      obj%max_interp_length_after    = 0.0
      obj%max_interp_length_velcon   = 0.0
      obj%interp_weights_before      = .false.
      obj%interp_weights_after       = .false.

      call memman_allocate (obj%thresh_time        , 1, ierr)
      call memman_allocate (obj%rms_ampl_thresh    , 1, ierr)
      call memman_allocate (obj%max_ampl_thresh    , 1, ierr)
      call memman_allocate (obj%gather_semb_thresh , 1, ierr)
      call memman_allocate (obj%struct_semb_thresh , 1, ierr)

      call memman_allocate (obj%stdev_time         , 1, ierr)
      call memman_allocate (obj%stdev              , 1, ierr)

      call memman_allocate (obj%vav_time           , 1, ierr)
      call memman_allocate (obj%vav_min_percent    , 1, ierr)
      call memman_allocate (obj%vav_max_percent    , 1, ierr)

      call memman_allocate (obj%vint_time          , 1, ierr)
      call memman_allocate (obj%vint_min_percent   , 1, ierr)
      call memman_allocate (obj%vint_max_percent   , 1, ierr)

      obj%thresh_time        = 0.0
      obj%rms_ampl_thresh    = KADABRA_RMS_AMPL_THRESH
      obj%max_ampl_thresh    = KADABRA_MAX_AMPL_THRESH
      obj%gather_semb_thresh = KADABRA_SEMB_THRESH
      obj%struct_semb_thresh = KADABRA_SEMB_THRESH

      obj%stdev_time         = 0.0
      obj%stdev              = KADABRA_STDEV

      obj%vav_time           = 0.0
      obj%vav_min_percent    = KADABRA_MIN_PERCENT
      obj%vav_max_percent    = KADABRA_MAX_PERCENT

      obj%vint_time          = 0.0
      obj%vint_min_percent   = KADABRA_MIN_PERCENT
      obj%vint_max_percent   = KADABRA_MAX_PERCENT

      obj%num_thresh         = 1
      obj%num_stdev          = 1
      obj%num_vav            = 1
      obj%num_vint           = 1

      call ka_update (obj)
      end subroutine ka_initialize


!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!


      subroutine ka_update (obj)
      type(ka_struct),intent(inout),target :: obj             ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      call ka_update_read    (obj)
      call ka_update_verify  (obj)
      call ka_update_write   (obj)
      call ka_update_prepare (obj)

      end subroutine ka_update


!!------------------------- update read ------------------------------------!!
!!------------------------- update read ------------------------------------!!
!!------------------------- update read ------------------------------------!!


      subroutine ka_update_read (obj)
      type(ka_struct),intent(inout) :: obj                 ! arguments

!!!!!!!!!!!!!!! register array names:

      call pc_register_array_names ('THRESH_TIME_ARRAYSET',       &
                                         (/'THRESH_TIME       ',  &
                                           'RMS_AMPL_THRESH   ',  &
                                           'MAX_AMPL_THRESH   ',  &
                                           'GATHER_SEMB_THRESH',  &
                                           'STRUCT_SEMB_THRESH'/))

      call pc_register_array_names ('STDEV_TIME_ARRAYSET',        &
                                               (/'STDEV_TIME  ',  &
                                                 'STDEV       '/))

      call pc_register_array_names ('VAV_TIME_ARRAYSET',          &
                                            (/'VAV_TIME       ',  &
                                              'VAV_MIN_PERCENT',  &
                                              'VAV_MAX_PERCENT'/))

      call pc_register_array_names ('VINT_TIME_ARRAYSET',          &
                                            (/'VINT_TIME       ',  &
                                              'VINT_MIN_PERCENT',  &
                                              'VINT_MAX_PERCENT'/))

!!!!!!!!!!!!!!! get global parameters:

      call pc_get_global ('numtr'   , obj%numtr_input)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('num_cpus', obj%num_cpus)

      obj%tstop = obj%tstrt + (obj%ndpt - 1) * obj%dt

!!!!!!!!!!!!!!! get scalar process parameters:

      call pc_get ('APPLY_RMS_AMPL_thresh    ', obj%apply_rms_ampl_thresh)
      call pc_get ('APPLY_MAX_AMPL_thresh    ', obj%apply_max_ampl_thresh)
      call pc_get ('APPLY_GATHER_SEMB_thresh ', obj%apply_gather_semb_thresh)
      call pc_get ('APPLY_STRUCT_SEMB_thresh ', obj%apply_struct_semb_thresh)
      call pc_get ('apply_vav_constraints    ', obj%apply_vav_constraints)
      call pc_get ('apply_vint_constraints   ', obj%apply_vint_constraints)

      call pc_get ('HDR_IDENT                ', obj%hdr_ident)
      call pc_get ('hdr_flattening           ', obj%hdr_flattening)
      call pc_get ('HDR_X                    ', obj%hdr_x)
      call pc_get ('HDR_Y                    ', obj%hdr_y)
      call pc_get ('X_INIT                   ', obj%x_init)
      call pc_get ('Y_INIT                   ', obj%y_init)
      call pc_get ('X_INC                    ', obj%x_inc)
      call pc_get ('Y_INC                    ', obj%y_inc)
      call pc_get ('MAX_X_BINS               ', obj%max_x_bins)

      call pc_get ('NUM_TR_X                 ', obj%num_tr_x)
      call pc_get ('NUM_TR_Y                 ', obj%num_tr_y)
      call pc_get ('WIN_LEN                  ', obj%win_len)

      call pc_get ('pass_through             ', obj%pass_through)
      call pc_get ('OUTPUT_beta              ', obj%output_beta)
      call pc_get ('OUTPUT_xdip              ', obj%output_xdip)
      call pc_get ('OUTPUT_ydip              ', obj%output_ydip)
      call pc_get ('OUTPUT_WEIGHT            ', obj%output_weight)
      call pc_get ('OUTPUT_gamma             ', obj%output_gamma)
      call pc_get ('OUTPUT_VAV               ', obj%output_vav)

      call pc_get ('USE_WEIGHTED_EDIT        ', obj%use_weighted_edit)
      call pc_get ('REMOVE_BAD_VALUES        ', obj%remove_bad_values)
      call pc_get ('apply_smoothing          ', obj%apply_smoothing)
      call pc_get ('USE_RMS_AMPL_WEIGHTING   ', obj%use_rms_ampl_weighting)
      call pc_get ('USE_MAX_AMPL_WEIGHTING   ', obj%use_max_ampl_weighting)
      call pc_get ('USE_GATHER_SEMB_WEIGHTING', obj%use_gather_semb_weighting)
      call pc_get ('USE_STRUCT_SEMB_WEIGHTING', obj%use_struct_semb_weighting)
      call pc_get ('USE_WEIGHTED_SMOOTHING   ', obj%use_weighted_smoothing)
      call pc_get ('USE_DISTANCE_SMOOTHING   ', obj%use_distance_smoothing)
      call pc_get ('USE_tapered_SMOOTHING    ', obj%use_tapered_smoothing)

      call pc_get ('center_weight_percent    ', obj%center_weight_percent)
      call pc_get ('tapered_weight_percent   ', obj%tapered_weight_percent)
      call pc_get ('editing_PERCENT_VALID    ', obj%editing_percent_valid)
      call pc_get ('smoothing_PERCENT_VALID  ', obj%smoothing_percent_valid)
      call pc_get ('max_interp_length_before ', obj%max_interp_length_before)
      call pc_get ('max_interp_length_after  ', obj%max_interp_length_after)
      call pc_get ('max_interp_length_velcon ', obj%max_interp_length_velcon)
      call pc_get ('interp_weights_before    ', obj%interp_weights_before)
      call pc_get ('interp_weights_after     ', obj%interp_weights_after)

!!!!!!!!!!!!!!! get array process parameters:

 call kadabra_get ('thresh_TIME       ', obj%thresh_time       , obj%num_thresh)
 call kadabra_get ('RMS_AMPL_THRESH   ', obj%rms_ampl_thresh   , obj%num_thresh)
 call kadabra_get ('MAX_AMPL_THRESH   ', obj%max_ampl_thresh   , obj%num_thresh)
 call kadabra_get ('GATHER_SEMB_THRESH', obj%gather_semb_thresh, obj%num_thresh)
 call kadabra_get ('STRUCT_SEMB_THRESH', obj%struct_semb_thresh, obj%num_thresh)

 call kadabra_get ('STDEV_TIME      ', obj%stdev_time   , obj%num_stdev)
 call kadabra_get ('STDEV           ', obj%stdev        , obj%num_stdev)

 call kadabra_get ('vav_time        ', obj%vav_time       , obj%num_vav)
 call kadabra_get ('vav_min_percent ', obj%vav_min_percent, obj%num_vav)
 call kadabra_get ('vav_max_percent ', obj%vav_max_percent, obj%num_vav)

 call kadabra_get ('vint_time       ', obj%vint_time       , obj%num_vint)
 call kadabra_get ('vint_min_percent', obj%vint_min_percent, obj%num_vint)
 call kadabra_get ('vint_max_percent', obj%vint_max_percent, obj%num_vint)

!!!!!!!!!!!!!!! get and put combo box parameters:

      call kadabra_get_put_options &
                           ('REPLACE_NILS_BEFORE',obj%replace_nils_before, &
                          (/'use linear interpolation                  ',  &
                            'use scaled interpolation                  ',  &
                            'linear interpolation between weight peaks ',  &
                            'scaled interpolation between weight peaks ',  &
                            'replace nils with zeroes                  ',  &
                            'do not replace nils                       '/))

      call kadabra_get_put_options &
                           ('REPLACE_NILS_AFTER',obj%replace_nils_after,   &
                          (/'use linear interpolation                  ',  &
                            'use scaled interpolation                  ',  &
                            'linear interpolation between weight peaks ',  &
                            'scaled interpolation between weight peaks ',  &
                            'replace nils with zeroes                  ',  &
                            'do not replace nils                       '/))

      call kadabra_get_put_options &
                           ('REPLACE_NILS_VELCON',obj%replace_nils_velcon, &
                          (/'use linear interpolation                  ',  &
                            'use scaled interpolation                  ',  &
                            'linear interpolation between weight peaks ',  &
                            'scaled interpolation between weight peaks ',  &
                            'replace nils with zeroes                  ',  &
                            'do not replace nils                       '/))

      call kadabra_get_put_options &
                           ('trace_sample_unit',obj%trace_sample_unit,   &
                                                       (/'depth ',       &
                                                         'time  '/))

      end subroutine ka_update_read


!!------------------------- update verify -----------------------------------!!
!!------------------------- update verify -----------------------------------!!
!!------------------------- update verify -----------------------------------!!


      subroutine ka_update_verify (obj)
      type(ka_struct),intent(inout) :: obj                   ! arguments
      real                          :: tlength               ! local
      integer                       :: kount                 ! local
      logical                       :: need_vel_constraints  ! local

!!!!!!!!!!!!!!! general verifications:

      obj%apply_any_threshhold = obj%apply_rms_ampl_thresh    .or. &
                                 obj%apply_max_ampl_thresh    .or. &
                                 obj%apply_gather_semb_thresh .or. &
                                 obj%apply_struct_semb_thresh

      if (pc_verify_end() .and. obj%numtr_input <= 1) then
           call pc_warning ('ONLY ONE TRACE IS BEING INPUT AT A TIME.')
           call pc_warning ('TRACES MUST BE GATHERED BY CMP.')
           call pc_warning ('Everything is OK if you intend to have &
                                     &only one trace per gather.')
           call pc_warning ('Otherwise, you must insert a GATHER process.')
      end if

      if (obj%win_len <= 0.0) then
           call pc_warning ('WIN_LEN MUST BE > ZERO - reset to 0.1')
           call pc_warning ('WIN_LEN is on the 3D Analysis Window screen')
           obj%win_len = 0.1
      end if
 
      tlength = obj%tstop - obj%tstrt

      call mth_constrain (obj%win_len                 , obj%dt, tlength, obj%dt)
      call mth_constrain (obj%num_tr_x                ,      0,      99)
      call mth_constrain (obj%num_tr_y                ,      0,      99)
      call mth_constrain (obj%center_weight_percent   ,    0.0,   100.0)
      call mth_constrain (obj%editing_percent_valid   ,    0.0,   100.0)
      call mth_constrain (obj%smoothing_percent_valid ,    0.0,   100.0)
      call mth_constrain (obj%max_interp_length_before,    0.0, tlength, obj%dt)
      call mth_constrain (obj%max_interp_length_after ,    0.0, tlength, obj%dt)

      if (obj%hdr_ident <= 0 .or. obj%hdr_ident > obj%nwih) then
           call pc_warning ('Bad HDR_IDENT number - reset to 48')
           call pc_warning ('HDR_IDENT is on the Input and Output screen')
           obj%hdr_ident = 48
      end if

      if (obj%hdr_flattening < 0 .or. obj%hdr_flattening > obj%nwih) then
           call pc_warning ('Bad HDR_FLATTENING number - reset to 0')
           call pc_warning ('HDR_FLATTENING is on the Input and Output screen')
           obj%hdr_flattening = 0
      end if

      if (obj%hdr_x <= 0 .or. obj%hdr_x > obj%nwih) then
           call pc_warning ('Bad HDR_X number - reset to 7')
           call pc_warning ('HDR_X is on the 3D Data Volume screen')
           obj%hdr_x = 7
      end if

      if (obj%hdr_y <= 0 .or. obj%hdr_y > obj%nwih) then
           call pc_warning ('Bad HDR_Y number - reset to 8')
           call pc_warning ('HDR_Y is on the 3D Data Volume screen')
           obj%hdr_y = 8
      end if

      if (obj%x_inc <= 0.0) then
           call pc_warning ('X_INC must be > zero - reset to 1.0')
           call pc_warning ('X_INC is on the 3D Data Volume screen')
           obj%x_inc = 1.0
      end if

      if (obj%y_inc <= 0.0) then
           call pc_warning ('Y_INC must be > zero - reset to 1.0')
           call pc_warning ('Y_INC is on the 3D Data Volume screen')
           obj%y_inc = 1.0
      end if

      if (obj%max_x_bins == INIL .or. obj%max_x_bins <= 0) then
        obj%max_x_bins = INIL
        if (pc_verify_end()) then
          call pc_error('MAX_X_BINS must be specified on 3D Data Volume screen')
        end if
      end if

      obj%nt      = nint(obj%win_len / obj%dt) + 1
      obj%nx      = 2 * obj%num_tr_x + 1
      obj%ny      = 2 * obj%num_tr_y + 1
      obj%ngather = obj%nx * obj%ny
      obj%mid     = (obj%ngather + 1) / 2

      if (pc_verify_end()) then
           if (obj%apply_vav_constraints .and. obj%num_vav == 0) then
                call pc_error ('The VAV_TIME arrayset must be specified&
                               & on the Velocity Constraint Editing screen')
           end if
           if (obj%apply_vint_constraints .and. obj%num_vint == 0) then
                call pc_error ('The VINT_TIME arrayset must be specified&
                               & on the Velocity Constraint Editing screen')
           end if
           if (obj%apply_any_threshhold .and. obj%num_thresh == 0) then
                call pc_error ('The THRESH_TIME arrayset must be specified&
                               & on the Threshhold Editing screen')
           end if
           if (obj%remove_bad_values .and. obj%num_stdev == 0) then
                call pc_error ('The STDEV_TIME arrayset must be specified&
                               & on the Anomaly Editing screen')
           end if
      end if

!!!!!!!!!!!!!!! array verifications:

      call ka_verify_times (obj,obj%num_thresh,obj%thresh_time, &
                                 'THRESH_TIME','Threshhold Editing')
      call ka_verify_times (obj,obj%num_vav,obj%vav_time, &
                                 'VAV_TIME','Velocity Constraint Editing')
      call ka_verify_times (obj,obj%num_vint,obj%vint_time, &
                                 'VINT_TIME','Velocity Constraint Editing')
      call ka_verify_times (obj,obj%num_stdev,obj%stdev_time, &
                                 'STDEV_TIME','Anomaly Editing')

      call kadabra_verify_minmax &
                     (obj%num_vav ,obj%vav_min_percent ,obj%vav_max_percent)
      call kadabra_verify_minmax &
                     (obj%num_vint,obj%vint_min_percent,obj%vint_max_percent)

      call kadabra_verify_stdev (obj%num_stdev,obj%stdev)

      call kadabra_verify_thresh &
               (obj%num_thresh,obj%rms_ampl_thresh, KADABRA_RMS_AMPL_THRESH)
      call kadabra_verify_thresh &
               (obj%num_thresh,obj%max_ampl_thresh, KADABRA_MAX_AMPL_THRESH)
      call kadabra_verify_thresh &
               (obj%num_thresh,obj%gather_semb_thresh, KADABRA_SEMB_THRESH)
      call kadabra_verify_thresh &
               (obj%num_thresh,obj%struct_semb_thresh, KADABRA_SEMB_THRESH)

!!!!!!!!!!!!!!! learn the input trace needs:

      need_vel_constraints = obj%output_beta .and. &
                (obj%apply_vav_constraints .or. obj%apply_vint_constraints)

      obj%need_vav         = obj%output_vav .or. need_vel_constraints
      obj%need_beta        = obj%output_beta .or. obj%output_vav .or. &
                                                  obj%output_gamma
      obj%need_xdip        = obj%output_xdip
      obj%need_ydip        = obj%output_ydip
      obj%need_rms_ampl    = obj%use_rms_ampl_weighting    .or. &
                             obj%apply_rms_ampl_thresh
      obj%need_max_ampl    = obj%use_max_ampl_weighting    .or. &
                             obj%apply_max_ampl_thresh
      obj%need_gather_semb = obj%use_gather_semb_weighting .or. &
                             obj%apply_gather_semb_thresh
      obj%need_struct_semb = obj%use_struct_semb_weighting .or. &
                             obj%apply_struct_semb_thresh

      if (pc_verify_end()) then
        kount = 0
        if(obj%need_beta       ) kount = kount + 1
        if(obj%need_xdip       ) kount = kount + 1
        if(obj%need_ydip       ) kount = kount + 1
        if(obj%need_vav        ) kount = kount + 1
        if(obj%need_rms_ampl   ) kount = kount + 1
        if(obj%need_max_ampl   ) kount = kount + 1
        if(obj%need_gather_semb) kount = kount + 1
        if(obj%need_struct_semb) kount = kount + 1
        if (kount > obj%numtr_input) then
          call pc_error ('Not enough input traces for the specified parameters')
          call pc_error ('Actual number of input traces =',obj%numtr_input)
          call pc_error ('Minimum required number of input traces =',kount)
          call pc_error ('Please make adjustments on Input and Output screen,')
          call pc_error ('and other places, to reduce your requirements,')
          call pc_error ('or arrange to input the required traces.')
        end if
      end if

!!!!!!!!!!!!!!! learn the processing and output trace needs for multgather:

      obj%use_or_output_weight = obj%output_weight .or.                   &
          ( (obj%use_weighted_edit .or. obj%use_weighted_smoothing) .and. &
            (obj%output_beta .or. obj%output_xdip .or. obj%output_ydip) )

      obj%use_or_output_beta  = obj%output_beta .or. obj%output_vav .or. &
                                                     obj%output_gamma
      obj%use_or_output_xdip  = obj%output_xdip
      obj%use_or_output_ydip  = obj%output_ydip
      obj%use_or_output_vav   = obj%output_vav .or. need_vel_constraints

!!!!!!!!!!!! get number of input traces being passed to multgather and output:

      if (obj%pass_through) then
           obj%numtr_in_and_out = obj%numtr_input
      else
           obj%numtr_in_and_out = 0
      end if

!!!!!!!!!!!!!!! get processing trace indices:

      obj%itr_process_beta   = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_process_xdip   = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_process_ydip   = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_process_weight = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_process_vav    = 1    ! this is 1 instead of 0 deliberately.

      obj%numtr_process = obj%numtr_in_and_out

      if (obj%use_or_output_beta) then
           obj%numtr_process      = obj%numtr_process + 1
           obj%itr_process_beta   = obj%numtr_process
      end if

      if (obj%use_or_output_xdip) then
           obj%numtr_process      = obj%numtr_process + 1
           obj%itr_process_xdip   = obj%numtr_process
      end if

      if (obj%use_or_output_ydip) then
           obj%numtr_process      = obj%numtr_process + 1
           obj%itr_process_ydip   = obj%numtr_process
      end if

      if (obj%use_or_output_weight) then
           obj%numtr_process      = obj%numtr_process + 1
           obj%itr_process_weight = obj%numtr_process
      end if

      if (obj%use_or_output_vav) then
           obj%numtr_process      = obj%numtr_process + 1
           obj%itr_process_vav    = obj%numtr_process
      end if

!!!!!!!!!!!!!!! get output trace indices:

      obj%itr_output_beta   = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_output_xdip   = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_output_ydip   = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_output_weight = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_output_gamma  = 1    ! this is 1 instead of 0 deliberately.
      obj%itr_output_vav    = 1    ! this is 1 instead of 0 deliberately.

      obj%numtr_output  = obj%numtr_in_and_out

      if (obj%output_beta) then
           obj%numtr_output      = obj%numtr_output + 1
           obj%itr_output_beta   = obj%numtr_output
      end if

      if (obj%output_xdip) then
           obj%numtr_output      = obj%numtr_output + 1
           obj%itr_output_xdip   = obj%numtr_output
      end if

      if (obj%output_ydip) then
           obj%numtr_output      = obj%numtr_output + 1
           obj%itr_output_ydip   = obj%numtr_output
      end if

      if (obj%output_weight) then
           obj%numtr_output      = obj%numtr_output + 1
           obj%itr_output_weight = obj%numtr_output
      end if

      if (obj%output_gamma) then
           obj%numtr_output      = obj%numtr_output + 1
           obj%itr_output_gamma  = obj%numtr_output
      end if

      if (obj%output_vav) then
           obj%numtr_output      = obj%numtr_output + 1
           obj%itr_output_vav    = obj%numtr_output
      end if

      end subroutine ka_update_verify


!!------------------------- update write -----------------------------------!!
!!------------------------- update write -----------------------------------!!
!!------------------------- update write -----------------------------------!!


      subroutine ka_update_write (obj)
      type(ka_struct),intent(inout) :: obj                    ! arguments
      integer                       :: nscratch,nstore        ! local
      logical                       :: use_any_weighting      ! local
      logical                       :: do_editing             ! local
      character(50) :: msg_in,msg_out                         ! local
      character(30) :: msg_o_beta,msg_o_xdip                  ! local
      character(30) :: msg_o_ydip,msg_o_weight,msg_o_gamma    ! local
      character(30) :: msg_o_vav                              ! local
      character(70) :: msg01,msg02,msg03,msg04,msg05,msg06    ! local
      character(70) :: msg07,msg08                            ! local
      character(70) :: msgx1,msgx2,msgx3,msgx4,msgx5,msgx6    ! local

!!!!!!!!!!!!!!! put global parameters:
      !!! This will allocate enough memory in pcpsx so that the main job
      !!! will not run out of space when combining results from other PE's. 
      !!! wmm 2/14/2006
      !!! since boss does not execute as a worker, we don't need his
      !!! cpu to be counted in the output numtr array size. wmm
      call pc_put_global ('numtr', max(1,(obj%num_cpus-1))*obj%numtr_output)

!!!!!!!!!!!!!!! put scalar process parameters:

      call pc_put ('APPLY_RMS_AMPL_thresh    ', obj%apply_rms_ampl_thresh)
      call pc_put ('APPLY_MAX_AMPL_thresh    ', obj%apply_max_ampl_thresh)
      call pc_put ('APPLY_GATHER_SEMB_thresh ', obj%apply_gather_semb_thresh)
      call pc_put ('APPLY_STRUCT_SEMB_thresh ', obj%apply_struct_semb_thresh)
      call pc_put ('apply_vav_constraints    ', obj%apply_vav_constraints)
      call pc_put ('apply_vint_constraints   ', obj%apply_vint_constraints)

      call pc_put ('NUM_CPUS                 ', obj%num_cpus)
      call pc_put ('HDR_IDENT                ', obj%hdr_ident)
      call pc_put ('hdr_flattening           ', obj%hdr_flattening)
      call pc_put ('HDR_X                    ', obj%hdr_x)
      call pc_put ('HDR_Y                    ', obj%hdr_y)
      call pc_put ('X_INIT                   ', obj%x_init)
      call pc_put ('Y_INIT                   ', obj%y_init)
      call pc_put ('X_INC                    ', obj%x_inc)
      call pc_put ('Y_INC                    ', obj%y_inc)
      call pc_put ('MAX_X_BINS               ', obj%max_x_bins)

      call pc_put ('NUM_TR_X                 ', obj%num_tr_x)
      call pc_put ('NUM_TR_Y                 ', obj%num_tr_y)
      call pc_put ('WIN_LEN                  ', obj%win_len, ndec=4)

      call pc_put ('pass_through             ', obj%pass_through)
      call pc_put ('OUTPUT_beta              ', obj%output_beta)
      call pc_put ('OUTPUT_xdip              ', obj%output_xdip)
      call pc_put ('OUTPUT_ydip              ', obj%output_ydip)
      call pc_put ('OUTPUT_WEIGHT            ', obj%output_weight)
      call pc_put ('OUTPUT_gamma             ', obj%output_gamma)
      call pc_put ('OUTPUT_VAV               ', obj%output_vav)

      call pc_put ('USE_WEIGHTED_EDIT        ', obj%use_weighted_edit)
      call pc_put ('REMOVE_BAD_VALUES        ', obj%remove_bad_values)
      call pc_put ('apply_smoothing          ', obj%apply_smoothing)
      call pc_put ('USE_RMS_AMPL_WEIGHTING   ', obj%use_rms_ampl_weighting)
      call pc_put ('USE_MAX_AMPL_WEIGHTING   ', obj%use_max_ampl_weighting)
      call pc_put ('USE_GATHER_SEMB_WEIGHTING', obj%use_gather_semb_weighting)
      call pc_put ('USE_STRUCT_SEMB_WEIGHTING', obj%use_struct_semb_weighting)
      call pc_put ('USE_WEIGHTED_SMOOTHING   ', obj%use_weighted_smoothing)
      call pc_put ('USE_DISTANCE_SMOOTHING   ', obj%use_distance_smoothing)
      call pc_put ('USE_tapered_SMOOTHING    ', obj%use_tapered_smoothing)

      call pc_put ('center_weight_percent    ', obj%center_weight_percent)
      call pc_put ('tapered_weight_percent   ', obj%tapered_weight_percent)
      call pc_put ('editing_PERCENT_VALID    ', obj%editing_percent_valid)
      call pc_put ('smoothing_PERCENT_VALID  ', obj%smoothing_percent_valid)
      call pc_put ('max_interp_length_before ', obj%max_interp_length_before)
      call pc_put ('max_interp_length_after  ', obj%max_interp_length_after)
      call pc_put ('max_interp_length_velcon ', obj%max_interp_length_velcon)
      call pc_put ('interp_weights_before    ', obj%interp_weights_before)
      call pc_put ('interp_weights_after     ', obj%interp_weights_after)

!!!!!!!!!!!!!!! put array process parameters:

    call pc_put ('thresh_TIME       ', obj%thresh_time       , obj%num_thresh)
    call pc_put ('RMS_AMPL_THRESH   ', obj%rms_ampl_thresh   , obj%num_thresh)
    call pc_put ('MAX_AMPL_THRESH   ', obj%max_ampl_thresh   , obj%num_thresh)
    call pc_put ('GATHER_SEMB_THRESH', obj%gather_semb_thresh, obj%num_thresh)
    call pc_put ('STRUCT_SEMB_THRESH', obj%struct_semb_thresh, obj%num_thresh)

      call pc_put ('STDEV_TIME        ', obj%stdev_time   , obj%num_stdev)
      call pc_put ('STDEV             ', obj%stdev        , obj%num_stdev)

      call pc_put ('vav_time          ', obj%vav_time        , obj%num_vav)
      call pc_put ('vav_min_percent   ', obj%vav_min_percent , obj%num_vav)
      call pc_put ('vav_max_percent   ', obj%vav_max_percent , obj%num_vav)

      call pc_put ('vint_time         ', obj%vint_time        , obj%num_vint)
      call pc_put ('vint_min_percent  ', obj%vint_min_percent , obj%num_vint)
      call pc_put ('vint_max_percent  ', obj%vint_max_percent , obj%num_vint)

      !!!! these minsize calls do not appear to work:

      call pc_put_minsize_arrayset ('thresh_time_arrayset' , 1)
      call pc_put_minsize_arrayset ('stdev_time_arrayset'  , 1)
      call pc_put_minsize_arrayset ('vav_time_arrayset'    , 1)
      call pc_put_minsize_arrayset ('vint_time_arrayset'   , 1)

!!!!!!!!!!!!!!! prepare messages:

      msg_o_beta   = ' '
      msg_o_xdip   = ' '
      msg_o_ydip   = ' '
      msg_o_weight = ' '
      msg_o_gamma  = ' '
      msg_o_vav    = ' '

      msg_in  = trim(string_ii2ss(obj%numtr_input     ))//' input traces'
      msg_out = trim(string_ii2ss(obj%numtr_in_and_out))//' input traces' &
                //'  plus  '// &
                trim(string_ii2ss(obj%numtr_output - obj%numtr_in_and_out))// &
                                                     ' new output traces'
      msg01 = ' '  
      msg02 = ' '   
      msg03 = ' '    
      msg04 = ' '
      msg05 = ' ' 
      msg06 = ' '  
      msg07 = ' '
      msg08 = ' '

      msgx1 = ' '  
      msgx2 = ' '   
      msgx3 = ' '    
      msgx4 = ' '
      msgx5 = ' ' 
      msgx6 = ' ' 

      if (obj%output_beta  ) msg_o_beta   = MSG_ID_OUTPUT_BETA
      if (obj%output_xdip  ) msg_o_xdip   = MSG_ID_OUTPUT_XDIP
      if (obj%output_ydip  ) msg_o_ydip   = MSG_ID_OUTPUT_YDIP
      if (obj%output_weight) msg_o_weight = MSG_ID_WEIGHT
      if (obj%output_gamma ) msg_o_gamma  = MSG_ID_GAMMA 
      if (obj%output_vav   ) msg_o_vav    = MSG_ID_OUTPUT_VAV

      if (obj%need_beta)        msg01 = trim(MSG_INPUT_BETA )//SUFFIX1
      if (obj%need_xdip)        msg02 = trim(MSG_INPUT_XDIP )//SUFFIX1
      if (obj%need_ydip)        msg03 = trim(MSG_INPUT_YDIP )//SUFFIX1
      if (obj%need_rms_ampl)    msg04 = trim(MSG_RMS_AMPL   )//SUFFIX1
      if (obj%need_max_ampl)    msg05 = trim(MSG_MAX_AMPL   )//SUFFIX1
      if (obj%need_gather_semb) msg06 = trim(MSG_GATHER_SEMB)//SUFFIX1
      if (obj%need_struct_semb) msg07 = trim(MSG_STRUCT_SEMB)//SUFFIX1
      if (obj%need_vav)         msg08 = trim(MSG_INPUT_VAV  )//SUFFIX1

      if (obj%output_beta  ) msgx1 = trim(MSG_OUTPUT_BETA)//SUFFIX2
      if (obj%output_xdip  ) msgx2 = trim(MSG_OUTPUT_XDIP)//SUFFIX2
      if (obj%output_ydip  ) msgx3 = trim(MSG_OUTPUT_YDIP)//SUFFIX2
      if (obj%output_weight) msgx4 = trim(MSG_WEIGHT     )//SUFFIX2
      if (obj%output_gamma ) msgx5 = trim(MSG_GAMMA      )//SUFFIX2
      if (obj%output_vav   ) msgx6 = trim(MSG_OUTPUT_VAV )//SUFFIX2

!!!!!!!!!!!!!!! put gui only parameters:

      call pc_put_gui_only ('msg_o_beta          ', msg_o_beta)
      call pc_put_gui_only ('msg_o_xdip          ', msg_o_xdip)
      call pc_put_gui_only ('msg_o_ydip          ', msg_o_ydip)
      call pc_put_gui_only ('msg_o_weight        ', msg_o_weight)
      call pc_put_gui_only ('msg_o_gamma         ', msg_o_gamma)
      call pc_put_gui_only ('msg_o_vav           ', msg_o_vav)
      call pc_put_gui_only ('msg_in              ', msg_in)
      call pc_put_gui_only ('msg_out             ', msg_out)
      call pc_put_gui_only ('MSG01               ', msg01)
      call pc_put_gui_only ('MSG02               ', msg02)
      call pc_put_gui_only ('MSG03               ', msg03)
      call pc_put_gui_only ('MSG04               ', msg04)
      call pc_put_gui_only ('MSG05               ', msg05)
      call pc_put_gui_only ('MSG06               ', msg06)
      call pc_put_gui_only ('MSG07               ', msg07)
      call pc_put_gui_only ('MSG08               ', msg08)
      call pc_put_gui_only ('NT                  ', obj%nt)
      call pc_put_gui_only ('NX                  ', obj%nx)
      call pc_put_gui_only ('NY                  ', obj%ny)

!!!!!!!!!!!!!!! print messages:

                        call pc_print ('------------------------')
                        call pc_print (msg_in)
                        call pc_print (msg_out)
                        call pc_print ('------------------------')
      if (msg01 /= ' ') call pc_print (msg01)
      if (msg02 /= ' ') call pc_print (msg02)
      if (msg03 /= ' ') call pc_print (msg03)
      if (msg04 /= ' ') call pc_print (msg04)
      if (msg05 /= ' ') call pc_print (msg05)
      if (msg06 /= ' ') call pc_print (msg06)
      if (msg07 /= ' ') call pc_print (msg07)
      if (msg08 /= ' ') call pc_print (msg08)
                        call pc_print ('------------------------')
      if (msgx1 /= ' ') call pc_print (msgx1)
      if (msgx2 /= ' ') call pc_print (msgx2)
      if (msgx3 /= ' ') call pc_print (msgx3)
      if (msgx4 /= ' ') call pc_print (msgx4)
      if (msgx5 /= ' ') call pc_print (msgx5)
      if (msgx6 /= ' ') call pc_print (msgx6)
                        call pc_print ('------------------------')

!!!!!!!!!!!!!!! put control parameters:

      nscratch = multgather_scratch                                    &
                 (obj%numtr_process, obj%nwih,obj%ndpt,obj%nx,obj%ny)  &
                 + obj%numtr_process * obj%ngather * (obj%nwih + obj%ndpt)
      nstore   = multgather_store                                      &
                 (obj%numtr_process, obj%nwih,obj%ndpt,obj%nx,obj%ny,  &
                  obj%max_x_bins)

      call pc_put_control ('need_request'           , .true.)
      call pc_put_control ('need_label'             , .true.)
      call pc_put_control ('twosets'                , .true.)
      call pc_put_control ('nscratch'               , nscratch)
      call pc_put_control ('nstore'                 , nstore)
      call pc_put_control ('parallel_safe'          , .true.)
      call pc_put_control ('PCPS_BOSS_EXEC_MODE'    , 'PCPS_BOSS_DISTRIBUTES')
      call pc_put_control ('PCPS_SEND_MODE'         , 'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_SEND_EOF_MODE'     , 'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_RECEIVE_MODE'      , 'PCPS_RECEIVE_GROUP')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE'  , 'PCPS_RECEIVE_GROUP')
      call pc_put_control ('PCPS_GENERATOR_MODE'    , 'PCPS_TRACE_GEN')
!-wmm-call pc_put_control ('PCPS_BUNCH_TRACE_MODE'  , 'PCPS_BUNCH_TRACE_GROUPS')
    

!!!!!!!!!!!!!!! put sensitivity parameters:

      use_any_weighting = obj%use_rms_ampl_weighting    .or. &
                          obj%use_max_ampl_weighting    .or. &
                          obj%use_gather_semb_weighting .or. &
                          obj%use_struct_semb_weighting

      do_editing = obj%use_or_output_beta .or. &
                   obj%use_or_output_xdip .or. &
                   obj%use_or_output_ydip

      call pc_put_sensitive_arrayset_flag &
                     ('vav_time_arrayset', obj%apply_vav_constraints)
      call pc_put_sensitive_arrayset_flag &
                     ('vint_time_arrayset', obj%apply_vint_constraints)

      call pc_put_sensitive_field_flag ('use_weighted_edit', &
            obj%remove_bad_values .and. use_any_weighting .and. do_editing)
      call pc_put_sensitive_field_flag ('editing_percent_valid', &
            obj%remove_bad_values .and. do_editing)
      call pc_put_sensitive_arrayset_flag ('stdev_time_arrayset', &
            obj%remove_bad_values .and. do_editing)

      call pc_put_sensitive_field_flag ('use_weighted_smoothing', &
            obj%apply_smoothing .and. use_any_weighting .and. do_editing)
      call pc_put_sensitive_field_flag ('use_distance_smoothing', &
            obj%apply_smoothing .and. do_editing)
      call pc_put_sensitive_field_flag ('use_tapered_smoothing', &
            obj%apply_smoothing .and. do_editing)
      call pc_put_sensitive_field_flag ('center_weight_percent', &
            obj%apply_smoothing .and. do_editing .and.           &
            obj%use_distance_smoothing)
      call pc_put_sensitive_field_flag ('tapered_weight_percent', &
            obj%apply_smoothing .and. do_editing .and.           &
            obj%use_tapered_smoothing)
      call pc_put_sensitive_field_flag ('smoothing_percent_valid', &
            obj%apply_smoothing .and. do_editing)

  !   call pc_put_sensitive_field_flag ('max_interp_length_before', &
  !         obj%replace_nils_before(1:4) == 'use')
  !   call pc_put_sensitive_field_flag ('max_interp_length_after', &
  !         obj%replace_nils_after(1:4) == 'use')
  !   call pc_put_sensitive_field_flag ('max_interp_length_velcon', &
  !         obj%replace_nils_velcon(1:4) == 'use')

      call pc_put_sensitive_arrayset_flag &
                        ('thresh_time_arrayset', obj%apply_any_threshhold)

      end subroutine ka_update_write


!!------------------------- update prepare ---------------------------------!!
!!------------------------- update prepare ---------------------------------!!
!!------------------------- update prepare ---------------------------------!!


      subroutine ka_update_prepare (obj)
      type(ka_struct),intent(inout) :: obj                       ! arguments
      integer                       :: ierr                      ! local
      logical                       :: whoops                    ! local

!!!!!!!!!!!!!!! deallocate dependent memory which might be reallocated later:

      call multgather_delete (obj%multgather)

      call memman_deallocate (obj%rms_ampl_fillout   )
      call memman_deallocate (obj%max_ampl_fillout   )
      call memman_deallocate (obj%gather_semb_fillout)
      call memman_deallocate (obj%struct_semb_fillout)
      call memman_deallocate (obj%stdev_fillout      )
      call memman_deallocate (obj%vav_min_fillout    )
      call memman_deallocate (obj%vav_max_fillout    )
      call memman_deallocate (obj%vint_min_fillout   )
      call memman_deallocate (obj%vint_max_fillout   )
      call memman_deallocate (obj%dweights           )
      call memman_deallocate (obj%tweights           )

      call memman_deallocate (obj%hdsave)   ! for parallel
      call memman_deallocate (obj%trsave)   ! for parallel

!!!!!!!!!!!!!!! return if not planning to process traces:

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!!!!!!!!!!!!!!! initialize dependent parameters:

      obj%kounti          = 0    ! input gather counter
      obj%kounto          = 0    ! output gather counter
      obj%itr_input_beta  = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_input_xdip  = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_input_ydip  = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_rms_ampl    = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_max_ampl    = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_gather_semb = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_struct_semb = 1    ! input trace index    (or 1 instead of 0)
      obj%itr_input_vav   = 1    ! input trace index    (or 1 instead of 0)

      obj%max_interp_points_before = nint(obj%max_interp_length_before / obj%dt)
      obj%max_interp_points_after  = nint(obj%max_interp_length_after  / obj%dt)
      obj%max_interp_points_velcon = nint(obj%max_interp_length_velcon / obj%dt)

!!!!!!!!!!!!!!! create primitive objects:

      if (obj%numtr_process > 0) then


           call multgather_create (obj%multgather,lunprint,obj%numtr_process, &
                                   obj%nwih,obj%ndpt,obj%hdr_x,obj%hdr_y,     &
                                   obj%x_init,obj%y_init,obj%x_inc,obj%y_inc, &
                                   obj%nx,obj%ny,obj%max_x_bins,              &
                                   'KA',whoops)
           if (whoops) call pc_error ('error in multgather_create')
      end if

!!!!!!!!!!!!!!! allocate dependent memory:

      call memman_allocate (obj%rms_ampl_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%max_ampl_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%gather_semb_fillout, obj%ndpt, ierr)
      call memman_allocate (obj%struct_semb_fillout, obj%ndpt, ierr)
      call memman_allocate (obj%stdev_fillout      , obj%ndpt, ierr)
      call memman_allocate (obj%vav_min_fillout    , obj%ndpt, ierr)
      call memman_allocate (obj%vav_max_fillout    , obj%ndpt, ierr)
      call memman_allocate (obj%vint_min_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%vint_max_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%dweights        , obj%ngather, ierr)
      call memman_allocate (obj%tweights        , obj%ngather, ierr)

if (pcps_num_workers > 1) then
  call memman_allocate (obj%hdsave,obj%nwih,obj%ngather,obj%numtr_process,ierr)
  call memman_allocate (obj%trsave,obj%ndpt,obj%ngather,obj%numtr_process,ierr)
endif

!!!!!!!!!!!!!!! convert input array parameters to traces:

      call ka_fillout (obj,obj%thresh_time,obj%num_thresh, &
                            obj%rms_ampl_thresh,obj%rms_ampl_fillout)
      call ka_fillout (obj,obj%thresh_time,obj%num_thresh, &
                            obj%max_ampl_thresh,obj%max_ampl_fillout)
      call ka_fillout (obj,obj%thresh_time,obj%num_thresh, &
                            obj%gather_semb_thresh,obj%gather_semb_fillout)
      call ka_fillout (obj,obj%thresh_time,obj%num_thresh, &
                            obj%struct_semb_thresh,obj%struct_semb_fillout)

      call ka_fillout (obj,obj%stdev_time,obj%num_stdev, &
                            obj%stdev,obj%stdev_fillout)

      call ka_fillout (obj,obj%vav_time,obj%num_vav, &
                            obj%vav_min_percent,obj%vav_min_fillout)
      call ka_fillout (obj,obj%vav_time,obj%num_vav, &
                            obj%vav_max_percent,obj%vav_max_fillout)

      call ka_fillout (obj,obj%vint_time,obj%num_vint, &
                            obj%vint_min_percent,obj%vint_min_fillout)
      call ka_fillout (obj,obj%vint_time,obj%num_vint, &
                            obj%vint_max_percent,obj%vint_max_fillout)

      obj%vav_min_fillout = 0.01 * obj%vav_min_fillout
      obj%vav_max_fillout = 0.01 * obj%vav_max_fillout

      obj%vint_min_fillout = 0.01 * obj%vint_min_fillout
      obj%vint_max_fillout = 0.01 * obj%vint_max_fillout

!!!!!!!!!!!!!!!! get distance weights and tapered weights:

      call kadabra_get_distance_weights (obj%use_distance_smoothing,  &
                                         obj%center_weight_percent,   &
                                         obj%kounto,obj%dweights,obj%nx,obj%ny)

      call kadabra_get_tapered_weights  (obj%use_tapered_smoothing,   &
                                         obj%tapered_weight_percent,  &
                                         obj%kounto,obj%tweights,obj%nx,obj%ny)

!!!!!!!!!!!!!!!! prepare for parallel run:

      obj%counter_saved = 0
      obj%ntr_saved     = 0
      obj%no_more_in    = .false.

      end subroutine ka_update_prepare


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- verify times ---------------------------------!!
!!--------------------------- verify times ---------------------------------!!
!!--------------------------- verify times ---------------------------------!!


      subroutine ka_verify_times (obj,num,time,keyword,screen)
      type(ka_struct) ,intent(inout) :: obj                ! arguments
      integer         ,intent(in)    :: num                ! arguments
      real            ,intent(inout) :: time(:)            ! arguments
      character(len=*),intent(in)    :: keyword            ! arguments
      character(len=*),intent(in)    :: screen             ! arguments
      integer                        :: indx               ! local

      do indx = 1,num
        if (time(indx) == FNIL) time(indx) = 0.0
        call mth_constrain (time(indx), obj%tstrt, obj%tstop, obj%dt)
      end do

      if (pc_verify_end()) then
        do indx = 2,num
          if (time(indx) < time(indx-1)) then
            call pc_error (trim(keyword)//' VALUES',indx-1,'and',indx, &
                           'ARE OUT OF ORDER on tab '//screen)
          else if (time(indx) == time(indx-1)) then
            call pc_error (trim(keyword)//' VALUES',indx-1,'and',indx, &
                           'ARE THE SAME on tab '//screen)
          end if
        end do
      end if
      end subroutine ka_verify_times


!!----------------------------- fillout ------------------------------------!!
!!----------------------------- fillout ------------------------------------!!
!!----------------------------- fillout ------------------------------------!!


      subroutine ka_fillout (obj,time,num,ordinates,fillout)
      type(ka_struct) ,intent(inout) :: obj                ! arguments
      real            ,intent(in)    :: time(:)            ! arguments
      integer         ,intent(in)    :: num                ! arguments
      real            ,intent(in)    :: ordinates(:)       ! arguments
      real            ,intent(out)   :: fillout(:)         ! arguments

      call terputil_fastsamp (time,num,ordinates, &
                              obj%tstrt,obj%dt,obj%ndpt,fillout)
      end subroutine ka_fillout


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine ka (obj,ntr,hd1,tr1,hd2,tr2)
      type(ka_struct) ,intent(inout) :: obj                   ! arguments
      integer         ,intent(inout) :: ntr                   ! arguments
      double precision,intent(inout) :: hd1(:,:)               ! arguments
      real            ,intent(inout) :: tr1(:,:)               ! arguments
      double precision,intent(inout) :: hd2(:,:)               ! arguments
      real            ,intent(inout) :: tr2(:,:)               ! arguments
      double precision :: hdi(obj%nwih,obj%numtr_process)             ! local
      real             :: tri(obj%ndpt,obj%numtr_process)             ! local
      double precision :: hdo(obj%nwih,obj%ngather,obj%numtr_process) ! local
      real             :: tro(obj%ndpt,obj%ngather,obj%numtr_process) ! local
      logical          :: need_more                                   ! local

!!!!!!!!!!!!!!! print input traces:

      if (ntr > 0) then
           if (obj%kounti == 0) call ka_print_traces (obj,'input',hd1,ntr)
           obj%kounti = obj%kounti + 1
      end if

!!!!!!!!!!!!!!! pre-process the input traces:

      if (ntr > 0) then
           call ka_indices (obj,hd1,tr1,ntr)

           if (ntr == FATAL_ERROR) then
                call pc_error ('KA: ERROR IN ka_INDICES')
                call ka_wrapup (obj)
                return
           end if

           call ka_pre_process (obj,hd1,tr1,hdi,tri)

      end if

!!!!!!!!!!!!!!! take shortcut if not outputting traces:

      if (obj%numtr_process == 0) then
           if (ntr > 0) then
                ntr = NEED_TRACES
           else if (ntr == NO_MORE_TRACES) then
                call ka_wrapup (obj)
           end if
           return
      end if

!!!!!!!!!!!!!!! get the mix gathers:

      if (ntr > 0) ntr = obj%numtr_process

      if (pcps_num_workers > 1) then
           if (obj%no_more_in) then
                ntr = NO_MORE_TRACES
                goto 200
           end if
           if (ntr == NO_MORE_TRACES) obj%no_more_in = .false.
      end if

100   continue
      !--- data is stored in multgather for later use. It absorbs the data on 
      !--- the first pass through, until enough data has come in,
      !--- then after enough data has come in, it
      !--- begins returning small data cubes in hdo, tro for use below.
      call multgather (obj%multgather,ntr,hdi,tri,hdo,tro)

      if (ntr == FATAL_ERROR) then
           call pc_error ('KA: ERROR IN MULTGATHER')
           call ka_wrapup (obj)
           return
      end if

      if (pcps_num_workers > 1) then
        call ka_parallel_store (obj,ntr,hdo,tro,need_more)
        if (ntr == NEED_TRACES .and. need_more) goto 100
      endif

200   continue

      if (ntr == NEED_TRACES) return

      if (ntr == NO_MORE_TRACES) then
           call ka_wrapup (obj)
           return
      end if

      ! now ntr == obj%numtr_process

      !--- output counter obj%kounto ---  We should have one group output
      !--- for each group input (obj%kounti).
      obj%kounto = obj%kounto + 1

!!!!!!!!!!!!!!! post-process the output traces:

      !--- Here is where the work is done.  hdo, tro are a small
      !--- cube of data that will be worked on to produce averages
      !--- or dips, etc which are loaded into output hd2, tr2.

      call ka_post_process (obj,hdo,tro,hd2,tr2)

      ntr = obj%numtr_output
      !--- this sets the Largest Absolute Value (header word 25) for
      !--- each trace output.
      call lav_set_hdr (hd2,tr2,obj%ndpt,ntr)

!!!!!!!!!!!!!!! print output traces:

      if (obj%kounto == 1) call ka_print_traces (obj,'output',hd2,ntr)


      end subroutine ka


!!-------------------------------- indices --------------------------------!!
!!-------------------------------- indices --------------------------------!!
!!-------------------------------- indices --------------------------------!!


      subroutine ka_indices (obj,hd,tr,ntr)
      type(ka_struct) ,intent(inout) :: obj                ! arguments
      integer         ,intent(inout) :: ntr                ! arguments
      double precision,intent(in)    :: hd(:,:)            ! arguments
      real            ,intent(in)    :: tr(:,:)            ! arguments

!!!!!!!!!!!!!!! verify number of input traces:

      if (ntr <= 0) return

      if (ntr /= obj%numtr_input) then
        call pc_error ('KA: NUMBER OF INPUT TRACES IS',ntr)
        call pc_error &
                ('KA: NUMBER OF INPUT TRACES SHOULD BE',obj%numtr_input)
        ntr = FATAL_ERROR
        return
      end if

!!!!!!!!!!!!!!! get input trace indices:

      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_INPUT_BETA ,obj%need_beta       ,obj%itr_input_beta)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_INPUT_XDIP ,obj%need_xdip       ,obj%itr_input_xdip)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_INPUT_YDIP ,obj%need_ydip       ,obj%itr_input_ydip)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_RMS_AMPL   ,obj%need_rms_ampl   ,obj%itr_rms_ampl)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_MAX_AMPL   ,obj%need_max_ampl   ,obj%itr_max_ampl)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_GATHER_SEMB,obj%need_gather_semb,obj%itr_gather_semb)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_STRUCT_SEMB,obj%need_struct_semb,obj%itr_struct_semb)
      call ka_location &
   (obj,hd,ntr,ABRAKADABRA_INPUT_VAV  ,obj%need_vav        ,obj%itr_input_vav )

 !!!  now ntr might be FATAL_ERROR.

      end subroutine ka_indices


!!---------------------------- print traces ------------------------------!!
!!---------------------------- print traces ------------------------------!!
!!---------------------------- print traces ------------------------------!!


      subroutine ka_print_traces (obj,word,hd,ntr)
      type(ka_struct) ,intent(in) :: obj                        ! arguments
      character(len=*),intent(in) :: word                       ! arguments
      integer         ,intent(in) :: ntr                        ! arguments
      double precision,intent(in) :: hd(:,:)                    ! arguments
      character(len=50)           :: start,msg                  ! local
      integer                     :: itr,ident                  ! local

      start = '------------------KA: '//trim(word)//' trace'
      call pc_print (' ')
      do itr = 1,ntr
        ident = nint(hd(obj%hdr_ident,itr))
        msg = abrakadabra_message(ident)
        call pc_print (start,itr,' has '//msg)
      end do
      call pc_print (' ')

      end subroutine ka_print_traces


!!------------------------- pre process -----------------------------------!!
!!------------------------- pre process -----------------------------------!!
!!------------------------- pre process -----------------------------------!!

       ! hdi and tri must have first dimension == nwih and ndpt.
       ! hdi and tri must have second dimension == numtr_process.


      subroutine ka_pre_process (obj,hd,tr,hdi,tri)
      type(ka_struct) ,intent(inout)      :: obj              ! arguments
      double precision,intent(in) ,target :: hd(:,:)          ! arguments
      real            ,intent(in) ,target :: tr(:,:)          ! arguments
      double precision,intent(inout),target :: hdi(:,:)         ! arguments
      real            ,intent(inout),target :: tri(:,:)         ! arguments

      double precision,pointer       :: input_general_header (:)   ! local
      double precision,pointer       :: input_beta_header    (:)   ! local
      double precision,pointer       :: input_xdip_header    (:)   ! local
      double precision,pointer       :: input_ydip_header    (:)   ! local
      double precision,pointer       :: rms_ampl_header      (:)   ! local
      double precision,pointer       :: max_ampl_header      (:)   ! local
      double precision,pointer       :: gather_semb_header   (:)   ! local
      double precision,pointer       :: struct_semb_header   (:)   ! local
      double precision,pointer       :: input_vav_header     (:)   ! local
      double precision,pointer       :: process_beta_header  (:)   ! local
      double precision,pointer       :: process_xdip_header  (:)   ! local
      double precision,pointer       :: process_ydip_header  (:)   ! local
      double precision,pointer       :: process_weight_header(:)   ! local
      double precision,pointer       :: process_vav_header   (:)   ! local

      real            ,pointer       :: input_beta_trace     (:)   ! local
      real            ,pointer       :: input_xdip_trace     (:)   ! local
      real            ,pointer       :: input_ydip_trace     (:)   ! local
      real            ,pointer       :: rms_ampl_trace       (:)   ! local
      real            ,pointer       :: max_ampl_trace       (:)   ! local
      real            ,pointer       :: gather_semb_trace    (:)   ! local
      real            ,pointer       :: struct_semb_trace    (:)   ! local
      real            ,pointer       :: input_vav_trace      (:)   ! local
      real            ,pointer       :: process_beta_trace   (:)   ! local
      real            ,pointer       :: process_xdip_trace   (:)   ! local
      real            ,pointer       :: process_ydip_trace   (:)   ! local
      real            ,pointer       :: process_weight_trace (:)   ! local
      real            ,pointer       :: process_vav_trace    (:)   ! local

!!!!!!!!!!!!!!! set pointers to needed input and output traces:

      input_general_header  => hd (1:obj%nwih,1)
      input_beta_header     => hd (1:obj%nwih,obj%itr_input_beta    )
      input_xdip_header     => hd (1:obj%nwih,obj%itr_input_xdip    )
      input_ydip_header     => hd (1:obj%nwih,obj%itr_input_ydip    )
      rms_ampl_header       => hd (1:obj%nwih,obj%itr_rms_ampl      )
      max_ampl_header       => hd (1:obj%nwih,obj%itr_max_ampl      )
      gather_semb_header    => hd (1:obj%nwih,obj%itr_gather_semb   )
      struct_semb_header    => hd (1:obj%nwih,obj%itr_struct_semb   )
      input_vav_header      => hd (1:obj%nwih,obj%itr_input_vav     )
      process_beta_header   => hdi(1:obj%nwih,obj%itr_process_beta  )
      process_xdip_header   => hdi(1:obj%nwih,obj%itr_process_xdip  )
      process_ydip_header   => hdi(1:obj%nwih,obj%itr_process_ydip  )
      process_weight_header => hdi(1:obj%nwih,obj%itr_process_weight)
      process_vav_header    => hdi(1:obj%nwih,obj%itr_process_vav   )

      input_beta_trace      => tr (1:obj%ndpt,obj%itr_input_beta    )
      input_xdip_trace      => tr (1:obj%ndpt,obj%itr_input_xdip    )
      input_ydip_trace      => tr (1:obj%ndpt,obj%itr_input_ydip    )
      rms_ampl_trace        => tr (1:obj%ndpt,obj%itr_rms_ampl      )
      max_ampl_trace        => tr (1:obj%ndpt,obj%itr_max_ampl      )
      gather_semb_trace     => tr (1:obj%ndpt,obj%itr_gather_semb   )
      struct_semb_trace     => tr (1:obj%ndpt,obj%itr_struct_semb   )
      input_vav_trace       => tr (1:obj%ndpt,obj%itr_input_vav     )
      process_beta_trace    => tri(1:obj%ndpt,obj%itr_process_beta  )
      process_xdip_trace    => tri(1:obj%ndpt,obj%itr_process_xdip  )
      process_ydip_trace    => tri(1:obj%ndpt,obj%itr_process_ydip  )
      process_weight_trace  => tri(1:obj%ndpt,obj%itr_process_weight)
      process_vav_trace     => tri(1:obj%ndpt,obj%itr_process_vav   )

!!!!!!!!!!!!!!! pass all input traces to arrays for processing:

      if (obj%pass_through) then
           hdi(1:obj%nwih,1:obj%numtr_input) = hd(1:obj%nwih,1:obj%numtr_input)
           tri(1:obj%ndpt,1:obj%numtr_input) = tr(1:obj%ndpt,1:obj%numtr_input)
      end if

!!!!!!!!!!!!!!! initialize additional traces for processing and/or output:

      if (obj%use_or_output_beta) then
           process_beta_header(:) = input_beta_header(:)
           process_beta_trace (:) = input_beta_trace (:)
      end if

      if (obj%use_or_output_xdip) then
           process_xdip_header(:) = input_xdip_header(:)
           process_xdip_trace (:) = input_xdip_trace (:)
      end if

      if (obj%use_or_output_ydip) then
           process_ydip_header(:) = input_ydip_header(:)
           process_ydip_trace (:) = input_ydip_trace (:)
      end if

      if (obj%use_or_output_weight) then
           process_weight_header(:) = input_general_header(:)
           process_weight_trace (:) = 1.0
      end if

      if (obj%use_or_output_vav) then
           process_vav_header(:) = input_vav_header(:)
           process_vav_trace (:) = input_vav_trace (:)
      end if

!!!!!!!!!!!!!!! calculate the weight volume:

      if (obj%use_or_output_weight) then
           call ka_calculate_weights (obj,process_weight_trace,  &
                                          rms_ampl_trace,        &
                                          max_ampl_trace,        &
                                          gather_semb_trace,     &
                                          struct_semb_trace)
      end if

!!!!!!!!!!!!!!! apply threshholds to the attribute volumes:

      if (obj%use_or_output_beta) then
           call ka_apply_threshholds (obj,process_beta_trace, &
                                          rms_ampl_trace,     &
                                          max_ampl_trace,     &
                                          gather_semb_trace,  &
                                          struct_semb_trace)
      end if

      if (obj%use_or_output_xdip) then
           call ka_apply_threshholds (obj,process_xdip_trace, &
                                          rms_ampl_trace,     &
                                          max_ampl_trace,     &
                                          gather_semb_trace,  &
                                          struct_semb_trace)
      end if

      if (obj%use_or_output_ydip) then
           call ka_apply_threshholds (obj,process_ydip_trace, &
                                          rms_ampl_trace,     &
                                          max_ampl_trace,     &
                                          gather_semb_trace,  &
                                          struct_semb_trace)
      end if
      end subroutine ka_pre_process


!!---------------------- calculate weights ----------------------------------!!
!!---------------------- calculate weights ----------------------------------!!
!!---------------------- calculate weights ----------------------------------!!


      subroutine ka_calculate_weights (obj,weight_trace,      &
                                           rms_ampl_trace,    &
                                           max_ampl_trace,    &
                                           gather_semb_trace, &
                                           struct_semb_trace)
      type(ka_struct),intent(inout) :: obj                   ! arguments
      real           ,intent(out)   :: weight_trace     (:)  ! arguments
      real           ,intent(in)    :: rms_ampl_trace   (:)  ! arguments
      real           ,intent(in)    :: max_ampl_trace   (:)  ! arguments
      real           ,intent(in)    :: gather_semb_trace(:)  ! arguments
      real           ,intent(in)    :: struct_semb_trace(:)  ! arguments

      weight_trace = 1.0

      if (obj%use_rms_ampl_weighting) then
        where (rms_ampl_trace > 0 .and. rms_ampl_trace /= FNIL)
          weight_trace = weight_trace * rms_ampl_trace
        elsewhere
          weight_trace = 0.0
        end where
      end if

      if (obj%use_max_ampl_weighting) then
        where (max_ampl_trace /= FNIL)
          weight_trace = weight_trace * abs(max_ampl_trace)
        elsewhere
          weight_trace = 0.0
        end where
      end if

      if (obj%use_gather_semb_weighting) then
        where (gather_semb_trace > 0 .and. gather_semb_trace /= FNIL)
          weight_trace = weight_trace * gather_semb_trace
        elsewhere
          weight_trace = 0.0
        end where
      end if

      if (obj%use_struct_semb_weighting) then
        where (struct_semb_trace > 0 .and. struct_semb_trace /= FNIL)
          weight_trace = weight_trace * struct_semb_trace
        elsewhere
          weight_trace = 0.0
        end where
      end if

      end subroutine ka_calculate_weights


!!------------------------- apply threshholds -------------------------------!!
!!------------------------- apply threshholds -------------------------------!!
!!------------------------- apply threshholds -------------------------------!!


      subroutine ka_apply_threshholds (obj,attribute_trace,   &
                                           rms_ampl_trace,    &
                                           max_ampl_trace,    &
                                           gather_semb_trace, &
                                           struct_semb_trace)
      type(ka_struct),intent(inout) :: obj                   ! arguments
      real           ,intent(inout) :: attribute_trace  (:)  ! arguments
      real           ,intent(in)    :: rms_ampl_trace   (:)  ! arguments
      real           ,intent(in)    :: max_ampl_trace   (:)  ! arguments
      real           ,intent(in)    :: gather_semb_trace(:)  ! arguments
      real           ,intent(in)    :: struct_semb_trace(:)  ! arguments

      if (obj%apply_rms_ampl_thresh) then
           where (rms_ampl_trace == FNIL .or. &
                  rms_ampl_trace < obj%rms_ampl_fillout)
                attribute_trace = FNIL
           end where
      end if

      if (obj%apply_max_ampl_thresh) then
           where (max_ampl_trace == FNIL .or. &
                  abs(max_ampl_trace) < obj%max_ampl_fillout)
                attribute_trace = FNIL
           end where
      end if

      if (obj%apply_gather_semb_thresh) then
           where (gather_semb_trace == FNIL .or. &
                  gather_semb_trace < obj%gather_semb_fillout)
                attribute_trace = FNIL
           end where
      end if

      if (obj%apply_struct_semb_thresh) then
           where (struct_semb_trace == FNIL .or. &
                  struct_semb_trace < obj%struct_semb_fillout)
                attribute_trace = FNIL
           end where
      end if
      end subroutine ka_apply_threshholds


!!----------------------------- post process --------------------------------!!
!!----------------------------- post process --------------------------------!!
!!----------------------------- post process --------------------------------!!


      subroutine ka_post_process (obj,hdo,tro,hd,tr)
      type(ka_struct) ,intent(inout) :: obj                      ! arguments
      double precision,intent(inout) :: hdo(:,:,:)               ! arguments
      real            ,intent(inout) :: tro(:,:,:)               ! arguments
      double precision,intent(out)   :: hd(:,:)                  ! arguments
      real            ,intent(out)   :: tr(:,:)                  ! arguments
      character(len=40)              :: replace_dip_nils_before  ! local
      character(len=40)              :: replace_dip_nils_after   ! local
      integer                        :: num,igather              ! local
      real                           :: gamma(obj%ndpt)          ! local

!!!!!!!!!!!!!!! set dead input traces to nil:

      do num = 1,obj%numtr_process
      do igather = 1,obj%ngather
           if (hdo(25,igather,num) == 0.0) then
                tro(1:obj%ndpt,igather,num) = FNIL
           end if
      end do
      end do

!!!!!!!!!!!!!!! do the editing and smoothing using the entire cube:

      if (obj%replace_nils_before == 'use scaled interpolation') then
         replace_dip_nils_before = 'use linear interpolation'
      else if (obj%replace_nils_before == &
                    'scaled interpolation between weight peaks') then
         replace_dip_nils_before = 'linear interpolation between weight peaks'
      else
         replace_dip_nils_before = obj%replace_nils_before
      end if

      if (obj%replace_nils_after == 'use scaled interpolation') then
         replace_dip_nils_after = 'use linear interpolation'
      else if (obj%replace_nils_after == &
                   'scaled interpolation between weight peaks') then
         replace_dip_nils_after = 'linear interpolation between weight peaks'
      else
         replace_dip_nils_after = obj%replace_nils_after
      end if

      if (obj%use_or_output_beta) then
           call ka_edit_and_smooth                           &
                       (obj,obj%replace_nils_before,         &
                            obj%replace_nils_after,          &
                            hdo(:,:,obj%itr_process_weight), &
                            tro(:,:,obj%itr_process_weight), &
                            tro(:,:,obj%itr_process_beta),.true.)
      end if

      if (obj%use_or_output_xdip) then
           call ka_edit_and_smooth                           &
                       (obj,replace_dip_nils_before,         &
                            replace_dip_nils_after,          &
                            hdo(:,:,obj%itr_process_weight), &
                            tro(:,:,obj%itr_process_weight), &
                            tro(:,:,obj%itr_process_xdip),.false.)
      end if

      if (obj%use_or_output_ydip) then
           call ka_edit_and_smooth                           &
                       (obj,replace_dip_nils_before,         &
                            replace_dip_nils_after,          &
                            hdo(:,:,obj%itr_process_weight), &
                            tro(:,:,obj%itr_process_weight), &
                            tro(:,:,obj%itr_process_ydip),.false.)
      end if

!!!!!!!!!!!!!!! do additional editing of the beta and vav data:

      if (obj%use_or_output_beta .and. obj%use_or_output_vav) then
           call ka_edit_beta_and_vav                             &
                       (obj,tro(:,obj%mid,obj%itr_process_beta), &
                            tro(:,obj%mid,obj%itr_process_vav))
      end if

!!!!!!!!!!!!!!! calculate the gamma trace:

      if (obj%output_gamma) then
           call ka_calculate_gamma                             &
                       (obj,tro(:,obj%mid,obj%itr_process_beta),gamma)
      end if

!!!!!!!!!!!!!!! pass all input traces to output arrays:

      if (obj%pass_through) then
           hd(1:obj%nwih,1:obj%numtr_input) = hdo(:,obj%mid,1:obj%numtr_input)
           tr(1:obj%ndpt,1:obj%numtr_input) = tro(:,obj%mid,1:obj%numtr_input)
      end if

!!!!!!!!!!!!!!! return required output traces:

      if (obj%output_beta) then
 hd(1:obj%nwih   ,obj%itr_output_beta)   = hdo(:,obj%mid,obj%itr_process_beta)
 tr(1:obj%ndpt   ,obj%itr_output_beta)   = tro(:,obj%mid,obj%itr_process_beta)
 hd(obj%hdr_ident,obj%itr_output_beta)   = ABRAKADABRA_OUTPUT_BETA
      end if

      if (obj%output_xdip) then
 hd(1:obj%nwih   ,obj%itr_output_xdip)   = hdo(:,obj%mid,obj%itr_process_xdip)
 tr(1:obj%ndpt   ,obj%itr_output_xdip)   = tro(:,obj%mid,obj%itr_process_xdip)
 hd(obj%hdr_ident,obj%itr_output_xdip)   = ABRAKADABRA_OUTPUT_XDIP
      end if

      if (obj%output_ydip) then
 hd(1:obj%nwih   ,obj%itr_output_ydip)   = hdo(:,obj%mid,obj%itr_process_ydip)
 tr(1:obj%ndpt   ,obj%itr_output_ydip)   = tro(:,obj%mid,obj%itr_process_ydip)
 hd(obj%hdr_ident,obj%itr_output_ydip)   = ABRAKADABRA_OUTPUT_YDIP
      end if

      if (obj%output_weight) then
 hd(1:obj%nwih   ,obj%itr_output_weight) = hdo(:,obj%mid,obj%itr_process_weight)
 tr(1:obj%ndpt   ,obj%itr_output_weight) = tro(:,obj%mid,obj%itr_process_weight)
 hd(obj%hdr_ident,obj%itr_output_weight) = ABRAKADABRA_WEIGHT
      end if

      if (obj%output_gamma) then
 hd(1:obj%nwih   ,obj%itr_output_gamma) = hdo(:,obj%mid,obj%itr_process_beta)
 tr(1:obj%ndpt   ,obj%itr_output_gamma) = gamma(:)
 hd(obj%hdr_ident,obj%itr_output_gamma) = ABRAKADABRA_GAMMA
      end if

      if (obj%output_vav) then
 hd(1:obj%nwih   ,obj%itr_output_vav)    = hdo(:,obj%mid,obj%itr_process_vav)
 tr(1:obj%ndpt   ,obj%itr_output_vav)    = tro(:,obj%mid,obj%itr_process_vav)
 hd(obj%hdr_ident,obj%itr_output_vav)    = ABRAKADABRA_OUTPUT_VAV
      end if

      end subroutine ka_post_process


!!----------------------------- edit and smooth -----------------------------!!
!!----------------------------- edit and smooth -----------------------------!!
!!----------------------------- edit and smooth -----------------------------!!

               !!!!! only the center of the output attribute cube 
               !!!!! should be used after calling this routine.


      subroutine ka_edit_and_smooth &
                                (obj,replace_nils_before,replace_nils_after, &
                                 header_cube,weight_cube,attribute_cube,beta)
      type(ka_struct) ,intent(inout) :: obj                      ! arguments
      character(len=*),intent(in)    :: replace_nils_before      ! arguments
      character(len=*),intent(in)    :: replace_nils_after       ! arguments
      double precision,intent(in)    :: header_cube   (:,:)      ! arguments
      real            ,intent(inout) :: weight_cube   (:,:)      ! arguments
      real            ,intent(inout) :: attribute_cube(:,:)      ! arguments
      logical         ,intent(in)    :: beta                     ! arguments
      integer                        :: igather                  ! local
      real        :: flat_weight_cube   (obj%ndpt,obj%ngather)   ! local
      real        :: flat_stdev_fillout (obj%ndpt)               ! local

!!!!!!!!!!!!! flatten the weight and attribute cubes (and stdev_fillout array):

      flat_weight_cube   = weight_cube
      flat_stdev_fillout = obj%stdev_fillout

      do igather = 1,obj%ngather
           call ka_flatten &
               (obj,.true.,header_cube(:,igather),flat_weight_cube(:,igather))

           call ka_flatten &
               (obj,.true.,header_cube(:,igather),attribute_cube(:,igather))
      end do

      call ka_flatten &
               (obj,.true.,header_cube(:,obj%mid), flat_stdev_fillout)

!!!!!!!!!!!!!!! remove anomalous values from the attribute cube:

      if (obj%remove_bad_values) then
           call ka_edit &
                    (obj,flat_weight_cube,attribute_cube,flat_stdev_fillout)
      end if

!!!!!!!!!!!!!!! remove impossible values from the beta attribute cube:

      if (beta) then
           where (attribute_cube <= -1.0) attribute_cube = FNIL
      end if

!!!!!!!!!!!!!!! replace nils in attribute cube before smoothing:
!!!!!!!!!!!!!!! (might also edit the weight cube)

      do igather = 1,obj%ngather
           call ka_replace_nils (obj,replace_nils_before,      &
                                 obj%max_interp_points_before, &
                                 obj%interp_weights_before,    &
                                 weight_cube(:,igather),       &
                                 attribute_cube(:,igather))
      end do

!!!!!!!!!!!!!!! smooth the attribute cube (and put smoothed result in center):

      if (obj%apply_smoothing) then
           call ka_smooth (obj,flat_weight_cube,attribute_cube)
      end if

!!!!!!!!!!!!!!! replace nils in (center of) attribute cube after smoothing:
!!!!!!!!!!!!!!! (might also edit the center of the weight cube)

      call ka_replace_nils (obj,replace_nils_after,      &
                            obj%max_interp_points_after, &
                            obj%interp_weights_after,    &
                            weight_cube(:,obj%mid),      &
                            attribute_cube(:,obj%mid))

!!!!!!!!!!!!!!! unflatten (center of) attribute cube:

      call ka_flatten &
                (obj,.false.,header_cube(:,obj%mid),attribute_cube(:,obj%mid))

      end subroutine ka_edit_and_smooth


!!----------------------- edit beta and vav ---------------------------------!!
!!----------------------- edit beta and vav ---------------------------------!!
!!----------------------- edit beta and vav ---------------------------------!!


      subroutine ka_edit_beta_and_vav (obj,beta_trace,vav_trace)
      type(ka_struct),intent(inout) :: obj                          ! arguments
      real           ,intent(inout) :: beta_trace  (:)              ! arguments
      real           ,intent(inout) :: vav_trace   (:)              ! arguments
      real                          :: input_vav_trace  (obj%ndpt)  ! local
      real                          :: input_vint_trace (obj%ndpt)  ! local
      real                          :: output_vav_trace (obj%ndpt)  ! local
      real                          :: output_vint_trace(obj%ndpt)  ! local

!!!!!!!!!!!!!!! replace impossible input values with nil:

      where (beta_trace <= -1.0) beta_trace = FNIL
      where (vav_trace  <=  0.0) vav_trace  = FNIL

      input_vav_trace  = vav_trace
      output_vav_trace = vav_trace

!!!!!!!!!!!!!!! get input interval velocities from input average velocities:

      if (obj%apply_vint_constraints) then
           call ka_vint_from_vav (obj,input_vav_trace,input_vint_trace)
      end if

!!!!!!!!!!!!!!! edit the average velocities using the edited BETA trace:

      where (input_vav_trace /= FNIL .and. beta_trace /= FNIL)
           output_vav_trace = input_vav_trace / sqrt(beta_trace + 1.0)
      end where

!!!!!!!!!!!!!!! get output interval velocities from output average velocities:

      if (obj%apply_vint_constraints) then
           call ka_vint_from_vav (obj,output_vav_trace,output_vint_trace)
      end if

!!!!!!!!!!!!!!! apply velocity contraints to the edited BETA volume:

      if (obj%apply_vav_constraints) then
        where (output_vav_trace == FNIL .or. input_vav_trace == FNIL  .or. &
               output_vav_trace < obj%vav_min_fillout*input_vav_trace .or. &
               output_vav_trace > obj%vav_max_fillout*input_vav_trace)
             beta_trace = FNIL
        end where
      end if

      if (obj%apply_vint_constraints) then
        where (output_vint_trace == FNIL .or. input_vint_trace == FNIL   .or. &
               output_vint_trace < obj%vint_min_fillout*input_vint_trace .or. &
               output_vint_trace > obj%vint_max_fillout*input_vint_trace)
             beta_trace = FNIL
        end where
      end if

!!!!!!!!!!!!!!! optionally replace nil values in the new edited BETA trace:

      call ka_replace_nils (obj,obj%replace_nils_velcon,  &
                            obj%max_interp_points_velcon, &
                            .false.,         & ! interp_weights always false
                            beta_trace,      & ! weight cube (not used)
                            beta_trace)

!!!!!!!!!!!!!!! edit average velocities again using the new edited BETA:

      where (input_vav_trace /= FNIL .and. beta_trace /= FNIL)
           vav_trace = input_vav_trace / sqrt(beta_trace + 1.0)
      end where

!!!!!!!!!!!!!!! always replace nil values in new edited average velocity trace:

      call terputil_replace_nils (vav_trace, obj%ndpt, TERPUTIL_FLAT)

      end subroutine ka_edit_beta_and_vav


!!---------------------------- vint from vav --------------------------------!!
!!---------------------------- vint from vav --------------------------------!!
!!---------------------------- vint from vav --------------------------------!!

                 ! input and output can contain nils.


      subroutine ka_vint_from_vav (obj,vav_trace,vint_trace)
      type(ka_struct),intent(inout) :: obj                       ! arguments
      real           ,intent(in)    :: vav_trace (:)             ! arguments
      real           ,intent(out)   :: vint_trace(:)             ! arguments
      integer                       :: indx                      ! local
      real                          :: temp_vav_trace(obj%ndpt)  ! local
      real                          :: reciprocal                ! local

      temp_vav_trace = vav_trace
      where (temp_vav_trace <= 0.0) temp_vav_trace = FNIL

      call terputil_replace_nils (temp_vav_trace, obj%ndpt, TERPUTIL_FLAT)

!!!!!!!!!!!!!!! do the following if trace sample interval is constant in time:

      if (obj%trace_sample_unit == 'time') then
           vint_trace(1) = temp_vav_trace(1)
           do indx = 2,obj%ndpt
                vint_trace(indx) = indx * temp_vav_trace(indx) &
                                      - (indx-1) * temp_vav_trace(indx-1)
           end do

!!!!!!!!!!!!!!! do the following if trace sample interval is constant in depth:

      else
           vint_trace(1) = temp_vav_trace(1)
           do indx = 2,obj%ndpt
                reciprocal = indx / temp_vav_trace(indx) &
                                      - (indx-1) / temp_vav_trace(indx-1)
                if (reciprocal > 0.0) then
                     vint_trace(indx) = 1.0 / reciprocal
                else
                     vint_trace(indx) = FNIL
                end if
           end do
      end if

      where (vint_trace     <=  0.0) vint_trace = FNIL
      where (temp_vav_trace == FNIL) vint_trace = FNIL

      end subroutine ka_vint_from_vav


!!------------------------------- flatten ---------------------------------!!
!!------------------------------- flatten ---------------------------------!!
!!------------------------------- flatten ---------------------------------!!


      subroutine ka_flatten (obj,pre,hd,tr)
      type(ka_struct) ,intent(inout) :: obj                ! arguments
      logical         ,intent(in)    :: pre                ! arguments
      double precision,intent(in)    :: hd(:)              ! arguments
      real            ,intent(inout) :: tr(:)              ! arguments
      real                           :: temp(obj%ndpt)     ! local
      real                           :: flattening         ! local
      integer                        :: ishift,indx,indx2  ! local

      if (obj%hdr_flattening == 0) return

      flattening = hd(obj%hdr_flattening)
      call mth_constrain (flattening, obj%tstrt, obj%tstop, obj%dt)

      ishift = nint((flattening - obj%tstrt) / obj%dt)
      if (ishift == 0) return
      if (pre) ishift = -ishift

      do indx = 1,obj%ndpt
           indx2 = indx - ishift
           if (indx2 >= 1 .and. indx2 <= obj%ndpt) then
                temp(indx) = tr(indx2)
           else
                temp(indx) = FNIL
           end if
      end do

      tr(1:obj%ndpt) = temp(:)

      end subroutine ka_flatten


!!----------------------------- location -----------------------------------!!
!!----------------------------- location -----------------------------------!!
!!----------------------------- location -----------------------------------!!

          ! if obj%kounti == one:
          ! sets itr_choice to the index of the required trace.
          ! sets itr_choice to 1 (instead of 0) if trace not found.
          ! sets ntr to FATAL_ERROR if trace is needed but not found.
          ! otherwise does not set ntr.

          ! if obj%kounti > one:
          ! makes sure that itr_choice does not have to change.
          ! sets ntr to FATAL_ERROR if itr_choice needs to change.
          ! otherwise does not set ntr.


      subroutine ka_location (obj,hd,ntr,id_choice,need_choice,itr_choice)
      type(ka_struct) ,intent(in)    :: obj             ! arguments
      integer         ,intent(inout) :: ntr             ! arguments
      double precision,intent(in)    :: hd(:,:)         ! arguments
      integer         ,intent(in)    :: id_choice       ! arguments
      logical         ,intent(in)    :: need_choice     ! arguments
      integer         ,intent(inout) :: itr_choice      ! arguments
      integer                        :: itr,ident       ! local
      character(len=50)              :: msg             ! local

!!!!!!!!!!!!!!! return right away if an error has already occurred:

      if (ntr == FATAL_ERROR) return

!!!!!!!!!!!!!!! get itr_choice if this is the first group of traces received:

      if (obj%kounti == 1) then
           do itr = 1,ntr
                ident = nint(hd(obj%hdr_ident,itr))
                if (ident == id_choice) then
                     itr_choice = itr
                     return
                end if
           end do
           if (need_choice) then
                msg = abrakadabra_message(id_choice)
                call pc_error ('KA: REQUIRED INPUT TRACE',msg,'NOT FOUND')
                ntr = FATAL_ERROR
           end if
           itr_choice = 1    ! this is 1 instead of 0 deliberately.
           return
      end if

!!!!!!!!!!!!!!! verify itr_choice if this is a subsequent group of traces:

      if (.not.need_choice) then
           continue
      else if (itr_choice > ntr) then
           msg = abrakadabra_message(id_choice)
           call pc_error ('KA: INPUT TRACE',msg,'NOT WHERE EXPECTED')
           ntr = FATAL_ERROR
      else if (hd(obj%hdr_ident,itr_choice) /= id_choice) then
           msg = abrakadabra_message(id_choice)
           call pc_error ('KA: INPUT TRACE',msg,'NOT WHERE EXPECTED')
           ntr = FATAL_ERROR
      end if
      end subroutine ka_location


!!------------------------------- edit -----------------------------------!!
!!------------------------------- edit -----------------------------------!!
!!------------------------------- edit -----------------------------------!!

              ! anomalous values based on standard deviation
              ! are removed from the entire attribute_cube.


      subroutine ka_edit (obj,weight_cube,attribute_cube,stdev_fillout)
      type(ka_struct),intent(in)    :: obj                          ! arguments
      real           ,intent(in)    :: weight_cube   (:,:)          ! arguments
      real           ,intent(inout) :: attribute_cube(:,:)          ! arguments
      real           ,intent(in)    :: stdev_fillout (:)            ! arguments
      integer                       :: indx,indx2,igather           ! local
      real                          :: edited(obj%ndpt,obj%ngather) ! local
      integer                       :: ia,ib,nhalf,nhalf2           ! local
      integer                       :: total,kount                  ! local
      real                          :: esum2,esum,wsum,weight       ! local
      real                          :: fraction,average,stdev       ! local

!!!!!!!!!!!!!!! calculate edited array:

      edited(:,:) = attribute_cube(1:obj%ndpt,1:obj%ngather)

      fraction = 0.01 * obj%editing_percent_valid
      nhalf    = obj%nt/2

      do indx = 1,obj%ndpt
           nhalf2 = min(indx-1,nhalf)
           ia = indx - nhalf2
           ib = indx + nhalf2
           ia = max(ia,1)
           ib = min(ib,obj%ndpt)
           esum2 = 0.0
           esum  = 0.0
           wsum  = 0.0
           kount = 0
           total = 0
           do indx2 = ia,ib
                do igather = 1,obj%ngather
                     total  = total  + 1
                     if (attribute_cube(indx2,igather) == FNIL) cycle
                     if (obj%use_weighted_edit) then
                          weight = weight_cube(indx2,igather)
                     else
                          weight = 1.0
                     end if
                     if (weight == FNIL) cycle
                     esum2  = esum2 + weight * attribute_cube(indx2,igather)**2
                     esum   = esum  + weight * attribute_cube(indx2,igather)
                     wsum   = wsum  + weight
                     kount  = kount + 1
                end do
           end do
           if (kount < fraction * total) then
                edited(indx,1:obj%ngather) = FNIL
           else if (wsum == 0.0) then
                edited(indx,1:obj%ngather) = FNIL
           else if (kount == 0) then
                edited(indx,1:obj%ngather) = FNIL
           else
                average = esum / wsum
                stdev   = sqrt(esum2 / wsum)
                do igather = 1,obj%ngather
                     if (abs(attribute_cube(indx,igather) - average) &
                                    > stdev * stdev_fillout(indx)) then
                          edited(indx,igather) = FNIL
                     end if
                end do
           end if
      end do

!!!!!!!!!!!!!!! save edited array in attribute cube:

      attribute_cube(1:obj%ndpt,1:obj%ngather) = edited(:,:)

      end subroutine ka_edit


!!------------------------------- smooth -----------------------------------!!
!!------------------------------- smooth -----------------------------------!!
!!------------------------------- smooth -----------------------------------!!

                  ! only the middle trace in the output
                  ! attribute_cube contains the smoothed values.


      subroutine ka_smooth (obj,weight_cube,attribute_cube)
      type(ka_struct),intent(in)    :: obj                   ! arguments
      real           ,intent(in)    :: weight_cube   (:,:)   ! arguments
      real           ,intent(inout) :: attribute_cube(:,:)   ! arguments
      integer                       :: indx,indx2,igather    ! local
      integer                       :: ia,ib,nhalf,nhalf2    ! local
      integer                       :: total,kount           ! local
      real                          :: smoothed(obj%ndpt)    ! local
      real                          :: aweight,fraction      ! local
      real                          :: esum,wsum,weight      ! local

!!!!!!!!!!!!!!! calculate smoothed array:

      fraction = 0.01 * obj%smoothing_percent_valid
      nhalf    = obj%nt/2

      do indx = 1,obj%ndpt
           nhalf2 = min(indx-1,nhalf)
           ia = indx - nhalf2
           ib = indx + nhalf2
           ia = max(ia,1)
           ib = min(ib,obj%ndpt)
           esum  = 0.0
           wsum  = 0.0
           kount = 0
           total = 0
           do indx2 = ia,ib
                do igather = 1,obj%ngather
                     total  = total  + 1
                     if (attribute_cube(indx2,igather) == FNIL) cycle
                     if (obj%use_weighted_smoothing) then
                          aweight = weight_cube(indx2,igather)
                     else
                          aweight = 1.0
                     end if
                     if (aweight == FNIL) cycle
                     weight = aweight * obj%dweights(igather)  &
                                      * obj%tweights(igather)
                     esum   = esum  + weight * attribute_cube(indx2,igather)
                     wsum   = wsum  + weight
                     kount  = kount + 1
                end do
           end do
           if (kount < fraction * total) then
                smoothed(indx) = FNIL
           else if (wsum == 0.0) then
                smoothed(indx) = FNIL
           else
                smoothed(indx) = esum / wsum
           end if
      end do

!!!!!!!!!!!!!!! save smoothed array in middle of attribute cube:

      do indx = 1,obj%ndpt
           attribute_cube(indx,obj%mid) = smoothed(indx)
      end do

      end subroutine ka_smooth


!!--------------------------- replace nils --------------------------------!!
!!--------------------------- replace nils --------------------------------!!
!!--------------------------- replace nils --------------------------------!!


      subroutine ka_replace_nils (obj,replace_nils,max_interp_points, &
                                      interp_weights,weights,trace)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      character(len=*),intent(in)    :: replace_nils         ! arguments
      integer         ,intent(in)    :: max_interp_points    ! arguments
      logical         ,intent(in)    :: interp_weights       ! arguments
      real            ,intent(inout) :: weights(:)           ! arguments
      real            ,intent(inout) :: trace  (:)           ! arguments
      real                           :: tracekeep(obj%ndpt)  ! local

      tracekeep(:) = trace(1:obj%ndpt)

      select case (replace_nils)
        case ('use linear interpolation')

            call ka_linear_interp  (obj,trace)
            call ka_restore_nils   (obj,max_interp_points,tracekeep,trace)

        case ('use scaled interpolation')

            call ka_scaled_interp  (obj,trace)
            call ka_restore_nils   (obj,max_interp_points,tracekeep,trace)

        case ('replace nils with zeroes')

            where (trace(1:obj%ndpt) == FNIL) trace(1:obj%ndpt) = 0.0

        case ('linear interpolation between weight peaks')

            call ka_kill_non_peaks (obj,weights,trace)
            call ka_linear_interp  (obj,trace)

        case ('scaled interpolation between weight peaks')

            call ka_kill_non_peaks (obj,weights,trace)
            call ka_scaled_interp  (obj,trace)

        case ('do not replace nils')

                 continue

        case default

                 continue

      end select

      call ka_interp_weights (obj,interp_weights,weights,tracekeep,trace)

      end subroutine ka_replace_nils


!!--------------------------- linear interp ---------------------------------!!
!!--------------------------- linear interp ---------------------------------!!
!!--------------------------- linear interp ---------------------------------!!


      subroutine ka_linear_interp (obj,trace)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      real            ,intent(inout) :: trace(:)             ! arguments

      call terputil_replace_nils (trace, obj%ndpt, TERPUTIL_FLAT)

      end subroutine ka_linear_interp


!!--------------------------- scaled interp ---------------------------------!!
!!--------------------------- scaled interp ---------------------------------!!
!!--------------------------- scaled interp ---------------------------------!!


      subroutine ka_scaled_interp (obj,trace)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      real            ,intent(inout) :: trace(:)             ! arguments
      real                           :: gamma(obj%ndpt)      ! local

      where (trace(1:obj%ndpt) == FNIL .or. trace(1:obj%ndpt) <= -1.0)
          gamma(:) = FNIL
      elsewhere
          gamma(:) = sqrt(trace(1:obj%ndpt) + 1.0)
      end where
      call terputil_replace_nils (gamma, obj%ndpt, TERPUTIL_FLAT)
      trace(1:obj%ndpt) = gamma(:)**2 - 1.0

      end subroutine ka_scaled_interp


!!--------------------------- kill non peaks --------------------------------!!
!!--------------------------- kill non peaks --------------------------------!!
!!--------------------------- kill non peaks --------------------------------!!


      subroutine ka_kill_non_peaks (obj,weights,trace)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      real            ,intent(in)    :: weights(:)           ! arguments
      real            ,intent(inout) :: trace(:)             ! arguments
      integer                        :: indx                 ! local

      do indx = 2,obj%ndpt-1
          if (weights(indx) < weights(indx-1) .or. &
              weights(indx) < weights(indx+1)) trace(indx) = FNIL
      end do

      if (obj%ndpt > 1) then
          if (weights(1)        < weights         (2)) trace(1)        = FNIL
          if (weights(obj%ndpt) < weights(obj%ndpt-1)) trace(obj%ndpt) = FNIL
      end if

      end subroutine ka_kill_non_peaks


!!--------------------------- restore nils --------------------------------!!
!!--------------------------- restore nils --------------------------------!!
!!--------------------------- restore nils --------------------------------!!


      subroutine ka_restore_nils (obj,max_interp_points,tracekeep,trace)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      integer         ,intent(in)    :: max_interp_points    ! arguments
      real            ,intent(in)    :: tracekeep(:)         ! arguments
      real            ,intent(inout) :: trace(:)             ! arguments
      integer                        :: indx,istart          ! local

      if (max_interp_points <= 0) return
      if (max_interp_points >= obj%ndpt) return

      istart = 0
      do indx = 1,obj%ndpt
           if (tracekeep(indx) == FNIL) then
                if (istart == 0) istart = indx
           else if (istart == 0) then
                continue
           else if (indx - istart >= max_interp_points) then
                trace(istart:indx-1) = FNIL
                istart = 0
           else
                istart = 0
           end if
      end do

      if (istart > 0) then
           if (obj%ndpt+1 - istart >= max_interp_points) then
                trace(istart:obj%ndpt) = FNIL
           end if
      end if

      end subroutine ka_restore_nils


!!--------------------------- interp weights ------------------------------!!
!!--------------------------- interp weights ------------------------------!!
!!--------------------------- interp weights ------------------------------!!


      subroutine ka_interp_weights (obj,interp_weights,weights,tracekeep,trace)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      logical         ,intent(in)    :: interp_weights       ! arguments
      real            ,intent(inout) :: weights(:)           ! arguments
      real            ,intent(in)    :: tracekeep(:)         ! arguments
      real            ,intent(in)    :: trace(:)             ! arguments

      if (.not.interp_weights) return

      where (tracekeep(1:obj%ndpt) == FNIL) weights(1:obj%ndpt) = FNIL

      call terputil_replace_nils (weights, obj%ndpt, TERPUTIL_FLAT)

      end subroutine ka_interp_weights


!!------------------------- calculate gamma -------------------------------!!
!!------------------------- calculate gamma -------------------------------!!
!!------------------------- calculate gamma -------------------------------!!


      subroutine ka_calculate_gamma (obj,beta,gamma)
      type(ka_struct) ,intent(in)    :: obj                  ! arguments
      real            ,intent(in)    :: beta(:)              ! arguments
      real            ,intent(out)   :: gamma(:)             ! arguments

      where (beta(1:obj%ndpt) == FNIL) 
           gamma(1:obj%ndpt) = FNIL
      elsewhere
           gamma(1:obj%ndpt) = sqrt(beta(1:obj%ndpt) + 1.0)
      end where

      call terputil_replace_nils (gamma, obj%ndpt, TERPUTIL_FLAT)

      end subroutine ka_calculate_gamma


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine ka_wrapup (obj)
      type(ka_struct),intent(inout) :: obj              ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      write (lunprint,*) ' '
      write (lunprint,*) '@@@@@@@@@@@@@@@@@ START KA WRAPUP @@@@@@@@@@@@@@@@@@'
      write (lunprint,*) ' '
      write (lunprint,*) obj%kounti,' input CMP locations'
      write (lunprint,*) obj%kounto,' output CMP locations'
      write (lunprint,*) ' '

      call multgather_delete (obj%multgather)

      write (lunprint,*) ' '
      write (lunprint,*) '@@@@@@@@@@@@@@@@@@ END KA WRAPUP @@@@@@@@@@@@@@@@@@@'
      write (lunprint,*) ' '

      end subroutine ka_wrapup


!!--------------------------- parallel store --------------------------------!!
!!--------------------------- parallel store --------------------------------!!
!!--------------------------- parallel store --------------------------------!!

!     If parallel job, have the appropriate worker store the gather
!     until all workers have something to process. If NO_MORE_TRACES,
!     go ahead and have workers with data process.


       subroutine ka_parallel_store (obj,ntr,hdo,tro,need_more)
       type(ka_struct) ,intent(inout) :: obj                      ! arguments
       integer         ,intent(inout) :: ntr                      ! arguments
       double precision,intent(inout) :: hdo(:,:,:)               ! arguments
       real            ,intent(inout) :: tro(:,:,:)               ! arguments
       logical         ,intent(out)   :: need_more                ! arguments
 
       need_more = .false.
       select case (ntr)
          case (1:)
             obj%counter_saved = obj%counter_saved + 1
             if (obj%counter_saved == pcps_current_worker_num) then
                obj%ntr_saved         = ntr
                obj%hdsave(:,:,1:ntr) = hdo(:,:,1:ntr)
                obj%trsave(:,:,1:ntr) = tro(:,:,1:ntr)
             end if
             need_more = .true.
             ntr = NEED_TRACES
             if (obj%counter_saved == pcps_num_workers) then
                ntr               = obj%ntr_saved
                hdo(:,:,1:ntr)    = obj%hdsave(:,:,1:ntr)
                tro(:,:,1:ntr)    = obj%trsave(:,:,1:ntr)
                obj%counter_saved = 0
             end if
          case (NO_MORE_TRACES)
             if (obj%counter_saved > 0) then
                if (pcps_current_worker_num <= obj%counter_saved) then
                   ntr            = obj%ntr_saved
                   hdo(:,:,1:ntr) = obj%hdsave(:,:,1:ntr)
                   tro(:,:,1:ntr) = obj%trsave(:,:,1:ntr)
                else
                   ntr = NEED_TRACES
                end if
                obj%counter_saved = 0
                obj%no_more_in    = .true.
             end if
       end select
 
       end subroutine ka_parallel_store


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module ka_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

