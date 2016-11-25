!<CPS_v1 type="PROCESS"/>
!!----------------------------- dabra.f90 ---------------------------------!!
!!----------------------------- dabra.f90 ---------------------------------!!
!!----------------------------- dabra.f90 ---------------------------------!!


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
! Name       : DABRA              (analyse Dip and ABRA horizon slices)
! Category   : migrations
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2005-01-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Analyze and edit a combo of input horizon slices for tomography.
! Portability: No known limitations, but see note below.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process prepares data for tomography by analyzing and editing a
! combination of horizon-sliced 3D data input volumes which are in trace format.
! Each horizon corresponds to one sample point in all the traces.
!
! Possible input and output traces at each CMP are the following:
!
!      91  The weights                                      (calculated by KA).
!      85  The output average velocity                          (edited by KA).
!     181  The residual NMO velocity ratio       (BETA attribute edited by KA).
!     188  The structural dip in the X direction (XDIP attribute edited by KA).
!     189  The structural dip in the Y direction (YDIP attribute edited by KA).
!       1  The time or depth of the horizon                (created by SLICER).
!
!    Header word HDR_IDENT identifies the type of information each trace
!    contains.  These header word values are listed above.
!
!    Not all input traces are required; only those which are needed to
!    prepare the requested output traces are needed.  The GUI lists the
!    required input traces dynamically depending on the current state
!    of the process parameters.
!
! This process performs the following steps in the indicated order:
!
!    (1) Removes anomalous values from the ATTRIBUTE horizons.
!    (2) Optionally replaces nil values in the ATTRIBUTE horizons.
!    (3) Smooths the ATTRIBUTE horizons.
!    (4) Optionally replaces nil values in the ATTRIBUTE horizons.
!  If the ATTRIBUTE horizons are BETA, also performs the following steps:
!    (5) Edits the input average velocities using the edited BETA horizons.
!    (6) Applies velocity constraints to the edited BETA horizons.
!    (7) Optionally replaces nil values in the new edited BETA horizons.
!    (8) Edits input average velocities again using new edited BETA horizons.
!    (9) Always replaces nil values in the new edited average velocity horizons.
!
! Notes regarding the above steps:
!
!    Anomalies:   Values in the ATTRIBUTE horizons are set to nil where they
!      (1)        differ from the mean in the surrounding data window by
!                 more than the specified standard deviations.
!
!    Smoothing:   Values in the ATTRIBUTE horizons are reset to the mean of all
!      (3)        non-nil values in the surrounding data window.
!
!    Constraints: Values in the edited BETA horizons are set to nil where the
!      (6)        differences between the input and edited average or inteval
!                 velocities are outside of the specified constraints.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process is a multiple-trace process.
!
! Input traces can arrive in any CMP order, but they must be input in
! gathers of one or more traces at a CMP.  Each trace in the input gather
! is considered to belong to a separate data volume to be edited.  All
! input gathers must contain the same number of traces.  Each trace sample
! in the input traces corresponds to a single sliced horizon.
!
! The traces in the input gather will contain certain information listed
! in the GENERAL DESCRIPTION section above.  Each type of information will
! be contained in a single trace.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs a gather of traces at a time.
! Each output gather corresponds to a single CMP location.
! The output gathers have the same traces as the input gathers.
! The output gathers are passed out in CMP order, with X changing fastest,
! filled out to the specified rectangular extent of CMPs.
!
! The traces in the output gather will contain certain information listed
! in the GENERAL DESCRIPTION section above.  Each type of information will
! be contained in a single trace.
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
! NUMTR     max number of traces input/output       used but not changed.
! NWIH      number of words in trace header         used but not changed.
! NDPT      number of sample values in trace        used but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#           Description                Action taken
! ----           -----------                ------------
! HDR_IDENT      trace identification       used to identify traces.
! HDR_X          X coordinate               used to identify CMP location.
! HDR_Y          Y coordinate               used to identify CMP location.
! 1              sequential trace number    reset as necessary.
! 2              top mute index             reset to 1.
! 3              current gather number      reset as necessary.
! 4              current channel number     reset as necessary.
! 25             largest absolute value     reset as necessary.
! 64             bottom mute index          reset to NDPT (number of horizons).
!
! All other header words are reset to zero.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  7. 2005-01-31  Stoeckley  Add SMOOTH_KAPPA_VOLUME, VERTICAL_SMOOTHING_DIST,
!                             USE_TAPERED_SMOOTHING, and TAPERED_WEIGHT_PERCENT.
!  6. 2003-10-23  Stoeckley  Add REPLACE_NILS_AFTER_CONSTRAINTS and
!                             SMOOTHING_TRIM_PERCENT; add an option
!                             to REPLACE_NILS_AFTER.
!  5. 2003-10-02  Stoeckley  Add nullify calls before the memman_nullify calls
!                             to satisfy Portland Group compiler.
!  4. 2003-09-29  Stoeckley  Fix GAMMA = sqrt(BETA+1) which was upside down;
!                             fix bug with the application of interval velocity
!                             contraints for the top horizon.
!  3. 2003-08-22  Stoeckley  Fix calculation of standard deviation; remove
!                             requirement that VAV must always be input when
!                             BETA is edited; add parameter HORIZON_UNIT.
!  2. 2003-07-30  Stoeckley  Remove special treatment of input depth horizon.
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
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           true      whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
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
!                    analyse Dip and ABRA horizon slices
!
! NUMTR~~~~=`XXX   [/L]Number of input and output data volumes.
! NDPT~~~~~=`XXX   [/L]Number of horizons on input and output traces.
!
! HDR_IDENT~~~=`II    [/L]Header word containing trace identification.
! HORIZON_UNIT=`CCCCC [/L]Whether horizons are in time or depth.
!
! EDIT_BETA =`KK   [/L]BETA attribute horizons.    [/L]NEED_BETA~~~=`XX   [/L]ID_BETA~~~=`XX
! EDIT_XDIP =`KK   [/L]XDIP attribute horizons.    [/L]NEED_XDIP~~~=`XX   [/L]ID_XDIP~~~=`XX
! EDIT_YDIP =`KK   [/L]YDIP attribute horizons.    [/L]NEED_YDIP~~~=`XX   [/L]ID_YDIP~~~=`XX
! EDIT_VAV~~=`KK   [/L]Average velocity horizons.  [/L]NEED_VAV~~~~=`XX   [/L]ID_VAV~~~~=`XX
!                  [/L]Weight horizons.            [/L]NEED_WEIGHT =`XX   [/L]ID_WEIGHT =`XX
!                  [/L]Horizon time/depths.        [/L]NEED_DEPTH~~=`XX   [/L]ID_DEPTH~~=`XX
!
! Data Volume:
!  `--------------- `---------------
!   HDR_X~~=`II      HDR_Y~~=`II     [/L]Header words containing X and Y coordinates.
!   X_INIT =`FFFFF   Y_INIT =`FFFFF  [/L]First X and Y bin center.
!   X_INC~~=`FFFFF   Y_INC~~=`FFFFF  [/L]X and Y bin increment.
!   X_LAST =`FFFFF   Y_LAST =`FFFFF  [/L]Last X and Y bin center.
!   X_TOT~~=`FFFFF   Y_TOT~~=`FFFFF  [/L]Number of X and Y bins.
!  `--------------- `---------------
!
! MBYTES =`XXXXXXXXXXXX  [/L]Number of megabytes of permanent memory required.
!
!<NS Analysis Window Size/NC=80>
!
!     This analysis window is used for anomaly editing and for smoothing.
!
!                  WINSIZE_HORIZNUM NUM_TR_X NUM_TR_Y 
!                  `IIIIIIIIIIIIIIII`IIIIIIII`IIIIIIII
!                  `IIIIIIIIIIIIIIII`IIIIIIII`IIIIIIII
!                  `IIIIIIIIIIIIIIII`IIIIIIII`IIIIIIII
!                  `IIIIIIIIIIIIIIII`IIIIIIII`IIIIIIII
!                  `IIIIIIIIIIIIIIII`IIIIIIII`IIIIIIII
!
!<PARMS WINSIZE_HORIZNUM_ARRAYSET [/XST/YST]>
!
!<NS Velocity Constraint Editing/NC=80>
!
!          Apply velocity constraints to the BETA attribute horizons.
! BETA values are set to nil if velocity changes fall outside the constraints.
!     Nil BETA values are optionally replaced after applying constraints.
!
!  APPLY_VAV_CONSTRAINTS~~=`KK  [/L]Apply average velocity constraints.
!  APPLY_VINT_CONSTRAINTS =`KK  [/L]Apply interval velocity constraints.
!
!                  VAV_HORIZNUM  VAV_MIN_PERCENT VAV_MAX_PERCENT
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!
!                  VINT_HORIZNUM VINT_MIN_PERCENTVINT_MAX_PERCENT
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!                  `IIIIIIIIIIIII`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!
!  REPLACE_NILS_AFTER_CONSTRAINTS =`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!<PARMS VAV_HORIZNUM_ARRAYSET [/XST/YST]>
!<PARMS VINT_HORIZNUM_ARRAYSET[/XST/YST]>
!
!<NS Anomaly Editing/NC=80>
!
!      Apply anomaly editing to the BETA or XDIP or YDIP attribute volume.
!           Attribute values are set to nil if they are anomalous.
!
!  REMOVE_BAD_VALUES ~~~ =`KK    [/L]Remove anomalous values based on standard deviation.
!  USE_WEIGHTED_EDIT ~~~ =`KK    [/L]Use weight volume for standard deviation calculation.
!  EDITING_PERCENT_VALID =`FFFF  [/L]Percent of analysis window required to contain valid picks.
!
!              STDEV_HORIZNUM STDEV
!              `IIIIIIIIIIIIII`FFFF
!              `IIIIIIIIIIIIII`FFFF
!              `IIIIIIIIIIIIII`FFFF
!              `IIIIIIIIIIIIII`FFFF
!              `IIIIIIIIIIIIII`FFFF
!<PARMS STDEV_HORIZNUM_ARRAYSET[/XST/YST]>
!
!<NS Smoothing/NC=80>
!
!  Smooth the BETA or XDIP or YDIP attribute volume after anomaly editing is finished.
!           The anomaly editing will have set some attribute values to nil.
!       Nil attribute values are optionally replaced before or after smoothing.
!
! [/L]Before smoothing:
!  `--------------------------------------------------------------------------
!    REPLACE_NILS_BEFORE =`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
!    SMOOTHING_PERCENT_VALID =`FFFF  [/L]Percent of analysis window required to contain valid picks.
!    SMOOTHING_TRIM_PERCENT~~=`FFFF  [/L]Percent of data to trim when smoothing.
!  `--------------------------------------------------------------------------
!
! [/L]After smoothing:
!  `--------------------------------------------------------------------------
!    REPLACE_NILS_AFTER~~=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  `--------------------------------------------------------------------------
!
! [/L]Additional smoothing of BETA:
!  `--------------------------------------------------------------------------
!    SMOOTH_KAPPA_VOLUME~~~~~=`KK    [/L]Smooth the edited and smoothed BETA volume using KAPPA.
!    VERTICAL_SMOOTHING_DIST =`FFFFFFFFFFFF  [/L]Vertical smoothing distance in time/depth units.
!  `--------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="HDR_IDENT">
!<Tip> Header word containing trace identification. </Tip>
! Default = 48
! Allowed = 1-NWIH   (normally 48-55 or 65-NWIH)
!
! The following identification integers must reside in the specified
! header word of each trace:
!
!     91  The weights                                      (calculated by KA).
!     85  The output average velocity                          (edited by KA).
!    181  The residual NMO velocity ratio       (BETA attribute edited by KA).
!    188  The structural dip in the X direction (XDIP attribute edited by KA).
!    189  The structural dip in the Y direction (YDIP attribute edited by KA).
!      1  The time or depth of the horizon.
!</Help>
!
!
!<Help KEYWORD="HORIZON_UNIT">
!<Tip> Whether horizons are in time or depth. </Tip>
! Default = depth
! Allowed = time or depth
!
! This parameter is needed only when calculating interval velocities from
! the average velocity horizons.  The algorithm is different depending on
! whether the time/depth trace contains horizon times or horizon depths.
!
! This parameter is used only when applying interval velocity constraints
! to the BETA attribute.
!</Help>
!
!                     ++++++++++++++++++++++++++++
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
!<Tip> X coordinate of first bin center. </Tip>
! Default =  1.0
! Allowed = real (in units of header word HDR_X).
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Y coordinate of first bin center. </Tip>
! Default =  1.0
! Allowed = real (in units of header word HDR_X).
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip> Bin increment in X direction. </Tip>
! Default =  1.0
! Allowed = real > 0.0 (in units of header word HDR_X).
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Bin increment in Y direction. </Tip>
! Default =  1.0
! Allowed = real > 0.0 (in units of header word HDR_X).
!</Help>
!
!
!<Help KEYWORD="X_LAST">
!<Tip> X coordinate of last bin center. </Tip>
!<Tip> Minimum value of HDR_X. </Tip>
! Default =  1.0
! Allowed = real >= X_INIT (in units of header word HDR_X).
!</Help>
!
!
!<Help KEYWORD="Y_LAST">
!<Tip> Y coordinate of last bin center. </Tip>
! Default =  1.0
! Allowed = real >= Y_INIT (in units of header word HDR_X).
!</Help>
!
!
!<Help KEYWORD="X_TOT">
!<Tip> Total number of bins in the X direction. </Tip>
! Default =  1
! Allowed = int > 0.
!</Help>
!
!
!<Help KEYWORD="Y_TOT">
!<Tip> Total number of bins in the Y direction. </Tip>
! Default =  1
! Allowed = int > 0.
!</Help>
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="NUMTR" TYPE="DISPLAY_ONLY">
!<Tip> Number of input and output data volumes to slice. </Tip>
!</Help>
!
!
!<Help KEYWORD="NDPT" TYPE="DISPLAY_ONLY">
!<Tip> Number of horizons on input and output traces. </Tip>
!</Help>
!
!
!<Help KEYWORD="MBYTES" TYPE="DISPLAY_ONLY">
!<Tip> Number of megabytes of permanent memory required. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="STDEV_HORIZNUM">
!<Tip> Horizon number on trace for specifying standard deviation. </Tip>
! Default = 1
! Allowed = valid horizon number (1 to NDPT)
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! volume.
!
! This is the horizon number for which the user wishes to specify STDEV.
!
! The horizon numbers must be specified in ascending order.
!
! Interpolations will be performed between the specified horizons, and flat
! extrapolations will be performed above the first specified horizon and below
! the last specified horizon.
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
! calculated from an analysis window centered at that value.  A value
! will be set to nil if it differs from the mean value by more than
! the specified number of standard deviations.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="APPLY_VAV_CONSTRAINTS">
!<Tip> Whether to apply average velocity constraints. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to edit the BETA attribute horizons.
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
! This parameter is used to edit the BETA attribute horizons.
!
! The VINT_MIN_PERCENT and VINT_MAX_PERCENT arrays will not be used if this
! parameter is NO.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="VAV_HORIZNUM">
!<Tip> Horizon number for specifying average velocity contraints. </Tip>
! Default = 1
! Allowed = valid horizon number (1 to NDPT)
!
! This parameter is used to edit the BETA attribute horizons.
!
! This is the horizon number for which the user wishes to specify the following
! parameters:
!               VAV_MIN_PERCENT
!               VAV_MAX_PERCENT
!
! The horizon numbers must be specified in ascending order.
!
! Interpolations will be performed between the specified horizons, and flat
! extrapolations will be performed above the first specified horizon and below
! the last specified horizon.
!</Help>
!
!
!<Help KEYWORD="VINT_HORIZNUM">
!<Tip> Horizon number for specifying interval velocity contraints. </Tip>
! Default = 1
! Allowed = valid horizon number (1 to NDPT)
!
! This parameter is used to edit the BETA attribute horizons.
!
! This is the horizon number for which the user wishes to specify the following
! parameters:
!               VINT_MIN_PERCENT
!               VINT_MAX_PERCENT
!
! The horizon numbers must be specified in ascending order.
!
! Interpolations will be performed between the specified horizons, and flat
! extrapolations will be performed above the first specified horizon and below
! the last specified horizon.
!</Help>
!
!
!<Help KEYWORD="VAV_MIN_PERCENT">
!<Tip> Minimum percentage of average velocity to accept. </Tip>
! Default = 75.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to edit the BETA attribute horizons.
!
! This array is not used if APPLY_VAV_CONSTRAINTS is NO.
!
! Average velocities smaller than the specified threshhold will cause the
! corresponding value in the BETA attribute horizon to be set to nil.
!</Help>
!
!
!<Help KEYWORD="VAV_MAX_PERCENT">
!<Tip> Maximum percentage of average velocity to accept. </Tip>
! Default = 125.0
! Allowed = 100.0 or greater
!
! This parameter is used to edit the BETA attribute horizons.
!
! This array is not used if APPLY_VAV_CONSTRAINTS is NO.
!
! Average velocities larger than the specified threshhold will cause the
! corresponding value in the BETA attribute horizon to be set to nil.
!</Help>
!
!
!<Help KEYWORD="VINT_MIN_PERCENT">
!<Tip> Minimum percentage of interval velocity to accept. </Tip>
! Default = 75.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to edit the BETA attribute horizons.
!
! This array is not used if APPLY_VINT_CONSTRAINTS is NO.
!
! Interval velocities smaller than the specified threshhold will cause the
! corresponding value in the BETA attribute horizon to be set to nil.
!</Help>
!
!
!<Help KEYWORD="VINT_MAX_PERCENT">
!<Tip> Maximum percentage of interval velocity to accept. </Tip>
! Default = 125.0
! Allowed = 100.0 or greater
!
! This parameter is used to edit the BETA attribute horizons.
!
! This array is not used if APPLY_VINT_CONSTRAINTS is NO.
!
! Interval velocities larger than the specified threshhold will cause the
! corresponding value in the BETA attribute horizon to be set to nil.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="WINSIZE_HORIZNUM">
!<Tip> Horizon number for specifying NUM_TR_X and NUM_TR_Y. </Tip>
! Default = 1
! Allowed = valid horizon number (1 to NDPT)
!
! This is the horizon number for which the user wishes to specify the following
! parameters which describe the analysis window size:
!           NUM_TR_X  (size in the X direction is 2 * NUM_TR_X + 1)
!           NUM_TR_Y  (size in the Y direction is 2 * NUM_TR_Y + 1)
!
! The horizon numbers must be specified in ascending order.
!
! Interpolations will be performed between the specified horizons, and flat
! extrapolations will be performed above the first specified horizon and below
! the last specified horizon.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_X">
!<Tip> Number of traces on each side in X dir in analysis window. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of traces on each side of the center trace in the
! analysis window.
!
! The analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute horizons.
!
! The analysis window size is specified with the following two parameters:
!           NUM_TR_X  (size in the X direction is 2 * NUM_TR_X + 1)
!           NUM_TR_Y  (size in the Y direction is 2 * NUM_TR_Y + 1)
!</Help>
!
!
!<Help KEYWORD="NUM_TR_Y">
!<Tip> Number of traces on each side in Y dir in analysis window. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of traces on each side of the center trace in the
! analysis window.
!
! The analysis window is used to edit and smooth the selected BETA or
! XDIP or YDIP attribute horizons.
!
! The analysis window size is specified with the following two parameters:
!           NUM_TR_X  (size in the X direction is 2 * NUM_TR_X + 1)
!           NUM_TR_Y  (size in the Y direction is 2 * NUM_TR_Y + 1)
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="EDIT_BETA">
!<Tip> Whether to edit the BETA attribute horizons. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The output BETA attribute horizons will be the edited and smoothed versions
! of the input BETA attribute horizons.
!
! BETA = (input average velocity / output average velocity)**2 - 1
!
! sqrt(BETA + 1) = input average velocity / output average velocity.
!
! The output edited BETA attribute horizons might contain nil values unless
! REPLACE_NILS_AFTER and REPLACE_NILS_AFTER_CONSTRAINTS say to replace nil
! values.
!</Help>
!
!
!<Help KEYWORD="EDIT_XDIP">
!<Tip> Whether to edit the XDIP attribute horizons. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The output XDIP attribute horizons will be the edited and smoothed versions
! of the input XDIP attribute volume.
!
! The output edited XDIP attribute horizons might contain nil values unless
! REPLACE_NILS_AFTER says to replace nil values.
!</Help>
!
!
!<Help KEYWORD="EDIT_YDIP">
!<Tip> Whether to edit the YDIP attribute horizons. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The output YDIP attribute horizons will be the edited and smoothed versions
! of the input YDIP attribute horizons.
!
! The output edited YDIP attribute horizons might contain nil values unless
! REPLACE_NILS_AFTER says to replace nil values.
!</Help>
!
!
!<Help KEYWORD="EDIT_VAV">
!<Tip> Whether to edit the average velocity horizons. </Tip>
! Default = NO
! Allowed = YES or NO
!
! These horizons will be calculated from the input average velocity horizons
! and the edited BETA horizons.
!
! The output modified average velocity horizons will never contain nil or
! zero values.
!</Help>
!
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="ID_BETA" TYPE="DISPLAY_ONLY">
!<Tip> Identification in HDR_IDENT for the BETA attribute horizons. </Tip>
!</Help>
!
!
!<Help KEYWORD="ID_XDIP" TYPE="DISPLAY_ONLY">
!<Tip> Identification in HDR_IDENT for the XDIP attribute horizons. </Tip>
!</Help>
!
!
!<Help KEYWORD="ID_YDIP" TYPE="DISPLAY_ONLY">
!<Tip> Identification in HDR_IDENT for the YDIP attribute horizons. </Tip>
!</Help>
!
!
!<Help KEYWORD="ID_VAV" TYPE="DISPLAY_ONLY">
!<Tip> Identification in HDR_IDENT for the average velocity horizons. </Tip>
!</Help>
!
!
!<Help KEYWORD="ID_WEIGHT" TYPE="DISPLAY_ONLY">
!<Tip> Identification in HDR_IDENT for the weights. </Tip>
!</Help>
!
!
!<Help KEYWORD="ID_DEPTH" TYPE="DISPLAY_ONLY">
!<Tip> Identification in HDR_IDENT for the horizon time/depths. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="NEED_BETA" TYPE="DISPLAY_ONLY">
!<Tip> Whether the BETA attribute horizons are required input. </Tip>
!</Help>
!
!
!<Help KEYWORD="NEED_XDIP" TYPE="DISPLAY_ONLY">
!<Tip> Whether the XDIP attribute horizons are required input. </Tip>
!</Help>
!
!
!<Help KEYWORD="NEED_YDIP" TYPE="DISPLAY_ONLY">
!<Tip> Whether the YDIP attribute horizons are required input. </Tip>
!</Help>
!
!
!<Help KEYWORD="NEED_VAV" TYPE="DISPLAY_ONLY">
!<Tip> Whether the average velocity horizons are required input. </Tip>
!</Help>
!
!
!<Help KEYWORD="NEED_WEIGHT" TYPE="DISPLAY_ONLY">
!<Tip> Whether the weights are required input. </Tip>
!</Help>
!
!
!<Help KEYWORD="NEED_DEPTH" TYPE="DISPLAY_ONLY">
!<Tip> Whether the horizon time/depths are required input. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
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
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="USE_WEIGHTED_SMOOTHING">
!<Tip> Whether to use the weight volume for smoothing. </Tip>
! Default = NO
! Allowed = YES or NO
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! volume.
!
! SMOOTHING_TRIM_PERCENT must be set to zero in order to use this option.
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
!
! SMOOTHING_TRIM_PERCENT must be set to zero in order to use this option.
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
!
! SMOOTHING_TRIM_PERCENT must be set to zero in order to use this option.
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
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="EDITING_PERCENT_VALID">
!<Tip> Percent of window required to contain valid picks for editing. </Tip>
! Default = 60.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to edit the selected BETA or XDIP or YDIP attribute
! horizons.
!
! The analysis window size is specified with the following two parameters:
!           NUM_TR_X  (size in the X direction is 2 * NUM_TR_X + 1)
!           NUM_TR_Y  (size in the Y direction is 2 * NUM_TR_Y + 1)
!</Help>
!
!
!<Help KEYWORD="SMOOTHING_PERCENT_VALID">
!<Tip> Percent of window required to contain valid picks for smoothing. </Tip>
! Default = 60.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! horizons.
!
! The analysis window size is specified with the following two parameters:
!           NUM_TR_X  (size in the X direction is 2 * NUM_TR_X + 1)
!           NUM_TR_Y  (size in the Y direction is 2 * NUM_TR_Y + 1)
!</Help>
!
!
!<Help KEYWORD="SMOOTHING_TRIM_PERCENT">
!<Tip> Percent of data in window to trim when smoothing. </Tip>
! Default = 0.0
! Allowed = 0.0 to 100.0
!
! This parameter is used to smooth the selected BETA or XDIP or YDIP attribute
! horizons.
!
! This parameter specifies how much of the data in the analysis window
! should be trimmed while smoothing.
!
! The value 0.0 causes a mean value to be calculated (no trimming).
! The value 100.0 causes a median value to be calculated (100% trimming).
!
! Only the non-nil values are used to calculate the trimmed mean.
! If SMOOTHING_TRIM_PERCENT is greater than zero, USE_WEIGHTED_SMOOTHING,
! USE_DISTANCE_SMOOTHING and USE_TAPERED_SMOOTHING are not used (i.e. all
! data points have the same weight).
!
! The analysis window size is specified with the following two parameters:
!           NUM_TR_X  (size in the X direction is 2 * NUM_TR_X + 1)
!           NUM_TR_Y  (size in the Y direction is 2 * NUM_TR_Y + 1)
!</Help>
!
!
!<Help KEYWORD="SMOOTH_KAPPA_VOLUME">
!<Tip> Whether to smooth the BETA volume using KAPPA. </Tip>
! Default = NO 
! Allowed = YES or NO
!
! This parameter is used to smooth the BETA volume using KAPPA.
! The KAPPA horizons will be smoothed laterally over the specified analysis
! window, and then the KAPPA traces will be smoothed vertically using the
! VERTICAL_SMOOTHING_DIST parameter.
!
! This is an additional action which is performed after the BETA volume
! has been edited and smoothed in the normal manner.
!
! KAPPA = input interval velocity / output interval velocity.
! GAMMA = input average velocity / output average velocity.
! GAMMA = sqrt(BETA + 1)
! BETA  = GAMMA**2 - 1
!
! These steps are performed:
!  (1) Any nils in the BETA volume are interpolated.
!  (2) The BETA volume is converted to a GAMMA volume.
!  (3) The GAMMA volume is converted to a KAPPA volume.
!  (4) Any nils originally in the BETA volume are restored in the KAPPA volume.
!  (5) The KAPPA volume is smoothed laterally.
!  (6) Any nils in the smoothed KAPPA volume are interpolated.
!  (7) The KAPPA volume is smoothed vertically.
!  (8) The smoothed KAPPA volume is converted to a GAMMA volume.
!  (9) The GAMMA volume is converted to a BETA volume.
! (10) Any nils originally in the BETA volume are restored.
!</Help>
!
!
!<Help KEYWORD="VERTICAL_SMOOTHING_DIST">
!<Tip> Vertical smoothing distance for KAPPA in time/depth units. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! This parameter is used with the SMOOTH_KAPPA_VOLUME parameter.
! No vertical smoothing is performed if this parameter is zero.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="REPLACE_NILS_BEFORE">
!<Tip> Whether and how to replace nil attribute values before smoothing. </Tip>
! Default = do not replace nils
! Allowed = use bilinear interpolation
! Allowed = replace nils with zeroes
! Allowed = do not replace nils
!
! This parameter is used before smoothing the selected BETA or XDIP or
! YDIP attribute horizons.
!
! Bilinear interpolation simply replaces nils with linearly interpolated values.
!</Help>
!
!
!<Help KEYWORD="REPLACE_NILS_AFTER">
!<Tip> Whether and how to replace nil attribute values after smoothing. </Tip>
! Default = do not replace nils
! Allowed = use bilinear interpolation
! Allowed = replace nils with zeroes
! Allowed = do not replace nils
! Allowed = reset interpolated values back to nil
!
! This parameter is used after smoothing the selected BETA or XDIP or
! YDIP attribute horizons.
!
! Bilinear interpolation simply replaces nils with linearly interpolated values.
!
! Resetting interpolated values to nil after smoothing will replace smoothed
! values with nil for those values which were originally nil before the
! REPLACE_NILS_BEFORE action was taken.
!</Help>
!
!
!<Help KEYWORD="REPLACE_NILS_AFTER_CONSTRAINTS">
!<Tip> Whether and how to replace nil BETA values after constraints. </Tip>
! Default = do not replace nils
! Allowed = use bilinear interpolation
! Allowed = replace nils with zeroes
! Allowed = do not replace nils
!
! This parameter is used after applying velocity constraints to the BETA
! attribute horizons.
!
! Bilinear interpolation simply replaces nils with linearly interpolated values.
!</Help>
!
!
!<Help KEYWORD="APPLY_SMOOTHING">
!<Tip> Whether to smooth the BETA or XDIP or YDIP attribute horizons. </Tip>
! Default = YES
! Allowed = YES or NO
!
! The attribute horizons will be smoothed over the specified analysis window.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module dabra_module
      use pc_module
      use named_constants_module
      use abrakadabra_module
      use kadabra_module
      use memman_module
      use string_module
      use mth_module
      use lav_module
      use terputil_module
      use statutil_module
      use sizeof_module

      implicit none
      private
      public :: dabra_create
      public :: dabra_initialize
      public :: dabra_update
      public :: dabra_delete
      public :: dabra            ! main trace processing routine.
      public :: dabra_wrapup

      character(len=100),public,save :: DABRA_IDENT = &
'$Id: dabra.f90,v 1.7 2005/01/31 14:05:24 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: dabra_struct

        private
        logical            :: skip_wrapup                 ! wrapup flag.

        integer            :: numtr                       ! global
        integer            :: nwih                        ! global
        integer            :: ndpt                        ! global

        logical            :: apply_vav_constraints       ! process parameter
        logical            :: apply_vint_constraints      ! process parameter

        integer            :: hdr_ident                   ! process parameter
        integer            :: hdr_x                       ! process parameter
        integer            :: hdr_y                       ! process parameter
        real               :: x_init                      ! process parameter
        real               :: y_init                      ! process parameter
        real               :: x_inc                       ! process parameter
        real               :: y_inc                       ! process parameter
        real               :: x_last                      ! process parameter
        real               :: y_last                      ! process parameter
        integer            :: x_tot                       ! process parameter
        integer            :: y_tot                       ! process parameter

        logical            :: edit_beta                   ! process parameter
        logical            :: edit_xdip                   ! process parameter
        logical            :: edit_ydip                   ! process parameter
        logical            :: edit_vav                    ! process parameter

        logical            :: use_weighted_edit           ! process parameter
        logical            :: remove_bad_values           ! process parameter
        logical            :: apply_smoothing             ! process parameter
        logical            :: use_weighted_smoothing      ! process parameter
        logical            :: use_distance_smoothing      ! process parameter
        logical            :: use_tapered_smoothing       ! process parameter

        real               :: center_weight_percent       ! process parameter
        real               :: tapered_weight_percent      ! process parameter
        real               :: editing_percent_valid       ! process parameter
        real               :: smoothing_percent_valid     ! process parameter
        real               :: smoothing_trim_percent      ! process parameter
        logical            :: smooth_kappa_volume         ! process parameter
        real               :: vertical_smoothing_dist     ! process parameter
        character(len=40)  :: replace_nils_before         ! process parameter
        character(len=40)  :: replace_nils_after          ! process parameter
        character(len=40)  :: replace_nils_after_constraints
        character(len=8)   :: horizon_unit                ! process parameter

        integer            :: num_winsize                 ! process parameter
        integer,pointer    :: winsize_horiznum(:)         ! process parameter
        integer,pointer    :: num_tr_x(:)                 ! process parameter
        integer,pointer    :: num_tr_y(:)                 ! process parameter

        integer            :: num_stdev                   ! process parameter
        integer,pointer    :: stdev_horiznum(:)           ! process parameter
        real,pointer       :: stdev(:)                    ! process parameter

        integer            :: num_vav                     ! process parameter
        integer,pointer    :: vav_horiznum(:)             ! process parameter
        real,pointer       :: vav_min_percent(:)          ! process parameter
        real,pointer       :: vav_max_percent(:)          ! process parameter

        integer            :: num_vint                    ! process parameter
        integer,pointer    :: vint_horiznum(:)            ! process parameter
        real,pointer       :: vint_min_percent(:)         ! process parameter
        real,pointer       :: vint_max_percent(:)         ! process parameter

        real      ,pointer :: slices(:,:,:,:)
        integer   ,pointer :: ident(:)

        integer            :: ix,iy

        integer            :: kounti              ! input gather counter
        integer            :: kounto              ! output gather counter

        integer            :: itr_beta      ! index of beta
        integer            :: itr_xdip      ! index of xdip
        integer            :: itr_ydip      ! index of ydip
        integer            :: itr_vav       ! index of vav
        integer            :: itr_weight    ! index of weights
        integer            :: itr_depth     ! index of horizon time/depth

        real   ,pointer                 :: stdev_fillout(:)
        real   ,pointer                 :: vav_min_fillout(:)
        real   ,pointer                 :: vav_max_fillout(:)
        real   ,pointer                 :: vint_min_fillout(:)
        real   ,pointer                 :: vint_max_fillout(:)
        integer,pointer                 :: num_tr_x_fillout(:)
        integer,pointer                 :: num_tr_y_fillout(:)

        logical                         :: need_beta  
        logical                         :: need_xdip  
        logical                         :: need_ydip  
        logical                         :: need_vav  
        logical                         :: need_weight
        logical                         :: need_depth

      end type dabra_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                   ,save :: lunprint  ! unit number for printing.
      type(dabra_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine dabra_create (obj)
      type(dabra_struct),pointer :: obj       ! arguments
      integer                    :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
           call pc_error ("Unable to allocate obj in dabra_create")
           return
      end if

      nullify (obj%winsize_horiznum  )
      nullify (obj%num_tr_x          )
      nullify (obj%num_tr_y          )

      nullify (obj%vav_horiznum      )
      nullify (obj%vav_min_percent   )
      nullify (obj%vav_max_percent   )

      nullify (obj%vint_horiznum     )
      nullify (obj%vint_min_percent  )
      nullify (obj%vint_max_percent  )

      nullify (obj%stdev_horiznum    )
      nullify (obj%stdev             )

      nullify (obj%slices            )
      nullify (obj%ident             )

      nullify (obj%stdev_fillout     )
      nullify (obj%vav_min_fillout   )
      nullify (obj%vav_max_fillout   )
      nullify (obj%vint_min_fillout  )
      nullify (obj%vint_max_fillout  )
      nullify (obj%num_tr_x_fillout  )
      nullify (obj%num_tr_y_fillout  )

!!! The above nullify statements must precede the memman_nullify calls
!!! for the Portland Group Compiler.

      call memman_nullify (obj%winsize_horiznum  , "dabra_winsize_horiznum")
      call memman_nullify (obj%num_tr_x          , "dabra_num_tr_x")
      call memman_nullify (obj%num_tr_y          , "dabra_num_tr_y")

      call memman_nullify (obj%vav_horiznum      , "dabra_vav_horiznum")
      call memman_nullify (obj%vav_min_percent   , "dabra_vav_min_percent")
      call memman_nullify (obj%vav_max_percent   , "dabra_vav_max_percent")

      call memman_nullify (obj%vint_horiznum     , "dabra_vint_horiznum")
      call memman_nullify (obj%vint_min_percent  , "dabra_vint_min_percent")
      call memman_nullify (obj%vint_max_percent  , "dabra_vint_max_percent")

      call memman_nullify (obj%stdev_horiznum    , "dabra_stdev_horiznum")
      call memman_nullify (obj%stdev             , "dabra_stdev")

      call memman_nullify (obj%slices            , "dabra_slices")
      call memman_nullify (obj%ident             , "dabra_ident")

      call memman_nullify (obj%stdev_fillout     , "dabra_stdev_fillout")
      call memman_nullify (obj%vav_min_fillout   , "dabra_vav_min_fillout")
      call memman_nullify (obj%vav_max_fillout   , "dabra_vav_max_fillout")
      call memman_nullify (obj%vint_min_fillout  , "dabra_vint_min_fillout")
      call memman_nullify (obj%vint_max_fillout  , "dabra_vint_max_fillout")
      call memman_nullify (obj%num_tr_x_fillout  , "dabra_num_tr_x_fillout")
      call memman_nullify (obj%num_tr_y_fillout  , "dabra_num_tr_y_fillout")

      call dabra_initialize (obj)
      end subroutine dabra_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine dabra_delete (obj)
      type(dabra_struct),pointer :: obj            ! arguments
      integer                    :: ierr           ! for error checking

      call dabra_wrapup (obj)

      call memman_free (obj%slices)
      call memman_free (obj%ident)

      call memman_free (obj%winsize_horiznum)
      call memman_free (obj%num_tr_x)
      call memman_free (obj%num_tr_y)

      call memman_free (obj%stdev_horiznum)
      call memman_free (obj%stdev)

      call memman_free (obj%vav_horiznum)
      call memman_free (obj%vav_min_percent)
      call memman_free (obj%vav_max_percent)

      call memman_free (obj%vint_horiznum)
      call memman_free (obj%vint_min_percent)
      call memman_free (obj%vint_max_percent)

      call memman_free (obj%stdev_fillout      )
      call memman_free (obj%vav_min_fillout    )
      call memman_free (obj%vav_max_fillout    )
      call memman_free (obj%vint_min_fillout   )
      call memman_free (obj%vint_max_fillout   )
      call memman_free (obj%num_tr_x_fillout   )
      call memman_free (obj%num_tr_y_fillout   )

      deallocate(obj, stat=ierr)
      if (ierr /= 0) &
             call pc_warning ("error deallocating obj in dabra_delete")
      end subroutine dabra_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine dabra_initialize (obj)
      type(dabra_struct),intent(inout) :: obj            ! arguments
      integer                          :: ierr           ! local

      obj%apply_vav_constraints    = .false.
      obj%apply_vint_constraints   = .false.

      obj%hdr_ident                = 48
      obj%hdr_x                    = 7
      obj%hdr_y                    = 8
      obj%x_init                   = 1.0
      obj%y_init                   = 1.0
      obj%x_inc                    = 1.0
      obj%y_inc                    = 1.0
      obj%x_last                   = 1.0
      obj%y_last                   = 1.0
      obj%x_tot                    = 1
      obj%y_tot                    = 1

      obj%edit_beta                = .true.
      obj%edit_xdip                = .false.
      obj%edit_ydip                = .false.
      obj%edit_vav                 = .true.

      obj%use_weighted_edit        = .false.
      obj%remove_bad_values        = .true.
      obj%apply_smoothing          = .true.
      obj%use_weighted_smoothing   = .false.
      obj%use_distance_smoothing   = .false.
      obj%use_tapered_smoothing    = .false.

      obj%center_weight_percent          = KADABRA_CENTER_PERCENT
      obj%tapered_weight_percent         = KADABRA_TAPERED_PERCENT
      obj%editing_percent_valid          = KADABRA_PERCENT_VALID
      obj%smoothing_percent_valid        = KADABRA_PERCENT_VALID
      obj%smoothing_trim_percent         = 0.0
      obj%smooth_kappa_volume            = .false.
      obj%vertical_smoothing_dist        = 0.0
      obj%replace_nils_before            = 'do not replace nils'
      obj%replace_nils_after             = 'do not replace nils'
      obj%replace_nils_after_constraints = 'do not replace nils'
      obj%horizon_unit                   = 'depth'

      call memman_allocate (obj%winsize_horiznum, 1, ierr)
      call memman_allocate (obj%num_tr_x        , 1, ierr)
      call memman_allocate (obj%num_tr_y        , 1, ierr)

      call memman_allocate (obj%stdev_horiznum  , 1, ierr)
      call memman_allocate (obj%stdev           , 1, ierr)

      call memman_allocate (obj%vav_horiznum    , 1, ierr)
      call memman_allocate (obj%vav_min_percent , 1, ierr)
      call memman_allocate (obj%vav_max_percent , 1, ierr)

      call memman_allocate (obj%vint_horiznum   , 1, ierr)
      call memman_allocate (obj%vint_min_percent, 1, ierr)
      call memman_allocate (obj%vint_max_percent, 1, ierr)

      obj%winsize_horiznum = 1
      obj%num_tr_x         = KADABRA_NUM_TR_XY
      obj%num_tr_y         = KADABRA_NUM_TR_XY

      obj%stdev_horiznum   = 1
      obj%stdev            = KADABRA_STDEV

      obj%vav_horiznum     = 1
      obj%vav_min_percent  = KADABRA_MIN_PERCENT
      obj%vav_max_percent  = KADABRA_MAX_PERCENT

      obj%vint_horiznum    = 1
      obj%vint_min_percent = KADABRA_MIN_PERCENT
      obj%vint_max_percent = KADABRA_MAX_PERCENT

      obj%num_winsize      = 1
      obj%num_stdev        = 1
      obj%num_vav          = 1
      obj%num_vint         = 1

      call dabra_update (obj)
      end subroutine dabra_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine dabra_update (obj)
      type(dabra_struct),intent(inout),target :: obj               ! arguments
      real                                    :: x_last_keep       ! local
      real                                    :: y_last_keep       ! local
      integer                                 :: x_tot_keep        ! local
      integer                                 :: y_tot_keep        ! local
      integer                                 :: nstore            ! local
      real                                    :: xstore,mbytes     ! local
      integer                                 :: ierr,indx,kount   ! local
      logical                             :: need_vel_constraints  ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!--------------------------- read parameters -----------------------------!!
!!--------------------------- read parameters -----------------------------!!
!!--------------------------- read parameters -----------------------------!!


      call pc_register_array_names (    'WINSIZE_HORIZNUM_ARRAYSET',  &
                                      (/'WINSIZE_HORIZNUM ',          &
                                        'NUM_TR_X         ',          &
                                        'NUM_TR_Y         '/))

      call pc_register_array_names (    'STDEV_HORIZNUM_ARRAYSET',  &
                                      (/'STDEV_HORIZNUM   ',        &
                                        'STDEV            '/))

      call pc_register_array_names (    'VAV_HORIZNUM_ARRAYSET',   &
                                      (/'VAV_HORIZNUM     ',       &
                                        'VAV_MIN_PERCENT  ',       &
                                        'VAV_MAX_PERCENT  '/))

      call pc_register_array_names (    'VINT_HORIZNUM_ARRAYSET',   &
                                      (/'VINT_HORIZNUM     ',       &
                                        'VINT_MIN_PERCENT  ',       &
                                        'VINT_MAX_PERCENT  '/))

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)

      x_last_keep = obj%x_last
      y_last_keep = obj%y_last
      x_tot_keep  = obj%x_tot
      y_tot_keep  = obj%y_tot

      call pc_get    ('HDR_X             ', obj%hdr_x)
      call pc_get    ('HDR_Y             ', obj%hdr_y)
      call pc_get    ('x_init            ', obj%x_init)
      call pc_get    ('y_init            ', obj%y_init)
      call pc_get    ('x_inc             ', obj%x_inc)
      call pc_get    ('y_inc             ', obj%y_inc)
      call pc_get    ('x_last            ', obj%x_last)
      call pc_get    ('y_last            ', obj%y_last)
      call pc_get    ('x_tot             ', obj%x_tot)
      call pc_get    ('y_tot             ', obj%y_tot)

      call pc_get ('apply_vav_constraints    ', obj%apply_vav_constraints)
      call pc_get ('apply_vint_constraints   ', obj%apply_vint_constraints)
      call pc_get ('HDR_IDENT                ', obj%hdr_ident)
      call pc_get ('edit_beta                ', obj%edit_beta)
      call pc_get ('edit_xdip                ', obj%edit_xdip)
      call pc_get ('edit_ydip                ', obj%edit_ydip)
      call pc_get ('edit_VAV                 ', obj%edit_vav)
      call pc_get ('USE_WEIGHTED_EDIT        ', obj%use_weighted_edit)
      call pc_get ('REMOVE_BAD_VALUES        ', obj%remove_bad_values)
      call pc_get ('apply_smoothing          ', obj%apply_smoothing)
      call pc_get ('USE_WEIGHTED_SMOOTHING   ', obj%use_weighted_smoothing)
      call pc_get ('USE_DISTANCE_SMOOTHING   ', obj%use_distance_smoothing)
      call pc_get ('USE_tapered_SMOOTHING    ', obj%use_tapered_smoothing)

      call pc_get ('center_weight_percent    ', obj%center_weight_percent)
      call pc_get ('tapered_weight_percent   ', obj%tapered_weight_percent)
      call pc_get ('editing_PERCENT_VALID    ', obj%editing_percent_valid)
      call pc_get ('smoothing_PERCENT_VALID  ', obj%smoothing_percent_valid)
      call pc_get ('smoothing_trim_percent   ', obj%smoothing_trim_percent)
      call pc_get ('smooth_kappa_volume      ', obj%smooth_kappa_volume)
      call pc_get ('vertical_smoothing_dist  ', obj%vertical_smoothing_dist)

  call kadabra_get ('winsize_horiznum', obj%winsize_horiznum , obj%num_winsize)
  call kadabra_get ('num_tr_x        ', obj%num_tr_x         , obj%num_winsize)
  call kadabra_get ('num_tr_y        ', obj%num_tr_y         , obj%num_winsize)

  call kadabra_get ('STDEV_horiznum  ', obj%stdev_horiznum   , obj%num_stdev)
  call kadabra_get ('STDEV           ', obj%stdev            , obj%num_stdev)

  call kadabra_get ('vav_horiznum    ', obj%vav_horiznum     , obj%num_vav)
  call kadabra_get ('vav_min_percent ', obj%vav_min_percent  , obj%num_vav)
  call kadabra_get ('vav_max_percent ', obj%vav_max_percent  , obj%num_vav)

  call kadabra_get ('vint_horiznum   ', obj%vint_horiznum    , obj%num_vint)
  call kadabra_get ('vint_min_percent', obj%vint_min_percent , obj%num_vint)
  call kadabra_get ('vint_max_percent', obj%vint_max_percent , obj%num_vint)

      call kadabra_get_put_options &
                           ('REPLACE_NILS_BEFORE',obj%replace_nils_before, &
                              (/'use bilinear interpolation    ',          &
                                'replace nils with zeroes      ',          &
                                'do not replace nils           '/))

      call kadabra_get_put_options &
                           ('REPLACE_NILS_AFTER',obj%replace_nils_after,   &
                              (/'use bilinear interpolation            ',  &
                                'replace nils with zeroes              ',  &
                                'do not replace nils                   ',  &
                                'reset interpolated values back to nil '/))

      call kadabra_get_put_options &
     ('REPLACE_NILS_AFTER_CONSTRAINTS',obj%replace_nils_after_constraints, &
                              (/'use bilinear interpolation    ',          &
                                'replace nils with zeroes      ',          &
                                'do not replace nils           '/))

      call kadabra_get_put_options &
                           ('horizon_unit',obj%horizon_unit,   &
                                             (/'depth ',       &
                                               'time  '/))


!!------------------------- verify parameters -------------------------------!!
!!------------------------- verify parameters -------------------------------!!
!!------------------------- verify parameters -------------------------------!!


      if (obj%hdr_x <= 0 .or. obj%hdr_x > obj%nwih) then
           call pc_warning ('Bad HDR_X number - reset to 7')
           obj%hdr_x = 7
      end if

      if (obj%hdr_y <= 0 .or. obj%hdr_y > obj%nwih) then
           call pc_warning ('Bad HDR_Y number - reset to 8')
           obj%hdr_y = 8
      end if

      if (obj%x_inc <= 0.0) then
           call pc_warning ('X_INC cannot be <= zero - reset to 1')
           obj%x_inc = 1.0
      end if

      if (obj%y_inc <= 0.0) then
           call pc_warning ('Y_INC cannot be <= zero - reset to 1')
           obj%y_inc = 1.0
      end if

      if (obj%x_last /= x_last_keep .and. obj%x_tot == x_tot_keep) then
           obj%x_tot = 1 + nint((obj%x_last - obj%x_init) / obj%x_inc)
      end if

      if (obj%y_last /= y_last_keep .and. obj%y_tot == y_tot_keep) then
           obj%y_tot = 1 + nint((obj%y_last - obj%y_init) / obj%y_inc)
      end if

      if (obj%x_tot <= 0) then
           call pc_warning ('X_TOT cannot be <= zero - reset to 1')
           obj%x_tot = 1
      end if

      if (obj%y_tot <= 0) then
           call pc_warning ('Y_TOT cannot be <= zero - reset to 1')
           obj%y_tot = 1
      end if

      obj%x_last = obj%x_init + (obj%x_tot - 1) * obj%x_inc
      obj%y_last = obj%y_init + (obj%y_tot - 1) * obj%y_inc

      if (obj%numtr <= 1) then
           call pc_error ('TRACES MUST BE GATHERED BY CMP')
      end if

      do indx = 1,obj%num_winsize
           call mth_constrain (obj%num_tr_x(indx),   0,    9999)
           call mth_constrain (obj%num_tr_y(indx),   0,    9999)
      end do

      call mth_constrain (obj%center_weight_percent  , 0.0,       100.0)
      call mth_constrain (obj%editing_percent_valid  , 0.0,       100.0)
      call mth_constrain (obj%smoothing_percent_valid, 0.0,       100.0)
      call mth_constrain (obj%smoothing_trim_percent , 0.0,       100.0)
      call mth_constrain (obj%vertical_smoothing_dist, 0.0,    999999.0)

      if (obj%hdr_ident <= 0 .or. obj%hdr_ident > obj%nwih) then
           call pc_warning ('Bad HDR_IDENT number - reset to 48')
           call pc_warning ('HDR_IDENT is on the Input and Output screen')
           obj%hdr_ident = 48
      end if

      if (pc_verify_end()) then
           if (obj%apply_vav_constraints .and. obj%num_vav == 0) then
                call pc_error ('The VAV_HORIZNUM arrayset must be specified&
                               & on the Velocity Constraint Editing screen')
           end if
           if (obj%apply_vint_constraints .and. obj%num_vint == 0) then
                call pc_error ('The VINT_HORIZNUM arrayset must be specified&
                               & on the Velocity Constraint Editing screen')
           end if
      end if

      call dabra_verify_horizons (obj,obj%num_vav,obj%vav_horiznum, &
                                  'VAV_HORIZNUM','Velocity Constraint Editing')
      call dabra_verify_horizons (obj,obj%num_vint,obj%vint_horiznum, &
                                  'VINT_HORIZNUM','Velocity Constraint Editing')
      call dabra_verify_horizons (obj,obj%num_stdev,obj%stdev_horiznum, &
                                  'STDEV_HORIZNUM','Anomaly Editing')
      call dabra_verify_horizons (obj,obj%num_winsize,obj%winsize_horiznum, &
                                  'WINSIZE_HORIZNUM','Analysis Window Size')

      call kadabra_verify_minmax &
                 (obj%num_vav ,obj%vav_min_percent ,obj%vav_max_percent)
      call kadabra_verify_minmax &
                 (obj%num_vint,obj%vint_min_percent,obj%vint_max_percent)

      call kadabra_verify_stdev (obj%num_stdev,obj%stdev)

      need_vel_constraints = obj%need_beta .and. &
                (obj%apply_vav_constraints .or. obj%apply_vint_constraints)

      obj%need_vav    = obj%edit_vav  .or. need_vel_constraints
      obj%need_beta   = obj%edit_beta .or. obj%edit_vav
      obj%need_xdip   = obj%edit_xdip
      obj%need_ydip   = obj%edit_ydip
      obj%need_weight = obj%use_weighted_edit .or. obj%use_weighted_smoothing
      obj%need_depth  = obj%edit_beta .or. obj%edit_vav

      if (pc_verify_end()) then
        kount = 0
        if(obj%need_beta       ) kount = kount + 1
        if(obj%need_xdip       ) kount = kount + 1
        if(obj%need_ydip       ) kount = kount + 1
        if(obj%need_vav        ) kount = kount + 1
        if(obj%need_weight     ) kount = kount + 1
        if(obj%need_depth      ) kount = kount + 1
        if (kount > obj%numtr) then
          call pc_error ('Not enough input traces for the specified parameters')
          call pc_error ('Actual number of input traces =',obj%numtr)
          call pc_error ('Minimum required number of input traces =',kount)
          call pc_error ('Please make adjustments to reduce your requirements,')
          call pc_error ('or arrange to input the required traces.')
        end if
      end if


!!---------------------- write parameters ----------------------------------!!
!!---------------------- write parameters ----------------------------------!!
!!---------------------- write parameters ----------------------------------!!


      call pc_put    ('HDR_X             ', obj%hdr_x)
      call pc_put    ('HDR_Y             ', obj%hdr_y)
      call pc_put    ('x_init            ', obj%x_init)
      call pc_put    ('y_init            ', obj%y_init)
      call pc_put    ('x_inc             ', obj%x_inc)
      call pc_put    ('y_inc             ', obj%y_inc)
      call pc_put    ('x_last            ', obj%x_last)
      call pc_put    ('y_last            ', obj%y_last)
      call pc_put    ('x_tot             ', obj%x_tot)
      call pc_put    ('y_tot             ', obj%y_tot)

      call pc_put ('apply_vav_constraints    ', obj%apply_vav_constraints)
      call pc_put ('apply_vint_constraints   ', obj%apply_vint_constraints)
      call pc_put ('HDR_IDENT                ', obj%hdr_ident)
      call pc_put ('edit_beta                ', obj%edit_beta)
      call pc_put ('edit_xdip                ', obj%edit_xdip)
      call pc_put ('edit_ydip                ', obj%edit_ydip)
      call pc_put ('edit_VAV                 ', obj%edit_vav)
      call pc_put ('USE_WEIGHTED_EDIT        ', obj%use_weighted_edit)
      call pc_put ('REMOVE_BAD_VALUES        ', obj%remove_bad_values)
      call pc_put ('apply_smoothing          ', obj%apply_smoothing)
      call pc_put ('USE_WEIGHTED_SMOOTHING   ', obj%use_weighted_smoothing)
      call pc_put ('USE_DISTANCE_SMOOTHING   ', obj%use_distance_smoothing)
      call pc_put ('USE_tapered_SMOOTHING    ', obj%use_tapered_smoothing)

      call pc_put ('center_weight_percent    ', obj%center_weight_percent)
      call pc_put ('tapered_weight_percent   ', obj%tapered_weight_percent)
      call pc_put ('editing_PERCENT_VALID    ', obj%editing_percent_valid)
      call pc_put ('smoothing_PERCENT_VALID  ', obj%smoothing_percent_valid)
      call pc_put ('smoothing_trim_percent   ', obj%smoothing_trim_percent)
      call pc_put ('smooth_kappa_volume      ', obj%smooth_kappa_volume)
      call pc_put ('vertical_smoothing_dist  ', obj%vertical_smoothing_dist)

      call pc_put ('winsize_horiznum ', obj%winsize_horiznum , obj%num_winsize)
      call pc_put ('num_tr_x         ', obj%num_tr_x         , obj%num_winsize)
      call pc_put ('num_tr_y         ', obj%num_tr_y         , obj%num_winsize)

      call pc_put ('STDEV_horiznum   ', obj%stdev_horiznum   , obj%num_stdev)
      call pc_put ('STDEV            ', obj%stdev            , obj%num_stdev)

      call pc_put ('vav_horiznum     ', obj%vav_horiznum     , obj%num_vav)
      call pc_put ('vav_min_percent  ', obj%vav_min_percent  , obj%num_vav)
      call pc_put ('vav_max_percent  ', obj%vav_max_percent  , obj%num_vav)

      call pc_put ('vint_horiznum    ', obj%vint_horiznum    , obj%num_vint)
      call pc_put ('vint_min_percent ', obj%vint_min_percent , obj%num_vint)
      call pc_put ('vint_max_percent ', obj%vint_max_percent , obj%num_vint)

      !!!! these minsize calls do not appear to work:

      call pc_put_minsize_arrayset ('winsize_horiznum_arrayset', 1)
      call pc_put_minsize_arrayset ('stdev_horiznum_arrayset'  , 1)
      call pc_put_minsize_arrayset ('vav_horiznum_arrayset'    , 1)
      call pc_put_minsize_arrayset ('vint_horiznum_arrayset'   , 1)

      xstore = real(obj%x_tot) * real(obj%y_tot) * obj%ndpt * obj%numtr
      nstore = nint(xstore)
      mbytes = xstore * sizeof(mbytes) * 1.0e-6

      if (mbytes > 1000.0) call pc_error ('required storage > 1 megabyte')

      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('iftd'         , .true.)
      call pc_put_control ('nstore'       , nstore)

      call pc_put_gui_only ('numtr'       , obj%numtr)
      call pc_put_gui_only ('ndpt'        , obj%ndpt)
      call pc_put_gui_only ('mbytes'      , mbytes, ndec=2)

      call pc_put_gui_only ('id_beta      ', ABRAKADABRA_OUTPUT_BETA)
      call pc_put_gui_only ('id_xdip      ', ABRAKADABRA_OUTPUT_XDIP)
      call pc_put_gui_only ('id_ydip      ', ABRAKADABRA_OUTPUT_YDIP)
      call pc_put_gui_only ('id_vav       ', ABRAKADABRA_OUTPUT_VAV)
      call pc_put_gui_only ('id_weight    ', ABRAKADABRA_WEIGHT)
      call pc_put_gui_only ('id_depth     ', ABRAKADABRA_TIMEDEPTH)

      call pc_put_gui_only ('need_beta      ', obj%need_beta)
      call pc_put_gui_only ('need_xdip      ', obj%need_XDIP)
      call pc_put_gui_only ('need_ydip      ', obj%need_YDIP)
      call pc_put_gui_only ('need_vav       ', obj%need_vav)
      call pc_put_gui_only ('need_weight    ', obj%need_WEIGHT)
      call pc_put_gui_only ('need_depth     ', obj%need_DEPTH)

      call pc_put_sensitive_field_flag ('use_weighted_smoothing', &
              obj%apply_smoothing .and. obj%smoothing_trim_percent == 0.0)
      call pc_put_sensitive_field_flag ('use_distance_smoothing', &
              obj%apply_smoothing .and. obj%smoothing_trim_percent == 0.0)
      call pc_put_sensitive_field_flag ('use_tapered_smoothing', &
              obj%apply_smoothing .and. obj%smoothing_trim_percent == 0.0)
      call pc_put_sensitive_field_flag ('center_weight_percent', &
              obj%apply_smoothing .and. obj%use_distance_smoothing .and. &
              obj%smoothing_trim_percent == 0.0)
      call pc_put_sensitive_field_flag ('tapered_weight_percent', &
              obj%apply_smoothing .and. obj%use_tapered_smoothing .and. &
              obj%smoothing_trim_percent == 0.0)
      call pc_put_sensitive_field_flag ('smoothing_percent_valid', &
              obj%apply_smoothing)
      call pc_put_sensitive_field_flag ('smoothing_trim_percent', &
              obj%apply_smoothing)
      call pc_put_sensitive_field_flag ('vertical_smoothing_dist', &
              obj%edit_beta .and. obj%smooth_kappa_volume)


!!-------------------- prepare for execution -------------------------------!!
!!-------------------- prepare for execution -------------------------------!!
!!-------------------- prepare for execution -------------------------------!!


      call memman_deallocate (obj%slices)
      call memman_deallocate (obj%ident)

      call memman_deallocate (obj%stdev_fillout      )
      call memman_deallocate (obj%vav_min_fillout    )
      call memman_deallocate (obj%vav_max_fillout    )
      call memman_deallocate (obj%vint_min_fillout   )
      call memman_deallocate (obj%vint_max_fillout   )
      call memman_deallocate (obj%num_tr_x_fillout   )
      call memman_deallocate (obj%num_tr_y_fillout   )

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call memman_allocate &
            (obj%slices, obj%x_tot, obj%y_tot, obj%ndpt, obj%numtr, ierr)

      if (ierr /= 0) then
           call pc_error ('error trying to allocate slices')
           return
      end if

      call memman_allocate (obj%ident, obj%numtr, ierr)

      if (ierr /= 0) then
           call pc_error ('error trying to allocate ident')
           return
      end if

      obj%slices = FNIL
      obj%ident  = INIL

      obj%kounti     = 0    ! input gather counter
      obj%kounto     = 0    ! output gather counter
      obj%itr_beta   = 0    ! index of beta
      obj%itr_xdip   = 0    ! index of xdip
      obj%itr_ydip   = 0    ! index of ydip
      obj%itr_vav    = 0    ! index of vav
      obj%itr_weight = 0    ! index of weights
      obj%itr_depth  = 0    ! index of horizon time/depth

      call memman_allocate (obj%stdev_fillout      , obj%ndpt, ierr)
      call memman_allocate (obj%vav_min_fillout    , obj%ndpt, ierr)
      call memman_allocate (obj%vav_max_fillout    , obj%ndpt, ierr)
      call memman_allocate (obj%vint_min_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%vint_max_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%num_tr_x_fillout   , obj%ndpt, ierr)
      call memman_allocate (obj%num_tr_y_fillout   , obj%ndpt, ierr)

      call dabra_fillout  (obj,obj%stdev_horiznum,obj%num_stdev, &
                               obj%stdev,obj%stdev_fillout)

      call dabra_fillout  (obj,obj%vav_horiznum,obj%num_vav, &
                               obj%vav_min_percent,obj%vav_min_fillout)
      call dabra_fillout  (obj,obj%vav_horiznum,obj%num_vav, &
                               obj%vav_max_percent,obj%vav_max_fillout)

      call dabra_fillout  (obj,obj%vint_horiznum,obj%num_vint, &
                               obj%vint_min_percent,obj%vint_min_fillout)
      call dabra_fillout  (obj,obj%vint_horiznum,obj%num_vint, &
                               obj%vint_max_percent,obj%vint_max_fillout)

      call dabra_ifillout (obj,obj%winsize_horiznum,obj%num_winsize, &
                               obj%num_tr_x,obj%num_tr_x_fillout)
      call dabra_ifillout (obj,obj%winsize_horiznum,obj%num_winsize, &
                               obj%num_tr_y,obj%num_tr_y_fillout)

      obj%vav_min_fillout = 0.01 * obj%vav_min_fillout
      obj%vav_max_fillout = 0.01 * obj%vav_max_fillout

      obj%vint_min_fillout = 0.01 * obj%vint_min_fillout
      obj%vint_max_fillout = 0.01 * obj%vint_max_fillout


!!----------------------------- finish update ------------------------------!!
!!----------------------------- finish update ------------------------------!!
!!----------------------------- finish update ------------------------------!!


      end subroutine dabra_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- verify horizons -------------------------------!!
!!--------------------------- verify horizons -------------------------------!!
!!--------------------------- verify horizons -------------------------------!!


      subroutine dabra_verify_horizons (obj,num,horiznum,keyword,screen)
      type(dabra_struct) ,intent(inout) :: obj                ! arguments
      integer            ,intent(in)    :: num                ! arguments
      integer            ,intent(inout) :: horiznum(:)        ! arguments
      character(len=*)   ,intent(in)    :: keyword            ! arguments
      character(len=*)   ,intent(in)    :: screen             ! arguments
      integer                           :: indx               ! local

      do indx = 1,num
        if (horiznum(indx) == INIL) horiznum(indx) = 1
        call mth_constrain (horiznum(indx), 1, obj%ndpt)
      end do

      if (pc_verify_end()) then
        do indx = 2,num
          if (horiznum(indx) < horiznum(indx-1)) then
            call pc_error (trim(keyword)//' HORIZONS',horiznum(indx-1), &
                           'and',horiznum(indx), &
                           'ARE OUT OF ORDER on screen '//screen)
          else if (horiznum(indx) == horiznum(indx-1)) then
            call pc_error (trim(keyword)//' VALUES',horiznum(indx-1), &
                           'and',horiznum(indx), &
                           'ARE THE SAME on screen '//screen)
          end if
        end do
      end if
      end subroutine dabra_verify_horizons


!!----------------------------- fillout ------------------------------------!!
!!----------------------------- fillout ------------------------------------!!
!!----------------------------- fillout ------------------------------------!!


      subroutine dabra_fillout (obj,horiznum,num,ordinates,fillout)
      type(dabra_struct) ,intent(inout) :: obj                  ! arguments
      integer            ,intent(in)    :: horiznum(:)          ! arguments
      integer            ,intent(in)    :: num                  ! arguments
      real               ,intent(in)    :: ordinates(:)         ! arguments
      real               ,intent(out)   :: fillout(:)           ! arguments
      real                              :: horiznum2(num)       ! local

      horiznum2(:) = horiznum(1:obj%ndpt)

      call terputil_fastsamp &
                      (horiznum2,num,ordinates,1.0,1.0,obj%ndpt,fillout)

      end subroutine dabra_fillout


!!----------------------------- ifillout ------------------------------------!!
!!----------------------------- ifillout ------------------------------------!!
!!----------------------------- ifillout ------------------------------------!!


      subroutine dabra_ifillout (obj,horiznum,num,ordinates,fillout)
      type(dabra_struct) ,intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: horiznum(:)           ! arguments
      integer            ,intent(in)    :: num                   ! arguments
      integer            ,intent(in)    :: ordinates(:)          ! arguments
      integer            ,intent(out)   :: fillout(:)            ! arguments
      real                              :: horiznum2 (num)       ! local
      real                              :: ordinates2(num)       ! local
      real                              :: fillout2  (obj%ndpt)  ! local

      horiznum2(:)  = horiznum (1:num)
      ordinates2(:) = ordinates(1:num)

      call terputil_fastsamp &
                      (horiznum2,num,ordinates2,1.0,1.0,obj%ndpt,fillout2)

      fillout(1:obj%ndpt) = nint(fillout2(:))

      end subroutine dabra_ifillout


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine dabra (obj,ntr,hd,tr)
      type(dabra_struct),intent(inout) :: obj                   ! arguments
      integer           ,intent(inout) :: ntr                   ! arguments
      double precision  ,intent(inout) :: hd(:,:)               ! arguments
      real              ,intent(inout) :: tr(:,:)               ! arguments
      integer                          :: indx,itr,ix,iy,ident  ! local
      real                             :: xcoord,ycoord         ! local
      logical                          :: whoops                ! local

!!!!!!!!!!!!!!! gathering mode:

      if (ntr == obj%numtr) then
           if (obj%kounti == 0) then
                do itr = 1,obj%numtr
                     obj%ident(itr) = nint(hd(obj%hdr_ident,itr))
                end do
                call dabra_first_time (obj,whoops)
                if (whoops) then
                     call dabra_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
           else
                do itr = 1,obj%numtr
                     ident = nint(hd(obj%hdr_ident,itr))
                     if (ident /= obj%ident(itr)) then
                          call pc_error &
                                ('DABRA: trace',itr,'has wrong ident',ident)
                          call pc_error &
                                ('DABRA:',obj%kounti,'gathers previously read')
                          call dabra_wrapup (obj)
                          ntr = FATAL_ERROR
                          return
                     end if
                end do
           end if

           obj%kounti = obj%kounti + 1
           do itr = 1,obj%numtr
                xcoord = hd(obj%hdr_x,itr)
                ycoord = hd(obj%hdr_y,itr)
                ix = mth_bin_number(obj%x_init, obj%x_inc, xcoord)
                iy = mth_bin_number(obj%y_init, obj%y_inc, ycoord)
                if (ix < 1 .or. ix > obj%x_tot) cycle
                if (iy < 1 .or. iy > obj%y_tot) cycle

                do indx = 1,obj%ndpt
                     obj%slices(ix,iy,indx,itr) = tr(indx,itr)
                end do
           end do
           ntr = NEED_TRACES
           return
      end if

!!!!!!!!!!!!!!! solution mode:

      if (ntr == NO_MORE_TRACES) then
           call dabra_solve (obj)
           obj%ix = 0
           obj%iy = 1
           ntr = NEED_TRACES
           !!! do not return here !!!
      end if

!!!!!!!!!!!!!!! output mode:

      if (ntr == NEED_TRACES) then
           obj%ix = obj%ix + 1
           if (obj%ix > obj%x_tot) then
                obj%ix = 1
                obj%iy = obj%iy + 1
                if (obj%iy > obj%y_tot) then
                     call dabra_wrapup (obj)
                     ntr = NO_MORE_TRACES
                     return
                end if
           end if
           obj%kounto = obj%kounto + 1
           do itr = 1,obj%numtr
                hd(        :    ,itr) = 0.0
                hd(        1    ,itr) = itr + obj%numtr * &
                                        (obj%ix + (obj%iy - 1) * obj%x_tot - 1)
                hd(        2    ,itr) = 1.0
                hd(        3    ,itr) = obj%ix + (obj%iy - 1) * obj%x_tot
                hd(        4    ,itr) = itr
                hd(       64    ,itr) = obj%ndpt
                hd(obj%hdr_x    ,itr) = obj%x_init + (obj%ix - 1) * obj%x_inc
                hd(obj%hdr_y    ,itr) = obj%y_init + (obj%iy - 1) * obj%y_inc
                hd(obj%hdr_ident,itr) = obj%ident(itr)
                do indx = 1,obj%ndpt
                     tr(indx,itr) = obj%slices(obj%ix,obj%iy,indx,itr)
                end do
           end do
           ntr = obj%numtr
           call lav_set_hdr (hd,tr,obj%ndpt,ntr)
           return
      end if

!!!!!!!!!!!!!!! error mode:

      call pc_error ('DABRA: wrong number of input traces =',ntr)
      call pc_error ('DABRA: right number of input traces =',obj%numtr)
      call dabra_wrapup (obj)
      ntr = FATAL_ERROR
      return
      end subroutine dabra


!!---------------------------- first time ---------------------------------!!
!!---------------------------- first time ---------------------------------!!
!!---------------------------- first time ---------------------------------!!


      subroutine dabra_first_time (obj,whoops)
      type(dabra_struct),intent(inout) :: obj                ! arguments
      logical           ,intent(out)   :: whoops             ! arguments
      integer                          :: itr                ! local
      character(len=50)                :: start,msg          ! local

!!!!!!!!!!!!!!! get trace indices:

      obj%itr_beta   = 0
      obj%itr_xdip   = 0
      obj%itr_ydip   = 0
      obj%itr_weight = 0
      obj%itr_vav    = 0
      obj%itr_depth  = 0

      do itr = 1,obj%numtr
           select case (obj%ident(itr))
                case (ABRAKADABRA_OUTPUT_BETA) ; obj%itr_beta   = itr
                case (ABRAKADABRA_OUTPUT_XDIP) ; obj%itr_xdip   = itr
                case (ABRAKADABRA_OUTPUT_YDIP) ; obj%itr_ydip   = itr
                case (ABRAKADABRA_WEIGHT     ) ; obj%itr_weight = itr
                case (ABRAKADABRA_OUTPUT_VAV ) ; obj%itr_vav    = itr
                case (ABRAKADABRA_TIMEDEPTH  ) ; obj%itr_depth  = itr
           end select
      end do

!!!!!!!!!!!!!!! print the traces:

      start = '------------------DABRA: trace'
      call pc_print (' ')
      do itr = 1,obj%numtr
           msg = abrakadabra_message(obj%ident(itr))
           call pc_print (start,itr,' has '//msg)
      end do
      call pc_print (' ')

!!!!!!!!!!!!!!! make sure we have the required traces:

      msg = ' '
      if (obj%need_beta   .and. obj%itr_beta   == 0) msg = trim(msg)//' BETA'
      if (obj%need_xdip   .and. obj%itr_xdip   == 0) msg = trim(msg)//' XDIP'
      if (obj%need_ydip   .and. obj%itr_ydip   == 0) msg = trim(msg)//' YDIP'
      if (obj%need_weight .and. obj%itr_weight == 0) msg = trim(msg)//' WEIGHT'
      if (obj%need_vav    .and. obj%itr_vav    == 0) msg = trim(msg)//' VAV'
      if (obj%need_depth  .and. obj%itr_depth  == 0) msg = trim(msg)//' DEPTH'
      if (msg /= ' ') then
           call pc_error ('DABRA: required input traces',msg,'not found')
           whoops = .true.
           return
      end if

      whoops = .false.

      end subroutine dabra_first_time


!!-------------------------------- solve ----------------------------------!!
!!-------------------------------- solve ----------------------------------!!
!!-------------------------------- solve ----------------------------------!!


      subroutine dabra_solve (obj)
      type(dabra_struct),intent(inout) :: obj                   ! arguments
      integer                          :: indx                  ! local
      logical                          :: we_are_at_the_top     ! local
      real             :: beta_previous  (obj%x_tot,obj%y_tot)  ! local
      real             :: beta_keep      (obj%x_tot,obj%y_tot)  ! local

!!!!!!!!!!!!!!! do the editing and smoothing:

      do indx = 1,obj%ndpt

           if (obj%edit_beta) then
                call dabra_edit_and_smooth                              &
                       (obj,obj%num_tr_x_fillout(indx),                 &
                            obj%num_tr_y_fillout(indx),                 &
                            obj%stdev_fillout   (indx),                 &
                            obj%slices(:,:,indx,max(obj%itr_weight,1)), &
                            obj%slices(:,:,indx,max(obj%itr_beta  ,1)),.true.)
           end if

           if (obj%edit_xdip) then
                call dabra_edit_and_smooth                              &
                       (obj,obj%num_tr_x_fillout(indx),                 &
                            obj%num_tr_y_fillout(indx),                 &
                            obj%stdev_fillout   (indx),                 &
                            obj%slices(:,:,indx,max(obj%itr_weight,1)), &
                            obj%slices(:,:,indx,max(obj%itr_xdip  ,1)),.false.)
           end if

           if (obj%edit_ydip) then
                call dabra_edit_and_smooth                              &
                       (obj,obj%num_tr_x_fillout(indx),                 &
                            obj%num_tr_y_fillout(indx),                 &
                            obj%stdev_fillout   (indx),                 &
                            obj%slices(:,:,indx,max(obj%itr_weight,1)), &
                            obj%slices(:,:,indx,max(obj%itr_ydip  ,1)),.false.)
           end if

      end do

!!!!!!!!!!!!!!! do kappa smoothing of the beta data:

      if (obj%edit_beta .and. obj%smooth_kappa_volume) then

           call dabra_smooth_kappa (obj)

      end if

!!!!!!!!!!!!!!! do additional editing of the beta data:

      if (obj%edit_beta) then

           do indx = 1,obj%ndpt
                we_are_at_the_top = (indx == 1)
                beta_keep = obj%slices(:,:,indx,max(obj%itr_beta,1))
                if (we_are_at_the_top) beta_previous = beta_keep
                call dabra_edit_beta                                         &
                      (obj,we_are_at_the_top,                                &
                         obj%vav_min_fillout (indx),                         &
                         obj%vav_max_fillout (indx),                         &
                         obj%vint_min_fillout(indx),                         &
                         obj%vint_max_fillout(indx),                         &
                         obj%slices(:,:,indx,         max(obj%itr_depth,1)), &
                         obj%slices(:,:,indx,         max(obj%itr_vav  ,1)), &
                         obj%slices(:,:,indx,         max(obj%itr_beta ,1)), &
                         obj%slices(:,:,max(indx-1,1),max(obj%itr_depth,1)), &
                         obj%slices(:,:,max(indx-1,1),max(obj%itr_vav  ,1)), &
                         beta_previous)
                beta_previous = beta_keep
           end do

      end if

!!!!!!!!!!!!!!! edit the vav data:

      if (obj%edit_vav) then

           do indx = 1,obj%ndpt
                call dabra_edit_vav                                     &
                        (obj,obj%slices(:,:,indx,max(obj%itr_beta,1)),  &
                             obj%slices(:,:,indx,max(obj%itr_vav ,1)))
           end do

      end if

      end subroutine dabra_solve


!!----------------------------- edit and smooth -----------------------------!!
!!----------------------------- edit and smooth -----------------------------!!
!!----------------------------- edit and smooth -----------------------------!!


      subroutine dabra_edit_and_smooth (obj,num_tr_x,num_tr_y,stdev, &
                                        weight_horizon,attribute_horizon,beta)
      type(dabra_struct),intent(inout) :: obj                      ! arguments
      integer           ,intent(in)    :: num_tr_x                 ! arguments
      integer           ,intent(in)    :: num_tr_y                 ! arguments
      real              ,intent(in)    :: stdev                    ! arguments
      real              ,intent(in)    :: weight_horizon   (:,:)   ! arguments
      real              ,intent(inout) :: attribute_horizon(:,:)   ! arguments
      logical           ,intent(in)    :: beta                     ! arguments
      real                           :: keep(obj%x_tot,obj%y_tot)  ! local

!!!!!!!!!!!!!!! remove anomalous values from the attribute horizon:

      if (obj%remove_bad_values) then
           call dabra_edit (obj,num_tr_x,num_tr_y,stdev, &
                            weight_horizon,attribute_horizon)
      end if

!!!!!!!!!!!!!!! remove impossible values from the beta attribute cube:

      if (beta) then
           where (attribute_horizon <= -1.0) attribute_horizon = FNIL
      end if

!!!!!!!!!!!!!!! replace nils in attribute horizon before smoothing:

      keep(:,:) = attribute_horizon(1:num_tr_x,1:num_tr_y)
      call dabra_replace_nils (obj,obj%replace_nils_before,attribute_horizon)

!!!!!!!!!!!!!!! smooth the attribute horizon:

      if (obj%apply_smoothing) then
           call dabra_smooth (obj,num_tr_x,num_tr_y, &
                              weight_horizon,attribute_horizon)
      end if

!!!!!!!!!!!!!!! replace nils in attribute horizon after smoothing:

      call dabra_replace_nils &
                       (obj,obj%replace_nils_after,attribute_horizon,keep)

      end subroutine dabra_edit_and_smooth


!!--------------------------- smooth kappa ----------------------------------!!
!!--------------------------- smooth kappa ----------------------------------!!
!!--------------------------- smooth kappa ----------------------------------!!

                 ! gamma = vav_old / vav_new
                 ! kappa = vint_old / vint_new
                 ! input and output can contain nils.

      subroutine dabra_smooth_kappa (obj)
      type(dabra_struct),intent(inout) :: obj                   ! arguments
      integer            :: ix,iy,indx,itrace,nrun              ! local
      real               :: keep(obj%x_tot,obj%y_tot,obj%ndpt)  ! local
      real               :: beta,gamma,gamma1,gamma2,dmax,dx    ! local
      real               :: depth1,depth2,kappa2,depth,kappa    ! local
      integer,parameter  :: NTRACE = 1001                       ! local
      real               :: trace(NTRACE)                       ! local

!!!!!!!!!!!!!!! get rid of all nils in the beta volume:

      where (obj%slices(:,:,:,obj%itr_beta) <= -1.0) &
             obj%slices(:,:,:,obj%itr_beta) = FNIL

      keep = obj%slices(:,:,:,obj%itr_beta)

      do indx = 1,obj%ndpt
           call terputil_replace_nilx &
                    (obj%x_tot,obj%y_tot,obj%slices(:,:,indx,obj%itr_beta))
      end do

!!!!!!!!!!!!!!! convert beta to gamma (no input nils):

      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot
      do indx = 1,obj%ndpt

           beta = obj%slices(ix,iy,indx,obj%itr_beta)
           gamma = sqrt(beta + 1.0)
           obj%slices(ix,iy,indx,obj%itr_beta) = gamma

      end do
      end do
      end do

!!!!!!!!!!!!!!! convert gamma to kappa (no input nils):

      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot
      do indx = 1,obj%ndpt     ! kappa == gamma for indx == 1.

           gamma2 = obj%slices(ix,iy,indx,obj%itr_beta)
           depth2 = obj%slices(ix,iy,indx,obj%itr_depth)

           if (indx == 1) then
                kappa2 = gamma2
           else if (depth2 <= depth1 .or. depth1 < 0.0) then
                kappa2 = FNIL
           else
                kappa2 = (depth2 - depth1) / (depth2/gamma2 - depth1/gamma1)
           end if

           obj%slices(ix,iy,indx,obj%itr_beta) = kappa2
           gamma1 = gamma2
           depth1 = depth2

      end do
      end do
      end do

!!!!!!!!!!!!!!! reset previously nil values back to nil:

      do indx = 1,obj%ndpt
           call dabra_replace_nils                                 &
                     (obj,'reset interpolated values back to nil', &
                      obj%slices(:,:,indx,obj%itr_beta),keep(:,:,indx))
      end do

!!!!!!!!!!!!!!! laterally smooth the kappa horizons (might be input nils):

      do indx = 1,obj%ndpt

           call dabra_smooth (obj,obj%num_tr_x_fillout(indx),               &
                                obj%num_tr_y_fillout(indx),                 &
                                obj%slices(:,:,indx,max(obj%itr_weight,1)), &
                                obj%slices(:,:,indx,    obj%itr_beta     ))

      end do

!!!!!!!!!!!!!!! get rid of all nils introduced into the kappa volume:

      do indx = 1,obj%ndpt
           call terputil_replace_nilx &
                    (obj%x_tot,obj%y_tot,obj%slices(:,:,indx,obj%itr_beta))
      end do

!!!!!!!!!!!!!!! vertically smooth the kappa traces (no input nils):

      if (obj%vertical_smoothing_dist > 0.0) then
      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot

            dmax = obj%slices(ix,iy,obj%ndpt,obj%itr_depth)
            dx = dmax / (NTRACE - 1)
            nrun = nint((NTRACE * obj%vertical_smoothing_dist) / dmax)
            trace(:) = FNIL

            do indx = 1,obj%ndpt
                 kappa = obj%slices(ix,iy,indx,obj%itr_beta)
                 depth = obj%slices(ix,iy,indx,obj%itr_depth)
                 itrace = 1 + nint(depth/dx)
                 call mth_constrain (itrace,1,NTRACE)
                 trace(itrace) = kappa
            end do

            call terputil_replace_nils    (trace,NTRACE)
            call statutil_1d_smooth_quick (trace,NTRACE,nrun)

            do indx = 1,obj%ndpt
                 depth = obj%slices(ix,iy,indx,obj%itr_depth)
                 itrace = 1 + nint(depth/dx)
                 call mth_constrain (itrace,1,NTRACE)
                 kappa = trace(itrace)
                 obj%slices(ix,iy,indx,obj%itr_beta) = kappa
            end do

      end do
      end do
      end if

!!!!!!!!!!!!!!! convert kappa to gamma (no input nils):

      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot
      do indx = 1,obj%ndpt     ! gamma == kappa for indx == 1.

           kappa2 = obj%slices(ix,iy,indx,obj%itr_beta)
           depth2 = obj%slices(ix,iy,indx,obj%itr_depth)

           if (indx == 1) then
                gamma2 = kappa2
           else if (kappa2 == FNIL   .or. gamma1 == FNIL .or.  &
                    kappa2 <= 0.0    .or. gamma1 <= 0.0  .or.  &
                    depth2 <= depth1 .or. depth1 <  0.0) then
                gamma2 = FNIL
           else
                gamma2 = depth2 / ( (depth2-depth1)/kappa2 + depth1/gamma1 )
           end if

           obj%slices(ix,iy,indx,obj%itr_beta) = gamma2
           gamma1 = gamma2
           depth1 = depth2

      end do
      end do
      end do

!!!!!!!!!!!!!!! convert gamma to beta (might be input nils):

      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot
      do indx = 1,obj%ndpt

           gamma = obj%slices(ix,iy,indx,obj%itr_beta)
           if (gamma /= FNIL) then
                beta = gamma**2 - 1.0
                obj%slices(ix,iy,indx,obj%itr_beta) = beta
           end if

      end do
      end do
      end do

!!!!!!!!!!!!!!! reset previously nil values back to nil:

      do indx = 1,obj%ndpt
           call dabra_replace_nils                                 &
                     (obj,'reset interpolated values back to nil', &
                      obj%slices(:,:,indx,obj%itr_beta),keep(:,:,indx))
      end do

      end subroutine dabra_smooth_kappa


!!--------------------------- edit beta -------------------------------------!!
!!--------------------------- edit beta -------------------------------------!!
!!--------------------------- edit beta -------------------------------------!!


      subroutine dabra_edit_beta (obj,we_are_at_the_top,                 &
                                      vav_min,vav_max,vint_min,vint_max, &
                                      depth_horizon,vav_horizon,         &
                                      beta_horizon,depth_previous,       &
                                      vav_previous,beta_previous)
      type(dabra_struct),intent(inout) :: obj                      ! arguments
      logical           ,intent(in)    :: we_are_at_the_top        ! arguments
      real              ,intent(in)    :: vav_min,vav_max          ! arguments
      real              ,intent(in)    :: vint_min,vint_max        ! arguments
      real              ,intent(in)    :: depth_horizon (:,:)      ! arguments
      real              ,intent(in)    :: vav_horizon   (:,:)      ! arguments
      real              ,intent(inout) :: beta_horizon  (:,:)      ! arguments
      real              ,intent(in)    :: depth_previous(:,:)      ! arguments
      real              ,intent(in)    :: vav_previous  (:,:)      ! arguments
      real              ,intent(in)    :: beta_previous (:,:)      ! arguments
      real            :: input_vav_horizon  (obj%x_tot,obj%y_tot)  ! local
      real            :: input_vav_previous (obj%x_tot,obj%y_tot)  ! local
      real            :: input_vint_horizon (obj%x_tot,obj%y_tot)  ! local
      real            :: output_vav_horizon (obj%x_tot,obj%y_tot)  ! local
      real            :: output_vav_previous(obj%x_tot,obj%y_tot)  ! local
      real            :: output_vint_horizon(obj%x_tot,obj%y_tot)  ! local

!!!!!!!!!!!!!!! replace impossible input values with nil:

      input_vav_horizon  = vav_horizon
      input_vav_previous = vav_previous

      where (beta_horizon       <= -1.0) beta_horizon       = FNIL
      where (input_vav_horizon  <=  0.0) input_vav_horizon  = FNIL
      where (input_vav_previous <=  0.0) input_vav_previous = FNIL

!!!!!!!!!!!!!!! get input interval velocities from input average velocities:

      if (obj%apply_vint_constraints) then
           call dabra_vint_from_vav (obj,we_are_at_the_top,         &
                              input_vav_horizon,input_vint_horizon, &
                              depth_horizon,depth_previous,input_vav_previous)
      end if

!!!!!!!!!!!!!!! edit the average velocities using the edited BETA horizon:

      where (input_vav_horizon /= FNIL .and. beta_horizon /= FNIL)
           output_vav_horizon = input_vav_horizon / sqrt(beta_horizon + 1.0)
      elsewhere
           output_vav_horizon = FNIL
      end where

      where (input_vav_previous /= FNIL .and. beta_previous /= FNIL)
           output_vav_previous = input_vav_previous / sqrt(beta_previous + 1.0)
      elsewhere
           output_vav_previous = FNIL
      end where

!!!!!!!!!!!!!!! get output interval velocities from output average velocities:

      if (obj%apply_vint_constraints) then
           call dabra_vint_from_vav (obj,we_are_at_the_top,           &
                              output_vav_horizon,output_vint_horizon, &
                              depth_horizon,depth_previous,output_vav_previous)
      end if

!!!!!!!!!!!!!!! apply velocity contraints to the edited BETA horizon:

      if (obj%apply_vav_constraints) then
        where (output_vav_horizon == FNIL                       .or. &
               input_vav_horizon  == FNIL                       .or. &
               output_vav_horizon < vav_min * input_vav_horizon .or. &
               output_vav_horizon > vav_max * input_vav_horizon)
             beta_horizon = FNIL
        end where
      end if

      if (obj%apply_vint_constraints) then
        where (output_vint_horizon == FNIL                         .or. &
               input_vint_horizon  == FNIL                         .or. &
               output_vint_horizon < vint_min * input_vint_horizon .or. &
               output_vint_horizon > vint_max * input_vint_horizon)
             beta_horizon = FNIL
        end where
      end if

!!!!!!!!!!!!!!! optionally replace nil values in the new edited BETA horizon:

      call dabra_replace_nils &
                     (obj,obj%replace_nils_after_constraints,beta_horizon)

      end subroutine dabra_edit_beta


!!----------------------------- edit vav ------------------------------------!!
!!----------------------------- edit vav ------------------------------------!!
!!----------------------------- edit vav ------------------------------------!!


      subroutine dabra_edit_vav (obj,beta_horizon,vav_horizon)
      type(dabra_struct),intent(inout) :: obj                      ! arguments
      real              ,intent(in)    :: beta_horizon  (:,:)      ! arguments
      real              ,intent(inout) :: vav_horizon   (:,:)      ! arguments

!!!!!!!!!!!!!!! replace impossible input values with nil:

      where (vav_horizon <= 0.0) vav_horizon = FNIL

!!!!!!!!!!!!!!! edit average velocities using the edited BETA:

      where (vav_horizon /= FNIL .and. beta_horizon /= FNIL)
           vav_horizon = vav_horizon / sqrt(beta_horizon + 1.0)
      end where

!!!!!!!!!!!!! always replace nil values in new edited average velocity horizon:

      call terputil_replace_nilx (obj%x_tot,obj%y_tot,vav_horizon)

      end subroutine dabra_edit_vav


!!---------------------------- vint from vav --------------------------------!!
!!---------------------------- vint from vav --------------------------------!!
!!---------------------------- vint from vav --------------------------------!!

                 ! input and output can contain nils.


      subroutine dabra_vint_from_vav (obj,we_are_at_the_top,    &
                                      vav_horizon,vint_horizon, &
                                      depth_horizon,depth_previous,vav_previous)
      type(dabra_struct),intent(inout) :: obj                       ! arguments
      logical           ,intent(in)    :: we_are_at_the_top         ! arguments
      real              ,intent(in)    :: vav_horizon   (:,:)       ! arguments
      real              ,intent(out)   :: vint_horizon  (:,:)       ! arguments
      real              ,intent(in)    :: depth_horizon (:,:)       ! arguments
      real              ,intent(in)    :: depth_previous(:,:)       ! arguments
      real              ,intent(in)    :: vav_previous  (:,:)       ! arguments
      integer            :: ix,iy                                   ! local
      real               :: depth1,depth2,vav1,vav2                 ! local
      real               :: time1,time2,time,vint,depth             ! local
      real               :: temp_vav_horizon (obj%x_tot,obj%y_tot)  ! local
      real               :: temp_vav_previous(obj%x_tot,obj%y_tot)  ! local

      if (we_are_at_the_top) then
           vint_horizon = vav_horizon
           return
      end if

      temp_vav_horizon  = vav_horizon
      temp_vav_previous = vav_previous

      call terputil_replace_nilx (obj%x_tot,obj%y_tot,temp_vav_horizon)
      call terputil_replace_nilx (obj%x_tot,obj%y_tot,temp_vav_previous)

!!!!!!!!!!!!!!! do the following if trace sample interval is constant in time:

      if (obj%horizon_unit == 'time') then

           do iy = 1,obj%y_tot
           do ix = 1,obj%x_tot

                time1  = depth_previous   (ix,iy)
                time2  = depth_horizon    (ix,iy)
                vav1   = temp_vav_previous(ix,iy)
                vav2   = temp_vav_horizon (ix,iy)
                depth1 = time1 * vav1
                depth2 = time2 * vav2
                time   = time2 - time1
                depth  = depth2 - depth1

                if (time > 0.0 .and. depth > 0.0) then
                     vint = depth / time
                else
                     vint = FNIL
                end if

                vint_horizon(ix,iy) = vint

           end do
           end do

!!!!!!!!!!!!!!! do the following if trace sample interval is constant in depth:

      else

           do iy = 1,obj%y_tot
           do ix = 1,obj%x_tot

                depth1 = depth_previous   (ix,iy)
                depth2 = depth_horizon    (ix,iy)
                vav1   = temp_vav_previous(ix,iy)
                vav2   = temp_vav_horizon (ix,iy)
                time1  = depth1 / vav1
                time2  = depth2 / vav2
                time   = time2 - time1
                depth  = depth2 - depth1

                if (time > 0.0 .and. depth > 0.0) then
                     vint = depth / time
                else
                     vint = FNIL
                end if

                vint_horizon(ix,iy) = vint

           end do
           end do

      end if

      where (vint_horizon <=  0.0) vint_horizon = FNIL
      where (vav_horizon  == FNIL) vint_horizon = FNIL

      end subroutine dabra_vint_from_vav


!!------------------------------- edit -----------------------------------!!
!!------------------------------- edit -----------------------------------!!
!!------------------------------- edit -----------------------------------!!

      ! anomalous values based on standard deviation are removed from
      ! the entire attribute_horizon.


      subroutine dabra_edit (obj,num_tr_x,num_tr_y,stdev, &
                             weight_horizon,attribute_horizon)
      type(dabra_struct),intent(in)    :: obj                      ! arguments
      integer           ,intent(in)    :: num_tr_x                 ! arguments
      integer           ,intent(in)    :: num_tr_y                 ! arguments
      real              ,intent(in)    :: stdev                    ! arguments
      real              ,intent(in)    :: weight_horizon   (:,:)   ! arguments
      real              ,intent(inout) :: attribute_horizon(:,:)   ! arguments
      real                         :: edited(obj%x_tot,obj%y_tot)  ! local
      integer                          :: total,kount              ! local
      real                             :: esum2,esum,wsum,weight   ! local
      real                             :: fraction,average,stdev2  ! local
      integer                          :: ix,iy,kx,ky              ! local
      integer                          :: nx1,nx2,ny1,ny2          ! local

!!!!!!!!!!!!!!! calculate edited array:

      fraction = 0.01 * obj%editing_percent_valid

      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot
           esum2 = 0.0
           esum  = 0.0
           wsum  = 0.0
           kount = 0
           total = 0
           ny1 = max(iy - num_tr_y, 1)
           ny2 = min(iy + num_tr_y, obj%y_tot)
           nx1 = max(ix - num_tr_x, 1)
           nx2 = min(ix + num_tr_x, obj%x_tot)
           do ky = ny1,ny2
           do kx = nx1,nx2
                total  = total  + 1
                if (attribute_horizon(kx,ky) == FNIL) cycle
                if (obj%use_weighted_edit) then
                     weight = weight_horizon(kx,ky)
                else
                     weight = 1.0
                end if
                if (weight == FNIL) cycle
                esum2  = esum2 + weight * attribute_horizon(kx,ky)**2
                esum   = esum  + weight * attribute_horizon(kx,ky)
                wsum   = wsum  + weight
                kount  = kount + 1
           end do
           end do
           if (kount < fraction * total) then
                edited(ix,iy) = FNIL
           else if (wsum == 0.0) then
                edited(ix,iy) = FNIL
           else if (kount == 0) then
                edited(ix,iy) = FNIL
           else
                average = esum / wsum
                stdev2  = sqrt(esum2 / wsum)
                if (abs(attribute_horizon(ix,iy) - average) &
                               > stdev2 * stdev) then
                     edited(ix,iy) = FNIL
                else
                     edited(ix,iy) = attribute_horizon(ix,iy)
                end if
           end if
      end do
      end do

!!!!!!!!!!!!!!! save edited array in attribute horizon:

      attribute_horizon(:,:) = edited(:,:)

      end subroutine dabra_edit


!!------------------------------- smooth -----------------------------------!!
!!------------------------------- smooth -----------------------------------!!
!!------------------------------- smooth -----------------------------------!!


      subroutine dabra_smooth (obj,num_tr_x,num_tr_y, &
                               weight_horizon,attribute_horizon)
      type(dabra_struct),intent(in)    :: obj                       ! arguments
      integer           ,intent(in)    :: num_tr_x                  ! arguments
      integer           ,intent(in)    :: num_tr_y                  ! arguments
      real              ,intent(in)    :: weight_horizon   (:,:)    ! arguments
      real              ,intent(inout) :: attribute_horizon(:,:)    ! arguments
      integer                          :: total,kount               ! local
      real                             :: aweight,fraction          ! local
      real                 :: smoothed(obj%x_tot,obj%y_tot)         ! local
      real                 :: trimlist(obj%x_tot*obj%y_tot)         ! local
      real                 :: dweights(2*num_tr_x+1,2*num_tr_y+1)   ! local
      real                 :: tweights(2*num_tr_x+1,2*num_tr_y+1)   ! local
      real                             :: esum,wsum,weight          ! local
      integer                          :: ix,iy,kx,ky               ! local
      integer                          :: nx1,nx2,ny1,ny2           ! local
      integer                          :: midx,midy,nx,ny           ! local

!!!!!!!!!!!!!!! get distance weights and tapered weights:

      nx   = 2 * num_tr_x + 1
      ny   = 2 * num_tr_y + 1
      midx = 1 + num_tr_x
      midy = 1 + num_tr_y

      call kadabra_get_distance_weights (obj%use_distance_smoothing,  &
                                         obj%center_weight_percent,   &
                                         obj%kounto,dweights,nx,ny)

      call kadabra_get_tapered_weights  (obj%use_tapered_smoothing,   &
                                         obj%tapered_weight_percent,  &
                                         obj%kounto,tweights,nx,ny)

!!!!!!!!!!!!!!! calculate smoothed array:

      fraction = 0.01 * obj%smoothing_percent_valid

      do iy = 1,obj%y_tot
      do ix = 1,obj%x_tot
           esum  = 0.0
           wsum  = 0.0
           kount = 0
           total = 0
           ny1 = max(iy - num_tr_y, 1)
           ny2 = min(iy + num_tr_y, obj%y_tot)
           nx1 = max(ix - num_tr_x, 1)
           nx2 = min(ix + num_tr_x, obj%x_tot)
           do ky = ny1,ny2
           do kx = nx1,nx2
                total  = total  + 1
                if (attribute_horizon(kx,ky) == FNIL) cycle
                if (obj%use_weighted_smoothing) then
                     aweight = weight_horizon(kx,ky)
                else
                     aweight = 1.0
                end if
                if (aweight == FNIL) cycle
                weight = aweight * dweights(midx+kx-ix,midy+ky-iy) &
                                 * tweights(midx+kx-ix,midy+ky-iy)
                esum   = esum  + weight * attribute_horizon(kx,ky)
                wsum   = wsum  + weight
                kount  = kount + 1
                trimlist(kount) = attribute_horizon(kx,ky)
           end do
           end do
           if (kount < fraction * total) then
                smoothed(ix,iy) = FNIL
           else if (wsum == 0.0) then
                smoothed(ix,iy) = FNIL
           else if (obj%smoothing_trim_percent == 0.0) then
                smoothed(ix,iy) = esum / wsum
           else
                smoothed(ix,iy) = statutil_trimmed_mean &
                                   (trimlist,kount,obj%smoothing_trim_percent)
           end if
      end do
      end do

!!!!!!!!!!!!!!! save smoothed array in attribute horizon:

      attribute_horizon(:,:) = smoothed(:,:)

      end subroutine dabra_smooth


!!--------------------------- replace nils --------------------------------!!
!!--------------------------- replace nils --------------------------------!!
!!--------------------------- replace nils --------------------------------!!


      subroutine dabra_replace_nils (obj,replace_nils,slice,keep)
      type(dabra_struct),intent(in)    :: obj                  ! arguments
      character(len=*)  ,intent(in)    :: replace_nils         ! arguments
      real              ,intent(inout) :: slice(:,:)           ! arguments
      real   ,optional  ,intent(in)    :: keep (:,:)           ! arguments

      select case (replace_nils)
           case ('use bilinear interpolation')
                       call terputil_replace_nilx (obj%x_tot,obj%y_tot,slice)
           case ('replace nils with zeroes')
                       where (slice == FNIL) slice = 0.0
           case ('do not replace nils')
                       continue
           case ('reset interpolated values back to nil')
                       if (present(keep)) then
                            where (keep == FNIL) slice = FNIL
                       end if
           case default
                       continue
      end select

      end subroutine dabra_replace_nils


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine dabra_wrapup (obj)
      type(dabra_struct),intent(inout) :: obj              ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      write (lunprint,*) ' '
      write (lunprint,*) '@@@@@@@@@@@@@@@ START DABRA WRAPUP @@@@@@@@@@@@@@@@'
      write (lunprint,*) ' '
      write (lunprint,*) obj%kounti,' input CMP locations'
      write (lunprint,*) obj%kounto,' output CMP locations'
      write (lunprint,*) ' '
      write (lunprint,*) '@@@@@@@@@@@@@@@@ END DABRA WRAPUP @@@@@@@@@@@@@@@@@'
      write (lunprint,*) ' '

      end subroutine dabra_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module dabra_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

