!<CPS_v1 type="PROCESS"/>


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
! Name       : VPICK
! Category   : velocity_analysis
! Written    : 1991-04-23   by: Bill Harlan
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Automatic stacking velocity picking of input CMPs.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! VPICK automatically estimates normal-moveout corrections for each input
! CMP gather.  The velocity functions picked by VPICK are constrained to be
! within a user specified deviation from the reference stacking velocities
! contained in the velocity file PATHNAME_VEL_IN.
!
! The input data is NMO corrected with the revised stacking velocities and then
! output.  The revised stacking velocities are written to the velocity file
! PATHNAME_VEL_OUT.
!
! VPICK internally performs a semblance stacking velocity analysis on each CMP
! gather.  Semblance panels are muted to exclude all velocities not satisfying
! the constraints.  Stacking velocity functions are perturbed until the total
! (integrated) semblance along each velocity function is maximized.
!
! (Automatic moveout correction of CMPs that have been NMO corrected is
! provided by the VTRIM process.)
!
! (The VTRIM process was formerly part of the old Cray VPICK process.)
!
!-------------------------------------------------------------------------------
!                     CONSTRAINTS ON ESTIMATED VELOCITY
!
! The allowed deviations of the estimated velocity functions from the reference
! functions can be time dependent and are of a type defined by CONSTRAINT.
!
! The allowed range of residual moveout correction is specified by a linked
! array of RMO_MIN, RMO_MAX, TIMES and OFF_MAX values where:
!
!       TIMES is the array of zero-offset times in seconds.
!
!       OFF_MAX is the array of maximum offset values.
!
!   If CONSTRAINT=RMO: 
!
!       RMO_MIN is the array of the smallest (negative) residual moveout
!       correction allowed, in seconds, for the specified zero-offset time
!       at the specified far offset.
!
!       RMO_MAX is the array of the largest (positive) residual moveout
!       correction allowed, in seconds, for the specified zero-offset time
!       at the specified far offset.
!
!   If CONSTRAINT=MINMAX: 
!
!       RMO_MIN is the array of the minimum velocity for the specified
!       zero-offset time.
!
!       RMO_MAX is the array of the maximum velocity for the specified
!       zero-offset time.
!
!   If CONSTRAINT=TOLERANCE: 
!
!       RMO_MIN is the array of the smallest (negative) change in
!       velocity for the specified zero-offset time.
!
!       RMO_MAX is the array of the largest (positive) change in
!       velocity for the specified zero-offset time.
!
!   If CONSTRAINT=FRACTION: 
!
!       RMO_MIN is the array of the smallest (negative) fractional
!       change in velocity for the specified zero-offset time.
!
!       RMO_MAX is the array of the largest (positive) fractional
!       change in velocity for the specified zero-offset time.
!
! At least two constraints per second should be made.  VPICK converts
! all constraints to squared slowness for interpolation.
!
!-------------------------------------------------------------------------------
!          OUTPUT VELOCITY FUNCTION TEMPORAL SMOOTHING AND SAMPLING
!
! TIM_SMOOTH is the length of a temporal running average smoothing operator
! applied to the estimated velocity functions.  Output velocity functions will
! have a time sample interval of SAMP_INT, which cannot be greater than
! 0.5 * TIM_SMOOTH.
!
! NOTE: Currently SAMP_INT is not implemented, and output velocity functions
! will always have a time sample interval of 0.5 * TIM_SMOOTH.
!
! NOTE: Remember that time smoothing and resampling can be performed
! afterwards by the VA program instead of (or in addition to) any time
! smoothing or resampling performed by this process.
!
!-------------------------------------------------------------------------------
!           OUTPUT VELOCITY FUNCTION LATERAL SMOOTHING AND INCREMENT
!
! Velocity functions are calculated for each input CMP, then laterally smoothed
! over the number of input CMP gathers specified by CMP_SMOOTH.  Thus, the
! picked velocity functions will not change too abruptly.  Normally CMP_SMOOTH
! should be set to 3 or more.
!
! NOTE: Some smoothing takes place even when CMP_SMOOTH is 1.  To eliminate
! all smoothing between CMPs, set CMP_SMOOTH to 0.
!
! NOTE: For parallel jobs, CMP_SMOOTH must be set to zero and any desired
! lateral smoothing must be performed in the VA program.  This is required
! when running a parallel job, because adjacent CMP gathers will normally be
! processed by different CPUs who are not communicating with each other.
! To smooth velocity functions laterally in such cases, use the VA program.
!
! NOTE: Even for scalar jobs, CMP_SMOOTH should be set to zero for 3D data,
! because otherwise the end of one line will be mixed with the beginning
! of the next line.  If the data are not sorted by CMP, undesirable mixing
! will occur even with 2D data.  Also be aware that the mixing done in this
! process is not symmetric, because earlier CMPs are mixed into later CMPs but
! not vice versa.  If any of these behaviors are undesirable, CMP_SMOOTH
! should be set to zero and any desired lateral smoothing should be
! performed in the VA program.
!
! One velocity function will be output for each CMP_INC input CMPs.
!
! NOTE: Currently CMP_INC is not yet implemented.  Therefore one velocity
! function will be output for each input CMP.
!
! NOTE: Remember that lateral smoothing and resampling can be performed
! afterwards by the VA program instead of (or in addition to) any lateral
! smoothing or resampling performed by this process.
!
!-------------------------------------------------------------------------------
!                      RESTRICTIONS ON CMP GATHER SIZE
!
! Picking will not be performed on a CMP if (1) the CMP gather contains fewer
! than eight live traces or (2) the nearest live trace offset is more than
! half the farthest live trace offset.  Instead, reference velocities will
! be used, without change, for output and NMO corrections.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Preprocessing:
!
! Preprocessing of input data should be the same as for any semblance velocity
! analysis.  Normally this consists of a strong amplitude balancing (XP) and a
! filter (e.g. 8-45 Hz).  Compositing over midpoint may help in areas of low
! signal to noise ratio.
!
!
! Examples of Constraints:
!
!   If CONSTRAINT=RMO, MIN and MAX are allowed residual moveout.
!            Example:  [MIN = -0.05 (seconds) and MAX = 0.06 (seconds)]
!
!   If CONSTRAINT=MINMAX, MIN and MAX are allowed minimum and maximum
!       velocity.
!        Example:  [MIN = 7800 (feet/sec) and MAX = 8600 (feet/sec)]
!
!   If CONSTRAINT=TOLERANCE, MIN and MAX are allowed positive and negative
!   changes in velocity.
!        Example:  [MIN = -300 (feet/sec) and MAX = 250 (feet/sec)]
!
!   If CONSTRAINT=FRACTION, MIN and MAX are allowed positive and negative
!   fractional changes in velocity.
!        Example:  [MIN = -0.04 (unitless) and MAX = 0.05 (unitless)]
!
!
! Residual Velocity Analysis:
!
! If you wish to perform a residual velocity analysis you can apply a reverse
! NMO correction to your already corrected data (using a constant velocity or a
! single velocity function) and then perform a velocity analysis done in the
! normal fashion.  The difference between the velocity field resulting from the
! velocity analysis and the velocity field used for the reverse NMO is the
! "residual velocity."
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
! Process requires traces to be input in CMP gathers without NMO correction
! applied.
!
! WARNING: Negative trace times are not treated correctly.  Normally,
! traces passed to this process should not have negative trace times,
! because this process is properly used at the velocity analysis and NMO
! correction phase of processing, which should be done before large
! trace shifts to final datum.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs NMO corrected traces with same gather status as the
! input traces.
!
! NOTE: Velocity functions on the output velocity file will be in the order
! of the input and output trace gathers.  But for parallel jobs, velocity
! functions on the output velocity file will not be in any expected order.
! For most purposes this should not matter.  For example, NMO no longer
! requires input velocity functions to be in any particular order.  However,
! remember that the VA program can always be used to sort or resample the
! velocity functions on a velocity file.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NUM_CPUS number of CPUs in a parallel job      (job data parameter)
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GATHERED gathered flag                         used but not changed
! NUMTR    maximum number of traces per gather   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                    Action taken
! ----    -----------                    ------------
!   2     Top mute                       Modified
!   6     Offset                         Used
!   7     Midpoint X grid coordinate     Used
!   8     Midpoint Y grid coordinate     Used
!  17     Midpoint X survey coordinate   Used
!  18     Midpoint Y survey coordinate   Used
!  37     Midpoint shotpoint             Used
!  38     Midpoint line number           Used
!  25     LAV                            Reset
!  64     Bottom mute                    Modified
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!046. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
! 45. 2005-12-22  Stoeckley  Make parallel.  Add ability to set CMP_SMOOTH
!                             to zero.  Add documentation.
! 44. 2005-08-09  Goodger    Change argument ierr from intent(out) to 
!                            intent(inout) to make the absoft 9.0 compiler
!                            happy.
! 43. 2003-06-17  Goodger    Add use getlun_module.
! 42. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
! 41. 2001-04-04  Stoeckley  Fix GUI bug which did not allow array input.
! 40. 2001-02-27  Stoeckley  Fix bug involving X and Y coordinate values in
!                             the output velocity file; change the X and Y
!                             coordinate header words in the output file to
!                             match those in the input velocity file.
! 39. 2001-02-12  Stoeckley  Add warning in documentation about the fact that
!                             negative trace times are treated incorrectly.
! 38. 2001-01-26  Stoeckley  Move most of the code into a new primitive named
!                             VTRIMMER where it can be used by both VTRIM and
!                             VPICK; remove code which re-muted the data;
!                             change header word named constants to those in
!                             the NAMED_CONSTANTS module; fixed a bug which
!                             incorrectly used velocity files with headers 7,8
!                             or 17,18; fixed a number of bugs in the moved
!                             code (see the VTRIMMER revision history for
!                             details); implement CONSTRAINT.
! 37. 2001-01-22  Stoeckley  Fix error which was using PATHNAME_VEL_IN for
!                             output instead of PATHNAME_VEL_OUT.
! 36. 2000-12-28  Stoeckley  Continue conversion to new system; SAMP_INT,
!                             CMP_INC, and CONSTRAINT not yet implemented.
! 35. 2000-12-15  Coleman    Converted to new system.
! 34. 1996-06-20  Vunderink  Added SAVE FILENO to VPICKFNEW subroutine
! 33. 1995-04-03  Harlan     Allow more than 1999 picks per function.
! 32. 1994-10-17  Harlan     Shorten dcode card to 80 characters. Fortran!
! 31. 1994-10-07  Harlan     Yet another typo in internal NMC dcode card.
! 30. 1994-10-06  Harlan     Fix problem interpolating input file headers.
! 29. 1994-08-29  Harlan     Update documentation.
! 28. 1994-07-18  Harlan     Fix another NCODE typo calling NMC.
! 27. 1994-07-15  Harlan     Fix NCODE problem calling NMC internally.
! 26. 1994-07-15  Harlan     Bad input interpolation default, header 7&8.
! 25. 1994-07-15  Harlan     Add more scratch memory.
! 24. 1994-06-29  Harlan     Fix bottom mute.
! 23. 1994-05-06  Harlan     Allow OUTFILE="NONE"
! 22. 1994-04-27  Harlan     Change name to VPICK; add header word NHYVEL
!         for 3D; call NMC internally for input velocity interpolation;
!         get header words from VELFILE.
! .....................................................................
! Lower revisions refer to earlier name VPIK
! 21. 1994-03-04  Harlan     Double resolution of semblances, slower by 2.
! 20. 1994-02-23  Harlan      Avoid SQRT of negative number in VPICKRMT3.
! 19. 1993-05-04  Harlan      Make variable IMIN static in VPICKPARM.
! 18. 1993-04-29  Harlan      Initialization in VPICKSPCK for new compiler.
! 17. 1993-02-12  Harlan      Fix VPICKCOMP for bottom zero padding.
! 16. 1992-09-03  Harlan      Allow initial velocities outside constraints
! 15. 1992-07-31  Harlan      Add "FLAT" option for VFILE.
! 14. 1992-03-24  Harlan      I-O negative moveouts as negative velocities
! 13. 1992-02-14  Harlan      Ignore dead traces when averaging headers.
! 12. 1992-02-03  Harlan      Increase resolution of internal semblances.
! 11. 1992-01-24  Harlan      Reduce significant digits in veloc. file.
! 10. 1992-01-24  Harlan      Changes to documentation only.
! 9.  1991-12-30  Harlan      If can't pick, apply moveout anyway.
! 8.  1991-12-20  Harlan      Output of VPICK uses smoothed velocities.
! 7.  1991-11-26  Harlan      Minor changes in comments to *.cpr file.
! 6.  1991-10-31  Harlan      Check for dead gathers. Insure N < NTRMAX.
! 5.  1991-10-02  Harlan      ASCII semblance plots to *.cpr file.
! 4.  1991-08-01  Harlan      Add smoothing of picks over midpoint.
! 3.  1991-06-20  Harlan      Adjust to UNICOS changes in CLOSFIL.
! 2.  1991-06-05  Harlan      Add new constraint options.
! 1.  1991-04-23  Harlan      Original version
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
!
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
! NDISK           >0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
! PCPS_SEND_MODE           PCPS_SEND_FIRST_AVAIL
! PCPS_RECEIVE_MODE        PCPS_RECEIVE_PASSTHRU
! PCPS_BUNCH_MODE          PCPS_BUNCH_TRACE_GROUPS
! PCPS_SEND_EOF_MODE       PCPS_SEND_ALL_EOF
! PCPS_ALT_SEND_MODE       PCPS_SEND_ALL
! PCPS_ALT_RECEIVE_MODE    PCPS_RECEIVE_ALL_EOF
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
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
! Internal semblances are sampled 5 times per TIM_SMOOTH.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS VPICK Process/NC=80/NR=24>
!
!                             VPICK (Velocity PICK)
!               Automatically revise stacking velocity functions.
!
! Select PATHNAME_VEL_IN [pathname_vel_in ]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                         [pathname_vel_in_info ]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! Select PATHNAME_VEL_OUT[pathname_vel_out]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                         [pathname_vel_out_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! TIM_SMOOTH=`FFFFFFFF seconds          SAMP_INT=`FFFFFFFF seconds 
!
! CMP_SMOOTH=`IIIIIIII                  CMP_INC =`IIIIIIII
!
! CONSTRAINT=`CCCCCCCC                  NCPUS~~~=`XXXX
!
! TIMES       RMO_MIN     RMO_MAX     OFF_MAX
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF~~~~~~
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF~~~~~~
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF~~~~~~
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF~~~~~~
!
!<PARMS PATHNAME_VEL_IN [/ML=140/XST]>
!<PARMS PATHNAME_VEL_OUT[/ML=140/XST]>
!<PARMS TIMES_ARRAYSET  [/XST/YST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="NCPUS" TYPE="DISPLAY_ONLY">
!<Tip> Number of processors in the job. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME_VEL_IN">
!<Tip> Choose PATHNAME_VEL_IN using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME_VEL_OUT">
!<Tip> Choose PATHNAME_VEL_OUT using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_VEL_IN_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_VEL_IN. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_VEL_OUT_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_VEL_OUT. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_VEL_IN">
!<Tip> Pathname for the input reference stacking velocity file. </Tip>
! Default =
! Allowed = char
!
! Pathname for the input reference velocity file containing the best stacking
! velocity functions.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_VEL_OUT">
!<Tip> Pathname for the (improved) output stacking velocity file. </Tip>
! Default =
! Allowed = char
!
! Pathname for the output velocity file containing the stacking velocity
! functions that have been revised by VPICK.
!
! NOTE: Velocity functions on the output velocity file will be in the order
! of the input and output trace gathers.  But for parallel jobs, velocity
! functions on the output velocity file will not be in any expected order.
! For most purposes this should not matter.  For example, NMO no longer
! requires input velocity functions to be in any particular order.  However,
! remember that the VA program can always be used to sort or resample the
! velocity functions on a velocity file.
!</Help>
!
!
!<Help KEYWORD="TIM_SMOOTH">
!<Tip> Length of time running average smoothing for velocity functions. </Tip>
! Default = 0.3
! Allowed = real > 0.0
!
! Length of the temporal running average smoothing, in seconds, for output
! velocity functions.
!
! NOTE: Since SAMP_INT is not yet implemented, output velocity functions
! will always have a time sample interval of 0.5 * TIM_SMOOTH.
!
! NOTE: Remember that time smoothing and resampling can be performed
! afterwards by the VA program instead of (or in addition to) any time
! smoothing or resampling performed by this process.
!</Help>
!
!
!<Help KEYWORD="SAMP_INT">
!<Tip> Desired time sample interval for output velocity functions. </Tip>
! Default = 0.1
! Allowed = 0.5 * TIM_SMOOTH >= real > 0.0
!
!           ++++++++++++++ NOT YET IMPLEMENTED +++++++++++++
!
! Desired time sample interval, in seconds, for output velocity functions.
! SAMP_INT cannot be greater than 0.5 * TIM_SMOOTH.
!
! NOTE: Since SAMP_INT is not yet implemented, output velocity functions
! will always have a time sample interval of 0.5 * TIM_SMOOTH.
!
! NOTE: Remember that time smoothing and resampling can be performed
! afterwards by the VA program instead of (or in addition to) any time
! smoothing or resampling performed by this process.
!</Help>
!
!
!<Help KEYWORD="CMP_SMOOTH">
!<Tip> Number of CMPs for running average smooth of velocity functions. </Tip>
! Default =  3
! Allowed = int >= 0
!
! Velocity functions are calculated for each input CMP, then laterally smoothed
! over the number of input CMP gathers specified by CMP_SMOOTH.  Thus, the
! picked velocity functions will not change too abruptly.  Normally CMP_SMOOTH
! should be set to 3 or more.  (But see the following notes.)
!
! NOTE: Some smoothing takes place even when CMP_SMOOTH is 1.  To eliminate
! all smoothing between CMPs, set CMP_SMOOTH to 0.
!
! NOTE: For parallel jobs, CMP_SMOOTH must be set to zero and any desired
! lateral smoothing must be performed in the VA program.  This is required
! when running a parallel job, because adjacent CMP gathers will normally be
! processed by different CPUs who are not communicating with each other.
! To smooth velocity functions laterally in such cases, use the VA program.
!
! NOTE: Even for scalar jobs, CMP_SMOOTH should be set to zero for 3D data,
! because otherwise the end of one line will be mixed with the beginning
! of the next line.  If the data are not sorted by CMP, undesirable mixing
! will occur even with 2D data.  Also be aware that the mixing done in this
! process is not symmetric, because earlier CMPs are mixed into later CMPs but
! not vice versa.  If any of these behaviors are undesirable, CMP_SMOOTH
! should be set to zero and any desired lateral smoothing should be
! performed in the VA program.
!
! NOTE: Remember that lateral smoothing can be performed afterwards by
! the VA program instead of (or in addition to) any lateral smoothing
! performed by this process.
!</Help>
!
!
!<Help KEYWORD="CMP_INC">
!<Tip> Output velocity function lateral spacing in number of input CMPs. </Tip>
! Default =  10
! Allowed = int > 0
!
!           ++++++++++++++ NOT YET IMPLEMENTED +++++++++++++
!
! One velocity function will be output for each CMP_INC input CMPs.
!
! NOTE: Since CMP_INC is not yet implemented, one velocity function will
! be output for each input CMP.
!
! NOTE: Remember that lateral resampling can be performed afterwards by
! the VA program instead of (or in addition to) any lateral resampling
! performed by this process.
!</Help>
!
!
!<Help KEYWORD="CONSTRAINT">
!<Tip> Option on type of velocity function constraint. </Tip>
! Default = RMO
! Allowed = RMO       (Constraint is +/- residual moveout, in seconds.)
! Allowed = MINMAX    (Constraint is min. and max. values of velocity.)
! Allowed = TOLERANCE (Constraint is +/- tolerance on velocity.)
! Allowed = FRACTION  (Constraint is +/- fractional change in velocity.)
!
! If CONSTRAINT=RMO, then RMO_MIN is the smallest (negative) residual moveout
! and RMO_MAX is the largest (positive) residual moveout, in seconds, for the
! specified zero-offset time.
!
! If CONSTRAINT=MINMAX, then RMO_MIN is the minimum velocity and RMO_MAX is the
! maximum velocity for the specified zero-offset time.
!
! If CONSTRAINT=TOLERANCE, then RMO_MIN is the smallest (negative) change in
! velocity and RMO_MAX is the largest change in velocity for the specified
! zero-offset time.
!
! If CONSTRAINT=FRACTION, then RMO_MIN is the smallest (negative) fractional
! change in velocity and RMO_MAX is the largest fractional change in velocity
! for the specified zero-offset time.
!</Help>
!
!
!<Help KEYWORD="TIMES">
!<Tip> Array of zero offset times for specifying velocity constraints. </Tip>
! Default =  -
! Allowed = real (linked array)
!</Help>
!
!
!<Help KEYWORD="RMO_MIN">
!<Tip> Array of lower velocity constraints (seconds or fractional). </Tip>
! Default =  -
! Allowed = real (linked array)
!
! RMO_MIN is the type of constraint defined by CONSTRAINT.
!</Help>
!
!
!<Help KEYWORD="RMO_MAX">
!<Tip> Array of upper velocity constraints (seconds or fractional). </Tip>
! Default =  -
! Allowed = real (linked array)
!
! RMO_MAX is the type of constraint defined by CONSTRAINT.
!</Help>
!
!
!<Help KEYWORD="OFF_MAX">
!<Tip> Array of maximum offsets at specified zero offset times. </Tip>
! Default =  -
! Allowed = real (linked array)
!
! OFF_MAX is active for CONSTRAINT=RMO only.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module vpick_module

    use getlun_module
    use pc_module
    use vtrimmer_module
    use nmo_module
    use velio_module
    use pathcheck_module
    use pathchoose_module
    use named_constants_module
    use pcps_module
    use pcpsx_module

    implicit none

    private
    public  :: vpick_create
    public  :: vpick_initialize
    public  :: vpick_update
    public  :: vpick_delete
    public  :: vpick
    public  :: vpick_wrapup
    public  :: vpick_dump_object


    character(len=100),public,save :: VPICK_IDENT = &
'$Id: vpick.f90,v 1.46 2006/10/17 13:45:50 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    integer, parameter :: MAX_TIMES = 51

    type,public :: vpick_struct

        private
        logical                        :: skip_wrapup      ! skip wrapup flag.

        character(len=FILENAME_LENGTH) :: pathname_vel_in  ! process parameter
        character(len=FILENAME_LENGTH) :: pathname_vel_out ! process parameter
        real                           :: tim_smooth       ! process parameter
        real                           :: samp_int         ! process parameter
        integer                        :: cmp_smooth       ! process parameter
        integer                        :: cmp_inc          ! process parameter
        character(len=12)              :: constraint       ! process parameter
        integer                        :: ntimes           ! process parameter
        real, dimension(MAX_TIMES)     :: times            ! process parameter
        real, dimension(MAX_TIMES)     :: rmo_min          ! process parameter
        real, dimension(MAX_TIMES)     :: rmo_max          ! process parameter
        real, dimension(MAX_TIMES)     :: off_max          ! process parameter

        integer                        :: num_cpus         ! job data parameter
        integer                        :: nwih             ! global parameter
        integer                        :: ndpt             ! global parameter
        real                           :: dt               ! global parameter
        real                           :: tstrt            ! global parameter

        logical                        :: undo             ! constant parameter

        integer                        :: nvf,nhx,nhy      ! dependent variable
        integer                        :: idevtmp          ! dependent variable
        logical                        :: lvout            ! dependent variable
        integer                        :: iters            ! dependent variable
        real            , pointer      :: slast(:)         ! dependent variable
        integer                        :: received         ! dependent variable
        integer                        :: processed        ! dependent variable
        integer                        :: unchanged        ! dependent variable
        type(nmo_struct), pointer      :: nmo_obj          ! dependent variable
        type(pathchoose_struct),pointer :: dialog_in
        type(pathchoose_struct),pointer :: dialog_out

    end type vpick_struct


!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!


    integer, parameter :: MAX_PICKS = 1000
    integer, save      :: lunprint  =    6

contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine vpick_create( obj )

    type(vpick_struct),pointer :: obj

    lunprint = pc_get_lun()

    allocate( obj )

    nullify ( obj%slast   )
    nullify ( obj%nmo_obj )
    nullify (obj%dialog_in) ! jpa
    nullify (obj%dialog_out) ! jpa

    call pathchoose_create (obj%dialog_in , 'pathname_vel_in' , 'vel')
    call pathchoose_create (obj%dialog_out, 'pathname_vel_out', 'vel')
    call vpick_initialize  (obj)

end subroutine vpick_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine vpick_delete( obj )

    type(vpick_struct),pointer :: obj

    call vpick_wrapup( obj )

    if( associated( obj%slast   ) ) deallocate( obj%slast )
    if( associated( obj%nmo_obj ) ) call nmo_delete( obj%nmo_obj )

    call pathchoose_delete (obj%dialog_in)
    call pathchoose_delete (obj%dialog_out)

    deallocate( obj )

end subroutine vpick_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine vpick_initialize( obj )

    type(vpick_struct),intent(inout) :: obj

    obj%pathname_vel_in  = PATHCHECK_EMPTY
    obj%pathname_vel_out = PATHCHECK_EMPTY
    obj%tim_smooth       = 0.3
    obj%samp_int         = 0.1                  ! not yet implemented.
    obj%cmp_smooth       = 3
    obj%cmp_inc          = 10                   ! not yet implemented.
    obj%constraint       = 'RMO'
    obj%ntimes           = 0
    obj%times  (:)       = 0.0
    obj%rmo_min(:)       = 0.0
    obj%rmo_max(:)       = 0.0
    obj%off_max(:)       = 0.0

    obj%undo             = .false.              ! constant parameter.

    call vpick_update( obj )

end subroutine vpick_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine vpick_update( obj )

    type(vpick_struct),intent(inout),target :: obj

!-------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------

    logical                      :: gathered
    integer                      :: ierr,numtr,nvf
    integer                      :: n1,n2,n3,n4
    character(len=80)            :: msg
    type(velio_struct), pointer  :: velio_obj

!-------------------------------------------------------------------------------

    nullify (velio_obj) ! jpa
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    if (pathchoose_update(obj%dialog_in , obj%pathname_vel_in )) return
    if (pathchoose_update(obj%dialog_out, obj%pathname_vel_out)) return

    call pc_register_array_names( 'TIMES_ARRAYSET', (/ 'TIMES  ', 'RMO_MIN', &
                                  'RMO_MAX', 'OFF_MAX' /), 4)

    call pc_get_jdata  ('num_cpus', obj%num_cpus)

    call pc_get_global( 'NWIH'    , obj%nwih  )
    call pc_get_global( 'NDPT'    , obj%ndpt  )
    call pc_get_global( 'DT'      , obj%dt    )
    call pc_get_global( 'TSTRT'   , obj%tstrt )
    call pc_get_global( 'GATHERED', gathered  )
    call pc_get_global( 'NUMTR'   , numtr     )

    n1 = obj%ntimes
    n2 = obj%ntimes
    n3 = obj%ntimes
    n4 = obj%ntimes

    call pc_get( 'PATHNAME_VEL_IN' , obj%pathname_vel_in  )
    call pc_get( 'PATHNAME_VEL_OUT', obj%pathname_vel_out )
    call pc_get( 'TIM_SMOOTH'      , obj%tim_smooth       )
    call pc_get( 'SAMP_INT'        , obj%samp_int         )
    call pc_get( 'CMP_SMOOTH'      , obj%cmp_smooth       )
    call pc_get( 'CMP_INC'         , obj%cmp_inc          )
    call pc_get( 'CONSTRAINT'      , obj%constraint       )

    call pc_get( 'TIMES'           , obj%times  , n1 )
    call pc_get( 'RMO_MIN'         , obj%rmo_min, n2 )
    call pc_get( 'RMO_MAX'         , obj%rmo_max, n3 )
    call pc_get( 'OFF_MAX'         , obj%off_max, n4 )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    obj%ntimes = min(n1,n2,n3,n4)

    if( .not. gathered .or. numtr <= 1 ) then
        call pc_error( 'VPICK: Input traces must be gathered by CMP')
    endif

    call vtrimmer_verify_scalars &
                (obj%tim_smooth, obj%constraint, obj%cmp_smooth, msg)
    if (msg /= ' ') call pc_warning ('VPICK:',msg)

    if (pc_verify_arrayset('times_arrayset')) then
        call vtrimmer_verify_arrays (n1,n2,n3,n4, &
           obj%times, obj%rmo_min, obj%rmo_max, obj%off_max, obj%ntimes, msg)
        if (msg /= ' ') call pc_error ('VPICK:',msg)
    endif

    call pathcheck('PATHNAME_VEL_IN' , obj%pathname_vel_in , 'vel', .true. , &
                           show=PATHCHECK_INFO_INPUT)
    call pathcheck('PATHNAME_VEL_OUT', obj%pathname_vel_out, 'vel', .false., &
                           show=PATHCHECK_INFO_OUTPUT)

    if (obj%num_cpus > 1 .and. obj%cmp_smooth > 0)   &
          call pc_error ('CMP_SMOOTH must be 0 when running a parallel job')


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


    call pc_put_options_field( 'constraint',                 &
                                (/ 'RMO      ', 'MINMAX   ', &
                                   'TOLERANCE', 'FRACTION ' /), 4)

    call pc_put( 'PATHNAME_VEL_IN' , obj%pathname_vel_in  )
    call pc_put( 'PATHNAME_VEL_OUT', obj%pathname_vel_out )
    call pc_put( 'TIM_SMOOTH'      , obj%tim_smooth       )
    call pc_put( 'SAMP_INT'        , obj%samp_int         )
    call pc_put( 'CMP_SMOOTH'      , obj%cmp_smooth       )
    call pc_put( 'CMP_INC'         , obj%cmp_inc          )
    call pc_put( 'CONSTRAINT'      , obj%constraint       )

    call pc_put( 'TIMES'           , obj%times  , obj%ntimes )
    call pc_put( 'RMO_MIN'         , obj%rmo_min, obj%ntimes )
    call pc_put( 'RMO_MAX'         , obj%rmo_max, obj%ntimes )
    call pc_put( 'OFF_MAX'         , obj%off_max, obj%ntimes )

    call pc_put_minsize_arrayset( 'TIMES_ARRAYSET', 1 )
    call pc_put_maxsize_arrayset( 'TIMES_ARRAYSET', MAX_TIMES )

    call pc_put_gui_only ('ncpus' , obj%num_cpus)

    call pc_put_control ('parallel_safe'        , .true.)
    call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
    call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
    call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
    call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
    call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
    call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

    if( associated( obj%slast   ) ) deallocate( obj%slast )
    if( associated( obj%nmo_obj ) ) call nmo_delete( obj%nmo_obj )

!  THE BOSS AND WORKERS ARE EXECUTING HERE.

    if( pc_do_not_process_traces() ) return

!  ONLY THE WORKERS ARE EXECUTING HERE IF PCPS_SEND_FIRST_AVAIL.  <----
!  ONLY THE WORKERS ARE EXECUTING HERE IF PCPS_BOSS_DISTRIBUTES.
!  BOSS AND WORKERS ARE EXECUTING HERE IF PCPS_BOSS_EXECS.

    obj%skip_wrapup = .false.

    if (pcps_num_procs /= obj%num_cpus) then
         call pc_error ('wrong number of processes',pcps_num_procs, &
                        '- should be',obj%num_cpus)
    endif

    allocate( obj%slast(obj%ndpt), stat = ierr )
    if( ierr /= 0 ) call pc_error( 'VPICK: slast memory allocation error' )

    if( pc_do_not_process_traces() ) return   ! in case of allocation errors.

    obj%nvf        = 0
    obj%idevtmp    = 0
    obj%lvout      = (obj%pathname_vel_out /= PATHCHECK_EMPTY)
    obj%iters      = 0
    obj%slast(:)   = 0.0
    obj%received   = 0
    obj%processed  = 0
    obj%unchanged  = 0

!----------OPEN INPUT VELOCITY FILE TO GET HEADER WORD NUMBERS.

    call velio_open_read (velio_obj, obj%pathname_vel_in,   &
                          nvf, ierr, msg, obj%nhx, obj%nhy)
    if( ierr /= VELIO_OK ) then
        call pc_error( msg )
        call pc_error( 'VPICK: PROBLEM OPENING INPUT VELOCITY FILE' )
        return
    endif
    call velio_close (velio_obj)
    call pc_print ('input velocity file header words are', &
                   obj%nhx,'and',obj%nhy)
    if (obj%nhx == 0 .or. obj%nhy == 0) then
         obj%nhx = HDR_MIDPOINT_XGRID
         obj%nhy = HDR_MIDPOINT_YGRID
    endif
    if (obj%lvout) then
         call pc_print ('output velocity file header words will be', &
                        obj%nhx,'and',obj%nhy)
    endif

!----------SET UP FOR NMO.

    call pc_clear()
    call pc_put( 'PATHNAME', obj%pathname_vel_in )
    call pc_put( 'OPT_NMO' , 'STK_VEL' )
    call nmo_create( obj%nmo_obj )
    call pc_restore()

!----------OPEN TEMPORARY VELOCITY FILE.

    if( obj%lvout ) then
        call getlun( obj%idevtmp, ierr )
        if( ierr == 0 ) then
            open(unit=obj%idevtmp,form='FORMATTED',status='SCRATCH',IOSTAT=ierr)
            if( ierr /= 0 ) call pc_error( 'VPICK: Cannot open temporary file' )
        else
            call pc_error( 'VPICK: GETLUN error' )
        endif
    endif


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


end subroutine vpick_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


subroutine vpick( obj, ntr, hd, tr )

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vpick_struct), intent(inout) :: obj
    integer           , intent(inout) :: ntr
    double precision  , intent(inout) :: hd(:,:)
    real              , intent(inout) :: tr(:,:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer          :: indx, ierr
    integer          :: nmo_ntr
    real             :: vmute
    real             :: xgrid,ygrid, xloc,yloc, shotpoint,line, xcoord,ycoord
    real             :: slow      (obj%ndpt)
    double precision :: headers   (obj%nwih,1)
    real             :: velocities(obj%ndpt,1)
!-------------------------------------------------------------------------------

    if( ntr == NO_MORE_TRACES ) then
        call vpick_wrapup( obj )
        return
    endif

!----------BUILD HEADERS FOR NMO USING AVERAGE VALUE OF COORDINATES.

    xgrid     = 0.0
    ygrid     = 0.0
    xloc      = 0.0
    yloc      = 0.0
    shotpoint = 0.0
    line      = 0.0
    xcoord    = 0.0
    ycoord    = 0.0

    do indx = 1, ntr
        xgrid     = xgrid     + hd(HDR_MIDPOINT_XGRID    ,indx)
        ygrid     = ygrid     + hd(HDR_MIDPOINT_YGRID    ,indx)
        xloc      = xloc      + hd(HDR_MIDPOINT_XLOC     ,indx)
        yloc      = yloc      + hd(HDR_MIDPOINT_YLOC     ,indx)
        shotpoint = shotpoint + hd(HDR_MIDPOINT_SHOTPOINT,indx)
        line      = line      + hd(HDR_MIDPOINT_LINE     ,indx)
        xcoord    = xcoord    + hd(obj%nhx               ,indx)
        ycoord    = ycoord    + hd(obj%nhy               ,indx)
    enddo

    xgrid     = xgrid     / ntr
    ygrid     = ygrid     / ntr
    xloc      = xloc      / ntr
    yloc      = yloc      / ntr
    shotpoint = shotpoint / ntr
    line      = line      / ntr
    xcoord    = xcoord    / ntr
    ycoord    = ycoord    / ntr

    headers(:                     ,1) = hd(1:obj%nwih,1)
    headers(HDR_MIDPOINT_XGRID    ,1) = xgrid
    headers(HDR_MIDPOINT_YGRID    ,1) = ygrid
    headers(HDR_MIDPOINT_XLOC     ,1) = xloc
    headers(HDR_MIDPOINT_YLOC     ,1) = yloc
    headers(HDR_MIDPOINT_SHOTPOINT,1) = shotpoint
    headers(HDR_MIDPOINT_LINE     ,1) = line    
    headers(obj%nhx               ,1) = xcoord
    headers(obj%nhy               ,1) = ycoord
    headers(HDR_TOP_MUTE          ,1) = 1
    headers(HDR_BOTTOM_MUTE       ,1) = obj%ndpt

!----------CALL NMO TO INTERPOLATE STACKING SLOWNESS FUNCTION.

    nmo_ntr = 1
    
    call nmo( obj%nmo_obj, nmo_ntr, headers, velocities )

    if( nmo_ntr == FATAL_ERROR ) then
        call pc_error( 'VPICK: PROBLEM EXECUTING NMO' )
        call vpick_wrapup( obj )
        ntr = FATAL_ERROR
        return
    endif

    slow(:) = 1.0 / ( velocities(:,1) * velocities(:,1) )

!----------GET MINIMUM VELOCITY FOR MUTE.

    vmute  = maxval( slow )
    vmute  = 1.0 / sqrt(vmute)

!----------GUARANTEE VELOCITY MUTE.

    call vpick_private_rmt2 ( obj, tr, hd, ntr, vmute )

!----------DO MOVEOUT TRIMMING.

    call vtrimmer ( slow, hd, tr, obj%ndpt, ntr, obj%tstrt, obj%dt,    &
                    obj%tim_smooth,                                    &
                    obj%times, obj%rmo_min, obj%rmo_max, obj%off_max,  &
                    obj%ntimes, obj%constraint,                        &
                    obj%slast, obj%cmp_smooth, obj%iters, obj%undo, ierr )

    if( ierr == VTRIMMER_OK ) then
        obj%received  = obj%received  + 1
        obj%processed = obj%processed + 1
    elseif( ierr == VTRIMMER_SKIP ) then
        obj%received  = obj%received  + 1
        obj%unchanged = obj%unchanged + 1
    else
        call pc_error( 'VPICK: PROBLEM EXECUTING VTRIMMER' )
        call vpick_wrapup( obj )
        ntr = FATAL_ERROR
        return
    endif

!----------SAVE ONE VELOCITY FUNCTION (TIMES AND VELOCITIES) TO TEMPORARY FILE.

    if( obj%lvout ) then
        call vpick_private_vsav ( obj, slow, xcoord, ycoord, ierr )
        if( ierr /= 0 ) then
            call pc_error( 'VPICK: PROBLEM EXECUTING VPICK_PRIVATE_VSAV' )
            call vpick_wrapup( obj )
            ntr = FATAL_ERROR
            return
        endif
    endif

end subroutine vpick


!!------------------------- vpick private rmt2 ------------------------------!!
!!------------------------- vpick private rmt2 ------------------------------!!
!!------------------------- vpick private rmt2 ------------------------------!!

! MAKE SURE THAT MUTE INCLUDES SPECIFIED VELOCITY MUTE.


subroutine vpick_private_rmt2( obj, tr, hd, ntr, vmute )

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vpick_struct), intent(in)    :: obj
    real              , intent(inout) :: tr(:,:)  ! (ndpt,ntr)
    double precision  , intent(inout) :: hd(:,:)  ! (nwih,ntr)
    integer           , intent(in)    :: ntr
    real              , intent(in)    :: vmute
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i2, ilast
!-------------------------------------------------------------------------------

    do i2 = 1,  ntr
        ilast = int( 0.5 + ( hd(HDR_OFFSET,i2) / vmute - obj%tstrt ) / obj%dt )
        ilast = min( obj%ndpt, ilast )
        hd(2,i2) = min( float(obj%ndpt), max( real(hd(2,i2)), float(ilast+1) ) )
        if( ilast >= 1 ) tr(:ilast,i2) = 0.0
    enddo

end subroutine vpick_private_rmt2


!!------------------------- vpick private vsav ------------------------------!!
!!------------------------- vpick private vsav ------------------------------!!
!!------------------------- vpick private vsav ------------------------------!!

! save one velocity function to temporary file.


subroutine vpick_private_vsav ( obj, slow, xcoord, ycoord, ierr )

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vpick_struct), intent(inout) :: obj
    real              , intent(in)    :: slow(:)       ! slow(ndpt)
    real              , intent(in)    :: xcoord,ycoord
    integer           , intent(inout)   :: ierr
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer        :: is, ivp, nvp
    real           :: time, velocity
    real,parameter :: EPS = 1.0e-20
!-------------------------------------------------------------------------------

    obj%nvf = obj%nvf + 1
    nvp = int( 2.0 * obj%dt * float( obj%ndpt-1 ) / obj%tim_smooth + 1.499 )
    nvp = min0( nvp, MAX_PICKS )

    write( obj%idevtmp, '( i5, 2e15.7 )', iostat=ierr ) nvp, xcoord, ycoord
    if( ierr /= 0 ) return

    do ivp = 1, nvp

!----------get the time and the index (is) for the slowness-squared.

        time  = obj%tstrt + 0.5 * obj%tim_smooth * float(ivp-1)
        is = &
         max( 1, min( obj%ndpt, int( (time - obj%tstrt) / obj%dt + 1.499 ) ) )

!----------calculate the velocity from the slowness-squared.

        velocity = sign( 1.0, slow(is) ) / sqrt( max( EPS, abs( slow(is) ) ) )

!----------output time & velocity to temporary file.

        write( obj%idevtmp, '( 2e15.7)', iostat=ierr ) time, velocity
        if( ierr /= 0 ) return
    enddo

    ierr = 0

end subroutine vpick_private_vsav


!!------------------------ vpick private vout ------------------------------!!
!!------------------------ vpick private vout ------------------------------!!
!!------------------------ vpick private vout ------------------------------!!

! read velocity functions from temporary velocity file.
! output velocity functions to permanent velocity file.

! this routine is the only place where inter-cpu communication is required.


subroutine vpick_private_vout( obj )

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vpick_struct), intent(in)    :: obj
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    type(velio_struct), pointer  :: velio_obj
    character(len=80)            :: msg
    integer                      :: ivf, ivp, nvp, ierr, icpu, nvf
    real                         :: xcoord, ycoord
    real, dimension(MAX_PICKS)   :: time, velocity
    integer                      :: boss
    integer,parameter            :: TAG1 = 1
    integer,parameter            :: TAG2 = 2
    integer,parameter            :: TAG3 = 3
    integer,parameter            :: TAG4 = 4
    integer,parameter            :: TAG5 = 5
    integer,parameter            :: TAG6 = 6

    integer                      :: nworkers
    integer,pointer              :: workers(:)
 !  integer                      :: nvf2
    integer                      :: received,processed,unchanged

    nullify (velio_obj) ! jpa
!----------find out who is boss:

    if (obj%num_cpus > 1) then
         boss = 1         ! the real boss (= 0) is not being used.
         nworkers = obj%num_cpus - 1
    else
         boss = 0
         nworkers = 1
    endif

!----------learn total number of velocity functions:

!!! call pcpsx_sum_all_reduce (obj%nvf, nvf)
!!!    (cannot use pcpsx_sum_all_reduce because it hangs on icpu = 0)

    allocate (workers(nworkers))
    workers = (/ (icpu, icpu = boss,obj%num_cpus-1) /)

 ! returns sum only to boss:
    nvf       = obj%nvf
    received  = obj%received
    processed = obj%processed
    unchanged = obj%unchanged
    call pcpsx_sum_reduce_group (boss, nworkers, workers, nvf)
    call pcpsx_sum_reduce_group (boss, nworkers, workers, received)
    call pcpsx_sum_reduce_group (boss, nworkers, workers, processed)
    call pcpsx_sum_reduce_group (boss, nworkers, workers, unchanged)

 ! also works:
 ! returns sum to everyone:
 !  nvf = obj%nvf
 !  call pcpsx_sum_all_reduce_group   (nworkers, workers, nvf)

    deallocate (workers)

 ! also works:
 ! returns sum to everyone:
 !  nvf = 0
 !  do icpu = boss,obj%num_cpus-1
 !      if (icpu == pcps_current_worker_num) then
 !          nvf2 = obj%nvf
 !          if (icpu /= boss) then
 !              call pcpsx_send_data (boss,nvf2,TAG1)
 !          endif
 !      elseif (pcps_current_worker_num == boss) then
 !          call pcpsx_receive_data (icpu,nvf2,TAG1)
 !      endif
 !      nvf = nvf + nvf2
 !  enddo

!----------open output velocity file:

    if (pcps_current_worker_num == boss) then
        call velio_open_write( velio_obj, obj%pathname_vel_out,        &
                               nvf, ierr, msg,                         &
                               obj%nhx, obj%nhy, DEFAULT_VELTYPE='VTRM' )
        if( ierr /= VELIO_OK ) then
            call pc_error( msg )
            call pc_error( 'VPICK: PROBLEM EXECUTING VPICK_PRIVATE_VOUT' )
            return
        endif
    endif

!----------write to output velocity file:

    rewind (obj%idevtmp)

    do icpu = boss,obj%num_cpus-1

        if (icpu == pcps_current_worker_num) then
            nvf = obj%nvf
            if (icpu /= boss) then
                call pcpsx_send_data (boss,nvf,TAG1)
            endif
        elseif (pcps_current_worker_num == boss) then
            call pcpsx_receive_data (icpu,nvf,TAG1)
        endif

        if (pcps_current_worker_num == boss) then
             print *, "VPICK: CPU ",icpu," processed ",nvf," trace gathers"
        endif

        do ivf = 1, nvf

            if (icpu == pcps_current_worker_num) then
                 read( obj%idevtmp, * ) nvp, xcoord, ycoord
                 do ivp = 1, nvp
                      read( obj%idevtmp, * ) time(ivp), velocity(ivp)
                 enddo
                 if (icpu /= boss) then
                      call pcpsx_send_data (boss,nvp,         TAG2)
                      call pcpsx_send_data (boss,xcoord,      TAG3)
                      call pcpsx_send_data (boss,ycoord,      TAG4)
                      call pcpsx_send_data (boss,nvp,time,    TAG5)
                      call pcpsx_send_data (boss,nvp,velocity,TAG6)
                 endif
            elseif (pcps_current_worker_num == boss) then
                 call pcpsx_receive_data (icpu,nvp,         TAG2)
                 call pcpsx_receive_data (icpu,xcoord,      TAG3)
                 call pcpsx_receive_data (icpu,ycoord,      TAG4)
                 call pcpsx_receive_data (icpu,nvp,time,    TAG5)
                 call pcpsx_receive_data (icpu,nvp,velocity,TAG6)
            endif

            if (pcps_current_worker_num == boss) then
                call velio_write_velfun &
                    (velio_obj, xcoord, ycoord, nvp, time, velocity, ierr, &
                     msg, PDATE="VPICK", COMMENT="CPU="//string_ii2ss(icpu))
                if( ierr /= VELIO_OK ) then
                    call pc_error(msg)
                    call pc_error('VPICK: PROBLEM EXECUTING VPICK_PRIVATE_VOUT')
                    call velio_close (velio_obj)
                    return
                endif
            endif

        enddo

    enddo

!----------close output velocity file:

    if (pcps_current_worker_num == boss) then
         call velio_close (velio_obj)
    endif

    if (pcps_current_worker_num == boss) then
         print *, 'VPICK: ',received ,' trace gathers received'
         print *, 'VPICK: ',processed,' trace gathers processed'
         print *, 'VPICK: ',unchanged,' trace gathers unchanged'
         !!! Using pc_print here does not print when parallel
         !!! because the boss is not really the boss.
    endif

end subroutine vpick_private_vout


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


subroutine vpick_wrapup( obj )

    type(vpick_struct),intent(inout) :: obj       ! arguments

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    if( obj%lvout ) then
        call vpick_private_vout ( obj )
        close( unit = obj%idevtmp )
    endif

end subroutine vpick_wrapup


!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!


subroutine vpick_dump_object( obj )

    type(vpick_struct),intent(inout) :: obj

    integer :: i

901 format( / 1x, '   I    TIMES(I)  RMO_MIN(I)  RMO_MAX(I)  OFF_MAX(I)' / )
902 format( 1x, i4, 4f12.2 )

!-------------------------------------------------------------------------------
    write( lunprint, '( / " ***** DUMP VPICK OBJECT ***** " / )' )

    write( lunprint, '( " skip_wrapup      = ", l1     )' ) obj%skip_wrapup
    write( lunprint, '( " pathname_vel_in  = ", a80    )' ) obj%pathname_vel_in
    write( lunprint, '( " pathname_vel_out = ", a80    )' ) obj%pathname_vel_out
    write( lunprint, '( " tim_smooth       = ", f8.3   )' ) obj%tim_smooth
    write( lunprint, '( " samp_int         = ", f8.3   )' ) obj%samp_int   
    write( lunprint, '( " cmp_smooth       = ", i4     )' ) obj%cmp_smooth
    write( lunprint, '( " cmp_inc          = ", i4     )' ) obj%cmp_inc   
    write( lunprint, '( " constraint       = ", a12    )' ) obj%constraint
    write( lunprint, '( " nwih             = ", i4     )' ) obj%nwih
    write( lunprint, '( " ndpt             = ", i4     )' ) obj%ndpt
    write( lunprint, '( " dt               = ", f8.3   )' ) obj%dt
    write( lunprint, '( " tstrt            = ", f8.3   )' ) obj%tstrt
    write( lunprint, '( " ntimes           = ", i4     )' ) obj%ntimes
    write( lunprint, '( " iters            = ", i4     )' ) obj%iters
    write( lunprint, '( " nvf              = ", i4     )' ) obj%nvf
    write( lunprint, '( " idevtmp          = ", i4     )' ) obj%idevtmp
    write( lunprint, '( " lvout            = ", l1     )' ) obj%lvout

    write( lunprint, 901 )
    do i = 1, obj%ntimes
        write( lunprint, 902 ) &
                i, obj%times(i), obj%rmo_min(i), obj%rmo_max(i), obj%off_max(i)
    enddo

    write( lunprint, '( / " ***** END OF DUMP VPICK OBJECT ***** " / )' )

end subroutine vpick_dump_object


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module vpick_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

