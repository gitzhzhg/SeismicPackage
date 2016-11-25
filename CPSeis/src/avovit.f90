!<CPS_v1 type="PROCESS"/>
!!------------------------------- avovit.f90 ---------------------------------!!
!!------------------------------- avovit.f90 ---------------------------------!!
!!------------------------------- avovit.f90 ---------------------------------!!

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
! Name       : AVOVIT
! Category   : velocity_analysis
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : AVO and Velocity Iteration (AVOVIT).
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! AVOVIT is tool to perform AVO sensitive residual velocity analysis.   
! The primary purpose of this tool is to compute residual velocities for the
! purpose of flattening reflections within CDP gathers.  AVOVIT will normally
! be applied to post migrated CDP gathers.  Input data requirements are     
! prestack CDP gathers with no NMO correction applied and a stacking
! velocity function.  The program requires offset gathers as input.  The
! algorithm only operates on data in the time domain.  Depth migrated data
! needs to be converted to time prior to input to this process.
! 
! The following traces are output from AVOVIT:                          
!   1. Revised stacking velocities                     ( Header 49 = 51 )
!   2. Revised interval velocities                     ( Header 49 = 13 )
!   3. Imaginary part of correlation coefficient (AB*) ( Header 49 = 41 )
!       (also referred to as the residual velocity indicator (RVI))
!
! The above traces are output from AVOVIT for each input CDP gather for
! each iteration.
!
! Note that Header word 51 will be set to the iteration number and the above
! traces will be output for each iteration.  Normally, the AVOVIT process will
! be followed by a SELECT to save the revised stacking velocities from the last 
! iteration.  See the ADVICE FOR USERS section for a sample work flow.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! AVOVIT operates as a 2D algorithm.  It operates, in the case of 3D data, on a
! single inline at a time.  Because of this, there is the possibility of 
! creating crossline "jitter" in the resulting velocity field.  Since AVOVIT 
! outputs a velocity function at each CDP location, high frequency variations
! in the velocity field can even occur in the inline direction depending on 
! parameter selection.  Because of these inherent problems, it is highly 
! recommended to apply some amount of smoothing to the output velocity traces
! before they are used to flatten CDP gathers.  The process MIX can be used to
! do a simple 3D smoothing to the data.  
!
! AVOVIT does not allow a time variant window specification.  What may be 
! appropriate at the shallow times may not be appropriate deeper.  Try to stay
! away from using windows which include extremely shallow and deep reflections.
!
! AVOVIT fits the seismic amplitudes, using Shuey's approximation,
! with either two terms (2nd-order) or three terms (4th-order).
!
! If you have "good quality" data out to 40 degrees or higher, then 
! 4th-order is recommended. This is because higher order moveout and
! AVO effects will corrupt the estimate of the gradient (B) (which is
! defined to be the coefficient of sine squared of the angle) if only
! two terms are used.  The C coefficient will leak into the B term, 
! since their functionals are not orthogonal. 
!
! If you have only 30 degress or less, then you should use 2nd-order,
! since it will not be practical to separate 4th-order effects from
! 2nd-order ones.
!
! Recommended AVOVIT / ALAMO Work Flow
!
! Input CDP's include (angles <= 30 degrees)
! Use 2nd-order AVOVIT to refine velocities / flatten gathers
!  (Test 4th-order option once you've obtained good 2nd-order results) 
! Use ALAMO to compute 4th-order velocity correction (if necessary)
! Compute AVO attributes (if desired)
!
! ----------------------------------------------------------
! Sample AVOVIT Job 
! 
! TRIN - Input prestack CDP gathers with NMO correction (e.g. migrated CDPs)
! Data conditioning processes for velocity analysis (e.g. MUTE, FILTER etc..)
!   Note that if the data will require ALAMO (4th order velocity corrections),
!   then a mute should be applied to mute data beyond angles > 30 degrees.
! NMO Inverse
! GATHER - Data input to AVOVIT should be gathered.
! AVOVIT 
! SELECT - select velocity traces for last iteration
! TROT - Output velocity traces
! 
! Sample job to smooth AVOVIT velocities
! 
! TRIN - Final velocity traces from AVOVIT
! MIX 
! TROT - Output final refined stacking velocity traces 
! ----------------------------------------------------------
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Input traces must be pre-stack CDP gathers which have not been moved out.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Output traces will consist of the following:
!   1. Revised stacking velocities (51 trace)
!   2. Revised interval velocities (13 trace)
!   3. Imaginary part of correlation coefficient (41 trace)
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       increased.
! GATHERED  whether traces are a legitimate gather  none.
! NWIH      number of words in trace header         none.
! NDPT      number of sample values in trace        none.
! TSTRT     starting time on trace                  none.
! DT        trace sample interval                   none.
! GRID      grid transformation structure           none.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!    1    HDR_SEQUENCE               Used as trace sequence number.
!    3    HDR_CURRENT_GROUP          Used as CDP number.
!    4    HDR_CURRENT_CHANNEL        Used as sequence number.
!    5    HDR_FOLD                   Used as fold number.
!    6    HDR_OFFSET                 Used as offset value.
!    7    HDR_MIDPOINT_XGRID         Used as x-line number.
!    8    HDR_MIDPOINT_YGRID         Used as inline number.
!   49    HDR_USER_49                Set to trace type (see list below)
!                                       1  = live seismic trace.
!                                       51 = revised stacking velocity trace.
!                                       43 = in-phase zero-offset trace.
!                                       44 = in-phase gradient trace.
!                                       41 = imag part of corr coeff trace.
!                                       13 = revised interval velocity trace.
!   50    HDR_USER_50                Set to gather type (seel list below).
!                                       1  = offset.
!                                       2  = angle.
!   51    HDR_USER_51                Set to iteration number.
!   52    HDR_USER_52                Set to AVO angle.
!   52    HDR_USER_53                Set to dominant frequency.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  7. 2007-11-29  Stoeckley  Remove unused reference to memman.
!  6. 2007-02-05  D. Glover  Added NULLIFY statements for Intel compiler
!005. 2006-06-12  B. Menger  Removed Unused Variables.
!  4. 2005-01-31  B. Lucas   Fixed bug in the initialization so that
!                            'nmo_create' is called only the # of times
!                            specified by the ITER_CNT parameter, rather
!                            than the max 99 times.
!  3. 2004-06-01  B. Lucas   Fixed bug in the GUI which defined both the
!                            start and end times to be integers. Converted
!                            them both to real.
!  2. 2004-04-27  B. Lucas   Added more detailed help and user information.
!                            Added option for generating an average velocity
!                            function from the stacking velocity file.
!                            Removed A and B traces (43, 44) from output.
!                            Removed original data traces from output.
!                            Switched time parameters from msec to sec.
!                            Removed "More AVEL After This" parameter.
!                            Removed "Variable Trace Weighting" parameter.
!                            Defaulted "Use Curved Raypath" to YES.
!                            Modified text of "Method of Computing A,B".
!                            Modified text of velocity change parameters.
!  1. 2003-08-26  B. Lucas   Initial version.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
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
! None.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! This tool acts primarily as a wrapper. The computational work is done
! in the B_AVEL, B_UHCI and PPAVO primitives.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS General/NC=120>
!                          General Parameters
!
!   `-General-----------------------------------------------------------------------------------------------------------
!   | Number of Iterations =~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Maximum CDPs per Line =~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Header Word of Primary Sort Coord. =~~~~`II
!   | Header Word of Secondary Sort Coord. =~~`II
!   | Start of Window =~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   | End of Window =~~~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   | AVEL Smoothing Length =~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   `-------------------------------------------------------------------------------------------------------------------
!   <PARMS Number of Iterations                  [ITER_CNT]>
!   <PARMS Maximum CDPs per Line                 [NCDP_MAX]>
!   <PARMS Header Word of Primary Sort Coord.    [ILIN_HDR]>
!   <PARMS Header Word of Secondary Sort Coord.  [XLIN_HDR]>
!   <PARMS Start of Window                       [STIM_WIN]>
!   <PARMS End of Window                         [ETIM_WIN]>
!   <PARMS AVEL Smoothing Length                 [SMTH_LEN]>
!
!
!<NS AVO and Vel. Analysis/NC=120>
!                  AVO and Velocity Analysis Parameters
!   
!   `-AVO and Velocity Analysis ----------------------------------------------------------------------------------------
!   | Select Stacking Velocity File [SVELNAME]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   |                                [SVELNAME_INFO]=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!   | Use Curved Raypaths =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`CC
!   | Use Interval Velocities for Raypaths =~~~~~~~~~~~~~~~`CC
!   | Select Interval Velocity File [IVELNAME]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   |                                [IVELNAME_INFO]=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!   | Maximum Incidence Angle =~~~~~~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   | Compute Average Velocity Function for Angle Calc. =~~`CC
!   | Smoothing Length for Average Velocity Function =~~~~~`FFFFFF
!   | Wavelet Type =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`CCCCCCC
!   | Ricker Wavelet Frequency =~~~~~~~~~~~~~~~~~~~~~~~~~~~`FFFFFF
!   | Bandpass Filter Frequencies =~~~~~~~~~~~~~~~~~~~~~~~~`SSSSSSSSSSSSSSS
!   | Method of Computing A,B =~~~~~~~~~~~~~~~~~~~~~~~~~~~~`CCCCC
!   `-------------------------------------------------------------------------------------------------------------------
!   <PARMS Use Curved Raypaths                                [INCR_OPT]>   
!   <PARMS Use Interval Velocities for Raypaths               [USE_IVEL]>
!   <PARMS Maximum Incidence Angle                            [ANGL_MAX]>
!   <PARMS Compute Average Velocity Function for Angle Calc.  [AVF_COMP]>
!   <PARMS Smoothing Length for Average Velocity Function     [AVF_SLEN]>
!   <PARMS Wavelet Type                                       [FLT_TYPE]>
!   <PARMS Ricker Wavelet Frequency                           [RIC_FREQ]>
!   <PARMS Bandpass Filter Frequencies                        [BP_FREQS]>
!   <PARMS Method of Computing A,B                            [METHODAB]>
!   <PARMS SVELNAME                                           [/ML=128/XST]>
!   <PARMS IVELNAME                                           [/ML=128/XST]>
!
!<NS Velocity Constraints/NC=120>
!                     Velocity Constraint Parameters
!
!   `-Velocity Constraints----------------------------------------------------------------------------------------------
!   | Wavelet Center Frequency for Velocity Estimation =~~~~~~~~~~`FFFFFFFFF
!   | Quality Factor =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   | Time Duration of Correlation Window =~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   | Spatial Extent of Correlation Window =~~~~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Level of Debug Print =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`CCCCCCCCCC
!   | Dump VELF Cards? =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`CC
!   | Time between VELF Dump Picks =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFF
!   | Name of VELF Card File =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`SSSSSSSSSSSSSSSS
!   | Minimum Derived Interval Velocity =~~~~~~~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Maximum Derived Interval Velocity =~~~~~~~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Max. Negative Interval Vel. Correction per Iteration (%) =~~`FFFFFFFFF
!   | Max. Positive Interval Vel. Correction per Iteration (%) =~~`FFFFFFFFF
!   | Minimum Derived Stacking Velocity =~~~~~~~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Maximum Derived Stacking Velocity =~~~~~~~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Max. Negative Stacking Vel. Correction per Iteration (%) =~~`FFFFFFFFF
!   | Max. Positive Stacking Vel. Correction per Iteration (%) =~~`FFFFFFFFF
!   | Velocity Cutoff Time =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Velocity Cutoff Exponent =~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   `-------------------------------------------------------------------------------------------------------------------
!   <PARMS Wavelet Center Frequency for Velocity Estimation          [CTR_FREQ]>
!   <PARMS Quality Factor                                            [Q_FACTOR]>
!   <PARMS Time Duration of Correlation Window                       [CWIN_TIM]>
!   <PARMS Spatial Extent of Correlation Window                      [CWIN_CDP]>
!   <PARMS Level of Debug Print                                      [DBUG_OPT]>
!   <PARMS Dump VELF Cards?                                          [DUMP_OPT]>
!   <PARMS Time between VELF Dump Picks                              [DTIM_DMP]>
!   <PARMS Name of VELF Card File                                    [VELDNAME]>
!   <PARMS Minimum Derived Interval Velocity                         [IVEL_MIN]>
!   <PARMS Maximum Derived Interval Velocity                         [IVEL_MAX]>
!   <PARMS Max. Negative Interval Vel. Correction per Iteration (%)  [IVEL_DMN]>
!   <PARMS Max. Positive Interval Vel. Correction per Iteration (%)  [IVEL_DMX]>
!   <PARMS Minimum Derived Stacking Velocity                         [SVEL_MIN]>
!   <PARMS Maximum Derived Stacking Velocity                         [SVEL_MAX]>
!   <PARMS Max. Negative Stacking Vel. Correction per Iteration (%)  [SVEL_DMN]>
!   <PARMS Max. Positive Stacking Vel. Correction per Iteration (%)  [SVEL_DMX]>
!   <PARMS Velocity Cutoff Time                                      [VCUT_TIM]>
!   <PARMS Velocity Cutoff Exponent                                  [VCUT_POW]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="ANGL_MAX">
!<Tip> Maximum incidence angle for AVO (degrees).</Tip>
! Default = 40
! Allowed = greater than or equal to 0, less than or equal to 90
! Maximum incidence angle for AVO (degrees).
!
! This parameter can be used to "mute" the data.  It should not look at data
! beyond the angle set with this parameter.  If you do not want further muting
! of the data, then set the angle to a large angle like 60 degrees.  Ideally, 
! you should mute your input data prior to AVOVIT and then use a large enough
! angle so that no further muting occurs in the algorithm.   
!
! Note that AVOVIT calls the NMO module to apply the 2nd order velocity 
! correction.  A Doppler Mute = 1.7 is used by default and this number is
! typically around 35 degrees. 
!</Help>
!
!<Help KEYWORD="AVF_COMP">
!<Tip> Compute Single Average Velocity Function for Angle Calculations.</Tip>
! Default = NO
! Allowed = YES or NO
! The primary purpose of this option is to provide the flexibility to 
! compute a consistent angle at each CDP gather location so that a 
! consistent "mute" will be defined at each CDP location.  If you have
! already muted your data prior to AVOVIT and are setting the Max Incidence
! Angle parameter to a large number to be sure to include all data, then
! this option will have no affect.
!
! If this option is set NO, the angle calculation is done at each velocity
! location, and the offset limits for each panel are linearly interpolated
! for intermediate locations.  The intent of the using the NO option is to
! be able to accommodate a SLOWLY varying water depth.  It is up to you to
! put in functions that have a smooth lateral variation. It is NOT recommended
! that the NO option be used with raw stacking velocities.
!</Help>
!
!<Help KEYWORD="AVF_SLEN">
!<Tip> Smoothing Length for Average Interval Velocity Function (sec).</Tip>
! Default = 0.2
! Allowed = and value greater than or equal to 0.0
! Length of running average smoothing for interval velocity functions
! in seconds. If the smoothing length = 0.0, then no smoothing is done.
!    
! Normally some smoothing is required to avoid jagged velocity contours.
! Typical values of the smoothing length should be at least as great as the
! vertical sampling interval of your velocity functions.
!</Help>
!
!<Help KEYWORD="BP_FREQS">
!<Tip> Bandpass frequencies: F1(0%)/F2(100%)/F3(100%)/F4(0%).</Tip>
! Default = 8/12.5/40/50
! Allowed = greater than or equal to 0
! Enter the four corner frequencies: F1(0%)/F2(100%)/F3(100%)/F4(0%).
!</Help>
!
!<Help KEYWORD="CTR_FREQ">
!<Tip> Wavelet center frequency for velocity estimation (Hz).</Tip>
! Default = 30.0
! Allowed = greater than or equal to 5.0, less than or equal to 100.0
! Wavelet center frequency for velocity estimation (Hz).
! The smaller the frequency, the greater the velocity adjustment
! each iteration, for all times.
! 
! The wavelet center frequency should not be set less than the center
! frequency of the data.
!</Help>
!
!<Help KEYWORD="CWIN_CDP">
!<Tip> Spatial extent of correlation window (CDPs).</Tip>
! Default = 21
! Allowed = greater than 1, less than or equal to 99999
! Spatial extent of correlation window (CDPs).
!</Help>
!
!<Help KEYWORD="CWIN_TIM">
!<Tip> Time duration of correlation window (sec).</Tip>
! Default = 0.25
! Allowed = greater than or equal to 0.01, less than or equal to 5.0
! Time duration or correlation window (sec).
!</Help>
!
!<Help KEYWORD="DBUG_OPT">
!<Tip> Level of debugging print.</Tip>
! Default = SILENT
! Allowed = SILENT, INFORMATION, MEM_CHECK, EXIT_CODES, EVERYTHING
! Level of debugging print:
!   SILENT -> Minimal diagnostic print.
!   INFORMATION -> Print CDPs processes and their statistics.
!   MEM_CHECK -> Frequenty verify memory integrity.
!   EXIT_CODES -> Exit codes are additionally checked.
!   EVERYTHING -> Full debug print is requested.
!</Help>
!
!<Help KEYWORD="DTIM_DMP">
!<Tip> Time between velocity picks for VELF dump (sec).</Tip>
! Default = 0.05
! Allowed = greater than 0.0
! Enter the time (sec) between time/velocity pairs
! to be dumped to the output VELF file.
!</Help>
!
!<Help KEYWORD="DUMP_OPT">
!<Tip> Dump VELF cards?</Tip>
! Default = NO
! Allowed = NO, YES
! Dump VELF cards? VELF cards are sometimes necessary for
! backward compatibility with older software packages.
!</Help>
!
!<Help KEYWORD="FLT_TYPE">
!<Tip> Type of filter.</Tip>
! Default = RICKER
! Allowed = RICKER or BANDPASS
! Select the desired type of filter, Ricker or Bandpass.
!</Help>
!
!<Help KEYWORD="INCR_OPT">
!<Tip> Use curved raypaths?</Tip>
! Default = NO
! Allowed = NO, YES
! Use curved raypaths? If 'YES', this causes AVO to be done with respect to
! true incidence angles instead of straight ray angles. Strongly affected
! by the length of the stacking velocity smoothing filter.
!
! The smoother is applied to the stacking "sloths", which are 1/(V**2).
! Smoothed stacking sloths (or velocities) are essential when computing
! interval velocities which are used in the curved ray-path option.
!
! The smoothed sloths are used to moveout the data prior to computing
! the residual velocity indicator(RVI). This happens every iteration.
! The revised velocities of each iteration are smoothed again and it is
! the smoothed velocity function which is iterated.
!</Help>
!
!<Help KEYWORD="ITER_CNT">
!<Tip> Number of Iterations.</Tip>
! Default = 1
! Allowed = greater or equal to 1, less than or equal to 10
! Enter the number of velocity iterations (between 1 and 10).
!
! The number of iterations is important.  Run time will increase with
! increasing iterations.  Too many iterations can yield unstable behavior.
! Run time increases linearly with increasing number of iterations assuming
! no other parameter changes.  Total run time can be estimated by running
! one iteration and checking elapsed time.  Then just multiply by the
! number of iterations desired to get an estimate of total run time.
!
! Output trace #3 (AB*) is a QC trace that can be used to establish the
! appropriate number of iterations. 
! 
! This QC trace is sometimes referred to as the residual velocity 
! indicator (RVI).  This trace is the imaginary part of AB*, where
! A = Ar + iAi, B* = Br + iBi.  Ar and Br are the intercept and gradient
! traces of the real data.  Ai and Bi are their respective Hilbert 
! transforms.
!
! The point at which the values of this trace start bouncing around is
! indicative of the maximum number of iterations that should be used.  The
! Q and center frequency settings generally have greater impact on the
! stability of the solution.
!</Help>
!
!<Help KEYWORD="IVELNAME">
!<Tip> Name of file containing interval velocities.</Tip>
! Default = none
! Allowed = any velocity file name.
! Name of file containing interval velocities.
!</Help>
!
!<Help KEYWORD="IVELNAME_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of IVELNAME.</Tip>
!</Help>
!
!<Help KEYWORD="IVEL_DMN">
!<Tip> Largest negative interval velocity correction per iteration (%).</Tip>
! Default = 10.0
! Allowed = greater than or equal to 0.0, less than or equal to 100.0
! Largest negative interval velocity correction per iteration(%).
!</Help>
!
!<Help KEYWORD="IVEL_DMX">
!<Tip> Largest positive interval velocity correction per iteration (%).</Tip>
! Default = 10.0
! Allowed = greater than or equal to 0.0, less than or equal to 100.0
! Largest positive interval velocity correction per iteration (%).
!</Help>
!
!<Help KEYWORD="IVEL_MAX">
!<Tip> Maximum derived interval velocity (ft/s or m/s).</Tip>
! Default = 50000
! Allowed = greater than or equal to 0
! Maximum derived interval velocity (ft/s or m/s).
! 
! No need to change the default unless you are really trying to constrain
! the velocity range that is output.
!</Help>
!
!<Help KEYWORD="IVEL_MIN">
!<Tip> Minimum derived interval velocity (ft/s or m/s).</Tip>
! Default = 0
! Allowed = greater than or equal to 0
! Minimum derived interval velocity (ft/s or m/s).
! 
! No need to change the default unless you are really trying to constrain
! the velocity range that is output.
!</Help>
!
!<Help KEYWORD="ETIM_WIN">
!<Tip> End time of analysis window (sec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99
! End time of analysis window (sec).
!</Help>
!
!<Help KEYWORD="STIM_WIN">
!<Tip> Start time of analysis window (sec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99
! Start time of analysis window (sec).
!</Help>
!
!<Help KEYWORD="METHODAB">
!<Tip> Method of computing A and B.</Tip>
! Default = 2ND-ORDER
! Allowed = 4TH-ORDER, 2ND-ORDER
! Method of computing A and B:
! 
! AVOVIT fits the seismic amplitudes, using Shuey's approximation,
! with either two terms (2nd-order) or three terms (4th-order).  
!
! If you have "good quality" data out to 40 degrees or higher, then 
! 4th-order is recommended. This is because higher order moveout and
! AVO effects will corrupt the estimate of the gradient (B) (which is
! defined to be the coefficient of sine squared of the angle) if only
! two terms are used.  The C coefficient will leak into the B term, 
! since their functionals are not orthogonal. 
!
! If you have only 30 degress or less, then you should use 2nd-order,
! since it will not be practical to separate 4th-order effects from
! 2nd-order ones.
!
! Recommended AVOVIT / ALAMO work flow
!
! Input CDP's include angles <= 30 degrees
! Use 2nd-order AVOVIT to refine velocities / flatten gathers
!  (Test 4th-order option once you've obtained good 2nd-order results) 
! Use ALAMO to get 4th-order velocity correction (if necessary)
! Compute AVO attributes (if desired)
!</Help>
!
!<Help KEYWORD="NCDP_MAX">
!<Tip> Maximum number of CDPs per line.</Tip>
! Default = 500
! Allowed = greater than 0, less than 20000
! Maximum number of CDPs per line.
!
! Job will abort if AVOVIT encounters more CDP's in a line than specified.
!</Help>
!
!<Help KEYWORD="ILIN_HDR">
!<Tip> Header location of primary sort coordinate.</Tip>
! Default = 8
! Allowed = any header word
! 
! Primary sort header (changes slowly) designating a single line. 
! Primary sort header should be a different whole number for each
! distinct input line.  
!</Help>
!
!<Help KEYWORD="XLIN_HDR">
!<Tip> Header location of secondary sort coordinate.</Tip>
! Default = 7
! Allowed = any header word
!
! Secondary sort header (changes rapidly) designating CMP gathers 
! within a single line.
! Secondary sort header should be a different whole number for each
! distinct CMP gather. 
!</Help>
!
!<Help KEYWORD="Q_FACTOR">
!<Tip> Quality factor.</Tip>
! Default = 80
! Allowed = greater than or equal to 1, less than or equal to 9999
! Quality factor. A lower Q will increase the velocity
! adjustment at progressively greater times.
!</Help>
!
!<Help KEYWORD="RIC_FREQ">
!<Tip> Ricker center frequency (Hz).</Tip>
! Default = 30
! Allowed = greater than or equal to 5, less than or equal to 50
! Ricker center frequency (Hz) for NMO de-stretching.
!</Help>
!
!<Help KEYWORD="SMTH_LEN">
!<Tip> AVEL smoothing length (sec).</Tip>
! Default = 0.05
! Allowed = greater than or equal to 0.0, less than or equal to 5.0
! AVEL smoothing length (sec). 
!
! This smoother is applied to the stacking "sloths", which are 1/(V**2).
! Smoothed stacking sloths (or velocities) are essential when computing
! interval velocities which are used in the curved ray-path option.
!
! These smoothed sloths are used to moveout the data prior to computing
! the residual velocity indicator(RVI). This happens every iteration.
! The revised velocities of each iteration are smoothed again and it is
! the smoothed velocity function which is iterated.
!
! The RVI is one of the QC traces output by AVOVIT. This trace is the
! imaginary part of AB*, where A = Ar + iAi, B* = Br + iBi.  Ar and Br
! are the intercept and gradient traces of the real data.  Ai and Bi
! are their respective Hilbert transforms.
!</Help>
!
!<Help KEYWORD="SELECT_IVELNAME">
!<Tip> Selection dialog for velocity files.</Tip>
! Default = N/A
! Allowed = N/A
! Selection dialog for velocity files.
!</Help>
!
!<Help KEYWORD="SELECT_SVELNAME">
!<Tip> Selection dialog for velocity files.</Tip>
! Default = N/A
! Allowed = N/A
! Selection dialog for velocity files.
!</Help>
!
!<Help KEYWORD="SVELNAME">
!<Tip> Name of file containing stacking velocities.</Tip>
! Default = none
! Allowed = any velocity file name
! Name of file containing stacking velocities.
!</Help>
!
!<Help KEYWORD="SVELNAME_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of SVELNAME.</Tip>
!</Help>
!
!<Help KEYWORD="SVEL_DMN">
!<Tip> Largest negative stacking velocity correction per iteration (%).</Tip>
! Default = 5.0
! Allowed = greater than or equal to 0.0, less than or equal to 100.0
! Largest negative stacking velocity correction per iteration (%).
!</Help>
!
!<Help KEYWORD="SVEL_DMX">
!<Tip> Largest positive stacking velocity correction per iteration (%).</Tip>
! Default = 5.0
! Allowed = greater than or equal to 0.0, less than or equal to 100.0
! Largest positive stacking velocity correction per iteration (%).
!</Help>
!
!<Help KEYWORD="SVEL_MAX">
!<Tip> Maximum derived stacking velocity (ft/s or m/s).</Tip>
! Default = 50000
! Allowed = greater than or equal to 0
! Maximum derived stacking velocity (ft/s or m/s).
! 
! No need to change the default unless you are really trying to constrain
! the velocity range that is output.
!</Help>
!
!<Help KEYWORD="SVEL_MIN">
!<Tip> Minimum derived stacking velocity (ft/s or m/s).</Tip>
! Default = 0
! Allowed = greater than or equal to 0
! Minimum derived stacking velocity (ft/s or m/s).
! 
! No need to change the default unless you are really trying to constrain
! the velocity range that is output.
!</Help>
!
!<Help KEYWORD="USE_IVEL">
!<Tip> Use interval velocities for raypath calculation?</Tip>
! Default = NO
! Allowed = NO, YES
! Use interval velocities for raypath calculation?
! 'YES', indicates that interval velocities independent of
! the revised stacking velocities will be used in raypath
! calculations.
!</Help>
!
!<Help KEYWORD="VCUT_POW">
!<Tip> Velocity cutoff exponent.</Tip>
! Default = 0.2
! Allowed = greater than or equal to 0
!
! The Velocity cutoff time and Velocity cutoff exponent are
! dependent parameters.  
!
! The applied change in RMS velocity will be multipled by
!
!   1.0/(1.0 + (time/cutoff time))**(cutoff exponent)).
!
! To leave the computed change in RMS velocity unaltered, set the cutoff  
! exponent parameter = 0   Therefore, the smaller number for the 
! exponent, the smaller percentage adjustment will be made to the
! computed change in velocity.
!
! Additional examples: 
!
! exponent = 0.2, cutoff time = 8 s, record length = 8 s
!   1 / (1 + 8 / 8))**0.2 = 0.87  (% change at 8 seconds)
!
! exponent = 1, cutoff time = 8 s, record length = 8 s
!   1 / (1 + 8 / 8))**1 = 0.5  (% change at 8 seconds)
!
! exponent = 1, cutoff time = 4 s, record length = 8 s
!   1 / (1 + 8 / 4))**1 = 0.33 (% change at 8 seconds)
!
! The cutoff time can be used to further reduce the % change at the
! deeper times without changing the exponent.
!</Help>
!
!<Help KEYWORD="VCUT_TIM">
!<Tip> Velocity cutoff time (sec).</Tip>
! Default = RECORD LENGTH FROM JOB_DATA
! Allowed = greater than or equal to 0
! Velocity cutoff time (sec).
!
! The Velocity cutoff time and Velocity cutoff exponent are
! dependent parameters.  
!
! The applied change in RMS velocity will be multipled by
!
!   1.0/(1.0 + (time/cutoff time))**(cutoff exponent)).
!
! To leave the computed change in RMS velocity unaltered, set the cutoff 
! exponent parameter = 0   Therefore, the smaller number for the
! exponent, the smaller percentage adjustment will be made to the
! computed change in velocity.
!
! Additional examples:
!
! exponent = 0.2, cutoff time = 8 s, record length = 8 s
!   1 / (1 + 8 / 8))**0.2 = 0.87  (% change at 8 seconds)
!
! exponent = 1, cutoff time = 8 s, record length = 8 s
!   1 / (1 + 8 / 8))**1 = 0.5  (% change at 8 seconds)
!
! exponent = 1, cutoff time = 4 s, record length = 8 s
!   1 / (1 + 8 / 4))**1 = 0.33 (% change at 8 seconds)
!
! The cutoff time can be used to further reduce the % change at the
! deeper times without changing the exponent.
!</Help>
!
!<Help KEYWORD="VELDNAME">
!<Tip> Name of VELF Dump File</Tip>
! Default = none
! Allowed = any valid filename
! Specify the full pathname of the VELF dataset to output.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module avovit_module
      use pc_module
      use named_constants_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use nmo_module
      use bavel_module
      use buhci_module
      use ppavo_module

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: avovit_create
      public :: avovit_initialize
      public :: avovit_update
      public :: avovit_delete
      public :: avovit            ! main trace processing routine.
      public :: avovit_wrapup

      character(len=100),public,save :: AVOVIT_IDENT = &
'$Id: avovit.f90,v 1.7 2007/11/30 13:55:16 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: bavel_ptr
         type(bavel_struct), pointer :: obj
      end type bavel_ptr
      type,public :: buhci_ptr
         type(buhci_struct), pointer :: obj
      end type buhci_ptr
      type,public :: avovit_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        real                       :: angl_max
        real                       :: avf_slen
        character(len=3)           :: avf_comp
        character(len=3)           :: any_more
        character(len=16)          :: bp_freqs
        real                       :: ctr_freq
        integer                    :: cwin_cdp
        real                       :: cwin_tim
        character(len=11)          :: dbug_opt
        real                       :: dtim_dmp
        character(len=3)           :: dump_opt
        character(len=8)           :: flt_type
        character(len=6)           :: gath_typ
        character(len=3)           :: incr_opt
        integer                    :: iter_cnt
        character(len=FILENAME_LENGTH) :: ivelname
        real                       :: ivel_dmn
        real                       :: ivel_dmx
        integer                    :: ivel_max
        integer                    :: ivel_min
        real                       :: etim_win
        real                       :: stim_win
        character(len=9)           :: methodab
        integer                    :: ncdp_max
        integer                    :: ilin_hdr
        integer                    :: xlin_hdr
        real                       :: q_factor
        real                       :: ric_freq
        real                       :: smth_len
        character(len=FILENAME_LENGTH) :: svelname
        real                       :: svel_dmn
        real                       :: svel_dmx
        integer                    :: svel_max
        integer                    :: svel_min
        character(len=3)           :: trfo_opt
        character(len=3)           :: use_ivel
        real                       :: vcut_pow
        real                       :: vcut_tim
        character(len=FILENAME_LENGTH) :: veldname

        integer                    :: init_iterations
        integer                    :: fcdp
        integer                    :: lagndx
        integer                    :: maxlag
        integer                    :: purged
        integer                    :: maxdtrz
        logical                    :: end_of_data
        logical                    :: first_update
        logical                    :: first_gather
        double precision           :: ilineno
        double precision           :: xlineno
        double precision           :: ilineno_prev
        double precision           :: ocdpX
        double precision           :: trace_no
        character(len=20)          :: survey_units
        character(len=FILENAME_LENGTH) :: pathname_dir

! --> Insert any other needed variables or pointers here.
        type(pathchoose_struct), pointer          :: svelname_pathchoose
        type(pathchoose_struct), pointer          :: ivelname_pathchoose
        type(bavel_ptr), pointer, dimension(:)   :: bavel
        type(buhci_ptr), pointer, dimension(:)   :: buhci
        double precision, pointer, dimension(:,:) :: hd_x
        real,             pointer, dimension(:,:) :: tr_x
        double precision, pointer, dimension(:,:) :: hd_g
        real,             pointer, dimension(:,:) :: tr_g
        double precision, pointer, dimension(:,:) :: hd_m
        real,             pointer, dimension(:,:) :: tr_m
        double precision, pointer, dimension(:)   :: hd_t
        real,             pointer, dimension(:)   :: tr_t

      end type avovit_struct

      character(len=FILENAME_LENGTH) :: svelname
      character(len=FILENAME_LENGTH) :: ivelname
      integer                        :: any_more
      integer                        :: avf_comp
      integer                        :: dbug_opt
      integer                        :: flt_type
      integer                        :: incr_opt
      integer                        :: methodab
      integer                        :: trfo_opt
      integer                        :: use_ivel
      integer                        :: len_sflt
      integer                        :: hdiv_opt
      integer                        :: pdat_opt
      integer                        :: pass_opt
      integer                        :: method_a
      integer                        :: method_b
      integer                        :: iter_num
      integer                        :: mplx_opt
      integer                        :: obdn_opt
      integer                        :: dump_opt
      integer                        :: dtim_dmp
      integer                        :: con_velp
      real                           :: scal_fct
      real                           :: ccoef_re
      character(len=256)             :: mode_opt
      character(len=256)             :: otrc_opt


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(avovit_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine avovit_create (obj)
      type(avovit_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking
      integer                   :: nit

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in avovit_create")


! --> Nullify any additional pointers in the OBJ data structure here.
      allocate(obj%bavel(99))
      allocate(obj%buhci(99))

      nullify(obj%hd_m)
      nullify(obj%tr_m)
      nullify(obj%hd_x)
      nullify(obj%tr_x)
      nullify(obj%hd_g)
      nullify(obj%tr_g)
      nullify(obj%hd_t)
      nullify(obj%tr_t)      
      nullify (obj%svelname_pathchoose) ! jpa
      nullify (obj%ivelname_pathchoose) ! jpa

      call pathchoose_create (obj%svelname_pathchoose, 'svelname' , '.vel')
      call pathchoose_create (obj%ivelname_pathchoose, 'ivelname' , '.vel')

      do nit = 1, 99
         call bavel_create (obj%bavel(nit)%obj)
!         call nmo_create (obj%bavel(nit)%obj%svel_nmo)
!         call nmo_create (obj%bavel(nit)%obj%ivel_nmo)
         call buhci_create (obj%buhci(nit)%obj)
      end do

      call avovit_initialize (obj)
      end subroutine avovit_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine avovit_delete (obj)
      type(avovit_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking
      integer                   :: nit

      call avovit_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      call pathchoose_delete (obj%svelname_pathchoose)
      call pathchoose_delete (obj%ivelname_pathchoose)
      do nit = 1, 99
         call bavel_delete (obj%bavel(nit)%obj)
         call buhci_delete (obj%buhci(nit)%obj)
      end do

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in avovit_delete")
      end subroutine avovit_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine avovit_initialize (obj)
      type(avovit_struct),intent(inout) :: obj       ! arguments
      integer :: nit

! --> Change the default values below as needed

      obj%angl_max = 40.0
      obj%any_more = 'NO'
      obj%avf_comp = 'NO'
      obj%avf_slen = 0.2
      obj%bp_freqs = '8/12.5/40/50'
      obj%ctr_freq = 30.0
      obj%cwin_cdp = 21
      obj%cwin_tim = 0.25
      obj%dbug_opt = 'SILENT'
      obj%dtim_dmp = 0.05
      obj%dump_opt = 'NO'
      obj%flt_type = 'RICKER'
      obj%incr_opt = 'YES'
      obj%iter_cnt = 1
      obj%ivelname = PATHCHECK_EMPTY
      obj%ivel_dmn = 10.0
      obj%ivel_dmx = 10.0
      obj%ivel_max = 50000
      obj%ivel_min = 0
      obj%etim_win = 99.
      obj%stim_win = 0.0
      obj%methodab = '2ND-ORDER'
      obj%ncdp_max = 500
      obj%ilin_hdr = 8
      obj%xlin_hdr = 7
      obj%q_factor = 80.0
      obj%ric_freq = 30.0
      obj%smth_len = 0.05
      obj%svelname = PATHCHECK_EMPTY
      obj%svel_dmn = 5.0
      obj%svel_dmx = 5.0
      obj%svel_max = 50000
      obj%svel_min = 0
      obj%trfo_opt = 'NO'
      obj%use_ivel = 'NO'
      obj%vcut_pow = 0.2
      obj%vcut_tim = 6.0
      obj%veldname = ' '

      obj%init_iterations = 1
!FIX      obj%fcdp = 1
      obj%fcdp = 0
      obj%lagndx = 0
      obj%maxlag = 999
      obj%purged = 0
      obj%trace_no = 0.0
      obj%end_of_data = .false.

      obj%first_update = .true.
      obj%first_gather = .true.

      do nit = 1, 99
         call bavel_init_parms(obj%bavel(nit)%obj)
         call buhci_init_parms(obj%buhci(nit)%obj)
      end do

      call avovit_update (obj)
      end subroutine avovit_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine avovit_update (obj)
      type(avovit_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.
      integer :: nit, num_vels, ntrtemp, smth_len
      integer :: cwin_tim, stim_win, etim_win, vcut_tim, vcut_pow
      integer :: length, status, status2, update_state, iter_max
      character(len=FILENAME_LENGTH) :: tempstr
      character(len=FILENAME_LENGTH) :: warnmsg
      integer :: temp

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



! --> Delete any of the globals below that are not needed:

      if (pathchoose_update (obj%svelname_pathchoose, obj%svelname)) return
      if (pathchoose_update (obj%ivelname_pathchoose, obj%ivelname)) return

      obj%ipn = pc_get_ipn()
      update_state = pc_get_update_state()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      if(.not. obj%gathered) then
         write (warnmsg, *) 'WARNING! The input to AVOVIT must&
     & be gathered first.'
         call pc_warning(warnmsg)
      end if

      call pc_get('ANGL_MAX', obj%angl_max, avovit_angl_max)
      call pc_get('AVF_COMP', obj%avf_comp, avovit_avf_comp)
      call pc_get('AVF_SLEN', obj%avf_slen, avovit_avf_slen)
      call pc_get('BP_FREQS', obj%bp_freqs, avovit_bp_freqs)
      call pc_get('CTR_FREQ', obj%ctr_freq, avovit_ctr_freq)
      call pc_get('CWIN_CDP', obj%cwin_cdp, avovit_cwin_cdp)
      call pc_get('CWIN_TIM', obj%cwin_tim, avovit_cwin_tim)
      call pc_get('DBUG_OPT', obj%dbug_opt, avovit_dbug_opt)
      call pc_get('DTIM_DMP', obj%dtim_dmp, avovit_dtim_dmp)
      call pc_get('DUMP_OPT', obj%dump_opt, avovit_dump_opt)
      call pc_get('FLT_TYPE', obj%flt_type, avovit_flt_type)
      call pc_get('INCR_OPT', obj%incr_opt, avovit_incr_opt)
      call pc_get('ITER_CNT', obj%iter_cnt, avovit_iter_cnt)
      call pc_get('IVELNAME', obj%ivelname, avovit_ivelname)
      call pc_get('IVEL_DMN', obj%ivel_dmn, avovit_ivel_dmn)
      call pc_get('IVEL_DMX', obj%ivel_dmx, avovit_ivel_dmx)
      call pc_get('IVEL_MAX', obj%ivel_max, avovit_ivel_max)
      call pc_get('IVEL_MIN', obj%ivel_min, avovit_ivel_min)
      call pc_get('ETIM_WIN', obj%etim_win, avovit_etim_win)
      call pc_get('STIM_WIN', obj%stim_win, avovit_stim_win)
      call pc_get('METHODAB', obj%methodab, avovit_methodab)
      call pc_get('NCDP_MAX', obj%ncdp_max, avovit_ncdp_max)
      call pc_get('ILIN_HDR', obj%ilin_hdr, avovit_ilin_hdr)
      call pc_get('XLIN_HDR', obj%xlin_hdr, avovit_xlin_hdr)
      call pc_get('Q_FACTOR', obj%q_factor, avovit_q_factor)
      call pc_get('RIC_FREQ', obj%ric_freq, avovit_ric_freq)
      call pc_get('SMTH_LEN', obj%smth_len, avovit_smth_len)
      call pc_get('SVELNAME', obj%svelname, avovit_svelname)
      call pc_get('SVEL_DMN', obj%svel_dmn, avovit_svel_dmn)
      call pc_get('SVEL_DMX', obj%svel_dmx, avovit_svel_dmx)
      call pc_get('SVEL_MAX', obj%svel_max, avovit_svel_max)
      call pc_get('SVEL_MIN', obj%svel_min, avovit_svel_min)
      call pc_get('USE_IVEL', obj%use_ivel, avovit_use_ivel)
      call pc_get('VCUT_POW', obj%vcut_pow, avovit_vcut_pow)
      call pc_get('VCUT_TIM', obj%vcut_tim, avovit_vcut_tim)
      call pc_get('VELDNAME', obj%veldname, avovit_veldname)

      if(obj%first_update) then
         obj%etim_win = obj%tstrt + ((obj%ndpt-1) * obj%dt)
         obj%first_update = .false.
      end if


!      *** screen traps ***

      call pc_call_screen_trap('AVOANDVELANALYSIS', avovit_avoandvelanalysis)
      call pc_call_screen_trap('GENERAL          ', avovit_general)
      call pc_call_screen_trap('UHCI             ', avovit_uhci)

!      *** end trap ***

      call pc_call_end_trap(avovit_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).

      status2 = 0
!     Check stacking velocity file, always.
      call pathcheck('SVELNAME', obj%svelname, 'vel', status=status,&
     &   show=PATHCHECK_INFO_INPUT)
      if(status .ne. PATHCHECK_VALID .and. update_state .ne. PC_GUI) then
         call pc_error('Stacking Velocity File must be specified.')
         status2 = status2 + 1
      end if
!     Check interval velocity file, if necessary.
      if(obj%use_ivel .eq. 'YES') then
         call pathcheck('IVELNAME', obj%ivelname, 'vel', status=status,&
     &      show=PATHCHECK_INFO_INPUT)
         if(status .ne. PATHCHECK_VALID .and. update_state .ne. PC_GUI) then
            call pc_error('Interval Velocity File must be specified.')
            status2 = status2 + 2
         end if
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.

      call pc_get_pdata('SURVEY_UNITS', obj%survey_units)
      call pc_get_jdata('PATHNAME_DIR', obj%pathname_dir)
      iter_max = 99
      if(iter_max .gt. obj%iter_cnt) iter_max = obj%iter_cnt
      if(iter_max .lt. 1) iter_max = 1
      if(update_state .ne. PC_GUI .and. status2 .eq. 0) then
         do nit = 1, iter_max
            call pc_put_global('OPT_NMO', 'STK_VEL')
            call pc_put_global('PATHNAME', obj%svelname)
            call pc_put_process('OPT_NMO', 'STK_VEL')
            call pc_put_process('PATHNAME', obj%svelname)
            if(associated(obj%bavel(nit)%obj%svel_nmo)) then
               call nmo_update(obj%bavel(nit)%obj%svel_nmo)
            else
               call nmo_create(obj%bavel(nit)%obj%svel_nmo)
            end if
            if(obj%use_ivel .eq. 'YES') then
               call pc_put_global('OPT_NMO', 'DIX_VEL')
               call pc_put_global('PATHNAME', obj%ivelname)
               call pc_put_process('OPT_NMO', 'DIX_VEL')
               call pc_put_process('PATHNAME', obj%ivelname)
               if(associated(obj%bavel(nit)%obj%ivel_nmo)) then
                  call nmo_update(obj%bavel(nit)%obj%ivel_nmo)
               else
                  call nmo_create(obj%bavel(nit)%obj%ivel_nmo)
               end if
            end if
         end do
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field('DBUG_OPT', (/'SILENT     ', 'INFORMATION',&
     &                                        'MEM_CHECK  ', 'EXIT_CODES ',&
     &                                        'EVERYTHING '/) )
      call pc_put_options_field('DUMP_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('FLT_TYPE', (/'RICKER  ', 'BANDPASS'/) )
      call pc_put_options_field('INCR_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('METHODAB', (/'2ND-ORDER', '4TH-ORDER'/) )
      call pc_put_options_field('USE_IVEL', (/'NO ', 'YES'/) )
      call pc_put_options_field('AVF_COMP', (/'NO ', 'YES'/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('ANGL_MAX', obj%angl_max)
      call pc_put('AVF_COMP', obj%avf_comp)
      call pc_put('AVF_SLEN', obj%avf_slen)
      call pc_put('BP_FREQS', obj%bp_freqs)
      call pc_put('CTR_FREQ', obj%ctr_freq)
      call pc_put('CWIN_CDP', obj%cwin_cdp)
      call pc_put('CWIN_TIM', obj%cwin_tim)
      call pc_put('DBUG_OPT', obj%dbug_opt)
      call pc_put('DTIM_DMP', obj%dtim_dmp)
      call pc_put('DUMP_OPT', obj%dump_opt)
      call pc_put('FLT_TYPE', obj%flt_type)
      call pc_put('INCR_OPT', obj%incr_opt)
      call pc_put('ITER_CNT', obj%iter_cnt)
      call pc_put('IVELNAME', obj%ivelname)
      call pc_put('IVEL_DMN', obj%ivel_dmn)
      call pc_put('IVEL_DMX', obj%ivel_dmx)
      call pc_put('IVEL_MAX', obj%ivel_max)
      call pc_put('IVEL_MIN', obj%ivel_min)
      call pc_put('ETIM_WIN', obj%etim_win)
      call pc_put('STIM_WIN', obj%stim_win)
      call pc_put('METHODAB', obj%methodab)
      call pc_put('NCDP_MAX', obj%ncdp_max)
      call pc_put('ILIN_HDR', obj%ilin_hdr)
      call pc_put('XLIN_HDR', obj%xlin_hdr)
      call pc_put('Q_FACTOR', obj%q_factor)
      call pc_put('RIC_FREQ', obj%ric_freq)
      call pc_put('SMTH_LEN', obj%smth_len)
      call pc_put('SVELNAME', obj%svelname)
      call pc_put('SVEL_DMN', obj%svel_dmn)
      call pc_put('SVEL_DMX', obj%svel_dmx)
      call pc_put('SVEL_MAX', obj%svel_max)
      call pc_put('SVEL_MIN', obj%svel_min)
      call pc_put('USE_IVEL', obj%use_ivel)
      call pc_put('VCUT_POW', obj%vcut_pow)
      call pc_put('VCUT_TIM', obj%vcut_tim)
      call pc_put('VELDNAME', obj%veldname)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)

! --> Add here any other parameter cache calls such as to set sensitivities.
      obj%init_iterations = obj%iter_cnt

!     AVO and velocity analysis parms.
      select case (obj%incr_opt)
         case ('NO')
            incr_opt = 0
         case ('YES')
            incr_opt = 1
      end select

      select case (obj%use_ivel)
         case ('NO')
            use_ivel = 0
         case ('YES')
            use_ivel = 1
      end select 

      select case (obj%flt_type)
         case ('RICKER')
            flt_type = 1
         case ('BANDPASS')
            flt_type = 2
      end select 

      select case (obj%methodab)
         case ('4TH-ORDER')
            method_a = 3
            method_b = 2
         case ('2ND-ORDER')
            method_a = 2
            method_b = 1
      end select

      select case (obj%trfo_opt)
         case ('NO')
            trfo_opt = 0
         case ('YES')
            trfo_opt = 1
      end select

      select case (obj%avf_comp)
         case ('NO')
            avf_comp = 0
         case ('YES')
            avf_comp = 1
      end select

      num_vels = 1
      pdat_opt = 4
      pass_opt = 1
      hdiv_opt = 1
      smth_len = obj%smth_len * 1000
      len_sflt = smth_len
      cwin_tim = obj%cwin_tim * 1000

!     UHCI parms.
      mode_opt = 'VA'
      otrc_opt = 'ABIEX'
      mplx_opt = 2
      obdn_opt = 1
      con_velp = 1
      scal_fct = -1.0
      ccoef_re = 0.0
      select case (obj%any_more)
         case ('NO')
            any_more = 0
         case ('YES')
            any_more = 1
      end select
      select case (obj%dbug_opt)
         case ('SILENT')
            dbug_opt = -1
         case ('INFORMATION')
            dbug_opt = 0
         case ('MEM_CHECK')
            dbug_opt = 1
         case ('EXIT_CODES')
            dbug_opt = 2
         case ('EVERYTHING')
            dbug_opt = 3
      end select
      select case (obj%dump_opt)
         case ('NO')
            dump_opt = 0
         case ('YES')
            dump_opt = 1
      end select

      stim_win = obj%stim_win * 1000
      etim_win = obj%etim_win * 1000
      dtim_dmp = obj%dtim_dmp * 1000
      vcut_tim = obj%vcut_tim * 1000
      vcut_pow = obj%vcut_pow * 100

      temp = len(obj%pathname_dir)
      call ppavo_strlen(obj%pathname_dir, temp, length)
      length = length + 1
      tempstr = obj%pathname_dir
      tempstr(length:) = obj%veldname

      obj%maxdtrz = obj%numtr + (obj%iter_cnt * 20)
      ntrtemp = (obj%numtr + 20) * obj%cwin_cdp * obj%iter_cnt
      do nit = 1, 99

         call bavel_set_parm(obj%bavel(nit)%obj, 'stim_win', stim_win)
         call bavel_set_parm(obj%bavel(nit)%obj, 'etim_win', etim_win)
         call bavel_set_parm(obj%bavel(nit)%obj, 'num_vels', num_vels)      
         call bavel_set_parm(obj%bavel(nit)%obj, 'svelname', obj%svelname)
         call bavel_set_parm(obj%bavel(nit)%obj, 'incr_opt', incr_opt)
         call bavel_set_parm(obj%bavel(nit)%obj, 'use_ivel', use_ivel)
         call bavel_set_parm(obj%bavel(nit)%obj, 'ivelname', obj%ivelname)
         call bavel_set_parm(obj%bavel(nit)%obj, 'flt_type', flt_type)
         call bavel_set_parm(obj%bavel(nit)%obj, 'ric_freq', obj%ric_freq)
         call bavel_set_parm(obj%bavel(nit)%obj, 'bp_freqs', obj%bp_freqs)
         call bavel_set_parm(obj%bavel(nit)%obj, 'len_sflt', len_sflt)
         call bavel_set_parm(obj%bavel(nit)%obj, 'angl_max', obj%angl_max)
         call bavel_set_parm(obj%bavel(nit)%obj, 'avf_comp', avf_comp)
         call bavel_set_parm(obj%bavel(nit)%obj, 'avf_slen', obj%avf_slen)
         call bavel_set_parm(obj%bavel(nit)%obj, 'pdat_opt', pdat_opt)
         call bavel_set_parm(obj%bavel(nit)%obj, 'pass_opt', pass_opt)
         call bavel_set_parm(obj%bavel(nit)%obj, 'method_a', method_a)
         call bavel_set_parm(obj%bavel(nit)%obj, 'method_b', method_b)
         call bavel_set_parm(obj%bavel(nit)%obj, 'hdiv_opt', hdiv_opt)
         call bavel_set_parm(obj%bavel(nit)%obj, 'trfo_opt', trfo_opt)

         iter_num = nit
         call buhci_set_parm(obj%buhci(nit)%obj, 'iter_cnt', obj%iter_cnt)
         call buhci_set_parm(obj%buhci(nit)%obj, 'iter_num', iter_num)
         call buhci_set_parm(obj%buhci(nit)%obj, 'any_more', any_more)
         call buhci_set_parm(obj%buhci(nit)%obj, 'mode_opt', mode_opt)
         call buhci_set_parm(obj%buhci(nit)%obj, 'otrc_opt', otrc_opt)      
         call buhci_set_parm(obj%buhci(nit)%obj, 'veldname', tempstr)
         call buhci_set_parm(obj%buhci(nit)%obj, 'dbug_opt', dbug_opt)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ncdp_max', obj%ncdp_max)
         call buhci_set_parm(obj%buhci(nit)%obj, 'stim_win', stim_win)
         call buhci_set_parm(obj%buhci(nit)%obj, 'etim_win', etim_win)
         call buhci_set_parm(obj%buhci(nit)%obj, 'cwin_cdp', obj%cwin_cdp)
         call buhci_set_parm(obj%buhci(nit)%obj, 'cwin_tim', cwin_tim)
         call buhci_set_parm(obj%buhci(nit)%obj, 'mplx_opt', mplx_opt)
         call buhci_set_parm(obj%buhci(nit)%obj, 'obdn_opt', obdn_opt)
         call buhci_set_parm(obj%buhci(nit)%obj, 'smth_vel', smth_len)
         call buhci_set_parm(obj%buhci(nit)%obj, 'con_velp', con_velp)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ivel_min', obj%ivel_min)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ivel_max', obj%ivel_max)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ivel_dmn', obj%ivel_dmn)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ivel_dmx', obj%ivel_dmx)
         call buhci_set_parm(obj%buhci(nit)%obj, 'svel_min', obj%svel_min)
         call buhci_set_parm(obj%buhci(nit)%obj, 'svel_max', obj%svel_max)
         call buhci_set_parm(obj%buhci(nit)%obj, 'svel_dmn', obj%svel_dmn)
         call buhci_set_parm(obj%buhci(nit)%obj, 'svel_dmx', obj%svel_dmx)
         call buhci_set_parm(obj%buhci(nit)%obj, 'vcut_tim', vcut_tim)
         call buhci_set_parm(obj%buhci(nit)%obj, 'vcut_pow', vcut_pow)
         call buhci_set_parm(obj%buhci(nit)%obj, 'dump_opt', dump_opt)
         call buhci_set_parm(obj%buhci(nit)%obj, 'dtim_dmp', dtim_dmp)
         call buhci_set_parm(obj%buhci(nit)%obj, 'scal_fct', scal_fct)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ccoef_re', ccoef_re)
         call buhci_set_parm(obj%buhci(nit)%obj, 'q_factor', obj%q_factor)
         call buhci_set_parm(obj%buhci(nit)%obj, 'ctr_freq', obj%ctr_freq)
            
         if(ntrtemp .gt. obj%numtr) then
            obj%numtr = ntrtemp
            call pc_put_global ('numtr', obj%numtr)
         end if

      end do


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine avovit_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! *** Trap for variable ANGL_MAX ***

      subroutine avovit_angl_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%angl_max)
      if(object%angl_max .lt. 0 .or. object%angl_max .gt. 90) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 90.&
     & Restoring to default of 40.'
         call pc_error(msg)
         object%angl_max = 40
         call pc_put(keyword, object%angl_max)
      end if

      return
      end subroutine avovit_angl_max

! *** Trap for variable AVF_COMP ***

      subroutine avovit_avf_comp(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      flag = (object%avf_comp .eq. 'YES')
      call pc_put_sensitive_field_flag('avf_slen', flag)

      return
      end subroutine avovit_avf_comp

! *** Trap for variable AVF_SLEN ***

      subroutine avovit_avf_slen(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%avf_slen)
      if(object%avf_slen .lt. 0.0) then
         write (msg, '(a,a)') keyword,&
     &' must be greater than or equal to 0.&
     & Restoring to default of 0.2.'
         call pc_error(msg)
         object%avf_slen = 0.2
         call pc_put(keyword, object%avf_slen)
      end if

      return
      end subroutine avovit_avf_slen

! *** Trap for variable BP_FREQS ***

      subroutine avovit_bp_freqs(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_bp_freqs

! *** Trap for variable CTR_FREQ ***

      subroutine avovit_ctr_freq(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ctr_freq)
      if(object%ctr_freq .lt. 5 .or. object%ctr_freq .gt. 100) then
         write (msg, '(a,a)') keyword,&
     &' must be between 5 and 100.&
     & Restoring to default of 30.'
         call pc_error(msg)
         object%ctr_freq = 30
         call pc_put(keyword, object%ctr_freq)
      end if

      return
      end subroutine avovit_ctr_freq

! *** Trap for variable CWIN_CDP ***

      subroutine avovit_cwin_cdp(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%cwin_cdp)
      if(object%cwin_cdp .lt. 1 .or. object%cwin_cdp .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 1 and 99999.&
     & Restoring to default of 21.'
         call pc_error(msg)
         object%cwin_cdp = 21
         call pc_put(keyword, object%cwin_cdp)
      end if

      return
      end subroutine avovit_cwin_cdp

! *** Trap for variable CWIN_TIM ***

      subroutine avovit_cwin_tim(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%cwin_tim)
      if(object%cwin_tim .lt. 0.01 .or. object%cwin_tim .gt. 5.0) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0.01 and 5.0.&
     & Restoring to default of 0.25.'
         call pc_error(msg)
         object%cwin_tim = 0.25
         call pc_put(keyword, object%cwin_tim)
      end if

      return
      end subroutine avovit_cwin_tim

! *** Trap for variable DBUG_OPT ***

      subroutine avovit_dbug_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_dbug_opt

! *** Trap for variable DTIM_DMP ***

      subroutine avovit_dtim_dmp(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%dtim_dmp)
      if(object%dtim_dmp .le. 0.0) then
         write (msg, '(a,a)') keyword,&
     &' must be greater than 0.&
     & Restoring to default of 0.05.'
         call pc_error(msg)
         object%dtim_dmp = 0.05
         call pc_put(keyword, object%dtim_dmp)
      end if

      return
      end subroutine avovit_dtim_dmp

! *** Trap for variable DUMP_OPT ***

      subroutine avovit_dump_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get(keyword, object%dump_opt)

      flag = (object%dump_opt .eq. 'YES')
      call pc_put_sensitive_field_flag('dtim_dmp', flag)
      call pc_put_sensitive_field_flag('veldname', flag)

      return
      end subroutine avovit_dump_opt

! *** Trap for variable FLT_TYPE ***

      subroutine avovit_flt_type(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag1, flag2

      call pc_get('flt_type', object%flt_type)

      flag1 = (object%flt_type .eq. 'RICKER')
      flag2 = (object%flt_type .eq. 'BANDPASS')
      call pc_put_sensitive_field_flag('ric_freq', flag1)
      call pc_put_sensitive_field_flag('bp_freqs', flag2)

! --> Insert code to validate data input
      return
      end subroutine avovit_flt_type

! *** Trap for variable GATH_TYP ***

      subroutine avovit_gath_typ(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovit_gath_typ

! *** Trap for variable INCR_OPT ***

      subroutine avovit_incr_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_incr_opt

! *** Trap for variable ITER_CNT ***

      subroutine avovit_iter_cnt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%iter_cnt)
      if(object%iter_cnt .lt. 1 .or. object%iter_cnt .gt. 99) then
         write (msg, '(a,a)') keyword,&
     &' must be between 1 and 99.&
     & Restoring to default of 1.'
         call pc_error(msg)
         object%iter_cnt = 1
         call pc_put(keyword, object%iter_cnt)
      end if

      return
      end subroutine avovit_iter_cnt

! *** Trap for variable IVELNAME ***

      subroutine avovit_ivelname(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_ivelname

! *** Trap for variable IVEL_DMN ***

      subroutine avovit_ivel_dmn(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ivel_dmn)
      if(object%ivel_dmn .lt. 0.0 .or. object%ivel_dmn .gt. 100.0) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 100.&
     & Restoring to default of 10.'
         call pc_error(msg)
         object%ivel_dmn = 10
         call pc_put(keyword, object%ivel_dmn)
      end if

      return
      end subroutine avovit_ivel_dmn

! *** Trap for variable IVEL_DMX ***

      subroutine avovit_ivel_dmx(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ivel_dmx)
      if(object%ivel_dmx .lt. 0.0 .or. object%ivel_dmx .gt. 100.0) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 100.&
     & Restoring to default of 10.'
         call pc_error(msg)
         object%ivel_dmx = 10
         call pc_put(keyword, object%ivel_dmx)
      end if

      return
      end subroutine avovit_ivel_dmx

! *** Trap for variable IVEL_MAX ***

      subroutine avovit_ivel_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ivel_max)
      if(object%ivel_max .lt. 0 .or. object%ivel_max .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 50000.'
         call pc_error(msg)
         object%ivel_max = 50000
         call pc_put(keyword, object%ivel_max)
      end if

      return
      end subroutine avovit_ivel_max

! *** Trap for variable IVEL_MIN ***

      subroutine avovit_ivel_min(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ivel_min)
      if(object%ivel_min .lt. 0 .or. object%ivel_min .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%ivel_min = 0
         call pc_put(keyword, object%ivel_min)
      end if

      return
      end subroutine avovit_ivel_min

! *** Trap for variable ETIM_WIN ***

      subroutine avovit_etim_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg
      real :: min_tm, max_tm

      min_tm = object%tstrt
      max_tm = min_tm + ((object%ndpt-1) * object%dt)

      call pc_get(keyword, object%etim_win)
      if(object%etim_win .lt. min_tm .or. object%etim_win .gt. max_tm) then
         write (msg, '(a,a)') keyword,&
     &' must be between start and end times in JOB_DATA.&
     & Restoring to last sample time.'
         call pc_error(msg)
         object%etim_win = max_tm
         call pc_put(keyword, object%etim_win)
      end if

      return
      end subroutine avovit_etim_win

! *** Trap for variable STIM_WIN ***

      subroutine avovit_stim_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg
      real :: min_tm, max_tm

      min_tm = object%tstrt
      max_tm = min_tm + ((object%ndpt-1) * object%dt)

      call pc_get(keyword, object%stim_win)
      if(object%stim_win .lt. min_tm .or. object%stim_win .gt. max_tm) then
         write (msg, '(a,a)') keyword,&
     &' must be between start and end times in JOB_DATA.&
     & Restoring to first sample time.'
         call pc_error(msg)
         object%stim_win = min_tm
         call pc_put(keyword, object%stim_win)
      end if

      return
      end subroutine avovit_stim_win

! *** Trap for variable METHODAB ***

      subroutine avovit_methodab(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_methodab

! *** Trap for variable NCDP_MAX ***

      subroutine avovit_ncdp_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ncdp_max)
      if(object%ncdp_max .lt. 1) then
         write (msg, '(a,a)') keyword,&
     &' must be greater than 0.&
     & Restoring to default of 500.'
         call pc_error(msg)
         object%ncdp_max = 500
         call pc_put(keyword, object%ncdp_max)
      end if

      return
      end subroutine avovit_ncdp_max

! *** Trap for variable ILIN_HDR ***

      subroutine avovit_ilin_hdr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_ilin_hdr

! *** Trap for variable XLIN_HDR ***

      subroutine avovit_xlin_hdr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_xlin_hdr

! *** Trap for variable Q_FACTOR ***

      subroutine avovit_q_factor(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%q_factor)
      if(object%q_factor .lt. 1 .or. object%q_factor .gt. 9999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 1 and 9999.&
     & Restoring to default of 80.'
         call pc_error(msg)
         object%q_factor = 80
         call pc_put(keyword, object%q_factor)
      end if

      return
      end subroutine avovit_q_factor

! *** Trap for variable RIC_FREQ ***

      subroutine avovit_ric_freq(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ric_freq)
      if(object%ric_freq .lt. 5 .or. object%ric_freq .gt. 80) then
         write (msg, '(a,a)') keyword,&
     &' must be between 5 and 80.&
     & Restoring to default of 30.'
         call pc_error(msg)
         object%ric_freq = 30
         call pc_put(keyword, object%ric_freq)
      end if

      return
      end subroutine avovit_ric_freq

! *** Trap for variable SMTH_LEN ***

      subroutine avovit_smth_len(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg
      character(len=256) :: tmsg
      real :: max_len

      call pc_get(keyword, object%smth_len)
      max_len = (object%ndpt-1) * object%dt
      if(object%smth_len .lt. 0.0 .or. object%smth_len .gt. max_len) then
         write (tmsg, *) ' must be between 0 and ',max_len,'.&
     & Restoring to default of 0.05'
         write (msg, '(a,a)') keyword, tmsg
         call pc_error(msg)
         object%smth_len = 0.05
         call pc_put(keyword, object%smth_len)
      end if

      return
      end subroutine avovit_smth_len

! *** Trap for variable SVELNAME ***

      subroutine avovit_svelname(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_svelname

! *** Trap for variable SVEL_DMN ***

      subroutine avovit_svel_dmn(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%svel_dmn)
      if(object%svel_dmn .lt. 0 .or. object%svel_dmn .gt. 100) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 100.&
     & Restoring to default of 5.'
         call pc_error(msg)
         object%svel_dmn = 5
         call pc_put(keyword, object%svel_dmn)
      end if

      return
      end subroutine avovit_svel_dmn

! *** Trap for variable SVEL_DMX ***

      subroutine avovit_svel_dmx(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%svel_dmx)
      if(object%svel_dmx .lt. 0 .or. object%svel_dmx .gt. 100) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 100.&
     & Restoring to default of 5.'
         call pc_error(msg)
         object%svel_dmx = 5
         call pc_put(keyword, object%svel_dmx)
      end if

      return
      end subroutine avovit_svel_dmx

! *** Trap for variable SVEL_MAX ***

      subroutine avovit_svel_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%svel_max)
      if(object%svel_max .lt. 0 .or. object%svel_max .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 50000.'
         call pc_error(msg)
         object%svel_max = 50000
         call pc_put(keyword, object%svel_max)
      end if

      return
      end subroutine avovit_svel_max

! *** Trap for variable SVEL_MIN ***

      subroutine avovit_svel_min(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%svel_min)
      if(object%svel_min .lt. 0 .or. object%svel_min .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%svel_min = 0
         call pc_put(keyword, object%svel_min)
      end if

      return
      end subroutine avovit_svel_min

! *** Trap for variable TRFO_OPT ***

      subroutine avovit_trfo_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_trfo_opt

! *** Trap for variable USE_IVEL ***

      subroutine avovit_use_ivel(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('use_ivel', object%use_ivel)

      flag = (object%use_ivel .eq. 'YES')
      call pc_put_sensitive_field_flag('select_ivelname', flag)
      call pc_put_sensitive_field_flag('ivelname', flag)

      return
      end subroutine avovit_use_ivel

! *** Trap for variable VCUT_POW ***

      subroutine avovit_vcut_pow(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%vcut_pow)
      if(object%vcut_pow .lt. 0 .or. object%vcut_pow .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99.999.&
     & Restoring to default of 0.2.'
         call pc_error(msg)
         object%vcut_pow = 0.2
         call pc_put(keyword, object%vcut_pow)
      end if

      return
      end subroutine avovit_vcut_pow

! *** Trap for variable VCUT_TIM ***

      subroutine avovit_vcut_tim(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%vcut_tim)
      if(object%vcut_tim .lt. 0.0 .or. object%vcut_tim .gt. 99.) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99.&
     & Restoring to default of 6.'
         call pc_error(msg)
         object%vcut_tim = 6.0
         call pc_put(keyword, object%vcut_tim)
      end if

      return
      end subroutine avovit_vcut_tim

! *** Trap for variable VELDNAME ***

      subroutine avovit_veldname(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_veldname


! *** Screen trap for  AVOANDVELANALYSIS ***

      subroutine avovit_avoandvelanalysis(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_avoandvelanalysis

! *** Screen trap for  GENERAL ***

      subroutine avovit_general(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_general

! *** Screen trap for  UHCI ***

      subroutine avovit_uhci(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avovit_uhci

! *** End Trap ***

      subroutine avovit_end
      implicit none

! --> Insert code to validate data input
      return
      end subroutine avovit_end



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine avovit (obj,ntr,hd,tr)
      type(avovit_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      integer :: i, j, k, m, n
      integer :: ierr, jerr
      integer :: promode, iteration
      integer :: trc_type   
      integer :: nit, cnt, ndx, ntr1
      integer :: npiped, isendens
      integer :: nio            
      integer :: nflushed   
      integer :: ispurge, maxpurge
      integer :: hdrlag   
      integer :: ieog_flag, ieoj_flag
      integer ::              nhd 
      integer :: stackword, gath_type

      integer :: ntr_gather_in
      integer :: ntr_gather_out
      integer :: iflv, illv   

      integer :: multigather, multiindex, multiiter, multicount
      integer :: acnt, bcnt, abcnt
      integer :: ghndx, gtndx
      integer :: mhndx, mtndx


      integer :: trdone(6)
      double precision ::       ocdp 
      double precision :: cdp
      double precision :: offset
      double precision :: head_mute
      double precision :: tail_mute
      double precision :: trace_no
      double precision :: seq_no
      double precision :: trc_fold
      double precision :: iline_no
      double precision :: xline_no
      double precision :: avo_angle, dom_freq, user
      double precision :: lav, trcval

! --> Insert code for processing logic.
      ierr = 0
      jerr = 0

      if (.not. obj%bavel(1)%obj%initialized) then

         allocate(obj%hd_x(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_x(obj%ndpt, obj%numtr))

         allocate(obj%hd_g(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_g(obj%ndpt, obj%numtr))

         allocate(obj%hd_m(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_m(obj%ndpt, obj%numtr))

         allocate(obj%hd_t(ppavo_nhdrpz+obj%nwih))
         allocate(obj%tr_t(obj%ndpt))

         do nit = 1, obj%init_iterations
            write(*,*) 'AVOVIT: Initialization for iteration #',nit,'.'
            obj%bavel(nit)%obj%sampratz = obj%dt * 1000
            if(obj%survey_units .eq. 'FEET') then
               obj%bavel(nit)%obj%iunitsz  = ppavo_englishpz
            else
               obj%bavel(nit)%obj%iunitsz  = ppavo_metricpz
            end if
            obj%bavel(nit)%obj%imultcz  = 0
            obj%bavel(nit)%obj%icdpasnz = 1
            obj%bavel(nit)%obj%igeoasnz = 1
            obj%bavel(nit)%obj%ipsortz  = ppavo_cdppz
            obj%bavel(nit)%obj%idtypez  = ppavo_normalpz
            obj%bavel(nit)%obj%maxdtrz  = obj%numtr
            obj%bavel(nit)%obj%maxdtrz  = obj%maxdtrz
            obj%bavel(nit)%obj%numsmpz  = obj%ndpt
            obj%bavel(nit)%obj%nthz     = ppavo_nhdrpz + obj%nwih
            obj%bavel(nit)%obj%cleanupz = .false.
            call bavel_init (obj%bavel(nit)%obj, nio)
            if(nio .eq. FATAL_ERROR) then
               ntr = FATAL_ERROR
               return
            end if

            obj%buhci(nit)%obj%sampratz = obj%dt * 1000
            if(obj%survey_units .eq. 'FEET') then
               obj%buhci(nit)%obj%iunitsz  = ppavo_englishpz
            else
               obj%buhci(nit)%obj%iunitsz  = ppavo_metricpz
            end if
            obj%buhci(nit)%obj%imultcz  = 0
            obj%buhci(nit)%obj%icdpasnz = 1
            obj%buhci(nit)%obj%igeoasnz = 1
            obj%buhci(nit)%obj%ipsortz  = ppavo_cdppz
            obj%buhci(nit)%obj%idtypez  = ppavo_normalpz
            obj%buhci(nit)%obj%maxdtrz  = obj%numtr
            obj%buhci(nit)%obj%maxdtrz  = obj%maxdtrz
            obj%buhci(nit)%obj%numsmpz  = obj%ndpt
            obj%buhci(nit)%obj%nthz     = ppavo_nhdrpz + obj%nwih
            obj%buhci(nit)%obj%cleanupz = .false.

            call buhci_init (obj%buhci(nit)%obj, nio)
            if(nio .eq. FATAL_ERROR) then
               ntr = FATAL_ERROR
               return
            end if

         end do
      end if

      if(ntr .eq. NO_MORE_TRACES) obj%end_of_data = .true.
      if(ntr .eq. NEED_TRACES) then
         ntr_gather_out = NEED_TRACES
         goto 99999
      end if

      ntr_gather_in = ntr
      ntr_gather_out = 0

      ntr1 = 1
      maxpurge = obj%init_iterations
      acnt = 0
      bcnt = 0
      abcnt = 0

      multigather = 0
      multiiter = 0
      nio = ntr_gather_in

      if(obj%end_of_data) then
         ieoj_flag = 0
         ntr = 2
         abcnt = 2
         goto 69
      end if

!     Set end-of-job flag to zero in header.
      do nit = 1, obj%numtr
         obj%hd_g(ppavo_end_jobz, nit) = 0
         obj%hd_g(ppavo_lagz, nit) = 0
         obj%hd_g(ppavo_subz, nit) = 0
      end do

!     Get inline/crossline indices (3d data only).
!     Assume iline is constant (2d data only).
! old default      obj%ilineno = hd(ppavo_iline_noz, 1)
! old default      obj%xlineno = hd(ppavo_xline_noz, 1)
      obj%ilineno = hd(obj%ilin_hdr, 1)
      obj%xlineno = hd(obj%xlin_hdr, 1)

!     Get CDP number for the gather (1st trace only).
      cdp = hd(ppavo_cdpz, 1)
      obj%ocdpX = hd(ppavo_cdpz, 1)

      if(obj%first_gather) then
         obj%fcdp = 1
         obj%first_gather = .false.
      else
         if(obj%ilineno .ne. obj%ilineno_prev) then
            obj%fcdp = 1
         else
            obj%fcdp = obj%fcdp + 1
         end if
      end if
      obj%ilineno_prev = obj%ilineno

!FIX      obj%fcdp = obj%fcdp + 1
      if(obj%fcdp .gt. obj%ncdp_max) then
         call pc_error('Maximum number of CDPs exceeded!')
         ntr = FATAL_ERROR
         return
      end if

      obj%lagndx = obj%lagndx + 1
      if(obj%lagndx .gt. obj%maxlag) obj%lagndx = 1

60    ieoj_flag = 0
      promode = 1
      if(obj%end_of_data) then
         hd(ppavo_trc_typez, 1) = 43
         hd(ppavo_trc_typez, 2) = 44
      end if

      do nit = 1, nio

         trc_type  = hd(ppavo_trc_typez,  nit)
         gath_type = hd(ppavo_gath_typez, nit)
         stackword = hd(ppavo_stackwordz, nit)
         avo_angle = hd(ppavo_avo_anglez, nit)
         dom_freq  = hd(ppavo_dom_freqz,  nit)
         user      = hd(ppavo_userz,      nit)

!        Copy input traces to gather buffers(_g).
         do j = 1, obj%ndpt
            obj%tr_g(j, nit) = tr(j, nit)
         end do
         do j = 1, obj%nwih
            obj%hd_g(ppavo_nhdrpz+j, nit) = hd(j, nit)
         end do
         do j = 1, ppavo_nhdrpz
            obj%hd_g(j, nit) = 0.0
         end do
         obj%hd_g(ppavo_trace_noz,  nit) = hd(ppavo_trace_noz,  nit)
         obj%hd_g(ppavo_head_mutez, nit) = hd(ppavo_head_mutez, nit)
         obj%hd_g(ppavo_tail_mutez, nit) = hd(ppavo_tail_mutez, nit)
         obj%hd_g(ppavo_cdpz,       nit) = hd(ppavo_cdpz,       nit)
         obj%hd_g(ppavo_seq_noz,    nit) = hd(ppavo_seq_noz,    nit)
         obj%hd_g(ppavo_trc_foldz,  nit) = hd(ppavo_trc_foldz,  nit)
         obj%hd_g(ppavo_offsetz,    nit) = hd(ppavo_offsetz,    nit)
         obj%hd_g(ppavo_iline_noz,  nit) = hd(ppavo_iline_noz,  nit)
         obj%hd_g(ppavo_xline_noz,  nit) = hd(ppavo_xline_noz,  nit)
        
         obj%hd_g(ppavo_trc_typez,  nit) = hd(ppavo_trc_typez,  nit)
         obj%hd_g(ppavo_gath_typez, nit) = hd(ppavo_gath_typez, nit)
         obj%hd_g(ppavo_stackwordz, nit) = hd(ppavo_stackwordz, nit)
         obj%hd_g(ppavo_avo_anglez, nit) = hd(ppavo_avo_anglez, nit)
         obj%hd_g(ppavo_dom_freqz,  nit) = hd(ppavo_dom_freqz,  nit)
         obj%hd_g(ppavo_userz,      nit) = hd(ppavo_userz,      nit)

         stackword = 1
         if(hd(HDR_LAV, nit) .eq. 0) then
            trc_type = ppavo_deadpz
         else
            trc_type = ppavo_livepz
         end if
         obj%hd_g(ppavo_trc_typez,  nit)  = trc_type
         obj%hd_g(ppavo_stackwordz, nit) = stackword

         obj%hd_g(ppavo_purgez,     nit) = obj%purged
         obj%hd_g(ppavo_end_jobz,   nit) = 0
         obj%hd_g(ppavo_end_ensz,   nit) = ppavo_nlastpz
         if(nit .eq. nio) then
            obj%hd_g(ppavo_end_ensz, nit) = ppavo_lasttrpz
         end if
         obj%hd_g(ppavo_seq_noz,    nit) = nit
         obj%hd_g(ppavo_cdpz,       nit) = cdp
         obj%hd_g(ppavo_iline_noz,  nit) = obj%ilineno
         obj%hd_g(ppavo_xline_noz,  nit) = obj%xlineno

!FIX         if(trc_type .eq. 44) obj%fcdp = obj%fcdp + 1

         call ppavo_trlive(obj%tr_g(1:,nit), obj%ndpt, iflv, illv)
!         obj%hd_g(ppavo_live_sz,    nit) = (iflv-1) * obj%dt * 1000
!         obj%hd_g(ppavo_live_ez,    nit) = (illv-1) * obj%dt * 1000
         obj%hd_g(ppavo_full_sz,    nit) = obj%tstrt
         obj%hd_g(ppavo_full_ez,    nit) = obj%tstrt + ((obj%ndpt-1) * obj%dt)
         obj%hd_g(ppavo_full_sz,    nit) = obj%hd_g(ppavo_full_sz, nit) * 1000
         obj%hd_g(ppavo_full_ez,    nit) = obj%hd_g(ppavo_full_ez, nit) * 1000
         obj%hd_g(ppavo_live_sz,    nit) = obj%hd_g(ppavo_full_sz, nit)
         obj%hd_g(ppavo_live_ez,    nit) = obj%hd_g(ppavo_full_ez, nit)
!         obj%hd_g(ppavo_trc_foldz,  nit) = 1.0
         obj%hd_g(ppavo_amp_normz,  nit) = 1.0
         obj%hd_g(ppavo_lagz,       nit) = obj%lagndx

      end do

      iteration = 1
      nit = 1
      if(obj%purged .gt. 0) then
         nit = obj%purged
         iteration = obj%purged
      end if

62    if(nit .le. obj%init_iterations) then

         trdone(1) = 0
         trdone(2) = 0
         trdone(3) = 0
         trdone(4) = 0
         trdone(5) = 0
         trdone(6) = 0

!        Call main B_AVEL work routine.
         nhd = ppavo_nhdrpz + obj%nwih
         call bavel_work(obj%bavel(nit)%obj, nhd, nio, obj%hd_g, obj%tr_g)
         if(nio .eq. FATAL_ERROR) then
            ntr = FATAL_ERROR
            return
         end if

         promode = 1
         npiped = 1
         nflushed = 0
         obj%buhci(nit)%obj%ipsortz = ppavo_cdpz
         obj%buhci(nit)%obj%idtypez = ppavo_stackedpz

!        Record indices and count of A and B traces.
!         do i = 1, nio
!            trc_type = obj%hd_g(ppavo_trc_typez, i)
!            if(trc_type .eq. 43) then
!               acnt = acnt + 1
!               andx(acnt) = i
!            else if (trc_type .eq. 44) then
!               bcnt = bcnt + 1
!               bndx(bcnt) = i
!            end if
!         end do
!         abcnt = (acnt + bcnt) / 2

!        Skip if either A or B trace count is zero.
!         if(acnt .eq. 0 .or. bcnt .eq. 0) then
!            jerr = 1
!            goto 67
!         end if

!        Skip if unequal number of A and B traces.
!         if(acnt .ne. bcnt) then
!            jerr = 2
!            goto 67
!         end if

64       if(npiped .le. nio) then
 
!           Copy gather buffers(_g) to trace buffers(_t).
            do j = 1, (obj%nwih + ppavo_nhdrpz)
               obj%hd_t(j) = obj%hd_g(j, npiped)
            end do
            do j = 1, obj%ndpt
               obj%tr_t(j) = obj%tr_g(j, npiped)
            end do

!           If flushmode: send a dummy trace.
            if(promode .eq. 3) then
               obj%hd_t(ppavo_trc_typez) = ppavo_dummypz
            end if

!           Set the end-of-job flag
            obj%hd_t(ppavo_end_jobz) = 0
            if(obj%purged .gt. 0 .and. obj%purged .eq. nit) then
               obj%hd_t(ppavo_end_jobz) = 1
               obj%hd_t(ppavo_trc_typez) = ppavo_dummypz
            end if

!           Call main B_UHCI work routine.
            call buhci_work(obj%buhci(nit)%obj, ntr1, &
     &         obj%hd_t, obj%tr_t, promode)
            if(ntr1 .eq. FATAL_ERROR) then
               ntr = FATAL_ERROR
               return
            end if

!           If quitmode: skip rest of process (need to cleanup).
            if(promode .eq. 0) then
               npiped = nio + 1
            end if

!           If fillmode or pipemode: increment input counter.
            if(promode .eq. 1 .or. promode .eq. 2) then
               npiped = npiped + 1
            end if

!           If pipemode or flushmode: increment output counter.
            if(promode .eq. 2 .or. promode .eq. 3) then

               nflushed = nflushed + 1

!              Copy trace buffers(_t) to gather buffers(_x).
               do j = 1, (obj%nwih + ppavo_nhdrpz)
                  obj%hd_x(j, nflushed) = obj%hd_t(j)
               end do
               do j = 1, obj%ndpt
                  obj%tr_x(j, nflushed) = obj%tr_t(j)
               end do
               
               isendens = obj%hd_t(ppavo_end_ensz)
            end if
            goto 64
         end if

!        Copy gather buffers(_x) to gather buffers(_g).
         do k = 1, nflushed
            do j = 1, (obj%nwih + ppavo_nhdrpz)
               obj%hd_g(j, k) = obj%hd_x(j, k)
            end do
            do j = 1, obj%ndpt
               obj%tr_g(j, k) = obj%tr_x(j, k)
            end do
         end do

         nio = nflushed
         n = 1

65       if(n .le. nflushed) then

            trc_type  = obj%hd_g(ppavo_trc_typez, n)
            ieog_flag = obj%hd_g(ppavo_end_ensz,  n)
            ocdp      = obj%hd_g(ppavo_cdpz,      n)
            ispurge   = obj%hd_g(ppavo_purgez,    n)
            if(obj%end_of_data .and. nit .eq. obj%init_iterations .and.&
     &         ocdp .eq. obj%ocdpX) then
               ieoj_flag = 1
            end if

            if(ispurge .ne. 0) then
               goto 69
            end if

            ndx = 0
            if(trc_type .eq. 43) ndx = 1
            if(trc_type .eq. 44) ndx = 2
            if(trc_type .eq. 41) ndx = 3
            if(trc_type .eq. 51) ndx = 4
            if(trc_type .eq. 13) ndx = 5
            if(trc_type .eq. 1 .and. nit .eq. obj%init_iterations) ndx = 6
            if(ndx .eq. 6) ndx = 0

            if(ndx .ge. 1 .and. ndx .le. 6 .and. trdone(ndx) .eq. 0) then
               cdp       = obj%hd_g(ppavo_cdpz,       n)
               trace_no  = obj%hd_g(ppavo_trace_noz,  n)
               head_mute = obj%hd_g(ppavo_head_mutez, n)
               tail_mute = obj%hd_g(ppavo_tail_mutez, n)
               seq_no    = obj%hd_g(ppavo_seq_noz,    n)
               trc_fold  = obj%hd_g(ppavo_trc_foldz,  n)
               offset    = obj%hd_g(ppavo_offsetz,    n)
               iline_no  = obj%hd_g(ppavo_iline_noz,  n)
               xline_no  = obj%hd_g(ppavo_xline_noz,  n)

               trc_type  = obj%hd_g(ppavo_trc_typez,  n)
               gath_type = obj%hd_g(ppavo_gath_typez, n)
               stackword = obj%hd_g(ppavo_stackwordz, n)
               avo_angle = obj%hd_g(ppavo_avo_anglez, n)
               dom_freq  = obj%hd_g(ppavo_dom_freqz,  n)
               user      = obj%hd_g(ppavo_userz,      n)

               trdone(ndx) = 1
               if(ndx .eq. 6) then
                  if(ieog_flag .eq. ppavo_lasttrpz) then
                     trdone(ndx) = 1
                  else
                     trdone(ndx) = 0
                  end if
               end if
               ntr_gather_out = ntr_gather_out + 1
               cnt = ntr_gather_out

               offset = 0.0
               if(trc_type .eq. 1) then
                  offset = obj%hd_g(ppavo_offsetz, n)
               end if
               hdrlag = obj%hd_g(ppavo_lagz, n)
               trace_no = hdrlag

!              Copy trace sample values
               lav = 0.0
               do j = 1, obj%ndpt
                  tr(j, ntr_gather_out) = obj%tr_g(j, n)
                  trcval = tr(j, ntr_gather_out)
                  if(trcval .lt. 0.0) trcval = -trcval
                  if(trcval .gt. lav) lav = trcval
               end do

!              Copy trace header values.
               do j = 1, obj%nwih
                  hd(j, ntr_gather_out) = obj%hd_g(j+ppavo_nhdrpz, n)
               end do

               obj%trace_no = obj%trace_no + 1
               hd(ppavo_cdpz,       ntr_gather_out) = cdp
               hd(ppavo_offsetz,    ntr_gather_out) = offset
               hd(ppavo_trace_noz,  ntr_gather_out) = obj%trace_no
               hd(ppavo_seq_noz,    ntr_gather_out) = seq_no
               hd(ppavo_trc_foldz,  ntr_gather_out) = trc_fold
               hd(ppavo_head_mutez, ntr_gather_out) = head_mute
               hd(ppavo_tail_mutez, ntr_gather_out) = tail_mute
! old default               hd(ppavo_iline_noz,  ntr_gather_out) = iline_no
! old default               hd(ppavo_xline_noz,  ntr_gather_out) = xline_no
               hd(obj%ilin_hdr,     ntr_gather_out) = iline_no
               hd(obj%xlin_hdr,     ntr_gather_out) = xline_no

               hd(HDR_LAV,          ntr_gather_out) = lav

               hd(ppavo_trc_typez,  ntr_gather_out) = trc_type
               hd(ppavo_gath_typez, ntr_gather_out) = gath_type
               hd(ppavo_stackwordz, ntr_gather_out) = iteration
               hd(ppavo_avo_anglez, ntr_gather_out) = avo_angle
               hd(ppavo_dom_freqz,  ntr_gather_out) = dom_freq
               hd(ppavo_userz,      ntr_gather_out) = user

            end if
            if(ieog_flag .ne. 0) then
               trdone(1) = 0
               trdone(2) = 0
               trdone(3) = 0
               trdone(4) = 0
               trdone(5) = 0
               trdone(6) = 0
            end if
66          n = n + 1
            goto 65
         end if

!        If multiple gathers have been flushed, store excess
!        and then continue.
         do n = 1, nflushed
            if(obj%hd_g(ppavo_end_ensz, n) .ne. 0 .and. &
     &         obj%hd_g(ppavo_trc_typez, n) .eq. 1 .and. &
     &         n .lt. nflushed) then
               do m = (n+1), nflushed
                  gtndx = m
                  ghndx = m
                  mtndx = m-n
                  mhndx = m-n
                  if((obj%hd_g(ppavo_end_ensz,  m) .ne. 0 .and.&
     &                obj%hd_g(ppavo_trc_typez, m) .eq. 1) .or.&
     &                m .eq. nflushed) then
                     multigather = multigather + 1
                  end if

!                 Copy gather buffers(_g) to multigather buffers(_m).
                  do j = 1, obj%ndpt
                     obj%tr_m(j,mtndx) = obj%tr_g(j,gtndx)
                  end do
                  do j = 1, (obj%nwih+ppavo_nhdrpz)
                     obj%hd_m(j,mhndx) = obj%hd_g(j,ghndx)
                  end do
               end do
               multiiter = iteration + 1
               multicount = nflushed - n
               multiindex = 1
               nio = n
               goto 67
            end if
         end do
67       iteration = iteration + 1
         nit = nit + 1
         goto 62
      end if

!     If excess gathers have been stored, load next
!     and then continue.
      if(multigather .ne. 0) then
         do m = 1, multicount
            gtndx = m
            ghndx = m
            mtndx = multiindex
            mhndx = multiindex
            multiindex = multiindex + 1
!           Copy multigather buffers(_g) to gather buffers(_m).
            do j = 1, obj%ndpt
               obj%tr_g(j,gtndx) = obj%tr_m(j,mtndx)
            end do
            do j = 1, (obj%nwih + ppavo_nhdrpz)
               obj%hd_g(j,ghndx) = obj%hd_m(j,mhndx)
            end do
            if((obj%hd_g(ppavo_end_ensz,ghndx) .ne. 0 .and.&
     &          obj%hd_g(ppavo_trc_typez,ghndx) .eq. 1) .or.&
     &          m .eq. multicount) then
               nio = m
               goto 68
            end if
         end do
68       multigather = multigather - 1
         multicount = multicount - nio
         nit = multiiter
         iteration = nit
         if(nit .le. obj%init_iterations) then
            goto 62
         end if
      end if

69    continue

!     Check for end-of-job flag.
      if (ieoj_flag .ne. 0) then
         obj%purged = maxpurge + 1
         goto 99999
      end if

!     If at end-of-job, manually force a purge.
      if(obj%end_of_data .and. obj%purged .lt. maxpurge) then
         nio = 1
         obj%purged = obj%purged + 1
         nit = obj%purged
         if(obj%cwin_cdp .gt. 1) then
            write(*,*) 'AVOVIT: Flushing CDPs for iteration #',obj%purged,'.'
            goto 60
         end if
      end if

99999 continue
      ntr = ntr_gather_out
      if(ntr .eq. 0 .and. .not. obj%end_of_data) ntr = NEED_TRACES
      if(ntr .eq. 0 .and. obj%end_of_data) ntr = NO_MORE_TRACES
      if(ntr .ge. 1) then
         i = 1
         do
            if(hd(ppavo_trc_typez,i) .eq. 43 .or.&
     &         hd(ppavo_trc_typez,i) .eq. 44) then
               if(i .lt. ntr) then
                  do j = i, ntr
                     hd(1:obj%nwih,j) = hd(1:obj%nwih,j+1)
                     tr(1:obj%ndpt,j) = tr(1:obj%ndpt,j+1)
                  end do
               end if
               ntr = ntr - 1
            else
               i = i + 1
            end if
            if(i .gt. ntr) exit
         end do
      end if

      end subroutine avovit


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine avovit_wrapup (obj)
      type(avovit_struct),intent(inout) :: obj       ! arguments
      integer :: nhd, ntr, nit
      integer :: promode

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      do nit = 1, obj%init_iterations
         write(*,*) 'AVOVIT: Cleanup for iteration #',nit,'.'
         if(obj%bavel(nit)%obj%initialized) then
            obj%bavel(nit)%obj%cleanupz = .true.
            call bavel_work(obj%bavel(nit)%obj, nhd, ntr, &
     &         obj%hd_g, obj%tr_g)
         end if
         if(obj%buhci(nit)%obj%initialized) then
            obj%buhci(nit)%obj%cleanupz = .true.
            call buhci_work(obj%buhci(nit)%obj, ntr, &
     &         obj%hd_t, obj%tr_t, promode)
         end if
      end do
      if(associated(obj%hd_x)) deallocate(obj%hd_x)
      if(associated(obj%tr_x)) deallocate(obj%tr_x)
      if(associated(obj%hd_g)) deallocate(obj%hd_g)
      if(associated(obj%tr_g)) deallocate(obj%tr_g)
      if(associated(obj%hd_m)) deallocate(obj%hd_m)
      if(associated(obj%tr_m)) deallocate(obj%tr_m)
      if(associated(obj%hd_t)) deallocate(obj%hd_t)
      if(associated(obj%tr_t)) deallocate(obj%tr_t)

      end subroutine avovit_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module avovit_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

