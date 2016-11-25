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
! Name       : FFAVA    (Fluid Factor AVA method)
! Category   : stacks
! Written    : 1994-03-09   by: Bob Baumel & Javaid Durrani
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Generate "fluid factor" AVA trace displays
! Portability: No known limitations.
! Parallel   : No
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! FFAVA produces "fluid factor" traces F(t), intended to highlight anomalous
! AVA behavior of hydrocarbon-related reflections as compared with background
! reflections from water-saturated rocks.  We define F(t) as the difference
! between the ACTUAL AVA gradient B(t) and a PREDICTED gradient based on
! background "fluid" trend; i.e.:
!
!                  F(t)   =   B(t)  -  W(t)*A(t),
!
! where A(t) is zero-offset trace value and W(t) is estimated slope of the
! background fluid trend.   (This is the default fluid factor type - others are
! also available.)
!
! This assumes that background "fluid" events follow linear trends in the (A,B)
! plane (i.e., B = W*A), as described by Foster et al (1993).  Note:
! W = -cot(phi)  where "phi" is the "fluid angle" defined by Foster et al.
!
! We make W(t) a (slowly-varying) function of time because we expect the
! background fluid trend to be depth-dependent (where W becomes increasingly
! negative at greater depths).  Foster et al (1993) did not consider depth
! dependence of their fluid trends.  Our depth dependent W(t) is similar in
! concept to the "geogain" function of Gidlow & Smith (1992).
!
!
! Weighting Function Options
!
! FFAVA can derive weighting functions W(t) automatically from the seismic data
! (OPT_WF = CALC), or you can input a W(t) function manually (OPT_WF = USER),
! or you can generate constant weight function panels to pick your W(t)
! functions manually (OPT_WF = TRIAL).  If OPT_WF = TRIAL, output traces are
! sorted internally into constant weight factor panel order.
!
! When OPT_WF = CALC, FFAVA derives a single weight-factor function W(t) for
! each input "section," where each "section" is identified by a constant value
! of header word HDR_IN.  If you wish, you may fool FFAVA into deriving new
! weighting functions for any desired groups of traces, by setting HDR_IN
! appropriately.  When OPT_WF = USER, you input only one W(t) function, which
! is used for all traces read into the job.
!
!
! Automatic Weighting Function Calculation
!
! Derivation of W(t) when OPT_WF = CALC is a two-step process:  First, a W(t)
! function is derived for each input [A(t),B(t)] pair by finding W-values to
! minimize the median absolute value of [B(t)-W*A(t)] within time windows of
! height WIN_LEN. (These windowed medians are handled as in process MVXP. The
! window length WIN_LEN should be big enough to contain mostly background
! "fluid" events instead of being dominated by events with anomalous AVA.) Once
! W(t) functions have been derived for each trace pair, they are averaged over
! an entire section as defined by HDR_IN.
!
!
! Trial Weighting Function Parameters
!
! The role of WF_INIT, WF_INC and WF_TOT depend on your value of OPT_WF:
! When OPT_WF = CALC, these define a search range used by FFAVA internally as
! it looks for W-values to minimize the windowed medians; thus, they must cover
! the range of possible W-values, and must cover it densely enough to locate
! the minimum with reasonable accuracy.
!
! When OPT_WF = TRIAL, these parameters simply specify the trial W-values for
! your output panels.  The WF_INIT, WF_INC and WF_TOT parameters are not used
! when OPT_WF = USER.
!
! The default values (WF_INIT=-1.5, WF_INC=0.1, WF_TOT=16) are only appropriate
! when FFAVA has been immediately preceded by MVXP.  In this case, derived
! W-values are typically on the order of -1.0 to -0.5.  If MVXP has NOT been
! applied just before FFAVA, try setting WF_INIT=-3.0, WF_INC=0.2; in this
! case, typical W-values are around -2.5 to -2.0.
!
!
! Fluid Factor Options
!
! The OPT_FF parameter provides four options for the type of "fluid factor"
! output:
!
!      OPT_FF = 1:   F = (B - W*A).
!      OPT_FF = 2:   F = (B - W*A) * [(2+W)*A + (1+W)*B] / [1 + (1+W)**2].
!      OPT_FF = 3:   F = (B - W*A) * (A + W*B) / (1 + W**2).
!      OPT_FF = 4:   Anomaly Classifier (See below).
!
! The default (OPT_FF = 1) is our "standard" fluid factor, analogous to the
! fluid factors of Smith & Gidlow et al; this is a linear combination of A and
! B.
!
! OPT_FF = 2 and 3 are PRODUCT measures, based on an idea of Fred
! Hilterman: to generalize the familiar A*B product (effective only for shallow
! gas [Class III] anomalies) to a product measure that works better for Class
! II and Class I anomalies.  Hilterman uses the "fluid trend" idea by rotating
! his coordinate axes until they are parallel and perpendicular to the fluid
! trend; then he multiplies coordinate components.  Instead of the (A,B) plane,
! Hilterman works in the (NI,PR) plane where
!
!       NI = "Normal-Incidence Reflectivity" = A.
!       PR = "Poisson Reflectivity"          = A + B.
!
! This results in the slightly complicated formula shown above for OPT_FF=2
! when expressed in terms of A and B.  OPT_FF=3 is a simpler formula obtained
! by applying Hilterman's rotation idea directly in the (A,B) plane.  In all
! cases, the "fluid trend" is B = W*A.
!
!
! Anomaly Classifier Option
!
! The Anomaly Classifier option (OPT_FF=4) is intended to determine whether
! events are "anomalous," and if so, which Rutherford & Williams class (I, II,
! III or IV) the anomaly belongs in.  The classification is based on input
! (A,B) values, but output trace samples are restricted to a few discrete
! values, intended to be plotted with a linear color bar.
!
! Specifically, output traces contain only 11 possible values: -4, -3, -2, -1,
! -0.5, 0, +0.5, +1, +2, +3, +4.  Absolute values of 1 - 4 are intended to
! indicate anomalies of Class I, II, III and IV respectively.  The algebraic
! sign of the result always matches that of the ordinary (OPT_FF=1) FFAVA
! trace.  Absolute value 0.5 indicates non-anomalous events, meaning that the
! magnitude of the ordinary FFAVA trace is less than THRESH_FF.  A result of 0
! means that the (A,B) point is very close to the origin (absolute value of A
! and B both less than 0.001) - usually missing data zones.
!
! In the Anomaly Classifier (OPT_FF=4), if an event is classified as anomalous
! (meaning that the ordinary OPT_FF=1 trace has magnitude greater than
! THRESH_FF), it is further classified as Class I, II, III or IV according to
! the value of "A" (zero offset reflectivity) and the sign of "B."
!
! Events with negative FFAVA response (top-of-reservoir reflection) are
! classified as follows:
!
!     Class I   if B < 0.0 and A > CLASS2_MAX
!
!     Class II  if B < 0.0 and  CLASS2_MIN < A < CLASS2_MAX
!
!     Class III if B < 0.0 and  A < CLASS2_MIN,
!
!     Class IV  if B > 0.0 and  A < CLASS2_MIN,
!
! Events with positive FFAVA response (bottom-of-reservoir reflection), are
! classified with the same regions reflected through the origin of the (A,B)
! plane.
!
!
! References
!
! Gidlow, P.M., Smith, G.C., and Vail, P.J., 1992, Hydrocarbon detection
!   using fluid factor traces: a case history; in Robinson, J.H., ed.,
!   How Useful is Amplitude-Versus-Offset Analysis?, Technical Program
!   and Abstracts, Joint SEG/EAEG Summer Research Workshop, Aug 9-14,
!   Big Sky, Montana
!
! Smith, G.C. and Gidlow, P.M., 1987, Weighted stacking for rock
!   property estimation and detection of gas; Geophysical Prospecting,
!   35, 993-1014.
!
! Foster, D.J., Smith, S.W., Dey-Sarkar, S., and Swan, H.W., 1993,
!   A Closer Look at Hydrocarbon Indicators; presented at 63rd Annual
!   SEG Meeting (paper SL2.2), Sept 26-30, Washington, DC.
!
! Castagna, J.P and Smith, S.W., 1994, Comparison of AVO Indicators: A
!   modeling study; GEOPHYSICS, 59, 1849-1855.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! FFAVA must be PRECEDED by process AVAGRAD with OPT_OUTPUT = A_AND_B.  Thus,
! the input to FFAVA must consist of 2-trace gathers where the first trace of
! each  gather is the zero-offset extrapolated trace and the second trace of
! the gather is the AVA gradient.
!
! FFAVA should be preceded by MVXP with no debrighten (after AVAGRAD and before
! FFAVA).  Although not absolutely necessary, this better standardizes the
! amplitudes of the input data, allowing various parameters to be set in a more
! data-independent manner, especially if using the FFAVA Anomaly Classifier
! (OPT_FF = 4).
!
!
! Run Time
!
! Run time of FFAVA when OPT_WF = CALC should be similar to running the MVXP
! process WF_TOT times on your stacked data.  When OPT_WF = USER or TRIAL,
! execution time is much faster.
!
!
! Trace Output
!
! FFAVA outputs traces one at a time when OPT_WF = USER or CALC. When
! OPT_WF = TRIAL, it outputs traces one at a time sorted into WF-value panels.
!
!
! Plotting Anomaly Classifier Output
!
! When plotting results of the Anomaly Classifier (OPT_FF=4) using a linear
! color bar in a program such as COLR, all vertical and horizontal color
! grading must be turned OFF.  Similarly, after outputting these traces from
! FFAVA with OPT_FF=4, don't do ANYTHING to them that could alter the trace
! values before plotting.  As one example, do not apply a static shift, because
! shifting by a non-integer number of samples will interpolate the trace values!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
!
! Process is a multiple-trace/all-trace (loop-splitting) process.*
! This process requires traces to be input in gathers.
!
! The input to FFAVA must consist of 2-trace gathers where the first trace of
! each gather is the zero-offset extrapolated trace and the second trace of the
! gather is the AVA gradient.  Normally FFAVA will be preceded by process
! AVAGRAD with OPT_OUTPUT = A_AND_B.
!
! * If OPT_WF = CALC, this is a multiple-trace (loop-splitting) process, as
!   FFAVA must receive a complete input section (and calculate its weight
!   function) before outputting any traces from that section.
!
!   If OPT_WF = TRIAL, this is an all-trace (loop-splitting) process, as the
!   sort to panels requires putting the whole dataset to disk before outputting
!   any traces.
!
!   If OPT_WF = USER, this is simply a multiple-trace process that transforms
!   each 2-trace input gather to a single output trace.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
! This process alters input traces.
!
! FFAVA always outputs traces one at a time.
!
! For OPT_WF = CALC or USER, the number of output traces is exactly half the
! number of input traces, as each 2-trace input gather is reduced to a single
! output trace.
!
! For OPT_WF = TRIAL, the number of output traces is  WF_TOT * N/2,  where
! N is the number of input traces. Here, the output is sorted into Weight-value
! panels, consisting of WF_TOT such panels, with N/2 traces in each panel.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                           Action taken
! ----      -----------                           ------------
! NUMTR     max number of traces input/output     2 on input, 1 on output
! GATHERED  whether traces are legitimate gather  true on input, false on output
! NWIH      number of words in trace header       used but not changed
! NDPT      number of sample values in trace      used but not changed
! TSTRT     starting time on trace                used but not changed
! DT        trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#   Description                 Action taken
! ----   -----------                 ------------
!
! 1      Sequential Trace Count      Renumbered.
! 2      Head mute                   Used, may be rest.
! 3      Current group number        Renumbered (based on HDR_IN values) *
! 4      Trace number within group   Renumbered (based on HDR_IN values) *
! 24     Panel number                Set for WF panels if OPT_WF = TRIAL
! 25     Largest Absolute Value      Reset
! 64     Tail mute                   Used, may be reset.
!        HDR_IN                      Identifies input sections
!        HDR_WF                      Carries weight value if OPT_WF = TRIAL
!
! * Header words 3 and 4 are renumbered according to changds in HDR_IN when
!   OPT_WF = CALC or USER. However, when OPT_WF = TRIAL, header words 3 and 4
!   are renumbered by the bin sorting algorithm, and HDR_IN has no effect.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date      Author       Description
!     ----      ------       -----------
! 16.  2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 15.  2001-11-05 Baumel     Add statement to compile in new absoft compiler.
! 14.  2001-06-18 Baumel     Finished alpha testing; various bug fixes.
!                            PRODUCTION.
! 13.  2001-01-25 Selzler    Conversion to new CPS, ready to alpha test.
! 12.  1999-01-25 Vunderink  Begin using the f90 compiler.
! 11.  1996-09-19 Baumel     Moved to conlib.
! 10.  1996-03-11 Baumel     Change defaults for WTRY1, DWTRY (assume MVXP
!                            precedes FFAVA). Documentation additions.
!  9.  1996-02-23 Baumel     Add FFAVA Anomaly Classifier (FFTYPE=4).
!  8.  1995-10-03 Baumel     Major rewrite: New algorithm for computing
!                            weight factors based on minimizing medians;
!                            many parameter changes.
!  7.  1994-12-09 Baumel     Another minor bug fix.
!  6.  1994-10-31 Baumel     Couple of minor bug fixes.
!  5.  1994-07-07 Baumel     New options for fluid factor type (currently
!                            available through WFCALC parameter).
!  4.  1994-06-24 Baumel     Allow manual entry of weight-factors; also
!                            new sign convention for weight-factors.
!  3.  1994-03-29 Baumel     More robust weight-factor calculation.
!  2.  1994-03-24 Baumel     Fluid Factor = new linear comb. of A & B.
!  1.  1994-03-09 Baumel     Original working version.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST  varies*    whether this process ever needs to request traces.
! NEED_LABEL    varies*    whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! * NEED_REQUEST and NEED_LABEL are true when OPT_WF = CALC or TRIAL, false
!   when OPT_WF = USER.
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
!    Our F(t) is similar, although not identical, to the 1987 and 1992
!    fluid factors from Gidlow et al (which differed from each other)!
!    Our F(t) is a linear combination of A and B.  Both of Gidlow's
!    versions are APPROXIMATE linear combinations of A and B (but not
!    exactly because they parameterized the AVA curves differently).
!    Their 1987 version was a FIXED linear combination, with weighting
!    based on standard empirical relationships (possibly refined by
!    data from local well logs).  Their 1992 version is a DIFFERENT
!    linear combination, with depth-dependent weighting ("geogain"
!    function) derived from the seismic data using a process similar
!    to velocity analysis (no more need for empirical relationships or
!    well-log calibration).
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>
!
!-------------------------------------------------------------------------------
!<gui_def>
!<NS FFAVA Process/NC=80>
!          FFAVA (Fluid Factor AVA). Finds F(t) = B(t) - W(t)*A(t)
!      where A = AVA Intercept, B = AVA Gradient, W = Weight function.
!    This process must be PRECEDED by AVAGRAD with OPT_OUTPUT = A_AND_B.
!
!    OPT_WF=~~~`CCCCC        OPT_FF=~~~~`C            HDR_IN=~~~`III
!
!                       Used only when OPT_WF = CALC:
!    WIN_LEN=~~`FFFFFFF      TIM_BEG=~~~`FFFFFFF      TIM_END=~~~`FFFFFFF
!                            OPT_WF_OUT=`CCCCC
!
!                 Used when OPT_WF = CALC or OPT_WF = TRIAL:
!    WF_INIT=~~`FFFFFFF      WF_INC=~~~~`FFFFFFF      WF_TOT=~~~~`III
!                            HDR_WF=~~~~`III
!
!                   For Anomaly Classifier (OPT_FF = 4):
!    THRESH_FF=`FFFFFFF      CLASS2_MIN=`FFFFFFF      CLASS2_MAX=`FFFFFFF
!
!                    Weight function for OPT_WF = USER:
!                            TIMES   WEIGHTS
!                            `FFFFFFF`FFFFFFF
!                            `FFFFFFF`FFFFFFF
!                            `FFFFFFF`FFFFFFF
!                            `FFFFFFF`FFFFFFF
!                            `FFFFFFF`FFFFFFF
!<PARMS TIMES_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="OPT_WF">
!<Tip> Whether to calculate weights, use user specified or trial weights. </Tip>
! Default = CALC
! Allowed = CALC   (Calculate weight function automatically).
! Allowed = TRIAL  (Output trial weight function panels).
! Allowed = USER   (Use a user specified weight function).
!
! When OPT_WF = CALC, FFAVA calculates the weight function W(t) for you. Here,
! W(t) is determined as a slowly-varying function of time (depth). A separate
! weight function is calculated and applied to each input section as defined
! by jumps in header word HDR_IN.
!
! When OPT_WF = TRIAL, FFAVA produces fluid factor panels for a set of trial
! W-values, allowing you to pick a W(t) function manually. This is analogous
! to velocity analysis by generating constant-velocity panels using the GVS
! process. At any depth, the optimal W-value is that which produces the
! quietest background with clearly visible (AVA) anomalous events.
!
! When OPT_WF = USER, you input a user-specified W(t) function, which may have
! been determined by examining panels generated by FFAVA using OPT_WF = TRIAL.
! For OPT_WF = USER, the W(t) function is specified using the TIMES and WEIGHTS
! arrays.
!</Help>
!
!<Help KEYWORD="OPT_FF">
!<Tip> Type of fluid factor calculation to perform. </Tip>
! Default = 1
! Allowed = 1   Standard, (B - W*A).
! Allowed = 2   Hilterman product (rotated NI*PR).
! Allowed = 3   Rotated A*B product.
! Allowed = 4   FFAVA Anomaly Classifier.
! The default (OPT_FF = 1) produces our "standard" fluid factor, analogous
! to the fluid factors of Smith & Gidlow et al; this is a linear combination
! of A and B (Intercept and Gradient).
!
! The "Hilterman product" is the product of Normal-Incidence Reflectivity (NI)
! and Poisson Reflectivity (PR) after rotating axes so the background fluid
! trend runs along the NI axis.  [Note: NI = A  ;  PR = A + B]
!
! The "rotated A*B product" is a similar product after rotating axes, but in
! the original (A,B) plane instead of the (NI,PR) plane.
!
! The "Anomaly Classifier" is intended to produce plots indicating whether AVA
! anomalies are Rutherford and Williams Class 1, 2 or 3, or Castagna Class 4.
!</Help>
!
!<Help KEYWORD="HDR_IN">
!<Tip> Header word to use for identifying input sections. </Tip>
! Default = 8
! Allowed = 0 - NWIH
! The values in header word HDR_IN should be integers, identifying
! successive input sections.
!
! HDR_IN has its greatest effect when OPT_WF = CALC, in which case FFAVA
! calculates and applies a separate weight function for every section.
!
! If you want to be tricky (when OPT_WF = CALC), you could set HDR_IN to
! calculate weight functions for various selected trace groupings, instead
! of standard seismic lines. For example, if HDR_IN = 1, FFAVA will derive
! and apply a separate weight function for every trace.
!
! HDR_IN is also used for renumbering header words 3 and 4 when OPT_WF = CALC
! or USER. However, when OPT_WF = TRIAL, numbering of header words 3 and 4 is
! determined by the panel sorting routine, so HDR_IN has no effect.
!</Help>
!
!-------------------parameters active if OPT_WF = CALC----------------------
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of time window, in seconds, for weight function calculation.</Tip>
! Default = 1.0
! Allowed = real > 0.0
! Length of time window, in seconds, for automatic weight function calculation.
!
! Weight factors are estimated for each input [A(t),B(t)] trace pair by
! finding W to minimize the median absolute value of [B(t) - W*A(t)] in time
! windows of length WIN_LEN. (The estimated weight functions are then averaged
! over the whole section.)  The windowed medians are handled the same as in
! process MVXP.  Choose WIN_LEN big enough so it contains MOSTLY background
! "fluid" events rather than events with anomalous AVA character.
!
! Active only when OPT_WF = CALC.
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Beginning time, in seconds, for weight function calculation. </Tip>
! Default = TSTRT
! Allowed = real
! Beginning time, in seconds, for automatic weight function calculation.
!
! Active only when OPT_WF = CALC.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Ending time, in seconds, for weight function calculation. </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG
! Ending time, in seconds, for automatic weight function calculation.
!
! Active only when OPT_WF = CALC.
!</Help>
!
!<Help KEYWORD="OPT_WF_OUT">
!<Tip> Option for outputting weight function traces when OPT_WF = CALC. </Tip>
! Default = NONE
! Allowed = NONE  (Output NO weight functions in output section).
! Allowed = FIRST (Output weight function as FIRST trace of output section).
! Allowed = ONLY  (Output ONLY the weight functions -- no seismic data!).
!
! The default is to not output any of the weight functions calculated by
! FFAVA when OPT_WF = CALC.
!
! Choosing OPT_WF_OPT = FIRST may be useful for QC, so you can look at the
! weight functions that FFAVA generates.
!
! Choosing OPT_WF_OUT = ONLY may POSSIBLY produce a useful attribute. In
! this case, it would be best to compute FINE-GRAINED weight functions by
! choosing WIN_LEN very small (just a few samples), and by manipulating
! HDR_IN so that each input "section" contains only a few traces.
!
! Active only when OPT_WF = CALC.
!</Help>
!
!--------------parameters active if OPT_WF = CALC or TRIAL------------------
!
!<Help KEYWORD="WF_INIT">
!<Tip> First trial weight function value. </Tip>
! Default = -1.5
! Allowed = real
! First Weight value to try when OPT_WF = CALC or TRIAL.
!
! The default of WF_INIT = -1.5 is appropriate if FFAVA is immediately
! preceded by MVXP.  If not, you might try WF_INIT = -3.
!
! When OPT_WF = CALC, WF_INIT, WF_INC and WF_TOT define an internal search
! range for minimizing windowed medians of [B(t)-W*A(t)].
!
! When OPT_WF = TRIAL, WF_INIT, WF_INC and WF_TOT set trial Weight values for
! output panels.
!
! Active only if OPT_WF = CALC or TRIAL.
!</Help>
!
!<Help KEYWORD="WF_INC">
!<Tip> Increment between trial weight function values. </Tip>
! Default = 0.1
! Allowed = real
! Trial Weight value increment when OPT_WF = CALC or TRIAL.
!
! The default of WF_INC = 0.1 is appropriate if FFAVA is immediately preceded
! by MVXP.  If not, you might try WF_INC = 0.2.
!
! When OPT_WF = CALC, WF_INIT, WF_INC and WF_TOT define an internal search
! range for minimizing windowed medians of [B(t)-W*A(t)].
!
! When OPT_WF = TRIAL, WF_INIT, WF_INC and WF_TOT set trial Weight values for
! output panels.
!
! Active only if OPT_WF = CALC or TRIAL.
!</Help>
!
!<Help KEYWORD="WF_TOT">
!<Tip> Total number of trial weight function values. </Tip>
! Default = 16
! Allowed = int > 0
! Number of trial Weight values when OPT_WF = CALC or TRIAL.
!
! When OPT_WF = CALC, WF_INIT, WF_INC and WF_TOT define an internal search
! range for minimizing windowed medians of [B(t)-W*A(t)].
!
! When OPT_WF = TRIAL, WF_INIT, WF_INC and WF_TOT set trial Weight values for
! output panels.
!
! Active only if OPT_WF = CALC or TRIAL.
!</Help>
!
!<Help KEYWORD="HDR_WF">
!<Tip> Header word to carry weight function value if OPT_WF = TRIAL. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!
! Active only if OPT_WF = TRIAL.
!</Help>
!
!--------------------parameters active if OPT_FF = 4----------------------------
!
!<Help KEYWORD="THRESH_FF">
!<Tip> Classifier threshold. </Tip>
! Default = 5.0
! Allowed = real >= 0.0
! The absolute value of (B-W*A) must exceed this threshold in order to
! classify an event as anomalous. For events considered non-anomalous (i.e.,
! within this distance of the fluid trend), the anomaly classifier trace will
! have a value of -0.5 or +0.5 (with sign matching the ordinary OPT_FF=1 FFAVA
! trace), or 0.0 in case the original (A,B) point is very close to the origin.
!
! Active only if OPT_FF = 4.
!</Help>
!
!<Help KEYWORD="CLASS2_MIN">
!<Tip> Class II minimum for classifier. </Tip>
! Default = -6.0
! Allowed = real < 0.0
! Minimum value of Intercept (A) to classify an event as Class II [This is for
! events with negative B-W*A; i.e., top-of-reservoir reflection. For events
! with positive B-W*A, the corresponding zones are obtained by reflection
! through the origin of the (A,B) plane].
!
! Active only if OPT_FF = 4.
!</Help>
!
!<Help KEYWORD="CLASS2_MAX">
!<Tip> Class II maximum for classifier. </Tip>
! Default = 4.0
! Allowed = real > 0.0
! Maximum value of Intercept (A) to classify an event as Class II [This is for
! events with negative B-W*A; i.e., top-of-reservoir reflection. For events
! with positive B-W*A, the corresponding zones are obtained by reflection
! through the origin of the (A,B) plane].
!
! Active only if OPT_FF = 4.
!</Help>
!
!----------------user specified weight function parameters----------------------
!
!<Help KEYWORD="TIMES">
!<Tip> Trace times, in seconds, for user specified weight functions. </Tip>
! Default = -
! Allowed = real linked array
! Entries must be in increasing order.
!</Help>
!
!<Help KEYWORD="WEIGHTS">
!<Tip> Weight values for user specified weight functions. </Tip>
! Default = -
! Allowed = real linked array
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module ffava_module
      use pc_module
      use named_constants_module
      use mth_module
      use median_module
      use mutehw_module
      use interp_module
      use temptfile_module
      use binsort_module
      use mem_module
      use lav_module
      use string_module
      implicit none
      private
      public :: ffava_create
      public :: ffava_initialize
      public :: ffava_update
      public :: ffava_delete
!<execute_only>
      public :: ffava
      public :: ffava_wrapup
!</execute_only>


      character(len=100),public,save :: ffava_IDENT = &
'$Id: ffava.f90,v 1.16 2006/09/11 13:15:45 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: ffava_struct
        private
        logical                    :: skip_wrapup      ! wrapup flag.

        character(len=5)           :: opt_wf           ! process parameters.
        integer                    :: opt_ff           ! process parameters.
        integer                    :: hdr_in           ! process parameters.
        character(len=5)           :: opt_wf_out       ! process parameters.
        integer                    :: hdr_wf           ! process parameters.
        real                       :: win_len          ! process parameters.
        real                       :: tim_beg          ! process parameters.
        real                       :: tim_end          ! process parameters.
        real                       :: wf_init          ! process parameters.
        real                       :: wf_inc           ! process parameters.
        integer                    :: wf_tot           ! process parameters.
        real                       :: thresh_ff        ! process parameters.
        real                       :: class2_min       ! process parameters.
        real                       :: class2_max       ! process parameters.
        real             , pointer :: times(:)         ! process parameters.
        real             , pointer :: weights(:)       ! process parameters.

        integer                    :: nwih, ndpt       ! global parameters.
        real                       :: dt, tstrt        ! global parameters.

        integer                    :: times_cnt        ! dependent parameter
        integer                    :: tim_beg_idx      ! dependent parameter
        integer                    :: tim_end_idx      ! dependent parameter
        integer                    :: num_in_sect      ! dependent parameter
        integer                    :: num_sect         ! dependent parameter
        integer                    :: ioutsect         ! dependent parameter
        integer                    :: iouttot          ! dependent parameter
        integer                    :: win_len_cnt      ! dependent parameter
        logical                    :: isaved           ! dependent parameter
        double precision , pointer :: hdsave(:,:)      ! Saved header
                                                       ! D1 = nwih, D2 = 2
        real             , pointer :: trsave(:,:)      ! Saved trace
                                                       ! D1 = ndpt, D2 = 2
        real             , pointer :: grad_wght(:)     ! FFAVA weight function.
                                                       ! D1 = ndpt
        integer                    :: nwtsum           ! dependent parameter
        integer                    :: hsval            ! dependent parameter
        integer                    :: print_lun        ! dependent parameter
        type(temptfile_struct),pointer :: tempt        ! dependent parameter
        type(binsort_struct),pointer :: binsort        ! dependent parameter
      end type ffava_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(ffava_struct),pointer,save :: object      ! needed for traps.

      character(len=5),dimension(3),parameter :: OPT_WF_OPTIONS = &
        (/ 'CALC ', 'TRIAL', 'USER ' /)

      character(len=1),dimension(4),parameter :: OPT_FF_OPTIONS = &
        (/ '1', '2', '3', '4' /)

      character(len=5),dimension(3),parameter :: OPT_WF_OUT_OPTIONS = &
        (/ 'NONE ', 'FIRST', 'ONLY ' /)

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine ffava_create (obj)
      implicit none
      type(ffava_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%times)
      nullify(obj%weights)
      nullify(obj%hdsave)
      nullify(obj%trsave)
      nullify(obj%grad_wght)
      nullify(obj%tempt)
      nullify(obj%binsort)

      call ffava_initialize (obj)
      return
      end subroutine ffava_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine ffava_delete (obj)
      implicit none
      type(ffava_struct),pointer :: obj       ! arguments

!<execute_only>
      call ffava_wrapup (obj)
!</execute_only>

      call mem_free(obj%times)
      call mem_free(obj%weights)
      call mem_free(obj%hdsave)
      call mem_free(obj%trsave)
      call mem_free(obj%grad_wght)
      if(associated(obj%tempt)) call temptfile_close(obj%tempt)
      if(associated(obj%binsort)) call binsort_close(obj%binsort)

      deallocate(obj)
      return
      end subroutine ffava_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine ffava_initialize (obj)
      implicit none
      type(ffava_struct),intent(inout) :: obj       ! arguments

      real :: end_time

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      end_time = obj%tstrt + (obj%ndpt - 1) * obj%dt

      obj%opt_wf = 'CALC'
      obj%opt_ff = 1
      obj%hdr_in = 8
      obj%opt_wf_out = 'NONE'
      obj%hdr_wf = 6
      obj%win_len = 1.0
      obj%tim_beg = obj%tstrt
      obj%tim_end = end_time
      obj%wf_init = -1.5
      obj%wf_inc = 0.1
      obj%wf_tot = 16
      obj%thresh_ff = 5.0
      obj%class2_min = -6.0
      obj%class2_max = 4.0

      obj%times_cnt = 0
      obj%tim_beg_idx = 0
      obj%tim_end_idx = 0
      obj%num_in_sect = 0
      obj%num_sect = 0
      obj%ioutsect = 0
      obj%iouttot = 0
      obj%win_len_cnt = 0
      obj%isaved = .false.
      obj%nwtsum = 0
      obj%hsval = 0
      obj%print_lun = pc_get_lun()

      call ffava_update (obj)
      return
      end subroutine ffava_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine ffava_update (obj)
      implicit none
      type(ffava_struct),intent(inout),target :: obj             ! arguments

      real    :: end_time
      integer :: numtr, ier, times_cnt2, tim_beg_end_cnt
      integer :: weights_do, state, nscratch, nstore
      logical :: verify, gathered

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("times_arrayset", (/  &
                                    "times  ",            &
                                    "weights" /))

      call pc_get_global('numtr'   , numtr)
      call pc_get_global('gathered', gathered)
      call pc_get_global('ndpt'    , obj%ndpt)
      call pc_get_global('nwih'    , obj%nwih)
      call pc_get_global('dt'      , obj%dt)
      call pc_get_global('tstrt'   , obj%tstrt)

      end_time = obj%tstrt + (obj%ndpt - 1) * obj%dt

      call pc_get ('opt_wf', obj%opt_wf)
      call string_to_upper (obj%opt_wf)

      call pc_get ('opt_ff', obj%opt_ff)
      call pc_get ('hdr_in', obj%hdr_in)
      call pc_get ('opt_wf_out', obj%opt_wf_out)
      call string_to_upper (obj%opt_wf_out)

      call pc_get ('hdr_wf', obj%hdr_wf)
      call pc_get ('win_len', obj%win_len)
      call pc_get ('tim_beg', obj%tim_beg)
      call pc_get ('tim_end', obj%tim_end)
      call pc_get ('wf_init', obj%wf_init)
      call pc_get ('wf_inc', obj%wf_inc)
      call pc_get ('wf_tot', obj%wf_tot)
      call pc_get ('thresh_ff', obj%thresh_ff)
      call pc_get ('class2_min', obj%class2_min)
      call pc_get ('class2_max', obj%class2_max)
      call pc_alloc('times', obj%times, obj%times_cnt)
      times_cnt2 = obj%times_cnt
      call pc_alloc('weights', obj%weights, times_cnt2)
      if (times_cnt2 /= obj%times_cnt) then
        call pc_error ('TIMES and WEIGHTS arrays have different lengths.')
      end if

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (numtr < 2) then
        call pc_error ('This process requires gathered input. Please insert &
                       &GATHER before FFAVA.')
      else if (.not.gathered) then
        call pc_error("Traces are currently grouped but aren't properly &
                      &gathered. Please insert GATHER before FFAVA.")
      else if (numtr > 2) then
        call pc_error('This process requires 2-trace input gathers. Current &
                      &input global NUMTR =', numtr, 'is too big.')
      end if

      if(all(OPT_WF_OPTIONS /= obj%opt_wf)) then
        call pc_error( &
          'Invalid OPT_WF value. Valid values are CALC, TRIAL and USER')
        obj%opt_wf = 'CALC'
      end if

      if(obj%opt_ff < 1 .or. obj%opt_ff > 4) then
        call pc_error('Invalid OPT_FF value. Only 1, 2, 3 and 4 allowed.')
        obj%opt_ff = 1
      end if

      obj%hdr_in = min (max(obj%hdr_in, 0), obj%nwih)

      if(all(OPT_WF_OUT_OPTIONS /= obj%opt_wf_out)) then
        call pc_error('Invalid OPT_WF_OUT value. Valid values are &
                      &NONE, FIRST and ONLY.')
        obj%opt_wf_out = 'NONE'
      end if

      obj%hdr_wf = min (max(obj%hdr_wf, 1), obj%nwih)

      obj%win_len_cnt = max(min(nint(obj%win_len/obj%dt)+1, obj%ndpt), 3)
      obj%win_len = (obj%win_len_cnt - 1) * obj%dt

      obj%tim_beg_idx = nint((obj%tim_beg - obj%tstrt)/obj%dt) + 1
      obj%tim_beg_idx = min (max(obj%tim_beg_idx, 1), obj%ndpt)
      obj%tim_beg = obj%tstrt + (obj%tim_beg_idx-1) * obj%dt

      obj%tim_end_idx = nint((obj%tim_end - obj%tstrt)/obj%dt) + 1
      obj%tim_end_idx = min(max(obj%tim_end_idx, obj%tim_beg_idx), obj%ndpt)
      obj%tim_end = obj%tstrt + (obj%tim_end_idx-1) * obj%dt

      tim_beg_end_cnt = obj%tim_end_idx - obj%tim_beg_idx + 1

      if (tim_beg_end_cnt <= 10) then
        call pc_info('Check TIM_BEG and TIM_END.')
      else if (tim_beg_end_cnt < obj%win_len_cnt) then
        call pc_error ('WIN_LEN must not exceed (TIM_END - TIM_BEG).')
      end if

      if(obj%wf_tot <= 0) then
        call pc_error('Invalid WF_TOT value. Greater than zero required')
        obj%wf_tot = 16
      end if

      if(obj%thresh_ff < 0.0) then
        call pc_error('Invalid THRESH_FF value. Non-negative required')
        obj%thresh_ff = 5.0
      end if

      if(obj%class2_min >= 0.0) then
        call pc_error('Invalid CLASS2_MIN value. Less than zero required')
        obj%class2_min = -6.0
      end if

      if(obj%class2_max <= 0.0) then
        call pc_error('Invalid CLASS2_MAX value. Greater than zero required')
        obj%class2_max = 4.0
      end if

      if(obj%opt_wf == 'USER' .and. &
        (verify .or. pc_verify_arrayset("times_arrayset"))) then
        if(obj%times_cnt < 1) then
          call pc_error('Invalid WEIGHTS and TIMES.  One or more required')
        else
          if(obj%times(1) < obj%tstrt .or. obj%times(1) > end_time) then
            call pc_error('Invalid TIMES. First must be >= TSTRT and <= end')
          end if
        end if

        do weights_do = 2, obj%times_cnt
          if(obj%times(weights_do) > end_time) then
            call pc_error('Invalid TIMES.  Must be less then end time')
            exit
          else if(obj%times(weights_do) <= obj%times(weights_do - 1)) then
            call pc_error('Invalid TIMES.  Must be monotonically increasing')
            exit
          end if
        end do
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_global('numtr'   ,  1)
      call pc_put_global('gathered', .false.)

      nscratch     = obj%ndpt
      nstore       = 2*obj%nwih + 3*obj%ndpt
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)

      call pc_put_options_field('opt_wf', OPT_WF_OPTIONS, 3)
      call pc_put ('opt_wf', obj%opt_wf)

      call pc_put_options_field('opt_ff', OPT_FF_OPTIONS, 4)
      call pc_put ('opt_ff', obj%opt_ff)

      call pc_put ('hdr_in', obj%hdr_in)
      call pc_put ('win_len', obj%win_len)
      call pc_put ('tim_beg', obj%tim_beg)
      call pc_put ('tim_end', obj%tim_end)

      call pc_put_options_field('opt_wf_out', OPT_WF_OUT_OPTIONS, 3)
      call pc_put ('opt_wf_out', obj%opt_wf_out)

      call pc_put ('wf_init', obj%wf_init)
      call pc_put ('wf_inc', obj%wf_inc)
      call pc_put ('wf_tot', obj%wf_tot)
      call pc_put ('hdr_wf', obj%hdr_wf)
      call pc_put ('thresh_ff', obj%thresh_ff)
      call pc_put ('class2_min', obj%class2_min)
      call pc_put ('class2_max', obj%class2_max)
      call pc_put ('times', obj%times, obj%times_cnt)
      call pc_put ('weights', obj%weights, obj%times_cnt)

      if(obj%opt_wf == 'CALC') then
        call pc_put_sensitive_field_flag('win_len'   , .true.)
        call pc_put_sensitive_field_flag('tim_beg'   , .true.)
        call pc_put_sensitive_field_flag('tim_end'   , .true.)
        call pc_put_sensitive_field_flag('opt_wf_out', .true.)
      else
        call pc_put_sensitive_field_flag('win_len'   , .false.)
        call pc_put_sensitive_field_flag('tim_beg'   , .false.)
        call pc_put_sensitive_field_flag('tim_end'   , .false.)
        call pc_put_sensitive_field_flag('opt_wf_out', .false.)
      end if

      if(obj%opt_wf == 'CALC' .or. obj%opt_wf == 'TRIAL') then
        call pc_put_control('need_request', .true.)
        call pc_put_control('need_label'  , .true.)
        call pc_put_sensitive_field_flag('wf_init',.true.)
        call pc_put_sensitive_field_flag('wf_inc',.true.)
        call pc_put_sensitive_field_flag('wf_tot',.true.)
      else
        call pc_put_control('need_request', .false.)
        call pc_put_control('need_label'  , .false.)
        call pc_put_sensitive_field_flag('wf_init',.false.)
        call pc_put_sensitive_field_flag('wf_inc',.false.)
        call pc_put_sensitive_field_flag('wf_tot',.false.)
      end if

      if(obj%opt_wf == 'TRIAL') then
        call pc_put_sensitive_field_flag('hdr_in',.false.)
        call pc_put_sensitive_field_flag('hdr_wf',.true.)
      else
        call pc_put_sensitive_field_flag('hdr_in',.true.)
        call pc_put_sensitive_field_flag('hdr_wf',.false.)
      end if

      if(obj%opt_wf == 'USER') then
        call pc_put_sensitive_arrayset_flag('times_arrayset',.true.)
      else
        call pc_put_sensitive_arrayset_flag('times_arrayset',.false.)
      end if

      if(obj%opt_ff == 4) then
        call pc_put_sensitive_field_flag('thresh_ff',.true.)
        call pc_put_sensitive_field_flag('class2_min',.true.)
        call pc_put_sensitive_field_flag('class2_max',.true.)
      else
        call pc_put_sensitive_field_flag('thresh_ff',.false.)
        call pc_put_sensitive_field_flag('class2_min',.false.)
        call pc_put_sensitive_field_flag('class2_max',.false.)
      end if

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free(obj%hdsave)
      call mem_free(obj%trsave)
      call mem_free(obj%grad_wght)
      if(associated(obj%tempt)) call temptfile_close(obj%tempt)
      if(associated(obj%binsort)) call binsort_close(obj%binsort)

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if (obj%opt_wf /= 'CALC') then
        obj%opt_wf_out = 'NONE'
        if (obj%opt_wf == 'USER') obj%wf_tot = 1
      end if
      obj%num_sect = 0
      obj%isaved = .false.

      call mem_alloc(obj%hdsave   , obj%nwih, 2)
      call mem_alloc(obj%trsave   , obj%ndpt, 2)
      call mem_alloc(obj%grad_wght, obj%ndpt)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      if(obj%opt_wf == 'USER') then
         CALL interp_1d_var_lin_real (obj%times, &
           obj%weights, obj%times_cnt, &
           obj%trsave(:,1), obj%grad_wght, &
           obj%ndpt, obj%tstrt, end_time)
      end if

      if(obj%opt_wf == 'CALC') then
        call temptfile_open(obj%tempt, 'ffava_tempt', obj%nwih, obj%ndpt, &
          0, ier)   ! suppress temptfile printing -- too many rewinds !
      else if(obj%opt_wf == 'TRIAL') then
        call binsort_open(obj%binsort, obj%wf_tot, 'ffava_binsort', &
          obj%nwih, obj%ndpt, obj%print_lun, ier)
      end if

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine ffava_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine ffava (obj, ntr, hd, tr)
      implicit none
      type(ffava_struct),intent(inout) :: obj                   ! arguments
      integer           ,intent(inout) :: ntr                   ! arguments
      double precision  ,intent(inout) :: hd(:,:)               ! arguments
      real              ,intent(inout) :: tr(:,:)               ! arguments

      integer :: win_len_gap, win_len_gap2
      integer :: ISTART, ISTOP, IWCENTLST, IWTOP, IWCENT, IWTRY
      integer :: num_in_buf, i, IWMIN, j, jmax, ier, itop(2), ibot(2)
      real    :: WTRY, VTEST, WGHTVAL, DENOM, DELTA, WGHTLST, WFACT
      real    :: trtemp(obj%ndpt)
      real    :: trial_buf(obj%wf_tot)

      if(obj%opt_wf == 'CALC') then
        ! Note: temptfile is used when opt_wf == 'CALC'
        IF (ntr == 2) then
          IF (obj%num_sect == 0) GO TO 100  !(start accumulating new section)
          IF (obj%hdr_in > 0) THEN
            IF (nint(hd(obj%hdr_in,1)) /= obj%hsval) THEN
              obj%hdsave = hd(:obj%nwih,:2)
              obj%trsave = tr(:obj%ndpt,:2)
              obj%isaved = .true.
              GO TO 300                     !(start outputting a section)
            ENDIF
          ENDIF
          GO TO 200                         !(add traces to section)
        ELSE IF (ntr == NO_MORE_TRACES) then
          obj%isaved = .false.
          GO TO 300                         !(start outputting a section)
        ELSE IF (ntr == NEED_TRACES) then
          IF (obj%ioutsect < obj%num_in_sect) then
            goto 400                             !(output next trace)
          ELSE IF (obj%isaved) then
            call temptfile_rewind(obj%tempt)
            hd(:obj%nwih,:2) = obj%hdsave
            tr(:obj%ndpt,:2) = obj%trsave
            obj%isaved = .false.
            ntr = 2
            GO TO 100                      !(start accumulating new section)
          ELSE
            ntr = NO_MORE_TRACES
            goto 5000
          ENDIF
        ELSE
          call pc_error('FFAVA: Received illegal number of traces =', ntr)
          ntr = FATAL_ERROR
          goto 5000
        ENDIF
        return
      end if

      ! assert(opt_wf=='TRIAL' .or. opt_wf=='USER')

      IF (ntr == 2) then
        IF (obj%num_sect == 0) GO TO 100  !(start accumulating new section)
        IF (obj%hdr_in > 0) THEN
          IF (nint(hd(obj%hdr_in,1)) /= obj%hsval) GOTO 100  !(start acum new)
        END IF
        GO TO 200                         !(add traces to section)
      ELSE IF (ntr == NO_MORE_TRACES) then
        if(obj%opt_wf == 'TRIAL') then
          ! When binsort is called with ntr=NO_MORE_TRACES,
          ! it will return with ntr=1 to output the first trace.
          call binsort(obj%binsort, ntr, hd, tr)
          if (ntr==NO_MORE_TRACES .or. ntr==FATAL_ERROR) goto 5000
        else  ! if(obj%opt_wf == 'USER') then
          goto 5000
        end if
      ELSE IF (ntr == NEED_TRACES) then
        if(obj%opt_wf == 'TRIAL') then
          ! When binsort is called with ntr=NEED_TRACES
          ! it will return with ntr=1 to output the next trace
          ! or ntr=0 (NO_MORE_TRACES) if they are all gone.
          call binsort(obj%binsort, ntr, hd, tr)
          if (ntr==NO_MORE_TRACES .or. ntr==FATAL_ERROR) goto 5000
        else  ! if(obj%opt_wf == 'USER') then
          call pc_error('FFAVA Fatal Error: Received NTR = NEED_TRACES &
                        &while OPT_WF = USER.')
          ntr = FATAL_ERROR
          goto 5000
        end if
      ELSE
        call pc_error('FFAVA: Received illegal number of traces =', ntr)
        ntr = FATAL_ERROR
        goto 5000
      END IF
      return
!
!**** Start accumulating a new section
  100 CONTINUE
      ! opt_wf may be anything ('CALC', 'TRIAL' or 'USER')
      obj%num_sect = obj%num_sect + 1
      IF (obj%hdr_in > 0) obj%hsval = nint(hd(obj%hdr_in,1))
      obj%num_in_sect = 0
      IF (obj%opt_wf == 'CALC') THEN
        obj%nwtsum = 0
        obj%grad_wght = 0.0
      ELSE
        obj%ioutsect = 0
      ENDIF
!
!**** Add traces to section
  200 CONTINUE
      ! opt_wf may be anything ('CALC', 'TRIAL' or 'USER')
      obj%num_in_sect = obj%num_in_sect + 1
!**** Find usable portions of input traces
      do i = 1, 2
        call mutehw (hd(:,i), tr(:,i), obj%ndpt, 0.0, MUTEHW_SET)
        call ffava_first_last (tr(:obj%ndpt,i), obj%ndpt, itop(i), ibot(i))
        itop(i) = max (itop(i), nint(hd(HDR_TOP_MUTE,i)))
        ibot(i) = min (ibot(i), nint(hd(HDR_BOTTOM_MUTE,i)))
      end do
      itop(1) = max (itop(1), itop(2))
      ibot(1) = min (ibot(1), ibot(2))
      do i = 1, 2
        hd(HDR_TOP_MUTE,i)    = itop(1)
        hd(HDR_BOTTOM_MUTE,i) = ibot(1)
        call mutehw (hd(:,i), tr(:,i), obj%ndpt, 0.0, MUTEHW_BOTH)
      end do
!**** Jump to 400 if OPT_WF /= CALC
      IF (obj%opt_wf /= 'CALC') goto 400
!**** Will now find contribution of input traces to weight function
      ISTART = max (itop(1), obj%tim_beg_idx)
      ISTOP  = min (ibot(1), obj%tim_end_idx)
      win_len_gap = MIN (obj%win_len_cnt - 1, ISTOP - ISTART)
      IF (2*win_len_gap < obj%win_len_cnt) goto 275  ! Inadequate trace length
      win_len_gap2 = win_len_gap / 2
      IWCENTLST = 0
      IWTOP = ISTART
!**** Begin loop over windows for median weight-factor determination
  225 CONTINUE
      IWTOP = MIN (IWTOP, ISTOP - win_len_gap)
!     print*,'Window between times ',tstrt+(iwtop-1)*dt,'  and ', &
!             tstrt+(iwtop+win_len_gap-1)*dt
      IWCENT = IWTOP + win_len_gap2
      DO IWTRY = 1, obj%wf_tot
        WTRY = obj%wf_init + (IWTRY - 1)*obj%wf_inc
        num_in_buf = 0
        DO i = IWTOP, IWTOP + win_len_gap
          if (tr(i,1)==0.0 .or. tr(i,2)==0.0) cycle
          num_in_buf = num_in_buf + 1
          VTEST = ABS (tr(i,2) - WTRY*tr(i,1))
          trtemp(num_in_buf) = VTEST
        END DO
        IF (num_in_buf <= win_len_gap2) GO TO 250
        CALL MEDIAN (trtemp(:num_in_buf), num_in_buf, trial_buf(iwtry))
!       print '(1x,f6.2,i6,2x,e14.6)', wtry, num_in_buf, trial_buf(iwtry)
      END DO
      IWMIN = mth_ismin (obj%wf_tot, trial_buf, 1)
      IF (IWMIN==1 .OR. IWMIN==obj%wf_tot) THEN
        WGHTVAL = obj%wf_init + (IWMIN - 1)*obj%wf_inc
      ELSE
        DENOM = trial_buf(iwmin-1) + trial_buf(iwmin+1) - 2.0*trial_buf(iwmin)
        IF (DENOM /= 0.0) THEN
          DELTA = 0.5*(trial_buf(iwmin-1) - trial_buf(iwmin+1)) / denom
          WGHTVAL = obj%wf_init + (IWMIN - 1 + DELTA)*obj%wf_inc
        ELSE
          WGHTVAL = obj%wf_init + (IWMIN - 1)*obj%wf_inc
        ENDIF
      ENDIF
!     print*,'Weight for this window: ',wghtval
      IF (IWCENTLST == 0) WGHTLST = WGHTVAL
      DELTA = (WGHTVAL - WGHTLST)/(IWCENT - IWCENTLST)
      do i = IWCENTLST+1, IWCENT
        obj%grad_wght(i) = obj%grad_wght(i) &
                           + WGHTLST + (I-IWCENTLST)*DELTA
      end do
      IWCENTLST = IWCENT
      WGHTLST = WGHTVAL
  250 CONTINUE
      IWTOP = IWTOP + win_len_gap + 1
      IF (IWTOP <= ISTOP) GO TO 225
      IF (IWCENTLST /= 0) THEN
        do i = IWCENTLST+1, obj%ndpt
          obj%grad_wght(i) = obj%grad_wght(i) + WGHTLST
        end do
        obj%nwtsum = obj%nwtsum + 1
      ENDIF
!**** Contribution to weight-factor function is now determined.
!**** Add this pair of input traces to tempt file:
  275 CONTINUE
      do i = 1, 2
        call temptfile_write (obj%tempt, 0, hd(:,i), tr(:,i), ier)
        if (ier /= 0) then
          ntr = FATAL_ERROR
          goto 5000
        end if
      end do
      ntr = NEED_TRACES
      return                                ! Go back for more traces
!
!**** Start outputting a section
  300 CONTINUE
      ! assert(opt_wf == 'CALC')
      IF (obj%num_in_sect == 0) THEN
        ntr = NO_MORE_TRACES
        goto 5000
      ENDIF
      call temptfile_rewind(obj%tempt)
      obj%ioutsect = 0
!**** Scale the weight-factor function
      IF (obj%nwtsum > 0) THEN
        WFACT = 1.0 / obj%nwtsum
        obj%grad_wght = obj%grad_wght * wfact
      ELSE
        write(obj%print_lun, 3000) obj%num_sect, obj%hdr_in, obj%hsval
 3000   FORMAT(' =>FFAVA:  Cannot compute weight factors for section', &
                           I5, 5X, 'HD(', I3, ') =', I10, /,           &
               ' =>FFAVA:  Inadequate live samples in this section')
      ENDIF
!**** Output the weight-factor trace if requested
      if (obj%opt_wf_out /= 'NONE') then
        obj%iouttot = obj%iouttot + 1
        tr(:obj%ndpt,1) = obj%grad_wght
        hd(:obj%nwih,1) = 0.0
        if (obj%hdr_in > 0) hd(obj%hdr_in,1) = obj%hsval
        hd(HDR_SEQUENCE,1) = obj%iouttot
        if (obj%nwtsum > 0) then
          hd(HDR_TOP_MUTE,1) = 1
        else
          hd(HDR_TOP_MUTE,1) = obj%ndpt
        end if
        hd(HDR_CURRENT_GROUP,1)   = obj%num_sect
        hd(HDR_CURRENT_CHANNEL,1) = 1
        hd(HDR_BOTTOM_MUTE,1)     = obj%ndpt
        call lav_set_hdr (hd(:,1), tr(:,1), obj%ndpt)
        ntr = 1
        if (obj%opt_wf_out == 'ONLY') obj%ioutsect = obj%num_in_sect
        return
      end if
!
!**** Output next trace to processes below
  400 CONTINUE
      if (obj%opt_wf == 'CALC') then
        do i = 1, 2
          call temptfile_read (obj%tempt, 0, hd(:,i), tr(:,i), ier)
          if (ier /= 0) then
            ntr = FATAL_ERROR
            goto 5000
          end if
        end do
      end if
      trtemp = tr(:obj%ndpt,1)
      obj%ioutsect = obj%ioutsect + 1
      if (obj%opt_wf == 'TRIAL') then
        jmax = obj%wf_tot
      else
        hd(HDR_CURRENT_GROUP,1) = obj%num_sect
        if(obj%opt_wf_out == 'NONE') then
          hd(HDR_CURRENT_CHANNEL,1) = obj%ioutsect
        else if(obj%opt_wf_out == 'FIRST') then
          hd(HDR_CURRENT_CHANNEL,1) = obj%ioutsect + 1
        end if
        jmax = 1
      end if
      do j = 1, jmax
        if (obj%opt_wf == 'TRIAL') then
          WTRY = obj%wf_init + (j-1)*obj%wf_inc
          obj%grad_wght    = WTRY
          hd(HDR_PANEL,1)  = j
          hd(obj%hdr_wf,1) = WTRY
        end if
        SELECT CASE (obj%opt_ff)
        CASE (1)
          tr(:obj%ndpt,1) = tr(:obj%ndpt,2) - obj%grad_wght * trtemp
        CASE (2)
          tr(:obj%ndpt,1) = &
            (tr(:obj%ndpt,2) - obj%grad_wght * trtemp) * &
            (tr(:obj%ndpt,2) * (1.+ obj%grad_wght) &
              + trtemp * (2.+ obj%grad_wght)) / &
            (1. + (1. + obj%grad_wght)**2)
        CASE (3)
          tr(:obj%ndpt,1) = &
            (tr(:obj%ndpt,2) - obj%grad_wght * trtemp) * &
            (trtemp + obj%grad_wght * tr(:obj%ndpt,2)) / &
            (1. + obj%grad_wght **2)
        CASE (4)
          do i = 1, obj%ndpt
            tr(i,1) = ffava_tone (trtemp(i), tr(i,2), obj%grad_wght(i), &
                           obj%thresh_ff, obj%class2_min, obj%class2_max)
          end do
        END SELECT
        obj%iouttot = obj%iouttot + 1
        hd(HDR_SEQUENCE,1) = obj%iouttot
        call lav_set_hdr (hd(:,1), tr(:,1), obj%ndpt)
        ntr = 1
        if(obj%opt_wf == 'TRIAL') then
          ! When binsort is called with ntr>0,
          ! it will return with ntr=NEED_TRACES
          call binsort(obj%binsort, ntr, hd, tr)
        end if
      end do
      return

 5000 call ffava_wrapup (obj)
      return
      end subroutine ffava

!!------------------------- ffava_tone -------------------------------------!!
!!------------------------- ffava_tone -------------------------------------!!
!!------------------------- ffava_tone -------------------------------------!!

      function ffava_tone(A, B, W, thresh_ff, class2_min, class2_max) result(C)
      implicit none
      real, intent(in) :: A, B, W, thresh_ff, class2_min, class2_max  ! args
      real             :: C                                           ! result

      real, parameter  :: vzero = 0.001                               ! local
      real             :: ffa                                         ! local

      if (abs(A)<vzero .and. abs(B)<vzero) then
        C = 0.
        return
      end if
      ffa = B - W*A
      if (abs(ffa) < thresh_ff) then
        C = sign (0.5, ffa)
      else if (ffa < 0.) then
        IF (A < class2_min) THEN
          if (B > 0.0) then
            C = -4.
          else
            C = -3.
          end if
        ELSE IF (A > class2_max) THEN
          C = -1.
        ELSE
          C = -2.
        END IF
      else                                  !if (ffa > 0.) then
        IF (A < (-class2_max)) THEN
          C = 1.
        ELSE IF (A > (-class2_min)) THEN
          if (B < 0.0) then
            C = 4.
          else
            C = 3.
          end if
        ELSE
          C = 2.
        END IF
      end if
      return
      end function ffava_tone

!!--------------------------- ffava_first_last -----------------------------!!
!!--------------------------- ffava_first_last -----------------------------!!
!!--------------------------- ffava_first_last -----------------------------!!
!
!  Locate first and last non-zero elements of a 1-D real array
!  (ifirst and ilast both returned as n+1 if whole array is dead)
!
      subroutine ffava_first_last (x, n, ifirst, ilast)
      implicit none
      integer , intent(in)  :: n                ! Arguments
      real    , intent(in)  :: x(n)             ! Arguments
      integer , intent(out) :: ifirst, ilast    ! Arguments
!
! Note: this initialization (ifirst = 0) shouldn't be necessary, but new
!       absoft compiler seems to require it!
!
      ifirst = 0
      do ifirst = 1, n
        if (x(ifirst) /= 0.0) exit
      end do
!
      if (ifirst > n) then
        ilast  = ifirst
        return
      end if
!
      do ilast = n, ifirst+1, -1
        if (x(ilast) /= 0.0) exit
      end do
!
      return
      end subroutine ffava_first_last

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine ffava_wrapup (obj)
      implicit none
      type(ffava_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine ffava_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module ffava_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
