!<CPS_v1 type="PROCESS"/>
!!------------------------------ avagrad.f90 ---------------------------------!!
!!------------------------------ avagrad.f90 ---------------------------------!!
!!------------------------------ avagrad.f90 ---------------------------------!!
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
! Name       : AVAGRAD   (AVA GRADient products)   [formerly ASTK and AVATUNE]
! Category   : diagnostics
! Written    : 1990-06-28   by: Bill Harlan
! Revised    : 2006-10-31   by: Bob Baumel
! Maturity   : production
! Purpose    : Generate various AVA gradient related products.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! AVAGRAD produces various AVA gradient related output products, including
! Offset-Dependent Tuning Corrections (ODTC), if desired.  AVAGRAD can be used
! to produce AVA gradient data as a final product, but typically it is used to
! provide input data to the FFAVA and PSLINV processes.
!
!      If OPT_INPUT = ANGLE, then the input must be CMP angle gathers with
!      the reflection angle for each trace, in degrees, in header word 6.
!      Normally these angle range gathers will have been produced by AVAST.
!
!      If OPT_INPUT = OFFSET, then the input must be ordinary NMO-corrected
!      CMP gathers with trace offset in header word 6.
!
!      If OPT_INPUT = A_AND_B, then the input must consist of two-trace
!      Intercept-Gradient gathers (where Intercept is the first trace of each
!      input gather). Usually, this is for the purpose of applying Wavelet-
!      Dependent Offset Dependent Tuning Correction (ODTC), either because
!      the A_AND_B pairs were produced previously by AVAGRAD without ODTC, or
!      because they were obtained from a contractor.
!
!
! Intercept-Gradient Calculation
!
! If OPT_INPUT = ANGLE, then AVAGRAD fits each input CMP with flat events
! whose amplitudes vary with angle according to:
!
!             data(angle,t0) = A(t0) + B(t0)*(sin(angle)**2),
!
! where t0 is the zero-offset travel time, A is the intercept and B is the
! gradient.  (This use of A and B is consistent with Castagna's notation.)
!
!
! If OPT_INPUT = OFFSET, AVAGRAD fits each input CMP with flat events given by:
!
!               data(x,t0) = A(t0) + B(t0)*(sin(angle)**2),
!
! where x is offset, t0 is the zero-offset travel time, A is the intercept and
! B is the gradient.  The sine of the reflection angle is approximated by:
!
!                   sin(angle) = (x*VI)/(t*(VR**2)),
!
! where VR = stacking (RMS) velocity, VI = interval velocity (related to VR
! by the Dix equation) and t = reflection time derived from the NMO equation:
!
!                  t = sqrt( t0**2 + (x**2)/(VR**2) ).
!
! Normally, an interval velocity derived from the RMS velocity by the Dix
! equation is a jagged function of time and should be smoothed for use in
! converting offset to angle.  LEN_SMOOTH is the length of a running average
! smoothing operator, in seconds, that is applied to the interval velocity.
! Typical values of LEN_SMOOTH are from 0.1 s to 0.5 s.
!
!
! Straight Ray Offset-Angle Conversion
!
! Prior to September 1995, ASTK (the precursor to AVAGRAD) used an inaccurate
! offset-angle conversion based on a straight ray approximation, equivalent to
! the approximation described earlier with the further simplification that
! VI = VR; thus, the assumed relation between offset and angle was:
!
!                         sin(angle) = x/(t*VR),
!
! which uses only the RMS velocity and makes no use of interval velocity.
! For reflections at depth, this equation may underestimate reflection angle
! by around a factor of two.  Setting OPT_SR = YES causes AVAGRAD to use this
! inaccurate method.  This option is provided for historical interest or for
! comparison with contractor-processed data since some contractors may still
! compute AVA gradients this way.  Normally, OPT_SR should be set to NO.
!
!
! Gradient Related Products Available
!
! If OPT_INPUT = ANGLE or OFFSET, AVAGRAD first fits the input CMP as described
! above and extracts an intercept trace and a gradient trace, including Offset
! Dependent Tuning Correction (ODTC) if desired.  AVAGRAD then calculates the
! desired gradient related product based on these intercept and gradient traces
! (or may optionally produce MODEL or RESID output to indicate the quality of
! data fitting).
!
! If OPT_INPUT = A_AND_B, then AVAGRAD may optionally apply wavelet-dependent
! ODTC, and will output only those products that can be calculated from the
! intercept and gradient traces (Thus, the MODEL and RESID outputs are not
! available).
!
!     If OPT_OUTPUT = A_AND_B, the intercept and gradient traces are output
!     in two-trace gathers (thus, 2 output traces per input gather). Here, the
!     intercept trace will be the first trace in each two-trace output gather.
!
!     If OPT_OUTPUT = P_REFL, a P-wave reflectivity trace at angle ANG_REF
!     is output.  (1 trace per input gather.)  A(t0) + B(t0)*(sin(ANGREF)**2).
!     When ANGREF = 0, this shows perturbations in log(P-Impedance). For
!     non-zero ANGREF, it represents perturbations in the logarithm of an
!     "Elastic Impedance" (Patrick Connolly, Leading Edge, April 1999).
!
!     If OPT_OUTPUT = S_REFL, an estimated S-wave reflectivity trace at normal
!     incidence is output.  (1 trace per input gather.)  0.5*( A(t0) - B(t0)).
!
!     If OPT_OUTPUT = VS_VP, an estimated VS/VP ratio trace is output. (One
!     trace per input gather.)  -0.5*( A(t0) + B(t0)).  Note that this has
!     the opposite polarity of a Poisson's ratio stack.
!
!     If OPT_OUTPUT = AB_PROD, the product of the intercept times the gradient
!     traces is output.  (1 trace per input gather.)  This highlights bright
!     spots with both large amplitudes and large AVA changes (Rutherford &
!     Williams Class 3).
!
!     If OPT_OUTPUT = MODEL, AVAGRAD models the input gathers (as many traces
!     per gather as in the input traces) according to the calculated A(t0)
!     and B(t0) traces, including ODTC if specified.  Typically used for QC.
!     Not available when OPT_INPUT = A_AND_B since, in this case, it would be
!     identical to OPT_OUTPUT = A_AND_B.
!
!     If OPT_PROD = RESID, the input data minus the best-fit model is output.
!     (As many traces output as in the input gather.)  Typically used for QC.
!     Not available when OPT_INPUT = A_AND_B or when the input gathers contain
!     only two traces, since in these cases, the residual would be exactly 0.
!
!
! Offset Dependent Tuning Correction (ODTC)
!
! AVAGRAD can apply two different kinds of ODTC: High-Order AVA Correction,
! and Wavelet-Dependent Stretch Correction:
!
!   High-Order AVA Correction (First kind of ODTC)
!
!   If NP_FIT = 2 (the default), then AVAGRAD performs a 2-parameter fit of
!   the AVA trend as described above.
!
!   If NP_FIT = 3, then AVAGRAD performs a 3-parameter fit of the AVA trend to
!   include a higher order AVA term.  This is the first part of an algorithm
!   for correcting Offset-Dependent Tuning (the NMO stretch effect), based on
!   work of H.W. Swan (1997 SEG, paper AVO 2.5).  In this case AVAGRAD fits
!   the input CMPs using the equation:
!
!             data  =  A + B*(sin**2) + C*(sin**2)*(tan**2)
!
!   but then discards the C term.  The idea is that NMO stretch produces a
!   spurious C term which is not indicative of rock properties, but decreases
!   the accuracy of calculated A and B if you do only 2-parameter fitting.
!   Therefore, NP_FIT = 3 is recommended if you have adequate data quality,
!   including at least 3 offsets or angles.
!
!   The NP_FIT = 3 option is available only when OPT_INPUT = ANGLE or OFFSET
!   with at least 3 traces per input gather.
!
!   If OPT_OUTPUT = MODEL, output traces are modeled using only the A and B
!   terms, even if a C term was computed when fitting the data.
!
!   If OPT_OUTPUT = RESID, output residual traces are based on the complete
!   data fitting, including any C term if it was computed.
!
!
!   Wavelet-Dependent Stretch Correction (Second kind of ODTC)
!
!   If PATH_WVLT is specified, a correction is made for wavelet-dependent
!   contamination of the gradient term by the intercept.  This correction is
!   the second part of the algorithm based on the work of H.W. Swan, cited
!   above. The correction is made using the following algorithm:
!
!   At setup time, AVAGRAD derives a transfer function f(t) designed to
!   transform the estimated wavelet w(t) [read from disk] into the modified
!   wavelet:
!                         w1(t) = 0.5 * t * w'(t),
!
!   (Here, * denotes ordinary multiplication).  At processing time, each input
!   gradient trace B(t) is replaced by a corrected gradient "Bcorr" where
!
!                       Bcorr(t) = B(t) + f(t)*A(t),
!
!   (Here, * denotes convolution), where A(t) is the matching intercept trace.
!   The intercept traces A(t) are not altered.  This routine operates entirely
!   in the time domain. The wavelet derivative w'(t) is calculated as a
!   centered finite-difference.  Calculation of the transfer function f(t) is
!   the same as in MTFUN.
!
!   Note: Because the equation expressing w1(t) in terms of w(t) includes
!   multiplication by time, it is important to know the zero time (centering
!   time) of your wavelet accurately. You must supply this information in the
!   TIM_FIRST parameter.
!
!   The Wavelet-Dependent stretch correction, if specified, is applied after
!   initial A(t) and B(t) estimates are derived by data fitting.  If you
!   specify both High Order AVA correction (NP_FIT = 3) and Wavelet-Dependent
!   correction, then the High Order AVA correction is done first (as part of
!   the data fitting); then the Wavelet-Dependent correction is applied.
!   If OPT_INPUT = A_AND_B, then no initial data fitting is done, but you may
!   still apply the Wavelet-Dependent correction.
!
!   If OPT_OUTPUT = MODEL, then results of Wavelet-Dependent correction, if
!   specified, are included in the output traces.  You may use this option to
!   see how well the Wavelet-Dependent correction removes NMO stretch.
!
!   If OPT_OUTPUT = RESID, Wavelet-Dependent correction is not available, as
!   the output traces show only the residual after initial data fitting.
!
!
! Velocity Handling
!
! Manipulation of the velocities needed for converting offset to angle when
! OPT_INPUT = OFFSET depends on settings of OPT_AVE and OPT_SR and is designed
! to follow the treatment in process AVAST as closely as possible. As in AVAST,
! you may input Stacking or RMS velocities (type VTNM or VTRM) or Interval
! velocities (type VTIN).
!
! If OPT_AVE = YES, velocity functions are combined to arrive at a single
! interval velocity function: If input velocities are of type VTNM or VTRM,
! they are first resampled to the trace sample interval, then averaged, then
! converted to interval velocity using Dix's formula. If they are of type
! VTIN, they are resampled to the trace sample interval (honoring the constant
! interval velocity "steps"), then averaged. In either case, the composited
! interval velocity is then smoothed using the LEN_SMOOTH parameter. This
! smoothed interval velocity is also converted to RMS velocity, because both
! interval and RMS velocities are needed in our approximate offset-to-angle
! conversion formula.
!
! If OPT_AVE = NO, a smoothed interval velocity and corresponding RMS velocity
! is derived separately at every velocity location and saved in a table. When
! processing traces, interval and RMS velocities are interpolated separately
! from the stored table to the current CMP location.
!
! The intent of the OPT_AVE = NO option is to be able to accommodate a SLOWLY
! varying water depth.  It is up to you to put in functions which have a smooth
! lateral variation. It is NOT recommended that the option be used with raw
! stacking velocites.
!
! If OPT_SR = YES, velocity handling is as described above, except that no
! smoothing is done, and only the RMS velocity is used (You may still input
! interval velocities, if you wish, even when OPT_SR = YES).
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Input Data Preparation
!
! If OPT_INPUT = OFFSET, input data should have an amplitude balance applied
! prior to this process. This should be a deterministic balance such as TPOW or
! GDIV, but MVXP may be satisfactory in some cases.  The input data must be
! NMO-corrected CMP gathers, preferably with multiples removed and residual NMO
! corrected (e.g., with RFAB).  Data should be muted (head and tail) to remove
! noise or any data that might be misinterpreted by AVAGRAD.  Use a zero length
! taper in MUTE to avoid tapers that AVAGRAD might interpret as AVA gradients.
!
! If OPT_INPUT = OFFSET, header word 6 for each input trace should be set to 
! the center value of the offset range that trace represents. 
!
! If OPT_INPUT = ANGLE, input data will normally be angle gathers produced by
! AVAST, although it might also be output from a migration process designed
! specifically to produce angle gathers.  This input data should be prepared
! the same as for ordinary CMP gathers, but some of the input data preparation
! may be applied before the AVAST process.
!
! If OPT_INPUT = A_AND_B, the input data will be two-trace intercept-gradient
! gathers. Usually, this is for the purpose of applying Wavelet-Dependent
! Offset Dependent Tuning Correction (ODTC), where the input may consist of
! either A_AND_B pairs produced previously by AVAGRAD (without ODTC) or
! A_AND_B pairs produced by a contractor.
!
!
! Choosing Diagonal Load
!
! The diagonal load factor DLD_FIT helps to stabilize data fitting in the
! presence of noise. If you can estimate the signal/noise ratio (S/N) in your
! data (expressed in terms of energies, not amplitudes), then a suitable
! diagonal load may be chosen as:
!
!                     DLD_FIT  =  100 / (S/N)
!
! which is simply the noise/signal ratio (multiplied by 100 because diagonal
! load is expressed as a percentage).
!
! If OPT_INPUT = ANGLE, you can probably use a smaller value of DLD_FIT than
! when OPT_INPUT = OFFSET, as the partial stacking performed in generating
! angle gathers usually improves signal/noise ratio. Similarly, you can
! probably use a smaller DLD_FIT when working with migrated data than with
! unmigrated data.
!
!
! Output Traces
!
! If output traces from AVAGRAD will be inverted using PSLINV, do not apply any
! amplitude balance between AVAGRAD and PSLINV. On the other hand, if output
! traces will be sent to FFAVA, an MVXP is usually applied between AVAGRAD and
! FFAVA.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in gathers.
!
! If OPT_INPUT = ANGLE, input must consist of CMP angle gathers; in this case,
! header word 6 must contain the reflection angle of each trace in degrees.
!
! If OPT_INPUT = OFFSET, input must consist of NMO-corrected CMP gathers in
! offset; in this case, header word 6 must contain the offset of each trace.
!
! If OPT_INPUT = A_AND_B, input must consist of two-trace intercept-gradient
! gathers, where the first trace of each gather is the intercept trace (A),
! and the second trace of each gather is the gradient trace (B).
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs trace gathers.
! This process outputs traces with same gather status as the input traces.
!
! If OPT_OUTPUT = A_AND_B, then each input gather gives rise to a two-trace
! output gather consisting of an intercept trace (A) and a gradient trace (B).
!
! If OPT_OUTPUT = P_REFL, S_REFL, VS_VP, or AB_PROD, then each input gather
! gives rise to a single output trace (i.e., a 1-trace gather).
!
! If OPT_OUTPUT = MODEL or RESID, then each input gather gives rise to an
! output gather of the same size as the input gather.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       must be > 1 on input,
!                                                   generally changed
! GATHERED  whether traces are a legitimate gather  must be .true. on input,
!                                                   remains .true. on output
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#     Description                Action taken
! ----     -----------                ------------
! 1        Sequential Trace Count     Renumbered.
! 2        Head mute index            Reset.
! 4        Trace number in gather     Renumbered.
! 5        Stacking fold              May be reset.
! 6        Offset or angle            Used and may be reset.
! 24       Panel number               May be reset.
! 25       Largest Absolute Value     Reset.
! 64       Tail mute index            Reset.
! varies   X and Y CMP locations      Used for Velocity location when
!                                     OPT_INPUT = OFFSET.
!
! For all options, header words 2 and 64 (top and bottom mutes) and 25 (LAV)
! are reset. When OPT_OUTPUT = MODEL or RESID, these are generally the ONLY
! header words which get changed.
!
! The other options reduce input gathers to 1-trace or 2-trace output gathers
! and reset more header words. Here, most output header words are taken from
! the first live trace of the input gather. Header words 1 and 4 are renumbered
! when changing the number of traces. Header word 5 (fold) is reset to the
! number of traces in the input gather. Header word 6 is reset to 0.0 (or to
! ANG_REF when OPT_OUTPUT = P_REFL). In addition, when OPT_OUTPUT = A_AND_B,
! header word 24 is reset to 1 for the intercept traces and 2 for the gradient
! traces, to facilitate possible sorting to intercept and gradient panels.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                         AVAGRAD REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  8. 2006-10-31  Baumel      Add ability to read velocity functions from a
!                              trcio trace file - required addition of VELTYPE
!                              parameter, used only when velocities are in a
!                              trace file.
!  7. 2006-01-10  B. Menger   Removed Unused Variables.
!  6. 2002-08-26  CI Burch    Doc change only.
!  5. 2002-08-21  Baumel      Fix bug so FOLD_MIN works as advertised; replace
!                             Iterative Reweighted Least Squares (IRLS) method
!                             with simpler, faster, more robust diagonal load
!                             calculation; add file selection boxes to GUI.
!  4. 2001-11-01  Baumel      Add statement to compile in new absoft compiler.
!  3. 2001-04-26  Baumel      Add OPT_INPUT = OFFSET option (and all associated
!                             velocity processing).
!  2. 2001-02-14  Baumel      Renamed NTR_MIN parameter to FOLD_MIN to match
!                             analogous parameter in PSLINV process.
!  1. 2001-01-25  Bob Baumel  Converted ASTK and AVATUNE from old system
!                             (but OPT_INPUT = OFFSET not implemented yet).
!
!                          ASTK REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
! 20. 1998-06-02  Baumel      Add offset-detuning options and IDO=12,
!                             remove CCEXP option completely.
! 19. 1997-02-26  Baumel      Make sure headers 2 & 64 are set to first &
!                             last non-zero value in all output traces;
!                             new Note 6 in doc.
! 18. 1997-02-05  Baumel      Make sure gradient has good header if IDO=11;
!                             Also make sure header 1 is set correctly.
! 17. 1995-11-15  Baumel      Partly reverse changes of 1995-10-06, so
!                             IDO=6 models only the angles used in fitting.
! 16. 1995-10-06  Baumel      Model ALL angles when IDO=6 (including angles
!                             not used in fitting). Also (all IDO) now uses
!                             angle.le.ANGMAX (previously angle.lt.ANGMAX).
! 15. 1995-09-26  Baumel      Improved robustness of fitting by adopting
!                             Iterative Reweighted Least Squares (IRLS)
!                             formulation (new parameter NUMLS).
! 14. 1995-09-18  Baumel      Improved accuracy of offset-angle conversion
!                             when input traces in OFFSET domain & VELFILE
!                             specifies velocity file (new param TSMOOTH).
! 13. 1995-03-08  Baumel      Add NTRMIN parameter to set minimum fold.
! 12. 1995-01-16  Harlan      Insure that ignores hard zeros in mute zone.
! 11. 1994-10-03  Baumel      Add CCEXP parameter for coherence-weighting
!                             of gradients.
! 10. 1994-06-29  Harlan      Ignore bottom mute of 0 (assume not set).
!  9. 1994-04-29  Harlan      Include bottom mute from header word 64.
!  8. 1994-03-03  Harlan      Add option IDO=11.
!  7. 1994-02-17  Harlan      ASTKGSTK blew up at zero-offset, zero-time
!  6. 1993-02-12  Harlan      Fix problem with ANGMAX=90.
!  5. 1993-02-12  Harlan      Fix ASTKCOMP for bottom zero padding.
!  4. 1993-01-11  Harlan      Add VELFILE='ANGLE' to input angles.
!  3. 1992-12-14  Harlan      Remove automatic mute of low fold.
!  2. 1992-12-11  Harlan      Change definition of option IDO=5.
!  1. 1990-06-28  Harlan      Original version
!
!                         AVATUNE REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  2. 1998-06-02  Baumel      Use F90 dynamic scratch & storage, calculate
!                             operator in ASTK_DTUNE, now part of ASTK.
!                             Moved to conlib.
!  1. 1998-01-28  Baumel      Original version.
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
! NSCRATCH       varies     amount of temporary memory needed.
! NSTORE         varies    amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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
!<NS AVAGRAD Process/NC=80>
!           AVA GRADient products (includes former ASTK, AVATUNE)
!
!                            General Parameters
!           `----------------------------------------------------
!             OPT_INPUT=`CCCCCC           OPT_OUTPUT=`CCCCCC
!             NUMTR_IN= `XXXXXXXXX        NUMTR_OUT= `XXXXXXXXX
!             FOLD_MIN= `IIIIIIIII        DLD_FIT=~~~`FFFFFFFFF
!             ANG_MAX=~~`FFFFFFFFF        ANG_REF=~~~`FFFFFFFFF
!           `----------------------------------------------------
!
!               Parameters used only when OPT_INPUT = OFFSET
! `-----------------------------------------------------------------------------
!   Select PATH_VEL[PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [PATH_VEL_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!    VELTYPE=`CCCC      OPT_AVE=`CC     LEN_SMOOTH=`FFFFFFFFF      OPT_SR=`CC
! `-----------------------------------------------------------------------------
!
!          Parameters for Offset Dependent Tuning Correction (ODTC)
! `-----------------------------------------------------------------------------
!                                NP_FIT=`C
!   Select PATH_WVLT[PATH_WVLT]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                    [PATH_WVLT_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!    TIM_FIRST=`FFFFFFFFF        LEN_OP=`FFFFFFFFF         DLD_ODT=`FFFFFFFFF
! `-----------------------------------------------------------------------------
!<PARMS PATH_VEL[/ML=140/XST]>
!<PARMS PATH_VEL_INFO[/ML=140/XST]>
!<PARMS PATH_WVLT[/ML=140/XST]>
!<PARMS PATH_WVLT_INFO[/ML=140/XST]>
!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_INPUT">
!<Tip> Input CMPs in angle or offset or intercept-gradient pairs. </Tip>
! Default = ANGLE
! Allowed = ANGLE     (Input consists of CMP angle gathers.)
! Allowed = OFFSET    (Input consists of ordinary CMP gathers in offset.)
! Allowed = A_AND_B   (Input consists of 2-trace intercept-gradient gathers.)
!
! If OPT_INPUT = ANGLE, header word 6 must contain reflection angle for each
! trace, in degrees.
!
! If OPT_INPUT = OFFSET, header word 6 must contain offset.
!
! If OPT_INPUT = A_AND_B, each 2-trace input gather must consist of:
! first the intercept trace (A), then the gradient trace (B).
!</Help>
!
!<Help KEYWORD="OPT_OUTPUT">
!<Tip> Type of AVA gradient related output product. </Tip>
! Default = A_AND_B
! Allowed = A_AND_B  (Intercept and Gradient.)
! Allowed = P_REFL   (Modeled P-wave reflectivity at angle ANG_REF.)
! Allowed = S_REFL   (Estimated S-wave reflectivity at normal incidence.)
! Allowed = VS_VP    (Estimated VS/VP ratio.)
! Allowed = AB_PROD  (Product of Intercept times Gradient.)
! Allowed = MODEL    (Best-fit model of input traces.  For QC.)
! Allowed = RESID    (Input data minus best-fit model.  For QC.)
!
! OPT_OUTPUT = MODEL is not available when OPT_INPUT = A_AND_B.
! OPT_OUTPUT = RESID is not available when OPT_INPUT = A_AND_B or, more
! generally, whenever the input gathers contain only two traces.
!
! OPT_OUTPUT = A_AND_B produces a 2-trace output gather from each input gather.
!
! OPT_OUTPUT = P_REFL, S_REFL, VS_VP or AB_PROD produces a single output trace
! from each input gather.
!
! OPT_OUTPUT = MODEL or RESID produces output gathers of the same size as the
! input gathers.
!</Help>
!
!<Help KEYWORD="NUMTR_IN" TYPE="DISPLAY_ONLY">
!<Tip> Value of NUMTR global upon input, shown for reference. </Tip>
! AVAGRAD requires gathered input. NUMTR for your input traces must be at
! least 2 for all options, and must be exactly 2 when OPT_INPUT = A_AND_B.
!</Help>
!
!<Help KEYWORD="NUMTR_OUT" TYPE="DISPLAY_ONLY">
!<Tip> Value of NUMTR global upon output, shown for reference. </Tip>
! The value of NUMTR for your output traces depends on OPT_OUTPUT.
! NUMTR_OUT will be 2 when OPT_OUTPUT = A_AND_B.
! NUMTR_OUT will be 1 when OPT_OUTPUT = P_REFL, S_REFL, VS_VP or AB_PROD.
! NUMTR_OUT will be the same as NUMTR_IN when OPT_OUTPUT = MODEL or RESID.
!</Help>
!
!<Help KEYWORD="FOLD_MIN">
!<Tip> Minimum # of live trace samples at any depth to do calculations. </Tip>
! Default = 2
! Allowed = int >= NP_FIT
! At each time/depth level, AVAGRAD will not do any calculations unless the
! input gather contains at least this many live trace samples at that level.
!
! If NP_FIT = 3, then FOLD_MIN must be at least 3.
!
! Active only when OPT_INPUT = ANGLE or OFFSET with 3 or more traces in the
! input gathers.
!</Help>
!
!<Help KEYWORD="DLD_FIT">
!<Tip> Diagonal load, in percent, for fitting data to compute A and B. </Tip>
! Default = 2.0
! Allowed = real >= 0.0
! Given noisy data, the diagonal load factor stabilizes the data fitting in
! computation of intercepts and gradients. Theoretically, if the signal/noise
! ratio in your data is S/N (expressed in terms of energies, not amplitudes),
! the diagonal load should be chosen as:
!
!                      DLD_FIT  =  100 / (S/N)
!
! which is simply the noise/signal ratio (multiplied by 100 because diagonal
! load is expressed as a percentage).
!
! If OPT_INPUT = ANGLE, you can probably use a smaller value of DLD_FIT than
! when OPT_INPUT = OFFSET, as the partial stacking performed in generating
! angle gathers generally improves signal/noise ratio. Similarly, you can
! probably use a smaller DLD_FIT when working with migrated data than with
! unmigrated data.
!
! The DLD_FIT parameter is not active when OPT_INPUT = A_AND_B, as no data
! fitting is performed in that case.
!</Help>
!
!<Help KEYWORD="ANG_MAX">
!<Tip> Maximum reflection angle to consider in input, in degrees. </Tip>
! Default = 45.0
! Allowed = 90.0 > real > 0.0
! All events in your input data regarded as having a reflection angle greater
! than ANG_MAX will be ignored. Here, the reflection angle of an event is
! either read from header word 6 (when OPT_INPUT = ANGLE), or calculated from
! reflection time, velocity and offset (when OPT_INPUT = OFFSET).
!
! ANG_MAX must be strictly less than 90 degrees. This guarantees that AVAGRAD
! will not use "fullstack" traces from AVAST in case such traces were
! accidentally left in the data, as fullstack traces from AVAST are marked by
! having 90.0 in header word 6.
!
! Active only when OPT_INPUT = ANGLE or OFFSET.
!</Help>
!
!<Help KEYWORD="ANG_REF">
!<Tip> Reference angle to use, in degrees, when OPT_OUTPUT = P_REFL. </Tip>
! Default = 0.0
! Allowed = 90.0 >= real >= 0.0
!
! Active only when OPT_OUTPUT = P_REFL.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> Choose PATH_VEL using a file selection dialog box. </Tip>
! Active only when OPT_INPUT = OFFSET.
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> Pathname for a stacking or interval velocity file. </Tip>
! Default = NONE
! Allowed = char
! A velocity file is needed to convert from offset to angle when the input
! consists of CMP gathers in offset.  As in process AVAST, this file may
! contain either stacking velocity (type VTNM or VTRM) or interval velocity
! (type VTIN).
!
! This file can be a VELIO velocity file or a modspec file or a TRCIO trace
! file containing velocity functions.  If this file is a TRCIO file, the
! parameter VELTYPE must be set correctly.
!
! Also, if this file is a TRCIO trace file, X and Y locations must be stored
! within this file in header words 7 and 8 (these numbers are hard-wired into
! the velocity primitives that AVAGRAD calls). Velocity traces in this file
! must be sorted with X and Y in ascending order, X increasing most rapidly,
! and must fill a rectangular grid.
!
! Active only when OPT_INPUT = OFFSET.
!</Help>
!
!<Help KEYWORD="PATH_VEL_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_VEL. </Tip>
!</Help>
!
!<Help KEYWORD="VELTYPE">
!<Tip> Type of velocities on the PATH_VEL file (if is trace file). </Tip>
! Default = VTNM   (RMS velocities versus time)
! Allowed = VTNM   (RMS velocities versus time)
! Allowed = VTRM   (RMS velocities versus time)
! Allowed = VTIN   (interval velocities versus time)
! This parameter needs to be specified only if the PATH_VEL file is a
! TRCIO velocity trace file.  Otherwise this parameter is not used.
!
! Active only when OPT_INPUT = OFFSET.
!</Help>
!
!<Help KEYWORD="OPT_AVE">
!<Tip> Whether to average all velocity functions in the file into one. </Tip>
! Default = YES
! Allowed = YES/NO
! Whether to average all velocity functions in the file to a single velocity
! function.
!
! As in process AVAST, the intent of OPT_AVE = NO is to be able to accommodate
! a SLOWLY varying water depth. It is up to you to put in functions which have
! a smooth lateral variation. It is NOT recommended that the option be used
! with raw stacking velocites.
!
! This parameter is active only when OPT_INPUT = OFFSET.
!</Help>
!
!<Help KEYWORD="LEN_SMOOTH">
!<Tip> Length of running average smoothing for interval velocities. </Tip>
! Default = 0.2
! Allowed = real >= 0.0
! Length, in seconds, of running average smoothing for interval velocity
! functions.  If LEN_SMOOTH = 0.0, then no smoothing is done.
!
! Normally some smoothing is required to avoid jagged velocity contours when
! converting from offset to angle.  Typical values of LEN_SMOOTH are from
! 0.1 s to 0.5 s.
!
! Active only when OPT_INPUT = OFFSET and OPT_SR = NO.
!</Help>
!
!<Help KEYWORD="OPT_SR">
!<Tip> Use the inaccurate Straight Ray method to convert offset to angle. </Tip>
! Default = NO
! Allowed = YES/NO
! The OPT_SR = YES option is included for historical reasons, or to compare
! with contractor processed data, as some contractors may still be using such
! a method for calculating gradients.  If OPT_SR = NO, an improved conversion
! from offset to angle is used.
!
! Active only if OPT_INPUT = OFFSET.
!</Help>
!
!---------ODTC (Offset Dependent Tuning Correction) Related Parameters---------
!
!<Help KEYWORD="NP_FIT">
!<Tip> Number of Parameters To Fit. </Tip>
! Default = 2
! Allowed = 2, 3
!
! When NP_FIT = 2, AVAGRAD fits your data with TWO parameters of the form
!                     data  =  A + B*(sin**2)
! and then outputs A and B. This is standard intercept-gradient analysis.
!
! When NP_FIT = 3, AVAGRAD fits your data with THREE parameters of the form
!             data  =  A + B*(sin**2) + C*(sin**2)*(tan**2)
! but then discards the computed C values and outputs only A and B. This part
! of the ODTC algorithm is based on the notion that NMO stretch produces a
! spurious C term which is not indicative of rock properties, but decreases
! accuracy of calculated A and B if you do only 2-parameter fitting. This
! option should not be attempted, however, unless you have very high-quality
! data.
!
! NP_FIT = 3 is available only when OPT_INPUT = OFFSET or ANGLE with 3 or more
! traces in the input gathers.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_WVLT">
!<Tip> Choose PATH_WVLT using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_WVLT">
!<Tip> Pathname of file containing estimated wavelet for ODT correction. </Tip>
! Default = NONE
! Allowed = char
! If you specify PATH_WVLT, the wavelet in this file is used to derive an
! operator to reduce contamination of the gradient by the intercept due to
! Offset-Dependent Tuning (NMO stretch effect).
!
! This file may be in TRCIO or SEGY format or any other format readable by the
! TRCIO primitives in CPS. Only the first trace in your file will be read.
!
! If PATH_WVLT = NONE, no wavelet-dependent ODT correction will be made for
! contamination of the gradient by the intercept.
!
! Wavelet-dependent ODTC is not available when OPT_OUTPUT = RESID.
!</Help>
!
!<Help KEYWORD="PATH_WVLT_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_WVLT. </Tip>
!</Help>
!
!<Help KEYWORD="TIM_FIRST">
!<Tip> Time of first sample in wavelet, in seconds. </Tip>
! Default = 0.0 (or read from wavelet file)
! Allowed = real
! Time of first sample in wavelet is used for proper centering of the
! wavelet (same as in wavelets used by DSIG, although correctness of the
! centering time is more critical here than it is in DSIG).
!
! As in the case of DSIG, the front end will attempt to read the TMIN value
! from your wavelet file (if that file exists already when building the job
! containing AVAGRAD) and will use this as the default for TIM_FIRST; however,
! you may override this setting manually, if necessary.
!
! If your wavelet is in a SEGY file, the TMIN extracted from the file will
! always be 0.0 (because SEGY format doesn't support non-zero TMIN), so you'll
! have to know the correct TIM_FIRST value independently.  If you extracted
! your wavelet using interactive inversion software you probably determined
! its TIM_FIRST value quite accurately in the extraction process.  If your
! estimated wavelet was calculated by C4WE, the TMIN in your wavelet file is
! only very approximate; in this case, we recommend analyzing it with SPCT to
! determine a more accurate TIM_FIRST before using this wavelet in AVAGRAD.
!
! TIM_FIRST is not active if PATH_WVLT = NONE.
!</Help>
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of operator for wavelet-dependent correction, in seconds. </Tip>
! Default = -
! Allowed = real > 0.0
! LEN_OP should be approximately the same as the wavelet length.
!
! Not active if PATH_WVLT = NONE.
!</Help>
!
!<Help KEYWORD="DLD_ODT">
!<Tip> Diagonal load, in percent, for computing ODTC operator. </Tip>
! Default = 1.0
! Allowed = real >= 0.0
! Diagonal load, in percent, for computing wavelet-dependent correction
! operator.
!
! Not active if PATH_WVLT = NONE.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module avagrad_module
      use pc_module
      use named_constants_module
      use mem_module
      use string_module
      use pathcheck_module
      use pathchoose_module
      use trcio_module
      use lav_module
      use ameq_module
      use fltr_module
      use opfilt_module
      use mutehw_module
      use velio_module
      use interp_module
      use intpvelf_module
      use temptfile_module

      implicit none
      private
      public :: avagrad_create
      public :: avagrad_initialize
      public :: avagrad_update
      public :: avagrad_delete
      public :: avagrad
      public :: avagrad_wrapup

      character(len=100),public,save :: AVAGRAD_IDENT = &
'$Id: avagrad.f90,v 1.8 2006/10/30 14:01:44 Baumel prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: avagrad_struct
       private
       logical                    :: skip_wrapup       ! wrapup flag.

       character(len=7)           :: opt_input         ! process parameters.
       character(len=7)           :: opt_output        ! process parameters.
       integer                    :: fold_min          ! process parameters.
       real                       :: dld_fit           ! process parameters.
       real                       :: ang_max, ang_ref  ! process parameters.
       character(len=FILENAME_LENGTH) :: pathname_vel  ! process parameters.
       character(len=FILENAME_LENGTH) :: path_vel      ! process parameters.
       character(len=4)           :: veltype           ! process parameters.
       logical                    :: opt_ave, opt_sr   ! process parameters.
       real                       :: len_smooth        ! process parameters.
       integer                    :: np_fit            ! process parameters.
       character(len=FILENAME_LENGTH) :: pathname_wvlt ! process parameters.
       character(len=FILENAME_LENGTH) :: path_wvlt     ! process parameters.
       real                       :: tim_first, len_op ! process parameters.
       real                       :: diag_load         ! process parameters.
       real                       :: dld_odt           ! process parameters.

       integer                    :: nwih, ndpt        ! globals.
       real                       :: tstrt, dt         ! globals.
       integer                    :: numtr_in          ! global upon input.
       integer                    :: numtr_out         ! global upon output.

       type(pathchoose_struct),pointer :: pathchoose_vel   ! path_vel dialog.
       type(velio_struct)     ,pointer :: velio            ! for velocities.
       type(temptfile_struct) ,pointer :: temptfile_veltab ! velocity tables.
       type(pathchoose_struct),pointer :: pathchoose_wvlt  ! path_wvlt dlog.
       type(trcio_struct),pointer :: trcio             ! input wavelet file.

       integer                    :: lun, nsmooth      ! dependent variables.
       integer                    :: ntr_total         ! dependent variables.
       logical                    :: wvlt_odtc         ! dependent variables.
       integer                    :: noplen, ishift    ! dependent variables.
       real                       :: sin2max, sin2ref  ! dependent variables.
       real                       :: dld_fit1          ! dependent variables.
       real              ,pointer :: opr(:)            ! dependent variables.
       integer                    :: ndptvel           ! dependent variables.
       integer                    :: trcfirst          ! dependent variables.
       integer                    :: velfirst          ! dependent variables.
       integer                    :: numvels           ! dependent variables.
       logical                    :: disktables        ! dependent variables.
       integer                    :: ix1mem, ix2mem    ! dependent variables.
       integer                    :: nxmem             ! dependent variables.
       integer                    :: iy1mem, iy2mem    ! dependent variables.
       integer                    :: nymem             ! dependent variables.
       integer                    :: nhx, nhy          ! dependent variables.
       integer                    :: nxbins, nybins    ! dependent variables.
       real              ,pointer :: xbins (:)         ! dependent variables.
       real              ,pointer :: ybins (:)         ! dependent variables.
       real                       :: xlast, ylast      ! dependent variables.
       real              ,pointer :: vintlast (:)      ! dependent variables.
       real              ,pointer :: vrmslast (:)      ! dependent variables.
       real              ,pointer :: vintbuf (:,:,:)   ! dependent variable.
       real              ,pointer :: vrmsbuf (:,:,:)   ! dependent variable.

      end type avagrad_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(avagrad_struct),pointer,save :: object       ! needed for traps
      integer             ,parameter    :: stride = 50  ! for table printouts

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine avagrad_create (obj)
      implicit none
      type(avagrad_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%opr)
      nullify (obj%xbins)
      nullify (obj%ybins)
      nullify (obj%vintlast)
      nullify (obj%vrmslast)
      nullify (obj%vintbuf)
      nullify (obj%vrmsbuf)
      nullify (obj%velio)
      nullify (obj%temptfile_veltab)
      nullify (obj%trcio)
      nullify (obj%pathchoose_vel)
      nullify (obj%pathchoose_wvlt)

      call pathchoose_create (obj%pathchoose_vel, 'PATH_VEL', 'vel')
      call pathchoose_create (obj%pathchoose_wvlt, 'PATH_WVLT', 'trc')

      call avagrad_initialize (obj)
      return
      end subroutine avagrad_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine avagrad_delete (obj)
      implicit none
      type(avagrad_struct),pointer :: obj       ! arguments
      integer                      :: ier1      ! local

!<execute_only>
      call avagrad_wrapup (obj)
!</execute_only>

      call mem_free (obj%opr)
      call mem_free (obj%xbins)
      call mem_free (obj%ybins)
      call mem_free (obj%vintlast)
      call mem_free (obj%vrmslast)
      call mem_free (obj%vintbuf)
      call mem_free (obj%vrmsbuf)
      if (associated(obj%velio))   call velio_close     (obj%velio)
      if (associated(obj%temptfile_veltab))  &
                                   call temptfile_close (obj%temptfile_veltab)
      if (associated(obj%trcio))   ier1 = trcio_close   (obj%trcio)
      call pathchoose_delete (obj%pathchoose_vel)
      call pathchoose_delete (obj%pathchoose_wvlt)

      deallocate(obj)
      return
      end subroutine avagrad_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine avagrad_initialize (obj)
      implicit none
      type(avagrad_struct),intent(inout) :: obj       ! arguments

      obj%opt_input  = 'ANGLE'
      obj%opt_output = 'A_AND_B'
      obj%fold_min   = 2
      obj%dld_fit    = 2.0
      obj%ang_max    = 45.0
      obj%ang_ref    = 0.0
      obj%pathname_vel = PATHCHECK_EMPTY
      obj%path_vel     = PATHCHECK_EMPTY
      obj%veltype    = 'VTNM'
      obj%opt_ave    = .true.
      obj%len_smooth = 0.2
      obj%opt_sr     = .false.
      obj%np_fit     = 2
      obj%pathname_wvlt = PATHCHECK_EMPTY
      obj%path_wvlt     = PATHCHECK_EMPTY
      obj%tim_first  = 0.0
      obj%len_op     = FNIL
      obj%diag_load  = -1000.0
      obj%dld_odt    = 1.0

      call avagrad_update (obj)
      return
      end subroutine avagrad_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine avagrad_update (obj)
      implicit none
      type(avagrad_struct),intent(inout),target :: obj           ! arguments

      logical                        :: gathered                 ! local
      character(len=FILENAME_LENGTH) :: wavelet_prev, path_dummy ! local
      character(len=128)             :: message                  ! local
      integer                        :: status, update_state     ! local
      integer                        :: status2, noplen2         ! local
      integer                        :: nstore, nscratch         ! local
      integer                        :: maxpicks                 ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      wavelet_prev = obj%path_wvlt

      if (pathchoose_update(obj%pathchoose_vel, obj%path_vel)) return
      if (pathchoose_update(obj%pathchoose_wvlt, obj%path_wvlt)) return

      obj%lun = pc_get_lun()
      update_state = pc_get_update_state()

      call pc_get_global ('GATHERED', gathered)
      call pc_get_global ('NUMTR'   , obj%numtr_in)
      call pc_get_global ('NWIH'    , obj%nwih)
      call pc_get_global ('NDPT'    , obj%ndpt)
      call pc_get_global ('TSTRT'   , obj%tstrt)
      call pc_get_global ('DT'      , obj%dt)

      call pc_get ('OPT_INPUT'    ,obj%opt_input)
      call pc_get ('OPT_OUTPUT'   ,obj%opt_output)
      call pc_get ('FOLD_MIN'     ,obj%fold_min)
      call pc_get ('DLD_FIT'      ,obj%dld_fit)
      call pc_get ('ANG_MAX'      ,obj%ang_max)
      call pc_get ('ANG_REF'      ,obj%ang_ref)
      call pc_get ('PATHNAME_VEL' ,obj%pathname_vel)
      call pc_get ('PATH_VEL'     ,obj%path_vel)
      call pc_get ('VELTYPE'      ,obj%veltype)
      call pc_get ('OPT_AVE'      ,obj%opt_ave)
      call pc_get ('LEN_SMOOTH'   ,obj%len_smooth)
      call pc_get ('OPT_SR'       ,obj%opt_sr)
      call pc_get ('NP_FIT'       ,obj%np_fit)
      call pc_get ('PATHNAME_WVLT',obj%pathname_wvlt)
      call pc_get ('PATH_WVLT'    ,obj%path_wvlt)
      call pc_get ('TIM_FIRST'    ,obj%tim_first)
      call pc_get ('LEN_OP'       ,obj%len_op)
      call pc_get ('DIAG_LOAD'    ,obj%diag_load)
      call pc_get ('DLD_ODT'      ,obj%dld_odt)

! Backward compatibility substitutions (for altered parameter names)
      if (obj%pathname_vel /= PATHCHECK_EMPTY) then
        obj%path_vel = obj%pathname_vel
        obj%pathname_vel = PATHCHECK_EMPTY
      end if
      if (obj%pathname_wvlt /= PATHCHECK_EMPTY) then
        obj%path_wvlt = obj%pathname_wvlt
        obj%pathname_wvlt = PATHCHECK_EMPTY
      end if
      if (obj%diag_load >= 0.0) then
        obj%dld_odt = obj%diag_load
        obj%diag_load = -1000.0
      end if
! End of backward compatibility substitutions

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! Check OPT_INPUT
      call string_to_upper (obj%opt_input)
      if (obj%opt_input(1:2) == 'AN') then
        obj%opt_input = 'ANGLE'
      else if (obj%opt_input(1:2) == 'OF') then
        obj%opt_input = 'OFFSET'
      else if (obj%opt_input(1:2) == 'A_') then
        obj%opt_input = 'A_AND_B'
      else
        call pc_error ('OPT_INPUT must be ANGLE, OFFSET, or A_AND_B.')
      end if

! Check OPT_OUTPUT
      call string_to_upper (obj%opt_output)
      if (obj%opt_output(1:2) == 'A_') then
        obj%opt_output = 'A_AND_B'
      else if (obj%opt_output(1:2) == 'P_') then
        obj%opt_output = 'P_REFL'
      else if (obj%opt_output(1:2) == 'S_') then
        obj%opt_output = 'S_REFL'
      else if (obj%opt_output(1:2) == 'VS') then
        obj%opt_output = 'VS_VP'
      else if (obj%opt_output(1:2) == 'AB') then
        obj%opt_output = 'AB_PROD'
      else if (obj%opt_output(1:2) == 'MO') then
        obj%opt_output = 'MODEL'
      else if (obj%opt_output(1:2) == 'RE') then
        obj%opt_output = 'RESID'
      else
        call pc_error ('OPT_OUTPUT must be A_AND_B, P_REFL, S_REFL,&
                       & VS_VP, AB_PROD, MODEL, or RESID.')
      end if

! Make sure input is properly gathered
      if (obj%numtr_in < 2) then
        call pc_error ('This process requires gathered input. Please &
                       &insert GATHER before AVAGRAD.')
      else if (.not. gathered) then
        call pc_error ('This process requires gathered input. Input &
                       &traces are currently in groups but not in &
                       &functional gathers. Please insert GATHER &
                       &before AVAGRAD.')
      end if

! Check options based on input NUMTR
      if (obj%numtr_in > 2) then
        if (obj%opt_input == 'A_AND_B') call pc_error ('You chose OPT_INPUT &
            &= A_AND_B but input groups contain', obj%numtr_in, 'traces. &
            &Either change OPT_INPUT or provide only 2-trace input gathers.')
        call pc_put_sensitive_field_flag ('FOLD_MIN', .true.)
        obj%fold_min = min(max(obj%fold_min, 2), obj%numtr_in)
        call pc_put_sensitive_field_flag ('NP_FIT'  , .true.)
        obj%np_fit  = min(max(obj%np_fit, 2), 3)
        if (obj%np_fit > obj%fold_min) then
          obj%fold_min = obj%np_fit
          if (update_state == PC_GUI) call pc_info ('FOLD_MIN increased &
                &to 3 to match NP_FIT.')
        end if
      else
        if (obj%opt_output == 'RESID') call pc_error ('OPT_OUTPUT = RESID &
            &is only available when OPT_INPUT = ANGLE or OFFSET with at &
            &least 3 traces in the input gathers.')
        call pc_put_sensitive_field_flag ('FOLD_MIN', .false.)
        call pc_put_sensitive_field_flag ('NP_FIT'  , .false.)
        if (update_state /= PC_GUI) then
          obj%fold_min = 2
          obj%np_fit   = 2
        end if
      end if

! Check options based on OPT_INPUT
      if (obj%opt_input == 'A_AND_B') then
        if (obj%opt_output == 'MODEL') then
          call pc_info ("OPT_OUTPUT = MODEL isn't available with OPT_INPUT &
                        &= A_AND_B. OPT_OUTPUT has been reset to A_AND_B, &
                        &which ought to be equivalent.")
          obj%opt_output = 'A_AND_B'
        end if
        call pc_put_sensitive_field_flag ('DLD_FIT', .false.)
        call pc_put_sensitive_field_flag ('ANG_MAX', .false.)
        if (update_state /= PC_GUI) then
          obj%dld_fit = 2.0
          obj%ang_max = 45.0
        end if
      else
        call pc_put_sensitive_field_flag ('DLD_FIT', .true.)
        call pc_put_sensitive_field_flag ('ANG_MAX', .true.)
        obj%dld_fit = abs(obj%dld_fit)
        if (obj%ang_max <= 0.0) then
          call pc_error ('ANG_MAX must be greater than 0.')
        else if (obj%ang_max >= 90.0) then
          call pc_error ('ANG_MAX must be less than 90.')
        end if
      end if

! Set output NUMTR global
      if (obj%opt_output == 'A_AND_B') then
        obj%numtr_out = 2
      else if (obj%opt_output=='MODEL' .or. obj%opt_output=='RESID') then
        obj%numtr_out = obj%numtr_in
      else
        obj%numtr_out = 1
      end if

! Set sensitivity for ANG_REF
      if (obj%opt_output == 'P_REFL') then
        call pc_put_sensitive_field_flag ('ANG_REF', .true.)
      else
        call pc_put_sensitive_field_flag ('ANG_REF', .false.)
        if (update_state /= PC_GUI) obj%ang_ref = 0.0
      end if

! Set sensitivity of velocity-related fields
      if (obj%opt_input == 'OFFSET') then
        call pc_put_sensitive_field_flag ('SELECT_PATH_VEL', .true.)
        call pc_put_sensitive_field_flag ('PATH_VEL'       , .true.)
        call pc_put_sensitive_field_flag ('VELTYPE'        , .true.)
        call pc_put_sensitive_field_flag ('OPT_AVE'        , .true.)
        call pc_put_sensitive_field_flag ('OPT_SR'         , .true.)
        call pathcheck ('PATH_VEL', obj%path_vel, 'vel', status=status, &
                         show=PATHCHECK_INFO_INPUT)
        if (status /= PATHCHECK_VALID .and. update_state /= PC_GUI) then
          call pc_error ('PATH_VEL must be specified when OPT_INPUT &
                         &= OFFSET.')
        end if
      else
        call pc_put_sensitive_field_flag ('SELECT_PATH_VEL', .false.)
        call pc_put_sensitive_field_flag ('PATH_VEL'       , .false.)
        call pc_put_sensitive_field_flag ('VELTYPE'        , .false.)
        call pc_put_sensitive_field_flag ('OPT_AVE'        , .false.)
        call pc_put_sensitive_field_flag ('OPT_SR'         , .false.)
        if (update_state /= PC_GUI) then
          obj%path_vel = PATHCHECK_EMPTY
          obj%veltype  = 'VTNM'
          obj%opt_ave  = .true.
          obj%opt_sr   = .false.
        end if
        path_dummy = PATHCHECK_EMPTY
        call pathcheck ('PATH_VEL', path_dummy, status=status, &
                         show=PATHCHECK_INFO_INPUT)
      end if
      if (obj%veltype /= 'VTNM' .and. obj%veltype /= 'VTRM' .and. &
          obj%veltype /= 'VTIN') then
        if (obj%opt_input == 'OFFSET') then
          call pc_error ('VELTYPE must be VTNM, VTRM or VTIN')
        else
          obj%veltype  = 'VTNM'
        end if
      end if
      if (obj%opt_input /= 'OFFSET' .or. obj%opt_sr) then
        call pc_put_sensitive_field_flag ('LEN_SMOOTH', .false.)
        if (update_state /= PC_GUI) obj%len_smooth = 0.2
      else
        call pc_put_sensitive_field_flag ('LEN_SMOOTH', .true.)
      end if
      obj%nsmooth = 2 * nint(0.5*max(obj%len_smooth,0.0)/obj%dt) + 1
      obj%len_smooth = (obj%nsmooth - 1) * obj%dt

! Determine relationship between trace arrays and (temporary) velocity arrays,
! where element 'velfirst' of the velocity array matches element 'trcfirst'
! of the trace array, and element 1 of the velocity array is always time 0.
      if (obj%opt_input == 'OFFSET') then
        if (obj%tstrt == 0.0) then
          obj%trcfirst = 1
          obj%velfirst = 1
          obj%ndptvel  = obj%ndpt
        else if (obj%tstrt > 0.0) then
          obj%trcfirst = 1
          obj%velfirst = nint(obj%tstrt/obj%dt) + 1
          obj%ndptvel  = obj%ndpt + obj%velfirst - 1
        else ! if (obj%tstrt < 0.0) then
          obj%velfirst = 1
          obj%trcfirst = nint(abs(obj%tstrt)/obj%dt) + 1
          obj%ndptvel  = obj%ndpt - obj%trcfirst + 1
          if (obj%ndptvel <= 0) then
            call pc_error ('No positive times in input traces!')
          end if
        end if
      else
        obj%trcfirst = 1
      end if

! Set sensitivities for wavelet-dependent ODTC
      if (obj%opt_output == 'RESID') then
        call pc_put_sensitive_field_flag ('SELECT_PATH_WVLT', .false.)
        call pc_put_sensitive_field_flag ('PATH_WVLT'       , .false.)
        if (update_state /= PC_GUI) obj%path_wvlt = PATHCHECK_EMPTY
        path_dummy = PATHCHECK_EMPTY
        call pathcheck ('PATH_WVLT', path_dummy, status=status, &
                         show=PATHCHECK_INFO_INPUT)
      else
        call pc_put_sensitive_field_flag ('SELECT_PATH_WVLT', .true.)
        call pc_put_sensitive_field_flag ('PATH_WVLT'       , .true.)
        call pathcheck ('PATH_WVLT', obj%path_wvlt, 'trc', status=status, &
                         show=PATHCHECK_INFO_INPUT)
        if (status /= PATHCHECK_VALID) goto 100
        if (obj%path_wvlt == wavelet_prev) goto 100
        obj%trcio => trcio_open (obj%path_wvlt, 'r')
        if (.not.associated(obj%trcio)) then
          if (update_state == PC_BACKEND) then
            call pc_error ('Unable to open Wavelet file for &
                           &Wavelet-Dependent ODTC.')
          else
            call pc_warning("Your Wavelet file doesn't seem to exist yet. &
                            &Be sure its name is right and TIM_FIRST is &
                            &set correctly.")
          end if
          goto 100
        end if
        if (ameq(obj%trcio%dt, obj%dt, 0.0001*obj%dt)) then
          if (update_state == PC_GUI) then
            obj%tim_first = obj%trcio%tmin
            call pc_info('TIM_FIRST set to wavelet file TMIN = ',  &
                          obj%trcio%tmin,                       &
                         ' but you may override this value if necessary.')
          end if
        else
          call pc_error('DT in wavelet file = ', obj%trcio%dt, &
            ' differs from current DT global = ', obj%dt)
          obj%path_wvlt = PATHCHECK_EMPTY
          call pathcheck ('PATH_WVLT', obj%path_wvlt, status=status, &
                           show=PATHCHECK_INFO_INPUT)
        end if
        if (pc_do_not_process_traces()) status2 = trcio_close (obj%trcio)
      end if
 100  continue
!
      obj%dld_odt = abs(obj%dld_odt)
      obj%noplen = 0
      if (status == PATHCHECK_VALID) then
        obj%wvlt_odtc = .true.
        if (obj%len_op == FNIL) then
          if (update_state /= PC_GUI) then
            call pc_error ('LEN_OP must be set when specifying &
                           &Wavelet-Dependent ODTC.')
          end if
        else
          noplen2 = max ( nint(0.5*obj%len_op/obj%dt) , 1 )
          obj%noplen = 2 * noplen2  +  1
          obj%len_op = (obj%noplen - 1) * obj%dt
          obj%ishift = - noplen2
        end if
      else
        obj%wvlt_odtc = .false.
        if (update_state /= PC_GUI) then
          obj%path_wvlt = PATHCHECK_EMPTY
          obj%tim_first = 0.0
          obj%len_op    = FNIL
          obj%dld_odt   = 1.0
        end if
      end if
      call pc_put_sensitive_field_flag ('TIM_FIRST' , obj%wvlt_odtc)
      call pc_put_sensitive_field_flag ('LEN_OP'    , obj%wvlt_odtc)
      call pc_put_sensitive_field_flag ('DLD_ODT'   , obj%wvlt_odtc)

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('OPT_INPUT', (/ 'ANGLE  ', 'OFFSET ', &
                                                 'A_AND_B' /), 3)
      call pc_put_options_field ('OPT_OUTPUT', (/ 'A_AND_B', 'P_REFL ', &
                                 'S_REFL ', 'VS_VP  ', 'AB_PROD', &
                                 'MODEL  ', 'RESID  ' /), 7)
      call pc_put_options_field ('NP_FIT', (/ 2, 3 /), 2)
      call pc_put_options_field ('VELTYPE', (/ 'VTNM', 'VTRM', 'VTIN' /), 3)

      call pc_put ('OPT_INPUT'  ,obj%opt_input)
      call pc_put ('OPT_OUTPUT' ,obj%opt_output)
      call pc_put ('NUMTR_IN'   ,obj%numtr_in)
      call pc_put ('NUMTR_OUT'  ,obj%numtr_out)
      call pc_put ('FOLD_MIN'   ,obj%fold_min)
      call pc_put ('DLD_FIT'    ,obj%dld_fit)
      call pc_put ('ANG_MAX'    ,obj%ang_max)
      call pc_put ('ANG_REF'    ,obj%ang_ref)
      call pc_put ('PATH_VEL'   ,obj%path_vel)
      call pc_put ('VELTYPE'    ,obj%veltype)
      call pc_put ('OPT_AVE'    ,obj%opt_ave)
      call pc_put ('LEN_SMOOTH' ,obj%len_smooth)
      call pc_put ('OPT_SR'     ,obj%opt_sr)
      call pc_put ('NP_FIT'     ,obj%np_fit)
      call pc_put ('PATH_WVLT'  ,obj%path_wvlt)
      call pc_put ('TIM_FIRST'  ,obj%tim_first)
      call pc_put ('LEN_OP'     ,obj%len_op)
      call pc_put ('DLD_ODT'    ,obj%dld_odt)

      call pc_put_global  ('NUMTR', obj%numtr_out)

      nscratch = 6 * obj%numtr_in  +  3 * obj%ndpt
      nstore   = obj%noplen
      if (obj%opt_input == 'OFFSET') then
        if (obj%opt_ave) then
          nstore = nstore + 2*obj%ndpt
        else
          nstore = nstore + 10*obj%ndpt
        end if
      end if
      call pc_put_control ('NSCRATCH', nscratch)
      call pc_put_control ('NSTORE'  , nstore)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free (obj%opr)
      call mem_free (obj%xbins)
      call mem_free (obj%ybins)
      call mem_free (obj%vintlast)
      call mem_free (obj%vrmslast)
      call mem_free (obj%vintbuf)
      call mem_free (obj%vrmsbuf)
      if (associated(obj%velio))   call velio_close     (obj%velio)
      if (associated(obj%temptfile_veltab))  &
                                   call temptfile_close (obj%temptfile_veltab)

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.    ! needed for the wrapup routine.

      obj%ntr_total = 0

      if (obj%dld_fit > 0.0) then
        obj%dld_fit1 = 0.01 * obj%dld_fit
      else
        obj%dld_fit1 = 1.e-7
      end if

      if (obj%opt_input /= 'OFFSET' .or. obj%opt_sr) then
        obj%nsmooth = 1
        obj%len_smooth = 0.0
      end if

      if (obj%opt_input == 'A_AND_B') then
        obj%sin2max = 0.5
      else
        obj%sin2max = min (sin(real(RADIANS_PER_DEGREE*obj%ang_max))**2, 0.99)
      end if

      if (obj%opt_output == 'P_REFL') then
        obj%sin2ref = sin(real(RADIANS_PER_DEGREE*obj%ang_ref)) ** 2
      else
        obj%ang_ref = 0.0
        obj%sin2ref = 0.0
      end if

      if (obj%opt_input == 'OFFSET') then
!!------Check velocity file:
        call velio_scan_alloc (obj%path_vel, obj%numvels, status2, &
                 message, nhx=obj%nhx, nhy=obj%nhy, maxpicks=maxpicks, &
                 xbins=obj%xbins, ybins=obj%ybins, nxbins=obj%nxbins,  &
                 nybins=obj%nybins)
        if (status2 /= VELIO_OK) then
          call pc_error ('Error scanning velocity file '//obj%path_vel)
          return
        end if
        obj%disktables = .false.
        if (.not. obj%opt_ave) then
          if (obj%nxbins==0 .and. obj%nybins==0) then
            call pc_error ("Velocities aren't on a rectangular grid.")
            return
          end if
          if (obj%numvels < 2) then
            write (obj%lun, *) 'Velocity file contains only one &
                               &velocity function.'
            write (obj%lun, *) 'Will handle as if OPT_AVE = YES.'
            obj%opt_ave = .true.
          else
            obj%ix1mem = 1
            obj%ix2mem = min (obj%nxbins, 2)
            obj%nxmem  = obj%ix2mem
            obj%iy1mem = 1
            obj%iy2mem = min (obj%nybins, 2)
            obj%nymem  = obj%iy2mem
            call mem_alloc (obj%vintbuf, obj%ndpt, obj%nxmem, obj%nymem)
            call mem_alloc (obj%vrmsbuf, obj%ndpt, obj%nxmem, obj%nymem)
            if (pc_do_not_process_traces()) return ! if allocation errors
            if (obj%nxbins>2 .or. obj%nybins>2) then ! store vels on disk
              obj%disktables = .true.
              call temptfile_open (obj%temptfile_veltab, 'avagradvels', &
                    0, obj%ndpt, obj%lun, status2)
              if (status2 /= TEMPTFILE_OK) then
                call pc_error ('Error opening disk file for velocities.')
                return
              end if
            end if
          end if
        end if
        call mem_alloc (obj%vintlast, obj%ndpt)
        call mem_alloc (obj%vrmslast, obj%ndpt)
        if (pc_do_not_process_traces()) return   ! if allocation errors
!!------Open velocity file for reading
        call velio_open_read (obj%velio, obj%path_vel, obj%numvels, &
                              status2, message)
        if (status2 /= VELIO_OK) then
          call pc_error ('Error opening velocity file '//obj%path_vel)
          return
        end if
!!------Call subroutine to read velocities and store them in tables
        call avagrad_vels_to_tables (obj, maxpicks)
!!------Close velocity file
        call velio_close (obj%velio)
      end if

      if (obj%wvlt_odtc) then
        if (.not.associated(obj%trcio)) then
          call pc_error ("AVAGRAD: Wavelet file isn't open.")
          return
        end if
        call avagrad_detune (obj, status)
        if (status /= 0) return
      end if
      if (associated(obj%trcio)) status2 = trcio_close (obj%trcio)

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine avagrad_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine avagrad (obj, ntr, hd, tr)
      implicit none
      type(avagrad_struct),intent(inout) :: obj                  ! arguments
      integer             ,intent(inout) :: ntr                  ! arguments
      double precision    ,intent(inout) :: hd(:,:)              ! arguments
      real                ,intent(inout) :: tr(:,:)              ! arguments

      integer :: itrace, itime, numang, nang1, nang2, ierr       ! local
      integer :: ntr_new                                         ! local
      real    :: sin2temp  ! local
      integer :: itop(obj%numtr_in), ibot(obj%numtr_in)          ! local
      real    :: wgts(obj%numtr_in)  ! local
      real    :: sin2(obj%numtr_in), sin2tan2(obj%numtr_in)      ! local
      real    :: a(obj%ndpt), b(obj%ndpt), c(obj%ndpt)           ! local

      if (ntr == NO_MORE_TRACES) then
        call avagrad_wrapup (obj)
        return
      end if

      if (obj%opt_input == 'A_AND_B') then
        if (ntr /= 2) then
          call pc_print ('AVAGRAD Fatal Error: Received group', &
                         nint(hd(HDR_CURRENT_GROUP,1)), 'with', ntr, &
                         'traces while OPT_INPUT = A_AND_B.')
          ntr = FATAL_ERROR
          call avagrad_wrapup (obj)
          return
        end if
        a = tr(:obj%ndpt, 1)
        b = tr(:obj%ndpt, 2)
        goto 1000
      end if

      if (ntr > obj%numtr_in) then
        call pc_print ('AVAGRAD: Received group', &
                       nint(hd(HDR_CURRENT_GROUP,1)), 'with', ntr, &
                       'traces--exceeds NUMTR global. Processing only &
                       &NUMTR traces.')
        ntr = obj%numtr_in
      end if
!
! Interpolate velocity functions if OPT_INPUT = OFFSET and not averaging
!
      if (.not. obj%opt_ave) then
        call avagrad_interpolate_vel (obj, hd, ntr)
        if (ntr == FATAL_ERROR) then
          call avagrad_wrapup (obj)
          return
        end if
      end if
!
! Find usable tops and bottoms of traces; fill sin2 and sin2tan2 arrays
!
      do itrace = 1, ntr
        call mutehw (hd(:,itrace), tr(:,itrace), obj%ndpt, 0.0, MUTEHW_SET)
        call avagrad_first_last (tr(:obj%ndpt, itrace), obj%ndpt, &
                                 itop(itrace), ibot(itrace))
        itop(itrace) = max(nint(hd(HDR_TOP_MUTE,itrace)), itop(itrace), &
                           obj%trcfirst)
        ibot(itrace) = min(nint(hd(HDR_BOTTOM_MUTE,itrace)), ibot(itrace))
        if (obj%opt_input == 'ANGLE') then
          sin2(itrace) = &
                 sin(real(RADIANS_PER_DEGREE*hd(HDR_OFFSET,itrace))) ** 2
          sin2temp = min(sin2(itrace), obj%sin2max)
          sin2tan2(itrace) = sin2temp * sin2temp / (1.0 - sin2temp)
        end if
      end do
!
! Begin main loop over trace times
!
      do itime = 1, obj%ndpt
        if (obj%opt_input == 'OFFSET') then
          call avagrad_offset_to_angle (obj, itime, hd, ntr, sin2, sin2tan2)
        end if
        numang = 0
        do itrace = 1, ntr
          if (itime >= itop(itrace) .and. itime <= ibot(itrace) &
                                    .and. sin2(itrace) <= obj%sin2max) then
            if (numang == 0) nang1 = itrace
            numang = numang + 1
            nang2 = itrace
            wgts(itrace) = 1.0
          else
            wgts(itrace) = 0.0
          end if
        end do
!
! Fit least squares 2-parameter or 3-parameter line
        if (numang >= obj%fold_min) then
          call avagrad_fit (obj%np_fit, nang2-nang1+1, numang, obj%dld_fit1, &
                            wgts(nang1:nang2), sin2(nang1:nang2),            &
                            sin2tan2(nang1:nang2), tr(itime,nang1:nang2),    &
                            a(itime), b(itime), c(itime), ierr)
        else
          ierr = 1
        end if
        if (ierr /= 0) then
          a(itime) = 0.0 ; b(itime) = 0.0 ; c(itime) = 0.0
          wgts(:ntr) = 0.0
        end if
!
! Find output traces if OPT_OUTPUT = RESID
        if (obj%opt_output == 'RESID') then
          do itrace = 1, ntr
            if (wgts(itrace) > 0.0) then
              tr(itime,itrace) = tr(itime,itrace) - a(itime) &
                       - b(itime)*sin2(itrace) - c(itime)*sin2tan2(itrace)
            else
              tr(itime,itrace) = 0.0
            end if
          end do
        end if
      end do
!
! End main loop over trace times
!
 1000 continue
!
! Apply Wavelet-Dependent ODTC if requested
!
      if (obj%wvlt_odtc) then
        call fltr_filtrgs (obj%opr, obj%noplen, a, obj%ndpt, b, obj%ndpt, &
                           0, obj%ishift)
      end if
!
! Set output traces and ntr_new for all options (except that output traces
! for OPT_OUTPUT = RESID were already set earlier
!
      select case (obj%opt_output)
      case ('A_AND_B')
        tr(:obj%ndpt, 1) = a
        tr(:obj%ndpt, 2) = b
        ntr_new = 2
      case ('P_REFL')
        tr(:obj%ndpt, 1) = a  +  obj%sin2ref * b
        ntr_new = 1
      case ('S_REFL')
        tr(:obj%ndpt, 1) = 0.5 * (a - b)
        ntr_new = 1
      case ('VS_VP')
        tr(:obj%ndpt, 1) = (-0.5) * (a + b)
        ntr_new = 1
      case ('AB_PROD')
        tr(:obj%ndpt, 1) = a * b
        ntr_new = 1
      case ('MODEL')
        do itime = 1, obj%ndpt
          if (obj%opt_input == 'OFFSET') then
            call avagrad_offset_to_angle (obj, itime, hd, ntr, sin2, sin2tan2)
          end if
          do itrace = 1, ntr
            if (itime >= itop(itrace) .and. itime <= ibot(itrace) &
                                      .and. sin2(itrace) <= obj%sin2max) then
              tr(itime,itrace) = a(itime) + b(itime)*sin2(itrace)
            else
              tr(itime,itrace) = 0.0
            end if
          end do
        end do
        ntr_new = ntr
      case ('RESID')
        ntr_new = ntr
      end select
!
! Set header words for output
!
      if (obj%opt_output /= 'MODEL' .and. obj%opt_output /= 'RESID') then
        if (obj%opt_input /= 'A_AND_B') then
          ! Use header of 1st live trace of input gather
          do itrace = 1, ntr
            if (hd(HDR_LAV,itrace) > 0.0D0) then
              if (itrace > 1) hd(:obj%nwih,1) = hd(:obj%nwih,itrace)
              exit
            end if
          end do
          hd(HDR_FOLD,1) = ntr
        end if
        hd(HDR_OFFSET,1) = obj%ang_ref
        if (obj%opt_output=='A_AND_B') then
          hd(:obj%nwih,2) = hd(:obj%nwih,1)
          hd(HDR_PANEL,1) = 1
          hd(HDR_PANEL,2) = 2
        end if
      end if
      ntr = ntr_new
      do itrace = 1, ntr
        hd(HDR_SEQUENCE,itrace) = obj%ntr_total + itrace
        hd(HDR_CURRENT_CHANNEL,itrace) = itrace
        call avagrad_first_last (tr(:obj%ndpt, itrace), obj%ndpt, &
                                 itop(itrace), ibot(itrace))
        hd(HDR_TOP_MUTE,itrace) = min(itop(itrace), obj%ndpt)
        hd(HDR_BOTTOM_MUTE,itrace) = min(ibot(itrace), obj%ndpt)
      end do
      obj%ntr_total = obj%ntr_total + ntr
      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      return
      end subroutine avagrad

!!---------------------------- avagrad_fit --------------------------------!!
!!---------------------------- avagrad_fit --------------------------------!!
!!---------------------------- avagrad_fit --------------------------------!!
!
!  Perform 2-param or 3-param least squares weighted fit
!
!   NPAR  = number of params (2 or 3).
!   N     = number of data points.
!   NLIVE = number of live data points (with non-zero weights).
!   DLD   = diagonal load.
!   WGTS  = weight array (length N).
!   X     = 1st independent variable (array(N)).
!   Y     = 2nd independent variable (array(N))--ignored if NPAR=2.
!   DTA   = dependent variable (array(N)).
!   A     = 1st coefficient returned
!   B     = 2nd coefficient returned
!   C     = 3rd coefficient returned - set to zero if NPAR=2.
!   IERR  = returned as 0 if no error, 1 if error.
!
!      Fitting:    DTA = A + B*X          if NPAR=2;
!                  DTA = A + B*X + C*Y    if NPAR=3.
!
      subroutine avagrad_fit (npar, n, nlive, dld, wgts, x, y, dta, &
                              a, b, c, ierr)
      implicit none
      integer , intent(in)  :: npar, n, nlive                    ! arguments
      real    , intent(in)  :: dld, wgts(n), x(n), y(n), dta(n)  ! arguments
      real    , intent(out) :: a, b, c                           ! arguments
      integer , intent(out) :: ierr                              ! arguments
!
      real                  :: mat(3,3), matinv(3,3), rvect(3)   ! local
      real                  :: diagfact, adeterm                 ! local
      integer               :: i, j                              ! local
!
! Initialize error flag
!
      ierr = 0
!
! Set up equations
!
      mat(1,1) = sum (wgts)
      mat(1,2) = sum (wgts * x)
      mat(2,2) = sum (wgts * x * x)
      rvect(1) = sum (wgts * dta)
      rvect(2) = sum (wgts * dta * x)
      if (npar == 3) then
        mat(1,3) = sum (wgts * y)
        mat(2,3) = sum (wgts * x * y)
        mat(3,3) = sum (wgts * y * y)
        rvect(3) = sum (wgts * dta * y)
      end if
!
! Add diagonal load term
!
      diagfact = 0.0
      do i = 1, npar
        diagfact = diagfact + mat(i,i)
      end do
      diagfact = dld * (diagfact / nlive)
      do i = 1, npar
        mat(i,i) = mat(i,i) + diagfact
      end do
!
! Solve equations
!
      if (npar == 2) then
        adeterm = mat(1,1)*mat(2,2) - mat(1,2)*mat(1,2)
        if (adeterm <= 0.0) goto 100
        adeterm = 1.0 / adeterm
        matinv(1,1) =  mat(2,2) * adeterm
        matinv(1,2) = -mat(1,2) * adeterm
        matinv(2,2) =  mat(1,1) * adeterm
        a = matinv(1,1)*rvect(1) + matinv(1,2)*rvect(2)
        b = matinv(1,2)*rvect(1) + matinv(2,2)*rvect(2)
        c = 0.0
      else ! if (npar == 3) then
        matinv(1,1) = mat(2,2)*mat(3,3) - mat(2,3)*mat(2,3)
        matinv(1,2) = mat(2,3)*mat(1,3) - mat(1,2)*mat(3,3)
        matinv(1,3) = mat(1,2)*mat(2,3) - mat(2,2)*mat(1,3)
        matinv(2,2) = mat(1,1)*mat(3,3) - mat(1,3)*mat(1,3)
        matinv(2,3) = mat(1,2)*mat(1,3) - mat(1,1)*mat(2,3)
        matinv(3,3) = mat(1,1)*mat(2,2) - mat(1,2)*mat(1,2)
        adeterm = mat(1,1)*matinv(1,1) + mat(1,2)*matinv(1,2) &
                  + mat(1,3)*matinv(1,3)
        if (adeterm <= 0.0) goto 100
        adeterm = 1.0 / adeterm
        do i = 1, 3
          do j = i, 3
            matinv(i,j) = matinv(i,j) * adeterm
          end do
        end do
        a = matinv(1,1)*rvect(1) + matinv(1,2)*rvect(2) + matinv(1,3)*rvect(3)
        b = matinv(1,2)*rvect(1) + matinv(2,2)*rvect(2) + matinv(2,3)*rvect(3)
        c = matinv(1,3)*rvect(1) + matinv(2,3)*rvect(2) + matinv(3,3)*rvect(3)
      end if
      return
!
!  Error return
!
 100  ierr = 1
      return
      end subroutine avagrad_fit

!!--------------------------- avagrad_detune ------------------------------!!
!!--------------------------- avagrad_detune ------------------------------!!
!!--------------------------- avagrad_detune ------------------------------!!
!
!  Routine to estimate NMO destretch operator based on estimate of the
!  wavelet. This routine should be called only once at setup time.
!  The estimated wavelet is read from disk using TRCIO, where it is
!  assumed that the TRCIO file has already been opened before calling
!  this routine.  Arguments:
!
! Name   Type*  Valid  Description     *Type: I=IN, O=OUT, B=BOTH
! ----   -----  -----  -----------
! obj      B           Data structure of type avagrad_struct.
! ierror   O    int    =0 if okay; =1 if error occurs.
!
!    The returned operator OPR is applied later by the subroutine call:
!
!          CALL FILTRGS(OPR,NOPLEN,A,NDPT,B,NDPT,0,ISHIFT)
!
!    where "A" is an AVA Intercept trace, "B" is (initially) an
!    uncorrected AVA Gradient trace, and ISHIFT is a number determined in
!    the update subroutine. Only the "B" array is altered by this call; it
!    is replaced by  B + OPR*A, which is the corrected Gradient. Note that
!    the next-to-last argument in call to FILTRGS is the number "zero."
!
!    Also note: OPR, NOPLEN, NDPT, and ISHIFT are all part of the
!    "obj" data structure. Memory for the OPR array is allocated by
!    this subroutine.
!
      subroutine avagrad_detune (obj, ierror)
      implicit none
      type(avagrad_struct),intent(inout) :: obj                  ! arguments
      integer             ,intent(out)   :: ierror               ! arguments

      integer           :: ier1, i, nfirst, nlast, nsamp         ! local
      real              :: tstrt1                                ! local
      double precision  :: hdtmp(obj%nwih)                       ! local
      real              :: wv_est (obj%trcio%num_values)         ! local
      real              :: wv_est1(obj%trcio%num_values)         ! local
      real              :: r(obj%noplen), x(obj%noplen)          ! local
      real              :: work(2*obj%noplen)                    ! local
!
      ierror = 0
!
!  Read wavelet from TRCIO file
      ier1 = trcio_read_trace (obj%trcio, hdtmp, wv_est)
      if (ier1 /= trcio_ok) then
        call pc_error ('AVAGRAD_DETUNE: Error reading wavelet from file.')
        goto 1005
      end if
!
!  Find first and last wavelet samples
!
      call avagrad_first_last (wv_est, obj%trcio%num_values, nfirst, nlast)
      ier1 = trcio_close(obj%trcio)
      nsamp = nlast - nfirst + 1
      if (nsamp < 5) then
        call pc_error ('AVAGRAD_DETUNE: Wavelet read from disk is DEAD &
                       &or less than 5 samples long.')
        goto 1005
      end if
!
!  Allocate memory for detuning operator
!
      allocate (obj%opr(obj%noplen), stat=ier1)
      if (ier1 /= 0) then
        call pc_error ('AVAGRAD_DETUNE: Unable to allocate memory for &
                       &detuning operator.')
        goto 1005
      end if
!
!  Estimated wavelet in gradient error
!
      tstrt1 = obj%tim_first / obj%dt  -  1.0
      wv_est1 = 0.0
      do i = nfirst+1, nlast-1
        wv_est1(i) = 0.25 * (tstrt1+i) * (wv_est(i+1)-wv_est(i-1))
      end do
!
!  Wavelet Autocorrelation
!
      call fltr_filtrgs (wv_est (nfirst+1:nlast-1), nsamp-2, &
                         wv_est (nfirst+1:nlast-1), nsamp-2, &
                         r, obj%noplen, 1, 0)
      r(1) = r(1) * (1.0 + obj%dld_odt/100.)
!
!  Crosscorrelation
!
      call fltr_filtrgs (wv_est1(nfirst+1:nlast-1), nsamp-2, &
                         wv_est (nfirst+1:nlast-1), nsamp-2, &
                         x, obj%noplen, 1, obj%ishift)
!
!  Find shaping filter from trace wavelet to error term wavelet
!
      call opfilt (obj%noplen, obj%opr, x, work, r)
!
      return
 1005 ierror = 1
      return
      end subroutine avagrad_detune

!!--------------------------- avagrad_smooth ------------------------------!!
!!--------------------------- avagrad_smooth ------------------------------!!
!!--------------------------- avagrad_smooth ------------------------------!!
!
! Input array XIN is smoothed by applying a running average of length NAVG;
! output in XOUT. The running average is always performed over an ODD number
! of points centered around a desired output point. If NAVG is even, an
! averaging length of NAVG+1 is used. Averaging length is tapered down when
! approaching the beginning and end of the XIN array, to keep the averaging
! operator from running off the ends of that array.
!
      subroutine avagrad_smooth (xin, n, navg, xout)
      implicit none
!
      integer , intent(in)  :: n          ! Arguments
      real    , intent(in)  :: xin(n)     ! Arguments
      integer , intent(in)  :: navg       ! Arguments
      real    , intent(out) :: xout(n)    ! Arguments
!
      integer  :: navg2, i, j             ! local variables
      real     :: sfact                   ! local variables
!
      if (navg <= 1) then
        xout = xin
        return
      end if
!
      navg2 = min(navg,n-1)/2
      sfact = 1.0/(2*navg2 + 1)
      xout = 0.
      do j = -navg2, navg2
        do i = 1+abs(j), n-abs(j)
          xout(i) = xout(i) + xin(i+j)
        end do
      end do
      do i = 2, navg2
        xout(i) = xout(i) / (2*i-1)
      end do
      xout(navg2+1:n-navg2) = xout(navg2+1:n-navg2) * sfact
      do i=n-navg2+1,n-1
        xout(i) = xout(i) / (2*(n-i)+1)
      end do
!
      return
      end subroutine avagrad_smooth

!!------------------------- avagrad_first_last ----------------------------!!
!!------------------------- avagrad_first_last ----------------------------!!
!!------------------------- avagrad_first_last ----------------------------!!
!
!  Locate first and last non-zero elements of a 1-D real array
!  (ifirst and ilast both returned as n+1 if whole array is dead)
!
      subroutine avagrad_first_last (x, n, ifirst, ilast)
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
      end subroutine avagrad_first_last

!!------------------------ avagrad_read_velocity --------------------------!!
!!------------------------ avagrad_read_velocity --------------------------!!
!!------------------------ avagrad_read_velocity --------------------------!!
!
! This routine reads a single velocity function from the velocity file and
! interpolates it to the trace sample rate. Because this routine is called
! only by avagrad_vels_to_tables, which is itself called only by
! avagrad_update, we can use the parameter cache.
! Note that in addition to returning vel_interp, veltype and errflag, this
! routine also sets obj%xlast and obj%ylast in the process data structure.
!
      subroutine avagrad_read_velocity (obj, maxpicks, vel_interp, &
                                        veltype, errflag)
      implicit none
      type(avagrad_struct),intent(inout) :: obj             ! arguments
      integer             ,intent(in)    :: maxpicks        ! arguments
      real                ,intent(out)   :: vel_interp(:)   ! arguments
      character(len=4)    ,intent(out)   :: veltype         ! arguments
      integer             ,intent(out)   :: errflag         ! arguments

      integer            :: npicks, i, ipick                ! local
      real               :: ttest                           ! local
      real               :: tpicks(maxpicks)                ! local
      real               :: vpicks(maxpicks)                ! local
      real               :: work(1)                         ! local
      character(len=128) :: message                         ! local

      call velio_read_velfun (obj%velio, obj%xlast, obj%ylast, npicks, &
                     tpicks, vpicks, errflag, message, veltype=veltype)
      if (errflag /= VELIO_OK) then
        call pc_error ('Error reading velocity function.')
        return
      end if
      if (veltype == '') veltype = obj%veltype   ! for velocity trace files.
      if (veltype=='VTNM' .or. veltype=='VTRM') then
        call intpvelf (npicks, tpicks(:npicks), vpicks(:npicks), work,  &
           4, 0., obj%dt, obj%ndptvel, vel_interp(:obj%ndptvel), errflag)
        if (errflag /= 0) return
      else if (veltype == 'VTIN') then
        ipick = 1
        do i = 1, obj%ndptvel
          ttest = (i-1) * obj%dt
          do
            if (ttest<=tpicks(ipick) .or. ipick==npicks) exit
            ipick = ipick + 1
          end do
          vel_interp(i) = vpicks(ipick)
        end do
      else
        errflag = 1
        call pc_error ('Illegal velocity type '//veltype//'. Velocity type &
                       &must be VTRM, VTNM, or VTIN.')
      end if

      return
      end subroutine avagrad_read_velocity

!!----------------------- avagrad_vels_to_tables --------------------------!!
!!----------------------- avagrad_vels_to_tables --------------------------!!
!!----------------------- avagrad_vels_to_tables --------------------------!!
!
! This routine reads the velocity functions and derives tables of RMS and
! interval velocity. Because this routine is called by avagrad_update, it
! can use the parameter cache.
!
      subroutine avagrad_vels_to_tables (obj, maxpicks)
      implicit none
      type(avagrad_struct),intent(inout) :: obj             ! arguments
      integer             ,intent(in)    :: maxpicks        ! arguments

      integer            :: ivel, errflag, j, ix, iy, irec  ! local
      real               :: vel_interp (obj%ndptvel)        ! local
      real               :: vel_sum    (obj%ndptvel)        ! local
      double precision   :: hdtmp (1)                       ! local
      character(len=4)   :: veltype, veltype_last           ! local


      hdtmp(1) = 0.0
      obj%vintlast(:obj%trcfirst-1) = 0.0
      obj%vrmslast(:obj%trcfirst-1) = 0.0

      if (obj%opt_ave) then  ! derive composite velocity
        vel_sum = 0.0
        do ivel = 1, obj%numvels
          call avagrad_read_velocity (obj, maxpicks, vel_interp, &
                                      veltype, errflag)
          if (errflag /= 0) goto 100
          if (ivel > 1) then
            if (veltype_last=='VTNM' .or. veltype_last=='VTRM') then
              if (veltype == 'VTIN') errflag = 1
            else if (veltype=='VTNM' .or. veltype=='VTRM') then
              errflag = 1
            end if
            if (errflag /= 0) then
              call pc_error ('Velocity type '//veltype//' different &
                             &from type of previous function.')
              goto 100
            end if
          end if
          veltype_last = veltype
          vel_sum = vel_sum + vel_interp
        end do
        vel_sum = vel_sum / obj%numvels
        write (obj%lun, *)
        if (veltype == 'VTIN') then
          write (obj%lun, *) 'Interval velocities were input'
        else
          write (obj%lun, *) 'Stacking or RMS velocities were input'
        end if
        if ((.not. obj%opt_sr) .or. (veltype == 'VTIN')) then
          write (obj%lun, *) 'Composite input velocity @', stride*obj%dt, 's'
          write (obj%lun, '(5(F7.2,I6))')  &
                 ((j-1)*obj%dt,nint(vel_sum(j)),j=1,obj%ndptvel,stride)
          write (obj%lun, *)
        end if
        call avagrad_convert_vels (obj, vel_sum, veltype, vel_interp, errflag)
        if (errflag /= 0) return
        obj%nxbins = 1
        obj%nybins = 1
        obj%xlast = 0.0
        obj%ylast = 0.0
      end if

      do iy = 1, obj%nybins
        do ix = 1, obj%nxbins

          if (.not. obj%opt_ave) then
            call avagrad_read_velocity (obj, maxpicks, vel_sum, &
                                        veltype, errflag)
            if (errflag /= 0) goto 100
            write (obj%lun, *)
            write (obj%lun, *) 'Velocity of type ', veltype, ' input at &
                           &location: X =', obj%xlast, ', Y =', obj%ylast
            call avagrad_convert_vels (obj, vel_sum, veltype, &
                                       vel_interp, errflag)
            if (errflag /= 0) return
          end if

          obj%vintlast(obj%trcfirst:)   = vel_interp(obj%velfirst:)
          obj%vrmslast(obj%trcfirst:)   = vel_sum(obj%velfirst:)

          if (.not. obj%opt_ave) then
            if (ix<3 .and. iy<3) then
              obj%vintbuf (:, ix, iy) = obj%vintlast
              obj%vrmsbuf (:, ix, iy) = obj%vrmslast
            end if
            if (obj%disktables) then
              irec = 2 * ((iy-1)*obj%nxbins + ix-1)  +  1
              call temptfile_write (obj%temptfile_veltab, irec, hdtmp, &
                                    obj%vintlast, errflag)
              if (errflag /= TEMPTFILE_OK) goto 200
              irec = irec + 1
              call temptfile_write (obj%temptfile_veltab, irec, hdtmp, &
                                    obj%vrmslast, errflag)
              if (errflag /= TEMPTFILE_OK) goto 200
            end if
          end if

        end do
      end do

      return

 100  call pc_error ('Problem in velocity at location X =', obj%xlast, &
                     ', Y =', obj%ylast)
      return
 200  call pc_error ('Error writing velocity to temporary disk file.')
      return
      end subroutine avagrad_vels_to_tables

!!------------------------ avagrad_convert_vels ---------------------------!!
!!------------------------ avagrad_convert_vels ---------------------------!!
!!------------------------ avagrad_convert_vels ---------------------------!!
!
! Input for this routine is a velocity of type VELTYPE in array VEL_SUM.
! Upon output, if OPT_SR is false, VEL_INTERP contains smoothed interval
! velocity while VEL_SUM contains the corresponding RMS velocity.
! If OPT_SR is true, there is no smoothing, and VEL_INTERP and VEL_SUM
! both contain the RMS velocity.
! Arrays VEL_SUM and VEL_INTERP must both be of length obj%ndptvel.
! The routine also prints a table of decimated velocity to unit obj%lun.
!
      subroutine avagrad_convert_vels (obj, vel_sum, veltype, &
                                       vel_interp, errflag)
      implicit none
      type(avagrad_struct),intent(in)    :: obj             ! arguments
      real                ,intent(inout) :: vel_sum(:)      ! arguments
      character(len=4)    ,intent(in)    :: veltype         ! arguments
      real                ,intent(out)   :: vel_interp(:)   ! arguments
      integer             ,intent(out)   :: errflag         ! arguments

      integer             :: j                              ! local

      errflag = 0
      if (obj%opt_sr) then
        if (veltype == 'VTIN') then
          call avagrad_int_to_rms_vel (obj%ndptvel, vel_sum, vel_interp)
          vel_sum = vel_interp
        else
          vel_interp = vel_sum
        end if
        if (obj%opt_ave) then
          write (obj%lun, *) 'Composite RMS velocity @', stride*obj%dt, 's'
        else
          write (obj%lun, *) 'RMS velocity @', stride*obj%dt, &
                 's  for location: X =', obj%xlast, ', Y =', obj%ylast
        end if
      else
        if (veltype /= 'VTIN') then
          call avagrad_rms_to_int_vel (obj%ndptvel, vel_sum, errflag)
          if (errflag /= 0) return
        end if
        call avagrad_smooth (vel_sum, obj%ndptvel, obj%nsmooth, &
                             vel_interp)
        call avagrad_int_to_rms_vel (obj%ndptvel, vel_interp, vel_sum)
        if (obj%opt_ave) then
          write (obj%lun, *) 'Smoothed, composite INTERVAL velocity @', &
                             stride*obj%dt, 's'
        else
          write (obj%lun, *) 'Smoothed INTERVAL velocity @',        &
                             stride*obj%dt, 's  for location: X =', &
                             obj%xlast, ', Y =', obj%ylast
        end if
      end if
      write (obj%lun, '(5(F7.2,I6))')  &
             ((j-1)*obj%dt,nint(vel_interp(j)),j=1,obj%ndptvel,stride)
      return
      end subroutine avagrad_convert_vels

!!----------------------- avagrad_rms_to_int_vel --------------------------!!
!!----------------------- avagrad_rms_to_int_vel --------------------------!!
!!----------------------- avagrad_rms_to_int_vel --------------------------!!
!
! This routine converts RMS to Interval velocity (in place within "velocity"
! array). It is assumed that the array begins at time zero. This routine
! calls pc_error to report errors, and also returns errflag = 1 in case of
! error.
!
      subroutine avagrad_rms_to_int_vel (ndptvel, velocity, errflag)
      implicit none
      integer , intent(in)    :: ndptvel               ! arguments
      real    , intent(inout) :: velocity(ndptvel)     ! arguments
      integer , intent(out)   :: errflag               ! arguments

      integer     :: j                                 ! local

      errflag = 0
      if (ndptvel < 2) return

      do j = 1, ndptvel
        velocity(j) = (j-1) * velocity(j)**2
      end do

      velocity(2:ndptvel) = velocity(2:ndptvel) - velocity(1:ndptvel-1)

      if (minval(velocity(2:ndptvel)) <= 0.0) then
        call pc_error ('Error converting RMS to Interval velocity.')
        errflag = 1
        return
      end if

      velocity(2:ndptvel) = sqrt (velocity(2:ndptvel))
      velocity(1) = velocity(2)

      return
      end subroutine avagrad_rms_to_int_vel

!!----------------------- avagrad_int_to_rms_vel --------------------------!!
!!----------------------- avagrad_int_to_rms_vel --------------------------!!
!!----------------------- avagrad_int_to_rms_vel --------------------------!!
!
! This routine converts Interval to RMS velocity and the conversion is NOT
! done in place (input is in vel_int, output in vel_rms). It is assumed that
! the arrays begin at time zero.
!
      subroutine avagrad_int_to_rms_vel (ndptvel, vel_int, vel_rms)
      implicit none
      integer , intent(in)  :: ndptvel                 ! arguments
      real    , intent(in)  :: vel_int(ndptvel)        ! arguments
      real    , intent(out) :: vel_rms(ndptvel)        ! arguments

      integer     :: j                                 ! local
      real        :: sumv2                             ! local

      if (ndptvel < 2) then
        vel_rms = vel_int
      else
        sumv2 = 0.0
        do j = 2, ndptvel
          sumv2 = sumv2 + vel_int(j)**2
          vel_rms(j) = sqrt(sumv2/(j-1))
        end do
        vel_rms(1) = vel_rms(2)
      end if

      return
      end subroutine avagrad_int_to_rms_vel

!!----------------------- avagrad_interpolate_vel -------------------------!!
!!----------------------- avagrad_interpolate_vel -------------------------!!
!!----------------------- avagrad_interpolate_vel -------------------------!!

      subroutine avagrad_interpolate_vel (obj, hd, ntr)
      implicit none
      type(avagrad_struct),intent(inout) :: obj               ! arguments
      double precision    ,intent(in)    :: hd(:,:)           ! arguments
      integer             ,intent(inout) :: ntr               ! arguments

      integer           :: ilive, i1, i2, j1, j2              ! local
      integer           :: ix, iy, irec, errflag              ! local
      real              :: xnow, ynow, factor                 ! local
      double precision  :: hdtmp (1)                          ! local

! Assume that if this subroutine is called, OPT_INPUT = OFFSET
! and OPT_AVE is false

! Use location of first live trace in gather
      do ilive = 1, ntr
        if (hd(HDR_LAV,ilive) > 0.0D0) exit
      end do
      if (ilive > ntr) ilive = 1
      xnow = hd(obj%nhx,ilive)
      ynow = hd(obj%nhy,ilive)

! Do nothing if current location = last location
      if (xnow==obj%xlast .and. ynow==obj%ylast) return

! Begin interpolation process; retrieve tables from disk if necessary
      i1 = obj%ix1mem
      i2 = obj%ix2mem
      j1 = obj%iy1mem
      j2 = obj%iy2mem
      if (obj%disktables) then  ! CHECK LOCATIONS NEEDED IN TABLES
        if (obj%nxbins > 2) then
          call interp_1d_lin (obj%xbins(:obj%nxbins), xnow, i1, i2, factor)
          if (i1 == obj%nxbins) i1 = obj%nxbins - 1
        end if
        if (obj%nybins > 2) then
          call interp_1d_lin (obj%ybins(:obj%nybins), ynow, j1, j2, factor)
          if (j1 == obj%nybins) j1 = obj%nybins - 1
        end if
        if (i1/=obj%ix1mem .or. j1/=obj%iy1mem) then ! MUST READ FROM DISK
          do iy = 1, obj%nymem
            do ix = 1, obj%nxmem
              irec = 2 * ((j1+iy-2)*obj%nxbins + i1+ix-2)  +  1
              call temptfile_read (obj%temptfile_veltab, irec, hdtmp, &
                                   obj%vintbuf(:,ix,iy), errflag)
              if (errflag /= TEMPTFILE_OK) goto 100
              irec = irec + 1
              call temptfile_read (obj%temptfile_veltab, irec, hdtmp, &
                                   obj%vrmsbuf(:,ix,iy), errflag)
              if (errflag /= TEMPTFILE_OK) goto 100
            end do
          end do
          obj%ix1mem = i1
          obj%ix2mem = i2
          obj%iy1mem = j1
          obj%iy2mem = j2
        end if
      end if

! Needed parts of tables are now in memory; do interpolation
      call interp_2d_var_lin_real (obj%xbins(i1:i2), obj%ybins(j1:j2), &
                                   obj%nxmem, obj%nymem, obj%vintbuf,  &
                                   obj%ndpt, xnow, ynow, obj%vintlast)
      call interp_2d_var_lin_real (obj%xbins(i1:i2), obj%ybins(j1:j2), &
                                   obj%nxmem, obj%nymem, obj%vrmsbuf,  &
                                   obj%ndpt, xnow, ynow, obj%vrmslast)
      obj%xlast = xnow
      obj%ylast = ynow
      return

 100  call pc_print ('AVAGRAD: Error reading velocity tables from &
                     &temporary disk file.')
      ntr = FATAL_ERROR
      return
      end subroutine avagrad_interpolate_vel

!!----------------------- avagrad_offset_to_angle -------------------------!!
!!----------------------- avagrad_offset_to_angle -------------------------!!
!!----------------------- avagrad_offset_to_angle -------------------------!!

      subroutine avagrad_offset_to_angle (obj, itime, hd, ntr, &
                                          sin2, sin2tan2)
      implicit none
      type(avagrad_struct),intent(in)  :: obj               ! arguments
      integer             ,intent(in)  :: itime             ! arguments
      double precision    ,intent(in)  :: hd(:,:)           ! arguments
      integer             ,intent(in)  :: ntr               ! arguments
      real                ,intent(out) :: sin2(:)           ! arguments
      real                ,intent(out) :: sin2tan2(:)       ! arguments

      integer       :: itrace                               ! local
      real          :: t0, t02, vint2, vrms2, vrms4         ! local
      real          :: x2, tnmo2, sin2temp                  ! local

      t0 = obj%tstrt + (itime-1)*obj%dt
      if (t0 > 0.0) then
        t02 = t0 ** 2
        vint2 = obj%vintlast(itime) ** 2
        vrms2 = obj%vrmslast(itime) ** 2
        vrms4 = vrms2 ** 2
        do itrace = 1, ntr
          x2 = hd(HDR_OFFSET,itrace) ** 2
          tnmo2 = t02 + x2/vrms2
          sin2(itrace) = x2 * vint2 / (tnmo2 * vrms4)
          sin2temp = min(sin2(itrace), obj%sin2max)
          sin2tan2(itrace) = sin2temp * sin2temp / (1.0 - sin2temp)
        end do
      else
        sin2(:ntr) = 2.0
        sin2tan2(:ntr) = 0.0
      end if

      return
      end subroutine avagrad_offset_to_angle

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine avagrad_wrapup (obj)
      implicit none
      type(avagrad_struct),intent(inout) :: obj       ! arguments
      integer                            :: ier1      ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call mem_free (obj%opr)
      call mem_free (obj%xbins)
      call mem_free (obj%ybins)
      call mem_free (obj%vintlast)
      call mem_free (obj%vrmslast)
      call mem_free (obj%vintbuf)
      call mem_free (obj%vrmsbuf)
      if (associated(obj%velio))   call velio_close     (obj%velio)
      if (associated(obj%temptfile_veltab))  &
                                   call temptfile_close (obj%temptfile_veltab)
      if (associated(obj%trcio))   ier1 = trcio_close   (obj%trcio)

      return
      end subroutine avagrad_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module avagrad_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
