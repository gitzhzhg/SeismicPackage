!<CPS_v1 type="PROCESS"/>
!!------------------------------- hsyn.f90 ---------------------------------!!
!!------------------------------- hsyn.f90 ---------------------------------!!
!!------------------------------- hsyn.f90 ---------------------------------!!

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
! Name       : HSYN       (Horizontally layered SYNthetic)
! Category   : synthetics
! Written    : 1989-04-15   by: Bob Baumel
! Revised    : 2007-01-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Produce AVO synthetics in X-T, TAU-P or ANGLE domain.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! HSYN generates primaries-only, pre-critical synthetics with AVO for a
! horizontally layered earth model, which can be specified in either of two
! ways: by providing a file containing well log data (or a sparse synthetic
! model), or by inputting a sparse synthetic model manually on screen. For file
! input, the specified file must contain rock properties consisting of
! Densities and P-wave and S-wave velocities or slownesses. HSYN can then
! generate the synthetic with reflection coefficients computed using either
! full Zoeppritz equations, Aki and Richards approximation, or Shuey
! approximation. In the manual on-screen input mode, instead of specifying rock
! properties, you specify A, B and C coefficients to use in the Aki and
! Richards formula:
!
!  Amplitude  =  A  +  B * sin(angle)^2  +  C * sin(angle)^2 * tan(angle)^2,
!
! which is the only available option for computing the reflection amplitudes in
! this case, and you also specify interval velocities to use in moveout and
! angle-offset calculations.
!
! The generated synthetic may be in either the x-t, tau-p or angle domain.
! When output is in tau-p or angle, you may also view the effect of limiting
! your data to a specified offset range in the original x-t domain. Similarly,
! when output is in x-t or tau-p, you may view the effect of limiting your
! data to a specified angle range.
!
! Several moveout options are available. For output in the x-t or tau-p
! domain, the default is to use exact (non-hyperbolic) ray-traced moveout as
! determined by your layered interval-velocity model. For output in x-t, you
! may also select hyperbolic moveout calculated from the RMS velocity
! corresponding to your specified interval velocity. For all output domains,
! you may also suppress moveout entirely, thereby producing perfectly flat
! events (When output is in the angle domain, you ALWAYS get flat events).
!
! Geometric spreading loss can be modeled when output is in the x-t domain.
! When calculating geometric divergence, you have the option to compute it for
! either a point-source (3-D spreading) or line-source (2-D spreading). By
! default, HSYN produces synthetics without geometric spreading decay.
!
! Events may include a Ricker wavelet of specified peak frequency, or you may
! model the events as spikes instead. Use of spike wavelets is, however, best
! limited to cases where moveout has been suppressed (so the events are flat).
! When synthetics include moveout, spike wavelets would result in discontinuous
! moveout curves, due to rounding of spike locations to seismic time samples.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
! Reading Input Model from a FILE
!
! If MODEL_SOURCE = FILE, you must specify a columnar ASCII file, containing
! well logs in LAS or similar format or perhaps a sparse synthetic model that
! you build yourself. This file must include at least four columns of numbers,
! containing the quantities:
!    Traveltime (two-way) in seconds or milliseconds,
!    Density,
!    P-wave slowness or velocity,
!    S-wave slowness or velocity.
! These columns may appear in any order, and the file may include additional
! columns (e.g., other well logs) which HSYN will ignore.
!
! The FIRSTLINE parameter lets you start reading at any desired line in the
! file, so you can ignore header information (as present in LAS or similar
! format files) and you can also ignore bad values at the top of your logs.
! HSYN reads from line FIRSTLINE to the end of the file. If there are bad
! values at the bottom of your logs that you want HSYN to ignore, you should
! remove them by editing your file prior to running HSYN.
!
! Setting the NILSTRING parameter can be tricky, as it needs to match the nil
! values in your file exactly as ASCII strings, not just numerically. For
! example, -999.25 and -999.2500 are not the same string. Regardless of your
! NILSTRING setting, HSYN assumes that traveltimes are always non-negative,
! while Densities, P-wave and S-wave values are always positive; therefore,
! any values in these columns that fail to satisfy these conditions are
! interpreted as nils.
!
! A traveltime column must be present so HSYN can place events at the correct
! seismic trace times. Traveltimes need not be sampled uniformly (e.g., the
! logs might still be sampled uniformly in depth), and may be sampled more
! finely than the seismic time samples. When reading your file, HSYN resamples
! logs to the seismic sample rate by binning log values into the intervals
! between successive seismic trace samples and averaging all values that fall
! into the same bin. (We place bins BETWEEN the seismic time samples, instead
! of centering them on the seismic time samples, so we can calculate
! reflectivities more accurately AT the seismic time samples.)
!
! P-wave and S-wave values in the file may be either velocities or slownesses
! but must be in CONSISTENT UNITS. If both are velocities (or both slownesses),
! they should be in the SAME units. If one is a velocity and the other a
! slowness, their units must match (e.g., velocity in meters per second and
! slowness in microseconds per meter).
!
! For raytrace calculations, HSYN uses the interval velocity defined by your
! P-wave log, at all depth levels where the P-wave log is present (whether or
! not the Density and S-wave logs are also present). If there are gaps in your
! P-wave log, Vp values are linearly interpolated to fill those gaps. If your
! P-wave log doesn't come all the way to the surface, your shallowest Vp value
! is extended, as a constant velocity layer, up to the surface.
!
! If you set LEN_SMOOTH > 0.0, the interval velocities derived from your Vp log
! are smoothed with a running average smoother. The resulting smoothed velocity
! is used only for the raytrace calculations, while reflectivities are still
! computed from your original UNSMOOTHED densities and velocities. This case
! does, however, require some additional subtleties in angle calculation; see
! discussion of "Reflection Angles" below.
!
! Reflection amplitudes are calculated only in intervals where all three of the
! logs (Density, P-wave and S-wave) are present. More precisely, reflections
! are calculated only where there are pairs of adjacent levels (after binning
! as explained above) where all three of these logs are present at both levels.
!
! (Warning: If the log sampling is so coarse that pairs of adjacent seismic
! bins are never both populated, HSYN will not calculate any reflections.)
!
! If you wish to build the ASCII file for a synthetic model with coarsely
! spaced interfaces that produce sharp reflections, then build each interface
! by entering a PAIR of rows separated by one seismic sample interval (DT) to
! produce the rock property contrasts at that interface. In particular, place
! these rows 0.5*DT above and 0.5*DT below the time where you want the
! reflection to appear. For example, if you wish to place reflections at
! 1.500 and 1.700 seconds, and the sample interval in your job is 0.002 s,
! enter rows describing rock properties at traveltimes 1.499, 1.501, 1.699 and
! 1.701 seconds.
!
!
! MANUAL On-screen Model Input
!
! If MODEL_SOURCE = MANUAL, the model is entered manually in the table on the
! second screen in the CFE module. Here, each row of the table denotes a
! REFLECTOR, specified by its (zero-offset, two-way) traveltime and its (A,B,C)
! AVO coefficients. The VEL_INT parameter specifies a (constant) interval
! velocity for the layer ABOVE this reflector, to be used in HSYN's raytrace
! calculations. Thus, VEL_INT denotes velocity between the specified reflector
! and the reflector immediately above it (or in the case of your shallowest
! reflector, between that reflector and the surface).
!
! If you put a reflector at time zero, it will behave as a direct arrival, and
! its VEL_INT value must match the VEL_INT of your second reflector (assuming
! your model has a second reflector). An event at time zero is visible only if
! OPT_DOMAIN = X_T, and its amplitude depends only on its A coefficient; i.e.,
! its B and C coefficients are ignored.
!
!
! Crossing Events
!
! When events cross in output from HSYN, their amplitudes combine by linear
! superposition, as they would in actual seismic data.
!
!
! Hyperbolic approximation
!
! When choosing OPT_MOVOUT = HYPER (available only when OPT_DOMAIN = X_T),
! the hyperbolic approximation is used ONLY for locating the event times.
! Calculation of angles (for use in Shuey approximation) as well as geometric
! divergence calculations are both still done using exact ray-tracing
!
!
! Reflection Angles
!
! All "reflection angles" in HSYN, whether outputting synthetics directly in
! the angle domain or limiting the angles of x-t and tau-p synthetics using the
! ANG_MIN and ANG_MAX parameters, are not simply the incident angle at the
! reflector, but are actually an AVERAGE of the incident and refracted angles.
! Actually, to simplify calculations, we average the SINES of these angles
! rather than the angles themselves. Thus, we take the relationship between
! reflection angle and ray parameter to be:
!                         sin(angle)  =  p * vavg
! where p is the ray parameter and vavg is the average interval velocity of
! the layers immediately above and below the reflector (Note: This approach
! is also used for ray-tracing in process AVAST).
!
! While using this "averaged" angle to define the "reflection angles" of all
! events, HSYN also keeps track of the individual incident and refracted angles
! and makes sure that all calculated events are strictly pre-critical; i.e.,
! that the incident and refracted angles are each real and less than 90
! degrees. When computing reflection amplitudes with the Aki and Richards or
! Shuey approximation, the "averaged" angle is used in the Aki and Richards or
! Shuey equation. When computing reflectivities with full Zoeppritz equations,
! the actual incident angle must be used in the calculation.
!
! Setting LEN_SMOOTH > 0 (when MODEL_SOURCE = FILE) adds another subtlety. In
! this case, we keep track of both the original, finely detailed interval
! velocities "vpfine" (after resampling to the seismic sample rate but before
! smoothing), as well as smoothed velocities "vpsmooth". Ray tracing is done
! using "vpsmooth" while reflectivities are calculated using "vpfine". This
! requires precision as to how all angles are specified. We use the following
! conventions: First, in all of the following, the ray parameter "p" is always
! found by raytracing using the SMOOTHED velocity "vpsmooth". For specifying
! angles of traces in the angle domain or limiting angles using the ANG_MIN
! and ANG_MAX parameters, we define reflection angles by:
!                     sin(angle)  =  p * vpsmooth_avg
! where vpsmooth_avg is the average of the vpsmooth values immediately above
! and below the reflector. For checking that reflections are pre-critical and
! for computing reflectivities using full Zoeppritz equations, we calculate
! the incident and refracted angles by:
!                  sin(angle_incident)  =  p * vpfine_above
!                  sin(angle_refracted) =  p * vpfine_below
! where vpfine_above and vpfine_below are the vpfine values immediately above
! and below the reflector. For calculating reflectivities using the Aki and
! Richards or Shuey approximation, we use the angle defined by:
!                      sin(angle)  =  p * vpfine_avg
! where vpfine_avg is the average of the vpfine values immediately above and
! below the reflector.
!
!
! Limiting offsets in TAU-P or ANGLE synthetics
!
! The OFF_MIN and OFF_MAX parameters can limit the offset range when output
! is in the TAU-P or ANGLE domain. Setting OFF_MAX = 0 will allow arbitrarily
! large offsets. Even when doing this, the maximum ray parameters or angles in
! your tau-p or angle synthetics will be limited. This happens because HSYN
! cannot ray-trace through layers where propagation would be evanescent; i.e.,
! where the ray parameter (p) would exceed 1/VEL_INT or, equivalently, where
! sin(angle) would be greater than 1.0 (In a full-wave synthetic, some energy
! may "tunnel" through VERY THIN layers where propagation is evanescent; in a
! ray-traced synthetic, NOTHING gets through layers where propagation is
! evanescent).
!
!
! Ricker wavelets
!
! When specifying a Ricker wavelet (PEAK_FREQ > 0.0), the wavelet is given by:
!               (1.0 - A*(T-T0)**2) * EXP(-0.5*A*(T-T0)**2)
! where T = time, T0 = time at center of wavelet, and A is a constant given by:
!                     A = 2.0 * (PI * PEAK_FREQ)**2
! This wavelet is normalized to unit amplitude at its center (T = T0).
!
! If PEAK_FREQ = 0.0, the wavelet is a spike of unit amplitude, placed at the
! time sample nearest the calculated event time. This gives you the option to
! apply a wavelet later (if you wish) using GENFILT or other suitable process.
! We suggest, however, that this method be used only when OPT_MOVOUT = NONE.
! If you use spike wavelets when your synthetics include moveout, you'll get
! discontinuous moveout curves, due to the rounding necessary to place the
! spikes at seismic time samples.
!
!
! Comparison with Angle Synthetics from PSCOVAR
!
! HSYN and PSCOVAR can both read ASCII files of well log data and can both
! generate synthetics in the angle domain and can both use the Aki & Richards
! approximation, so it may be of interest to compare them. To obtain comparable
! results with HSYN and PSCOVAR, be sure to set LEN_SMOOTH = 0 in HSYN. There
! may still be some very minor differences due to rounding effects.
! Occasionally, due to rounding when resampling the well logs, a time shift of
! at most one seismic sample can occur (particularly in jobs where the TSTRT
! global is non-zero). In general, however, they should line up and match very
! well, with one major exception: For events at large angles, an HSYN synthetic
! may cut off at angles less than the maximum requested, while the PSCOVAR
! result includes every requested angle (up to the limit of 85 degrees hard-
! wired into both programs).
!
! This difference occurs because HSYN always does ray tracing, based on an
! interval velocity model, and will not model an event unless it is
! pre-critical and reachable by rays traced from the surface. It can happen,
! for example, that a high-velocity layer between the surface and a desired
! reflector makes it impossible to reach the reflector at larger angles. HSYN
! will not calculate such events. PSCOVAR does no such checking, but simply
! calculates (Aki and Richards) reflection amplitudes for every reflector at
! every requested angle.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! NO input traces -- HSYN is a trace supplying process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs one trace at a time.
! This is a trace-supplying process.
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
! GATHERED  whether traces are a legitimate gather  set to .false.
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
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Set
! 2       Head mute                  Set
! 3       Current group number       Set
! 4       Trace # within group       Set
! 25      Largest absolute value     Set
! 64      Tail mute                  Set
!         HDR_LINE (default 8)       Set
!         HDR_CMP  (default 7)       Set
!         HDR_OFF  (default 6)       Set
! All other headers set to zero.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
! 14. 2007-01-03  Stoeckley   Add call to pc_register_array_names for SeisSpace.
! 13. 2006-04-20  Baumel      Add ability to smooth log-derived Vp for raytrace
!                             calculations (but still compute reflectivities
!                             from *unsmoothed* densities and velocities). Also
!                             add check to ignore log values with nonphysical
!                             Vs/Vp ratios when calculating reflectivities.
! 12. 2006-04-11  Baumel      Add ability to read a layered earth model as
!                             Density, Vp, Vs from an ASCII file (e.g., well
!                             log file) and, in this case, provide choice of
!                             reflectivities by Zoeppritz, Aki and Richards or
!                             Shuey approximation. For manual on-screen model
!                             input, accept the three Aki and Richards AVO
!                             coefficients: A, B, C (not backward compatible
!                             with previous parameter names AMP and GRAD, now
!                             renamed A and B). Add ability to limit x-t and
!                             tau-p synthetics to a desired angle range. If
!                             outputting more than one gather, compute the
!                             gather only once and save in memory to output
!                             repeatedly.
! 11. 2001-03-30  Baumel      Double precision much of calculation and add
!                             tests to avoid infinite loop in raytrace solver
!                             due to precision problem at very large offsets.
! 10. 2001-03-13  Baumel      GUI change: Dynamically adjust options list for
!                             OPT_MOVOUT combo box based on OPT_DOMAIN setting
!                             (Required fix in CFE program to implement this).
!  9. 2001-03-12  Bob Baumel  Converted from old system (adding several options
!                             which were in private versions in my VAX account
!                             but never in the installed version on Cray).
!  8. 1998-11-13  Vunderink   Begin using the f90 compiler.
!  7. 1997-01-16  Baumel      Add GRAD parameter for simple AVO synthetics.
!  6. 1994-04-26  Baumel      Faster solver for plane-layer ray tracing.
!  5. 1994-01-26  Baumel      Fix bug involving small values of WVLT_SIZ;
!                             generate spike wavelet if WVLT_SIZ = 0.
!  4. 1990-10-22  Baumel      Avoid generating EXTREMELY TINY amplitudes.
!  3. 1990-04-26  Baumel      Generate multiple copies of gather.
!  2. 1989-04-24  Baumel      Add XMINP,XMAXP parameters.
!  1. 1989-04-15  Bob Baumel  Original version.
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
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting a trace.
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
!<NS HSYN Process/NC=80>
!
!    Horizontally layered SYNthetic with AVO in X-T, TAU-P or ANGLE domain
!
!         OPT_DOMAIN=`CCCC     OPT_MOVOUT=`CCCC     OPT_GEOMDIV=`CCCC
!
! HDR_LINE=`IIIIII   LINE_INIT=`FFFFFF   LINE_INC=`FFFFFF   LINE_TOT=`IIIIII
! HDR_CMP= `IIIIII   CMP_INIT= `FFFFFF   CMP_INC= `FFFFFF   CMP_TOT= `IIIIII
! HDR_OFF= `IIIIII   OFF_INIT= `FFFFFF   OFF_INC= `FFFFFF   OFF_TOT= `IIIIII
!
! ANG_MIN= `FFFFFF   ANG_MAX=~~`FFFFFF   OFF_MIN= `FFFFFF   OFF_MAX= `FFFFFF
!
!              PEAK_FREQ=`FFFFFF         MODEL_SOURCE=`CCCCCC
!
!       Parameters to specify layered earth model if MODEL_SOURCE = FILE
! Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! NUM_COLUMNS=`II           FIRSTLINE=`IIII          NILSTRING=`SSSSSSSSSSSSSS
! TIME_COLUMN=`II    DENSITY_COLUMN=`II    PWAVE_COLUMN=`II    SWAVE_COLUMN=`II
! TIME_UNITS=`CCCCCCCCCCCCC       PWAVE_TYPE=`CCCCCCCCC   SWAVE_TYPE=`CCCCCCCCC
!           REF_COEFF=`CCCCCCCCCCCCC   LEN_SMOOTH=`FFFFFFFFF (seconds)
!
!           If MODEL_SOURCE = MANUAL, please go to the second screen
!        (Manual Model Entry) to enter your layered earth model by hand.
!<PARMS PATHNAME[/ML=140/XST]>
!<PARMS PATHNAME_INFO[/ML=140/XST]>
!<NS Manual Model Entry>
!
!     If MODEL_SOURCE = MANUAL, please enter your layered earth
!                   model using the table below.
!
!         TIME      VEL_INT   A         B         C
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!         `FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!
!<PARMS TIME_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_DOMAIN">
!<Tip> Whether synthetics will be in X-T, TAU-P or ANGLE domain. </Tip>
! Default = X_T
! Allowed = X_T    (Offset vs. Time)
! Allowed = TAU_P  (Ray Parameter vs. Intercept Time)
! Allowed = ANGLE  (Reflection Angle vs. Zero-Offset Time)
!</Help>
!
!<Help KEYWORD="OPT_MOVOUT">
!<Tip> Whether events will have exact, hyperbolic or no moveout. </Tip>
! Default = EXACT
! Allowed = EXACT  (Exact moveout for horizontally layered earth)
! Allowed = HYPER  (Hyperbolic moveout based on RMS velocity)
! Allowed = NONE   (No moveout - events will be perfectly flat)
!
! If OPT_MOVOUT = EXACT, events will have exact, non-hyperbolic (ray-traced)
! moveout according to your layered interval-velocity model. This option is
! available when OPT_DOMAIN = X_T or TAU_P.
!
! If OPT_MOVOUT = HYPER, events will have hyperbolic moveout according to
! the RMS velocity corresponding to your layered interval-velocity. Such
! moveout can be removed exactly by the NMO process, although you'd see NMO
! stretch after applying NMO. The OPT_MOVOUT = HYPER option is available
! only when OPT_DOMAIN = X_T.
!
! If OPT_MOVOUT = NONE, events will be perfectly flat, as if ideal NMO
! correction (without the usual NMO stretch) had been applied. The
! OPT_MOVOUT = NONE option is available for all OPT_DOMAIN values (and
! is, in fact, the ONLY available moveout option when OPT_DOMAIN = ANGLE).
!</Help>
!
!<Help KEYWORD="OPT_GEOMDIV">
!<Tip> Type of geometric divergence to model if OPT_DOMAIN = X_T. </Tip>
! Default = NONE
! Allowed = NONE  - No decay due to geometric spreading.
! Allowed = POINT - Include point source (3-D) geometric spreading loss.
! Allowed = LINE  - Include line source (2-D) geometric spreading loss.
! If OPT_GEOMDIV = NONE, synthetics will not include any amplitude decay due
! to geometric spreading.
!
! If OPT_GEOMDIV = POINT, synthetics will include the correct geometric
! spreading decay for a point-source in three dimensions, according to your
! layered interval-velocity model. This is just (1/r) decay in the case of
! constant velocity.
!
! If OPT_GEOMDIV = LINE, synthetics will include the correct geometric
! spreading decay for a point-source in two dimensions (equivalent to a
! line-source in three dimensions when data is collected in a plane
! perpendicular to that line-source). This is (1/SQRT(r)) decay in the
! case of constant velocity. Synthetics generated this way may be useful
! in testing algorithms based on a 2-D wave equation.
!
! Active only if OPT_DOMAIN = X_T.
!</Help>
!
!<Help KEYWORD="HDR_LINE">
!<Tip> Header word designating lines. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="LINE_INIT">
!<Tip> Initial value for line header word. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="LINE_INC">
!<Tip> Increment for line header word. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="LINE_TOT">
!<Tip> Total number of line header word values. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="HDR_CMP">
!<Tip> Header word designating CMPs. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="CMP_INIT">
!<Tip> Initial value for CMP header word. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="CMP_INC">
!<Tip> Increment for CMP header word. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="CMP_TOT">
!<Tip> Total number of CMP header word values. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word designating offsets, ray parameters or angles. </Tip>
! Default = 6
! Allowed = 1 - NWIH
! If OPT_DOMAIN = X_T, header word HDR_OFF will contain offsets in meters
! or feet.
! If OPT_DOMAIN = TAU_P, header word HDR_OFF will contain ray parameters in
! MICROseconds per meter or foot.
! If OPT_DOMAIN = ANGLE, header word HDR_OFF will contain angles in degrees.
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Initial value for offset, ray parameter or angle. </Tip>
! Default = 0.0
! Allowed = real
! If OPT_DOMAIN = X_T, then OFF_INIT is initial offset in meters or feet.
! If OPT_DOMAIN = TAU_P, then OFF_INIT is the initial ray parameter in
! MICROseconds per meter or foot.
! If OPT_DOMAIN = ANGLE, then OFF_INIT is the initial angle in degrees.
!
! It is legitimate to make OFF_INIT negative, if you wish, in order to
! generate split-spread data with signed offsets.
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment for offset, ray parameter or angle. </Tip>
! Default = 50.0
! Allowed = real  (must be non-zero if OPT_DOMAIN = X_T)
! If OPT_DOMAIN = X_T, then OFF_INC is offset increment in meters or feet.
! If OPT_DOMAIN = TAU_P, then OFF_INC is the ray parameter increment in
! MICROseconds per meter or foot.
! If OPT_DOMAIN = ANGLE, then OFF_INC is the angle increment in degrees.
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offsets, ray parameters or angles. </Tip>
! Default = 100
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="ANG_MIN">
!<Tip> Minimum angle to include when OPT_DOMAIN = X_T or TAU_P. </Tip>
! Default = 0.0
! Allowed = 85.0 > real >= 0.0
! If OPT_DOMAIN = X_T or TAU_P, you can use ANG_MIN and ANG_MAX to limit your
! synthetic to a specified range of reflection angles. Here, ANG_MIN specifies
! the minimum angle, in degrees, to include in your x-t or tau-p synthetic.
!
! Active only if OPT_DOMAIN = X_T or TAU_P.
!</Help>
!
!<Help KEYWORD="ANG_MAX">
!<Tip> Maximum angle to include when OPT_DOMAIN = X_T or TAU_P. </Tip>
! Default = 85.0
! Allowed = 85.0 >= real > ANG_MIN
! If OPT_DOMAIN = X_T or TAU_P, you can use ANG_MIN and ANG_MAX to limit your
! synthetic to a specified range of reflection angles. Here, ANG_MAX specifies
! the maximum angle, in degrees, to include in your x-t or tau-p synthetic.
!
! HSYN will not compute reflection amplitudes for angles greater than 85
! degrees because the  C * sin(angle)^2 * tan(angle)^2  term in the Aki and
! Richards formula blows up as the angle approaches 90 degrees.
!
! Active only if OPT_DOMAIN = X_T or TAU_P.
!</Help>
!
!<Help KEYWORD="OFF_MIN">
!<Tip> Minimum offset to include when OPT_DOMAIN = TAU_P or ANGLE. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! If OPT_DOMAIN = TAU_P or ANGLE, you can use OFF_MIN and OFF_MAX to show the
! effect of a limited offset range in the original X_T domain. Here, OFF_MIN
! specifies the minimum offset to include in your tau-p or angle synthetics.
!
! If OFF_MIN = 0.0, arbitrarily small (absolute values of) offsets will be
! allowed in the tau-p or angle synthetics.
!
! Active only if OPT_DOMAIN = TAU_P or ANGLE.
!</Help>
!
!<Help KEYWORD="OFF_MAX">
!<Tip> Maximum offset to include when OPT_DOMAIN = TAU_P or ANGLE. </Tip>
! Default = 0.0
! Allowed = 0.0 or real > OFF_MIN
! If OPT_DOMAIN = TAU_P or ANGLE, you can use OFF_MIN and OFF_MAX to show the
! effect of a limited offset range in the original X_T domain. Here, OFF_MAX
! specifies the maximum offset to include in your tau-p or angle synthetics.
!
! If OFF_MAX = 0.0, equivalent to setting it infinite, arbitrarily large
! (absolute values of) offsets are allowed in the tau-p or angle synthetics.
!
! Active only if OPT_DOMAIN = TAU_P or ANGLE.
!</Help>
!
!<Help KEYWORD="PEAK_FREQ">
!<Tip> Peak frequency, in Hz, of Ricker wavelet for synthetic traces. </Tip>
! Default = 25.0
! Allowed = real >= 0.0
! If PEAK_FREQ > 0.0, this is the peak frequency, in Hz, of a Ricker wavelet.
! These Ricker wavelets are generated on the fly for each event in each trace,
! so they are centered exactly on calculated event times (which are generally
! between trace samples). The resulting events follow smooth moveout curves.
!
! If PEAK_FREQ = 0.0, the wavelets will be SPIKES placed at the trace samples
! nearest the calculated event times. This allows the possibility of applying
! a wavelet later, using GENFILT or other suitable process. Use of spike
! wavelets is, however, best limited to cases where OPT_MOVOUT = NONE. If your
! synthetics have moveout, rounding of spikes to trace sample times would
! result in discontinuous moveout curves.
!</Help>
!
!<Help KEYWORD="MODEL_SOURCE">
!<Tip> Whether layered model is read from a file or entered manually. </Tip>
! Default = MANUAL
! Allowed = FILE
! Allowed = MANUAL
! If MODEL_SOURCE = FILE, enter your model file using the PATHNAME parameter
! and specify related parameters. This must be an ASCII file containing
! densities, P velocities/slownesses and S velocities/slownesses, from either
! a well log or synthetic model, in the same format as input to the PSCOVAR
! process.
!
! If MODEL_SOURCE = MANUAL, click the Manual Model Entry tab to go the second
! screen, and fill in the table to specify a model by traveltimes, interval (P)
! velocities and A,B,C (AVO) coefficients.
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Input file containing layered earth model if MODEL_SOURCE = FILE. </Tip>
! Default = NONE
! Allowed = valid file name.
!
! If MODEL_SOURCE = FILE, you must specify an input file containing well log
! data or a synthetic model. This must be an ASCII file containing four or more
! columns of numbers (same format as read by process PSCOVAR). The 4 required
! columns consist of Two-way traveltime, Density, P-wave velocity or slowness,
! and S-wave velocity or slowness. The file may have additional columns which
! HSYN will ignore (see NUM_COLUMNS parameter) and header information that HSYN
! will ignore (see FIRSTLINE parameter).
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="PATHNAME_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="NUM_COLUMNS">
!<Tip> Number of columns in the file specified by PATHNAME. </Tip>
! Default = 4
! Allowed = integer >= 4.
!
! NUM_COLUMNS is the total number of columns in your file. The file must have
! at least four columns (Traveltime, Density, P-wave velocity or slowness, and
! S-wave velocity or slowness). It may have additional columns, such as other
! well logs: These will be ignored when generating the synthetic; however, you
! must specify the total number of columns in order to enable HSYN to read the
! file correctly.
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="FIRSTLINE">
!<Tip> Row on file to start reading. </Tip>
! Default = 1
! Allowed = integer >= 1
!
! This parameter allows skipping header information, as may be present in an
! LAS or similar format file, and you may also set FIRSTLINE to start reading
! deeper in order to ignore bad values at the top of your logs.
!
! Note: HSYN reads from line FIRSTLINE to the end of the file. If there are
! bad values at the bottom of your logs that you want HSYN to ignore, remove
! them by editing your file before running HSYN.
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="NILSTRING">
!<Tip> Symbol for nil value on file. </Tip>
! Default = -999.2500
! Allowed = any character string containing no blank characters.
!
! WARNING: This is a CHARACTER STRING, not a number, so it must match the nil
! string in your file EXACTLY (e.g., -999.2500 isn't the same as -999.25).
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="TIME_COLUMN">
!<Tip> Column number containing (two-way) Traveltime values in file. </Tip>
! Default = 1
! Allowed = 1 - NUM_COLUMNS
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="DENSITY_COLUMN">
!<Tip> Column number containing Density values in file. </Tip>
! Default = 2
! Allowed = 1 - NUM_COLUMNS
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="PWAVE_COLUMN">
!<Tip> Column number in file containing P-Wave slowness or velocity. </Tip>
! Default = 3
! Allowed = 1 - NUM_COLUMNS
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="SWAVE_COLUMN">
!<Tip> Column number in file containing S-Wave slowness or velocity. </Tip>
! Default = 4
! Allowed = 1 - NUM_COLUMNS
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="TIME_UNITS">
!<Tip> Units of times in TIME_COLUMN on file. </Tip>
! Default = MILLISECONDS
! Allowed = MILLISECONDS or SECONDS
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="PWAVE_TYPE">
!<Tip> Whether P-Wave values in file are Slowness or Velocity. </Tip>
! Default = SLOWNESS
! Allowed = SLOWNESS or VELOCITY
!
! P-Wave and S-Wave values may be either velocities in meters or feet per
! second, or slownesses in MICROseconds per meter or foot. It is acceptable
! for P-Wave values to be slowness while S-Wave values are velocity (or vice
! versa) as long as both are in meters or both in feet.
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="SWAVE_TYPE">
!<Tip> Whether S-Wave values in file are Slowness or Velocity. </Tip>
! Default = SLOWNESS
! Allowed = SLOWNESS or VELOCITY
!
! P-Wave and S-Wave values may be either velocities in meters or feet per
! second, or slownesses in MICROseconds per meter or foot. It is acceptable
! for P-Wave values to be slowness while S-Wave values are velocity (or vice
! versa) as long as both are in meters or both in feet.
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="REF_COEFF">
!<Tip> Whether to use Zoeppritz, Aki and Richards or Shuey approximation.</Tip>
! Default = AKI_RICHARDS
! Allowed = ZOEPPRITZ, AKI_RICHARDS or SHUEY
! Specifies whether reflection coefficients will be computed from densities
! and velocities using the Zoeppritz equations, Aki & Richards approximation,
! or Shuey approximation.
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="LEN_SMOOTH">
!<Tip> Length (in seconds) of smoother for raytrace velocities. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! Length, in seconds, of running average smoother applied to log-derived Vp
! values used for ray tracing. This is analogous to LEN_SMOOTH in AVAST, where
! a smoothed velocity is usually used for the ray tracing to determine the
! relationship between offsets and angles. In HSYN, the resulting smoothed
! velocity is used ONLY for the raytrace calculations, while the original
! (unsmoothed) Vp values are always used for calculating reflectivities.
! If LEN_SMOOTH = 0.0, then no smoothing is done.
!
! Active only if MODEL_SOURCE = FILE.
!</Help>
!
!<Help KEYWORD="TIME">
!<Tip> Array of two-way vertical travel times for events (reflections). </Tip>
! Default = -
! Allowed = real linked array
!
! These are the (zero-offset) times of the events (reflections) in your model.
! Event times (in seconds) must be non-negative and in increasing order.
!
! Normally, reflection events will have positive TIME values. If you include
! an event at time zero, it will behave as a direct arrival. In this case,
! VEL_INT(1) should be the same as VEL_INT(2); also, as AVO is not meaningful
! for such an event (because the angle is always 90 degrees at all offsets),
! its amplitude will be determined from only its A coefficient, ignoring its
! B and C coefficients.
!
! Active only if MODEL_SOURCE = MANUAL.
!</Help>
!
!<Help KEYWORD="VEL_INT">
!<Tip> Interval velocity of layer immediately above specified TIME. </Tip>
! Default = -
! Allowed = real linked array
!
! Use this array to enter a layered interval velocity model. On the row
! containing the (i)th reflector, enter VEL_INT as the velocity of the
! layer between the (i-1)th and (i)th reflectors.
!
! For special case of the first row: If TIME(1) > 0.0, then VEL_INT(1) is
! interval velocity of the layer between TIME(1) and the surface.
! If TIME(1) = 0.0, then VEL_INT(1) should be the same as VEL_INT(2).
!
! Active only if MODEL_SOURCE = MANUAL.
!</Help>
!
!<Help KEYWORD="A">
!<Tip> Zero-offset amplitude (AVA Intercept) of event at specified TIME. </Tip>
! Default = -
! Allowed = real linked array
! Event amplitudes for non-zero offsets are determined by A, B and C as:
!   Amplitude  =  A  +  B * sin(angle)^2  +  C * sin(angle)^2 * tan(angle)^2.
! where "angle" is the reflection angle (or, more precisely, the average of
! reflection and refraction angles) at the specified event.
!
! Active only if MODEL_SOURCE = MANUAL.
!</Help>
!
!<Help KEYWORD="B">
!<Tip> AVA Gradient of the event at specified TIME. </Tip>
! Default = -
! Allowed = real linked array
! Event amplitudes for non-zero offsets are determined by A, B and C as:
!   Amplitude  =  A  +  B * sin(angle)^2  +  C * sin(angle)^2 * tan(angle)^2.
! where "angle" is the reflection angle (or, more precisely, the average of
! reflection and refraction angles) at the specified event.
!
! Active only if MODEL_SOURCE = MANUAL.
!</Help>
!
!<Help KEYWORD="C">
!<Tip> AVA coefficient C of the event at specified TIME. </Tip>
! Default = -
! Allowed = real linked array
! Event amplitudes for non-zero offsets are determined by A, B and C as:
!   Amplitude  =  A  +  B * sin(angle)^2  +  C * sin(angle)^2 * tan(angle)^2.
! where "angle" is the reflection angle (or, more precisely, the average of
! reflection and refraction angles) at the specified event.
!
! Active only if MODEL_SOURCE = MANUAL.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module hsyn_module
      use pc_module
      use named_constants_module
      use mem_module
      use pathchoose_module
      use pathcheck_module
      use floatio_module
      use terputil_module
      use mth_module
      use zoeppritz_module
      use lav_module

      implicit none
      private
      public :: hsyn_create
      public :: hsyn_initialize
      public :: hsyn_update
      public :: hsyn_delete
      public :: hsyn            ! main execution (trace processing) routine.
      public :: hsyn_wrapup

      character(len=100),public,save :: HSYN_IDENT = &
'$Id: hsyn.f90,v 1.14 2007/01/03 14:01:40 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: hsyn_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.
        character(len=5)           :: opt_domain       ! process parameters.
        character(len=5)           :: opt_movout       ! process parameters.
        character(len=5)           :: opt_geomdiv      ! process parameters.
        integer                    :: hdr_line         ! process parameters.
        real                       :: line_init        ! process parameters.
        real                       :: line_inc         ! process parameters.
        integer                    :: line_tot         ! process parameters.
        integer                    :: hdr_cmp          ! process parameters.
        real                       :: cmp_init         ! process parameters.
        real                       :: cmp_inc          ! process parameters.
        integer                    :: cmp_tot          ! process parameters.
        integer                    :: hdr_off          ! process parameters.
        real                       :: off_init         ! process parameters.
        real                       :: off_inc          ! process parameters.
        integer                    :: off_tot          ! process parameters.
        real                       :: ang_min, ang_max ! process parameters.
        real                       :: off_min, off_max ! process parameters.
        real                       :: peak_freq        ! process parameters.
        character(len=8)           :: model_source     ! process parameters.
        character(len=FILENAME_LENGTH) :: pathname     ! process parameters.
        integer                    :: num_columns      ! process parameters.
        integer                    :: firstline        ! process parameters.
        character(len=20)          :: nilstring        ! process parameters.
        integer                    :: time_column      ! process parameters.
        integer                    :: density_column   ! process parameters.
        integer                    :: pwave_column     ! process parameters.
        integer                    :: swave_column     ! process parameters.
        character(len=20)          :: time_units       ! process parameters.
        character(len=20)          :: pwave_type       ! process parameters.
        character(len=20)          :: swave_type       ! process parameters.
        character(len=20)          :: ref_coeff        ! process parameters.
        real                       :: len_smooth       ! process parameters.
        real              ,pointer :: time(:)          ! process parameters.
        real              ,pointer :: vel_int(:)       ! process parameters.
        real              ,pointer :: a(:), b(:), c(:) ! process parameters.

        integer                    :: nwih, ndpt       ! global parameters.
        real                       :: dt, tstrt        ! global parameters.

        type(pathchoose_struct),pointer :: pathchoose  ! dependent variables.
        integer                    :: nref, ngrp, ntot ! dependent variables.
        integer                    :: ilin, icmp, ioff ! dependent variables.
        integer                    :: nsmooth          ! dependent variables.
        real                       :: wvltsiz, tdelt   ! dependent variables.
        real                       :: sinangmin        ! dependent variables.
        real                       :: sinangmax        ! dependent variables.
        real                       :: sinangfinemax    ! dependent variables.
        real              ,pointer :: vpfine(:)        ! dependent variables.
        real              ,pointer :: density(:)       ! dependent variables.
        real              ,pointer :: vshear(:)        ! dependent variables.
        double precision  ,pointer :: dtim(:)          ! dependent variables.
        double precision  ,pointer :: dtim2(:)         ! dependent variables.
        double precision  ,pointer :: pmax(:)          ! dependent variables.
        double precision  ,pointer :: vp2(:)           ! dependent variables.
        double precision  ,pointer :: tvp2(:)          ! dependent variables.
        double precision  ,pointer :: t2vp2(:)         ! dependent variables.
        double precision  ,pointer :: vrms2(:)         ! dependent variables.
        logical                    :: save_gather      ! dependent variables.
        double precision  ,pointer :: hdsave(:,:)      ! dependent variables.
        real              ,pointer :: trsave(:,:)      ! dependent variables.

      end type hsyn_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(hsyn_struct),pointer,save :: object      ! needed for traps.

      real, parameter :: ANGLE_BIG = 85.0
      real, parameter :: VSVPMAX = 0.866

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine hsyn_create (obj)
      type(hsyn_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%time)
      nullify (obj%vel_int)
      nullify (obj%a)
      nullify (obj%b)
      nullify (obj%c)
      nullify (obj%vpfine)
      nullify (obj%density)
      nullify (obj%vshear)
      nullify (obj%dtim)
      nullify (obj%dtim2)
      nullify (obj%pmax)
      nullify (obj%vp2)
      nullify (obj%tvp2)
      nullify (obj%t2vp2)
      nullify (obj%vrms2)
      nullify (obj%hdsave)
      nullify (obj%trsave)
      nullify (obj%pathchoose)

      call pathchoose_create (obj%pathchoose, 'PATHNAME', '*')

      call hsyn_initialize (obj)
      end subroutine hsyn_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine hsyn_delete (obj)
      type(hsyn_struct),pointer :: obj       ! arguments

      call hsyn_wrapup (obj)

      call mem_free (obj%time)
      call mem_free (obj%vel_int)
      call mem_free (obj%a)
      call mem_free (obj%b)
      call mem_free (obj%c)
      call mem_free (obj%vpfine)
      call mem_free (obj%density)
      call mem_free (obj%vshear)
      call mem_free (obj%dtim)
      call mem_free (obj%dtim2)
      call mem_free (obj%pmax)
      call mem_free (obj%vp2)
      call mem_free (obj%tvp2)
      call mem_free (obj%t2vp2)
      call mem_free (obj%vrms2)
      call mem_free (obj%hdsave)
      call mem_free (obj%trsave)
      call pathchoose_delete (obj%pathchoose)

      deallocate(obj)
      end subroutine hsyn_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine hsyn_initialize (obj)
      type(hsyn_struct),intent(inout) :: obj       ! arguments

      obj%opt_domain     = 'X_T'
      obj%opt_movout     = 'EXACT'
      obj%opt_geomdiv    = 'NONE'
      obj%hdr_line       = 8
      obj%line_init      = 0.0
      obj%line_inc       = 1.0
      obj%line_tot       = 1
      obj%hdr_cmp        = 7
      obj%cmp_init       = 0.0
      obj%cmp_inc        = 1.0
      obj%cmp_tot        = 1
      obj%hdr_off        = 6
      obj%off_init       = 0.0
      obj%off_inc        = 50.0
      obj%off_tot        = 100
      obj%ang_min        = 0.0
      obj%ang_max        = ANGLE_BIG
      obj%off_min        = 0.0
      obj%off_max        = 0.0
      obj%peak_freq      = 25.0
      obj%model_source   = 'MANUAL'
      obj%pathname       = PATHCHECK_EMPTY
      obj%num_columns    = 4
      obj%firstline      = 1
      obj%nilstring      = '-999.2500'
      obj%time_column    = 1
      obj%density_column = 2
      obj%pwave_column   = 3
      obj%swave_column   = 4
      obj%time_units     = 'MILLISECONDS'
      obj%pwave_type     = 'SLOWNESS'
      obj%swave_type     = 'SLOWNESS'
      obj%ref_coeff      = 'AKI_RICHARDS'
      obj%len_smooth     = 0.0
      obj%nref           = 0

      call hsyn_update (obj)
      end subroutine hsyn_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine hsyn_update (obj)
      type(hsyn_struct),intent(inout),target :: obj              ! arguments

      integer          :: update_state, n1, n2, n3, n4, n5, iref ! local
      integer          :: ndptfile                               ! local
      logical          :: movout_sensitive, geomdiv_sensitive    ! local
      double precision :: sumtvp2                                ! local
      character(len=5) :: opt_domain_prev                        ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update(obj%pathchoose,obj%pathname)) return
      call pc_get_global ('NWIH'  , obj%nwih)
      call pc_get_global ('NDPT'  , obj%ndpt)
      call pc_get_global ('TSTRT' , obj%tstrt)
      call pc_get_global ('DT'    , obj%dt)

      call pc_register_array_names ("time_arrayset", (/  &
                                    "time   ",           &
                                    "vel_int",           &
                                    "a      ",           &
                                    "b      ",           &
                                    "c      " /))

      opt_domain_prev = obj%opt_domain
      update_state = pc_get_update_state()

      call pc_get ('OPT_DOMAIN'    , obj%opt_domain)
      call pc_get ('OPT_MOVOUT'    , obj%opt_movout)
      call pc_get ('OPT_GEOMDIV'   , obj%opt_geomdiv)
      call pc_get ('HDR_LINE'      , obj%hdr_line)
      call pc_get ('LINE_INIT'     , obj%line_init)
      call pc_get ('LINE_INC'      , obj%line_inc)
      call pc_get ('LINE_TOT'      , obj%line_tot)
      call pc_get ('HDR_CMP'       , obj%hdr_cmp)
      call pc_get ('CMP_INIT'      , obj%cmp_init)
      call pc_get ('CMP_INC'       , obj%cmp_inc)
      call pc_get ('CMP_TOT'       , obj%cmp_tot)
      call pc_get ('HDR_OFF'       , obj%hdr_off)
      call pc_get ('OFF_INIT'      , obj%off_init)
      call pc_get ('OFF_INC'       , obj%off_inc)
      call pc_get ('OFF_TOT'       , obj%off_tot)
      call pc_get ('ANG_MIN'       , obj%ang_min)
      call pc_get ('ANG_MAX'       , obj%ang_max)
      call pc_get ('OFF_MIN'       , obj%off_min)
      call pc_get ('OFF_MAX'       , obj%off_max)
      call pc_get ('PEAK_FREQ'     , obj%peak_freq)
      call pc_get ('MODEL_SOURCE'  , obj%model_source)
      call pc_get ('PATHNAME'      , obj%pathname)
      call pc_get ('NUM_COLUMNS'   , obj%num_columns)
      call pc_get ('FIRSTLINE'     , obj%firstline)
      call pc_get ('NILSTRING'     , obj%nilstring)
      call pc_get ('TIME_COLUMN'   , obj%time_column)
      call pc_get ('DENSITY_COLUMN', obj%density_column)
      call pc_get ('PWAVE_COLUMN'  , obj%pwave_column)
      call pc_get ('SWAVE_COLUMN'  , obj%swave_column)
      call pc_get ('TIME_UNITS'    , obj%time_units)
      call pc_get ('PWAVE_TYPE'    , obj%pwave_type)
      call pc_get ('SWAVE_TYPE'    , obj%swave_type)
      call pc_get ('REF_COEFF'     , obj%ref_coeff)
      call pc_get ('LEN_SMOOTH'    , obj%len_smooth)
      n1 = obj%nref ; n2 = obj%nref
      n3 = obj%nref ; n4 = obj%nref; n5 = obj%nref
      call pc_alloc ('TIME'        , obj%time   , n1)
      call pc_alloc ('VEL_INT'     , obj%vel_int, n2)
      call pc_alloc ('A'           , obj%a      , n3)
      call pc_alloc ('B'           , obj%b      , n4)
      call pc_alloc ('C'           , obj%c      , n5)
      if (n2 /= n1 .or. n3 /= n1 .or. n4 /= n1 .or. n5 /= n1) then
        call pc_error ('TIME, VEL_INT, A, B and C arrays have different &
                       &lengths.')
        obj%nref = min (n1, n2, n3, n4, n5)
      else
        obj%nref = n1
      end if

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

!!----Check value of OPT_DOMAIN
      if (obj%opt_domain(1:1)=='X' .or. obj%opt_domain(1:1)=='x') then
        obj%opt_domain = 'X_T'
      else if (obj%opt_domain(1:1)=='T' .or. obj%opt_domain(1:1)=='t') then
        obj%opt_domain = 'TAU_P'
      else if (obj%opt_domain(1:1)=='A' .or. obj%opt_domain(1:1)=='a') then
        obj%opt_domain = 'ANGLE'
      else
        call pc_error ('OPT_DOMAIN must be X_T, TAU_P or ANGLE.')
      end if
      if (obj%opt_domain /= opt_domain_prev .and. update_state == PC_GUI) then
        call pc_warning ("You've changed OPT_DOMAIN. Make sure OFF_INIT, &
                         &OFF_INC, and OFF_TOT are set appropriately for &
                         &new domain.")
      end if

!!----Set some sensitivities
      if (obj%opt_domain == 'X_T') then
        geomdiv_sensitive = .true.
        call pc_put_sensitive_field_flag ('OFF_MIN', .false.)
        call pc_put_sensitive_field_flag ('OFF_MAX', .false.)
        if (update_state /= PC_GUI) then
          obj%off_min = 0.0
          obj%off_max = 0.0
        end if
      else
        geomdiv_sensitive = .false.
        call pc_put_sensitive_field_flag ('OFF_MIN', .true.)
        call pc_put_sensitive_field_flag ('OFF_MAX', .true.)
        if (update_state /= PC_GUI) obj%opt_geomdiv = 'NONE'
        obj%off_min = max (obj%off_min, 0.0)
        obj%off_max = max (obj%off_max, 0.0)
        if (obj%off_max > 0.0 .and. obj%off_max <= obj%off_min) then
          call pc_error ('OFF_MAX must be greater than OFF_MIN (or else &
                         &set OFF_MAX = 0.0 to indicate NO upper limit).')
        end if
      end if

!!----Set sensitivity for OPT_MOVOUT
      if (obj%opt_domain == 'ANGLE') then
        movout_sensitive = .false.
        if (update_state /= PC_GUI) obj%opt_movout = 'NONE'
      else
        movout_sensitive = .true.
      end if

!!----Check value of OPT_MOVOUT
      if (obj%opt_movout(1:1)=='E' .or. obj%opt_movout(1:1)=='e') then
        obj%opt_movout = 'EXACT'
      else if (obj%opt_movout(1:1)=='H' .or. obj%opt_movout(1:1)=='h') then
        obj%opt_movout = 'HYPER'
        if (obj%opt_domain == 'TAU_P') then
          call pc_error ('OPT_MOVOUT = HYPER is not available when &
                         &OPT_DOMAIN = TAU_P.')
          obj%opt_movout = 'EXACT'
        end if
      else if (obj%opt_movout(1:1)=='N' .or. obj%opt_movout(1:1)=='n') then
        obj%opt_movout = 'NONE'
      else
        call pc_error ('OPT_MOVOUT must be EXACT, HYPER or NONE.')
      end if

!!----Check value of OPT_GEOMDIV
      if (obj%opt_geomdiv(1:1)=='N' .or. obj%opt_geomdiv(1:1)=='n') then
        obj%opt_geomdiv = 'NONE'
      else if (obj%opt_geomdiv(1:1)=='P' .or. obj%opt_geomdiv(1:1)=='p') then
        obj%opt_geomdiv = 'POINT'
      else if (obj%opt_geomdiv(1:1)=='L' .or. obj%opt_geomdiv(1:1)=='l') then
        obj%opt_geomdiv = 'LINE'
      else
        call pc_error ('OPT_GEOMDIV must be NONE, POINT or LINE.')
      end if

!!----Check LINE, CMP, OFF values
      obj%hdr_line = min(max(obj%hdr_line, 1), obj%nwih)
      obj%line_tot = max(obj%line_tot, 1)
      obj%hdr_cmp  = min(max(obj%hdr_cmp, 1), obj%nwih)
      obj%cmp_tot  = max(obj%cmp_tot, 1)
      obj%hdr_off  = min(max(obj%hdr_off, 1), obj%nwih)
      obj%off_tot  = max(obj%off_tot, 1)
      if (obj%off_inc == 0.0 .and. obj%opt_domain == 'X_T') then
        if (obj%off_tot > 1) then
          call pc_error ('OFF_INC must be non-zero when OPT_DOMAIN = X_T.')
        else
          call pc_error ('OFF_INC must be non-zero when OPT_DOMAIN = X_T &
                         &(even if OFF_TOT = 1).')
        end if
      end if

!!----Check ANG_MIN and ANG_MAX
      obj%ang_min = min(max(obj%ang_min, 0.0), ANGLE_BIG)
      obj%ang_max = min(max(obj%ang_max, 0.0), ANGLE_BIG)
      if (obj%opt_domain == 'ANGLE') then
        call pc_put_sensitive_field_flag ('ANG_MIN', .false.)
        call pc_put_sensitive_field_flag ('ANG_MAX', .false.)
        if (update_state /= PC_GUI) then
          obj%ang_min = 0.0
          obj%ang_max = ANGLE_BIG
        end if
      else
        call pc_put_sensitive_field_flag ('ANG_MIN', .true.)
        call pc_put_sensitive_field_flag ('ANG_MAX', .true.)
        if (obj%ang_min >= obj%ang_max) then
          call pc_error('ANG_MIN must be less than ANG_MAX.')
        end if
      end if
      if (update_state /= PC_GUI) then
        obj%sinangmin = sin(real(RADIANS_PER_DEGREE * obj%ang_min))
        obj%sinangmax = sin(real(RADIANS_PER_DEGREE * obj%ang_max))
        obj%sinangfinemax = sin(real(RADIANS_PER_DEGREE * ANGLE_BIG))
      end if

!!----Check PEAK_FREQ value
      obj%peak_freq = abs(obj%peak_freq)
      if (update_state /= PC_GUI) then
        if (obj%peak_freq == 0.0 .and. obj%opt_movout /= 'NONE') then
          call pc_warning ('HSYN Warning: Spike wavelets (PEAK_FREQ = 0) &
                           &are not recommended for synthetics that &
                           &include moveout.')
        end if
      end if

!!----Check MODEL_SOURCE and related parameters
      if (obj%model_source(1:1)=='F' .or. obj%model_source(1:1)=='f') then
        obj%model_source = 'FILE'
        call pc_put_sensitive_field_flag ('SELECT_PATHNAME' , .true.)
        call pc_put_sensitive_field_flag ('PATHNAME'        , .true.)
        call pc_put_sensitive_field_flag ('NUM_COLUMNS'     , .true.)
        call pc_put_sensitive_field_flag ('FIRSTLINE'       , .true.)
        call pc_put_sensitive_field_flag ('NILSTRING'       , .true.)
        call pc_put_sensitive_field_flag ('TIME_COLUMN'     , .true.)
        call pc_put_sensitive_field_flag ('DENSITY_COLUMN'  , .true.)
        call pc_put_sensitive_field_flag ('PWAVE_COLUMN'    , .true.)
        call pc_put_sensitive_field_flag ('SWAVE_COLUMN'    , .true.)
        call pc_put_sensitive_field_flag ('TIME_UNITS'      , .true.)
        call pc_put_sensitive_field_flag ('PWAVE_TYPE'      , .true.)
        call pc_put_sensitive_field_flag ('SWAVE_TYPE'      , .true.)
        call pc_put_sensitive_field_flag ('REF_COEFF'       , .true.)
        call pc_put_sensitive_field_flag ('LEN_SMOOTH'      , .true.)
        call pc_put_sensitive_arrayset_flag ('TIME_ARRAYSET', .false.)
        if (update_state /= PC_GUI) obj%nref = 0
        call pathcheck ('PATHNAME', obj%pathname, required=.true., &
                        show=PATHCHECK_INFO_INPUT)
        if (obj%nilstring == ' ') then
          call pc_error ('You must specify NILSTRING.')
        end if
      else if (obj%model_source(1:1)=='M' .or. obj%model_source(1:1)=='m') then
        obj%model_source = 'MANUAL'
        call pc_put_sensitive_field_flag ('SELECT_PATHNAME' , .false.)
        call pc_put_sensitive_field_flag ('PATHNAME'        , .false.)
        call pc_put_sensitive_field_flag ('NUM_COLUMNS'     , .false.)
        call pc_put_sensitive_field_flag ('FIRSTLINE'       , .false.)
        call pc_put_sensitive_field_flag ('NILSTRING'       , .false.)
        call pc_put_sensitive_field_flag ('TIME_COLUMN'     , .false.)
        call pc_put_sensitive_field_flag ('DENSITY_COLUMN'  , .false.)
        call pc_put_sensitive_field_flag ('PWAVE_COLUMN'    , .false.)
        call pc_put_sensitive_field_flag ('SWAVE_COLUMN'    , .false.)
        call pc_put_sensitive_field_flag ('TIME_UNITS'      , .false.)
        call pc_put_sensitive_field_flag ('PWAVE_TYPE'      , .false.)
        call pc_put_sensitive_field_flag ('SWAVE_TYPE'      , .false.)
        call pc_put_sensitive_field_flag ('REF_COEFF'       , .false.)
        call pc_put_sensitive_field_flag ('LEN_SMOOTH'      , .false.)
        call pc_put_sensitive_arrayset_flag ('TIME_ARRAYSET', .true.)
        if (update_state /= PC_GUI) then
          obj%ref_coeff = 'AKI_RICHARDS'
          obj%len_smooth = 0.0
          obj%pathname = PATHCHECK_EMPTY
        end if
        call pathcheck ('PATHNAME', obj%pathname, show=PATHCHECK_INFO_INPUT)
      else
        call pc_error('MODEL_SOURCE must be FILE or MANUAL.')
      end if

      if (obj%num_columns < 4) then
        if (obj%model_source == 'FILE') then
          call pc_error ('You set NUM_COLUMNS =',obj%num_columns,'but your &
                         &file must include at least 4 columns, containing &
                         &Traveltime, Density, P-wave and S-wave.')
        else
          obj%num_columns = 4
        end if
      else
        obj%time_column    = min(max(obj%time_column,1)   ,obj%num_columns)
        obj%density_column = min(max(obj%density_column,1),obj%num_columns)
        obj%pwave_column   = min(max(obj%pwave_column,1)  ,obj%num_columns)
        obj%swave_column   = min(max(obj%swave_column,1)  ,obj%num_columns)
        if (update_state /= PC_GUI) then
          if (obj%time_column    == obj%density_column .or. &
              obj%time_column    == obj%pwave_column   .or. &
              obj%time_column    == obj%swave_column   .or. &
              obj%density_column == obj%pwave_column   .or. &
              obj%density_column == obj%swave_column   .or. &
              obj%pwave_column   == obj%swave_column) then
            if (obj%model_source == 'FILE') then
              call pc_error ('TIME_COLUMN, DENSITY_COLUMN, PWAVE_COLUMN and &
                             &SWAVE_COLUMN must all be DIFFERENT.')
            else
              obj%time_column    = 1
              obj%density_column = 2
              obj%pwave_column   = 3
              obj%swave_column   = 4
            end if
          end if
        end if
      end if
      obj%firstline = max (obj%firstline, 1)
      if (obj%time_units(1:1) == 'S' .or. obj%time_units(1:1) == 's') then
        obj%time_units = 'SECONDS'
      else
        obj%time_units = 'MILLISECONDS'
      end if
      if (obj%pwave_type(1:1) == 'V' .or. obj%pwave_type(1:1) == 'v') then
        obj%pwave_type = 'VELOCITY'
      else
        obj%pwave_type = 'SLOWNESS'
      end if
      if (obj%swave_type(1:1) == 'V' .or. obj%swave_type(1:1) == 'v') then
        obj%swave_type = 'VELOCITY'
      else
        obj%swave_type = 'SLOWNESS'
      end if
      if (obj%ref_coeff(1:1) == 'Z' .or. obj%ref_coeff(1:1) == 'z') then
        obj%ref_coeff = 'ZOEPPRITZ'
      else if (obj%ref_coeff(1:1) == 'S' .or. obj%ref_coeff(1:1) == 's') then
        obj%ref_coeff = 'SHUEY'
      else
        obj%ref_coeff = 'AKI_RICHARDS'
      end if
      obj%nsmooth = 2 * nint(0.5*max(obj%len_smooth,0.0)/obj%dt) + 1
      if (obj%nsmooth >= obj%ndpt) then
        if (obj%model_source == 'FILE') then
          call pc_warning('Are you sure you want LEN_SMOOTH longer than your &
                          &trace length? Note that LEN_SMOOTH is in SECONDS &
                          &even if times in your file are in milliseconds.')
        else
          obj%nsmooth = 1
        end if
      end if
      obj%len_smooth = (obj%nsmooth - 1) * obj%dt

!!----Verify layered model when leaving arrayset
      if (obj%model_source == 'MANUAL' &
           .and. pc_verify_arrayset('TIME_ARRAYSET')) then
        if (obj%nref < 1) then
          call pc_error ('You must have at least one reflector in your &
                         &model.')
        else
          do iref = 1, obj%nref
            if (obj%time(iref) == FNIL) then
              call pc_error ('Missing TIME value for reflector', iref)
            else if (obj%time(iref) < 0.0) then
              call pc_error ('Negative TIME value for reflector', iref)
            else if (iref > 1) then
              if (obj%time(iref) <= obj%time(iref-1)) then
                call pc_error ('TIME values must be monotone increasing.')
              end if
            end if
            if (obj%vel_int(iref) == FNIL) then
              call pc_error ('Missing VEL_INT value for reflector', iref)
            else if (obj%vel_int(iref) <= 0.0) then
              call pc_error ('VEL_INT for layer', iref, 'is <= 0.0')
            end if
            if (obj%a(iref) == FNIL) then
              call pc_error ('Missing A value for reflector', iref)
            end if
            if (obj%b(iref) == FNIL) obj%b(iref) = 0.0
            if (obj%c(iref) == FNIL) obj%c(iref) = 0.0
          end do
          if (obj%time(1) == 0.0) then
            if (obj%nref > 1) then
              if (obj%vel_int(2) /= FNIL &
                         .and. obj%vel_int(1) /= obj%vel_int(2)) then
                obj%vel_int(1) = obj%vel_int(2)
                call pc_info ('VEL_INT for reflector at time zero reset to &
                              &match VEL_INT(2).')
              end if
            end if
            if (obj%b(1) /= 0.0) then
              obj%b(1) = 0.0
              call pc_info ('B coefficient reset to zero for reflector at &
                            &time zero.')
            end if
            if (obj%c(1) /= 0.0) then
              obj%c(1) = 0.0
              call pc_info ('C coefficient reset to zero for reflector at &
                            &time zero.')
            end if
          end if
        end if
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('OPT_DOMAIN', (/ 'X_T  ', 'TAU_P', &
                                                  'ANGLE' /), 3)
      if (obj%opt_domain == 'TAU_P') then
        call pc_put_options_field ('OPT_MOVOUT', (/ 'EXACT', 'NONE ' /), 2)
      else
        call pc_put_options_field ('OPT_MOVOUT', (/ 'EXACT', 'HYPER', &
                                                    'NONE ' /), 3)
      end if
      call pc_put_options_field ('OPT_GEOMDIV', (/ 'NONE ', 'POINT', &
                                                   'LINE ' /), 3)
      call pc_put_options_field ('MODEL_SOURCE', (/ 'FILE  ', 'MANUAL' /), 2)
      call pc_put_options_field ('TIME_UNITS', (/ 'MILLISECONDS ', &
                                                  'SECONDS      ' /), 2 )
      call pc_put_options_field ('PWAVE_TYPE', (/ 'SLOWNESS ', &
                                                  'VELOCITY ' /), 2 )
      call pc_put_options_field ('SWAVE_TYPE', (/ 'SLOWNESS ', &
                                                  'VELOCITY ' /), 2 )
      call pc_put_options_field ('REF_COEFF',  (/ 'ZOEPPRITZ    ', &
                                                  'AKI_RICHARDS ', &
                                                  'SHUEY        ' /), 3 )
      call pc_put ('OPT_DOMAIN'    , obj%opt_domain)
      call pc_put ('OPT_MOVOUT'    , obj%opt_movout)
      call pc_put ('OPT_GEOMDIV'   , obj%opt_geomdiv)
      call pc_put ('HDR_LINE'      , obj%hdr_line)
      call pc_put ('LINE_INIT'     , obj%line_init)
      call pc_put ('LINE_INC'      , obj%line_inc)
      call pc_put ('LINE_TOT'      , obj%line_tot)
      call pc_put ('HDR_CMP'       , obj%hdr_cmp)
      call pc_put ('CMP_INIT'      , obj%cmp_init)
      call pc_put ('CMP_INC'       , obj%cmp_inc)
      call pc_put ('CMP_TOT'       , obj%cmp_tot)
      call pc_put ('HDR_OFF'       , obj%hdr_off)
      call pc_put ('OFF_INIT'      , obj%off_init)
      call pc_put ('OFF_INC'       , obj%off_inc)
      call pc_put ('OFF_TOT'       , obj%off_tot)
      call pc_put ('ANG_MIN'       , obj%ang_min)
      call pc_put ('ANG_MAX'       , obj%ang_max)
      call pc_put ('OFF_MIN'       , obj%off_min)
      call pc_put ('OFF_MAX'       , obj%off_max)
      call pc_put ('PEAK_FREQ'     , obj%peak_freq)
      call pc_put ('MODEL_SOURCE'  , obj%model_source)
      call pc_put ('PATHNAME'      , obj%pathname)
      call pc_put ('NUM_COLUMNS'   , obj%num_columns)
      call pc_put ('FIRSTLINE'     , obj%firstline)
      call pc_put ('NILSTRING'     , obj%nilstring)
      call pc_put ('TIME_COLUMN'   , obj%time_column)
      call pc_put ('DENSITY_COLUMN', obj%density_column)
      call pc_put ('PWAVE_COLUMN'  , obj%pwave_column)
      call pc_put ('SWAVE_COLUMN'  , obj%swave_column)
      call pc_put ('TIME_UNITS'    , obj%time_units)
      call pc_put ('PWAVE_TYPE'    , obj%pwave_type)
      call pc_put ('SWAVE_TYPE'    , obj%swave_type)
      call pc_put ('REF_COEFF'     , obj%ref_coeff)
      call pc_put ('LEN_SMOOTH'    , obj%len_smooth)
      call pc_put ('TIME'          , obj%time   , obj%nref)
      call pc_put ('VEL_INT'       , obj%vel_int, obj%nref)
      call pc_put ('A'             , obj%a      , obj%nref)
      call pc_put ('B'             , obj%b      , obj%nref)
      call pc_put ('C'             , obj%c      , obj%nref)

      call pc_put_global  ('NUMTR'     , 1)
      call pc_put_global  ('GATHERED'  , .false.)
      call pc_put_control ('NEED_LABEL', .true.)

      call pc_put_sensitive_field_flag ('OPT_MOVOUT' , movout_sensitive)
      call pc_put_sensitive_field_flag ('OPT_GEOMDIV', geomdiv_sensitive)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free (obj%dtim)
      call mem_free (obj%dtim2)
      call mem_free (obj%pmax)
      call mem_free (obj%vp2)
      call mem_free (obj%tvp2)
      call mem_free (obj%t2vp2)
      call mem_free (obj%vrms2)

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if (obj%model_source == 'FILE') then
        ndptfile = nint(obj%tstrt/obj%dt) + obj%ndpt + 1
        if (ndptfile < 2) then
          call pc_error ('Traces do not include positive event times.')
          return
        end if
        call hsyn_read_model_file (obj, ndptfile)
        if (pc_do_not_process_traces()) return
      end if

      call mem_alloc (obj%dtim,    obj%nref)
      call mem_alloc (obj%dtim2,   obj%nref)
      call mem_alloc (obj%pmax,    obj%nref)
      call mem_alloc (obj%vp2,     obj%nref)
      call mem_alloc (obj%tvp2,    obj%nref)
      call mem_alloc (obj%t2vp2,   obj%nref)
      call mem_alloc (obj%vrms2,   obj%nref)
      if (obj%model_source == 'MANUAL') then
        call mem_realloc (obj%vel_int, obj%nref + 1)
        call mem_alloc (obj%vpfine, obj%nref + 1)
      end if

!!----Check if need to save gather
      if (obj%cmp_tot > 1 .or. obj%line_tot > 1) then
        obj%save_gather = .true.
        call mem_alloc (obj%hdsave, obj%nwih, obj%off_tot)
        call mem_alloc (obj%trsave, obj%ndpt, obj%off_tot)
      else
        obj%save_gather = .false.
      end if

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!!----Fill DTIM array
      obj%dtim(1) = obj%time(1)
      do iref = 2, obj%nref
        obj%dtim(iref) = dble(obj%time(iref)) - dble(obj%time(iref-1))
      end do

!!----Fill DTIM2 array
      obj%dtim2 = obj%dtim * obj%dtim

!!----Fill PMAX array
      obj%pmax(1) = 1.0D0 / obj%vel_int(1)
      do iref = 2, obj%nref
        obj%pmax(iref) = min (obj%pmax(iref-1), 1.0D0/obj%vel_int(iref))
      end do

!!----Fill VP2 array
      do iref = 1, obj%nref
        obj%vp2(iref) = dble(obj%vel_int(iref)) ** 2
      end do

!!----Fill TVP2 array
      obj%tvp2 = obj%dtim * obj%vp2

!!----Fill T2VP2 array
      obj%t2vp2 = obj%dtim2 * obj%vp2

!!----Fill VRMS2 array
      sumtvp2 = obj%tvp2(1)
      obj%vrms2(1) = obj%vp2(1)
      do iref = 2, obj%nref
        sumtvp2 = sumtvp2 + obj%tvp2(iref)
        obj%vrms2(iref) = sumtvp2 / obj%time(iref)
      end do

!!----Set VEL_INT(NREF+1) and fill VPFINE array if necessary
      if (obj%model_source == 'MANUAL') then
        obj%vel_int(obj%nref + 1) = obj%vel_int(obj%nref)
        obj%vpfine = obj%vel_int
      end if

!!----Set WVLTSIZ
      if (obj%peak_freq > 0.0) then
        obj%wvltsiz = 2.0  *  (PI * obj%peak_freq * obj%dt) ** 2
        obj%tdelt = sqrt(32./obj%wvltsiz)
      else
        obj%wvltsiz = 0.0
        obj%tdelt = 0.0
      end if

!!----Initialize counter variables
      obj%ilin = 1
      obj%icmp = 1
      obj%ioff = 0
      obj%ngrp = 1
      obj%ntot = 0

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      end subroutine hsyn_update

!!----------------------- hsyn_read_model_file -----------------------------!!
!!----------------------- hsyn_read_model_file -----------------------------!!
!!----------------------- hsyn_read_model_file -----------------------------!!

      subroutine hsyn_read_model_file (obj, ndptfile)
!
      type(hsyn_struct),intent(inout) :: obj                     ! arguments
      integer          ,intent(in)    :: ndptfile                ! arguments

      type(floatio_struct),pointer :: floatio                    ! local
      real                         :: trtemp(ndptfile,3)         ! local
      real                         :: vptemp(ndptfile)           ! local
      real                         :: vptemp2(ndptfile)          ! local
      integer                      :: kount_vp(ndptfile)         ! local
      integer                      :: kount_all3(ndptfile)       ! local
      real                         :: vline(obj%num_columns)     ! local
      integer                      :: err, nlines, ncolumns      ! local
      integer                      :: indx, iref, istrt, istop   ! local
      integer                      :: istrtall3, itr, usecol(3)  ! local
      real                         :: thistime, vsvp2, refl(3)   ! local
      logical                      :: flipcol(3)                 ! local
      character(len=80)            :: msg                        ! local

      call floatio_easy_read (floatio,obj%pathname,nlines,ncolumns,err,msg, &
                              obj%firstline,obj%nilstring)
      if (err /= FLOATIO_OK) then
        call floatio_close (floatio)
        call pc_error ('Error opening file',trim(obj%pathname))
        call pc_error (trim(msg))
        return
      endif

! Load Density, Vp, and Vs into temp traces 1, 2 and 3; also Vp into VPTEMP

      usecol(1) = obj%density_column
      usecol(2) = obj%pwave_column
      usecol(3) = obj%swave_column
      flipcol(:) = .false.
      if (obj%pwave_type == 'SLOWNESS') flipcol(2) = .true.
      if (obj%swave_type == 'SLOWNESS') flipcol(3) = .true.

      vptemp = FNIL
      trtemp = 0.0
      kount_vp = 0
      kount_all3 = 0

      do

        call floatio_read_line  (floatio,err,msg,vline)
        if (err == FLOATIO_EOF) exit
        if (err == FLOATIO_ERROR) then
          call floatio_close (floatio)
          call pc_error ('Error reading from file',trim(obj%pathname))
          call pc_error (trim(msg))
          return
        end if

        thistime = vline(obj%time_column)
        if (thistime == FNIL .or. thistime < 0.0) cycle
        if (obj%time_units == 'MILLISECONDS') thistime = thistime / 1000.0
        indx = 1 + int(thistime/obj%dt)
        if (indx < 1 .or. indx > ndptfile) cycle

        if (vline(usecol(2)) == FNIL .or. vline(usecol(2)) <= 0.0) cycle
        if (flipcol(2)) vline(usecol(2)) = 1.0E6 / vline(usecol(2))
        if (kount_vp(indx) == 0) then
          vptemp(indx) = vline(usecol(2))
        else
          vptemp(indx) = vptemp(indx) + vline(usecol(2))
        endif
        kount_vp(indx) = kount_vp(indx) + 1

        if (vline(usecol(1)) == FNIL .or. vline(usecol(1)) <= 0.0 .or. &
            vline(usecol(3)) == FNIL .or. vline(usecol(3)) <= 0.0) cycle
        if (flipcol(3)) vline(usecol(3)) = 1.0E6 / vline(usecol(3))
        do itr = 1, 3
          if (kount_all3(indx) == 0) then
            trtemp(indx,itr) = vline(usecol(itr))
          else
            trtemp(indx,itr) = trtemp(indx,itr) + vline(usecol(itr))
          endif
        enddo
        kount_all3(indx) = kount_all3(indx) + 1

      end do

      call floatio_close (floatio)

! Finish averaging for Density, Vp and Vs and Interpolate nils in VPTEMP

      istrtall3 = ndptfile + 1
      istop = 0
      do indx = 1, ndptfile
        if (kount_vp(indx) > 0) then
          vptemp(indx) = vptemp(indx) / kount_vp(indx)
        end if
        if (kount_all3(indx) > 0) then
          if (trtemp(indx,3) > VSVPMAX*trtemp(indx,2)) then
            trtemp(indx,1:3) = 0.0
            kount_all3(indx) = 0
          else
            trtemp(indx,1:3) = trtemp(indx,1:3) / kount_all3(indx)
            if (istrtall3 > ndptfile) istrtall3 = indx
            istop = indx
          end if
        end if
      end do
      if (istrtall3 >= istop) then
        call pc_error ('Model from file has no events to calculate.')
        return
      end if
      call terputil_replace_nils (vptemp, ndptfile)
      call hsyn_smooth (vptemp, ndptfile, obj%nsmooth, vptemp2)
      istrt = istrtall3
      do indx = 2, istrtall3
        if (mth_compare(vptemp2(indx), vptemp2(1)) /= 0) then
          istrt = indx - 1
          exit
        end if
      end do
      obj%nref = istop - istrt

! Allocate memory for Layered Earth Model

      call mem_alloc (obj%time   , obj%nref)
      call mem_alloc (obj%vel_int, obj%nref+1)
      call mem_alloc (obj%vpfine , obj%nref+1)
      if (obj%ref_coeff == 'ZOEPPRITZ') then
        call mem_alloc (obj%density, obj%nref+1)
        call mem_alloc (obj%vshear , obj%nref+1)
      else
        call mem_alloc (obj%a , obj%nref)
        call mem_alloc (obj%b , obj%nref)
        call mem_alloc (obj%c , obj%nref)
      end if
      if (pc_do_not_process_traces()) return

! Fill the Layered Earth Model

      iref = 0
      do indx = istrt, istop
        iref  = iref + 1
        if (indx < istop) obj%time(iref) = indx * obj%dt
        obj%vpfine(iref)  = vptemp(indx)
        obj%vel_int(iref) = vptemp2(indx)
        if (obj%ref_coeff == 'ZOEPPRITZ') then
          if (kount_all3(indx) > 0 &
                .and. trtemp(indx,3) <= VSVPMAX*vptemp(indx)) then
            obj%density(iref) = trtemp(indx,1)
            obj%vshear(iref)  = trtemp(indx,3)
          else
            obj%density(iref) = 0.0
            obj%vshear(iref)  = 0.0
          end if
        else
          if (indx == istop) exit
          if (kount_all3(indx) == 0 .or. kount_all3(indx+1) == 0) then
            obj%a(iref) = 0.0
            obj%b(iref) = 0.0
            obj%c(iref) = 0.0
          else
            do itr = 1, 3
              refl(itr) = (trtemp(indx+1,itr) - trtemp(indx,itr)) &
                          / (trtemp(indx+1,itr) + trtemp(indx,itr))
            end do
            vsvp2 = ((trtemp(indx+1,3) + trtemp(indx,3)) &
                     / (trtemp(indx+1,2) + trtemp(indx,2))) ** 2
            obj%a(iref) = refl(1) + refl(2)
            obj%b(iref) = refl(2) - 4.0*vsvp2*(refl(1) + 2.0*refl(3))
            if (obj%ref_coeff == 'SHUEY') then
              obj%c(iref) = 0.0
            else
              obj%c(iref) = refl(2)
            end if
          end if
        end if
      end do

      end subroutine hsyn_read_model_file

!!----------------------------- hsyn_smooth --------------------------------!!
!!----------------------------- hsyn_smooth --------------------------------!!
!!----------------------------- hsyn_smooth --------------------------------!!
!
! Input array XIN is smoothed by applying a running average of length NAVG;
! output in XOUT. The running average is always performed over an ODD number
! of points centered around a desired output point. If NAVG is even, an
! averaging length of NAVG+1 is used. Averaging length is tapered down when
! approaching the beginning and end of the XIN array, to keep the averaging
! operator from running off the ends of that array.
!
      subroutine hsyn_smooth (xin, n, navg, xout)

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

      end subroutine hsyn_smooth

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

      subroutine hsyn (obj, ntr, hd, tr)
      type(hsyn_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer          :: imute_top, imute_bot, iref            ! local
      integer          :: i, istrt, istop, status, i_err        ! local
      real             :: t, afact, sinang, sinangfine          ! local
      real             :: sinang2, radcrv, inc_ang              ! local
      complex          :: refp, refs, transp, transs            ! local
      double precision :: x, x2, p, p2, xtest, eps, tau         ! local
      double precision :: rootarg, root                         ! local

!!----Check offset, basement & line counters
      if (obj%ioff < obj%off_tot)  then
        obj%ioff = obj%ioff + 1
      else
        if (obj%icmp < obj%cmp_tot)  then
          obj%icmp = obj%icmp + 1
        else
          if (obj%ilin < obj%line_tot)  then
            obj%ilin = obj%ilin + 1
            obj%icmp = 1
          else  !-----------------------------We're finished when get here
            ntr = NO_MORE_TRACES
            call hsyn_wrapup (obj)
            return
          end if
        end if
        obj%ngrp = obj%ngrp + 1
        obj%ioff = 1
      end if
      obj%ntot = obj%ntot + 1

!!----Initialize header and trace (retrieve from memory if later gather)
      if (obj%ntot > obj%off_tot) then
        hd(:obj%nwih,1) = obj%hdsave(:,obj%ioff)
        tr(:obj%ndpt,1) = obj%trsave(:,obj%ioff)
      else
        hd(:obj%nwih,1) = 0.0D0
        tr(:obj%ndpt,1) = 0.0
      end if

!!----Set some of the header words for output
      hd(obj%hdr_line,1) = obj%line_init + (obj%ilin-1)*obj%line_inc
      hd(obj%hdr_cmp,1)  = obj%cmp_init  + (obj%icmp-1)*obj%cmp_inc
      hd(obj%hdr_off,1)  = obj%off_init  + (obj%ioff-1)*obj%off_inc
      hd(HDR_SEQUENCE,1)        = obj%ntot
      hd(HDR_CURRENT_GROUP,1)   = obj%ngrp
      hd(HDR_CURRENT_CHANNEL,1) = obj%ioff
      if (obj%ntot > obj%off_tot) then  ! We can output saved trace now
        ntr = 1
        return
      end if
      x = abs(hd(obj%hdr_off,1))
      imute_top = obj%ndpt
      imute_bot = 0

!!----Get ready to calculate events
      select case (obj%opt_domain)
      case ('X_T')
        eps = abs(obj%off_inc) * 1.0D-3
        x2 = x ** 2
      case ('TAU_P')
        p = x * 1.0D-6
        p2 = p ** 2
        xtest = 0.0D0
        tau = 0.0D0
      case ('ANGLE')
        sinang = sin (real(RADIANS_PER_DEGREE * x))
      end select

!!----Now loop over desired events
events_loop: &
      do iref = 1, obj%nref

!!------Initialize Amplitude Factor
        afact = 1.0

!!------Locate events according to OPT_DOMAIN
        select case (obj%opt_domain)
        case ('X_T')
          call hsyn_solve (obj, x, iref, eps, p, t, radcrv, status)
          if (status /= 0) exit events_loop
          if (obj%opt_movout == 'NONE') then
            t = obj%time(iref)
          else if (obj%opt_movout == 'HYPER') then
            t = sqrt ( obj%time(iref)**2 + x2/obj%vrms2(iref) )
          end if
          if (obj%opt_geomdiv /= 'NONE') then
            if (radcrv == 0.0) then
              afact = 0.0
            else if (obj%opt_geomdiv == 'LINE') then
              afact = afact / sqrt(radcrv)
            else if (x == 0.0) then
              afact = afact / radcrv
            else
              afact = afact * sqrt(p*obj%vel_int(1)/(x*radcrv))
            end if
          end if
        case ('TAU_P')
          if (p >= obj%pmax(iref)) exit events_loop
          rootarg = 1.0D0 - obj%vp2(iref)*p2
          if (rootarg <= 0.0D0) exit events_loop
          root = sqrt(rootarg)
          if (obj%opt_movout == 'NONE') then
            t = obj%time(iref)
          else  ! if (obj%opt_movout == 'EXACT') then
            tau = tau + obj%dtim(iref)*root
            t = real(tau)
          end if
          if (obj%off_min > 0.0 .or. obj%off_max > 0.0) then
            xtest = xtest + p*obj%tvp2(iref)/root
            if (real(xtest) < obj%off_min) cycle events_loop
            if (obj%off_max > 0.0) then
              if (real(xtest) > obj%off_max) exit events_loop
            end if
          end if
        case ('ANGLE')
          if (mth_compare(real(x), ANGLE_BIG) > 0) exit events_loop
          p = 2.0 * sinang / (obj%vel_int(iref) + obj%vel_int(iref+1))
          if (p >= obj%pmax(iref)) cycle events_loop
          t = obj%time(iref)
          if (obj%off_min > 0.0 .or. obj%off_max > 0.0) then
            p2 = p ** 2
            xtest = 0.0D0
            do i = 1, iref
              rootarg = 1.0D0 - obj%vp2(i)*p2
              if (rootarg <= 0.0) cycle events_loop
              xtest = xtest + p*obj%tvp2(i)/sqrt(rootarg)
            end do
            if (real(xtest) < obj%off_min) cycle events_loop
            if (obj%off_max > 0.0) then
              if (real(xtest) > obj%off_max) cycle events_loop
            end if
          end if
        end select

!!------Find Event Amplitude
        if (iref > 1 .or. obj%time(iref) > 0.0) then
          if (obj%opt_domain /= 'ANGLE') then
            sinang = 0.5 * p * (obj%vel_int(iref) + obj%vel_int(iref+1))
          end if
          sinangfine = 0.5 * p * (obj%vpfine(iref) + obj%vpfine(iref+1))
          if (mth_compare(sinang, obj%sinangmax) > 0                  &
              .or. mth_compare(sinang, obj%sinangmin) < 0             &
              .or. mth_compare(sinangfine, obj%sinangfinemax) > 0     &
              .or. mth_compare(real(p*obj%vpfine(iref)), 1.0) >= 0    &
              .or. mth_compare(real(p*obj%vpfine(iref+1)), 1.0) >= 0) &
            cycle events_loop
          if (obj%ref_coeff == 'ZOEPPRITZ') then
            if (obj%density(iref) == 0.0 .or. obj%density(iref+1) == 0.0) &
              cycle events_loop
            inc_ang = asin (real(p*obj%vpfine(iref)))
            call zoeppritz (inc_ang, 'p', 'c', obj%density(iref),    &
                            obj%vpfine(iref), obj%vshear(iref),      &
                            obj%density(iref+1), obj%vpfine(iref+1), &
                            obj%vshear(iref+1), refp, refs, transp,  &
                            transs, i_err)
            if (i_err /= 0 .or. abs(refp) >= 1.0) cycle events_loop
            afact = afact * real(refp)
          else
            sinang2 = sinangfine ** 2
            afact = afact * (obj%a(iref) + obj%b(iref)*sinang2 &
                             + obj%c(iref)*sinang2**2/(1.0-sinang2))
          end if
        else if (obj%opt_domain == 'X_T') then
          afact = afact * obj%a(1)
        else
          cycle events_loop
        end if

!!------Now add event to trace
        t = (t - obj%tstrt)/obj%dt + 1.
        istrt = nint(max(1.0, t-obj%tdelt))
        istop = nint(min(real(obj%ndpt), t+obj%tdelt))
        if (istop < istrt) cycle events_loop
        if (obj%wvltsiz > 0.0) then
          do i = istrt, istop
            tr(i,1) = tr(i,1)  +  afact * &
               (1.-obj%wvltsiz*(t-i)**2) * exp(-0.5*obj%wvltsiz*(t-i)**2)
          end do
        else
          tr(istrt,1) = tr(istrt,1) + afact
        end if
        imute_top = min (imute_top, istrt)
        imute_bot = max (imute_bot, istop)
      end do events_loop
!!----End of loop over desired events

!!----Set MUTE & LAV headers and return
      if (imute_bot == 0) imute_bot = obj%ndpt
      hd(HDR_TOP_MUTE,1)    = imute_top
      hd(HDR_BOTTOM_MUTE,1) = imute_bot
      call lav_set_hdr (hd(:,1), tr(:,1), obj%ndpt)
      if (obj%save_gather) then
        obj%hdsave(:,obj%ioff) = hd(:obj%nwih,1)
        obj%trsave(:,obj%ioff) = tr(:obj%ndpt,1)
      end if
      ntr = 1
      end subroutine hsyn

!!------------------------------ hsyn_solve --------------------------------!!
!!------------------------------ hsyn_solve --------------------------------!!
!!------------------------------ hsyn_solve --------------------------------!!

      subroutine hsyn_solve (obj, x, iref, eps, p, t, radcrv, status)
!---------------------------------------------------------------------------
! Newton's method plane-layered ray tracer for stratified model.
!
! Arguments:
! Name   Type*   Valid    Description         *Type: I=IN, O=OUT, B=BOTH
! ----   ----    -----    -----------
! OBJ     I  hsyn_struct  HSYN data structure (contains layered model).
! X       I      double   Offset distance between source and receiver.
! IREF    I      int>0    Reflection number to solve for (Routine is
!                         intended to be called repeatedly, with fixed X,
!                         for successive layers of a given model).
! EPS     I     double>0  Tolerance for solution Offset (Distance units).
! P       B      double   Ray parameter (reciprocal velocity units).
!                         For IREF=1, the value of P is simply returned.
!                         For IREF>1, you must input the P from IREF-1,
!                         and the solution for IREF is returned.
! T       O       real    Travel-time from source to receiver.
! RADCRV  O       real    Radius of curvature of wavefront at receiver.
! STATUS  O      integer  Error indicator (zero if no error).
!---------------------------------------------------------------------------
      type(hsyn_struct),intent(in)    :: obj                    ! arguments
      double precision ,intent(in)    :: x                      ! arguments
      integer          ,intent(in)    :: iref                   ! arguments
      double precision ,intent(in)    :: eps                    ! arguments
      double precision ,intent(inout) :: p                      ! arguments
      real             ,intent(out)   :: t, radcrv              ! arguments
      integer          ,intent(out)   :: status                 ! arguments

      integer          :: i                                     ! local
      double precision :: x2, p2, ptemp, ttemp                  ! local
      double precision :: xtest, xnow, dxdp, pnew               ! local
      double precision :: rootarg(obj%nref)                     ! local
      double precision :: root(obj%nref)                        ! local

      status = 0

      x2 = x ** 2
      if (obj%dtim(iref) > 0.0D0) then
        ptemp = sqrt(x2/(x2 + obj%t2vp2(iref))) / obj%vel_int(iref)
      else
        ptemp = 1.0D0 / obj%vel_int(iref)
      end if
      if (iref==1 .or. (iref==2 .and. obj%dtim(1)==0.0D0)) then
        p = ptemp
        t = sqrt ( obj%dtim2(iref) + x2/obj%vp2(iref) )
        radcrv = obj%vel_int(iref) * t
        return
      end if

      p = min(p,ptemp)
      xtest = abs(x)
      DO
        p2 = p ** 2
        xnow = 0.0D0
        dxdp = 0.0D0
        do i = 1, iref
          rootarg(i) = 1.0D0 - obj%vp2(i)*p2
          if (rootarg(i) <= 0.0D0) then
            status = 1
            return
          end if
          root(i) = sqrt(rootarg(i))
          xnow = xnow + obj%tvp2(i)/root(i)
          dxdp = dxdp + obj%tvp2(i)/(rootarg(i)*root(i))
        end do
        xnow = p * xnow
        if (abs(xnow-xtest) <= eps) exit
        pnew = p - (xnow-xtest)/dxdp
        if (mth_compare(p,pnew) == 0) exit
        p = pnew
      END DO
      ttemp = 0.0D0
      do i = 1, iref
        ttemp = ttemp + obj%dtim(i)/root(i)
      end do
      t = ttemp
      radcrv = dxdp * rootarg(1) / obj%vel_int(1)

      end subroutine hsyn_solve

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

      subroutine hsyn_wrapup (obj)
      type(hsyn_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine hsyn_wrapup

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module hsyn_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
