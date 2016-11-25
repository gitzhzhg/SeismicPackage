!<CPS_v1 type="PROCESS"/>
!<-- This documentation header was last revised by SMCook on 2001-05-04. />

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
! Name       : SLST    (SLant STack)
! Category   : transforms 
! Written    : 1986-06-28   by: Bob Baumel
! Revised    : 2007-12-06   by: Bill Menger
! Maturity   : beta
! Purpose    : Transform seismic data both ways between (X,T) and (P,Tau)
!              domains.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! SLST performs a Slant Stack (also known as "Radon Transform") transform
! of seismic trace gathers (usually CMP gathers or shot profiles) in both 
! directions between the (X,T) and (P,Tau) domains.
! 
! SLST performs both line-source (simple) slant stacks and point-source 
! (cylindrical) slant stacks.  The cylindrical slant stack can be calculated 
! using explicit Bessel functions, or asymptotic approximation, or the 
! Brysk-McCowan method.  Most options also allow semblance windowing to reduce
! artifacts.
!
! The user has the option to return to the (X,T) domain after some (P,Tau) 
! processing (such as deconvolution). 
!
! Applications of (P,Tau) processing include multiple attenuation, dip 
! filtering, and enhancement of doubly mode converted (PSSP) events.  SLST also
! serves as a preprocessing step for various inversion algorithms, and is used
! with the PMOD modeling process.
!
!
! Header Words
!
! Offsets and P-values are BOTH kept in header word 6 (P-values have units of
! MICROSECONDS per unit distance).  P-values replace offsets in FORward 
! transforms, and vice-versa in INVerse transforms. When OPT_DIR = FOR2 or INV2,
! header words HDR_SAVE, HDR_SAVE + 1 and HDR_SAVE + 2 are used to save the 
! original trace offset, head mute value and tail mute value.  (The HDR_SAVE
! header words must be user defined headers.)
!
! SLST always sets words 1, 2, 3, 4, 6 and 64 for each output trace.  Words 2 
! and 64 are usually reset to 1 and NDPT, except that when OPT_DIR=INV2 the 
! original (X,T) mutes are restored (see below for additional details when
! opt_geom = PB or PB1).  Word 6 is always reset to the P-value or 
! offset of the output trace.  
!
! For each header word OTHER than those just listed, and for each OPT_DIR 
! option except FOR1, the (N)th header of the output gather is taken from the 
! (N)th header of the input gather (filling zeros when output gather is bigger
! than input gather).
!
!
! OPT_DIR Options and Header Words
!
! To be sure of recovering original headers when going from (X,T) to (P,TAU) 
! and back to (X,T), use FOR2 and INV2 transforms, and make sure you generate 
! at least as many traces in the (P,TAU) domain as you start with in the (X,T) 
! domain.  Also, make sure HDR_SAVE is set the same in both the INV2 and the
! FOR2 transform.
!   
! The FOR1 option is intended to supply usable headers for all your (P,TAU) 
! output traces if you intend to remain in (P,TAU), and process to stack in 
! that domain.  With this option, each header word of the (P,TAU) output trace 
! [except for words 1, 2, 3, 4, 6 and 64] is set to the AVERAGE of all the 
! non-zero values in this header word in the (X,T) input gather.  [Note that 
! every trace of the output gather gets the SAME header, except in words 1, 4,
! and 6].
!
! The OPT_DIR=INV2 option produces output traces at your original (X,T) offsets.
! Use OPT_DIR=INV1 if you want to explicitly specify the offsets to output,
! or if your (P,TAU) data was NOT obtained by transform from (X,T); e.g., if 
! the input is synthetic (P,TAU) traces generated by PMOD.
!
!
! OPT_GEOM Options
!
! The actual slant stack algorithm used is specified by the OPT_GEOM parameter.
! Normally the LS (line source) option should be used for shot profile and CMP
! inputs.  More detail can be found in the Algorithm Description Section.
!
!
! Rho Filter
!
! SLST always applies the "RHO" filter symmetrically in FORward and INVerse 
! transforms, chosen so that reflection events have the SAME wavelet in (X,T) 
! and (P,TAU) domains.  (But NON-reflection events, such as refractions, may 
! have different wavelets.)   For OPT_GEOM=LS, the RHO filter is SQRT(i*omega)
! in the FORward transform, and SQRT(-i*omega) in INVerse transform.  For
! GEOM=Px, the RHO filter is (i*omega) in FORward transform, and (-i*omega) in
! the INVerse transform.  [Fourier sign convention here is that (i*omega)
! represents time differentiation.]
!
!
! Signed Offsets and P-values
!
! SLST allows negative and positive offsets and P-values, but negative values
! are truly meaningful only with the line-source (OPT_GEOM=LS) transform.  To 
! perform a two-sided transform of split-spread data, first use SETWORD to add 
! signs to the offsets; then run SLST with OPT_GEOM=LS, generating a full
! range of negative and positive P's.  (But see Advice for Users for another
! method.)
!
! The point-source (GEOM=Px) case, because it assumes cylindrical symmetry, 
! ignores signs of offsets and P-values.  It won't reject negative offsets or
! P-values, but only their absolute values are used in the calculation.
!
!
! Semblance Based Artifact Reduction
!
! SLST provides Semblance Windowing (OPT_SEMB = THRESH) and Weighting
! (OPT_SEMB = WEIGHT) options which can reduce aliasing and edge-effect 
! artifacts. These semblance options are usable in both FORward and INVerse
! transforms with any OPT_GEOM value except PJ.
!
! OPT_SEMB = THRESH provides semblance windowing. This feature was described by
! Stoffa et al (1981).  It is performed by dividing the input gather into
! overlapping subarrays; then for each subarray, the output at each (P,TAU) 
! point is accepted only when the semblance exceeds a specified threshold.  This
! can substantially reduce artifacts, although it may distort wavelets and 
! introduce trace-to-trace amplitude variations.  It roughly doubles execution
! time because SLST chooses the subarrays so each input trace gets counted 
! twice.  Suggested range for SEMB_THRESH is 0.2 to 0.3.
!
! OPT_SEMB = WEIGHT  provides the simpler option of semblance weighting.  In 
! this case, the input is not divided into subarrays, and the output is simply
! multiplied by Semblance**SEMB_WEIGHT.  This doesn't reduce artifacts as well
! as semblance windowing, but is considerably faster; typically, it adds only
! about 20% to the execution time.  Typical value for SEMB_WEIGHT is 1.0.
!
!
! TSTRT_OUT Parameter
!
! The TSTRT_OUT parameter in SLST alters the TSTRT global.  This is most
! commonly used for resetting TSTRT to zero when performing an INV1 transform 
! of synthetic (P,TAU) data from PMOD which was preferably generated with 
! slightly negative TSTRT to be sure of including events located near TAU=0.
! Of course, you may also do the reverse; i.e., set TSTRT_OUT slightly negative
! in a FORward transform to be sure you fully image events that show up near 
! TAU=0 (such as direct arrivals or surface waves).
!
!
! Applications
!
! Attenuation of multiples by deconvolution in the (P,TAU) domain is described
! in a Conoco Research Report by Durrani and Baumel (1990).  This technique is
! useful when velocity discrimination is lacking between primaries and 
! multiples.  Still more slant stack applications are discussed in Yilmaz's 
! book on Seismic Processing (1987).
!
!
! REFERENCES
!
!  H. Brysk and D.W. McCowan, 1986, A slant-stack procedure for point-
!    source data, GEOPHYSICS, Vol 51, pp 1370-1386.
!
!  J.A. Durrani and R.T. Baumel, 1990, Multiple Attenuation by (p,tau)
!    Processing in Somalia Block 28, Conoco Res. Rpt. 8754-Q01-003-1-90.
!
!  Mike Howard, 1983, Slant Stacks, Conoco Research Report 687-2-1-83.
!
!  M.E. Kappus, A.J. Harding, and J.A. Orcutt, 1990, A comparison of
!    tau-p transform methods, GEOPHYSICS, Vol 55, pp 1202-1215.
!
!  A.R. Mitchell and P.G. Kelamis, 1990, Efficient tau-p hyperbolic
!    velocity filtering, GEOPHYSICS, Vol 55, pp 619-625.
!
!  P.L. Stoffa, P. Buhl, J.B. Diebold, and F. Wenzel, 1981, Direct map-
!    ping of seismic data to the domain of intercept time and ray para-
!    meter--A plane-wave decomposition, GEOPHYSICS, Vol 46, pp 255-267.
!
!  R.H. Tatham and D.V. Goolsbee, 1984, Separation of S-wave and P-wave
!    reflections offshore western Florida, GEOPHYSICS, V 49, pp 493-508.
!
!  O. Yilmaz, 1987, Seismic Data Processing, published by Society of
!    Exploration Geophysicists, Tulsa, OK.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Normally, input data will be shot profile or CMP gathers.  Offset must
! monotonically increase or decrease.
!
! Input and output P-values are in MICROSECONDS per unit distance.  Positive dip
! is increasing travel time with increasing offset.
!
! Using a negative P_INIT value (e.g. -100) will better preserve wavelet
! character at the gather edges when OPT_GEOM = LS or PA.
!
!
! Suggested Parameter Values for Simple Applications
!
!     OPT_DIR = FOR2 to transform from (X,T) to (P,Tau) space and 
!             = INV2 to transform from (P,Tau) to (X,T) space.
!
!     OPT_GEOM should be set to LS.
!
!     Make sure header words 48 - 50 do not contain information you do not want
!     written over (or set HDR_SAVE to another value).
!
!     OFF_TOT = number of traces in input gathers.
!
!     P_INIT = minimum P-value to preserve (may be negative), [P-value is 
!     dip in microseconds per trace (in X-T space) divided by the offset 
!     increment in feet or meters.]
!
!     P_INC = approximately 5.0 microseconds per foot, or less
!
!     P_TOT = INT(1.5 + (P_LAST - P_INIT)/P_INC).  P_TOT >= OFF_TOT
!
!     Set RHO filter parameters to pass the useful frequency range in the data.
!
!     (Take defaults for all other parameters.)
!
!
! Split Spread Shot Profiles
!
! For better run-time efficiency, split spread shot profiles should be broken 
! up into equivalent off-end shot profiles.  The forward off-end shot profiles 
! can be processed through SLST separately from the reverse off-end shot 
! profiles.  (Separate jobs are required if the forward and reverse off-end 
! shot profiles have different offset polarity.)   Alternatively, you can use 
! SETWORD and TSORT to give both kinds of off-end shot profile the same offset
! range and trace order and then process both through SLST in the same job.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in gathers with offset monotonically
! increasing or decreasing.
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
! This process outputs trace gathers.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used and possibly increased
! GATHERED  whether traces are a legitimate gather  used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used and possibly changed
! TSTRT     starting time on trace                  used and possibly changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 2       head mute                  used or set
! 3       Current gather             used
! 4       trace in current gather    set
! 6       offset or P-value          used and set
! 25      LAV                        reset
! 64      tail mute                  used or set
!
! HDR_SAVE, HDR_SAVE + 1 and HDR_SAVE + 2 are used to save the original trace 
! offset, head mute value and tail mute value when OPT_DIR = FOR2 or INV2.
!
! For each header word OTHER than those just listed, and for each OPT_DIR 
! option except FOR1, the (N)th header of the output gather is taken from the 
! (N)th header of the input gather (filling zeros when output gather is bigger
! than input gather).
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author     Description
!     ----       ------     -----------
! 41. 2007-12-06 B. Menger  Fixed a help bug on P-value units.
! 40. 2007-01-03 Stoeckley  Fixed a bug which kept the high cut filter from
!                            being increased above the original nyquist, even
!                            when the sample interval was made smaller.  Also
!                            fixed two bugs in the HelpSection which caused
!                            SeisSpace menus not to show the prompts for the
!                            two parameters P_LAST and SEMB_WEIGHT.
! 39. 2006-06-20  B. Menger   Removed Unused Variables.
! 38. 2004-05-24 SMCook     Fixed problem with expanded headers being rejected.
! 37. 2002-09-11 Goodger    Use mth module for binning
! 36. 2002-07-01 SMCook     Fixed OFF_TOT GATHER trap (bug report #790).
! 35. 2002-05-06 Vunderink  Added parallel control parameters
! 34. 2002-03-28 SMCook     Corrected situation where in INV2 mode the process
!                            would erroneously abort if the input gathers did
!                            not have full trace counts.
! 33. 2001-11-01 SMCook     Temporarily commented out parallel control
!                            statements to be compatible with current betalib
!                            version of DECON.
! 32. 2001-07-31 SMCook     Group logic removed and
!                Vunderink   added parallel control parameters.
! 31. 2001-06-11 SMCook     Put conditional clause around "debug file" logic in
!                            wrapup function.  Was causing fatal error, but at
!                            such a late stage that output was unaffected.
!                            PRODUCTION.
! 30. 2001-05-04 SMCook     Fixed erroneous check -- the check comparing NWPT
!                            and 2*NW_LIVE should only be applied for geom PJ.
! 29. 2001-04-25 SMCook     All options now work.  Removed restrictions on use
!                            of PBx geometries, and on semblance noise 
!                            suppression.
!                           Problems with PBx modes turned out to be related
!                            to variable XOUTMINI in the slst_bm_setup routine,
!                            and a rho-filter coefficient that wasn't scaled.
!                           Added trap forbidding negative P_INIT values for
!                            PBx geometries.
!                           Added separate SEMB_WEIGHT parameter for clarity.
!                           Made minor user documentation changes.
!                           Removed most commented/debug/obsolete code.
! 28. 2001-04-06 SMCook     Removed restriction on time and tau ranges.
!                           Removed restriction forcing use of FOR2/INV2 modes.
!                           Still disabled:  PB and PB1 methods.
!                           Still disabled:  Semblance noise attenuation.
!                           Corrected the GUI (comments concerning disabled
!                            options were removed, OFF_LAST and P_LAST were
!                            previously omitted, etc.).
!                           Removed all references to earlier attempts at
!                            interprocess communication (had experimented with
!                            basing defaults on earlier SLST in same job).
! 27. 2000-12-29 SMCook     Changed wrapped_up to skip_wrapup.
! 26. 2000-12-05 SMCook     Corrected operation of 3-point interpolator in inner
!                            loop.  Terms had been wrongly modified during the
!                            recent conversion from the old system, causing
!                            noise in the output.
! 25. 2000-11-29 SMCook     (MAJOR) Converted to new CPS system.  Much of
!                            the preliminary work by Mike O'Brian.
! 24. 1998-11-11 Goodger    Begin using fortran90 compiler.
! 23. 1997-05-12 Vunderink  Save original HW 64
! 22. 1990-10-22 Baumel     Added DIR=INV1 and INV2, and HUD# parameter,
!                           GEOM=PB and PB1, semblance WINDOWING, more
!                           robust setup (e.g., independent of trace
!                           order), extensive documentation re-write.
! 21. 1989-10-06 Baumel     Added DIR=FOR1 and FOR2 options.
! 20. 1989-05-27 Baumel     Improved hyperbolic velocity windowing option.
! 19. 1989-02-06 Baumel     Semblance calculation more robust.
! 18. 1989-01-05 Baumel     Match new CPS velocity file conventions;
!                           also add alternate return if offsets out of
!                           range.
! 17. 1988-11-21 Baumel     Extra created traces have dead headers
!                           rather than copies of first input trace.
! 16. 1988-11-14 Baumel     Allow variable-degree semblance weighting.
! 15. 1988-10-27 Baumel     Allow X0 and P0 (P_INIT) negative when GEOM=L.
! 14. 1988-09-29 Sinton     NWIH conversion and SQRTFN removed.
! 13. 1988-08-17 Baumel     Match new form of TVFBPS primitive.
! 12. 1988-06-10 Baumel     Add Semblance Weighting feature.
! 11. 1987-11-11 Baumel     &  Hill Improve efficiency of
!                           interpolation for velocity windowing in
!                           subroutine SLSTCAL.
! 10. 1987-05-03 Baumel     Interpolate vel. functions with INTPVELF.
! 9.  1987-01-16 Baumel     Add IPRT parameter (print switch).
! 8.  1986-12-14 Baumel     Add "exact" Hankel slant stack.
! 7.  1986-10-29 Baumel     Multiple velocity functions for windowing.
! 6.  1986-09-02 Baumel     Change SQRT to SQRTFN, add T0NEW parameter.
! 5.  1986-08-22 Baumel     RHO filter by FFT instead of convolution.
! 4.  1986-08-18 Baumel     Call to MEMPRT added.
! 3.  1986-07-30 Baumel     PARAMETERS CHANGED (for memory management).
! 2.  1986-07-20 Baumel     Change Header use;  Bandpass in RHO filter.
! 1.  1986-06-28 Baumel     Add hyperbolic velocity windowing. 
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
!
! 
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
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
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
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
!    The OPT_GEOM parameter determines the mathematical algorithm used for
!    computing the slant stack.  OPT_GEOM=LS yields a "simple" slant stack
!    that theoretically yields plane-wave decomposition in a 2-D earth
!    with line-sources, and is given (FORward direction) by:
!                                     /
!                F(p,tau)   =   K RHO | dx f(x,t=tau+p*x)            (1)
!                                     /
!    where RHO is a "RHO filter" which acts only on T or TAU coordinate,
!    and K is a normalization factor.  The K factor used with OPT_GEOM=LS is
!    rather arbitrary, namely  K = 1000*SQRT(2*PI) in FORward transform;
!    this was chosen to simplify the calculation when OPT_GEOM=PA (note 15).
!    You do (approximately) recover original amplitudes after FORward
!    and INVerse transforms.
!
!    The "point-source" OPT_GEOM options (PJ, PA, PB, PB1) provide various
!    implementations of the formula (in FORward direction):
!                                    /
!                 F(p,tau)   =   RHO | d(2)x f(x,t=tau+p.x)          (2)
!                                    /
!    where, in this case, "x" and "p" are 2-dimensional vectors in the
!    horizontal plane (thus "p.x" is a dot product, and the integration
!    is 2-dimensional), but all functions of x and p are assumed to be
!    cylindrically symmetric.
!
!    The point-source (cylindrical) slant stack is theoretically the
!    correct plane-wave decomposition for a point source in a 3D (but
!    horizontally-stratified) earth.  It includes compensation for the
!    geometrical spreading associated with cylidrical geometry.  It may be
!    applied to CMP gathers if structure is not severe, but only the
!    line-source (OPT_GEOM=LS) slant stack should be used for the slant
!    stacking of shot profiles.
!
!    The point-source (OPT_GEOM=Px) slant stack may produce more artifacts
!    than the line-source (OPT_GEOM=LS) option, but is required for applica-
!    tions such as INVERSION and AVO where you must scrupulously honor
!    the amplitudes of the 3D point-source data.  (Note that it would
!    not make sense to use a point-source slant stack if you've already
!    screwed up the amplitudes with XP or some similar process!)
!
!    OPT_GEOM=PJ option: This implementation of the cylindrical slant stack
!    is derived by expressing equation (2) in the FREQUENCY DOMAIN (by
!    Fourier transforming T and TAU variables), and casting the surface
!    integration into polar coordinates and analytically performing the
!    angular integration--which yields a J0 BESSEL FUNCTION.
!       This is a fairly accurate calculation of the point-source slant
!    stack, although the explicit computation of Bessel functions makes
!    it slow (about 3-4 times as slow as OPT_GEOM=LS).  Because the cylindri-
!    cal symmetry assumption mathematically rotates your data around the
!    source in a horizontal plane, the result may contain "reverse dip"
!    artifacts as you might expect from a slant stack of split-spread
!    data, even if your recording geometry was one sided!  Also, because
!    OPT_GEOM=PJ is implemented in the frequency domain, it is not compati-
!    ble with semblance windowing, which is a time-domain processes.
!
!    OPT_GEOM=PA option: This is an approximate point-source slant stack,
!    derived by replacing the Bessel function in GEOM=PJ with its high-
!    frequency asymptotic form.  The result is expressible very simply
!    in the TIME DOMAIN as a simple (OPT_GEOM=LS) slant stack with square
!    root pre- and post-weighting.  For the FORward transform, we pre-
!    multiply by SQRT(X) and post-multiply by 1/SQRT(P); for the INVerse
!    transform, pre-multiply by SQRT(P) and post-multiply by 1/SQRT(X).
!       Because of the high-frequency approximation, results are inac-
!    curate (in both amplitude and phase) at small P-values and Offsets
!    (which may not be too big a problem if your data has a large near-
!    offset).  OPT_GEOM=PA does NOT generate reverse-dip artifacts, because
!    part of the approximation is to neglect the half of the asymptotic
!    Bessel function containing the reverse dips.  OPT_GEOM=PA cannot pro-
!    duce an output trace at ZERO P-value or Offset; if you ask for one,
!    you get a dead trace.  OPT_GEOM=PA runs almost as rapidly as OPT_GEOM=LS.
!
!    OPT_GEOM=PB option: This is a TIME DOMAIN implementation of the point-
!    source slant stack which is mathematically equivalent to OPT_GEOM=PJ,
!    but runs faster.  It may be derived from equation 2 by casting the
!    integration in polar coordinates while staying in the time domain.
!    Perform the surface integral by doing first the radial integration
!    and then the angular integration.  The result is a weighted sum of
!    simple (OPT_GEOM=LS) slant stacks of offset-weighted data.
!       This concept was described in a Conoco Research Report by Mike
!    Howard (1983), but was first published by Brysk and McCowan (1986).
!    Execution time of OPT_GEOM=PB is a bit more than double that of 
!    OPT_GEOM=LS.  Like OPT_GEOM=PJ, it does produce reverse-dip artifacts.  
!    But because it's a time-domain process, these artifacts can be reduced
!    by using semblance windowing.
!
!    OPT_GEOM=PB1 option: This is the same as OPT_GEOM=PB, except that in the
!    intermediate step of doing simple slant stacks of offset-weighted
!    data, it ignores reverse dips.  This actually makes it nearly twice
!    as fast as OPT_GEOM=PB.  (Usually, it's just a little slower than
!    OPT_GEOM=LS.)   Like OPT_GEOM=PA, the OPT_GEOM=PB1 option avoids
!    reverse-dip artifacts, but suffers from some errors at small P-values and
!    Offsets.  However, the errors aren't as severe as with OPT_GEOM=PA because
!    there is no high-frequency approximation. If you generate an output
!    trace at ZERO P-value or Offset, the wavelets will be accurate, but
!    the amplitude will be exactly half the correct value.
!
!    If your input data lacks offsets close to zero, then any output
!    trace you generate at P=0 is really all artifact anyway.  In this
!    case, OPT_GEOM=PB1 is probably an excellent approximation in the region
!    where you actually have data.  Of course, if your data does contain
!    zero offset (e.g., if it's synthetic data) you should use OPT_GEOM=PJ
!    or OPT_GEOM=PB assuming you want a point-source slant stack.  (If you
!    want to apply the LINE-SOURCE slant stack to synthetic data, you
!    may wish to generate a TWO-SIDED synthetic gather--negative as well
!    as positive P-values or Offsets--before running SLST with OPT_GEOM=LS.)
!
!    A curious aspect of OPT_GEOM=PB and OPT_GEOM=PB1 is that the accuracy of
!    individual output traces can depend on which OTHER output traces
!    you've asked for.  More specifically, it depends on the fineness of
!    the grid of output P-values or Offsets that you request in the SLST
!    parameters.  This is because the intermediate step of calculating
!    simple slant stacks is performed on the same grid as you specify
!    for your output traces.  Thus, asking for a denser set of output
!    traces may improve the quality of individual traces.  Such an
!    effect never occurs with the other GEOM options (LS, PJ, or PA).
!
!    There is a second curious consequence due to identity of the inter-
!    mediate calculation grid and final output grid when OPT_GEOM=PB or PB1:
!    When DIR=INV2, SLST normally produces output traces at exactly the
!    original (X,T) offsets (as saved during a previous FOR2 transform).
!    But if OPT_GEOM=PB or PB1, those original offsets are projected onto
!    your requested output grid, and rounded to the nearest grid value.
!    Thus, you must be careful when setting OFF_INIT, OFF_INC, and OFF_TOT in an
!    INV2 transform if OPT_GEOM=PB or OPT_GEOM=PB1.
!
!    A recent GEOPHYSICS article by Kappus et al (1990) compared five
!    methods of calculating slant stacks.  Four of those methods are
!    available as options in SLST.  Here is a translation table:
!                Kappus et al term          SLST option
!                       2DM                     LS
!                       2D+                     PA
!                       BMC                     PB
!                       3DM                     PJ
!    Kappus' fifth method (which they call HOP) is a "linear inverse"
!    approach that is not part of SLST.  Conoco does, however, have an
!    inverse theory slant stack program, namely LSSS by Bill Harlan.
!    Note: LSSS implements the line-source slant stack, while the HOP
!    method discussed by Kappus et al is a point-source version.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS SLST Process/NC=80>
!
!                         [/C]SLant STack
!
!
!                                                        OPT_GEOM= `CC
!  OPT_DIR= `CCC      HDR_SAVE=`III
!
!
!  OFF_INIT=`FFFFFF   OFF_INC=`FFFFFF   OFF_LAST=`FFFFFF   OFF_TOT=`IIIII
!  P_INIT~~=`FFFFFF   P_INC~~=`FFFFFF   P_LAST~~=`FFFFFF   P_TOT~~=`IIIII
!
!
!  TSTRT_OUT=`FFFFFF       TAPER_BEG=`FFFFFF
!  TAU_MAX~~=`FFFFFF       TAPER_END=`FFFFFF
!
!
!  FREQ_LOW_NONE~~~=`FFFFFF
!  FREQ_LOW_FULL~~~=`FFFFFF
!  FREQ_HIGH_FULL~~=`FFFFFF
!  FREQ_HIGH_NONE~~=`FFFFFF
!
!
!     OPT_SEMB= `CC     SEMB_THRESH= `FFFFFF     SEMB_WEIGHT= `FFFFFF
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="OPT_DIR">
!<Tip> Direction for the slant stack transform. </Tip>
! Default = FOR2
! Allowed = FOR1   From (X,T) to (P,Tau), one way only.
! Allowed = FOR2   From (X,T) to (P,Tau), so you can return.
! Allowed = INV1   From (P,Tau) to (X,T), one way only.
! Allowed = INV2   From (P,Tau) to (X,T), returning after FOR2.
! If OPT_DIR = FOR1, transform from (X,T) to (P,Tau), setting headers so NMO 
! and stack can be done in (P,Tau) domain.
! 
! If OPT_DIR = FOR2, transform from (X,T) to (P,Tau), setting headers so you 
! can return to (X,T) with the original headers.
! 
! If OPT_DIR = INV1, transform from (P,Tau) to (X,T), if not returning after 
! OPT_DIR = FOR2 or if you want to specify the output offsets.
!
! If OPT_DIR = INV2, transform from (P,Tau) to (X,T), if returning after 
! OPT_DIR = FOR2.  Output offsets are the same as the original (X,T) traces.
!</Help>
!
!<Help KEYWORD="OPT_GEOM">
!<Tip> Slant stack algorithm to use. </Tip>
! Default = LS
! Allowed = LS     (Line  Source algorithm -- a simple slant stack.)
! Allowed = PJ     (Point Source algorithm -- J0 Bessel function.) 
! Allowed = PA     (Point Source algorithm -- asymptotic approximation.)
! Allowed = PB     (Point Source algorithm -- Brysk-McCowan method.)
! Allowed = PB1    (Same as PB but ignores reverse dip.) 
!</Help>
!
!<Help KEYWORD="OPT_SEMB">
!<Tip> Semblance option to use for artifact reduction. </Tip>
! Default = NONE
! Allowed = THRESH  (Accept scan only when Semblance >= SEMB_THRESH.)
! Allowed = WEIGHT  (Multiply output by Semblance**SEMB_WEIGHT.) 
! Allowed = NONE    (Do not use semblance artifact reduction.)
!</Help>
!
!<Help KEYWORD="SEMB_THRESH">
!<Tip> Semblance threshold value when OPT_SEMB = THRESH. </Tip>
! Default = 0.2
! Allowed = real > 0.0
! Suggested SEMB_THRESH values are in the range of 0.2 to 0.3.
!</Help>
!
!<Help KEYWORD="SEMB_WEIGHT">
!<Tip> Semblance exponent when OPT_SEMB = WEIGHT. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! Not active if OPT_GEOM = PJ.
!</Help>
!
!<Help KEYWORD="HDR_SAVE">
!<Tip> First of 3 consecutive header words for saving offset and mutes. </Tip>
! Default = 48
! Allowed = 1 - NWIH
! Header words HDR_SAVE, HDR_SAVE + 1 and HDR_SAVE + 2 are used to save the 
! original trace offset, head mute value and tail mute value when 
! OPT_DIR = FOR2 or INV2.  (The HDR_SAVE header words must be user defined 
! headers.)
!
! HDR_SAVE is ignored if OPT_DIR = FOR1 or INV1.
!</Help>
!
!----------------------input and output trace parameters------------------------
!
!<Help KEYWORD="OFF_INIT">
!<Tip> First offset to generate when OPT_DIR = INV1. </Tip>
! Default = 0.0
! Allowed = real
! OFF_INIT is only used when OPT_DIR = INV1 (and also when OPT_DIR = INV2 and
! OPT_GEOM = PB.)
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment for generated offsets when OPT_DIR = INV1. </Tip>
! Default = 0.0
! Allowed = real
! OFF_INC is only used when OPT_DIR = INV1 (and also when OPT_DIR = INV2 and
! OPT_GEOM = PB).
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Last offset to generate when OPT_DIR = INV1. </Tip>
! Default = 0.0
! Allowed = real
! OFF_LAST is only used when OPT_DIR = INV1 (and also when OPT_DIR = INV2 and
! OPT_GEOM = PB.)
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offsets to use (FOR) or generate (INV). </Tip>
! Default = NUMTR
! Allowed = int > 0
!</Help>
!
!
!<Help KEYWORD="P_INIT">
!<Tip> First P value to generate when OPT_DIR = FOR1 or FOR2. </Tip>
! Default = 0.0
! Allowed = real
! P_INIT is only used when OPT_DIR = FOR1 or FOR2.
!
! Note: P-value units are MICROseconds/distance.
!</Help>
!
!<Help KEYWORD="P_INC">
!<Tip> Increment for generated p-values when OPT_DIR = FOR1 or FOR2. </Tip>
! Default = 5.0
! Allowed = real >= 0.0
! P_INC is only used when OPT_DIR = FOR1 or FOR2.
!</Help>
!
!<Help KEYWORD="P_LAST">
!<Tip> Last P value to generate when OPT_DIR = FOR1 or FOR2. </Tip>
! Default = 0.0
! Allowed = real
! P_LAST is only used when OPT_DIR = FOR1 or FOR2.
!
! Note: P-value units are MICROseconds/distance.
!</Help>
!
!<Help KEYWORD="P_TOT">
!<Tip> Total number of P-values to use (INV) or generate (FOR). </Tip>
! Default = -
! Allowed = int > 0
! Normally, P_TOT should not be less than OFF_TOT.
!</Help>
!
!
!<Help KEYWORD="TSTRT_OUT">
!<Tip> Time of first sample in output traces (this changes the globals). </Tip>
! Default = TSTRT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TAU_MAX">
!<Tip> Maximum tau-value to use (INV) or generate (FOR). </Tip>
! Default = end of trace
! Allowed = real > 0.0
! This parameter does not change any globals.
!</Help>
!
!<Help KEYWORD="TAPER_BEG">
!<Tip> Number of traces to taper at start of gather before slant stack. </Tip>
! Default = 0
! Allowed = int >= 0
!</Help>
!
!<Help KEYWORD="TAPER_END">
!<Tip> Number of traces to taper at end of gather before slant stack. </Tip>
! Default = 0
! Allowed = int >= 0
!</Help>
!
!
!---------------------------RHO filter parameters-------------------------------
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Frequency (in Hz) where low frequency taper passes nothing. </Tip>
! Default = 0.0
! Allowed = real>=0.0
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Frequency (in Hz) where low freq taper passes full amplitude. </Tip>
! Default = 4.0
! Allowed = real>=FREQ_LOW_NONE
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> Frequency (in Hz) where high freq taper passes full amplitude. </Tip>
! Default = 75.0
! Allowed = real>=FREQ_LOW_FULL
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> Frequency (in Hz) where high frequency taper passes nothing. </Tip>
! Default = 85.0
! Allowed = Nyquist >= real >= FREQ_HIGH_FULL
!</Help>
!-------------------------------------------------------------------------------
!
!</HelpSection>


!-------------------------------------------------------------------------------
!
! NOTES FOR CONVERSION PROGRAMMER
!
!
!
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module slst_module

      use bandps_module
      use cio_module
      use fft_module
      use lav_module
      use median_module
      use mth_module
      use mutehw_module
      use named_constants_module
      use pattern_module
      use pc_module
      use rcsum_module
      use sizeof_module

      implicit none

      private
      public :: slst_create
      public :: slst_initialize
      public :: slst_update
      public :: slst_delete

!<execute_only>

      public :: slst           ! main trace processing routine.
      public :: slst_wrapup

!</execute_only>

      character(len=100),public,save :: SLST_IDENT = '$Id: slst.f90,v 1.41 2007/12/07 15:25:25 Menger beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

!---- the usual parameter structure:

      type,public :: slst_struct

        private

        logical              :: skip_wrapup    ! dependent variable
        integer              :: istat          ! dependent variable

        character(len=4)     :: opt_dir        ! User parameters
        character(len=3)     :: opt_geom       ! User parameters
        character(len=6)     :: opt_semb       ! User parameters

        character(len=3)     :: opt_print      ! User parameters

        real                 :: semb_thresh    ! User parameters
        real                 :: semb_weight    ! User parameters

        integer              :: hdr_save       ! User parameters

        real                 :: tstrt_out      ! User parameters
        real                 :: tau_max        ! User parameters
        real                 :: taper_beg      ! User parameters
        real                 :: taper_end      ! User parameters

        real                 :: off_init       ! User parameters
        real                 :: off_inc        ! User parameters
        real                 :: off_last       ! User parameters
        integer              :: off_tot        ! User parameters

        real                 :: p_init         ! User parameters
        real                 :: p_inc          ! User parameters
        real                 :: p_last         ! User parameters
        integer              :: p_tot          ! User parameters

        real                 :: f1             ! User parameters
        real                 :: f2             ! User parameters
        real                 :: f3             ! User parameters
        real                 :: f4             ! User parameters

        integer              :: nhb            ! User parameters
        integer              :: nhl            ! User parameters

        integer              :: npt            ! User parameters

        integer              :: ndpt           ! Global
        integer              :: ndpt_orig

        real                 :: tstrt          ! Global
        real                 :: tstrt_orig

        integer              :: numtr          ! Global
        integer              :: numtr_orig

        integer              :: nwih           ! Global
        real                 :: dt             ! Global
        logical              :: gathered       ! Global

!------ more dependent variables
        character(len=10)    :: ftyp           ! Filter type for bandps

        logical              :: phase_debug
        integer              :: debug_before_lun
        integer              :: debug_after_lun

        integer              :: prtlu          ! A unit for printing info
        integer              :: npow2          ! Power of 2 used for ffts
        integer              :: nw             ! NumSmp in freq domain
        integer              :: nwpt           ! ???????????????
        real                 :: dw             ! SmpInt in freq domain (w)
        real                 :: df             ! SmpInt in freq domain (Hz)
        real                 :: fnyq           ! Nyquist frequency
        integer              :: iw1,iw4        ! live w index endpoints
        integer              :: nw_live        ! number of non-zero w values

        integer              :: ntimax         ! Max input NDPT ?
        integer              :: ntomax         ! Max output NDPT ?
        integer              :: ntrmid         ! dependent variable
        integer              :: ntrin0         ! dependent variable
        real                 :: tfact          ! dependent variable
        real                 :: wdiff          ! dependent variable
        real                 :: annt           ! Amplitude of near trc taper
        real                 :: anft           ! Amplitude of far trc taper
        real                 :: shift0         ! dependent variable

        real                 :: dxin           ! dependent variable
!        integer              :: ntrot          ! dependent variable
        integer              :: ntrot0         ! dependent variable

        integer              :: ngrp           ! Counts groups processed
        integer              :: ntotal         ! Counts traces processed

        real,pointer         :: rtrace(:)      ! Real trace
        complex,pointer      :: ctrace(:)      ! Complex trace
        complex,pointer      :: rho(:)         ! Rho filter
        type(fft_struct),pointer:: rcfft       ! Forward fft object
        type(fft_struct),pointer:: crfft       ! Inverse fft object

        real                 :: xin0           ! dependent variable
        real                 :: xout0          ! dependent variable
        real                 :: xoutmin        ! dependent variable
        real                 :: dxout          ! dependent variable

        real, pointer        :: xout(:)        ! dependent variable
        real, pointer        :: tmptrc(:)      ! dependent variable
        real, pointer        :: sumsq(:)       ! dependent variable
        real, pointer        :: fold(:)        ! dependent variable
        real, pointer        :: scratch(:,:)   ! dependent variable

        real, pointer        :: coffbm(:)      ! dependent variable

      end type slst_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(slst_struct),pointer,save :: object      ! needed for traps

      integer,parameter              :: dir_nopt=4      ! used in setup
      character(len=4),save          :: dir_options(4)  ! used in traps

      integer,parameter              :: geom_nopt=5     ! used in setup
      character(len=3),save          :: geom_options(5) ! used in traps

      integer,parameter              :: semb_nopt=3     ! used in setup
      character(len=6),save          :: semb_options(3) ! used in traps

      data dir_options &
        /'FOR1','FOR2','INV1','INV2'/

      data geom_options &
        /'LS','PJ','PA','PB','PB1'/

      data semb_options &
        /'THRESH','WEIGHT','NONE'/


      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine slst_create (obj)
      implicit none
      type(slst_struct),pointer :: obj       ! arguments

!---- allocate the structure

      allocate (obj)

!---- nullify all pointers

      nullify (obj%rtrace)
      nullify (obj%ctrace)
      nullify (obj%rho)

      nullify (obj%rcfft)
      nullify (obj%crfft)

      nullify (obj%xout)
      nullify (obj%tmptrc)
      nullify (obj%sumsq)
      nullify (obj%fold)
      nullify (obj%scratch)

      nullify (obj%coffbm)


      call slst_initialize (obj)

      return
      end subroutine slst_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine slst_delete (obj)
      implicit none
      type(slst_struct),pointer :: obj       ! arguments

!<execute_only>
      call slst_wrapup (obj)
!</execute_only>

      if (associated(obj%rtrace))   deallocate (obj%rtrace)
      if (associated(obj%ctrace))   deallocate (obj%ctrace)
      if (associated(obj%rho))      deallocate (obj%rho)

      if (associated(obj%rcfft))    call fft_delete (obj%rcfft)
      if (associated(obj%crfft))    call fft_delete (obj%crfft)

      if (associated(obj%xout))     deallocate (obj%xout)
      if (associated(obj%tmptrc))   deallocate (obj%tmptrc)
      if (associated(obj%sumsq))    deallocate (obj%sumsq)
      if (associated(obj%fold))     deallocate (obj%fold)
      if (associated(obj%scratch))  deallocate (obj%scratch)

      if (associated(obj%coffbm))   deallocate (obj%coffbm)

      deallocate(obj)

      return
      end subroutine slst_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine slst_initialize (obj)
      implicit none
      type(slst_struct),pointer :: obj       ! arguments

!---- get global values in initialization
      obj%ndpt = FNIL
      call pc_get_global ('NDPT', obj%ndpt)
      if ( obj%ndpt == FNIL ) then
        call pc_error('SLST: Global param NDPT has not been properly set.')
      endif
      obj%ndpt_orig = obj%ndpt              ! invariant from here on

      obj%tstrt = FNIL
      call pc_get_global ('TSTRT', obj%tstrt)
      if ( obj%tstrt == FNIL ) then
        call pc_error('SLST: Global param TSTRT has not been properly set.')
      endif
      obj%tstrt_orig = obj%tstrt

      obj%dt = FNIL
      call pc_get_global ('DT', obj%dt)
      if ( obj%dt == FNIL ) then
        call pc_error('SLST: Global param DT has not been properly set.')
      endif

      obj%numtr = FNIL
      call pc_get_global ('NUMTR', obj%numtr)
      if ( obj%numtr == FNIL ) then
        call pc_error('SLST: Global param NUMTR has not been properly set.')
      endif
      obj%numtr_orig = obj%numtr

      obj%fnyq  = 0.5/obj%dt

      obj%opt_print = 'YES'           ! hardwired for now, maybe permanent?

!---- initialize process variables
      obj%phase_debug = .false. ! .true. turns on ASCII log, careful!!  SMCook

      obj%opt_dir  = 'FOR2'
      obj%opt_geom = 'LS'
      obj%opt_semb = 'NONE'
      obj%semb_thresh = 0.2
      obj%semb_weight = 1.0

      obj%hdr_save = HDR_USER_48

      obj%tstrt_out = obj%tstrt
      obj%tau_max   = obj%tstrt + (obj%ndpt-1)*obj%dt

      obj%taper_beg = 0
      obj%taper_end = 0

      obj%off_init = 0.0
      obj%off_inc  = 0.0
      obj%off_last = 0.0
      obj%off_tot  = obj%numtr

      obj%p_init   = 0.0
      obj%p_inc    = 5.0
      obj%p_tot    = obj%numtr
      obj%p_last   = 5.0 * (obj%numtr-1)

      obj%f1     = 0.0
      obj%f2     = 4.0
      obj%f3     = 75.0
      obj%f4     = 85.0

      obj%ftyp   = 'BANDPASS'

!---- initialize all the dependent variables
      obj%ntrmid    = 0

      obj%prtlu     = 0
      obj%npow2     = 0
      obj%nw        = 0
      obj%dw        = 0.0
      obj%df        = 0.0
      obj%iw1       = 0
      obj%iw4       = 0
      obj%nw_live   = 0

      call slst_update (obj)

      return
      end subroutine slst_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine slst_update (obj)
      implicit none
      type(slst_struct),intent(inout),target :: obj

      integer     ::   nstore,nscratch ! Local
      integer     :: ier1,ier2,ier3                 ! Local
      integer     :: update_state                   ! Local
      integer     :: ier,ibytes                     ! Local

      real, parameter    :: TWOPI = 2.0*PI

      integer            :: i,itmp
      integer            :: nxin,nxout

      integer            :: lstor,ltrot,lscr

!      integer :: NNYQ


      complex            :: cfact

      real               :: dwdelt

      double precision   :: fact






      real               :: tmp_f1, tmp_f2, tmp_f3, tmp_f4

      integer            :: result
      character(len=120) :: s
      character(len=98)  :: message


      integer        :: SIZEOF_REAL
      integer        :: SIZEOF_DOUBLE
      integer        :: SIZEOF_COMPLEX

      SIZEOF_REAL    = sizeof(1.0)
      SIZEOF_DOUBLE  = sizeof(1.0d0)
      SIZEOF_COMPLEX = sizeof(cmplx(1.0,1.0))


      object => obj         ! needed for traps.
      update_state = pc_get_update_state()

      obj%skip_wrapup = .true.

!---- initializations
      ltrot = 0
      lstor = 0
      lscr  = 0

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

!---- retrieve globals
      call pc_get_global ('NDPT'    , obj%ndpt    )
      call pc_get_global ('NWIH'    , obj%nwih    )
      call pc_get_global ('DT'      , obj%dt      )
      call pc_get_global ('TSTRT'   , obj%tstrt   )
      call pc_get_global ('GATHERED', obj%gathered)
      call pc_get_global ('NUMTR'   , obj%numtr)

      obj%fnyq  = 0.5/obj%dt    !!!!!! added 2006-10-24

      if(.not. obj%gathered) then
        call pc_error('SLST: This process requires gathered data.')
      endif

      if ( obj%ndpt <= 0 ) then
        call pc_error('SLST: Global parameter NDPT <= 0. Nothing to do.')
      endif

      if ( obj%nwih < HDR_NOMINAL_SIZE ) then
        call pc_error('SLST: Global parameter NWIH < HDR_NOMINAL_SIZE.')
      endif

      if ( obj%dt <= 0.0 ) then
        call pc_error('SLST: Global parameter DT <= 0.0.')
      endif

!---- retrieve user paramerters
      call pc_get ('OPT_DIR'        , obj%opt_dir    , slst_trap)
      call pc_get ('OPT_GEOM'       , obj%opt_geom   , slst_trap)
      call pc_get ('OPT_SEMB'       , obj%opt_semb   , slst_trap)
      call pc_get ('SEMB_THRESH'    , obj%semb_thresh, slst_trap)
      call pc_get ('SEMB_WEIGHT'    , obj%semb_weight, slst_trap)

      call pc_get ('HDR_SAVE'       , obj%hdr_save   , slst_trap)

      call pc_get ('TSTRT_OUT'      , obj%tstrt_out  , slst_trap)
      call pc_get ('TAU_MAX'        , obj%tau_max    , slst_trap)
      call pc_get ('TAPER_BEG'      , obj%taper_beg  , slst_trap)
      call pc_get ('TAPER_END'      , obj%taper_end  , slst_trap)

      call pc_get ('OFF_INIT'       , obj%off_init   , slst_trap)
      call pc_get ('OFF_INC'        , obj%off_inc    , slst_trap)
      call pc_get ('OFF_LAST'       , obj%off_last   , slst_trap)
      call pc_get ('OFF_TOT'        , obj%off_tot    , slst_trap)

      call pc_get ('P_INIT'         , obj%p_init     , slst_trap)
      call pc_get ('P_INC'          , obj%p_inc      , slst_trap)
      call pc_get ('P_LAST'         , obj%p_last     , slst_trap)
      call pc_get ('P_TOT'          , obj%p_tot      , slst_trap)

      call pc_get ('FREQ_LOW_NONE'  , obj%f1         , slst_trap)
      call pc_get ('FREQ_LOW_FULL'  , obj%f2         , slst_trap)
      call pc_get ('FREQ_HIGH_FULL' , obj%f3         , slst_trap)
      call pc_get ('FREQ_HIGH_NONE' , obj%f4         , slst_trap)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

!---- don't check unless sensitive 
      if(slst_offsets_are_user_specified(obj)) then
        ier = pattern_stop2('SLST:', .true.,                         &
          obj%off_init, obj%off_inc, obj%off_last, obj%off_tot,      &
          'OFF_INIT', 'OFF_INC', 'OFF_LAST', 'OFF_TOT',              &
          pc_verify_scalar('off_init'), pc_verify_scalar('off_inc'), &
          pc_verify_scalar('off_last'), pc_verify_scalar('off_tot'), &
          inc_min=1.0)
      end if

      ier = pattern_stop2('SLST:', .true.,                         &
        obj%p_init, obj%p_inc, obj%p_last, obj%p_tot,              &
        'P_INIT', 'P_INC', 'P_LAST', 'P_TOT',                      &
        pc_verify_scalar('p_init'), pc_verify_scalar('p_inc'),     &
        pc_verify_scalar('p_last'), pc_verify_scalar('p_tot'),     &
        inc_min=0.0)

!---- get the fft size
      obj%npow2 = 8
      do while ( obj%npow2 < obj%ndpt )
        obj%npow2 = obj%npow2*2
      enddo

!---- set nw and dw for frequency domain filters
      obj%nw = obj%npow2/2 + 1

      obj%df = obj%fnyq/(obj%nw-1)    ! ?? check this

      obj%dw = TWOPI*obj%df
      obj%iw1 = int(TWOPI*obj%f1/obj%dw + 1.9999)         !SMCook
      obj%iw4 = int(TWOPI*obj%f4/obj%dw + 1.0001)         !SMCook
      obj%nw_live = obj%iw4 - obj%iw1 + 1

!
!  Note: Will require   NWPT  >=  2*nw_live   for PJ process to work.
!
      if (obj%opt_geom == 'PJ') then
        if(obj%nwpt < 2*obj%nw_live) then
          call pc_error('SLST: For PJ geometry, NWPT must be >= 2*nw_live')
          call pc_error('SLST: Please increase TAUMAX or decrease bandwidth.')
   !      return       !!!!! removed 2006-10-24.
        end if
      end if

!---- dimensions depend on direction
      if(obj%opt_dir(1:3) == 'FOR') then
        obj%ntrin0 = obj%off_tot
        obj%ntrot0 = obj%p_tot
      else
        obj%ntrin0 = obj%p_tot
        obj%ntrot0 = obj%off_tot
      end if

      if (obj%opt_dir(1:3) == 'FOR') then
        obj%xin0  = obj%off_init
        obj%dxin  = obj%off_inc
        obj%xout0 = obj%p_init
        obj%dxout = obj%p_inc
      else
        obj%xin0  = obj%p_init
        obj%dxin  = obj%p_inc
        obj%xout0 = obj%off_init
        obj%dxout = obj%off_inc
      endif

!---- run the bandpass parameter checker
!---- the way bandps_check() changes values isn't very friendly on the
!---- front end so copies of the frequency points are used in place of
!---- the users frequencies

      if ( update_state /= PC_GUI ) then

        tmp_f1 = obj%f1
        tmp_f2 = obj%f2
        tmp_f3 = obj%f3
        tmp_f4 = obj%f4
        call bandps_check &
              (result, message, obj%fnyq, obj%ftyp, &
               tmp_f1, tmp_f2, tmp_f3, tmp_f4)

        select case (result)
          case (BANDPS_ERROR);    call pc_error(message)
          case (BANDPS_ENDERROR); call pc_error(message)
        end select

      endif


!---- figure out input/output dimensions
!---- note that ndpt may change
      if (obj%opt_dir(1:3) == 'FOR') then
        obj%tfact = 1.0/(1.e6*obj%dt)
        obj%ntimax = obj%ndpt_orig
        obj%ntomax = &
          nint(min((obj%tau_max-obj%tstrt_out)/obj%dt+1.0,real(obj%ndpt)))
      else

        itmp  = nxin
        nxin  = nxout
        nxout = itmp
        obj%tfact = -1.0/(1.0e6*obj%dt)

!        obj%ntimax =                                  &
!          nint(min(                                   &
!            (obj%tau_max-obj%tstrt_orig)/obj%dt+1.0,  &
!            real(obj%ndpt_orig)))

        obj%ntimax = obj%ndpt_orig      ! SMCook replaced ntimax assignment

        obj%ntomax = obj%ndpt
      endif

      if (obj%opt_geom == 'PJ') then
        obj%opt_semb = 'NONE'
      endif

      if (obj%ntrin0 <= 0) then
        if (obj%opt_dir(1:3) == 'FOR') then
          call pc_error('SLST: OFF_TOT not set for input (', obj%ntrin0, ')')
        elseif (obj%opt_dir(1:3) == 'INV') then
          call pc_error('SLST: P_TOT not set for input (', obj%ntrin0, ')')
        endif
      endif

      if (obj%dxout <= 0.0) then
        if (obj%opt_dir/='INV2' .or. obj%opt_geom(1:2)=='PB') then
          if (obj%opt_dir(1:3) == 'FOR') then
            call pc_error('SLST: P_INC not set for output (', obj%dxout, ')')
          elseif (obj%opt_dir(1:3) == 'INV') then
            call pc_error('SLST: OFF_INC not set for output (', obj%dxout, ')')
          endif
        endif
      endif

      if (obj%ntrot0 <= 0) then
        if (obj%opt_dir(1:3) == 'FOR') then
          call pc_error('SLST: P_TOT not set for output (', obj%ntrot0, ')')
        elseif (obj%opt_dir(1:3) == 'INV') then
          call pc_error('SLST: OFF_TOT not set for output (', obj%ntrot0, ')')
        endif
      endif

      if (obj%opt_print == 'YES')  then
        call pc_print('SLST:          Direction of slant stack OPT_DIR = ', &
                         obj%opt_dir)
        call pc_print('SLST:             Point or Line source OPT_GEOM = ', &
                         obj%opt_geom)
        call pc_print('SLST:           Semblance threshold SEMB_THRESH = ', &
                         obj%semb_thresh)
        call pc_print('SLST:           Semblance threshold SEMB_WEIGHT = ', &
                         obj%semb_weight)

        if (obj%opt_dir(1:3) == 'FOR') then
          call pc_print('SLST:                       Min Offset OFF_INIT = ', &
                           obj%off_init)
          call pc_print('SLST: Max traces used from input gather OFF_TOT = ', &
                           obj%off_tot)
          call pc_print('SLST: Smallest P-value for output gather P_INIT = ', &
                           obj%p_init)
          call pc_print('SLST:         Increment for output traces P_INC = ', &
                           obj%p_inc)
          call pc_print('SLST:   Number of Traces in Output Gather P_TOT = ', &
                           obj%p_tot)
          call pc_print('SLST: Maximum TAU to generate in Output TAU_MAX = ', &
                           obj%tau_max)

        elseif (obj%opt_dir(1:3) == 'INV') then

          call pc_print('SLST:                         Min P-value P_INIT = ', &
                           obj%p_init)
          call pc_print('SLST:  Max traces to use from input gather P_TOT = ', &
                           obj%ntrin0)
          call pc_print('SLST: Smallest offset for output gather OFF_INIT = ', &
                           obj%off_init)
          call pc_print('SLST:        Increment for output traces OFF_INC = ', &
                           obj%dxout)
          call pc_print('SLST:  Number of Traces in Output Gather OFF_TOT = ', &
                           obj%ntrot0)
          call pc_print('SLST:      Maximum TAU to use from input TAU_MAX = ', &
                           obj%tstrt_orig+(obj%ntimax-1)*obj%dt)
        endif

        call pc_print('SLST:  Number of Near traces to Taper TAPER_BEG = ', &
                         obj%taper_beg)
        call pc_print('SLST:  Number of Far  traces to Taper TAPER_END = ', &
                         obj%taper_end)
        call pc_print('SLST:     New TSTRT global for output TSTRT_OUT = ', &
                         obj%tstrt_out)

        call pc_print('SLST: Parameters for RHO filter:')
        call pc_print('        FREQ_LOW_NONE  = ',obj%f1)
        call pc_print('        FREQ_LOW_FULL  = ',obj%f2)
        call pc_print('        FREQ_HIGH_FULL = ',obj%f3)
        call pc_print('        FREQ_HIGH_NONE = ',obj%f4)
      endif


      obj%annt = 1.0/(obj%taper_beg+1)
      obj%anft = 1.0/(obj%taper_end+1)

      obj%shift0 = (obj%tstrt_out-obj%tstrt)/obj%dt

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

! Nothing to do.  FFTs are initialized on the backend.

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

!---- may need to change global start time??? not this release
      call pc_put_global('TSTRT',obj%tstrt_out)

!---- number of samples in trace may change
      obj%ndpt    = nint( (obj%tau_max-obj%tstrt_out)/obj%dt + 1.0 )
      obj%tau_max = obj%tstrt_out + (obj%ndpt-1)*obj%dt

      call pc_put_global('NDPT',obj%ndpt)

!---- may need to increase global maximum trace count
!---- variable numtr_orig prevents user from accidentally increasing this more
!---- than needed (erroneously retaining an inadvertantly high value due to a
!-----a user "typo")

      if(obj%opt_dir(1:3) == 'FOR') then
        if(obj%p_tot > obj%numtr_orig) then
          obj%numtr = obj%p_tot
          call pc_put_global('NUMTR',obj%numtr)
        end if
      end if

      call pc_put_options_field ('OPT_DIR' ,  dir_options,  dir_nopt)
      call pc_put_options_field ('OPT_GEOM', geom_options, geom_nopt)
      call pc_put_options_field ('OPT_SEMB', semb_options, semb_nopt)

      call pc_put ('OPT_DIR'       , obj%opt_dir    )
      call pc_put ('OPT_GEOM'      , obj%opt_geom   )
      call pc_put ('OPT_SEMB'      , obj%opt_semb   )
      call pc_put ('SEMB_THRESH'   , obj%semb_thresh)
      call pc_put ('SEMB_WEIGHT'   , obj%semb_weight)

      call pc_put ('HDR_SAVE'      , obj%hdr_save  )

      call pc_put ('TSTRT_OUT'     , obj%tstrt_out )
      call pc_put ('TAU_MAX'       , obj%tau_max    )
      call pc_put ('TAPER_BEG'     , obj%taper_beg )
      call pc_put ('TAPER_END'     , obj%taper_end )

      call pc_put ('OFF_INIT'      , obj%off_init  )
      call pc_put ('OFF_INC'       , obj%off_inc   )
      call pc_put ('OFF_LAST'      , obj%off_last  )
      call pc_put ('OFF_TOT'       , obj%off_tot   )

      call pc_put ('P_INIT'        , obj%p_init    )
      call pc_put ('P_INC'         , obj%p_inc     )
      call pc_put ('P_LAST'        , obj%p_last    )
      call pc_put ('P_TOT'         , obj%p_tot     )

      call pc_put ('FREQ_LOW_NONE'  , obj%f1       )
      call pc_put ('FREQ_LOW_FULL'  , obj%f2       )
      call pc_put ('FREQ_HIGH_FULL' , obj%f3       )
      call pc_put ('FREQ_HIGH_NONE' , obj%f4       )

!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
      if(obj%opt_dir(1:3) == 'FOR') then
        obj%nwpt = obj%ndpt
      else
        obj%nwpt = obj%ndpt_orig
      end if

      nscratch = min(obj%off_tot*obj%nwpt,obj%p_tot*obj%ntomax)
      nscratch = nscratch + max(obj%npow2 ,obj%nwih) &
                          + max(2*obj%nw ,obj%nwih) &
                          + max(obj%p_tot,obj%ntrmid)

!---- convert bytes to words
      nstore = 3*obj%nw*SIZEOF_COMPLEX
      nstore = nstore + (obj%npow2+2)*SIZEOF_REAL
      nstore = nstore + 2*fft_mem_usage(obj%npow2,1)*SIZEOF_REAL
      nstore = nstore/SIZEOF_REAL

      call pc_put_control ('NSTORE'  , nstore  )
      call pc_put_control ('NSCRATCH', nscratch)
      call pc_put_control ('nstore'  , nstore)
      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')


!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!

      if ( update_state == PC_GUI .or. update_state == PC_FRONTEND ) then
        call slst_set_sensitivities(obj)
      endif


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!---- set up RHO filter
      obj%df  = 1./(obj%npow2*obj%dt)
!      NNYQ = obj%npow2/2 + 1

!---- allocate the traces and arrays carried by slst_struct
      allocate (obj%rho(obj%nw)         , stat= ier1 )
      allocate (obj%rtrace(obj%npow2+2) , stat= ier2 )
      allocate (obj%ctrace(obj%nw)      , stat= ier3 )

      obj%rho    = cmplx(0.0,0.0)
      obj%rtrace = 0.0
      obj%ctrace = cmplx(0.0,0.0)

      allocate(obj%tmptrc(obj%ntomax))   ! used when semblance weighting
      allocate(obj%sumsq (obj%ntomax))
      allocate(obj%fold  (obj%ntomax))

      itmp=max(obj%ntimax,obj%ntomax)
      allocate(obj%scratch( itmp, itmp ))

!---- precompute the basic filter
      call bandps ( obj%rho(:), obj%nw, obj%df, obj%ftyp,  &
                    obj%f1, obj%f2, obj%f3, obj%f4 )

!---- modify filter depending on opt_geom
      if (obj%opt_geom == 'PJ') then

        if (obj%opt_dir(1:3) == 'FOR') then
          fact =  obj%df * TWOPI**2
        else
          fact = -obj%df / 1.e12
        endif
        dwdelt = (TWOPI*obj%shift0)/obj%npow2

        do i=obj%iw1,obj%iw4
          obj%rho(i) = obj%rho(i) *      cmplx(0.0,(i-1)*fact)        &
                                  * cexp(cmplx(0.0,(i-1)*dwdelt))
        enddo

        obj%wdiff  = TWOPI * obj%df / 1000000.0

      elseif (obj%opt_geom(1:2) == 'PB') then

        if (obj%opt_dir(1:3) == 'FOR') then
          fact = 2.0 * obj%df * TWOPI
        else
          fact = - obj%df / (PI * 1.e12)
        end if

        do i=obj%iw1,obj%iw4
          obj%rho(i) = obj%rho(i) * cmplx(0.0,(i-1)*fact)
        enddo

      else  ! applies to LS and PA

        if (obj%opt_dir(1:3) == 'FOR') then
          cfact = (sqrt(0.5*obj%df)*TWOPI) * (1.0, 1.0)
        else
          cfact = (sqrt(0.5*obj%df)/TWOPI) * (1.0,-1.0)
        endif

        do i=obj%iw1,obj%iw4
          obj%rho(i) = cfact * sqrt(real(i-1)) * obj%rho(i)
        enddo

      endif

!---- extra security in case iw1 and/or iw4 computations are a little off
      do i=1,obj%iw1-1
        obj%rho(i) = cmplx(0.0,0.0)
      end do

      do i=obj%iw4+1,obj%nw
        obj%rho(i) = cmplx(0.0,0.0)
      end do

!
!---- now just take the real part of the obj%nw array element?
!---- commented this line of code out -- SMCook
!---- doesn't make analytical sense -- scale factor seems inconsistent with
!----                                  what's done above
!
!      obj%rho(obj%nw) = cmplx (real(obj%rho(obj%nw)),0.0)
!

      obj%ntotal = 0
      obj%ngrp = 1


!---- setup for Brysk-McCowan method
      call slst_bm_setup(obj)    ! call setup for bm method

      lstor = 4*obj%npow2 + 6
!      if (obj%nbas /= 0) lstor = lstor + obj%nbas*obj%nlin*nxt*npt +   &
!                                      obj%nbas + obj%nlin + nxt + npt

      if (obj%opt_geom == 'PJ') then
        ltrot = obj%ntomax
        lscr  = min(obj%ntrin0*obj%ntimax,obj%ntrot0*ltrot)
      elseif (obj%opt_geom(1:2) /= 'PB') then
        lscr  = min(obj%ntrin0*obj%ntimax,obj%ntrot0*obj%ntomax)
      else
        lscr  = obj%ntrmid*obj%ntomax
        lstor = lstor + (obj%ntrmid*(obj%ntrmid+1))/2
      endif

      lscr = lscr + max(obj%npow2,obj%nwih,obj%ntrin0) +     &
                    max(obj%npow2+2,obj%nwih) +              &
                    max(obj%ntrot0,obj%ntrmid)

      if (obj%opt_geom(1:2) == 'PB' .OR. obj%opt_semb /= 'NONE') &
        lscr = lscr + obj%ntomax

      if (obj%opt_semb /= 'NONE') lscr = lscr + obj%ntomax   ! ???? again?

!      if (obj%nbas /= 0)   lscr = lscr + nxt*npt

!---- change global start time
      obj%tstrt = obj%tstrt_out


      allocate(obj%xout(max(obj%ntrot0,obj%ntrmid)))

!---- get an i/o unit for printing
      obj%prtlu = pc_get_lun()

!---- create forward and reverse fft objects
      ier = fft_create (obj%rcfft,-1,obj%npow2,'rtoc', &
                        opt_scale=1.0/real(obj%npow2))
      ier = fft_create (obj%crfft, 1,obj%npow2,'ctor')

!---- perform a final check on the RHO filter
!---- this time allow bandps_check() to alter values and report INFO message
      call bandps_check &
            (result, message, obj%fnyq, obj%ftyp, &
             obj%f1, obj%f2, obj%f3, obj%f4)

      select case (result)
        case (BANDPS_INFO);     call pc_info(message)
        case (BANDPS_ERROR);    call pc_error(message)
        case (BANDPS_ENDERROR); call pc_error(message)
      end select

!---- memory size calc uses nw*sizeof(complex) = (npow2+2)*sizeof(real)
      ibytes = 3*(obj%npow2+2) * SIZEOF_COMPLEX
      if (ier1/=0 .or. ier2/=0 .or. ier3/=0) then
        call pc_error('SLST: Unable to allocate memory for filter &
                      &and workspace ', ibytes,' bytes.')
      else
        call pc_print('SLST: Allocated ', ibytes, &
                      ' bytes for filter and workspace.')
        call pc_print('SLST: obj%nw   =',obj%nw);
        call pc_print('SLST: obj%npow2=',obj%npow2);
      endif

!---- debug file
      if(obj%phase_debug) then
        s = 'debug.slst.before.' // obj%opt_dir // '.txt'
        obj%debug_before_lun = cio_fopen(s, 'w')
        if(obj%debug_before_lun <= 0) then
          call pc_error ( &
            'file error, pathname = ', s)
        end if
        i = cio_fputline(obj%opt_dir,120,obj%debug_before_lun)

        s = 'debug.slst.after.' // obj%opt_dir // '.txt'
        obj%debug_after_lun = cio_fopen(s, 'w')
        if(obj%debug_after_lun <= 0) then
          call pc_error ( &
            'file error, pathname = ', s)
        end if
        i = cio_fputline(obj%opt_dir,120,obj%debug_after_lun)
      end if

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine slst_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
   
      subroutine slst_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

      integer             :: itmp,warning              ! local



!---- make sure tstrt_out is a multiple of the sample rate
      itmp = nint(object%tstrt_out/object%dt)
      object%tstrt_out = itmp * object%dt

!-------------------------------------------------------------------------------
! Mostly TEMPORARY PC_ERROR calls for preliminary release -- can intentionally
! exclude certain MODES OF OPERATION.
!-------------------------------------------------------------------------------
      warning = 0

!      if(object%opt_geom(1:2) == 'PB') then
!        call pc_warning( &
!         'SLST: Sorry, this SLST release does not allow OPT_GEOM = PB or PB1.')
!        object%opt_geom = 'LS'
!        warning = warning + 1
!      end if

!      if(object%opt_semb /= 'NONE') then
!        call pc_warning( &
!          'SLST: Sorry, this SLST release forces OPT_SEMB = NONE.')
!        object%opt_semb = 'NONE'
!        warning = warning + 1
!      end if

!      if(object%opt_dir(4:4) /= '2') then
!        call pc_warning( &
!          'SLST: Sorry, this SLST release forces OPT_DIR = FOR2 or INV2.')
!        object%opt_dir  = 'FOR2'
!        warning = warning + 1
!      end if

!---- possible warning situations?
!
!   if(object%opt_dir(1:3) == 'FOR' .and. object%off_tot /= object%numtr) then
!     call pc_warning('SLST: &
!       &Sorry, this SLST release forces FORWARD OFF_TOT = NUMTR (global).')
!     object%off_tot = object%numtr
!     warning = warning + 1
!   end if

!---- there may be a duplicate error check for this, but this is friendlier
      if(object%opt_dir(1:3) == 'FOR') then
        if(object%off_tot > object%numtr) then
          call pc_warning('SLST: OFF_TOT = ', object%off_tot)
          call pc_warning('SLST: NUMTR   = ', object%numtr)
          call pc_warning('SLST: OFF_TOT must be <= NUMTR global.')
          object%off_tot = object%numtr
          warning = warning + 1
        end if
      end if

!---- there may be a duplicate error check for this, but this is friendlier
      if(object%opt_dir(1:3) == 'INV') then
        if(object%p_tot > object%numtr) then
          call pc_warning('SLST: P_TOT = ', object%p_tot)
          call pc_warning('SLST: NUMTR = ', object%numtr)
          call pc_warning('SLST: P_TOT must be <= NUMTR global.')
          object%p_tot = object%numtr
          warning = warning + 1
        end if
      end if

!   if(object%opt_dir(1:3) == 'INV' .and. object%p_tot /= object%numtr) then
!     call pc_warning('SLST: &
!       &Sorry, this SLST release forces INVERSE P_TOT = NUMTR (global).')
!     object%p_tot = object%numtr
!     warning = warning + 1
!   end if

!      if(object%tstrt /= object%tstrt_out) then
!        call pc_warning('SLST: &
!          &Sorry, this SLST release forces TSTRT_OUT = TSTRT (global).')
!        object%tstrt_out = object%tstrt
!        warning = warning + 1
!      end if

!---- taper logic seems to work fine, so omit these traps
!
!      if(object%taper_beg /= 0) then
!        call pc_warning( &
!          'SLST: Sorry, this SLST release forces TAPER_BEG = 0.')
!        object%taper_beg = 0
!        warning = warning + 1
!      end if
!
!      if(object%taper_end /= 0) then
!        call pc_warning( &
!          'SLST: Sorry, this SLST release forces TAPER_END = 0.')
!        object%taper_end = 0
!        warning = warning + 1
!      end if


!---- EOTrace was at one time forced to be invariant
!      ftmp = object%tstrt_orig + (object%ndpt_orig-1)*object%dt
!      if(abs(object%tau_max-ftmp) > .00001) then
!        call pc_warning('SLST: tau_max = ', object%tau_max)
!        call pc_warning('SLST: EOTrace = ', ftmp)
!        call pc_warning( &
!         'SLST: Sorry, this SLST release forces TAU_MAX = EOTrace = ',ftmp,'.')
!        object%tau_max = ftmp
!        warning = warning + 1
!      end if

      if(warning > 1) then
        call pc_warning('Illegal values were automatically reset by SLST.')
      else if(warning > 0) then
        call pc_warning('Illegal value was automatically reset by SLST.')
      end if

!-------------------------------------------------------------------------------

!---- tapers must have sensible values
      if(object%taper_beg < 0 .or. object%taper_beg > object%numtr/2) then
        call pc_error( &
          'SLST: TAPER_BEG must be between 0 and NUMTR/2, inclusive.')
      end if

      if(object%taper_end < 0 .or. object%taper_end > object%numtr/2) then
        call pc_error( &
          'SLST: TAPER_END must be between 0 and NUMTR/2, inclusive.')
      end if

!---- must have enough traces in P-Tau domain
      if(object%p_tot < object%off_tot) then
        call pc_error('SLST: P_TOT must be greater than or equal to OFF_TOT.')
      end if

!---- etc.
      keyword_select: select case (keyword)

        case ('OPT_DIR') keyword_select
          if ( all(dir_options/=object%opt_dir)) then
            call pc_error &
                ('SLST: Invalid value for DIR, '//object%opt_dir//'. &
                 &Resetting to default, FOR2.')
            object%opt_dir = 'FOR2'
          endif

        case ('OPT_GEOM') keyword_select
          if ( all(geom_options /= object%opt_geom)) then
            call pc_error &
                ('SLST: Invalid value for GEOM, '//object%opt_geom//'. &
                 &Resetting to default geometry = LS.')
            object%opt_geom = 'LS'
          endif

          if(object%opt_geom(1:2) == 'PB' .and. object%p_init < 0) then
            call pc_error( &
              'SLST: P_INIT must be >= 0 for OPT_GEOM=PBx.')
          end if

        case ('HDR_SAVE') keyword_select
          if ( object%hdr_save < 1 .or. object%hdr_save > object%nwih ) then
            call pc_error &
                ('SLST: HDR_SAVE must be in the range 1 to ',object%nwih)
            call pc_error &
                 ('Resetting HDR_SAVE to default value of ',HDR_USER_48)
            object%hdr_save = HDR_USER_48
          endif

        case ('OFF_TOT') keyword_select
          if (object%off_tot == FNIL) then
            call pc_error ('SLST: OFF_TOT field cannot be blank.')
          endif

          if (object%off_tot > object%numtr) then
            call pc_error ('SLST: Value of OFF_TOT exceeds NUMTR global.')
          endif

        case ('P_INC') keyword_select
          if (object%opt_dir(1:3) == 'FOR') then
            if (object%p_inc == FNIL) then
              call pc_error ('SLST: P_INC field cannot be blank.')
            endif
          endif

        case ('P_TOT') keyword_select
          if (object%opt_dir(1:3) == 'FOR') then
            if (object%p_tot == INIL) then
              call pc_error ('SLST: P_TOT field cannot be blank.')
            endif
          endif

!       case ('TSTRT_OUT') keyword_select
!         ntau_min = &
!           min(nint((object%tau_max-object%tstrt_out)/object%dt),object%ndpt-1)
!         if (object%opt_dir(1:3) == 'FOR') then
!           object%tau_max = object%tstrt_out + ntau_min*object%dt
!         endif

!        case ('TAU_MAX') keyword_select
!          ntau_min = &
!          min(nint((object%tau_max-object%tstrt_out)/object%dt),object%ndpt-1)
!          if (object%opt_dir(1:3) == 'FOR') then
!            object%tau_max = object%tstrt_out + ntau_min*object%dt
!          elseif (object%opt_dir(1:3) == 'INV') then
!            object%tau_max = object%tstrt + ntau_min*object%dt
!          endif

        case ('FREQ_LOW_NONE') keyword_select
          if (object%f1 ==   FNIL    ) object%f1 = 0.0
          if (object%f1 <     0.0    ) object%f1 = 0.0
          if (object%f1 > object%fnyq) object%f1 = object%fnyq

        case ('FREQ_LOW_FULL') keyword_select
          if (object%f2 ==   FNIL    ) object%f2 = 0.0
          if (object%f2 <     0.0    ) object%f2 = 0.0
          if (object%f2 > object%fnyq) object%f2 = object%fnyq

        case ('FREQ_HIGH_FULL') keyword_select
          if (object%f3 ==   FNIL    ) object%f3 = object%fnyq
          if (object%f3 <     0.0    ) object%f3 = 0.0
          if (object%f3 > object%fnyq) object%f3 = object%fnyq

        case ('FREQ_HIGH_NONE') keyword_select
          if (object%f4 ==   FNIL    ) object%f4 = object%fnyq
          if (object%f4 <     0.0    ) object%f4 = 0.0
          if (object%f4 > object%fnyq) object%f4 = object%fnyq

        case ('OPT_SEMB') keyword_select

          if (object%opt_semb == 'THRESH') then
            if(object%semb_thresh < 0.0) &
              call pc_error ('SLST: SEMB_THRESH value must be positive.')
          end if

          if (object%opt_semb == 'WEIGHT') then
            if(object%semb_weight < 0.0) &
              call pc_error ('SLST: SEMB_WEIGHT value must be positive.')
          end if

      end select keyword_select

      return
      end subroutine slst_trap


!-------------------------------------------------------------------------------
! Subroutine to handle all the GUI sensitivity settings during update.
!-------------------------------------------------------------------------------
      subroutine slst_set_sensitivities(obj)
      implicit none
      type(slst_struct) :: obj

!---- hdr_save is ignored for FOR1 and INV1
      if(obj%opt_dir(4:4) == '2') then
        call pc_put_sensitive_field_flag ('HDR_SAVE', .false.)
      else
        call pc_put_sensitive_field_flag ('HDR_SAVE', .true.)
      end if

!---- sensible sensitivity for threshold/weight
      select case (obj%opt_semb)
        case ('THRESH')
          call pc_put_sensitive_field_flag ('SEMB_THRESH' , .true. )
          call pc_put_sensitive_field_flag ('SEMB_WEIGHT' , .false.)
        case ('WEIGHT')
          call pc_put_sensitive_field_flag ('SEMB_THRESH' , .false. )
          call pc_put_sensitive_field_flag ('SEMB_WEIGHT' , .true.)
        case ('NONE')
          call pc_put_sensitive_field_flag ('SEMB_THRESH' , .false. )
          call pc_put_sensitive_field_flag ('SEMB_WEIGHT' , .false.)
      end select

      call pc_put_sensitive_field_flag ('OPT_SEMB' , .true. )
      select case (obj%opt_geom)
        case ('LS')
          !No changes.
        case ('PJ')
          call pc_put_sensitive_field_flag ('OPT_SEMB'    , .false. )
          call pc_put_sensitive_field_flag ('SEMB_THRESH' , .false. )
          call pc_put_sensitive_field_flag ('SEMB_WEIGHT' , .false. )
        case ('PA')
          !No changes.
        case ('PB')
          !No changes.
        case ('PB1')
          !No changes.
      end select

      if(slst_offsets_are_user_specified(obj)) then
        call pc_put_sensitive_field_flag ('OFF_INIT' , .true. )
        call pc_put_sensitive_field_flag ('OFF_INC'  , .true. )
        call pc_put_sensitive_field_flag ('OFF_LAST' , .true. )
      else
        call pc_put_sensitive_field_flag ('OFF_INIT' , .false. )
        call pc_put_sensitive_field_flag ('OFF_INC'  , .false. )
        call pc_put_sensitive_field_flag ('OFF_LAST' , .false. )

        obj%off_init = 0.
        obj%off_inc  = 0.
        obj%off_last = 0.
        call pc_put('OFF_INIT' , obj%off_init)
        call pc_put('OFF_INC'  , obj%off_inc)
        call pc_put('OFF_LAST' , obj%off_last)
      end if

      return
      end subroutine slst_set_sensitivities


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine slst (obj,ntr,hd,tr)
      implicit none
      type(slst_struct)               :: obj               ! arguments
      integer,         intent(inout)  :: ntr               ! arguments
      double precision,intent(inout)  :: hd(:,:)           ! arguments
      real,            intent(inout)  :: tr(:,:)           ! arguments

      character(len=60)          :: s1,s2,s3,s4,s5
      real                       :: ftmp1,ftmp2

      integer                    :: i,j  ,icount ! local
      integer                    :: ntrin,ntrot,ltrot
      integer                    :: numzero


      integer                    :: nhdcnt(obj%nwih)
      double precision           :: hdsum(obj%nwih)        ! local

!-------------------------------------------------------------------------------
      if (ntr == NO_MORE_TRACES) then
        obj%tstrt = obj%tstrt_out
        return
      endif

!---- SCAN HEADERS TO DECIDE WHICH TRACES AND HEADER INFO TO USE:
      ntrin = min(ntr,obj%ntrin0)

      if (obj%opt_dir == 'INV2') then

        do j=1,min(ntrin,obj%ntrot0)
          if (hd(obj%hdr_save,j) < -1.e25) exit
        enddo

        ntrot = j-1
        if (ntrot <= 0) then
          call pc_error('SLST: encountered alternate return 2')
          go to 999
        end if

        if (obj%opt_geom(1:2) /= 'PB') THEN
          obj%xout(1:ntrot) = hd(obj%hdr_save,1:ntrot)
        endif

      else

        ntrot = obj%ntrot0

        if (obj%opt_dir == 'FOR1') then

          hdsum(1:obj%nwih) = 0.0
          nhdcnt(1:obj%nwih) = 0.0

          do j=1,ntrin
            do i=1,obj%nwih
              hdsum(i) = hdsum(i) + hd(i,j)
              if (hd(i,j) /= 0.0)  nhdcnt(i) = nhdcnt(i) + 1
            enddo
          enddo

          do i=1,obj%nwih
            if (i == HDR_OFFSET) cycle
            if (nhdcnt(i) /= 0)  hdsum(i) = hdsum(i) / nhdcnt(i)
            hd(i,1:ntrot) = hdsum(i)
          enddo

        else

          hd(1:obj%nwih,ntrin+1:ntrot) = 0.0

          if (obj%opt_dir == 'FOR2') then       ! Save original offsets

            do j=1,min(ntrin,ntrot)
              hd(obj%hdr_save  ,j) = hd(HDR_OFFSET,j)
              hd(obj%hdr_save+1,j) =                               &
                obj%tstrt + (hd(HDR_TOP_MUTE,j)-1.)*obj%dt
              hd(obj%hdr_save+2,j) =                               &
                obj%tstrt + (hd(HDR_BOTTOM_MUTE,j)-1.)*obj%dt
            enddo

            do j=ntrin+1,ntrot
              hd(obj%hdr_save,j) = -1.E30
            enddo

          endif

        endif

        if (obj%opt_geom(1:2) /= 'PB') then
          do j=1,ntrot
            obj%xout(j)=mth_bin_center(obj%xout0,obj%dxout,j)
!rev37            obj%xout(j) = obj%xout0 + (j-1)*obj%dxout
          end do
        endif

      endif

      if (obj%opt_geom /= 'LS') then
        hd(HDR_OFFSET,1:ntrin) = abs(hd(HDR_OFFSET,1:ntrin))
      end if

      if (obj%opt_geom == 'PJ' .or. obj%opt_geom == 'PA') then
        obj%xout(1:ntrot) = abs(obj%xout(1:ntrot))

      elseif (obj%opt_geom(1:2) == 'PB') then
        obj%xout(1) = 0.0
        do j=2,obj%ntrmid
          obj%xout(j) = obj%xoutmin + (j-2)*abs(obj%dxout)
        end do

      endif

!-------------------------------------------------------------------------------
! ASCII debug dump of amplitude and phase spectra BEFORE.
!-------------------------------------------------------------------------------
      if(obj%phase_debug) then
        do i=1,ntrin,5
          call fft_rc_transform(obj%rcfft,tr(:,i),obj%ctrace)

          do j=1,obj%nw
            ftmp1 =  real(obj%ctrace(j))
            ftmp2 = aimag(obj%ctrace(j))
            call string_ii2cc(j ,s1)
            call string_ff2cc(sqrt(ftmp1**2 + ftmp2**2) ,s2)
            call string_ff2cc(ftmp1                     ,s3)
            call string_ff2cc(ftmp2                     ,s4)
            call string_ff2cc(atan2(ftmp1,ftmp2)        ,s5)
            icount = cio_fputline( &
              trim(s1)     // ' '&
              &// trim(s2) // ' '&
              &// trim(s3) // ' '&
              &// trim(s4) // ' '&
              &// trim(s5) ,     &
              120,obj%debug_before_lun)
          end do

        end do
      end if

!-------------------------------------------------------------------------------
! Branch to proper code for geometry.
!-------------------------------------------------------------------------------
      obj%scratch = 0.0

      select case (obj%opt_geom)

!-------------------------------------------------------------------------------
! SLANT STACK FOR LINE SOURCE (LS) OR ASYMPTOTIC POINT SOURCE.
!-------------------------------------------------------------------------------
        case('LS','PA')

          if (ntrot*obj%ntomax  >=  ntrin*obj%ntimax)  then
            obj%scratch(1:obj%ntimax,1:ntrin) = tr(1:obj%ntimax,1:ntrin)

            call slst_calc ( &
              obj,hd,ntrin,obj%scratch,obj%ntimax,ntrot,tr,obj%nwpt)
            if(obj%istat > 0) then
              call pc_error('SLST: slst_calc (1) istat = ',obj%istat)
              go to 999
            end if

            call slst_rho (obj,ntrot,tr,obj%nwpt,tr,obj%nwpt)

          else
            call slst_calc ( &
              obj,hd,ntrin,tr,obj%nwpt,ntrot,obj%scratch,obj%ntomax)
            if(obj%istat > 0) then
              call pc_error('SLST: slst_calc (2) istat = ',obj%istat)
              go to 999
            end if

            call slst_rho (obj,ntrot,obj%scratch,obj%ntomax,tr,obj%nwpt)

          endif

!-------------------------------------------------------------------------------
! POINT-SOURCE SLANT STACK USING BRYSK-McCOWAN METHOD.
!-------------------------------------------------------------------------------
        case('PB','PB1')

!-------- it is critical to note that slst_bm_apply routine assumes that the
!-------- obj%scratch array contains the input!!!

          call slst_calc ( &
            obj,hd,ntrin,tr,obj%nwpt,obj%ntrmid,obj%scratch,obj%ntomax)
          if(obj%istat > 0) then
            call pc_error('SLST: slst_calc (3) istat = ',obj%istat)
            go to 999
          end if

          tr = 0.0
          call slst_bm_apply (obj,hd,obj%ntrmid,tr,obj%nwpt)
          call slst_rho (obj,ntrot,tr,obj%nwpt,tr,obj%nwpt)

!-------------------------------------------------------------------------------
! POINT-SOURCE SLANT STACK USING J0 BESSEL FUNCTION.
!-------------------------------------------------------------------------------
        case('PJ')

          ltrot = max (obj%ntomax,2*obj%nw_live)

          if (ntrot*ltrot  >  ntrin*obj%ntimax)  then

            obj%scratch(1:obj%ntimax,1:ntrin) = tr(1:obj%ntimax,1:ntrin)

            call slst_exj ( &
              obj,hd,ntrin,obj%scratch,obj%ntimax,ntrot,tr,obj%nwpt)

          else

            call slst_exj ( &
              obj,hd,ntrin,tr,obj%nwpt,ntrot,obj%scratch,ltrot)

            tr(1:obj%ntomax,1:ntrot) = obj%scratch(1:obj%ntomax,1:ntrot)
          end if

          tr(obj%ntomax+1:obj%ndpt,1:ntrot) = 0.0

      end select

!-------------------------------------------------------------------------------
! ASCII debug dump of amplitude and phase spectra AFTER.
!-------------------------------------------------------------------------------
      if(obj%phase_debug) then
        do i=1,ntrot,5
          call fft_rc_transform(obj%rcfft,tr(:,i),obj%ctrace)

          do j=1,obj%nw
            ftmp1 =  real(obj%ctrace(j))
            ftmp2 = aimag(obj%ctrace(j))
            call string_ii2cc(j ,s1)
            call string_ff2cc(sqrt(ftmp1**2 + ftmp2**2) ,s2)
            call string_ff2cc(ftmp1                     ,s3)
            call string_ff2cc(ftmp2                     ,s4)
            call string_ff2cc(atan2(ftmp1,ftmp2)        ,s5)
            icount = cio_fputline( &
              trim(s1)     // ' '&
              &// trim(s2) // ' '&
              &// trim(s3) // ' '&
              &// trim(s4) // ' '&
              &// trim(s5) ,     &
              120,obj%debug_after_lun)
          end do

        end do
      end if

!-------------------------------------------------------------------------------
! Set headers for output.
!-------------------------------------------------------------------------------
      if (obj%opt_dir == 'INV2') then

        if (obj%opt_geom(1:2) /= 'PB') then
          hd(HDR_OFFSET,1:ntrot) = hd(obj%hdr_save,1:ntrot)
        endif

        do j=1,ntrot

          numzero = &
            nint(min &
              (max( real((hd(obj%hdr_save+1,j)-obj%tstrt_out)/obj%dt),0.0 ), &
                real(obj%ndpt) ) )

          tr(1:numzero,j) = 0.0
          hd(HDR_TOP_MUTE,j) = numzero + 1

          numzero = &
            nint(min &
              (max( real((hd(obj%hdr_save+2,j)-obj%tstrt_out)/obj%dt+1),0.0 ), &
                 real(obj%ndpt+1)))

          if (numzero < obj%ndpt) tr(numzero+1:obj%ndpt,j) = 0.0
          hd(HDR_BOTTOM_MUTE,j) = numzero

        enddo

      else

        if (obj%opt_geom(1:2) /= 'PB') then
          do j=1,ntrot
            hd(HDR_OFFSET,j)=mth_bin_center(obj%xout0,obj%dxout,j)
          enddo
!rev37        hd(HDR_OFFSET,1:ntrot) = obj%xout0 + obj%dxout*(/(j,j=0,ntrot-1)/)
        endif

        hd(HDR_TOP_MUTE,1:ntrot) = 1.0
      endif

      hd(HDR_SEQUENCE,       1:ntrot) = obj%ntotal + (/(j,j=1,ntrot)/)
      hd(HDR_CURRENT_GROUP,  1:ntrot) = obj%ngrp
      hd(HDR_CURRENT_CHANNEL,1:ntrot) = (/(j,j=1,ntrot)/)

      obj%ntotal = obj%ntotal + ntrot
      obj%ngrp = obj%ngrp + 1


      ntr = ntrot

      go to 1000
 999  ntr=FATAL_ERROR
      return

!-------------------------------------------------------------------------------
! Reset LAV and return.
!-------------------------------------------------------------------------------
1000  call lav_set_hdr(hd, tr, obj%ndpt, ntr)


      return
      end subroutine slst

!-------------------------------------------------------------------------------
! Subroutine with the actual number-crunching guts of the slant stack process.
! It is included to permit flexibility in choosing the arrays to contain the
! input and output of the stack.
!-------------------------------------------------------------------------------
      subroutine slst_calc (obj,hd,ntrin,trin,ltrin,ntrot,trot,ltrot)
      implicit none
      type(slst_struct)  :: obj                  ! Arguments
      double precision   :: hd(:,:)
      integer            :: ntrin
      integer            :: ltrin
      real,intent(inout) :: trin(ltrin,*)
      integer            :: ntrot
      integer            :: ltrot
      real,intent(out)   :: trot(ltrot,*)

      integer, parameter  :: isemwin=15
      integer, parameter  :: isemcnt=(isemwin+1)/2
      integer, parameter  :: isem1  =isemcnt-1
      integer, parameter  :: isem2  =isemcnt-2
      integer, parameter  :: msubary=10
      integer, parameter  :: narymax=2*msubary+1

      integer          :: nsubary,i1ary(narymax),i2ary(narymax)


      integer          :: i,j,k
      integer          :: ishift      ,kshift 
      integer          ::                 iary  

      integer          :: NTOMAX,ITSTART,ITSTOP

      real             :: shift

      real             :: tfact1

      real             :: afact0,afact

      real             :: aplus,azero,aminus
      real             :: value
      real             :: x  
      real             :: xx                     !  ,xxl,vlimit
      real             :: pp           

      real             :: TRCOUT(ltrot)
      real             :: TMPBUF(ltrot)

!---- initializations
      NTOMAX    = obj%ntomax                ! for brevity, legacy, and speed?
      ITSTART   = 1
      ITSTOP    = obj%ntimax

      obj%istat = 0                         ! return error flag

!---- velocity windowing NOT active

      i1ary(1) = 1
      if (obj%opt_semb == 'THRESH') then
        nsubary = max ( min(nint(ntrin/real(msubary)),msubary) , 1 )
      else
        nsubary = 1
      endif
      if (nsubary > 1) then
        nsubary = 2*nsubary + 1
        afact0 = real(ntrin)/(nsubary-1)
        i2ary(1:nsubary+1) = nint( afact0*(/(i,i=1,nsubary+1)/) )
        i1ary(2) = 1
        i1ary(3:nsubary) = i2ary(1:nsubary-2) + 1
      endif
      i2ary(nsubary) = ntrin

!---- scale the input traces (includes taper)
      if (obj%opt_geom(1:2) == 'PB') then
        afact0 =         slst_dx(obj,ntrin,hd,HDR_OFFSET,obj%rtrace)
      elseif (obj%opt_dir(1:3) == 'FOR') then
        afact0 = 1.e+3 * slst_dx(obj,ntrin,hd,HDR_OFFSET,obj%rtrace)
      else
        afact0 = 1.e-9 * slst_dx(obj,ntrin,hd,HDR_OFFSET,obj%rtrace)
      endif

      do j=1,ntrin
        xx = hd(HDR_OFFSET,j)

!------ pre-multiply
        if (obj%opt_geom == 'LS') then
          afact = afact0
        else if(obj%opt_geom == 'PA') then
          afact = afact0 * sqrt(xx)
        else
          afact = afact0 * xx
        end if

        if (j <= obj%taper_beg) &
          afact = afact*slst_tpr(obj%annt*j)
        if (j > (ntrin-obj%taper_end)) &
          afact = afact*slst_tpr(obj%anft*((ntrin+1)-j))

        trin(1:obj%ntimax,j) = afact * trin(1:obj%ntimax,j)

      enddo


!---- OUTER LOOP OVER OUTPUT TRACES
      j_loop: do j=1,ntrot

        obj%tmptrc = 0.0

        tfact1 = obj%tfact

!------ possibly needed when semblance option is turned on
!        if (obj%opt_semb /= 'NONE') then
!          trot(1:NTOMAX,j) = 0.0
!        endif

        pp = obj%xout(j)

!------ LOOP OVER SEMBLANCE SUBARRAYS
!------   (zero arrays for an output trace and come back
!------   here with flipped sign of TFACT1 when obj%opt_geom='PB')

    199 continue

        TRCOUT = 0.0

        do iary=1,nsubary

          if (obj%opt_semb /= 'NONE') then
            obj%tmptrc(1:NTOMAX) = 0.0
            obj%sumsq (1:NTOMAX) = 0.0
            obj%fold  (1:NTOMAX) = 0.0
          endif

!------- LOOP OVER INPUT TRACES (within semblance subarray)

          do i=i1ary(iary),i2ary(iary)

            xx = hd(HDR_OFFSET,i)
            shift = tfact1 * xx * pp

!            if(obj%opt_dir(1:3) == 'INV') then
!              write(*,*) 'pp, xx, shift=',pp,',',xx,',',shift
!            end if

            if (obj%opt_geom == 'PA' .and. shift == 0.)  cycle

            shift = shift + obj%shift0
            ishift = nint(shift)

            x  = shift - ishift

            aminus = 0.5 * x * (x-1.)
            azero  = 1.0 - x**2
            aplus  = 0.5 * x * (x+1.)

!
!---------- here's the MAIN (INNERMOST) calculation loop
!
!---------- code generated by VAST is very hard to read here--went back to
!---------- original
            if (obj%opt_semb == 'NONE')  then

              DO K=MAX(1,ITSTART+1-ISHIFT),MIN(NTOMAX,ITSTOP-1-ISHIFT)
                 kshift = k + ishift
                 TRCOUT(K) = TRCOUT(K) + AMINUS*TRIN(kshift - 1,I) +    &
                                          AZERO*TRIN(kshift    ,I) +    &
                                          APLUS*TRIN(kshift + 1,I)
              END DO

            else

              do k=max(1,itstart+1-ishift),min(NTOMAX,itstop-1-ishift)
                kshift = k + ishift
                value =  aminus*trin(kshift-1,i) &
                       + azero *trin(kshift  ,i) &
                       + aplus *trin(kshift+1,i)
                obj%tmptrc(k) = obj%tmptrc(k) + value
                obj%sumsq(k)  = obj%sumsq(k) + value**2
                obj%fold(k)   = obj%fold(k) + 1.0
              enddo

            endif

          enddo

!-------- we have finished looping within a semblance subarray.
!-------- now finish semblance calculation for this subarray if needed
          if (obj%opt_semb /= 'NONE') then

            call rcsum (NTOMAX,obj%sumsq,obj%rtrace)
            do k=isemcnt+1,NTOMAX-isem1
              if (obj%fold(k) >= 3.0 .and. obj%sumsq(k) > 0.0) then
                obj%fold(k) = &
                  obj%fold(k) * (obj%rtrace(k+isem1)-obj%rtrace(k-isemcnt))
!-------------- protective code needed, SMCook
                if(obj%fold(k) /= 0.0) obj%fold(k) =  1.0 / obj%fold(k)
              else
                obj%fold(k) = 0.0
              endif
            enddo
            obj%sumsq(1:NTOMAX) = obj%tmptrc(1:NTOMAX)**2

            call rcsum (NTOMAX,obj%sumsq,obj%rtrace)
            if (nsubary > 1) then

              do k=isemcnt+1,NTOMAX-isem1
                obj%fold(k) = &
                  obj%fold(k) * (obj%rtrace(k+isem1)-obj%rtrace(k-isemcnt))
                if(obj%fold(k) >= obj%semb_thresh) &
                  trcout(k) = trcout(k) + 0.5*obj%tmptrc(k)
              enddo

            elseif (obj%opt_semb == 'THRESH') then

              do k=isemcnt+1,NTOMAX-isem1
                obj%fold(k) = &
                  obj%fold(k) * (obj%rtrace(k+isem1)-obj%rtrace(k-isemcnt))
                if(obj%fold(k) >= obj%semb_thresh) &
                  trcout(k) = trcout(k) + obj%tmptrc(k)
              enddo

            else

              do k=isemcnt+1,NTOMAX-isem1
                obj%fold(k) = &
                  obj%fold(k) * (obj%rtrace(k+isem1)-obj%rtrace(k-isemcnt))
                trcout(k) = trcout(k) + &
                  obj%tmptrc(k)*obj%fold(k)**obj%semb_weight
              enddo

            endif

          endif

        enddo

!
! We have finished looping over all input traces.  Now scale the output trace,
! and re-do loop with reversed sign of TFACT1 if GEOM=PB:
!
!------ PA post-multiplication
        if (obj%opt_geom == 'PA') then
          afact = sqrt(pp)
          if (afact > 0.0) then
            TRCOUT = TRCOUT / afact
          else
            TRCOUT = 0.0
          end if

          trot(1:NTOMAX,j) = TRCOUT

!------ special handling for PB case
        elseif (obj%opt_geom == 'PB') then
          if (tfact1 == obj%tfact) then              ! PB, first time through
            tfact1 = -obj%tfact
            TMPBUF = TRCOUT
            go to 199
          else
            trot(1:NTOMAX,j) = TMPBUF + TRCOUT
          endif

!------ others
        else
          trot(1:NTOMAX,j) = TRCOUT
        endif


      enddo j_loop

      return
      end subroutine slst_calc


!-----------------------------------------------------------------------------
!   Subroutine to perform the "exact" point-source slant stack using explicit
!   computation of Bessel functions.
!-----------------------------------------------------------------------------
      subroutine slst_exj (obj,hd,ntrin,trin,ltrin,ntrot,trot,ltrot)
      implicit none

      type(slst_struct)    :: obj                           ! Arguments
      double precision     :: hd(:,:)
      integer              :: ntrin
      integer              :: ltrin
      real                 :: trin(ltrin,ntrin)
      integer              :: ntrot
      integer              :: ltrot
      real                 :: trot(ltrot,ntrot)

      ! Probably should be allocated in update              ! local
      complex, allocatable  :: cslst(:,:)

      integer  :: itr_in, itr_out, nlow, k
      real     :: x, xx, dx, r, f, d, aj0
      real     :: afact, afact0

! The following parameters are weights for the Bessel function
! These are double precision because some have 8 significant digits
      double precision,parameter :: a1=-2.2499997, a2=+1.2656208
      double precision,parameter :: a3=-0.3163866, a4=+0.0444479
      double precision,parameter :: a5=-0.0039444, a6=+0.0002100

      double precision,parameter :: f0=+0.79788456, f1=-0.00000077
      double precision,parameter :: f2=-0.00552740, f3=-0.00009512
      double precision,parameter :: f4=+0.00137237, f5=-0.00072805
      double precision,parameter :: f6=+0.00014476

      double precision,parameter :: d0=-0.78539816, d1=-0.04166397
      double precision,parameter :: d2=-0.00003954, d3=+0.00262573
      double precision,parameter :: d4=-0.00054125, d5=-0.00029333
      double precision,parameter :: d6=+0.00013558
!------------------------------------------------------
!
! Set up an array to hold the complex slant stack
! This should probably be allocated in the update routine and carried
! around in the object.
!------------------------------------------------------

      allocate(cslst(obj%nw,ntrot))
      cslst(1:obj%nw,1:ntrot) = 0.0

      afact0 = slst_dx(obj,ntrin,hd,HDR_OFFSET,obj%rtrace)

      do itr_in=1,ntrin

!------ retrieve the taper weight for this trace
        xx = hd(HDR_OFFSET,itr_in)
        afact = afact0 * xx

        if (itr_in <= obj%taper_beg) &
          afact = afact * slst_tpr(obj%annt*itr_in)
        if (itr_in > (ntrin-obj%taper_end)) &
          afact = afact * slst_tpr(obj%anft*((ntrin+1)-itr_in))

!------ apply the taper and transform to the frequency domain
        obj%rtrace(1:obj%ntimax) = afact * trin(1:obj%ntimax,itr_in)
        obj%rtrace(obj%ntimax+1:obj%npow2) = 0.0
        call fft_rc_transform(obj%rcfft,obj%rtrace,obj%ctrace)


!------ slant stack the data
        do itr_out=1,ntrot

          dx = obj%wdiff * xx * obj%xout(itr_out)
          if (dx == 0.0) then
            nlow = obj%iw4
          else
            nlow = 3.0/dx + 1.0
          endif

          do k=obj%iw1,min(nlow,obj%iw4)

            x = (k-1)*dx
            r = (x/3.0)**2
            aj0 = 1.0+r*(a1+r*(a2+r*(a3+r*(a4+r*(a5+r*a6)))))
!---------- make sure cslst() is allocated with nw elements
            cslst(k,itr_out) = cslst(k,itr_out) + aj0*obj%ctrace(k)
          enddo

          do k=max(nlow+1,obj%iw1),obj%iw4
            x = (k-1)*dx
            r = 3.0/x
            f =   f0+r*(f1+r*(f2+r*(f3+r*(f4+r*(f5+r*f6)))))
            d = x+d0+r*(d1+r*(d2+r*(d3+r*(d4+r*(d5+r*d6)))))
            aj0 = f * cos(d) / sqrt(x)
            cslst(k,itr_out) = cslst(k,itr_out) + aj0*obj%ctrace(k)
          enddo

        enddo

      enddo

! transform the complex slant stack back to the time domain
      do itr_out=1,ntrot
        obj%ctrace(1:obj%nw) = cslst(1:obj%nw,itr_out) * obj%rho(1:obj%nw)
        call fft_cr_transform(obj%crfft,obj%ctrace,obj%rtrace)
        trot(1:obj%ntomax,itr_out) = obj%rtrace(1:obj%ntomax)
      enddo

      deallocate(cslst)

      return
      end subroutine slst_exj


!------------------------------------------------------------------------------
! A function to tell whether user-specified offsets are being used.
! This is true only for certain combinations of parameters.
!------------------------------------------------------------------------------
      function slst_offsets_are_user_specified(obj)
      implicit none
      type(slst_struct)  :: obj
      logical :: slst_offsets_are_user_specified

      slst_offsets_are_user_specified = .true.

      if(obj%opt_dir(1:4) == 'INV1') return

      if(obj%opt_dir(1:4) == 'INV2' .and. obj%opt_geom(1:2) == 'PB') return

      slst_offsets_are_user_specified = .false.

      end function slst_offsets_are_user_specified


!------------------------------------------------------------------------------
! A function to get a taper weight.
!------------------------------------------------------------------------------
      function slst_tpr(x)
      implicit none
      real :: x,slst_tpr

      slst_tpr = (3.0-2.0*x) * x**2

      end function slst_tpr

!-----------------------------------------------------------------------
!  Function to scan the OFFSETS or P-VALUES of the INPUT traces
!  (which may be ordered arbitrarily within the gather), and determine
!  the appropriate Offset or P-value INCREMENT for integration.
!     NTRIN = Number of input traces.
!     HD    = Header array.
!     NH    = Header word containing Offset or P-value of Input traces.
!     XWORK = Work array of dimension at least NTRIN.
!-----------------------------------------------------------------------


      function slst_dx (obj,ntrin,hd,nh,xwork)
      implicit none
      real  :: slst_dx

      type(slst_struct)  :: obj                   ! Arguments
      integer            :: ntrin
      double precision   :: hd(:,:)
      integer            :: nh
      real               :: xwork(:)

      integer         :: imax(1)
      integer         :: nmax,ndiff
      real            :: valmax(1)


      if (ntrin < 2) then
        slst_dx = 1.0
        return
      elseif (ntrin == 2) then
        slst_dx = abs(hd(HDR_OFFSET,2)-hd(HDR_OFFSET,1))
        return
      endif
      xwork(1:ntrin) = hd(HDR_OFFSET,1:ntrin)
      do nmax=ntrin,2,-1
        imax = maxloc(xwork(1:nmax))
        if (imax(1) < nmax) then
          valmax(1)      = xwork(imax(1))
          xwork(imax(1)) = xwork(nmax)
          xwork(nmax)    = valmax(1)
        endif
      enddo
      if (ntrin > 6) then
        ndiff = (ntrin-1)/2
        xwork(1:ndiff) = xwork(2*1+1:2*ndiff+1) - xwork(2*1-1:2*ndiff-1)
      else
        ndiff = ntrin-1
        xwork(1:ndiff) = xwork(1+1:ndiff+1) - xwork(1:ndiff)
      endif
      call median (xwork,ndiff,slst_dx)
      if (ntrin > 6)  slst_dx = 0.5*slst_dx

      return
      end function slst_dx


!-------------------------------------------------------------------------------
!  Subroutine to apply the RHO filter to the traces in TRIN array
!  and put results in the TROT array.
!     NTROT = Number of traces to apply RHO filter to.
!     TRIN  = Input trace array.
!     LTRIN = Memory allocation per trace in TRIN array.
!     TROT  = Output trace array.
!     LTROT = Memory allocation per trace in TROT array.
!  The arguments LTRIN and LTROT provide DIMENSIONING information for
!  the 2-dimensional trace arrays.  The actual WORKING trace length is
!  assumed to be NTOMAX for the input traces and NT for output traces
!  (NTOMAX and NT are both in common block SLSTP1).
!  Note: TRIN and TROT may be the SAME array if LTROT.le.LTRIN.
!-------------------------------------------------------------------------------
      subroutine slst_rho (obj,ntrot,trin,ltrin,trot,ltrot)
      implicit none
      type(slst_struct)  :: obj                   ! Arguments

      integer            :: ntrot,ltrin,ltrot
      real               :: trin(:,:)
      real               :: trot(:,:)

      integer            :: i,j

      do i = 1,ntrot
        obj%rtrace = 0.0
        obj%rtrace(1:obj%ntomax)  = trin(1:obj%ntomax,i)

        obj%ctrace = cmplx(0.0,0.0)
        call fft_rc_transform(obj%rcfft,obj%rtrace,obj%ctrace)

        do j=1,obj%nw
          obj%ctrace(j) = obj%rho(j) * obj%ctrace(j)
        enddo

        obj%rtrace = 0.0
        call fft_cr_transform(obj%crfft,obj%ctrace,obj%rtrace)


        trot(1:obj%ntomax,i) = obj%rtrace(1:obj%ntomax)

      enddo

      return
      end subroutine slst_rho


!-------------------------------------------------------------------------------
!  SETUP FOR BRYSK-McCOWAN METHOD (GEOM=PB or PB1).               
!  This subroutine scans the range of output P or X, and sets the  
!  table of weights for Brysk-McCowan integration.                  
!  (It also includes a call to GETS to ALLOCATE STORAGE for this    
!  weighting table.)                                              
!-------------------------------------------------------------------------------
      subroutine slst_bm_setup (obj)
      implicit none
      type(slst_struct)  :: obj                   ! Arguments


      integer            :: i,n

      real               :: dxa,dxai
      real               :: x,xmax,xoutmini,xoutlst,xoutmax,xmax2

      real,allocatable   :: bsin(:),bsqrt(:)


      if (obj%opt_geom(1:2) /= 'PB') then
        obj%ntrmid = 0
        return
      endif

!---- determine range of output X or P
      xoutlst = mth_bin_center(obj%xout0,obj%dxout,ibin=obj%ntrot0)
!rev37      xoutlst = obj%xout0 + (obj%ntrot0-1)*obj%dxout
      dxa = abs(obj%dxout)

      if (obj%xout0*xoutlst >= 0.) then
        obj%xoutmin = mod ( abs(obj%xout0) , dxa )
      else
        obj%xoutmin = abs ( obj%xout0 - nint(obj%xout0/obj%dxout)*obj%dxout )
      endif

      if (obj%xoutmin < dxa/10000.)  obj%xoutmin = dxa
      xoutmax = max ( abs(obj%xout0) , abs(xoutlst) , obj%xoutmin )
      obj%ntrmid = 2 + nint((xoutmax-obj%xoutmin)/dxa)

!---- allocate STORAGE for Integration Weight table
!----   (now that value of ntrmid is known)
      allocate (obj%coffbm(obj%ntrmid))

      allocate(bsin(obj%ntrmid))
      allocate(bsqrt(obj%ntrmid))

!---- fill the table
      dxai = 1.0/dxa
      xoutmini = 1.0/obj%xoutmin             ! SMCook fixed problem here.

      bsin(:)  = 0.0
      bsqrt(:) = 0.0

      obj%coffbm(1) = asin(1.0)
      do n=2,obj%ntrmid

        xmax = obj%xoutmin + (n-2)*dxa
        xmax2 = xmax ** 2

        do i=2,n
          x = obj%xoutmin + (i-2)*dxa
          bsin(i)  = asin ( x/xmax )
          bsqrt(i) = sqrt ( xmax2 - x**2 )
        enddo

        obj%coffbm(1) = bsin(2) - xoutmini*(xmax-bsqrt(2))
        obj%coffbm(2) = xoutmini*(xmax-bsqrt(2))

        do i=3,n
          x = obj%xoutmin + (i-3)*dxa
          obj%coffbm(i) = dxai * ( bsqrt(i-1)-bsqrt(i) - x*(bsin(i)-bsin(i-1)) )
        enddo

        do i=2,n-1
          x = obj%xoutmin + (i-1)*dxa
          obj%coffbm(i) =  obj%coffbm(i) &
                     + dxai * ( x*(bsin(i+1)-bsin(i)) - (bsqrt(i)-bsqrt(i+1)) )
        enddo

      enddo

      deallocate(bsin)
      deallocate(bsqrt)

      end subroutine slst_bm_setup

!-------------------------------------------------------------------------------
!  APPLY BRYSK-McCOWAN RE-WEIGHTING (GEOM=PB or PB1).                 
!  This subroutine computes the point-source slant stack as a         
!  weighted sum of line-source slant stacks.  The weights are first   
!  computed in subroutine SLSTBMS.  The input line-source slant       
!  stacks are found by subroutine SLSTCAL and are assumed to be in    
!  SCRATCH array SCR.  Output traces are put in array TROT.           
!  Notes: This subroutine also puts the correct Offset or P-value      
!         for the output traces in header word NH.                     
!         Formerly SLSTBMA.
!-------------------------------------------------------------------------------
!
      subroutine slst_bm_apply (obj,hd,ntrot,trot,ltrot)
      implicit none
      type(slst_struct)  :: obj                   ! Arguments

      double precision   :: hd(:,:)
      real               :: trot(:,:)

      integer            :: ntrot
      integer            :: ltrot

      integer            :: i,j,n,itrot
      real               :: xval,dxa,dxai


      dxa = abs(obj%dxout)
      dxai = 1.0/dxa

      do itrot=1,ntrot

        if (obj%opt_dir == 'INV2') then
          xval = hd(obj%hdr_save,itrot)
        else
          xval=mth_bin_center(obj%xout0,obj%dxout,itrot)
!rev37          xval = obj%xout0 + (itrot-1)*obj%dxout
        endif

        if (2.*abs(xval) < obj%xoutmin) then
          n = 1
          xval = 0.
        else
          n = min ( max(nint((abs(xval)-obj%xoutmin)*dxai)+2,2) , obj%ntrmid )
          xval = sign ( obj%xoutmin+(n-2)*dxa , xval )
        endif

        hd(HDR_OFFSET,itrot) = xval

        trot(1:obj%ntomax,itrot) = obj%coffbm(1)*obj%scratch(1:obj%ntomax,1)
        do i=2,n
          do j=1,obj%ntomax
            trot(j,i) = trot(j,i) + obj%coffbm(i)*obj%scratch(j,i)
          end do
        end do

      enddo

      return
      end subroutine slst_bm_apply


!-------------------------------------------------------------------------------
! Estimate number of traces needed for scratch space ?
!-------------------------------------------------------------------------------
      subroutine slst_bms2 (obj,ntrot)
      implicit none

      type(slst_struct)  :: obj                   ! Arguments
      integer            :: ntrot                 ! Arguments

      real     :: xoutmax, xoutlst, dxa           ! Local


      if (obj%opt_geom(1:2) /= 'PB') then
        obj%ntrmid = 0
        return
      endif

      xoutlst = mth_bin_center(obj%xout0,obj%dxout,ibin=obj%ntrot0)
!rev37      xoutlst = obj%xout0 + (ntrot-1)*obj%dxout
      dxa = abs(obj%dxout)

      if (obj%xout0*xoutlst >= 0.0) then
        obj%xoutmin = mod ( abs(obj%xout0) , dxa )
      else
        obj%xoutmin = abs ( obj%xout0 - nint(obj%xout0/obj%dxout)*obj%dxout )
      endif

      if (obj%xoutmin <= 0.0)  obj%xoutmin = dxa

      xoutmax = max ( abs(obj%xout0) , abs(xoutlst) , obj%xoutmin )
      obj%ntrmid = 2 + nint((xoutmax-obj%xoutmin)/dxa)

      return
      end subroutine slst_bms2

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine slst_wrapup (obj)
      implicit none
      type(slst_struct) :: obj       ! arguments

      integer           :: ier

!---- conditional return
      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

!---- flush and close debug file
      if(obj%phase_debug) then
        ier = cio_fflush(obj%debug_before_lun)
        if(ier > 0) then
          call pc_error('SLST: error flushing debug file')
          return
        end if
        ier = cio_fclose(obj%debug_before_lun)
        if(ier > 0) then
          call pc_error('SLST: error closing debug file')
          return
        end if

        ier = cio_fflush(obj%debug_after_lun)
        if(ier > 0) then
          call pc_error('SLST: error flushing debug file')
          return
        end if
        ier = cio_fclose(obj%debug_after_lun)
        if(ier > 0) then
          call pc_error('SLST: error closing debug file')
          return
        end if
      end if

      end subroutine slst_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module slst_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
