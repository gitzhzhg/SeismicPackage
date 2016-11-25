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
! Name       : SVA    (Semblance Velocity Analysis)
! Category   : velocity_analysis
! Written    : 1988-12-16   by: John Reed
! Revised    : 2006-11-14   by: D. Glover
! Maturity   : production
! Purpose    : Semblance stacking velocity analysis of CMP gathers.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! For each velocity scan, SVA does semblance stacking velocity analysis by
! calculating the semblance as a function of time after performing NMO
! correction on the CMP gather. This results in a "semblance panel," a plot of
! semblance versus two-way time and stacking velocity.
!
! Semblances are calculated in time windows defined by WIN_LEN and WIN_INC.
! You may choose to calculate either Standard or AVO-sensitive semblance
! by setting the TYPE_SEMB parameter:
!
! Standard semblance, as defined in the classic paper of Taner and Koehler
! (see references below), is the square of the stacked trace divided by the
! sum of squares of individual traces in the CMP.
!
! AVO-sensitive semblance is a generalized semblance measure intended to give
! better results for events with strong AVO, especially polarity reversals
! (see paper of Sarkar, Baumel and Larner). When using conventional semblance,
! an event with polarity reversal may fail to show up at the correct velocity
! (because the stack response is zero) and may produce peaks at incorrect
! velocities instead. Events with polarity reversals produce peaks at the
! correct velocity when using AVO-sensitive semblance.
!
! Whether computing Standard or AVO-sensitive semblance, the final semblance
! values output by SVA are normalized using an AGC-style expansion which
! preserves relative values for all velocities at a fixed time-level but
! alters relative values at different time levels.
!
!
! Velocity Scans
!
! SVA calculates the semblance as a function of time for each of a large number
! of trial stacking velocities (known as velocity scans).
!
! If TYPE_SCAN=CALC, SVA will calculate an array of velocity scans, starting
! with VEL_BEG, with an increment that increases with increasing velocity.  The
! first velocity scan increment will be no less than VEL_INC.  The calculated
! velocity scan increment is based on the stacking response (allowable stacking
! velocity variation that results in an optimum stack).  The stacking response
! is given by
!                      (To * V**3)/(4 * F * X**2),
!
! where To is the two-way travel time, V is the stacking velocity, F is the
! dominant frequency in the data and X is the offset.
!
! If TYPE_SCAN=CONST, SVA will use an array of velocity scans, starting with
! VEL_BEG, with a constant increment equal to VEL_INC.
!
! TYPE_SCAN is always CONST for fourth order residual NMO correction
! (ORDER_MO = 4).
!
!
! Output CMPs
!
! In addition to a semblance panel bytefile (with "svas" in the filename), SVA
! also outputs a bytefile (with "svat" in the filename) containing a central
! CMP gather from each group of NUM_SEMB CMPs that are presumed by SVA to come
! from a particular velocity analysis location.  The workstation application VA
! can be used to display these bytefiles.
!
!
! Regularizing Output CMPs
!
! The REGULAR option regularizes offsets and fold in CMPs, after the semblance
! calculation, by placing no more than one trace in each offset bin.  Traces
! are not composited, rather the first trace occupying an offset bin is used.
! (This method avoids artifacts created when compositing traces within bins
! containing a large offset range.)  This is the preferred method of creating a
! CMP output file with regular fold and offset for VA display.
!
!
! Order of Moveout
!
! Ordinary NMO is 2nd order, hyperbolic, NMO.  If the theoretical moveout
! expression is expanded in a series, the first non-zero term is the typical
! 2nd order, hyperbolic, NMO.  The next non-zero term is 4th order,
! non-hyperbolic, moveout.
!
! ORDER_MO allows choice between 2nd order moveout and 4th order residual
! moveout.  If 4th order residual moveout is chosen, SVA assumes that the
! 2nd order (regular NMO) correction has already been applied to the input
! data.
!
!
! References
!
! Taner and Koehler, Geophysics, Vol. 34, 1969, pp. 859-881.
!
! Sarkar, Baumel and Larner, Geophysics, Vol. 67, 2002, pp. 1664-1672.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Input Data
!
! Input data must be in CMP gathers without NMO correction.  Normally, input
! data has a strong amplitude balance applied such as XP and has a filter
! applied, such as (8 - 45 Hz).
!
!
! Compositing Semblances
!
! Semblance panels are always calculated on individual CMPs as input.  SVA
! will composite NUM_SEMB semblance panels together after they are calculated
! individually.  (Usually, NUM_SEMB corresponds to the number of adjacent CMPs
! from each velocity analysis location that are input to SVA.)  If super-CMPs
! or composited CMPs are desired, they must be constructed before using SVA.
!
!
! Sensitivity to WIN_LEN
!
! Theory suggests that AVO-sensitive semblance may be more sensitive than
! conventional semblance to choice of the window length (WIN_LEN). This hasn't
! yet been tested extensively as the AVO-sensitive option is being added to
! SVA (2002-11-13).
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! Process requires traces to be input in CMP gathers without NMO correction.
! If ORDER_MO = 4, ordinary NMO correction must have already been applied
! to the input data.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces as it receives unaltered.
! This process outputs traces with same gather status as the input traces.
!
! SVA does not alter traces in the job stream. The REAL output of this process
! is in the two bytefiles that it generates (*svas.trc8 and *svat.trc8 files).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! GATHERED  whether traces are a legitimate gather  checked (must be true)
! NUMTR     max number of traces input/output       checked (must be > 1)
! NDPT      number of sample values in trace        used but not changed
! NWIH      number of words in each header          used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                  Action taken
! ----    -----------                  ------------
!
!  1      sequential trace count       renumbered in bytefiles (not job stream)
!  2      top mute                     used but not changed
!  3      current group (CMP)          used but not changed
!         HDR_OFF (user parameter)     used but not changed
!  7      midpoint xgrid               copied to output headers
!  8      midpoint ygrid               copied to output headers
! 17      midpoint xloc                copied to output headers
! 18      midpoint yloc                copied to output headers
! 19      midpoint elevation           used but not changed
! 25      largest absolute value       used but not changed
! 37      midpoint shotpoint           copied to output headers
! 38      midpoint line                copied to output headers
! 64      bottom mute                  used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!098. 2006-11-14 D. Glover  Added NULLIFY statements for Intel compiler.
!097. 2006-01-10 B. Menger  Removed Unused Variables.
! 96. 2003-06-17 Baumel     Added AVO-sensitive semblance option; also general
!                           speed-up, bug fixes and documentation improvements.
! 95. 2002-10-25 Stoeckley  Require TYPE_SCAN to be CONST for 4th order moveout;
!                           improve some documentation and fix printout error.
! 94. 2002-03-11 Selzler    Corrected error message (500 to 4000 limit).
! 93. 2002-01-23 Selzler    Validate nwin during frontend processing and
!                           eliminate extraneous validation of OFF_INC and
!                           OFF_TOT.
! 92. 2001-11-30 Selzler    Correct OFF_INIT, _INC, _LAST, _TOT update loop.
! 91. 2001-11-16 Selzler    Force 2 GB extent size for output files.
! 90. 2001-06-04 Selzler    Corrected facp array size and memory overrun.
!                           Latent bug from old CPS.
! 89. 2001-02-15 Stoeckley  Change to use PERMTFILE (which uses TRCIO) instead
!                            of BYTE to output semblance and CMP files.
! 88. 2001-01-22 Stoeckley  Add error message when REGULAR = YES and the
!                           OFF_... parameters are not set.
! 87. 2000-12-08 Stoeckley  Change wrapup flag.
! 86. 2000-09-05 Coleman    Changed VEL_BEG, VEL_END, & VEL_INC defaults again.
! 85. 2000-08-29 Coleman    Changed defaults for VEL_BEG, VEL_END, & VEL_INC
!                           when SURVEY_UNITS = METERS.
! 84. 2000-08-21 Coleman    Changed default for REGULAR to YES.
! 83. 2000-06-14 Coleman    Fixed bug that caused VEL_INC to always be equal to
!                           30 in CALC mode; uses LAV primative to set LAV in
!                           bytefile output; insures that the global GATHERED is
!                           true; & sets both min and max size of HDR_DIAG to 3.
! 82. 2000-05-19 Coleman    Fixed bug that erroneously reported a fatal error
! 81. 2000-05-17 Coleman    Added gui_def
! 80. 2000-04-17 Coleman    Removed write to history file
! 79. 2000-03-30 Coleman    Added dynamic default for WIN_INC, changed default
!                           for HDR_DIAG from (3,7,8) to (3,7,37), changed
!                           default for REGULAR to NO, and moved the HDSEM
!                           array into the structure; thus, fixing the
!                           annotation bug among other things.
! 78. 2000-03-10 Coleman    Added pc_do_not_process_traces check in sva_wrapup.
! 77. 2000-03-08 Coleman    Convert from old system w/ general rewrite
! 76. 1999-02-15 Vunderink  Fixed FVINC and FMAX to work as documented
! 75. 1999-01-11 Vunderink  Add missing routine from old plotting code
! 74. 1999-01-11 Vunderink  Begin using the f90 compiler.
! 73. 1998-06-02 Vunderink  Changed REG and HD2 defaults, set header 3 on
!                           sembalence, and added blank line in printout
! 72. 1998-05-21 Vunderink  Added parameters for SVAT regularization and
!                           removed plotting.
! 71. 1998-04-22 Vunderink  Added HD1, HD2, and HD3 parameters
! 70. 1997-09-25 Vunderink  Fixed incorrect headers whenever entire CDP
!                           is dead.
! 69. 1997-07-02 Vunderink  Check for lowercase ".byt" when parsing
!                           filename
! 68. 1997-04-23 Vunderink  Added moveout order information to history
!                           and CPR printout
! 67. 1997-03-06 Vunderink  Make HW(1) for trace byte file sequential.
! 66. 1997-03-04 Vunderink  Use the header from the near "live" trace in
!                           the middle NCDP for the semblance header
! 65. 1997-02-11 Hanson     Added nonhyperbolic moveout correction.
!                           t(offset) = sqrt(t(offset=0)**2 + NHOSIGN *
!                           (offset - velocity)**NHOEXP)
! 64. 1996-11-21 Cooper     Make the file for the groups memory resident
!                           File Name is DP$xx, where xx is ipn(process
!                           number)
! 63. 1996-07-01 Vunderink  Changed ARR(K) = ALOG(MINAMP*ARR(K)) to
!                           ATMP = MINAMP*ARR(K)
!                           ARR(K) = ALOG(ATMP)
!                           for Fortran 1990 compile.
! 62. 1996-02-28 Cooper     Use absolute value of FVINC as the constant
!                           velocity increment if FMAX is less than 0.
! 61. 1996-02-15 Vunderink  Added JOBNAME to Conplot setup call.
! 60. 1995-09-25 Goodger    Increase array ARR from 50000 to 200000.
! 59. 1995-08-01 Vunderink  Documentation change only on DEV parameter.
! 58. 1994-06-10 Troutt     Fix bug when "ptmp-" is lower case (check for
!                           both upper and lower as does BYTE).
! 57. 1994-05-13 Goodger    Documentation change on NLOC parameter.
! 56. 1993-07-16 Peterson   To encompass 3D data, obtain HW8, HW18, HW38
!                           as done in revision 55 for output to the trace
!                           headers in the byte file.
! 55. 1993-04-28 Peterson   Use the SPID, BSMT and GRID from first live
!                           trace in the middle NCDP to reset trace header
!                           words 37, 17 and 7 respectivly for the trace
!                           byte file.
! 54. 1993-02-11 Peterson   Fix to get SPID, BSMT and GRID from first live
!                           trace in the middle NCDP.
! 53. 1992-09-01 Peterson   For BYTE file output to a unix DE! machine use
!                           .BYT"  the " is for unix DE! and no " for VMS.
! 52. 1992-08-20 Troutt     Fix semblance byte file. Header words 17 & 37
!                           didn't come from "middle" group when NCDP>1
!                           and POPT=NO. Header word 7 never came from
!                           "middle" group (add GRID to -VELCPARM-).
! 51. 1992-03-23 Peterson   Clear the ARR array before the next set of
!                           group come in for semblance compositing.
! 50. 1992-02-28 Peterson   Better printout of SEQ, GROUP,BSMT,SP and IOFF
! 49. 1992-02-27 Peterson   Correction to the least amount of mute of all
!                           groups within a semblance composite.
! 48. 1991-12-23 Peterson   Correction to output the proper straight through
!                           traces. Problem caused by improper logic during
!                           the 1991-10-29 output of only the middle cdp code.
! 47. 1991-12-09 Peterson   Remove CALL PLOT (0.0,0.1,2) fix of 1991-10-16.
! 46. 1991-11-11 Peterson   Add DEV=CONP.
! 45. 1991-10-29 Peterson   Output only the middle CDP to the BYTE file.
! 44. 1991-10-16 Peterson   Add CALL PLOT (0.0,0.1,2) to end plot properly.
! 43. 1991-09-11 Peterson   Fix, Continue to semblance composite if we
!                           have not hit the end.
! 42. 1991-06-19 Peterson   Save new POINTER IM6 in -VELCPARM-.Remove IP10
! 41. 1991-06-18 Peterson   Clear ARR with 50000, NVEL is not calculated
!                           until CALL VELCSGEN.
! 40. 1991-05-21 Reed       Changed compositing from CDP to semblance
! 39. 1991-02-12 Peterson   UNICOS changes.
! 38. 1990-11-20 Peterson   Eliminate ENTRY VELCSEGI in SUBROUTINE VELCSEGL
!                           by combining arguments into the VELCSEGL call.
! 37. 1990-10-17 Peterson   Fix problem with setting NC=1 during partially
!                           GATHRed input.
! 36. 1990-09-07 Peterson   COMMON -CRAYPLT- IUNIT,NDUM,IDUMA(5000) was 1000.
! 35. 1990-04-30 Troutt     Fix timing problem in Semblance traces output
!                           to BYTE file - shift semblance values down by
!                           one sample (i.e. by TINC). Add "1" to NWIN
!                           calculation. Constrain TSL! and TIN! such
!                           that each is a multiple of DT and TSLC-2 is
!                           evenly divisible by TINC.
! 34. 1990-04-25 Peterson   Increase SEMBLANCE FILE from A40 TO A60.
! 33. 1990-02-22 Peterson   NETP, add RES=200 and NETP plot label info.
! 32. 1990-02-22 Peterson   Set # of header words to BYTE to be NWIH.
! 31. 1990-02-19 Peterson   Fix problem with HDO and TRO indexing on BYTE.
! 30. 1990-02-15 Peterson   Start internal BYTE call code for traces and
!                           semblance output to vax SVAT and SVAS files.
! 29. 1990-02-06 Peterson   Correct a buffer size problem in VELCMSCL.
! 28. 1990-01-26 Peterson   Dont call VELCMATR. (old IVEL routine for
!                           when no plotting - to manipulate ARR array).
! 27. 1990-01-11 Peterson   CALL PLCARD(S) to collect and transfer
!                           plot label control cards to DPLT.
! 26. 1989-12-01 Peterson   Fix so with OUTP=SEMB, we can get the
!                           ARR array contured,sorted and inter-
!                           polated without POPT=YES.
! 25. 1989-11-10 Peterson   Fix NUMTR reset when gathered input
!                           And OUTP=SEMB.
! 24. 1989-10-27 Peterson   Clarify VSCL parameter and do metric
!                           conversion of FIS and VSCL after ncode.
! 23. 1989-10-12 Peterson   VELC, fix dimension of TRO(NWPO,*).
! 22. 1989-09-26 Peterson   VELCVELD, fix re-enter W-MULT. RL plots.
! 21. 1989-09-08 Peterson   VELC, let traces pass through or output
!                           semblance traces (for ICP processing),
!                           plus re-entrancy.
! 20. 1989-09-07 Peterson   Put VEL,TIMES,OFF and XMUTE in storage.
! 19. 1989-09-06 Peterson   Changed X**2.0 to X**2 to accept neg. X.
! 18. 1989-08-11 Peterson   VELC, increase ARR to 50000 + checks.
! 17. 1989-08-08 Peterson   VELC, change defaults for IS and VSCL.
! 16. 1989-08-07 Peterson   VELC, NETP plots destination and ID.
! 15. 1989-07-26 Peterson   VELC, better error checking, clean-up.
! 14. 1989-07-05 Peterson   Include DEV=ESP and DEV=NETP.
! 13. 1989-06-26 Reed       Fix for TINC .NE. to multiple of DT.
! 12. 1989-06-16 Peterson   Fix for NUMTR on gathered trace input.
! 11. 1989-06-07 Peterson   'RL' plots, prevent -X with too many panels.
! 10. 1989-06-01 Peterson   Recode loops for vectorized efficency.
! 9.  1989-04-26 Peterson   Fix offset gathering by OFFERR = 10.
! 8.  1989-04-18 Peterson   Fix mute scaling, save MSC,NVIBS.
! 7.  1989-03-17 Peterson   Fix velocity annotation in VELCVELD.
! 6.  1989-03-02 Peterson   For RL, need NPANL or NTRACE and NFLD
! 5.  1989-02-24 Peterson   Hardwired buffers to storage-scratch,
!                           and get S.P. from header word 37.
! 4.  1989-02-15 Peterson   Get SP BSMT and ELEV from correct HDWD
! 3.  1989-02-10 Peterson   VELCVELD, plot routine fixes + doc.
! 2.  1989-02-01 Peterson   1st release.
! 1.  1988-12-16 Peterson   Original CPS version.
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
! This process uses two sets of trace and header arrays.
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
! AVO-sensitive semblance calculation
!
! The AVO-sensitive semblance computed in subroutine SVA_COMPUTE_SEMBLANCE_AVO
! is the "AK semblance" described in the paper by Sarkar, Baumel and Larner in
! Sep-Oct 2002 Geophysics. However, its actual calculation has been refined
! somewhat compared with the Appendix in that paper. First, the basis of
! model vectors E1 and E2 is converted to an orthonormal basis. In the
! present subroutine, quantities D_DOT_E1 and D_DOT_E2 are initially computed
! as inner products of the data with the NON-orthogonal model vectors:
!     E1 = (1, 1, ..., 1)           --  n elements
!     E2 = (x1^2, x2^2, ... xn^2)   --  n elements
! where x1, x2, ..., xn are offsets (selected individually for each semblance
! window--see mute scaling discussion below). After this initial calculation,
! D_DOT_E1 and D_DOT_E2 are adjusted so as to represent inner products with
! the orthonormal basis corresponding to E1 and E2.
!
! As a second change (compared with description in Sarkar, Baumel and Larner),
! instead of parameterizing our model as:
!                       M(t) = A(t) * [E1 + K*E2]
! (where A(t) are coefficients for all traveltimes t in a semblance window
! while K is a single parameter for each semblance window), we write this
! using the equivalent parameterization:
!               M(t) = A(t) * [cos(theta)*E1 + sin(theta)*E2]
! where theta is some angle (fixed in each semblance window) replacing the
! parameter K in the original AK semblance formulation. Use of theta instead
! of K provides a more symmetrical treatment of E1 and E2 and, along with
! selection of E1 and E2 as an orthonormal basis, yields simpler equations
! for the resulting generalized semblance.
!
!
! Mute scaling calculations
!
! CMP gathers supplied as input to SVA are generally muted (top mutes and
! possibly bottom mutes). Moreover, NMO correction shifts those mute times and
! introduces an additional top mute by killing values with excessive stretch
! (the NMO correction in SVA includes a hard-wired stretch mute [Doppler mute
! factor] of 1.8). These mute values are used in various ways:
!
! First, when TYPE_SCAN = CALC, the top mute times in the input traces (before
! any NMO correction) are used in calculating the velocities to scan. These
! mute times are used because aliasing due to moveout tends to be worst in the
! shallow part of the traces.
!
! Then, computation of semblance for any trial velocity is done using only the
! data between the adjusted top and bottom mutes after applying NMO for that
! velocity. However, this is done somewhat differently for Standard semblance
! and AVO-sensitive semblance:
!
! For Standard semblance, SVA attempts to use ALL of the available data after
! NMO correction; thus semblance values are computed from non-rectangular
! windows in time-offset space (i.e., different numbers of offsets are
! available for different time values in a semblance window). To account for
! these variations, SVA uses a legacy Conoco mute scaling algorithm (quarter
! scaling) and assumes that mutes were applied using 60 ms cosine tapers (This
! algorithm attempts to account for top mutes but ignores bottom mutes).
!
! For AVO-sensitive semblance, solution of the equations requires a rectangular
! window in time-offset space (after NMO correction). Thus, for each semblance
! window, we use only those offsets for which the entire semblance window
! contains good data (i.e., doesn't overlap the top or bottom mute regions).
! Consequently, the AVO-sensitive calculation is done using slightly less data
! than the Standard semblance calculation. The AVO-sensitive calculation
! doesn't use the legacy Conoco mute scaling and doesn't make any assumption
! about cosine tapers.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS SVA Process/NC=80>
!
!                      Semblance Velocity Analysis Process
!              Semblance stacking velocity analysis of CMP gathers
!
!    ORDER_MO=`C     NUM_SEMB=`IIII     TYPE_SCAN=`CCCC     TYPE_SEMB=`CCCCCCC
!
!    PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!    VEL_BEG =`FFFFFFFFFFF        FREQ_DOM=`FFFFFFFFFFF           HDR_DIAG
!                                                                 `IIIIIIIII
!    VEL_END =`FFFFFFFFFFF        WIN_LEN =`FFFFFFFFFFF           `IIIIIIIII
!                                                                 `IIIIIIIII
!    VEL_INC =`FFFFFFFFFFF        WIN_INC =`FFFFFFFFFFF
!
!                                `-----------------------
!    HDR_OFF =`IIIIIIIIIII        OFF_INIT=`FFFFFFFFFFF
!                                 OFF_INC =`FFFFFFFFFFF
!    REGULAR =`CCC                OFF_LAST=`FFFFFFFFFFF
!                                 OFF_TOT =`IIIIIIIIIII
!                                `-----------------------
!<PARMS HDR_DIAG[/XST/YST]>
!<PARMS PATHNAME[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="ORDER_MO">
!<Tip> Order of moveout expansion series to perform. </Tip>
! Default = 2
! Allowed = 2    (Normal hyperbolic moveout.)
! Allowed = 4    (4th order, non-hyperbolic, residual moveout.)
!
! Ordinary NMO is 2nd order, hyperbolic, NMO.  If the theoretical moveout
! expression is expanded in a series, the first non-zero term is the typical
! 2nd order, hyperbolic, NMO.  The next non-zero term is 4th order,
! non-hyperbolic, moveout.
!
! ORDER_MO allows choice between 2nd order moveout and 4th order residual
! moveout.  If 4th order residual moveout is chosen, SVA assumes that 2nd
! order (regular NMO) correction has already been applied to the input data.
!</Help>
!
!<Help KEYWORD="NUM_SEMB">
!<Tip> Composite NUM_SEMB semblances together. </Tip>
! Default = 1
! Allowed = int > 0
! Semblances are always calculated on individual CMPs as input.  SVA will
! composite NUM_SEMB semblances together after they are calculated
! individually.  If super-CMPs or composited CMPs are desired, they must be
! constructed before using SVA.
!</Help>
!
!<Help KEYWORD="TYPE_SCAN">
!<Tip> Type of velocity scan to use for semblance analysis. </Tip>
! Default = CALC
! Allowed = CALC
! Allowed = CONST
! If TYPE_SCAN=CALC, SVA will calculate an array of velocity scans with an
! increment that increases with increasing velocity.  The first velocity scan
! increment will be no less than VEL_INC.
!
! If TYPE_SCAN=CONST, SVA will use an array of velocity scans with a constant
! increment equal to VEL_INC.
!
! TYPE_SCAN is always CONST for fourth order residual NMO correction
! (ORDER_MO = 4).
!</Help>
!
!<Help KEYWORD="TYPE_SEMB">
!<Tip> Type of semblance to calculate--Standard or AVO Sensitive. </Tip>
! Default = STANDARD
! Allowed = STANDARD   (Standard semblance)
! Allowed = AVO_SENS   (AVO-sensitive semblance)
!
! If TYPE_SEMB = STANDARD, then SVA calculates conventional semblance, i.e.,
! the ratio of the squared stack trace to the sum of squares of individual
! trace values.
!
! If TYPE_SEMB = AVO_SENS, then SVA calculates AVO-sensitive semblance, a
! generalized semblance measure intended to provide more accurate velocities
! for events with strong AVO, especially polarity reversals (see paper of
! Sarkar, Baumel and Larner in Sep-Oct 2002 Geophysics).
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> (Partial) pathname for output files of semblance and CMP traces. </Tip>
!  Default = blank
!  Allowed = char
! This parameter generates the pathnames for both the semblance and CMP trace
! files. Any extension in your entered pathname will be ignored. From whatever
! remains, 'svas.trc8' is appended to form the pathname of the semblance file
! and 'svat.trc8' is appended to form the pathname of the trace file.
!</Help>
!
!<Help KEYWORD="VEL_BEG">
!<Tip> Minimum velocity for semblance analysis. </Tip>
! Default = 1400*
! Allowed = real > 0.0
!
! * The default for VEL_BEG actually depends on setting of SURVEY_UNITS on
! the PROJECT DATA screen.  In particular, the default for VEL_BEG is 1400 if
! SURVEY_UNITS = METERS or 4500 if SURVEY_UNITS = FEET.
!</Help>
!
!<Help KEYWORD="VEL_END">
!<Tip> Maximum velocity for semblance analysis. </Tip>
! Default = 6000*
! Allowed = real > 0.0
!
! * The default for VEL_END actually depends on setting of SURVEY_UNITS on
! the PROJECT DATA screen.  In particular, the default for VEL_END is 6000 if
! SURVEY_UNITS = METERS or 20000 if SURVEY_UNITS = FEET.
!</Help>
!
!<Help KEYWORD="VEL_INC">
!<Tip> First velocity scan increment to use for semblance analysis. </Tip>
! Default = 10.0*
! Allowed = real > 0.0
! If TYPE_SCAN=CALC, SVA will calculate an array of velocity scans with an
! increment that increases with increasing velocity.  The first velocity scan
! increment will be no less than VEL_INC.
!
! If TYPE_SCAN=CONST, SVA will use an array of velocity scans with a constant
! increment equal to VEL_INC.
!
! * The default for VEL_INC actually depends on setting of SURVEY_UNITS on
! the PROJECT DATA screen.  In particular, the default for VEL_INC is 10.0 if
! SURVEY_UNITS = METERS or 30.0 if SURVEY_UNITS = FEET.
!</Help>
!
!<Help KEYWORD="FREQ_DOM">
!<Tip> Dominant frequency parameter for velocity scan calculation. </Tip>
! Default = 35.0
! Allowed = real > 0.0
! SVA requires the dominant frequency in the data in order to calculate an array
! of velocity scans.  Active only when TYPE_SCAN=CALC.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<tip> Window length, in seconds, for semblance calculation. </tip>
!  Default = 0.104
!  Allowed = real>5*DT
! Window length, in seconds, for semblance calculation.  WIN_LEN must be a
! multiple of 2.0*DT.  Shorter windows give more detail (and more fluctuations).
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<tip> Time increment for window locations, for semblance calculation. </tip>
!  Default = 0.5 * WIN_LEN
!  Allowed = real>=DT
!  Time increment for window locations, in seconds, for semblance calculation.
!  WIN_INC must be a multiple of DT.
!</Help>
!
!<Help KEYWORD="HDR_DIAG">
!<tip> Array of header words for diagnostic printout. </tip>
!  Default = 3, 7, 37
!  Allowed = 1 - NWIH (array of 3)
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<tip> Header word designating offset. </tip>
!  Default = 6
!  Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="REGULAR">
!<tip> Whether to regularize CMPs to uniform fold. </tip>
!  Default = YES
!  Allowed = YES/NO
! This option regularizes offsets and fold in output CMPs, after the semblance
! calculation, by placing no more than one trace in each offset bin.  Traces
! are not composited, rather the first trace occupying an offset bin is used.
! (This method avoids artifacts created when compositing traces within bins
! containing a large offset range.)
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<tip> Value of header word HDR_OFF at center of first bin. </tip>
!  Default = 1.0
!  Allowed = real > 0.0
! Active for REGULAR=YES only.
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<tip> Increment between and width of offset bins. </tip>
!  Default = 1.0
!  Allowed = real > 0.0
! Active for REGULAR=YES only.
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<tip> Value of header word HDR_OFF at center of last bin. </tip>
!  Default = 1.0
!  Allowed = real > 0.0
! Active for REGULAR=YES only.
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<tip> Total number of offset bins. </tip>
!  Default = 1
!  Allowed = int > 0.0
! Active for REGULAR=YES only.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module sva_module

    use pc_module
    use named_constants_module
    use getlun_module
    use string_module
    use permtfile_module
    use pattern_module
    use mth_module
    use lav_module
    use mutehw_module
    use pathcheck_module

    implicit none

    private
    public  :: sva_create
    public  :: sva_delete
    public  :: sva_initialize
    public  :: sva_update
    public  :: sva
    public  :: sva_wrapup
    public  :: sva_dump_struct

    character(len=100),public,save :: sva_IDENT = &
'$Id: sva.f90,v 1.98 2006/11/14 14:32:56 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    type,public :: sva_struct
        private
        logical                        :: skip_wrapup ! wrapup flag
        integer                        :: order_mo    ! process parameter
        integer                        :: num_semb    ! process parameter
        character(len=5)               :: type_scan   ! process parameter
        character(len=8)               :: type_semb   ! process parameter
        character(len=FILENAME_LENGTH) :: pathname    ! process parameter
        integer, dimension(3)          :: hdr_diag    ! process parameter
        integer                        :: hdr_off     ! process parameter
        integer                        :: off_tot     ! process parameter
        logical                        :: regular     ! process parameter
        real                           :: vel_beg     ! process parameter
        real                           :: vel_end     ! process parameter
        real                           :: vel_inc     ! process parameter
        real                           :: freq_dom    ! process parameter
        real                           :: win_len     ! process parameter
        real                           :: win_inc     ! process parameter
        real                           :: off_init    ! process parameter
        real                           :: off_inc     ! process parameter
        real                           :: off_last    ! process parameter
        integer                        :: nwih        ! global
        integer                        :: ndpt        ! global
        real                           :: tstrt       ! global
        real                           :: dt          ! global
        integer                        :: lu_tpnt     ! dependent variable
        real, pointer                  :: vel(:)      ! dependent variable
        real, pointer                  :: times(:)    ! dependent variable
        real, pointer                  :: arr(:,:)    ! dependent variable
        real, pointer                  :: trtmp(:,:)  ! dependent variable
        double precision,pointer       :: hdtmp(:,:)  ! dependent variable
        double precision,pointer       :: hdsem(:,:)  ! dependent variable
        integer                        :: nho_exp     ! dependent variable
        integer                        :: nho_sign    ! dependent variable
        integer                        :: nslc        ! dependent variable
        integer                        :: ninc        ! dependent variable
        real                           :: fvmin       ! dependent variable
        real                           :: tmute_min   ! dependent variable
        real                           :: omute_min   ! dependent variable
        real                           :: xnearoff    ! dependent variable
        integer                        :: nwin        ! dependent variable
        integer                        :: nmin        ! dependent variable
        integer                        :: nout        ! dependent variable
        integer                        :: igrp        ! dependent variable
        integer                        :: ndone       ! dependent variable
        integer                        :: ntotal      ! dependent variable
        real                           :: trout       ! dependent variable
        real                           :: troutb      ! dependent variable
        type(permtfile_struct),pointer :: ptrbytt     ! dependent variable
        type(permtfile_struct),pointer :: ptrbyts     ! dependent variable
        integer                        :: ncdp        ! dependent variable
        integer                        :: ilive       ! dependent variable
        integer                        :: nvel        ! dependent variable
    end type sva_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

    type(sva_struct),pointer,save :: object            ! needed for traps
    logical,parameter             :: prin = .false.    ! for detailed printouts
    integer,save                  :: lu_print =   6
    integer,parameter             :: MAX_LINE = 160
    real,parameter                :: doppler  = 1.8
    real,parameter                :: doppler2 = doppler * doppler

contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

subroutine sva_create( obj )
    implicit none
    type(sva_struct),pointer :: obj       ! arguments

    allocate( obj )

!---nullify pointers
    nullify( obj%vel   )
    nullify( obj%times )
    nullify( obj%arr   )
    nullify( obj%hdsem )
    nullify( obj%hdtmp )
    nullify( obj%trtmp )
    nullify (obj%ptrbytt) ! jpa
    nullify (obj%ptrbyts) ! jpa

    call sva_initialize( obj )

end subroutine sva_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

subroutine sva_delete( obj )
    implicit none
    type(sva_struct),pointer :: obj       ! arguments

    call sva_wrapup( obj )

    if( associated( obj%hdsem   ) ) deallocate          ( obj%hdsem   )
    if( associated( obj%hdtmp   ) ) deallocate          ( obj%hdtmp   )
    if( associated( obj%trtmp   ) ) deallocate          ( obj%trtmp   )
    if( associated( obj%times   ) ) deallocate          ( obj%times   )
    if( associated( obj%vel     ) ) deallocate          ( obj%vel     )
    if( associated( obj%arr     ) ) deallocate          ( obj%arr     )
    if( associated( obj%ptrbytt ) ) call permtfile_close( obj%ptrbytt )
    if( associated( obj%ptrbyts ) ) call permtfile_close( obj%ptrbyts )

    deallocate(obj)

end subroutine sva_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

subroutine sva_initialize( obj )
    implicit none
    type(sva_struct),intent(inout) :: obj

    character(len=7) :: units

    if( pc_pdata_keyword_present( 'SURVEY_UNITS' ) ) then
        call pc_get_pdata( 'SURVEY_UNITS', units )
        call string_to_upper( units )
    else
        units = 'UNKNOWN'
    endif

    if( units == 'METERS' ) then
        obj%vel_beg =  1400.0
        obj%vel_end =  6000.0
        obj%vel_inc =    10.0
    else
        obj%vel_beg =  4500.0
        obj%vel_end = 20000.0
        obj%vel_inc =    30.0
    endif

    obj%order_mo    =    2
    obj%num_semb    =    1
    obj%type_scan   = "CALC"
    obj%type_semb   = "STANDARD"
    obj%freq_dom    =   35
    obj%win_len     =    0.104
    obj%win_inc     =    0.052
    obj%hdr_diag(1) =    3
    obj%hdr_diag(2) =    7
    obj%hdr_diag(3) =   37
    obj%pathname    = PATHCHECK_EMPTY
    obj%regular     = .true.
    obj%hdr_off     =    6
    obj%off_init    =    1.0
    obj%off_inc     =    1.0
    obj%off_last    =    1.0
    obj%off_tot     =    1
!
! other dependent variables
!
    obj%lu_tpnt     = 0
    obj%nho_exp     = 2
    obj%nho_sign    = 1
    obj%omute_min   = 0.0
    obj%xnearoff    = huge( obj%xnearoff )
    obj%trout       = 0.0
    obj%troutb      = 0.0
    obj%fvmin       = 0.0  ! calculated in update
    obj%tmute_min   = 0.0  ! calculated in update
    obj%nwin        = 0    ! calculated in update
    obj%nmin        = 0    ! calculated in update
    obj%nout        = 0    ! calculated in update
    obj%nslc        = 0    ! calculated in update
    obj%ninc        = 0    ! calculated in update
    obj%ncdp        = 0
    obj%ilive       = 0
    obj%nvel        = 0

! The following are only used in sva_gathr:

    obj%igrp        = 0
    obj%ndone       = 0
    obj%ntotal      = 0

!   clear globals then check later to insure that they are updated

    obj%ndpt        =  0
    obj%nwih        =  0
    obj%dt          =  0.0
    obj%tstrt       = -huge( obj%tstrt )

    call sva_update( obj )

end subroutine sva_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

subroutine sva_update( obj )
    implicit none
    type(sva_struct),intent(inout),target :: obj             ! arguments

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------


    character(len=FILENAME_LENGTH) :: filenc
    integer                        :: i,ipn,err
    integer, dimension(4)          :: ierr
    integer                        :: ierr0
    integer                        :: ii1
    integer                        :: ii2
    integer                        :: numtr
    logical                        :: gathered
    logical                        :: verify
    integer                        :: state
    integer                        :: status


    integer                        :: nhd = 3
    integer                        :: nmod
    integer                        :: nslc2
    integer                        :: max_rec
    real                           :: winci
    real                           :: wleni
!
!-------------------------------------------------------------------------------
!
    object => obj               ! needed for traps
    obj%skip_wrapup = .true.    ! needed for the wrapup routine
    
    call pc_put_minsize_array( 'HDR_DIAG', 3 )
    call pc_put_maxsize_array( 'HDR_DIAG', 3 )

    state = pc_get_update_state()

    if(state == PC_FRONTEND .or. state == PC_BACKEND) then
      verify = .true.
    else
      verify = .false.
    end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

    ipn = pc_get_ipn()

    call pc_get( 'ORDER_MO' , obj%order_mo  )
    call pc_get( 'NUM_SEMB' , obj%num_semb  )
    call pc_get( 'TYPE_SCAN', obj%type_scan )
    call pc_get( 'TYPE_SEMB', obj%type_semb )
    call pc_get( 'PATHNAME' , obj%pathname  )
    call pc_get( 'VEL_BEG'  , obj%vel_beg   )
    call pc_get( 'VEL_END'  , obj%vel_end   )
    call pc_get( 'VEL_INC'  , obj%vel_inc   )
    call pc_get( 'FREQ_DOM' , obj%freq_dom  )
    call pc_get( 'WIN_LEN'  , obj%win_len, sva_win_len_trap )
    call pc_get( 'WIN_INC'  , obj%win_inc   )
    call pc_get( 'REGULAR'  , obj%regular   )
    call pc_get( 'HDR_OFF'  , obj%hdr_off   )
    call pc_get( 'HDR_DIAG' , obj%hdr_diag, nhd )
    call pc_get( 'OFF_INIT' , obj%off_init)
    call pc_get( 'OFF_INC'  , obj%off_inc )
    call pc_get( 'OFF_LAST' , obj%off_last)
    call pc_get( 'OFF_TOT'  , obj%off_tot )

    call pc_get_global( 'NUMTR'   , numtr  )
    call pc_get_global( 'GATHERED', gathered  )
    call pc_get_global( 'NWIH'    , obj%nwih  )
    call pc_get_global( 'NDPT'    , obj%ndpt  )
    call pc_get_global( 'TSTRT'   , obj%tstrt )
    call pc_get_global( 'DT'      , obj%dt    )

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    call string_to_upper( obj%type_scan )
    call string_to_upper( obj%type_semb )

    call pathcheck( 'PATHNAME', obj%pathname, REQUIRED=.true. )

    if( .not.gathered .or. numtr<2 ) then
        call pc_error( 'Input traces must be gathered by CMP' )
    endif

    if( obj%order_mo /= 2 .and. obj%order_mo /= 4 ) then
        call pc_error( msg1='Invalid value for ORDER_MO', var1=obj%order_mo )
        obj%order_mo = 2
    endif

    if( obj%num_semb <= 0 ) then
        call pc_error( msg1='Invalid value for NUM_SEMB', var1=obj%num_semb )
        obj%num_semb = 1
    endif

    if( obj%vel_beg <= 0.0 ) then
        call pc_error( msg1='Invalid value for VEL_BEG', var1=obj%vel_beg )
        obj%vel_beg = 1400.0
    endif

    if( obj%vel_end <= 0.0 ) then
        call pc_error( msg1='Invalid value for VEL_END', var1=obj%vel_end )
        obj%vel_end = 6000.0
    endif

    if( obj%vel_inc <= 0.0 ) then
        call pc_error( msg1='Invalid value for VEL_INC', var1=obj%vel_inc )
        obj%vel_inc = 30.0
    endif

    if     ( obj%type_scan(1:2) == 'CA' ) then
        obj%type_scan = 'CALC'
    else if( obj%type_scan(1:2) == 'CO' ) then
        obj%type_scan = 'CONST'
    endif

    if( (obj%type_scan /= 'CALC') .and. (obj%type_scan /= 'CONST') ) then
        call pc_error( msg1='Invalid value for TYPE_SCAN = ' // obj%type_scan )
        obj%type_scan = 'CALC'
    endif

    if(obj%type_scan == 'CALC' .and. obj%order_mo /= 2) then
        call pc_info( 'TYPE_SCAN changed to CONST')
        obj%type_scan = 'CONST'
    endif

    if (obj%type_semb(1:1) == 'A') then
        obj%type_semb = 'AVO_SENS'
    else
        obj%type_semb = 'STANDARD'
    end if

    if( (obj%freq_dom <= 0.0) .and. (obj%type_scan == 'CALC') ) then
        call pc_error( msg1='Invalid value for FREQ_DOM', var1=obj%freq_dom )
        obj%freq_dom = 35.0
    endif

    if( obj%win_len <= 5.0*obj%dt ) then
        call pc_error( msg1='Invalid value for WIN_LEN', var1=obj%win_len )
        obj%win_len = 0.104
    endif

    if( obj%win_inc < obj%dt ) then
        call pc_error( msg1='Invalid value for WIN_INC', var1=obj%win_inc )
        obj%win_inc = 0.5 * obj%win_len
    endif

    if( (obj%hdr_off < 1) .or. (obj%hdr_off > obj%nwih) ) then
        call pc_error( msg1='Invalid value for HDR_OFF', var1=obj%hdr_off )
        obj%hdr_off = 6
    endif

    if( nhd /= 3 ) then
        call pc_error( msg1='Invalid number of elements for HDR_DIAG', var1=nhd)
    endif

    do i = 1, nhd
        if( (obj%hdr_diag(i) < 1) .or. (obj%hdr_diag(i) > obj%nwih) ) then
            select case( i )
            case( 1 )
                call pc_error( msg1='Invalid value for HDR_DIAG(1)', &
                               var1=obj%hdr_diag(1) )
                obj%hdr_diag(1) = 3
            case( 2 )
                call pc_error( msg1='Invalid value for HDR_DIAG(2)', &
                               var1=obj%hdr_diag(2) )
                obj%hdr_diag(2) = 7
            case( 3 )
                call pc_error( msg1='Invalid value for HDR_DIAG(3)', &
                               var1=obj%hdr_diag(3) )
                obj%hdr_diag(3) = 37
            end select
        endif
    end do

    status = pattern_stop2('SVA:', verify, &
      obj%off_init, obj%off_inc, obj%off_last, obj%off_tot, &
      'OFF_INIT', 'OFF_INC', 'OFF_LAST', 'OFF_TOT', &
      pc_verify_scalar('off_init'), pc_verify_scalar('off_inc'), &
      pc_verify_scalar('off_last'), pc_verify_scalar('off_tot'))

    if (verify .and. obj%regular) then
        if (obj%off_tot <= 1 .or. obj%off_inc == 0.0) then
            call pc_error ('OFF_TOT > 1 and OFF_INC != 0.0 required &
                               &when REGULAR = YES')
        end if
    end if

!---Make sure WIN_LEN and WIN_INC are multiples of DT and that NSLC is
!---evenly divisible by 2*NINC.

    wleni    = obj%win_len
    winci    = obj%win_inc
    obj%nslc = max (nint(obj%win_len/obj%dt), 6)
    nslc2    = (obj%nslc + 1) / 2
    obj%nslc = 2 * nslc2
    obj%ninc = max (nint(obj%win_inc/obj%dt), 1)

    if( mod( nslc2, obj%ninc ) /= 0 ) then
        nmod     = max (nint(real(nslc2)/real(obj%ninc)), 1)
        obj%ninc = nslc2 / nmod
        do
          nslc2    = nmod * obj%ninc
          obj%nslc = 2 * nslc2
          if (obj%nslc > 5) exit
          obj%ninc = obj%ninc + 1
        end do
    endif

    obj%win_len = obj%nslc * obj%dt
    obj%win_inc = obj%ninc * obj%dt
    wleni = abs( wleni - obj%win_len )
    winci = abs( winci - obj%win_inc )

    if( (wleni > 0.1*obj%dt) .or. (winci > 0.1*obj%dt) ) then
        call pc_warning(' WIN_LEN and/or WIN_INC adjusted to be')
        call pc_warning(' multiples of DT and to make WIN_LEN ' // &
                            'divisible by 2*WIN_INC.')
        call pc_warning('     WIN_LEN = ', obj%win_len, &
                        '     WIN_INC = ', obj%win_inc)
    endif

    obj%nwin = (obj%ndpt - obj%nslc)/obj%ninc + 1
    if( obj%nwin > 4000 ) then
        call pc_error( '**** ERROR: number of times > 4000 ****' )
        return
    endif

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


    call pc_put_options_field( 'ORDER_MO' , (/  2     ,  4      /), 2 )
    call pc_put_options_field( 'TYPE_SCAN', (/ 'CALC ', 'CONST' /), 2 )
    call pc_put_options_field( 'TYPE_SEMB', (/ 'STANDARD', 'AVO_SENS' /), 2 )
    call pc_put_options_field( 'REGULAR'  , (/ 'YES'  , 'NO '   /), 2 )

    call pc_put( 'ORDER_MO' , obj%order_mo      )
    call pc_put( 'NUM_SEMB' , obj%num_semb      )
    call pc_put( 'TYPE_SCAN', obj%type_scan     )
    call pc_put( 'TYPE_SEMB', obj%type_semb     )
    call pc_put( 'PATHNAME' , obj%pathname      )
    call pc_put( 'VEL_BEG'  , obj%vel_beg       )
    call pc_put( 'VEL_END'  , obj%vel_end       )
    call pc_put( 'VEL_INC'  , obj%vel_inc       )
    call pc_put( 'FREQ_DOM' , obj%freq_dom      )
    call pc_put( 'WIN_LEN'  , obj%win_len       )
    call pc_put( 'WIN_INC'  , obj%win_inc       )
    call pc_put( 'REGULAR'  , obj%regular       )
    call pc_put( 'HDR_OFF'  , obj%hdr_off       )
    call pc_put( 'HDR_DIAG' , obj%hdr_diag, nhd )
    call pc_put( 'OFF_INIT' , obj%off_init      )
    call pc_put( 'OFF_INC'  , obj%off_inc       )
    call pc_put( 'OFF_LAST' , obj%off_last      )
    call pc_put( 'OFF_TOT'  , obj%off_tot       )

    call pc_put_sensitive_field_flag( 'OFF_INIT' , obj%regular )
    call pc_put_sensitive_field_flag( 'OFF_LAST' , obj%regular )
    call pc_put_sensitive_field_flag( 'OFF_INC'  , obj%regular )
    call pc_put_sensitive_field_flag( 'OFF_TOT'  , obj%regular )
    call pc_put_sensitive_field_flag( 'TYPE_SCAN', (obj%order_mo == 2) )
    call pc_put_sensitive_field_flag( 'FREQ_DOM' , (obj%type_scan == 'CALC') )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

!---get logical unit numbers for printing and open temporary print file

    lu_print = pc_get_lun()
    call getlun( obj%lu_tpnt, ierr0 )

    if( ierr0 == 0 ) then
        open( unit=obj%lu_tpnt, action='READWRITE', iostat=ierr0, &
              status='SCRATCH' )
        if( ierr0 /= 0 ) then
            write( pc_get_lun(), '( / " **** ERROR: temp print file open  ", &
                          &"failed, iostat = ", I4, "****" / )' ) ierr0
            return
        endif
    else
        write( pc_get_lun(), '( / " **** ERROR: getlun failed, istat = ", i4, &
                      &"****" / )' ) ierr0
        return
    endif

!---miscellaneous initializations

    obj%tmute_min = obj%ndpt
    obj%fvmin     = obj%vel_inc

!---set exponent and sign for move out

    if( obj%order_mo == 2 ) then
        obj%nho_sign =  1
        obj%nho_exp  =  2
    else
        obj%nho_sign = -1
        obj%nho_exp  =  4
    endif

!---WRITE INFO

    if( obj%order_mo == 2 ) then
        write( obj%lu_tpnt, * ) 'SVA: ORDER = 2, COMMENT=REGULAR NMO'
    else
        write( obj%lu_tpnt, * ) 'SVA: ORDER = 4, COMMENT=NON-HYPERBOLIC NMO'
    endif

    write( obj%lu_tpnt, '( " SVA: VELOCITY CONTOUR" )' )
    write( obj%lu_tpnt, '( " SVA: MINIMUM VELOCITY = ", f8.0 )' ) obj%vel_beg
    write( obj%lu_tpnt, '( " SVA: MAXIMUM VELOCITY = ", f8.0 )' ) obj%vel_end

    if( obj%type_scan == 'CALC' ) then
        write( obj%lu_tpnt, '(" SVA: FIRST SCAN INCREMENT FOR VELOCITY = ",    &
                            & f8.1)' ) obj%vel_inc
    else
        write( obj%lu_tpnt, '(" SVA: CONSTANT SCAN INCREMENT FOR VELOCITY = ", &
                            & f8.1)' ) obj%vel_inc
    endif

!---allocate storage

    allocate( obj%times(obj%nwin)            , stat = ierr(1) )
    allocate( obj%hdtmp(obj%nwih,obj%off_tot), stat = ierr(2) )
    allocate( obj%trtmp(obj%ndpt,obj%off_tot), stat = ierr(3) )
    allocate( obj%hdsem(obj%nwih,1)          , stat = ierr(4) )

    do i = 1, 4
        if( ierr(i) /= 0 ) then
            call pc_error( 'sva_update: memory allocation error' )
            return
        endif
    end do

    obj%times(:obj%nwin) = ( obj%ninc * (/ ( i, i = 0, obj%nwin-1 ) /) + &
                             obj%nslc / 2 ) * obj%dt + obj%tstrt

!---open trace file to output CMP gathers.

    filenc = obj%pathname

    ii1 = min (len_trim(filenc) + 1, FILENAME_LENGTH - 8)
    ii2 = index (filenc, '.')
    if (ii2 > 0) ii1 = min (ii1, ii2)

    filenc(ii1:) = 'svat.trc8'

    write( obj%lu_tpnt, '( " SVA: TRACE     BYTE FILE = ",a160 )' ) filenc

    ! set max_rec to ensure 2 gigabyte file extent size.
    max_rec = (1.1 * 2*(1024**3)) / (8*obj%nwih + obj%ndpt)

    call permtfile_open_write (obj%ptrbytt, filenc, obj%nwih, obj%ndpt, &
                               obj%tstrt, obj%dt, lu_print, err, ipn, 8, &
                               max_rec)
    if (err /= PERMTFILE_OK) call pc_error ('error opening trace CMP file')

!---open trace file to output semblance trace panels.

    obj%nmin = (obj%times(1) - obj%tstrt) / obj%win_inc + 0.1
    obj%nout = obj%nmin + obj%nwin

    filenc(ii1:) = 'svas.trc8'

    write( obj%lu_tpnt, '( " SVA: SEMBLANCE BYTE FILE = ",a160 )' ) filenc

    ! set max_rec to ensure 2 gigabyte file extent size.
    max_rec = (1.1 * 2*(1024**3)) / (8*obj%nwih + obj%nout)

    call permtfile_open_write (obj%ptrbyts, filenc, obj%nwih, obj%nout, &
                               obj%tstrt, obj%win_inc, lu_print, err, ipn, 8, &
                               max_rec)
    if (err /= PERMTFILE_OK) call pc_error ('error opening semblance file')

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

end subroutine sva_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

subroutine sva_win_len_trap( keyword )              ! scalar trap
    implicit none
    character(len=*),intent(in) :: keyword          ! arguments

    object%win_inc = 0.5 * object%win_len
    call pc_jump_field( 'WIN_INC' )

end subroutine sva_win_len_trap

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

subroutine sva( obj, ntr, hd, tr )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(sva_struct), intent(inout) :: obj
    integer,          intent(inout) :: ntr
    double precision, intent(inout) :: hd(:,:)
    real,             intent(inout) :: tr(:,:)
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!   The following were process parameters in the old system
!
    integer           :: npsm = 5       ! # pts to smooth to normalize
    integer,parameter :: msc  = 4       ! mute scaling parameter
    integer,parameter :: ihdg = 3       ! header word for CMPs
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real                      :: a0
    real                      :: a1
    real                      :: amut

    real                      :: d
    character(len=132)        :: fmt

    integer                   :: icntr
    integer                   :: ico
    real                      :: ielev
    integer                   :: ierr
    integer                   :: ioff
    integer                   :: ivel
    integer                   :: iwin
    integer                   :: jtr
    logical                   :: midcdp


    integer                   :: ncl
    integer                   :: nelev
    integer                   :: nn,ii,err
    real                      :: offsets(ntr)
    real                      :: omax
    real                      :: offvel
    real                      :: tstrt1
    real                      :: t0
    real                      :: tmax
    real                      :: tmute(ntr)
    real                      :: bmute(ntr)
    real                      :: trsem(obj%nout,1)
    real                      :: vmut
    real                      :: x2sum
    real                      :: xsum
    real                      :: xysum
    real                      :: ysum
!
!-------------------------------------------------------------------------------
!
    if (ntr == NO_MORE_TRACES) then
      call sva_wrapup (obj)
      return
    end if
!
!-------------------------------------------------------------------------------
!
    obj%ncdp = obj%ncdp + 1                       ! CDP counter
    icntr    = mod (obj%ncdp, obj%num_semb)

    if (icntr == 1 .or. obj%num_semb <= 2) then
!
!-------FIRST CMP OF NEW COMPOSITE
!
        obj%hdsem(1:,1)  = hd(1:obj%nwih,1)
        obj%hdsem(12,1)  = 1
        obj%ilive        = 0
        obj%xnearoff     = huge (obj%xnearoff)
    endif
!
!-------------------------------------------------------------------------------
!
    ielev = 0
    nelev = 0

    if( icntr == (obj%num_semb/2 + 1) .or. obj%num_semb <= 2 ) then
        midcdp = .true.
    else
        midcdp = .false.
    endif

    do jtr = 1, ntr

        if( midcdp ) then
            if( hd(HDR_LAV,jtr) /= 0.0 ) then
                if( obj%xnearoff > abs( hd(obj%hdr_off,jtr) ) ) then
                    obj%xnearoff     = abs( hd(obj%hdr_off,jtr) )
                    obj%ilive       = 1
                    obj%hdsem(1:,1) = hd(1:obj%nwih,jtr)
                    obj%hdsem(12,1) = obj%ncdp
                endif
            else if( obj%ilive == 0 ) then
                obj%hdsem(1:,1) = hd(1:obj%nwih,jtr)
                obj%hdsem(12,1) = obj%ncdp
            endif
        endif
!
!-------ADD NEW OFFSET & NEW TRACE
!
        offsets(jtr) = abs (hd(obj%hdr_off,jtr))

        call mutehw (hd(:,jtr), tr(:,jtr), obj%ndpt, 0.0, MUTEHW_SET)
        tmute(jtr) = hd(HDR_TOP_MUTE,jtr)
        bmute(jtr) = hd(HDR_BOTTOM_MUTE,jtr)
        if (hd(HDR_LAV,jtr) <= 0.0) tmute(jtr) = obj%ndpt + 1

        if (hd(HDR_MIDPOINT_ELEV,jtr) /= 0.0) then
           ielev = ielev + hd(HDR_MIDPOINT_ELEV,jtr)
           nelev = nelev + 1
        end if

    end do
!
!-------------------------------------------------------------------------------
!
!---ALL DATA FOR THIS cdp GROUP HAS BEEN INPUT
!---START stacking HERE
!
    if( obj%ncdp == 1 ) then
!
!-------FIND MAXIMUM NON-ZERO OFFSET AND CORRESPONDING MUTE TIME
!
        tmax  = (obj%ndpt - obj%nslc + 1) * obj%dt
        omax  = 0.0
!
!-------Fit least-squares line to offsets & times
!
        xsum  = 0.0
        ysum  = 0.0
        x2sum = 0.0
        xysum = 0.0
        ico   = 0
        do ioff = 1, ntr
            omax = max (offsets(ioff), omax)
            if (tmute(ioff) * obj%dt > tmax) cycle
            ico   = ico + 1
            xsum  = xsum  + offsets(ioff)
            ysum  = ysum  + tmute(ioff)
            x2sum = x2sum + offsets(ioff) ** 2
            xysum = xysum + offsets(ioff) * tmute(ioff)
        end do

        d = ico * x2sum - xsum ** 2
        if (ico < 2 .or. d == 0.0) then
            write( obj%lu_tpnt, * ) ' SVA: LEAST-SQUARES FIT PROBLEM'
            write( obj%lu_tpnt, * ) ' MUTES WILL NOT BE USED, ALL DATA ANALYZED'
        else
            a0 = ( ysum * x2sum - xsum * xysum ) / d
            a1 = ( ico  * xysum - xsum * ysum  ) / d
            amut = a0 * obj%dt
            if( a1 == 0.0 ) then
                write(obj%lu_tpnt,*)' SVA: UN-MUTED DATA IN LEAST-SQUARES FIT'
                write(obj%lu_tpnt,*)' MUTES WILL NOT BE USED, ALL DATA ANALYZED'
            else
                vmut = 1.0 / (a1 * obj%dt)
                tmax = amut + omax / vmut
                fmt  = '( " SVA: LEAST-SQUARES AMUT,VMUT = ", 2(1x,f10.2) )'
                write( obj%lu_tpnt, fmt ) amut+obj%tstrt, vmut
            endif
        endif

        tmax = tmax + obj%tstrt

        call sva_vgen( obj%type_scan, obj%vel_beg, obj%vel_end, obj%vel_inc,   &
                       omax, tmax, obj%vel, obj%nvel, obj%freq_dom, obj%fvmin, &
                       obj%lu_tpnt, ierr )
        if( ierr /= 0 ) then
            write(obj%lu_tpnt,'(/"**** ERROR: memory allocation error in &
                                 &sva_vgen ****"/)')
            ntr = FATAL_ERROR
            go to 800         ! exit subroutine
        endif

        allocate( obj%arr(obj%nwin,obj%nvel), stat=ierr )
        if( ierr /= 0 )  then
            write(obj%lu_tpnt,'( / "**** ERROR: memory allocation error in &
                                   &sva ****" / )')
            ntr = FATAL_ERROR
            go to 800         ! exit subroutine
        endif

    endif
!
!-------------------------------------------------------------------------------
!
    fmt = '( " GROUP#=", i8, "  HW", i2.2, "=", f12.3, "  HW", i2.2, "=", ' &
          // 'f12.3, "  HW", i2.2, "=", f12.3, "  #OFFSETS=", i8 )'
    write( obj%lu_tpnt, fmt ) obj%ncdp,                                  &
                              obj%hdr_diag(1), hd(obj%hdr_diag(1),1),    &
                              obj%hdr_diag(2), hd(obj%hdr_diag(2),1),    &
                              obj%hdr_diag(3), hd(obj%hdr_diag(3),1), ntr
!
!---GENERATE CONSTANT VELOCITY ANALYSIS TRACES
!
!---get the mute time and offset

    call sva_compute_mute( obj%ndpt, ntr, tmute, offsets, &
                           obj%tmute_min, obj%omute_min )

!---compute the semblance for nvel velocities

    if( icntr == 1 .or. obj%num_semb <= 2 ) obj%arr = 0

    call sva_compute_semblance_n( msc, obj%nwin, obj%nslc, obj%ninc, obj%nvel, &
                tr, obj%nho_exp, obj%nho_sign, obj%vel, ntr, offsets, tmute,   &
                bmute, obj%nwih, obj%ndpt, obj%tstrt, obj%dt, obj%type_semb,   &
                obj%arr )

!---Continue to semblance composite if we have not hit the end.

    if( mod(obj%ncdp,obj%num_semb) /= 0 ) go to 190      ! Need more CDPs
!
!-------------------------------------------------------------------------------
!
!---WE NOW HAVE ALL OF THE CDPs FOR THIS COMPOSITE
!---NORMALIZE SEMBLANCE MATRIX and plot
!
    offvel = obj%nho_sign * (obj%omute_min/obj%vel(1))**obj%nho_exp / obj%dt**2
    tstrt1 = obj%tstrt/obj%dt - 1.0
    t0 = sqrt(max(0.0,max(0.0,obj%tmute_min + tstrt1)**2 - offvel)) - tstrt1
    t0 = max (t0, 1.0)

!---keep the mute time above the first mute time
!---this is needed for negative moveouts, DWH 02-10-97

    t0  = min (t0, obj%tmute_min)
    ncl = nint(t0 - 1.0) / obj%ninc

    call sva_norm( obj%arr, obj%nvel, obj%nwin, npsm, obj%times, ncl, &
                   obj%lu_tpnt )
!
!-------------------------------------------------------------------------------
!
!---PRINT-IF REQUESTED
!
    if( prin ) then
        do iwin = 1, obj%nwin
            write( obj%lu_tpnt, '( " SVA: TIMES = ", f12.3 )' ) obj%times(iwin)
            do ivel = 1, obj%nvel
                write( obj%lu_tpnt, '( 1x, i10, 1x, i10, 1x, f20.5 )' ) &
                       ivel, int( obj%vel(ivel) ), obj%arr(iwin,ivel)
            end do
        end do
    endif
    write( obj%lu_tpnt, '(A)' ) '  '
!
!-------------------------------------------------------------------------------
!
!---OUTPUT GROUPS OF CONSTANT VELOCITY TRACES
!
    do ivel = 1, obj%nvel
        trsem(1:obj%nmin,1)  = 0.0
        trsem(obj%nmin+1:,1) = obj%arr(:,ivel)

!-------SET UP HEADER TO OUTPUT SG-TYPE TRACE
        obj%hdsem( 1,1) = ivel
        obj%hdsem( 2,1) = 1.0
        obj%hdsem( 3,1) = hd(ihdg,1)
        obj%hdsem( 6,1) = obj%vel(ivel)
        obj%hdsem(13,1) = ivel
        obj%hdsem(15,1) = 0.0
        if( nelev > 0 ) then
            obj%hdsem(HDR_MIDPOINT_ELEV,1) = ielev / nelev
        else
            obj%hdsem(HDR_MIDPOINT_ELEV,1) = 0.0
        endif
        obj%hdsem(25,1) = lav( trsem(:,1), obj%nout )
        obj%hdsem(64,1) = obj%nout

        call permtfile_write (obj%ptrbyts, obj%hdsem(:,1), trsem(:,1), err)
        if (err /= PERMTFILE_OK) then
             call pc_error ('error writing to semblance file')
             ntr = FATAL_ERROR
             call sva_wrapup( obj )
             return
        end if
    end do
!
!-------------------------------------------------------------------------------
!
!---FINISHED - Transfer data to next process
!
190 continue
    do jtr = 1, ntr
        obj%trout  = obj%trout + 1.0
        hd( 1,jtr) = obj%trout

        if( midcdp ) then
            obj%troutb = obj%troutb + 1.0
            if( obj%regular ) then
                nn = 1
                call sva_gathr( nn, hd(1:,jtr:), tr(1:,jtr:), obj%hdtmp,      &
                                obj%trtmp, obj%nwih, obj%ndpt, ihdg,          &
                                obj%hdr_off, obj%off_init, obj%off_inc,       &
                                obj%off_tot, obj%igrp, obj%ndone, obj%ntotal, &
                                ierr )

                if( jtr == ntr ) then
                    nn = 0
                    call sva_gathr( nn, hd(1:,jtr:), tr(1:,jtr:), obj%hdtmp,   &
                                    obj%trtmp, obj%nwih, obj%ndpt, ihdg,       &
                                    obj%hdr_off, obj%off_init, obj%off_inc,    &
                                    obj%off_tot, obj%igrp, obj%ndone,          &
                                    obj%ntotal, ierr )
                    call lav_set_hdr( obj%hdtmp, obj%trtmp, obj%ndpt, nn )
                    do ii = 1,nn
                        call permtfile_write &
                           (obj%ptrbytt, obj%hdtmp(:,ii), obj%trtmp(:,ii), err)
                        if (err /= PERMTFILE_OK) then
                             call pc_error ('error writing to CMP trace file')
                             ntr = FATAL_ERROR
                             call sva_wrapup( obj )
                             return
                        end if
                    end do
                endif
            else
                call permtfile_write (obj%ptrbytt, hd(:,jtr), tr(:,jtr), err)
                if (err /= PERMTFILE_OK) then
                     call pc_error ('error writing to CMP trace file')
                     ntr = FATAL_ERROR
                     call sva_wrapup( obj )
                     return
                end if
            endif
        endif
    end do
!
!-------------------------------------------------------------------------------
!
!---EXIT SUBROUTINE
!
800 continue
end subroutine sva

!!--------------------- ESTIMATE VELOCITY INCREMENTS -------------------------!!
!!--------------------- ESTIMATE VELOCITY INCREMENTS -------------------------!!
!!--------------------- ESTIMATE VELOCITY INCREMENTS -------------------------!!

subroutine sva_vgen( type_scan, vel_beg, vel_end, vel_inc, off, tzero, &
                     vel, nvel, freq_dom, fvmin, lu_tpnt, ierr )
!
!     THIS SUBROUTINE ESTIMATES THE FIRST VELOCITY INCREMENT
!     NEEDED TO AVOID ALIASING, AND BUILDS CONSTANT VELOCITY
!     SCAN VALUES BASED ON VEL**IPWR*(FIRST INCREMENT).
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    character(len=5), intent(in)    :: type_scan
    real,             intent(in)    :: vel_beg
    real,             intent(in)    :: vel_end
    real,             intent(inout) :: vel_inc
    real,             intent(in)    :: off
    real,             intent(in)    :: tzero
    real, pointer                   :: vel(:)
    integer,          intent(out)   :: nvel
    real,             intent(in)    :: freq_dom
    real,             intent(in)    :: fvmin
    integer,          intent(in)    :: lu_tpnt
    integer,          intent(out)   :: ierr
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real    :: alpha
    integer :: ipwr
    integer :: j

    real    :: v
!-----------------------------------------------
!
901 format( ' SVA: FAR OFFSET = ', f12.3, '   FAR MUTE TIME = ', f12.3 )
902 format( ' SVA: CALCULATED FIRST INCREMENT=', 4x, f6.2 )
903 format( ' SVA: VELOCITY INCREMENT TOO SMALL, RESET TO ', 4x, f6.2 )
904 format( ' SVA: FIRST INCREMENT=', 1p, e13.3, 10x, '# OF SCANS=', i8 )
905 format( // '         SCAN #       VELOCITY' / )
906 format( 5x, i10, 5x, f10.0 )
!
!-------------------------------------------------------------------------------
!
    ierr = 0
    write( lu_tpnt, 901 ) off, tzero
!
    if( type_scan == "CALC" ) then
        ipwr = 3
        vel_inc = 0.560 / freq_dom / off ** 2 * tzero * vel_beg ** 3
        write( lu_tpnt, 902 ) vel_inc

!       RESET TO FVMIN IF TOO SMALL
        if( vel_inc <= fvmin ) then
            vel_inc = fvmin
            write( lu_tpnt, 903 ) vel_inc
        endif
    else
        ipwr = 0
    endif

    alpha = vel_inc / vel_beg ** ipwr
    nvel  = 1
    v     = vel_beg
    do while( v < vel_end )
        nvel = nvel + 1
        v    = v + alpha * v**ipwr
    end do

    allocate( vel(nvel), stat=ierr )
    if( ierr /= 0 ) return

    nvel      = 1
    vel(nvel) = vel_beg
    do while( vel(nvel) < vel_end )
        nvel      = nvel + 1
        vel(nvel) = vel(nvel-1) + alpha * vel(nvel-1) ** ipwr
    end do

    vel(nvel) = vel_end
!
!   OUTPUT SCAN TABLE
!
    write( lu_tpnt, 904 ) vel_inc, nvel
    write( lu_tpnt, 905 )
    do j = 1, nvel
        write( lu_tpnt, 906 ) j, vel(j)
    end do

end subroutine sva_vgen

!!--------------------------- COMPUTE SEMBLANCE N ----------------------------!!
!!--------------------------- COMPUTE SEMBLANCE N ----------------------------!!
!!--------------------------- COMPUTE SEMBLANCE N ----------------------------!!

subroutine sva_compute_semblance_n( msc, nwin, nslc, ninc, nvel, trc, &
                nho_exp, nho_sign, vel, noff, off, tmute, bmute, nwih, &
                ndpt, tstrt, dt, type_semb, arr )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer         ,intent(in)    :: msc
    integer         ,intent(in)    :: nwin
    integer         ,intent(in)    :: nslc
    integer         ,intent(in)    :: ninc
    integer         ,intent(in)    :: nvel
    real            ,intent(in)    :: trc(:,:)
    integer         ,intent(in)    :: nho_exp
    integer         ,intent(in)    :: nho_sign
    integer         ,intent(in)    :: noff
    integer         ,intent(in)    :: nwih
    integer         ,intent(in)    :: ndpt
    real            ,intent(in)    :: tstrt
    real            ,intent(in)    :: dt
    real            ,intent(in)    :: vel(:)
    real            ,intent(in)    :: off(:)
    real            ,intent(in)    :: tmute(:)
    real            ,intent(in)    :: bmute(:)
    character(len=8),intent(in)    :: type_semb
    real            ,intent(inout) :: arr(:,:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: imhdr
    integer :: ivel
    integer :: imute(noff)
    real    :: st1(ndpt)
    real    :: st2(ndpt)
!-----------------------------------------------
!
    do ivel = 1, nvel

      if (type_semb == 'STANDARD') then

        call sva_add_trace_to_stack_n( nho_exp, nho_sign, vel(ivel), noff, &
             off, tmute, bmute, imute, nwih, ndpt, tstrt, dt, trc, st1, st2 )

        call sva_mscl( st1, noff, imute, msc, imhdr, dt, ndpt )
        call sva_mscl( st2, noff, imute, msc, imhdr, dt, ndpt )

        call sva_compute_semblance_1( noff, ndpt, nwin, nslc, ninc, &
                                      arr(:,ivel), st1, st2 )

      else

        call sva_compute_semblance_avo (nho_exp, nho_sign, vel(ivel), &
             noff, off, tmute, bmute, nwih, ndpt, tstrt, dt, trc,     &
             nwin, nslc, ninc, arr(:,ivel) )

      end if

    end do

end subroutine sva_compute_semblance_n

!!--------------------------- COMPUTE SEMBLANCE 1 ----------------------------!!
!!--------------------------- COMPUTE SEMBLANCE 1 ----------------------------!!
!!--------------------------- COMPUTE SEMBLANCE 1 ----------------------------!!

subroutine sva_compute_semblance_1( noff, ndpt, nwin, nslc, ninc, arr, st1, st2)

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: noff
    integer, intent(in)    :: ndpt
    integer, intent(in)    :: nwin
    integer, intent(in)    :: nslc
    integer, intent(in)    :: ninc
    real,    intent(inout) :: arr(:)
    real,    intent(in)    :: st1(:)
    real,    intent(in)    :: st2(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer                :: index1
    integer                :: islc
    integer                :: iwin
    integer                :: k
    real, dimension(nwin)  :: sembl
    real, dimension(ndpt)  :: st3
    real                   :: sum2
    real                   :: sum3
    real                   :: xtr
!-----------------------------------------------
!

    index1 = 1
    xtr    = noff
    st3    = 0.0

    do iwin = 1, nwin
        sum2 = 0.0
        sum3 = 0.0
        k    = index1

        st3(k:nslc-1+k) = st1(k:nslc-1+k) ** 2

        do islc = 1, nslc
            sum2 = sum2 + st2(islc-1+k)
            sum3 = sum3 + st3(islc-1+k)
        end do

        index1 = index1 + ninc

        if( sum3 /= 0.0 .and. sum2 /= 0.0 ) then
            sembl(iwin) = sum3 / ( sum2 * xtr )
        else
            sembl(iwin) = 0.0
        endif

    end do

!  Composite new semblance values into semblance array

    arr = arr + sembl

end subroutine sva_compute_semblance_1

!!-------------------------- ADD TRACE TO STACK N ----------------------------!!
!!-------------------------- ADD TRACE TO STACK N ----------------------------!!
!!-------------------------- ADD TRACE TO STACK N ----------------------------!!

subroutine sva_add_trace_to_stack_n( nho_exp, nho_sign, vel, noff, off, &
               tmute, bmute, imute, nwih, ndpt, tstrt, dt, trc, st1, st2 )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in) :: nho_exp
    integer, intent(in) :: nho_sign
    integer, intent(in) :: noff
    integer, intent(in) :: ndpt
    integer, intent(in) :: nwih
    real,    intent(in) :: vel
    real,    intent(in) :: tstrt
    real,    intent(in) :: dt
    integer, intent(out):: imute(:)
    real,    intent(in) :: off(:)
    real,    intent(in) :: tmute(:)
    real,    intent(in) :: bmute(:)
    real,    intent(in) :: trc(:,:)
    real,    intent(out):: st1(:)
    real,    intent(out):: st2(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer             :: ioff
!-----------------------------------------------
!
!  initialize the two stacks to 0.0

    st1 = 0.0
    st2 = 0.0

!  add the contribution from each offset

    do ioff = 1, noff
        call sva_add_trace_to_stack_1( nho_exp, nho_sign, vel, off(ioff), &
                  tmute(ioff), bmute(ioff), imute(ioff), ndpt, tstrt, dt, &
                  trc(1:,ioff), st1, st2 )
    end do

end subroutine sva_add_trace_to_stack_n

!!-------------------------- ADD TRACE TO STACK 1 ----------------------------!!
!!-------------------------- ADD TRACE TO STACK 1 ----------------------------!!
!!-------------------------- ADD TRACE TO STACK 1 ----------------------------!!

subroutine sva_add_trace_to_stack_1( nho_exp, nho_sign, vel, off, tmute, &
                             bmute, imute, ndpt, tstrt, dt, trc, st1, st2 )
!
!  add moveout corrected trace and trace**2 to st1 and st2
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: nho_exp
    integer, intent(in)    :: nho_sign
    integer, intent(out)   :: imute
    integer, intent(in)    :: ndpt
    real,    intent(in)    :: vel
    real,    intent(in)    :: off
    real,    intent(in)    :: tmute
    real,    intent(in)    :: bmute
    real,    intent(in)    :: tstrt
    real,    intent(in)    :: dt
    real,    intent(in)    :: trc(:)
    real,    intent(inout) :: st1(:)
    real,    intent(inout) :: st2(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: ibmute1
    real    :: trc1(ndpt)
!-----------------------------------------------
!
    call sva_nmo (nho_exp, nho_sign, vel, off, ndpt, tstrt, &
                  dt, tmute, bmute, trc, imute, ibmute1, trc1 )

    st1(imute:ibmute1) = st1(imute:ibmute1) + trc1(imute:ibmute1)
    st2(imute:ibmute1) = st2(imute:ibmute1) + trc1(imute:ibmute1)**2

end subroutine sva_add_trace_to_stack_1

!!-------------------------- COMPUTE SEMBLANCE AVO ---------------------------!!
!!-------------------------- COMPUTE SEMBLANCE AVO ---------------------------!!
!!-------------------------- COMPUTE SEMBLANCE AVO ---------------------------!!

subroutine sva_compute_semblance_avo (nho_exp, nho_sign, vel, noff, off, &
             tmute, bmute, nwih, ndpt, tstrt, dt, trc, nwin, nslc, ninc, &
             arr )
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer         ,intent(in)    :: nho_exp
    integer         ,intent(in)    :: nho_sign
    real            ,intent(in)    :: vel
    integer         ,intent(in)    :: noff
    real            ,intent(in)    :: off(:)
    real            ,intent(in)    :: tmute(:)
    real            ,intent(in)    :: bmute(:)
    integer         ,intent(in)    :: nwih
    integer         ,intent(in)    :: ndpt
    real            ,intent(in)    :: tstrt
    real            ,intent(in)    :: dt
    real            ,intent(in)    :: trc(:,:)
    integer         ,intent(in)    :: nwin
    integer         ,intent(in)    :: nslc
    integer         ,intent(in)    :: ninc
    real            ,intent(inout) :: arr(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer        :: noff_use
    integer        :: ioff, iwin, iwtop, iwbot
    real           :: alpha, beta, gamma, delta, sembl
    real           :: trc1(ndpt,noff)
    real           :: off2(noff)
    integer        :: itmute1(noff)
    integer        :: ibmute1(noff)
    real           :: d_dot_e1(nslc)
    real           :: d_dot_e2(nslc)
    real           :: d_dot_d(nslc)
    real           :: e1_dot_e2
    real           :: e2_dot_e2
    real           :: e2_norm
    real,parameter :: eps = 1.0E-6
!-----------------------------------------------
!
    do ioff = 1, noff
      call sva_nmo (nho_exp, nho_sign, vel, off(ioff), ndpt, tstrt, dt,   &
                    tmute(ioff), bmute(ioff), trc(:,ioff), itmute1(ioff), &
                    ibmute1(ioff), trc1(:,ioff) )
    end do

    off2 = off(:noff) ** 2
    iwtop = 1 - ninc

    do iwin = 1, nwin

      iwtop = iwtop + ninc
      iwbot = iwtop + nslc - 1
      noff_use  = 0
      d_dot_e1  = 0.0
      d_dot_e2  = 0.0
      d_dot_d   = 0.0
      e1_dot_e2 = 0.0
      e2_dot_e2 = 0.0

      do ioff = 1, noff
        if (itmute1(ioff) > iwtop .or. ibmute1(ioff) < iwbot) cycle
        noff_use  = noff_use  + 1
        d_dot_e1  = d_dot_e1  + trc1(iwtop:iwbot,ioff)
        d_dot_e2  = d_dot_e2  + trc1(iwtop:iwbot,ioff)*off2(ioff)
        d_dot_d   = d_dot_d   + trc1(iwtop:iwbot,ioff)**2
        e1_dot_e2 = e1_dot_e2 + off2(ioff)
        e2_dot_e2 = e2_dot_e2 + off2(ioff)**2
      end do

      if (noff_use < 2 .or. e2_dot_e2 == 0.0) cycle
      e2_norm = e2_dot_e2 - ((e1_dot_e2 ** 2) / noff_use)
      if (e2_norm <= eps * e2_dot_e2) cycle
      e2_norm = 1.0 / sqrt(e2_norm)
      d_dot_e2 = e2_norm * (d_dot_e2 - (e1_dot_e2 / noff_use) * d_dot_e1)
      d_dot_e1 = (1.0 / sqrt(real(noff_use))) * d_dot_e1

      alpha = sum (d_dot_e1 ** 2)
      beta  = sum (d_dot_e1 * d_dot_e2)
      gamma = sum (d_dot_e2 ** 2)
      delta = sum (d_dot_d)
      if (delta == 0.0) cycle

      sembl = sqrt ((0.5*(alpha-gamma))**2 + beta**2)
      sembl = (0.5*(alpha+gamma) + sembl) / delta
      arr(iwin) = arr(iwin) + sembl

    end do

end subroutine sva_compute_semblance_avo

!!-------------------------- APPLY NMO CORRECTION ----------------------------!!
!!-------------------------- APPLY NMO CORRECTION ----------------------------!!
!!-------------------------- APPLY NMO CORRECTION ----------------------------!!

subroutine sva_nmo (nho_exp, nho_sign, vel, off, ndpt, tstrt, dt, &
                    tmute, bmute, trc, itmute1, ibmute1, trc1 )
!
!  This routine applies NMO correction
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)  :: nho_exp
    integer, intent(in)  :: nho_sign
    real,    intent(in)  :: vel
    real,    intent(in)  :: off
    integer, intent(in)  :: ndpt
    real,    intent(in)  :: tstrt
    real,    intent(in)  :: dt
    real,    intent(in)  :: tmute
    real,    intent(in)  :: bmute
    real,    intent(in)  :: trc(:)
    integer, intent(out) :: itmute1
    integer, intent(out) :: ibmute1
    real,    intent(out) :: trc1(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: it, it1
    real    :: offvel, tstrt1
    real    :: tmute1, tmute_dopp, bmute1
    real    :: tnmo(ndpt)
!-----------------------------------------------
!
    trc1(:ndpt) = 0.0
    if (nint(tmute) > ndpt) then
      itmute1 = ndpt
      ibmute1 = ndpt
      return
    end if

    offvel = nho_sign * (off/vel)**nho_exp / dt**2
    tstrt1 = tstrt/dt - 1.0

    tmute1 = sqrt(max(0.0,max(0.0,tmute+tstrt1)**2 - offvel)) - tstrt1
    tmute_dopp = sqrt(abs(offvel)/(doppler2 - 1.0))
    if (offvel < 0.0) tmute_dopp = tmute_dopp * doppler
    tmute_dopp = tmute_dopp - tstrt1
    bmute1 = sqrt(max(0.0,max(0.0,bmute+tstrt1)**2 - offvel)) - tstrt1

    itmute1 = max (1, nint(max(tmute1,tmute_dopp)))
    if (real(itmute1) < tmute1) itmute1 = itmute1 + 1
    ibmute1 = min (ndpt, nint(bmute1))
    if (real(ibmute1) >= bmute1) ibmute1 = ibmute1 - 1
    if (itmute1 > ibmute1) goto 10

!   compute the moveout time
    do it = itmute1, ibmute1
      tnmo(it) = sqrt(max(0.0,(it+tstrt1)**2 + offvel)) - tstrt1
    end do
    if (int(tnmo(itmute1)) < nint(tmute)) itmute1 = itmute1 + 1
    if (int(tnmo(ibmute1)) >= nint(bmute)) ibmute1 = ibmute1 - 1

!   move out the trace
    do it = itmute1, ibmute1
      it1 = int(tnmo(it))
      trc1(it) =  trc(it1) + (tnmo(it)-it1)*(trc(it1+1)-trc(it1))
    end do
 10 itmute1 = min (itmute1, ndpt)
    ibmute1 = max (ibmute1, itmute1)

end subroutine sva_nmo

!!------------------------------ COMPUTE MUTE --------------------------------!!
!!------------------------------ COMPUTE MUTE --------------------------------!!
!!------------------------------ COMPUTE MUTE --------------------------------!!

subroutine sva_compute_mute( ndpt, noff, tmute, off, tmute_min, omute_min )

!  compute the minimum time mute and the corresponding offset

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)  :: ndpt
    integer, intent(in)  :: noff
    real,    intent(in)  :: tmute(:)
    real,    intent(in)  :: off(:)
    real,    intent(out) :: tmute_min
    real,    intent(out) :: omute_min
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: ioff
!-----------------------------------------------
!

    tmute_min = ndpt
    omute_min = 0.0

    do ioff = 1, noff
        if( tmute(ioff) < tmute_min ) then
            tmute_min = tmute(ioff)
            omute_min = off(ioff)
        endif
    end do

end subroutine sva_compute_mute

!!------------------------- MUTE SCALING -------------------------------------!!
!!------------------------- MUTE SCALING -------------------------------------!!
!!------------------------- MUTE SCALING -------------------------------------!!

subroutine sva_mscl( tr, npart, mtbl, ifac, mtbp, dt, nt )
!
!     MUTE SCALING. BALANCE THE EARLY PORTION OF A COMPOSITED
!     TRACE WHOSE PARTS HAVE BEEN MUTED TO VARIOUS POINTS ACCORDING
!     TO OFFSET
!
!     ITR    = TRACE TO BE BALANCED
!     NPART  = NUMBER OF PARTS (TRACES) USED TO FORM THE COMPOSITE
!              IN ITR
!     MTBL   = TABLE CONTAINING THE UNSORTED MUTE SUBSCRIPTS OF
!              EACH PART
!     FAC    = FLAG, INPUT TO INDICATE DEGREE OF SCALING TO BE DONE
!              OR WEIGHTING FACTOR
!              0 = NO SCALING
!              1 = FULL SCALING
!              2 = HALF SCALING
!              4 = QUARTER SCALING
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: npart
    integer, intent(in)    :: ifac
    integer, intent(out)   :: mtbp
    integer, intent(in)    :: nt
    real,    intent(in)    :: dt
    integer, intent(inout) :: mtbl(:)
    real,    intent(inout) :: tr(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real           :: costab(101)
    real           :: fac
    real           :: facp(npart)
    integer        :: i
    integer        :: i1
    integer        :: j
    integer        :: j1
    integer        :: jj
    integer        :: k
    integer        :: maxt
    integer        :: mpart
    integer        :: nct
    integer        :: nctm
    integer        :: np
    real           :: part
    real,parameter :: pi = 3.14159265

    real           :: sum
    real           :: theta
!-------------------------------------------------------------------------------
!
!---SORT THE POINTERS
    fac   = ifac
    mpart = npart
    call sva_srtr( mtbl, mpart, 1, 1 )
!
    nct  = 0.060 / dt
    nct  = min0( 100, nct )
    nct  = ( nct / 2 ) * 2 + 1
    nctm = nct - 1
    maxt = nt - nct + 1

!---COMPUTE COSINE TAPER TABLE
    costab = 0.1
    do i = 1, nct
        theta     = ( pi * ( i - 1 ) ) / ( nct - 1 )
        costab(i) = ( 1.0 - cos( theta ) ) / 2.0
    end do
    mpart = mpart - count( mtbl(:npart) >= maxt )
!
    if( mpart < 1 ) return
    if( fac < 1.0 ) return

    np   = mpart - 1
    part = mpart
!
!---COSINE TAPER MUTE SCALE
!
    mtbp = mtbl(mpart) + nctm
    facp(:mpart) = ( part / (/ ( j1, j1 = 1, mpart ) /) - 1.0 ) / fac + 1.0
!
    i1 = max0( 1, mtbl(1) )
    do i = i1, mtbp
        sum = 0.0
        jj  = 0
!
        do j = 1, mpart
            if( mtbl(j) <= i ) then
                jj = jj + 1
                if( (mtbl(j) <= 1) .or. (i > mtbl(j)+nctm) ) then
                    sum = sum + 1.0
                else
                    k   = i - mtbl(j) + 1
                    sum = sum + costab(k)
                endif
            endif
        end do

!-------NOW, APPLY SCALE TO THIS VALUE
        if( sum /= 0.0 ) tr(i) = tr(i) * ( facp(jj) / sum ) * jj
    end do
!
!---TRACE SCALED, REAPPLY COSINE TAPER
    mtbp        = mtbl(1) - 1
    tr(1:mtbp)  = 0.0
    mtbp        = mtbp + nct
    tr(i1:mtbp) = tr(i1:mtbp) * costab(1:mtbp-i1+1)
!
end subroutine sva_mscl

!!----------------------------- SORT TRACES ----------------------------------!!
!!----------------------------- SORT TRACES ----------------------------------!!
!!----------------------------- SORT TRACES ----------------------------------!!

subroutine sva_srtr( a, n, nwd, ifld )
!
!   CALLING SEQUENCE
!
!     CALL SVA_SRTR(A, N, NWD, IFLD)
!           A      = ARRAY TO BE SORTED (CONTAINS FIXED RECORD LENGTHS)
!           N      = NO. OF RECORDS
!           NWD    = NO. OF WORDS IN EACH RECORD
!           IFLD   = WORD NO. IN THE RECORD WHICH IS THE SORT KEY
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n
    integer, intent(in)    :: nwd
    integer, intent(in)    :: ifld
    integer, intent(inout) :: a(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: b
    integer :: i
    integer :: ii
    integer :: iii
    integer :: j
    integer :: k
    integer :: kk
    integer :: m
!-------------------------------------------------------------------------------
!
    m = n
    do
        m = m / 2
        if( m == 0 ) return

        do j = 1, n - m
            do i = j, 1, -m
                k  = i + m
                ii = i * nwd - nwd + ifld
                kk = k * nwd - nwd + ifld
                if( a(ii) <= a(kk) ) exit

!---------------SWITCH A(I) & A(K)
                ii = ii - ifld
                kk = kk - ifld
                do iii = 1, nwd
                    ii = ii + 1
                    kk = kk + 1
                    b = a(ii)
                    a(ii) = a(kk)
                    a(kk) = b
                end do
            end do
        end do
    end do
!
end subroutine sva_srtr

!!------------------------------ NORMALIZE ----------------------------------!!
!!------------------------------ NORMALIZE ----------------------------------!!
!!------------------------------ NORMALIZE ----------------------------------!!

subroutine sva_norm( arr, nvel, nwin, npsm, times, ncl, lu_tpnt )
!
!---COMPUTES EXPANSION ARRAY FOR SEMBLANCE
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    real,    intent(inout) :: arr(:,:)
    integer, intent(in)    :: nvel
    integer, intent(in)    :: nwin
    integer, intent(inout) :: npsm
    real,    intent(in)    :: times(:)
    integer, intent(in)    :: ncl
    integer, intent(in)    :: lu_tpnt
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer                :: ivel
    integer                :: iwin
    real                   :: xmax
    real, dimension(nwin)  :: xpn
!-------------------------------------------------------------------------------

    if( mod( npsm, 2 ) == 0 ) npsm = npsm + 1
!
!---FIRST, COMPUTE HORIZONTAL SUM OVER VELOCITIES
!
    xpn(1:nwin) = sum( arr(1:nwin,:), dim = 2 )
!
!---FIND MAX IN FIRST NPSM VALUES
!
    xmax = 0.0
    do iwin = 1, npsm
        xmax = max( xpn(iwin), xmax )
    end do
    if( xmax == 0.0 ) xmax = nvel

    call sva_smsp( xpn, nwin, npsm )
!
!---FILL IN FIRST VALUES
!
    xpn(1:npsm/2) = xmax
!
!---COMPUTE & APPLY EXPANSION
!
    where( xpn(1:nwin) <= 0.0 )
        xpn(1:nwin) = 0.0
    elsewhere
        xpn(1:nwin) = 1.0 / xpn(:nwin)
    end where

    xpn(1:ncl) = 0.0
    do ivel = 1, nvel
        arr(1:nwin,ivel) = xpn(1:nwin) * arr(1:nwin,ivel)
    end do

    if( prin ) then
        write( lu_tpnt, * )' EXPANSION FOR SEMBLANCE: WINDOW #, TIME, EXPANSION'
        do iwin = 1, nwin
            write( lu_tpnt, '(5x,i5,g15.5,g15.5)' ) iwin, times(iwin), xpn(iwin)
        end do
    endif

end subroutine sva_norm

!!--------------------------- MOVING AVERAGE ---------------------------------!!
!!--------------------------- MOVING AVERAGE ---------------------------------!!
!!--------------------------- MOVING AVERAGE ---------------------------------!!

subroutine sva_smsp( p, np, na )

!     MOVING AVERAGE OF AN ARRAY
!
!     P      = ARRAY TO BE SMOOTHED
!     NP     = NO. OF POINTS IN P
!     NA     = NO. OF POINTS TO AVERAGE

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    real,    intent(inout) :: p(:)
    integer, intent(in)    :: np
    integer, intent(in)    :: na
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real                   :: fa
    integer                :: i

    integer                :: k
    integer                :: n1
    integer                :: n2
    integer                :: n3
    integer                :: n4
    integer                :: n5
    integer                :: npm1
    real, dimension(np)    :: wrk
!-------------------------------------------------------------------------------
!
    n1 = na
    if( mod( n1, 2 ) == 0 ) n1 = n1 + 1

    fa     = 1.0 / real( n1 )
    n2     = n1 / 2
    n3     = n2 + 1
    n4     = np - n2
    n5     = n4 + 1
    npm1   = np - 1
    wrk(1) = p(1)

    if( n2 >= 2 ) then
        k = 0
        do i = 2, n2
            k  = k + 1
            wrk(i) = sum( p(i-k:i+k) ) / real(k+k+1)
        end do
    endif

    do i = n3, n4
        wrk(i) = fa * sum( p(i-n2:i+n2) )
    end do

    k = n2
    if( n2 >= 2 ) then
        do i = n5, npm1
            k  = k - 1
            wrk(i) = wrk(i) + sum( p(i-k:i+k) ) / real(k+k+1)
        end do
    endif
    wrk(np) = 0.5 * ( p(np) + p(np-1) )

    p(1:np) = wrk(1:np)

end subroutine sva_smsp

!!------------------------------- GATHER -----------------------------------!!
!!------------------------------- GATHER -----------------------------------!!
!!------------------------------- GATHER -----------------------------------!!

subroutine sva_gathr( nn, hdi, tri, hdo, tro, nwih, ndpt, ihdg, hdr_off,    &
                      off_init, off_inc, off_tot, igrp, ndone, ntotal, ierr )
!
!***********************************************************************
!*******  the following special option is not available in cfe *********
!***********************************************************************
!  opt       -      -3      same as 3 but does not composite.
!
!  off_tot   -      >=1     number of traces per output group.
!  ihdg      -      1-nwih  header number to define groups.
!  hdr_off   -      1-nwih  header number to define sort bins.
!  off_init  -      real    header value for first sort bin.
!  off_inc   -      real    increment between sort bins.
!
!   option 3 collects consecutive traces having the same
!   group bin number  =  hd(ihdg)
!   any positive or negative integer values of the group bin
!   number are allowed.
!   the traces are sorted (stacked if necessary) into bins
!   defined by the sort bin number  =  nint((hd(hdr_off)-off_init)/off_inc+1.),
!   which must be in the range from 1 to off_tot.  traces outside
!   this range are ignored.  output traces are normalized by
!   the fold of stack (saved in header 32).  missing bins
!   are returned with dead traces.
!
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer,         intent(inout) :: nn
    double precision,intent(in)    :: hdi(:,:)
    real,            intent(in)    :: tri(:,:)
    double precision,intent(inout) :: hdo(:,:)
    real,            intent(inout) :: tro(:,:)
    integer,         intent(in)    :: nwih
    integer,         intent(in)    :: ndpt
    integer,         intent(in)    :: ihdg
    integer,         intent(in)    :: hdr_off
    real,            intent(in)    :: off_init
    real,            intent(in)    :: off_inc
    integer,         intent(in)    :: off_tot
    integer,         intent(inout) :: igrp
    integer,         intent(inout) :: ndone
    integer,         intent(inout) :: ntotal
    integer,         intent(out)   :: ierr
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: jgrp
    integer :: k
    integer :: khd
    integer :: nbin
    integer :: nleft1
    integer :: nleft2
!
!-------------------------------------------------------------------------------
!
    ierr = 0
    if( nn == 0 .and. ndone == 0 ) return

    if( nn > 0 ) then
        nleft1 = 1
        nleft2 = nn

        if( ndone == 0 ) then
!-----------set up output area
            tro      = 0.0
            hdo      = 0.0D0
            hdo(2,:) = ndpt + 1
        endif

        do k = nleft1, nleft2
!-----------check if this trace belongs to this gather
            jgrp = hdi(ihdg,k)
            if( ndone >= 1 .and. jgrp  /= igrp ) then
                ierr = 1
                return
            endif

!-----------find correct bin for trace
            nbin = nint( ( hdi(hdr_off,k) - off_init ) / off_inc + 1.0 )

!-----------ignore trace if not in bin
            if( nbin < 1 .or. nbin > off_tot ) cycle

            ndone = ndone + 1

!-----------move trace to output bin
            tro(1:ndpt,nbin) = tri(1:ndpt,k)
            hdo(5:nwih,nbin) = hdi(5:nwih,k)
            hdo( 2,    nbin) = hdi(2,     k)
            hdo(32,    nbin) = 1.0D0

!-----------store header value for first trace in gather
            if( ndone == 1 ) igrp = hdi(ihdg,k)
        enddo
    else
!-------pass output group to next process
        if( ndone > 0 ) ndone = off_tot

!-------normalize to fold of stack
        do khd = 1, ndone
            if( hdo(32,khd) /= 0 ) exit
        enddo

        do k = 1, ndone
            if( hdo(32,k)  > 1.5 ) then
                tro( 1:ndpt,k) = tro( 1:ndpt,k) / hdo(32,k)
                hdo( 6:21,  k) = hdo( 6:21,  k) / hdo(32,k)
                hdo(33:47,  k) = hdo(33:47,  k) / hdo(32,k)
                hdo(56:nwih,k) = hdo(56:nwih,k) / hdo(32,k)
            end if
        enddo

        do k = 1, ndone
            if( hdo(32,k) == 0 ) then
                hdo( 7,k) = hdo( 7,khd)
                hdo( 8,k) = hdo( 8,khd)
                hdo(17,k) = hdo(17,khd)
                hdo(18,k) = hdo(18,khd)
                hdo(37,k) = hdo(37,khd)
                hdo(38,k) = hdo(38,khd)

                if( ihdg == 7 .or. ihdg == 8  .or. ihdg == 17 .or. ihdg == 18 &
                              .or. ihdg == 37 .or. ihdg == 38 ) then
                    hdo( 7,k) = hdo( 7,khd)
                    hdo( 8,k) = hdo( 8,khd)
                    hdo(17,k) = hdo(17,khd)
                    hdo(18,k) = hdo(18,khd)
                    hdo(37,k) = hdo(37,khd)
                    hdo(38,k) = hdo(38,khd)

                elseif( ihdg == 9 .or. ihdg == 11 .or. ihdg == 12       &
                                  .or. ihdg == 33 .or. ihdg == 34 ) then
                    hdo( 9,k) = hdo( 9,khd)
                    hdo(11,k) = hdo(11,khd)
                    hdo(12,k) = hdo(12,khd)
                    hdo(33,k) = hdo(33,khd)
                    hdo(34,k) = hdo(34,khd)

                elseif( ihdg == 14 .or. ihdg == 15 .or. ihdg == 35 &
                                   .or. ihdg == 36 ) then
                    hdo(14,k) = hdo(14,khd)
                    hdo(15,k) = hdo(15,khd)
                    hdo(35,k) = hdo(35,khd)
                    hdo(36,k) = hdo(36,khd)
                endif

                hdo(ihdg,   k) = igrp
                hdo(hdr_off,k) = off_init + ( k - 1 ) * off_inc
            endif
        enddo

!-------return to pass out gather
!
!-------set headers 1, 3 and 4 for output

        if( ndone > off_tot ) ndone = off_tot

        do k = 1, ndone
            hdo(1,k) = ntotal + k
            hdo(3,k) = igrp
            hdo(4,k) = k
        enddo

        ntotal = ntotal + ndone
        nn     = ndone
        ndone  = 0

    endif

end subroutine sva_gathr

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

subroutine sva_wrapup( obj )
    implicit none
    type(sva_struct),intent(inout) :: obj       ! dummy variable

    integer                        :: i         ! local
    integer                        :: ierr      ! local - return condition code
    character(len=MAX_LINE)        :: line      ! local - line buffer
    integer                        :: n         ! local

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

!---dump temporary printer file to the printer

    endfile obj%lu_tpnt
    rewind  obj%lu_tpnt

    write( lu_print, '( " ", 70("*") )' )
    write( lu_print, '( " -------> SVA PROCESS OUTPUT <------- " / )' )

    do
        read(  obj%lu_tpnt, '( A132 )', iostat=ierr ) line
        if( ierr /= 0 ) exit

        n = MAX_LINE
        do while( line(n:n) == ' ' .and. n > 1 )
            n = n - 1
        end do

        write( lu_print, '( 160A )' ) (line(i:i), i = 1, n)
    end do

    write( lu_print, '( / " -------> END SVA PROCESS <------- " )' )
    write( lu_print, '( " ", 70("*") )' )

end subroutine sva_wrapup

!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!

subroutine sva_dump_struct( obj )
    implicit none

    type(sva_struct),intent(inout) :: obj       ! dummy variable

    write( obj%lu_tpnt, '( / " ***** DUMP STRUCTURE ***** " / )' )

    write( obj%lu_tpnt, '( " skip_wrapup = ", l1       )' ) obj%skip_wrapup
    write( obj%lu_tpnt, '( " order_mo    = ", i4       )' ) obj%order_mo
    write( obj%lu_tpnt, '( " num_semb    = ", i4       )' ) obj%num_semb
    write( obj%lu_tpnt, '( " hdr_diag    = ", 3(i4,2x) )' ) obj%hdr_diag
    write( obj%lu_tpnt, '( " hdr_off     = ", i4       )' ) obj%hdr_off
    write( obj%lu_tpnt, '( " off_tot     = ", i4       )' ) obj%off_tot
    write( obj%lu_tpnt, '( " regular     = ", l1       )' ) obj%regular
    write( obj%lu_tpnt, '( " vel_beg     = ", f8.3     )' ) obj%vel_beg
    write( obj%lu_tpnt, '( " vel_end     = ", f8.3     )' ) obj%vel_end
    write( obj%lu_tpnt, '( " vel_inc     = ", f8.3     )' ) obj%vel_inc
    write( obj%lu_tpnt, '( " freq_dom    = ", f8.3     )' ) obj%freq_dom
    write( obj%lu_tpnt, '( " win_len     = ", f8.3     )' ) obj%win_len
    write( obj%lu_tpnt, '( " win_inc     = ", f8.3     )' ) obj%win_inc
    write( obj%lu_tpnt, '( " off_init    = ", f8.3     )' ) obj%off_init
    write( obj%lu_tpnt, '( " off_inc     = ", f8.3     )' ) obj%off_inc
    write( obj%lu_tpnt, '( " off_last    = ", f8.3     )' ) obj%off_last
    write( obj%lu_tpnt, '( " type_scan   = ", a5       )' ) obj%type_scan
    write( obj%lu_tpnt, '( " type_semb   = ", a8       )' ) obj%type_semb
    write( obj%lu_tpnt, '( " pathname    = ", a60      )' ) obj%pathname
    write( obj%lu_tpnt, '( " nwih        = ", i4       )' ) obj%nwih
    write( obj%lu_tpnt, '( " ndpt        = ", i4       )' ) obj%ndpt
    write( obj%lu_tpnt, '( " tstrt       = ", f8.3     )' ) obj%tstrt
    write( obj%lu_tpnt, '( " dt          = ", f8.3     )' ) obj%dt
    write( obj%lu_tpnt, '( " lu_tpnt     = ", i4       )' ) obj%lu_tpnt
    write( obj%lu_tpnt, '( " nho_sign    = ", i4       )' ) obj%nho_sign
    write( obj%lu_tpnt, '( " nho_exp     = ", i4       )' ) obj%nho_exp
    write( obj%lu_tpnt, '( " nslc        = ", i4       )' ) obj%nslc
    write( obj%lu_tpnt, '( " ninc        = ", i4       )' ) obj%ninc
    write( obj%lu_tpnt, '( " fvmin       = ", f8.3     )' ) obj%fvmin
    write( obj%lu_tpnt, '( " tmute_min   = ", f8.3     )' ) obj%tmute_min
    write( obj%lu_tpnt, '( " omute_min   = ", f8.3     )' ) obj%omute_min
    write( obj%lu_tpnt, '( " xnearoff    = ", f8.3     )' ) obj%xnearoff
    write( obj%lu_tpnt, '( " nwin        = ", i4       )' ) obj%nwin
    write( obj%lu_tpnt, '( " nmin        = ", i4       )' ) obj%nmin
    write( obj%lu_tpnt, '( " nout        = ", i4       )' ) obj%nout
    write( obj%lu_tpnt, '( " igrp        = ", i4       )' ) obj%igrp
    write( obj%lu_tpnt, '( " ndone       = ", i4       )' ) obj%ndone
    write( obj%lu_tpnt, '( " ntotal      = ", i4       )' ) obj%ntotal
    write( obj%lu_tpnt, '( " trout       = ", f8.3     )' ) obj%trout
    write( obj%lu_tpnt, '( " troutb      = ", f8.3     )' ) obj%troutb
    write( obj%lu_tpnt, '( " ncdp        = ", i4       )' ) obj%ncdp
    write( obj%lu_tpnt, '( " ilive       = ", i4       )' ) obj%ilive
    write( obj%lu_tpnt, '( " nvel        = ", i4       )' ) obj%nvel
    write( obj%lu_tpnt, * )

    write( obj%lu_tpnt, '( / " ***** END OF DUMP STRUCTURE ***** " / )' )

end subroutine sva_dump_struct

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

end module sva_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
