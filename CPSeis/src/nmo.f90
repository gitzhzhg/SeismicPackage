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
!------------------------------------------------------------------------------
!                        C P S   P R O C E S S
!
! Name       : NMO  (Normal MoveOut correction)
! Category   : transforms
! Written    : 1990-03-15   by: John B. Sinton, Bob Baumel, and Bill Troutt
! Revised    : 2007-07-03   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : To apply or remove NMO corrections to seismic data.
! Portability: No known limitations.
! Parallel   : Yes.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! NMO has several modes of operation: perform normal moveout correction
! on seismic data, restore normal moveout to seismic data, perform partial
! moveout, and put velocity values into output traces.
!
! NMO requires that the velocities in the input velocity file are stacking
! velocities and will abort if they are not type VTNM (NMO velocities versus
! time) or VTRM (RMS velocities versus time). The front-end will attempt to
! read the specified velocity file and will post the coordinate header word
! numbers used by that velocity file in the read-only parameters HDR_INL and
! HDR_CRL.
!
! There is no limit to the maximum number of time/depth/velocity picks on
! any velocity function.
!
! Order of Moveout Term
!
! The NMO parameter ORDER_MO allows choice between 2nd order moveout and
! 4th order residual moveout, or both combined.  2nd order is ordinary,
! hyperbolic, NMO and is the default.  If 4th order residual moveout is
! chosen (but not both combined), NMO assumes that the 2nd order correction
! has already been applied to the input data.  There are three options for
! the 4th order moveout, one using the traditional fourth order "velocity"
! parameter and two using the eta (anisotropic moveout) parameter.
!
! Stretch Muting
!
! Stretch muting is applied in both forward and reverse NMO.  A cosine taper
! (of length 15 samples) is applied to the mute in FORWARD (but not REVERSE)
! NMO.
!
! Partial NMO
!
! NMO has a "partial NMO" option which transforms a trace from its input
! offset to the offset specified in scratch header 32.  The offset header
! word 6 will be swapped with scratch header 32 before exiting the process.
! This option is used by DMOPREP and is available by internal call only, not
! in CFE.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS
!
! Demultiple:
!
! NMO provides parameters that allow the user to perform F-K demultiple in
! conjunction with the CPS process FKF.  This functionality is provided for
! diagnostic or experimental tasks; normally RADON demultiple, such as
! provided by CPS process RMUL, yields superior demultiple results.
!
! F-K demultiple is performed jointly by CPS processes NMO and FKF as
! follows:
!
! 1.  Apply NMO with OPT_DEMULT = Y and OPT_NMO = FORWARD.  This moves out the
! data so that primaries and multiples will have opposite dip in CMPs.  In this
! mode NMO expects stacking velocities that will flatten the primaries and
! will calculate special dmult velocity functions for the forward dmult NMO
! operation.
!
! 2.  Run FKF in DEMULT mode to zero the half of the F-K plane occupied by the
! multiples.
!
! 3.  Run NMO with OPT_DEMULT = Y and OPT_NMO = REVERSE to restore the
! moveout to the data.  NMO expects the same input velocities as in step 1.
! (Typically velocity analysis is run after this step.)
!
!-------------------------------------------------------------------------------
!                         PARAMETERS NOT IN CFE
!
! The parameters VEL_BIAS and VEL_SCALE are used to modify the velocity
! function as needed by GVS.
! These parameters are not available on the NMO front-end GUI.
! VEL_BIAS and VEL_SCALE default to 0.0 and 1.0 respectively.
! This is effectively a no-op if NMO is not called by GVS.
!
! The parameter TRACEMUTE controls whether or not the trace is muted.
! If TRACEMUTE is true (default), the trace is muted as determined by the
! doppler mute parameter.  If TRACEMUTE is false, the trace is not muted
! although the mute header words are still set as if the trace were muted.
! This parameter is primarily intended for the VA program and is not available
! on the NMO front-end GUI.
!
!------------------------------------------------------------------------------
!                       TYPES OF CPS VELOCITY FILES
!
! The following CPS velocity file formats can be read by NMO:
!
!  (1) old-style CPS ascii  velocity files.
!  (2) self-defining ascii  velocity files.
!  (3) self-defining hybrid velocity files.
!  (4) self-defining binary velocity files.
!  (5) modspec files containing velocities.
!
! Velocities in modspec files are always interval velocities, but may be
! in either the time or depth domain.  They will be converted to NMO velocities
! versus time when they are read from the file.
!
! Velocities in the other types of files must be NMO velocities versus time
! (either type VTNM or VTRM); no velocity conversion will occur.  Otherwise
! NMO will generate a fatal error message.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                       TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! If ORDER_MO = 4 or ETA4 or ETA, ordinary NMO correction must have already
! been applied to the input data.
!
! Process requires traces to be input in CMP order for the demultiple option
! only.
!
! If OPT_VELSOURCE = VELOCITY_TRACE_INPUT, seismic traces must be gathered by
! CMP, and velocity traces must be the last one or two traces in the gather.
! If ORDER_MO = 2, there must be one velocity trace (stacking velocity).
! If ORDER_MO = 4, there must be one velocity trace (4th order "velocity"
! parameter).  For all other ORDER_MO choices, there must be two velocity
! traces (stacking velocity trace followed by 4th order "velocity" or eta
! parameter).
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives.
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of trace header words          used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! NUMTR    maximum number of traces in gather    used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#      Description                      Action taken
! ----      -----------                      ------------
! 2         head mute                        used and reset
! 6         offset                           used in NMO calculation
! 25        largest absolute value           recomputed
! 32        new offset for partial NMO       temporary/scratch usage
! HDR_X     vel func X coordinate hdr word   used
! HDR_Y     vel func Y coordinate hdr word   used
! 64        tail mute                        used and reset
!
! Headers 2 and 64 are set to 1 and NDPT for options 'STK_VEL' and 'DIX_VEL'.
! Headers 6 and 32 are swapped for option 'PARTIAL'.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 87. 2007-07-03 Stoeckley  Remove error dialog box when trying to read a
!                            velocity trace file in a parallel job (works
!                            when tried in SeisSpace).
! 86. 2005-01-31 Stoeckley  Add ability to read modspec files (documentation
!                            change only).  Fix to allow filename to be omitted
!                            when vel_bias is not zero.
! 85. 2004-10-14 Stoeckley  Add error dialog box when trying to read a velocity
!                            trace file in a parallel job (interim problem
!                            until an unsolved bug is fixed).
! 84. 2004-05-11 Stoeckley  Add additional velocity input options; add
!                            additional moveout options including use of
!                            the eta parameter.
! 83. 2003-11-03 Stoeckley  Documentation changes to reflect changes in
!                            several primitives; this includes reinstatement
!                            and improvement of some doppler mute capabilities.
! 82. 2003-10-27 Stoeckley  Fix several long-standing bugs (such as array
!                            out-of-bounds bugs) which sometimes caused aborts
!                            (particularly for less-used combinations of
!                            parameters); add protective code to guard against
!                            sqrt(negative) and uninitialization problems;
!                            prohibit certain inadvisable parameter
!                            combinations; move most code to the new primitive
!                            NMOPRIM (and to several additional new primitives
!                            used by NMOPRIM) in order to enhance reusability
!                            and simplify maintenance.
!                           The new primitives have some changes and additions,
!                            such as a test for valid velocity function type
!                            on file, and use of trace bin numbers (nearest
!                            integer) rather than exact coordinates when
!                            deciding whether it is necessary to interpolate
!                            a new velocity function.
!                           The new primitives also eliminate the use of a
!                            temp file to store resampled velocity functions.
!                           There is no longer any limit to the maximum number
!                            of time/velocity picks on any velocity function.
!                           The old NMO had the curious habit of muting time
!                            reversals with doppler<0 but not with doppler=0,
!                            (probably the reverse of what was intended), and
!                            muting refractions with doppler <=0 and >2 (but
!                            not between 0 and 2); therefore this new NMO
!                            eliminates the refraction mute, and always mutes
!                            time reversals since it is dangerous not to do so.
! 81. 2003-10-14 R.S.Day    Move location of scratch velocity file to /tmp/.
!                           Return from update if temporary-file open fails.
! 80. 2003-06-17 Stoeckley  Increase maximum time/velocity pair limit from 100
!                            to 401 as a stopgap measure prior to more complete
!                            overhauls.
! 79. 2002-05-20 Goodger    Remove TIM_BEG and TIM_END parameters.
! 78. 2002-05-06 Vunderink  Added parallel control parameters
! 77. 2002-04-17 Goodger    Expand message when resetting TIM_END to indicate
!                           old and new time.
! 76. 2001-12-10 Selzler    Removed reference to spline_module (not needed).
! 75. 2001-10-12 Selzler    Temporarily back out parallel support
! 74. 2001-10-11 Selzler    Clarified backend error message if does not exist.
! 73. 2001-10-04 Selzler    Added GUI file selection for input velocity file
!                           and warning if the file does not exist.
! 72. 2001-08-03 Selzler    Added PCPS call for parallelization
! 71. 2001-04-30 Selzler    Corrected print_lun initialization and NMC usage.
! 70. 2001-02-20 Selzler    Changed default for pathcheck extension.
! 69. 2000-12-11 Selzler    Changed wrapup logic to use skip_wrapup
! 68. 2000-11-07 Selzler    Eliminated double msg when velio open has an error.
! 67. 2000-06-07 Selzler    Fix minor bug in gui_def.
! 66. 2000-06-01 Selzler    Fix minor bug in parameter validation.
! 65. 2000-05-11 Selzler    Inserted EZGUI layout
! 64. 2000-05-10 Selzler    Corrected sensitivity for OPT_DEMULT parms
! 63. 2000-05-02 Selzler    Removed PARTIAL from gui put options field.
! 62. 2000-04-27 Selzler    Added VEL_BIAS and VEL_SCALE for GVS
! 61. 2000-02-23 Selzler    Workaround velio_open_read error reporting
! 60. 2000-02-10 Selzler    Corrected bug in wrapup logic
! 59. 2000-02-07 Selzler    improved gui support
! 58. 2000-02-02 Selzler    Added support for GUI and general cleanup
! 57. 2000-01-13 Selzler    Corrected FFT scale factor, 2nd try
! 56. 2000-01-12 Selzler    Corrected FFT scale factor
! 55. 2000-01-11 Selzler    fixed bug with unitialized variable
! 54. 2000-01-03 Selzler    fixed bug with hdr word for vel coordinate
! 53. 1999-12-07 Selzler    fixed bug with conversion to gridcheck primitive
! 52. 1999-11-23 Selzler    fixed lines that exeeded 80 characters.
! 51. 1999-11-12 Selzler    fixed bug regarding Y coordinate
! 50. 1999-10-27 Selzler    Added RCS "Id" strings to tag executeable
! 49. 1999-10-20 Selzler    Updated velio calls to new version.
! 48. 1999-09-29 Selzler    Conversion to f90.
! 47. 1998-11-10 Vunderink  Begin using the f90 compiler.
! 46. 1998-07-20 Vunderink  Added NMOT=PARTIAL for use by DMOPREP.
! 45. 1998-03-03 Stoeckley  Add IRES = -1 option for cubic interpolation.
! 44. 1997-11-17 Goodger    Fix bug when IPRT=1.  The calls to cpsprt
!                           were invalid.
! 43. 1997-08-18 Hanson     Add do loops 1029 and 1129 to compute
!                           istrt and istop when NHOSIGN<0.
! 42. 1997-04-29 Vunderink  Increased length NCODE format statemetns.
! 41. 1997-04-23 Vunderink  Added moveout order information to history
!                           and CPR printout
! 40. 1996-10-03 Hanson     Added nonhyperbolic moveout correction.
!                           t(offset) = sqrt(t(offset=0)**2 + NHOSIGN *
!                                     (offset - velocity)**NHOEXP)
! 39. 1996-07-11 Vunderink  Added FFT interpolation to reverse NMO
!                           correction.
! 38. 1996-07-08 Vunderink  Added IRES parameter and FFT interpolation to
!                           forward NMO correction.
! 37. 1994-05-06 Troutt     Make minor changes to VTRC=YES for Bill Harlan.
!                           Avoid reciprocals, squares, and square roots
!                           so that negative and zero "velocities" from
!                           VPICK will survive.
! 36. 1994-04-26 Troutt     Make internally callable (always putp,repp,
!                           getp)
! 35. 1993-07-27 Troutt     Change muting for NMOT=FORWARD, TAU_P=NO, and
!                           DOPLER=-1 to match that for NMOT=REVERSE (i.e.
!                           it is now more severe than before, but less
!                           severe than DOPLER=0).  The zone of decreasing
!                           travel times (TX's) is removed in both cases.
! 34. 1993-02-23 Troutt     Change default for RATO from 1.0 to 0.7 and
!                           abort if RATO.GE.1.0 when DEMULT=YES.
! 33. 1992-10-08 Troutt     Allow DOPLER=-1 for NMOT=REVERSE.
! 32. 1992-02-28 Troutt     Add logic for tail mute HW64 (call MUTEHW).
! 31. 1990-12-07 Ball       Put in compile directive CDIR$ IVDEP on
!                           SQRTFN loops for performance.
! 30. 1990-06-18 Troutt     Clarify "functions read from disk" message at
!                           N=0 time.
! 29. 1990-04-20 Troutt     If DOPLER<0.0, then disable the "Refraction"
!                           and "Event-crossing" mute logic. This option
!                           available only for NMOT=FORWARD and TAU_P=NO.
! 28. 1990-03-15 Troutt     CHANGED PROCESS NAME TO "NMO" due to major
!                           algorithm changes in velocity interpolation
!                           (using NMO to back out corrections made via
!                           NMO - or vice versa - would not be valid).
!                              *** Specific changes *********
!                           a. All input functions are ultimately resampled
!                           to the trace sample interval "DT" at setup
!                           for ALL process OPTIONS.  Furthermore, the
!                           1-V**2 operation on velocities (TAU_P=NO)
!                           now takes place AFTER interpolation to a
!                           rate of DT.  These changes allow functions
!                           for TAU_P=NO (and DEMULT=NO) to be faithfully
!                           interpolated in time when ITPTYP=LINEAR.
!                           (i.e. For "regular" nmo of x-t data, the
!                           function "knees" are completely honored).
!                           DeMult functions are still INITIALLY resampled
!                           to "VRSP" for purposes of calculating multiple
!                           velocities.
!                           b. If there are more than 4 fns., all fns. are
!                           are managed via DTWRITE-DTREAD (in Buffer
!                           Memory if available).  The algorithm only does
!                           spatial vel. interpolation when the trace
!                           header words (NXB# and-or NYB#) change, and
!                           only retrieves fns. from "disk" when the needed
!                           input fns. change.
!                           *** MINIMAL function I-O is achieved when ****
!                           *** traces come in in CDP order.          ****
!                           c. VTR! and DEMULT must be "NO" for TAU_P=YES.
!                           d. Added VTRC=DIX option.
!                           e. Added checks at setup for velocity function
!                           locations: must be on a grid, and locations
!                           must increase (x within y) else abort.
!
!.                          *******
!.                          All revisions through #27 apply to process name NMO
!.                          *******
!
! 27. 1990-01-04 Troutt     Added parameter VTRC to capture moveout
!                           velocities in trace. Also added printout
!                           of zero-offset table times for IPRT=1
! 26. 1989-08-15 Baumel     Allow ITPTYP=LINEAR with TAU_P=YES.
! 25. 1989-08-10 Baumel     Taper mute in FORWARD NMO.
! 24. 1989-08-09 Baumel     Fix mute to better suppress crossing events
!                           and refractions.  Also fix SCRATCH pointers.
! 23. 1989-01-23 Sinton     Added missing pointer IBCOF.
! 22. 1988-12-29 Sinton     Added VELFILE parameter.
! 21. 1988-09-26 Howard     NWIH and NWPT conversion.
! 20. 1988-08-26 Baumel     Change $ALL_VEL to ALL_VEL for future
!                           compatibility with UNICOS.
! 19. 1988-06-20 Baumel     Interpolate on the fly in forward NMO (inacti-
!                           vates INTT); reverse NMO for TAU_P=YES has
!                           doppler mute, interpolates same as TAU_P=NO;
!                           some additional cleanup.
! 18. 1988-06-06 Baumel     New convention for mute header word.
! 17. 1988-05-17 Sinton     Modified doppler mute and reverse NMO for
!                           TAU_P=NO mode.
! 16. 1988-04-06 Sinton     Added linear interpolation of velocities as
!                           the default for TAU_P=NO mode. Also corrected
!                           an error in scratch memory calculation.
! 15. 1988-02-16 Sinton     Changed FOLDOFST parameter to NOFFSET.
! 14. 1988-01-20 Sinton     Corrected NMOT=REVERSE option to use
!                           velocity as a function of offset travel time.
! 13. 1987-12-10 Sinton     Modified space-time NMO correction for the
!                           NMOT=REVERSE (Produces better results).
! 12. 1987-11-30 Sinton     Added DE-MULT option.
! 11. 1987-06-19 Sinton     Increased NVELF from 100 to 500.
! 10. 1987-05-15 Sinton     Added parameter OFFSET to override header
!                           value.  Added the vf_name=$ALL_VEL option.
! 9.  1987-05-03 Baumel     Split off velocity function interpolation
!                           into a new primitive routine INTPVELF.
! 8.  1987-04-28 Baumel     Fix a bug in interpolating velocity functions.
! 7.  1987-04-13 Baumel     Remove the now-inactive NOFF and OFFMAX parms.
! 6.  1987-04-09 Hanson     Add NCODE for history records.
! 5.  1987-03-31 Baumel     Correct layered moveout in TAU_P option,
!                           more correct doppler mute with either option.
! 4.  1986-09-15 Sinton     Fixed a problem with interp_2d_var_lin_real and
!                             interp_2d_var_lin_int.
! 3.  1986-08-25 Sinton     Added TAU_P=YES modification for eliptical
!                           NMO.
! 2.  1986-07-30 Sinton     A call to CSPPRTF was added to print STORAGE,
!                           SCRATCH, and PARAMETERs.
!                           Change input parameter NMOT from numeric to
!                           alphanumeric
!                           (see INPUT PARAMETERS below).
! 1.  1986-07-14 Sinton     A call to RDEOF was added.
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
!                    SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                   SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
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
!                  ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! Order Parameter
! The NMOSIGN and NMOEXP parameters define a moveout correction of the form:
!
!     time(offset) =
!          SQRT(time(offset=0)**2 + NHOSIGN * (offset / velocity)**NHOEXP)
!
! The default values of NMOSIGN=+1 and NMOEXP=2 yield the traditional
! hyperbolic moveout.  Using values of NMOSIGN=-1 and NMOEXP=4 yield the
! fourth order residual nonhyperbolic moveout.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS NMO Process/NC=80>
!
!           To apply or remove NMO corrections to seismic data.
!
! OPT_NMO=`CCCCCC     OPT_VELSOURCE=`CCCCCCCCCCCCCCCCCCCCC  [/L]HDR_X=`II  HDR_Y=`II
!
! Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                 [PATHNAME_MORE]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! Select PATHNAME4[PATHNAME4]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [PATHNAME4_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                  [PATHNAME4_MORE]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! OPT_INTERP= `CCCCC   DOPPLER= `FFFFFFFFFFF 
!
! OPT_NMO_RES=`CCC     ORDER_MO=`CCCCCCCCCCCCCCCCCCCCCC
!
! OPT_DEMULT= `CC
!
! `- Demult Parameters -------------------------------------------------------
!   OFF_MIN=~~`FFFFFFFFFFF    OFF_MAX= `FFFFFFFFFFF    NUM_OFF= `II
!   TIM_RES=~~`FFFFFFFFFFF    VEL_MIN= `FFFFFFFFFFF    VEL_MUTE=`FFFFFFFFFFF 
!   TIM_ADD=~~`FFFFFFF        OFF_MUTE=`FFFFFFFFFFF    RATIO=~~~`FFFFFFFFFFF
!   FREQ_MEAN=`FFFFFFF
! `---------------------------------------------------------------------------
!
!<PARMS pathname      [/ML=128/XST]>
!<PARMS pathname4     [/ML=128/XST]>
!<PARMS pathname_info [/ML=128/XST]>
!<PARMS pathname4_info[/ML=128/XST]>
!<PARMS pathname_more [/ML=128/XST]>
!<PARMS pathname4_more[/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_NMO">
!<Tip> Type of NMO operation to perform. </Tip>
! Default = FORWARD
! Allowed = FORWARD  (Apply NMO correction.)
! Allowed = REVERSE  (Remove NMO correction.)
! Allowed = STK_VEL  (Put stacking velocity field into output traces.)
! Allowed = DIX_VEL  (Put Dix interval velocity field into output traces.)
! Allowed = PARTIAL  (Partial NMO correction - not available from CFE.)
!
! Options FORWARD and REVERSE apply NMO correction and remove NMO correction in
! the normal manner.  Option STK_VEL takes the interpolated input stacking
! velocity field and puts the velocity values in the output traces.  Option
! DIX_VEL is the same as STK_VEL except that the Dix interval velocity is first
! calculated from the input stacking velocity field and put into the output
! traces.
!
! Output traces have head and tail mute header words set to 1 and NDPT
! respectively in options STK_VEL and DIX_VEL.
!</Help>
!
!
!<Help KEYWORD="OPT_VELSOURCE">
!<Tip> Source of velocities to use for NMO correction. </Tip>
! Default = CPS VELOCITY FILE
! Allowed = CPS VELOCITY FILE      (standard CPS velocity file or modspec file)
! Allowed = VELOCITY TRACE FILE    (trace file containing velocity traces)
! Allowed = VELOCITY TRACE INPUT   (velocity traces input with trace gathers)
!
! CPS VELOCITY FILE:
!
! A standard CPS velocity file (normally with the extension .vel) is the
! traditional source of velocity functions.  The velocities on this file
! typically contain the time/velocity pairs picked using a program such as
! VA.  The velocity functions on this file are usually sparsely distributed
! through the seismic survey.  They must be distributed on a sorted rectangular
! grid to allow bilinear interpolation.  The velocity functions must be
! of type VTNM or VTRM (RMS velocity versus time).
!
! Modspec files containing velocities can also be read using this option.
! Velocities in modspec files are always interval velocities, but may be
! in either the time or depth domain.  They will be converted to NMO velocities
! versus time when they are read from the file.
!
! VELOCITY TRACE FILE:
!
! A velocity trace file (normally with extension .trc or .trc8 or .tr8)
! is a much larger file.  The velocities on this file are already sampled
! to the trace sample rate.  Currently, the velocity trace order on the file
! must match the order of the input seismic traces, and velocity traces
! matching the input seismic trace CMP locations must reside on the file,
! although this requirement may be relaxed in the future.  Also, the seismic
! traces and velocity traces must have the same sample rate, and the velocity
! traces must start at zero time.
!
! VELOCITY TRACE INPUT:
!
! With this option, no velocity file is read.  Instead, the velocity function
! is passed to NMO as a velocity trace along with the gather of traces to
! which the velocity function is to be applied.  The traces must be gathered
! by CMP with this option.  The velocity function is simply the last trace
! in the gather.  With this option, the traces must start at zero time.
!
! SOMETIMES TWO VELOCITY SETS ARE REQUIRED:
!
! If velocities are required for both 2nd order and 4th order moveout, then
! two CPS velocity files, or two velocity trace files, or two input velocity
! traces are required.  If two input velocity traces are required, they will
! be the last two traces in the gather, with the stacking velocity being the
! first trace of the two.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Trace header word containing X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!
! This header word must be specified when OPT_VELSOURCE is VELOCITY TRACE FILE.
!
! This header word is obtained from the CPS velocity files when
! OPT_VELSOURCE is CPS VELOCITY FILE.
!
! This header word is not used when OPT_VELSOURCE is VELOCITY TRACE INPUT.
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Trace header word containing Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!
! This header word must be specified when OPT_VELSOURCE is VELOCITY TRACE FILE.
!
! This header word is obtained from the CPS velocity files when
! OPT_VELSOURCE is CPS VELOCITY FILE.
!
! This header word is not used when OPT_VELSOURCE is VELOCITY TRACE INPUT.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME4_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME4. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_MORE" TYPE= "DISPLAY_ONLY">
!<Tip> Origin of stacking velocities used in this process. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME4_MORE" TYPE= "DISPLAY_ONLY">
!<Tip> Origin of 4th order or ETA parameters used in this process. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME4">
!<Tip> Choose PATHNAME4 using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the velocity file to be used for the NMO operation. </Tip>
! Default = NONE
! Allowed = character string
!
! This file must contain the stacking velocities needed for 2nd order moveout.
!
! This file is needed for all ORDER_MO choices except ORDER_MO = 4:
!
! If OPT_VELSOURCE is CPS VELOCITY FILE, this file must be a standard CPS
! velocity file (normally with the extension .vel) . This file must contain
! velocities of type VTNM or VTRM (stacking or RMS velocity versus time).
!
! If OPT_VELSOURCE is VELOCITY TRACE FILE, this file must be a velocity trace
! file (normally with the extension .trc or .trc8 or .tr8).
!
! If OPT_VELSOURCE is VELOCITY TRACE INPUT, this file name is not used;
! a velocity trace is input with a seismic trace gather instead.
!
! NOTE REGARDING MODSPEC FILES:  If OPT_VELSOURCE is CPS VELOCITY FILE,
! this file can also be a modspec file which contains velocities in either
! the time or depth domain.
!</Help>
!
!
!<Help KEYWORD="PATHNAME4">
!<Tip> Pathname for the velocity file to be used for the NMO operation. </Tip>
! Default = NONE
! Allowed = character string
!
! This file must contain the residual "velocity" or ETA parameters needed
! for 4th order moveout.
!
! This file is needed for all ORDER_MO choices except ORDER_MO = 2:
!
! If OPT_VELSOURCE is CPS VELOCITY FILE, this file must be a standard CPS
! velocity file (normally with the extension .vel). This file must contain
! velocities of type VTNM or VTRM (stacking or RMS velocity versus time).
!
! If OPT_VELSOURCE is VELOCITY TRACE FILE, this file must be a velocity trace
! file (normally with the extension .trc or .trc8 or .tr8).
!
! If OPT_VELSOURCE is VELOCITY TRACE INPUT, this file name is not used;
! a velocity trace is input with a seismic trace gather instead.
!
! NOTE REGARDING MODSPEC FILES:  If OPT_VELSOURCE is CPS VELOCITY FILE,
! this file can also be a modspec file which contains velocities in either
! the time or depth domain.
!</Help>
!
!
!<Help KEYWORD="OPT_INTERP">
!<Tip> Type of interpolation within velocity function. </Tip>
! Default = LINEAR
! Allowed = LINEAR  (Linear interpolation)
! Allowed = SPLINE  (Spline interpolation)
!
! This interpolation is used to resample the velocity function picks to the
! trace sample spacing.
!
! LINEAR is the more robust (and recommended) option.  The SPLINE option gives
! potentially smoother results but can swing wildly if picks are not well
! behaved.
!
! This OPT_INTERP parameter is used only with CPS velocity files, since
! velocity functions obtained otherwise are already resampled to the trace
! sample spacing.
!</Help>
!
!
!<Help KEYWORD="DOPPLER">
!<Tip> Doppler mute (stretch mute) parameter. </Tip>
! Default = 1.7
! Allowed = real >  1.0  (enables all muting)
! Allowed = real =  0.0  (enables all muting except the stretch mute)
! Allowed = real = -1.0  (enables only time reversal muting)
!
! DOPPLER is the maximum stretch factor allowed by the stretch mute.
! Larger values of DOPPLER correspond to less severe muting.
! Values of DOPPLER which are set <= 1.0 are reset to 0.0 or -1.0.
!
! The top and bottom mute header words (2 and 64) will first be adjusted up
! or down by the same amount as the corresponding trace sample moves according
! to the moveout adjustment.  Then further adjustments may be performed
! depending on the DOPPLER parameter.  The top of the trace will then be
! muted down to the doppler mute, and a mute taper will be applied.  See
! the MOVEOUT primitive for further details.
!
! This DOPPLER parameter is not used if OPT_NMO is STK_VEL or DIX_VEL.
!</Help>
!
!
!<Help KEYWORD="OPT_NMO_RES">
!<Tip> Type of resampling to perform within the NMO operation. </Tip>
! Default = FFT4
! Allowed = NONE  (No interpolation.)
! Allowed = FFT2  (FFT interpolation to 2 times more samples.)
! Allowed = FFT4  (FFT interpolation to 4 times more samples.)
! Allowed = FFT8  (FFT interpolation to 8 times more samples.)
! Allowed = CUB4  (4 point cubic spline interpolation.)
!
! Interpolating the trace data prior to NMO correction reduces attenuation at
! high frequencies, with finer interpolation producing less attenuation.
! 4-fold FFT interpolation gives good results at normal seismic frequencies
! (FFT4 option), but 4 point cubic spline interpolation (CUB4 option) gives
! nearly as good results at lower cost.
!
! This OPT_NMO_RES parameter is not used if OPT_NMO is STK_VEL or DIX_VEL.
!</Help>
!
!
!<Help KEYWORD="ORDER_MO">
!<Tip> Order of moveout expansion series to perform. </Tip>
! Default = 2
! Allowed = 2         (Normal 2nd order hyperbolic moveout.)
! Allowed = 4         (4th order non-hyperbolic residual moveout.)
! Allowed = ETA4      (4th order non-hyperbolic residual moveout using ETA.)
! Allowed = ETA       ("Exact" non-hyperbolic residual moveout using ETA.)
! Allowed = 2 + 4     (2nd order plus 4th order moveout combined.)
! Allowed = 2 + ETA4  (2nd order plus 4th order moveout combined using ETA.)
! Allowed = 2 + ETA   (2nd order plus "Exact" moveout combined using ETA.)
!
! Ordinary NMO is 2nd order, hyperbolic, NMO.  If the theoretical moveout
! expression is expanded in a series, the first non-zero term is the typical
! 2nd order, hyperbolic, NMO.  The next non-zero term is 4th order,
! non-hyperbolic, moveout.
!
! ORDER_MO allows choice between 2nd order moveout, three different residual
! moveout operations, or the combination of 2nd order plus residual moveout.
! If residual moveout only is chosen, NMO assumes that 2nd order (regular NMO)
! correction has already been applied to the input data.
!
! If ORDER_MO is not 2, it is reset to 2 in the following cases:
!  - whenever OPT_NMO is set to something other than FORWARD or REVERSE.
!  - (however, it is OK for ORDER_MO to be 4 when OPT_NMO is STK_VEL).
!</Help>
!
!
!<Help KEYWORD="OPT_DEMULT">
!<Tip> Option whether to perform special moveout for F-K demultiple. </Tip>
! Default = NO
! Allowed = YES/NO
!
! See Advice for Users for more information on running F-K demultiple.
!
! If OPT_DEMULT is YES, it is reset to NO in the following cases:
!  - whenever OPT_NMO is set to something other than FORWARD or REVERSE.
!  - whenever ORDER_MO is set to something other than 2.
!</Help>
!
!
!<Help KEYWORD="OFF_MIN">
!<Tip> Minimum offset in dataset. </Tip>
! Default = 0.0
! Allowed = real>0.0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="OFF_MAX">
!<Tip> Maximum offset in dataset. </Tip>
! Default = 5000.0
! Allowed = real>0.0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="NUM_OFF">
!<Tip> Maximum number of offsets in a shot profile. </Tip>
! Default = 120
! Allowed = int>0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="TIM_RES">
!<Tip> Temporal resampling interval for demult velocity, in seconds. </Tip>
! Default = 0.35
! Allowed = real>=0.2
!
! A new velocity function is calculated for NMO correction prior to F-K
! filtering.  TIM_RES is the temporal sample interval of this demult velocity
! function.
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="VEL_MIN">
!<Tip> Minimum demult velocity. </Tip>
! Default = 1200.0
! Allowed = real>0.0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="VEL_MUTE">
!<Tip> Velocity mute parameter for mute in demult operation. </Tip>
! Default = 1500.0
! Allowed = real>0.0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Additional mute time for mute in demult operation, in seconds. </Tip>
! Default = 0.0
! Allowed = real
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="OFF_MUTE">
!<Tip> Minimum offset for mute in demult operation. </Tip>
! Default = 0.0
! Allowed = real>=0.0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="RATIO">
!<Tip> Desired ratio of primary event amplitudes, after vs. before dmult. </Tip>
! Default = 0.7
! Allowed = 1.0>real>0.0
!
! Desired ratio of stacked primary event amplitude after demult to stacked
! primary event amplitude without demult.
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!
!<Help KEYWORD="FREQ_MEAN">
!<Tip> Mean frequency in data, in Hz. </Tip>
! Default = 20.0
! Allowed = real>0.0
!
! This parameter is used only if OPT_DEMULT is YES.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module nmo_module
      use pc_module
      use named_constants_module
      use pathcheck_module
      use pathchoose_module
      use lav_module
      use nmoprim_module
      use mth_module
      implicit none
      private
      public :: nmo_create
      public :: nmo_initialize
      public :: nmo_update
      public :: nmo_delete
      public :: nmo      
      public :: nmo_wrapup


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: nmo_struct
      private
        logical                         :: skip_wrapup  ! wrapup flag.

        integer                         :: nwih         ! global parameter.
        integer                         :: ndpt         ! global parameter.
        real                            :: dt           ! global parameter.
        real                            :: tstrt        ! global parameter.

        character(len=7)                :: opt_nmo      ! process parameter
        character(len=24)               :: opt_velsource! process parameter
        integer                         :: hdr_x        ! process parameter.
        integer                         :: hdr_y        ! process parameter.
        character(len=FILENAME_LENGTH)  :: pathname     ! process parameter
        character(len=FILENAME_LENGTH)  :: pathname4    ! process parameter
        character(len=6)                :: opt_interp   ! process parameter
        real                            :: doppler      ! process parameter.
        character(len=4)                :: opt_nmo_res  ! process parameter
        character(len=12)               :: order_mo     ! process parameter.
        logical                         :: opt_demult   ! process parameter.
        real                            :: off_min      ! process parameter.
        real                            :: off_max      ! process parameter.
        integer                         :: num_off      ! process parameter.
        real                            :: tim_res      ! process parameter.
        real                            :: vel_min      ! process parameter.
        real                            :: vel_mute     ! process parameter.
        real                            :: tim_add      ! process parameter.
        real                            :: off_mute     ! process parameter.
        real                            :: ratio        ! process parameter.
        real                            :: freq_mean    ! process parameter.
        real                            :: vel_bias     ! process parameter.
        real                            :: vel_scale    ! process parameter.
        logical                         :: tracemute    ! process parameter.

        type(pathchoose_struct),pointer :: pathchoose   ! dependent parameter.
        type(pathchoose_struct),pointer :: pathchoose4  ! dependent parameter.
        type(nmoprim_struct)   ,pointer :: nmoprim      ! dependent parameter.
      end type nmo_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(nmo_struct),pointer,save :: object           ! needed for traps.
      integer                 ,save :: lunprint = 0     ! state variable

      character(len=100),public :: nmo_ident = &
        "$Id: nmo.f90,v 1.87 2007/07/11 14:07:23 Stoeckley beta sps $"

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine nmo_create (obj)
      implicit none
      type(nmo_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()

      allocate (obj)

      nullify (obj%pathchoose)
      nullify (obj%pathchoose4)
      nullify (obj%nmoprim)

      call pathchoose_create (obj%pathchoose, 'pathname', 'vel')
      call pathchoose_create (obj%pathchoose4, 'pathname4', 'vel')

      call nmo_initialize (obj)
      return
      end subroutine nmo_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine nmo_delete (obj)
      implicit none
      type(nmo_struct),pointer :: obj       ! arguments

      call nmo_wrapup (obj)

      if (associated(obj%pathchoose))  call pathchoose_delete (obj%pathchoose)
      if (associated(obj%pathchoose4)) call pathchoose_delete (obj%pathchoose4)
      if (associated(obj%nmoprim))     call nmoprim_delete    (obj%nmoprim)

      deallocate(obj)
      return
      end subroutine nmo_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine nmo_initialize (obj)
      implicit none
      type(nmo_struct),pointer :: obj       ! arguments

      obj%opt_nmo       = 'FORWARD'
      obj%opt_velsource = 'CPS VELOCITY FILE'
      obj%hdr_x         = 7
      obj%hdr_y         = 8
      obj%pathname      = PATHCHECK_EMPTY
      obj%pathname4     = PATHCHECK_EMPTY
      obj%opt_interp    = 'LINEAR'
      obj%doppler       = 1.7
      obj%opt_nmo_res   = 'FFT4'   ! old default='NONE' newdoc='FFT4' (SELZLER)
      obj%order_mo      = '2'
      obj%opt_demult    = .false.
      obj%off_min       = 0.0
      obj%off_max       = 5000.0   ! old default=3500. newdoc= 5000. (SELZLER)
      obj%num_off       = 120
      obj%tim_res       = 0.35
      obj%vel_min       = 1200.0
      obj%vel_mute      = 1500.0
      obj%tim_add       = 0.0      ! old default= 0.2 newdoc= 0.0 (SELZLER)
      obj%off_mute      = 0.0
      obj%ratio         = 0.7
      obj%freq_mean     = 20.0
      obj%vel_bias      = 0.0      ! not in CFE - used only by GVS.
      obj%vel_scale     = 1.0      ! not in CFE - used only by GVS.
      obj%tracemute     = .true.   ! not in CFE.

      call nmo_update (obj)
      return
      end subroutine nmo_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine nmo_update (obj)
      implicit none
      type(nmo_struct),target :: obj                         ! arguments
      character(len=80)       :: msg,more1,more2             ! local
      logical                 :: error                       ! local
      logical                 :: general_sense               ! local
      logical                 :: opt_demult_sense            ! local
      logical                 :: opt_interp_sense            ! local
      logical                 :: hdr_xy_sense                ! local
      logical                 :: order_mo_sense              ! local
      logical                 :: pathname_sense              ! local
      logical                 :: pathname4_sense             ! local
      integer                 :: action,terpmode,sampmode    ! local
      integer                 :: numtr,velsource,order       ! local
      integer                 :: num_cpus                    ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update(obj%pathchoose, obj%pathname)) return
      if (pathchoose_update(obj%pathchoose4, obj%pathname4)) return

      call pc_get_jdata  ('num_cpus', num_cpus)

      call pc_get_global ('nwih' , obj%nwih)
      call pc_get_global ('ndpt' , obj%ndpt)
      call pc_get_global ('dt'   , obj%dt)
      call pc_get_global ('tstrt', obj%tstrt)
      call pc_get_global ('numtr', numtr)

      call pc_get('opt_nmo'      , obj%opt_nmo    )
      call pc_get('opt_velsource', obj%opt_velsource)
      call pc_get('hdr_x'        , obj%hdr_x      )
      call pc_get('hdr_y'        , obj%hdr_y      )
      call pc_get('pathname'     , obj%pathname   )
      call pc_get('pathname4'    , obj%pathname4  )
      call pc_get('opt_interp'   , obj%opt_interp )
      call pc_get('doppler'      , obj%doppler    )
      call pc_get('opt_nmo_res'  , obj%opt_nmo_res)
      call pc_get('order_mo'     , obj%order_mo   )
      call pc_get('opt_demult'   , obj%opt_demult )
      call pc_get('off_min'      , obj%off_min    )
      call pc_get('off_max'      , obj%off_max    )
      call pc_get('num_off'      , obj%num_off    )
      call pc_get('tim_res'      , obj%tim_res    )
      call pc_get('vel_min'      , obj%vel_min    )
      call pc_get('vel_mute'     , obj%vel_mute   )
      call pc_get('tim_add'      , obj%tim_add    )
      call pc_get('off_mute'     , obj%off_mute   )
      call pc_get('ratio'        , obj%ratio      )
      call pc_get('freq_mean'    , obj%freq_mean  )
      call pc_get('vel_bias'     , obj%vel_bias   ) ! not in CFE - for GVS only.
      call pc_get('vel_scale'    , obj%vel_scale  ) ! not in CFE - for GVS only.
      call pc_get('tracemute'    , obj%tracemute  ) ! not in CFE.

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      !*************************************
      !**** validate doppler ***************
      !*************************************

      if (obj%doppler < 0.0) then
           call pc_print ('**********************************************')
           call pc_print ('** Doppler set to -1 -- all muting disabled **')
           call pc_print ('**   except for elimination of zone with    **')
           call pc_print ('**      decreasing total travel times.      **')
           call pc_print ('**********************************************')
           obj%doppler = -1.0
      else if (obj%doppler <= 1.0) then
           call pc_print ('*************************************************')
           call pc_print ('** Doppler set to 0 -- stretch muting disabled **')
           call pc_print ('*************************************************')
           obj%doppler = 0.0
      end if

      !*************************************
      !**** validate opt_nmo ***************
      !*************************************

      call string_to_upper(obj%opt_nmo)

      select case (obj%opt_nmo(1:1))
           case ('F')   ; obj%opt_nmo = 'FORWARD'
           case ('R')   ; obj%opt_nmo = 'REVERSE'
           case ('S')   ; obj%opt_nmo = 'STK_VEL'
           case ('D')   ; obj%opt_nmo = 'DIX_VEL'
           case ('P')   ; obj%opt_nmo = 'PARTIAL'
           case default ; obj%opt_nmo = 'FORWARD'
                          call pc_warning ('Bad OPT_NMO - reset to FORWARD')
      end select

      !*************************************
      !**** validate opt_velsource *********
      !*************************************

      call string_to_upper(obj%opt_velsource)

      select case (obj%opt_velsource)
           case ('CPS VELOCITY FILE')    ; continue
           case ('VELOCITY TRACE FILE')  ; continue
           case ('VELOCITY TRACE INPUT') ; continue
           case default ; obj%opt_velsource = 'CPS VELOCITY FILE'
                          call pc_warning ('Bad OPT_VELSOURCE - &
                                           &reset to CPS VELOCITY FILE')
      end select

      if (obj%opt_velsource == 'VELOCITY TRACE INPUT') then
           if (numtr <= 1) then
             call pc_error ('Traces must be gathered if OPT_VELSOURCE is &
                            &VELOCITY TRACE INPUT')
           end if
           if (obj%tstrt /= 0.0) then
             call pc_error ('Traces must start at zero time if &
                            &OPT_VELSOURCE is VELOCITY TRACE INPUT')
           end if
      end if

 !    if (obj%opt_velsource == 'VELOCITY TRACE FILE') then
 !         if (num_cpus > 1) then
 !              call pc_error ('Because of an unsolved bug, OPT_VELSOURCE &
 !               &cannot be set to VELOCITY TRACE FILE for parallel jobs.')
 !              call pc_error ('Therefore you must change OPT_VELSOURCE &
 !               &or run a 1-CPU job until the problem gets fixed.')
 !         end if
 !    end if

      !***************************************
      !**** validate hdr_x and hdr_y *********
      !***************************************

      call mth_constrain (obj%hdr_x, 1, obj%nwih)
      call mth_constrain (obj%hdr_y, 1, obj%nwih)

      !*************************************
      !**** validate opt_nmo_res ***********
      !*************************************

      call string_to_upper(obj%opt_nmo_res)

      select case (obj%opt_nmo_res)
        case ('NONE') ; continue
        case ('FFT2') ; continue
        case ('FFT4') ; continue
        case ('FFT8') ; continue
        case ('CUB4') ; continue
        case default  ; obj%opt_nmo_res = 'FFT4'
                        call pc_warning ('Bad OPT_NMO_RES - reset to FFT4')
      end select

      !*************************************
      !**** validate order_mo **************
      !*************************************

      call string_to_upper(obj%order_mo)

      select case (obj%order_mo)
        case ('2')        ; continue
        case ('4')        ; continue
        case ('ETA4')     ; continue
        case ('ETA')      ; continue
        case ('2 + 4')    ; continue
        case ('2 + ETA4') ; continue
        case ('2 + ETA')  ; continue
        case default      ; obj%order_mo = '2'
                            call pc_warning ('Bad ORDER_MO - reset to 2')
      end select

      if (obj%order_mo /= '2') then
        if (obj%opt_nmo == 'STK_VEL' .and. obj%order_mo /= '4') then
           call pc_warning ('ORDER_MO must be 2 or 4 when OPT_NMO = STK_VEL')
           call pc_warning ('ORDER_MO reset to 2')
           obj%order_mo = '2'
        else if (obj%opt_nmo == 'DIX_VEL') then
           call pc_warning ('ORDER_MO must be 2 when OPT_NMO = DIX_VEL')
           call pc_warning ('ORDER_MO reset to 2')
           obj%order_mo = '2'
        else if (obj%opt_nmo == 'PARTIAL') THEN
           call pc_warning ('PARTIAL NON-HYPERBOLIC NMO NOT AVAILABLE')
           call pc_warning ('ORDER_MO reset to 2')
           obj%order_mo = '2'
        end if
      end if

      !*************************************
      !**** validate opt_interp ************
      !*************************************

      call string_to_upper(obj%opt_interp)

      select case (obj%opt_interp(1:1))
           case ('L')   ; obj%opt_interp = 'LINEAR'
           case ('S')   ; obj%opt_interp = 'SPLINE'
           case default ; obj%opt_interp = 'LINEAR'
                          call pc_warning ('Bad OPT_INTERP - reset to LINEAR')
      end select

      !*************************************
      !**** validate opt_demult ************
      !*************************************

      if(obj%opt_demult) then
        if (obj%opt_nmo == 'STK_VEL') then
           call pc_warning ('OPT_DEMULT must be NO when OPT_NMO = STK_VEL')
           call pc_warning ('OPT_DEMULT reset to NO')
           obj%opt_demult = .false.
        else if (obj%opt_nmo == 'DIX_VEL') then
           call pc_warning ('OPT_DEMULT must be NO when OPT_NMO = DIX_VEL')
           call pc_warning ('OPT_DEMULT reset to NO')
           obj%opt_demult = .false.
        else if(obj%opt_nmo == 'PARTIAL') then
           call pc_warning ('PARTIAL DEMULT NMO NOT AVAILABLE -')
           call pc_warning ('OPT_DEMULT reset to NO')
           obj%opt_demult = .false.
        else if (obj%order_mo /= '2') then
           call pc_warning ('DEMULT NON-HYPERBOLIC NMO NOT AVAILABLE')
           call pc_warning ('OPT_DEMULT reset to NO')
           obj%opt_demult = .false.
        end if
      end if

      !*************************************
      !**** validate demult parameters *****
      !*************************************

      if(obj%off_min < 0.0) then
          call pc_warning ('OFF_MIN must be greater than or equal to zero')
          call pc_warning (' - reset appropriately')
          obj%off_min = 0.0
      end if

      if(obj%off_max < obj%off_min) then
          call pc_warning ('OFF_MAX must be greater than or equal to OFF_MIN')
          call pc_warning (' - reset appropriately')
          obj%off_max = max(5000.0,obj%off_min)
      end if

      if(obj%num_off <= 0) then
          call pc_warning ('NUM_OFF must be greater than zero')
          call pc_warning (' - reset appropriately')
          obj%num_off = 120
      end if

      if(obj%tim_res < 0.2) then
          call pc_warning ('TIM_RES must be greater than or equal to 0.2')
          call pc_warning (' - reset appropriately')
          obj%tim_res = 0.35
      end if

      if(obj%vel_min <= 0.0) then
          call pc_warning ('VEL_MIN must be greater than 0.0')
          call pc_warning (' - reset appropriately')
          obj%vel_min = 1200.0
      end if

      if(obj%vel_mute <= 0.0) then
          call pc_warning ('VEL_MUTE must be greater than 0.0')
          call pc_warning (' - reset appropriately')
          obj%vel_mute = 1500.0
      end if

      if(obj%off_mute < 0.0) then
          call pc_warning ('OFF_MUTE must be greater than or equal to 0.0')
          call pc_warning (' - reset appropriately')
          obj%off_mute = 0.0
      end if

      if(obj%ratio <= 0.0 .or. obj%ratio >= 1.0) then
          call pc_warning ('RATIO must be > 0.0 and < 1.0')
          call pc_warning (' - reset appropriately')
          obj%ratio = 0.7
      end if

      if(obj%freq_mean <= 0.0) then
          call pc_warning ('FREQ_MEAN must be greater than 0.0')
          call pc_warning (' - reset appropriately')
          obj%freq_mean = 20.0
      end if

      !*************************************
      !**** validate pathname **************
      !*************************************

      pathname_sense = (obj%order_mo /= '4' .and. &
                        obj%opt_velsource /= 'VELOCITY TRACE INPUT')

      call pathcheck ('pathname', obj%pathname, 'vel',                     &
                      required=(pathname_sense .and. obj%vel_bias == 0.0), &
                      show=PATHCHECK_INFO_INPUT)

      !*************************************
      !**** validate pathname4 *************
      !*************************************

      pathname4_sense = (obj%order_mo /= '2' .and. &
                         obj%opt_velsource /= 'VELOCITY TRACE INPUT')

      call pathcheck ('pathname4', obj%pathname4, 'vel',                    &
                      required=(pathname4_sense .and. obj%vel_bias == 0.0), &
                      show=PATHCHECK_INFO_INPUT)

      !*****************************************
      !**** generate informational messages ****
      !*****************************************

      more1 = ' '
      more2 = ' '

      if (obj%order_mo /= '4') then
           select case (obj%opt_velsource)
                case ('CPS VELOCITY FILE')
                        more1 = 'CPS velocity file (or modspec file) &
                                &containing stacking velocities'
                case ('VELOCITY TRACE FILE')
                        more1 = 'velocity trace file &
                                &containing stacking velocities'
                case ('VELOCITY TRACE INPUT')
                        more1 = 'stacking velocities obtained from'
                        if (obj%order_mo == '2') then
                          more1 = trim(more1)//' last trace in gather'
                        else
                          more1 = trim(more1)//' 2nd to last trace in gather'
                        end if
           end select
      end if

      if (obj%order_mo /= '2') then
           select case (obj%order_mo)
                case ('4')        ; more2 = '4th order parameters'
                case ('ETA4')     ; more2 = 'ETA parameters'
                case ('ETA')      ; more2 = 'ETA parameters'
                case ('2 + 4')    ; more2 = '4th order parameters'
                case ('2 + ETA4') ; more2 = 'ETA parameters'
                case ('2 + ETA')  ; more2 = 'ETA parameters'
           end select
           select case (obj%opt_velsource)
                case ('CPS VELOCITY FILE')
                    more2 = 'CPS velocity file (or modspec file) &
                            &containing '//more2
                case ('VELOCITY TRACE FILE')
                    more2 = 'velocity trace file &
                            &containing '//more2
                case ('VELOCITY TRACE INPUT')
                    more2 = trim(more2)//' obtained from &
                            &last trace in gather'
           end select
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      ! Note: "PARTIAL" is not available from CFE (internal calls only).

      call pc_put_options_field ('opt_velsource',(/'CPS VELOCITY FILE   ', &
                                                   'VELOCITY TRACE FILE ', &
                                                   'VELOCITY TRACE INPUT'/))

      call pc_put_options_field ('order_mo', (/ '2        ', &
                                                '4        ', & 
                                                'ETA4     ', & 
                                                'ETA      ', & 
                                                '2 + 4    ', & 
                                                '2 + ETA4 ', & 
                                                '2 + ETA  ' /))

      call pc_put_options_field ('opt_interp', (/ "LINEAR", "SPLINE" /))

      call pc_put_options_field ('opt_nmo', &
                            (/ "FORWARD", "REVERSE", "STK_VEL", "DIX_VEL"/))

      call pc_put_options_field ('opt_nmo_res', &
                            (/ "NONE", "FFT2", "FFT4", "FFT8", "CUB4" /))

      call pc_put('opt_nmo'      , obj%opt_nmo    )
      call pc_put('opt_velsource', obj%opt_velsource)
      call pc_put('hdr_x'        , obj%hdr_x      )
      call pc_put('hdr_y'        , obj%hdr_y      )
      call pc_put('pathname'     , obj%pathname   )
      call pc_put('pathname4'    , obj%pathname4  )
      call pc_put('opt_interp'   , obj%opt_interp )
      call pc_put('doppler'      , obj%doppler    )
      call pc_put('opt_nmo_res'  , obj%opt_nmo_res)
      call pc_put('order_mo'     , obj%order_mo   )
      call pc_put('opt_demult'   , obj%opt_demult )
      call pc_put('off_min'      , obj%off_min    )
      call pc_put('off_max'      , obj%off_max    )
      call pc_put('num_off'      , obj%num_off    )
      call pc_put('tim_res'      , obj%tim_res    )
      call pc_put('vel_min'      , obj%vel_min    )
      call pc_put('vel_mute'     , obj%vel_mute   )
      call pc_put('tim_add'      , obj%tim_add    )
      call pc_put('off_mute'     , obj%off_mute   )
      call pc_put('ratio'        , obj%ratio      )
      call pc_put('freq_mean'    , obj%freq_mean  )

      call pc_put_process ('vel_bias'  , obj%vel_bias)     ! not in CFE.
      call pc_put_process ('vel_scale' , obj%vel_scale)    ! not in CFE.
      call pc_put_process ('tracemute' , obj%tracemute)    ! not in CFE.

      call pc_put_gui_only ('pathname_more' , more1)
      call pc_put_gui_only ('pathname4_more', more2)

      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')

      general_sense    = (obj%opt_nmo /= 'STK_VEL' .and. &
                          obj%opt_nmo /= 'DIX_VEL')
      order_mo_sense   = (obj%opt_nmo /= 'DIX_VEL' .and. &
                          obj%opt_nmo /= 'PARTIAL')
      opt_demult_sense = (obj%opt_nmo /= 'STK_VEL' .and. &
                          obj%opt_nmo /= 'DIX_VEL' .and. &
                          obj%opt_nmo /= 'PARTIAL' .and. &
                          obj%order_mo == '2')
      opt_interp_sense = (obj%opt_velsource == 'CPS VELOCITY FILE')
      hdr_xy_sense     = (obj%opt_velsource == 'VELOCITY TRACE FILE')

      call pc_put_sensitive_field_flag ('doppler'       , general_sense   )
      call pc_put_sensitive_field_flag ('opt_nmo_res'   , general_sense   )
      call pc_put_sensitive_field_flag ('order_mo'      , order_mo_sense  )
      call pc_put_sensitive_field_flag ('opt_demult'    , opt_demult_sense)
      call pc_put_sensitive_field_flag ('opt_interp'    , opt_interp_sense)
      call pc_put_sensitive_field_flag ('hdr_x'         , hdr_xy_sense    )
      call pc_put_sensitive_field_flag ('hdr_y'         , hdr_xy_sense    )
      call pc_put_sensitive_field_flag ('off_min'       , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('off_max'       , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('num_off'       , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('tim_res'       , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('vel_min'       , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('vel_mute'      , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('tim_add'       , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('off_mute'      , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('ratio'         , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('freq_mean'     , obj%opt_demult  )
      call pc_put_sensitive_field_flag ('pathname'      , pathname_sense  )
      call pc_put_sensitive_field_flag ('pathname4'     , pathname4_sense )
      call pc_put_visible_flag         ('pathname_info' , pathname_sense  )
      call pc_put_visible_flag         ('pathname4_info', pathname4_sense )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%nmoprim))   call nmoprim_delete   (obj%nmoprim)

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      select case (obj%opt_nmo)
           case ('FORWARD') ; action = NMOPRIM_FORWARD
           case ('REVERSE') ; action = NMOPRIM_REVERSE
           case ('STK_VEL') ; action = NMOPRIM_VNMO
           case ('DIX_VEL') ; action = NMOPRIM_VINT
           case ('PARTIAL') ; action = NMOPRIM_PARTIAL
           case default     ; action = NMOPRIM_FORWARD
      end select

      select case (obj%opt_velsource)
           case ('CPS VELOCITY FILE')    ; velsource = NMOPRIM_CPSFILE
           case ('VELOCITY TRACE FILE')  ; velsource = NMOPRIM_TRACEFILE
           case ('VELOCITY TRACE INPUT') ; velsource = NMOPRIM_VELTRACE
           case default                  ; velsource = NMOPRIM_CPSFILE
      end select

      select case (obj%order_mo)
           case ('2')        ; order = NMOPRIM_2
           case ('4')        ; order = NMOPRIM_4
           case ('ETA4')     ; order = NMOPRIM_ETA4
           case ('ETA')      ; order = NMOPRIM_ETA
           case ('2 + 4')    ; order = NMOPRIM_2_PLUS_4
           case ('2 + ETA4') ; order = NMOPRIM_2_PLUS_ETA4
           case ('2 + ETA')  ; order = NMOPRIM_2_PLUS_ETA
           case default      ; order = NMOPRIM_2
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

      call nmoprim_create (obj%nmoprim, lunprint, obj%nwih, obj%ndpt, &
                           obj%tstrt, obj%dt,                         &
                           action, order, terpmode,                   &
                           obj%doppler, obj%tracemute,                &
                           obj%pathname, obj%pathname4,               &
                           obj%vel_bias, obj%vel_scale, sampmode,     &
                           obj%hdr_x, obj%hdr_y, error, msg, velsource)

      if (error) call pc_error (msg)

      call nmoprim_demult (obj%nmoprim, obj%opt_demult, obj%tim_res,  &
                           obj%ratio, obj%off_max, obj%off_min,       &
                           obj%num_off, obj%freq_mean, obj%tim_add,   &
                           obj%vel_mute, obj%off_mute, obj%vel_min)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine nmo_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine nmo (obj,ntr,hd,tr)
      implicit none
      type(nmo_struct),intent(inout) :: obj                  ! arguments
      integer         ,intent(inout) :: ntr                  ! arguments
      double precision,intent(inout) :: hd(:,:)              ! arguments
      real   ,target  ,intent(inout) :: tr(:,:)              ! arguments
      integer                        :: indx,mtop,mbottom    ! local
      real                           :: xcoord,ycoord        ! local
      real                           :: offset,offnew        ! local
      logical                        :: error                ! local
      character(len=256)             :: msg                  ! local
      real   ,pointer                :: veltrace2(:)         ! local
      real   ,pointer                :: veltrace4(:)         ! local
      integer                        :: ntr2                 ! local

      if (ntr == NO_MORE_TRACES) then
           call nmo_wrapup (obj)
           return
      end if

      if (obj%opt_velsource == 'VELOCITY TRACE INPUT') then
           if (obj%order_mo == '2' .or. obj%order_mo == '4') then
                ntr2 = ntr - 1
                veltrace2 => tr(:,ntr)
                veltrace4 => tr(:,ntr)
           else if (ntr <= 1) then
                call pc_error ('NMO: trace gather too small to contain &
                               &required velocity traces')
                call nmo_wrapup (obj)
                ntr = FATAL_ERROR
                return
           else
                ntr2 = ntr - 2
                veltrace2 => tr(:,ntr-1)
                veltrace4 => tr(:,ntr)
           end if
      else
           ntr2 = ntr
           veltrace2 => tr(:,1)
           veltrace4 => tr(:,1)
      end if

      do indx = 1, ntr2

        xcoord  = hd(obj%hdr_x,indx)
        ycoord  = hd(obj%hdr_y,indx)
        offset  = hd(        6,indx)
        offnew  = hd(       32,indx)
        mtop    = hd(        2,indx)
        mbottom = hd(       64,indx)

        call nmoprim_apply (obj%nmoprim, xcoord, ycoord, offset, offnew, &
                            tr(:,indx), mtop, mbottom, error, msg,       &
                            veltrace2, veltrace4, (indx == 1))
        if (error) then
             call pc_error ('NMO:',msg)
             call nmo_wrapup (obj)
             ntr = FATAL_ERROR
             return
        end if

        hd( 6,indx) = offset
        hd(32,indx) = offnew
        hd( 2,indx) = mtop
        hd(64,indx) = mbottom
      end do

      call lav_set_hdr (hd, tr, obj%ndpt, ntr)
      return
      end subroutine nmo


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine nmo_wrapup (obj)
      implicit none
      type(nmo_struct) :: obj       ! arguments

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine nmo_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module nmo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
