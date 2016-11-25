!<CPS_v1 type="PROCESS"/>
!!------------------------------- tsvf.f90 ---------------------------------!!
!!------------------------------- tsvf.f90 ---------------------------------!!
!!------------------------------- tsvf.f90 ---------------------------------!!


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
! Name       : TSVF      (Time and Space Varying Filter)
! Category   : filters
! Written    : 1986-07-17   by: Bob Baumel
! Revised    : 2006-09-11   by: B. Menger
! Maturity   : production
! Purpose    : Perform time and space varying frequency filtering.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                            GENERAL DESCRIPTION                   
!
!
! TSVF is a time and space varying frequency filter process operating in the 
! time domain.  Depth data may also be filtered, but you must use appropriate
! spatial (in the depth (z) direction) frequencies to obtain good results.
! A typical 4mil (sample rate) time section has maximum(nyquist) frequency of
! 125 hz, but a typical 30foot (sample rate) depth section has a maximum freq-
! uency of only 16.67 cycles per kilofoot.  Therefore, a full bandpass for 
! depth is 0-16.67Hz, but 0-125Hz for time.
!
! Individual filter bands with linear tapers are specified by the 
! parameters: FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL, FREQ_HIGH_NONE, 
! PHASE, FILTER_TYPE and TIME.  Allowed values for FILTER_TYPE are BANDPASS, 
! HIGHPASS, LOWPASS and ALLPASS.  (The ALLPASS filter allows a phase rotation 
! without altering the amplitude spectrum.)
!
! Each filter band is applied to the trace exactly as specified at the time
! associated with that band.  The first filter band is applied from the top of 
! the trace to the time specified for that band.  The last filter band is 
! applied from the time specified for that band to the bottom of the trace. 
! For adjacent bands the filter applied to the trace grades linearly between
! the two bands in the interval between the times associated with those
! adjacent bands.
!
!
!                         Allowed Frequency Values
! Any frequencies from 0.0 to the Nyquist frequency is allowed.  Nyquist can
! be computed by the following: Nyq = 0.5/(sample rate)
!
!                            Spatial Variation
!
! In the TIME_ONLY mode, the filter can vary in time only.  There is only one 
! set of frequencies and times.
!
! In the MUTE mode, the specified times are relative to the head mute time, so 
! that the head mute time acts as "zero."  Otherwise this mode is the same as 
! the TIME_ONLY mode.
! 
! In the GRID mode, times and filter bands are specified on a 3D grid.  In this 
! mode filter bands specified at different grid intersections can be entirely 
! independent, that is, there is no restriction that the same frequencies or
! number of bands be used.  
! 
!
!                             Detailed Operation
!
! TIME_ONLY mode:  
!
!    (Steps 1 - 4 happen at Setup time.)
!
!    1.  Calculate a filter band for EACH SAMPLE of the trace by linear 
!    interpolation between the times associated with each specified filter 
!    band and round the interpolated frequencies to whole numbers.  (Before the
!    time for the first specified band and after the time for the last 
!    specified band the bands are extrapolated as constants.)
!
!    2.  Find the total range of frequencies represented in the specified 
!    filter bands.  These frequencies (whole numbers only) are the allowed 
!    frequencies for the filter bands that will actually be applied to the 
!    traces.   
!
!    3.  Determine all the possible distinct lowpass filter bands using the 
!    allowed frequencies.  Calculate the time domain operators for these 
!    lowpass filter bands and store them in an array that is indexed by their
!    defining frequencies so they can be easily recovered.  
!
!    4.  When processing traces, for each set of samples with the same 
!    interpolated filter band, retrieve the lowpass operators with the 
!    appropriate frequencies and combine them to form the desired lowpass, 
!    highpass or bandpass operator.  Apply the operator as a convolutional 
!    operator to filter the trace.  (Because the frequencies change by only
!    1.0 Hz at a time, no overlap between adjacent bands in time is needed.)
! 
!    (Details of the algorithm used to form highpass or bandpass operators from
!    lowpass operators are shown in the Algorithm Description section.)
!
!
! MUTE mode:  
!
!    Operation in the MUTE mode is identical to operation in the TIME_ONLY mode
!    except that times associated with user specified filter bands are 
!    interpreted as relative to the head mute time.  (That is, the head mute 
!    time acts as "zero" time.)
!
!
! GRID mode:
!
!    At Setup time, operation in GRID mode is similar to operation in TIME_ONLY 
!    mode with the following differences.
!
!      1. Interpolation of filter bands to every sample is performed at all 
!         the 3D grid intersections specified.
!
!      2. The range of allowed frequencies is based on the entire range of 
!         frequencies specified at all the 3D grid intersections.
!
!    When processing traces, linear interpolation is performed first in the
!    X direction and then in the Y direction to determine the filter band 
!    values for each trace sample.  Then the trace is filtered in the same 
!    fashion as in TIME_ONLY mode.
!
!
!                              Mute Header Words
!
! Although filter tails are added to traces by TSVF, the values of the mute 
! header words are not changed by the process.  The specified filter always 
! begins at the top of the trace and ends at the bottom of the trace regardless
! of the mute header word values.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                    
!
!
! Filter Band Times
!
! Because TSVF does not use the overlap and taper method to achieve time 
! variation, the user specified times associated with filter bands are 
! "control point" times and not transition times.  Each filter band is applied 
! to the trace exactly as specified at the time associated with that band.
!
! 
! Width of Tapers
!
! It is necessary to use non-zero taper widths when specifying filter bands in
! order to limit the amplitude of ripple on the operator.  Using wide tapers 
! produces a compact, center weighted operator, but this also reduces frequency 
! selectivity.  A taper width that is 40% of the center-taper frequency gives
! good control over operator ripple, but 20% allows significant ripple after 
! the first sidelobe.  
!
!
! Operator Length
!
! High frequencies and wide tapers are associated with short operators while 
! low frequencies and narrow tapers require longer operators.   A LEN_OP value
! of 0.4 sec is barely adequate for a 10 Hz center-taper frequency with 4 Hz 
! taper width.  Lower center-taper frequencies and/or narrower taper widths will
! require longer operators.  Specifying a LEN_OP value that is too small 
! truncates the operator causing undesirable overshoot and ripple in the 
! frequency domain.
!
! For low frequencies it may be desirable to use a frequency domain filter, 
! such as GENFILT, rather than making LEN_OP very long. 
!
!-------------------------------------------------------------------------------
!                INTERPOLATION AMONG THE X AND Y COORDINATES
!
! Using the GRID mode, this process interpolates among control points
! distributed on a 2D plane.  The algorithm is bilinear interpolation, first
! in the X direction along lines of constant Y coordinate, and then in the
! Y direction between the two bracketing Y lines.
!
! There must be one or more lines of constant Y coordinate.  The spacing
! between these lines can be variable.  Each line must contain one or more
! control points.  The spacing between the control points can be variable
! along the line.  The number and distribution of control points can vary
! from one line to another.  No two control points can occupy the same
! location.
!
! To get an interpolated point at location (XCOORD,YCOORD), this algorithm
! first finds the two lines bracketing YCOORD.  Then, on each line, the
! algorithm finds the two control points bracketing XCOORD.  Finally, the
! algorithm calculates the weights to use for the four control points to
! determine an interpolated value.
!
! Example of a valid distribution of control points on a plane, showing the
! interpolation procedure:
!
! The control points are marked with the # character.
! The interpolated point is marked with the $ character.
!
!      line YCOORD1 -->   - - - # - - - - # - - - - - - - - # - # - - -
!
!
!      line YCOORD2 -->   - - # - # - - # - - #-----# - - - - - - - - -
!                                               |
!                                               |
!                                               $
!                                               |
!      line YCOORD3 -->   - - - - #-------------------# - - - - - # - -
!
!      line YCOORD4 -->   - - - - - - - - # - - - - - - - # - - - - - -
!
!
!                                     XCOORD ------>
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS       
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS              
!
! This process alters input traces.
! This process outputs the same traces as it receives.
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
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
! 2         top mute index                   used but not changed
! HDR_X     X coordinate of trace location   used but not changed (MODE = GRID)
! HDR_Y     Y coordinate of trace location   used but not changed (MODE = GRID)
! HDR_FLAG  flag word                        used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                   
!
!     Date        Author     Description
!     ----        ------     -----------
! 45. 2006-09-11  B. Menger  Added delta-frequency < 1.0 for depth domain.
!044. 2006-01-10  B. Menger  Removed Unused Variables.
! 43. 2005-03-22  Ried       Made sure # of points in filter was odd
! 42. 2003-06-17  Stoeckley  Simplify code by using new linked array feature
!                             in PC_REGISTER_ARRAY_NAMES.
! 41. 2002-05-06  Vunderink  Added parallel control parameters
! 40. 2001-02-26  Stoeckley  Fix inadvertent switching of columns F3 and F4.
! 39. 2001-02-12  Stoeckley  Fix call to pc_put_options_field which was causing
!                             the GUI to be inoperative.
! 38. 2001-01-31  Stoeckley  Fixed bug which had inactivated the GUI MODE box.
! 37. 2000-12-14  Stoeckley  Re-implemented from scratch with new algorithm.
! 36. 2000-08-25  O'Brien    Fixed bug in tsvf_rsamp_tran_pts(), where
!                             transition times of the lowest XY grid
!                             coordinate were used everywhere, preventing
!                             spacially varying transition zones when
!                             MODE_SPACE==GRID and MODE_TIME==TRANSITIONS.
! 35. 2000-08-10  O'Brien    Documentation update to accomodate changes to
!                             header word naming convention... variable names
!                             made to be consistent with new convention.
!                             Missing grid values now interpolated/replicated
!                             along the X axis.
!                            Fix bug when retrieving filters for
!                             MODE_SPACE=='GRID'.
!                            Added filter parameter GUI sensitivity settings.
!                            Added messages to assist users wanting
!                             time-invariant filters.
!                            Adjusted traps to accomodate non-essential filter
!                             parameters.
! 34. 2000-06-16  O'Brien    Fix FRONTEND_UPDATE traps for filter parameters.
!                            Improve accounting for NSTORE, NSCRATCH values.
! 33. 2000-05-24  O'Brien    Implement EzGUI Layout.
!                             Adjust link list handling.
! 32. 2000-03-16  O'Brien    Force LEN_OP to be an integer number of samples
!                             long (cosmetics for front end).
! 31. 2000-03-01  O'Brien    Straightened out inquire() logic in wrapup that
!                             was causing 'Unconnected unit' error.
!                             Brought documentation closer to a current state
! 30. 2000-02-21  O'Brien    TROT_QC impelemented.
!                             Add inline interpolation/replication of filter
!                             parameters to missing grid nodes.
! 29. 2000-02-11  O'Brien    Revised array traps.
! 28. 2000-02-08  O'Brien    Put in some GUI sensitivity flags for testing
! 27. 2000-02-03  O'Brien    Implemented MODE_SPACE='GRID' options.
!                             Changed array association tests to adapt to
!                             new behavior in pc_module.
!                             PATHNAME_QC implemented.
! 26. 2000-01-14  O'Brien    Remove debugging routine fltr_filtrgs.
!                             Reinstate use fltr_module.
! 25. 2000-01-13  O'Brien    Revised traps, added GUI pull-down lists.
! 24. 1999-12-29  O'Brien    Force array sizes .ge. 0 in tsvf_trap.
! 23. 1999-12-21  O'Brien    Made tsvf_update and tsvf_initialize public
! 22. 1999-12-17  O'Brien    Full f90 conversion.
! 21. 1998-11-10  Vunderink  Begin using the f90 compiler.
! 20. 1998-05-18  Vunderink  Added OPT=6.
! 19. 1997-11-19  Vunderink  Fixed NCODE to output names for F1, F2, and
!                             TZL parameters in OPT=4 and 5 which are unique
!                             from parameters for OPT= 1, 2, and 3.
! 18. 1997-11-17  Vunderink  Fixed bug in saving filters for reuse in 
!                             OPT=4 and 5.
! 17. 1997-07-17  Vunderink  Changed assign command for temporary files.
! 16. 1997-07-16  Vunderink  Added OPT=4 and 5.
! 15. 1997-02-25  Goodger    Fix ncode to output unique names on TTIM,TZL
!                             parameters for ATB. Calculate nparm.
! 14. 1996-07-02  Goodger    Put variable IT2 in a SAVE statement.
! 13. 1989-07-19  Troutt     Swapped locations of input trace and filter
!                             output in SCRATCH to avoid addressing problems
!                             in FILTGS.  Also cleared NTZ in setup before
!                             main DCODE, fixed problem in History File
!                             with multiple TSVF's-job.
! 12. 1989-07-10  Troutt     Put END-OF-TRACE time in TTIM(nfilt) for
!                             history file.
! 11. 1989-06-21  Troutt     Added parameter HF# for flagged traces.
! 10. 1989-06-01  Troutt     Renamed program from TVF to TSVF
!                             (Began w- USER10:[CPS.FILTERS]TVF.CFT;15).
!                             Deleted code which accomodated old-old jobs
!                             (prior to linked arrays), including #FILT
!                             and IPRT parameters.
!                             Allowed user to spatially vary the times
!                             for filter application as in CONSEIS TVF:
!                             added parameters OPT, HB#, HL#, BAS, & LIN;
!                             replaced TLAST array w- TTIM array, and
!                             OVLP parameter w- TZL array.
! 9.  1988-09-23  Howard     NWIH and NWPT conversion.
! 8.  1988-08-17  Baumel     Match change in TVFBPS primitive.
! 7.  1988-07-28  Baumel     Use new DCODE-NCODE tables.
! 6.  1988-06-02  Baumel     TVF leaves mute header word alone.
! 5.  1988-04-23  Baumel     Add CPSPRT calls.
! 4.  1987-04-14  Wang       Fixed bug involving zero in mute header word.
! 3.  1987-04-09  Hanson     Added NCODE for history records.
! 2.  1987-01-16  Baumel     Added IPRT (print switch).
! 1.  1986-07-17  Baumel     Original Version. 
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
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS       
!
! Using the properties of the Fourier Transform, it is easy to calculate all 
! the possible highpass and bandpass operators from the stored lowpass 
! operators.  
!
! Prior to being stored, the lowpass operators are scaled so that the sum of 
! their coefficients is unity.  Then, if:
!
!     g(n;f1,f2) is a lowpass operator with frequencies f1 =< f2 defining 
!     the taper and n the index of the coefficients, then
!
!     delta(n) - g(n;f1,f2) is the highpass operator associated with 
!     f1 and f2, and
!
!     g(n;f3,f4) - g(n;f1,f2) is the bandpass operator associated with 
!     f1, f2, f3 and f4, where f1 =< f2 =< f3 =< f4.
!
! All operators that reject DC must sum to zero.  It is easier to meet this 
! condition by scaling the lowpass operators so that their coefficients add to 
! unity.  Then, for highpass operators, if the delta function central 
! coefficient is set to an amplitude of 1.0, this condition should be met.  
! And, for bandpass operators, no adjustment should be needed. 
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
!<NS TSVF Process/NC=80>
!     MODE=`CCCCCCCCCC      HDR_X=`I       F1 = FREQ_LOW_NONE
!     LEN_OP=`FFFFFF sec    HDR_Y=`I       F2 = FREQ_LOW_FULL
!                           HDR_FLAG=`I    F3 = FREQ_HIGH_FULL
!                                          F4 = FREQ_HIGH_NONE
!
!     COOR_X  COOR_Y  TIME  FILTER_TYPE F1   F2   F3   F4   PHASE
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!     `FFFFFFF`FFFFFFF`FFFFF`SSSSSSSSSSS`FFFF`FFFF`FFFF`FFFF`FFFF
!<PARMS COOR_X_ARRAYSET [/XST/YST]>
!<PARMS F1 [FREQ_LOW_NONE]> 
!<PARMS F2 [FREQ_LOW_FULL]>
!<PARMS F3 [FREQ_HIGH_FULL]>
!<PARMS F4 [FREQ_HIGH_NONE]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Mode describing spatial variation of the filter. </Tip>
! Default = TIME_ONLY
! Allowed = TIME_ONLY (Filter may vary in time only.)
! Allowed = MUTE      (Specified times are relative to the mute time.)
! Allowed = GRID      (Times and frequencies are specified on a 3D grid.)
! 
! If MODE = TIME_ONLY, the filter may vary in time only.  There is only one 
! set of frequencies and times.
! 
! If MODE = MUTE, same as TIME_ONLY but specified times are relative to the
! mute time, so that mute time acts as "zero."
! 
! If MODE = GRID, times and frequencies are specified on a 3D grid.
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0 
! Allowed = 0 - NWIH 
!
! If HDR_FLAG = 0, then all traces are filtered.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are filtered.   
!</Help>
!
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of convolutional operators in seconds. </Tip>
! Default = 0.4
! Allowed = real > 0.0 
!
! High frequencies and wide tapers are associated with short operators while 
! low frequencies and narrow tapers require longer operators.   A LEN_OP value
! of 0.4 sec is barely adequate for a 10 Hz center-taper frequency with 4 Hz 
! taper width.  Lower center-taper frequencies and/or narrower taper widths will
! require longer operators.  Specifying a LEN_OP value that is too small 
! truncates the operator causing undesirable overshoot and ripple in the 
! frequency domain. 
!
! This operator length is used for all filters.
! Larger values of LEN_OP will increase run time.
!</Help>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word for arbitrary X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!
! Header word for arbitrary X coordinate to use in specifying spatially varying
! filters.
!
! Active only if MODE_SPACE = GRID. 
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word for arbitrary Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!
! Header word for arbitrary Y coordinate to use in specifying spatially varying
! filters.
!
! Active only if MODE_SPACE = GRID. 
!</Help>
!
!
!<Help KEYWORD="COOR_X">
!<Tip> Array of HDR_X coordinates for spatially varying filters. </Tip>
! Default = 0.0
! Allowed = real (array)
!
! This parameter is specified separately for each filter time and location.
!
! The X and Y coordinates must lie on lines of constant Y coordinate, but
! the number or values of the X coordinates need not be the same for each Y
! coordinate.
!
! The X coordinates must be specified in ascending order.
! The Y coordinates must be specified in ascending order.
!
! The X and Y coordinates must be specified in such a way that all X
! coordinates for a given Y coordinate are grouped together.
!</Help>
!
!
!<Help KEYWORD="COOR_Y">
!<Tip> Array of HDR_Y coordinates for spatially varying filters. </Tip>
! Default = 0.0
! Allowed = real (array)
!
! This parameter is specified separately for each filter time and location.
!
! The X and Y coordinates must lie on lines of constant Y coordinate, but
! the number or values of the X coordinates need not be the same for each Y
! coordinate.
!
! The X coordinates must be specified in ascending order.
! The Y coordinates must be specified in ascending order.
!
! The X and Y coordinates must be specified in such a way that all X
! coordinates for a given Y coordinate are grouped together.
!</Help>
!
!
!<Help KEYWORD="FILTER_TYPE">
!<Tip> Type of filter to use (BANDPASS, HIGHPASS, LOWPASS or ALLPASS). </Tip>
! Default = BANDPASS
! Allowed = BANDPASS, HIGHPASS, LOWPASS or ALLPASS (array)
!
! If FILTER_TYPE = BANDPASS, the user must specify FREQ_LOW_NONE, FREQ_LOW_FULL
! FREQ_HIGH_FULL and FREQ_HIGH_NONE.
!
! If FILTER_TYPE = HIGHPASS, the user must specify FREQ_LOW_NONE, FREQ_LOW_FULL.
! FREQ_HIGH_FULL and FREQ_HIGH_NONE are set to Nyquist by TSVF.
!
! If FILTER_TYPE = LOWPASS, FREQ_LOW_NONE, FREQ_LOW_FULL are set to 0.0 by TSVF.
! FREQ_HIGH_FULL and FREQ_HIGH_NONE must be set by the user.
!
! If FILTER_TYPE = ALLPASS, TSVF sets FREQ_LOW_NONE, FREQ_LOW_FULL to 0.0 and
! FREQ_HIGH_FULL and FREQ_HIGH_NONE to Nyquist.  (The ALLPASS filter allows a 
! phase rotation without altering the amplitude spectrum.)
!
! This parameter is specified separately for each filter time and location.
!
! This filter type can be specified by simply typing the first letter of
! the type (B, H, L, or A) in either upper or lower case.
!</Help>
!
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Low frequency limit where amp spectrum diminishes to zero, in Hz. </Tip>
! Default = 0.0
! Allowed = 0.0 =< FREQ_LOW_NONE =< FREQ_LOW_FULL (array)
!
! This parameter is specified separately for each filter time and location.
!</Help>
!
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Low frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default = 0.0
! Allowed = FREQ_LOW_NONE =< FREQ_LOW_FULL =< FREQ_HIGH_FULL  (array)
!
! This parameter is specified separately for each filter time and location.
!</Help>
!
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> High frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default = Nyquist
! Allowed = FREQ_LOW_FULL =< FREQ_HIGH_FULL =< FREQ_HIGH_NONE (array)
!
! This parameter is specified separately for each filter time and location.
!</Help>
!
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> High frequency limit where amp spectrum diminishes to zero, in Hz. </Tip>
! Default = Nyquist
! Allowed = FREQ_HIGH_FULL =< FREQ_HIGH_NONE =< Nyquist (array)
!
! This parameter is specified separately for each filter time and location.
!</Help>
!
!
!<Help KEYWORD="PHASE">
!<Tip> Phase, in degrees, associated with each filter band. </Tip>
! Default = 0.0
! Allowed = real (array)
!
! This parameter is specified separately for each filter time and location.
!
! This parameter can range between -180 and +180, or between 0 and 360,
! as desired.
!</Help>
!
!
!<Help KEYWORD="TIME">
!<Tip> Control point time, in seconds, associated with each filter band. </Tip>
! Default = 0.0
! Allowed = real (array)
!
! Increasing control point times, in seconds, where each time is the time where
! the associated filter band is applied exactly as specified.  Adjacent filter
! bands grade in the time interval between their associated times.
!
! This parameter is specified separately for each filter time and location.
!
! At least one control time must be specified for each (X,Y) location, but
! the number of control times can vary from one location to another.
!
! All control times for a given (X,Y) location must be specified in ascending
! order before specifying the control times for the next (X,Y) location.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tsvf_module
      use pc_module
      use named_constants_module
      use mem_module
      use mth_module
      use terputil_module
      use linterp_module
      use tdfilter_module
      use lav_module
      implicit none
      private
      public :: tsvf_create
      public :: tsvf_initialize
      public :: tsvf_update
      public :: tsvf_delete
      public :: tsvf
      public :: tsvf_wrapup


      character(len=100),public,save :: TSVF_IDENT = &
'$Id: tsvf.f90,v 1.45 2006/09/11 13:15:52 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: tsvf_struct              
 
        private
        logical                   :: skip_wrapup         ! wrapup flag.
        integer                   :: ndpt                ! global parameter
        real                      :: tstrt               ! global parameter
        real                      :: dt                  ! global parameter

        character(len=10)         :: mode                ! process parameter
        real                      :: len_op              ! process parameter
        integer                   :: hdr_x               ! process parameter
        integer                   :: hdr_y               ! process parameter
        integer                   :: hdr_flag            ! process parameter

        integer                   :: nfilters            ! process parameter
        real             ,pointer :: coor_x        (:)   ! process parameter
        real             ,pointer :: coor_y        (:)   ! process parameter
        real             ,pointer :: time          (:)   ! process parameter
        character(len=10),pointer :: filter_type   (:)   ! process parameter
        real             ,pointer :: freq_low_none (:)   ! process parameter
        real             ,pointer :: freq_low_full (:)   ! process parameter
        real             ,pointer :: freq_high_full(:)   ! process parameter
        real             ,pointer :: freq_high_none(:)   ! process parameter
        real             ,pointer :: phase         (:)   ! process parameter

        integer    :: ncorr            ! number of points in filters.
        real       :: nyquist          ! nyquist frequency.
        logical    :: all_zero_phase   ! true if all filters are zero phase.
        integer    :: npoints          ! number of control points.

        type(linterp_struct) ,pointer :: linterp
        type(tdfilter_struct),pointer :: tdfilter

        integer,pointer :: ifirst         (:)
        integer,pointer :: ilast          (:)

        real   ,pointer :: array1_f1      (:)
        real   ,pointer :: array1_f2      (:)
        real   ,pointer :: array1_f3      (:)
        real   ,pointer :: array1_f4      (:)
        real   ,pointer :: array1_phase   (:)

        real   ,pointer :: array2_f1      (:)
        real   ,pointer :: array2_f2      (:)
        real   ,pointer :: array2_f3      (:)
        real   ,pointer :: array2_f4      (:)
        real   ,pointer :: array2_phase   (:)

        real   ,pointer :: array3_f1      (:)
        real   ,pointer :: array3_f2      (:)
        real   ,pointer :: array3_f3      (:)
        real   ,pointer :: array3_f4      (:)
        real   ,pointer :: array3_phase   (:)

        real   ,pointer :: array4_f1      (:)
        real   ,pointer :: array4_f2      (:)
        real   ,pointer :: array4_f3      (:)
        real   ,pointer :: array4_f4      (:)
        real   ,pointer :: array4_phase   (:)

        real   ,pointer :: merged_f1      (:)
        real   ,pointer :: merged_f2      (:)
        real   ,pointer :: merged_f3      (:)
        real   ,pointer :: merged_f4      (:)
        real   ,pointer :: merged_phase   (:)

      end type tsvf_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(tsvf_struct),pointer,save :: object      ! needed for traps.

      integer,parameter      :: mode_noptions        = 3
      integer,parameter      :: filter_type_noptions = 4
      integer,parameter      :: narrays              = 9

      character(len=10),save :: mode_options       (mode_noptions)
      character(len=10),save :: filter_type_options(filter_type_noptions)
      character(len=16),save :: arrays             (narrays)

      data mode_options       /'TIME_ONLY','MUTE','GRID'/
      data filter_type_options/'BANDPASS','HIGHPASS','LOWPASS','ALLPASS'/

      data arrays/'COOR_X','COOR_Y','TIME','FILTER_TYPE',            &
                  'FREQ_LOW_NONE','FREQ_LOW_FULL','FREQ_HIGH_FULL',  &
                  'FREQ_HIGH_NONE','PHASE'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tsvf_create (obj)
      implicit none
      type(tsvf_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%coor_x        )      ! process parameter
      nullify (obj%coor_y        )      ! process parameter
      nullify (obj%time          )      ! process parameter
      nullify (obj%filter_type   )      ! process parameter
      nullify (obj%freq_low_none )      ! process parameter
      nullify (obj%freq_low_full )      ! process parameter
      nullify (obj%freq_high_full)      ! process parameter
      nullify (obj%freq_high_none)      ! process parameter
      nullify (obj%phase         )      ! process parameter

      nullify (obj%linterp       )
      nullify (obj%tdfilter      )

      nullify (obj%ifirst        )
      nullify (obj%ilast         )

      nullify (obj%array1_f1     )
      nullify (obj%array1_f2     )
      nullify (obj%array1_f3     )
      nullify (obj%array1_f4     )
      nullify (obj%array1_phase  )

      nullify (obj%array2_f1     )
      nullify (obj%array2_f2     )
      nullify (obj%array2_f3     )
      nullify (obj%array2_f4     )
      nullify (obj%array2_phase  )

      nullify (obj%array3_f1     )
      nullify (obj%array3_f2     )
      nullify (obj%array3_f3     )
      nullify (obj%array3_f4     )
      nullify (obj%array3_phase  )

      nullify (obj%array4_f1     )
      nullify (obj%array4_f2     )
      nullify (obj%array4_f3     )
      nullify (obj%array4_f4     )
      nullify (obj%array4_phase  )

      nullify (obj%merged_f1     )
      nullify (obj%merged_f2     )
      nullify (obj%merged_f3     )
      nullify (obj%merged_f4     )
      nullify (obj%merged_phase  )

      call tsvf_initialize (obj)
      return
      end subroutine tsvf_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tsvf_delete (obj)
      implicit none
      type(tsvf_struct),pointer :: obj       ! arguments

      call tsvf_wrapup (obj)

      call mem_free (obj%coor_x        )      ! process parameter
      call mem_free (obj%coor_y        )      ! process parameter
      call mem_free (obj%time          )      ! process parameter
      call mem_free (obj%filter_type   )      ! process parameter
      call mem_free (obj%freq_low_none )      ! process parameter
      call mem_free (obj%freq_low_full )      ! process parameter
      call mem_free (obj%freq_high_full)      ! process parameter
      call mem_free (obj%freq_high_none)      ! process parameter
      call mem_free (obj%phase         )      ! process parameter

      call linterp_delete  (obj%linterp)
      call tdfilter_delete (obj%tdfilter)

      call mem_free (obj%ifirst        )
      call mem_free (obj%ilast         )

      call mem_free (obj%array1_f1     )
      call mem_free (obj%array1_f2     )
      call mem_free (obj%array1_f3     )
      call mem_free (obj%array1_f4     )
      call mem_free (obj%array1_phase  )

      call mem_free (obj%array2_f1     )
      call mem_free (obj%array2_f2     )
      call mem_free (obj%array2_f3     )
      call mem_free (obj%array2_f4     )
      call mem_free (obj%array2_phase  )

      call mem_free (obj%array3_f1     )
      call mem_free (obj%array3_f2     )
      call mem_free (obj%array3_f3     )
      call mem_free (obj%array3_f4     )
      call mem_free (obj%array3_phase  )

      call mem_free (obj%array4_f1     )
      call mem_free (obj%array4_f2     )
      call mem_free (obj%array4_f3     )
      call mem_free (obj%array4_f4     )
      call mem_free (obj%array4_phase  )

      call mem_free (obj%merged_f1     )
      call mem_free (obj%merged_f2     )
      call mem_free (obj%merged_f3     )
      call mem_free (obj%merged_f4     )
      call mem_free (obj%merged_phase  )

      deallocate(obj)
      return
      end subroutine tsvf_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine tsvf_initialize (obj)
      implicit none
      type(tsvf_struct),intent(inout) :: obj       ! arguments

      obj%mode      = 'TIME_ONLY'
      obj%len_op    = 0.4
      obj%hdr_x     = 7
      obj%hdr_y     = 8
      obj%hdr_flag  = 0
      obj%nfilters  = 0

      call tsvf_update (obj)
      return
      end subroutine tsvf_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine tsvf_update (obj)
      implicit none
      type(tsvf_struct),intent(inout),target :: obj             ! arguments
      integer                 :: nwih,ifilt,action ! local
      real                    :: delta_frequency,delta_phase

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_register_array_names ('coor_x_arrayset', arrays, narrays)
   
      call pc_get_global ('nwih'    , nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('dt'      , obj%dt) 
      call pc_get_global ('tstrt'   , obj%tstrt)
   
      call pc_get   ('mode'          , obj%mode    )
      call pc_get   ('len_op'        , obj%len_op  )
      call pc_get   ('hdr_x'         , obj%hdr_x   )
      call pc_get   ('hdr_y'         , obj%hdr_y   )
      call pc_get   ('hdr_flag'      , obj%hdr_flag)

      call pc_alloc ('coor_x'        , obj%coor_x        , obj%nfilters)
      call pc_alloc ('coor_y'        , obj%coor_y        , obj%nfilters)
      call pc_alloc ('time'          , obj%time          , obj%nfilters)
      call pc_alloc ('filter_type'   , obj%filter_type   , obj%nfilters)
      call pc_alloc ('freq_low_none' , obj%freq_low_none , obj%nfilters)
      call pc_alloc ('freq_low_full' , obj%freq_low_full , obj%nfilters)
      call pc_alloc ('freq_high_full', obj%freq_high_full, obj%nfilters)
      call pc_alloc ('freq_high_none', obj%freq_high_none, obj%nfilters)
      call pc_alloc ('phase'         , obj%phase         , obj%nfilters)
   

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      obj%ncorr   = 1 + nint(obj%len_op / obj%dt)
      call mth_constrain (obj%ncorr,5,obj%ndpt)
      obj%len_op  = (obj%ncorr-1)*obj%dt
      obj%nyquist = 0.5 / obj%dt

      if (obj%mode /= 'TIME_ONLY' .and. obj%mode /= 'MUTE' .and. &
          obj%mode /= 'GRID') then
           call pc_error ('illegal MODE - must be TIME_ONLY or MUTE or GRID')
      end if

      if (obj%hdr_x <= 0 .or. obj%hdr_x > nwih) then
           call pc_error ('illegal HDR_X - must be between 1 and',nwih)
      end if

      if (obj%hdr_y <= 0 .or. obj%hdr_y > nwih) then
           call pc_error ('illegal HDR_Y - must be between 1 and',nwih)
      end if

      if (obj%hdr_flag < 0 .or. obj%hdr_flag > nwih) then
           call pc_error ('illegal HDR_FLAG - must be between 0 and',nwih)
      end if

      if (obj%hdr_x == obj%hdr_y) then
           call pc_error ('HDR_X and HDR_Y cannot be the same')
      end if

      if (pc_verify_element('time',ifilt,action)) then
           if (action /= PC_REMOVE) then
                call tsvf_verify_element (obj,ifilt)
           end if
      end if

      if (obj%nfilters == 0) then
           obj%nfilters = 1
           obj%coor_x        (1) = 0.0
           obj%coor_y        (1) = 0.0
           obj%time          (1) = obj%tstrt
           obj%filter_type   (1) = 'BANDPASS'
           obj%freq_low_none (1) = 0.0
           obj%freq_low_full (1) = 0.0
           obj%freq_high_full(1) = obj%nyquist
           obj%freq_high_none(1) = obj%nyquist
           obj%phase         (1) = 0.0
      end if

      if (pc_verify_array('time')) then
           do ifilt = 1,obj%nfilters
              call tsvf_verify_element (obj,ifilt)
           end do
           call tsvf_verify_order (obj)
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field &
              ('mode'       ,mode_options       ,mode_noptions)
      call pc_put_options_array &
              ('filter_type',filter_type_options,filter_type_noptions)

      call pc_put   ('mode'           , obj%mode    )
      call pc_put   ('len_op'         , obj%len_op  )
      call pc_put   ('hdr_x'          , obj%hdr_x   )
      call pc_put   ('hdr_y'          , obj%hdr_y   )
      call pc_put   ('hdr_flag'       , obj%hdr_flag)

      call pc_put   ('coor_x'         , obj%coor_x        , obj%nfilters)
      call pc_put   ('coor_y'         , obj%coor_y        , obj%nfilters)
      call pc_put   ('time'           , obj%time          , obj%nfilters)
      call pc_put   ('filter_type'    , obj%filter_type   , obj%nfilters)
      call pc_put   ('freq_low_none'  , obj%freq_low_none , obj%nfilters)
      call pc_put   ('freq_low_full'  , obj%freq_low_full , obj%nfilters)
      call pc_put   ('freq_high_full' , obj%freq_high_full, obj%nfilters)
      call pc_put   ('freq_high_none' , obj%freq_high_none, obj%nfilters)
      call pc_put   ('phase'          , obj%phase         , obj%nfilters)

      call pc_put_minsize_arrayset ('coor_x_arrayset', 1)

      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call linterp_delete  (obj%linterp)
      call tdfilter_delete (obj%tdfilter)

      call mem_free (obj%ifirst        )
      call mem_free (obj%ilast         )

      call mem_free (obj%array1_f1     )
      call mem_free (obj%array1_f2     )
      call mem_free (obj%array1_f3     )
      call mem_free (obj%array1_f4     )
      call mem_free (obj%array1_phase  )

      call mem_free (obj%array2_f1     )
      call mem_free (obj%array2_f2     )
      call mem_free (obj%array2_f3     )
      call mem_free (obj%array2_f4     )
      call mem_free (obj%array2_phase  )

      call mem_free (obj%array3_f1     )
      call mem_free (obj%array3_f2     )
      call mem_free (obj%array3_f3     )
      call mem_free (obj%array3_f4     )
      call mem_free (obj%array3_phase  )

      call mem_free (obj%array4_f1     )
      call mem_free (obj%array4_f2     )
      call mem_free (obj%array4_f3     )
      call mem_free (obj%array4_f4     )
      call mem_free (obj%array4_phase  )

      call mem_free (obj%merged_f1     )
      call mem_free (obj%merged_f2     )
      call mem_free (obj%merged_f3     )
      call mem_free (obj%merged_f4     )
      call mem_free (obj%merged_phase  )

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call mem_alloc (obj%ifirst        , obj%npoints)
      call mem_alloc (obj%ilast         , obj%npoints)

      call mem_alloc (obj%array1_f1     , obj%ndpt)
      call mem_alloc (obj%array1_f2     , obj%ndpt)
      call mem_alloc (obj%array1_f3     , obj%ndpt)
      call mem_alloc (obj%array1_f4     , obj%ndpt)
      call mem_alloc (obj%array1_phase  , obj%ndpt)

      call mem_alloc (obj%array2_f1     , obj%ndpt)
      call mem_alloc (obj%array2_f2     , obj%ndpt)
      call mem_alloc (obj%array2_f3     , obj%ndpt)
      call mem_alloc (obj%array2_f4     , obj%ndpt)
      call mem_alloc (obj%array2_phase  , obj%ndpt)

      call mem_alloc (obj%array3_f1     , obj%ndpt)
      call mem_alloc (obj%array3_f2     , obj%ndpt)
      call mem_alloc (obj%array3_f3     , obj%ndpt)
      call mem_alloc (obj%array3_f4     , obj%ndpt)
      call mem_alloc (obj%array3_phase  , obj%ndpt)

      call mem_alloc (obj%array4_f1     , obj%ndpt)
      call mem_alloc (obj%array4_f2     , obj%ndpt)
      call mem_alloc (obj%array4_f3     , obj%ndpt)
      call mem_alloc (obj%array4_f4     , obj%ndpt)
      call mem_alloc (obj%array4_phase  , obj%ndpt)

      call mem_alloc (obj%merged_f1     , obj%ndpt)
      call mem_alloc (obj%merged_f2     , obj%ndpt)
      call mem_alloc (obj%merged_f3     , obj%ndpt)
      call mem_alloc (obj%merged_f4     , obj%ndpt)
      call mem_alloc (obj%merged_phase  , obj%ndpt)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      call tsvf_find_control_points (obj)

      ! make sure the ncorr value is odd
      if (mod(obj%ncorr,2).eq.0) then
        obj%ncorr = obj%ncorr + 1
      end if
      !----------------------------------------------------------------------!
      !--- wmm 2006/06/15 --- added the fractional frequency argument here to!
      !---                    scale properly for depth domain.               !
      !---                    This will give you no less than 251 samples    !
      !---                    in your frequency space all the time. Examples:!
      !---                    If your data is time domain                    !
      !---                    and 2 mils, then scaling is 250/250=1.0        !
      !---                        4 mil                   125/250=0.5        !
      !---                        1 mil *(see "min" fntn) 500/500=1.0        !
      !---                        30 feet (.03 dt)      16.67/250=.067       !
      !---                                                                   !
      delta_frequency = min(obj%nyquist/250.0,1.0) ! keeps us to 1 hz max.   !
      !----------------------------------------------------------------------!
      if (obj%all_zero_phase) then
        delta_phase = 0.0
      else
        delta_phase = 1.0
      endif
      call tdfilter_create (obj%tdfilter,obj%ncorr,obj%nyquist, &
                            delta_frequency, delta_phase)
      if (.not.associated(obj%tdfilter)) &
                     call pc_error ('error creating TDFILTER object')


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine tsvf_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- verify element -------------------------------!!
!!--------------------------- verify element -------------------------------!!
!!--------------------------- verify element -------------------------------!!


      subroutine tsvf_verify_element (obj,ifilt)
      implicit none
      type(tsvf_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(in)    :: ifilt                  ! arguments
      real                            :: tstop                  ! local

      if (obj%coor_x(ifilt) == FNIL) obj%coor_x(ifilt) = 0.0
      if (obj%coor_y(ifilt) == FNIL) obj%coor_y(ifilt) = 0.0
      if (obj%time  (ifilt) == FNIL) obj%time  (ifilt) = obj%tstrt

      tstop = obj%tstrt + (obj%ndpt-1)*obj%dt

      call mth_constrain (obj%time(ifilt), obj%tstrt, tstop)

      call tdfilter_constrain (obj%nyquist,               &
                               obj%filter_type   (ifilt), &
                               obj%freq_low_none (ifilt), &
                               obj%freq_low_full (ifilt), &
                               obj%freq_high_full(ifilt), &
                               obj%freq_high_none(ifilt), &
                               obj%phase         (ifilt))
      return
      end subroutine tsvf_verify_element


!!--------------------------- verify order -------------------------------!!
!!--------------------------- verify order -------------------------------!!
!!--------------------------- verify order -------------------------------!!

! Also sets obj%all_zero_phase.
! Also sets obj%npoints.


      subroutine tsvf_verify_order (obj)
      type(tsvf_struct),intent(inout) :: obj                    ! arguments
      integer                         :: ifilt                  ! local
      logical                         :: tbad,xbad,ybad,mbad    ! local

      tbad = .false.
      xbad = .false.
      ybad = .false.
      mbad = .false.
      obj%all_zero_phase = (obj%phase(1) == 0.0)
      obj%npoints = 1

      do ifilt = 2,obj%nfilters
           if (obj%coor_y(ifilt) == obj%coor_y(ifilt-1)) then
                if (obj%coor_x(ifilt) == obj%coor_x(ifilt-1)) then
                     if (obj%time(ifilt) <= obj%time(ifilt-1)) tbad = .true.
                else if (obj%coor_x(ifilt) < obj%coor_x(ifilt-1)) then
                     xbad = .true.
                else
                     obj%npoints = obj%npoints + 1
                end if
           else if (obj%coor_y(ifilt) < obj%coor_y(ifilt-1)) then
                ybad = .true.
           else
                obj%npoints = obj%npoints + 1
           end if
           if (obj%mode /= 'GRID') then
                if (obj%coor_x(ifilt) /= obj%coor_x(1)) mbad = .true.
                if (obj%coor_y(ifilt) /= obj%coor_y(1)) mbad = .true.
           end if
           if (obj%phase(ifilt) /= 0.0) obj%all_zero_phase = .false.
      end do
      if (tbad) call pc_error ('control times out of order or duplicated')
      if (xbad) call pc_error ('X coordinates out of order')
      if (ybad) call pc_error ('Y coordinates out of order')
      if (mbad) call pc_error &
                      ('multiple coordinates specified when mode is not GRID')
      return
      end subroutine tsvf_verify_order


!!------------------------- find control points --------------------------!!
!!------------------------- find control points --------------------------!!
!!------------------------- find control points --------------------------!!

! obj%npoints has been set.
! obj%ifirst and obj%ilast have been allocated.


      subroutine tsvf_find_control_points (obj)
      type(tsvf_struct),intent(inout) :: obj                    ! arguments
      integer                         :: ifilt,ipoint,indx      ! local
      real                            :: xcoords(obj%npoints)   ! local
      real                            :: ycoords(obj%npoints)   ! local
      character(len=80)               :: msg                    ! local

      ipoint = 1
      ifilt  = 1
      indx   = 1 + nint((obj%time(ifilt) - obj%tstrt) / obj%dt)
      xcoords    (ipoint) = obj%coor_x(ifilt)
      ycoords    (ipoint) = obj%coor_y(ifilt)
      obj%ifirst (ipoint) = 1
      print *, ' '
      print *, '                         control        trace    filter '
      print *, ' filter  coor_x  coor_y   point   time  sample    type  ', &
                           '     f1    f2    f3    f4    phase'
      print 1000, ifilt,obj%coor_x(ifilt),obj%coor_y(ifilt),           &
                  ipoint,obj%time(ifilt),indx,obj%filter_type(ifilt),  &
                  obj%freq_low_none (ifilt),                           &
                  obj%freq_low_full (ifilt),                           &
                  obj%freq_high_full(ifilt),                           &
                  obj%freq_high_none(ifilt),obj%phase(ifilt)

      do ifilt = 2,obj%nfilters
           if (obj%coor_y(ifilt) /= obj%coor_y(ifilt-1) .or.  &
               obj%coor_x(ifilt) /= obj%coor_x(ifilt-1)) then
                obj%ilast  (ipoint) = ifilt-1
                ipoint              = ipoint + 1
                xcoords    (ipoint) = obj%coor_x(ifilt)
                ycoords    (ipoint) = obj%coor_y(ifilt)
                obj%ifirst (ipoint) = ifilt
           end if
           indx = 1 + nint((obj%time(ifilt) - obj%tstrt) / obj%dt)
           print 1000, ifilt,obj%coor_x(ifilt),obj%coor_y(ifilt),          &
                       ipoint,obj%time(ifilt),indx,obj%filter_type(ifilt), &
                       obj%freq_low_none (ifilt),                          &
                       obj%freq_low_full (ifilt),                          &
                       obj%freq_high_full(ifilt),                          &
                       obj%freq_high_none(ifilt),obj%phase(ifilt)
      end do
      obj%ilast  (ipoint) = obj%nfilters

1000  format (1x,i5,f10.1,f8.1,i6,f10.3,i6,1x,a12,4f6.2,f8.2)
2000  format (1x,i5,f10.1,f8.1,i7,i9)

      print *, ' '
      print *, ' control                   first    last'
      print *, '  point  coor_x  coor_y    filter   filter'
      do ipoint = 1,obj%npoints
          print 2000, ipoint,xcoords(ipoint),ycoords(ipoint), &
                           obj%ifirst(ipoint),obj%ilast(ipoint)
      end do
      print *, ' '
      if (obj%all_zero_phase) then
           print *, ' phases are all zero'
      else
           print *, ' phases are not all zero'
      end if
      print *, ' '

      call linterp_create (obj%linterp,xcoords,ycoords,obj%npoints,msg)
      if (msg /= ' ') call pc_error (msg)
      return
      end subroutine tsvf_find_control_points


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine tsvf (obj,ntr,hd,tr)
      implicit none
      type(tsvf_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      integer                         :: itrace,mute,ntr2       ! local
      real                            :: xcoord,ycoord          ! local

      ntr2 = ntr
      do itrace = 1,ntr2

           if (obj%hdr_flag > 0) then
                if (hd(obj%hdr_flag,itrace) == 0.0) cycle
           end if

           if (obj%mode == 'MUTE') then
                mute = nint(hd(2,itrace))
           else
                mute = 1
           end if

           xcoord = hd(obj%hdr_x,itrace)
           ycoord = hd(obj%hdr_y,itrace)

           call tsvf_update_control_points (obj,xcoord,ycoord     , ntr)
           call tsvf_filter_trace          (obj,mute,tr(1:,itrace), ntr)

           if (ntr == FATAL_ERROR) exit

           hd(25,itrace) = lav(tr(1:,itrace),obj%ndpt)

      end do

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call tsvf_wrapup (obj)
      end if
      return
      end subroutine tsvf


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine tsvf_wrapup (obj)
      implicit none
      type(tsvf_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine tsvf_wrapup


!!----------------------- update control points ---------------------------!!
!!----------------------- update control points ---------------------------!!
!!----------------------- update control points ---------------------------!!

! Updates the four control points and the corresponding frequencies and
!  phases if the trace has moved out of the polygon defined by the current
!  control points.
! Updates the weighted merged frequencies and phases if the trace has moved
!  at all.
! Resets NTR to FATAL_ERROR if an error occurs.


      subroutine tsvf_update_control_points (obj,xcoord,ycoord,ntr)
      implicit none
      type(tsvf_struct),intent(inout) :: obj                      ! arguments
      real             ,intent(in)    :: xcoord,ycoord            ! arguments
      integer          ,intent(inout) :: ntr                      ! arguments
      integer                         :: i1,i2,i3,i4,indx         ! local
      real                            :: w1,w2,w3,w4              ! local
      logical                         :: moved,escaped            ! local

!----------get started:

      if (ntr == FATAL_ERROR) return

      call linterp_weights &
            (obj%linterp,xcoord,ycoord,i1,i2,i3,i4,w1,w2,w3,w4,moved,escaped)

!----------calculate new filter arrays at the four new control points:

      if (escaped) then
           call tsvf_get_filter_arrays (obj,i1,obj%array1_f1       ,  &
                                               obj%array1_f2       ,  &
                                               obj%array1_f3       ,  &
                                               obj%array1_f4       ,  &
                                               obj%array1_phase)

           call tsvf_get_filter_arrays (obj,i2,obj%array2_f1       ,  &
                                               obj%array2_f2       ,  &
                                               obj%array2_f3       ,  &
                                               obj%array2_f4       ,  &
                                               obj%array2_phase)

           call tsvf_get_filter_arrays (obj,i3,obj%array3_f1       ,  &
                                               obj%array3_f2       ,  &
                                               obj%array3_f3       ,  &
                                               obj%array3_f4       ,  &
                                               obj%array3_phase)

           call tsvf_get_filter_arrays (obj,i4,obj%array4_f1       ,  &
                                               obj%array4_f2       ,  &
                                               obj%array4_f3       ,  &
                                               obj%array4_f4       ,  &
                                               obj%array4_phase)
      end if

!----------merge the four filter arrays using the new weights:

      if (moved) then
           do indx = 1,obj%ndpt

                obj%merged_f1   (indx) = w1 * obj%array1_f1   (indx) +  &
                                         w2 * obj%array2_f1   (indx) +  &
                                         w3 * obj%array3_f1   (indx) +  &
                                         w4 * obj%array4_f1   (indx)

                obj%merged_f2   (indx) = w1 * obj%array1_f2   (indx) +  &
                                         w2 * obj%array2_f2   (indx) +  &
                                         w3 * obj%array3_f2   (indx) +  &
                                         w4 * obj%array4_f2   (indx)

                obj%merged_f3   (indx) = w1 * obj%array1_f3   (indx) +  &
                                         w2 * obj%array2_f3   (indx) +  &
                                         w3 * obj%array3_f3   (indx) +  &
                                         w4 * obj%array4_f3   (indx)

                obj%merged_f4   (indx) = w1 * obj%array1_f4   (indx) +  &
                                         w2 * obj%array2_f4   (indx) +  &
                                         w3 * obj%array3_f4   (indx) +  &
                                         w4 * obj%array4_f4   (indx)

                obj%merged_phase(indx) = w1 * obj%array1_phase(indx) +  &
                                         w2 * obj%array2_phase(indx) +  &
                                         w3 * obj%array3_phase(indx) +  &
                                         w4 * obj%array4_phase(indx)
           end do
      end if
      return
      end subroutine tsvf_update_control_points


!!------------------------ get filter arrays -----------------------------!!
!!------------------------ get filter arrays -----------------------------!!
!!------------------------ get filter arrays -----------------------------!!


      subroutine tsvf_get_filter_arrays (obj,ipoint,array_f1       ,  &
                                                    array_f2       ,  &
                                                    array_f3       ,  &
                                                    array_f4       ,  &
                                                    array_phase)
      implicit none
      type(tsvf_struct),intent(inout) :: obj                   ! arguments
      integer          ,intent(in)    :: ipoint                ! arguments
      real             ,intent(out)   :: array_f1    (:)       ! arguments
      real             ,intent(out)   :: array_f2    (:)       ! arguments
      real             ,intent(out)   :: array_f3    (:)       ! arguments
      real             ,intent(out)   :: array_f4    (:)       ! arguments
      real             ,intent(out)   :: array_phase (:)       ! arguments
      integer                         :: ifilt,indx            ! local

!----------initialize the filter arrays to nil:

      array_f1   (:) = FNIL
      array_f2   (:) = FNIL
      array_f3   (:) = FNIL
      array_f4   (:) = FNIL
      array_phase(:) = FNIL

!----------fill in the filter arrays for each control time:

      do ifilt = obj%ifirst(ipoint),obj%ilast(ipoint)

           indx = 1 + nint((obj%time(ifilt) - obj%tstrt) / obj%dt)

           array_f1   (indx) = obj%freq_low_none (ifilt)
           array_f2   (indx) = obj%freq_low_full (ifilt)
           array_f3   (indx) = obj%freq_high_full(ifilt)
           array_f4   (indx) = obj%freq_high_none(ifilt)
           array_phase(indx) = obj%phase         (ifilt)
      end do

!----------interpolate the filter arrays to each trace sample:

      call terputil_replace_nils (array_f1    ,obj%ndpt)
      call terputil_replace_nils (array_f2    ,obj%ndpt)
      call terputil_replace_nils (array_f3    ,obj%ndpt)
      call terputil_replace_nils (array_f4    ,obj%ndpt)
      call terputil_replace_nils (array_phase ,obj%ndpt)
      return
      end subroutine tsvf_get_filter_arrays


!!---------------------------- filter trace -------------------------------!!
!!---------------------------- filter trace -------------------------------!!
!!---------------------------- filter trace -------------------------------!!

! Resets NTR to FATAL_ERROR if an error occurs.


      subroutine tsvf_filter_trace (obj,mute,tr,ntr)
      implicit none
      type(tsvf_struct),intent(inout) :: obj                      ! arguments
      integer          ,intent(in)    :: mute                     ! arguments
      real             ,intent(inout) :: tr(:)                    ! arguments
      integer          ,intent(inout) :: ntr                      ! arguments
      real                            :: tr2(obj%ndpt)            ! local
      real                            :: filter(obj%ncorr)        ! local
      integer                         :: half,indx,indxmod        ! local
      integer                         :: indx1,indx2,indx3,offset ! local
      real                            :: f1,f2,f3,f4,phase        ! local

!----------get started:

      if (ntr == FATAL_ERROR) return

      half = obj%ncorr/2
!----------apply lowcut (highpass) filter:

      do indx = 1,obj%ndpt

           indx1     = max(indx-half,1)
           indx2     = min(indx+half,obj%ndpt)
           offset    = indx - half - 1
           indxmod   = max(indx-mute+1,1)
           f1        = obj%merged_f1(indxmod)
           f2        = obj%merged_f2(indxmod)

           call tdfilter_make_highpass_filter (obj%tdfilter,f1,f2,filter)

           tr2(indx) = 0.0
           do indx3 = indx1,indx2
                tr2(indx) = tr2(indx) + filter(indx3-offset) * tr(indx3)
           end do

      end do
      tr(1:obj%ndpt) = tr2(:)

!----------apply highcut (lowpass) filter:

      do indx = 1,obj%ndpt

           indx1     = max(indx-half,1)
           indx2     = min(indx+half,obj%ndpt)
           offset    = indx - half - 1
           indxmod   = max(indx-mute+1,1)
           f3        = obj%merged_f3(indxmod)
           f4        = obj%merged_f4(indxmod)

           call tdfilter_make_lowpass_filter (obj%tdfilter,f3,f4,filter)

           tr2(indx) = 0.0
           do indx3 = indx1,indx2
                tr2(indx) = tr2(indx) + filter(indx3-offset) * tr(indx3)
           end do

      end do
      tr(1:obj%ndpt) = tr2(:)

!----------apply phase filter:

      if (obj%all_zero_phase) return

      do indx = 1,obj%ndpt

           indx1     = max(indx-half,1)
           indx2     = min(indx+half,obj%ndpt)
           offset    = indx - half - 1
           indxmod   = max(indx-mute+1,1)
           phase     = obj%merged_phase(indxmod)

           call tdfilter_make_phase_filter (obj%tdfilter,phase,filter)

           tr2(indx) = 0.0
           do indx3 = indx1,indx2
                tr2(indx) = tr2(indx) + filter(indx3-offset) * tr(indx3)
           end do

      end do
      tr(1:obj%ndpt) = tr2(:)
      return
      end subroutine tsvf_filter_trace


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tsvf_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

