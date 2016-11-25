!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- statutil.f90 -------------------------------!!
!!---------------------------- statutil.f90 -------------------------------!!
!!---------------------------- statutil.f90 -------------------------------!!


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
!                        C P S   P R I M I T I V E                        
!
! Name       : statutil 
! Category   : math
! Written    : 1990-05-15   by: Tom Stoeckley
! Revised    : 2006-06-05   by: B. Menger
! Maturity   : production
! Purpose    : Utility routines for static data manipulation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                            
!
!    STATUTIL is a set of utility routines for manipulating static
!    information and performing functions related to statics data.
!
!    STATUTIL is called from processes such as GRAB, AER, SHFT, FISH, SISC,
!    IMS, CC3D, etc.
!
!    The STATIO primitive should be called to read or write a static file.
!
!-------------------------------------------------------------------------------
!</descript_doc>
 

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS                            
!
!   Most routines use header values passed to them as pointers into a regular
!      grid of static values. These header words are defined in the calling
!      routines and not altered by statutil routines.
!   STATGET1, STATGET2, and STATGET3 interogate words of a header array as
!      defined by the calling routines, but headers are not altered.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS                        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!  To get the correct static value from the static file, as determined
!  by the trace header words:
!
!           o                   i     i     i   i   i    i
!          AAA = STATUTIL_GET1 (HD,STATICS,NHX,NHY,NHX2,NHY2, &
!                               X1,Y1,XINC,YINC,NX,NY)   
!                               i  i   i    i   i  i
!
!           o                   i     i     i   i   i    i
!          BBB = STATUTIL_GET2 (HD,STATICS,NHX,NHY,NHX2,NHY2, &
!                               X1,Y1,XINC,YINC,NX,NY)
!                               i  i   i    i   i  i
!
!           o                   i     i     i   i   i    i
!          CCC = STATUTIL_GET3 (HD,STATICS,NHX,NHY,NHX2,NHY2, &
!                               X1,Y1,XINC,YINC,NX,NY)
!                               i  i   i    i   i  i
!
!  real              AAA     = Nearest static value.
!  real              BBB     = Interpolated static value, treating
!                               NILs as nearly zero.
!  real              CCC     = Static value (NIL if no header word match).
!  double precision  HD(*)   = Trace header word array.
!  real              STATICS = Array containing the static values.
!
!  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = Static file parameters.
!
!-------------------------------------------------------------------------------
!  To build a static file from averages of header word values:
!  First, initialize a static array and counter array to zero:
!
!                               i  i     o      o
!           CALL STATUTIL_BLD1 (NX,NY,STATICS,KOUNTS)
!
!  Then, add a static value to the appropriate element in the static
!  array, and increment the corresponding counter:
!
!                               i     i       b      b     i   i   i    i
!           CALL STATUTIL_BLD2 (HD,STATVAL,STATICS,KOUNTS,NHX,NHY,NHX2,NHY2, &
!                               X1,Y1,XINC,YINC,NX,NY)
!                               i  i   i    i   i  i
!
!  Finally, divide each element of the static array by its corresponding
!  counter, or set the array element to NIL if its counter is zero:
!
!                               i  i     b      i
!           CALL STATUTIL_BLD3 (NX,NY,STATICS,KOUNTS)
!
!  real              STATICS(NX,NY) = array containing the static values.
!  integer           KOUNTS (NX,NY) = array containing counts of values
!                                     summed into STATICS.
!  double precision  HD(*)          = trace header word array.
!  real              STATVAL        = static value to add to element of
!                                     array STATICS.
!
!  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = static file parameters.
!
!-------------------------------------------------------------------------------
!  To create a taper to apply to correlations:
!                                                   opt
!                                  i    o     i      i
!           CALL STATUTIL_TAPER (TAPER,TAPR,NCORR,lunprint)
!
!  real    TAPER       = Amount to taper the ends of the correlation.
!  real    TAPR(NCORR) = Taper to apply to correlation.
!  integer NCORR       = Number of values in a correlation (should be odd).
!  integer lunprint    = Logical unit number to print to.
!                                     ( <= 0 or missing means not to print)
!
!  The calculated taper is symmetric and linear.
!  The center of the taper (index NCORR/2 + 1) always has value 1.0.
!  The ends of the taper (indices 1 and NCORR) have value (1.0 - TAPER).
!  TAPER should be between 0.0 and 1.0.
!  If TAPER is 1.0, the correlation will be tapered down to 0.0 at the ends.
!  If TAPER is 0.5, the correlation will be tapered down to 0.5 at the ends.
!  If TAPER is 0.0, the correlation will not be tapered (all values 1.0).
!
!-------------------------------------------------------------------------------
!  To do a correlation:
!
!                                     i     i     i     o    i     o
!      CALL STATUTIL_CORR          (TRACE,RTRACE,NWIN,CORR,NCORR,DENOM)
!
!                                     i     i     i     o    i
!      CALL STATUTIL_CORR_ENHANCED (TRACE,RTRACE,NWIN,CORR,NCORR,
!                                   NORMALIZE,SUBTRACT,TAPR,CCOEF,DENOM)
!                                      i         i      i     o     o
!                                     opt       opt    opt   opt   opt
!
!  real     TRACE(NWIN) = Object trace to correlate.
!  real    RTRACE(NWIN) = Reference trace to correlate with.
!                          (It is assumed that the reference trace is the
!                           sum of many traces including the object trace.)
!  real     CORR(NCORR) = Correlation function (output).
!  real          DENOM  = Normalizing denominator (output).
!  logical   NORMALIZE  = Whether to normalize CORR (default false).
!  logical    SUBTRACT  = Whether to subtract TRACE from RTRACE (default false).
!  real     TAPR(NCORR) = Taper to apply to correlation.
!  real          CCOEF  = Correlation coefficient (output).
!
!  If the relevant optional input argument is present (or present and true):
!  The object trace is subtracted from the reference trace before correlation.
!  The correlation function is multiplied by TAPR before returning.
!  The correlation function is normalized before returning.
!
!-------------------------------------------------------------------------------
!  To pick a correlation:
!
!                                        i     i    o    o
!          CALL STATUTIL_PICK          (CORR,NCORR,SHFT,PEAK)
!
!                                        i     i     i 
!          CALL STATUTIL_PICK_ENHANCED (CORR,NCORR,NPICK,
!                                       CCMIN,DENOM,SHFT,CCOEF)
!                                         i     i    o     o  
!
!  real      CORR(NCORR) = Correlation function to pick.
!  real            SHFT  = Shift of peak (in sample units) from center of corr.
!  real            PEAK  = Value of corr at location of peak.
!  integer        NPICK  = number of points (in middle of CORR) to pick from.
!  real           CCMIN  = Minimum correlation coefficient for good pick.
!  real           DENOM  = Normalizing denominator of CORR.
!  real           CCOEF  = Correlation coefficient (output).
!
!  SHFT can have fractional values.
!  SHFT and PEAK are found from a parabola which passes through the
!    largest value and the value on each side.
!  The returned SHFT has been negated to indicate the direction
!    traces must be shifted to REMOVE the shift.
!
!  PEAK is set to zero when:
!    - the maximum value of the correlation is zero or negative.   
!    - the maximum value is at the beginning or end of the correlation.
!  CCOEF is set to zero when:
!    - the maximum value of the correlation is zero or negative.   
!    - the maximum value is at the beginning or end of the correlation.
!    - DENOM is zero or negative.
!  SHFT is set to FNIL when:
!    - the maximum value of the correlation is zero or negative.   
!    - the maximum value is at the beginning or end of the correlation.
!    - DENOM is zero or negative.
!    - CCOEF is less than CCMIN.
!
!-------------------------------------------------------------------------------
!               VARIOUS ROUTINES WHICH OPERATE ON A 2D ARRAY
!
! To grade static values over a rectangular subset of the array:
! To integrate the array:
! To replace NILs by interpolated values:
! To smooth the array                          (using running trimmed mean):
! To subtract a running average from the array (using running trimmed mean):
! To reverse the static values in the array (if xinc or yinc is negative):
!
!                           i  i     b      i     i     i     i
!  call statutil_grade     (NX,NY,STATICS,ixmin,ixmax,iymin,iymax)
!  call statutil_integrate (NX,NY,STATICS,preserve)
!                           i  i     b       i
!                                           opt
!
!                               i  i     b
!  call statutil_rep_nilx      (NX,NY,STATICS) interpolate first in X direction
!  call statutil_rep_nily      (NX,NY,STATICS) interpolate first in Y direction
!  call statutil_rep_nilx_only (NX,NY,STATICS) interpolate only in X direction
!  call statutil_rep_nily_only (NX,NY,STATICS) interpolate only in Y direction
!
!                                 i  i    b       i      i       i
!  call statutil_rep_nearby_nils (NX,NY,STATICS,ixdist,iydist,require)
!
!  call statutil_smooth
!  call statutil_runav 
!                  (NX,NY,STATICS,nxsmooth,nysmooth,endopt,trim,preserve,wild)
!                  (NX,NY,STATICS,nxsmooth,nysmooth,endopt,trim,preserve,wild)
!                   i  i     b       i        i       i     i      i      i
!                                                    opt   opt    opt    opt
!
!                         b  b   b    b   i  i     b
!  call statutil_reverse (X1,Y1,XINC,YINC,NX,NY,STATICS)
!
!                     +++++++++++++++++++++++++++++
!
! real     STATICS(NX,NY) = array from which the running average is obtained.
! integer           ixmin = minimum X index where grading begins.
! integer           ixmax = maximum X index where grading ends.
! integer           iymin = minimum Y index where grading begins.
! integer           iymax = maximum Y index where grading ends.
! integer          ixdist = number of points in + and - X direction to search.
! integer          iydist = number of points in + and - Y direction to search.
! logical         require = whether to require non-nil values on both sides.
! integer        NXSMOOTH = number of points to smooth in X direction.
! integer        NYSMOOTH = number of points to smooth in Y direction.
! logical        preserve = whether to preserve nils      (default false).
! logical            wild = whether input values are wild (default false).
!
! character(*)     endopt = 'TT' FOR TRUNCATED END RANGE (DEFAULT).
!                           'SS' FOR SHIFTED END RANGE.
!                           'EE' FOR EXTENDED END RANGE.
!                           'NN' FOR NARROWED END RANGE (GRADED ENDS).
!
! real               trim = percent to trim (default 0.0).
!                             0.0 means calculate a running average (mean).
!                           100.0 means to calculate a running median.
!                           (any percent between 0.0 and 100.0 will calculate
!                            a running trimmed mean)
!
! The STATICS array can be either 1D (dimensioned nx*ny)
!                              or 2D (dimensioned nx,ny).
!
! If PRESERVE is absent or false, the output array will not contain any nils.
! If PRESERVE is present and true, the nils in the array are preserved.
!
! If WILD is absent or false:
!  (1) The values in the input array should be reliable.
!  (2) Interpolation of nils is done before smoothing or running average.
!
! If WILD is present and true:
!  (1) The values in the input array are considered to be wild (unreliable).
!  (2) Smoothing or running average is done before interpolation of nils.
!
!                     +++++++++++++++++++++++++++++
!
! STATUTIL_GRADE:
!  (1) If any of the specified index pairs corresponds to a nil value, that
!       value is treated as if it were nearly zero.
!
! STATUTIL_INTEGRATE:
!  (1) Integrate the array.
!  (2) All nils will be preserved except the last one.
!  (3) The first non-nil value will become zero.
!  (4) The very last value (which normally is a nil) is unused and will
!       become the sum of all non-nil values in the array.
!  (5) Uses STATUTIL_1D_INTEGRATE.
!  (6) If PRESERVE is absent or false, replaces nils with interpolated values
!       after the integration is performed.
!  (7) All operations are performed in the X direction only.
!  (8) Each value of the Y index is treated separately.
!
! STATUTIL_REP_NILX:
! STATUTIL_REP_NILY:
! STATUTIL_REP_NILX_ONLY:
! STATUTIL_REP_NILY_ONLY:
!  (1) Interpolate first in one direction, then in the other direction.
!  (2) The output array will not contain any nils, unless the input array
!       is entirely nil, in which case the array is unchanged.
!  (3) Flat extrapolation is performed.
!  (4) Simply calls TERPUTIL_REPLACE_NILX or TERPUTIL_REPLACE_NILY.
!
! STATUTIL_SMOOTH:
! STATUTIL_RUNAV:
!  (1) Replace nils with interpolated values.
!  (2) Smooth the array, or remove a running average from the array.
!  (3) If WILD is absent or false, interpolation of nils is done first.
!  (4) If WILD is present and true, interpolation of nils is done last.
!  (5) If PRESERVE is present and true, put back all nils in the original array.
!  (6) Does nothing in the NX direction if NX <= one.
!  (7) Does nothing in the NY direction if NY <= one.
!  (8) Uses STATUTIL_1D_SMOOTH or STATUTIL_1D_RUNAV.
!
! STATUTIL_REVERSE:
!  (1) If XINC is negative, the first dimension in the array is reversed,
!       the sign of XINC is changed, and X1 is appropriately reset.
!  (2) If YINC is negative, the second dimension in the array is reversed,
!       the sign of YINC is changed, and Y1 is appropriately reset.
!  (3) Nothing is done if XINC and YINC are positive.
!
!-------------------------------------------------------------------------------
!          VARIOUS LOW-LEVEL ROUTINES WHICH OPERATE ON A 1D ARRAY
!
! To integrate the array:
! To replace NILs by interpolated values:
! To smooth the array                          (using running trimmed mean):
! To subtract a running average from the array (using running trimmed mean):
!
!                                      b   i
!   call statutil_1d_integrate      (ARRAY,N,                 preserve)
!   call statutil_1d_interpolate    (ARRAY,N)
!   call statutil_1d_smooth_quick   (ARRAY,N,nrun)
!   call statutil_1d_smooth_no_nils (ARRAY,N,nrun,endopt,trim)
!   call statutil_1d_runav_no_nils  (ARRAY,N,nrun,endopt,trim)
!   call statutil_1d_smooth         (ARRAY,N,nrun,endopt,trim,preserve,wild)
!   call statutil_1d_runav          (ARRAY,N,nrun,endopt,trim,preserve,wild)
!                                      b   i  i     i     i      i      i
!                                                  opt   opt    opt    opt
!
!     trimmed_mean = statutil_trimmed_mean (array, n, trim, absolute)
!           o                                 i    i   i       o
!                                                     opt     opt
!
!                     +++++++++++++++++++++++++++++
!
! real           ARRAY(N) = array from which the running average is obtained.
! integer            NRUN = NUMBER OF POINTS TO INCLUDE IN RUNNING AVERAGE.
! logical        preserve = whether to preserve nils      (default false).
! logical            wild = whether input values are wild (default false).
! real        trimed_mean = trimmed mean of the array.
! real           absolute = mean of the absolute values of the
!                            trimmed values in the array.
!
! character(*)     endopt = 'TT' FOR TRUNCATED END RANGE (DEFAULT).
!                           'SS' FOR SHIFTED END RANGE.
!                           'EE' FOR EXTENDED END RANGE.
!                           'NN' FOR NARROWED END RANGE (GRADED ENDS).
!
! real               trim = percent to trim (default 0.0).
!                             0.0 means calculate a running average (mean).
!                           100.0 means to calculate a running median.
!                           (any percent between 0.0 and 100.0 will calculate
!                            a running trimmed mean)
!
! If PRESERVE is absent or false, the output array will not contain any nils.
! If PRESERVE is present and true, the nils in the array are preserved.
!
! If WILD is absent or false:
!  (1) The values in the input array should be reliable.
!  (2) Interpolation of nils is done before smoothing or running average.
!
! If WILD is present and true:
!  (1) The values in the input array are considered to be wild (unreliable).
!  (2) Smoothing or running average is done before interpolation of nils.
!
! If ABSOLUTE is specified, it will be set to the mean of the absolute values
! of the same array elements which are used to calculate the trimmed mean.
!
!                     +++++++++++++++++++++++++++++
!
! The array can contain nil values.
!
! STATUTIL_1D_INTEGRATE:
!  (1) Integrate the array.
!  (2) All nils will be preserved except the last one.
!  (3) The first non-nil value will become zero.
!  (4) The very last value (which normally is a nil) is unused and will
!       become the sum of all non-nil values in the array.
!  (5) If PRESERVE is absent or false, replaces nils with interpolated values
!       after the integration is performed.
!  (6) If PRESERVE is absent or false, uses STATUTIL_1D_INTERPOLATE.
!
! STATUTIL_1D_INTERPOLATE:
!  (1) The nil values are replaced by interpolated and extrapolated values.
!  (2) The output array will not contain any nils.
!  (3) Flat extrapolation is performed.
!  (4) Does nothing if the input array is entirely nil.
!  (5) Simply calls TERPUTIL_REPLACE_NILS.
!
! STATUTIL_1D_SMOOTH:
! STATUTIL_1D_RUNAV:
!  (1) Smooth the array, or remove a running average from the array.
!  (2) Does nothing if N <= one.
!  (3) If WILD is absent or false, interpolation of nils is done first.
!  (4) If WILD is present and true, interpolation of nils is done last.
!  (5) If PRESERVE is present and true, put back all nils in the original array.
!  (6) Uses STATUTIL_1D_SMOOTH_NO_NILS or STATUTIL_1D_RUNAV_NO_NILS.
!  (7) Uses STATUTIL_1D_INTERPOLATE.
!
! STATUTIL_1D_SMOOTH_QUICK:
!  (1) Smooth the array using running average and end option 'TT'.
!  (2) The input array should not contain any nils.
!  (3) Does nothing if N <= one.
!  (4) Designed to be fast.
!
! STATUTIL_1D_SMOOTH_NO_NILS:
! STATUTIL_1D_RUNAV_NO_NILS:
!  (1) Smooth the array, or remove a running average from the array.
!  (2) The output array will not contain any nils.
!  (3) Does nothing if N <= one.
!  (4) If the input array contains any nil values, the nil values are
!       ignored while getting the running average.
!  (5) Uses STATUTIL_TRIMMED_MEAN.
!
! STATUTIL_1D_TRIMMED_MEAN:
!  (1) Return the trimmed mean of the input array.
!  (2) Nil values are trimmed first.
!
!-------------------------------------------------------------------------------
!                   SCAN STATIC ARRAY TO GET LIMITS AND NILS
!
!                                  i   i      o       o       o
!      call statutil_scan_statics (n,array,statmin,statmax,numnils)
!
! real     array(n) = array of static values.
! real     statmin  = minimum (non-nil) static value.
! real     statmax  = maximum (non-nil) static value.
! integer  numnils  = number of nil static values.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                              
!
!     Date        Author       Description
!     ----        ------       -----------
! 31. 2006-06-05  B. Menger    Removed Unused Variables.
! 30. 2005-01-31  Stoeckley    Add optional argument ABSOLUTE to the
!                               STATUTIL_TRIMMED_MEAN function.
! 29. 2002-09-23  Stoeckley    Fix typo introduced in revision 28.
! 28. 2002-09-09  Stoeckley    Change to use the MTH module for binning.
! 27. 2001-12-10  Stoeckley    Add routines STATUTIL_GRADE,
!                               STATUTIL_REP_NILX_ONLY, STATUTIL_REP_NILY_ONLY,
!                               STATUTIL_REP_NEARBY_NILS, STATUTIL_REVERSE,
!                               STATUTIL_SMOOTH_QUICK, and
!                               STATUTIL_SCAN_STATICS. 
! 26. 2001-03-21  Stoeckley    Add missing bracket to header_word_doc.
! 25. 2001-02-13  Stoeckley    Add the optional WILD argument.
! 24. 2000-12-14  Stoeckley    Remove interpolation code to new primitive
!                               TERPUTIL.  Therefore STATUTIL_ROOT and
!                               STATUTIL_INTERP are eliminated, and
!                               STATUTIL_REP_NILX and STATUTIL_REP_NILY
!                               now call TERPUTIL.
! 23. 2000-10-19  Stoeckley    Add missing required documentation section.
! 22. 2000-08-23  Stoeckley    Change STATUTIL_PICK to allow picks at the
!                               edges of the correlation functions.
! 21. 2000-07-20  Stoeckley    Fix bug in STATUTIL_PRIVATE_RUNAV regarding
!                               even value for NRUN.
! 20. 2000-06-23  Stoeckley    Change the smoothing and running average
!                               routines to do nothing in the direction
!                               being smoothed if the number of points
!                               is <= 1 in that direction.
! 19. 2000-06-16  Stoeckley    Add STATUTIL_RUNAV, STATUTIL_INTEGRATE,
!                               STATUTIL_PRIVATE_RUNAV, STATUTIL_TRIMMED_MEAN,
!                               STATUTIL_1D_RUNAV_NO_NILS, STATUTIL_1D_RUNAV,
!                               STATUTIL_1D_SMOOTH_NO_NILS, STATUTIL_1D_SMOOTH,
!                               and STATUTIL_1D_INTEGRATE; make some changes
!                               (and add optional arguments) to STATUTIL_CORR
!                               and STATUTIL_PICK;
!                               change some intents; remove STATUTIL_TRAN
!                               (obsolete); make slight change to
!                               STATUTIL_TAPER to correspond to previous
!                               results (note also that this routine previously
!                               did not match the documentation and now it
!                               does, reversing the value of the TAPER
!                               argument); improve STATUTIL_SMOOTH to allow
!                               nils, various end options, and running trimmed
!                               means; fix bug in STATUTIL_REP_NIL1 while
!                               changing it to STATUTIL_1D_INTERPOLATE.
! 18. 2000-05-08  Stoeckley    Fix bug in STATUTIL_BLD2.
! 17. 2000-04-07  Stoeckley    Fix several cases where keyword/argument
!                               association documentation was wrong; fix
!                               potential problem where arrays passed to
!                               some routines were assumed to have size
!                               (NX,NY) when they might actually be larger;
!                               change argument M to KOUNTS; replace ZNIL
!                               with named constant FNIL.
! 16. 2000-03-01  Stoeckley    Change name back to STATUTIL.  Also remove file
!                               I/O routines STATUTIL_OPEN, STATUTIL_OPEN2,
!                               STATUTIL_CARD, and STATUTIL_SAVE; the STATIO
!                               primitive should be used instead.  Also move
!                               this primitive to the MATH subdirectory.  Also
!                               remove calls to the parameter cache and
!                               hardwired prints to unit 6.
! 15. 1999-09-29  O'Brien      Document updates.
! 14. 1999-09-20  O'Brien      Update calls to new module fltr_module.
! 13. 1999-09-09  O'Brien      Documentation fix.
! 12. 1999-08-24  O'Brien      Changed header array type to double precision.
! 11. 1999-08-12  O'Brien      Full f90 conversion.
!                               Subroutine naming convention changed.
!                               * interface has been altered:
!                                From: statopen   ==>  To: statutil_open *
!                                From: statopen2  ==>  To: statutil_open2 *
!                                From: statget1   ==>  To: statutil_get1 *
!                                From: statget2   ==>  To: statutil_get2 *
!                                From: statget3   ==>  To: statutil_get3 *
!                                From: statcard   ==>  To: statutil_card *
!                                From: stattran   ==>  To: statutil_tran
!                                From: statsave   ==>  To: statutil_save
!                                From: statrepl   ==>  To: statutil_rep_nilx
!                                From: statrepy   ==>  To: statutil_rep_nily
!                                From: statsmoo   ==>  To: statutil_smooth *
!                                From: statbld1   ==>  To: statutil_bld1
!                                From: statbld2   ==>  To: statutil_bld2 *
!                                From: statbld3   ==>  To: statutil_bld3
!                                From: statterp   ==>  To: statutil_interp
!                                From: statroot   ==>  To: statutil_root
!                                From: statcorr   ==>  To: statutil_corr *
!                                From: stattaper  ==>  To: statutil_taper
!                                From: statpick   ==>  To: statutil_pick
! 10. 1999-01-11  Goodger      Begin using the fortran90 compiler.          
!  9. 1996-10-30  Stoeckley    Add subroutines STATTAPER,STATCORR,STATPICK.
!  8. 1994-02-11  Troutt       Add error checking for HPALLOC calls.
!  7. 1992-01-16  Stoeckley    Add subroutine entry STATREPY.
!  6. 1992-01-10  Stoeckley    Add subroutine entry STATOPEN2, and add
!                               subroutines STATTERP, STATROOT, STATTRAN,
!                               and add Y-direction interpolation in
!                               STATREPL.
!  5. 1990-11-07  Ball         Change CRD from 1000 to 2000 like NCODE.
!  4. 1990-10-23  Peterson     Include error and abort arguments on calls 
!                               to HPALLOC.
!  3. 1990-07-26  Stoeckley    Fix bug in STATGET2 for 2-D (X,Y) static
!                               files, and speed up STATGET2 for 1-D files.
!  2. 1990-05-25  Stoeckley    Add file name length flexibility to STATSAVE.
!  1. 1990-05-15  Stoeckley    Initial version.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES                             
!
!  MEMORY REQUIREMENTS
!     Storage         -  0
!     Heap (dynamic)  -  space needed for static file and comment cards
!     scratch space   -  handled by automatic arrays
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module statutil_module

      use named_constants_module
      use terputil_module
      use onesort_module
      use fltr_module
      use mth_module

      implicit none

      public
      private :: statutil_private_runav

      character(len=100),public,save :: STATUTIL_IDENT = &
       '$Id: statutil.f90,v 1.31 2006/06/05 13:19:11 Menger prod sps $'

      contains


!!--------------------------- get1 get2 get3 ------------------------------!!
!!--------------------------- get1 get2 get3 ------------------------------!!
!!--------------------------- get1 get2 get3 ------------------------------!!

 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
!     get static value from static array using nearest neighbor.
!------------------------------------------------------------------------
      real function statutil_get1 &
           (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny) 

      implicit none

      integer, intent(in) :: nhx,nhx2,nx,nhy,nhy2,ny     ! arguments
      real,    intent(in) :: x1,xinc, y1,yinc            ! arguments
      double precision, intent(in) :: hd(*)              ! arguments
      real,    intent(in) :: statics(nx,ny)              ! arguments

      integer             :: kx, ky                      ! local
!-----------------------------------------------
!     static is determined with header words (nhx,nhy) and also
!        with header words (nhx2,nhy2) if they are non-zero.
!     the nearest static in the static file is used.
!     nhx is always greater than zero.
!     nhy,nhx2,nhy2 can be zero.
!-----------------------------------------------
!----------trap nhx <= 0
      statutil_get1 = 0.0
      if (nhx <= 0) return
!----------get x index of nhx
      kx = mth_bin_number(x1,xinc,real(hd(nhx)))
      kx = min(nx,max(1,kx)) 
!----------get y index of nhy
      ky = 1 
      if (nhy > 0) then 
        ky = mth_bin_number(y1,yinc,real(hd(nhy)))
        ky = min(ny,max(1,ky)) 
      endif 
!----------accumulate the static.
      statutil_get1 = statutil_get1 + statics(kx,ky) 
!----------return if second headerset is 0
      if (nhx2 == 0) return  
!----------get x index of nhx2
      kx = mth_bin_number(x1,xinc,real(hd(nhx2)))
      kx = min(nx,max(1,kx)) 
!----------get y index of nhy2
      ky = 1 
      if (nhy2 > 0) then 
        ky = mth_bin_number(y1,yinc,real(hd(nhy2)))
        ky = min(ny,max(1,ky)) 
      endif 
!----------accumulate the static.
      statutil_get1 = statutil_get1 + statics(kx,ky) 
!----------all done.
      return  
      end function statutil_get1 


 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
!     get static value from static array with interpolation.
!------------------------------------------------------------------------
      real function statutil_get2 &
           (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny) 

      implicit none

      integer, intent(in)       :: nhx,nhx2,nx, nhy,nhy2,ny   ! arguments
      real,    intent(in)       :: x1,xinc, y1,yinc           ! arguments
      double precision, intent(in) :: hd(*)                   ! arguments
      real,    intent(in)       :: statics(nx,ny)             ! arguments

      integer                   :: kx,lx, ky,ly               !local
      real                      :: x,y, sky,sly               !local
!-----------------------------------------------
!     static is determined with header words (nhx,nhy) and also
!        with header words (nhx2,nhy2) if they are non-zero.
!     static is interpolated between values in the file.
!     nils are treated as nearly zero.
!     nhx is always greater than zero.
!     nhy,nhx2,nhy2 can be zero.
!-----------------------------------------------
!----------trap nhx <= 0
      statutil_get2 = 0.0
      if (nhx <= 0) return  
!----------get bracketing x indices of nhx
      x = (hd(nhx)-x1)/xinc + 1. 
      kx = int(x)
      lx = kx + 1 
      kx = min(nx,max(1,kx)) 
      lx = min(nx,max(1,lx)) 
!----------get bracketing y indices of nhy (and accumulate the static).
      if (nhy > 0) then 
        y = (hd(nhy)-y1)/yinc + 1. 
        ky = int(y)
        ly = ky + 1 
        ky = min(ny,max(1,ky)) 
        ly = min(ny,max(1,ly)) 
        sky = statics(kx,ky) + (x-float(kx))*(statics(lx,ky)-statics(kx,ky)) 
        sly = statics(kx,ly) + (x-float(kx))*(statics(lx,ly)-statics(kx,ly)) 
        statutil_get2 = statutil_get2 + sky + (y-float(ky))*(sly - sky) 
      else 
        statutil_get2 = statutil_get2 &
                          + statics(kx,1) &
                          + (x-float(kx))*(statics(lx,1)-statics(kx,1)) 
      endif 
!----------return if second set of headers is 0
      if (nhx2 == 0) return  
!----------get bracketing x indices of nhx2
      x = (hd(nhx2)-x1)/xinc + 1. 
      kx = int(x)
      lx = kx + 1 
      kx = min(nx,max(1,kx)) 
      lx = min(nx,max(1,lx)) 
!----------get bracketing y indices of nhy2 (and accumulate the static).
      if (nhy2 > 0) then 
        y = (hd(nhy2)-y1)/yinc + 1. 
        ky = int(y)
        ly = ky + 1 
        ky = min(ny,max(1,ky)) 
        ly = min(ny,max(1,ly)) 
        sky = statics(kx,ky) + (x-float(kx))*(statics(lx,ky)-statics(kx,ky)) 
        sly = statics(kx,ly) + (x-float(kx))*(statics(lx,ly)-statics(kx,ly)) 
        statutil_get2 = statutil_get2 + sky + (y-float(ky))*(sly - sky) 
      else 
        statutil_get2 = statutil_get2 &
                          + statics(kx,1) &
                          + (x-float(kx))*(statics(lx,1)-statics(kx,1)) 
      endif 
!----------all done.
      return  
      end function statutil_get2 

 
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
!     Get static value from static array. Honor nil values.
!------------------------------------------------------------------------
      real function statutil_get3 &
           (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny) 

      implicit none

      integer, intent(in) :: nhx,nhx2,nx, nhy,nhy2,ny     ! arguments
      real,    intent(in) :: x1,xinc, y1,yinc             ! arguments
      double precision, intent(in) :: hd(*)               ! arguments
      real,    intent(in) :: statics(nx,ny)               ! arguments

      integer             :: kx, ky                       ! local
      real                :: x,xdif, y,ydif               ! local

!-----------------------------------------------
!     static is determined with header words (nhx,nhy) and also
!        with header words (nhx2,nhy2) if they are non-zero.
!     static is extracted from file only if header word is _very_
!        close to a node in the statics(nx,ny) grid. otherwise a
!        nil is returned.
!     nhx is always greater than zero.
!     nhy,nhx2,nhy2 can be zero.
!-----------------------------------------------
!----------trap nhx <= 0
      statutil_get3 = FNIL
      if (nhx <= 0) return
!----------get x index of nhx
      x = (hd(nhx)-x1)/xinc + 1. 
      kx = nint(x) 
      kx = min(nx,max(1,kx)) 
!----------test if x is on a grid node
      xdif = abs(x-float(kx)) 
      if (xdif <= 0.00001) then 
!----------get y index of nhy
        ky = 1 
        if (nhy > 0) then 
          y = (hd(nhy)-y1)/yinc + 1. 
          ky = nint(y) 
          ky = min(ny,max(1,ky)) 
!----------test if y is on a grid node
          ydif = abs(y-float(ky)) 
          if (ydif<=0.00001) then 
            statutil_get3 = statics(kx,ky) 
          endif 
        else 
          statutil_get3 = statics(kx,ky) 
        endif
      endif 
!----------return if second set of headers is 0
      if (nhx2 == 0) return  
!----------get x index of nhx2
      x = (hd(nhx2)-x1)/xinc + 1. 
      kx = nint(x) 
      kx = min(nx,max(1,kx)) 
      xdif = abs(x-float(kx)) 
!----------get y index of nhy2
      ky = 1 
      if (nhy2 > 0) then 
        y = (hd(nhy2)-y1)/yinc + 1. 
        ky = nint(y) 
        ky = min(ny,max(1,ky)) 
        ydif = abs(y - float(ky)) 
        if (xdif<=0.00001 .and. ydif<=0.00001) then 
          if (statutil_get3 == FNIL) then 
            statutil_get3 = statics(kx,ky) 
          else 
            statutil_get3 = statutil_get3 + statics(kx,ky) 
          endif 
        endif 
      else 
        if (xdif <= 0.00001) then 
          if (statutil_get3 == FNIL) then 
            statutil_get3 = statics(kx,1) 
          else 
            statutil_get3 = statutil_get3 + statics(kx,1) 
          endif 
        endif 
      endif 
      return  
      end function statutil_get3 

 
!!----------------------- statutil grade ------------------------------!! 
!!----------------------- statutil grade ------------------------------!! 
!!----------------------- statutil grade ------------------------------!! 


      subroutine statutil_grade (nx, ny, statics, ixmin, ixmax, iymin, iymax)
      implicit none
      integer,         intent(in)    :: nx,ny                    ! arguments
      real   ,         intent(inout) :: statics(nx,ny)           ! arguments
      integer,         intent(in)    :: ixmin,ixmax,iymin,iymax  ! arguments
      real                           :: slopeya,slopeyb          ! local
      real                           :: sa,sb,slopex             ! local
      real                           :: saa,sab,sba,sbb          ! local
      integer                        :: ix,iy                    ! local

      SAA     = statics(IXmin,IYmin)
      SBA     = statics(IXmax,IYmin)
      SAB     = statics(IXmin,IYmax)
      SBB     = statics(IXmax,IYmax)
      SLOPEYA = (SAB-SAA) / max(IYmax-IYmin,1)
      SLOPEYB = (SBB-SBA) / max(IYmax-IYmin,1)
      DO IY = IYmin,IYmax
           sa     = saa + (iy-iymin) * slopeya
           sb     = sba + (iy-iymin) * slopeyb
           SLOPEX = (SB-SA) / max(IXmax-IXmin,1)
           DO IX = IXmin,IXmax
                statics(IX,IY) = SA + (IX-IXmin) * SLOPEX
           END DO
      END DO
      return
      end subroutine statutil_grade

 
!!----------------------- statutil integrate ------------------------------!! 
!!----------------------- statutil integrate ------------------------------!! 
!!----------------------- statutil integrate ------------------------------!! 


      subroutine statutil_integrate (nx, ny, statics, preserve)
      implicit none
      integer,         intent(in)    :: nx,ny                    ! arguments
      real   ,         intent(inout) :: statics(nx,ny)           ! arguments
      logical,optional,intent(in)    :: preserve                 ! arguments
      integer                        :: iy                       ! local
 
      do iy = 1,ny
           call statutil_1d_integrate (statics(1:nx,iy),nx,preserve)
      end do
      return
      end subroutine statutil_integrate

 
!!----------------------- statutil rep nilx ------------------------------!!
!!----------------------- statutil rep nilx ------------------------------!!
!!----------------------- statutil rep nilx ------------------------------!!


      subroutine statutil_rep_nilx (nx, ny, statics)
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call terputil_replace_nilx (nx,ny,statics)
      return
      end subroutine statutil_rep_nilx


!!----------------------- statutil rep nily ------------------------------!!
!!----------------------- statutil rep nily ------------------------------!!
!!----------------------- statutil rep nily ------------------------------!!
 

      subroutine statutil_rep_nily (nx, ny, statics)
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call terputil_replace_nily (nx,ny,statics)
      return
      end subroutine statutil_rep_nily


!!----------------------- statutil rep nilx only -------------------------!!
!!----------------------- statutil rep nilx only -------------------------!!
!!----------------------- statutil rep nilx only -------------------------!!


      subroutine statutil_rep_nilx_only (nx, ny, statics)
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call terputil_replace_nilx (nx,ny,statics,only=.true.)
      return
      end subroutine statutil_rep_nilx_only


!!----------------------- statutil rep nily only -------------------------!!
!!----------------------- statutil rep nily only -------------------------!!
!!----------------------- statutil rep nily only -------------------------!!
 

      subroutine statutil_rep_nily_only (nx, ny, statics)
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call terputil_replace_nily (nx,ny,statics,only=.true.)
      return
      end subroutine statutil_rep_nily_only


!!----------------------- statutil rep nearby nils -----------------------!!
!!----------------------- statutil rep nearby nils -----------------------!!
!!----------------------- statutil rep nearby nils -----------------------!!
 

      subroutine statutil_rep_nearby_nils &
                                   (nx, ny, statics, ixdist, iydist, require)
      implicit none
      integer, intent(in)    :: nx,ny                           ! arguments
      real,    intent(inout) :: statics(nx,ny)                  ! arguments
      integer, intent(in)    :: ixdist,iydist                   ! arguments
      logical, intent(in)    :: require                         ! arguments
      real                   :: temporary(nx,ny)                ! local
      real                   :: sum,weight,w,xdist,ydist        ! local
      logical                :: iyminus,iyplus,iyok             ! local
      logical                :: ixminus,ixplus,ixok             ! local
      integer                :: ix,iy,ix2,iy2,k,kount           ! local
      integer                :: ixmin,ixmax,iymin,iymax         ! local

      temporary(:,:) = statics(:,:)
      KOUNT = 0
      DO IY = 1,NY 
      DO IX = 1,NX
          if (statics(ix,iy) /= FNIL) cycle     
          iymin = max(iy - iydist,  1)
          iymax = min(iy + iydist, ny)
          ixmin = max(ix - ixdist,  1)
          ixmax = min(ix + ixdist, nx)
          k = 0
          DO IY2 = iymin,iymax
          DO IX2 = ixmin,ixmax
              if (statics(ix2,iy2) /= FNIL) k = k+1
          end do
          end do
          if (k == 0) cycle     
          if (require) then
               iyminus = .false.
               iyplus  = .false.
               ixminus = .false.
               ixplus  = .false.
               DO IY2 = iymin,iy
                   if (statics(ix,iy2) /= FNIL) iyminus = .true.
               end do
               DO IY2 = iy,iymax
                   if (statics(ix,iy2) /= FNIL) iyplus  = .true.
               end do
               DO Ix2 = ixmin,ix
                   if (statics(ix2,iy) /= FNIL) ixminus = .true.
               end do
               DO Ix2 = ix,ixmax
                   if (statics(ix2,iy) /= FNIL) ixplus  = .true.
               end do
               iyok = (iyminus .and. iyplus)
               ixok = (ixminus .and. ixplus)
               if(.not.iyok .and. .not.ixok) cycle     
          end if
          sum    = 0.0
          weight = 0.0
          DO IY2 = iymin,iymax
          DO IX2 = ixmin,ixmax
              if (statics(ix2,iy2) /= FNIL) then
                   xdist  = ix2 - ix
                   ydist  = iy2 - iy
                   w      = 1.0 / sqrt(xdist*xdist+ydist*ydist)
                   weight = weight + w
                   sum    = sum + w * statics(ix2,iy2)
              end if
          end do
          end do
          if (weight > 0.0) then
           temporary(ix,iy) = sum / weight
           kount = kount + 1
          end if
      END DO
      END DO
      statics(:,:) = temporary(:,:)
      return
      end subroutine statutil_rep_nearby_nils


!!----------------------- statutil runav ----------------------------------!! 
!!----------------------- statutil runav ----------------------------------!! 
!!----------------------- statutil runav ----------------------------------!! 


      subroutine statutil_runav &
                    (nx,ny,statics,nxsmooth,nysmooth,endopt,trim,preserve,wild)
      implicit none
      integer         ,         intent(in)    :: nx,ny             ! arguments
      real            ,         intent(inout) :: statics(nx,ny)    ! arguments
      integer         ,         intent(in)    :: nxsmooth,nysmooth ! arguments
      character(len=*),optional,intent(in)    :: endopt            ! arguments
      real            ,optional,intent(in)    :: trim              ! arguments
      logical         ,optional,intent(in)    :: preserve,wild     ! arguments
      integer                                 :: ix,iy             ! local
 
      do iy = 1,ny
           call statutil_1d_runav &
                     (statics(1:nx,iy),nx,nxsmooth,endopt,trim,preserve,wild)
      end do

      do ix = 1,nx
           call statutil_1d_runav &
                     (statics(ix,1:ny),ny,nysmooth,endopt,trim,preserve,wild)
      end do
      return
      end subroutine statutil_runav

 
!!----------------------- statutil smooth ----------------------------------!! 
!!----------------------- statutil smooth ----------------------------------!! 
!!----------------------- statutil smooth ----------------------------------!! 


      subroutine statutil_smooth &
                    (nx,ny,statics,nxsmooth,nysmooth,endopt,trim,preserve,wild)
      implicit none
      integer         ,         intent(in)    :: nx,ny             ! arguments
      real            ,         intent(inout) :: statics(nx,ny)    ! arguments
      integer         ,         intent(in)    :: nxsmooth,nysmooth ! arguments
      character(len=*),optional,intent(in)    :: endopt            ! arguments
      real            ,optional,intent(in)    :: trim              ! arguments
      logical         ,optional,intent(in)    :: preserve,wild     ! arguments
      integer                                 :: ix,iy             ! local
 
      do iy = 1,ny
           call statutil_1d_smooth &
                     (statics(1:nx,iy),nx,nxsmooth,endopt,trim,preserve,wild)
      end do

      do ix = 1,nx
           call statutil_1d_smooth &
                     (statics(ix,1:ny),ny,nysmooth,endopt,trim,preserve,wild)
      end do
      return
      end subroutine statutil_smooth
 

!!----------------------- statutil reverse ---------------------------------!! 
!!----------------------- statutil reverse ---------------------------------!! 
!!----------------------- statutil reverse ---------------------------------!! 


      subroutine statutil_reverse (x1,y1,xinc,yinc,nx,ny,statics)
      implicit none
      real            ,         intent(inout) :: x1,y1,xinc,yinc   ! arguments
      integer         ,         intent(in)    :: nx,ny             ! arguments
      real            ,         intent(inout) :: statics(nx,ny)    ! arguments
      integer                                 :: ix,iy             ! local
      integer                                 :: nx2,ix2,ny2,iy2   ! local
      real                                    :: sss               ! local

!----------REVERSE THE FILE (IN X DIRECTION) IF NECESSARY.

      if (xinc < 0.0) then
           IF (NX > 1) THEN
                NX2 = (NX+1) / 2
                DO IY = 1,NY
                DO IX = 1,NX2
                     IX2             = NX - IX + 1
                     SSS             = statics(IX ,IY)
                     statics(IX ,IY) = statics(IX2,IY)
                     statics(IX2,IY) = SSS
                end do
                end do
                X1 = X1 + (NX-1) * XINC
           END IF
           xinc = -xinc
      end if

!----------REVERSE THE FILE (IN Y DIRECTION) IF NECESSARY.

      if (yinc < 0.0) then
           IF (NY > 1) THEN
                NY2 = (NY+1) / 2
                DO IX = 1,NX
                DO IY = 1,NY2
                     IY2             = NY - IY + 1
                     SSS             = statics(IX,IY)
                     statics(IX,IY ) = statics(IX,IY2)
                     statics(IX,IY2) = SSS
                end do
                end do
                Y1 = Y1 + (NY-1) * YINC
           END IF
           yinc = -yinc
      end if
      return
      end subroutine statutil_reverse
 

!!------------------------- bld1 bld2 bld3 ---------------------------------!!
!!------------------------- bld1 bld2 bld3 ---------------------------------!!
!!------------------------- bld1 bld2 bld3 ---------------------------------!!


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
!     static file initialization routine.
!     part of trio statbld1,statbld2,statbld3.
!------------------------------------------------------------------------
      subroutine statutil_bld1 (nx, ny, statics, kounts) 

      implicit none

      integer, intent(in)    :: nx,ny          ! arguments
      real,    intent(inout) :: statics(:,:)   ! arguments
      integer, intent(inout) :: kounts (:,:)   ! arguments

      statics(1:nx,1:ny) = 0.0
      kounts (1:nx,1:ny) = 0
      return
      end subroutine statutil_bld1

 
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
!     static file finalization routine.
!     part of trio statbld1,statbld2,statbld3.
!------------------------------------------------------------------------
      subroutine statutil_bld3 (nx, ny, statics, kounts) 

      implicit none

      integer, intent(in)    :: nx,ny          ! arguments
      real,    intent(inout) :: statics(nx,ny) ! arguments
      integer, intent(in)    :: kounts (nx,ny) ! arguments

      where (kounts(1:nx,1:ny) /= 0)
        statics(1:nx,1:ny) = statics(1:nx,1:ny) / kounts(1:nx,1:ny)
      elsewhere
        statics(1:nx,1:ny) = FNIL
      endwhere
      return
      end subroutine statutil_bld3

 
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
!     static file update routine.
!     part of trio statbld1,statbld2,statbld3.
!------------------------------------------------------------------------
      subroutine statutil_bld2 &
   (hd, statval, statics, kounts, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny) 

      implicit none

      integer,         intent(in)    :: nhx,nhx2,nx, nhy,nhy2,ny   ! arguments
      real,            intent(in)    :: x1,xinc, y1,yinc, statval  ! arguments
      double precision,intent(in)    :: hd(*)                      ! arguments
      real,            intent(inout) :: statics(nx,ny)             ! arguments
      integer,         intent(inout) :: kounts (nx,ny)             ! arguments

      integer                        :: kx, ky                     ! local
!-----------------------------------------------
!     static ground position is determined from header words (nhx,nhy)
!        and also from header words (nhx2,nhy2).
!     traces outside the range of the static file are not used.
!     nhx is always greater than zero.
!     nhy,nhx2,nhy2 can be zero.
!-----------------------------------------------
!----------get x index of nhx
      kx = mth_bin_number(x1,xinc,real(hd(nhx)))
!----------get y index of nhy
      ky = 1 
      if (nhy > 0) then 
        ky = mth_bin_number(y1,yinc,real(hd(nhy)))
      endif 
!----------update the static and counter arrays.
      if (kx >= 1 .and. kx <= nx .and. ky >= 1 .and. ky <= ny) then
        statics(kx,ky) = statics(kx,ky) + statval 
        kounts (kx,ky) = kounts (kx,ky) + 1 
      endif 
!----------when nhx2 points to header word 0, we're done.
      if (nhx2 == 0) return  
!----------get x index of nhx2
      kx = mth_bin_number(x1,xinc,real(hd(nhx2)))
      if (kx<1 .or. kx>nx) return  
!----------get y index of nhy2
      ky = 1 
      if (nhy2 > 0) then 
        ky = mth_bin_number(y1,yinc,real(hd(nhy2)))
        if (ky<1 .or. ky>ny) return  
      endif 
!----------update the static and counter arrays.
      statics(kx,ky) = statics(kx,ky) + statval 
      kounts (kx,ky) = kounts (kx,ky) + 1 
!----------all done.
      return  
      end subroutine statutil_bld2 


!!------------------------- subroutines ----------------------------------!! 
!!------------------------- subroutines ----------------------------------!! 
!!------------------------- subroutines ----------------------------------!! 
 
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!------------------------------------------------------------------------
! fill an array with taper weights
!------------------------------------------------------------------------
      subroutine statutil_taper (taper, tapr, ncorr, lunprint) 
      implicit none
      integer,         intent(in)    :: ncorr                ! arguments
      real,            intent(in)    :: taper                ! arguments
      real,            intent(inout) :: tapr(ncorr)          ! arguments
      integer,optional,intent(in)    :: lunprint             ! arguments
      integer                        :: indx,icenter         ! local
      real                           :: value                ! local

!     integer                        :: jcenter, jj          ! local
!     real                           :: slope, bias          ! local
 
!-----------------------------------------------
!  the function is symmetric and linear with 1.0 at the central value.
!  tapr() = 1.0-taper at the endpoints
!-----------------------------------------------
!     jcenter = ncorr/2 + 1 
!     slope   = taper/jcenter
!     bias    = 1.0-taper
!     tapr(1:jcenter) = slope * (/(jj, jj = 0,jcenter-1)/) + bias
!     tapr(jcenter+1:ncorr) = tapr(jcenter-1:1:-1)

      icenter       = ncorr/2 + 1
      tapr(icenter) = 1.0
      do indx = 1,ncorr/2
           value              = 1.0 - (2.0*(taper/ncorr)*(icenter-indx))
           tapr(indx)         = value
           tapr(ncorr-indx+1) = value
      end do

      if (present(lunprint)) then
           if (lunprint > 0) then
                write (lunprint,*) ' ' 
                write (lunprint,*) 'STATUTIL_TAPER:' 
                write (lunprint,*) 'Correlation functions are tapered &
                            &by multiplying by the following array:' 
                write (lunprint,'(5X,10F7.3)') (tapr(indx),indx=1,ncorr) 
                write (lunprint,*) ' ' 
           end if
      end if
      return  
      end subroutine statutil_taper 


!!--------------------------- statutil corr -------------------------------!! 
!!--------------------------- statutil corr -------------------------------!! 
!!--------------------------- statutil corr -------------------------------!! 


      subroutine statutil_corr (trace,rtrace,nwin,corr,ncorr,denom) 
      implicit none
      real   ,          intent(in)  :: trace(:),rtrace(:)     ! arguments
      integer,          intent(in)  :: nwin,ncorr             ! arguments
      real   ,          intent(out) :: corr(:)                ! arguments
      real   ,          intent(out) :: denom                  ! arguments
      integer                       :: nhalf                  ! local
      real                          :: scratch(nwin+ncorr)    ! local
      real                          :: auto1(1),auto2(1)      ! local

!----------get started.

      nhalf   = (ncorr-1)/2
      scratch = 0.0
      scratch(1+nhalf:nwin+nhalf) = rtrace(1:nwin)

!----------do the correlation.

      call fltr_filterg (trace, nwin, scratch, nwin+ncorr-1, corr)

!----------get the zero-lag (unnormalized) autocorrelations.

      call fltr_filterg (trace           , nwin, trace           , nwin, auto1)
      call fltr_filterg (scratch(nhalf+1), nwin, scratch(nhalf+1), nwin, auto2)

!----------get the normalizing denominator.

        denom = sqrt(auto1(1)*auto2(1))
      return
      end subroutine statutil_corr


!!!!!!!!!! alternate code:
!!!!!!!!!! alternate code:
!!!!!!!!!! alternate code:

      subroutine statutil_corr_alternate (trace,rtrace,nwin,corr,ncorr,denom) 
      implicit none
      integer ,intent(in)    :: nwin,ncorr                    ! arguments
      real    ,intent(in)    :: trace(nwin)                   ! arguments
      real    ,intent(in)    :: rtrace(nwin)                  ! arguments
      real    ,intent(out)   :: corr(ncorr)                   ! arguments
      real    ,intent(out)   :: denom                         ! arguments
      integer                :: nzero,j,ishft,i1,i2 ! local
      real                   :: aa,bb                         ! local

!----------DO THE CORRELATION.

      nzero = 1 + ncorr/2.0
      do j = 1, ncorr
           corr(j) = 0.0
           ishft = nzero - j
           i1 = max(1,1 + ishft)
           i2 = min(nwin,nwin + ishft)
           corr(j) = corr(j) + sum(trace(i1-ishft:i2-ishft)*rtrace(i1:i2))
      end do

!----------GET THE NORMALIZING DENOMINATOR.

      aa = 0.0
      bb = 0.0
      aa = sum(trace**2)
      bb = sum(rtrace**2)
      denom = sqrt(aa*bb)
      return
      end subroutine statutil_corr_alternate


!!------------------------- statutil corr enhanced -----------------------!!
!!------------------------- statutil corr enhanced -----------------------!!
!!------------------------- statutil corr enhanced -----------------------!!


      subroutine statutil_corr_enhanced &
           (trace,rtrace,nwin,corr,ncorr,normalize,subtract,tapr,ccoef,denom) 
      implicit none
      real   ,          intent(in) :: trace(:),rtrace(:)     ! arguments
      integer,          intent(in) :: nwin,ncorr             ! arguments
      real   ,          intent(out):: corr(:)                ! arguments
      logical, optional,intent(in) :: normalize,subtract     ! arguments
      real   , optional,intent(in) :: tapr(:)                ! arguments
      real   , optional,intent(out):: ccoef,denom            ! arguments
      real                         :: denom2                 ! local
      real                         :: scratch(nwin)          ! local

!  remove the object trace from the reference trace before correlation.

      if (.not.present(subtract)) then
           scratch(:) = rtrace(1:nwin)
      else if (.not.subtract) then
           scratch(:) = rtrace(1:nwin)
      else
           scratch(:) = rtrace(1:nwin) - trace(1:nwin) 
      end if
 
!  do the correlation.

      call statutil_corr (trace,scratch,nwin,corr,ncorr,denom2) 

!  get the normalizing denominator (if DENOM is present).

      if (present(denom)) then
           denom = denom2
      end if

!  get the correlation coefficient (if CCOEF is present).

      if (present(ccoef)) then
           ccoef = 0.0
           if (denom2 > 0.0) ccoef = corr(1+ncorr/2)/denom2
      end if

!  normalize the correlation function (if NORMALIZE is present).

      if (present(normalize)) then
           corr(1:ncorr) = corr(1:ncorr)/denom2 
      endif 

!  apply a linear taper to bias picking to small lags (if TAPR is present).

      if (present(tapr)) then
           corr(1:ncorr) = corr(1:ncorr)*tapr(1:ncorr)
      end if
      return  
      end subroutine statutil_corr_enhanced

 
!!-------------------------- statutil pick --------------------------------!!
!!-------------------------- statutil pick --------------------------------!!
!!-------------------------- statutil pick --------------------------------!!


      subroutine statutil_pick (corr, ncorr, shft, peak) 
      implicit none
      integer,intent(in)  :: ncorr                  ! arguments
      real   ,intent(in)  :: corr(ncorr)            ! arguments
      real   ,intent(out) :: shft, peak             ! arguments
      integer             :: imax(1),jmax           ! local
      real                :: a, b, xmin             ! local

! This monkey business with imax and jmax is needed
! because maxloc() returns an array...
! We can't simply assign arrays to scalars in f90.

!     imax = maxloc(corr) 
!     jmax = imax(1)
!     if (jmax <= 1 .or. jmax >= ncorr) then 
!       shft = FNIL
!       peak = 0.0
!     else if (corr(jmax) <= 0.0) then 
!       shft = FNIL
!       peak = 0.0
!     else 
!       shft = ncorr/2 + 1 - jmax
!       peak = corr(jmax)
!       a = 2.0*peak - corr(jmax+1) - corr(jmax-1)
!       if (a > 0.0) then
!            b = corr(jmax-1) - corr(jmax+1)
!            xmin = -0.5*b/a
!            if (abs(xmin) <= 1.0) then
!              shft = shft - xmin
!              peak = peak - 0.5*(xmin*(a*xmin + b))
!            end if
!       end if
!       shft = -shft                 ! convert shift to "correction shift" 
!     endif
!     return
!     end subroutine statutil_pick 
 
 
      imax = maxloc(corr) 
      jmax = imax(1)
      if (jmax < 1 .or. jmax > ncorr) then 
        shft = FNIL
        peak = 0.0
      else if (corr(jmax) <= 0.0) then 
        shft = FNIL
        peak = 0.0
      else 
        shft = ncorr/2 + 1 - jmax
        peak = corr(jmax)
        if (jmax > 1 .and. jmax < ncorr) then
             a = 2.0*peak - corr(jmax+1) - corr(jmax-1)
             if (a > 0.0) then
                  b = corr(jmax-1) - corr(jmax+1)
                  xmin = -0.5*b/a
                  if (abs(xmin) <= 1.0) then
                    shft = shft - xmin
                    peak = peak - 0.5*(xmin*(a*xmin + b))
                  end if
             end if
        end if
        shft = -shft                 ! convert shift to "correction shift" 
      endif
      return
      end subroutine statutil_pick 
 
 
!!----------------------- statutil pick enhanced ---------------------------!!
!!----------------------- statutil pick enhanced ---------------------------!!
!!----------------------- statutil pick enhanced ---------------------------!!


      subroutine statutil_pick_enhanced &
                      (corr, ncorr, npick, ccmin, denom, shft, ccoef) 
      implicit none
      integer         ,intent(in)  :: ncorr,npick            ! arguments
      real            ,intent(in)  :: corr(ncorr)            ! arguments
      real            ,intent(in)  :: ccmin,denom            ! arguments
      real            ,intent(out) :: shft, ccoef            ! arguments
      integer                      :: na,nb                  ! local
      real                         :: peak                   ! local

      if (denom > 0.0) then
           na = 1     + (ncorr-npick)/2
           nb = ncorr - (ncorr-npick)/2
           call statutil_pick (corr(na:nb), npick, shft, peak) 
           ccoef = peak/denom    ! might be zero.
           if (ccoef < ccmin) shft = FNIL
      else
           shft = FNIL
           ccoef = 0.0
      end if
      return
      end subroutine statutil_pick_enhanced 
 
 
!!------------------------- statutil 1d integrate --------------------------!!
!!------------------------- statutil 1d integrate --------------------------!!
!!------------------------- statutil 1d integrate --------------------------!!


      subroutine statutil_1d_integrate (array, n, preserve)
      implicit none
      real            ,intent(inout) :: array(:)            ! arguments
      integer         ,intent(in)    :: n                   ! arguments
      logical,optional,intent(in)    :: preserve            ! arguments
      logical                        :: preserve2           ! local
      integer                        :: i                   ! local
      real                           :: sum,value           ! local
 
      sum = 0.0
      do i = 1,n
           if (i == n) then
                array(i) = sum
           else if (array(i) /= FNIL) then
                value = array(i)
                array(i) = sum
                sum = sum + value
           end if
      end do

      if (present(preserve)) then
           preserve2 = preserve
      else
           preserve2 = .false.
      end if

      if (.not.preserve2) then
           call statutil_1d_interpolate (array,n)
      end if
      return
      end subroutine statutil_1d_integrate

 
!!------------------------- statutil 1d interpolate ------------------------!!
!!------------------------- statutil 1d interpolate ------------------------!!
!!------------------------- statutil 1d interpolate ------------------------!!


      subroutine statutil_1d_interpolate (array, n) 
      implicit none
      real,    intent(inout)   :: array(:)            ! arguments
      integer, intent(in)      :: n                   ! arguments

      call terputil_replace_nils (array,n)
      return
      end subroutine statutil_1d_interpolate

 
 
!!-------------------- statutil private runav ----------------------------!!
!!-------------------- statutil private runav ----------------------------!!
!!-------------------- statutil private runav ----------------------------!!

! The array may contain nils.
! The scratch array is dimensioned (nrun+1) to allow for even value of nrun.


      function statutil_private_runav &
                           (I,array,N,nrun,endopt,trim) result (runav)
      implicit none
      integer                  ,intent(in) :: i,n,nrun           ! arguments
      real                     ,intent(in) :: array(:)           ! arguments
      character(len=*),optional,intent(in) :: endopt             ! arguments
      real            ,optional,intent(in) :: trim               ! arguments
      real                                 :: runav              ! result
      real                                 :: scratch(nrun+1)    ! local
      integer                              :: nhalf,ja,jb,ksum   ! local
      integer                              :: j,jj               ! local
      character(len=8)                     :: endopt2            ! local

!----------GET PRELIMINARY VALUE OF RUNNING AVERAGE.

      if (n <= 0) then
           runav = 0.0
           return
      end if

      if (array(i) == FNIL) then
           runav = 0.0
      else
           runav = array(i)
      end if

      if (present(endopt)) then
           endopt2 = endopt
      else
           endopt2 = 'TT'
      end if

!----------GET NUMBER OF POINTS IN ONE SIDE OF RUNNING AVERAGE.

      nhalf  = nrun/2
      IF (nhalf <= 0) RETURN
      IF (endopt2 == 'NN') nhalf = MIN(nhalf,N-I,I-1)

!----------GET LOWER AND UPPER POINT LIMITS FOR THE AVERAGE.

      if (endopt2 == 'SS') then
           JA = MAX(1,MIN(I-nhalf,N-2*nhalf))
           JB = MIN(N,MAX(I+nhalf,1+2*nhalf))
      else if (endopt2 == 'EE') then
           JA = I - nhalf
           JB = I + nhalf
      else                      ! endopt2 == 'TT' or 'NN'
           JA=MAX(I-nhalf,1)
           JB=MIN(I+nhalf,N)
      end if

!----------LOAD UP THE ARRAY TO AVERAGE.

      KSUM = 0
      DO J = JA,JB
           IF (endopt2 == 'EE') then
                JJ = MIN(N,MAX(1,J))
           else
                JJ = J
           end if
           IF (array(JJ) == FNIL) cycle
           KSUM = KSUM + 1
           scratch(KSUM) = array(JJ)
      end do
      IF (KSUM == 0) return

!----------USE ONLY HALF OF EACH END POINT FOR EVEN NPOINT.

      IF (nrun == 2*nhalf .AND. KSUM >= 2) THEN
           scratch(1) = (scratch(1)+scratch(KSUM))/2.
           KSUM = KSUM-1
      END IF

!----------GET THE AVERAGE OR TRIMMED MEAN VALUE.

      RUNAV = statutil_trimmed_mean(scratch,ksum,trim)
      return
      end function statutil_private_runav


!!---------------------- statutil trimmed mean ---------------------------!!
!!---------------------- statutil trimmed mean ---------------------------!!
!!---------------------- statutil trimmed mean ---------------------------!!

! the array may contain nils.
! nils are removed first while trimming.


      function statutil_trimmed_mean (array,n,trim, &
                                      absolute) result (trimmed_mean)
      implicit none
      real            ,intent(in)  :: array(:)                 ! arguments
      integer         ,intent(in)  :: n                        ! arguments
      real   ,optional,intent(in)  :: trim                     ! arguments
      real   ,optional,intent(out) :: absolute                 ! arguments
      real                         :: trimmed_mean             ! result
      real                         :: scratch(n)               ! local
      integer                      :: ntrim,ia,ib,i,kount      ! local
      real                         :: sum,trim_fraction        ! local

      kount = 0
      do i = 1,n
           if (array(i) /= FNIL) then
                kount = kount + 1
                scratch(kount) = array(i)
           end if
      end do

      if (kount == 0) then
           trimmed_mean = 0.0
           return
      end if

      if (present(trim)) then
            trim_fraction = 0.01 * trim
      else
            trim_fraction = 0.0
      end if

      ntrim = nint(0.5 * (n * trim_fraction - (n-kount)))
                                             ! ^^^^^^^ already trimmed
      ntrim = MIN((kount-1)/2,MAX(0,ntrim))
      IA  = 1     + ntrim
      IB  = kount - ntrim
      IF (ntrim > 0) call onesort_sort1 (scratch,kount)
      SUM = 0.0
      do I = IA,IB
           SUM = SUM + scratch(I)
      end do
      trimmed_mean = SUM / (IB-IA+1)

      if (present(absolute)) then
           absolute = 0.0
           do I = IA,IB
                absolute = absolute + abs(scratch(I))
           end do
           absolute = absolute / (IB-IA+1)
      end if
      return
      end function statutil_trimmed_mean


!!-------------------- statutil 1d smooth quick ---------------------------!!
!!-------------------- statutil 1d smooth quick ---------------------------!!
!!-------------------- statutil 1d smooth quick ---------------------------!!


      subroutine statutil_1d_smooth_quick (array,n,nrun) 
      implicit none
      real                     ,intent(inout) :: array(:)         ! arguments
      integer                  ,intent(in)    :: n,nrun           ! arguments
      real                                    :: integrated(n+1)  ! local
      integer                                 :: i,nhalf,ia,ib    ! local

      nhalf         = nrun/2
      integrated(1) = 0.0
      do i = 1,n
           integrated(i+1) = integrated(i) + array(i)
      end do

      do i = 1,n
           ia = max(i-nhalf,1)
           ib = min(i+nhalf,n) + 1
           array(i) = (integrated(ib) - integrated(ia)) / (ib-ia)
      end do
      return
      end subroutine statutil_1d_smooth_quick


!!-------------------- statutil 1d smooth no nils ---------------------------!!
!!-------------------- statutil 1d smooth no nils ---------------------------!!
!!-------------------- statutil 1d smooth no nils ---------------------------!!


      subroutine statutil_1d_smooth_no_nils (array,n,nrun,endopt,trim) 
      implicit none
      real                     ,intent(inout) :: array(:)         ! arguments
      integer                  ,intent(in)    :: n,nrun           ! arguments
      character(len=*),optional,intent(in)    :: endopt           ! arguments
      real            ,optional,intent(in)    :: trim             ! arguments
      real                                    :: smoothed(n)      ! local
      integer                                 :: i                ! local

      if (n <= 1) return
      do i = 1, n
           smoothed(i) = statutil_private_runav(i,array,n,nrun,endopt,trim)
      enddo

      array(1:n) = smoothed(1:n)
      return
      end subroutine statutil_1d_smooth_no_nils

 
!!----------------------- statutil 1d runav no nils -------------------------!!
!!----------------------- statutil 1d runav no nils -------------------------!!
!!----------------------- statutil 1d runav no nils -------------------------!!


      subroutine statutil_1d_runav_no_nils (array,n,nrun,endopt,trim) 
      implicit none
      real                     ,intent(inout) :: array(:)         ! arguments
      integer                  ,intent(in)    :: n,nrun           ! arguments
      character(len=*),optional,intent(in)    :: endopt           ! arguments
      real            ,optional,intent(in)    :: trim             ! arguments
      real                                    :: smoothed(n)      ! local
      integer                                 :: i                ! local

      if (n <= 1) return
      do i = 1, n
           smoothed(i) = statutil_private_runav(i,array,n,nrun,endopt,trim)
      enddo

      array(1:n) = array(1:n) - smoothed(1:n)
      return
      end subroutine statutil_1d_runav_no_nils

 
!!------------------------ statutil 1d smooth ------------------------------!!
!!------------------------ statutil 1d smooth ------------------------------!!
!!------------------------ statutil 1d smooth ------------------------------!!


      subroutine statutil_1d_smooth (array,n,nrun,endopt,trim,preserve,wild) 
      implicit none
      real                     ,intent(inout) :: array(:)        ! arguments
      integer                  ,intent(in)    :: n,nrun          ! arguments
      character(len=*),optional,intent(in)    :: endopt          ! arguments
      real            ,optional,intent(in)    :: trim            ! arguments
      logical         ,optional,intent(in)    :: preserve,wild   ! arguments
      logical                                 :: preserve2,wild2 ! local
      real                                    :: scratch(n)      ! local

      if (n <= 1) return
      scratch(1:n) = array(1:n)

      if (present(wild)) then
           wild2 = wild
      else
           wild2 = .false.
      end if

      if (wild2) then
           call statutil_1d_smooth_no_nils (scratch,n,nrun,endopt,trim)
           where (array(1:n) == FNIL) scratch(1:n) = FNIL
           call statutil_1d_interpolate (scratch,n)
           if (scratch(1) == FNIL) return
      else
           call statutil_1d_interpolate (scratch,n)
           if (scratch(1) == FNIL) return
           call statutil_1d_smooth_no_nils (scratch,n,nrun,endopt,trim)
      end if

      if (present(preserve)) then
           preserve2 = preserve
      else
           preserve2 = .false.
      end if

      if (preserve2) then
           where (array(1:n) /= FNIL) array(1:n) = scratch(1:n)
      else
           array(1:n) = scratch(1:n)
      end if
      return
      end subroutine statutil_1d_smooth

 
!!------------------------ statutil 1d runav -------------------------------!!
!!------------------------ statutil 1d runav -------------------------------!!
!!------------------------ statutil 1d runav -------------------------------!!


      subroutine statutil_1d_runav (array,n,nrun,endopt,trim,preserve,wild) 
      implicit none
      real                     ,intent(inout) :: array(:)        ! arguments
      integer                  ,intent(in)    :: n,nrun          ! arguments
      character(len=*),optional,intent(in)    :: endopt          ! arguments
      real            ,optional,intent(in)    :: trim            ! arguments
      logical         ,optional,intent(in)    :: preserve,wild   ! arguments
      logical                                 :: preserve2,wild2 ! local
      real                                    :: scratch(n)      ! local

      if (n <= 1) return
      scratch(1:n) = array(1:n)

      if (present(wild)) then
           wild2 = wild
      else
           wild2 = .false.
      end if

      if (wild2) then
           call statutil_1d_runav_no_nils (scratch,n,nrun,endopt,trim)
           where (array(1:n) == FNIL) scratch(1:n) = FNIL
           call statutil_1d_interpolate (scratch,n)
           if (scratch(1) == FNIL) return
      else
           call statutil_1d_interpolate (scratch,n)
           if (scratch(1) == FNIL) return
           call statutil_1d_runav_no_nils (scratch,n,nrun,endopt,trim)
      end if

      if (present(preserve)) then
           preserve2 = preserve
      else
           preserve2 = .false.
      end if

      if (preserve2) then
           where (array(1:n) /= FNIL) array(1:n) = scratch(1:n)
      else
           array(1:n) = scratch(1:n)
      end if
      return
      end subroutine statutil_1d_runav

 
!!------------------------- statutil scan statics -------------------------!!
!!------------------------- statutil scan statics -------------------------!!
!!------------------------- statutil scan statics -------------------------!!


      subroutine statutil_scan_statics (n,array,statmin,statmax,numnils)
      implicit none
      integer                  ,intent(in)  :: n                ! arguments
      real                     ,intent(in)  :: array(:)         ! arguments
      real                     ,intent(out) :: statmin,statmax  ! arguments
      integer                  ,intent(out) :: numnils          ! arguments
      integer                               :: indx             ! local

      statmin = FNIL
      statmax = FNIL
      numnils = 0

      do indx = 1,n
           if (array(indx) == FNIL) then
                numnils = numnils + 1
           else if (statmin == FNIL) then
                statmin = array(indx)
                statmax = array(indx)
           else
                statmin = min(statmin,array(indx))
                statmax = max(statmax,array(indx))
           end if
      end do

      if (statmin == FNIL) statmin = 0.0
      if (statmax == FNIL) statmax = 0.0
      return
      end subroutine statutil_scan_statics

 
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module statutil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

