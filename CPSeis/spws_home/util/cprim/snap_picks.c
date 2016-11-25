/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*
C      snap_picks.c
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  snap_picks    (snap picks on traces)
C          Written:  93/06/11  by:  Paul Hauge and Tom Stoeckley
C     Last revised:  98/02/26  by:  Ed Schmauch
C
C  Purpose:       Snap pick times to the nearest peak, trough, or
C                 zero crossing on traces.
C
C  Related Documentation: 
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            snap_picks.c
C
C  static functions:       adjust_pick       zero_picks
C                          break_picks       follow_picks
C                          follow_picks_helper
C                          correlate         max_correlation
C                          max_corr_offset   mostly_zeros
C                          sub_snap_adjacent lstsq_adjacent
C                          may_sub_snap_adjacent
C                          get_bracketing    snap_adjacent
C                          minMaxOffset      setOffWindPick
C                          checkOffWindPick
C
C  documented functions:   slope_picks  snap_picks    snap_pick
C                          stat_sear    derive_picks_spws  break_pick
C                          set_hurst_func
C
C  The user should include the above header file in his code.
C  The function stat_sear was written by Paul Hauge.
C  The other functions were written by Tom Stoeckley.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C         (this utility does not reference X, Xt, and Motif)
C
C  libraries:     none
C  header files:  cprim.h
C  functions:     none
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  9. 98/07/31  Schmauch   Fixed bug with get_bracketing and snap_adjacent
C  8. 98/03/25  Schmauch   Fixed bug with minMaxOffset
C  7. 98/02/26  Schmauch   Fixed bug with derive_picks_spws in PICK_CORR mode
C                          with multiple points picks.
C  6. 98/02/24  Schmauch   Fixed bug with lstsq_adjacent, makes sure x varies.
C  5. 97/11/26  Schmauch   Fixed bug with zero-offsets in max_corr_offset
C                          and snap_adjacent.
C  4. 96/11/18  Schmauch   Add combo (threshold and Hurst) 1st break picking.
C  3. 96/10/11  Schmauch   Add Hurst exponent method for 1st break picking.
C  2. 93/09/15  Stoeckley  Add missing_pick and zero_pick to arguments.
C  2. 93/07/19  Stoeckley  Add derive_picks_spws and adjust_picks.
C  1. 93/06/11  Stoeckley  Initial version; stat_sear copied from
C                            stat_pick.c (in cbyt) to this utility for
C                            general use.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C  To to fill/replace an array of pick times with new values derived
C    from the input parameters and/or from traces:
C    For derive_picks_spws, all parameters are i, except picks (b) & amplitudes (o).
C
C    void derive_picks_spws(action, pickmode, automode, direction,
C             threshhold, minSigDiff, first_trace, last_trace, num_traces,
C             first_time, last_time, tmincur, tmaxcur,
C             picks, missing_pick, zero_pick, traces,
C                    nsamp, tmin, dt, amplitudes, head, nwords,
C               b                          o
C             all_traces, tot_num_traces, all_picks, all_head,
C             index_into_all, orf, shortMin, shortMax, longMin, longMax, poto,
C             outlier)
C
C  long        action = what type of action to take (see below).
C  long      pickmode = automatic picking attribute (see below).
C  long      automode = automatic picking method (see below).
C  long     direction = direction of automatic picking (see below).
C  float   threshhold = fractional amplitude for first break picking.
C  float   minSigDiff = min normalized different required for Hurst picking.
C  long   first_trace = trace number to start on (1 or more).
C  long    last_trace = trace number to finish on (first_trace or more).
C  long    num_traces = total number of traces.
C  float   first_time = pick time (seconds) on first trace.
C  float    last_time = pick time (seconds) on last trace.
C  float      tmincur = minimum allowed pick time (seconds).
C  float      tmaxcur = maximum allowed pick time (seconds).
C  float      picks[] = array of pick times (seconds).
C  float missing_pick = special value indicating a missing pick.
C  float    zero_pick = special value indicating a zero-ed pick.
C  unsigned char traces[] = array of byte traces (back to back).
C  long         nsamp = number of samples in each trace.
C  float         tmin = time (seconds) of first trace sample.
C  float           dt = sample rate (seconds) between trace samples.
C  float amplitudes[] = trace amplitudes corresponding to the
C                            returned pick times (not used if NULL).
C
C  Last 14 arguments added by ehs
C
C  const float head[]     = header words
C  long      nwords       = no. of header words
C  const unsigned char all_traces[] = all traces in memory.
C  long tot_num_traces    = total number of traces in memory.
C  float all_picks[]      = all picks for traces in memory.
C  const float all_head[] = all header words for traces in memory.
C  long index_into_all    = index of picks into traces in memory.
C  float orf              = offset ratio factor for max_corr_offset.
C  float shortMin         = min pick time at shortest offset.
C  float shortMax         = max pick time at shortest offset.
C  float longMin          = min pick time at longest offset.
C  float longMax          = max pick time at longest offset.
C  float poto             = percent offset threshold only for combo picking.
C  float outlier          = outlier for lstsq_adjacent in std. dev. units.
C
C  All parameters are input except picks (b) and amplitudes (o).
C
C  The array of byte traces must match the array of pick times.
C
C  A portion of the array of pick times is modified by this routine,
C     using the requested action.
C  The first element modified is pick[first_trace - 1].
C  The last  element modified is pick[ last_trace - 1].
C  Picks preset to the value missing_pick are not reset.
C  All picks are constrained to the limits tmincur and tmaxcur.
C  If the index for any pick[] is outside of the range 0 through
C     num_traces-1, that value is not referenced.
C
C  When a "straight line" is refered to below, this line is defined by
C     the parameters first_trace, last_trace, first_time, and last_time.
C  When "snap" is referred to below, this means that a pick value is
C     modified by moving it to the nearest location on the corresponding
C     trace given by the pickmode parameter.
C
C  The action parameter should be set to one of these constants:
C     ZERO      Set all picks to the value zero_pick.
C     MANUAL    Set all picks to points along the straight line.
C     AUTOMATIC Snap all picks from points given by the automode parameter.
C     SNAP      Snap all picks from points preset in pick[].
C  The array of byte traces is not used for ZERO or MANUAL.
C
C  The pickmode parameter (used only for action = AUTOMATIC or SNAP)
C  should be set to one of these constants:
C     PEAK      pick nearest peak amplitude.
C     TROUGH    pick nearest trough amplitude.
C     POSITIVE  pick nearest zero crossing of positive slope.
C     NEGATIVE  pick nearest zero crossing of negative slope.
C
C  The automode parameter (used only for action = AUTOMATIC) should
C  be set to one of these constants:
C     FOLLOW_LINE   Snap each point from the straight line.
C     FOLLOW_SLOPE  Snap each point from a location given by the previous
C                     snapped point and the slope of the straight line.
C     FOLLOW_CURVE  Snap each point from a location given a straight line
C                     defined by the previous two snapped points.
C     FIRST_BREAK   Snap each point from a location given by searching
C                     down the trace from the straight line to the first
C                     amplitude exceeding the threshhold amplitude.
C     FIRST_BREAK_NO_SNAP   Same as FIRST_BREAK except no snapping is
C                     done after finding the first amplitude exceeding
C                     the threshhold amplitude.
C     FIRST_BREAK_CORR   First does first break, then removes statistical
C                     outliers from lst sq analysis of pick vs. offset,
C                     and finally repicks removed picks with correlation.
C     HURST_BREAK   Same as FIRST_BREAK except using Hurst exponent
C                     method instead of threshold to pick 1st break.
C     HURST_BREAK_NO_SNAP   Same as FIRST_BREAK_NO_SNAP except using
C                     Hurst exponent method instead of threshold to
C                     pick 1st break.
C     HURST_CORR   HURST_BREAK with correlation to fill in picks.
C     COMBO  Uses FIRST_BREAK and HURST_BREAK methods.
C     COMBO_CORR   COMBO with correlation to fill in picks.
C     PICK_CORR   Fill with correlation from pick(s).
C     CORRELATE   Fill zero_picks with correlation.
C
C  The direction parameter (used only for automode = FOLLOW_SLOPE or
C  FOLLOW_CURVE) should be set to one of these constants:
C     FORWARD    start at first_trace.
C     BACKWARD   start at last_trace.
C
C  The threshhold amplitude is given by the product of the threshhold
C     parameter and the maximum amplitude on the trace below the
C     straight line.  The threshhold amplitude will be positive if
C     pickmode = PEAK or POSITIVE, and negative if pickmode = TROUGH
C     or NEGATIVE.
C
C  The amplitude parameter (if not NULL) is not set when action =
C     ZERO or MANUAL, or when action = AUTOMATIC and automode =
C     FIRST_BREAK_NO_SNAP.
C
C  The above constants are defined in the cprim.h header file.
C  This is the highest-level routine in this file.
C  This routine calls snap_picks and slope_picks.
C-----------------------------------------------------------------------
C  To fill an array of pick times with picks along a sloping line:
C
C                           i           i           i           i
C    void slope_picks (first_trace, last_trace, num_traces, first_time,
C      last_time, tmincur, tmaxcur, picks, missing_pick)
C          i         i        i       b         i
C
C  long   first_trace = trace number to start on (1 or more).
C  long    last_trace = trace number to finish on (first_trace or more).
C  long    num_traces = total number of traces.
C  float   first_time = pick time (seconds) on first trace.
C  float    last_time = pick time (seconds) on last trace.
C  float      tmincur = minimum allowed pick time (seconds).
C  float      tmaxcur = maximum allowed pick time (seconds).
C  float      picks[] = array of pick times (seconds).
C  float missing_pick = special value indicating a missing pick.
C
C  The pick time for the first trace is put into pick[first_trace - 1].
C  The pick time for the last  trace is put into pick[ last_trace - 1].
C  The pick times for intervening traces are obtained by straight line
C     interpolation.
C  All picks are constrained to the limits tmincur and tmaxcur.
C  The array of pick times is not touched outside of the above range.
C  Picks preset to the value missing_pick are not reset.  Any other
C     preset value is irrelevant and will be reset.
C  If the index for any pick[] is outside of the range 0 through
C     num_traces-1, that value is not referenced.
C-----------------------------------------------------------------------
C  To replace an array of pick times with snapped picks from traces:
C
C                         i           i           i          i        i
C   void snap_picks (first_trace, last_trace, num_traces, tmincur, tmaxcur,
C
C               picks, missing_pick, zero_pick,
C                 b         i            i
C
C               traces, nsamp, tmin, dt, pickmode, amplitudes)
C                 i       i     i    i      i          o
C
C  long   first_trace = trace number to start on (1 or more).
C  long    last_trace = trace number to finish on (first_trace or more).
C  long    num_traces = total number of traces.
C  float      tmincur = minimum allowed pick time (seconds).
C  float      tmaxcur = maximum allowed pick time (seconds).
C  float      picks[] = array of pick times (seconds).
C  float missing_pick = special value indicating a missing pick.
C  float    zero_pick = special value indicating a zero-ed pick.
C  unsigned char traces[] = array of byte traces (back to back).
C  long         nsamp = number of samples in each trace.
C  float         tmin = time (seconds) of first trace sample.
C  float           dt = sample rate (seconds) between trace samples.
C  long      pickmode = picking mode for sampling (see below).
C  float amplitudes[] = trace amplitudes corresponding to the
C                            returned pick times (not used if NULL).
C
C  The array of byte traces matches the array of pick times.
C  A portion of the array of pick times is modified by this routine.
C     Each element is modified by snapping it to the nearest location
C     in the corresponding trace given by the picking mode (see below).
C     The first element modified is pick[first_trace - 1].
C     The last  element modified is pick[ last_trace - 1].
C     The array of pick times is not touched outside of the above range.
C     All picks are constrained to the limits tmincur and tmaxcur.
C  If the index for any pick[] is outside of the range 0 through
C     num_traces-1, that value is not referenced.
C  The picking mode should be set to one the following constants
C     defined in cprim.h:
C         PEAK      snap to nearest peak amplitude.
C         TROUGH    snap to nearest trough amplitude.
C         POSITIVE  snap to nearest zero crossing of positive slope.
C         NEGATIVE  snap to nearest zero crossing of negative slope.
C  This routine calls snap_pick for each selected trace.
C  Picks preset to the value missing_pick or zero_pick are not reset.
C     Any other preset value will be reset as described above.
C-----------------------------------------------------------------------
C  To replace a pick time with a snapped pick from a trace.
C
C                      b      i      i     i    i      i          o
C    void snap_pick (&pick, trace, nsamp, tmin, dt, pickmode, &amplitude)
C
C  float            pick = pick time (seconds).
C  unsigned char trace[] = byte trace.
C  long            nsamp = number of samples in trace.
C  float            tmin = time (seconds) of first trace sample.
C  float              dt = sample rate (seconds) between trace samples.
C  long         pickmode = picking mode for sampling (see above).
C  float       amplitude = trace amplitude corresponding to the
C                                 returned pick time (not used if NULL).
C
C  This routine calls stat_sear, which does all the work.
C  The parameter pickmode  is the same as ipol   in stat_sear.
C  The parameter nsamp     is the same as nrecl  in stat_sear.
C  The parameter trace     is the same as jdat   in stat_sear.
C  The parameter amplitude is the same as datmax in stat_sear (if not NULL).
C  This routine sets irange = 20 and interp = 2 in the call to stat_sear.
C-----------------------------------------------------------------------
C  To get a single snapped pick from a trace.
C
C                 i      i      i       i     i      o       o      i
C void stat_sear(ipol, irang, interp, nrecl, xsam, &smax, &datmax, jdat)
C
C  int        ipol  = picking mode for sampling.
C  int       irange = range of points to search over.
C  int       interp = 1 to return nearest sample and 2 to interpolate.
C  int        nrecl = number of samples in each trace.
C  float       xsam = exact trace sample index before snapping.
C  float       smax = exact trace sample index after snapping (returned).
C  float     datmax = trace amplitude for the sample index smax (returned).
C  unsigned char jdat[] = byte trace.
C
C  This routine, written by Paul Hauge, has been copied unaltered from
C    stat_pick.c (in cbyt) and placed in this utility for general use.
C-----------------------------------------------------------------------
C  To set function pointer that replaces break_picks with a Hurst exponent
C  1st break picker.  Enables cprim function derive_picks_spws to access class
C  Hurst in libpick.a.   11oct96   ehs
C
C                             i
C void set_hurst_func(void (*hurst_func)
C	(long first_trace, long last_trace, long num_traces,
C	float tmincur, float tmaxcur, float picks[], float missing_pick,
C	float zero_pick, unsigned char traces[], long nsamp, float tmin,
C       float dt, float minSigDiff, float *minPicks, float *maxPicks))
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#ifndef sun
#include <time.h>
#endif

#include "cprim.h"
#include "named_constants.h"


#define GET_SLOPE(first_trace,last_trace,first_time,last_time,slope)  \
  if((last_trace) != (first_trace))                                   \
          (slope) = ((last_time ) - (first_time )) /                  \
                    ((last_trace) - (first_trace));                   \
  else    (slope) = 0.0;

#define GET_INDICES(first_trace,last_trace,num_traces,index1,index2) \
  (index1) = ConstrainValue((first_trace) - 1, 0, (num_traces) - 1); \
  (index2) = ConstrainValue(( last_trace) - 1, 0, (num_traces) - 1);



/*------------------------ set_hurst_func -------------------------------*/

/*
 * 11oct96   ehs
 * Enables cprim function derive_picks_spws to access class Hurst in libpick.a.
 */

static void (*hurst_break)
	(long first_trace, long last_trace, long num_traces,
	float tmincur, float tmaxcur, float picks[], float missing_pick,
	float zero_pick, unsigned char traces[], long nsamp, float tmin,
        float dt, float minSigDiff, float *minPicks, float *maxPicks)
	= (void (*)
	(long first_trace, long last_trace, long num_traces,
	float tmincur, float tmaxcur, float picks[], float missing_pick,
	float zero_pick, unsigned char traces[], long nsamp, float tmin,
        float dt, float minSigDiff, float *minPicks, float *maxPicks)) NULL;

void set_hurst_func(void (*hurst_func)
	(long first_trace, long last_trace, long num_traces,
	float tmincur, float tmaxcur, float picks[], float missing_pick,
	float zero_pick, unsigned char traces[], long nsamp, float tmin,
        float dt, float minSigDiff, float *minPicks, float *maxPicks))
{
	hurst_break = hurst_func;
}



/*------------------ super_snap_pick ------------------------------------*/

static void super_snap_pick(long tn, long num_traces,
                float tmincur, float tmaxcur,
                float picks[], float missing_pick, float zero_pick,
                unsigned char traces[],
                long nsamp, float tmin, float dt, long pickmode,
                float *amplitudes)
{
  float amplitude;
  int i = tn - 1;

  if(tn < 1 || tn > num_traces) return;
  if(picks[i] == missing_pick || picks[i] == zero_pick) return;
  snap_pick(&picks[i], &traces[i * nsamp],
                         nsamp, tmin, dt, pickmode, &amplitude);
  picks[i] = ConstrainValue(picks[i], tmincur, tmaxcur);
  if(amplitudes) amplitudes[i] = amplitude;
}



/*------------------------- break_pick ----------------------------------*/

void break_pick(float *pick, unsigned char trace[],
      float tmincur, float tmaxcur,
      long nsamp, float tmin, float dt, long pickmode, float threshhold)
{
  float f, fmax, test;
  int j, jstart, jmin, jmax;

  if(threshhold <= 0.0) return;
  jstart = (*pick   - tmin) / dt + 0.5;
  jmin   = (tmincur - tmin) / dt + 0.5;
  jmax   = (tmaxcur - tmin) / dt + 0.5;
  if     (jstart >= jmax - 3) return;
  if     (jstart <  jmin    ) jstart = jmin;
  fmax = 0.0;
  for(j = jstart; j < jmax; j++)
       {
       f = (float)((int)trace[j] - 128);
       if(pickmode == TROUGH || pickmode == NEGATIVE) f *= -1;
       fmax = MaximumValue(f, fmax);
       }
  test = threshhold * fmax;
  for(j = jstart; j < jmax; j++)
       {
       f = (float)((int)trace[j] - 128);
       if(pickmode == TROUGH || pickmode == NEGATIVE) f *= -1;
       if(f >= test)
            {
            *pick = tmin + j * dt;
            return;
            }
       }
}



/*------------------------- break_picks ---------------------------------*/

static void break_picks(long first_trace, long last_trace, long num_traces,
          float tmincur, float tmaxcur,
          float picks[], float missing_pick, unsigned char traces[],
          long nsamp, float tmin, float dt, long pickmode, float threshhold)
{
  int number = last_trace - first_trace + 1;
  int i, index1, index2;

  for(i = 0; i < number; i++)
       {
       index1 = first_trace - 1 + i;
       if(index1 >= 0 && index1 < num_traces && 
                                 picks[index1] != missing_pick)
            {
            index2 = index1 * nsamp;
            break_pick(&picks[index1], &traces[index2], tmincur, tmaxcur,
                         nsamp, tmin, dt, pickmode, threshhold);
            }
       }
}


/*----------------- follow_picks_helper ---------------------------------*/

static void follow_picks_helper(long tn, long num_traces,
                long *prev_trace, float *prev_time, float *slope, 
                float tmincur, float tmaxcur,
                float picks[], float missing_pick, float zero_pick,
                unsigned char traces[],
                long nsamp, float tmin, float dt, long pickmode,
                long automode, float *amplitudes)
{
  long          i = tn - 1;
  long difference = tn - *prev_trace;

  if(tn < 1 || tn > num_traces) return;
  if(picks[i] == missing_pick) return;
  picks[i] = *prev_time + *slope * difference;
  super_snap_pick(tn, num_traces, tmincur, tmaxcur,
                picks, missing_pick, zero_pick,
                traces, nsamp, tmin, dt, pickmode, amplitudes);
  if(difference != 0 && automode == FOLLOW_CURVE)
       {
       *slope = (picks[i] - *prev_time) / difference; 
       }
  if(automode != FOLLOW_LINE)
       {
       *prev_trace = tn;
       *prev_time  = picks[i];
       }
}

 
/*----------------------- follow_picks ----------------------------------*/
    /* for AUTOMODE == FOLLOW_LINE, FOLLOW_SLOPE, and FOLLOW_CURVE */

static void follow_picks(long first_trace, long last_trace, long num_traces,
                float first_time, float last_time,
                float tmincur, float tmaxcur,
                float picks[], float missing_pick, float zero_pick,
                unsigned char traces[],
                long nsamp, float tmin, float dt, long pickmode,
                long automode, long direction,
                float *amplitudes)
{
  long tn, prev_trace;
  float slope, prev_time;

  GET_SLOPE  (first_trace, last_trace, first_time, last_time, slope);
  if(direction == FORWARD)
       {
       prev_trace = first_trace;
       prev_time  = first_time;
       for(tn = first_trace; tn <= last_trace; tn++)
            {
            follow_picks_helper(tn, num_traces, &prev_trace,
                     &prev_time, &slope, tmincur, tmaxcur,
                     picks, missing_pick, zero_pick,
                     traces, nsamp, tmin, dt,
                     pickmode, automode, amplitudes);
            }
       }
  else
       {
       prev_trace = last_trace;
       prev_time  = last_time;
       for(tn = last_trace; tn >= first_trace; tn--)
            {
            follow_picks_helper(tn, num_traces, &prev_trace,
                     &prev_time, &slope, tmincur, tmaxcur,
                     picks, missing_pick, zero_pick,
                     traces, nsamp, tmin, dt,
                     pickmode, automode, amplitudes);
            }
       }
}

 

/*--------------------------- zero_picks --------------------------------*/

static void zero_picks(long first_trace, long last_trace, long num_traces,
           float picks[], float missing_pick, float zero_pick)
{
  long i, index1, index2;

  GET_INDICES(first_trace, last_trace, num_traces, index1, index2);
  for(i = index1; i <= index2; i++)
       {
       if(picks[i] != missing_pick) picks[i] = zero_pick;
       }
}



/*-------------------------- slope_picks --------------------------------*/

void slope_picks(long first_trace, long last_trace, long num_traces,
                 float first_time, float last_time, 
                 float tmincur, float tmaxcur,
                 float picks[], float missing_pick)
{
  long i, index1, index2;
  float slope, time;

  GET_SLOPE  (first_trace, last_trace, first_time, last_time, slope);
  GET_INDICES(first_trace, last_trace, num_traces, index1, index2);
  for(i = index1; i <= index2; i++)
       {
       if(picks[i] != missing_pick)
            {
            time = first_time + (i + 1 - first_trace) * slope;
            picks[i] = ConstrainValue(time, tmincur, tmaxcur);
            }
       }
}



/*------------------------- adjust_pick ---------------------------------*/

#define ADJUST_TOLERANCE 0.03  /* adjustment (seconds) for ends of rubberband */

static void adjust_pick(long tn, float *time, long num_traces,
        float tmincur, float tmaxcur, float picks[],
        float missing_pick, float zero_pick, unsigned char traces[],
        long nsamp, float tmin, float dt, long pickmode)
{
  int i = tn - 1;

  if(tn < 1 || tn > num_traces) return;
  if(picks[i] == missing_pick || picks[i] == zero_pick) return;
  picks[i] = *time;
  super_snap_pick(tn, num_traces, tmincur, tmaxcur,
                picks, missing_pick, zero_pick,
                traces, nsamp, tmin, dt, pickmode, NULL);
  if(fabs(picks[i] - *time) < ADJUST_TOLERANCE) *time = picks[i];
}



/*------------------------- correlate -----------------------------------*/

#define CORR_WINDOW 20

static float correlate(unsigned char trace1[], unsigned char trace2[],
	long trace1_pos, long trace2_pos, long num)
{
	float a, b, sum_ab, sum_aa, sum_bb, denom;
	long i;

	for (sum_ab = sum_aa = sum_bb = 0.0F, i = 0; i < num; i++)
	{
		a = (float) trace1[trace1_pos++] - 128.0F;
		b = (float) trace2[trace2_pos++] - 128.0F;
		sum_ab += a * b;
		sum_aa += a * a;
		sum_bb += b * b;
	}

	denom = (float) sqrt((double) (sum_aa * sum_bb));

	return (denom == 0.0F) ? 0.0F : sum_ab / denom;
}



/*------------------------- max_correlation -----------------------------*/

/*
 * Replaced by max_corr_offset.
 *
 * static long max_correlation(unsigned char trace1[], unsigned char trace2[],
 * 	long nsamp, long window, long trace1_pick)
 * {
 * 	long retval;
 * 	long min1, max1, minMin2, maxMin2, trace2_pos;
 * 	float maxCorr, corr;
 * 
 * 	min1 = trace1_pick - window / 2;
 * 	max1 = min1 + window - 1;
 * 	min1 = (min1 < 0) ? 0 : min1;
 * 	max1 = (max1 >= nsamp) ? nsamp - 1: max1;
 * 	window = max1 - min1 + 1;
 * 	minMin2 = min1 - window + 1;
 * 	minMin2 = (minMin2 < 0) ? 0 : minMin2;
 * 	maxMin2 = (max1 + window - 1 >= nsamp) ? nsamp - window : max1;
 * 
 * 	for (maxCorr = correlate(trace1, trace2, min1, minMin2, window),
 * 		retval = minMin2, trace2_pos = minMin2 + 1;
 * 		trace2_pos <= maxMin2;
 * 		trace2_pos++)
 * 	{
 * 		corr = correlate(trace1, trace2, min1, trace2_pos, window);
 * 
 * 		if (corr > maxCorr)
 * 		{
 * 			maxCorr = corr;
 * 			retval = trace2_pos;
 * 		}
 * 	}
 * 
 * 	return retval - min1;
 * }
 */



/*------------------------- max_corr_offset------------------------------*/

static long max_corr_offset(unsigned char trace1[], unsigned char trace2[],
	long nsamp, long window, long trace1_pick, float offset1, float offset2,
	float orf)
{
	long retval;
	long min1, max1, init_guess2, min2, max2, upMaxI, downMaxI, trace2_pos;
	float upMaxCorr, downMaxCorr, corr;

	min1 = trace1_pick - window / 2;
	max1 = min1 + window - 1;
	min1 = (min1 < 0) ? 0 : min1;
	max1 = (max1 >= nsamp) ? nsamp - 1: max1;
	window = max1 - min1 + 1;

	if (offset1 == 0.0F)
		init_guess2 = trace1_pick;
	else
		init_guess2 = trace1_pick + (long) ((float) trace1_pick
			* orf * (offset2 - offset1) / offset1 + 0.5F);

	min2 = init_guess2 - window / 2;
	max2 = min2 + window - 1;

	if (min2 < 0)
	{
		max2 -= min2;
		min2 = 0;
	}

	if (max2 >= nsamp)
	{
		min2 -= max2 - nsamp + 1;
		max2 = nsamp - 1;
	}

	assert(min2 >= 0 && max2 < nsamp);

	for (upMaxCorr = correlate(trace1, trace2, min1, min2, window),
		upMaxI = min2, trace2_pos = min2 - 1;
		trace2_pos >= 0;
		trace2_pos--)
	{
		corr = correlate(trace1, trace2, min1, trace2_pos, window);

		if (corr > upMaxCorr)
		{
			upMaxCorr = corr;
			upMaxI = trace2_pos;
		}
		else
		{
			break;
		}
	}

	for (downMaxCorr = correlate(trace1, trace2, min1, min2, window),
		downMaxI = min2, trace2_pos = min2 + 1;
		trace2_pos <= nsamp - window;
		trace2_pos++)
	{
		corr = correlate(trace1, trace2, min1, trace2_pos, window);

		if (corr > downMaxCorr)
		{
			downMaxCorr = corr;
			downMaxI = trace2_pos;
		}
		else
		{
			break;
		}
	}

	retval = (upMaxCorr > downMaxCorr) ? (upMaxI   - min1)
					   : (downMaxI - min1);

	return retval;
}



/*------------------------- mostly_zeros --------------------------------*/

#define NON_ZERO_THRESH 0.02F

static long mostly_zeros(unsigned char trace[], long nsamp)
{
	long non_zeros, i;

	for (non_zeros = i = 0; i < nsamp; i++)
		if (trace[i] != 128)
			non_zeros++;

	if ((float) non_zeros / (float) nsamp < NON_ZERO_THRESH)
		return 1;
	else
		return 0;
}


 
/*------------------------- sub_snap_adjacent ---------------------------*/

#define REPICK -777.0F

static void sub_snap_adjacent(long first_trace, long last_trace,
   long num_traces, float tmincur, float tmaxcur, float picks[],
   float missing_pick, float zero_pick, unsigned char traces[], long nsamp,
   float tmin, float dt, long pickmode, float *amplitudes,
   const float *head, long nwords, float orf)
{
   long  *zeroes    = (long  *) malloc((size_t)num_traces * sizeof(long ));
   float *tempPicks = (float *) malloc((size_t)num_traces * sizeof(float));
   long numZeroes, unpicked, lag, last_picked, first_in_set, i, j;
   float pick, deltaPick;

   for (numZeroes = unpicked = 0, i = first_trace - 1; i < last_trace; i++)
   {
      zeroes[i] = mostly_zeros(&traces[i * nsamp], nsamp);

      if (zeroes[i])
      {
         picks[i] = zero_pick;
         numZeroes++;
      }
      else
      {
         if (picks[i] == zero_pick)
         {
            picks[i] = REPICK;
            unpicked++;
         }
      }
   }

   if (numZeroes + unpicked == last_trace - first_trace + 1)
   {
      for (i = first_trace - 1; unpicked && (i < last_trace); i++)
         if (picks[i] == REPICK)
         {
            picks[i] = zero_pick;
            unpicked--;
          }

      assert(unpicked == 0);

      free((void *) zeroes   );
      free((void *) tempPicks);
      return;
   }

   for (last_picked = first_in_set = -1, i = first_trace - 1;
      (unpicked || first_in_set != -1) && i < last_trace;
      i++)
   {
      if (!zeroes[i])
      {
         if (picks[i] == REPICK)
         {
            if (last_picked != -1)
            {
               lag = max_corr_offset(
                  &traces[last_picked * nsamp],
                  &traces[i           * nsamp],
                  nsamp, CORR_WINDOW, (long) (picks[last_picked] / dt + 0.5F),
                  head[last_picked * nwords + 5],
                  head[i           * nwords + 5],
                  orf);

               picks[i] = picks[last_picked] + (float) lag * dt;

               super_snap_pick(i + 1, num_traces, tmincur, tmaxcur, picks,
                  missing_pick, zero_pick, traces, nsamp, tmin, dt,
                  pickmode, amplitudes);

               if (first_in_set == -1)
                  first_in_set = i;

               assert(unpicked > 0);
               unpicked--;
               last_picked = i;
            }
         }
         else
         {
            if (last_picked == -1)
            {
               assert(first_in_set == -1);

               for (last_picked = i, j = i - 1; j >= first_trace - 1; j--)
               {
                  if (!zeroes[j])
                  {
                     assert(picks[j] == REPICK);

                     lag = max_corr_offset(
                        &traces[last_picked * nsamp],
                        &traces[j           * nsamp],
                        nsamp, CORR_WINDOW,
                        (long) (picks[last_picked] / dt + 0.5F),
                        head[last_picked * nwords + 5],
                        head[j           * nwords + 5],
                        orf);

                     picks[j] = picks[last_picked] + (float) lag * dt;

                     super_snap_pick(j + 1, num_traces,
                        tmincur, tmaxcur, picks,
                        missing_pick, zero_pick, traces,
                        nsamp, tmin, dt, pickmode,
                        amplitudes);

                     unpicked--;
                     last_picked = j;
                  }
               }
            }
            else if (first_in_set != -1)
            {
               assert(last_picked != -1);

               lag = max_corr_offset(
                  &traces[last_picked * nsamp],
                  &traces[i           * nsamp],
                  nsamp, CORR_WINDOW, (long) (picks[last_picked] / dt + 0.5F),
                  head[last_picked * nwords + 5],
                  head[i           * nwords + 5],
                  orf);

               pick = picks[last_picked] + (float) lag * dt;

               super_snap_pick(1, 1, tmincur, tmaxcur, &pick,
                  missing_pick, zero_pick, &traces[i * nsamp], nsamp, tmin, dt,
                  pickmode, amplitudes);

               if (pick != picks[i])
               {
                  deltaPick = pick - picks[i];

                  for (last_picked = i, j = i - 1; j >= first_in_set; j--)
                  {
                     if (!zeroes[j])
                     {
                        assert(picks[j] != REPICK);

                        lag = max_corr_offset(
                           &traces[last_picked * nsamp],
                           &traces[j           * nsamp],
                           nsamp, CORR_WINDOW,
                           (long) (picks[last_picked] / dt + 0.5F),
                           head[last_picked * nwords + 5],
                           head[j           * nwords + 5],
                           orf);
   
                        tempPicks[j] = picks[j];
                        picks    [j] = picks[last_picked] + (float) lag * dt;
   
                        super_snap_pick(j + 1, num_traces,
                           tmincur, tmaxcur, picks,
                           missing_pick, zero_pick, traces,
                           nsamp, tmin, dt, pickmode,
                           amplitudes);

                        last_picked = j;
                     }
                  }

                  assert(last_picked != i);

                  for (; j >= first_trace - 1 && zeroes[j]; j--);
                  assert(j >= first_trace - 1 && picks[j] != REPICK);

                  lag = max_corr_offset(
                     &traces[last_picked * nsamp],
                     &traces[j           * nsamp],
                     nsamp, CORR_WINDOW,
                     (long) (picks[last_picked] / dt + 0.5F),
                     head[last_picked * nwords + 5],
                     head[j           * nwords + 5],
                     orf);

                  pick = picks[last_picked] + (float) lag * dt;

                  super_snap_pick(1, 1, tmincur, tmaxcur,
                     &pick, missing_pick, zero_pick,
                     &traces[j * nsamp],
                     nsamp, tmin, dt, pickmode, amplitudes);

                  if (fabs((double) (pick - picks[j]))
                    > fabs((double)  deltaPick       ))
                  {
                     for (j = i - 1; j >= first_in_set; j--)
                        if (!zeroes[j])
                           picks[j] = tempPicks[j];
                  }
               }

               first_in_set = -1;
            }

            last_picked = i;
         }
      }
   }

   assert(unpicked == 0);

   free((void *) zeroes   );
   free((void *) tempPicks);
}



/*------------------------- lstsq_adjacent ------------------------------*/

static long lstsq_adjacent(long first_trace, long last_trace, float picks[],
	float missing_pick, float zero_pick, const float *head, long nwords,
	float outlier, long min_to_lstsq)
{
	long retval = 0;

	float x, y, sum_x, sum_y, sum_xx, sum_yy, sum_xy;
	float slope, inter, ss, std, y_calc;
	long cnt, i;
	float first_nonzero = 0.0F;  /* init only to avoid compiler warning */
	long got_first_nonzero, x_changes;

	for (sum_x = sum_y = sum_xx = sum_yy = sum_xy = 0.0F, cnt = 0,
		got_first_nonzero = x_changes = 0,
		i = first_trace - 1;
		i < last_trace;
		i++)
	{
		y = picks[i];

		if (y != zero_pick && y != missing_pick)
		{
			cnt++;
			x = head[i * nwords + 5];
			sum_x += x;
			sum_y += y;
			sum_xx += x * x;
			sum_yy += y * y;
			sum_xy += x * y;

			if (!x_changes)
			{
				if (got_first_nonzero)
				{
					if (x != first_nonzero)
						x_changes = 1;
				}
				else
				{
					    first_nonzero = x;
					got_first_nonzero = 1;
				}
			}
		}
	}

	if ((cnt >= min_to_lstsq) && x_changes)
	{
		slope = ((float) cnt * sum_xy - sum_x * sum_y)
		      / ((float) cnt * sum_xx - sum_x * sum_x);

		inter = (sum_xx      * sum_y  - sum_x * sum_xy)
		      / ((float) cnt * sum_xx - sum_x * sum_x);

		ss = sum_yy - sum_y * sum_y / (float) cnt
		  - (sum_xy - sum_x * sum_y / (float) cnt) * slope;

		std = (float) sqrt((double) ss / (double) (cnt - 2));

		for (i = first_trace - 1; i < last_trace; i++)
		{
			y = picks[i];

			if (y != zero_pick && y != missing_pick)
			{
				y_calc = slope *  head[i * nwords + 5] + inter;

				if ((float) fabs((double) (y_calc - y)) / std
					> outlier)
				{
					retval++;
					picks[i] = zero_pick;
				}
			}
		}

	}

	return retval;
}



/*------------------------- may_sub_snap_adjacent -----------------------*/

#define OFFSET_THRESH 0.0F
#define MAX_ITER       3	/* 3 sub_snap_adjacent & 2 lstsq_adjacent */
#define MIN_TO_LSTSQ  10

static void may_sub_snap_adjacent(long first_trace, long last_trace,
	long num_traces, float tmincur, float tmaxcur, float picks[],
	float missing_pick, float zero_pick, unsigned char traces[], long nsamp,
	float tmin, float dt, long pickmode, float *amplitudes,
	const float *head, long nwords, float *pick1, float *pick2, float orf,
	float outlier)
{
	float first_offset, last_offset, thresh_offset;
	long iter, i;

	first_offset = head[(first_trace-1) * nwords + 5];
	 last_offset = head[( last_trace-1) * nwords + 5];

	if (first_offset < last_offset)
		thresh_offset = first_offset
			+ (last_offset - first_offset) * OFFSET_THRESH;
	else
		thresh_offset = last_offset
			+ (first_offset - last_offset) * OFFSET_THRESH;

	for (i = first_trace - 1; i < last_trace; i++)
		if ((picks[i] != zero_pick)
		 && (head[i * nwords + 5] >= thresh_offset))
		{
			iter = 0;

			do
			{
				sub_snap_adjacent(first_trace, last_trace,
					num_traces, tmincur, tmaxcur, picks,
					missing_pick, zero_pick, traces, nsamp,
					tmin, dt, pickmode, amplitudes,
					head, nwords, orf);

				iter++;
			} while ((last_trace - first_trace + 1 >= MIN_TO_LSTSQ)
			      && (iter < MAX_ITER)
			      && (lstsq_adjacent(first_trace, last_trace,
					picks, missing_pick, zero_pick,
					head, nwords, outlier, MIN_TO_LSTSQ)));

			return;
		}

	if ((pick1 != (float *) NULL) && (pick2 != (float *) NULL))
	{
		double minDiff, diff;
		long minIndex;

		for (minDiff = 0.0, minIndex = -1, i = first_trace - 1;
			i < last_trace; i++)
		{
			if ((pick1[i] != zero_pick) && (pick2[i] != zero_pick)
			 && (head[i * nwords + 5] >= thresh_offset))
			{
				if (minIndex == -1)
				{
					minIndex = i;
					minDiff = fabs((double)
						(pick1[i] - pick2[i]));
				}
				else
				{
					diff = fabs((double)
						(pick1[i] - pick2[i]));

					if (diff < minDiff)
					{
						minIndex = i;
						minDiff = diff;
					}
				}
			}
		}

		if (minIndex != -1)
		{
			picks[minIndex] = pick1[minIndex];
			iter = 0;

			do
			{
				sub_snap_adjacent(first_trace, last_trace,
					num_traces, tmincur, tmaxcur, picks,
					missing_pick, zero_pick, traces, nsamp,
					tmin, dt, pickmode, amplitudes,
					head, nwords, orf);

				iter++;
			} while ((last_trace - first_trace + 1 >= MIN_TO_LSTSQ)
			      && (iter < MAX_ITER)
			      && (lstsq_adjacent(first_trace, last_trace,
					picks, missing_pick, zero_pick,
					head, nwords, outlier, MIN_TO_LSTSQ)));

			printf("may_sub_snap_adjacent:  diff of %lf\n",
				minDiff);
		}
	}
}



/*------------------------- get_bracketing ------------------------------*/

static void get_bracketing(long first_trace, long last_trace, long num_traces,
	const float head[], long nwords, long the_trace,
	long *bracket1, long *bracket2)
{
	enum { STARTING, MIN_SEARCH, MAX_SEARCH } search;
	long first, diff, i, j;

	assert(the_trace >= first_trace && the_trace <= last_trace);

	for (search = STARTING, first = i = first_trace; i < last_trace; )
		switch (search)
		{
			case STARTING:
				/*
				 * Use j instead of i so MIN_SEARCH
				 * or MAX_SEARCH can check for changing
				 * rcv line.
				 */
				for (j = i; (j < last_trace) && 
					(head[ j    * nwords + 5] ==
					 head[(j-1) * nwords + 5]);
					j++);

				if (j == last_trace)
				{
					*bracket1 = i;
					*bracket2 = last_trace;
					return;
				}
				else if (head[ j    * nwords + 5]
				       > head[(j-1) * nwords + 5])
				{
					search = MAX_SEARCH;
				}
				else
				{
					search = MIN_SEARCH;
				}

				break;
			case MIN_SEARCH:
				for (; i < last_trace; i++)
				{
					if (head[ i    * nwords + 5]
					  > head[(i-1) * nwords + 5])
					{
						search = MAX_SEARCH;
						break;
					}

					/*
					 * If seq rcv pos out of order,
					 * probably diff rcv line in same grp.
					 */
					diff = (long) floor((double)
						(head[ i    * nwords + 46] -
						 head[(i-1) * nwords + 46] +
						 0.5F));
					if (diff < -10 || diff > 10)
					{
						search = STARTING;
						break;
					}
				}

				/*
				 * If it falls thru this loop without setting 
				 * search, that's OK since we are done.
				 */

				if ((i != first) || (search == STARTING))
				{
					if (the_trace >= first
					 && the_trace <= i    )
					{
						*bracket1 = first;
						*bracket2 = i    ;
						return;
					}

					first = i + 1;
				}

				i++;
				break;
			case MAX_SEARCH:
				for (; i < last_trace; i++)
				{
					if (head[ i    * nwords + 5]
					  < head[(i-1) * nwords + 5])
					{
						search = MIN_SEARCH;
						break;
					}

					/*
					 * If seq rcv pos out of order,
					 * probably diff rcv line in same grp.
					 */
					diff = (long) floor((double)
						(head[ i    * nwords + 46] -
						 head[(i-1) * nwords + 46] +
						 0.5F));
					if (diff < -10 || diff > 10)
					{
						search = STARTING;
						break;
					}
				}

				/*
				 * If it falls thru this loop without setting 
				 * search, that's OK since we are done.
				 */

				if ((i != first) || (search == STARTING))
				{
					if (the_trace >= first
					 && the_trace <= i    )
					{
						*bracket1 = first;
						*bracket2 = i    ;
						return;
					}

					first = i + 1;
				}

				i++;
				break;
			default:
				assert(0);
		}

	assert((first == last_trace - 1) || (first == last_trace));
	*bracket1 = first     ;
	*bracket2 = last_trace;
}



/*------------------------- snap_adjacent -------------------------------*/

#define MIN_PTS_TO_CORR 5
#define MAX_REL_DIFF    0.1

static void snap_adjacent(long first_trace, long last_trace, long num_traces,
	float tmincur, float tmaxcur, float picks[], float missing_pick,
	float zero_pick, unsigned char traces[], long nsamp, float tmin,
	float dt, long pickmode, float *amplitudes,
	const float *head, long nwords, float *pick1, float *pick2, float orf,
	float outlier)
{
#ifndef sun
 	clock_t start = clock();
#endif

	enum { STARTING, MIN_SEARCH, MAX_SEARCH } search;
	long first, diff, i, j;

	for (search = STARTING, first = i = first_trace; i < last_trace; )
		switch (search)
		{
			case STARTING:
				/*
				 * Use j instead of i so MIN_SEARCH
				 * or MAX_SEARCH can check for changing
				 * rcv line.
				 */
				for (j = i; (j < last_trace) && 
					(head[ j    * nwords + 5] ==
					 head[(j-1) * nwords + 5]);
					j++);

				if (j == last_trace)
				{
					/*
					 * All offsets equal
					 */
					may_sub_snap_adjacent(i, last_trace,
						num_traces, tmincur, tmaxcur,
						picks, missing_pick, zero_pick,
						traces, nsamp, tmin, dt,
						pickmode, amplitudes,
						head, nwords, pick1, pick2,
						orf, outlier);

					first = i = last_trace;
				}
				else if (head[ j    * nwords + 5]
				       > head[(j-1) * nwords + 5])
				{
					search = MAX_SEARCH;
				}
				else
				{
					search = MIN_SEARCH;
				}

				break;
			case MIN_SEARCH:
				for (; i < last_trace; i++)
				{
					if (head[ i    * nwords + 5]
					  > head[(i-1) * nwords + 5])
					{
						search = MAX_SEARCH;
						break;
					}

					/*
					 * If seq rcv pos out of order,
					 * probably diff rcv line in same grp.
					 */
					diff = (long) floor((double)
						(head[ i    * nwords + 46] -
						 head[(i-1) * nwords + 46] +
						 0.5F));
					if (diff < -10 || diff > 10)
					{
						search = STARTING;
						break;
					}
				}

				/*
				 * may_sub_snap_adjacent called when
				 * either > offset or out of traces.
				 * i is used in call to reference last <=
				 * since may_sub_snap_adjacent counts
				 * traces from 1 and i is counted from 0.
				 */
				/*
				 * 1st see if you have MIN_PTS_TO_CORR
				 * in this monotonic offset run.
				 */
				if ((i >= first + MIN_PTS_TO_CORR - 1)
				 || (i == last_trace))
				{
					may_sub_snap_adjacent(first, i,
						num_traces, tmincur, tmaxcur,
						picks, missing_pick, zero_pick,
						traces, nsamp, tmin, dt,
						pickmode, amplitudes,
						head, nwords, pick1, pick2,
						orf, outlier);

					first = i + 1;
				}
				/*
				 * If you do not have MIN_PTS_TO_CORR,
				 * but have a big offset change or change
				 * in rcv line, use this monotonic offset
				 * run anyway.  Big changes are probably
				 * from a new group.
				 */
				else if (search == STARTING)
				{
					if (i != first)
						may_sub_snap_adjacent(first, i,
							num_traces, tmincur,
							tmaxcur, picks,
							missing_pick, zero_pick,
							traces, nsamp, tmin,
							dt, pickmode,
							amplitudes, head,
							nwords, pick1, pick2,
							orf, outlier);

					first = i + 1;
				}
				else
				{
					double off1 = head[(i-1) * nwords + 5];
					double off2 = head[ i    * nwords + 5];
					double denom = (off1 > off2) ? off1
								     : off2;

					if ((denom == 0.0F)
					 || (fabs((off2 - off1) / denom)
						> MAX_REL_DIFF))
					{
					   if (i != first)
						may_sub_snap_adjacent(first, i,
							num_traces, tmincur,
							tmaxcur, picks,
							missing_pick, zero_pick,
							traces, nsamp, tmin,
							dt, pickmode,
							amplitudes, head,
							nwords, pick1, pick2,
							orf, outlier);

						first = i + 1;
					}
				}

				i++;
				break;
			case MAX_SEARCH:
				for (; i < last_trace; i++)
				{
					if (head[ i    * nwords + 5]
					  < head[(i-1) * nwords + 5])
					{
						search = MIN_SEARCH;
						break;
					}

					/*
					 * If seq rcv pos out of order,
					 * probably diff rcv line in same grp.
					 */
					diff = (long) floor((double)
						(head[ i    * nwords + 46] -
						 head[(i-1) * nwords + 46] +
						 0.5F));
					if (diff < -10 || diff > 10)
					{
						search = STARTING;
						break;
					}
				}
				/*
				 * may_sub_snap_adjacent called when
				 * either < offset or out of traces.
				 * i is used in call to reference last >=
				 * since may_sub_snap_adjacent counts
				 * traces from 1 and i is counted from 0.
				 */
				/*
				 * 1st see if you have MIN_PTS_TO_CORR
				 * in this monotonic offset run.
				 */
				if ((i >= first + MIN_PTS_TO_CORR - 1)
				 || (i == last_trace))
				{
					may_sub_snap_adjacent(first, i,
						num_traces, tmincur, tmaxcur,
						picks, missing_pick, zero_pick,
						traces, nsamp, tmin, dt,
						pickmode, amplitudes,
						head, nwords, pick1, pick2,
						orf, outlier);

					first = i + 1;
				}
				/*
				 * If you do not have MIN_PTS_TO_CORR,
				 * but have a big offset change or change
				 * in rcv line, use this monotonic offset
				 * run anyway.  Big changes are probably
				 * from a new group.
				 */
				else if (search == STARTING)
				{
					if (i != first)
						may_sub_snap_adjacent(first, i,
							num_traces, tmincur,
							tmaxcur, picks,
							missing_pick, zero_pick,
							traces, nsamp, tmin,
							dt, pickmode,
							amplitudes, head,
							nwords, pick1, pick2,
							orf, outlier);

					first = i + 1;
				}
				else
				{
					double off1 = head[(i-1) * nwords + 5];
					double off2 = head[ i    * nwords + 5];
					double denom = (off1 > off2) ? off1
								     : off2;

					if ((denom == 0.0F)
					 || (fabs((off2 - off1) / denom)
						> MAX_REL_DIFF))
					{
					   if (i != first)
						may_sub_snap_adjacent(first, i,
							num_traces, tmincur,
							tmaxcur, picks,
							missing_pick, zero_pick,
							traces, nsamp, tmin,
							dt, pickmode,
							amplitudes, head,
							nwords, pick1, pick2,
							orf, outlier);

						first = i + 1;
					}
				}

				i++;
				break;
			default:
				assert(0);
		}

	if (first < last_trace)
		may_sub_snap_adjacent(first, last_trace, num_traces, tmincur,
			tmaxcur, picks, missing_pick, zero_pick, traces, nsamp,
			tmin, dt, pickmode, amplitudes, head, nwords,
			pick1, pick2, orf, outlier);

	/*
	 * If first == last_trace at this point, there
	 * is a single trace not included in the last
	 * may_sub_snap_adjacent call.  That is OK
	 * because may_sub_snap_adjacent can not do
	 * anything with a single trace.
	 */

#ifndef sun
	printf("snap_adjacent in %f sec.\n",
                (float) (clock() - start) / (float) CLOCKS_PER_SEC);
#endif
}



/*------------------------- minMaxOffset --------------------------------*/

static void minMaxOffset(long first_trace, long last_trace, const float head[],
	const float all_head[], long nwords, long tot_num_traces,
	float shortMin, float shortMax, float longMin, float longMax,
	long nsamp, float tmin, float dt, float *minPicks, float *maxPicks,
        float poto, float *oto)
{
	long i;
	float offset, minOffset, maxOffset, offsetRange;

	assert(shortMin >= 0.0F && shortMax >= 0.0F
	     && longMin >= 0.0F &&  longMax >= 0.0);

	for (minOffset = maxOffset = all_head[5], i = 1;
		i < tot_num_traces; i++)
	{
		offset = all_head[i * nwords + 5];

		if (offset < minOffset)
			minOffset = offset;

		if (offset > maxOffset)
			maxOffset = offset;
	}

	offsetRange = maxOffset - minOffset;

	if (oto)
		*oto = minOffset + poto / 100.0F * offsetRange;

	if (shortMax > shortMin && longMax > longMin)
	{
		if (offsetRange > 0.0F)
		{
			float minRange = longMin - shortMin;
			float maxRange = longMax - shortMax;

			for (i = first_trace - 1; i < last_trace; i++)
			{
				offset = head[i * nwords + 5];

				minPicks[i] = shortMin + (offset - minOffset)
					/ offsetRange * minRange;

				maxPicks[i] = shortMax + (offset - minOffset)
					/ offsetRange * maxRange;
			}
		}
		else
		{
			for (i = first_trace - 1; i < last_trace; i++)
			{
				minPicks[i] = shortMin;
				maxPicks[i] = shortMax;
			}
		}
	}
	else
	{
		float minPick = tmin;
		float maxPick = tmin + (float) (nsamp - 1) * dt;

		for (i = first_trace - 1; i < last_trace; i++)
		{
			minPicks[i] = minPick;
			maxPicks[i] = maxPick;
		}
	}	
}



/*------------------------- setOffWindPick ------------------------------*/

static void setOffWindPick(long first_trace, long last_trace,
	float picks[], float *minPicks)
{
	long i;

	for (i = first_trace - 1; i < last_trace; i++)
		if (picks[i] < minPicks[i])
			picks[i] = minPicks[i];
}



/*------------------------- checkOffWindPick ----------------------------*/

static void checkOffWindPick(long first_trace, long last_trace,
	float picks[], float *maxPicks, float zero_pick)
{
	long i;
	for (i = first_trace - 1; i < last_trace; i++)
		if (picks[i] > maxPicks[i])
			picks[i] = zero_pick;
}



/*------------------------- derive_picks_spws --------------------------------*/

void derive_picks_spws(long action, long pickmode, long automode, long direction,
        float threshhold, float minSigDiff,
        long first_trace, long last_trace, long num_traces,
        float first_time, float last_time,
        float tmincur, float tmaxcur, float picks[],
        float missing_pick, float zero_pick, unsigned char traces[],
        long nsamp, float tmin, float dt, float *amplitudes,
        const float head[], long nwords, unsigned char all_traces[],
        long tot_num_traces, float all_picks[], const float all_head[],
        long index_into_all, float orf,
        float shortMin, float shortMax, float longMin, float longMax,
        float poto, float outlier)
{
#ifndef sun
 	clock_t start = clock();
#endif

  if(action == ZERO)
       {
       zero_picks(first_trace, last_trace, num_traces, picks,
                           missing_pick, zero_pick);
       return;
       }
  if(action == MANUAL)
       {
       slope_picks(first_trace, last_trace, num_traces,
                 first_time, last_time, tmincur, tmaxcur, picks,
                 missing_pick);
       return;
       }
  if(action == SNAP)
       {
       snap_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, picks, missing_pick, zero_pick,
                 traces, nsamp, tmin, dt, pickmode, amplitudes);
       return;
       }
            /* now action == AUTOMATIC or unrecognized */
  if(automode == FIRST_BREAK || automode == FIRST_BREAK_NO_SNAP ||
     automode == FIRST_BREAK_CORR)
       {
       float *minPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       float *maxPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       slope_picks(first_trace, last_trace, num_traces,
                 first_time, last_time, tmincur, tmaxcur, picks,
                 missing_pick);
       minMaxOffset(first_trace, last_trace, head, all_head, nwords,
                    tot_num_traces, shortMin, shortMax, longMin, longMax,
                    nsamp, tmin, dt, minPicks, maxPicks, poto, (float *) NULL);
       setOffWindPick(first_trace, last_trace, picks, minPicks);
       break_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, picks, missing_pick, traces, nsamp,
                 tmin, dt, pickmode, threshhold);
       checkOffWindPick(first_trace, last_trace, picks, maxPicks, zero_pick);
       if(automode == FIRST_BREAK || automode == FIRST_BREAK_CORR)
       snap_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, picks, missing_pick, zero_pick,
                 traces, nsamp, tmin, dt, pickmode, amplitudes);
       if(automode == FIRST_BREAK_CORR)
       snap_adjacent(first_trace, last_trace, num_traces, tmincur, tmaxcur,
                 picks, missing_pick, zero_pick, traces, nsamp, tmin, dt,
                 pickmode, amplitudes, head, nwords,
		(float *) NULL, (float *) NULL, orf, outlier);
       free(minPicks);
       free(maxPicks);
       }
  else if(automode == HURST_BREAK || automode == HURST_BREAK_NO_SNAP ||
          automode == HURST_CORR)
       {
       float *minPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       float *maxPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       zero_picks(first_trace, last_trace, num_traces, picks,
                 missing_pick, zero_pick);
       minMaxOffset(first_trace, last_trace, head, all_head, nwords,
                    tot_num_traces, shortMin, shortMax, longMin, longMax,
                    nsamp, tmin, dt, minPicks, maxPicks, poto, (float *) NULL);
       if (hurst_break)
            (*hurst_break)(first_trace, last_trace, num_traces,
                      tmincur, tmaxcur, picks, missing_pick, zero_pick,
                      traces, nsamp, tmin, dt, minSigDiff, minPicks, maxPicks);
       if(automode == HURST_BREAK || automode == HURST_CORR)
       snap_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, picks, missing_pick, zero_pick,
                 traces, nsamp, tmin, dt, pickmode, amplitudes);
       if(automode == HURST_CORR)
       snap_adjacent(first_trace, last_trace, num_traces, tmincur, tmaxcur,
                 picks, missing_pick, zero_pick, traces, nsamp, tmin, dt,
                 pickmode, amplitudes, head, nwords,
		(float *) NULL, (float *) NULL, orf, outlier);
       free(minPicks);
       free(maxPicks);
       }
  else if(automode == COMBO || automode == COMBO_CORR)
       {
       float *thresholdPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       float *hurstPicks     =
         (float *) malloc((size_t) num_traces * sizeof(float));
       float *minPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       float *maxPicks =
         (float *) malloc((size_t) num_traces * sizeof(float));
       long i, agree;
       float oto;

       memcpy(thresholdPicks, picks, (size_t) num_traces * sizeof(float));
       memcpy(    hurstPicks, picks, (size_t) num_traces * sizeof(float));

       slope_picks(first_trace, last_trace, num_traces,
                 first_time, last_time, tmincur, tmaxcur, thresholdPicks,
                 missing_pick);

       minMaxOffset(first_trace, last_trace, head, all_head, nwords,
                    tot_num_traces, shortMin, shortMax, longMin, longMax,
                    nsamp, tmin, dt, minPicks, maxPicks, poto, &oto);
       setOffWindPick(first_trace, last_trace, thresholdPicks, minPicks);
       break_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, thresholdPicks, missing_pick, traces, nsamp,
                 tmin, dt, pickmode, threshhold);
       checkOffWindPick(first_trace, last_trace, thresholdPicks, maxPicks,
                        zero_pick);
       snap_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, thresholdPicks, missing_pick, zero_pick,
                 traces, nsamp, tmin, dt, pickmode, amplitudes);

       zero_picks(first_trace, last_trace, num_traces, hurstPicks,
                 missing_pick, zero_pick);

       if (hurst_break)
            (*hurst_break)(first_trace, last_trace, num_traces,
                      tmincur, tmaxcur, hurstPicks, missing_pick, zero_pick,
                      traces, nsamp, tmin, dt, minSigDiff, minPicks, maxPicks);
       snap_picks(first_trace, last_trace, num_traces,
                 tmincur, tmaxcur, hurstPicks, missing_pick, zero_pick,
                 traces, nsamp, tmin, dt, pickmode, amplitudes);

       for(agree = 0, i = first_trace - 1; i < last_trace; i++)
       {
            if (head[i * nwords + 5] < oto)
            {
                 picks[i] = thresholdPicks[i];
            }
            else if (thresholdPicks[i]!= zero_pick && hurstPicks[i]!= zero_pick
             && fabs((double)(thresholdPicks[i] - hurstPicks[i])) < (double) dt)
            {
                 picks[i] = thresholdPicks[i];
                 agree++;
            }
            else
            {
                 picks[i] = zero_pick;
            }
       }

       printf("derive_picks_spws combo:  %ld of %ld agree\n", agree, num_traces);

       if(automode == COMBO_CORR)
       snap_adjacent(first_trace, last_trace, num_traces, tmincur, tmaxcur,
                 picks, missing_pick, zero_pick, traces, nsamp, tmin, dt,
                 pickmode, amplitudes, head, nwords,
		 thresholdPicks, hurstPicks, orf, outlier);

       free(thresholdPicks);
       free(    hurstPicks);
       free(      minPicks);
       free(      maxPicks);
       }
  else if(automode == PICK_CORR)
       {
         if (num_traces == 1)
         {
		long bracket1, bracket2;

		get_bracketing(1, tot_num_traces, tot_num_traces,
			all_head, nwords, index_into_all + 1,
			&bracket1, &bracket2);

		zero_picks(bracket1, bracket2, tot_num_traces, all_picks,
			missing_pick, zero_pick);

		slope_picks(index_into_all + 1, index_into_all + 1,
			tot_num_traces, first_time, last_time,
			tmincur, tmaxcur, all_picks, missing_pick);

		snap_picks(index_into_all + 1, index_into_all + 1,
			tot_num_traces, tmincur, tmaxcur, all_picks,
			missing_pick, zero_pick, all_traces, nsamp, tmin, dt,
			pickmode, amplitudes);

		sub_snap_adjacent(bracket1, bracket2, tot_num_traces, tmincur,
			tmaxcur, all_picks, missing_pick, zero_pick, all_traces,
			nsamp, tmin, dt, pickmode, amplitudes, all_head,
			nwords, orf);
         }
         else
         {
		long picked_trace = first_trace;
		float picked_time = first_time ;

		if (direction == BACKWARD)
		{
			picked_trace = last_trace;
			picked_time  = last_time ;
		}
		else
		{
       			assert(direction == FORWARD);
		}

		zero_picks(first_trace, last_trace, num_traces, picks,
			missing_pick, zero_pick);

		slope_picks(picked_trace, picked_trace, num_traces,
			picked_time, picked_time, tmincur, tmaxcur, picks,
			missing_pick);

		snap_picks(picked_trace, picked_trace, num_traces,
			tmincur, tmaxcur, picks, missing_pick, zero_pick,
			traces, nsamp, tmin, dt, pickmode, amplitudes);

		sub_snap_adjacent(first_trace, last_trace, num_traces, tmincur,
			tmaxcur, picks, missing_pick, zero_pick, traces,
			nsamp, tmin, dt, pickmode, amplitudes, head, nwords,
			orf);
         }
       }
  else if(automode == CORRELATE)
       {
       snap_adjacent(1, tot_num_traces, tot_num_traces, tmincur, tmaxcur,
                 all_picks, missing_pick, zero_pick, all_traces, nsamp, tmin,
                 dt, pickmode, amplitudes, all_head, nwords,
		(float *) NULL, (float *) NULL, orf, outlier);
       }
  else
       {
       adjust_pick(first_trace, &first_time, num_traces,
               tmincur, tmaxcur, picks, missing_pick, zero_pick,
               traces, nsamp, tmin, dt, pickmode);
       adjust_pick(last_trace, &last_time, num_traces,
               tmincur, tmaxcur, picks, missing_pick, zero_pick,
               traces, nsamp, tmin, dt, pickmode);
       follow_picks(first_trace, last_trace, num_traces,
                 first_time, last_time,
                 tmincur, tmaxcur, picks, missing_pick,
                 zero_pick, traces, nsamp,
                 tmin, dt, pickmode, automode, direction, amplitudes);
       }

#ifndef sun
	printf("derive_picks_spws in %f sec.\n\n",
                (float) (clock() - start) / (float) CLOCKS_PER_SEC);
#endif
}




/*-------------------------- snap_picks ---------------------------------*/

void snap_picks(long first_trace, long last_trace, long num_traces,
                float tmincur, float tmaxcur,
                float picks[], float missing_pick, float zero_pick,
                unsigned char traces[],
                long nsamp, float tmin, float dt, long pickmode,
                float *amplitudes)
{
  int tn;

  for(tn = first_trace; tn <= last_trace; tn++)
       {
       super_snap_pick(tn, num_traces, tmincur, tmaxcur,
                 picks, missing_pick, zero_pick,
                 traces, nsamp, tmin, dt, pickmode, amplitudes);
       }
}


 

/*-------------------------- snap_pick ----------------------------------*/

void snap_pick(float *pick, unsigned char trace[],
                long nsamp, float tmin, float dt, long pickmode,
                float *amplitude)
{
  int irang = 20, interp = 2;
  float xsam, smax, datmax;

  xsam = (*pick - tmin) / dt;
  if     (xsam < 0     ) xsam = 0;
  else if(xsam >= nsamp) xsam = nsamp - 1;
  stat_sear(pickmode, irang, interp, nsamp, xsam, &smax, &datmax, trace);
  if     (smax < 0     ) smax = 0;
  else if(smax >= nsamp) smax = nsamp - 1;
  *pick = tmin + smax * dt;
  if(amplitude) *amplitude = datmax;
}

 


/*-------------------------- stat_sear ----------------------------------*/

/*  This routine, written by Paul Hauge, has been copied unaltered from */
/*  stat_pick.c (in cbyt) and placed in this utility for general use.  */

/*         
 *    stat_sear( apik->attr, apik->rang, apik->nterp, image->Cl.nsamp,
 *                xsam, &smax, &datmax, &image->byte_array[ipnt] ) ;
 */
     
void stat_sear( int ipol, int irang, int interp, int nrecl, 
       float xsam, float *smax, float *datmax, unsigned char jdat[] ) 
{
     float  /*temp, dummy,*/ con, tinue ;
     int    isam, /*isam1, isam2,*/ isamx ;
     int    izflag, ii, jj, nrecm ;
     int    idat, imid, is, inc ;
     int    idats, idif ;
     float  dat1, dat2, dat3, xnum, xden, ratio ;
     float  xmid, adat1, adat2, asum, xinc, xs ;

     tinue =  0.0 ;
     isam  = (int)(xsam + 0.5 ) ; 
     nrecm = nrecl - 1 ;

     if( isam < 1 )
       { isam = 1 ; }
     else if ( isam >= nrecm ) 
       { isam = nrecm - 1 ; }
/*
 *        Check to ses if isam1 and isam2 are used anywhere  - PSH
 *
 *   isam1  =  isam - irang ; 
 *   isam2  =  isam + irang ;
 *
 *   if( isam1 < 0 )     isam1 =  0 ;
 *   if( isam2 > nrecm ) isam2 = nrecm ;
 */
     isamx  =    isam ;
     *smax  =  (float)( isam ) ;
/*
 *      Check for Zero Trace 
 *        Search backwards, which will be faster for top-muted traces.
 *             izflag = 1 for zero traces,  
 *                    = 2 for non-zero traces.
 */
      imid  = 128 ;
      izflag = 2 ;
      jj = nrecl ;
      for( ii = 0 ; ii < nrecl ; ii ++ ) {
        jj = jj - 1 ;
        idat = (int) jdat[jj] ;
        if( idat != imid )  goto a50 ;
       }
        izflag = 1 ;
      *smax    =  -1.0 ;
      *datmax  =   0.0 ;
      goto a900 ;
  a50:  con=tinue ;
/*
 *  Trace has some non-zero data, so do auto-picking
 *    if ipol falls through the following checks, jump out of routine
 */
     if( ipol == 1 ) goto a100 ;
     else if( ipol == -1 ) goto a150 ;
     else if( ipol ==  2 ) goto a200 ;
     else if( ipol == -2 ) goto a250 ;
     else  goto a900 ;

/*
 *        ipol = 1, so search ( or grep ) for peak.
 */
     
  a100:  con=tinue ;
      is  =  isam ;
      idats = (int) jdat[is] ;

        idif = (int)( jdat[isam+1] ) - (int)( jdat[isam-1] ) ;
        if( idif < 0 ) inc = -1 ;
        else if( idif > 0 ) inc = 1 ;
        else {
          if( idats >= (int)( jdat[isam+1] ) )
            goto a180 ;           /*       Already on a peak   */
          else
            inc = 1 ;     /*  on trough, search down for peak  */
          }
        is = is - inc ;
        for( ii = 0 ; ii < irang ; ii++ ) {
          is = is + inc ;
          if( is <= 0 || is >= nrecm )  goto a180 ;
          idif = (int)( jdat[is] ) - (int)( jdat[is+inc] ) ;
          if( idif > 0 ) goto a180 ;
            }
        is = isam ;   /*   No solution, return to starting point  */   
        goto a180 ;


     
/*
 *           ipol = -1, so search for trough
 */
  a150:  con=tinue ;
      is  =  isam ;
      idats = (int) jdat[is] ;   

        idif = (int)( jdat[isam+1] ) - (int)( jdat[isam-1] ) ;
        if( idif > 0 ) inc = -1 ;
        else if( idif < 0 ) inc = 1 ;
        else {
          if( idats <= (int)( jdat[isam+1] ) )
            goto a180 ;           /*      Already on a trough   */
          else
            inc =  1 ;    /*  on peak, search down for trough */
          }
        is = is - inc ;
        for( ii = 0 ; ii < irang ; ii++ ) {
          is = is + inc ;
          if( is <= 0 || is >= nrecm )  goto a180 ;
          idif = (int)( jdat[is] ) - (int)( jdat[is+inc] ) ;
          if( idif < 0 ) goto a180 ;
            }
        is = isam ;   /*   No solution, return to starting point  */   

   a180:  con=tinue ;
/*
 *    Now do 3-point parabolic interpolaton 
 *      between samples if requested ( interp = 2 ).
 */
      xmid = 128.0 ;
      if( interp == 1 ) {
        *smax  =  (float) is ;
        *datmax = (float) ( jdat[ is ] )  - xmid ;
      }
      else  {               /*   interp =  2  */ 
        dat1  =  (float) ( jdat[is-1] )  - xmid ;
        dat2  =  (float) ( jdat[ is ] )  - xmid ;
        dat3  =  (float) ( jdat[is+1] )  - xmid ;
        xnum  =  0.5 * ( dat3 - dat1 ) ;
        xden  =  2.0 * dat2  -  dat1  - dat3 ;
        if( xden == 0.0 ) ratio = 0.0 ;
          else  ratio  =  xnum / xden ;
        *smax =  (float)(is) + ratio ;
        *datmax =  dat2  +  0.5*xnum*ratio ;  
      }
/*
 *  printf(" a180:  ii = %d  is = %d isam = %d *smax = %f \n ",
 *                      ii, is, isam, *smax ) ;
 *
 *  printf(" a182:  dat1 = %f  dat2 = %f   dat3 = %f  \n ",
 *                      dat1, dat2, dat3 ) ;
 *
 * printf(" a185:  idif = %d, idats = %d  jdat_is = %d  \n ",
 *                                   idif, idats, jdat[is] ) ;
 */

     goto a900;

/*
 *    ipol = 2, so search for zero-crossing with positive slope.
 */
     
  a200:  con=tinue ;
      imid  =  128 ;
      is  =  isam ;
      idats = (int) jdat[is] ;
  a210:  con=tinue ;
      if( idats < imid ) {    /*   Search down for zero-crossing   */
        inc = 1 ;
        for( ii = 0 ; ii < irang ; ii++ ) {
          is = is + inc ;
          idats = (int) jdat[is] ;
          if( is >= nrecm ) goto a280 ;
          if( idats > imid ) goto a270 ;  }
      }
      else if( idats > imid ) { /*   Search up for zero-crossing   */
        inc = -1 ;
        for( ii = 0 ; ii < irang ; ii++ ) {
          is = is + inc ;
          idats = (int) jdat[is] ;
          if( is <= 0 ) goto a280 ;
          if( idats < imid ) goto a270 ;  }
      }
      else  {
        idif = (int)( jdat[is+1] ) - (int)( jdat[is-1] ) ;
        if( idif >= 0 ) goto a280;     /* Already at z-crossing */
        is = is + 1 ;                  /* Z-crossing has wrong phase */
        if( is >= nrecm ) goto a280 ;  /*  Avoid infinite loop  */
        idats  = (int) jdat[is] ;
        if( idats < imid  ||  idats > imid )  goto a210 ;
        else  goto a280 ;
      } 

/*
 *    ipol = -2, so search for zero-crossing with negative slope.
 */
     
  a250:  con=tinue ;
      imid  =  128 ;
      is  =  isam ;
      idats = (int) jdat[is] ;
  a260:  con=tinue ;
      if( idats > imid ) {   /*   Search down for zero-crossing   */
        inc = 1 ;
        for( ii = 0 ; ii < irang ; ii++ ) {
          is = is + inc ;
          idats = (int) jdat[is] ;
          if( is >= nrecm ) goto a280 ;
          if( idats < imid ) goto a270 ;  }
      }
      else if( idats < imid ) {   /*   Search up for zero-crossing   */
        inc = -1 ;
        for( ii = 0 ; ii < irang ; ii++ ) {
          is = is + inc ;
          idats = (int) jdat[is] ;
          if( is <= 0 ) goto a280 ;
          if( idats > imid ) goto a270 ;  }
      }
      else  {
        idif = (int)( jdat[is+1] ) - (int)( jdat[is-1] ) ;
        if( idif <= 0 ) goto a280;     /* Already at z-crossing */
        is = is + 1 ;                  /* Z-crossing has wrong phase */
        if( is >= nrecm ) goto a280 ;  /*  Avoid infinite loop  */
        idats  = (int) jdat[is] ;
        if( idats < imid  ||  idats > imid )  goto a260 ;
        else  goto a280 ;
      } 
  a270:  con=tinue ;
/*
 *    Now do a 2-point straight line interpolation 
 *      between sample points if requested ( interp = 2 ) .
 *
 *      First find absolute values of two points 
 *         that enclose the zero-crossing.  ( dat1 and dat2 )
 */
      xmid = 128.0 ;
      adat1 = dat1 = (float) ( jdat[  is  ] )  - xmid ;
      adat2 = dat2 = (float) ( jdat[is-inc] )  - xmid ;
      if( adat1 < 0.0 ) adat1 = - adat1 ;
      if( adat2 < 0.0 ) adat2 = - adat2 ;
      xinc  =  (float) inc ;

      asum  =  adat1 + adat2 ;
      if( asum > 0.0 ) {
        ratio = adat1 / asum ;
        xs  =  (float) is - xinc * ratio ; }
      else
        xs  =  (float) is - 0.5*xinc ;
/*
 *   Interpolate for interp = 2 
 */
      if( interp == 1 )  *smax  =  (float) is ;
      else                   *smax  =  xs ;
/* 
 *   Set datmax proportional to slope
 *             for both interp = 1 or 2 for time being.
 */
      if( inc > 0 )      *datmax = dat1 - dat2 ;
      else if( inc < 0 ) *datmax = dat2 - dat1 ;
      else               *datmax = 0.0 ;
/*
 *  printf(" a182:  dat1 = %f  dat2 = %f   datmax = %f  \n ",
 *                      dat1, dat2, *datmax ) ;
 */
      goto a900 ;

  a280:  con=tinue ;
        *smax  =  (float) is ;
        *datmax =  0.0 ;

/*    goto a900    */




  a900:  con=tinue ; 

/*
 *printf(" stat_sear:  xsam = %f  isam = %d  *smax = %f  \n ",
 *                                 xsam, isam, *smax ) ;
 */
}
