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
// class that processes amplitude extrema for the color bar builder like CPS
#include "color/cbb_cps_amp_proc.hh"
#include "color/resampler.hh"

CBBCPSAmpProc::CBBCPSAmpProc () :
  AmplitudeProcessor ()
{
}

CBBCPSAmpProc::~CBBCPSAmpProc ()
{
}

float CBBCPSAmpProc::minimumOfLevel (int level)
{
  if (!_ready) _ready = init ();
  if (_levels_unique) {
    return _cell_tables[level]->value(_cells-1);
  }
  else {
    return _level_table->value(level) + _cell_tables[0]->value(_cells-1);
  }
}

float CBBCPSAmpProc::maximumOfLevel (int level)
{
  if (!_ready) _ready = init ();
  if (_levels_unique) {
    return _cell_tables[level]->value (0);
  }
  else {
    return _level_table->value(level);
  }
}

float CBBCPSAmpProc::valueOfLevel (float level)
{
  if (!_ready) _ready = init ();
  float fract = level - (float)((int)level);
  int cells = _cells - (int)(fract * _cells + (float).5) - 1;
  if (_levels_unique) {
    return _cell_tables[(int)level]->value(cells);
  }
  else {
    return _level_table->value((int)level) + _cell_tables[0]->value(cells);
  }
}

int CBBCPSAmpProc::initNonuniqueLevels ()
{
// It is assumed that a_max corresponds to the maximum value in the next
//   to the highest color region (seems like it should correspond to the
//   middle value in the highest color region!) 
// It is assumed that a_min corresponds to the maximum value in the lowest
//   color region (seems like it should correspond to the middle value in
//   the lowest color region!)
// This convention although questionable is chosen so as to be compatible with
//   CPS precedent.  It seems to me that this is an unecessary burden on
//   the user who wants specified trace values to be in the middle of a single
//   color range at the time the user defines the color bar.  Also in a # of
//   colors challenged Xterm world, for all practical purposes this approach
//   wastes two colors.

  if (_level_table || _cell_tables) return 0;

  sortExtrema ();

// create table of levels
  float real_min, real_max, incr;

  if (_levels < 3) {
    real_min = _minimum;
    real_max = _maximum;
    if (_levels < 2) {
      incr = (_maximum - _minimum);
      if (_levels < 1) _levels = 1;
    }
    else {
      incr = (_maximum - _minimum) / (float)(_levels - 1);
    }
  }
  else {
    incr = (_maximum - _minimum) / (float)(_levels - 2);
    real_min = _minimum;
    real_max = _maximum + incr; 
  }

  _level_table = new Resampler (real_min, incr, _levels, Resampler::LINEAR,
    (float)1, real_max + incr, real_min);

// create table of cells within each level
  if (_cells < 1) {
    incr = (float)0;
    real_min = (float)0;
    _cells = 1;
  }
  else {
    incr = -incr / (float)_cells;
    real_min = (_cells - 1) * incr;
  }

  _cell_tables = new Resampler*[1];
  _cell_tables[0] = new Resampler ((float)0, incr, _cells, Resampler::LINEAR,
    (float)1, (float)0, real_min + incr);

  return 1;
}

int CBBCPSAmpProc::initUniqueLevels ()
{
  if (_level_table || _cell_tables || _min_count != _levels-1) return 0;

  sortExtrema ();

// create table of levels
  float incr, real_min, real_max;

  if (_levels < 3) {
    real_min = _minimum;
    real_max = _maximum;
    if (_levels < 2) {
      incr = (_maximum - _minimum);
      if (_levels < 1) _levels = 1;
    }
    else {
      incr = (_maximum - _minimum) / (float)(_levels - 1);
    }
  }
  else {
    incr = (_maximum - _minimum) / (float)(_levels - 2);
    real_min = _minimum;
    real_max = _maximum + incr; 
  }

  _level_table = new Resampler (real_min, incr, _levels, Resampler::LINEAR,
    (float)1, real_max + incr, real_min);

  if (_cells < 1) return 0;

// remember the original extrema
  float old_minimum = _minimum;
  float old_maximum = _maximum;

// set the extrema for reconciling the set minimums
  _minimum = _minimum - incr;
  _maximum = _maximum + incr;
  if (!reconcileMinimums()) return 0;

// create a unique cell table for each level
  _cell_tables = new Resampler*[_levels];

  float cell_min, cell_incr, cur_min, cur_max;
  cur_max = _minimum; // assumes real min is not quite minimum
  int k2;
  for (k2 = 0; k2 < _levels; k2++) {
    cur_min = cur_max;
    if (k2 < _levels-1) {
      if (_min_sets[k2]) {
// compute the current level range
        cur_max = _minimums[k2];
        incr = cur_max - cur_min;
      }
      else {
// estimate the current level range
        incr = (_maximum - cur_max) / (float)(_levels - k2);
        cur_max = cur_min + incr;
      }
    }
    else {
// determine the final level range
      incr = _maximum - cur_max;
      cur_max = _maximum;
    }
// create a cell table for the current level range
    cell_incr = -incr / (float)_cells;
    cell_min  = cur_max + (_cells - 1) * cell_incr;
    _cell_tables[k2] = new Resampler (cur_max, cell_incr, _cells,
      Resampler::LINEAR, (float)1, cur_max, cell_min + cell_incr);
  }

// restore the original extrema
  _minimum = old_minimum;
  _maximum = old_maximum;
  return 1;
}
