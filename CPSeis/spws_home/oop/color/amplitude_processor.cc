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
// class that processes amplitudes
#include "color/amplitude_processor.hh"
#include "color/resampler.hh"
#include <stdlib.h>
#include <float.h>

AmplitudeProcessor::AmplitudeProcessor () :
  _levels_unique (0),
  _minimum       (0),
  _maximum       (1),
  _min_count     (0),
  _minimums      (0),
  _min_sets      (0),
  _levels        (32),
  _cells         (31),
  _level_table   (0),
  _cell_tables   (0),
  _ready         (0)
{
}

AmplitudeProcessor::~AmplitudeProcessor ()
{
  fini ();
}

void AmplitudeProcessor::setMinimum (float minimum)
{
  _minimum = minimum;
  _ready = 0;
}

void AmplitudeProcessor::setMaximum (float maximum)
{
  _maximum = maximum;
  _ready = 0;
}

void AmplitudeProcessor::setLevelsUnique (int levels_unique)
{
  if (_levels_unique != levels_unique) fini ();
  _levels_unique = levels_unique;
  _ready = 0;
}

int AmplitudeProcessor::setLevelMinimum (int level, float minimum)
{
  int status = 1;

  if (level > _levels-1 || level < 0) {
    status = 0;
  }
  else if (level == 0) {
    _minimum = minimum;
    _ready = 0;
  }
  else if (_levels_unique) {
// require a monotonic function regardless of the previously given
//   extrema
    float den, del;
    if (minimum < _minimum) {
      den = _levels - 1;
      if (den < 1) den = 1;
      del = (_maximum - _minimum) / den;
      if (del < 0) del *= -1;
      _minimum = minimum - del;
      _ready = 0;
    }
    else if (minimum > _maximum) {
      den = _levels - 1;
      if (den < 1) den = 1;
      del = (_maximum - _minimum) / den;
      if (del < 0) del *= -1;
      _maximum = minimum + del;
      _ready = 0;
    }
    if (checkMinimums ()) {
      _minimums[level-1] = minimum;
      _min_sets[level-1] = 1;
      _ready = 0;
    }
  else
    status = 0;
  }
  return status;
}

void AmplitudeProcessor::setLevels (int levels)
{
  _levels = levels;
  checkMinimums ();
  _ready = 0;
}

void AmplitudeProcessor::setCellsPerLevel (int cells)
{
  _cells = cells;
  checkMinimums ();
  _ready = 0;
}

float AmplitudeProcessor::minimum ()
{
  return minimumOfLevel (0);
}

float AmplitudeProcessor::maximum ()
{
  return maximumOfLevel (_levels-1);
}

float AmplitudeProcessor::minimumOfLevel (int level)
{
  if (!_ready) _ready = init ();
  if (_levels_unique) {
    return _cell_tables[level]->value(0);
  }
  else {
    return _level_table->value(level);
  }
}

float AmplitudeProcessor::maximumOfLevel (int level)
{
  if (!_ready) _ready = init ();
  if (_levels_unique) {
    return _cell_tables[level]->value(_cells-1);
  }
  else {
    return _level_table->value(level) + _cell_tables[0]->value(_cells-1);
  }
}

float AmplitudeProcessor::middleOfLevel (int level)
{
  if (!_ready) _ready = init ();
  if (_levels_unique) {
    return _cell_tables[level]->value(_cells/2);
  }
  else {
    return _level_table->value(level) + _cell_tables[0]->value(_cells/2);
  }
}

float AmplitudeProcessor::valueOfLevel (float level)
{
  if (!_ready) _ready = init ();
  float fract = level - (float)((int)level);
  int cells = (int)(fract * (float)(_cells - 1) + (float).5);
  if (_levels_unique) {
    return _cell_tables[(int)level]->value(cells);
  }
  else {
    return _level_table->value((int)level) + _cell_tables[0]->value(cells);
  }
}

int AmplitudeProcessor::levelOfValue (float value)
{
  if (!_ready) _ready = init ();

  if (_levels_unique) {
// there is an implicit assumption that the unique cell ranges are
//   monotonically increasing without overlap
    int k2;
    float val0, val1, minv, maxv=0, last_maxv;
    for (k2 = 0; k2 < _levels; k2++) {

// remember the previous maximum
      last_maxv = maxv;

// determine the current cell table extrema
      val0 = _cell_tables[k2]->value(0);
      val1 = _cell_tables[k2]->value(_cells-1);
      if (val0 > val1) {
        minv = val1;
        maxv = val0;
      }
      else {
        minv = val0;
        maxv = val1;
      }

// is it less than the least?
      if (k2 == 0 && value < minv) {
        return k2;
      }

// is it greater than the largest?
      else if (k2 == _levels-1 && value >= maxv) {
        return k2;
      }

// is it in this cell?
      else if (value >= minv && value < maxv) {
        return k2;
      }

// is it in between the last cell and this cell?
      else if (k2 != 0) {
        if (value >= last_maxv && value < minv) {
          if (value-last_maxv < minv-value) {
// the value is closer to the previous cell
            return k2-1;
          }
          else {
// the value is closer to this cell
            return k2;
          }
        }
      }
    }
  }
  else {
    return _level_table->index (value);
  }
}

int AmplitudeProcessor::init ()
{
  if (_levels_unique) return initUniqueLevels ();
  else                return initNonuniqueLevels ();
}

int AmplitudeProcessor::initNonuniqueLevels ()
{
  if (_level_table || _cell_tables) return 0;

  sortExtrema ();

// create table of levels
  float incr;

  if (_levels < 2) {
    incr = (_maximum - _minimum);
    if (_levels < 1) _levels = 1;
  }
  else {
    incr = (_maximum - _minimum) / (float)(_levels - 1);
  }

  _level_table = new Resampler (_minimum, incr, _levels,
     Resampler::LINEAR, (float)1, _maximum + incr, _minimum);

// create table of cells to be used for every level
  float cell_max;
  if (_cells < 1) {
    incr = (float)0;
    cell_max = (float)0;
    _cells = 1;
  }
  else {
    incr = incr / (float)_cells;
    cell_max = (_cells - 1) * incr;
  }

  _cell_tables = new Resampler*[1];
  _cell_tables[0] = new Resampler ((float)0, incr, _cells, Resampler::LINEAR,
    (float)1, cell_max + incr, (float)0);

  return 1;
}

int AmplitudeProcessor::initUniqueLevels ()
{
  if (_level_table || _cell_tables || _min_count != _levels-1) return 0;

  sortExtrema ();

  if (_levels < 1) _levels = 1;

  if (_cells < 1) _cells = 1;

  if (!reconcileMinimums()) return 0;

// create a cell table for each level
  _cell_tables = new Resampler*[_levels];

  float incr, cell_max, cell_incr, cur_min, cur_max = _minimum;
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
    }
// create a cell table for the current level range
    cell_incr = incr / (float)_cells;
    cell_max  = cur_min + (_cells - 1) * cell_incr;
    _cell_tables[k2] = new Resampler (cur_min, cell_incr, _cells,
      Resampler::LINEAR, (float)1, cell_max + cell_incr, cur_min);
  }

  return 1;
}

void AmplitudeProcessor::fini ()
{
  if (_levels_unique) finiUniqueLevels ();
  else                finiNonuniqueLevels ();
}

void AmplitudeProcessor::finiNonuniqueLevels ()
{
  if (_cell_tables) {
    if (_cell_tables[0]) delete _cell_tables[0], _cell_tables[0] = 0;
    delete _cell_tables, _cell_tables = 0;
  }

  if (_level_table) delete _level_table, _level_table = 0;
}

void AmplitudeProcessor::finiUniqueLevels ()
{
  if (_minimums) free (_minimums), _minimums = 0;
  if (_min_sets) free (_min_sets), _min_sets = 0;
  _min_count = 0;

  if (_cell_tables) {
    int k2;
    for (k2 = 0; k2 < _levels; k2++) {
      if (_cell_tables[k2]) delete _cell_tables[k2], _cell_tables[k2] = 0;
    }
    delete _cell_tables, _cell_tables = 0;
  }

  if (_level_table) delete _level_table, _level_table = 0;
}

int AmplitudeProcessor::checkMinimums ()
{
  if (_levels_unique && _levels > 1 && _minimums && _min_sets &&
    _min_count == _levels-1) return 1;

// delete any previous objects
  fini ();

  if (_levels_unique && _levels > 1) {
// create necessary objects
    _minimums = (float *)malloc (sizeof(float)*(size_t)(_levels-1));
    _min_sets = (int   *)malloc (sizeof(int  )*(size_t)(_levels-1));
    if (_minimums && _min_sets) {
      int k2;
      for (k2 = 0; k2 < _levels-1; k2++) _min_sets[k2] = 0;
      _min_count = _levels - 1;
      return 1;
    }
    else {
      return 0;
    }
  }
  return 1;
}

void AmplitudeProcessor::sortExtrema ()
{
// sort min and max as necessary
  if (_minimum > _maximum) {
    float temp = _minimum;
    _minimum = _maximum;
    _maximum = temp;
  }
}

int AmplitudeProcessor::reconcileMinimums ()
{
  if (_levels_unique && _levels > 1) {

    if (!_minimums || !_min_sets) return 0;

    if (_maximum <= _minimum) return 0;
    float del = (float).01 * (_maximum - _minimum) / (float)_levels;

// force sequence from minimum to maximum to be monotonically increasing
    float cur_min = _minimum;
    int k2;
    for (k2 = 0; k2 < _levels-1; k2++) {
      if (_min_sets[k2]) {
        if (_minimums[k2] <= cur_min) {
          _minimums[k2] += del;
        }
        cur_min = _minimums[k2];
      }
    }
    if (_maximum <= cur_min) _maximum += del;
  }
  return 1;
}
