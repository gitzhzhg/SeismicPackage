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
#ifndef AMPLITUDE_PROCESSOR_HH
#define AMPLITUDE_PROCESSOR_HH

class Resampler;

class AmplitudeProcessor {

public:
  AmplitudeProcessor ();			// constructor

  virtual ~AmplitudeProcessor ();		// destructor

  void setMinimum				// set the minimum given the
    (float minimum);				//   minimum amplitude

  void setMaximum				// set the maximum given the
    (float maximum);				//   maximum amplitude

  void setLevelsUnique				// set levels unique flag
    (int levels_unique = 1);			//   given the flag

  int setLevelMinimum				// set the minimum given the
    (int level,					//   level to apply it to and
     float minimum);				//   the minimum amplitude

  void setLevels				// set # of amplitude levels
    (int levels);				//   given the # of amplitudes

  void setCellsPerLevel				// set # of cells per level
    (int cells);				//   given # of cells

  float minimum ();				// return minimum

  float maximum ();				// return maximum

  int levels ()					// rtn # of amplitude levels
    { return _levels; }

  int cellsPerLevel ()				// rtn # of cells per level
    { return _cells; }

  virtual float minimumOfLevel			// return minimum of the
    (int level);				//   given level

  virtual float maximumOfLevel			// return maximum of the
    (int level);				//   given level

  virtual float middleOfLevel			// return middle of the
    (int level);				//   given level

  virtual float valueOfLevel			// return value of levl + frsc
    (float level);				//   given level

  int levelOfValue				// return level of
    (float value);				//   given value

protected:
  virtual int init ();				// initialize parameters

  virtual int initNonuniqueLevels ();		// init when levels nonunique

  virtual int initUniqueLevels ();		// init when levels are unique

  void fini ();					// free arrays and objects

  void finiNonuniqueLevels ();			// fini when levels nonunique

  void finiUniqueLevels ();			// fini when levels are unique

  virtual int checkMinimums ();			// adapt min arrays if nec 

  void sortExtrema ();				// sort extrema if necessary

  virtual int reconcileMinimums ();		// reconcile min arrays

  Resampler
    *_level_table,				// table of levels
    **_cell_tables;				// ptrs to tables of cells

  float
    _cell_size,					// size of an interlevel cell
    _minimum,					// minimum amplitude
    _maximum,					// maximum amplitude
    *_minimums;					// minimums for ea level

  int
    _levels_unique,				// flag!=0 if levls are unique
    *_min_sets,					// flags set for ea min set
    _min_count,					// size of _minimums array
    _ready,					// parameters ready flag
    _cells,					// # of color levels
    _levels;					// # of color levels

};

#endif
