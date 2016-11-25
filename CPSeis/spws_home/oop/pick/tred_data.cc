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
//---------------------- tred_data.cc ------------------------//
//---------------------- tred_data.cc ------------------------//
//---------------------- tred_data.cc ------------------------//

//  This class is derived from VectXdata.
//  This class minimizes index modification in the Trace Edit application.
//  A critical assumption in the use of this class is that the incoming x
//    arrays are sorted in ascending order and remain that way through the
//    life of the object!!!
//  This class was written by Kruger Corn on 9/13/94.

#include "pick/tred_data.hh"

#include <string.h>
#include <assert.h>

//------------- constructor and destructor -------------------//
//------------- constructor and destructor -------------------//
//------------- constructor and destructor -------------------//

TredData::TredData (int numPts, float *x, float y, long id)
	: VectXdata (numPts, x, y, id)
{
}

TredData::~TredData()
{
}

//---------------------- new functions -------------------------//
//---------------------- new functions -------------------------//
//---------------------- new functions -------------------------//

// replace new sorted data with previous sorted data while minimizing index
//   modification
void TredData::replace (int numPts, float *x, float epsilon)
{
  if (numPts || _numPts)
    {
      if (epsilon < 0)
        epsilon = -epsilon;
      int k2 = 0; // initialize the old data points index
      int k3 = 0; // initialize the new data points index
      int numIns = 0;
      int insTo = 0;
      int insFrom = 0;
      int numRem = 0;
      int remTo = 0;

      while (k2 < _numPts)
	{
          if (numPts > k3)  // there are still new points to compare
	    {
              if (x[k3] >= _xData[k2]-epsilon &&
                x[k3] <= _xData[k2]+epsilon) // new point matches old point
                {
                  if (numIns > 0) // previous points to insert exist
                    tredInsert (&k2, &numIns, x+insFrom, insTo);
                  else if (numRem > 0) // previous points to remove exist
                    tredRemove (&k2, &numRem, remTo);
                  k2++, k3++; // move to the next old and next new indices
		}
              else if (x[k3] < _xData[k2]-epsilon) // new point .LT. old point
		{
                  if (numRem > 0) // previous points to remove exist
                    tredRemove (&k2, &numRem, remTo);
                  if (numIns == 0) // initialize the insertion of new points
		    {
                      insTo = k2;
                      insFrom = k3;
		    }
                  numIns++; // increment the insertion counter
		  k3++; // move to the next new point and leave old point fixed
		}
              else // if (x[k3] > _xData[k2]+epsilon) // new point .GT. old pnt
		{
                  if (numIns > 0) // previous points to insert exist
                    tredInsert (&k2, &numIns, x+insFrom, insTo);
                  if (numRem == 0) // initialize the removal of old points
                    remTo = k2;
                  numRem++; // increment the removal counter
                  k2++; // move to the next old point and leave new point fixed
		}
	    }
          else // if (numPts <= k3) // no more new points to compare
	    {
              if (numIns > 0) // new points to insert exist
                tredInsert (&k2, &numIns, x+insFrom, insTo);
              else if (numRem > 0) // old points to remove exist
                tredRemove (&k2, &numRem, remTo);
              remove (k2, _numPts-k2); // remove rest of old points
              k2 = _numPts; // cause the for loop to quit
	    }
	}

      if (numIns > 0) // new points to insert exist
        tredInsert (&k2, &numIns, x+insFrom, insTo);
      else if (numRem > 0) // old points to remove exist
        tredRemove (&k2, &numRem, remTo);
      if (numPts > k3) // there are new points left over
        insert (_numPts, numPts-k3, x+k3); // insert rest of new pts
    }
}

void TredData::insertValueRange (float strt_vlu, float end_vlu, float dx,
  float epsilon)
{
  if (strt_vlu > end_vlu)
    {   // added by Tom Stoeckley 12/22/94 to satisfy HP
    float temp = end_vlu, end_vlu = strt_vlu, strt_vlu = temp;
    }   // added by Tom Stoeckley 12/22/94 to satisfy HP
  if (dx < 0)
    dx = -dx;
  if (epsilon < 0)
    epsilon = -epsilon;
  assert (dx > 0);
  float pts = (end_vlu - strt_vlu + dx) / dx;
  long numPts = (long) pts;

  float *x = new float[numPts];
  float vlu = strt_vlu;
  int k2;
  for (k2 = 0; k2 < numPts; k2++)
    x[k2] = vlu, vlu += dx;

      k2 = 0; // initialize the old data points index
  int k3 = 0; // initialize the new data points index
  int numIns = 0;
  int insTo = 0;
  int insFrom = 0;

  while (k2 < _numPts)
    {
      if (numPts > k3)  // there are still new points to compare
        {
          if (x[k3] >= _xData[k2]-epsilon &&
            x[k3] <= _xData[k2]+epsilon) // new point matches old point
            {
              if (numIns > 0) // previous points to insert exist
                tredInsert (&k2, &numIns, x+insFrom, insTo);
              k2++, k3++; // move to the next old and next new indices
	    }
          else if (x[k3] < _xData[k2]-epsilon) // new point .LT. old point
	    {
              if (numIns == 0) // initialize the insertion of new points
		{
                  insTo = k2;
                  insFrom = k3;
		}
              numIns++; // increment the insertion counter
	      k3++; // move to the next new point and leave old point fixed
	    }
          else // if (x[k3] > _xData[k2]+epsilon) // new point .GT. old pnt
	    {
              if (numIns > 0) // previous points to insert exist
                tredInsert (&k2, &numIns, x+insFrom, insTo);
              k2++; // move to the next old point and leave new point fixed
	    }
	}
      else // if (numPts <= k3) // no more new points to compare
	{
          if (numIns > 0) // new points to insert exist
            tredInsert (&k2, &numIns, x+insFrom, insTo);
          k2 = _numPts; // cause the for loop to quit
	}
    }
  if (numIns > 0) // new points to insert exist
    tredInsert (&k2, &numIns, x+insFrom, insTo);
  if (numPts > k3) // there are new points left over
    insert (_numPts, (int)(numPts-k3), x+k3); // insert rest of new pts
        // second arg above cast to int by Tom Stoeckley 12/22/94 for HP

  delete [] x;
}

void TredData::deleteValueRange (float strt_vlu, float end_vlu, float dx,
  float epsilon)
{
  if (strt_vlu > end_vlu)
    {   // added by Tom Stoeckley 12/22/94 to satisfy HP
    float temp = end_vlu, end_vlu = strt_vlu, strt_vlu = temp;
    }   // added by Tom Stoeckley 12/22/94 to satisfy HP
  if (dx < 0)
    dx = -dx;
  if (epsilon < 0)
    epsilon = -epsilon;
  assert (dx > 0);
  float pts = (end_vlu - strt_vlu + dx) / dx;
  long numPts = (long) pts;

  float x = strt_vlu;

  int k2 = 0; // initialize the old data points index
  int k3 = 0; // initialize the delete data points index
  int numRem = 0;
  int remTo = 0;

  while (k2 < _numPts)
    {
      if (numPts > k3)  // there are still delete points to compare
        {
          if (x >= _xData[k2]-epsilon &&
            x <= _xData[k2]+epsilon) // delete point matches old point
            {
              if (numRem == 0) // initialize the removal of old points
                remTo = k2;
              numRem++; // increment the removal counter
              k2++, k3++, x+=dx; // move to the next old & next del indices
	    }
          else // if (x < _xData[k2]-epsilon ||
//          x > _xData[k2]+epsilon) // delete point .NE. old pnt
	    {
              if (numRem > 0) // previous points to remove exist
                tredRemove (&k2, &numRem, remTo);
              k3++, x+=dx; // move to next del pnt and leave old pnt fixed
	    }
	}
      else // if (numPts <= k3) // no more delete points to compare
	{
          if (numRem > 0) // old points to remove exist
            tredRemove (&k2, &numRem, remTo);
          k2 = _numPts; // cause the for loop to quit
        }
    }
  if (numRem > 0) // old points to remove exist
    tredRemove (&k2, &numRem, remTo);
}

// Get the number of sorted segments that have data gaps greater than
//   dx+epsilon
int TredData::getNumSegs (float dx, float epsilon)
{
  int k2, segNum;

  if (_numPts > 0)
    {
      segNum = 1;
      for (k2 = 1; k2 < _numPts; k2++)
        {
          if (_xData[k2] > _xData[k2-1]+dx+epsilon)
	    segNum++;
        }
    }
  else // if (_numPts <= 0)
    segNum = 0;
  return segNum;
}  

// Get the start and end values for each sorted segment where a segment
//   break is defined at a place where data gaps exceed dx+epsilon.
// numSegs is input as the maximum number of segments allowable.
// Should the actual number of segments exceed the original numSegs,
//   then a false condition will be returned with no more than the
//   original numSegs.
// Should the actual number of segments be less than the original numSegs,
//   then a true condition will be returned and a numSegs will be returned
//   with the actual number of segments.
int TredData::getSegments (int *numSegs, float *strt_vlus,
  float *end_vlus, float dx, float epsilon)
{
  if (*numSegs <= 0)
    return 0;
  int end = getNumSegs (dx, epsilon);
  if (end <= 0)
    { // bracket added by Tom Stoeckley 12/22/94 because of HP complaint
    *numSegs = 0;
    return -1;
    } // bracket added by Tom Stoeckley 12/22/94 because of HP complaint
  if (end == 1)
    {
      *numSegs = 1;
      *strt_vlus = _xData[0];
      *end_vlus = _xData[_numPts-1];
      return -1;
    }
  end = 0;
  *strt_vlus = _xData[0];
  for (int k2 = 1; k2 < _numPts; k2++)
    {
      if (_xData[k2] > _xData[k2-1]+dx+epsilon)
        *(end_vlus+end) = _xData[k2-1];
      if (end+1 == *numSegs)
        {
          end++;
          *numSegs = end;
          return 0;
	}
      end++;
      *(strt_vlus+end) = _xData[k2];
    }
  *(end_vlus+end) = _xData[_numPts-1];
  end++;
  *numSegs = end;
  return -1;
}

// Insert new sorted data into previous sorted data
void TredData::tredInsert (int *ptIndx, int *numIns, float *x, int insTo)
{
  insert (insTo, *numIns, x);
  *ptIndx += *numIns;
  *numIns = 0;
}
// Remove sorted data from previous sorted data
void TredData::tredRemove (int *ptIndx, int *numRem, int remTo)
{
  remove (remTo, *numRem);
  *ptIndx -= *numRem;
  *numRem = 0;
}

//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//





















