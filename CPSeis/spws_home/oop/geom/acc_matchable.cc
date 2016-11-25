
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
//---------------------- acc_matchable.cc -----------------------//
//---------------------- acc_matchable.cc -----------------------//
//---------------------- acc_matchable.cc -----------------------//

//         implementation file for the AccMatchable class
//                derived from the AccBase class
//                     subdirectory oprim


#include "geom/acc_matchable.hh"
#include "geom/seis_survey.hh"
#include "geom/seis_line.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccMatchable::AccMatchable(SeisSurvey *survey)
           : AccBase(survey, 0, FALSE),
             _survey   (survey)
{
  assert(_survey);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccMatchable::~AccMatchable()
{
}



//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//

                         // private

void AccMatchable::getRange(long n, long* /*index1*/, long *index2)
{
  *index2 = n - 1;
}



//-------------------- nearly equal --------------------------//
//-------------------- nearly equal --------------------------//
//-------------------- nearly equal --------------------------//

static int nearly_equal(float a, float b)
{
  static const float tolerance = 0.001;
  return (AbsoluteValue(b - a) < tolerance);
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

      // private

void AccMatchable::fixRange(long n, long /*index1*/, long /*index2*/)
{
  if(n == 0) return;

  if(n == 1)
      {
      _survey->seisLine(0)->setFirstMatchableGP(1);
      return;
      }

  long index;


  float first_shotpoint_any_line    = FNIL;
  float shotpoint_interval_any_line = FNIL;
  for(index = 0; index < n; index++)
      {
      double value = DNIL;
      long nflags = _survey->numFlagsOnLine(index);
      if(nflags >= 2)
          {
          float first_shotpoint    = _survey->getShotpoint(index, 0);
          float second_shotpoint   = _survey->getShotpoint(index, 1);
          float min_sp_interval    = _survey->getMinShotpointIncrOnLine(index);
          float max_sp_interval    = _survey->getMaxShotpointIncrOnLine(index);
          float shotpoint_interval = second_shotpoint - first_shotpoint;
          if(first_shotpoint_any_line == FNIL && shotpoint_interval != 0.0 &&
             nearly_equal(min_sp_interval, max_sp_interval) &&
             nearly_equal(min_sp_interval, shotpoint_interval))
              {
              first_shotpoint_any_line    = first_shotpoint;
              shotpoint_interval_any_line = shotpoint_interval;
              }
          if(first_shotpoint_any_line != FNIL &&
             nearly_equal(shotpoint_interval_any_line, shotpoint_interval) &&
             nearly_equal(min_sp_interval, max_sp_interval) &&
             nearly_equal(min_sp_interval, shotpoint_interval))
              {
              value = (first_shotpoint - first_shotpoint_any_line) /
                                              shotpoint_interval_any_line;
              float value2 = (float)NearestInteger(value);
              if(!nearly_equal(value2, (float)value)) value = DNIL;
              }
          }
      _survey->seisLine(index)->setFirstMatchableGP
                                   (NearestInteger(value));
      }

  if(first_shotpoint_any_line == FNIL)
      {
      for(index = 0; index < n; index++)
          {
          _survey->seisLine(index)->setFirstMatchableGP(0);
          }
      return;
      }


  long minimum_value = INIL;
  for(index = 0; index < n; index++)
      {
      long ivalue = _survey->firstMatchableGroundPosition(index);
      if(ivalue != INIL)
          {
          if(minimum_value == INIL) minimum_value = ivalue;
          else minimum_value = MinimumValue(minimum_value, ivalue);
          }
      }

  if(minimum_value == INIL)
      {
      for(index = 0; index < n; index++)
          {
          _survey->seisLine(index)->setFirstMatchableGP(0);
          }
      return;
      }


  for(index = 0; index < n; index++)
      {
      long ivalue = _survey->firstMatchableGroundPosition(index);
      if(ivalue != INIL)
          {
          long value = ivalue - minimum_value + 1;
          _survey->seisLine(index)->setFirstMatchableGP(value);
          }
      else
          {
          _survey->seisLine(index)->setFirstMatchableGP(0);
          }
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

