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
#ifndef SHIFT_HH
#define SHIFT_HH


#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "seis_shift_pop.hh"

//type of last shift
#define LINEAR_SHIFT 0
#define NONLINEAR_SHIFT 1


//relative menu class
class SeisShiftPop;


class SeisShift : public SeisInform {

   private:

   protected:
     SeisPlot           *_sp;
     Boolean            _can_scan_shift;
     long               _num_shifts;
     float              *_bytes_floated;
     float              *_floats_shifted;
     int                _header;
     float              *_shifts;
     float              _velocity;
     float              _divisor;
     float              _flatten_to_time;
     Boolean            _reverse;
     Boolean            _forward;
     Boolean            _data_shifted;
     Boolean            _shift_reploted;
     int                _shift_type;
     SPList             _list;
     void noShift();
     void setShiftInfo(Boolean forward);
     void setDivisor(float d)          {_divisor = d;}
     void setReverseApplied(Boolean a) {_reverse = a;}
     void setForwardApplied(Boolean a) {_forward = a;}
     void addSP(SeisPlot *sp);

   public:
     SeisShift(SeisPlot *sp, float velocity = 5000.0, int header = 6);
     virtual ~SeisShift();

       // seis_inform functions
     virtual void newPlot(SeisPlot *sp);
     virtual void preScan(SeisPlot *sp, SeisPlot::ScanDir );
     virtual void postScan(SeisPlot *sp ,SeisPlot::ScanDir);
     virtual void noPlotDisplayed(SeisPlot *sp);
     virtual void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *separate_sp);
     
       // for velocity type shifting
     virtual Boolean linearShift(Boolean forward = True,
                                 Boolean make_plot = True);

       // for header word or other independent type shifting
     virtual Boolean nonlinearShift(Boolean forward, float *shifts_in = NULL, 
                                 Boolean make_plot = True);

       // remove all current shifts
     Boolean removeShift(Boolean replot = True);

       // returns the total shift of a memory trace 1-ntraces
     float getShift(long memtrace);

       // set and get the time to flatten data to
     void setFlattenTime(float t)      {_flatten_to_time = t;}
     float getFlattenTime()            {return _flatten_to_time;}

       // change and get the velocity used in linear shifting
     void changeVelocity(float v)      {_velocity = v;}
     float getVelocity()               {return _velocity;}

       // change and get a header to reference for a shift value
     void changeHeader(int h)          {_header = h;}
     int getHeader()                   {return _header;}

       // status of data 
     Boolean dataShifted()             {return _data_shifted;}
     Boolean reverseApplied()          {return _reverse;}
     Boolean forwardApplied()          {return _forward;}
     int getShiftType()                {return _shift_type;}
};



#endif










