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
#ifndef SEND_PICK_HH
#define SEND_PICK_HH

#include "sp/seis_inform.hh"
#include "sp/seis_plot.hh"

class SendPick : public SeisInform {
     
public:
  SendPick
    (class SLApp *sl_app,
     class SeisPlot *sp,
     int *header_list = 0, // user trace header numbers as above
     int list_size = 0);   // size of trace header list

  virtual ~SendPick ();

  virtual void newPlot
    (class SeisPlot *sp);

  virtual void dragImage
    (class SeisPlot *sp);

  virtual void post
    (class SeisPlot *sp);

  virtual void postScan
    (class SeisPlot *sp,
     SeisPlot::ScanDir dir);

  void loadHeaders
    (class SeisPlot *sp);

  int send
    (long x,
     long y);

private:
  float getHeader
    (long x,
     int header);  // one-relative

  float getZ
    (long y);

  float getAmplitude
    (long x,
     long y);

  class SLApp
    *_sl_app;

  class SeisPlot
    *_sp;

  class SendPickXY *_send_pick_xy;

  float
    *_hd;

  int
    *_header_list,
    _list_size;
};



#include "plot/pick_base.hh"

class SendPickXY : public PickBase {

public:
  SendPickXY
    (class SeisPlot *sp,
     class SendPick *s_p);

  virtual ~SendPickXY ();
           
protected:
  virtual void buttonAny
    (int x1,
     int x2,
     int y1,
     int y2,
     int button,
     Action action,
     Modifier modifier);

private:
  class SendPick
    *_s_p;
};

#endif
