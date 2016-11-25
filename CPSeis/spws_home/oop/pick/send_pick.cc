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
#include "pick/send_pick.hh"

#include "sl/sl_app.hh"
#include "ipc/ipc_io.hh"
#include "ipc/ipc_constants.hh"
#include "image_amplitude_recovery.hh"

#include <stdlib.h>
#include <stdio.h>

static int def_header_list[] = { IPC::HDR_MIDPOINT_XGRID,
  IPC::HDR_MIDPOINT_YGRID };
static char *picking_mode = "Mode: Send Pick";
static const char * const help_token = "SENDPICK";
static char *sendpickhelp= "mouse*SENDPICK:  BTN#1: Send Pick, \
BTN#2: None, BTN#3: None";

SendPick::SendPick (SLApp *sl_app, SeisPlot *sp, int *header_list,
  int list_size):
  SeisInform (sp),
  _sl_app       (sl_app),
  _sp           (sp),
  _header_list  (header_list),
  _hd           (NULL)
{
  if (!_header_list) {
    _header_list = def_header_list;
    _list_size = sizeof (def_header_list) / sizeof (int);
  }
  loadHeaders (_sp);
  _send_pick_xy = new SendPickXY (_sp, this);
}

SendPick::~SendPick ()
{
  if (_hd) free (_hd);
  if (_send_pick_xy) delete (_send_pick_xy);
}

void SendPick::newPlot (SeisPlot *sp)
{
  loadHeaders (sp);
}

void SendPick::dragImage (SeisPlot *sp)
{
  post (sp);
}

void SendPick::post (SeisPlot *sp)
{
  if (sp->imageIsDisplayed() && _hd == NULL) {
    loadHeaders (sp);
  }
}

void SendPick::postScan (SeisPlot *sp, SeisPlot::ScanDir)
{
  loadHeaders (sp);
}

// this grabs all the headers for each trace in the plot
void SendPick::loadHeaders(SeisPlot *sp)
{
  long headersize, i, images;
  const float *sphd = sp->headers(); 

  if (sp->imageIsDisplayed()) {

    images = sp->movie() ? sp->frames() : 1;

    headersize
      = max (sp->numHeaders(), (PlotImage::MAXHDRS *(2*sizeof(double))));

    headersize += (sp->plottedNplt() * images * headersize);
  
    if (_hd == NULL) {
      _hd = (float *)calloc (1, (unsigned int)headersize);
    }
    else {
      _hd  = (float *)realloc (_hd, (unsigned int)headersize);
    }

    if (_hd != NULL) {

      for (i = 0; i < (sp->numHeaders()*sp->plottedNplt()*images); i++) {
	_hd[i] = sphd[i];
	if (_hd[i] < 0.0) {
	  _hd[i] = (-_hd[i]); // Change negative offsets to positive
	}
      }
    }
    else {
      printf ("error in loadheaders allocation\n");
    }
  }
}

int SendPick::send (long x, long y)
{
  int retval;

  if (_sp->imageIsDisplayed()) {
    // use the SLApp to send a packet of the shotpoint, Z, & section info
    char buf[IPC::MAX_SOCKET_BUF];

    int len = sprintf (buf, "Z=%f,A=%f", getZ(y), getAmplitude(x,y));
    int k2, tot_len;
    for (k2 = 0, tot_len = len; k2 < _list_size; k2++) {
      len = sprintf (&buf[tot_len], ",H%d=%f", _header_list[k2],
        getHeader(x,_header_list[k2]));
      tot_len += len;
    }

    assert (tot_len < IPC::MAX_SOCKET_BUF);

    retval = _sl_app->ipcIO()->sendData (buf);
  }
  else {
    retval = 0;
  }
  return retval;
}

// assume that the header is one-relative
float SendPick::getHeader (long x, int header)
{
  // the return from getTraceFromPixel is one-relative
  float retval
    = _hd[(_sp->getTraceFromPixel(x)-1)*_sp->numHeaders()+header-1];
  return retval;
}

float SendPick::getZ (long y)
{
  float retval = _sp->ySampleNumFromPixel ((int)y) * _sp->sampleRate ();
  return retval;
}

float SendPick::getAmplitude (long x, long y)
{
  // getTraceFromPixel is one-relative however ySampleNumFromPixel
  //   is zero-relative
  float retval = _sp->getAmplitudeRecovery()->getTrueAmplitude (
    (long)(_sp->getTraceFromPixel(x)-1), _sp->ySampleNumFromPixel((int)y));
  return retval;
}



SendPickXY::SendPickXY (SeisPlot *sp, SendPick *s_p) :
  PickBase (sp, picking_mode, help_token, sendpickhelp, XC_tcross, allow,
    True),
  _s_p  (s_p)
{
}

SendPickXY::~SendPickXY ()
{
}

// determine the x, y, z values at the cursor and send to IPC I/O clients 
void SendPickXY::buttonAny (int x1, int x2, int y1, int y2, int button,
  Action action, Modifier /*modifier*/)
{
  long current_trace;
  float current_time;

 double temp_val;
 long temp_long;
 static long start, end;

 if (button == 1 && action == release) {
   _s_p->send (x2, y2);
 }
}
