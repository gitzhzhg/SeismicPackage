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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : imageScan
 *Purpose: Controls the scanning of image left or right to the
 *         next set of traces in the seismic data file.
 *
 *Author : Michael L. Sherrill
 *Date   : 93-09-24 (C++ version 4/97)
 *
 * Function Definition:
 * long image_scan (int              direction,
 *                  int              scan_original,
 *                  int              scan_screen)
 *
 * direction     in      Right or left
 * scan_original in      Scan original pre-zoom number of traces.
 * scan_screen   in      Scan current number of traces displayed.
 *
 *NOTES:
 * 1.
 * 2.
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *
 *END DOC
 *------------------------------------------------------------------*/

#include "plot_image.hh"



#define RESET "Warning, your Nplt and/or Iskp parameters have been \nreset to prevent reading outside of file.\n"




long PlotImage::imageScan (int direction, int scan_original, int scan_screen)
{
  int ndosets, part1, part2, addtrace;
  float numsets, remainder;
  long oldskip, oldfirsttrace, oldnplt;
  long stat;
  float original_x1;
  PlotImage *underlay = NULL;
  long traces_in_memory;
  int pattern_len;
  int new_nplt;
  long traces_in_file;
  int reset = 0;
  
  if (!_user->_movie)
    traces_in_memory = getTracesInMemory ();
  else
    traces_in_memory = getTracesInMemory() * _user->_frames;

  /* handle grid type scanning */
  if (_user->_mode == PlotGRID || _user->_mode == PlotHEADER) {
    if (_zoomed) {
      strcpy (_errstr,"Scanning a zoomed grid image not yet supported\n");
      return (ReadFail);
    }
    original_x1 = _user->_grid_x1;
    if (direction == ScanRight) {
      _user->_grid_x1 = _user->_grid_x1
        - (_user->_grid_x1-_user->_grid_x2-1.0); 
      _user->_grid_x2 = _user->_grid_x2
	+ (_user->_grid_x2 - original_x1 + 1.0);
    }
    else { /*scan left*/
      _user->_grid_x1 = _user->_grid_x1
        + (_user->_grid_x1-_user->_grid_x2-1.0);
      _user->_grid_x2 = _user->_grid_x2
        - (_user->_grid_x2 - original_x1 + 1.0);
    }
    stat = imageGrid ();
    _scanleft = False;
    _scanright= False;
    return stat;
  }

  /* handle other types */
  oldskip = _user->_iskp;
  if (direction == ScanLeft) {
    _scanleft = True;
    _scanright = False;
  }
  else if (direction == ScanRight) {
    _scanleft = False;
    _scanright = True;
  }
  else {
    _scanleft = False;
    _scanright = False;
  }

  oldfirsttrace = _first_trace_in_image;
  oldnplt = _user->_nplt;

  _first_trace_in_image = 1;
       
  if (_zoomed && scan_original) {
    _zindex = NO_ZOOM;
    _zoomxary[_zindex][0] = 1;
    _zoomxary[_zindex][1] = getTracesInMemory ();
    _zoomyary[_zindex][0] = _user->_tmin;
    _zoomyary[_zindex][1] = _user->_tmax =
      (getSamplesInMemory()-1) * _user->_G.srval + _user->_tmin;
    _user->_ti = _original_ti;
    _user->_is = _original_is;
    _user->_nplt = getTracesInMemory ();
    _zoomup = False;
    _zoomed = False;
    oldfirsttrace = 1;
  }

  if (scan_screen) {
    numsets = (float)_user->_iskp / (float)getTracesInMemory();
    ndosets = (int)numsets;
    remainder = numsets - ndosets;
    addtrace = (remainder>=0.5) ? 0 : (int)_user->_nskp;
    _user->_iskp = (long)(max(0, (numsets * getTracesInMemory()
      - _ntot - addtrace) ));
      if (numsets*getTracesInMemory()-_ntot-addtrace < 0) {
	reset = 1;
	strcpy (_errstr, RESET);
      }
  }
  else if (_ndo_select == NULL || !_use_ndo_selector) {
    /*this is the usual manner of scanning*/
    numsets = (float)traces_in_memory / (float)_user->_ndo;
    ndosets = (int)numsets;
    remainder = numsets - (float)ndosets;
    part1 = (int)(ndosets * (_user->_ndo + _user->_nskp));
    if (part1 == 0) part1 = ndosets;
    part2 = (int)(remainder * _user->_ndo + .5);
    if (!_user->_RtoL) {
      if (direction == ScanLeft) {
	if (_user->_iskp - part1 - part2 + (oldfirsttrace - 1) < 0) {
	  _user->_iskp = 0;
	  reset = 1;
	  strcpy (_errstr, RESET);
	}
	else {
	  _user->_iskp = _user->_iskp - part1 - part2 + (oldfirsttrace - 1);
	}
      }
      else {
	if (_user->_iskp + part1 + part2 + (oldfirsttrace - 1) < 0) {
	  _user->_iskp = 0;
	  reset = 1;
	  strcpy (_errstr, RESET);
	}
	else {
	  _user->_iskp = _user->_iskp + part1 + part2 + (oldfirsttrace - 1);
	}
      }
    }
    else {
      if (direction == ScanLeft) {
	if (_user->_iskp + part1 + part2 + (oldfirsttrace - 1) < 0) {
	  _user->_iskp = 0;
	  reset = 1;
	  strcpy (_errstr, RESET);
	}
	else {
	  _user->_iskp = _user->_iskp + part1 + part2 + (oldfirsttrace - 1);
	}
      }
      else {
	_user->_iskp = max (0, (_user->_iskp-part1-part2+(oldfirsttrace-1)));
	if (_user->_iskp-part1-part2+(oldfirsttrace-1) < 0) {
	  _user->_iskp = 0;
	  reset = 1;
	  strcpy (_errstr, RESET);
	}
	else {
	  _user->_iskp = _user->_iskp - part1 - part2 + (oldfirsttrace - 1);
	}
      }
    }
  }
  traces_in_file = _user->_G.ntrfil;

  pattern_len = _user->_ndo + _user->_nskp;

  new_nplt = (traces_in_file - _user->_iskp) /  pattern_len * _user->_ndo;

  int left_over = (traces_in_file - _user->_iskp) % pattern_len;

  if (left_over > _user->_ndo) new_nplt += _user->_ndo;
  else                         new_nplt += left_over;

  if (new_nplt < _user->_nplt) {
    reset = 1;
    _user->_nplt = new_nplt;
    strcpy (_errstr, RESET);
  }

  if (new_nplt > traces_in_file || new_nplt < 1) {
    _user->_iskp = oldskip;
    _first_trace_in_image = oldfirsttrace;
    _user->_nplt = oldnplt;
    strcpy (_errstr, "Could not plot, scan would read outside of file.\n");
    return ReadFail;
  }

  stat = checkSize ();
  if (stat != PlotSuccess  && stat != PlotWarning) {
    _user->_iskp = oldskip;
    _first_trace_in_image = oldfirsttrace;
    _user->_nplt = oldnplt;
    return stat;
  }

  _cpixm = 0;

  if (_chain_image) { /*unattach underlay during setup_plots call to refresh*/
    underlay = _chain_image;
    _chain_image = NULL;
    stat = plot ();
    _chain_image = underlay; /*reattach underlay*/
  }
  else {
    stat = plot ();
  }

  _scanleft = False;
  _scanright= False;                 
  _zoom_scan = _zoomed;

  if (_over_image != NULL) {
    _over_image->refresh (0, 0, ImageAll, ImageAll);
  }
  if (stat == PlotSuccess && reset) stat = PlotWarning;
  return stat ;
}
