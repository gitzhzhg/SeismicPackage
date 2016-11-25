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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================



//==========================================================================
//===================== Class for X image generation =======================
//===================== Michael L. Sherrill 04/97    =======================
//===================== Converted from c code of 11/91 =====================
//==========================================================================

#include <stdio.h>
#include "tfdefs.h"
#include "plot_image.hh"
#include "xy_display.hh"
#include "trace_selector.hh"
#include "image_amplitude_recovery.hh"
/////////////////////////// new //////////////////////////
#include "pixmap_set.hh"
#include "sl/ximage.hh"
#include "sl/color_info_set.hh"
/////////////////////////// new //////////////////////////

PlotImage::PlotImage(Widget  Graphic,  char *graph_font,
                     char              *small_graph_font,
                     ColorInfo         *col,
                     ColorInfo         *col_two,
                     Boolean           frame_buff) :
  _manual_grid_x1  (0),
  _manual_grid_x2  (0),
  _manual_grid_y1  (0),
  _manual_grid_y2  (0),
  _ximage_ptr      (NULL),
  _pixmap_set      (NULL),
  _col_segment     (NULL),
  _analyzer        (NULL)
{
  _user = new ImageInput();
  _xydisp = new XYdisp();
  initImage( Graphic, graph_font, small_graph_font,
             col, col_two, frame_buff);
  setMouseReadoutType(MOUSE_AMP);
  _abort_function = NULL;
  _abort_data = NULL;
  _vary = NULL;
  _amp_recovery = new AmplitudeRecovery(this);
/*
//////////// old ////////////////////
  _ximage.depth = 1;
//////////// old ////////////////////
*/
  _trace_selector = new TraceSelector();
  _use_selector = 0;
  _ndo_select = NULL;
  _use_ndo_selector = False;
}


PlotImage::PlotImage () :
  _ximage_ptr        (NULL),
  _pixmap_set        (NULL),
  _col_segment       (NULL),
  _analyzer          (NULL),
  _bitmap_pixmap     (0),
  _font_bold         (NULL),
  _font_fixed        (NULL),
  _gc1               (NULL),
  _gc2               (NULL),
  _pick_gc           (NULL),
  _bitmap_gc1        (NULL),
  _bitmap_gc2        (NULL)
{
  _user = new ImageInput();
  _xydisp = new XYdisp();
  setMouseReadoutType(MOUSE_AMP);
  _abort_function = NULL;
  _abort_data = NULL;
  _sharing_resources = False;
  _displayed = False;
  _vary = NULL;
  _amp_recovery = new AmplitudeRecovery(this);
/*
//////////// old ////////////////////
  _ximage.depth = 1;
//////////// old ////////////////////
*/
  _trace_selector = new TraceSelector();
  _use_selector = 0;
  _ndo_select = NULL;
  _use_ndo_selector = False;
}

PlotImage::~PlotImage()
{
  if( _sharing_resources == False)
    {
    delete _xydisp;
    delete _user;
    delete _amp_recovery;
    delete _trace_selector;
    if (_analyzer) delete _analyzer;
/////////////////////////// new //////////////////////////
    if (_col_segment) {
      ColorInfoCollection::remove (_col_segment);
      CheckColorSegments::deleteSegment (_col_segment);
    }
    if (_ximage_ptr) delete _ximage_ptr;
    if (_pixmap_set) delete _pixmap_set;
/////////////////////////// new //////////////////////////
    if (_ndo_select) delete _ndo_select;
    }
  if(_vary && _point_to_data == False) delete _vary;
}


//===========================================================================
//=== Find traces to display based on up to 3 header word values ============
//===========================================================================
long PlotImage::findTraces(char *filename,       int primary_header,
                           float primary_min,    float primary_max,
                           float primary_inc,    int secondary_header,
                           float secondary_min,  float secondary_max,
                           float secondary_inc,  int tertiary_header,
                           float tertiary_min,   float tertiary_max,
                           float tertiary_inc,   long nplt,
                           long iskp,            long ndo,
                           long nskp,            long num_frames, 
                           int pixmap)
{
//Note pixmap variable passed in is not currently used
long found = 0;
long ndo_nskp, ndo_sets;
long i;
long total_to_find;
long found_this_pass = 0;
long still_need;
int current_pixmap = pixmap;
int open_file = 1;
int cstat, user_aborted = 0, lun;
char msg[256];



  total_to_find = num_frames * nplt;

  still_need = nplt;

  for(i = 0; i < num_frames; i++)
     _trace_selector->resetNumTraces(i);

  while(found < total_to_find)
    {
    found_this_pass = _trace_selector->findTraces(
                                         filename,         primary_header,
                                         primary_min,      primary_max,
                                         primary_inc,      secondary_header,
                                         secondary_min,    secondary_max,
                                         secondary_inc,    tertiary_header,
                                         tertiary_min,     tertiary_max,
                                         tertiary_inc,     nplt,   
                                         iskp,             ndo,
                                         nskp,             still_need,
                                         current_pixmap,   open_file,       
                                         _nhdrs,           &user_aborted,  
                                         &lun); 

    open_file = 0;

    if(user_aborted == TraceSelector::USER_ABORTED)
      {
      tf_close_(&lun, &cstat, msg);
      return 0;
      }


    if(found_this_pass < 0)//hit end of file or read error
       break;

    found += found_this_pass;

    primary_min   += (found_this_pass * primary_inc);
    secondary_min += (found_this_pass * secondary_inc);
    tertiary_min  += (found_this_pass * tertiary_inc);

    //If the user's increment goes outside of the user's min and max values
    //we may have received a few traces that are ok to use. Those traces will
    //be reflected in the found_this_pass variable.
    if(user_aborted == TraceSelector::PAST_USER_LIMITS)
       break;


    still_need -= found_this_pass;

    if(found == nplt * (current_pixmap + 1) || still_need < 1)
      {
      ++current_pixmap;
      if(still_need < 1)
        still_need += nplt;
      else
        still_need = nplt;
      }

    //Increment trace skip for movies. If traces_per_group is 0 a normal
    //cbyt type movie is set up, if traces_per_group is 1 a vertical
    //crossline pattern is assumed, and if traces_per_group is > 1 a inline
    //pattern of traces is set up like a cvst file. 
    if(found < total_to_find) 
      {
      if(!_traces_per_group)
        {
        ndo_nskp = ndo + nskp;
        ndo_sets = nplt / ndo;
        iskp += (int)(ndo_nskp * ndo_sets);
        }
      else /*pulling off a cube crossline or gvs panel*/
        {
        iskp += (int)_traces_per_group;
        if(_user->_skip_frames)
           iskp += (int)(_traces_per_group*_user->_skip_frames);
        }
      }
    }


  tf_close_(&lun, &cstat, msg);

  //The following might happen with certain movie patterns
  if(found > total_to_find)
     found = total_to_find;

  return found;
}


//===========================================================================
//===========  Find traces to display based on NDoTraceSelection ============
//===========================================================================
long PlotImage::findTraces (char *filename,       NDoTraceSelection *select,
                            long ntraces,         long nplt,
			    long iskp,            long ndo,
			    long nskp,            long num_frames, 
			    int pixmap)
{
  long retval;

  NDoTraceSelection *tmp_select;
  int current_pixmap;
  // determine if file is regularized
  if (isRegularized(filename,select,ntraces)) {
    // create and store a select from the given select
    if (_ndo_select) _ndo_select->set (select);
    else             _ndo_select = new NDoTraceSelection (select);
    if (_ndo_select->valid()) {
      // create a temporary select that you can work with
      tmp_select = new NDoTraceSelection (select);
      // loop through all the frames to identify alL traces of interest
      //   and store them into _trace_selector
      int k2;
      for (k2 = 0, retval = 0, current_pixmap = pixmap; k2 < num_frames;
	k2++, current_pixmap++) {
	_trace_selector->resetNumTraces (k2);
	retval += _trace_selector->findTraces (tmp_select, nplt, iskp,
	  ndo, nskp, current_pixmap);
	// scoot over to the next frame
	tmp_select->scanFrame ();
      }
      delete tmp_select;
    }
    else {
      retval = 0; // bad selection parameters
    }
  }
  else {
    retval = 0; // data is not regularized
  }
  return retval;
}

Boolean PlotImage::isRegularized (char *filename, NDoTraceSelection *select,
  long ntraces)
{
  Boolean retval;

  int *headers;
  int hdrcnt = select->getHeaderCount (&headers, True);
  if (hdrcnt > 0) {
    if (_analyzer == NULL) {
      _analyzer = new TraceOrderAnalyzer ();
    }
    _analyzer->set (filename, ntraces);
    _analyzer->setAbortFunction (select->getAbortFunction(),
      select->getAbortData());
  
    int user_aborted = 0;
    // determine if file is regularized
    if (_analyzer->ok(hdrcnt,headers,select,&user_aborted)) {
      if (!user_aborted) {
	retval = True; // check validity later
      }
      else {
	retval = False; // user aborted
      }
    }
    else {
      retval = False; // data irregular
    }
    free (headers);
  }
  else {
    retval = False; // no headers specified
  }
  return retval;
}

Boolean PlotImage::matches (char *filename, NDoTraceSelection *select,
  long ntraces)
{
  Boolean retval;

  if (_analyzer == NULL) {
    _analyzer = new TraceOrderAnalyzer ();
  }

  retval = _analyzer->setOnProbableMatch (filename, ntraces, select);
  return retval;
}


//===========================================================================
//=== Find traces to display based on a change in a particular header word ==
//=== Note the found parameter may not be accurate for some intended       ==
//=== purposes but it will be > 0 if a pattern change is detected          ==
//===========================================================================
long PlotImage::findTracesByPattern(char *filename,    int xheader, 
                                   int yheader,        long num_traces,
                                   long *num_gathers,  int *user_aborted, 
                                   float *xloc,        float *yloc)
{
long found, i;
float vel_min =  9999999999.9F;
float vel_max = -9999999999.9F;
float min_x   =  9999999999.9F;
float max_x   = -9999999999.9F;
long max_traces = 1;


  for(i = 0; i < MAX_PIXMAP; i++)
     _trace_selector->resetNumTraces(i);


  found = _trace_selector->findTracesByPattern(filename,       xheader, 
                                               yheader,        num_traces,
                                               _nhdrs,
                                               num_gathers,    user_aborted, 
                                               xloc,           yloc,
                                               &vel_min,       &vel_max,
                                               &min_x,         &max_x);
  
  if(*user_aborted)
     found = 0;


  if(found)
    {
    for(i = 0; i < *num_gathers; i++)
       if(max_traces < getSelectorNumTraces(i))
          max_traces = getSelectorNumTraces(i);
   
    if(max_traces > _user->getVelocityArraySize())
      {
      printf("Too many traces for array in PlotImage::findTracesByPattern\n");
      found = 0;
      return found;
      }

    float vel_inc = (vel_max - vel_min) / (float)(max_traces - 1);
    for(i = 0; i < max_traces; i++)
       _vary[i] = i * vel_inc + vel_min;

    _user->setNumberToDo(max_traces);
    _user->setNumberToPlot(max_traces);
    _traces_per_group = max_traces;

    _user->setVelocityMinimum(vel_min);
    _vel_min = vel_min;
    _user->setVelocityMaximum(vel_max);
    _vel_max = vel_max;
    
    _user->setX1(min_x);
    _grid_x1 = min_x;
    _user->setX2(max_x);
    _grid_x2 = max_x;

    if(_user->getX2() == _user->getX1())
      {
      _grid_x2 += .0001;
      _user->setX2(_grid_x2);
      } 


    }


  return found;
}

//===========================================================================
//=== Set the appropriate selector panel number to use in a read request  ===
//===========================================================================
void PlotImage::setCurrentTraceSelectorPanel(int panel_index)
{
  _current_selector_panel = panel_index;
}


//===========================================================================
//=== Get the appropriate panel number to use in a read request           ===
//===========================================================================
int PlotImage::getCurrentPanel()
{
  if(!usingSelector())
    return _cpixm;
  else if(usingSelector() && _user->_movie)
    return _cpixm;
  else if(usingSelector())
    return _current_selector_panel;
}

void PlotImage::useSelector (int n)
{
/* using the irregular selector after a regular selector has been
   instantiated is not allowed
 */
  if (_ndo_select == NULL) {
    //    if (_use_ndo_selector == TRUE) _use_ndo_selector = FALSE;
    _use_selector = n;
  }
}

void PlotImage::useNDoSelector (Boolean n)
{
/* using the regular selector before it has been instantiated is not
   allowed
 */
  if (_ndo_select != NULL) {
    //  if (_use_selector == TRUE) _use_selector = FALSE;
    _use_ndo_selector = n;
  }
}

Boolean PlotImage::usingSelector ()
{
  return _use_selector || _use_ndo_selector;
}

TraceSelector *PlotImage::getTraceSelector ()
{
  return _trace_selector;
}

NDoTraceSelection *PlotImage::getNDoTraceSelector ()
{
  assert (_use_selector == FALSE);
  return _ndo_select;
}

//===========================================================================
//=== Set a trace number in the trace selector array                      ===
//===========================================================================
int PlotImage::setTraceSelectorTrace(int pixmap, long trace)
{
  return _trace_selector->setTrace(pixmap, trace);
}

//===========================================================================
//=== Get the number of traces for a pixmap from the trace selector       ===
//===========================================================================
long PlotImage::getSelectorNumTraces(int pixmap)
{
  return _trace_selector->getNumTraces(pixmap);
}

void PlotImage::resetSelectorNumTraces ()
{
  int k2;
  for (k2 = 0; k2 < MAX_PIXMAP; k2++) _trace_selector->resetNumTraces (k2);
}

//===========================================================================
//=== Get the number of traces that are currently displayed               ===
//===========================================================================
long PlotImage::getNumberDisplayedTraces()
{
long offset;
long num_traces = 0;

  if(!usingSelector())
    {
    return _displayed_traces;
    }
  else
    {
    if(!_zoomed)
      {
      return getSelectorNumTraces(_cpixm);
      }
    else
      {
      for(long i = _displayed_traces; i >= 0; i--)
        {
        offset = ((_cpixm * getTracesInMemory()) * _nhdrs) + (i * _nhdrs);
        if(_hd[offset] > 0.0F)
          {
          num_traces = i + 1;
          i = -1;
          }
        }          
      }
    }

  return num_traces;
}

//===========================================================================
//========= Get the tmin stored in memory          ==========================
//===========================================================================
float PlotImage::getMemoryTmin()
{

 return (float)( _user->getGlobals().tstrt + 
                (_Cl.samp1 - 1) * _user->getGlobals().srval );

}

//===========================================================================
//========= Get the tmax stored in memory          ==========================
//===========================================================================
float PlotImage::getMemoryTmax()
{
  return (float)(_user->getGlobals().tstrt + (_Cl.samp1 - 1 + _Cl.nsamp - 1) *
                 _user->getGlobals().srval );
}

//===========================================================================
//================ Get image sample rate           ==========================
//===========================================================================
float PlotImage::getImageSampleRate()
{
  return (float)(_user->getGlobals().srval * _Cl.sdec);
}

//===========================================================================
//================ Get first trace in image        ==========================
//===========================================================================
long PlotImage::getFirstTraceInImageIndex()
{
  // return _cpixm * _Cl.ntot + _first_trace_in_image;
  // put a -1 at the end to make it consistent with old image
  // -- trey
  return _cpixm * _Cl.ntot + _first_trace_in_image - 1;
}

//===========================================================================
//================ Get address of displayed header data =====================
//===========================================================================
float *PlotImage::getDisplayedHeaderData()
{
  return &_hd[getFirstTraceInImageIndex() * getNumberOfHeaders()];
}

//===========================================================================
//================ Get address of displayed byte data =======================
//===========================================================================
unsigned char *PlotImage::getDisplayedByteData()
{
long index;

  index = getFirstTraceInImageIndex() * getSamplesInMemory() +
          (int)((_zoomyary[_zindex][0]-getMemoryTmin())   /
          (_user->getGlobals().srval * _user->getTdec()) + 0.5);
  return &_byte_array[index];
}

//===========================================================================
//================ Get address of displayed float data ======================
//===========================================================================
float *PlotImage::getDisplayedFloatData()
{
long index;
  index = getFirstTraceInImageIndex() * getSamplesInMemory() + 
                       (int)((_zoomyary[_zindex][0]-getMemoryTmin())   /
                       (_user->getGlobals().srval * _user->getTdec()) + 0.5);
  return &_float_array[index];
}

//===========================================================================
//================ Get address of visible pixmap byte data ==================
//===========================================================================
unsigned char *PlotImage::getMemoryByteData()
{
long index;
  index = getCurrentPixmapIndex() * getTracesInMemory() * 
                      getSamplesInMemory();
  return &_byte_array[index];
}

//===========================================================================
//================ Get address of visible pixmap float data =================
//===========================================================================
float *PlotImage::getMemoryFloatData()
{
long index;
  index = getCurrentPixmapIndex() * getTracesInMemory() *
                       getSamplesInMemory();
  return &_float_array[index];
}

//===========================================================================
//================ Get address of visible pixmap header data ================
//===========================================================================
float *PlotImage::getMemoryHeaderData()
{
  return &_hd[getCurrentPixmapIndex() * getTracesInMemory() *
              getNumberOfHeaders()];
}


//===========================================================================
//================ Assign a widget to the x mouse readout    ================
//===========================================================================
void PlotImage::setMouseXoutWidget(Widget w)
{
  _xydisp->xloc = w;
}

//===========================================================================
//================ Assign a widget to the y mouse readout    ================
//===========================================================================
void PlotImage::setMouseYoutWidget(Widget w)
{
  _xydisp->yloc = w;
}

//===========================================================================
//================ Assign a widget to the z mouse readout    ================
//===========================================================================
void PlotImage::setMouseZoutWidget(Widget w)
{
  _xydisp->zloc = w;
}

//===========================================================================
//================ Assign the type of readout desired        ================
//===========================================================================
void PlotImage::setMouseReadoutType(int i)
{
  _xydisp->mouse_readout_type = i;
}


//===========================================================================
//================ Assign a mouse readout function to call   ================
//===========================================================================
void PlotImage::setImageXYOutputFunction (ImageXYOutputFunction func, 
                                          void *data)
{
  _xy_output_function = func;
  _xydisp->outfunc = func;
  _xydisp->outdata = data;
}

Boolean PlotImage::getManualTransformX ()
{
  if (_manual_grid_x1 != _manual_grid_x2   ) {
    return _manual_annotate;
  }
  else {
    return False;
  }
}

Boolean PlotImage::getManualTransformY ()
{
  if (_manual_grid_y1 != _manual_grid_y2   ) {
    return _manual_annotate;
  }
  else {
    return False;
  }
}

Boolean PlotImage::isFloatData()
{

  return (_user->getGlobals().nbydp > 1                 ||
          strcmp(_user->getGlobals().ftyp,"TROT")  == 0 ||
          strcmp(_user->getGlobals().ftyp,"DSEGY") == 0 ||
          strcmp(_user->getGlobals().ftyp,"QTROT") == 0   );

}

void PlotImage::setColorInfoStructure (ColorInfo *c)
{
  ColorInfoSet *set;
  if (set = ColorInfoCollection::fetchIfExisting(_col)) {
    set->removeSharedElement (_col_segment);
  }

  _col = c;

  if (_col) {
    set = ColorInfoCollection::fetchExisting (_col);
    set->addSharedElement (_col_segment);
    
    if (_col != _pixmap_set->colorInfo()) {
      _pixmap_set->init (_col);
    }

    if (_col != _ximage_ptr->colorInfo()) {
      _ximage_ptr->set (&_ximage, _col);
    }
  }
}

void PlotImage::setSecondaryColorInfoStructure (ColorInfo *c)
{
  ColorInfoSet *set;
  if (set = ColorInfoCollection::fetchIfExisting(_col_two)) {
    set->removeSharedElement (_col_segment);
  }

  _col_two = c;

  if (_col_two) {
    set = ColorInfoCollection::fetchExisting (_col_two);
    set->addSharedElement (_col_segment);
  }
}

void PlotImage::colorInfoChanged (ColorInfo *col)
{
  if (col == _col) {
    assert (0); // don't know what to do
  }
  else if (col == _col_two) {
    assert (0); // don't know what to do
  }
  else {
    assert (0);
  }
}
