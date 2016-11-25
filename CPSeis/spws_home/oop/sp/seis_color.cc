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
#include <string.h>
#include "sp/seis_plot.hh"
#include "sp/seis_color.hh"
#include "sp/seis_inform.hh"
#include "ipc/ll_sd.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"

static int SCO_DEBUG = 0;

class ColorInform : public SeisInform {

private:
     SeisColor *_sc;
public:
   ColorInform( SeisColor *sc, SeisPlot *sp =NULL) : SeisInform(sp), _sc(sc) {};
   virtual void postZoom(SeisPlot *sp, SeisPlot::ZoomDir)
                   {
                     if (_sc->_for_seisplot) 
                            _sc->updateAmpsFromSP(sp);
                     else if (_sc->_always_update_from_sp)
                            _sc->updatePredefInfoFromSP(sp);
                   }
    virtual void postScan(SeisPlot *sp, SeisPlot::ScanDir )
                   {
                     if (_sc->_for_seisplot) 
                            _sc->updateAmpsFromSP(sp);
                     else if (_sc->_always_update_from_sp)
                            _sc->updatePredefInfoFromSP(sp);
                   }
   virtual void newPlot(SeisPlot *sp )
                   {
                     if (_sc->_for_seisplot) 
                            _sc->updateAmpsFromSP(sp);
                     else if (_sc->_always_update_from_sp)
                            _sc->updatePredefInfoFromSP(sp);
                   }
   virtual void colorChange(long num_allocated, long which_set);
};



/*
 * Should be used only from SeisPlot
 */
SeisColor::SeisColor(SeisPlot     *sp, 
                     ColorInfoPtr col,
                     int          which_cols) :
       _sp(sp), _predef(1), _colors_from_predef(True),
       _which_cols(which_cols), _i_allocated_colors(False),
       _first_time(True), _inten(10), _compres(0), _for_seisplot(True),
       _new_color(False), _col_segment(NULL)
{
  _dpy= XtDisplay(sp->W());
  for (int i= 0; (i<1024); i++) _rgb[i]= 0;

  _role = ColorInfoSet::SENDER;
 
  if (col) {
      setColor(col);
      _try_to_allocate= False;
  }
  else {
      _try_to_allocate= True;
      allocColor();
  }
  _cinform= new ColorInform(this,sp);
  
}


/*
 * Should be used only from other places
 */
SeisColor::SeisColor(SeisPlot *sp, 
                     Boolean   separate_colors,
                     Boolean   always_update_from_sp,
                     int       which_cols) :
       _sp(sp), _predef(1), _colors_from_predef(True), 
       _which_cols(which_cols),
       _i_allocated_colors(False), _first_time(True), _inten(10), _compres(0),
       _for_seisplot(False), _new_color(False),
       _always_update_from_sp(always_update_from_sp),
       _col_segment(NULL)
{
  assert(!(separate_colors && always_update_from_sp));

  _role = ColorInfoSet::TWO_WAY;
  
  _dpy= XtDisplay(sp->W());
  for (int i= 0; (i<1024); i++) _rgb[i]= 0;

  if (separate_colors) {
      _try_to_allocate= True;
      allocColor();
  }
  else {
      setColor(_sp->_col);
      _try_to_allocate= False;
      updateAmps();
  }
  if (always_update_from_sp) updatePredefInfoFromSP(sp);
  _cinform= new ColorInform(this,sp);
}



SeisColor::~SeisColor ()
{
  if (_col_segment) {
    ColorInfoCollection::remove (_col_segment);
    CheckColorSegments::deleteSegment (_col_segment);

    ColorInfoCollection::remove (&_col); // remove ColorInfoSet instance
    ColorsetCollection::remove  (&_col); // remove Colorset instance
  }

  delete _cinform;
}


void SeisColor::setColor (ColorInfoPtr col)
{
  if (ColorsetCollection::readOnly(col)) {
    setReadOnlyColor (col);
  }
  else {
    setDynamicColor (col);
  }
}

// I'm being defined by a SeisPlot somewhere
void SeisColor::setReadOnlyColor (ColorInfoPtr col)
{

  if (!_col_segment) {
    _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISCOLOR);
  }

  if (!ColorInfoCollection::fetchIfExisting(&_col)) {
    ColorInfoCollection::fetch (&_col, _col_segment, _role);
  }

  if (col == &_col) return;

  ColorInfoSet *    set = ColorInfoCollection::fetchExisting (  col);
  ColorInfoSet *loc_set = ColorInfoCollection::fetchExisting (&_col);

  // define the local ColorInfo from a SeisPlot somewhere
  memcpy (&_col, col, sizeof(ColorInfo));

  if (_col.cnum > 0) {
    // the given ColorInfo is non-trivial so update the local ColorInfoSet
    // notify _col's loc_set that the colors have probably been updated
    loc_set->update (&_col);
  }

  // Often this SeisColor is a SeisCbar owned by the SeisColorPop and 
  // Other times this SeisColor is a SeisCbar owned by the SeisCbarPop
  // Many times the given col is from a SeisPlot
  // You want the SeisColorPop owned by the SeisPlot to drive this SeisCbar
  // 1) Add the segment associated with col's set as a TWO_WAY communicator
  //    to this SeisColor
  // 2) Add the segment associated with this SeisColor to the set associated
  //    with col as a TWO_WAY communicator
  loc_set->addSharedElement (set->segment(), ColorInfoSet::TWO_WAY);
  // notice, this shared element will not experience a removeSharedElement
  //   until the caller does a ColorInfoCollection::remove (col) which is
  //   beyond our scope. This may be a problem!!!!!
  set->addSharedSet (loc_set);

  _i_allocated_colors= False; // just means I currently have colors that
                              // originated from a SeisPlot's ColorInfo
                              // somewhere else
}

void SeisColor::setDynamicColor (ColorInfoPtr col)
{
  setReadOnlyColor (col);
}


void SeisColor::allocColor()
{
  if (ColorsetCollection::readOnly(_sp->W())) {
    allocReadOnlyColor ();
  }
  else {
    allocDynamicColor ();
  }
}


void SeisColor::allocReadOnlyColor()
{
  if (_i_allocated_colors) {
//  XFreeColors (_dpy, _col.cmap, _col.pix, _col.cnum, _col.numplanes= 0);
    ColorsetCollection::clear (&_col);
  }

  Colormap cmap;
  Widget w= _sp->W();
  XtVaGetValues(w, XmNcolormap, &cmap, NULL);

  _col.numplanes= 0;
  _col.cmap= cmap;
  _col.cnum= _sp->getLoadableColors();

  int error = ColorsetCollection::allocate (&_col);
  //long stat= alloc_gray_colors(_dpy, &_col);

  if (!error) {
    ColorsetCollection::store (&_col);
    _i_allocated_colors = True; // colors were generated internally

    // if _col_segment has not been created, do it now
    if (!_col_segment) {
      _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISCOLOR);
    }

    // if &_col has not been added to the collection, do it now
    if (!ColorInfoCollection::fetchIfExisting(&_col)) {
      ColorInfoCollection::fetch (&_col, _col_segment, _role);
    }

    // it seems like a set->update (&_col) should be done but this will
    //   be delayed until after colors are stored
  }
  else {
    _i_allocated_colors = False;
    setColor (_sp->_col);
    if ((_first_time) && (_sp->_col->colorsafe)) {
      printf ("SeisColor::Could not allocate colors - sharing with plot.\n");
      _first_time = False;
    }
  }
}


void SeisColor::allocDynamicColor()
{
  allocReadOnlyColor ();
}


/*
 * set a predefine color number
 */
void SeisColor::setPredef(int predef)
{
  long tracevalues;
  _predef= predef;
  _colors_from_predef= True; 
  _sp->_image.definedColor( _rgb, predef, &_numcolors, &tracevalues, _col.cnum);
  _new_color= True;
  if (_numcolors > _col.cnum) _numcolors= _col.cnum;
}




/*
 * read a CPS color file and load the colors into the rgb array.
 */
int SeisColor::readColorFile(char *fname)
{
 long stat;
 long tracevalues;
 long retval= CbarSucc;

  _colors_from_predef= True; 
  stat = _sp->_image.getCpsCbar(fname, _rgb, &_numcolors, &tracevalues);
  if (stat) {
      if (_numcolors <= _col.cnum) {
             _colors_from_predef= False; 
             _new_color= True;
      }
      else {
             retval= CbarTooMany;
      }
  } // End If
  else retval= CbarInvalid;

 return (retval);
}


void SeisColor::drawCbarOn (Widget w, float cell_width_factor)
{
  if (ColorsetCollection::readOnly(w)) {
    drawReadOnlyCbarOn (w, cell_width_factor);
  }
  else {
    drawDynamicCbarOn (w);
  }
}


void SeisColor::drawReadOnlyCbarOn (Widget w, float cell_width_factor)
{
  drawDynamicCbarOn (w);
}


void SeisColor::drawDynamicCbarOn (Widget w)
{
  long which_color= _colors_from_predef ? _predef : 0; 


  if(_which_cols != SeisPlot::CONT)
    {
    if (_col.colorsafe && XtWindow(w)) 
      {
      if (!ColorsetCollection::readOnly(w)) {
	if (!_sp->_image.getChainImage()) {
	  ColorsetCollection::shareWith (_sp->_image.getColorStructure());
	}
	else {
	  ColorsetCollection::shareWith (
            _sp->_image.getChainImage()->getColorStructure());
	}
      }
      _sp->_image.loadColors( w, which_color, &_col, &_col,
                              &_numcolors, _rgb, &_new_color );
      if (!ColorsetCollection::readOnly(w)) {
	ColorsetCollection::shareWith ();
      }
       _new_color= False;
      }
    }
  else
    {
    if (_sp->_col_contour.colorsafe && XtWindow(w)) 
      {
      _sp->_image.loadColors( w, which_color, &_sp->_col_contour, &_sp->_col_contour,
                              &_numcolors, _rgb, &_new_color );
       _new_color= False;
      }
    }


  if (!_col.colorsafe && XtWindow(w)) {
       printf("SeisColor::drawCbarOn: SeisPlot has no allocated colors.\n");
  }
}



void SeisColor::prepareColors(SeisPlot *sp, Boolean update_sp)
{
SeisPlot *curr_sp;
int i;


 curr_sp= sp ? sp : _sp;

 if (update_sp) {
    /*  if the this is different than the caller then load the
     *  rgb to the SeisPlot's SeisColor object.  Otherwise this is
     *  the SeisPlot's SeisColor object.
     */
    if (curr_sp->_sc != this) loadToSeisPlot(curr_sp);
    // copy out rgb array and numcolors to SeisPlot's user struct
    for (int i = 0; (i<1024); i++) curr_sp->_user->setCbarRgb(i, _rgb[i]);
    curr_sp->_user->setNumberCbarColors(_numcolors);
 }
 switch (_which_cols) {
     case SeisPlot::COLOR:
              curr_sp->_image.setColorInfoStructure(curr_sp->_col);
              break;
     case SeisPlot::GS:
              curr_sp->_image.setColorInfoStructure(&curr_sp->_col_gs);
              break;
     case SeisPlot::CONT:
              curr_sp->_image.setSecondaryColorInfoStructure(
                &curr_sp->_col_contour);
              break;
 }
 
 if (curr_sp->colorsAllocated() && _which_cols != SeisPlot::CONT) 
   {
   for (i = 3; (i<1024); i+=4)
     {
     _rgb_of_plot[i]=  _rgb[i];  // update amps
     }
   curr_sp->_image.storeColors(_rgb, _numcolors, &_compres, &_inten,
                   curr_sp->_image.getColorStructure(), _rgb_of_plot  );
   for (i = 0; (i<1024); i++)
     {
     curr_sp->_sc->_rgb_of_plot[i]= _rgb_of_plot[i];
     }
   if (curr_sp->_slaves) curr_sp->_slaves->updateRGBs();
   }
 else if(curr_sp->colorsAllocated())//contours
   {
   for (i = 3; (i<1024); i+=4)
     {
     _rgb_of_plot[i]=  _rgb[i];  // update amps
     }
   curr_sp->_image.storeColors(_rgb, _numcolors, &_compres, &_inten,
                   &curr_sp->_col_contour, _rgb_of_plot  );
   for (i = 0; (i<1024); i++)
     {
     curr_sp->_sc->_rgb_of_plot[i]= _rgb_of_plot[i];
     }
   if (curr_sp->_slaves) curr_sp->_slaves->updateRGBs();
   }



}


/*
 *  copy only the amplitudes from the SeisPlot's rgb array to our 
 *  rgb array. Then call prepare colors to make them ready for display.
 */
void SeisColor::updateAmps(SeisPlot *sp)
{
SeisPlot *curr_sp;

  curr_sp = sp ? sp : _sp;

  if(_which_cols != SeisPlot::CONT)
    {
    for (int i = 0; (i<1024); i++) _rgb[i]= curr_sp->_sc->_rgb[i];
    _numcolors= curr_sp->_sc->_numcolors;
    }
  else
    {
    for (int i = 0; (i<1024); i++) _rgb[i]= curr_sp->_user->getContourCbarRgb(i);
    _numcolors= curr_sp->contours();
    }


  prepareColors(curr_sp, False);
}


/*
 *  transfer the colors to the given SeisPlot's rgb array from this 
 *  rgb array. Preserve the amplitudes contained within the given
 *  SeisPlot. Then call prepare colors to make them ready for display.
 */
void SeisColor::transferColors (SeisPlot *to_sp)
{
  SeisPlot *curr_sp;

  curr_sp = to_sp ? to_sp : _sp;

  float amin = curr_sp->_sc->_rgb[3];
  float amax = curr_sp->_sc->_rgb[4*(curr_sp->_sc->_numcolors-1)+3];
  assert (_numcolors > 1);
  float del = (amax - amin) / (_numcolors - 1);
  
  int k2;
  float amp;
  if (_which_cols != SeisPlot::CONT) {
    for (k2 = 0, amp = amin; k2 < 1024; k2++) {
      if ((k2+1)%4) {
	curr_sp->_sc->_rgb[k2] = _rgb[k2];
      }
      else {
	curr_sp->_sc->_rgb[k2] = amp;
	amp += del;
      }
    }
    curr_sp->_sc->_numcolors = _numcolors;
  }
  else {
    for (k2 = 0, amp = amin;  k2 < 1024; k2++) {
      if ((k2+1)%4) {
        curr_sp->_user->setContourCbarRgb (k2, _rgb[k2]);
      }
      else {
	curr_sp->_user->setContourCbarRgb (k2, amp);
	amp += del;
      }
    }
    curr_sp->setContours (_numcolors);
  }

  prepareColors (curr_sp, FALSE);
}

/*
 *  copy the only the amplitudes from a SeisPlots rgb array to our rgb array.
 *  The method is only called from the ColorInform class.
 */
void SeisColor::updateAmpsFromSP(SeisPlot *sp)
{
int i;

  if(_which_cols != SeisPlot::CONT)
    {
    for (i = 0; (i<1024); i++) 
           if ((i % 4) == 3 ) _rgb[i]= sp->_user->getCbarRgb(i);
    }
  else
    {
    for (i = 0; (i<1024); i++) 
           if ((i % 4) == 3 ) _rgb[i]= sp->_user->getContourCbarRgb(i);
    }

}

/*
 *  copy the predef color info and  the numcolors from the SeisPlot's
 *  SeisColor
 */
void SeisColor::updatePredefInfoFromSP(SeisPlot *sp)
{
 if(_which_cols != SeisPlot::CONT)
   { 
   _predef=             sp->_sc->_predef;
   _numcolors=          sp->_sc->_numcolors;
   }
 else
   {
   _predef    = PlotImage::CONTOUR;
   _numcolors = sp->contours();
   }  

  _colors_from_predef= sp->_sc->_colors_from_predef;
  _new_color         = False;


}


/*
 *  load the rgb array, the number of colors, and the predefined color
 *  to SeisPlot. If a SeisPlot pointer is not passed (it usually isn't)
 *  load the values to the SeisPlot pointed to by this instance of SeisColor. 
 */
void SeisColor::loadToSeisPlot(SeisPlot *sp)
{

 if (!sp) sp= _sp;
 for (int i = 0; (i<1024); i++) sp->_sc->_rgb[i]= _rgb[i];
 sp->_sc->_numcolors= _numcolors;
 sp->_sc->_predef= _predef;
}

void SeisColor::colorChange(long num_allocated, long which_set)
{
 if (which_set == SeisPlot::COLOR) {
      if (num_allocated > 0) {
           if (_i_allocated_colors) {
                  allocColor();
           }
           else
             if (_try_to_allocate)
                    allocColor();
             else
                    setColor(_sp->_col); 
      }
 }
}

/*
 * Return all colors that are allocated.
 */
int SeisColor::getMaxNumColors(int *num_planes)
{
  if (num_planes) *num_planes= (int)_col.numplanes;
  return _col.cnum;
}

/*
 * Return all colors that are currently being used in the plot.
 */
int SeisColor::getNumColors()
{
  return _numcolors;
}

/*
 * Sets the number of colors that SeisPlot is currently using.
 */
void SeisColor::setNumColors(int n)
{
  _numcolors= n;
}

/*
 * get the rgb values for at a color index
 */
void SeisColor::getAColor(int idx, float *r, float *g, float *b, float *amp)
{
  assert(idx<_numcolors);
  int i= idx*4;
  *r=  _rgb[i];
  *g=  _rgb[i+1];
  *b=  _rgb[i+2];
  *amp=_rgb[i+3];
}

/*
 * set the rgb values for at a color index
 */
void SeisColor::setAColor(int idx, float r, float g, float b, float amp)
{
  assert(idx<_numcolors);
  int i= idx*4;
  _rgb[i]  = r;
  _rgb[i+1]= g;
  _rgb[i+2]= b;
  _rgb[i+3]= amp;
}

// this seemed to be in an earlier version of 

void SeisColor::addSP(SeisPlot *sp)
{
  if (sp) _cinform->addSeisPlot(sp);
}

void SeisColor::removeSP(SeisPlot *sp)
{
  if (sp) _cinform->delSeisPlot(sp);
}

float *SeisColor::rgbOfPlot()
{
  return _rgb_of_plot;
}

void SeisColor::setSeisPlot(SeisPlot *sp) { _sp= sp;}

void SeisColor::colorInfoChangedImmediately (ColorInfo *col)
{
  assert (col == &_col);
  // do nothing here but in derived classes, may do something non-trivial
}

void ColorInform::colorChange(long num_allocated, long which_set)
{
 _sc->colorChange(num_allocated, which_set);
}
