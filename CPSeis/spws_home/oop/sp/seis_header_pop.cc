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
// Author Michael L. Sherrill 09/94
// Creates menu to control a header graph above a SeisPlot

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "sp/seis_header_pop.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_push_box.hh"
#include "sp/seis_inform.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/paintset_collection.hh"
#include "minimax.h"
#include <X11/cursorfont.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define    HEIGHT_VAL          2.0
#define    PRIMARY_VAL         13
#define    SECONDARY_VAL       0
#define    TERTIARY_VAL        0
#define    TIMING_VAL          1.0
#define    MINVAL              -9999999.0
#define    MAXVAL               9999999.0 

static String  defres[]= {
    "*popup.title:                   Header Graph Menu",
    ".height:                        350",
    ".width:                         400",
    "*header_box1_Frame.topPosition:  20",
    "*header_box1_Frame.leftPosition: 10",
    "*bigheaderlab.fontList:         -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*bigheaderlab.labelString:      Header Graph",
    "*primaryheaderL.labelString:    Primary Header:",
    "*secondaryheaderL.labelString:  Secondary Header:",
    "*tertiaryheaderL.labelString:   Tertiary Header:",
    "*plotheightL.labelString:       Height/Inches:",
    "*timinglinesL.labelString:      Timing Lines:",
    "*primaryheader.value:           19",
    "*secondaryheader.value          19",
    "*tertiaryheader.value:          19",
    "*plotheight.value:              4.0",
    "*timinglines.value:             0",
    "*user_y_tog.labelString:        Override Data Y",
    "*user_y_tog.set:                False",
    "*user_y_topL.labelString:       Top Y",
    "*user_y_bottomL.labelString:    Bottom Y",
    "*user_y_top.value:              1.0",
    "*user_y_bottom.value:           0.0",
    NULL};


enum {PRIMARY, SECONDARY, TERTIARY, PLOTHEIGHT, TIMINGLINES, USER_Y,
      USER_Y_TOP, USER_Y_BOTTOM};


#define ParentClass SLFPopSep


//************************ Inform class actions ************************
class HeaderPopInform : public SeisInform {

 private:
     SeisHeaderPop *_hp;
     SeisPlot      *_sp;
   public:
     HeaderPopInform( SeisHeaderPop *hp, SeisPlot *sp) 
                    : SeisInform(sp), _sp(sp), _hp(hp) {};
     virtual void newPlot(SeisPlot *sp );
     virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
     virtual void postZoom (SeisPlot *sp, SeisPlot::ZoomDir  dir);
     virtual void postScan (SeisPlot *sp, SeisPlot::ScanDir  dir);
     virtual void notCurrentInWindow(SeisPlot *sp);
   };

void HeaderPopInform::newPlot(SeisPlot *)
{
  if(!_hp->_visible) return;
  _hp->drawHeaders(True);
}

void HeaderPopInform::postZoom(SeisPlot *, SeisPlot::ZoomDir)
{
  if(!_hp->_visible) return;
  _hp->drawHeaders(True);
}

void HeaderPopInform::postScan(SeisPlot *, SeisPlot::ScanDir)
{
  if(!_hp->_visible) return;
  _hp->drawHeaders(True);
}

void HeaderPopInform::postMovie(SeisPlot *, SeisPlot::MovieDir)
{
  if(!_hp->_visible) return;
  _hp->drawHeaders(True);
}

void HeaderPopInform::notCurrentInWindow(SeisPlot *sp)
{
 addSeisPlot(sp->currentSPInWindow());
 _hp->_parent_sp= (SeisPlotTie *)sp->currentSPInWindow();
 _hp->_sp->pointToArrayTypeData(_hp->_parent_sp);
 if(_hp->_visible) _hp->drawHeaders(True);
}
//************************ End inform class actions ************************






//==========================================================================
//===                   SeisHeaderPop Class                              ===
//==========================================================================
SeisHeaderPop::SeisHeaderPop( Widget            p,
                              char              *name,
                              SeisPlot          *sp,
                              SeisPlotTie       *parent_sp,
                              HelpCtx           hctx,
                              char              *primary_color,
                              char              *secondary_color,
                              char              *tertiary_color) 
       : SLFPopSep(p,name,FP_DOALL,hctx,True,False),   _plot_on_doaciton(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _sp(sp), _parent_sp(parent_sp), _first_time(True),
                            _hide(False)

{

static SLText texts[]  = 
   {
    {"primaryheader",   NULL, NULL,  SLType_int,   PRIMARY},
    {"secondaryheader", NULL, NULL,  SLType_int,   SECONDARY},
    {"tertiaryheader",  NULL, NULL,  SLType_int,   TERTIARY},
    {"plotheight",      NULL, NULL,  SLType_float, PLOTHEIGHT},
    {"timinglines",     NULL, NULL,  SLType_float, TIMINGLINES},
   };
   texts[0].target= &_primary_header;
   texts[1].target= &_secondary_header;
   texts[2].target= &_tertiary_header;
   texts[3].target= &_plot_height;
   texts[4].target= &_timing_lines;
     
static SLTog user_y_tog[]  = 
   {
     { "user_y_tog", NULL, USER_Y },
   };

static SLText userybox[]  = 
  {
    { "user_y_top",     NULL,  NULL,  SLType_float,   USER_Y_TOP },
    { "user_y_bottom",  NULL,  NULL,  SLType_float,   USER_Y_BOTTOM },
  };
userybox[0].target=&_user_y_top;
userybox[1].target=&_user_y_bottom;




   setDefaultResources( p, name, defres);
   
   _header_box= new SLTextBox( this, "header_box1", getHelpCtx(),
                               texts, XtNumber(texts), True, 1, True, False );

   _user_y_tog = new SLTogBox(this, "user_y_tog",getHelpCtx(),user_y_tog,
                              XtNumber(user_y_tog), False, False, False );


   _user_y_box = new SLTextBox( this,"user_y_box", getHelpCtx(), userybox,
                                XtNumber(userybox), True, 1, 
                                True, False);


   _inform= new HeaderPopInform(this, _parent_sp);

   _xdata = _ydata = NULL;

   if(primary_color == NULL)
      _primary_color = "black";
   else
      _primary_color = primary_color;
   if(secondary_color == NULL)
      _secondary_color = "black";
   else
      _secondary_color = secondary_color;
   if(tertiary_color == NULL)
      _tertiary_color = "black";
   else
      _tertiary_color = tertiary_color;

   _vect_primary_ll = new SeisVectLinkedList();
   _vect_primary_ll->addPlot(sp);
   _vect_secondary_ll = new SeisVectLinkedList();
   _vect_secondary_ll->addPlot(sp);
   _vect_tertiary_ll = new SeisVectLinkedList();
   _vect_tertiary_ll->addPlot(sp);

   _vect_primary_data    = NULL;
   _vect_secondary_data  = NULL;
   _vect_tertiary_data   = NULL;

   _first_primary_time   = True;
   _first_secondary_time = True;
   _first_tertiary_time  = True;
   _visible = False;
   _draw_headers = True;
}


SeisHeaderPop::~SeisHeaderPop()
{
  delete _inform;
  if(_xdata) free(_xdata);
  if(_ydata) free(_ydata);
  delete _vect_primary_ll;
  delete _vect_secondary_ll;
  delete _vect_tertiary_ll;
  if(_vect_primary_data)  delete _vect_primary_data;
  if(_vect_secondary_data)delete _vect_secondary_data;
  if(_vect_tertiary_data) delete _vect_tertiary_data;
}


Widget SeisHeaderPop::make(Widget p)
{

   if ( made() ) return topWidget();

   p = p ? p : wParent();

   ShellStatMsg  bld_info(p,"Building Header Graph Popup...");

   SLFPopSep::make(p);

   Widget bigheaderlab = XtVaCreateManagedWidget("bigheaderlab",
                              xmLabelWidgetClass,  topWidget(), 
                              XmNleftAttachment,   XmATTACH_POSITION,
                              XmNleftPosition,     25,
                              XmNtopAttachment,    XmATTACH_POSITION,
                              XmNtopPosition,      7,  NULL);


   XtVaSetValues( _header_box->W(), XmNleftAttachment, XmATTACH_POSITION,
                                    XmNtopAttachment,  XmATTACH_POSITION, NULL);
 

   XtVaSetValues( _user_y_tog->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _header_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,       _header_box->W(),
                                 XmNleftOffset,       0, 
                                 NULL);

   XtVaSetValues( _user_y_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _user_y_tog->W(),
                                 XmNtopOffset,        0,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,       _user_y_tog->W(),
                                 XmNleftOffset,       0, 
                                 NULL);

   _user_y_box->SetSensitive(USER_Y_TOP,    False);
   _user_y_box->SetSensitive(USER_Y_BOTTOM, False);


   return topWidget();

}


Boolean SeisHeaderPop::notifyComplex(SLDelay *obj, int /*index*/)
{
  if(obj == _user_y_tog)
    {
    _user_y_box->SetSensitive(USER_Y_TOP,    _user_y_tog->IsSelected(USER_Y));
    _user_y_box->SetSensitive(USER_Y_BOTTOM, _user_y_tog->IsSelected(USER_Y));
    }

  return True;
}

//************************ Header pop class ************************
//public method to set the marker colors
void SeisHeaderPop::headerColor(int which, char *color)
{

  switch(which)
     {
     case 1:
            if(_V_primary == NULL) break;
            _primary_color = color;
            _V_primary->setColor(_primary_color);
            break;
     case 2:
           if(_V_secondary == NULL) break;
            _secondary_color = color;
            _V_secondary->setColor(_secondary_color);
            break;
     case 3:
           if(_V_tertiary == NULL) break;
            _tertiary_color = color;
            _V_tertiary->setColor(_tertiary_color);
            break;
     }
}



Boolean SeisHeaderPop::ValidInput()
{
 Boolean stat;

  if (made()) 
     stat= ParentClass::ValidInput();
  else 
     stat= True;

  _hide= False;
  return (stat); 
}


void SeisHeaderPop::UndoInput()
{
  SLFormPop::UndoInput();
}

void SeisHeaderPop::DoAction()
{
 float primary_minval   = MAXVAL;
 float primary_maxval   = MINVAL;
 float secondary_minval = MAXVAL;
 float secondary_maxval = MINVAL;
 float tertiary_minval  = MAXVAL;
 float tertiary_maxval  = MINVAL;
 float smallest, largest, inches_wide;
 const float *hd = _parent_sp->firstDisplayedHeaderData();
 long i, pixels_per_inch;


  if(!_parent_sp->imageIsDisplayed()) return;

  ParentClass::DoAction();
  
  _sp->pointToArrayTypeData(_parent_sp);

  _sp->setPlotType(PlotImage::PlotHEADER);

  _sp->setGridX1(_parent_sp->plottedGridX1());
  _sp->setGridX2(_parent_sp->plottedGridX2());
  _sp->setRtoL(_parent_sp->rToL());

  for(i = _parent_sp->currentFrame(); i < _parent_sp->currentFrame() +1; i++)
      _sp->setTracesPerPanel( i, _parent_sp->plottedNplt());
  _sp->setNPlt( _parent_sp->plottedNplt() );

  _sp->setFirstLbl( _parent_sp->firstLbl() );
  _sp->setLblInc( _parent_sp->lblInc() );
  _sp->setHeader1( (int)_parent_sp->header1() );
  _sp->setHeader2( (int)_parent_sp->header2() ); 

  _sp->setBottomBorder(10);

  for(i = 0; i < _parent_sp->plottedNplt(); i++)
     {
     if(_primary_header)
        {
        if(hd[i*_parent_sp->numHeaders()+_primary_header-1] > primary_maxval) 
           primary_maxval = hd[i*_parent_sp->numHeaders()+_primary_header-1];
        if(hd[i*_parent_sp->numHeaders()+_primary_header-1] < primary_minval) 
           primary_minval = hd[i*_parent_sp->numHeaders()+_primary_header-1];
        }
     if(_secondary_header)
        {
        if(hd[i*_parent_sp->numHeaders()+_secondary_header-1]>secondary_maxval) 
          secondary_maxval = hd[i*_parent_sp->numHeaders()+_secondary_header-1];
        if(hd[i*_parent_sp->numHeaders()+_secondary_header-1]<secondary_minval) 
          secondary_minval = hd[i*_parent_sp->numHeaders()+_secondary_header-1];
        }
     if(_tertiary_header)
        {
        if(hd[i*_parent_sp->numHeaders()+_tertiary_header-1] > tertiary_maxval) 
           tertiary_maxval = hd[i*_parent_sp->numHeaders()+_tertiary_header-1];
        if(hd[i*_parent_sp->numHeaders()+_tertiary_header-1] < tertiary_minval) 
           tertiary_minval = hd[i*_parent_sp->numHeaders()+_tertiary_header-1];
        }
     }


   smallest = min(primary_minval,secondary_minval);
   smallest = min(smallest,tertiary_minval);
   largest  = max(primary_maxval,secondary_maxval);
   largest  = max(largest,tertiary_maxval);
   if(smallest == largest)largest += .001;


   if(!_user_y_tog->IsSelected(USER_Y))
     {
     _sp->setGridY1(largest);
     _sp->setGridY2(smallest);
     }
   else
     {
     _sp->setGridY1(_user_y_top);
     if(_user_y_bottom == _user_y_top)
       _user_y_bottom += .001;
     _sp->setGridY2(_user_y_bottom);
     }

  pixels_per_inch = _sp->getHorizontalPixelsPerInch(
                       XtDisplay(_parent_sp->getWidget()), 
                       DefaultScreen(XtDisplay(_parent_sp->getWidget())) );
  inches_wide = ((float)_parent_sp->imageWidth()) / ((float)pixels_per_inch);
  _sp->setGridWidth(inches_wide);
  _sp->setGridHeight(_plot_height);

  _sp->setDelta((int)_parent_sp->traceWidth());
  _sp->setTimingLines(_timing_lines, _timing_lines);

  if(_draw_headers) drawHeaders(False);


}



void SeisHeaderPop::manage()
{
Display *dpy;
Colormap colormap;
XColor closest, exact;

  if(_first_time)
     {
     _header_box->SetValue(PRIMARY,     (long int)PRIMARY_VAL);
     _header_box->SetValue(SECONDARY,   (long int)SECONDARY_VAL);
     _header_box->SetValue(TERTIARY,    (long int)TERTIARY_VAL);
     _header_box->SetValue(PLOTHEIGHT,  (float)HEIGHT_VAL);
     _header_box->SetValue(TIMINGLINES, (float)TIMING_VAL);
     _first_time = False;
     dpy = XtDisplay(topWidget());
     XtVaGetValues(topWidget(), XmNcolormap, &colormap, NULL);
     if(XAllocNamedColor(dpy, colormap, _primary_color, &closest, &exact))
         XtVaSetValues( _header_box->LabW( PRIMARY ),
                        XmNforeground, closest.pixel, NULL);
     else
         XtVaSetValues( _header_box->LabW( PRIMARY ),
                        XmNforeground, PaintsetCollection::black(
                        DefaultScreenOfDisplay(dpy)), NULL);
     if(XAllocNamedColor(dpy, colormap, _secondary_color, &closest, &exact))
         XtVaSetValues( _header_box->LabW( SECONDARY ),
                        XmNforeground, closest.pixel, NULL);
     else
         XtVaSetValues( _header_box->LabW( SECONDARY ),
                        XmNforeground, PaintsetCollection::black(
                        DefaultScreenOfDisplay(dpy)), NULL);
     if(XAllocNamedColor(dpy, colormap, _tertiary_color, &closest, &exact))
         XtVaSetValues( _header_box->LabW( TERTIARY ),
                        XmNforeground, closest.pixel, NULL);
     else
         XtVaSetValues( _header_box->LabW( TERTIARY ),
                        XmNforeground, PaintsetCollection::black(
                        DefaultScreenOfDisplay(dpy)), NULL);

     }


  XtManageChild(topWidget()); 

}

void SeisHeaderPop::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();

  _header_box->reloadDefaults();
  DoAction();
}


void SeisHeaderPop::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  _header_box->SetValue(PRIMARY,     (long int)PRIMARY_VAL);
  _header_box->SetValue(SECONDARY,   (long int)SECONDARY_VAL);
  _header_box->SetValue(TERTIARY,    (long int)TERTIARY_VAL);
  _header_box->SetValue(PLOTHEIGHT,  (float)HEIGHT_VAL);
  _header_box->SetValue(TIMINGLINES, (float)TIMING_VAL);
  DoAction();
}


void SeisHeaderPop::hide( Boolean hide )
{
  _hide = hide;
  if(_sp->imageIsDisplayed() && _hide)
    {
    _parent_sp->delHeaderGraph();
    return;
    }
}

void SeisHeaderPop::drawHeaders( Boolean new_size)
{
 static long old_numsegs;
 static long num_segments;
 long i, start, end;
 int marker_length;
 const float *sphd = _parent_sp->headers();
 long location, j;
 long header_offset = 0;
 int annotating;
 long PixelsPerTline;
 long ten_pixels = 10;
 double tl;



  if(_sp->imageIsDisplayed() && _hide)
    {
    _parent_sp->delHeaderGraph();
    return;
    }

  if(!_parent_sp->imageIsDisplayed()) return;

  //If this image is displaying headers on an image that has used the
  //trace selector option dummy in the number of traces
  if(_parent_sp->usingSelector())
    {
    for(i=0; i < _parent_sp->getSelectorNumTraces(_parent_sp->currentFrame());
        i++)
      _sp->setTraceSelectorTrace(_sp->currentFrame(), i);
    _sp->setSelectorParameters(1);
    }

  if(new_size)
    {
    _draw_headers = False; 
    DoAction();
    _draw_headers = True;
    }

  //All of the code referencing the variable annotating is to insure
  //the correct annotation of movie frames. Calling the plot routine
  //causes only the first frame annotation to be displayed since it
  //is ignorant of a frame change.
  annotating = _sp->getSeisAnnotation();
  if(annotating)
    {
    header_offset = _parent_sp->currentFrame() * _parent_sp->memoryTraces() * 
                    _parent_sp->numHeaders();
    _sp->setSeisAnnotation(False);
    }
 
  _sp->plot();

  if(annotating)//prevent timing lines that will obscure data
    {
    if(_sp->primTimingLine() && !_sp->isZoomed())
      {
      tl = _sp->primTimingLine();
      PixelsPerTline = (long)(_sp->primTimingLine() / _sp->yperPix());
      if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
      if(PixelsPerTline < ten_pixels)
        {
        while(PixelsPerTline < ten_pixels)
          {
          tl *= 2;
          _sp->setPrimTimingLine(tl);
          PixelsPerTline = (long)(tl /_sp-> yperPix());
          if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
          }
        }
      }
    if(_sp->secTimingLine() && !_sp->isZoomed())
      {
      tl = _sp->secTimingLine();
      PixelsPerTline = (long)(_sp->secTimingLine() / _sp->yperPix());
      if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
      if(PixelsPerTline < ten_pixels)
        {
        while(PixelsPerTline < ten_pixels)
          {
          tl *= 2;
          _sp->setSecTimingLine(tl);
          PixelsPerTline = (long)( tl / _sp->yperPix());
          if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
          }
        }
      }
    _sp->annotatePlot(header_offset);
    _sp->redraw( 0, 0, PlotImage::ImageAll, PlotImage::ImageAll);
    _sp->setSeisAnnotation(True);
    }

  
  if(!_sp->imageIsDisplayed())return;

  if(!_parent_sp->doingHeader() && !_hide) 
      _parent_sp->addHeaderGraph(_sp);

  if( (_primary_header == False) && (_secondary_header == False) 
                                 && (_tertiary_header  == False)) return;

  start = _parent_sp->firstTrace() - 1 + (_parent_sp->currentFrame() 
        * _parent_sp->originalTraces());
  end   = _parent_sp->firstTrace() - 1 + _parent_sp->plottedNplt()
        + (_parent_sp->currentFrame()  * _parent_sp->originalTraces() );


  if(_xdata == NULL)
     _xdata = (float *)calloc(1, (unsigned int)(_parent_sp->plottedNplt() 
                                      *sizeof(float)));
  else
     _xdata = (float *)realloc(_xdata,(unsigned int)(_parent_sp->plottedNplt()
                                      *sizeof(float))); 
  if(_ydata == NULL)
     _ydata = (float *)calloc(1, (unsigned int)(_parent_sp->plottedNplt() 
                                      *sizeof(float)));
  else
     _ydata = (float *)realloc(_ydata,(unsigned int)(_parent_sp->plottedNplt()
                                      *sizeof(float)));
  if(_xdata == NULL || _ydata == NULL)
     {
     printf("error in getheaders alloc\n");
     return;
     }


  if(_primary_header)
     {
     location = _parent_sp->firstTrace() - 1;
     num_segments = 1;
     j=0;
     for( i=start; i<end; i++)
        {
        _ydata[j] = sphd[i * _parent_sp->numHeaders() + _primary_header - 1
                         + header_offset];
        _xdata[j] = (float)location + 1.0;
        num_segments++; j++;
        location++;
        }
     num_segments--;
     if(_first_primary_time)
        {
        _first_primary_time = False;
        marker_length = (int)((float)_parent_sp->traceWidth());
        _vect_primary_data = new VectData((int)num_segments,_xdata,_ydata);
        _V_primary = _vect_primary_ll->add(_vect_primary_data,_primary_color,2,
                                   False,Vector::NoLine,
                                   Vector::HorizontalLineMarker,
                                   marker_length,2);
        }
     else
        {
        marker_length = (int)((float)_parent_sp->traceWidth());
        _V_primary->setMarker(Vector::HorizontalLineMarker, marker_length,2);
        _vect_primary_data->replace(0,(int)old_numsegs,(int)num_segments,
                                    _xdata,_ydata);
        }
     _vect_primary_ll->makeVisible();
     _visible = True;
     }
     else
     {
     if(!_first_primary_time) _vect_primary_ll->makeInvisible();
     }        

  if(_secondary_header)
     {
     location = _parent_sp->firstTrace() - 1;
     num_segments = 1;
     j=0;
     for( i=start; i<end; i++)
        {
        _ydata[j] = sphd[i * _parent_sp->numHeaders() + _secondary_header - 1];
        _xdata[j] = (float)location + 1.0;
        location++; j++;
        num_segments++;
        }
     num_segments--;
     if(_first_secondary_time)
        {
        _first_secondary_time = False;
        marker_length = (int)((float)_parent_sp->traceWidth());
        _vect_secondary_data = new VectData((int)num_segments,_xdata,_ydata);
        _V_secondary = _vect_secondary_ll->add(_vect_secondary_data,
                                   _secondary_color,2,
                                   False,Vector::NoLine,
				   Vector::HorizontalLineMarker,
                                   marker_length,2);
       }
     else
       {
        marker_length = (int)((float)_parent_sp->traceWidth());
        _V_secondary->setMarker(Vector::HorizontalLineMarker, marker_length,2);
        _vect_secondary_data->replace(0,(int)old_numsegs,(int)num_segments,
                                      _xdata,_ydata);
       }
     _vect_secondary_ll->makeVisible();
     _visible = True;
     }
  else
     {
     if(!_first_secondary_time) _vect_secondary_ll->makeInvisible();
     }


  if(_tertiary_header)
     {
     num_segments = 1;
     location = _parent_sp->firstTrace() - 1;
     j = 0;
     for( i=start; i<end; i++)
        {
        _ydata[j] = sphd[i * _parent_sp->numHeaders() + _tertiary_header - 1];
        _xdata[j] = (float)location + 1.0;
        location++; j++;
        num_segments++;
        }
     num_segments--;
     if(_first_tertiary_time)
        {
        _first_tertiary_time = False;
        marker_length = (int)((float)_parent_sp->traceWidth());
        _vect_tertiary_data = new VectData((int)num_segments,_xdata,_ydata);
        _V_tertiary = _vect_tertiary_ll->add(_vect_tertiary_data,
                                             _tertiary_color,2,
                                             False,Vector::NoLine,
	 	                             Vector::HorizontalLineMarker,
                                             marker_length,2);
        }
     else
        {
        marker_length = (int)((float)_parent_sp->traceWidth());
        _V_tertiary->setMarker(Vector::HorizontalLineMarker, marker_length,2);
        _vect_tertiary_data->replace(0,(int)old_numsegs,(int)num_segments, 
                                     _xdata,_ydata);
        }
     _vect_tertiary_ll->makeVisible();
     _visible = True;
     }
  else
     {
     if(!_first_tertiary_time) _vect_tertiary_ll->makeInvisible();
     }

  old_numsegs = num_segments;
}




