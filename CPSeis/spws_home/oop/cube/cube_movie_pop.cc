//********************************************
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
//Author Michael L. Sherrill 10/96
//Creates menu to control cube movie images
//********************************************
#include "sp/seis_plot.hh"
#include "cube/cube.hh"
#include "cube/cube_movie_pop.hh"
#include "cube/cube_movie_control.hh"
#include "cube/cube_movie_preferpop.hh"
#include "cube/cube_plot_error.hh"
#include "oprim/static_utils.hh"
#include "cprim.h"
#include "named_constants.h"
#include "sl/shell_stat_msg.hh"
#include "sl/shell_watch.hh"
#include <Xm/Label.h>

static String  defres[]= {
    "*XmScale.scaleMultiple:          1",
    // "*XmScale.fontList:               -*-*-*-r-*-*-*-100-*-*-*-*-*-*",
    "*Inline*XmScale.titleString:     Inline",
    "*Crossline*XmScale.titleString:  X Line",
    "*TimeSlice*XmScale.titleString:  Slice",
    "*CubeMovie*XmScale.titleString:  Cube",
    "*read_box_inline*columns:     8",
    "*read_box_crossline*columns:  8",
    "*read_box_timeslice*columns:  8",
    NULL};



//============================================================================
//====================== Constructor =========================================
//============================================================================
CubeMoviePop::CubeMoviePop( Widget              p,
                            char                *name,
                            HelpCtx             hctx,
                            CubeMovieControl    *cube_movie_control)
                            : SLFPopSep(p,name,FP_DOREMOVE,hctx,False,False)
{
  _derived_class_made = False;
  _cube_movie_control = cube_movie_control;
  _hctx = hctx;  
  _prefer_pop = NULL;
  _cube_number = 0;
  setDefaultResources( p, name, defres);
  addExtraButton("Preference...",PREFERENCE);
  setComplexNotify(this);
}



//============================================================================
//====================== Destructor                  =========================
//============================================================================
CubeMoviePop::~CubeMoviePop()
{
 if(_prefer_pop) delete _prefer_pop;
}


//============================================================================
//====================== Make popup         ==================================
//============================================================================
Widget CubeMoviePop::make(Widget p)
{
static SLText text1[]  = {
    {"read_inline",    NULL, NULL, SLType_float,   READ_INLINE}, };
static SLText text2[]  = {
    {"read_crossline", NULL, NULL, SLType_float,   READ_CROSSLINE},};
static SLText text3[]  = {
    {"read_timeslice", "range:-50000 *,default:0.000", NULL, 
                                               SLType_float,READ_TIMESLICE},};
   text1[0].target= &_read_inline;
   text2[0].target= &_read_crossline;
   text3[0].target= &_read_timeslice;

static SLPush loadinline   [] = { { "  ", LOAD_INLINE    }, };
static SLPush loadcrossline[] = { { "  ", LOAD_CROSSLINE }, };
static SLPush loadtimeslice[] = { { "  ", LOAD_TIMESLICE }, };
static SLPush loadall[] =       { { "  ", LOAD_ALL }, };



   if ( made() ) return topWidget();

   sprintf(_cube_id, "Cube %d = ",_cube_number); 

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Movie Control Menu...");

   SLFPopSep::make(p);

  _inline_scale    = new SLArrowScale(this,"Inline",    _hctx,
                                      &_inline_frame,    True);
  _crossline_scale = new SLArrowScale(this,"Crossline", _hctx,
                                      &_crossline_frame, True);
  _timeslice_scale = new SLArrowScale(this,"TimeSlice",_hctx, 
                                      &_timeslice_frame, True);
  _cube_scale      = new SLArrowScale(this,"CubeMovie",_hctx, 
                                      &_cube_number,     True);

  _previous_inline_read = 
                new SLpArrow(topWidget(), "previous_inline", 
                             PREVIOUS_INLINE_READ, SLpArrow::_LEFT);
  _next_inline_read = 
                new SLpArrow(topWidget(), "next_inline", 
                             NEXT_INLINE_READ, SLpArrow::_RIGHT); 
  _previous_crossline_read = 
                new SLpArrow(topWidget(), "previous_crossline", 
                             PREVIOUS_CROSSLINE_READ, SLpArrow::_LEFT);
  _next_crossline_read = 
                new SLpArrow(topWidget(), "next_crossline", 
                             NEXT_CROSSLINE_READ, SLpArrow::_RIGHT); 
  _previous_timeslice_read = 
                new SLpArrow(topWidget(), "previous_timeslice", 
                             PREVIOUS_TIMESLICE_READ, SLpArrow::_LEFT);
  _next_timeslice_read = 
                new SLpArrow(topWidget(), "next_timeslice", 
                             NEXT_TIMESLICE_READ, SLpArrow::_RIGHT);

  _previous_inline_read->setAtrap   (arrowTrap, this);
  _next_inline_read->setAtrap       (arrowTrap, this);
  _previous_crossline_read->setAtrap(arrowTrap, this);
  _next_crossline_read->setAtrap    (arrowTrap, this);
  _previous_timeslice_read->setAtrap(arrowTrap, this);
  _next_timeslice_read->setAtrap    (arrowTrap, this);

  _read_box_inline =    new SLTextBox( this, "read_box_inline", _hctx,
                               text1, XtNumber(text1), False, 1, False, False );
  _read_box_crossline = new SLTextBox( this, "read_box_crossline", _hctx,
                               text2, XtNumber(text2), False, 1, False, False );
  _read_box_timeslice = new SLTextBox( this, "read_box_timeslice", _hctx,
                               text3, XtNumber(text3), False, 1, False, False );
  _read_box_timeslice->SetRange((int)READ_TIMESLICE, (double)SLTextBox::same, 
                               (double)SLTextBox::same, 0.0, (long)3);
  _read_box_inline->setAltLosingAction(    (SLTextfunc)readLosingFocusAction,
                               this );
  _read_box_crossline->setAltLosingAction( (SLTextfunc)readLosingFocusAction,
                               this );
  _read_box_timeslice->setAltLosingAction( (SLTextfunc)readLosingFocusAction,
                               this );
  _read_box_inline->setAltFocusAction    ( (SLTextfunc)readFocusAction, this );
  _read_box_crossline->setAltFocusAction ( (SLTextfunc)readFocusAction, this );
  _read_box_timeslice->setAltFocusAction ( (SLTextfunc)readFocusAction, this );

  _load_inline = new SLPushBox(this,"load_inline",_hctx,
                           loadinline, XtNumber(loadinline));
  _load_crossline = new SLPushBox(this,"load_crossline",_hctx,
                           loadcrossline, XtNumber(loadcrossline));
  _load_timeslice = new SLPushBox(this,"load_timeslice",_hctx,
                           loadtimeslice, XtNumber(loadtimeslice));
  _load_all       = new SLPushBox(this,"load_all",_hctx,
                           loadall, XtNumber(loadall));
  _load_inline->setAltPushAction(    (SLPushButtonfunc)loadButton, this);
  _load_crossline->setAltPushAction( (SLPushButtonfunc)loadButton, this);
  _load_timeslice->setAltPushAction( (SLPushButtonfunc)loadButton, this);
  _load_all->setAltPushAction( (SLPushButtonfunc)loadButton, this);

  defaultButton(FP_DOAPPLY, False);

  setAttachments();

  _prefer_pop = new CubeMoviePreference(topWidget(),"Preferences",_hctx,
                                         this, _cube_movie_control);

  _derived_class_made = True;

  return topWidget();

}


//============================================================================
//====================== Manage popup ========================================
//============================================================================
void CubeMoviePop::manage()
{

  SLFPopSep::manage();
  updateCubeInfo();
}


//============================================================================
//====================== Make and manage popup       =========================
//============================================================================
void CubeMoviePop::makeAndManage(Widget p)
{
Widget parent;

  parent = p ? p : wParent();
  make(parent);
  manage();
  SLFPopSep::makeAndManage();

}


//============================================================================
//====================== Lot of attachments          =========================
//============================================================================
void CubeMoviePop::setAttachments()
{

  setTitle("Cube Movie Control");

//Left column of widgets
   Widget movie_label= XtVaCreateManagedWidget( "Movies",
                           xmLabelWidgetClass,  topWidget(),
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        5, 
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       _inline_scale->W(),
                           XmNleftOffset,       35, NULL );

   XtVaSetValues( _inline_scale->W(),
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNleftOffset,       5,
                           XmNtopOffset,        30, 
                           XmNheight,           40, NULL);


   XtVaSetValues( _inline_scale->scaleW(),
                           XmNscaleHeight,      10,
                           XmNscaleWidth,       60, NULL);


   XtVaSetValues(_inline_scale->leftW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);

   XtVaSetValues(_inline_scale->rightW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);

   XtVaSetValues( _crossline_scale->W(),  
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       _inline_scale->W(),
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _inline_scale->W(), 
                           XmNheight,           40, NULL);

   XtVaSetValues( _crossline_scale->scaleW(),
                           XmNscaleHeight,      10,
                           XmNscaleWidth,       60, NULL);

   XtVaSetValues(_crossline_scale->leftW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);

   XtVaSetValues(_crossline_scale->rightW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);
   
   XtVaSetValues( _timeslice_scale->W(),  
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       _crossline_scale->W(),
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _crossline_scale->W(), 
                           XmNheight,           40, NULL);

   XtVaSetValues( _timeslice_scale->scaleW(),
                           XmNscaleHeight,      10,
                           XmNscaleWidth,       60, NULL);

   XtVaSetValues(_timeslice_scale->leftW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);

   XtVaSetValues(_timeslice_scale->rightW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);

   XtVaSetValues( _cube_scale->W(),  
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       _timeslice_scale->W(),
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _timeslice_scale->W(), 
                           XmNheight,           40, NULL);

   XtVaSetValues( _cube_scale->scaleW(),
                           XmNscaleHeight,      10,
                           XmNscaleWidth,       60, NULL);


   XtVaSetValues(_cube_scale->leftW(),
                           XmNwidth,            30,
                           XmNheight,           30, NULL);

   XtVaSetValues(_cube_scale->rightW(),
                           XmNwidth,            30,
                           XmNheight,           30,NULL);

//Next column of widgets (read arrows)
   Widget arrow_label= XtVaCreateManagedWidget( "Scan Data",
                           xmLabelWidgetClass,  topWidget(),
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       _inline_scale->W(),
                           XmNleftOffset,       15,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        5, NULL );

   XtVaSetValues(_previous_inline_read->W(),
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       arrow_label,
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _inline_scale->W(),
                           XmNtopOffset,        10,
                           XmNheight,           30,NULL);

   XtVaSetValues(_next_inline_read->W(),
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      arrow_label,
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _inline_scale->W(),
                           XmNtopOffset,        10,
                           XmNheight,           30,NULL);

   XtVaSetValues(_previous_crossline_read->W(),
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       arrow_label,
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _crossline_scale->W(),
                           XmNtopOffset,        10,
                           XmNheight,           30,NULL);
   
   XtVaSetValues(_next_crossline_read->W(),
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      arrow_label,
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _crossline_scale->W(),
                           XmNtopOffset,        10,
                           XmNheight,           30,NULL);

   XtVaSetValues(_previous_timeslice_read->W(),
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       arrow_label,
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _timeslice_scale->W(),
                           XmNtopOffset,        10,
                           XmNheight,           30,NULL);
   
   XtVaSetValues(_next_timeslice_read->W(),
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      arrow_label,
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _timeslice_scale->W(),
                           XmNtopOffset,        10,
                           XmNheight,           30,NULL);

//Next column of widgets (read a line texts)
   Widget read_label= XtVaCreateManagedWidget( "Read Line",
                           xmLabelWidgetClass,  topWidget(),
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       _next_inline_read->W(),
                           XmNleftOffset,       25,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        5, NULL );

   XtVaSetValues( _read_box_inline->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _next_inline_read->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       read_label,
                           XmNleftOffset,       0, NULL);

   XtVaSetValues( _read_box_crossline->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _next_crossline_read->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       read_label, 
                           XmNleftOffset,       0,NULL);

   XtVaSetValues( _read_box_timeslice->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _next_timeslice_read->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       read_label, 
                           XmNleftOffset,       0, NULL); 

//Next column of load push buttons
   Widget load_label= XtVaCreateManagedWidget( "Load",
                           xmLabelWidgetClass,  topWidget(),
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       _read_box_inline->W(),
                           XmNleftOffset,       20,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        5, NULL );

   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        5,
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       load_label,
                           XmNleftOffset,       5,
                           XmNrightAttachment,  XmATTACH_FORM,
                           XmNrightOffset,      5, NULL);

   XtVaSetValues( _load_inline->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _read_box_inline->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       load_label, 
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      load_label,
                           XmNnumColumns,       1, NULL);

   XtVaSetValues( _load_crossline->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _read_box_crossline->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       load_label, 
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      load_label,
                           XmNnumColumns,       1, NULL);

   XtVaSetValues( _load_timeslice->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _read_box_timeslice->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       load_label, 
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      load_label,
                           XmNnumColumns,       1, NULL);  

   XtVaSetValues( _load_all->W(), 
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _load_timeslice->W(),
                           XmNtopOffset,        15,
                           XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNleftWidget,       load_label, 
                           XmNleftOffset,       0,
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      load_label,
                           XmNnumColumns,       1, NULL);  

  Widget load_all_label = XtVaCreateManagedWidget("Load All --->", 
                           xmLabelWidgetClass,   topWidget(),
                           XmNleftAttachment,    XmATTACH_WIDGET,
                           XmNleftWidget,        _cube_scale->rightW(),
                           XmNleftOffset,        100,
                           XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,         _cube_scale->rightW(), 
                           XmNtopOffset,         17, NULL);

  _cube_id_label = XtVaCreateManagedWidget(_cube_id, 
                           xmLabelWidgetClass,   topWidget(),
                           XmNleftAttachment,    XmATTACH_WIDGET,
                           XmNleftWidget,        _cube_scale->rightW(),
                           XmNleftOffset,        0,
                           XmNtopAttachment,     XmATTACH_WIDGET,
                           XmNtopWidget,         _cube_scale->rightW(), 
                           XmNtopOffset,         15, NULL);

  Widget tmp2=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopOffset,        5,
                           XmNtopWidget,        _cube_id_label,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       5, 
                           XmNbottomAttachment, XmATTACH_WIDGET,
                           XmNbottomWidget,     bottomSeparator(),
                           XmNbottomOffset,     5, NULL);
}


//============================================================================
//====================== Use in future if needed  ============================
//============================================================================
void CubeMoviePop::DoAction()
{
}

//============================================================================
//====================== Use in future if needed  ============================
//============================================================================
Boolean CubeMoviePop::ValidInput()
{
Boolean stat = True;

  return(stat);  
}
//============================================================================
//====================== Use in future if needed  ============================
//============================================================================
void CubeMoviePop::UndoInput()
{

}




 
//============================================================================
//====================== Movie scale frame changes   =========================
//============================================================================
Boolean CubeMoviePop::notifyComplex(SLDelay *obj, int /*ident*/)
{
Boolean stat = True;
int index;

  if( ! made() ) return stat;

  if(_derived_class_made == False) return stat;

  if(obj != _inline_scale    && obj != _crossline_scale && 
     obj != _timeslice_scale && obj != _cube_scale) return stat;

  if(obj == _inline_scale)
    {
    _inline_frame = MaximumValue(1, _inline_frame);
    _inline_frame = MinimumValue(_cube_movie_control->memoryInLineTotal(),_inline_frame);
    _cube_movie_control->changeFrame(Cube::InLine, _inline_frame);
    index = _cube_movie_control->memoryFirstInLine() + 
            (_inline_frame - 1 ) * _prefer_pop->_inline_movie_increment; 
    _read_inline = _cube_movie_control->getWCFromLineIndex(Cube::InLine,index);
    _read_box_inline->SetValue( (int)READ_INLINE, _read_inline );
    }
  else if(obj == _crossline_scale)
    {
    _crossline_frame = MaximumValue(1, _crossline_frame);
    _crossline_frame = MinimumValue(_cube_movie_control->memoryCrossLineTotal(),
                           _crossline_frame);
    _cube_movie_control->changeFrame(Cube::CrossLine, _crossline_frame);
    index = _cube_movie_control->memoryFirstCrossLine() + 
             (_crossline_frame - 1 ) * _prefer_pop->_crossline_movie_increment; 
    _read_crossline = _cube_movie_control->getWCFromLineIndex(Cube::CrossLine,
                                                              index);
    _read_box_crossline->SetValue( (int)READ_CROSSLINE, _read_crossline );
    }
  else if(obj == _timeslice_scale)
    {
    _timeslice_frame = MaximumValue(1, _timeslice_frame);
    _timeslice_frame = MinimumValue(_cube_movie_control->memoryTimeSliceTotal(),
                           _timeslice_frame);
    _cube_movie_control->changeFrame(Cube::TimeSlice, _timeslice_frame);
    index = _cube_movie_control->memoryFirstTimeSlice() +
             (_timeslice_frame - 1 ) * _prefer_pop->_timeslice_movie_increment; 
    _read_timeslice = _cube_movie_control->getWCFromLineIndex(Cube::TimeSlice,
                                                              index);
    _read_box_timeslice->SetValue( (int)READ_TIMESLICE, _read_timeslice );
    }
  else
    {
    _cube_movie_control->changeFrame(Cube::AllPlots, _cube_number);
    }

  return stat;
}


//============================================================================
//====================== Accelerator Movie scale frame changes  ==============
//============================================================================
void CubeMoviePop::movieFrame(long ident)
{
int modifier;
int index;

  if( ! made() ) return;


  if(ident == MOVIE_INLINE_LEFT || ident == MOVIE_CROSSLINE_LEFT ||
     ident == MOVIE_TIMESLICE_LEFT)
    modifier = -1;
  else
    modifier = 1;

  if(ident == MOVIE_INLINE_LEFT || ident == MOVIE_INLINE_RIGHT)
    {
    _inline_frame += modifier;
    _inline_frame = MaximumValue(1, _inline_frame);
    _inline_frame = MinimumValue(_cube_movie_control->memoryInLineTotal(),_inline_frame);
    _inline_scale->setValue(_inline_frame);
    _cube_movie_control->changeFrame(Cube::InLine, _inline_frame);
    index = _cube_movie_control->memoryFirstInLine() + 
                (_inline_frame - 1 ) * _prefer_pop->_inline_movie_increment; 
    _read_inline = _cube_movie_control->getWCFromLineIndex(Cube::InLine,index);
    _read_box_inline->SetValue( (int)READ_INLINE, _read_inline );
    }
  else if(ident ==  MOVIE_CROSSLINE_LEFT || ident == MOVIE_CROSSLINE_RIGHT)
    {
    _crossline_frame += modifier;
    _crossline_frame = MaximumValue(1, _crossline_frame);
    _crossline_frame = MinimumValue(_cube_movie_control->memoryCrossLineTotal(),
                           _crossline_frame);
    _crossline_scale->setValue(_crossline_frame);
    _cube_movie_control->changeFrame(Cube::CrossLine, _crossline_frame);
    index = _cube_movie_control->memoryFirstCrossLine() + 
             (_crossline_frame - 1 ) * _prefer_pop->_crossline_movie_increment; 
    _read_crossline = _cube_movie_control->getWCFromLineIndex(Cube::CrossLine,
                                                              index);
    _read_box_crossline->SetValue( (int)READ_CROSSLINE, _read_crossline );
    }
  else if(ident ==  MOVIE_TIMESLICE_LEFT || ident == MOVIE_TIMESLICE_RIGHT)
    {
    _timeslice_frame += modifier;
    _timeslice_frame = MaximumValue(1, _timeslice_frame);
    _timeslice_frame = MinimumValue(_cube_movie_control->memoryTimeSliceTotal(),
                           _timeslice_frame);
    _timeslice_scale->setValue(_timeslice_frame);
    _cube_movie_control->changeFrame(Cube::TimeSlice, _timeslice_frame);
    index = _cube_movie_control->memoryFirstTimeSlice() +
             (_timeslice_frame - 1 ) * _prefer_pop->_timeslice_movie_increment; 
    _read_timeslice = _cube_movie_control->getWCFromLineIndex(Cube::TimeSlice,
                                                              index);
    _read_box_timeslice->SetValue( (int)READ_TIMESLICE, _read_timeslice );
    }
  else
    {
    _cube_movie_control->changeFrame(Cube::AllPlots,  _cube_number);
    }

}


//============================================================================
//====================== Handle scan arrows       ============================
//============================================================================
void CubeMoviePop::arrowTrap(void * data, long ident)
{
CubeMoviePop *obj = (CubeMoviePop *)data;  

  obj->readPattern(ident, SCAN_ARROWS);

}



//============================================================================
//====================== Handle line text changes ============================
//============================================================================
void CubeMoviePop::readFocusAction( void * /*data*/, long /*ident*/ )
{
//CubeMoviePop *obj = (CubeMoviePop *)data;

//  obj->readPattern(ident, FOCUS_TRAP);

}

//============================================================================
//====================== Handle line text changes ============================
//============================================================================
void CubeMoviePop::readLosingFocusAction( void * data, long ident )
{
CubeMoviePop *obj = (CubeMoviePop *)data;

 switch(ident)
   {
   case READ_INLINE:
      if(obj->_read_box_inline->lastNotifyReason() == SLTextBox::Activate)
         obj->readPattern(ident, READ_LINES);
   break;

   case READ_CROSSLINE:
      if(obj->_read_box_crossline->lastNotifyReason() == SLTextBox::Activate)
         obj->readPattern(ident, READ_LINES);
   break;

   case READ_TIMESLICE:
      if(obj->_read_box_timeslice->lastNotifyReason() == SLTextBox::Activate)
         obj->readPattern(ident, READ_LINES);
   break;
   }

}

//============================================================================
//====================== Handle load data button requests ====================
//============================================================================
void CubeMoviePop::loadButton( void * data, long ident)
{
CubeMoviePop *obj = (CubeMoviePop *)data;
  
  obj->readPattern(ident, LOAD_BUTTONS);
  
}

//============================================================================
//====================== Create and pop up preference menu  ==================
//============================================================================
void CubeMoviePop::extraButton(int /*ident*/)
{
  _prefer_pop->makeAndManage();
}


//============================================================================
//====================== Update this popup and the preference popup =========
//============================================================================
void CubeMoviePop::updateCubeInfo()
{
char filename_only[256];
char junk[256];
int index;

  if( ! made() ) return;
  _prefer_pop->updateCubeInfo();

  if(strlen(_cube_movie_control->primaryFilename()) > 1)
    {
    parse_file_(_cube_movie_control->primaryFilename(), filename_only, junk);
    sprintf(_cube_id, "Cube %d = ",_cube_number);
    strcat(_cube_id,filename_only);
    wprocShowMsg( _cube_id_label, _cube_id );
    }


  int high, value;
  if (_cube_movie_control->memoryInLineTotal() > 1) {
    high  = _cube_movie_control->memoryInLineTotal ();
    value = _cube_movie_control->visibleInLineFrame() + 1;
    _inline_scale->setRange (1, high);
    setValue (_inline_scale, value, high);
    XtSetSensitive(_load_inline->pushW(LOAD_INLINE), False);
    }
   else
    {
    _inline_scale->setRange(1,1);
    _inline_scale->setValue(1);
    XtSetSensitive(_load_inline->pushW(LOAD_INLINE), True);
    }
  index = _cube_movie_control->memoryFirstInLine() +
                (_inline_frame - 1 ) * _prefer_pop->_inline_movie_increment; 
  _read_inline = _cube_movie_control->getWCFromLineIndex(Cube::InLine,index);
  _read_box_inline->SetValue( (int)READ_INLINE, _read_inline );



  if (_cube_movie_control->memoryCrossLineTotal() > 1) {
    high  = _cube_movie_control->memoryCrossLineTotal ();
    value = _cube_movie_control->visibleCrossLineFrame() + 1;
    _crossline_scale->setRange (1, high);
    setValue (_crossline_scale, value, high);
    XtSetSensitive(_load_crossline->pushW(LOAD_CROSSLINE), False);
    }
  else
    {
    _crossline_scale->setRange(1,1);
    _crossline_scale->setValue(1);
    XtSetSensitive(_load_crossline->pushW(LOAD_CROSSLINE), True);
    } 
  index = _cube_movie_control->memoryFirstCrossLine() +
             (_crossline_frame - 1 ) * _prefer_pop->_crossline_movie_increment; 
  _read_crossline = _cube_movie_control->getWCFromLineIndex(Cube::CrossLine,
                                                                  index);
  _read_box_crossline->SetValue( (int)READ_CROSSLINE, _read_crossline );



  if (_cube_movie_control->memoryTimeSliceTotal() > 1) {
    high  = _cube_movie_control->memoryTimeSliceTotal ();
    value = _cube_movie_control->visibleTimeSliceFrame() + 1;
    _timeslice_scale->setRange (1, high);
    setValue (_timeslice_scale, value, high);
    XtSetSensitive(_load_timeslice->pushW(LOAD_TIMESLICE), False);
    }
  else
    {
    _timeslice_scale->setRange(1,1);
    _timeslice_scale->setValue(1);
    XtSetSensitive(_load_timeslice->pushW(LOAD_TIMESLICE), True);
    }
  index = _cube_movie_control->memoryFirstTimeSlice() +
             (_timeslice_frame - 1 ) * _prefer_pop->_timeslice_movie_increment; 
  _read_timeslice = _cube_movie_control->getWCFromLineIndex(Cube::TimeSlice,
                                                                  index);
  _read_box_timeslice->SetValue( (int)READ_TIMESLICE, _read_timeslice );


  if (_cube_movie_control->memoryCubeTotal() > 1) {
    high  = _cube_movie_control->memoryCubeTotal ();
    value = _cube_movie_control->visibleCubeFrame() + 1;
    _cube_scale->setRange (1, high);
    setValue (_cube_scale, value, high);
  }
  else
    {
    _cube_scale->setRange(1,1);
    _cube_scale->setValue(1);
    }

}

void CubeMoviePop::setValue (SLArrowScale *scale, int value, int high)
{
  if (value > high) {
    scale->setValue (high);
  }
  else {
    scale->setValue (value);
  }
}


//============================================================================
//====================== Set pattern and request data read ===================
//============================================================================
void CubeMoviePop::readPattern(long ident, int /*who_called*/)
{
int first, skip, total;
int stat;
ShellWatch sw;

   switch(ident)
    {
    case PREVIOUS_INLINE_READ:
      if(_prefer_pop->_inline_autoload)
        {
        first = SU::max(0,_cube_movie_control->memoryFirstInLine() - 
                _prefer_pop->_inline_scan_increment);
        skip  = _prefer_pop->_inline_movie_increment - 1;
        total = _prefer_pop->_inline_total;
        }      
      else
        {
        first = SU::max(0,_cube_movie_control->memoryFirstInLine() - 
                      _prefer_pop->_inline_scan_increment);
        skip = total = 0;
        }
      _plot_type = Cube::InLine;
    break;

    case NEXT_INLINE_READ:
      if(_prefer_pop->_inline_autoload)
        {
        first = _cube_movie_control->memoryFirstInLine() + 
                _prefer_pop->_inline_scan_increment;
        skip  = _prefer_pop->_inline_movie_increment - 1;
        total = _prefer_pop->_inline_total;
        }      
      else
        {
        first = _cube_movie_control->memoryFirstInLine() + 
                _prefer_pop->_inline_scan_increment; 
        skip = total = 0;
        }
      _plot_type = Cube::InLine;
    break;

    case READ_INLINE:
      if(_prefer_pop->_inline_autoload)
        {
        first = _cube_movie_control->getLineIndexFromWC(Cube::InLine,
                                                        _read_inline);
        skip  = _prefer_pop->_inline_movie_increment - 1;
        total = _prefer_pop->_inline_total;
        }      
      else
        {
        first = _cube_movie_control->getLineIndexFromWC(Cube::InLine,
                                                        _read_inline);
        skip = total = 0;
        }
      _plot_type = Cube::InLine;
    break;

    case LOAD_INLINE:
      first = _cube_movie_control->memoryFirstInLine();
      skip  = _prefer_pop->_inline_movie_increment - 1;
      total = _prefer_pop->_inline_total;
      _plot_type = Cube::InLine; 
    break;

    case PREVIOUS_CROSSLINE_READ:
      if(_prefer_pop->_crossline_autoload)
        {
        first = SU::max(0,_cube_movie_control->memoryFirstCrossLine() - 
                _prefer_pop->_crossline_scan_increment);
        skip  = _prefer_pop->_crossline_movie_increment - 1;
        total = _prefer_pop->_crossline_total;
        }      
      else
        {
        first = SU::max(0,_cube_movie_control->memoryFirstCrossLine() - 
                      _prefer_pop->_crossline_scan_increment); 
        skip = total = 0;
        } 
      _plot_type = Cube::CrossLine;
    break;

    case NEXT_CROSSLINE_READ:
      if(_prefer_pop->_inline_autoload)
        {
        first = _cube_movie_control->memoryFirstCrossLine() + 
                _prefer_pop->_crossline_scan_increment;
        skip  = _prefer_pop->_crossline_movie_increment - 1;
        total = _prefer_pop->_crossline_total;
        }      
      else
        {
        first = _cube_movie_control->memoryFirstCrossLine() + 
                _prefer_pop->_crossline_scan_increment; 
        skip = total = 0;
        } 
      _plot_type = Cube::CrossLine;
    break;

    case READ_CROSSLINE:
      if(_prefer_pop->_crossline_autoload)
        {
        first = _cube_movie_control->getLineIndexFromWC(Cube::CrossLine,
                                                        _read_crossline);
        skip  = _prefer_pop->_crossline_movie_increment - 1;
        total = _prefer_pop->_crossline_total;
        }      
      else
        {
        first = _cube_movie_control->getLineIndexFromWC(Cube::CrossLine,
                                                        _read_crossline);
        skip = total = 0;
        }
      _plot_type = Cube::CrossLine;
    break;

    case LOAD_CROSSLINE:
      first = _cube_movie_control->memoryFirstCrossLine();
      skip  = _prefer_pop->_crossline_movie_increment - 1;
      total = _prefer_pop->_crossline_total;
      _plot_type = Cube::CrossLine; 
    break;

    case PREVIOUS_TIMESLICE_READ:
      if(_prefer_pop->_timeslice_autoload)
        {
        first = SU::max(0,_cube_movie_control->memoryFirstTimeSlice() - 
                _prefer_pop->_timeslice_scan_increment);
        skip  = _prefer_pop->_timeslice_movie_increment - 1;
        total = _prefer_pop->_timeslice_total;
        }      
      else
        {
        first = SU::max(0,_cube_movie_control->memoryFirstTimeSlice() - 
                      _prefer_pop->_timeslice_scan_increment); 
        skip = total = 0;
        } 
      _plot_type = Cube::TimeSlice;
    break;

    case NEXT_TIMESLICE_READ:
      if(_prefer_pop->_timeslice_autoload)
        {
        first = _cube_movie_control->memoryFirstTimeSlice() + 
                _prefer_pop->_timeslice_scan_increment;
        skip  = _prefer_pop->_timeslice_movie_increment - 1;
        total = _prefer_pop->_timeslice_total;
        }      
      else
        {
        first = _cube_movie_control->memoryFirstTimeSlice() + 
                _prefer_pop->_timeslice_scan_increment;
        skip = total = 0;
        } 
      _plot_type = Cube::TimeSlice;
    break;

    case READ_TIMESLICE:
      if(_prefer_pop->_timeslice_autoload)
        {
        first = _cube_movie_control->getLineIndexFromWC(Cube::TimeSlice,
                                                        _read_timeslice);
        skip  = _prefer_pop->_timeslice_movie_increment - 1;
        total = _prefer_pop->_timeslice_total;
        }      
      else
        {
        first = _cube_movie_control->getLineIndexFromWC(Cube::TimeSlice,
                                                        _read_timeslice);
        skip = total = 0;
        }
      _plot_type = Cube::TimeSlice;
    break;

    case LOAD_TIMESLICE:
      first = _cube_movie_control->memoryFirstTimeSlice();
      skip  = _prefer_pop->_timeslice_movie_increment - 1;
      total = _prefer_pop->_timeslice_total;
      _plot_type = Cube::TimeSlice; 
    break;

    case LOAD_ALL:
      first = _cube_movie_control->memoryFirstInLine();
      skip  = _prefer_pop->_inline_movie_increment - 1;
      total = _prefer_pop->_inline_total;
      _plot_type = Cube::InLine; 
      stat= _cube_movie_control->readData(_plot_type, first, skip, total);
      if (!stat) 
        {
         CubePlotError error (W(), _cube_movie_control->getDisplayedCube() );
        }
      first = _cube_movie_control->memoryFirstCrossLine();
      skip  = _prefer_pop->_crossline_movie_increment - 1;
      total = _prefer_pop->_crossline_total;
      _plot_type = Cube::CrossLine; 
      stat= _cube_movie_control->readData(_plot_type, first, skip, total);
      if (!stat) 
        {
         CubePlotError error (W(), _cube_movie_control->getDisplayedCube() );
        }
      first = _cube_movie_control->memoryFirstTimeSlice();
      skip  = _prefer_pop->_timeslice_movie_increment - 1;
      total = _prefer_pop->_timeslice_total;
      _plot_type = Cube::TimeSlice; 
      stat= _cube_movie_control->readData(_plot_type, first, skip, total);
      if (!stat)
        { 
         CubePlotError error (W(), _cube_movie_control->getDisplayedCube() );
        }
      break;

    }//end switch

    if(ident == LOAD_ALL) return;

    stat= _cube_movie_control->readData(_plot_type, first, skip, total);
    if (!stat) {
       CubePlotError error (W(), _cube_movie_control->getDisplayedCube() );
    }

}


//============================================================================
//====================== Set load button sensitivity       ===================
//============================================================================
void CubeMoviePop::setLoadInline(Boolean sensitive)
{
  XtSetSensitive(_load_inline->pushW(LOAD_INLINE), sensitive);
}

//============================================================================
//====================== Set load button sensitivity       ===================
//============================================================================
void CubeMoviePop::setLoadCrossline(Boolean sensitive)
{
  XtSetSensitive(_load_crossline->pushW(LOAD_INLINE), sensitive);
}

//============================================================================
//====================== Set load button sensitivity       ===================
//============================================================================
void CubeMoviePop::setLoadTimeslice(Boolean sensitive)
{
  XtSetSensitive(_load_timeslice->pushW(LOAD_INLINE), sensitive);
}
