//***********************************************
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
//Creates menu to control cube movie preferences
//***********************************************

#include "cube/cube_movie_pop.hh"
#include "cube/cube_movie_control.hh"
#include "cube/cube_movie_preferpop.hh"
#include "cprim.h"
#include "sl/shell_stat_msg.hh"
#include <Xm/Label.h>


static String  defres[]= {
  "*T0.labelString:                     Scan Increment",
  "*T1.labelString:                     Movie Increment",
  "*T2.labelString:                     Total Movie Frames",
  "*inlinestartL.labelString:           Inline",
  "*inlinestart.value:                  1",
  "*inlineskip.value:                   1", 
  "*inlinetotal.value:                  1",
  "*crosslinestartL.labelString:        Crossline",
  "*crosslinestart.value:               1",
  "*crosslineskip.value:                1",
  "*crosslinetotal.value:               1",
  "*timeslicestartL.labelString:        Horizontal Slice",
  "*timeslicestart.value:               1",
  "*timesliceskip.value:                1",
  "*timeslicetotal.value:               1",
  "*auto_load*.labelString: Auto Load",
  NULL
};

//============================================================================
//====================== Create and pop up preference menu  ==================
//============================================================================
CubeMoviePreference::CubeMoviePreference( Widget           p,
                                          char             *name,
                                          HelpCtx          hctx,
                                          CubeMoviePop     *cube_movie_pop,
                                          CubeMovieControl *cube_movie_control)
                            : SLFPopSep(p,name,FP_DOREMOVE, hctx,False,False)
{

  setDefaultResources( p, name, defres);
  _cube_movie_pop = cube_movie_pop;
  _cube_movie_control = cube_movie_control;

  _inline_autoload           = 0;
  _crossline_autoload        = 0;
  _timeslice_autoload        = 0;
  _cube_autoload             = 0;
  _inline_scan_increment     = 1;
  _inline_movie_increment    = 1;
  _inline_total              = 1;
  _previous_inline_total     = 0;
  _crossline_scan_increment  = 1;
  _crossline_movie_increment = 1;
  _crossline_total           = 1;
  _previous_crossline_total  = 0;
  _timeslice_scan_increment  = 1;
  _timeslice_movie_increment = 1;
  _timeslice_total           = 1;
  _previous_timeslice_total  = 0;
}

//============================================================================
//====================== Destroy pop up preference menu  =====================
//============================================================================
CubeMoviePreference::~CubeMoviePreference()
{
}

//============================================================================
//====================== Make                               ==================
//============================================================================
Widget CubeMoviePreference::make(Widget p)
{
static SLText texts[]  = {
    {"inlinestart",    NULL, NULL, SLType_int,   INLINE_SCAN_INCREMENT},
    {"inlineskip",     NULL, NULL, SLType_int,   INLINE_MOVIE_INCREMENT},
    {"inlinetotal",    NULL, NULL, SLType_int,   INLINE_TOTAL},
    {"crosslinestart", NULL, NULL, SLType_int,   CROSSLINE_SCAN_INCREMENT},
    {"crosslineskip",  NULL, NULL, SLType_int,   CROSSLINE_MOVIE_INCREMENT},
    {"crosslinetotal", NULL, NULL, SLType_int,   CROSSLINE_TOTAL},
    {"timeslicestart", NULL, NULL, SLType_int,   TIMESLICE_SCAN_INCREMENT},
    {"timesliceskip",  NULL, NULL, SLType_int,   TIMESLICE_MOVIE_INCREMENT},
    {"timeslicetotal", NULL, NULL, SLType_int,   TIMESLICE_TOTAL},
 };
texts[0].target= &_inline_scan_increment;
texts[1].target= &_inline_movie_increment;
texts[2].target= &_inline_total;
texts[3].target= &_crossline_scan_increment;
texts[4].target= &_crossline_movie_increment;
texts[5].target= &_crossline_total;
texts[6].target= &_timeslice_scan_increment;
texts[7].target= &_timeslice_movie_increment;
texts[8].target= &_timeslice_total;

static SLTog autoload_inline[]    = { {"",  NULL, INLINE_AUTOLOAD    },};
static SLTog autoload_crossline[] = { {"",  NULL, CROSSLINE_AUTOLOAD },};
static SLTog autoload_timeslice[] = { {"",  NULL, TIMESLICE_AUTOLOAD },}; 
static SLTog autoload_cube[]      = { {"",  NULL, CUBE_AUTOLOAD      },};
autoload_inline[0].target= &_inline_autoload;
autoload_crossline[0].target= &_crossline_autoload;
autoload_timeslice[0].target= &_timeslice_autoload;
autoload_cube[0].target= &_cube_autoload;

Widget parent;
    
  
  
  if ( made() )
    {
    updateCubeInfo(); 
    SLFPopSep::manage();
    return(topWidget()); 
    }

  sprintf(_cube_id, "Cube %d = ",_cube_movie_pop->_cube_number);

  parent = p ? p : wParent();

  SLFPopSep::make(parent);
  
  setTitle("Cube Movie Preferences");

  _line_select_text = new SLTextBox( topWidget(),"select_texts",getHelpCtx(),
                               texts, XtNumber(texts), True, 3, 
                               True, True, True );
  _line_select_text->setAltLosingAction((SLTextfunc)
                               lineSelectLosingFocusAction, this);
  _line_select_text->setAltFocusAction ( (SLTextfunc)lineSelectFocusAction, 
                               this );
  

  _auto_load_inline = new SLTogBox(topWidget(), "auto_loadil",getHelpCtx(),
                            autoload_inline, XtNumber(autoload_inline), 
                            False, False, True );
  _auto_load_inline->setAltChoiceAction((SLToggleButtonfunc)autoLoadAction,
                            this);
  _auto_load_crossline = new SLTogBox(topWidget(), "auto_loadcl",getHelpCtx(),
                            autoload_crossline, XtNumber(autoload_crossline), 
                            False, False, True );
  _auto_load_crossline->setAltChoiceAction((SLToggleButtonfunc)autoLoadAction,
                            this);
  _auto_load_timeslice = new SLTogBox(topWidget(), "auto_loadts",getHelpCtx(),
                            autoload_timeslice, XtNumber(autoload_timeslice), 
                            False, False, True );
  _auto_load_timeslice->setAltChoiceAction((SLToggleButtonfunc)autoLoadAction,
                            this); 
  _auto_load_cube = new SLTogBox(topWidget(), "auto_loadc",
                          getHelpCtx(),autoload_cube,XtNumber(autoload_cube), 
                          False, False, True );
  _auto_load_cube->setAltChoiceAction((SLToggleButtonfunc)autoLoadAction,
                            this);

  XtVaSetValues( _line_select_text->W(), 
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        10,
                           XmNleftAttachment,   XmATTACH_FORM, 
                           XmNleftOffset,       10, NULL);


   Widget load_label= XtVaCreateManagedWidget( "Auto Load",
                           xmLabelWidgetClass,  topWidget(),
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       _line_select_text->W(),
                           XmNleftOffset,       10,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        17, NULL );

   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        5,
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       load_label,
                           XmNleftOffset,       5,
                           XmNrightAttachment,  XmATTACH_FORM,
                           XmNrightOffset,        5, NULL);

   XtVaSetValues( _auto_load_inline->W(), 
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        load_label,
                           XmNtopOffset,        2,
                           XmNleftAttachment,   XmATTACH_WIDGET, 
                           XmNleftWidget,       _line_select_text->W(), 
                           XmNleftOffset,       40, NULL); 

   XtVaSetValues( _auto_load_crossline->W(), 
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _auto_load_inline->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_WIDGET, 
                           XmNleftWidget,       _line_select_text->W(), 
                           XmNleftOffset,       40,NULL); 

   XtVaSetValues( _auto_load_timeslice->W(), 
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _auto_load_crossline->W(),
                           XmNtopOffset,        0,
                           XmNleftAttachment,   XmATTACH_WIDGET, 
                           XmNleftWidget,       _line_select_text->W(), 
                           XmNleftOffset,       40,
                  //XmNbottomAttachment, XmATTACH_WIDGET,
                  //XmNbottomWidget, bottomSeparator(),
                  //XmNbottomOffset, 50,
                           NULL);

  

  
  

  _cube_id_label = XtVaCreateManagedWidget(_cube_id, 
                           xmLabelWidgetClass,  topWidget(),
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _line_select_text->W(),
                           XmNtopOffset,        10,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       10, NULL);
  
  Widget phantomlabB = XtVaCreateManagedWidget (
                               "",                  xmLabelWidgetClass, 
                               topWidget(),
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       1,
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _cube_id_label,
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,    bottomSeparator(),
                               XmNbottomOffset,     25,
                               NULL);
  



  Widget cube_scan_label=XtVaCreateManagedWidget("Load all cubes on scan", 
                           xmLabelWidgetClass,  topWidget(),
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _line_select_text->W(),
                           XmNtopOffset,        10,
                           XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNrightWidget,      _auto_load_timeslice->W(), 
                           XmNrightOffset,      73, NULL);

  XtVaSetValues( _auto_load_cube->W(), 
                           XmNtopAttachment,    XmATTACH_WIDGET,
                           XmNtopWidget,        _line_select_text->W(),
                           XmNtopOffset,        8,
                           XmNrightAttachment,   XmATTACH_OPPOSITE_WIDGET, 
                           XmNrightWidget,      _auto_load_timeslice->W(),
                           XmNrightOffset,      0, NULL);

  defaultButton(FP_OK, False);

  updateCubeInfo();

  return topWidget();
}

//============================================================================
//====================== Focus action for line changes      ==================
//============================================================================
void CubeMoviePreference::lineSelectFocusAction(void *data, long which)
{
CubeMoviePreference *obj = (CubeMoviePreference *)data;

  switch(which)
    {
    case INLINE_TOTAL:
       obj->_previous_inline_total =  obj->_inline_total;
    break;
 
    case CROSSLINE_TOTAL:
       obj->_previous_crossline_total =  obj->_crossline_total;
    break;

    case TIMESLICE_TOTAL:
       obj->_previous_timeslice_total =  obj->_timeslice_total;
    break;     
   
    case INLINE_MOVIE_INCREMENT:
       obj->_previous_inline_increment =  obj->_inline_movie_increment;
    break;

    case CROSSLINE_MOVIE_INCREMENT:
       obj->_previous_crossline_increment =  obj->_crossline_movie_increment;
    break;

    case TIMESLICE_MOVIE_INCREMENT:
       obj->_previous_timeslice_increment =  obj->_timeslice_movie_increment;
    break;

    }



}

//============================================================================
//====================== Losing focus action for line changes ================
//============================================================================
void CubeMoviePreference::lineSelectLosingFocusAction(void *data, long which )
{
CubeMoviePreference *obj = (CubeMoviePreference *)data;

  switch(which)
    {
    case INLINE_TOTAL:
       if(obj->_previous_inline_total !=  obj->_inline_total)
         obj->_cube_movie_pop->setLoadInline(True);
    break;
       
    case CROSSLINE_TOTAL:
        if(obj->_previous_crossline_total !=  obj->_crossline_total)
          obj->_cube_movie_pop->setLoadCrossline(True); 
    break;

    case TIMESLICE_TOTAL:
       if(obj->_previous_timeslice_total !=  obj->_timeslice_total)
         obj->_cube_movie_pop->setLoadTimeslice(True);
    break;     

    case INLINE_MOVIE_INCREMENT:
       if(obj->_previous_inline_increment != obj->_inline_movie_increment)
         obj->_cube_movie_pop->setLoadInline(True);
    break;

    case CROSSLINE_MOVIE_INCREMENT:
       if(obj->_previous_crossline_increment != obj->_crossline_movie_increment)
         obj->_cube_movie_pop->setLoadCrossline(True);
    break;

    case TIMESLICE_MOVIE_INCREMENT:
       if(obj->_previous_timeslice_increment != obj->_timeslice_movie_increment)
         obj->_cube_movie_pop->setLoadTimeslice(True);
    break;
    }

}

//============================================================================
//====================== Handle the autoload button actions ==================
//============================================================================
void CubeMoviePreference::autoLoadAction( void * /*data*/, long /*which*/ )
{
//CubeMoviePreference *obj = (CubeMoviePreference *)data;
}

//============================================================================
//====================== Get information about the cubes    ==================
//============================================================================
void CubeMoviePreference::updateCubeInfo()
{
char filename_only[256];
char junk[256];

  if( !made() ) return;

/*
  _line_select_text->SetValue(INLINE_MOVIE_INCREMENT,
                          (long)_cube_movie_control->memoryInLineSkip()+1); 
  _line_select_text->SetValue(CROSSLINE_MOVIE_INCREMENT,
                          (long)_cube_movie_control->memoryCrossLineSkip()+1);
  _line_select_text->SetValue(TIMESLICE_MOVIE_INCREMENT,
                          (long)_cube_movie_control->memoryTimeSliceSkip()+1);

  _line_select_text->SetValue(INLINE_TOTAL,
                          (long)_cube_movie_control->memoryInLineTotal());  
  _line_select_text->SetValue(CROSSLINE_TOTAL, 
                          (long)_cube_movie_control->memoryCrossLineTotal());  
  _line_select_text->SetValue(TIMESLICE_TOTAL, 
                          (long)_cube_movie_control->memoryTimeSliceTotal());
*/  

  //copy the filename into this string later
  if(strlen(_cube_movie_control->primaryFilename()) > 1)
    {
    parse_file_(_cube_movie_control->primaryFilename(), filename_only, junk);
    sprintf(_cube_id, "Cube %d = ",_cube_movie_pop->_cube_number);
    strcat(_cube_id,filename_only);
    wprocShowMsg( _cube_id_label, _cube_id );
    }
}
