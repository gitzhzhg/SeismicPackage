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


//=========================================================================
//========== Shared movie parameters used by the cmp             ==========
//========== and semblance plots                                 ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_common_movie_gui.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "sl/sl_form.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_common_movie_gui.hh"
#include "plot_image.hh"


/*
    "*domovie.labelString:        Set Movie Parameters",
    "*first_panelL.labelString:   First Panel:",
    "*skip_panelsL.labelString:   Panels To Skip :",
    "*total_panelsL.labelString:  Total Panels:",
    "*first_panel.value:          1",
    "*skip_panels.value:          0",
    "*total_panels.value:         1",
    */



//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaCommonMovieGui::VaCommonMovieGui( SLDelay            *p,
                                    char               *name,
                                    HelpCtx            hctx,
                                    VaPlotCommonParams *common_params,
                                    int                which_plot)
                              : SLForm(p, name, hctx, True, False, False)
{
 
static SLTog movietog[]  = {
         { "domovie", NULL, DO_MOVIE },
};
movietog[0].target = &_do_movie;


static SLText movietexts[]  = {
 {"first_panel",  NULL, NULL, SLType_int,   FIRST_PANEL},
 {"skip_panels",  NULL, NULL, SLType_int,   SKIP_PANELS},
 {"total_panels", NULL, NULL, SLType_int,   TOTAL_PANELS},
};
movietexts[0].target = &_first_panel;
movietexts[1].target = &_skip_panels;
movietexts[2].target = &_total_panels;




  _common_params = common_params;
  _which_plot = which_plot;
  _first_time = True;
  _first_panel = 1;
  _skip_panels = 0;
  _total_panels = 1;
  _do_movie     = False;


  _movie_tog= new SLTogBox( this, "movie_tog",getHelpCtx(),
                            movietog, XtNumber(movietog),False, False );

  _movie_box = new SLTextBox( this, "movie_box", getHelpCtx(), 
                              movietexts, XtNumber(movietexts), True, 1, True);

}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaCommonMovieGui::~VaCommonMovieGui()
{
  delete _movie_box;
  delete _movie_tog;
}

//=========================================================================
//========================= Make  =========================================
//=========================================================================
Widget VaCommonMovieGui::make(Widget p)
{

  if (made()) return topWidget ();

  SLForm::make (p);
  p = wParent ();
  
  XtVaSetValues( _movie_tog->W(),
                               XmNtopAttachment,   XmATTACH_FORM,
                               XmNtopOffset,       1, 
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      10, NULL );

   XtVaSetValues( _movie_box->W(),
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      0,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _movie_tog->W(), NULL);
   
   _movie_box->SetValue(FIRST_PANEL,   1L);
   _movie_box->SetValue(SKIP_PANELS,   0L);
   _movie_box->SetValue(TOTAL_PANELS,  1L);


   manage();

   return topWidget();
}


//=========================================================================
//========================== Handle notifies       ========================
//=========================================================================
Boolean VaCommonMovieGui::notifyComplex(SLDelay *obj, int ident)
{



  if(obj == _movie_tog)//movie toggle changing
    {
    _common_params->setMovie(_movie_tog->IsSelected(DO_MOVIE));
    XtSetSensitive(_movie_box->W(), _movie_tog->IsSelected(DO_MOVIE));
    }
  else//movie box parameter changing
    {
    setMovieParameters(ident);
    }

  notifyCommon();

  _common_params->setCmpReplot(True);
  _common_params->setSemReplot(True);

  return True;
}


//=========================================================================
//==================== Set movie parameters     ===========================
//=========================================================================
void VaCommonMovieGui::setMovieParameters(long ident)
{
long max_possible;
long current_maxpnl;
float temp;

  if(_first_panel < 1) _first_panel = 1;
  if(_skip_panels < 0) _skip_panels = 0;
  if(_total_panels< 1) _total_panels= 1;

  max_possible = _common_params->getFileMaxMovies();
  temp = ( (float)max_possible - _first_panel ) / (_skip_panels + 1.0) + 1.0;
  current_maxpnl = (long)temp;


  switch(ident)
    {
    case FIRST_PANEL:
      _total_panels = current_maxpnl;
      _movie_box->SetValue(TOTAL_PANELS,current_maxpnl);
      _common_params->setFirstMovie(max(1,_first_panel)); 
      _common_params->setTotalMovies(current_maxpnl);
      if(_first_panel >= max_possible)
        {
        _first_panel  = max_possible;
        _skip_panels  = 0;
        _total_panels = 1;
        _movie_box->SetValue(FIRST_PANEL,  _first_panel);
        _movie_box->SetValue(SKIP_PANELS,  _skip_panels);
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
        _common_params->setFirstMovie(max(1,_first_panel)); 
        _common_params->setSkipMovies(_skip_panels);
        _common_params->setTotalMovies(_total_panels); 
        }
      break;


    case SKIP_PANELS:
      if(_skip_panels)
        {
        temp = ((float)max_possible-_first_panel ) / (_skip_panels+1.0) + 1.0;
        current_maxpnl = (long)temp;
        _movie_box->SetValue(TOTAL_PANELS, current_maxpnl);
        _common_params->setSkipMovies(_skip_panels);
        _common_params->setTotalMovies(current_maxpnl);
        if( _skip_panels >= (max_possible - _first_panel) )
          {
          _skip_panels  = 0;
          temp = ( (float)max_possible - _first_panel )
                      / (_skip_panels + 1.0) + 1.0;
          current_maxpnl = (long)temp;
          _total_panels   = (int)(max_possible - _first_panel + 1);
          _movie_box->SetValue(SKIP_PANELS,  _skip_panels);
          _movie_box->SetValue(TOTAL_PANELS, _total_panels);
          _common_params->setSkipMovies(_skip_panels);
          _common_params->setTotalMovies(_total_panels); 
          }
        }
      else
        {
        _total_panels = current_maxpnl;
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
        _common_params->setSkipMovies(_skip_panels);
        _common_params->setTotalMovies(_total_panels); 
        }
      break;


    case TOTAL_PANELS:
      if(_total_panels > current_maxpnl)
         _total_panels = current_maxpnl;
      _movie_box->SetValue(TOTAL_PANELS, _total_panels);
      _common_params->setTotalMovies(_total_panels);
      break;
    }//end switch

}


//=========================================================================
//==================== Public sensitive method     ========================
//=========================================================================

void VaCommonMovieGui::setSensitive(Boolean sensitive)
{

  XtSetSensitive(_movie_box->W(), sensitive);
    
}


//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaCommonMovieGui::ValidInput()
{
 Boolean stat = True;
 
 if (made()) 
   {
   }

 return (stat);

}


//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaCommonMovieGui::setToFileDefaults()
{

}


//=========================================================================
//====================== Manage ===========================================
//=========================================================================
void VaCommonMovieGui::manage()
{


 SLForm::manage();

 if (_first_time) 
   {
   }
 else
   {
   }


 _first_time= False;
 
 XtManageChild(topWidget()); 
}



void VaCommonMovieGui::reloadSystemDefaults(Boolean /*do_method*/)
{
  _movie_box->SetValue(FIRST_PANEL,   1L);
  _movie_box->SetValue(SKIP_PANELS,   0L);
  _movie_box->SetValue(TOTAL_PANELS,  1L);
}


//=========================================================================
//====================  Update public method    ===========================
//====================  Called on new file      ===========================
//=========================================================================
void VaCommonMovieGui::updateParams()
{

}


//=========================================================================
//====================  Set movie option        ===========================
//=========================================================================
void VaCommonMovieGui::setMovieOption(Boolean set)
{
  _movie_tog->SetTog(DO_MOVIE, set); 
}

//=========================================================================
//====================  Set movie option        ===========================
//=========================================================================
Boolean VaCommonMovieGui::getMovieOption()
{
  return _movie_tog->IsSelected(DO_MOVIE);
}

//=========================================================================
//====================  Set movie file limits   ===========================
//=========================================================================
void VaCommonMovieGui::setFileMaxMovies(int which_plot, long max_movies)
{
  _common_params->setFileMaxMovies(which_plot, max_movies);
}


//=========================================================================
//====================  Set movie first movie panel =======================
//=========================================================================
void VaCommonMovieGui::setFirstMovie(long first_movie)
{
  _common_params->setFirstMovie(max(1,first_movie));
  _movie_box->SetValue(FIRST_PANEL, max(1,first_movie));
  setMovieParameters(FIRST_PANEL);
}


//=========================================================================
//====================  Set movie skip movie panels =======================
//=========================================================================
void VaCommonMovieGui::setSkipMovies(long skip_movies)
{
  _common_params->setSkipMovies(skip_movies);
  _movie_box->SetValue(SKIP_PANELS, skip_movies);
  setMovieParameters(SKIP_PANELS);
}


//=========================================================================
//====================  Set movie first movie panel =======================
//=========================================================================
void VaCommonMovieGui::setTotalMovies(long total_movies)
{
  _common_params->setTotalMovies(total_movies);
 _movie_box->SetValue(TOTAL_PANELS, total_movies);
  setMovieParameters(TOTAL_PANELS);
}

//=========================================================================
//====================  Get first movie panel =============================
//=========================================================================
long VaCommonMovieGui::getFirstMovie()
{
  return _common_params->getFirstMovie();
}

//=========================================================================
//====================  Get skip movie panels       =======================
//=========================================================================
long VaCommonMovieGui::getSkipMovies()
{
  return _common_params->getSkipMovies();
}

//=========================================================================
//====================  Get total movie panels  ===========================
//=========================================================================
long VaCommonMovieGui::getTotalMovies()
{
  return _common_params->getTotalMovies();
}

//=========================================================================
//====================  Notify all guis of changes    =====================
//=========================================================================
void VaCommonMovieGui::notifyCommon()
{
  _common_params->synchMovieGuis();
}

//=========================================================================
//====================  Notify all guis of changes    =====================
//=========================================================================
void VaCommonMovieGui::synch()
{

  switch (_which_plot)
    {
    case VaPlot::SEMBLANCE:
      if(_common_params->haveSemblanceFileLimits())
        {
        _movie_tog->SetTog(DO_MOVIE,    _common_params->getMovie());
        XtSetSensitive(_movie_box->W(), _common_params->getMovie());
        }
      else
        {
        _common_params->setSemReplot(False);
        }
    break;

    case VaPlot::CMP:
      if(_common_params->haveCmpFileLimits())
        {
        _movie_tog->SetTog(DO_MOVIE,    _common_params->getMovie());
        XtSetSensitive(_movie_box->W(), _common_params->getMovie());
        }
      else
        {
        _common_params->setCmpReplot(False);
        }
    break;
    }

  _movie_box->SetValue(FIRST_PANEL,  _common_params->getFirstMovie());
  _movie_box->SetValue(SKIP_PANELS,  _common_params->getSkipMovies());
  _movie_box->SetValue(TOTAL_PANELS, _common_params->getTotalMovies());

}
