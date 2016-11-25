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
//========== Eta plot gui class                            ==========
//========== Author Michael L. Sherrill 08/97                    ==========
//=========================================================================

// $Id: va_eta_gui.cc,v 1.3 2004/10/12 20:58:27 cornkc Exp $
// $Name: 12Oct04 $





//!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Some of this is temporary until the vel file format supports eta vals
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!










#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/psuedo_widget.hh"
#include "sp/seis_plot.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_update.hh"
#include "vf/vf_dataset.hh"
#include "vaplots/va_eta_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_eta_gui.hh"
#include "sp/seis_ctype.hh"
#include "vf/vf_file_base.hh"



#define    PWIDTH_DEF                    4.0F
#define    PHEIGHT_DEF                   4.0F
#define    WINDOW_LENGTH_DEF             0.02F
#define    HORZ_VELOCITY_WIDTH_DEF       500.0F
#define    NMO_VELOCITY_WIDTH_DEF        500.0F
#define    NMO_VELOCITY_INCREMENT_DEF    20.0F
#define    HORZ_VELOCITY_INCREMENT_DEF   20.0F
#define    GRADE_VERT_DEF                True
#define    GRADE_HORZ_DEF                True
#define    PLOT_CON_DEF                  False
#define    CONNUM_DEF                    10L
#define    REPLACE_WARNING "Warning, changing input data will remove all ETA\n\
data now in memory. Is this what you want to do?"
#define    NOINPUT_WARNING "Warning, no input file will remove all ETA data \n\
now in memory. Is this what you want to do?"


#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

static String  defres[]= {
    "_popup.title:                            Eta Menu",
    "*window_lengthL.labelString:             Window Length:",
    "*window_length.value:                    0.02",
    "*nmo_velocity_widthL.labelString:        Nmo Velocity Width:",
    "*nmo_velocity_width.value:               500.0",
    "*nmo_velocity_incrementL.labelString:    Nmo Velocity Increment:",
    "*nmo_velocity_increment.value:           20.0",
    "*horz_velocity_widthL.labelString:       Horizontal Velocity Width:",
    "*horz_velocity_width.value:              500.0",
    "*horz_velocity_incrementL.labelString:   Horizontal Velocity Increment:",
    "*horz_velocity_increment.value:          20.0",
    "*connumL.labelString:                    Number of Contours:",
    "*connum.value:                           10",
    "*pwidthL.labelString:                    Plot Width:",
    "*pwidth.value:                           5.0",
    "*pheightL.labelString:                   Plot Height:",
    "*pheight.value:                          5.0",
    "*gradvert.labelString:                   Grade Vertical",
    "*gradhor.labelString:                    Grade Horizontal",
    "*gradvert.set:                           True",
    "*gradhor.set:                            True",
    "*plot_con.labelString:                   Plot Contours Only",
    "*plot_con.set:                           False",
    NULL, };


enum {WINDOW_LENGTH,  
      NMO_VELOCITY_WIDTH,       NMO_VELOCITY_INCREMENT,   
      HORZ_VELOCITY_WIDTH,      HORZ_VELOCITY_INCREMENT,
      CONNUM,                   PWIDTH,
      PHEIGHT,                  GRADE_VERT,   
      GRADE_HORZ,               PLOT_CON};



//=========================================================================
//========= Class to handle replacing eta data with new file ==============
//=========================================================================
MySLQuestPop::MySLQuestPop(SLDelay *slparent, char *name, 
                           const char *question, VaEtaGui *gui) 
  : SLQuestPop (slparent, name, question, FALSE, NULL, NULL, NULL)
{
  _gui = gui;
}

void MySLQuestPop::answerYes()
{
  _gui->replaceData(True);
  _gui->possibleFileChange();
}


void MySLQuestPop::answerNo()
{
  _gui->replaceData(False);
  _gui->possibleFileChange();
}


//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaEtaGui::VaEtaGui( Widget            p,
                    char              *name,
                    HelpCtx           hctx,
                    VaEtaPlot         *eta_plot,
                    VaCmpPlot         *cmp_plot)
                : SLFPopSep(p,name,FP_DOOK|FP_DOCANCEL|FP_DOHELP,
                            hctx,False,False),
                 _eta_plot(eta_plot), _eta_sp(eta_plot->SP()), 
                 _cmp_plot(cmp_plot), _cmp_sp(cmp_plot->SP()),
                 _have_input_file(False), _have_output_file(False),
                 _replace_data(True)

{

  static SLText texts[]  = {
    {"window_length",          "range:0.00 *,default:0.020",    NULL,
     SLType_float, WINDOW_LENGTH        },

    {"nmo_velocity_width",     "range:0.00 9999999.00,default:500.00",NULL,
     SLType_float,NMO_VELOCITY_WIDTH },

    {"nmo_velocity_increment", "range:0.01 9999999.00,default:20.0",   NULL,
     SLType_float,NMO_VELOCITY_INCREMENT},

    {"horz_velocity_width",     "range:0.00 9999999.00,default:500.00",NULL,
     SLType_float,HORZ_VELOCITY_WIDTH },

    {"horz_velocity_increment","range:0.01 99999999.0,default:20.0",   NULL,
     SLType_float,HORZ_VELOCITY_INCREMENT},

    {"connum",                 "range:0 20,default:10",                 NULL,
     SLType_int,                CONNUM },

    {"pwidth",                 "range:.1 2000.0,default:4.00",         NULL,
     SLType_float,              PWIDTH },

    {"pheight",                "range:.1 2000.0,default:4.00",         NULL,
     SLType_float,              PHEIGHT},
  };

  texts[0].target = &_window_length;
  texts[1].target = &_nmo_velocity_width;
  texts[2].target = &_nmo_velocity_increment;
  texts[3].target = &_horz_velocity_width;
  texts[4].target = &_horz_velocity_increment;
  texts[5].target = &_connum;
  texts[6].target = &_pwidth;
  texts[7].target = &_pheight;

  static SLTog togs[]  = {
    { "gradvert", NULL, GRADE_VERT },
    { "gradhor",  NULL, GRADE_HORZ },
    { "plot_con", NULL, PLOT_CON },
  };
  togs[0].target = &_grade_vert;
  togs[1].target = &_grade_horz;
  togs[2].target = &_plot_con;



  _first_time = True;

  setDefaultResources( p, name, defres);


  
  _vel_infilebase = new VfFileBase("VelocityInFile", "vel", 
                                    _eta_plot->getTempVfManager(),
                                    VfFileBase::USE_FOR_INPUT, False);
  _vel_infile_choice = new SLFileChoice(this, "vel_intchoice", SLpFile::_INPUT,
                                        _vel_infilebase,
                                        "ETA File Input...",
                                        NULL, TRUE);
  _vel_infile_choice->setComplexNotify(this);

  _vel_outfilebase = new VfFileBase("VelocityOutFile", "vel", 
                                    _eta_plot->getTempVfManager(),
                                    VfFileBase::USE_FOR_OUTPUT, False);
  _vel_outfile_choice = new SLFileChoice(this, "vel_outchoice",SLpFile::_OUTPUT,
                                         _vel_outfilebase,
                                         "ETA File Output",
                                         NULL, TRUE);
  _vel_outfile_choice->setComplexNotify(this);






  _eta_params_box= new SLTextBox( this, "eta_params_box", getHelpCtx(), 
                                  texts, XtNumber(texts), True, 1, True,
                                  False );
  _eta_params_box->setComplexNotify(this);;

  _grade_box= new SLTogBox( this, "grade", getHelpCtx(), togs, XtNumber(togs),
                            False, False );

  //  make(p);

  //_colorbar->loadToSeisPlot(); 
}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaEtaGui::~VaEtaGui()
{
  if(_eta_params_box)       delete _eta_params_box;
  if(_grade_box)            delete _grade_box;
}



//=========================================================================
//========================= Make  =========================================
//=========================================================================
Widget VaEtaGui::make(Widget p)
{

  if (made()) return topWidget();

  Widget parent = p ? p : wParent();

  SLFPopSep::make(parent);
 
  XtVaSetValues(topWidget(),
                XmNdialogStyle  ,  XmDIALOG_FULL_APPLICATION_MODAL,
                NULL);



  XtVaSetValues( _vel_infile_choice->W(),
                 XmNtopAttachment,   XmATTACH_FORM,
                 XmNtopOffset,       10,
                 XmNleftAttachment,  XmATTACH_FORM, 
                 XmNleftOffset,      10, 
                 XmNrightAttachment, XmATTACH_FORM,
                 XmNrightOffset,     10, NULL );

  XtVaSetValues( _vel_outfile_choice->W(),
                 XmNtopAttachment,   XmATTACH_WIDGET,
                 XmNtopWidget,       _vel_infile_choice->W(),
                 XmNtopOffset,       10,
                 XmNleftAttachment,  XmATTACH_FORM, 
                 XmNleftOffset,      10, 
                 XmNrightAttachment, XmATTACH_FORM,
                 XmNrightOffset,     10, NULL );

  XtVaSetValues(  _eta_params_box->W(),
                  XmNtopAttachment,    XmATTACH_WIDGET,
                  XmNtopWidget,        _vel_outfile_choice->W(),
                  XmNleftAttachment,   XmATTACH_FORM,
                  XmNleftOffset,       10,
                  NULL);

  XtVaSetValues(  _grade_box->W(), 
                  XmNleftAttachment,   XmATTACH_FORM,
                  XmNleftOffset,       10,
                  XmNtopAttachment,    XmATTACH_WIDGET,
                  XmNtopWidget,        _eta_params_box->W(),
                  XmNtopOffset,        10,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget,     bottomSeparator(),
                  XmNbottomOffset,     10,
                  NULL);

  return topWidget();

  setTitle("Eta Menu");

}



//Temporary, and not any security here
Boolean VaEtaGui::notifyComplex(SLDelay * obj, int ident)
{
  char msg[128];
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();

  if(obj == _vel_infile_choice ) 
    {
      if(_vel_infile_choice->getFileBase()->inputValidity() == 
         FileBase::VALID_YES)
        {

          strcpy(_previous_input, 
                 _vel_infile_choice->getFileBase()->inputFilename());

          //Warn the user that all existing data will disappear
          if(ds->numVelocityFunctions())
            {
              MySLQuestPop *quest = new MySLQuestPop(this, 
                                                     "Data Replace", 
                                                     REPLACE_WARNING, this);
              return True;
            }

         
          ds->informer()->beforeChanges();
          ds->deleteAllVelocityFunctions();
          ds->informer()->afterChanges();

          ds->informer()->beforeChanges();

          int error = ds->readVelocityFile(
                  _vel_infile_choice->getFileBase()->inputFilename(), msg,
                  _vel_infilebase->readsave(), NULL);

          ds->informer()->afterChanges();

          if(error)
            {
              _eta_params_box->popError(msg);
              _have_input_file = False;
            }
          else
            {
              _have_input_file = True;
            }
        }
      else
        {
          if(ds->numVelocityFunctions())
            {
              MySLQuestPop *quest = new MySLQuestPop(this, 
                                                     "Data Replace", 
                                                     NOINPUT_WARNING, this);
              return True;
            }

          
          ds->informer()->beforeChanges();
          ds->deleteAllVelocityFunctions();
          ds->informer()->afterChanges();          
          _eta_params_box->popError("Input file not found");
          _have_input_file = False;
        }
    }

  if(obj == _vel_outfile_choice ) 
    {
      if(_vel_outfile_choice->getFileBase()->outputStatus() == 
         FileBase::OUTPUT_CREATE)
        {
        _have_output_file = True;
        }
      else if(_vel_outfile_choice->getFileBase()->outputStatus() == 
              FileBase::OUTPUT_OVERWRITE)
        {
          _eta_params_box->popError("Warning, you may be overwriting a file");
          _have_output_file = True;
        }
      else
        {
          _have_output_file = False;
        }
    }
   


  return True;
}


//Called by question popup 
void VaEtaGui::possibleFileChange()
{

   
  if(!_replace_data) 
    {
      _vel_infile_choice->getFileBase()->setInputFilename(_previous_input);
      _vel_infile_choice->updateFieldsFromFileBase();
      _replace_data = True;
      return;
    }

  char msg[128];
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();

  if(_vel_infile_choice->getFileBase()->inputValidity() == 
     FileBase::VALID_YES)
    {

      ds->informer()->beforeChanges();
      ds->deleteAllVelocityFunctions();
      ds->informer()->afterChanges();

      ds->informer()->beforeChanges();
      int error = ds->readVelocityFile(
                    _vel_infile_choice->getFileBase()->inputFilename(), msg,
                    _vel_infilebase->readsave(), NULL);
      ds->informer()->afterChanges();

      if(error)
        {
          _eta_params_box->popError(msg);
          _have_input_file = False;
        }
      else
        {
          _have_input_file = True;
        }
    }
  else
    {
      ds->informer()->beforeChanges();
      ds->deleteAllVelocityFunctions();
      ds->informer()->afterChanges();          
      _have_input_file = False;
    }
}

//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaEtaGui::ValidInput()
{
  Boolean stat;

  if (made()) 
    {

      if(!_cmp_plot->SP()->imageIsDisplayed())
        {
          _eta_params_box->popError("CMP data must be displayed.");
          return (stat = False);
        }

      if(_have_output_file == False)
        {
          _eta_params_box->popError("Output file must be specified");
          return (stat = False);
        }


      stat= SLFPopSep::ValidInput();
      if (stat)
        stat= _eta_params_box->validate();
      else
        return stat;

      if(_eta_params_box->GetFloat(WINDOW_LENGTH) < 0.0F)
        {
          _eta_params_box->SetValue(WINDOW_LENGTH, 0.02F);
          _eta_params_box->popError("Window Length was less than 0");
          return (stat = False);
        }
      if(_eta_params_box->GetFloat(WINDOW_LENGTH) > _cmp_plot->maxTime())
        {
          _eta_params_box->SetValue(WINDOW_LENGTH, _cmp_plot->maxTime() - 0.02F);
          _eta_params_box->popError(
                                  "Window Length was greater than time in file");
          return (stat = False);
        }

      //Update the output file
      VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();
      long numfunc = ds->numVelocityFunctions();
      if(numfunc > 0)
        updateFile();
    }
  else//not made yet
    { 
      stat= True;
    }

  return (stat); 
}


void VaEtaGui::applyButton()
{
  if(ValidInput())
    {
      _eta_plot->setActive(True);
      DoAction();
    }
}

void VaEtaGui::okButton()
{
  if(ValidInput())
    {
      _eta_plot->setActive(True);
      DoAction();
      unmanage();
    }
}

void VaEtaGui::cancelButton()
{
  _eta_plot->setActive(False);
  DoAction();
  unmanage();
}

//=========================================================================
//===================== Take action on Ok Apply etc========================
//=========================================================================
void VaEtaGui::DoAction()
{
  SLFPopSep::DoAction();

  setPlotParameters();
}


//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaEtaGui::setPlotParameters()
{
  VfUtilities *vfu = _eta_plot->getVfManager()->utilities();
  VfUpdate *update = vfu->update();
  long total_traces;


  setVelocityRanges();

  total_traces = (_ending_horz_velocity - _starting_horz_velocity) /
    _horz_velocity_increment + 1.5;
  
  _eta_sp->setPlotType(PlotImage::PlotARRAY);  
  _eta_sp->setNorm(PlotImage::EXTERNALNORM);
  _eta_sp->setDoAmplitude(True);
  _eta_sp->setDoPercent(False);
  _eta_sp->setExternalAmp(_eta_plot->getMaxAmp());
  _eta_sp->setMinColorAmp(_eta_plot->getMinAmp());
  _eta_sp->setMaxColorAmp(_eta_plot->getMaxAmp());
  _eta_sp->setTI(_pwidth); 
  _eta_sp->setIS(_pheight);
  _eta_sp->setGridWidth(_pwidth);
  _eta_sp->setGridHeight(_pheight);
  _eta_sp->setDrawXlines(True);
  _eta_sp->setDrawYlines(True);
  _eta_sp->setFirstLbl(1L);
  _eta_sp->setHeaders(6,7);
  _eta_sp->setMatchHeader(6);
  _eta_sp->setGridXYS(_starting_horz_velocity, _ending_horz_velocity,
                      _starting_nmo_velocity,_ending_nmo_velocity); 
  _eta_sp->setTminTmax(_starting_nmo_velocity,_ending_nmo_velocity );
  _eta_sp->setSymetricalAnnotation(_starting_horz_velocity, 
                                   _ending_horz_velocity,
                                   _starting_nmo_velocity,
                                   _ending_nmo_velocity);
  _eta_sp->setSrval(_nmo_velocity_increment);
  _eta_sp->setManualTransform(False);
  _eta_sp->setXLabelHeader(6);
  _eta_sp->setNPlt((_ending_horz_velocity - _starting_horz_velocity) /
                   _horz_velocity_increment + 1.5);
  _eta_sp->setContours(_connum);
  _eta_sp->setTminTmax(_starting_nmo_velocity,   _ending_nmo_velocity);
  _eta_sp->setMinMaxVel(_starting_horz_velocity, _ending_horz_velocity);
  _eta_sp->setMinMaxP(_eta_plot->getMinAmp(), _eta_plot->getMaxAmp());
  _eta_sp->setGradeVert((char)_grade_vert);
  _eta_sp->setGradeHorz((char)_grade_horz);
  _eta_sp->setPlotType(PlotImage::PlotARRAY);
  if(_connum > 1)
    _eta_sp->setPlotWithContours(True);
  else
    _eta_sp->setPlotWithContours(False);
  _eta_sp->setContoursOnly((int)_plot_con);
  _eta_sp->setContourIncrement((_eta_plot->getMaxAmp() - _eta_plot->getMinAmp())
                               / (_eta_sp->contours() - 1));
  _eta_sp->applySmoother(True);
  
}

//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaEtaGui::setToFileDefaults()
{

}


//=========================================================================
//===== All this file stuff is temporary until vel files support eta ======
//=========================================================================
void VaEtaGui::updateFile()
{
  char msg[128];

  _vel_outfilebase->setType(VTIN);

  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();
  int stat = ds->saveVelocityFile(
                  _vel_outfile_choice->getFileBase()->outputFilename(), msg,
                  _vel_outfilebase->readsave());
}

//=========================================================================
//====================== Manage ===========================================
//=========================================================================
void VaEtaGui::manage()
{

  SLBase::manage();

  if (_first_time) 
    {
      defaultButton(FP_DOAPPLY, False);
      setToFileDefaults();
      _vel_outfilebase->setOutputFilename((const char*)"eta.vel");
      _first_time = False;
    } 


  XtManageChild(topWidget());

}


//=========================================================================
//==================== Reset to system defaults ===========================
//=========================================================================
void VaEtaGui::reloadSystemDefaults(Boolean /*do_method*/)
{

  _eta_params_box->SetValue(PWIDTH,                 PWIDTH_DEF);
  _eta_params_box->SetValue(CONNUM,                 CONNUM_DEF);
  _eta_params_box->SetValue(WINDOW_LENGTH,          WINDOW_LENGTH_DEF);
  _eta_params_box->SetValue(NMO_VELOCITY_WIDTH,     NMO_VELOCITY_WIDTH_DEF);
  _eta_params_box->SetValue(NMO_VELOCITY_INCREMENT, NMO_VELOCITY_INCREMENT_DEF);
  _eta_params_box->SetValue(HORZ_VELOCITY_WIDTH,    HORZ_VELOCITY_WIDTH_DEF);
  _eta_params_box->SetValue(HORZ_VELOCITY_INCREMENT,HORZ_VELOCITY_INCREMENT_DEF);
  _grade_box->SetTog(GRADE_VERT,                GRADE_VERT_DEF);
  _grade_box->SetTog(GRADE_HORZ,                GRADE_HORZ_DEF);
  _grade_box->SetTog(PLOT_CON,                  PLOT_CON_DEF);


}


//=========================================================================
//====================                          ===========================
//=========================================================================
void VaEtaGui::UndoInput()
{
}



//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaEtaGui::seisPlotChanged()
{

  printf("need to test this gui's seisPlotChanged method\n");  

  _eta_params_box->SetValue(PWIDTH, _eta_sp->gridWidth());
  _eta_params_box->SetValue(CONNUM, _eta_sp->contours());
  _eta_params_box->SetValue(WINDOW_LENGTH,   0.02F);
  _eta_params_box->SetValue(NMO_VELOCITY_INCREMENT,  20.0F);
  _eta_params_box->SetValue(HORZ_VELOCITY_INCREMENT,  20.0F);
   
  _grade_box->SetTog(GRADE_VERT, (Boolean)_eta_sp->gradeVert());
  _grade_box->SetTog(GRADE_HORZ, (Boolean)_eta_sp->gradeHorz());

  //next if needs work
  if(_eta_sp->contours())
    _grade_box->SetTog(PLOT_CON,   True);
  else
    _grade_box->SetTog(PLOT_CON,   False);

  
}


//=========================================================================
//====================  Update public method    ===========================
//=========================================================================
void VaEtaGui::updateParams(Boolean update_file_limits)
{
  long possible_movies;
  long first_panel_index;
  VfUtilities *vfu = _eta_plot->getVfManager()->utilities();
  VfUpdate *update = vfu->update();


  //This method should be called when a cmp file is read in or changed

  if(update_file_limits)
    {
    }


}


//===========================================================================
//===================== Compute velocity extremes  ==========================
//===========================================================================
float VaEtaGui::startingNmoVelocity()
{
  setVelocityRanges();
  return _starting_nmo_velocity;
}

float VaEtaGui::endingNmoVelocity()
{
  setVelocityRanges();
  return _ending_nmo_velocity;
}

float VaEtaGui::startingHorzVelocity()
{
  setVelocityRanges();
  return _starting_horz_velocity;
}

float VaEtaGui::endingHorzVelocity()
{
  setVelocityRanges();
  return _ending_horz_velocity;
}

void VaEtaGui::setVelocityRanges()
{

  _starting_nmo_velocity = _eta_plot->getVelocityPick() - 
    (_nmo_velocity_width / 2.0F);

  _starting_nmo_velocity = max(_starting_nmo_velocity, 1.0F);

  _ending_nmo_velocity = _eta_plot->getVelocityPick() +
    (_nmo_velocity_width / 2.0F);

  _ending_nmo_velocity = max(_starting_nmo_velocity + _nmo_velocity_increment,
                             _ending_nmo_velocity);
  
  _starting_horz_velocity = _eta_plot->getVelocityPick() - 
    (_horz_velocity_width / 2.0F);

  _starting_horz_velocity = max(_starting_horz_velocity, 1.0F);

  _ending_horz_velocity = _eta_plot->getVelocityPick() +
    (_horz_velocity_width / 2.0F);

  _ending_horz_velocity =max(_starting_horz_velocity + _horz_velocity_increment,
                             _ending_horz_velocity);

}
