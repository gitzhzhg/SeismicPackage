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
//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************



//************************************************************************
//***             Menu to apply a constant to a mute file              ***
//***             Author:Michael L. Sherrill 01/2002                   ***
//************************************************************************

#include "pick/tp_mute_shift_pop.hh"
#include "oprim/mutefile_util.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_error_pop.hh"
#include "inquire.h"
#include "mutefile.h"

static String  defres[]= {
    "*popup.title:                     Mute File Shift",
    "*shiftvalL.labelString:           Shift Value Seconds:",
    "*shiftval.value:                  .001",
    NULL};


static const char * const FILETYPE      = "CPS 3D mute file";
static const char * const EXTENSION     = "mute";
static const Boolean      REQUIRED1     = TRUE;
static const Boolean      REQUIRED2     = TRUE;



//==========================================================================
//=============== File pair used by the shift menu class ===================
//==========================================================================
TpMuteShiftPair::TpMuteShiftPair(SLDelay *parent,
                                 char *name,
                                 HelpCtx hctx)
              :  SLFilePairPlus (parent, name, hctx, True, FILETYPE,
                                 EXTENSION, REQUIRED1, REQUIRED2)
{
  _input_valid = _output_valid = 0;
  _ss = mutefile_create();
}
             
                    
TpMuteShiftPair::~TpMuteShiftPair()
{
   mutefile_destroy(_ss);
}

void TpMuteShiftPair::doValidate(const char *filename1,
                                 const char *filename2,
                                 long *valid1, long *valid2,
                                 char *info1, char *info2,
                                 long *same_datasets)
{
  mutefile_check_validities(_ss, (char*)filename1, (char*)filename2,
                            valid1, valid2, info1, info2, same_datasets);
  _input_valid  = *valid1;
  _output_valid = *valid2;

}



//==========================================================================
//====================  Mute shift menu class  =============================
//==========================================================================
TpMuteShiftPop::TpMuteShiftPop( Widget              p,
                                char                *name,
                                HelpCtx             hctx)
                : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                  _shift_value(.001F)

{
static SLText shift_val_box[]  = 
  {
    { "shiftval",  NULL,  NULL,  SLType_float, 0 },
  };
shift_val_box[0].target=&_shift_value; 


  setDefaultResources( p, name, defres);

  _mutefile_pair = new TpMuteShiftPair (this, "mute_files", hctx);

  _shift_box = new SLTextBox( this,"shift_box",getHelpCtx(),
                               shift_val_box,XtNumber(shift_val_box), True,
                               1, True, False);

  _mutefile_util = new MutefileUtil();

  make(p);
}


TpMuteShiftPop::~TpMuteShiftPop()
{
  delete _mutefile_util;
}


Widget TpMuteShiftPop::make(Widget p)
{

   if ( made() ) return topWidget();

   Widget parent = p ? p : wParent();
 
   SLFPopSep::make(p);

   XtVaSetValues(_mutefile_pair->W(),
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 NULL);

   XtVaSetValues( _shift_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _mutefile_pair->W(),
                                 XmNtopOffset,        20,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       50, 
                                 XmNbottomAttachment, XmATTACH_WIDGET,
                                 XmNbottomWidget,     bottomSeparator(),
                                 XmNbottomOffset,     20,
                                 NULL);

   defaultButton(FP_OK, False);

   return topWidget();

}


void TpMuteShiftPop::okButton()
{

  if(validate())
    {
    DoAction();
    unmanage();
    }

}

void TpMuteShiftPop::applyButton()
{

  if( validate() ) 
    DoAction();

}


void TpMuteShiftPop::DoAction()
{
  SLFPopSep::DoAction();
}


int TpMuteShiftPop::validate()
{
int ok = 1;
long valid1, valid2, same_datasets;
char info1[256], info2[256];
int error = 0;
SLErrorPop *errorpop;

  if(_mutefile_pair->inputIsValid()  != INQUIRE_VALID_YES)
    {
    errorpop = new SLErrorPop(topWidget(), "Error", 
                                "Input file is not valid");
    return (ok = 0);
    }
  else
    {
    error = _mutefile_util->addConstant(_mutefile_pair->inputFilename(),
                                        _mutefile_pair->outputFilename(),
                                        _shift_value, info1);
    if(error)
      {
      errorpop = new SLErrorPop(topWidget(), "Error", 
                                "Error applying shift to the mute file.");
      return (ok = 0);
      }
    }

  return ok;

}
