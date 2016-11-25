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
//************************ COPYRIGHT NOTICE ******************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        ***
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         ***
//************************ COPYRIGHT NOTICE ******************************



//************************************************************************
//***             Class to display mute reference file overlays.       ***
//***             Author:Michael L. Sherrill 01/2002                   ***
//************************************************************************

#include "pick/tp_mute_reference.hh"
#include "cprim.h"
#include "pick/tp_reference_base.hh"
#include "pick/tp_popup_base.hh"
#include "pick/tp_resources.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_error_pop.hh"
#include "sp/seis_plot.hh"
#include "plot/pick_watch.hh"
#include <iostream.h>
#include <string.h>
#include <assert.h>
#include "inquire.h"
#include "mutefile.h"


//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpMuteReference::TpMuteReference(SLDelay *slparent,
                     char       *name,
                     int         io,
                     FileBase   *file,
                     const char *label,
                     HelpCtx     hctx,
                     TpPopupBase *pop)
  : TpReferenceBase(slparent, name, io, file, label, hctx)
{
  
  _pop     = pop;
  _ydisp1  = 0.0F;
  _ydisp2  = 0.0F;
  _zdisp1  = 0.0F;
  _zdisp2  = 0.0F;
  _ss      = mutefile_create();
  _allow_header_input = True;
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpMuteReference::~TpMuteReference()
{
  mutefile_destroy(_ss);
}



//------------------- get values from mute object ------------//
//------------------- get values from mute object ------------//
//------------------- get values from mute object ------------//

long  TpMuteReference::getSwitch() const { return mutefile_get_yz_switch(_ss); }
long  TpMuteReference::getInterp() const { return mutefile_get_interp   (_ss); }

long  TpMuteReference::getNhx ()  const  { return mutefile_get_nhx (_ss); }
long  TpMuteReference::getNhy ()  const  { return mutefile_get_nhy (_ss); }
long  TpMuteReference::getNhz ()  const  { return mutefile_get_nhz (_ss); }

float TpMuteReference::getXmin()  const  { return mutefile_get_xmin(_ss); }
float TpMuteReference::getYmin()  const  { return mutefile_get_ymin(_ss); }
float TpMuteReference::getZmin()  const  { return mutefile_get_zmin(_ss); }
float TpMuteReference::getXmax()  const  { return mutefile_get_xmax(_ss); }
float TpMuteReference::getYmax()  const  { return mutefile_get_ymax(_ss); }
float TpMuteReference::getZmax()  const  { return mutefile_get_zmax(_ss); }

float TpMuteReference::getYsel()  const  { return mutefile_get_ysel(_ss); }
float TpMuteReference::getZsel()  const  { return mutefile_get_zsel(_ss); }




//------------ get latest y or z updated -----------//
//------------ get latest y or z updated -----------//
//------------ get latest y or z updated -----------//


float TpMuteReference::getLatestYbinUpdated()  const
{
  return mutefile_get_ylatest(_ss);
}


float TpMuteReference::getLatestZbinUpdated()  const
{
  return mutefile_get_zlatest(_ss);
}



void TpMuteReference::newFilenameEntered()
{
char info[256];
long error = 1;
SLErrorPop *errpop;

  _is_mute_file = mutefile_check_validity(_ss, 
                                         (char *)getFileBase()->inputFilename(),
                                         info);
  //The following will populate the _ss struct xyzdim struct
  if(_is_mute_file)
     error = mutefile_read_file(_ss, 
                                (char *)getFileBase()->inputFilename(),
                                info);

  if(error && strlen(getFileBase()->inputFilename()))
    {
    _is_mute_file = 0;
    errpop = new SLErrorPop(topWidget(),"Error","Not a valid reference file.");
    }

  _pop->setShowVector(TP_REF, (Boolean) _is_mute_file);

  _pop->update();
}



void TpMuteReference::doReadCurrentPicks(float *picks,
                         const float *head, long nwords, long n)
{
  mutefile_get_picks(_ss, (float*)head, nwords, n, picks);
  if(n == 0)
      {
      _ydisp1 = 0.0;
      _ydisp2 = 0.0;
      _zdisp1 = 0.0;
      _zdisp2 = 0.0;
      }
  else
      {
      SeisPlot *sp = _pop->getSeisPlot();       // new 9/2/97.
      if(!sp->isPlotDisplayed())                // new 2/18/98
          {                                     // new 2/18/98
          _ydisp1 = 0.0;                        // new 2/18/98
          _ydisp2 = 0.0;                        // new 2/18/98
          _zdisp1 = 0.0;                        // new 2/18/98
          _zdisp2 = 0.0;                        // new 2/18/98
          return;                               // new 2/18/98
          }                                     // new 2/18/98
      const float *head = sp->firstMemoryHeaderData();
      long       nwords = sp->numHeaders();
      _ydisp1 = mutefile_get_y_from_header(_ss, &head[0]             );
      _ydisp2 = mutefile_get_y_from_header(_ss, &head[(n-1) * nwords]);
      _zdisp1 = mutefile_get_z_from_header(_ss, &head[0]             );
      _zdisp2 = mutefile_get_z_from_header(_ss, &head[(n-1) * nwords]);
      }
}



//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//

void TpMuteReference::doReadPreviousPicks(float *picks,
                         const float *head, long nwords, long n)
{
  mutefile_get_prev_picks(_ss, (float*)head, nwords, n, picks);
}



//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//

void TpMuteReference::doReadNextPicks(float *picks,
                         const float *head, long nwords, long n)
{
  mutefile_get_next_picks(_ss, (float*)head, nwords, n, picks);
}



void TpMuteReference::setSwitch(long yz_switch)
{
  if(yz_switch == mutefile_get_yz_switch(_ss)) return;
  mutefile_set_yz_switch(_ss, (int)yz_switch);
  _pop->updatePrevNextSelVectors();
  char msg[100];
  strcpy(msg, "overlay switch changed to\n");
  if(yz_switch)
       strcat(msg, "overlay previous/next crossline bin");
  else strcat(msg, "overlay previous/next inline bin");
  _pop->showMessage(msg);
}



void TpMuteReference::setInterp(long interp)
{
  if(interp == mutefile_get_interp(_ss)) return;
  mutefile_set_interp(_ss, (int)interp);
  _pop->updateAllVectors();
  char msg[100];
  strcpy(msg, "interpolation choice changed to\n");
  if(interp)
       strcat(msg, "interpolate/extrapolate in all directions");
  else strcat(msg, "interpolate in offset direction only");
  _pop->showMessage(msg);
}



void TpMuteReference::setNhx(long nhx)
{
  if(!_allow_header_input) return;
  mutefile_set_nhx(_ss, nhx);
}



void TpMuteReference::setNhy(long nhy)
{
  if(!_allow_header_input) return;
  mutefile_set_nhy(_ss, nhy);
}



void TpMuteReference::setNhz(long nhz)
{
  if(!_allow_header_input) return;
  mutefile_set_nhz(_ss, nhz);
}



void TpMuteReference::setYsel(float ysel, Boolean directional)
{
  Boolean changed = mutefile_set_ysel(_ss, ysel, directional);
  if(changed)
       {
       _pop->showMessage("new selected inline coordinate\nchosen");
       _pop->updateSelVector();
       }
}



void TpMuteReference::setZsel(float zsel, Boolean directional)
{
  Boolean changed = mutefile_set_zsel(_ss, zsel, directional);
  if(changed)
       {
       _pop->showMessage("new selected crossline coordinate\nchosen");
       _pop->updateSelVector();
       }
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
