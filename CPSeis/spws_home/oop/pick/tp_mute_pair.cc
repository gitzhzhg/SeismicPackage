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

//----------------------- tp_mute_pair.cc -----------------------//
//----------------------- tp_mute_pair.cc -----------------------//
//----------------------- tp_mute_pair.cc -----------------------//

//       implementation file for the TpMutePair class
//               derived from the TpPairBase class
//                       subdirectory pick

       // general CPS static file interface

#include "pick/tp_mute_pair.hh"
#include "cprim.h"
#include "pick/tp_popup_base.hh"
#include "sl/shell_watch.hh"
#include "sl/slp_file.hh"
#include "plot/pick_watch.hh"
#include "sp/seis_plot.hh"
#include <iostream.h>
#include <string.h>
#include <assert.h>
#include "inquire.h"
#include "mutefile.h"


//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpMutePair::TpMutePair(SLDelay *slparent,
                          TpPopupBase *pop, SeisPlot* /*dummy*/,
                          const char * const filetype,
                          const char * const extension,
                          const Boolean      required1,
                          const Boolean      required2)
       : TpPairBase(slparent, "mute_pair",
                         filetype, extension, required1, required2),
                  _ss                (NULL),
                  _pop               (pop),
/*
                  _sp                (sp),
*/

                  _ydisp1            (0.0),
                  _ydisp2            (0.0),
                  _zdisp1            (0.0),
                  _zdisp2            (0.0),

                  _allow_header_input(TRUE)
{
  assert(_pop);
/*
  assert(_pop && _sp);
*/
  _ss = mutefile_create();
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpMutePair::~TpMutePair()
{
  mutefile_destroy(_ss);
}



//------------------- get values from mute object ------------//
//------------------- get values from mute object ------------//
//------------------- get values from mute object ------------//

long  TpMutePair::getSwitch() const { return mutefile_get_yz_switch(_ss); }
long  TpMutePair::getInterp() const { return mutefile_get_interp   (_ss); }

long  TpMutePair::getNhx ()  const  { return mutefile_get_nhx (_ss); }
long  TpMutePair::getNhy ()  const  { return mutefile_get_nhy (_ss); }
long  TpMutePair::getNhz ()  const  { return mutefile_get_nhz (_ss); }

float TpMutePair::getXmin()  const  { return mutefile_get_xmin(_ss); }
float TpMutePair::getYmin()  const  { return mutefile_get_ymin(_ss); }
float TpMutePair::getZmin()  const  { return mutefile_get_zmin(_ss); }
float TpMutePair::getXmax()  const  { return mutefile_get_xmax(_ss); }
float TpMutePair::getYmax()  const  { return mutefile_get_ymax(_ss); }
float TpMutePair::getZmax()  const  { return mutefile_get_zmax(_ss); }

float TpMutePair::getYsel()  const  { return mutefile_get_ysel(_ss); }
float TpMutePair::getZsel()  const  { return mutefile_get_zsel(_ss); }



//-------------- put values into mute object --------------//
//-------------- put values into mute object --------------//
//-------------- put values into mute object --------------//





//------------ get latest y or z updated -----------//
//------------ get latest y or z updated -----------//
//------------ get latest y or z updated -----------//


float TpMutePair::getLatestYbinUpdated()  const
{
  return mutefile_get_ylatest(_ss);
}


float TpMutePair::getLatestZbinUpdated()  const
{
  return mutefile_get_zlatest(_ss);
}



//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//

            // overrides SLFilePairPlus
            // called from private SLFilePairPlus::validateTrap
            //       which is called from SLFilePair
            //       which is called from make_filepair

void TpMutePair::doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets)
{
  // cout << "am in TpMutePair::doValidate" << endl;
  mutefile_check_validities(_ss, (char*)filename1, (char*)filename2,
                      valid1, valid2, info1, info2, same_datasets);
  if(*valid1 != INQUIRE_VALID_YES)
       {
       _allow_header_input = SLpFile::isAnEmptyFilename (filename1);
       }
  else
       {
       _allow_header_input = FALSE;
       }
  _pop->update();
}



//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::openFiles()

int TpMutePair::doOpen (long status,
                              const char *filename1,
                              const char *filename2,
                              Boolean /*required1*/,
                              Boolean /*required2*/,
                              FppWorkingMessage *working_message_trap,
                              void              *working_message_data,
                              char *msg)
{
  // cout << "am in TpMutePair::doOpen" << endl;

  int error = mutefile_prepare(_ss, (char*)filename1, (char*)filename2,
          (MsgFun*)working_message_trap, working_message_data,
                   status, msg);
  if(error)
       {
       _allow_header_input = (status == FILE_CREATE);
       }
  else
       {
       _allow_header_input = FALSE;
       }
  _pop->update();
  return error;
}



//---------------------- do close ----------------------------//
//---------------------- do close ----------------------------//
//---------------------- do close ----------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::closeFiles()

void TpMutePair::doClose()
{
  // cout << "am in TpMutePair::doClose" << endl;
  ////// Nothing needs to be done here, since all opens and closes
  ////// are done within the doUpdateFile function.
}



//-------------------- do read current picks -------------------//
//-------------------- do read current picks -------------------//
//-------------------- do read current picks -------------------//

void TpMutePair::doReadCurrentPicks(float *picks,
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
/*
      const float *head = _sp->firstMemoryHeaderData();
      long       nwords = _sp->numHeaders();
*/
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

void TpMutePair::doReadPreviousPicks(float *picks,
                         const float *head, long nwords, long n)
{
  mutefile_get_prev_picks(_ss, (float*)head, nwords, n, picks);
}



//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//

void TpMutePair::doReadNextPicks(float *picks,
                         const float *head, long nwords, long n)
{
  mutefile_get_next_picks(_ss, (float*)head, nwords, n, picks);
}



//-------------------- do read selected picks -------------------//
//-------------------- do read selected picks -------------------//
//-------------------- do read selected picks -------------------//

void TpMutePair::doReadSelectedPicks(float *picks,
                         const float *head, long nwords, long n)
{
  mutefile_get_sel_picks(_ss, (float*)head, nwords, n, picks);
}



//-------------------- do save current picks -------------------//
//-------------------- do save current picks -------------------//
//-------------------- do save current picks -------------------//

/*
     // also sets picks to zero if action is not one of these:
     //      AUTOMATIC  MANUAL  ZERO  SNAP.
     // (this means that action == SHIFT_ZERO, which is a constant
     // which is not defined in this file.)
*/
     // also sets picks to zero if action is ZERO.

void TpMutePair::doSaveCurrentPicks(float *picks,
              const float *head, long nwords, long n, long action)
{
/*
  if(action != AUTOMATIC && action != MANUAL &&
     action != ZERO      && action != SNAP)
*/
  if(action == ZERO)
        {
        float missing = MISSING;
        for(int i = 0; i < n; i++)
            {
            if(picks[i] != missing) picks[i] = 0.0;
            }
        }
  mutefile_put_picks(_ss, (float*)head, nwords, n, picks);
}



//-------------------- do update file --------------------------//
//-------------------- do update file --------------------------//
//-------------------- do update file --------------------------//


void TpMutePair::doUpdateFile()
{
//////  start_watch_cursor();
  ShellWatch watch1;
  PickWatch  watch2;
  _pop->showMessage("saving picks\nto file...");
  mutefile_save_file(_ss, (char*)workingFilename(), NULL);
//////  stop_watch_cursor();
}



//-------------------- set variables -----------------------//
//-------------------- set variables -----------------------//
//-------------------- set variables -----------------------//


void TpMutePair::setSwitch(long yz_switch)
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



void TpMutePair::setInterp(long interp)
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



void TpMutePair::setNhx(long nhx)
{
  if(!_allow_header_input) return;
  mutefile_set_nhx(_ss, nhx);
}



void TpMutePair::setNhy(long nhy)
{
  if(!_allow_header_input) return;
  mutefile_set_nhy(_ss, nhy);
}



void TpMutePair::setNhz(long nhz)
{
  if(!_allow_header_input) return;
  mutefile_set_nhz(_ss, nhz);
}



void TpMutePair::setYsel(float ysel, Boolean directional)
{
  Boolean changed = mutefile_set_ysel(_ss, ysel, directional);
  if(changed)
       {
       _pop->showMessage("new selected inline coordinate\nchosen");
       _pop->updateSelVector();
       }
}



void TpMutePair::setZsel(float zsel, Boolean directional)
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
