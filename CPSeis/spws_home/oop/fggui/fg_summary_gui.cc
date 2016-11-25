
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
//---------------------- fg_summary_gui.cc ------------------------//
//---------------------- fg_summary_gui.cc ------------------------//
//---------------------- fg_summary_gui.cc ------------------------//

//         implementation file for the FgSummaryGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui


#include "fggui/fg_summary_gui.hh"
#include "geom/jd_file.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_label.hh"
#include "sl/sl_sep.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//------------------ update functions -------------------------//
//------------------ update functions -------------------------//
//------------------ update functions -------------------------//


static char *chn_upfun(void *data)
{
  static char   *buf1 = "horizontal";
  static char   *buf2 = "slope";
  static char   *buf3 = "none";
  static char   *buf4 = " ";
  FgSummaryGui  *gui  = (FgSummaryGui*)data;
  FieldGeometry *fg   = gui->getFg();
  JdFile        *jdi  = gui->getJdInput();
  JdFile        *jdo  = gui->getJdOutput();
  int chaining;
  if     (fg)  chaining = fg ->getChaining();
  else if(jdi) chaining = jdi->getInputChaining();
  else         chaining = jdo->getOutputChaining();
  switch(chaining)
      {
      case HORIZONTAL_CHAINING: return buf1;
      case SLOPE_CHAINING     : return buf2;
      case NO_CHAINING        : return buf3;
      default                 : break;
      }
  return buf4;
}
  


#define UPFUN(ld_upfun, totNumFlags, numInputCards, numOutputCards)   \
static long ld_upfun(void *data)                 \
{                                                \
  FgSummaryGui  *gui  = (FgSummaryGui*)data;     \
  FieldGeometry *fg   = gui->getFg();            \
  JdFile        *jdi  = gui->getJdInput();       \
  JdFile        *jdo  = gui->getJdOutput();      \
  if     (fg)  return fg ->totNumFlags();        \
  else if(jdi) return jdi->numInputCards();      \
               return jdo->numOutputCards();     \
}


UPFUN( ld_upfun, totNumFlags, numInputLdCards , numOutputLdCards)
UPFUN( rp_upfun, numRpCards , numInputRpCards , numOutputRpCards)
UPFUN( pp_upfun, numPpCards , numInputPpCards , numOutputPpCards)
UPFUN(zt1_upfun, numZt1Cards, numInputZt1Cards, numOutputZt1Cards)
UPFUN(zt2_upfun, numZt2Cards, numInputZt2Cards, numOutputZt2Cards)
UPFUN(zt3_upfun, numZt3Cards, numInputZt3Cards, numOutputZt3Cards)
UPFUN(zt4_upfun, numZt4Cards, numInputZt4Cards, numOutputZt4Cards)



static char *grd_upfun(void *data)
{
  static char   *buf1 = "yes";
  static char   *buf2 = "no";
  static char   *buf3 = "???";
  static char   *buf4 = " ";
  FgSummaryGui  *gui  = (FgSummaryGui*)data;
  FieldGeometry *fg   = gui->getFg();
  if(fg) return buf1;

  JdFile        *jdi  = gui->getJdInput();
  JdFile        *jdo  = gui->getJdOutput();

  int sense = TRUE;
  if(jdi) sense = (jdi->inputStatus () == FileBase::INPUT_VALID);
  if(jdo) sense = (jdo->outputStatus() == FileBase::OUTPUT_OVERWRITE);
  gui->setSensitivity(sense);   // sensitivity of entire object.

  int gridstat = 0;
  if(jdi) gridstat = jdi->inputGridTransformStatus();
  if(jdo) gridstat = jdo->outputGridTransformStatus();
  if(gridstat > 0)       return buf1;
  if(gridstat < 0)       return buf2;
  if(!sense)             return buf4;
  return buf3;
}



//--------------------- sense update functions ------------------//
//--------------------- sense update functions ------------------//
//--------------------- sense update functions ------------------//


static long grd_sense_upfun(void *data)
{                                       
  FgSummaryGui  *gui  = (FgSummaryGui*)data;
  FieldGeometry *fg   = gui->getFg();   
  if(fg) return TRUE;
  JdFile        *jdi  = gui->getJdInput();  
  JdFile        *jdo  = gui->getJdOutput();  
  int gridstat = 0;
  if(jdi) gridstat = jdi->inputGridTransformStatus();
  if(jdo) gridstat = jdo->outputGridTransformStatus();
  return (gridstat >= 0);     
}


#define SENSEFUN(ld_sense_upfun, totNumFlags, numInputCards, numOutputCards) \
static long ld_sense_upfun(void *data)              \
{                                                   \
  FgSummaryGui  *gui  = (FgSummaryGui*)data;        \
  FieldGeometry *fg   = gui->getFg();               \
  JdFile        *jdi  = gui->getJdInput();          \
  JdFile        *jdo  = gui->getJdOutput();         \
  if     (fg)  return (fg ->totNumFlags   () > 0);  \
  else if(jdi) return (jdi->numInputCards () > 0);  \
               return (jdo->numOutputCards() > 0);  \
}


SENSEFUN( ld_sense_upfun, totNumFlags, numInputLdCards , numOutputLdCards)
SENSEFUN( rp_sense_upfun, numRpCards , numInputRpCards , numOutputRpCards)
SENSEFUN( pp_sense_upfun, numPpCards , numInputPpCards , numOutputPpCards)
SENSEFUN(zt1_sense_upfun, numZt1Cards, numInputZt1Cards, numOutputZt1Cards)
SENSEFUN(zt2_sense_upfun, numZt2Cards, numInputZt2Cards, numOutputZt2Cards)
SENSEFUN(zt3_sense_upfun, numZt3Cards, numInputZt3Cards, numOutputZt3Cards)
SENSEFUN(zt4_sense_upfun, numZt4Cards, numInputZt4Cards, numOutputZt4Cards)



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


FgSummaryGui::FgSummaryGui(SLDelay *slparent, char *name, char *label,
                         FieldGeometry *fg, JdFile *jdi, JdFile *jdo)
       : SLSmartForm(slparent, name, NULL, TRUE),
               _fg           (fg),
               _jdi          (jdi),
               _jdo          (jdo)
{
  assert( ( _fg && !_jdi && !_jdo) ||
          (!_fg &&  _jdi && !_jdo) ||
          (!_fg && !_jdi &&  _jdo) );

  SLpLabel *title = new SLpLabel(this, "title", 0, label);
  SLSep    *sep1 = new SLSep   (this, "sep");
  SLSep    *sep2 = new SLSep   (this, "sep");
  SLSep    *sep3 = new SLSep   (this, "sep");
  SLSep    *sep4 = new SLSep   (this, "sep");
  SL2Text  *chn = new SL2Text (this, "chn",0, "chaining:" , SLpText::_CHAR, 10);
  SL2Text  *ld  = new SL2Text (this, "ld" ,0, "LD cards:" , SLpText::_LONG, 7);
  SL2Text  *rp  = new SL2Text (this, "rp" ,0, "RP cards:" , SLpText::_LONG, 7);
  SL2Text  *pp  = new SL2Text (this, "pp" ,0, "PP cards:" , SLpText::_LONG, 7);
  SL2Text  *zt1 = new SL2Text (this, "zt1",0, "ZT1 cards:", SLpText::_LONG, 7);
  SL2Text  *zt2 = new SL2Text (this, "zt2",0, "ZT2 cards:", SLpText::_LONG, 7);
  SL2Text  *zt3 = new SL2Text (this, "zt3",0, "ZT3 cards:", SLpText::_LONG, 7);
  SL2Text  *zt4 = new SL2Text (this, "zt4",0, "ZT4 cards:", SLpText::_LONG, 7);
  SL2Text  *grd = new SL2Text (this, "grd",0, "grid transform:",
                                                            SLpText::_CHAR, 3);

  chn->showLabelAppearance();
  ld ->showLabelAppearance();
  rp ->showLabelAppearance();
  pp ->showLabelAppearance();
  zt1->showLabelAppearance();
  zt2->showLabelAppearance();
  zt3->showLabelAppearance();
  zt4->showLabelAppearance();
  grd->showLabelAppearance();

  chn    ->setupCvarFun     (chn_upfun, this);
  ld     ->setupIvarFun     ( ld_upfun, this);
  rp     ->setupIvarFun     ( rp_upfun, this);
  pp     ->setupIvarFun     ( pp_upfun, this);
  zt1    ->setupIvarFun     (zt1_upfun, this);
  zt2    ->setupIvarFun     (zt2_upfun, this);
  zt3    ->setupIvarFun     (zt3_upfun, this);
  zt4    ->setupIvarFun     (zt4_upfun, this);
  grd    ->setupCvarFun     (grd_upfun, this);

  ld     ->setupSenseFun    ( ld_sense_upfun, this);
  rp     ->setupSenseFun    ( rp_sense_upfun, this);
  pp     ->setupSenseFun    ( pp_sense_upfun, this);
  zt1    ->setupSenseFun    (zt1_sense_upfun, this);
  zt2    ->setupSenseFun    (zt2_sense_upfun, this);
  zt3    ->setupSenseFun    (zt3_sense_upfun, this);
  zt4    ->setupSenseFun    (zt4_sense_upfun, this);
  grd    ->setupSenseFun    (grd_sense_upfun, this);

//                  left   right   top      bottom

  attach(title    , this , this  , this    , NULL, 0, 0, 8);
  attach(sep1     , this , this  , title   , NULL, 0, 0, 8);
  attach(chn      , this , this  , sep1    , NULL, 0, 0, 4);
  attach(sep2     , this , this  , chn     , NULL);
  attach(ld       , this , this  , sep2    , NULL, 0, 0, 4);
  attach(rp       , this , this  , ld      , NULL, 0, 0, 4);
  attach(pp       , this , this  , rp      , NULL, 0, 0, 4);
  attach(sep3     , this , this  , pp      , NULL);
  attach(zt1      , this , this  , sep3    , NULL, 0, 0, 4);
  attach(zt2      , this , this  , zt1     , NULL, 0, 0, 4);
  attach(zt3      , this , this  , zt2     , NULL, 0, 0, 4);
  attach(zt4      , this , this  , zt3     , NULL, 0, 0, 4);
  attach(sep4     , this , this  , zt4     , NULL);
  attach(grd      , this , this  , sep4    , this, 0, 0, 4);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


FgSummaryGui::~FgSummaryGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

