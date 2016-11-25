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
//------------------- CELLedit.hh -------------------------//

//            header file for the CellEdit Table
//                 derived from the SLSmartForm class

#ifndef _TIME_DEPTH_POP_
#define _TIME_DEPTH_POP_

#include <stdio.h>
#include "sl/sl_smart_form.hh"
#include "sl/sl_dialog.hh"

class SLpPush;
class ModelDesc;
class CvmApp;

class TimeDepth: public SLSmartForm 
{

//----------------------- data ----------------------------//

private:

  CvmApp      *_cvmapp;
  ModelDesc   *_mod;
  SLSmartForm *_form;

//----------- constructors and destructor -----------------//

public:

  TimeDepth(SLDelay *slparent, char *name,CvmApp *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now);
  TimeDepth (Widget parent, char *name,CvmApp *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now);
  virtual ~TimeDepth(void);
  friend class TimeDepthPop;


//-------------------- traps --------------------------------//

  static void compTrap(void *data,long );


protected:

  void setup();


    // make widget tree
  Widget make(Widget p =NULL);

  virtual WidgetClass topClass() {return xmFormWidgetClass;}
  virtual Boolean isDialog()     { return False; }
  virtual Boolean isTopLevel()   { return False; }

//------------------ end functions --------------------//

};




class TimeDepthPop : public SLDialog
{
//---------------- beginning of class-------------------//

public:

  TimeDepthPop (SLDelay *contain, char *name, HelpCtx Hctx,
   CvmApp *cvm);
  TimeDepthPop (Widget parent, char *name, HelpCtx Hctx ,
   CvmApp *cvm);
  virtual ~TimeDepthPop(void);
  Widget  Form();

private:
  TimeDepth *_timdep;
  SLSmartForm *_slform;

protected:
 Boolean removeNotify();

 enum PBTYPE {OK,CAN,REMOVE};
//-------------------- end of class ----------------------//
} ;

#endif

