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
//------------------- RayEdit.hh -------------------------//

//            header file for the RayEdit Table
//                 derived from the SLSmartForm class

#ifndef _RAY_EDIT_HH
#define _RAY_EDIT_HH

#include <stdio.h>
#include "transform.h"
#include "oprim/base_data.hh"
#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"

class Vector;
class VectorLinkedList;
class gridData;
class SLpPush;
class CvmApp;
class PickPoint;
class RadioList;

class RayEdit: public SLSmartForm 
{

//----------------------- data ----------------------------//

private:

  int    _mode;    // Ray shooting mode
  float  _xs;
  float  _ys;
  float  _zs;
  float  _angini;
  float  _delang;
  long   _numang;

  float  _taustep; // ray step
  float  _xmin;    // ray trace box
  float  _ymin;
  float  _zmin;
  float  _xmax;
  float  _ymax;
  float  _zmax;
  int    _n1;      // grid description
  int    _n2;
  int    _n3;
  float  _o1;
  float  _o2;
  float  _o3;
  float  _d1;
  float  _d2;
  float  _d3;
  float  *_slow;

  CvmApp *_cvmapp;
  SeisVectLinkedList *_rays;
  SLSmartForm *_form;
  RadioList *_radio1;
  PickPoint *_pp;

//----------- constructors and destructor -----------------//

public:

  enum RayMode { FanMode, HorizonMode };

  RayEdit(SLDelay *slparent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now,
           long ident=BaseData::defaultId);
  RayEdit (Widget parent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now,
           long ident=BaseData::defaultId);
  virtual ~RayEdit(void);
  friend class RayEditPop;


  Vector *cell_n_vector(VectorLinkedList *vll, int n);

//-------------------- traps --------------------------------//

  static void compTrap(void *data,long );

protected:
  long   _ident;

  void setup();
  int physicalGrid(gridData *gdpntr);
  int compute(gridData *gdpntr);
  Vector *closest(float x1, float x2, float x3);
  int    shootHorizon(gridData *gdpntr);
  static void radioTrap(void *data, long ident,long, long);

    // make widget tree
  Widget make(Widget p =NULL);

  virtual WidgetClass topClass() {return xmFormWidgetClass;}
  virtual Boolean isDialog()     { return False; }
  virtual Boolean isTopLevel()   { return False; }

//------------------ end functions --------------------//

} ;




class RayEditPop : public SLDialog
{
//---------------- beginning of class-------------------//

public:

  RayEditPop (SLDelay *contain, char *name, HelpCtx Hctx,
   void *cvm);
  RayEditPop (Widget parent, char *name, HelpCtx Hctx ,
   void *cvm);
  virtual ~RayEditPop(void);
  Widget  Form();

private:
  RayEdit *_redit;
  SLSmartForm *_slform;

protected:
 Boolean removeNotify();

 enum PBTYPE {OK,CAN,REMOVE};
//-------------------- end of class ----------------------//
} ;

#endif

