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

#ifndef _CELL_EDIT_HH
#define _CELL_EDIT_HH

#include <stdio.h>
#include "sl/sl_smart_form.hh"
#include "sl/sl_databox.hh"
#include "sl/sl_dialog.hh"
#include "oprim/base_data.hh"
#include "oprim/data_user.hh"

class Vector;
class VectorLinkedList;
class VectorListData;
class SLpPush;
class CvmApp;

class CellEdit: public SLSmartForm , public DataUser
{

//----------------------- data ----------------------------//

private:
  long   _n;
  long   _nmax;
  long   _numcell;
  long   _nvar, _isw;
  long   _mid[100];
  float  _xc[100];
  float  _yc[100];
  float  _sc[100];
  long   _in[100];
  long   _has[100];

  long   _iradio;
  long   _old_id;
  CvmApp *_cvmapp;
  VectorListData *_vldata;
  Vector *_vlist[8]; // highlighted cells
  Vector *_hot_label; // active cell pointer

  SLSmartForm *_form;
  SLDatabox   *_databox;
  SLDatabox   *_databox2;

//----------- constructors and destructor -----------------//

public:

  CellEdit(SLDelay *slparent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now,
           long ident=BaseData::defaultId);
  CellEdit (Widget parent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now,
           long ident=BaseData::defaultId);
  virtual ~CellEdit(void);
  friend class CellEditPop;

  int  CE_Set();
  int  CE_Get();
  virtual void modDone(BaseData *, long);

  Vector *cell_n_vector(VectorLinkedList *vll, int n);
  int cell_inside(Vector *vector, float *x, float *, float *z);
  Vector *which_cell(VectorLinkedList *vll,float *x, float *y, float *z);
  int     set_in();
  int     has(Vector *v);
  void    set_hot_label(Vector *v);
  int     DelEntry(long row);

//-------------------- traps --------------------------------//

  static void radio_trap(void *box, long *ident, long *index,
                    char *text, long *nread, char *endkey);
  static void htrap(void *box, long *ident, long *index,
                    char *text, long *nread, char *endkey);
  static void compTrap(void *data,long );

//---------- overriding method makeHelper ------------------//

protected:
  long   _ident;

  void setup();
  static  void makeTrap(void *);
  static  void makeTrap2(void *);
  virtual void makeHelper();
  void makeHelper2();

  virtual int data_to_table(VectorLinkedList *vll);
  virtual int table_to_data();
  Vector *update_list(VectorLinkedList *vll, long id,
       float xo, float zo, float yo, long n, int label);
  void dataDeleted(VectorListData *);

    // make widget tree
  Widget make(Widget p =NULL);

  virtual WidgetClass topClass() {return xmFormWidgetClass;}
  virtual Boolean isDialog()     { return False; }
  virtual Boolean isTopLevel()   { return False; }

//------------------ end functions --------------------//

} ;




class CellEditPop : public SLDialog
{
//---------------- beginning of class-------------------//

public:

  CellEditPop (SLDelay *contain, char *name, HelpCtx Hctx,
   void *cvm);
  CellEditPop (Widget parent, char *name, HelpCtx Hctx ,
   void *cvm);
  virtual ~CellEditPop(void);
  Widget  Form();

private:
  CellEdit *_cedit;
  SLSmartForm *_slform;

protected:
 Boolean removeNotify();

 enum PBTYPE {OK,CAN,REMOVE};
//-------------------- end of class ----------------------//
} ;

#endif

