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
//------------------- seg_edit.hh ------------------------------//
//------------------- seg_edit.hh ------------------------------//
//------------------- seg_edit.hh ------------------------------//

//              header file for the SegEdit class
//                 derived from the SLSmartForm class

#ifndef SEGEDIT_H
#define SEGEDIT_H

#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "sl/sl_smart_form.hh"

class ModBaseData;
class Vector;
class SeisVectLinkedList;
class SLSmartForm;
class SLpPush;
class SLpScale;
class SL2Text;
class SLRowColumn;
class ModelTable;


class SegEdit : public SLSmartForm {

//------------------ beginning of class--------------------------//
//------------------ beginning of class--------------------------//


  private:
     SLRowColumn *_slrca;
     ModelTable  *_table;
     SLSmartForm *_tab;
     SeisVectLinkedList *_vec_list;
     Vector      **_vectarr;
     Vector      *_loaded_vector;
     HelpCtx     _Hctx;

     int         _single;
     int         _phd; // save in case all data is deleted
     int         _shd;
     int         _thd;
     int         _zeit;
     int         _hasUserData;
     int         _id_val;
     char        _color_name[24];
     char        _segment_name[40];

     void    setup(void *data,char *segment_name);
     void    init_empty_list();

  protected:
     
     int     segment_count();


  public:
     SegEdit(Widget    parent,
             char     *name,
             void     *data,
             char     *segment_name,
             HelpCtx   Hctx=NULL,
             Boolean   doframe = False,
             Boolean   make_now = True,
             Boolean   manage_now = True,
             int       single=0);
     SegEdit(SLDelay  *contain,
             char     *name,
             void     *data,
             char     *segment_name,
             HelpCtx   Hctx=NULL,
             Boolean   doframe = False,
             Boolean   make_if_can = True,
             Boolean   manage_now = True,
             int       single=0);
     virtual ~SegEdit();

     Vector *SegSet(void *vec_list, char *segment_name);
     static Vector *SegSetv(Vector *vector,void *data);
     void   set_Phdr(int key) {_phd = key;}
     void   set_Shdr(int key) {_shd = key;}
     void   set_Thdr(int key) {_thd = key;}
     void   set_Zeit(int key) {_zeit= key;}

    // make widget tree
    Widget make(Widget p =NULL);

    virtual WidgetClass topClass() {return xmFormWidgetClass;}
    virtual Boolean isDialog()     { return False; }
    virtual Boolean isTopLevel()   { return False; }

//--------------------- end of class ----------------------------//
//--------------------- end of class ----------------------------//

 };

#endif

