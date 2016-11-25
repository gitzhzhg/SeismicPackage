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
//------------------- horiz_edit.hh ------------------------------//
//------------------- horiz_edit.hh ------------------------------//
//------------------- horiz_edit.hh ------------------------------//

//              header file for the HorizEdit class
//                 derived from the SLSmartForm class

#ifndef HORIZEDIT_H
#define HORIZEDIT_H

#include <stdio.h>
#include <Xm/Form.h>
#include "sl/sl_smart_form.hh"

class VectorListData;
class HorizTable;
class SegEdit;

class HorizEdit : public SLSmartForm {

//------------------ beginning of class--------------------------//
//------------------ beginning of class--------------------------//


  private:
     HorizTable     *_hztable;
     SegEdit        *_segedit;
     VectorListData *_vldata;
     HelpCtx        _Hctx;


  protected:


  public:
    HorizEdit(SLDelay *contain,
              char    *pname,
              void    *data,
              HelpCtx Hctx,
              Boolean doframe,
              Boolean make_if_can,
              Boolean   manage_now );
    HorizEdit(Widget parent,
              char   *pname,
              void   *data,
              HelpCtx Hctx,
              Boolean doframe,
              Boolean make_now,
              Boolean   manage_now );
    virtual ~HorizEdit();

    void   PassRepData(void *data);
    void   PassRepFunc(void (*func)(void *,int , int));
    void   SetData(void *vec_list);

    // make widget tree
    Widget make(Widget p =NULL);

    virtual WidgetClass topClass() {return xmFormWidgetClass;}
    virtual Boolean isTopLevel()   { return False; }
    virtual Boolean isDialog()     { return False; }

//--------------------- end of class ----------------------------//
//--------------------- end of class ----------------------------//

 };

#endif


