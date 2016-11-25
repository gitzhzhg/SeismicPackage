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
#ifndef SPECTRA_TABLE_POP_HH
#define SPECTRA_TABLE_POP_HH

#include "sl/sl_databox.hh"
#include "sl/sl_form_pop.hh"
#include <Xm/Xm.h>




class SpectraTableList : public SLDatabox
{

  public:
    SpectraTableList(Widget slparent, class SpectraTablePop *table_pop);
    void removeButton();
    int _being_deleted;
    void addRow(int index, int color, float time_shift, float time_first,
                float average_phase);
    void deleteAllRows();
  class SeveralFloatArrays *getData(){ return _data; }

  private:
    class SpectraTablePop *_table_pop;
    void makeHelper();
    class SeveralFloatArrays *_data;
};


class SpectraTablePop : public SLFPopSep
{

  public:
    SpectraTablePop(Widget        p, 
                    char          *name,
                    HelpCtx       hctx,
                    char          *title);
    ~SpectraTablePop();
    virtual Widget make(Widget p);    
    Boolean notifyComplex(SLDelay*, int);
    virtual void removeButton();
    void setNewTitle(char *newtitle);
    void addRow(int index, int color, float time_shift, float time_first,
                float average_phase);
    void deleteAllRows();


  private:
    char                   *_title;
    class SpectraTableList *_table_list;
};




#endif
