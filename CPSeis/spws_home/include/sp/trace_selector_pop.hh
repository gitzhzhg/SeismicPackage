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
#ifndef TRACE_SELECTOR_POP_H
#define TRACE_SELECTOR_POP_H

#include "wproc.h"
#include "sl/sl_form_pop.hh"


class TraceSelectorPop : public SLFPopSep {


     public:
       TraceSelectorPop( Widget    p,
                         char      *name,
                         HelpCtx   hctx,
                         class SeisSelect *ss);

       virtual ~TraceSelectorPop();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual Boolean notifyComplex(SLDelay*, int);
       Boolean _first_time;
       long tracesFound(){ return _traces_found; }
       void seisPlotChanged(class SeisPlot *sp);

       long  _doprimary;
       long  _dosecondary;
       long  _dotertiary;
       int   _primary_header;
       float _primary_min;
       float _primary_max;
       float _primary_inc;
       int   _secondary_header;
       float _secondary_min;
       float _secondary_max;
       float _secondary_inc;
       int   _tertiary_header;
       float _tertiary_min;
       float _tertiary_max;
       float _tertiary_inc;
       long  _traces_found;
       long  _frames_found;
       long  _traces_in_last_frame;


       enum{DOPRIMARY, DOSECONDARY, DOTERTIARY, PRIMARY_HEADER, 
            SECONDARY_HEADER, TERTIARY_HEADER, PRIMARY_MIN, SECONDARY_MIN,
            TERTIARY_MIN, PRIMARY_MAX, SECONDARY_MAX, TERTIARY_MAX, 
            PRIMARY_INC, SECONDARY_INC, TERTIARY_INC, SEARCH};


    protected:
      virtual void okButton();
      virtual void cancelButton();
      virtual long findTraces();


    private:
      static void headerTogChanged(void *data, long ident, Boolean set);
      static void headerNumberChanged(void *data, long which);
      static void headerMinChanged(void *data, long which);
      static void headerMaxChanged(void *data, long which);
      void  showInformation();
      class SeisSelect *_ss;
      class SeisPlot *_sp;
      class SLForm *_header_form;
      class SLTogBox *_header_togs;
      class SLTextBox *_header_text;
      class SLPushBox *_search_button;
      class DoAbort *_do_abort;
      Widget _information;
      int   _pixmap;
      long  _nplt;
      long  _iskp;
      long  _ndo;
      long  _nskp;
      long  _frames;
      long  _domovie;

};




#endif
