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
#ifndef ERRORHANDLER_H 
#define ERRORHANDLER_H 

#include <stdlib.h>
#include "sl/err_list.hh"

typedef void (*ErrorHandlerFunc)(void*);

class ErrorHandler;

class ErrorInform {
  private:
    ErrorHandlerFunc _altErrAction;
    void            *_altErrData;
    ErrorHandler    *_eh;
  public:
    ErrorInform(ErrorHandler *eh, 
                ErrorHandlerFunc func =NULL, void *data =NULL) :
                   _altErrAction(func), _altErrData(data), _eh(eh) {}
    void doCalls()  { if (_altErrAction) _altErrAction(_altErrData);}
    void ehDestroyed() {_eh= NULL;}
    ErrorHandler *eh() {return _eh;}
};



class ErrorHandler {
  private:
    static void doNothingCallback(Widget, XtPointer, XmAnyCallbackStruct*);
    static void doOkCallback(Widget, XtPointer, XmAnyCallbackStruct*);
       
  protected:
       static ErrList _box_list;
       char         *_window_name;
       Widget       _ebox;
       Widget       _shell;
       Boolean      _is_modal;
       ErrorInform     *_prog_info;

       Boolean _gui_perr;
       Boolean _cui_perr;

       Boolean _gui_err;
       Boolean _cui_err;

       Boolean _gui_warn;
       Boolean _cui_warn;

       void init(Widget w, char *window_name);
       //virtual void errorAck() {};

  public:
       enum _etype {Warning, Error, ProgError, GUI, CUI};
       ErrorHandler(Widget w, char *window_name = "Error", Boolean modal =True);
       //ErrorHandler(Widget w, char *window_name, ...);
       virtual ~ErrorHandler(); 
       void setWindowName(char*);
       void setEtype(const int etype, const int behavior, Boolean state);
       void deliverError(const char *estr = "Undefined Error",
                         const int  type = Error);
       void deliverInformation(const char *estr, 
                               const char *title = "Information");
       void setAltErrorAck( ErrorHandlerFunc action =NULL, void *data =NULL)
             { if (_prog_info) delete _prog_info;
               _prog_info= new ErrorInform(this, action, data); }
       void showPopup(const char *estr);
};
#endif
