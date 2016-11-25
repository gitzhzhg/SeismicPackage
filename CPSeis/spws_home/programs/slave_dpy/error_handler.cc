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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>

#include "sl/error_handler.hh"
#include "cprim.h"
#include "wproc.h"

ErrList ErrorHandler::_box_list;



ErrorHandler::ErrorHandler(Widget w,
                           char   *window_name,
                           Boolean modal) :
            _ebox(NULL),       _window_name(NULL),
            _gui_perr(False),  _cui_perr(True),
            _gui_err(True),    _cui_err(True),
            _gui_warn(True),   _cui_warn(False),
            _prog_info(NULL),  _is_modal(modal)
{
  init(w,window_name);
}


void ErrorHandler::init(Widget w,
                        char   *window_name)
{
  _shell= get_shell_child(w);
  EboxElement *ele;

  setWindowName(window_name);
  ele= _box_list.find(_shell);

  if (ele) {
       _ebox= ele->eBox();
  }
  else {
       _ebox= make_okbox(_shell, "Error",  XmDIALOG_ERROR);
       XtAddCallback( _ebox, XmNokCallback, 
                      (XtCallbackProc)doNothingCallback, NULL);
       _box_list.add(_ebox,_shell);
  }
}

ErrorHandler::~ErrorHandler() 
{
  if (_window_name) free(_window_name); 
  if (_prog_info) _prog_info->ehDestroyed();
}


void ErrorHandler::showPopup(const char *estr)
{
  XtVaSetValues(_ebox, XmNdialogType, XmDIALOG_ERROR, NULL); 
  XtAddCallback( _ebox, XmNokCallback, 
                 (XtCallbackProc)doOkCallback, (XtPointer)_prog_info );
  XtVaSetValues(_ebox, XmNdialogStyle, 
              _is_modal ? XmDIALOG_APPLICATION_MODAL : XmDIALOG_MODELESS, NULL);
  show_msg(_ebox, (char*)estr);
}



void ErrorHandler::setWindowName(char *window_name) 
{
  if (_window_name) free(_window_name);
  _window_name= newstr(window_name);
}

void ErrorHandler::deliverError(const char *estr,
                                const int  type)
{
  XtVaSetValues(XtParent(_ebox), XmNtitle, _window_name, NULL);

  switch (type) {
    case Warning:
                 if (_gui_warn) showPopup(estr);
                 if (_cui_warn) fprintf(stderr, "%s\n", estr);
                 break;

    case Error:
                 if (_gui_warn) showPopup(estr);
                 if (_cui_err)  fprintf(stderr, "%s\n", estr);
                 break;

    case ProgError:
                 if (_gui_warn) showPopup(estr);
                 if (_cui_perr) fprintf(stderr, "%s\n", estr);
                 break;
    default:
                 fprintf(stderr,  "%s\n%s\n%s\n%s\n"
                  "ErrorHandler::deliverError: you must pass the constants:"
                  "                               ErrorHandler::Warning," 
                  "                               ErrorHandler::Error,"
                  "                            or ErrorHandler::ProgError");
                 break;

  }
}


void ErrorHandler::deliverInformation(const char *estr, const char *title)
{
  XtVaSetValues(XtParent(_ebox), XmNtitle, title, NULL);
  XtVaSetValues(_ebox, XmNdialogType, XmDIALOG_INFORMATION, NULL); 
  XtVaSetValues(_ebox, XmNdialogStyle, 
              _is_modal ? XmDIALOG_APPLICATION_MODAL : XmDIALOG_MODELESS, NULL);
  XtAddCallback( _ebox, XmNokCallback, 
                 (XtCallbackProc)doOkCallback, (XtPointer)_prog_info );
  show_msg(_ebox, (char*)estr);
}




void ErrorHandler::setEtype(const int etype, const int behavior, Boolean state)
{

  switch (etype) {
    case Warning:
                 if (behavior == GUI) _gui_warn= state;
                 if (behavior == CUI) _cui_warn= state;
                 break;

    case Error:
                 if (behavior == GUI) _gui_err= state;
                 if (behavior == CUI) _cui_err= state;
                 break;

    case ProgError:
                 if (behavior == GUI) _gui_perr= state;
                 if (behavior == CUI) _cui_perr= state;
                 break;
    default:
                 fprintf(stderr,  "%s\n%s\n%s\n%s\n"
                  "ErrorHandler::deliverError: you must pass the constants:"
                  "                               ErrorHandler::Warning," 
                  "                               ErrorHandler::Error,"
                  "                            or ErrorHandler::ProgError");
                 break;

     }


}


void ErrorHandler::doNothingCallback(Widget, XtPointer, XmAnyCallbackStruct*) {};

void ErrorHandler::doOkCallback(Widget w , 
                                XtPointer udata, 
                                XmAnyCallbackStruct*)
{
  ErrorInform *obj = (ErrorInform *)udata;
  XtRemoveCallback( w, XmNokCallback, 
                    (XtCallbackProc)doOkCallback, (XtPointer)udata );
  if (obj) { 
       obj->doCalls();
       if (!obj->eh()) delete obj;
  } 
}
