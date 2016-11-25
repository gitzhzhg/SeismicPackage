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
#include "sl/shell_mouse_help.hh"
#include "sl/sl_shell_container.hh"
#include "wproc.h"
#include "cprim.h"

#define NOHELP \
    "ShellMouseHelp: no mouse help has been enabled for widget: %s\n"

static char *rollback_help= "mouse*%s.DEFAULT:  BTN#1: none, BTN#2: none, \
BTN#3: Popup Menu";


ShellMouseHelp::ShellMouseHelp( const Widget w, 
                                const char * const token, 
                                const char * const fallback) :
            _mhelp_w(w), _save_token(NULL), _token(NULL)
{
  Widget my_shell= get_shell_widget(w);
  Widget my_top_shell= get_toplevel_shell(w);
  _hctx= SLShellContainer::getShellHelpCtx(my_top_shell);
  _token= newstr((char*)token);
  if (_hctx) {
         _save_token= ctxhGetTolken(_hctx, w);
         if (_save_token) {
              _save_token= newstr(_save_token);
              changeToken(token, fallback);
         }
         else {
              Widget m_area= SLShellContainer::getMsgArea(my_shell);
              if (!m_area) 
                   m_area= SLShellContainer::getMsgArea(my_top_shell);
              if (m_area) {  // create a default save token
                   char defhelp[300];
                   _save_token= newstr("DEFAULT");
                   ctxhMouseHelp(_hctx, m_area, _mhelp_w, (char*)token);
                   changeToken(token, fallback);
                   sprintf(defhelp, rollback_help, XtName(_mhelp_w) );
                   ctxhMergeHelpLine(_hctx, defhelp);
              }
         }
  } 
}



ShellMouseHelp::~ShellMouseHelp()
{
  if ( (_hctx)&&(_save_token) ) {
          ctxhChangeTolken(_hctx, _mhelp_w, _save_token);
          free(_save_token);
  }
  if (_token) free(_token);
}

void ShellMouseHelp::setFallback(const char * const fallback)
{
  if (_hctx) ctxhMergeHelpLine(_hctx, (char*)fallback);
}


void ShellMouseHelp::changeToken(const char * const token,
                            const char * const fallback)
{
  if (fallback) setFallback(fallback);
  if ((_hctx)&&(token)) ctxhChangeTolken(_hctx, _mhelp_w, (char*)token);
}

void ShellMouseHelp::refresh()
{
  if ((_hctx)&&(_token)) ctxhChangeTolken(_hctx, _mhelp_w, _token);
}


void ShellMouseHelp::takeRecovery(ShellMouseHelp *smh)
{
  if (_save_token) free(_save_token);
  if (smh) {
     _save_token= smh->_save_token;
     smh->_save_token= NULL;
  }
  else 
     _save_token= NULL;
}
