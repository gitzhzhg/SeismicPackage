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
#include "sl/shell_mode.hh"
#include "sl/sl_shell_container.hh"
#include "wproc.h"
#include "cprim.h"

ShellMode::ShellMode(Widget w,char *s) : _current_str(NULL), _save_str(NULL)
{
    _my_shell= get_toplevel_shell(w);
    _current_str= newstr(s);
    _save_str= SLShellContainer::setModeWidget(_my_shell,_current_str);
}

ShellMode::~ShellMode()
{
    if (_save_str) SLShellContainer::resetModeWidget(_my_shell,_save_str);
    if (_current_str) free(_current_str);
}

void ShellMode::refresh()
{
    SLShellContainer::refreshModeWidget(_my_shell, _current_str);
}

void ShellMode::takeRecovery(ShellMode *sm)
{
    XtFree(_save_str);
    if (sm) {
        _save_str= sm->_save_str;
        sm->_save_str= NULL;
    }
    else
        _save_str= NULL;
}
