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
#include "sl/sl_def_app_pop.hh"
#include "sl/sl_app.hh"
#include "sl/shell_stat_msg.hh"
#include "wproc.h"

SLDefAppPop::SLDefAppPop(  Widget      p,
                           char        *name,
                           HelpCtx     hctx,
                           SLpFileData *slp_file_data,
                           SLApp       *app,
                           Boolean     make_now) :
             SLDefPop(p,name,hctx,slp_file_data,NULL,make_now), _app(app)
{}

void SLDefAppPop::saveFile( char *filename, void *)
{
  DefInfo def;
  ShellStatMsg  bld_info(W(),"Saving Defaults...");
  if (filename) def= DefFileInit(filename, 6, True);
  else          def= DefStandardInit(get_shell_widget(_app->mainWindow()), 6);
 
  SLDelay::makeAll();
  DefSave(def, _app->mainWindow());
  DefEnd(def);
}

void SLDefAppPop::getFile( char *, void *) {}
