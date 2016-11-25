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
#ifndef SLFORMFCPOP_H
#define SLFORMFCPOP_H

#include "sl/sl_form_pop.hh"
//#include "file_choice.h"

class SLFormFCPop : public SLFPopSep {
public:
  enum {NONE, GOOD, BAD};

  SLFormFCPop
    (Widget p, 
     char *name, 
     unsigned long buttons,
     class SLpFileData *slp_file_data,
     HelpCtx hctx,
     Boolean small_on_dpi = True,
     Boolean make_now = True);

  SLFormFCPop
    (PsuedoWidget *pw,
     char *name,
     unsigned long buttons,
     class SLpFileData *slp_file_data,
     HelpCtx hctx,
     Boolean small_on_dpi = True,
     Boolean make_now = True);

  virtual Widget make (Widget p = NULL);

  virtual void manage ();

  virtual Boolean ValidInput ();

  class SLpFile *file () { return _slp_file; };

  virtual Boolean setFilename
    (char *filename);

protected:
  virtual ~SLFormFCPop ();

  virtual void init
    (Display *dpy);

  virtual Boolean validateFile
    (char *filename);

  class SLpFile
    *_slp_file;

  class SLpFileData
    *_slp_file_data;

  int
    _state,
    _first_time;

private:
  static void FileSuccessCallback
    (void *data,
     long ident,
     char *oldvar,
     char *newvar);

  void FileSuccess
    (long ident,
     char *oldvar,
     char *newvar);
};

#endif
