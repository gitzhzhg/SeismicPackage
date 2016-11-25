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


//------------------------- slp_file.hh -------------------------------//
//------------------------- slp_file.hh -------------------------------//
//------------------------- slp_file.hh -------------------------------//

//               header file for the SLpFile class
//               derived from the SLSmartForm class
//                         subdirectory sl

#ifndef _SLP_FILE_HH_
#define _SLP_FILE_HH_

#include "sl/sl_smart_form.hh"
#include "sl/slp_text.hh"
#include <time.h>


class SLpFile : public SLSmartForm
{
public:      // data

  enum { _INPUT, _OUTPUT };

private:

  long                       _ident;
  char                      *_label;      // label for pushbutton or label.
  char                      *_filetype;   // title for file selection box.
  char                      *_extension;  // default extension for files.
  const int                  _io;         // either _INPUT or _OUTPUT.
  char                      *_pattern;    // default pattern for files.
  class SLpPush             *_slp_push;
  class SLpLabel            *_slp_label;
  class SLpText             *_slp_text;
  class SLFileSelectionPop  *_sl_filebox;
  CtrapFun                  *_trap;
  void                      *_data;

public:

  SLpFile(SLDelay *slparent, char *name, long ident = 0,
                             const char *label     = NULL,
                             const char *filetype  = NULL,
                             const char *extension = NULL,
	                     int         io        = _INPUT,
	                     const char *pattern   = NULL);

  SLpFile(Widget    wparent, char *name, long ident = 0,
                             const char *label     = NULL,
                             const char *filetype  = NULL,
                             const char *extension = NULL,
	                     int         io        = _INPUT,
	                     const char *pattern   = NULL);

  SLpFile(SLDelay *slparent, class SLpFileData *data);

  SLpFile(Widget    wparent, class SLpFileData *data);

  virtual ~SLpFile (void);

  virtual Widget make (Widget p = NULL); // overrides SLDelay

  void resetLabel (const char *label = NULL);

  void resetExtension (const char *extension = NULL);   // does not work.

  void resetPattern (const char *pattern = NULL);

  time_t timeStamp (char *filename);

public:   // functions which mimic SLpBase objects.

  void setCtrap (CtrapFun*trap, void *data) { _trap = trap; _data = data; }
  void setCvar  (char *x)                   { _slp_text->setCvar(x); }
  char *cvar    (void)  const               { return _slp_text->cvar(); }
  int type      (void)  const               { return _io; }

public:   // optional convenience functions

  char   *filename              (void)  const { return _slp_text->cvar(); }
  char   *oldFilename           (void)  const { return _slp_text->oldCvar(); }
  void setFilenameSystemDefault (char *x) { _slp_text->setCvarSystemDefault(x);}
  void setFilename              (char *x)   { _slp_text->setCvar(x); }
  void setupFilenameValue       (char *x)   { _slp_text->setupCvarValue(x); }
  void setupFilenamePoint (char *x, long n) { _slp_text->setupCvarPoint(x, n); }
  void setupFilenameFun (char*(*f)(void*), void *d)
                                            { _slp_text->setupCvarFun(f,d); }
  SLpText  *text  ()                        { return _slp_text; }
  SLpPush  *push  ()                        { return _slp_push; }
  SLpLabel *label ()                        { return _slp_label; }
  SLFileSelectionPop *filebox ()            { return _sl_filebox; }
  static Boolean isAnEmptyFilename (const char *filename);

private:

  void constructorHelper (long ident,
                          const char *label,
                          const char *filetype,
                          const char *extension,
                          const char *pattern);

  static void yesFun  (void *data, const char *filename);
  static void trapFun (void *data, long ident, char  *oldvar, char  *newvar);

/*
  static void charCallback(Widget w, XtPointer user, XtPointer call);
*/
} ;


#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
