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

//---------------------- wbox_help.cc -------------------------//
//---------------------- wbox_help.cc -------------------------//
//---------------------- wbox_help.cc -------------------------//

//          implementation file for the WboxHelp class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_help.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_field.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//

  // if hctx is not NULL, it is used for the help for this windowbox.
  // if hctx is NULL, it is created using toplevel, helpfile, and helptitle.

WboxHelp::WboxHelp(WboxBox *box, HelpCtx hctx, Widget toplevel,
                   const char *helpfile, const char *helptitle)
         :
            _box             (box),
            _hctx            (hctx),
            _helpdata        (NULL)
{
  assert(_box);
  if(_hctx != NULL) return;
  if(toplevel == NULL) return;
  if(helpfile  == NULL || helpfile [0] == '\0' || helpfile [0] == ' ') return;
  if(helptitle == NULL || helptitle[0] == '\0' || helptitle[0] == ' ') return;
  _hctx = setup_help(toplevel, &_helpdata,
                            (char*)helpfile, (char*)helptitle);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxHelp::~WboxHelp()
{
}



//------------------------- show -------------------------------------//
//------------------------- show -------------------------------------//
//------------------------- show -------------------------------------//

  // private.

  //     endkey      ident    help file keyword    when shown 
  //   ----------  ---------  ------------------  ------------
  //   "OVERVIEW"  ignored    boxname             always      
  //   "KEYHELP"   ignored    "wbox_keyhelp"      always      
  //   "message"   ignored    "wbox_messageline"  if displayed
  //   "index"     ignored    "wbox_indexline"    if displayed
  //   ignored     9999       "wbox_find_index"   if displayed
  //   ignored     9998       "wbox_find_value"   if displayed
  //   "HELP"      any other  boxname_ident       always      
  //   any other   any other  boxname_ident       if displayed


void WboxHelp::show(const char *endkey, int ident)  const
{
  if(_hctx == NULL) return;

  static char helpnamekeep[200] = " ";
  char helpname[200];

  if(!strcmp(endkey, "KEYHELP"))
      {
      overview_help("wbox_keyhelp", _hctx);
      return;
      }
  if(!strcmp(endkey, "OVERVIEW"))
      {
      overview_help((char*)_box->getBoxName(), _hctx);
      return;
      }
  else if(!strcmp(endkey, "message")) strcpy(helpname, "wbox_message_line");
  else if(!strcmp(endkey, "index"  )) strcpy(helpname, "wbox_index_line");
  else if(ident == 9999)              strcpy(helpname, "wbox_find_index");
  else if(ident == 9998)              strcpy(helpname, "wbox_find_value");
  else
      {
      sprintf(helpname, "%s_%d", _box->getBoxName(), ident);
      }
  if(!strcmp(endkey, "HELP"))
      {
      ctxh_context_help(helpname, _hctx, CTXH_ALLWAYS);
      strcpy(helpnamekeep, helpname);
      }
  else
      {
      if(!strcmp(helpname, helpnamekeep)) return;
      ctxh_context_help(helpname, _hctx, CTXH_IFDISP);
      strcpy(helpnamekeep, helpname);
      }
}



//------------------------- show help --------------------------------//
//------------------------- show help --------------------------------//
//------------------------- show help --------------------------------//

   // public.

void WboxHelp::showHelp(const char *endkey)  const
{
  WboxField *active = _box->getActiveFieldPointer();
  int        ident  = active->getIdent();
  show(endkey, ident);
}



//------------------------- show help --------------------------------//
//------------------------- show help --------------------------------//
//------------------------- show help --------------------------------//

   // public.

void WboxHelp::showHelp(int irow, int icol)  const
{
  int   min_switch = -76;
  WboxField *field = _box->findFieldPointer(irow, icol, min_switch);
  if(field)
      {
      int ident = field->getIdent();
      show(" ", ident);
      }
  else if(irow == _box->getNrow())
      {
      show("message", 0);
      }
  else if(irow == _box->getNrow() - 1 && _box->hasLinkedArrays())
      {
      show("index", 0);
      }
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

