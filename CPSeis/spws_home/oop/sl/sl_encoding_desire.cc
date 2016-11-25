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

//------------------------ sl_encoding_desire.cc ----------------------//
//------------------------ sl_encoding_desire.cc ----------------------//
//------------------------ sl_encoding_desire.cc ----------------------//

//          implementation file for the SLEncodingDesire class
//                  derived from the SLSmartForm class
//                           subdirectory sl


#include "sl/sl_encoding_desire.hh"
#include "oprim/file_support_interface.hh"
#include "sl/slp_option.hh"
#include "named_constants.h"
#include <ctype.h>
#include <assert.h>


enum { OLDCPS = 1, ASCII, HYBRID, BINARY };

#define OLABEL  "save old style CPS"
#define ALABEL  "save self defining ascii"
#define BLABEL  "save self defining binary"
#define HLABEL  "save self defining hybrid"


//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//


static void desire_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)
{
  FileSupportInterface *interface = (FileSupportInterface*)data;
  switch(newvar)
      {
      case OLDCPS : interface->setEncoding("oldcps"); break;
      case ASCII  : interface->setEncoding("ascii" ); break;
      case HYBRID : interface->setEncoding("hybrid"); break;
      case BINARY : interface->setEncoding("binary"); break;
      default:      interface->setEncoding("ascii" ); break;
                                 // default changed from "oldcps" 2003-09-23
      }
}



//----------------------------- update functions ---------------------------//
//----------------------------- update functions ---------------------------//
//----------------------------- update functions ---------------------------//


static long desire_upfun(void *data)
{
  FileSupportInterface *interface = (FileSupportInterface*)data;
  const char *encoding = interface->getEncoding();
  if(!strcmp(encoding, "oldcps")) return (long)OLDCPS;
  if(!strcmp(encoding, "ascii" )) return (long)ASCII;
  if(!strcmp(encoding, "hybrid")) return (long)HYBRID;
  if(!strcmp(encoding, "binary")) return (long)BINARY;
  return (long)ASCII;       // changed from OLDCPS 2003-09-23
}



//----------------------- sense update functions ---------------------------//
//----------------------- sense update functions ---------------------------//
//----------------------- sense update functions ---------------------------//



//--------------------------- static add option ----------------------------//
//--------------------------- static add option ----------------------------//
//--------------------------- static add option ----------------------------//


static void add_option (const char *adjective, int allow,
                        SLpOption *desire, int ident,
                        const char *phrase)
{
  if(!allow) return;
  char label[66];
  strcpy(label, phrase);
  if(adjective)
      {
      strcat(label, " ");
      strcat(label, adjective);
      }
  strcat(label, " file");
  desire->addOption("desire", ident, label);
}



//------------------------------ constructor -----------------------------//
//------------------------------ constructor -----------------------------//
//------------------------------ constructor -----------------------------//


SLEncodingDesire::SLEncodingDesire (SLDelay *slparent,
                                    FileSupportInterface *interface,
                                    const char *adjective)
                    : SLSmartForm(slparent, "sl_encoding_desire")
{
  assert(interface);
  showEvenSpacing();

  interface->setEncoding("ascii" );

  SLpOption *desire = new SLpOption(this, "desire", 0, "");

  add_option (adjective, interface->allowOldcps(), desire, OLDCPS, OLABEL);
  add_option (adjective, interface->allowAscii (), desire, ASCII , ALABEL);
  add_option (adjective, interface->allowBinary(), desire, BINARY, BLABEL);
  add_option (adjective, interface->allowHybrid(), desire, HYBRID, HLABEL);

  desire->setItrap     (desire_trap , interface);
  desire->setupIvarFun (desire_upfun, interface);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


SLEncodingDesire::~SLEncodingDesire()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
