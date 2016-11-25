
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
//---------------------- fg_active_choice.cc -----------------------//
//---------------------- fg_active_choice.cc -----------------------//
//---------------------- fg_active_choice.cc -----------------------//


//         implementation file for the FgActiveChoice class
//                  derived from the SL2Arrows class
//                         subdirectory fggui


#include "fggui/fg_active_choice.hh"
#include "geom/field_geometry.hh"
#include <stdlib.h>
#include <string.h>



//---------------------------- trap -------------------------------//
//---------------------------- trap -------------------------------//
//---------------------------- trap -------------------------------//


void FgActiveChoice::num1Trap(void *data, long newvar)
{
  FgActiveChoice *THIS = (FgActiveChoice*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  fg->preMultipleOperations();
  switch(THIS->_choice)
      {
      case TRACE_PLUS_S:
            {
            fg->setActiveSourceIndices(newvar);
            }
            break;
      case TRACE_PLUS_R:
            {
            fg->setActiveReceiverIndices(newvar);
            }
            break;
      case TRACE_PLUS_PP:
            {
            long ixpp = fg->findPpCardWithDesiredTrace(newvar);
            if(ixpp >= 0) fg->setActivePpCardIndex(ixpp);
            }
            //////// fall thru.
      case TRACE:
            {
            long ntraces = fg->numTraces();
            if(ntraces == 0) break;
            long trace = newvar;
            if(trace <= 0 || trace > ntraces) break;
            fg->setActiveTraceNumber(trace);
            }
            break;
      case GROUP_PLUS_PP:
            {
            long ixpp = fg->findPpCardWithDesiredGroup(newvar);
            if(ixpp >= 0) fg->setActivePpCardIndex(ixpp);
            }
            //////// fall thru.
      case GROUP:
            {
            long ngroups = fg->numGroups();
            if(ngroups == 0) break;
            long group = newvar;
            if(group <= 0 || group > ngroups) break;
            fg->setActiveGroupNumber(group);
            }
            break;
      case FLAG:
            {
            long ixl = fg->getActiveLineIndex();
            if(ixl == -1) break;
            long nflags = fg->numFlagsOnLine(ixl);
            if(nflags == 0) break;
            long ixf = newvar - 1;
            if(ixf < 0 || ixf >= nflags) break;
            fg->setActiveFlagIndexOnLine(ixl, ixf);
            }
            break;
      case LINE:
            {
            long nlines = fg->numLines();
            if(nlines == 0) break;
            long ixl = newvar - 1;
            if(ixl < 0 || ixl >= nlines) break;
            fg->setActiveLineIndex(ixl);
            }
            break;
      case RPCARD:
            {
            long nrp = fg->numRpCards();
            if(nrp == 0) break;
            long ixrp = newvar - 1;
            if(ixrp < 0 || ixrp >= nrp) break;
            fg->setActiveRpCardIndex(ixrp);
            }
            break;
      case PPCARD:
            {
            long npp = fg->numPpCards();
            if(npp == 0) break;
            long ixpp = newvar - 1;
            if(ixpp < 0 || ixpp >= npp) break;
            fg->setActivePpCardIndex(ixpp);
            }
            break;
      case CMP:
            {
            long ncmps = fg->numCmpGathers();
            if(ncmps == 0) break;
            long ixcmp = newvar - 1;
            if(ixcmp < 0 || ixcmp >= ncmps) break;
            fg->setActiveCmpIndex(ixcmp);
            }
            break;
      default:
            assert(FALSE);
      }
  fg->postMultipleOperations();
}



//------------------------ update functions -----------------------//
//------------------------ update functions -----------------------//
//------------------------ update functions -----------------------//


long FgActiveChoice::num1Update(void *data)
{
  FgActiveChoice *THIS = (FgActiveChoice*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  switch(THIS->_choice)
      {
      case TRACE_PLUS_S :
      case TRACE_PLUS_R :
      case TRACE_PLUS_PP:
      case TRACE        : return fg->getActiveTraceNumber();
      case GROUP_PLUS_PP:
      case GROUP        : return fg->getActiveGroupNumber();
      case FLAG         :
            {
            long ixl = fg->getActiveLineIndex();
            if(ixl == -1) return 0;
            return fg->getActiveFlagIndexOnLine(ixl) + 1;
            }
      case LINE         : return fg->getActiveLineIndex  () + 1;
      case RPCARD       : return fg->getActiveRpCardIndex() + 1;
      case PPCARD       : return fg->getActivePpCardIndex() + 1;
      case CMP          : return fg->getActiveCmpIndex   () + 1;
      default           : assert(FALSE);
      }
  assert(FALSE);
  return 0;
}


long FgActiveChoice::num2Update(void *data)
{
  FgActiveChoice *THIS = (FgActiveChoice*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  switch(THIS->_choice)
      {
      case TRACE_PLUS_S :
      case TRACE_PLUS_R :
      case TRACE_PLUS_PP:
      case TRACE        : return fg->numTraces();
      case GROUP_PLUS_PP:
      case GROUP        : return fg->numGroups();
      case FLAG         :
            {
            long ixl = fg->getActiveLineIndex();
            if(ixl == -1) return 0;
            return fg->numFlagsOnLine(ixl);
            }
      case LINE         : return fg->numLines();
      case RPCARD       : return fg->numRpCards();
      case PPCARD       : return fg->numPpCards();
      case CMP          : return fg->numCmpGathers();
      default           : assert(FALSE);
      }
  assert(FALSE);
  return 0;
}


char *FgActiveChoice::labelUpdate(void *data)
{
  FgActiveChoice *THIS = (FgActiveChoice*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  static char *label_trace_plus_s  = "active trace (plus source)";
  static char *label_trace_plus_r  = "active trace (plus receiver)";
  static char *label_trace_plus_pp = "active trace (plus PP card)";
  static char *label_trace         = "active trace";
  static char *label_group         = "active group";
  static char *label_group_plus_pp = "active group (plus PP card)";
  static char *label_flag          = "active flag";
  static char *label_line          = "active line";
  static char *label_rpcard        = "active RP card";
  static char *label_ppcard        = "active PP card";
  static char *label_cmp           = "active CMP";
  if(THIS->_label) return THIS->_label;
  switch(THIS->_choice)
      {
      case TRACE_PLUS_S : return label_trace_plus_s;
      case TRACE_PLUS_R : return label_trace_plus_r;
      case TRACE_PLUS_PP: return label_trace_plus_pp;
      case TRACE        : return label_trace;
      case GROUP_PLUS_PP: return label_group_plus_pp;
      case GROUP        : return label_group;
      case FLAG         : return label_flag;
      case LINE         : return label_line;
      case RPCARD       : return label_rpcard;
      case PPCARD       : return label_ppcard;
      case CMP          : return label_cmp;
      default           : assert(FALSE);
      }
  assert(FALSE);
  return NULL;
}



//---------------------- constructor ------------------------//
//---------------------- constructor ------------------------//
//---------------------- constructor ------------------------//


FgActiveChoice::FgActiveChoice(SLDelay *slparent, char *name,
                               FieldGeometry *fg,
                               ActiveChoice   choice,
                               const char *label)
           : SL2Arrows(slparent, name, NULL, 7),
                 _fg            (fg),
                 _choice        (TRACE),
                 _label         (NULL)
{
  assert(_fg);
  setActiveChoice(choice, label);

  registerNum1Trap    (num1Trap   , this);
  registerLabelUpdate (labelUpdate, this);
  registerNum1Update  (num1Update , this);
  registerNum2Update  (num2Update , this);
//registerSenseUpdate (senseUpdate, this);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FgActiveChoice::~FgActiveChoice()
{
  if(_label) delete [] _label;
}



//----------------------- set active choice ----------------------//
//----------------------- set active choice ----------------------//
//----------------------- set active choice ----------------------//

       // public.

void FgActiveChoice::setActiveChoice(ActiveChoice choice,
                                     const char *label)
{
  _choice = choice;
  if(_label)
      {
      delete [] _label;
      _label = NULL;
      }
  if(label)
      {
      int len = strlen(label);
      _label = new char [len + 1];
      strcpy(_label, label);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
