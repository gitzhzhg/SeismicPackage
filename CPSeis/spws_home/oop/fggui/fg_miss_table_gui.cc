
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
//---------------------- fg_miss_table_gui.cc -----------------------//
//---------------------- fg_miss_table_gui.cc -----------------------//
//---------------------- fg_miss_table_gui.cc -----------------------//

//        implementation file for the FgMissTableGui class
//                derived from the SLDatabox class
//               derived from the SmartArray class
//                       subdirectory fggui


#include "fggui/fg_miss_table_gui.hh"
#include "fggui/fg_miss_pop.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define STEP 100

//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FgMissTableGui::FgMissTableGui(SLDelay *slparent, char *name,
                                   FieldGeometry *fg, FgMissPop *pop)
           : SLDatabox(slparent, name, NULL, 4),
             SmartArray(STEP),
                        _fg    (fg),
                        _pop   (pop)
{
  assert(fg && pop);
  deleteMissingInfo();   // to get initial message in tables.
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FgMissTableGui::~FgMissTableGui()
{
  removeAllElements();
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

static long length_of_message = 0;

void *FgMissTableGui::doCreateObject()
{
  char *msg = new char [length_of_message + 1];
  msg[0] = '\0';
  return msg;
}


void FgMissTableGui::doDeleteObject(void *object)
{
  delete [] (char*)object;
}





//------------------------ update functions --------------------//
//------------------------ update functions --------------------//
//------------------------ update functions --------------------//


static long n_update(void *data)
{
  FgMissTableGui *THIS = (FgMissTableGui*)data;
  return THIS->numMessages();
}



static char *buff1 = "            MISSING INFORMATION IN JD            ";
static char *buff2 = " this list of missing information is OUT-OF-DATE ";


static char *prompt_update(void *data, long /*ident*/, long /*index*/)
{
  FgMissTableGui *THIS = (FgMissTableGui*)data;
  FgMissPop *pop = THIS->getFgMissPop();
  if(pop->getUpdateFlag()) return buff1;
  return buff2;
}



static char *msg_update(void *data, long /*ident*/, long index)
{
  FgMissTableGui *THIS = (FgMissTableGui*)data;
  return THIS->messageElement(index);
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void FgMissTableGui::makeHelper()
{
  static long zero =   0; 
  static long m44  = -44; 

         //    N       NMAX    ROW COL NCHAR MAXROWS
  dbox_rega(n_update, n_update, 0,  0,   6,    40);

         //  ID  PROMPT  SWITCH  SWITCH  COL NCHAR NDEC
  dbox_crega(77, buff2,   &m44 ,  &zero,  0,  70,   0);

  dbox_set_cfun (-77, prompt_update);
  dbox_set_cfun ( 77, msg_update);
}



//------------------- static variables -------------------------------//
//------------------- static variables -------------------------------//
//------------------- static variables -------------------------------//

       // these are for temporary use by updateMissingInfo
       // and by all but the first addMessage functions.

static int small_kount = 0;
static int large_kount = 0;
static char heading[200];



//------------------------ add message -------------------------------//
//------------------------ add message -------------------------------//
//------------------------ add message -------------------------------//

        // private.
        // call these functions to add one line to the list of
        //         missing information.

void FgMissTableGui::addMessage(const char *message)
{
  length_of_message = strlen(message);
  appendNullElement();
  long n = numElements();
  char *msg = messageElement(n - 1);
  strcpy(msg, message);
  length_of_message = 0;
}



void FgMissTableGui::addMessage(const char *type, const char *word,
                                         long index, long line)
{
  if(small_kount == 0) addMessage(heading);
  char msg[100];
  sprintf(msg, "on %s card# %d:  cannot find %s line# %d",
                          type, index+1, word, line);
  addMessage(msg);
  small_kount++;
  large_kount++;
}



void FgMissTableGui::addMessage(const char *type, const char *word,
                                         long index, float shot, long line)
{
  if(small_kount == 0) addMessage(heading);
  char msg[100];
  sprintf(msg, "on %s card# %d:  cannot find %s SP# %.3f on line# %d",
                          type, index+1, word, shot, line);
  addMessage(msg);
  small_kount++;
  large_kount++;
}



void FgMissTableGui::addMessage(const char *type,
                              long index, const char *word, long group)
{
  if(small_kount == 0) addMessage(heading);
  char msg[100];
  sprintf(msg, "on %s card# %d:  cannot find %s# %d",
                          type, index+1, word, group);
  addMessage(msg);
  small_kount++;
  large_kount++;
}



//-------------------------- delete missing info -------------------------//
//-------------------------- delete missing info -------------------------//
//-------------------------- delete missing info -------------------------//

       // public.
       // called to delete the list of missing information because
       //    it is now out-of-date.

void FgMissTableGui::deleteMissingInfo()
{
  removeAllElements();
  addMessage("Press 'List Missing Info' to get a list of");
  addMessage("missing information in this JD.");
}



//------------------- macros for use below ---------------------------//
//------------------- macros for use below ---------------------------//
//------------------- macros for use below ---------------------------//


#define HEADING(heading2)                                     \
           strcpy(heading, heading2);                         \
           small_kount = 0;


#define ERROR_CHECK                                           \
           if(small_kount > 500)                              \
                {                                             \
                addMessage("Too much missing information.");  \
                addMessage("Listing terminated.");            \
                break;                                        \
                }


#define MAYBE_BLANK                                           \
           if(small_kount > 0) addMessage(" ");               \
           small_kount = 0;



//-------------------------- update missing info -------------------------//
//-------------------------- update missing info -------------------------//
//-------------------------- update missing info -------------------------//

       // public.
       // called to create a new list of missing information.

void FgMissTableGui::updateMissingInfo()
{
  removeAllElements();
  long nlines = _fg->numLines();
  long nrp    = _fg->numRpCards();
  long npp    = _fg->numPpCards();
  long nzt1   = _fg->numZt1Cards();
  long nzt2   = _fg->numZt2Cards();
  long nzt3   = _fg->numZt3Cards();
  long nzt4   = _fg->numZt4Cards();
  small_kount = 0;
  large_kount = 0;

  if(nlines == 0)
      {
      addMessage("Since there are no LD cards,");
      addMessage("missing shotpoints and line numbers are not listed.");
      addMessage(" ");
      large_kount++;
      }
  if(nrp == 0)
      {
      addMessage("Since there are no RP cards,");
      addMessage("missing receiver pattern numbers are not listed.");
      addMessage(" ");
      large_kount++;
      }
  if(npp == 0)
      {
      addMessage("Since there are no PP cards,");
      addMessage("missing group numbers are not listed.");
      addMessage(" ");
      large_kount++;
      }


  if(nrp > 0 && npp > 0)
      {
      HEADING("PP RECEIVER PATTERN REFERENCES NOT FOUND ON RP CARDS:")
      for(long ixpp = 0; ixpp < npp; ixpp++)
          {
          long pattern = _fg->getPatternNumber(ixpp);
          long ixrp = _fg->findReceiverPattern(pattern);
          if(ixrp == -1) addMessage("PP", ixpp, "receiver pattern", pattern);
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(nlines > 0 && npp > 0)
      {
      HEADING("PP SOURCE REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixpp = 0; ixpp < npp; ixpp++)
          {
          long sline = _fg->getSourceLine(ixpp);
          long ixl   = _fg->findMatchingLineNumber(sline);
          if(ixl == -1) addMessage("PP", "source", ixpp, sline);
          else
              {
              float sshot = _fg->getSourceShotpoint(ixpp);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, sshot);
              if(ixf == -1) addMessage("PP", "source", ixpp, sshot, sline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(nlines > 0 && npp > 0)
      {
      HEADING("PP RECEIVER REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixpp = 0; ixpp < npp; ixpp++)
          {
          long rline = _fg->getReceiverLine(ixpp);
          long ixl   = _fg->findMatchingLineNumber(rline);
          if(ixl == -1) addMessage("PP", "receiver", ixpp, rline);
          else
              {
              float rshot = _fg->getReceiverShotpoint(ixpp);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, rshot);
              if(ixf == -1) addMessage("PP", "receiver", ixpp, rshot, rline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(nlines > 0 && nrp > 0)
      {
      HEADING("RP RECEIVER REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixrp = 0; ixrp < nrp; ixrp++)
          {
          long rline = _fg->getRpLineNumber(ixrp);
          long ixl   = _fg->findMatchingLineNumber(rline);
          if(ixl == -1) addMessage("RP", "receiver", ixrp, rline);
          else
              {
              float rshot = _fg->getRpShotpoint(ixrp);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, rshot);
              if(ixf == -1) addMessage("RP", "receiver", ixrp, rshot, rline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(nlines > 0 && nzt1 > 0)
      {
      HEADING("ZT1 SOURCE REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixzt1 = 0; ixzt1 < nzt1; ixzt1++)
          {
          long sline = _fg->getZt1SourceLineNumber(ixzt1);
          long ixl   = _fg->findMatchingLineNumber(sline);
          if(ixl == -1) addMessage("ZT1", "source", ixzt1, sline);
          else
              {
              float sshot = _fg->getZt1FromSourceShotpoint(ixzt1);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, sshot);
              if(ixf == -1) addMessage("ZT1", "source", ixzt1, sshot, sline);
              sshot = _fg->getZt1ToSourceShotpoint(ixzt1);
              ixf   = _fg->findMatchingShotpointOnLine(ixl, sshot);
              if(ixf == -1) addMessage("ZT1", "source", ixzt1, sshot, sline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(nlines > 0 && nzt2 > 0)
      {
      HEADING("ZT2 RECEIVER REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixzt2 = 0; ixzt2 < nzt2; ixzt2++)
          {
          long rline = _fg->getZt2ReceiverLineNumber(ixzt2);
          long ixl   = _fg->findMatchingLineNumber(rline);
          if(ixl == -1) addMessage("ZT2", "receiver", ixzt2, rline);
          else
              {
              float rshot = _fg->getZt2FromReceiverShotpoint(ixzt2);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, rshot);
              if(ixf == -1) addMessage("ZT2", "receiver", ixzt2, rshot, rline);
              rshot = _fg->getZt2ToReceiverShotpoint(ixzt2);
              ixf   = _fg->findMatchingShotpointOnLine(ixl, rshot);
              if(ixf == -1) addMessage("ZT2", "receiver", ixzt2, rshot, rline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(npp > 0 && nzt3 > 0)
      {
      HEADING("ZT3 GROUP REFERENCES NOT FOUND ON PP CARDS:")
      for(long ixzt3 = 0; ixzt3 < nzt3; ixzt3++)
          {
          long ngroups = _fg->numGroups();
          long group = _fg->getZt3FromGroupNumber(ixzt3);
          if(group < 1 || group > ngroups)
                   addMessage("ZT3", ixzt3, "group", group);
          group = _fg->getZt3ToGroupNumber(ixzt3);
          if(group < 1 || group > ngroups)
                   addMessage("ZT3", ixzt3, "group", group);
          ERROR_CHECK
          }
      MAYBE_BLANK
      }




  if(nlines > 0 && nzt4 > 0)
      {
      HEADING("ZT4 SOURCE REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixzt4 = 0; ixzt4 < nzt4; ixzt4++)
          {
          long sline = _fg->getZt4SourceLineNumber(ixzt4);
          long ixl   = _fg->findMatchingLineNumber(sline);
          if(ixl == -1) addMessage("ZT4", "source", ixzt4, sline);
          else
              {
              float sshot = _fg->getZt4FromSourceShotpoint(ixzt4);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, sshot);
              if(ixf == -1) addMessage("ZT4", "source", ixzt4, sshot, sline);
              sshot = _fg->getZt4ToSourceShotpoint(ixzt4);
              ixf   = _fg->findMatchingShotpointOnLine(ixl, sshot);
              if(ixf == -1) addMessage("ZT4", "source", ixzt4, sshot, sline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }




  if(nlines > 0 && nzt4 > 0)
      {
      HEADING("ZT4 RECEIVER REFERENCES NOT FOUND ON LD CARDS:")
      for(long ixzt4 = 0; ixzt4 < nzt4; ixzt4++)
          {
          long rline = _fg->getZt4ReceiverLineNumber(ixzt4);
          long ixl   = _fg->findMatchingLineNumber(rline);
          if(ixl == -1) addMessage("ZT4", "receiver", ixzt4, rline);
          else
              {
              float rshot = _fg->getZt4FromReceiverShotpoint(ixzt4);
              long  ixf   = _fg->findMatchingShotpointOnLine(ixl, rshot);
              if(ixf == -1) addMessage("ZT4", "receiver", ixzt4, rshot, rline);
              rshot = _fg->getZt4ToReceiverShotpoint(ixzt4);
              ixf   = _fg->findMatchingShotpointOnLine(ixl, rshot);
              if(ixf == -1) addMessage("ZT4", "receiver", ixzt4, rshot, rline);
              }
          ERROR_CHECK
          }
      MAYBE_BLANK
      }



  if(large_kount == 0)
      {
      addMessage("There appears to be no easily-detectable missing");
      addMessage("information in this JD.");
      addMessage("To verify this, you can create source gathers and");
      addMessage("notice whether there are any unplaced sources,");
      addMessage("or you can create receiver gathers (or CMP gathers)");
      addMessage("and notice whether there are any unplaced traces.");
      }
  else
      {
      addMessage("END OF LISTING");
      }
  small_kount = 0;
  large_kount = 0;
}




//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
