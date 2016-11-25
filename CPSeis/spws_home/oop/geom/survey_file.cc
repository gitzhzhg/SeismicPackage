
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
//---------------------- survey_file.cc ------------------------//
//---------------------- survey_file.cc ------------------------//
//---------------------- survey_file.cc ------------------------//

//          implementation file for the SurveyFile class
//                 derived from the FileBase class
//                 derived from the FgInform class
//                        subdirectory geom


#include "geom/survey_file.hh"
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"
#include "cprim.h"
#include "readcard.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>

static const int SMART_ALLOC = TRUE;
////static const int SMART_ALLOC = FALSE;


#define FILETYPE   "survey file"
#define EXTENSION  "dat"
#define NBUF       200
#define NCARDS      10
#define NCHARS1    101     // includes space for null termination.


//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//




//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


SurveyFile::SurveyFile(FieldGeometry *fg)
       : FileBase(FILETYPE, EXTENSION),
         FgInform(fg),
             _ncards                         (NCARDS),

             _add_option                     (ADD_ALL_NEW_LINES),
             _replace_option                 (SKIP_ALL_MATCHING_LINES),
             _line_to_add                    (INIL),
             _default_line                   (INIL),

             _sp_add                         (0.0),
             _x_add                          (0.0),
             _y_add                          (0.0),
             _sp_multiply                    (1.0),
             _x_multiply                     (1.0),
             _y_multiply                     (1.0),

             _save_option                    (SAVE_ALL_LINES)
{
  _file_contains_line_numbers[0] = FALSE;
  _file_contains_line_numbers[1] = FALSE;
  _cards[0]                      = NULL;
  _cards[1]                      = NULL;
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


SurveyFile::~SurveyFile()
{
  if(_cards[0]) delete [] _cards[0];
  if(_cards[1]) delete [] _cards[1];
}



//------------------------ get values -----------------------------//
//------------------------ get values -----------------------------//
//------------------------ get values -----------------------------//

        // public.

static const char * const blank = " ";

const char *SurveyFile::inputCard(int index)  const
{
  assert(index >= 0 && index < NCARDS);
  if(_cards[0] == NULL) return blank;
  return (_cards[0] + index * NCHARS1);
}

const char *SurveyFile::outputCard(int index)  const
{
  assert(index >= 0 && index < NCARDS);
  if(_cards[1] == NULL) return blank;
  return (_cards[1] + index * NCHARS1);
}



//--------------------- card message -----------------------------//
//--------------------- card message -----------------------------//
//--------------------- card message -----------------------------//

static void card_message(FieldGeometry *fg, char *prefix, long i)
{
  static char msg[80];
  if(i == 0 || i == 1000 * (i / 1000))
      {
      sprintf(msg, "%s survey card %d", prefix, i);
      fg->showMessage(msg);
      }
}



//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//

    // protected virtual function overriding FileBase.
    // sets _file_contains_line_numbers[io] to TRUE or FALSE.
    // sets _cards[io] to the contents of the first NCARDS on the file.
    // sets info to a string of information about the file.

FileBase::Validity
SurveyFile::virtualValidate(const char *filename, char *info)
{
  int io = ioIndex();
  _file_contains_line_numbers[io] = FALSE;
  if(_cards[io] == NULL) _cards[io] = new char [NCARDS * NCHARS1];
  memset(_cards[io], '\0', NCARDS * NCHARS1);
  if(strlen(filename) == 0)
      {
      return VALID_NO;
      }
  FILE *stream = fopen(filename, "r");
  if(stream == NULL)
      {
      return VALID_NO;
      }
  for(int i = 0; i < NCARDS; i++)
      {
      const char *card = readcard(stream);
      if(!card)
          {
          if(feof(stream) && i >= 1) break;     // EOF encountered.
          fclose(stream);
          return VALID_NO;
          }
      int len = strlen(card);
      if(len >= NCHARS1)
          {
          fclose(stream);
          return VALID_NO;
          }
      strcpy(_cards[io] + i * NCHARS1, card);
      float  shot;
      double xloc;
      double yloc;
      float  elev;
      long   line;
      int number = sscanf((char*)card, "%f %lf %lf %f %d",
                                 &shot, &xloc, &yloc, &elev, &line);
      if(i == 0 && number == 5)
          {
          _file_contains_line_numbers[io] = TRUE;
          }
      if(number != 4 && number != 5)
          {
          fclose(stream);
          return VALID_NO;
          }
      }
  fclose(stream);
  if(_file_contains_line_numbers[io])
       strcpy(info, "file contains line numbers");
  else strcpy(info, "file does not contain line numbers");
  return VALID_YES;
}



//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//

    // protected virtual function overriding FileBase.

void SurveyFile::preNewFilename()
{
  _fg->preSlowOperations();
}


void SurveyFile::postNewFilename()
{
  _fg->showMessage("survey file validation completed");
  _fg->postSlowOperations();
}



//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//

    // protected virtual function overriding FileBase.
    // this function does not put anything into FieldGeometry.

FileBase::Prepare
SurveyFile::virtualPrepareRead(const char* /*filename*/, char *errmsg)
{
  int io = ioIndex();
  if(_fg->getDataLock() > LOCK_DEL)
      {
      strcpy(errmsg,   "survey file cannot be input\nwhile data are locked");
      _fg->showMessage("survey file cannot be input while data are locked");
      _fg->ringBell();
      return PROHIBIT;
      }

  if(_fg->getDataLock() >= LOCK_DEL)
      {
      if(_replace_option != SKIP_ALL_MATCHING_LINES)
           {
           strcpy(errmsg, "Lines cannot be replaced\n");
           strcat(errmsg, "from the survey file\n");
           strcat(errmsg, "while data deletions are locked.");
           _fg->showMessage
  ("cannot replace lines from survey file while data deletions are locked");
           _fg->ringBell();
           return PROHIBIT;
           }
      }

  if(_fg->getChaining() != NO_CHAINING && _fg->totNumFlags() > 0)
      {
      strcpy(errmsg,   "Survey file cannot be input\n");
      strcat(errmsg,   "when there are existing LD cards\n");
      strcat(errmsg,   "unless the chaining parameter\n");
      strcat(errmsg,   "is set to NO CHAINING.");
      _fg->showMessage("survey file cannot be input except with NO CHAINING");
      _fg->ringBell();
      return PROHIBIT;
      }

  if(_add_option     == SKIP_ALL_NEW_LINES &&
     _replace_option == SKIP_ALL_MATCHING_LINES)
      {
      strcpy(errmsg, "You have not selected\nanything to read\n");
      strcat(errmsg, "from the survey file.\n----\nTry again!");
      _fg->showMessage("nothing selected to read from survey file");
      _fg->ringBell();
      return PROHIBIT;
      }

  if(!_file_contains_line_numbers[io] && _default_line == INIL)
      {
      strcpy(errmsg, "The survey file does not\n");
      strcat(errmsg, "contain line numbers.\n----\n");
      strcat(errmsg, "Therefore you must specify\n");
      strcat(errmsg, "a line number to use.\n");
      strcat(errmsg, "----\nTry again!");
      _fg->showMessage("no line number specified when not on survey file");
      _fg->ringBell();
      return PROHIBIT;
      }

  if(_add_option == ADD_SPECIFIED_NEW_LINE && _line_to_add == INIL)
      {
      strcpy(errmsg, "You have chosen to add\n");
      strcat(errmsg, "a specified line\n");
      strcat(errmsg, "from the survey file.\n----\n");
      strcat(errmsg, "Therefore you must specify\n");
      strcat(errmsg, "a line number to add.\n");
      strcat(errmsg, "----\nTry again!");
      _fg->showMessage("no line number specified to add from survey file");
      _fg->ringBell();
      return PROHIBIT;
      }

  if(_replace_option == REPLACE_ALL_MATCHING_LINES)
      {
      strcpy(errmsg, "All lines in your field geometry data\n");
      }
  if(_replace_option == REPLACE_SELECTED_MATCHING_LINES)
      {
      strcpy(errmsg, "Selected lines in your field geometry data\n");
      }
  if(_replace_option == REPLACE_ALL_MATCHING_LINES ||
     _replace_option == REPLACE_SELECTED_MATCHING_LINES)
      {
      strcat(errmsg, "which match line numbers in the survey file\n");
      strcat(errmsg, "will be replaced from the file\n");
      strcat(errmsg, "if you continue with this action.\n----\n");
      if(_fg->dataNeedsSaving())
          {
          strcat(errmsg, "You have data which\nhas NOT been saved.\n----\n");
          }
      strcat(errmsg, "Are you sure you want\nto replace this data\n");
      strcat(errmsg, "while reading this file?");
      return CAUTION;
      }

  return GODSPEED;
}



//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//

    // protected virtual function overriding FileBase.
    // currently not needed.




//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//

    // protected virtual function overriding FileBase.

FileBase::Result
SurveyFile::virtualRead(const char *filename, char *errmsg)
{
  _fg->preSlowOperations();
  /// int io = ioIndex();   // not needed here.
  FILE *stream = fopen(filename, "r");
  if(!stream)
      {
      strcpy(errmsg, "survey file cannot be opened for read");
      _fg->showMessage(errmsg);
      _fg->ringBell();
      _fg->postSlowOperations();
      return FAILURE;
      }
  strcpy(errmsg, "reading survey file...");
  _fg->showMessage(errmsg);
  if(_fg->getChaining() != NO_CHAINING)
      {
      _fg->resumeDependentUpdates();
      _fg->setChaining(NO_CHAINING);
      }
  _fg->preMajorChanges();
  int error = readSurveyCards (stream, errmsg);
  fclose(stream);
  _fg->postMajorChanges();
  if(error)
      {
      _fg->showMessage(errmsg);
      _fg->ringBell();
      _fg->postSlowOperations();
      return FAILURE;
      }
  _fg->showMessage(errmsg);
  char newmsg[200];
  strcpy(newmsg,   "survey file last read = ");
  strcat(newmsg,   filename);
  _fg->sendMessage(newmsg, 0, 0, 0, 0, 0);
  _fg->postSlowOperations();
  return SUCCESS;
}



//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//

    // protected virtual function overriding FileBase.

FileBase::Result
SurveyFile::virtualSave(const char *filename, char *errmsg)
{
  _fg->preSlowOperations();
  /// int io = ioIndex();  // not needed here.
  FILE *stream = fopen((char*)filename, "w");
  if(!stream)
      {
      strcpy(errmsg, "survey file cannot be opened for write");
      _fg->showMessage(errmsg);
      _fg->ringBell();
      _fg->postSlowOperations();
      return FAILURE;
      }
  strcpy(errmsg, "saving survey file...");
  _fg->showMessage(errmsg);
  int error = saveSurveyCards (stream, errmsg);
  fclose(stream);
  if(error)
      {
      _fg->showMessage(errmsg);
      _fg->ringBell();
      _fg->postSlowOperations();
      return FAILURE;
      }
  _fg->showMessage(errmsg);
  char newmsg[200];
  strcpy(newmsg,   "survey file last saved = ");
  strcat(newmsg,   filename);
  _fg->sendMessage(newmsg, 0, 0, 0, 0, 0);
  _fg->postSlowOperations();
  return SUCCESS;
}




//------------------------- formats ----------------------------------//
//------------------------- formats ----------------------------------//
//------------------------- formats ----------------------------------//


//  formats from FG.FOR on POGUN:

//  read  one line when line numbers exist on the file:
//                          sp   x     y    elev  line
//                 format (F9.0,F11.0,F13.0,F13.0,A30)

//  read  one line when line numbers are not there:
//                          sp   x     y    elev 
//                 free-field format with 4 numbers on each card.

//  write one line when line numbers exist on the file:
//                          sp   x     y    elev  line
//                 format (F9.3,F11.3,F13.3,F13.3,I10)




//------------------ read survey cards -------------------------//
//------------------ read survey cards -------------------------//
//------------------ read survey cards -------------------------//

         // private.
         // returns error = TRUE or FALSE.
         // sets errmsg if error.

int SurveyFile::readSurveyCards(FILE *stream, char *errmsg)
{
  int io = ioIndex();
  long line_keep = _default_line;
  long ixl       = -1;
  long smart_ixl = -1;
  long smart_add = 10000;
  long i = 0;
  int forever = TRUE;
  while(forever)
      {
      const char *card = readcard(stream);
      if(!card)
          {
          if(SMART_ALLOC && smart_ixl >= 0)
                          _fg->freeSpaceForLine(smart_ixl);
          if(feof(stream) && i >= 1)
            {
             sprintf(errmsg, "survey file (%d cards) successfully read", i);
             return FALSE;       // finished without error.
             }
          else
             {
             sprintf(errmsg, "error encountered on survey file card %d", i+1);
             return TRUE;       // an error occurred.
             }
          }
      i++;
      int len = strlen(card);
      if(len >= NCHARS1)
          {
          sprintf(errmsg, "survey card %d is too long", i);
          return TRUE;
          }
      card_message(_fg, "reading", i);
      float  shot;
      double xloc;
      double yloc;
      float  elev;
      long   line;
      int number = sscanf((char*)card, "%f %lf %lf %f %d",
                                 &shot, &xloc, &yloc, &elev, &line);
      if(number == 4)
          {
          if(i == 1 && _file_contains_line_numbers[io])
              {
              sprintf(errmsg, "missing line number on survey card %d", i);
              return TRUE;
              }
          line = line_keep;
          }
      else if(number == 5)
          {
          if(!_file_contains_line_numbers[io])
              {
              sprintf(errmsg, "unexpected line number on survey card %d", i);
              return TRUE;
              }
/////     line_keep = line;
          }
      else if(number == EOF)
          {
          sprintf(errmsg, "error encountered on survey card %d", i);
          return TRUE;
          }
      else if(_file_contains_line_numbers[io])
          {
          sprintf(errmsg,
            "%d numbers found on survey card %d (expected 4 or 5 numbers)",
             number, i);
          return TRUE;
          }
      else
          {
          sprintf(errmsg,
            "%d numbers found on survey card %d (expected 4 numbers)",
             number, i);
          return TRUE;
          }

      if(line == INIL)
          {
          sprintf(errmsg, "missing line number on survey card %d", i);
          return TRUE;
          }

      if(line != line_keep || i == 1)
          {
          line_keep = line;
          ixl = _fg->findMatchingLineNumber(line);
          if(ixl == -1)
              {
              if(_add_option == SKIP_ALL_NEW_LINES) continue;
              if(_add_option == ADD_SPECIFIED_NEW_LINE &&
                      line != _line_to_add) continue;
              }
          else
              {
              if(_replace_option == SKIP_ALL_MATCHING_LINES)
                   {
                   ixl = -1;
                   continue;
                   }
              if(_replace_option == REPLACE_SELECTED_MATCHING_LINES)
                   {
                   int selected = _fg->lineIsSelected(ixl);
                   if(!selected)
                       {
                       ixl = -1;
                       continue;
                       }
                   }
              _fg->deleteAllFlagsFromLine(ixl);
              }

          if(ixl == -1)
              {
              if(SMART_ALLOC && smart_ixl >= 0)
                  {
                  _fg->freeSpaceForLine(smart_ixl);
                  smart_add = _fg->numFlagsOnLine(smart_ixl);
                  }
              ixl = _fg->placeNewLine(line);
              if(ixl == -1)
                  {
                  sprintf(errmsg,
                      "error trying to append line from survey card %d", i);
                  return TRUE;
                  }
              if(SMART_ALLOC)
                  {
                  _fg->allocateSpaceForLine(ixl, smart_add);
                  smart_ixl = ixl;
                  }
              }
          }

      if(ixl == -1) continue;

      long ixf = _fg->appendNewFlagToLine(ixl);
      if(ixf == -1)
          {
          sprintf(errmsg,
              "error trying to append flag from survey card %d", i);
          return TRUE;
          }
      shot = _sp_multiply * shot + _sp_add;
      xloc =  _x_multiply * xloc +  _x_add;
      yloc =  _y_multiply * yloc +  _y_add;
      _fg->setShotpoint     (ixl, ixf, shot);
      _fg->setXloc          (ixl, ixf, xloc);
      _fg->setYloc          (ixl, ixf, yloc);
      _fg->setElevation     (ixl, ixf, elev);
      }
  return FALSE;   // statement never reached.
}



//--------------------- save survey cards --------------------------//
//--------------------- save survey cards --------------------------//
//--------------------- save survey cards --------------------------//

         // private.
         // returns error = TRUE or FALSE.
         // sets errmsg if error.

int SurveyFile::saveSurveyCards(FILE *stream, char *errmsg)
{
  long nlines = _fg->numLines();
  long  kount = 0;
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      long line   = _fg->getLineNumber (ixl);
      long nflags = _fg->numFlagsOnLine(ixl);
      if(_save_option == SAVE_SELECTED_LINES &&
                    !_fg->lineIsSelected(ixl)) continue;
      for(long ixf = 0; ixf < nflags; ixf++)
          {
          kount++;
          float  shot  = _fg->getShotpoint     (ixl, ixf);
          double xloc  = _fg->getXloc          (ixl, ixf);
          double yloc  = _fg->getYloc          (ixl, ixf);
          float  elev  = _fg->getElevation     (ixl, ixf);
          int num = fprintf(stream, "%9.3f %10.3f %12.3f %12.3f %9d\n",
                                   shot, xloc, yloc, elev, line);
          if(num < 0)
              {
              sprintf(errmsg,
                     "write error occurred\non survey card %d", kount);
              return TRUE;
              }
          card_message(_fg, "saving", kount);
          }
      }
  sprintf(errmsg, "survey file (%d cards) successfully saved", kount);
  return FALSE;
}




//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
