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
//A change
//---------------------- tred_file_pair.cc -----------------------//
//---------------------- tred_file_pair.cc -----------------------//
//---------------------- tred_file_pair.cc -----------------------//

//           implementation file for the TredFilePair class
//               derived from the SLFilePairPlus class
//                       subdirectory tred


#include <string.h>
#include <iostream.h>
#include <assert.h>
#include "pick/tred_table.hh"
#include "pick/tred_table_pop.hh"
#include "pick/tred_file_pair.hh"
#include "sl/sl_error_pop.hh"
#include "cprim.h"
#include "inquire.h"

//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//

static const char * const FILETYPE  = "TRED file";
static const char * const EXTENSION = "tred";

static const Boolean      REQUIRED1 = FALSE;
static const Boolean      REQUIRED2 = FALSE;

static char info[72];

//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//

TredFilePair::TredFilePair (SLDelay *slparent, class TredTable *tredtable,
                            class TredTablePop *tredtablepop)
            : SLFilePairPlus(slparent, "TredFilePair", NULL, TRUE,
                     FILETYPE, EXTENSION, REQUIRED1, REQUIRED2),
            _slparent     (slparent),
            _tredtable    (tredtable),
            _tredtablepop (tredtablepop)
{
}

//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//

TredFilePair::~TredFilePair()
{
}

//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//

            // overrides SLFilePairPlus
            // called from private SLFilePairPlus::validateTrap
            //       which is called from SLFilePair
            //       which is called from make_filepair

void TredFilePair::doValidate (const char *filename1,
                               const char *filename2,
                               long *valid1, long *valid2,
                               char *info1, char *info2,
                               long * /* same_datasets */)
{
//cout << "am in TredFilePair::doValidate" << endl;
  if (_tredtable)
    { 
      if (!tredfile_get (filename2, _tredtable->getTredFile(), info2))
        *valid2 = INQUIRE_VALID_YES;
      else
        *valid2 = INQUIRE_VALID_NO;

      if (!tredfile_get (filename1, _tredtable->getTredFile(), info1))
        *valid1 = INQUIRE_VALID_YES;
      else
        *valid1 = INQUIRE_VALID_NO;
      // Assume you want to start from
      // scratch
      if (*valid1 == INQUIRE_VALID_NO && *valid2 == INQUIRE_VALID_NO) 
	{
          _tredtable->fileReset ();
          _tredtablepop->filePut ();
	}
    } 
  else
    {
      *valid1 = INQUIRE_VALID_NO;
      sprintf (info1, "Error...TredTable object is NULL");
      *valid2 = INQUIRE_VALID_NO;
      sprintf (info2, "Error...TredTable object is NULL");
    }
}

//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::openFiles()

int TredFilePair::doOpen (long status,
                          const char *filename1,
                          const char *filename2,
                          Boolean /*required1*/, Boolean /*required2*/,
                          FppWorkingMessage * /*working_message_trap*/,
                          void              * /*working_message_data*/,
                          char *msg)
{
  int flag;

//cout << "am in TredFilePair::doOpen" << endl;
  if (!_tredtable)
    {
      sprintf (msg, "Error...TredTable object is NULL");
      return 1;
    }

  _tredtable->sensitizeTable ();

  if (status == FILE_READ_ONLY || status == FILE_UPDATE)
    {
      if (flag = tredfile_get (filename1, _tredtable->getTredFile(), msg) != 0)
        return flag;
//    _tredtable->post (); // this should now get done in the function
                        // TredTablePop::updateVectors (SeisPlot *sp, Why why);
      return flag;
    }

  else if (status == FILE_COPY)
    {
      if (flag = tredfile_get (filename1, _tredtable->getTredFile(), msg) != 0)
        return flag;
//    _tredtable->post (); // this should now get done in the function
                        // TredTablePop::updateVectors (SeisPlot *sp, Why why);
      return tredfile_put (filename2, _tredtable->getTredFile(), msg);
    }

  else if (status == FILE_CREATE)
    {
      if (flag = _tredtable->fileClear () != 0) return flag;
//    _tredtable->post (); // this should now get done in the function
                        // TredTablePop::updateVectors (SeisPlot *sp, Why why);
      return flag;
    }
  return 1;  // added by Tom Stoeckley 12/22/94 to keep the Sun happy.
}

//----------------------------- do close --------------------------//
//----------------------------- do close --------------------------//
//----------------------------- do close --------------------------//

                  // overrides SLFilePairPlus
                  // called from public SLFilePairPlus::closeFiles()

void TredFilePair::doClose ()
{
  _tredtable->fileReset ();
//_tredtable->post ();  // this should now get done in the function
                        // TredTablePop::updateVectors (SeisPlot *sp, Why why);
  _tredtable->desensitizeTable ();
}

void TredFilePair::goUpdateFile ()
{
  if (fileIsReadOnly()) return;
  if (tredfile_put (workingFilename(), _tredtable->getTredFile(), info))
    _errpop = new SLErrorPop (_slparent, "Error", (const char *)info);
}

int TredFilePair::filePut ()
{
  if (fileIsReadOnly()) return 0;
  int retval = tredfile_put (workingFilename(), _tredtable->getTredFile(),
    NULL);
  return retval;
}

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
