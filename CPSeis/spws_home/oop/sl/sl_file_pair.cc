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

//---------------------- sl_file_pair.cc -----------------------//
//---------------------- sl_file_pair.cc -----------------------//
//---------------------- sl_file_pair.cc -----------------------//

//          implementation file for the SLFilePair class
//                 derived from the SLDelay class
//                        subdirectory sl


#include "wproc.h"
#include "sl/sl_file_pair.hh"
#include "sl/slp_filepair.hh"
#include <string.h>
#include "tfdefs.h"
#include "cprim.h"
#include "inquire.h"

//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//


SLFilePair::SLFilePair(  SLDelay           *slparent,
                         char              *name,
                         const char * const filetype,
                         const char * const extension,
                         FilePairTrap      *user_trap,
                         void              *user_data,
                         char              *filename1,
                         char              *filename2,
                         const Boolean      required1,
                         const Boolean      required2,
                         HelpCtx            hctx,
                         Boolean            doframe,
                         Boolean            make_if_can  )
            : SLDelay(slparent, name, hctx, doframe),
                 _filetype1   (newstr((char*)filetype)),
                 _filetype2   (newstr((char*)filetype)),
                 _extension1  (newstr((char*)extension)),
                 _extension2  (newstr((char*)extension)),
                 _suffix1     (newstr("")),
                 _suffix2     (newstr("")),
                 _user_trap   (user_trap),
                 _user_data   (user_data),
                 _required1   (required1),
                 _required2   (required2)
{
  safe_strncpy(_filename1, filename1, NCHAR);
  safe_strncpy(_filename2, filename2, NCHAR);
  safe_strncpy(_basename , ""       , NCHAR);

  if     (slparent->made() && make_if_can) make();
  else if(slparent->topWidget()) supportUnmadeDefaults(slparent->topWidget());
  else if(slparent->pW       ()) supportUnmadeDefaults(slparent->pW());
}


SLFilePair::SLFilePair(  Widget             wparent,
                         char              *name,
                         const char * const filetype,
                         const char * const extension,
                         FilePairTrap      *user_trap,
                         void              *user_data,
                         char              *filename1,
                         char              *filename2,
                         const Boolean      required1,
                         const Boolean      required2,
                         HelpCtx            hctx,
                         Boolean            doframe,
                         Boolean            make_now  )
            : SLDelay(wparent, name, hctx, doframe),
                 _filetype1   (newstr((char*)filetype)),
                 _filetype2   (newstr((char*)filetype)),
                 _extension1  (newstr((char*)extension)),
                 _extension2  (newstr((char*)extension)),
                 _suffix1     (newstr("")),
                 _suffix2     (newstr("")),
                 _user_trap   (user_trap),
                 _user_data   (user_data),
                 _required1   (required1),
                 _required2   (required2)
{
  safe_strncpy(_filename1, filename1, NCHAR);
  safe_strncpy(_filename2, filename2, NCHAR);
  safe_strncpy(_basename , ""       , NCHAR);

  if(make_now) make();
  else supportUnmadeDefaults(wparent);
}


SLFilePair::SLFilePair(  SLDelay           *slparent,
                         char              *name,
                         const char * const filetype1,
                         const char * const filetype2,
                         const char * const extension1,
                         const char * const extension2,
                         FilePairTrap      *user_trap,
                         void              *user_data,
                         char              *filename1,
                         char              *filename2,
                         const Boolean      required1,
                         const Boolean      required2,
                         const char * const suffix1,
                         const char * const suffix2,
                         HelpCtx            hctx,
                         Boolean            doframe,
                         Boolean            make_if_can  )
            : SLDelay(slparent, name, hctx, doframe),
                 _filetype1   (newstr((char*)filetype1)),
                 _filetype2   (newstr((char*)filetype2)),
                 _extension1  (newstr((char*)extension1)),
                 _extension2  (newstr((char*)extension2)),
                 _suffix1     (newstr((char*)suffix1)),
                 _suffix2     (newstr((char*)suffix2)),
                 _user_trap   (user_trap),
                 _user_data   (user_data),
                 _required1   (required1),
                 _required2   (required2)
{
  safe_strncpy(_filename1, filename1, NCHAR);
  safe_strncpy(_filename2, filename2, NCHAR);
  safe_strncpy(_basename , ""       , NCHAR);

  if     (slparent->made() && make_if_can) make();
  else if(slparent->topWidget()) supportUnmadeDefaults(slparent->topWidget());
  else if(slparent->pW       ()) supportUnmadeDefaults(slparent->pW());
}


SLFilePair::~SLFilePair()
{
  free((char*)_filetype1);
  free((char*)_filetype2);
  free((char*)_extension1);
  free((char*)_extension2);
  free((char*)_suffix1);
  free((char*)_suffix2);
}



//----------------------- add before extension ---------------------//
//----------------------- add before extension ---------------------//
//----------------------- add before extension ---------------------//

        // if suffix is not blank, it is added to the end of
        // filename AFTER any existing extension is first removed.

static void add_before_extension(char *filename, const char *suffix)
{
  if(strlen(suffix) == 0) return;
  int len = strlen(filename);
  int last = -1;
  for(int i = 0; i < len; i++)
      {
      if(filename[i] == '.') last = i;
      if(filename[i] == '/') last = -1;
      }
  if(last != -1) filename[last] = '\0';
  strcat(filename, suffix);
}



//---------------------- update file pair --------------------//
//---------------------- update file pair --------------------//
//---------------------- update file pair --------------------//

long SLFilePair::updateFilePair(char *filename1, char *filename2,
                                char *msg)
{
  safe_strcpy(_filename1, filename1);
  safe_strcpy(_filename2, filename2);
  if(!topWidget())
       {
       safe_strcpy(msg, "filepair not yet made");
       return FILE_ERROR;
       }
  long status = update_slp_filepair(topWidget(), msg);
  safe_strcpy(filename1, _filename1);
  safe_strcpy(filename2, _filename2);
  return status;
}


long SLFilePair::updateFilePair(char *msg)
{
  if(!topWidget())
       {
       safe_strcpy(msg, "filepair not yet made");
       return FILE_ERROR;
       }
  long status = update_slp_filepair(topWidget(), msg);
  return status;
}


long SLFilePair::updateFilePairFromBaseName(char *basename, char *msg)
{
  char filename1[NCHAR+30];
  char filename2[NCHAR+30];
  int istat1, istat2;
  safe_strncpy(_basename , basename, NCHAR);
  safe_strncpy( filename1, basename, NCHAR);
  safe_strncpy( filename2, basename, NCHAR);
  add_before_extension(filename1, _suffix1);
  add_before_extension(filename2, _suffix2);
  addext_rep_(filename1, (char*)_extension1, &istat1);
  addext_rep_(filename2, (char*)_extension2, &istat2);
  if(istat1 != 0) safe_strcpy(filename1, "");
  if(istat2 != 0) safe_strcpy(filename2, "");
  return updateFilePair(filename1, filename2, msg);
}
  

long SLFilePair::updateFilePairIfNewBaseName(char *basename, char *msg)
{
  if(safe_strcmp(_basename, basename))
       {
       return updateFilePairFromBaseName(basename, msg);
       }
  return updateFilePair(msg);
}



//------------------- static trap ----------------------------//
//------------------- static trap ----------------------------//
//------------------- static trap ----------------------------//

void SLFilePair::staticTrap(void *data,
                            long *valid1, long *valid2,
                            char *info1, char *info2,
                            long *same_datasets)
{
  SLFilePair *obj = (SLFilePair*)data;
  if(obj->_user_trap) obj->_user_trap(obj->_user_data,
                              obj->_filename1, obj->_filename2,
                              valid1, valid2,
                              info1, info2,
                              same_datasets);
}


//--------------------- change message ------------------------//
//--------------------- change message ------------------------//
//--------------------- change message ------------------------//

void SLFilePair::changeMessage(int which, char *msg)
{
  if(topWidget()) change_slp_filepair_message(topWidget(), which, msg);
}



//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//

static String defres[]= {
/*
    "*pattern:                        *.xxx",
    ".width:                            500",
*/
    NULL,
    NULL,
    "*filepair_choice1.width:           500",
    "*filepair_choice2.width:           500",
    "*filepair_choice1.text.marginHeight: 1",
    "*filepair_choice2.text.marginHeight: 1",
    "*filepair_message1.fontList:  8x13bold",
    "*filepair_message2.fontList:  8x13bold",
    "*filepair_message3.fontList:  8x13bold",
    NULL };


Widget SLFilePair::make(Widget p)
{
  if(!made())
       {
       Widget w = SLDelay::make(p);
       if(!w)
           {
/*
             ////// this does not work on all platforms:
           sprintf(defres[0], "*pattern: *.%s", _extension1);
*/
           char firstRes1[80], firstRes2[80], fullname[200];
           get_full_name(wParent(), fullname);
           strcat(fullname, ".");
           strcat(fullname, (char*)instanceName());
           sprintf(firstRes1, "*filepair_choice1.pattern: *.%s", _extension1);
           sprintf(firstRes2, "*filepair_choice2.pattern: *.%s", _extension2);
           defres[0] = firstRes1;
           defres[1] = firstRes2;
           setDefaultResources(XtDisplay(wParent()), fullname, defres);
           w = make_nonmatching_slp_filepair (makeFrameIfNeeded(wParent()),
                 _name, (char*)_filetype1, (char*)_filetype2,
                 (char*)_extension1, (char*)_extension2, (char*)_suffix2,
                 getHelpCtx(),
    //           (void(*)(   ))staticTrap, this,   // replaced 2/1/95
                 staticTrap, this,
                 _filename1, _filename2, _required1, _required2);
           setTopWidget(w);
           }
       }
  makeChildren();
  return topWidget();
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
