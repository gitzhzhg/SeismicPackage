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


//-------------------------- backup_base.cc ----------------------------//
//-------------------------- backup_base.cc ----------------------------//
//-------------------------- backup_base.cc ----------------------------//

//            implementation file for the VfArray class
//                    not derived from any class
//                        subdirectory oprim


#include "oprim/backup_base.hh"
#include "cprim.h"
#include "inquire.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


#define SUFFIX  "_backup_"


//---------------------------- constructor -----------------------------//
//---------------------------- constructor -----------------------------//
//---------------------------- constructor -----------------------------//


BackupBase::BackupBase(const char *prefix, const char *extension)
           :
          _prefix                     (newstr(prefix)),
          _extension                  (newstr(extension)),
          _basename                   (newstr(prefix)),
          _backupfile                 (newstr("")),
          _last_read_description      (newstr("none")),
          _last_saved_description     (newstr("none")),
          _last_backup_description    (newstr("none"))
{
  assert(prefix && extension);
  assert(strlen(prefix)    > 0);
  assert(strlen(extension) > 0);
}



//----------------------------- destructor --------------------------//
//----------------------------- destructor --------------------------//
//----------------------------- destructor --------------------------//

BackupBase::~BackupBase()
{
  free((char*)_prefix);
  free((char*)_extension);
  free(_basename);
  free(_backupfile);
  free(_last_read_description);
  free(_last_saved_description);
  free(_last_backup_description);
}



//----------------------- save backup file --------------------------//
//----------------------- save backup file --------------------------//
//----------------------- save backup file --------------------------//

        // public.

int BackupBase::saveBackupFile(char *info)
{
  if(strlen(_backupfile) == 0) nameBackupFile();
  if(strlen(_backupfile) == 0)
      {
      if(info) strcpy(info, "ERROR - no names available to save backup file");
      free(_last_backup_description);
      _last_backup_description =
                      newstr("(no names available to save backup file)");
      return TRUE;
      }
  int error = virtualSaveBackupFile(_backupfile);
  if(error)
      {
      if(info) strcpy(info, "ERRORS saving backup file ");
      if(info) strcat(info, _backupfile);
      free(_last_backup_description);
      _last_backup_description = newstrcat("(errors) ", _backupfile, NULL);
      free(_backupfile);
      _backupfile = newstr("");
      }
  else
      {
      if(info) strcpy(info, "saved backup file ");
      if(info) strcat(info, _backupfile);
      free(_last_backup_description);
      _last_backup_description = newstr(_backupfile);
      }
  return error;
}



//------------------------ delete backup file ---------------------------//
//------------------------ delete backup file ---------------------------//
//------------------------ delete backup file ---------------------------//

     // private.
     // future saves will continue to use the same _basename
     //   unless this function is followed by calling provideNewBasename.

void BackupBase::deleteBackupFile(char *info)
{
  if(strlen(_backupfile) > 0)
      {
      remove(_backupfile);
      if(info) strcpy(info, "deleted backup file ");
      if(info) strcat(info, _backupfile);
      free(_backupfile);
      _backupfile = newstr("");
      free(_last_backup_description);
      _last_backup_description = newstr("none");
      }
  else
      {
      if(info) strcpy(info, "no backup file to delete");
      }
}



//--------------------- get proper filename -----------------------//
//--------------------- get proper filename -----------------------//
//--------------------- get proper filename -----------------------//

    // public static function.
    // get filename without directory path.
    // returns pointer to first character of proper filename.

const char *BackupBase::getProperFilename(const char *filename)
{
  assert(filename);
  int len = strlen(filename);
  int first = 0;
  for(int i = 0; i < len; i++)
      {
      if(filename[i] == '/' || filename[i] == ']') first = i + 1;
      }
  if(first >= len) first = 0;
  return &filename[first];
}



//------------------------ remove extension ------------------------//
//------------------------ remove extension ------------------------//
//------------------------ remove extension ------------------------//

    // public static function.
    // removes extension (by replacing '.' with '\0') if one is found.
    // otherwise does nothing.

void BackupBase::removeExtension(char *filename)
{
  assert(filename);
  int len = strlen(filename);
  for(int i = len - 1; i >= 0; i--)
      {
      if(filename[i] == '/' || filename[i] == ']') return;
      if(filename[i] == '.')
          {
          filename[i] = '\0';
          return;
          }
      }
}



//------------------------ replace extension ------------------------//
//------------------------ replace extension ------------------------//
//------------------------ replace extension ------------------------//

    // public static function.
    // first removes extension (by replacing '.' with '\0') if one is found.
    // then adds new extension.
    // the specified extension may or may not begin with a dot.
    // does nothing if the filename is empty.
    // removes any existing extension, but does not add a new one,
    //   if the extension is empty.

void BackupBase::replaceExtension(char *filename, const char *extension)
{
  assert(filename && extension);
  if(strlen(filename)         == 0) return;
  if(strcmp(filename, "NONE") == 0) return;
  BackupBase::removeExtension(filename);
  if(strlen(extension) == 0) return;
  if(extension[0] != '.') strcat(filename, ".");
  strcat(filename, extension);
}



//----------------- get history and title string --------------------//
//----------------- get history and title string --------------------//
//----------------- get history and title string --------------------//

    // public static functions.
    // return an appropriate history record and title bar string.

void BackupBase::getHistoryString (const char *word, const char *filename,
                                   int error, char *history)
{
  assert(word && filename && history);
  strcpy(history, "file ");
  strcat(history, word);
  if(error) strcat(history, " (errors): ");
  else      strcat(history,          ": ");
  strcat(history, filename);
}


void BackupBase::getTitleString (const char *word, const char *filename,
                                 int error, char *title)
{
  assert(word && filename && title);
  const char *proper = getProperFilename(filename);
  strcpy(title, "last file ");
  strcat(title, word);
  if(error) strcat(title, " (errors): ");
  else      strcat(title,          ": ");
  strcat(title, proper);
}



//---------------------- after reading file -------------------------//
//---------------------- after reading file -------------------------//
//---------------------- after reading file -------------------------//

    // public.

void BackupBase::afterReadingFile(const char *filename, int error,
                                  int replaced_all, char *info,
                                  char *history, char *title)
{
  assert(filename);
  const char *proper = getProperFilename(filename);
  free(_last_read_description);
  if(error) _last_read_description = newstrcat("(errors) ", proper, NULL);
  else      _last_read_description = newstr(proper);

  if(!error && replaced_all)
      {
      if(strlen(_backupfile) > 0)
          {
          if(info) strcpy(info, "backup file ");
          if(info) strcat(info, _backupfile);
          if(info) strcat(info, " will not be saved to again");
          }
      else
          {
          if(info) strcpy(info, "no backup file in use");
          }
      provideNewBasename(filename);
      }
  else
      {
      if(strlen(_backupfile) > 0)
          {
          if(info) strcpy(info, "backup file ");
          if(info) strcat(info, _backupfile);
          if(info) strcat(info, " retained for further saving");
          }
      else
          {
          if(info) strcpy(info, "no backup file in use");
          }
      }

  if(history) getHistoryString("read", filename, error, history);
  if(title)   getTitleString  ("read", filename, error, title);
}



//---------------------- after saving file -------------------------//
//---------------------- after saving file -------------------------//
//---------------------- after saving file -------------------------//

    // public.

void BackupBase::afterSavingFile(const char *filename, int error,
                                 int saved_all, char *info,
                                 char *history, char *title)
{
  assert(filename);
  const char *proper = getProperFilename(filename);
  free(_last_saved_description);
  if(error) _last_saved_description = newstrcat("(errors) ", proper, NULL);
  else      _last_saved_description = newstr(proper);

  if(!error && saved_all)
      {
      deleteBackupFile(info);
      provideNewBasename(filename);
      }
  else
      {
      if(strlen(_backupfile) > 0)
          {
          if(info) strcpy(info, "backup file ");
          if(info) strcat(info, _backupfile);
          if(info) strcat(info, " retained for further saving");
          }
      else
          {
          if(info) strcpy(info, "no backup file in use");
          }
      }

  if(history) getHistoryString("saved", filename, error, history);
  if(title)   getTitleString  ("saved", filename, error, title);
}



//---------------------- provide new basename ---------------------------//
//---------------------- provide new basename ---------------------------//
//---------------------- provide new basename ---------------------------//

    // private.
    // the name of the new backup file will be a non-existing file with
    //   a name created from this basename and _extension.

    // if you want to delete any backup file currently in use, you must
    //   call deleteBackupFile BEFORE calling this function.

    // the specified basename can be the name of a file which was
    //   recently read or saved, or any other name.
    // the directory path and extension in basename, if present,
    //   are ignored.
    // if the basename is not useable, _prefix is used instead.

void BackupBase::provideNewBasename(const char *filename)
{
  assert(filename);
  const char *proper = getProperFilename(filename);
  free(_basename);
  _basename = newstr(proper);
  removeExtension(_basename);
  int len       = strlen(_basename);
  int lensuffix = strlen(SUFFIX);
  if(len > lensuffix)
      {
      if(strncmp(&_basename[len - 1 - lensuffix], SUFFIX, lensuffix) == 0)
                  _basename[len - 1 - lensuffix] = '\0';
      }
  if(len <= 1)
      {
      free(_basename);
      _basename = newstr(_prefix);
      }
  if(_backupfile) free(_backupfile);
  _backupfile = newstr("");
}



//----------------------- name backup file --------------------------//
//----------------------- name backup file --------------------------//
//----------------------- name backup file --------------------------//

    // private.
    // creates a name based on _basename.
    // _backupfile must be empty string upon entry.
    // if a filename can be found which does not exist but is writeable:
    //     resets _backupfile to that name.
    // otherwise leaves _backupfile empty.

void BackupBase::nameBackupFile()
{
  assert(_backupfile);
  assert(_basename);
  assert(strlen(_backupfile) == 0);
  assert(strlen(_basename)    > 0);

  free(_backupfile);
  _backupfile = newstrcat(_basename, SUFFIX, "a.", _extension, NULL);
  int index = strlen(_basename) + strlen(SUFFIX);

  int ichar;
  for(ichar = 'a'; ichar <= 'z'; ichar++)
      {
      _backupfile[index] = ichar;
      long status = inquire_file(_backupfile, NULL);
      if(status == FILE_NOT_FOUND) return;
      }

  for(ichar = 'A'; ichar <= 'Z'; ichar++)
      {
      _backupfile[index] = ichar;
      long status = inquire_file(_backupfile, NULL);
      if(status == FILE_NOT_FOUND) return;
      }

  for(ichar = '0'; ichar <= '9'; ichar++)
      {
      _backupfile[index] = ichar;
      long status = inquire_file(_backupfile, NULL);
      if(status == FILE_NOT_FOUND) return;
      }

  free(_backupfile);
  _backupfile = newstr("");
}



//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//

