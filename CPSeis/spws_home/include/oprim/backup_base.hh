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

//--------------------------- backup_base.hh --------------------------//
//--------------------------- backup_base.hh --------------------------//
//--------------------------- backup_base.hh --------------------------//

//                header file for the UndoBase class
//                    not derived from any class
//                         subdirectory oprim


  // This class is a base class for maintaining a backup file
  // for a dataset.  Since this class needs to know when you read
  // and save files, and the filename you use, this class also
  // maintains information about these files (filename and description)
  // as a convenience for the user.

  // Here are the steps to take:
  //
  // (1) Every time you read or save a file, you must call
  //     afterReadingFile or afterSavingFile.
  //
  // (2) When you wish to save a backup file (after altering data),
  //     you must call saveBackupFile.
  //
  // (3) You must override the virtual function virtualSaveBackupFile
  //     to do the actual saving of the backup file.  This function is
  //     called from saveBackupFile.
  //
  // (4) Note: The backup file should be in a format you know how to
  //     read outside of this class.  There is no provision to read a
  //     backup file in this class.

  // When you save a backup file, the name of the backup file will
  // be created from the name of the last file read (if replaced_all
  // was TRUE) or saved (if saved_all was TRUE).  If no file was
  // previously read or saved, the prefix will be used.  The extension
  // is always used.  The backup file name will be a non-existing file
  // the first time it is saved with that name.  Subsequent saves
  // will overwrite the same file, until the name of the backup file
  // is changed.  The backup file is always saved in the local directory.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _BACKUP_BASE_HH_
#define _BACKUP_BASE_HH_


class BackupBase
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  const char *_prefix;      // prefix to use for backup file if necessary.
  const char *_extension;   // extension to use for backup file.
  char       *_basename;    // created from name of last file read or saved.
  char       *_backupfile;  // name of last backup file saved.

   // the following names do not include the directory path.
   // the following names are preceded by "(error) " if an error occurred.

  char *_last_read_description;     // description of last file read.
  char *_last_saved_description;    // description of last file saved.
  char *_last_backup_description;   // description of last backup file saved.


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:  // constructor and destructor.

           BackupBase (const char *prefix, const char *extension);
  virtual ~BackupBase ();

protected:   // virtual function to override.
             // filename is guaranteed not to be NULL or empty.
             // this must return error = TRUE or FALSE.

  virtual int virtualSaveBackupFile (const char *filename) = 0;

public:  // to call after reading a file.
         // filename = name of file which was read.
         // error = TRUE if a file read error occurred and FALSE otherwise.
         // asserts if filename is NULL.
         // resets _last_read_description.

    // Set replaced_all to TRUE if ALL previous data was replaced
    // by the data read from the file.
    // -----------------------------------------------------------
    // If replaced_all is TRUE and error is FALSE, subsequent
    // backup file saves will go to a filename based on the name of
    // this file just read.  Any previous backup files saved will
    // no longer be overwritten (and will not be deleted).

         // info (returned) will refer to the name of the previous backup file.
         // history and title (returned) will refer to the file just read.
         // history is appropriate to use as a history record.
         // title is appropriate to display on a title bar.

  void afterReadingFile (const char *filename, int error, int replaced_all,
               char *info = 0, char *history = 0, char *title = 0);

public:  // to call after saving a file.
         // filename = name of file which was saved.
         // error = TRUE if an error occurred and FALSE otherwise.
         // these assert if filename is NULL.
         // resets _last_saved_description.

    // Set saved_all to TRUE if ALL of the data was saved to the file.
    // -----------------------------------------------------------
    // If saved_all is TRUE and error is FALSE, any previously-saved
    // backup file will be deleted since it is now redundant.  Subsequent
    // backup file saves will go to a filename based on the name of this
    // file just saved.

         // info (returned) will refer to the name of the previous backup file.
         // history and title (returned) will refer to the file just read.
         // history is appropriate to use as a history record.
         // title is appropriate to display on a title bar.

  void afterSavingFile (const char *filename, int error, int saved_all,
              char *info = 0, char *history = 0, char *title = 0);

public:  // save backup file using a filename previously determined.
         // info (returned) will refer to backup filename saved.
         // returns error = TRUE or FALSE.
         // resets _last_backup_description.

  int  saveBackupFile    (char *info = 0);

public:    // learn name of last file read or saved.
           // the directory path is not included in the name.
           // "(errors) " preceding name if error occurred.

  const char *lastFileRead        () const { return _last_read_description; }
  const char *lastFileSaved       () const { return _last_saved_description; }
  const char *lastBackupFileSaved () const { return _last_backup_description; }

public:  // get filename without directory path.
         // asserts if filename is NULL.
         // returns pointer to first character of proper filename.
         // needed internally.
         // made available to the public for convenience.

  static const char *getProperFilename (const char *filename);

public:  // removes extension (by replacing '.' with '\0') if one is found.
         // otherwise does nothing.
         // asserts if filename is NULL.
         // needed internally.
         // made available to the public for convenience.

  static void  removeExtension  (char *filename);
  static void  replaceExtension (char *filename, const char *extension);

public:  // word should be "read" or "saved".
         // filename should be the file just read or saved.
         // error should be TRUE if a read or save error occurred.
         // history and title will refer to the file just read or saved.
         // history is appropriate to use as a history record.
         // title is appropriate to display on a title bar.
         // needed internally.
         // made available to the public for convenience.

  static void getHistoryString (const char *word, const char *filename,
                                int error, char *history);

  static void getTitleString   (const char *word, const char *filename,
                                int error, char *title);

private:

  void  deleteBackupFile   (char *info);
  void  provideNewBasename (const char *filename);
  void  nameBackupFile     ();


//---------------------- end of functions ----------------------------//
//---------------------- end of functions ----------------------------//
//---------------------- end of functions ----------------------------//

} ;

#endif

//---------------------------- end -------------------------------------//
//---------------------------- end -------------------------------------//
//---------------------------- end -------------------------------------//
