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

//--------------------------- undo_base.hh --------------------------//
//--------------------------- undo_base.hh --------------------------//
//--------------------------- undo_base.hh --------------------------//

//                header file for the UndoBase class
//                    not derived from any class
//                         subdirectory oprim


  // This class is a base class for maintaining an "undo file"
  // for a dataset.  The undo file can reside on disk or in central
  // memory.


  // Here are the steps to take:
  //
  // (1) When you wish to save an undo file (before altering data),
  // you must call saveUndoFile and identify yourself by providing
  // a pointer to yourself.  If the save is unsuccessful, the file is
  // deleted and error = TRUE is returned.
  //
  // (2) Then, if and when you want to read the file (to restore the data
  // to its earlier state), you must call maybeReadUndoFile and identify
  // yourself again.  If you are not the same person who saved the file,
  // or the file does not exist, it will not be read and error = TRUE
  // is returned.  Otherwise, whether or not the read is successful,
  // the file is deleted, and error = TRUE or FALSE is returned.
  //
  // (3) If at any time you want to delete the undo file (for example, when
  // the dialog box which is controlling the data change is popped down),
  // you can call maybeDeleteUndoFile.  If you are not the same person
  // who saved the file, it will not be deleted.
  //
  // (4) To find out at any time whether you are allowed to read or delete
  // the undo file (for example, to decide whether to make an Undo button
  // sensitive), you can call allowReadDeleteUndoFile.  If you are not
  // the same person who saved the file, or if the file does not exist,
  // you cannot read or delete it, and this function will return FALSE.


  // The undo file, if it exists, is deleted by the destructor.
  // If the program aborts, or is terminated by ^C, the undo file
  // may continue to exist.  To assist in identifying this file,
  // the identifier (supplied to the constructor) and the user name
  // are appended to the filename.


  // To derive from this class:
  //
  // Simply override the two virtual functions which will actually do
  // the reading and saving of the file.  Also optionally override
  // notifyRemovingUndoFile if you wish to be informed when the undo
  // file is about to be deleted.  (This function is not called from the
  // destructor, which always removes the undo file.)


  // If the undo file is to reside on disk:
  //
  // The name of the file is a temporary name returned by tmpnam, with
  // an identifier (supplied to the constructor) and the user name
  // appended.  The undo file is deleted at various times determined
  // by this class (see above), and also (if it exists) by the destructor.
  // If the program aborts, or is terminated by ^C, the undo file
  // may continue to exist.  The identifier and user name appended to
  // the filename will assist the user in finding and deleting these
  // unwanted files.  On many systems they will reside on /usr/tmp.


  // If the undo file is to reside in central memory:
  //
  // If the identifier is NULL, a disk file will not be used, and
  // _filename will be NULL.  The virtual function notifyRemovingUndoFile
  // will still be called just before this class would otherwise delete
  // the undo file.  The derived class should use this function to clear
  // its memory-resident file.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _UNDO_BASE_HH_
#define _UNDO_BASE_HH_


class UndoBase
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  char   *_filename;  // name of temporary undo file (NULL if memory-resident).
  void   *_doer;      // pointer to object which saved the undo file.

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:   // constructor and destructor.
          // identifier should be NULL if undo file will be memory-resident.

           UndoBase (const char *identifier);
  virtual ~UndoBase ();

public:   // interact with undo file.
          // doer must match for maybe... functions.
          // msg (returned) must be dimensioned at least [80].
          // saveUndoFile and maybeReadUndoFile return error = TRUE/FALSE.
          // allowReadDeleteUndoFile           returns allow = TRUE/FALSE.

  int   saveUndoFile            (void *doer, char *msg);
  int   allowReadDeleteUndoFile (void *doer)                const;
  int   maybeReadUndoFile       (void *doer, char *msg);
  void  maybeDeleteUndoFile     (void *doer);

protected:   // virtual functions which must be overridden.
             // these must return error = TRUE or FALSE.
             // filename is NULL if undo file is memory-resident.

  virtual int virtualSaveUndoFile (const char *filename) = 0;
  virtual int virtualReadUndoFile (const char *filename) = 0;

protected:   // virtual function to optionally override.
             // should be overridden for memory-resident undo files.
             // filename is NULL if undo file is memory-resident.

  virtual void notifyRemovingUndoFile (const char* /*filename*/)  {}

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
