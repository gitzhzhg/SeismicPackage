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

//------------------------ vf_disk_file.hh ----------------------------//
//------------------------ vf_disk_file.hh ----------------------------//
//------------------------ vf_disk_file.hh ----------------------------//

//               header file for the VfDiskFile class
//                 derived from the FileBase class
//                         subdirectory vf


//    This class controls the reading, writing, and validating of velocity
//      disk files of various types.
//    This class accesses the VfManager and VfDataset classes.
//    The VfDataset class does the actual reading, writing, and validating,
//      by using a helper class in VhelpUtilities.

//    Filetype, extension, and format must all match as follows:
//
//              filetype           extension           format
//          ------------------     ---------       --------------
//         "CPS velocity file"      "vel"          VfDataset::CPS
//         "velocity workfile"      "work"         VfDataset::WORK


#ifndef _VF_DISK_FILE_HH_
#define _VF_DISK_FILE_HH_

#include "oprim/file_base.hh"


class VfDiskFile  :  public FileBase
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

     // also int io = ioIndex() from FileBase base class.

private:

  int              _format;     // format of velocity file on disk.
  class VfManager *_manager;

//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:

           VfDiskFile (const char *filetype, const char *extension,
                                          int format, VfManager *manager);
  virtual ~VfDiskFile ();

  VfManager *getVfManager  ()  const  { return _manager; }

protected:   // overriding virtual functions.

  virtual Prepare  virtualPrepareRead (const char *filename, char *errmsg);
//virtual Prepare  virtualPrepareSave (const char *filename, char *errmsg);
  virtual Result   virtualRead        (const char *filename, char *errmsg);
  virtual Result   virtualSave        (const char *filename, char *errmsg);
  virtual Validity virtualValidate    (const char *filename, char *info);
  virtual void     preNewFilename     ();
  virtual void     postNewFilename    ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
