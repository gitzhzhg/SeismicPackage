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

//------------------------ vf_diskfile.hh -------------------------------//
//------------------------ vf_diskfile.hh -------------------------------//
//------------------------ vf_diskfile.hh -------------------------------//

//               header file for the VfDiskfile class
//               derived from the VelioWrapper class
//                         subdirectory vf

            // read and write velocity files.
            // copies data to and from VfKernal.
            // separate instances should be used for input and output.



//------------------------ start of coding -----------------------------//
//------------------------ start of coding -----------------------------//
//------------------------ start of coding -----------------------------//


#ifndef _VF_DISKFILE_HH_
#define _VF_DISKFILE_HH_

#include "vf/velio_wrapper.hh"


class VfDiskfile : public VelioWrapper
{

//----------------------------- data ----------------------------------//
//----------------------------- data ----------------------------------//
//----------------------------- data ----------------------------------//

private:


//--------------------------- functions ---------------------------------//
//--------------------------- functions ---------------------------------//
//--------------------------- functions ---------------------------------//

public:  // io must be 0 for input and 1 for output.

           VfDiskfile (int io, const char *defname = "velocity");
  virtual ~VfDiskfile ();

public:  // call this when the active kernal changes, or when something
         //  changes in the kernal which might affect the pickle jar
         //  parameters.
         // this is also called from saveVelocityFile().

  void updatePjarFromKernal (const class VfKernal *kernal,
                             const class HistoryCards *history,
                             int skiphist);


//----------------------- read or write file -----------------------------//
//----------------------- read or write file -----------------------------//
//----------------------- read or write file -----------------------------//

public:   // these must return msg, and error TRUE or FALSE.
          // working is a message like "validating velocity function" or NULL.
          // filename, msg, kernal, and history must never be NULL.

  int  prepareReadFile        (char *msg);
  int  prepareSaveFile        (char *msg);

  int  readVelocityFile       (const char *working,
                               class VfKernal *kernal,
                               class HistoryCards *history,
                               const char *filename, char *msg);

  int  saveVelocityFile       (const char *working,
                               int save_choice,
                               const class VfKernal *kernal,
                               const class HistoryCards *history,
                               const char *filename, char *msg,
                               int skiphist = 0);

   // readVelocityFile:
   //   (1) ADDs to data in kernal from the file.
   //   (2) kernal must already be cleared if original data are to be replaced.
   //   (3) does not need to check new/old compatibility when adding.
   //   (4) uses the current pickle jar contents from validating and subsequent
   //        optional GUI modifications.

   // saveVelocityFile:
   //   (1) saves data in kernal to the file.
   //   (2) save_choice must be an enum (in vf_constants.hh).
   //   (4) uses the current pickle jar contents.
   //   (5) skiphist can be set to true to avoid adding a new history card
   //        (e.g. for saving undo files).


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

} ;

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
