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

//-------------------- sl_file_pair_plus.hh ---------------------------//
//-------------------- sl_file_pair_plus.hh ---------------------------//
//-------------------- sl_file_pair_plus.hh ---------------------------//

//               header file for the SLFilePairPlus class
//                 derived from the SLSmartForm class
//                         subdirectory sl

#ifndef _SL_FILE_PAIR_PLUS_HH_
#define _SL_FILE_PAIR_PLUS_HH_

#include "sl/sl_smart_form.hh"


typedef void FppWorkingMessage (void *data, char *msg);

typedef void FppValidateTrap (void              *data,
                              const char        *filename1,
                              const char        *filename2,
                              long              *valid1,
                              long              *valid2,
                              char              *info1,
                              char              *info2,
                              long              *same_datasets);

typedef int  FppOpenTrap     (void              *data,
                              long               status,
                              const char        *filename1,
                              const char        *filename2,
                              Boolean            required1,
                              Boolean            required2,
                              FppWorkingMessage *working_message_trap,
                              void              *working_message_data,
                              char              *msg);

typedef void FppCloseTrap    (void              *data);


class SLFilePairPlus : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class SLFilePair *_pair;
  class SLpText    *_text;
  Boolean           _loaded;    // whether a file is currently loaded.
  Boolean           _read_only; // whether the loaded file is read-only.
  FppValidateTrap  *_validate_trap;
  void             *_validate_data;
  FppOpenTrap      *_open_trap;
  void             *_open_data;
  FppCloseTrap     *_close_trap;
  void             *_close_data;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  SLFilePairPlus (SLDelay *slparent, char *name,
                  HelpCtx hctx, Boolean doframe,
                  const char * const filetype,
                  const char * const extension,
                  const Boolean required1,
                  const Boolean required2);

  SLFilePairPlus (SLDelay *slparent, char *name,
                  HelpCtx hctx, Boolean doframe,
                  const char * const filetype1,
                  const char * const filetype2,
                  const char * const extension1,
                  const char * const extension2,
                  const Boolean required1,
                  const Boolean required2,
                  const char * const suffix1 = "",
                  const char * const suffix2 = "");

  virtual ~SLFilePairPlus();

  void        maybeUpdate     (char *basename);
  long        openFile        ();
  void        closeFile       ();

  Boolean     fileIsLoaded    ()  const  { return _loaded; }
  Boolean     fileIsReadOnly  ()  const  { return _read_only; }
  const char *workingFilename ()  const;
  const char *inputFilename   ()  const;
  const char *outputFilename  ()  const;

  void  registerValidateTrap (FppValidateTrap *trap, void *data);
  void  registerOpenTrap     (FppOpenTrap     *trap, void *data);
  void  registerCloseTrap    (FppCloseTrap    *trap, void *data);

protected:   // virtual functions to override

  virtual void doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets);

  virtual int doOpen (long status,
                      const char *filename1,
                      const char *filename2,
                      Boolean required1, Boolean required2,
                      FppWorkingMessage *working_message_trap,
                      void              *working_message_data,
                      char *msg);

  virtual void doClose ();

private:

  static void privateValidateTrap (void *data,
                                   char *filename1, char *filename2,
                                   long *valid1, long *valid2,
                                   char *info1, char *info2,
                                   long *same_datasets);

  void constructorHelper (const char * const filetype1,
                          const char * const filetype2,
                          const char * const extension1,
                          const char * const extension2,
                          const Boolean required1,
                          const Boolean required2,
                          const char * const suffix1,
                          const char * const suffix2);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
