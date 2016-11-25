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
//************************ COPYRIGHT NOTICE ******************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        ***
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         ***
//************************ COPYRIGHT NOTICE ******************************



//************************************************************************
//***             Base class for deriving reference overlay files.     ***
//***             Author:Michael L. Sherrill 01/2002                   ***
//************************************************************************


#ifndef _TP_REFERENCE_BASE_HH_
#define _TP_REFERENCE_BASE_HH_

#include "sl/sl_file_choice.hh"
#include "oprim/file_base.hh"


class TpReferenceBase : public SLFileChoice
{
  public:          // constructor and destructor

    TpReferenceBase( SLDelay    *slparent,
               char       *name,
               int         io,
               FileBase   *file,
               const char *label,
               HelpCtx     hctx);
  
    virtual ~TpReferenceBase();

    // virtual functions to override

    virtual void doReadCurrentPicks  (float *picks,
                   const float *head, long nwords, long n) = 0;

    virtual void doReadPreviousPicks (float *picks,
                   const float *head, long nwords, long n) = 0;

    virtual void doReadNextPicks     (float *picks,
                   const float *head, long nwords, long n) = 0;

    virtual int fileIsLoaded()
                   {return (getFileBase()->lastInputFilenameValidated() &&
                            _is_mute_file);}


  protected:
    long    _is_mute_file;

  private:

} ;

#endif

