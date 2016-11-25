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
//***             Menu to apply a constant to a mute file              ***
//***             Author:Michael L. Sherrill 01/2002                   ***
//************************************************************************



#ifndef _TP_MUTE_SHIFT_POP_HH_
#define _TP_MUTE_SHIFT_POP_HH_

#include "sl/sl_form_pop.hh"
#include "sl/sl_file_pair_plus.hh"

class TpMuteShiftPair : public SLFilePairPlus
{
  public:
    TpMuteShiftPair( SLDelay *parent,
                     char *name,
                     HelpCtx hctx);
    ~TpMuteShiftPair();
     virtual void doValidate(const char *filename1,
                             const char *filename2,
                             long *valid1, long *valid2,
                             char *info1, char *info2,
                             long *same_datasets);

    long inputIsValid(){ return _input_valid;}
    long outputIsValid(){ return _output_valid;}

  protected:
    long _input_valid;
    long _output_valid;

  private:
    struct _MuteStruct *_ss;

};


class TpMuteShiftPop : public SLFPopSep
{
  public:
    TpMuteShiftPop( Widget               p,
                    char                 *name,
                    HelpCtx              hctx);
    virtual ~TpMuteShiftPop();
    virtual Widget make(Widget p);
    virtual void DoAction();
    virtual void okButton();
    virtual void applyButton();
    virtual int validate();

  private:       
    TpMuteShiftPair        *_mutefile_pair;
    class SLTextBox        *_shift_box;
    float                  _shift_value;
    class MutefileUtil     *_mutefile_util;
};
#endif

