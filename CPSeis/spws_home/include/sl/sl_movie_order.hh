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

//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************



//************************************************************************
//***    Menu to control the order a movie scale will follow           ***
//***    Author:Michael L. Sherrill 05/2001                            ***
//************************************************************************



#ifndef _SL_MOVIE_ORDER_HH_
#define _SL_MOVIE_ORDER_HH_

#include "sl/sl_dialog.hh"
#include "sl/sl_databox.hh"
#include "sl/sl_arrow_scale.hh"


class SLMovieOrder : public SLDialog
{
  public :
    SLMovieOrder( Widget              w,
                  char                *name,
                  HelpCtx             hctx,
                  SLArrowScale        *scale,
                  Boolean             use_filenames);
    virtual ~SLMovieOrder();
    virtual long getNumFrames();
    virtual char *getFilename(long i); 
    virtual void clearSelections();
    virtual long frameSequenceNumber(int i);
    virtual Boolean notify(SLPrim *gui);
    virtual int getNextFrame(int current_frame);
    virtual int getPreviousFrame(int current_frame);
    virtual int isMovieOrderValue(int val);
    virtual void setUseMovieOrder(Boolean use);


  protected:


  private:
    class SLMovieTable *_table; 
    SLArrowScale       *_arrow_scale;
    SLpPush            *_clear_button;
    SLpPush            *_ok_button;
    SLpPush            *_apply_button;
    SLpPush            *_cancel_button;
    SLpPush            *_help_button;

};


class SLMovieTable  :  public SLDatabox
{

  public:
    SLMovieTable( SLDelay      *slparent,
                  char         *name,
                  SLMovieOrder *movie_order,
                  Boolean      use_filenames);
    virtual  ~SLMovieTable();
    int      _frames_set[SLArrowScale::MAX_FRAMES];
    virtual  void clearSelections();
    virtual  long getNumFrames();
    virtual  int getNumSequencedFrames();
    virtual  char *getFilename(long i);
    virtual  long frameSequenceNumber(int i);
    virtual  void setUseMovieOrder(Boolean use);
    int      _sequence;

  private:
    void         makeHelper();
    Boolean      _use_filenames;
    SLMovieOrder *_movie_order;
};

#endif
