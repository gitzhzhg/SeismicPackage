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

#include "sl/sl_smart_form.hh"
#include "sl/sl_movie_order.hh"
#include "sl/sl_arrow_scale.hh"
#include "sl/sl_tog_box.hh"
#include "sl/slp_push.hh"
#include <stdio.h>
#include <Xm/Label.h>



enum { SELECTED = 1, SEQUENCE, FILENAMES};








SLMovieOrder::SLMovieOrder( Widget              w,
                            char                *name,
                            HelpCtx             hctx,
                            SLArrowScale        *scale,
                            Boolean             use_filenames)
                : SLDialog(w, name, hctx, FALSE),
                _arrow_scale(scale)

{

  SLSmartForm *work   = workArea();
  _table = new SLMovieTable(work, name, this, use_filenames);

  _clear_button  = new SLpPush (work, "Start Over" );
  _clear_button->setNotify(this);

  work->attach (_clear_button, work, NULL, work, NULL, 10, 10);
  work->attach(_table, work, work, _clear_button, work,  0,   0,  10,  10);
  
  _ok_button     = addBottomOK(1, NULL, this);
  _apply_button  = addBottomApply(2, NULL, this);
  _cancel_button = addBottomCancel(3, NULL, this);
  addBottomHelp();

  setTitle("Movie Order");
}


SLMovieOrder::~SLMovieOrder()
{
  delete _table;
}


Boolean SLMovieOrder::notify (SLPrim *gui)
{
long frame_sequence_number;
Boolean use_order = False;


  SLDialog::notify(gui);

  if(gui == (SLPrim *)_ok_button)
    {//If any are selected set the SLArrowScale class to use the order selected
    for(int i = 0; i < SLArrowScale::MAX_FRAMES; i++)
      {
      if( (frame_sequence_number = _table->frameSequenceNumber(i)) )
        {
        _arrow_scale->setUseMovieOrder(True);
        return True;
        }
      }
    _arrow_scale->setUseMovieOrder(False);
    }

  else if(gui == (SLPrim *)_apply_button)
    {//If any are selected set the SLArrowScale class to use the order selected
    for(int i = 0; i < SLArrowScale::MAX_FRAMES; i++)
      {
      if( (frame_sequence_number = _table->frameSequenceNumber(i)) )
        {
        _arrow_scale->setUseMovieOrder(True);
        return True;
        }
      }
    _arrow_scale->setUseMovieOrder(False);
    }

  else if(gui == (SLPrim *)_cancel_button)
    {//Turn off the order selected
    _arrow_scale->setUseMovieOrder(False);
    }

  else if(gui == _clear_button)
    {
    clearSelections();
    }

  return True;

}


void SLMovieOrder::clearSelections()
{
  _table->clearSelections();
  setUseMovieOrder(False);
}


long SLMovieOrder::getNumFrames()
{
  return _arrow_scale->getNumFrames();
}


char *SLMovieOrder::getFilename(long i)
{
  return _arrow_scale->getFilename((int)i);
}


long SLMovieOrder::frameSequenceNumber(int i)
{
  return _table->frameSequenceNumber(i);
}


int SLMovieOrder::getNextFrame(int current_frame)
{
int current_frame_sequence = _table->_frames_set[current_frame - 1];
int desired_sequence;

  if(current_frame_sequence >= _table->getNumSequencedFrames()) 
    desired_sequence = 1;
  else if(current_frame_sequence == 0) // force to be valid
    desired_sequence = 1;
  else
    desired_sequence = current_frame_sequence + 1;

  for(int i = 0; i < getNumFrames(); i++)
     if(_table->_frames_set[i] == desired_sequence)
        return i + 1;

  assert(0);
}


int SLMovieOrder::getPreviousFrame(int current_frame)
{
int current_frame_sequence = _table->_frames_set[current_frame - 1];
int desired_sequence;

  if(current_frame_sequence == 1) 
    desired_sequence = _table->getNumSequencedFrames();
  else if(current_frame_sequence == 0) // force to be valid
    desired_sequence = 1;
  else
    desired_sequence = current_frame_sequence - 1;

  for(int i = 0; i < getNumFrames(); i++)
     if(_table->_frames_set[i] == desired_sequence)
        return i + 1;

  assert(0);

}


int SLMovieOrder::isMovieOrderValue(int val)
{
int found = 0;

  for(int i = 0; i < getNumFrames(); i++)
     if(_table->_frames_set[i] == val)
        return 1;

  return found;
}


void SLMovieOrder::setUseMovieOrder(Boolean use)
{
  _arrow_scale->setUseMovieOrder(use);
}






//=============================================================================
//====  The SLDataBox that handles user's frame selection                   ===
//=============================================================================
SLMovieTable::SLMovieTable( SLDelay             *slparent,
                            char                *name,
                            SLMovieOrder        *movie_order,
                            Boolean             use_filenames)
                : SLDatabox(slparent,name, NULL, 4),
                _use_filenames(use_filenames), _movie_order(movie_order),
                _sequence(0)
{
  for(int i = 0; i < SLArrowScale::MAX_FRAMES; i++)
    _frames_set[i] = 0;
}


SLMovieTable::~SLMovieTable()
{

}


void SLMovieTable::clearSelections()
{
  for(int i = 0; i < SLArrowScale::MAX_FRAMES; i++)
    _frames_set[i] = 0;
  _sequence = 0;
}


long SLMovieTable::frameSequenceNumber(int i)
{
  return _frames_set[i];
}

long SLMovieTable::getNumFrames()
{
  return _movie_order->getNumFrames();
}

int SLMovieTable::getNumSequencedFrames()
{
  return _sequence;
}

char *SLMovieTable::getFilename(long i)
{
  return _movie_order->getFilename(i);
}

void SLMovieTable::setUseMovieOrder(Boolean use)
{
  _movie_order->setUseMovieOrder(use);
}


//=============================================================================
//=== Set the number of rows equal to the number of movie frames            ===
//=============================================================================
static long num_frames_update(void *data)
{
SLMovieTable *mt = (SLMovieTable *)data;
static int last_num_frames = 0;
long frames = mt->getNumFrames();

  if(frames != last_num_frames)
    {
      //may need to do something here
    }

  last_num_frames = frames;

  assert(frames < SLArrowScale::MAX_FRAMES);

  return frames;
}



//=============================================================================
//=== Handle the user selection of frames to toggle between                 ===
//=============================================================================
static void select_trap(void *data, long ident, long index,
			long value, long nread, char* endkey)
{
SLMovieTable *mt = (SLMovieTable *)data;

  if(nread == 0) return;

  if(value)
    mt->_frames_set[index] = -1;

  //The user has changed a value so make sure it is not set on the 
  //SLArrowScale until the user hits apply or ok
  mt->setUseMovieOrder(False);
}

static long select_update(void *data, long ident, long index)
{
SLMovieTable *mt = (SLMovieTable *)data;


 if(mt->_frames_set[index] == -1)//Set by select_trap
    {
    ++mt->_sequence;
    mt->_frames_set[index] = mt->_sequence;
    }

  return mt->_frames_set[index];
  
}



//=============================================================================
//=== Post the sequence number of a frame the user has selected             ===
//=============================================================================
static void sequence_trap(void *data, long ident, long index,
			  long value, long nread, char* endkey)
{
SLMovieTable *mt = (SLMovieTable *)data;

  if(nread == 0) return;
  mt->_frames_set[index] = value;
}

static long sequence_update(void *data, long ident, long index)
{
SLMovieTable *mt = (SLMovieTable *)data;

  return mt->_frames_set[index];
  
}


//=============================================================================
//=== Post the file names of frames for the multiple file movie class       ===
//=============================================================================
static void name_trap(void *data, long /*ident*/, long index,
		      char* value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  printf("name_trap called \n");
}

static char *name_update(void *data, long /*ident*/, long index)
{
SLMovieTable *mt = (SLMovieTable *)data;
char filename[255];

  strcpy(filename, mt->getFilename(index));
  return filename; 
}


void SLMovieTable::makeHelper()
{
static long zero  =   0; 
static long one   =   1; 
static long two   =   2; 
static long three =   3; 
static long four  =   4; 
static long five  =   5; 
static long m1    =  -1; 
static long m66   = -66;


  //         N                  NMAX              ROW COL NCHAR MAXROWS
  regArrays(num_frames_update, num_frames_update, 3,  0,   6,    45);

  	//   ID        PROMPT              JSW      ISW     COL NCHAR
  regIarray (SELECTED,  "Frame"        , &zero  , &three ,   0,   2);
  regIarray (SEQUENCE,  "Sequence"     , &zero  , &five  ,   0,   3);
  if(_use_filenames)
    {
    regCarray(FILENAMES,"File name"    , &zero  , &five  ,   0,   48);
    funCvar  (FILENAMES, name_trap,       name_update);
    }

  funIvar  (SELECTED,     select_trap,   select_update   );
  funIvar  (SEQUENCE,     sequence_trap, sequence_update );
}
