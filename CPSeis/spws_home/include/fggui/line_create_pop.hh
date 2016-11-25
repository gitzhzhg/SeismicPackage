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

//------------------------ line_create_pop.hh ----------------------------//
//------------------------ line_create_pop.hh ----------------------------//
//------------------------ line_create_pop.hh ----------------------------//

//               header file for the LineCreatePop class
//                  derived from the SLDialog class
//                         subdirectory fggui

         // This dialog box is used to create a grid of seismic
         // lines and flags.


#ifndef _LINE_CREATE_POP_HH_
#define _LINE_CREATE_POP_HH_

#include "sl/sl_dialog.hh"
#include <X11/Intrinsic.h>


class LineCreatePop : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FieldGeometry *_fg;
  long                 _first_line;
  float                _first_shot;
  double               _first_xloc;
  double               _first_yloc;
  long                 _line_increment;
  float                _shot_increment;
  double               _xloc_increment;
  double               _yloc_increment;
  long                 _num_lines;
  long                 _num_flags;
  double               _angle;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  LineCreatePop(SLDelay *slparent, char *name, HelpCtx hctx,
                                       class FieldGeometry *fg);
  virtual ~LineCreatePop();

  FieldGeometry  *getFieldGeometry  ()    const  { return _fg; }
  void            createSeismicLines();

  long      getFirstLine    ()  const  { return _first_line; }
  float     getFirstShot    ()  const  { return _first_shot; }
  double    getFirstXloc    ()  const  { return _first_xloc; }
  double    getFirstYloc    ()  const  { return _first_yloc; }
  long      getLineIncrement()  const  { return _line_increment; }
  float     getShotIncrement()  const  { return _shot_increment; }
  double    getXlocIncrement()  const  { return _xloc_increment; }
  double    getYlocIncrement()  const  { return _yloc_increment; }
  long      getNumLines     ()  const  { return _num_lines; }
  long      getNumFlags     ()  const  { return _num_flags; }
  double    getAngle        ()  const  { return _angle; }

  void      setFirstLine    (long   value)  { _first_line = value; }
  void      setFirstShot    (float  value)  { _first_shot = value; }
  void      setFirstXloc    (double value)  { _first_xloc = value; }
  void      setFirstYloc    (double value)  { _first_yloc = value; }
  void      setLineIncrement(long   value);
  void      setShotIncrement(float  value);
  void      setXlocIncrement(double value);
  void      setYlocIncrement(double value);
  void      setNumLines     (long   value);
  void      setNumFlags     (long   value);
  void      setAngle        (double value)  { _angle = value; }

  
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
