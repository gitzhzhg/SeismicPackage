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

//------------------------ midpoints_table_gui.hh ---------------------//
//------------------------ midpoints_table_gui.hh ---------------------//
//------------------------ midpoints_table_gui.hh ---------------------//

//            header file for the FgMissTableGui class
//                derived from the SLDatabox class
//               derived from the SmartArray class
//                        subdirectory fggui

 
#ifndef _FG_MISS_TABLE_GUI_HH_
#define _FG_MISS_TABLE_GUI_HH_

#include "sl/sl_databox.hh"
#include "oprim/smart_array.hh"


class FgMissTableGui  :  public SLDatabox, public SmartArray
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FieldGeometry *_fg;
  class FgMissPop     *_pop;

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  FgMissTableGui (SLDelay *slparent, char *name,
                    class FieldGeometry *fg, class FgMissPop *pop);
  virtual ~FgMissTableGui();

  FieldGeometry  *getFieldGeometry ()  const  { return _fg; }
  FgMissPop      *getFgMissPop     ()  const  { return _pop; }

  void            updateMissingInfo();
  void            deleteMissingInfo();

  long            numMessages()  const  { return numElements(); }
  char           *messageElement(long index)  const
                              { return (char*)arrayElement(index); }

private:

  void makeHelper();
  void addMessage(const char *message);
  void addMessage(const char *type, const char *word,
                                long index, long line);
  void addMessage(const char *type, const char *word,
                                long index, float shot, long line);
  void addMessage(const char *type,
                                long index, const char *word, long group);

private:    // virtual functions overriding SmartArray.

  virtual void *doCreateObject  ();
  virtual void  doDeleteObject  (void *object);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
