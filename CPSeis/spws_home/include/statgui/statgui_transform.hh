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

//---------------------- statgui_transform.hh ---------------------------//
//---------------------- statgui_transform.hh ---------------------------//
//---------------------- statgui_transform.hh ---------------------------//

//             header file for the StatguiTransform class
//                 derived from the SLSmartForm class
//                        subdirectory statgui



#ifndef _STATGUI_TRANSFORM_HH_
#define _STATGUI_TRANSFORM_HH_

#include "sl/sl_smart_form.hh"


class StatguiTransform : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class StaticManager *_manager;
/*
  class StaticDataset *_proposed;  // pointer to dataset which contains
                                   //   proposed parameters.
*/

  float _x1old;
  float _x2old;
  float _y1old;
  float _y2old;
  float _x1new;
  float _x2new;
  float _y1new;
  float _y2new;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  StatguiTransform(SLDelay *slparent, class StaticManager *manager);
/*
  StatguiTransform(SLDelay *slparent, class StaticManager *manager,
                                      class StaticDataset *proposed);
*/

  virtual ~StatguiTransform();

  class StaticManager *manager       ()  const  { return _manager; }
/*
  class StaticDataset *proposed      ()  const  { return _proposed; }

  void updateProposedDataset();
  void takeProposedAction();
*/
  void takeAction();

public:  // get and set values.

  float getX1old ()  const  { return _x1old; }
  float getX2old ()  const  { return _x2old; }
  float getY1old ()  const  { return _y1old; }
  float getY2old ()  const  { return _y2old; }
  float getX1new ()  const  { return _x1new; }
  float getX2new ()  const  { return _x2new; }
  float getY1new ()  const  { return _y1new; }
  float getY2new ()  const  { return _y2new; }

  void  setX1old (float value);
  void  setX2old (float value);
  void  setY1old (float value);
  void  setY2old (float value);
  void  setX1new (float value);
  void  setX2new (float value);
  void  setY1new (float value);
  void  setY2new (float value);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
