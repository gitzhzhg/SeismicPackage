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

//----------------------- local_value.hh -------------------------//
//----------------------- local_value.hh -------------------------//
//----------------------- local_value.hh -------------------------//

//             header file for the LocalValue class
//                  not derived from any class
//                      subdirectory oprim


         // this class stores a value (locally) which might
         // be any one of several types.


#ifndef _LOCAL_VALUE_HH_
#define _LOCAL_VALUE_HH_


class LocalValue
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  enum  { _IVAR = -1, _FVAR = -2, _DVAR = -3 };

  int _type;  // type of variable (enum), or length of char variable (>=0).

  union
    {
    int     ivar;     // _type == _IVAR.
    float   fvar;     // _type == _FVAR.
    double  dvar;     // _type == _DVAR.
    char    cvar[8];  // _type >=   0  in-place  (length <  8).
    char   *avar;     // _type >=   8  allocated (length >= 8).
    } _var;


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  LocalValue ();

  virtual ~LocalValue();

public:    // get values.

  int         getIvar          ()  const;
  float       getFvar          ()  const;
  double      getDvar          ()  const;
  const char *getCvar          ()  const;

  int         isIvar           ()  const  { return (_type == _IVAR); }
  int         isFvar           ()  const  { return (_type == _FVAR); }
  int         isDvar           ()  const  { return (_type == _DVAR); }
  int         isCvar           ()  const  { return (_type >=   0  ); }

  void  getValue (int *ivar, float *fvar, double *dvar, char *cvar)  const;

public:    // set values.

  void        setIvar          (int         value =   0);
  void        setFvar          (float       value = 0.0);
  void        setDvar          (double      value = 0.0);
  void        setCvar          (const char *value =  "");

private:

  void        privateFree();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


