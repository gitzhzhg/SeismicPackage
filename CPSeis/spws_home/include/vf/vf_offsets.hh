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

//-------------------------- vf_offsets.hh -------------------------//
//-------------------------- vf_offsets.hh -------------------------//
//-------------------------- vf_offsets.hh -------------------------//

//               header file for the VfOffsets class
//                    not derived from any class
//                          subdirectory vf

           // this class contains the offsets used for
           // raytracing, plus other general information
           // about the offset range for raytracing.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_OFFSETS_HH_
#define _VF_OFFSETS_HH_


class VfOffsets
{

//------------------------------ data ---------------------------------//
//------------------------------ data ---------------------------------//
//------------------------------ data ---------------------------------//

public:

  enum {
       MUTEFLAG_NONE  = 1,   // do not mute at all when raytracing.
       MUTEFLAG_MATCH = 2,   // mute at maximum offset = depth.
       MUTEFLAG_ARRAY = 3    // use offset/time mutes after NMO correction.
       };

private:

  int   _muteflag;  // how to mute when raytracing (enum).
  float _offmin;    // minimum offset for ray tracing.
  float _offmax;    // maximum offset for ray tracing.

  class SimpleFloatArray *_omute;  // mute offsets (monotonically increasing).
  class SimpleFloatArray *_tmute;  // mute times   (monotonically increasing).

  int   _error;   // FALSE if the offsets and mute times are valid.
                  // TRUE if offsets or times are nil or not increasing.
                  // a nil value is given by FNIL in named_constants.h.

//---------------------------- functions -------------------------------//
//---------------------------- functions -------------------------------//
//---------------------------- functions -------------------------------//

public:   // constructor and destructor.

           VfOffsets ();
  virtual ~VfOffsets ();

public:   // get values.

  int          getMuteFlag                   ()  const  { return _muteflag; }
  float        getMinimumOffset              ()  const  { return _offmin; }
  float        getMaximumOffset              ()  const  { return _offmax; }
  long         numMuteTimes                  ()  const;
  float        getMuteOffset       (long imute)  const;
  float        getMuteTime         (long imute)  const;
  const float *getMuteOffsetPointer          ()  const;
  const float *getMuteTimePointer            ()  const;
  int          getErrorFlag                  ()  const  { return _error; }

public:   // set values.

  void         setMuteFlag                     (int muteflag);
  void         setMinimumOffset                (float offmin);
  void         setMaximumOffset                (float offmax);
  void         setMuteOffset       (long imute, float offset);
  void         setMuteTime         (long imute, float time);

public:   // insert and remove mute offset/time pairs.
          // previously-returned pointers will no longer be valid.
          // appendMutePair and insertMutePair insert nil values into
          //   the arrays, and should be followed by resetting the
          //   nil values.

  void         appendMutePair           ();
  void         insertMutePair           (long imute);
  void         insertMutePairFromBuffer (long imute);
  void         removeMutePair           (long imute);
  void         removeMutePairToBuffer   (long imute);

public:   // convert offset array between feet and meters.

  void         convertOffsetFeetToMeters ();
  void         convertOffsetMetersToFeet ();

private:

  void         privateValidate ();
  void         privateMultiply (float constant);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
