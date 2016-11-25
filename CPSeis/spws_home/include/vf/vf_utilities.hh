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

//-------------------------- vf_utilities.hh -------------------------//
//-------------------------- vf_utilities.hh -------------------------//
//-------------------------- vf_utilities.hh -------------------------//

//               header file for the VfUtilities class
//                    not derived from any class
//                          subdirectory vf

       // This class is a repository for several utilities
       // needed by various velocity file objects.  This class
       // owns (i.e. creates and deletes) these utilities, but
       // otherwise does not attempt to know much about them.

       // This class also contains a few parameters of general use.
       // This class also sets the value of FNIL when created.
       // Only one instance of this class should normally exist
       // in an application.

       // The objects in this class contain parameters needed when
       // various actions are performed.  Most of these objects also
       // contain algorithms for performing these actions.  It is
       // intended that user interface classes will set and display
       // the parameters, and that VfDataset will call the functions
       // which perform the actions (unless the actions do not
       // affect the data in VfDataset, in which case anyone can
       // call these functions).


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_UTILITIES_HH_
#define _VF_UTILITIES_HH_


class VfUtilities
{

//------------------------------ data ---------------------------------//
//------------------------------ data ---------------------------------//
//------------------------------ data ---------------------------------//

private:

  class VfOffsets  *_offsets;
  class VfUpdate   *_update;

  float   _timetol;    // tolerance   (seconds)    for matching times.
  float   _depthtol;   // tolerance (ft or meters) for matching depths.
  float   _mm;         // picking tolerance in millimeters.

  float   _xcenter;    // any X-bin center.
  float   _ycenter;    // any Y-bin center.
  float   _xwidth;     // X-bin width.
  float   _ywidth;     // Y-bin width.

//---------------------------- functions -------------------------------//
//---------------------------- functions -------------------------------//
//---------------------------- functions -------------------------------//

public:   // constructor and destructor.
          // see vf_update.hh for a description of the bogus_velocities arg.

           VfUtilities (int bogus_velocities = 0);
  virtual ~VfUtilities ();

public:   // get values.

  class VfOffsets  *offsets  ()  const  { return _offsets; }
  class VfUpdate   *update   ()  const  { return _update; }

  float getTimeTolerance     ()  const  { return _timetol; }
  float getDepthTolerance    ()  const  { return _depthtol; }
  float getPickingTolerance  ()  const  { return _mm; }

  float getXcenter           ()  const  { return _xcenter; }
  float getYcenter           ()  const  { return _ycenter; }
  float getXwidth            ()  const  { return _xwidth; }
  float getYwidth            ()  const  { return _ywidth; }

public:   // set values.

  void  setTimeTolerance           (float timetol);
  void  setDepthTolerance          (float depthtol);
  void  setPickingTolerance        (float mm);

public:  // the following are to be called only by VfManager.
         // to set these values, you must call similar functions in VfManager.

  void  setXcenterByVfManagerOnly  (float xcenter);
  void  setYcenterByVfManagerOnly  (float ycenter);
  void  setXwidthByVfManagerOnly   (float xwidth);
  void  setYwidthByVfManagerOnly   (float ywidth);

public:   // get center of bin containing specified coordinate.
          // or get bin number.

  float xbinCenter (float xloc)  const;   // uses _xcenter and _xwidth.
  float ybinCenter (float yloc)  const;   // uses _ycenter and _ywidth.
  long  xbinNumber (float xloc)  const;   // uses _xcenter and _xwidth.
  long  ybinNumber (float yloc)  const;   // uses _ycenter and _ywidth.

public:   // get velocity type information.

  static const char *typeSymbol        (int type);
  static const char *typeDescription   (int type);
  static int         getTypeFromSymbol (const char *symbol);

  static int abscissaIsTime      (int type);  // returns TRUE or FALSE.
  static int abscissaIsDepth     (int type);  // returns TRUE or FALSE.
  static int abscissaIsThickness (int type);  // returns TRUE or FALSE.
  static int ordinateIsVelocity  (int type);  // returns TRUE or FALSE.
  static int ordinateIsDepth     (int type);  // returns TRUE or FALSE.
  static int ordinateIsVNMO      (int type);  // returns TRUE or FALSE.
  static int ordinateIsVRMS      (int type);  // returns TRUE or FALSE.
  static int ordinateIsVAV       (int type);  // returns TRUE or FALSE.
  static int ordinateIsVINT      (int type);  // returns TRUE or FALSE.

public:   // get units information.

  static const char *getSymbolFromUnits (int units);
  static int         getUnitsFromSymbol (const char *symbol);

public:   // derive moveout information.

  static int   deriveMoveoutOrder (float nhosign, float nhoexp);
  static float deriveNhosign      (int order);
  static float deriveNhoexp       (int order);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
