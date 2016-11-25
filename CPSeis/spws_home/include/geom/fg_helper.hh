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

//---------------------- fg_helper.hh ---------------------//
//---------------------- fg_helper.hh ---------------------//
//---------------------- fg_helper.hh ---------------------//

//              header file for the FgHelper class
//                  not derived from any class
//                       subdirectory geom 

  // This class functions as a helper to the FieldGeometry class.


#ifndef _FG_HELPER_HH_
#define _FG_HELPER_HH_


class FgHelper
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int _data_needs_saving; // whether data has changed (since last saved).
  int _frozen;            // whether updates of dependent values are frozen.
  int _out_of_date;       // whether dependent values are out-of-date.
  int _major;             // whether major operations are in progress.
  int _data_changing;     // whether data is in the process of changing.
  int _lock;              // to what extent data changes are locked.

  class FieldGeometry       *_fg;
  class FgInformer          *_informer;
  class FgGathers           *_gathers;


//----------------------- functions -------------------------//
//----------------------- functions -------------------------//
//----------------------- functions -------------------------//

public:

  FgHelper (FieldGeometry *fg, FgInformer *informer, FgGathers *gathers);
  virtual ~FgHelper();

  int  isLocked            (int lowest_lock);
  void notifyDataWillChange(int grid, int chng);
  void notifyDataHasChanged(int grid, int chng);

  int  dataNeedsSaving              ()  const  { return _data_needs_saving; }
  int  dependentUpdatesFrozen       ()  const  { return _frozen; }
  int  dependentValuesOutOfDate     ()  const  { return _out_of_date; }

  void turnOffDataNeedsSavingFlag   ();

  int  getDataLock                  ()  const  { return _lock; }
  void setDataLock                  (int lock);
 
  int  allowDeletingData            ()  const;
  int  allowModifyingLdCards        ()  const;
  int  allowModifyingPpCards        ()  const;
  int  allowModifyingRpCards        ()  const;
  int  allowModifyingZt1Cards       ()  const;
  int  allowModifyingZt2Cards       ()  const;
  int  allowModifyingZt3Cards       ()  const;
  int  allowModifyingZt4Cards       ()  const;
  int  allowModifyingGridTransform  ()  const;

  void preMajorChanges            ();
  void postMajorChanges           ();

  void freezeDependentUpdates     ();
  void preResumeDependentUpdates  ();
  void postResumeDependentUpdates ();

  void preUpdateGathers           (int which);
  void postUpdateGathers          (int which);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
