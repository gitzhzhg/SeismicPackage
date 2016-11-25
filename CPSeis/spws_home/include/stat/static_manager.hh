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

//---------------------- static_manager.hh -------------------------------//
//---------------------- static_manager.hh -------------------------------//
//---------------------- static_manager.hh -------------------------------//

//               header file for the StaticManager class
//                     not derived from any class
//                          subdirectory stat


  // This class manages a small array of StaticDataset objects which
  // it owns.  Initially there is only one StaticDataset object.  The
  // number of StaticDataset objects can change during the lifetime of an
  // instance of this class, but can never be less than one.  These
  // StaticDataset objects are interacted with by requesting pointers
  // to them by calling the functions dataset(index) or activeDataset()
  // or referenceDataset().

  // This class manages (through its owned StaticInformer class)
  // a linked list of data users (derived from StaticInform).
  // These data users are to be created and deleted outside of
  // this class, and are registered with this class by the
  // StaticInform base class constructor, which must be provided
  // a pointer to this class.  All StaticDataset objects send messages
  // to all data users; most of these messages contain a pointer to the
  // StaticDataset object which sent the message.  This manager class also
  // sends a few messages to data users; these messages do not contain
  // a pointer to any StaticDataset object.


#ifndef _STATIC_MANAGER_HH_
#define _STATIC_MANAGER_HH_


class StaticManager
{

//----------------------------- data ------------------------------------//
//----------------------------- data ------------------------------------//
//----------------------------- data ------------------------------------//

public:   // arguments needed for mergeSelectedDatasets.

  enum { ACTION_ADD    = 1, // add selected datasets together.
         ACTION_AV     = 2, // average selected datasets together.
         ACTION_SUB    = 3  // subtract selected non-active dataset from
                            //   selected active dataset (there must be two
                            //   selected datasets, one of which is active).
       };

  enum { SIZE_ALL     = 1, // merged size includes all selected datasets.
         SIZE_ACTIVE  = 2  // merged size matches active dataset.
       };

  enum { WHERE_ACTIVE    = 1, // replace active dataset with merged dataset.
         WHERE_NEW       = 2, // put merged dataset into newly created dataset.
         WHERE_NEWACTIVE = 3  // put merged dataset into newly created dataset,
                              //   and make it active.
       };

  enum { NILS_KEEP    = 1, // do not get rid of nils.
         NILS_ZERO    = 2, // replace nils with zero.
         NILS_TERP    = 3, // replace nils with interpolated values.
         NILS_XDIR    = 4, // interpolate nils in X dir (no extrapolation).
         NILS_YDIR    = 5  // interpolate nils in Y dir (no extrapolation).
       };

  enum { INTERP_NEAR = 1, // while resampling: use nearest values
                          //         (nils preserved).
         INTERP_TERP = 2  // while resampling: interpolate among non-nil
                          //         values (some nils might survive).
       };

  enum { EXTRAP_EDGE = 1, // while resampling: extrapolate with edge value.
         EXTRAP_NILS = 2, // while resampling: extrapolate with nils.
         EXTRAP_ZERO = 3  // while resampling: extrapolate with zero.
       };

private:

  class StaticDatasetArray *_array;      // array of StaticDataset objects.
  class StaticInformer     *_informer;   // direct access by user allowed.

//------------------------- functions -----------------------------------//
//------------------------- functions -----------------------------------//
//------------------------- functions -----------------------------------//

public:      // constructor and destructor.

           StaticManager (const char *progname);
  virtual ~StaticManager ();

public:     // miscellaneous functions.

  int         numDatasets                ()           const;
  int         getActiveDatasetIndex      ()           const;
  void        setActiveDatasetIndex      (int index);
  int         getReferenceDatasetIndex   ()           const;
  void        setReferenceDatasetIndex   (int index);

  class StaticDataset  *dataset          (int index)  const;
  class StaticDataset  *activeDataset    ()           const;
  class StaticDataset  *referenceDataset ()           const;
  class StaticInformer *informer         ()   const  { return _informer; }

public:      // insert or remove one dataset.
             // asserts if index < 0 or index >= numDatasets().

  class StaticDataset  *appendNewDataset           ();
  class StaticDataset  *appendNewActiveDataset     ();
  class StaticDataset  *appendNewReferenceDataset  ();
  class StaticDataset  *duplicateDataset           (int index);
  class StaticDataset  *duplicateActiveDataset     ();
  class StaticDataset  *duplicateReferenceDataset  ();
  void                  removeDataset              (int index);
  void                  removeActiveDataset        ();
  void                  removeReferenceDataset     ();

public:   // merge selected datasets.
          // the parameters should be appropriate enums above.
          // for splicing datasets:
          //  - use ACTION_AV, SIZE_ALL, and EXTRAP_NILS.
          //  - do not use NILS_ZERO or NILS_TERP.
          //  - use xdist and ydist > 0.

  void  mergeSelectedDatasets (void *doer, int action, int size, int where,
                               int nils, int interp, int extrap,
                               int xdist = 0, int ydist = 0);

private:

  void  getMergedValues (int action, int nils, int interp, int extrap,
                         int xdist, int ydist,
                float x1, float y1, float xinc, float yinc, int nx, int ny,
                float *values);

public:   // convenience functions.

  int        numDatasetsNeedingSaving   ()              const;
  int        numDatasetsBackedUp        ()              const;
  int        numDatasetsSelected        ()              const;
  int        numDatasetsLocked          ()              const;
  int        numDatasetsWithUndoFile    (void *doer)    const;

  void       selectAllDatasets          ();
  void       unselectAllDatasets        ();
  void       lockAllDatasets            ();
  void       unlockAllDatasets          ();
  void       maybeDeleteUndoFiles       (void *doer);
  void       saveBackupFiles            ();

//---------------------- end of functions -----------------------------//
//---------------------- end of functions -----------------------------//
//---------------------- end of functions -----------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
