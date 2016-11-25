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

//---------------------- vf_manager.hh -------------------------//
//---------------------- vf_manager.hh -------------------------//
//---------------------- vf_manager.hh -------------------------//

//               header file for the VfManager class
//                     not derived from any class
//                          subdirectory vf


  // This class manages a small array of VfDataset objects which
  // it owns.  The initial number of VfDataset objects is passed to the
  // constructor, and could be an application command line parameter.
  // This class creates these VfDataset objects in the constructor
  // and deletes them in the destructor.  The number of VfDataset
  // objects can change during the lifetime of an instance of
  // this class.  These VfDataset objects are interacted with by
  // requesting pointers to them by calling the function dataset(index)
  // or activeDataset() or referenceDataset().

  // This class manages (through its owned VfInformer class)
  // a linked list of data users (derived from VfInform).
  // These data users are to be created and deleted outside of
  // this class, and are registered with this class by the
  // VfInform base class constructor, which must be provided
  // a pointer to this class.  All VfDataset objects send messages
  // to all data users; most of these messages contain a pointer to the
  // VfDataset object which sent the message.  This manager class also
  // sends a few messages to data users; these messages do not contain
  // a pointer to any VfDataset object.

  // The VfUtilities class (owned by VfManager) contains parameters,
  // functions, and other classes which are intended to be manipulated
  // outside of any VfDataset class, and therefore a function is provided
  // to get the pointer to the VfUtilities class.  (This eliminates the
  // need to provide numerous pass-thru functions to access these parameters
  // and classes, which are not needed by most data users.)  The VfUtilities
  // class, and the classes it owns, contain parameters (and sometimes
  // algorithms) which are needed when specific actions are performed on
  // a velocity dataset.  Changes to these parameters do NOT affect
  // any velocity dataset at the time the changes are made, and therefore
  // the VfDataset objects do NOT have to know that the changes are being
  // made, and data users are NOT informed when these changes are made.
  // (Data users ARE of course informed when actions based on these
  // parameters are taken on a velocity dataset.)



#ifndef _VF_MANAGER_HH_
#define _VF_MANAGER_HH_

#include "named_constants.h"


class VfManager
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class VfDatasetArray *_array;      // array of VfDataset objects.
  class VfInformer     *_informer;   // direct access by user allowed.
  class VfUtilities    *_utilities;  // direct access by user allowed.


//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor.
             // number = initial total number of datasets to create.
             // numedit = number of editable datasets to create.

    // The number of editable datasets (>= 0) will not change throughout
    // the life of the VfManager object.  The total number of datasets
    // (>= numedit) can be increased or decreased at any time, but can
    // never be less than numedit.  The editable datasets occupy the
    // beginning locations in the array.  The rest of the datasets (if any)
    // are not editable and are called "comparison datasets".

    // modification made 10/16/98:
    // if numedit is -1, all datasets will be editable but not backed up.

    // modification made 3/25/02:
    // see vf_update.hh for a description of the bogus_velocities argument.

           VfManager (long number, long numedit, const char *progname = "",
                                        int bogus_velocities = FALSE);
  virtual ~VfManager ();

public:     // miscellaneous functions.
            // getActiveDatasetIndex and getReferenceDatasetIndex
            //   return -1 if there are no datasets.
            // activeDataset and referenceDataset
            //   return NULL if there are no datasets.

  long     numDatasets              ()            const;
  long     numEditableDatasets      ()            const;
  long     getActiveDatasetIndex    ()            const;
  void     setActiveDatasetIndex    (long index);
  long     getReferenceDatasetIndex ()            const;
  void     setReferenceDatasetIndex (long index);

  class VfDataset *dataset          (long index)  const;
  class VfDataset *activeDataset    ()            const;
  class VfDataset *referenceDataset ()            const;
  VfInformer      *informer         ()            const { return _informer; }
  VfUtilities     *utilities        ()            const { return _utilities; }

public:    // bin tolerances.

  void     setXcenter    (float xcenter);    // pass-thru to VfUtilities.
  void     setYcenter    (float ycenter);    // pass-thru to VfUtilities.
  void     setXwidth     (float xwidth);     // pass-thru to VfUtilities.
  void     setYwidth     (float ywidth);     // pass-thru to VfUtilities.

public:      // insert or remove comparison (uneditable) dataset.
             // asserts if index < numedit or index >= number.
             // if numedit is -1, inserts or removes an (editable) dataset.

  void    appendNewComparisonDataset ();
  void    removeComparisonDataset    (long index);

public:     // convenience functions.

  long       numDatasetsNeedingSaving   ()              const;
  long       numDatasetsBackedUp        ()              const;
  long       numDatasetsSelected        ()              const;
  long       numDatasetsLocked          ()              const;
  long       numDatasetsWithUndoFile    (void *doer)    const;

  void       selectAllDatasets          ();
  void       unselectAllDatasets        ();
  void       lockAllDatasets            ();
  void       unlockAllDatasets          ();
  void       maybeDeleteUndoFiles       (void *doer);
  void       saveBackupFiles            ();

private:

  void       preChangeBinTolerances();
  void       postChangeBinTolerances();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
