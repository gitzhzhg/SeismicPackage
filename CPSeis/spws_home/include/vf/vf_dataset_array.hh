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

//------------------------ vf_dataset_array.hh --------------------------//
//------------------------ vf_dataset_array.hh --------------------------//
//------------------------ vf_dataset_array.hh --------------------------//

//            header file for the VfDatasetArray class
//                derived from the SmartArray class
//                         subdirectory vf

   // This class maintains an array of velocity datasets which it owns.
   // Datasets in the array cannot be NULL.
   // The buffer is not used.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VF_DATASET_ARRAY_HH_
#define _VF_DATASET_ARRAY_HH_


#include "oprim/smart_array.hh"


class VfDatasetArray : public SmartArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  const long  _numedit;   // num editable datasets (or -1 if all are editable).
  int         _editable;  // for constructor in doCreateObject.
  char       *_progname;
  class VfInformer   *_informer;
  class VfUtilities  *_utilities;
 

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:  // constructor and destructor.
         // number = initial total number of datasets to create.
         // numedit = number of editable datasets to create.

         // modification made 10/16/98:
         // if numedit is -1, all datasets will be editable.

           VfDatasetArray (long number, long numedit, const char *progname,
                           VfInformer *informer, VfUtilities *utilities);
  virtual ~VfDatasetArray ();

/*
  long numEditableDatasets()  const  { return _numedit; }
*/
  long numEditableDatasets()  const
              { return (_numedit >= 0 ? _numedit : numElements()); }


  class VfDataset *dataset(long ifun)  const
                         { return (VfDataset*)arrayElement(ifun); }

  class VfDataset *activeDataset()  const
                         { return (VfDataset*)activeElement(); }

  class VfDataset *referenceDataset()  const
                         { return (VfDataset*)referenceElement(); }

public:  // functions to use instead of similar functions in SmartArray.
         // the setActiveIndex function must not be used.
         // the setReferenceIndex function must not be used.
         // no other insert/remove functions should be used.

  void setActiveDatasetIndex    (long index);
  void setReferenceDatasetIndex (long index);
  void appendDataset            ();
  void removeDataset            (long index);

private:

  void privateResetParameters();

private:  // overriding virtual functions.

  virtual void *doCreateObject ();
  virtual void  doDeleteObject (void *object);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
