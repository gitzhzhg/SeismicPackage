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

//---------------------- static_dataset_array.hh ------------------------//
//---------------------- static_dataset_array.hh ------------------------//
//---------------------- static_dataset_array.hh ------------------------//

//            header file for the StaticDatasetArray class
//                derived from the SmartArray class
//                        subdirectory stat

   // This class maintains an array of static datasets which it owns.
   // Datasets in the array cannot be NULL.
   // The buffer is not used.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _STATIC_DATASET_ARRAY_HH_
#define _STATIC_DATASET_ARRAY_HH_


#include "oprim/smart_array.hh"


class StaticDatasetArray : public SmartArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  char                  *_progname;
  class StaticInformer  *_informer;
 

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:  // constructor and destructor.

           StaticDatasetArray (const char *progname, StaticInformer *informer);
  virtual ~StaticDatasetArray ();

  class StaticDataset *dataset(int ifun)  const
                         { return (StaticDataset*)arrayElement(ifun); }

  class StaticDataset *activeDataset()  const
                         { return (StaticDataset*)activeElement(); }

  class StaticDataset *referenceDataset()  const
                         { return (StaticDataset*)referenceElement(); }

public:  // functions to use instead of similar functions in SmartArray.
         // the setActiveIndex    function must not be used.
         // the setReferenceIndex function must not be used.
         // no other insert/remove functions should be used.

  void                 setActiveDatasetIndex     (int index);
  void                 setReferenceDatasetIndex  (int index);
  class StaticDataset *appendNewDataset          ();
  class StaticDataset *appendNewActiveDataset    ();
  class StaticDataset *appendNewReferenceDataset ();
  void                 removeDataset             (int index);

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
