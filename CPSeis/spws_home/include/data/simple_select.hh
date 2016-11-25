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

//------------------------ simple_select.hh ---------------------//
//------------------------ simple_select.hh ---------------------//
//------------------------ simple_select.hh ---------------------//

//             header file for the SimpleSelect class
//                  not derived from any class 
//                     subdirectory oprim

     // This class is similar to (and simpler than) the AccSelect
     // class.  This class differs from AccSelect in this manner:
     // This class optionally works with an array passed thru
     // argument lists, and does not interact with an array object
     // derived from SmartArray.

     // This class can be used as-is by registering with the object
     // an array of selection flags (_sel_array and _nelements).  The
     // virtual functions which access this array, if not overridden,
     // will reference this array directly through the registered
     // pointer, and will abort if this pointer is NULL.  A derived
     // class may override the virtual functions, in which case it
     // would not be necessary to register an array with the class,
     // since the array would not be used.

     // incrementSelectValue and setSelectValue must be followed
     // by updateSelections in order that numSelected will return
     // the correct value.

#ifndef _SIMPLE_SELECT_HH_
#define _SIMPLE_SELECT_HH_


class SimpleSelect
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  long  _nselect;     // number of selected array elements.
  char *_sa;          // pointer to array of selection flags.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor.

  SimpleSelect ();
  virtual ~SimpleSelect();

  void   registerSelectArray  (char *sa)  { _sa = sa; }
  void   clearSelections      (long n);
  void   setAllSelections     (long n);
  void   toggleAllSelections  (long n);
  void   updateSelections     (long n);
  long   numSelected          ()        const  { return _nselect; }
  void   incrementSelectValue (long i);
  int    isSelected           (long i)  const;

public:   // virtual functions to optionally override.

  virtual char getSelectValue (long i)  const  { return _sa[i]; }
  virtual void setSelectValue (long i, char value)  { _sa[i] = value; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
