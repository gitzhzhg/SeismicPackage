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

//---------------------- table_base.hh ------------------------------//
//---------------------- table_base.hh ------------------------------//
//---------------------- table_base.hh ------------------------------//

//         Header file for the TableBase class, 
//         a derived class from DataUser class.


#ifndef _TABLE_BASE_HH_
#define _TABLE_BASE_HH_


#include <stdio.h>
#include <iostream.h>
#include <X11/StringDefs.h>
#include "cprim.h"
#include "wproc.h"
#include "wbox.h"
#include "oprim/data_user.hh"
#include "oprim/base_data.hh"

class SLDelay;
class SLSmartForm;
class SLDialog;

class TableBase : public DataUser
{

public:

  SLDialog  *createDialog ( SLDelay *parent, char *name    ); 
  SLSmartForm    *createForm   ( SLDelay *parent ); 
  
  void      modIndicesBefore(BaseData * /*data*/, int /*startIndex*/,
            int /*numIndices*/,long /*ident = BaseData::defaultId*/){}
  void      modIndicesAfter(BaseData * /*data*/, int /*startIndex*/,
            int /*numIndices*/,long /*ident = BaseData::defaultId*/) {}
  void      modDone(BaseData *, long /*ident = BaseData::defaultId*/)
            { wbox_update(); }

protected:

                TableBase  (class BaseData *) ;
  virtual      ~TableBase  (void)   {}
  virtual void  contents   (void)  = 0 ;

private:

  static  void  Trap( void *);

};

#endif


