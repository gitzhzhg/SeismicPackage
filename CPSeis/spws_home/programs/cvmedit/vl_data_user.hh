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
#ifndef _VL_DATA_USER_HH
#define _VL_DATA_USER_HH


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "oprim/data_user.hh"

class Vector;
class VectorListData;

class VLDataUser : public DataUser
{
  public:

    VLDataUser(VectorListData *vectors,long ident = BaseData::defaultId);
    ~VLDataUser();
    void selectBefore(BaseData *bptr, long ident = BaseData::defaultId);
    void selectAfter(BaseData *bptr, long ident = BaseData::defaultId);
    Vector   *getSelected(BaseData *bptr, long ident = BaseData::defaultId);
    BaseData *getData();

  protected:

    long              _ident;
    VectorListData   *_dataObject;
    unsigned int      _oldWidth;

  private:

};

#endif




