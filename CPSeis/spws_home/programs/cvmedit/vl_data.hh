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
#ifndef _VL_DATA_HH
#define _VL_DATA_HH

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "transform.h"
#include "oprim/base_data.hh"

class VectorLinkedList;
class Vector;

class VectorListData: public BaseData
{
  public:

    VectorListData(VectorLinkedList *vectors, Vector *edit);
    ~VectorListData();
    void setEditObject(Vector *vector);
    Vector *getEditObject()
     { return _editObject; }
    VectorLinkedList *getDataObject()
     { return _dataObject; }
    void transform(ErsTransforms *tdata,
      char *xname, char *yname, char *zname);
    int    isMember(Vector *vector);
    void   removeMember(Vector *vector);
    void   changeMemberID(Vector *v, long id);
    void   changeMemberColor(Vector *v, char *cname);
    void   changeMemberLoc(Vector *v, float xo, float yo, float zo);
    void   changeMemberLoc(int n, float xo, float yo, float zo);
    void   changeMemberAll(Vector *v, long id, float xo,float yo,float zo);
    void   unSelect();
    void   select();

  protected:

    VectorLinkedList   *_dataObject;
    Vector             *_editObject;

  private:

};

#endif




