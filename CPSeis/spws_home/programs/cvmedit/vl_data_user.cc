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
#include "vl_data_user.hh"
#include "vl_data.hh"
#include "vect/vector.hh"
#include "oprim/base_data.hh"
#include "oprim/ll_base_data.hh"

#define FATTEN 2

#include <assert.h>
#include <string.h>

VLDataUser ::VLDataUser(VectorListData *vldata,long ident)
        : DataUser() , _dataObject(vldata), _ident(ident)
{Vector *editObject;
 assert(vldata);
 addData(vldata);
 editObject = vldata->getEditObject();
 _oldWidth  = 0;
 if(editObject) _oldWidth = editObject->getWidth();
}

VLDataUser::~VLDataUser()
{_dataObject = 0;
 _oldWidth   = 0;
}

void VLDataUser ::selectBefore(BaseData *bptr, long )
{Vector *editVector;
 VectorListData *dataObject = (VectorListData *) bptr;
 if(bptr==NULL) return;
 editVector = dataObject->getEditObject();
 if(editVector)
  {// unhighlight old selection.
   editVector->setWidth(_oldWidth);
  }
}

void VLDataUser ::selectAfter(BaseData *bptr, long )
{Vector *editVector;
 VectorListData *dataObject = (VectorListData *) bptr;
 if(bptr==NULL) return;
 editVector = dataObject->getEditObject();

 if(editVector)
  {// highlight new selection.
   int newWidth;
   _oldWidth = editVector->getWidth();
   newWidth = _oldWidth + FATTEN;
   if(newWidth > 9) newWidth = 9;
   editVector->setWidth(newWidth);
  }
}

Vector *VLDataUser ::getSelected(BaseData *bptr, long )
{VectorListData *dataObject = (VectorListData *) bptr;
 if(bptr == NULL) return NULL;
 return dataObject->getEditObject();}

BaseData *VLDataUser::getData()
{
   void *p;
   BaseData *retval = _baseDatas->top(&p);

   /* One and only one */
   assert((retval != NULL) && (_baseDatas->next(&p) == NULL));

   return retval;
}

