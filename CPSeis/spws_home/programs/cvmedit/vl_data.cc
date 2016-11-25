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
#include <assert.h>
#include <string.h>
#include "oprim/modbase_data.hh"
#include "vl_data.hh"
#include "vect/vector.hh"
#include "vect/ll_vector.hh"


VectorListData::VectorListData(VectorLinkedList *data, Vector *edit):
              BaseData()
{
 assert(data);
 _editObject = NULL;
 _dataObject = data;
 if(_dataObject->find(edit))
  {_editObject = edit;}
}

VectorListData::~VectorListData()
{
 _dataObject = 0;
 _editObject = 0;
}

void VectorListData::setEditObject(Vector *vector)
{
 if(vector)
  { if(isMember(vector)==0) return; }
 selectBefore();
 _editObject = vector;
 selectAfter();
}

void VectorListData::unSelect()
{
 selectBefore();
}

void VectorListData::select()
{
 selectAfter();
}

void VectorListData::transform(ErsTransforms *tdata,
 char *xname, char *yname, char *zname)
{ 
 Vector      *vector;
 ModBaseData *dobj;
 void   *p;
 int    i, old_vis;
 if(tdata==NULL) return ;
 if(xname==NULL || zname==NULL) return;
 vector = _dataObject->top(&p);
 while(vector != NULL)
  {old_vis = 0;
   if(vector->isVisible()) old_vis = 1;
   vector->makeInvisible();
   dobj = (ModBaseData *) vector->getData();
   i = dobj->trans_data_comps(tdata,xname,yname,NULL,zname);
   if(old_vis) vector->makeVisible();
   vector = _dataObject->next(&p);
  }

 modDone(); //signal everyone that changes are done
}

int VectorListData::isMember(Vector *vector)
{
 if(vector ==NULL) return 0;
 if(_dataObject->find(vector)) return 1;
 return 0;
}

void VectorListData::removeMember(Vector *v)
{
 if(!_dataObject || !v) return;
 if(!_dataObject->find(v)) return;
 _dataObject->remove(v);
 modDone(); //signal everyone that changes are done
}

void VectorListData::changeMemberID(Vector *v, long id)
{char          font[72],str[8];
 ModBaseData  *dobj=NULL;
 if(!_dataObject || !v) return;
 if(!_dataObject->find(v)) return;
 sprintf(str,"%d",id);
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");
 dobj   = (ModBaseData *) v->getData();
 if(dobj) dobj->setid(id);
 v->setLabel(str,font);
 modDone(); //signal everyone that changes are done
}

void VectorListData::changeMemberColor(Vector *v, char *cname)
{
 if(!_dataObject || !v) return;
 if(!_dataObject->find(v)) return;
 v->setColor(cname);
 modDone(); //signal everyone that changes are done
}

void VectorListData::changeMemberLoc(int n, float xo, float yo, float zo)
{Vector *v;
 int i=0;
 void *p;
 v = _dataObject->top(&p);
 while(v != NULL)
   {if(i==n) {changeMemberLoc(v,xo,yo,zo); return; }
    v = _dataObject->next(&p);
    i++;
   }
}

void VectorListData::changeMemberLoc(Vector *v, float xo, float yo, float zo)
{
 ModBaseData   *dobj=NULL;
 float          x=xo,y=yo,z=zo;
 if(!_dataObject || !v) return;
 if(!_dataObject->find(v)) return;
 if(v) dobj   = (ModBaseData *) v->getData();
 if(dobj)
  { dobj->setpt(x,z,y);
    if(dobj->getNumPts() < 1)
      dobj->insert(0,1,&x,&z,&y,NULL,NULL);
    else
      dobj->replace(0,1,&x,&z,&y,NULL,NULL);
  }
 modDone(); //signal everyone that changes are done
}

void VectorListData::changeMemberAll(Vector *v, long id, float xo,float yo,float zo)
{char          font[72],str[8];
 ModBaseData  *dobj=NULL;
 float x=xo,y=yo,z=zo;
 if(!_dataObject || !v) return;
 if(!_dataObject->find(v)) return;
 sprintf(str,"%d",id);
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");
 dobj   = (ModBaseData *) v->getData();
 if(dobj)
  { dobj->setpt(x,z,y);
    dobj->setid(id);
    if(dobj->getNumPts() < 1)
      dobj->insert(0,1,&x,&z,&y,NULL,NULL);
    else
      dobj->replace(0,1,&x,&z,&y,NULL,NULL);
  }
 v->setLabel(str,font);
 modDone(); //signal everyone that changes are done
}
