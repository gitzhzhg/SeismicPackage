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
#include <stdlib.h>
#include "grid_data.hh"


//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

gridData::gridData(float *arr, GridLimits *glim)
{
 _owns_data = 0;
 _garr = (char *) arr;
 _glim = NULL;
 _slice= 1;
 _axis = 3;
 strcpy(_word_string,"FLOAT");
 if(!glim) return;
 setGlimData(glim);
}

gridData::gridData(char *arr, GridLimits *glim)
{
 _owns_data = 0;
 _garr = (char *) arr;
 _glim = NULL;
 _slice= 1;
 _axis = 3;
 strcpy(_word_string,"BYTE");
 if(!glim) return;
 setGlimData(glim);
}


gridData::gridData(float *arr,int  N1,float O1, float D1,
            int  N2,float O2, float D2,
            int  N3,float O3, float D3,
            ErsTransform *t1, ErsTransform *t2, ErsTransform *t3)
{
 int   n1,n2,n3;
 float o1,o2,o3, d1,d2,d3;
 strcpy(_word_string,"FLOAT");
 _owns_data = 0;
 _garr = (char *) arr;
 _glim = new_glimits();
 _slice= 1;
 _axis = 3;
 n1 = N1; n2 = N2; n3 = N3;
 o1 = O1; o2 = O2; o3 = O3;
 d1 = D1; d2 = D2; d3 = D3;
 glimits_set(_glim, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             t1,t2,t3);
}

gridData::gridData(char *arr,int  N1,float O1, float D1,
            int  N2,float O2, float D2,
            int  N3,float O3, float D3,
            ErsTransform *t1, ErsTransform *t2, ErsTransform *t3)
{
 int   n1,n2,n3;
 float o1,o2,o3, d1,d2,d3;
 strcpy(_word_string,"BYTE");
 _owns_data = 0;
 _garr = (char *) arr;
 _glim = new_glimits();
 _slice= 1;
 _axis = 3;
 n1 = N1; n2 = N2; n3 = N3;
 o1 = O1; o2 = O2; o3 = O3;
 d1 = D1; d2 = D2; d3 = D3;
 glimits_set(_glim, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             t1,t2,t3);
}

gridData *gridData::copy()
{// Make a copy of this instance of gridData.
 gridData *out=NULL;
 int siz=0;
 out = new gridData((char *) NULL, this->_glim);
 if(!out) return out;

 out->_slice = this->_slice;
 out->_axis  = this->_axis;
 out->setWord(this->getWord());
 out->_owns_data = 0;
 

 if(_axis==1) siz = _glim->n2*_glim->n3;
 if(_axis==2) siz = _glim->n1*_glim->n3;
 if(_axis==3) siz = _glim->n1*_glim->n2;
 if(strcmp(_word_string,"FLOAT")==0 ) siz = siz *sizeof(float);
 if(siz > 0) 
  { out->_garr = new char[siz];
    memcpy(out->_garr,this->_garr,siz);
    out->_owns_data = 1;
  }

 return out;
}


gridData::~gridData(void)
{_garr = NULL;
 if(_glim) destroy_glimits(_glim);
 _glim = NULL;
 if(_owns_data==1) delete []_garr;
}

void gridData::setGlimData(GridLimits *glim)
{int   n1,n2,n3;
 float o1,o2,o3, d1,d2,d3;
 ErsTransform *t1,*t2,*t3;
 if(!glim) return; // dont eliminate _glim and keep data
 if(_glim) destroy_glimits(_glim);
 _glim = new_glimits();
 glimits_get(glim, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             &t1,&t2,&t3);
 glimits_set(_glim, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             t1,t2,t3);
} 

void gridData::setGridIncs(float d1, float d2, float d3)
{if(!_glim) return;
 glimits_set_incs(_glim,d1,d2,d3);
}

void gridData::setGridOrgs(float o1, float o2, float o3)
{if(!_glim) return;
 glimits_set_orgs(_glim,o1,o2,o3);
}

void gridData::setGridSizes(int  n1, int  n2, int  n3)
{if(!_glim) return;
 glimits_set_sizes(_glim,n1,n2,n3);
}

void gridData::getGridVals(int  *n1,float *o1,float *d1,
int  *n2,float *o2,float *d2,int  *n3,float *o3,float *d3)
{ErsTransform *t1,*t2,*t3;
 glimits_get(_glim, n1,o1,d1, n2,o2,d2, n3,o3,d3,
             &t1,&t2,&t3);
}

void gridData::getGridSize(int  *n1, int  *n2, int  *n3)
{GridLimits *glim = _glim;
 float o1,o2,o3, d1,d2,d3;
 ErsTransform *t1,*t2,*t3;

 *n1 = 0; *n2=0; *n3=0;
 if(!glim) return;

 glimits_get( glim, n1, &o1, &d1, n2, &o2, &d2, n3, &o3, &d3,
     &t1, &t2, &t3 );
}

int   gridData::getGridLabels(char **lab1, char **lab2, char **lab3)
{GridLimits *glim = _glim;
 int   n1,n2,n3, i=0;
 float o1,o2,o3, d1,d2,d3;
 ErsTransform *t1,*t2,*t3;
 char *str;

 *lab1 = NULL; *lab2 = NULL; *lab3 = NULL;
 if(!glim) return 0;

 glimits_get( glim,&n1, &o1, &d1,&n2, &o2, &d2,&n3, &o3, &d3,
     &t1, &t2, &t3 );
 str = transform_getname(t1);
 if(str)
  {*lab1 = (char *) malloc(strlen(str)+1);
   strcpy(*lab1,str);
   i++;
  }
 str = transform_getname(t2);
 if(str)
  {*lab2 = (char *) malloc(strlen(str)+1);
   strcpy(*lab2,str);
   i++;
  }
 str = transform_getname(t3);
 if(str)
  {*lab3 = (char *) malloc(strlen(str)+1);
   strcpy(*lab3,str);
   i++;
  }
 return 1;
}

int   gridData::getGridCoords( ErsTransform **t1,ErsTransform **t2,
                ErsTransform **t3)
{
 *t1 = NULL; *t2 = NULL; *t3 = NULL;
 if(!_glim) return 0;
 glimits_get_trans(_glim, t1,t2,t3);
 return 1;
}

int   gridData::setGridCoords( ErsTransform *t1,ErsTransform *t2,
                ErsTransform *t3)
{
 if(!_glim) return 0;
 glimits_set_trans(_glim, t1,t2,t3);
 return 1;
}

int  gridData::getGridHiLo(float *lo, float *hi)
{int i;
 int  n1,n2,n3;
 float *farr=0;
 char  clo,chi;
 *lo = 0.0;
 *hi = 0.0;
 if(!_garr) return 0;
 getGridSize( &n1, &n2, &n3);

 if(strcmp(_word_string,"FLOAT")==0)
  {farr = (float *) _garr;
   *lo = farr[0];
   *hi = farr[0];
   for(i=1;i<n1*n2;i++)
       { *hi = (farr[i]> *hi) ? farr[i] : *hi;
         *lo = (farr[i]< *lo) ? farr[i] : *lo;
       }
  }
 else
  {
   clo = _garr[0];
   chi = _garr[0];
   for(i=1;i<n1*n2;i++)
     { chi = (_garr[i]> chi) ? _garr[i] : chi;
       clo = (_garr[i]< clo) ? _garr[i] : clo;
     }
   *lo = clo;
   *hi = chi;
  }

 return 1;
}

void gridData::gridDataTrans(ErsTransforms *tdata,
               char *name1, char *name2, char *name3)
{
 if(!tdata || !_glim) return;
 glimits_trans(_glim, tdata, name2, name3, name1);
 
}

