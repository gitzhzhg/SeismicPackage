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
#include "glimits.h"

/*********************************************************
 * Access or Set members of the GridLimits structure.  ***
 ********************************************************/
GridLimits *new_glimits()
{GridLimits *glimits;
 glimits = (GridLimits *) calloc(1,sizeof(GridLimits));
 return glimits;
}

void destroy_glimits(GridLimits *g)
{
 if(g != NULL) free(g);
}

void glimits_set(GridLimits *glimits,
     int  *n1, float *o1, float *d1,
     int  *n2, float *o2, float *d2,
     int  *n3, float *o3, float *d3,
     ErsTransform *t1, ErsTransform *t2, ErsTransform *t3 )
{
  if(glimits != NULL)
   {glimits->n1 = (*n1 < 0) ? - *n1 : *n1;
    glimits->n2 = (*n2 < 0) ? - *n2 : *n2;
    glimits->n3 = (*n3 < 0) ? - *n3 : *n3;
    glimits->o1 = *o1;
    glimits->o2 = *o2;
    glimits->o3 = *o3;
    glimits->d1 = *d1;
    glimits->d2 = *d2;
    glimits->d3 = *d3;
    glimits->trans1 = t1;
    glimits->trans2 = t2;
    glimits->trans3 = t3;
   }

 return;
}

void glimits_get(GridLimits *glimits,
     int  *n1, float *o1, float *d1,
     int  *n2, float *o2, float *d2,
     int  *n3, float *o3, float *d3,
     ErsTransform **t1, ErsTransform **t2, ErsTransform **t3 )
{
  if(glimits != NULL)
   {*n1 = glimits->n1;
    *n2 = glimits->n2;
    *n3 = glimits->n3;
    *o1 = glimits->o1;
    *o2 = glimits->o2;
    *o3 = glimits->o3;
    *d1 = glimits->d1;
    *d2 = glimits->d2;
    *d3 = glimits->d3;
    *t1 = glimits->trans1;
    *t2 = glimits->trans2;
    *t3 = glimits->trans3;
   }

 return;
}

void glimits_get_trans(GridLimits *glimits,
     ErsTransform **t1,ErsTransform **t2,ErsTransform **t3)
{ *t1 = NULL;
  *t2 = NULL;
  *t3 = NULL;
  if(glimits == NULL) return;
  *t1 = glimits->trans1;
  *t2 = glimits->trans2;
  *t3 = glimits->trans3;
  return;
}

void glimits_set_trans(GridLimits *glimits,
     ErsTransform *t1,ErsTransform *t2,ErsTransform *t3)
{ 
  if(glimits == NULL) return;
  glimits->trans1 = t1;
  glimits->trans2 = t2;
  glimits->trans3 = t3;
  return;
}

void glimits_set_orgs(GridLimits *glimits, float o1, float o2, float o3)
{
 glimits->o1 = o1;
 glimits->o2 = o2;
 glimits->o3 = o3;
}

void glimits_set_incs(GridLimits *glimits, float d1, float d2, float d3)
{
 glimits->d1 = d1;
 glimits->d2 = d2;
 glimits->d3 = d3;
}

void glimits_set_sizes(GridLimits *glimits, int  n1, int  n2, int  n3)
{
 glimits->n1 = n1;
 glimits->n2 = n2;
 glimits->n3 = n3;
}

void glimits_setdef(GridLimits *glimits, ModLimits *mlimits)
{int   n1=101,n2=101,n3=1;
 float *o1,*o2,*o3, d1,d2,d3;
 float x1, x2, z1, z2, y1, y2;
 ErsTransform *t1,*t2,*t3;
 if(glimits==NULL || mlimits== NULL) return;
 o1 = &x1; o2 = &z1; o3 = &z1;
 mlimits_get(mlimits, &x1, &x2,
     &z1, &z2, &y1, &y2,&t1,&t2,&t3 );
 d1 = (n1>1) ? (x2 - x1)/(n1-1) : (x2 - x1);
 d2 = (n2>1) ? (z2 - z1)/(n2-1) : (z2 - z1);
 d3 = (n3>1) ? (y2 - y1)/(n3-1) : (y2 - y1);
 glimits_set(glimits, &n1, o1, &d1, &n2, o2, &d2, &n3, o3, &d3,
     t1, t2, t3 );
}

/******************************************************
 * Method to transform GridLimits object            ***
 *****************************************************/
int glimits_trans(GridLimits *glim, ErsTransforms *tdata,
 char *xname, char *yname, char *zname)
{/* z = 1, x = 2, y = 3 */
 ErsTransform *to[3],*ti[3];
 int   i,ierr,istat,one=1;

 if(glim==NULL || tdata==NULL) return 1;
 to[0] = ErsTransGetTran(tdata,xname);
 to[1] = ErsTransGetTran(tdata,yname);
 to[2] = ErsTransGetTran(tdata,zname);
 ti[0] = glim->trans2;
 ti[1] = glim->trans3;
 ti[2] = glim->trans1;
 istat = 0;
 if(to[0] != NULL && ti[0]!= NULL)
  {glim->o2 = transform_cnvrt(ti[0],glim->o2,to[0]);
   glim->d2 = transform_scale(ti[0],glim->d2,to[0]);
   glim->trans2 = to[0];
  }
 if(to[1] != NULL && ti[1]!=NULL)
  {glim->o3 = transform_cnvrt(ti[1],glim->o3,to[1]);
   glim->d3 = transform_scale(ti[1],glim->d3,to[1]);
   glim->trans3 = to[1];
  }
 if(to[2] != NULL && ti[2]!=NULL)
  {glim->o1 = transform_cnvrt(ti[2],glim->o1,to[2]);
   glim->d1 = transform_scale(ti[2],glim->d1,to[2]);
   glim->trans1 = to[2];
  }
 return istat;
}

void glimits_prt(GridLimits *glim)
{ErsTransform *t;
 if(glim == NULL) return;
 t = glim->trans1;
 printf(" glimits_prt:  Name=%s, n1=%d, d1=%f o1=%f\n",
        transform_getname(t),glim->n1,glim->d1,glim->o1);
 t = glim->trans2;
 printf(" glimits_prt:  Name=%s, n2=%d, d2=%f o2=%f\n",
        transform_getname(t),glim->n2,glim->d2,glim->o2);
 t = glim->trans3;
 printf(" glimits_prt:  Name=%s, n3=%d, d3=%f o3=%f\n",
        transform_getname(t),glim->n3,glim->d3,glim->o3);

}

