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
#include <cprim.h>

#ifdef VMS
#define gridrrec_ gridrrec
#define vxzgetv_  vxzgetv
#endif
#ifdef CRAY
#define gridrrec_ GRIDRREC
#define vxzgetv_  VXZGETV
#endif

int vxzgetv_(long *lun, long *c1, long *c2, long *npts, float *vxz,
long *pack, float *vmin, float *vmax)
{long  i,n,istat;
 long  jump,offset,one=1;
 float *velocity;
 union vel_
  {float *fvel;
   long  *lvel;
  } uvel;
 extern void gridrrec_();
 extern void pack16_();
#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) < (B) ? (A) : (B))

/*
 * Check validity of input parameters
 */
 if(lun == NULL) return 1;
 if(*lun== 0)    return 1;
 if(*npts == NULL) return 1;
 if(*npts < 1)   return 1;
 if(vmin == NULL)  return 1;
 if(vmax == NULL)  return 1;
 if(c1==NULL || c2 == NULL) return 1;
 if(*c1<=0    ) *c1 = 1;
 if(*c1 > *c2 )
  { long temp;
    temp = *c2;
    *c2  = *c1;
    *c1  = temp;
  }
 
 uvel.fvel = vxz;
 uvel.lvel = (long *) vxz;
 offset = 0;
 jump   = (*pack != 0) ? *npts : *npts/(sizeof(float)/2) + 1;
 for(n= *c1;n<= *c2;n++)
  {
   velocity = uvel.fvel+offset;
   gridrrec_(lun,&n,velocity,npts,&istat);
   if(istat != 0) goto error;

   if(n == *c1) { *vmin = velocity[0]; *vmax = velocity[0];}
   for(i=0;i< *npts;i++)
    { *vmin = min(*vmin,velocity[i]); 
      *vmax = max(*vmax,velocity[i]); 
    }

   if(*pack == 0)
    {long itemp;
     for(i=0;i< *npts;i++)
      {itemp = velocity[i];
       uvel.lvel[offset+i] = itemp;
      }
     pack16_(uvel.lvel+offset,&one, uvel.lvel+offset,npts);
    }
 
   offset += jump;
  }

 return 0;
error:
 printf("vxzgetv: gridrrec read error, record=%d\n",n);
 *c2 = n-1;
 return 1;
}
