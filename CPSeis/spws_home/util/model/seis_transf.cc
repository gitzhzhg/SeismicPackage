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
#include <math.h>
#include "sp/seis_plot.hh"
#include "seis_transf.hh"

#define NILKEY 0
SeisTransf:: SeisTransf(int phd,int shd, int thd)
{/* must set pheader before sheader & sheader before theader */
     _pheader = NILKEY;
     _sheader = NILKEY;
     _theader = NILKEY;
     _tol = 0.01;
     if(phd != NILKEY) {_pheader = phd; }
     if(shd != NILKEY) {_sheader = shd; }
     if(thd != NILKEY) {_theader = thd; }
     _seis_tz= NULL;
     _data_tz= NULL;

}

SeisTransf:: SeisTransf(int phd,int shd, int thd, void *tzs, void *tzd)
{/* must set pheader before sheader & sheader before theader */
     _pheader = NILKEY;
     _sheader = NILKEY;
     _theader = NILKEY;
     _tol = 0.01;
     if(phd != NILKEY) {_pheader = phd; }
     if(shd != NILKEY) {_sheader = shd; }
     if(thd != NILKEY) {_theader = thd; }
     _seis_tz= tzs;
     _data_tz= tzd;

}

SeisTransf:: ~SeisTransf()
{   _seis_tz= NULL; _data_tz= NULL; }


float SeisTransf::convDataToXOffset(SeisPlot *sp, float data)
{float trace;
 int   key;

 if(sp->plotType() >=  PlotImage::PlotCOLOR)
  {
   return data;
  }
 else
  { // return memory trace number
   key = goodKey(sp);
   trace = KeyToTrace(sp, data, key);
//   trace = PKeyToTrace(sp, data);
   return trace;
  }

}

float SeisTransf::convXOffsetToData(SeisPlot *sp, float trace)
{float pkey,skey,tkey,return_count;

 if(sp->plotType() >= PlotImage::PlotCOLOR)
  { 
   return trace;
  }
 else
  {// trace should be a memory trace number.
   return_count = TraceToKeys(sp, trace, &pkey, &skey, &tkey);
   return pkey;
  }
}

long SeisTransf:: TraceToKeys(SeisPlot *sp, float tn, float *pkey,
    float *skey, float *tkey)
{/* map memory (formerly relative) trace number to header keys */
 /* will interpolate between traces */
 /* Return value = the count of valid key values returned */
  float val1,val2,interp_factor;
  const float *hd;
//  long  mem_trno,rel_trno1,rel_trno2;
  long  mem_trno1,mem_trno2;
  long  memstrt,memend;
  long  mem1,mem2,io1,io2;
  long  nhdrs,ndisp,cnt;

  if(sp == NULL) return 0;
  if(sp->plotType() >= PlotImage::PlotCOLOR) return 0;
  hd    = sp->headers();
  nhdrs = sp->numHeaders();
  ndisp = sp->displayedTraces();
  if(nhdrs <= 0) return 0;
  if(hd == NULL) return 0;
  if(ndisp <= 0) return 0;

  memstrt= sp->firstTrace() + sp->currentFrame()*sp->originalTraces();
  memend = memstrt + ndisp -1;
  mem_trno1 = (long ) tn;
  if(mem_trno1 < memstrt) return 0;
  if(mem_trno1 > memend) return 0;

  mem_trno2 = mem_trno1 + 1;
  if(mem_trno2 > memend) mem_trno2 = memend;
  interp_factor = 0.0;
  if(mem_trno2 != mem_trno1)
   interp_factor = tn - (float) mem_trno1;

  mem1  = mem_trno1; //get_memtrace(sp,rel_trno1);
  mem2  = mem_trno2; //get_memtrace(sp,rel_trno2);

  if(mem1==0 || mem2==0) return 0;
  cnt = 0;
  io1 = (mem1-1)*nhdrs;
  io2 = (mem2-1)*nhdrs;
  if(_pheader != NILKEY && _pheader <= nhdrs)
   {val1 = hd[io1 + _pheader - 1];
    val2 = hd[io2 + _pheader - 1];
    *pkey= val1 + interp_factor*(val2-val1);
    cnt++;
   }
  else return 0;
  if(_sheader != NILKEY && _sheader <= nhdrs)
   {val1 = hd[io1 + _sheader - 1];
    val2 = hd[io2 + _sheader - 1];
    *skey= val1 + interp_factor*(val2-val1);
    cnt++;
   }
  if(_theader != NILKEY && _theader <= nhdrs)
   {val1 = hd[io1 + _theader - 1];
    val2 = hd[io2 + _theader - 1];
    *tkey= val1 + interp_factor*(val2-val1);
    cnt++;
   }
  return cnt;
}

float SeisTransf:: PKeyToTrace(SeisPlot *sp, float pkey)
{/* Map a primary key value to a memory( old was relative) trace number */
 /* Does interpolation between traces */
 /* return -1.0 when zero traces or no match */
  const float *hd;
  long  i,nhdrs,ndisp;
  long memstrt,memend;
  float ftn,rtn,val,p1,p2;
  long  tr1,tr2,rtn1,rtn2;
  int   hdr;
  ftn = -1.0; /* memory index */
  rtn = -1.0; /* display index */
  if(sp == NULL) return rtn;
  hd    = sp->headers();
  nhdrs = sp->numHeaders();
  ndisp = sp->displayedTraces();

  if(ndisp <= 0 || hd == NULL) return rtn;
  hdr= _pheader;
  val= pkey;
  if(hdr == NILKEY || hdr > nhdrs) return rtn;
  memstrt= sp->firstTrace() + sp->currentFrame()*sp->originalTraces();
  memend = ndisp -1 + memstrt;
  p1 = hd[(memstrt-1)*nhdrs+hdr-1];
/**********************************************
 *   Special case - one trace plotted        */
  if(ndisp==1)
   {rtn = 1.0;
    if(p1>val) rtn = -1.0;
    if(p1<val) rtn = (float) (ndisp+1);
    return rtn;
   }
/**********************************************
 * Hunt thru traces until pkey is bracketed  */
  tr1=memstrt;
  ftn=(float) memstrt;
  for(i=memstrt;i<memend;i++)
   { tr2= i+1;
     ftn= (float ) (tr2);
     p2 = hd[i*nhdrs+hdr-1];
     if((p1-val)*(p2-val) <= 0.)
       { ftn = (float) tr1 + ((val-p1)/(p2-p1));
         goto jump;
       }
      p1 = p2;
      tr1= tr2;
   }
jump:
 if(i == memend)
  {/* linearly extrapolate trace number */
   p1 = hd[(memstrt-1)*nhdrs+hdr-1];
   p2 = hd[(memend-1)*nhdrs+hdr-1];
   if(p1==p2) return rtn;
//   rtn = 1.0 + ( ndisp - 1.)*((val-p1)/(p2-p1));
   rtn = memstrt + ( memend - memstrt)*((val-p1)/(p2-p1));
   return rtn;
  }
 else
  {/* linearly interpolate trace number */
   rtn1 = tr1; //get_reltrace(sp,tr1);
   rtn2 = tr2; //get_reltrace(sp,tr2);
   if(rtn1==0 || rtn2==0)
    {rtn = -1.0;}
   else
    rtn  = rtn1 + (ftn-tr1)/(tr2-tr1) *(rtn2-rtn1);
   return rtn;
  }
}

float SeisTransf:: KeyToTrace(SeisPlot *sp, float key, int header)
{/* Map a key value to a memory trace number
    Does interpolation between traces 
    return -1.0 when zero traces or no match */
  const float *hd;
  int  i,nhdrs,ndisp;
  int  memstrt,memend;
  float ftn=-1.0,rtn=-1.0;
  float val=key,p1,p2;
  int  tr1,tr2,rtn1,rtn2;
  int   hdr=header;
  ftn = -1.0; /* memory index */
  rtn = -1.0; /* display index */
  if(sp == NULL) return rtn;
  hd    = sp->headers();
  nhdrs = (int) sp->numHeaders();
  ndisp = (int) sp->displayedTraces();

  if(ndisp <= 0 || hd == NULL) return rtn;
  if(hdr == NILKEY || hdr > nhdrs) return rtn;
  memstrt= (int) (sp->firstTrace() + sp->currentFrame()*sp->originalTraces());
  memend = ndisp -1 + memstrt;
  p1 = hd[(memstrt-1)*nhdrs+hdr-1];
/**********************************************
 *   Special case - one trace plotted        */
  if(ndisp==1)
   {rtn = 1.0;
    if(p1>val) rtn = -1.0;
    if(p1<val) rtn = (float) (ndisp+1);
    return rtn;
   }
/**********************************************
 * Hunt thru traces until key is bracketed   */
  tr1=memstrt;
  ftn=(float) memstrt;
  for(i=memstrt;i<memend;i++)
   { tr2= i+1;
     ftn= (float ) (tr2);
     p2 = hd[i*nhdrs+hdr-1];
     if((p1-val)*(p2-val) <= 0.)
       { ftn = (float) tr1 + ((val-p1)/(p2-p1));
         goto jump;
       }
      p1 = p2;
      tr1= tr2;
   }
jump:
 if(i == memend)
  {/* linearly extrapolate trace number */
   p1 = hd[(memstrt-1)*nhdrs+hdr-1];
   p2 = hd[(memend-1)*nhdrs+hdr-1];
   if(p1==p2) return rtn;
//   rtn = 1.0 + ( ndisp - 1.)*((val-p1)/(p2-p1));
   rtn = memstrt + ( memend - memstrt)*((val-p1)/(p2-p1));
   return rtn;
  }
 else
  {/* linearly interpolate trace number */
   rtn1 = tr1;
   rtn2 = tr2;
   if(rtn1==0 || rtn2==0)
    {rtn = -1.0;}
   else
    rtn  = rtn1 + (ftn-tr1)/(tr2-tr1) *(rtn2-rtn1);
   return rtn;
  }
}

int SeisTransf::goodKey(SeisPlot *sp)
{// determine which header is changing
 int    key=_pheader,mems,meme;
 int    nhd,ndisp;
 const  float *hd=NULL;
 float  p1,p2;

 if(sp == NULL) return key;
 hd    = sp->headers();
 nhd   = (int) sp->numHeaders();
 ndisp = (int) sp->displayedTraces();

 if(ndisp <= 0 || hd == NULL) return key;
 mems = (int) (sp->firstTrace() + sp->currentFrame()*sp->originalTraces());
 meme = ndisp -1 + mems;
 if(_sheader != NILKEY) {
  p1 = hd[(mems-1)*nhd+ _sheader-1];
  p2 = hd[(meme-1)*nhd+ _sheader-1];
  if(fabs(p2-p1) > _tol) key = _sheader;
 }
 if(_pheader != NILKEY) {
  p1 = hd[(mems-1)*nhd+ _pheader-1];
  p2 = hd[(meme-1)*nhd+ _pheader-1];
  if(fabs(p2-p1) > _tol) key = _pheader;
 }

 return key;
}

long SeisTransf:: get_memtrace(SeisPlot *sp, long rel_trno)
{ /* given the relative trace number-get the trace index in memory */
  long memtn;
  long cframe;
  if(sp==NULL) return 0;
  if(rel_trno < 1) rel_trno = 1;
  if(rel_trno > sp->displayedTraces()) rel_trno = sp->displayedTraces();
  cframe = sp->movie() ? sp->currentFrame() : 0;
  if(cframe< 0) cframe=0;
  memtn =   rel_trno +
          (sp->firstTrace() - 1 + cframe*sp->originalTraces());
/*
  if(! sp->rToL())
   {memtn = rel_trno  +
             (sp->firstTrace() - 1 + cframe*sp->originalTraces());
   }
  else
   {memtn = (sp->displayedTraces() + 1) - rel_trno  +
             (sp->firstTrace() - 1 + cframe*sp->originalTraces());
   }
*/

  return memtn;
}

long  SeisTransf:: get_reltrace(SeisPlot *sp, long memtn)
{ /* given the trace index in memory-get the relative trace number */
  long  inmem,rel_trno;
  long  frame_count,cframe;
  if(sp == NULL) return 0;
  frame_count= sp->movie() ? sp->frames() : 1;
  cframe = sp->movie() ? sp->currentFrame() : 0;
  inmem = frame_count*sp->originalTraces();
  if(memtn < 1) memtn = 1;
  if(memtn > inmem) memtn = inmem;
    rel_trno = memtn -
          (sp->firstTrace() - 1 + cframe*sp->originalTraces());
/*
  if(! sp->rToL())
   {
    rel_trno = memtn -
          (sp->firstTrace() - 1 + cframe*sp->originalTraces());
   }
  else
   {
    rel_trno = (sp->displayedTraces() + 1) - memtn +
            (sp->firstTrace() - 1 + cframe*sp->originalTraces());
   }
*/

  return rel_trno;
}

/* following are c wrappers around some c++ functions */
void *SeisTransfConnectToSP(void *sp, int phd, int shd, int thd)
{SeisPlot *seisplot;
 SeisTransf *sptrans;
 if(sp==NULL) return NULL;
 seisplot = (SeisPlot *) sp;
 sptrans = new SeisTransf(phd,shd,thd);
 seisplot->setTransform(sptrans);
 return (void *) sptrans;
}

void *SeisTransfXToSP(void *sp, int phd, int shd, int thd,
      void *tzs,void *tzd)
{SeisPlot *seisplot;
 SeisTransf *sptrans;
 if(sp==NULL) return NULL;
 seisplot = (SeisPlot *) sp;
 sptrans = new SeisTransf(phd,shd,thd,tzs,tzd);
 if(seisplot->transform()) delete seisplot->transform();
 seisplot->setTransform(sptrans);
 return (void *) sptrans;
}

void set_tz(void *stransf, void *tzseis,void *tzdata)
{SeisTransf *stf;
 if(stransf == NULL) return;
 stf = (SeisTransf *) stransf;
 stf->setTZvals(tzseis,tzdata);
}

void *get_tzsplot(void *stransf)
{SeisTransf *stf;
 if(stransf == NULL) return NULL;
 stf = (SeisTransf *) stransf;
 return (void *) stf->getTZsplot();
}

void *get_tzdata(void *stransf)
{SeisTransf *stf;
 if(stransf == NULL) return NULL;
 stf = (SeisTransf *) stransf;
 return (void *) stf->getTZdata();
}
