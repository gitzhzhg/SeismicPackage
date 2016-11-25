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
#ifndef SeisTransf_H
#define SeisTransf_H


#include "sp/sp_transform.hh"


class SeisTransf : public SPTransform
{
  public:
   SeisTransf(int phd,int shd, int thd);
   SeisTransf(int phd,int shd, int thd,void *tzs, void *tzd);
   virtual ~SeisTransf();
   float convDataToXOffset(SeisPlot *sp, float data);
   float convXOffsetToData(SeisPlot *sp, float offset);
   long  TraceToKeys(SeisPlot *sp, float rel_trno, float *pkey,
          float *skey, float *tkey);
   float PKeyToTrace(SeisPlot *sp, float pkey);
   float KeyToTrace(SeisPlot *sp, float key, int header);
   int   goodKey(SeisPlot *sp);
   long  get_memtrace(SeisPlot *sp, long rel_trno);
   long  get_reltrace(SeisPlot *sp, long mem_trno);
   void     setTZvals(void *tzs,void *tzd)
             { _seis_tz=tzs; _data_tz = tzd; }
   void    *getTZsplot(){ return _seis_tz; }
   void    *getTZdata() { return _data_tz; }
   void  setphd(int phd) { _pheader = phd; }
   void  setshd(int shd) { _sheader = shd; }
   void  setthd(int thd) { _theader = thd; }
   int   getphd() { return _pheader; }
   int   getshd() { return _sheader; }
   void  settol(float tol) { _tol=tol; }
   float tol() { return _tol; }
 
 protected:

   int   _pheader;
   int   _sheader;
   int   _theader;
   void *_seis_tz;
   void *_data_tz;
   float _tol;


};

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
void *SeisTransfConnectToSP(void *sp, int phd, int shd, int thd);
void *SeisTransfXToSP(void *sp, int phd, int shd, int thd,void *tzs, void *tzd);
void  set_tz(void *stransf, void *tzseis,void *tzdata);
void *get_tzsplot(void *stransf);
void *get_tzdata(void *stransf);
#ifdef __cplusplus
}                   // for C++
#endif

#endif

