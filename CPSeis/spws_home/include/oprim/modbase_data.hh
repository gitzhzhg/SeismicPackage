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
#ifndef _MODBASE_DATA_HH
#define _MODBASE_DATA_HH

#include <X11/Xlib.h>
#include "oprim/base_data.hh"
#include "transform.h"

#define NILKEY 0
#define NILVAL .12345


class ModBaseData : public BaseData
{
	public:

                enum DataComp
                {primary, secondary, tertiary, time};

		ModBaseData(int numPts, int phd, float *pv,
                 int shd, float *sv,
                 int thd, float *tv,float *y,void *user= 0,
                 long ident=defaultId);
		ModBaseData(int numPts, int phd, float *pv,
                 int shd, float *sv,
                 int thd, float *tv,int zeit, float *y,void *user= 0,
                 long ident=defaultId);
		ModBaseData(ModBaseData *dobj);
		virtual ~ModBaseData();

                void setIdent(long ident) { _ident = ident; }
                long getIdent() { return _ident; }
		int  getNumPts(long /*ident*/=defaultId){return _numPts;}

                float getX(int i,long ident = defaultId);
                float getY(int i,long ident = defaultId);
                float getS(int i,long ident = defaultId);
                float getT(int i);
                float getUser(int i);
                int   getNx(int i, int num, float *out,
                      long ident=defaultId);
                int   getNy(int i,int num, float *out,
                      long ident=defaultId);
                int   getNs(int i,int num, float *out,
                      long ident=defaultId);
                int   getNt(int i,int num, float *out);
                int   getNuser(int i,int num, float *out);

		void insert(int index, int numIns, float *x, float *y,
		 float *s, float *t,void *user);
		void remove(int index, int numRem);
		void remove(int *index, int numRem);
		void replace(int index, int numRep, float *x, float *y,
                 float *s, float *t, void *user);
		void replace(int index, int numRep, float *x, float *y);
		void replace(int index, int numRem, int numIns,
	         float *x, float *y, float *s, float *t,
                 void *user);
                int trans_data_comps(ErsTransforms *data,
                    char *p, char *s, char *t, char *zeit);
                int trans_data_comp(int comp, ErsTransforms *t, char *o);

                Bool isUserData(); 

		int  getphdr(){return _phdr;}
		int  getshdr(){return _shdr;}
		int  getthdr(){return _thdr;}
                int  getzeit(){ return _zeit; }
		void setphdr(int header);
		void setshdr(int header);
		void setthdr(int header);
                void setzeit(int zeit)   { _zeit = zeit; }
		int  getnumkeys(){return _numkeys;}
                int  getid() { return (int) _id; }
                void setid(int id) { _id = id; }
                void getpt(float *x, float *z) { *x = _xo; *z = _yo; }
                void getpt(float *x, float *z, float *s)
                  { *x = _xo; *z = _yo; *s = _so; }
                void setpt(float x, float z) { _xo=x; _yo=z; }
                void setpt(float x, float z, float s)
                  { _xo=x; _yo=z; _so= s; }

	protected:

		int   _phdr;
		int   _shdr;
		int   _thdr;
                int   _zeit;
		int   _numkeys;
                int   _numPts;
                int   _has_user_data;
                long  _ident;
                float *_xData;
                float *_yData;
                float *_sData;
                float *_tData;
                float *_user_data;
                int   _id;
                float _xo;
                float _yo;
                float _so;

		void initialize(int numPts,
                 int phd, float *pv,
                 int shd, float *sv,
                 int thd, float *tv,
                 int zeit, float *y,void *user);
                int init_data(int numPts,
                 float *pv, float *sv, float *tv,float *z, void *user);
                int trans_data_axis(int comp, ErsTransforms *td, char *o);

                void sethdrs(int phdr,int shdr,int thdr);

};

#endif
