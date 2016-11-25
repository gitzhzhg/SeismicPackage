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
#ifndef _hdrio_h
#define _hdrio_h

#include "cubeio/defs.hh"

class DataIO;

//- HdrIO
class HdrIO {
   //. Manage IO for trace headers when headers seperate from traces.

public:
   HdrIO(FILE *f, char *name, int esize, int n2, int n3, long off, bool close);
      //. Creates a HdrIO from a given file pointer.
   HdrIO(char *name, int off);
      //. Creates a HdrIO which creates its own descriptor.
  ~HdrIO();

   int  hread(int traceno, int num, char *headers);
   int  hread(int i1, int i2, int num, char *headers);
   int  hwrite(int traceno, int num, char *headers);
   int  hwrite(int i1, int i2, int num, char *headers);
   void print();

   long offset();
   void set_offset(long offset);
   int  esize();
   void set_esize(int esize);
   int  etype();
   void set_etype(int etype);
   int  nhdwd();
   void set_nhdwd(int nhdwd);
   DataIO *f() {return fio_;}
   char *name();

protected:
   int n1_;
   int n2_;
   int n3_;
   int esize_;
   int etype_;
   DataIO *fio_;         // Handles the IO for the headers
private:

};

/*____________________________________________________________________________*/



#endif




