#include <stdio.h>
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
#include "cubeio/hdrio.hh"
#include "cubeio/dataio.hh"

// Source code for handling storage of trace headers

HdrIO::HdrIO(FILE *f, char *name, int esize, int n2, int n3,
 long offs, bool close): n1_(64),n2_(n2),n3_(n3) {

 fio_=0;
 esize_ = esize;
 etype_ = 1;

 if(f) fio_ = new DataIO(f,name, close);
 if(fio_) fio_->set_offset(offs);
}

HdrIO::~HdrIO() {
 if(fio_) delete fio_;
}

int HdrIO::hread(int seqno, int num, char *hdbuf) {
 int offset, ntord;
 if(!fio_) return 0;
 
 offset = seqno * n1_*esize_;
 ntord  = num*esize_*n1_;
 fio_->seek(offset);
 return fio_->read(hdbuf,ntord);
}

int HdrIO::hread(int i2, int i3 , int num, char *hdbuf) {
 int seqno, offset, ntord;
 if(!fio_) return 0;
 
 seqno = i2 + i3*n2_;
 offset = seqno * n1_*esize_;
 ntord  = num*esize_*n1_;
 fio_->seek(offset);
 return fio_->read(hdbuf,ntord);
}

int HdrIO::hwrite(int seqno , int num, char *hdbuf) {
 int offset, ntowr;
 if(!fio_) return 0;
 
 offset = seqno * n1_*esize_;
 ntowr  = num*esize_*n1_;
 fio_->seek(offset);
 return fio_->write(hdbuf,ntowr);
}

int HdrIO::hwrite(int i2, int i3 , int num, char *hdbuf) {
 int seqno, offset, ntowr;
 if(!fio_) return 0;
 
 seqno = i2 + i3*n2_;
 offset = seqno * n1_*esize_;
 ntowr  = num*esize_*n1_;
 fio_->seek(offset);
 return fio_->write(hdbuf,ntowr);
}

int HdrIO::nhdwd() {
 return n1_;
}
void HdrIO::set_nhdwd(int val) {
 n1_ = val;
}

int HdrIO::esize() {
 return esize_;
}
void HdrIO::set_esize(int val) {
 esize_=val;
}

char *HdrIO::name() {
 if(fio_) return fio_->name(); else return 0;
}

int HdrIO::etype() {
 return etype_;
}
void HdrIO::set_etype(int val) {
 etype_=val;
}

long HdrIO::offset() {
 return fio_->offset();
}
void HdrIO::set_offset(long offset) {
 fio_->set_offset(offset);
}

void HdrIO::print() {
 printf("------ Header File Summary ---------\n");
 printf("HdrIO: element size=%d\n",esize_);
 printf("HdrIO: header words=%d\n",n1_);
 printf("HdrIO: Inline size=%d\n",n2_);
 printf("HdrIO:  Xline size=%d\n",n3_);
 printf("HdrIO: header offset=%d\n",fio_->offset());
}


