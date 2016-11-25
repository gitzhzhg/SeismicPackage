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
/* 
 * System include **must** be in front for namespace reasons
 */
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "cubeio/dataio.hh"

/****************************************************************************/
DataIO::DataIO(int  fd, char *name, char *mode, bool close) :
 close_(close), fd_(fd)  {
 offs_=0;
 name_ = 0;
 if(fd<1) return;
 if(name) {
  name_ = (char *) malloc(strlen(name)+1);
  strcpy(name_,name);
 }
 f_ = fdopen(fd_,mode);
}

DataIO::DataIO(FILE *f, char *name, bool close) :
 close_(close), f_(f)  {
 offs_=0;
 name_ = 0;
 fd_ = fileno(f_);
 if(fd_<0) return;
 if(name) {
  name_ = (char *) malloc(strlen(name)+1);
  strcpy(name_,name);
 }
}


DataIO::~DataIO() {
   if (f_ && close_) close(fd_);
   if(name_) free(name_);
}

bool DataIO::open() const {
    return (f_!=0);
}

int DataIO::flush() {
  return f_ ? fflush(f_) : EOF ;
}

void DataIO::wait() {
//   return f_ ? fflush(f_) : EOF ;
  return;
}

int DataIO::read(char* ptr, int n) {
    return f_ ? fread(ptr,sizeof(char),n,f_) : 0 ;
}

int DataIO::write(const char* ptr, int n) {
   return f_ ? fwrite(ptr,sizeof(char),n,f_) : 0 ;
}

int DataIO::seek(long offset) {
   return f_ ? fseek(f_,offset+offs_,0) : 1 ;
}

int DataIO::seek_end() {
   return f_ ? fseek(f_,0,2) : 1 ;
}

long DataIO::tell() const {
   return f_ ? ftell(f_) - offs_ : -1 ;
}

int DataIO::fd() const {
  return f_ ? fileno(f_) : -1 ;
}

FILE *DataIO::f() const {return f_; }

long DataIO::length()  {
   long now = tell();
   if (seek_end() != 0) return -1;
   long end = tell();
   seek(now);
   return end;
}

char *DataIO::name() {
  return name_;
}

void DataIO::set_offset(long offs) {
   if (f_) offs_ = offs;
}

long DataIO::offset() {
   return offs_;
}

/****************************************************************************/
/*
void DataFileIO::set_data_io_formater(DataIOFormater *f) {
    if( f_ != f ) {
       delete f_ ;
       f_ = f ;
    }
}


bool DataFileIO::same(const DataFileIO &f) {
   int fd1 = f.f_-> fd() ;
   int fd2 = f_-> fd() ;
   struct stat fs1, fs2 ;
   fstat(fd1,&fs1) ;
   fstat(fd2,&fs2) ;
   return fs1.st_ino == fs2.st_ino ;
}
*/


