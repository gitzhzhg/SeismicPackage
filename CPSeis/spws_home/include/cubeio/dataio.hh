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
#ifndef dataio_h
#define dataio_h

#include "cubeio/defs.hh"
#include <stdio.h>
#include <stdlib.h>
#ifdef CRAY
#endif



#ifdef __cplusplus
extern "C" {                 // for C++
#endif

#ifdef __cplusplus
}                   // for C++
#endif


//- DataIO
class DataIO {
   //. A class to handle IO requests. Provides a uniform interface for
   //. read and write requests.
public:
   DataIO(int fd, char * name, char *mode, bool close);
   DataIO(FILE *f, char *name, bool close);
    //. Creates a DataIO. IO is done using the FILE pointer.
    //. File Descriptor is converted to a FILE pointer.
  ~DataIO();

   virtual bool open() const ;
    //. Indicates whether file is open or not. Does not do an open!
   virtual int  flush() ;
   virtual void wait();

   virtual int  read(char*, int) ;
   virtual int  write(const char*, int) ;
   virtual int  seek(long);
   virtual int  seek_end() ;
   virtual long tell() const;

   virtual int  fd() const ;
   virtual FILE *f() const;

   long offset();
   void set_offset(long);
    //. Add offset to all seek() requests.

   virtual long length() ;
   char *name();

//   virtual bool same(const DataFileIO &) ;


//   void set_data_io_formater(DataIOFormater *) ;

protected:
   FILE *f_;
   int   fd_;
   char *name_;
   bool  close_;
   long  offs_;

private:

};

/*____________________________________________________________________________*/

#endif
