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
//--------------------------- JniConverter.hh --------------------------//
//--------------------------- JniConverter.hh --------------------------//
//--------------------------- JniConverter.hh --------------------------//

#ifndef _JNI_CONVERTER_HH_
#define _JNI_CONVERTER_HH_

#include <jni.h>

class JniConverter
{

//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//

public:

  enum { LENGTH = 160 };   // number of bytes for each string element in
                           // an array of strings (same as PCW::LENGTH).

//---------------------------- get array ----------------------------------//
//---------------------------- get array ----------------------------------//
//---------------------------- get array ----------------------------------//

public:   // these return an allocated array.
          // the returned array will have space for at least minsize elements.
          // these set nelements = number of elements in the array.
          // these set nsize = maximum number of elements the array can hold.
          // the returned array must be deleted when no longer needed.

  static char   *getStringArray  (JNIEnv *env, const jobjectArray array,
                                  int *nelements, int *nsize = NULL,
                                  int minsize = 0);

  static int    *getIntArray     (JNIEnv *env, const jintArray array,
                                  int *nelements, int *nsize = NULL,
                                  int minsize = 0);

  static float  *getFloatArray   (JNIEnv *env, const jfloatArray array,
                                  int *nelements, int *nsize = NULL,
                                  int minsize = 0);

  static double *getDoubleArray  (JNIEnv *env, const jdoubleArray array,
                                  int *nelements, int *nsize = NULL,
                                  int minsize = 0);

  static int    *getBooleanArray (JNIEnv *env, const jbooleanArray array,
                                  int *nelements, int *nsize = NULL,
                                  int minsize = 0);

  static class GTW *getGTW (JNIEnv *env,
                                                const jobject scalar);

//---------------------------- put array ----------------------------------//
//---------------------------- put array ----------------------------------//
//---------------------------- put array ----------------------------------//

public:   // these return a java array.

  static jobjectArray  putStringArray
                            (JNIEnv *env, const char   *buffer, int nelements);
  static jintArray     putIntArray
                            (JNIEnv *env, const int    *buffer, int nelements);
  static jfloatArray   putFloatArray
                            (JNIEnv *env, const float  *buffer, int nelements);
  static jdoubleArray  putDoubleArray
                            (JNIEnv *env, const double *buffer, int nelements);
  static jbooleanArray putBooleanArray
                            (JNIEnv *env, const int    *buffer, int nelements);

  static jobject       putGTW
                            (JNIEnv *env, const class GTW *grid);

//--------------------------- end of class ----------------------------//
//--------------------------- end of class ----------------------------//
//--------------------------- end of class ----------------------------//

};

#endif

//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
