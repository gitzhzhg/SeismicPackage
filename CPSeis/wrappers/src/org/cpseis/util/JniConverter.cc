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
//----------------------- JniConverter.cc ----------------------------------//
//----------------------- JniConverter.cc ----------------------------------//
//----------------------- JniConverter.cc ----------------------------------//
 
#include "JniConverter.hh"
#include "GTW.hh"
#include "named_constants.h"
#include <string.h>
#include <assert.h>

//---------------------------- static functions ------------------------//
//---------------------------- static functions ------------------------//
//---------------------------- static functions ------------------------//

static void static_assert()
{
  assert(sizeof(jint)    == sizeof(int));         // to simplify coding.
  assert(sizeof(jdouble) == sizeof(double));      // to simplify coding.
  assert(sizeof(jfloat)  == sizeof(float));       // to simplify coding.
  assert(sizeof(jsize)   == sizeof(int));         // to simplify coding.

  // to guarantee that JniConverter::LENGTH is an integral number of integer words:
  assert(JniConverter::LENGTH == sizeof(int) * (JniConverter::LENGTH / sizeof(int)));
}

//---------------------------- get array ----------------------------------//
//---------------------------- get array ----------------------------------//
//---------------------------- get array ----------------------------------//

char *JniConverter::getStringArray  (JNIEnv *env, const jobjectArray array,
                                     int *nelements, int *nsize, int minsize)
{
  static_assert();

       *nelements = (int)env->GetArrayLength(array);
  int   nsize2    = MaximumValue(*nelements, minsize);
  char *buffer    = new char [nsize2 * LENGTH + 1];
  for(int i = 0; i < *nelements; i++)
      {
      jstring     element  = (jstring)env->GetObjectArrayElement(array, i);
      const char *element9 = env->GetStringUTFChars(element, 0);
      strcpy(&buffer[i * LENGTH], element9);
      env->ReleaseStringUTFChars(element, element9);
      env->DeleteLocalRef(element);
      }
  if(nsize) *nsize = nsize2;
  return buffer;
}


int    *JniConverter::getIntArray     (JNIEnv *env, const jintArray array,
                                       int *nelements, int *nsize, int minsize)
{
       *nelements = (int)env->GetArrayLength(array);
  int   nsize2    = MaximumValue(*nelements, minsize);
  int  *buffer    = new int [nsize2 + 1];
  env->GetIntArrayRegion(array, 0, *nelements, buffer);
  if(nsize) *nsize = nsize2;
  return buffer;
}


float  *JniConverter::getFloatArray   (JNIEnv *env, const jfloatArray array,
                                       int *nelements, int *nsize, int minsize)
{
        *nelements = (int)env->GetArrayLength(array);
  int    nsize2    = MaximumValue(*nelements, minsize);
  float *buffer    = new float [nsize2 + 1];
  env->GetFloatArrayRegion(array, 0, *nelements, buffer);
  if(nsize) *nsize = nsize2;
  return buffer;
}


double *JniConverter::getDoubleArray  (JNIEnv *env, const jdoubleArray array,
                                       int *nelements, int *nsize, int minsize)
{
         *nelements = (int)env->GetArrayLength(array);
  int     nsize2    = MaximumValue(*nelements, minsize);
  double *buffer    = new double [nsize2 + 1];
  env->GetDoubleArrayRegion(array, 0, *nelements, buffer);
  if(nsize) *nsize = nsize2;
  return buffer;
}


int    *JniConverter::getBooleanArray (JNIEnv *env, const jbooleanArray array,
                                       int *nelements, int *nsize, int minsize)
{
           *nelements = (int)env->GetArrayLength(array);
  int       nsize2    = MaximumValue(*nelements, minsize);
  int      *buffer    = new int      [nsize2 + 1];
  jboolean *buffer9   = new jboolean [nsize2 + 1];
  env->GetBooleanArrayRegion(array, 0, *nelements, buffer9);
  for(int i = 0; i < *nelements; i++) { buffer[i] = (int)buffer9[i]; }
  delete [] buffer9;
  if(nsize) *nsize = nsize2;
  return buffer;
}


GTW *JniConverter::getGTW (JNIEnv *env, const jobject obj)
{
//jclass         clazz                 = env->GetObjectClass (obj);     // equivalent to the next line.
  jclass         clazz                 = env->FindClass      ("org/cpseis/util/GridTransform");

//----------this method returns a C++ pointer to the original C++ object owned by the java object:

  jmethodID      id_get_pointer        = env->GetMethodID    (clazz, "getPointer"      , "()J");
  long pointer = env->CallLongMethod (obj, id_get_pointer);
  return (GTW*)pointer;

//----------this method returns a C++ pointer to a new C++ object which is a copy of the
//          original C++ object owned by the java object:

/*
  jmethodID      id_get_xorigin        = env->GetMethodID    (clazz, "getXorigin"      , "()D");
  jmethodID      id_get_yorigin        = env->GetMethodID    (clazz, "getYorigin"      , "()D");
  jmethodID      id_get_rotation_angle = env->GetMethodID    (clazz, "getRotationAngle", "()D");
  jmethodID      id_get_xgrid_width    = env->GetMethodID    (clazz, "getXgridWidth"   , "()D");
  jmethodID      id_get_ygrid_width    = env->GetMethodID    (clazz, "getYgridWidth"   , "()D");
  jmethodID      id_get_handedness     = env->GetMethodID    (clazz, "getHandedness"   , "()I");
printf("%ld %ld %ld %ld %ld %ld\n", (long)id_get_xorigin, (long)id_get_yorigin, (long)id_get_rotation_angle,
                              (long)id_get_xgrid_width, (long)id_get_ygrid_width, (long)id_get_handedness);
  GTW *grid = new GTW();
  grid->setXorigin       (env->CallDoubleMethod (obj, id_get_xorigin       ));
  grid->setYorigin       (env->CallDoubleMethod (obj, id_get_yorigin       ));
  grid->setRotationAngle (env->CallDoubleMethod (obj, id_get_rotation_angle));
  grid->setXgridWidth    (env->CallDoubleMethod (obj, id_get_xgrid_width   ));
  grid->setYgridWidth    (env->CallDoubleMethod (obj, id_get_ygrid_width   ));
  grid->setHandedness    (env->CallIntMethod    (obj, id_get_handedness    ));
  return grid;
*/
}

//---------------------------- put array ----------------------------------//
//---------------------------- put array ----------------------------------//
//---------------------------- put array ----------------------------------//

jobjectArray  JniConverter::putStringArray  (JNIEnv *env, const char   *buffer, int nelements)
{
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("xxx");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(nelements, element_class, initial_element);
  for(int i = 0; i < nelements; i++)
      {
      jstring element = env->NewStringUTF(&buffer[i * LENGTH]);
      env->SetObjectArrayElement(new_array, i, element);
      env->DeleteLocalRef(element);
      }
  return new_array;
}


jintArray     JniConverter::putIntArray     (JNIEnv *env, const int    *buffer, int nelements)
{
  jintArray new_array = env->NewIntArray(nelements);
  env->SetIntArrayRegion(new_array, 0, nelements, (jint*)buffer);
  return new_array;
}


jfloatArray   JniConverter::putFloatArray   (JNIEnv *env, const float  *buffer, int nelements)
{
  jfloatArray new_array = env->NewFloatArray(nelements);
  env->SetFloatArrayRegion(new_array, 0, nelements, (jfloat*)buffer);
  return new_array;
}


jdoubleArray  JniConverter::putDoubleArray  (JNIEnv *env, const double *buffer, int nelements)
{
  jdoubleArray new_array = env->NewDoubleArray(nelements);
  env->SetDoubleArrayRegion(new_array, 0, nelements, (jdouble*)buffer);
  return new_array;
}


jbooleanArray JniConverter::putBooleanArray (JNIEnv *env, const int    *buffer, int nelements)
{
  jbooleanArray new_array = env->NewBooleanArray(nelements);
  jboolean *buffer9 = new jboolean [nelements + 1];
  for(int i = 0; i < nelements; i++) { buffer9[i] = (jboolean)buffer[i]; }
  env->SetBooleanArrayRegion(new_array, 0, nelements, buffer9);
  delete [] buffer9;
  return new_array;
}


/////// not used?

jobject JniConverter::putGTW (JNIEnv *env, const class GTW *grid)
{
  jclass    clazz                 = env->FindClass   ("org/cpseis/util/GridTransform");
  jmethodID id_constructor        = env->GetMethodID (clazz, "<init>"          , "()V");
  jmethodID id_set_xorigin        = env->GetMethodID (clazz, "setXorigin"      , "(D)V");
  jmethodID id_set_yorigin        = env->GetMethodID (clazz, "setYorigin"      , "(D)V");
  jmethodID id_set_rotation_angle = env->GetMethodID (clazz, "setRotationAngle", "(D)V");
  jmethodID id_set_xgrid_width    = env->GetMethodID (clazz, "setXgridWidth"   , "(D)V");
  jmethodID id_set_ygrid_width    = env->GetMethodID (clazz, "setYgridWidth"   , "(D)V");
  jmethodID id_set_handedness     = env->GetMethodID (clazz, "setHandedness"   , "(I)V");
/*
printf("%ld %ld %ld %ld %ld %ld %ld\n", (long)id_constructor, (long)id_set_xorigin, (long)id_set_yorigin,
                                         (long)id_set_rotation_angle,
                              (long)id_set_xgrid_width, (long)id_set_ygrid_width, (long)id_set_handedness);
*/
  jobject   obj                   = env->NewObject   (clazz, id_constructor);
  env->CallVoidMethod (obj, id_set_xorigin       , grid->getXorigin       ());
  env->CallVoidMethod (obj, id_set_yorigin       , grid->getYorigin       ());
  env->CallVoidMethod (obj, id_set_rotation_angle, grid->getRotationAngle ());
  env->CallVoidMethod (obj, id_set_xgrid_width   , grid->getXgridWidth    ());
  env->CallVoidMethod (obj, id_set_ygrid_width   , grid->getYgridWidth    ());
  env->CallVoidMethod (obj, id_set_handedness    , grid->getHandedness    ());
  return obj;
}

//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
