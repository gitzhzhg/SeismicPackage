//////// DO NOT EDIT THIS FILE - it is machine generated ////////

#include "CpseisMasker.hh"
#include <jni.h>

extern "C"
{
  JNIEXPORT jlong JNICALL Java_org_cpseis_wrappers_CpseisMasker_nativeCreate
    (JNIEnv *env, jobject obj)
  {
    CpseisMasker *module = new CpseisMasker();
    return (jlong)module;
  }
}