//------------------------- CpsBase_jni.cpp ---------------------------//
//------------------------- CpsBase_jni.cpp ---------------------------//
//------------------------- CpsBase_jni.cpp ---------------------------//

#include "org_cpseis_util_CpseisBase.h"
#include "CpseisBase.hh"
#include "named_constants.h"
#include <stdio.h>
#include <assert.h>

//---------------------------- create -------------------------------//
//---------------------------- create -------------------------------//
//---------------------------- create -------------------------------//

JNIEXPORT jlong JNICALL Java_org_cpseis_util_CpseisBase_nativeCreate
  (JNIEnv *env, jobject obj)
{
  // This method is overridden by subclasses of CpseisBase.
  return 0;
}

//-------------------------- destroy -----------------------------//
//-------------------------- destroy -----------------------------//
//-------------------------- destroy -----------------------------//

JNIEXPORT void JNICALL Java_org_cpseis_util_CpseisBase_nativeDestroy
  (JNIEnv *env, jobject obj, jlong pointer)
{
  if(!pointer) return;
  CpseisBase *module = (CpseisBase*)pointer;
  delete module;
}

//--------------------------- update -----------------------------//
//--------------------------- update -----------------------------//
//--------------------------- update -----------------------------//

JNIEXPORT void JNICALL Java_org_cpseis_util_CpseisBase_nativeUpdate
  (JNIEnv *env, jobject obj, jlong pointer)
{
  if(!pointer) return;
  CpseisBase *module = (CpseisBase*)pointer;
  module->update();
}

//--------------------------- wrapup -----------------------------//
//--------------------------- wrapup -----------------------------//
//--------------------------- wrapup -----------------------------//

JNIEXPORT void JNICALL Java_org_cpseis_util_CpseisBase_nativeWrapup
  (JNIEnv *env, jobject obj, jlong pointer)
{
  if(!pointer) return;
  CpseisBase *module = (CpseisBase*)pointer;
  module->wrapup();
}

//-------------------------- put trace -----------------------------//
//-------------------------- put trace -----------------------------//
//-------------------------- put trace -----------------------------//

JNIEXPORT void JNICALL Java_org_cpseis_util_CpseisBase_nativePutTrace
  (JNIEnv *env, jobject obj, jlong pointer, jint itr,
   jdoubleArray hd, jfloatArray tr)
{
  if(!pointer) return;
  jdouble *hd_elems = env->GetDoubleArrayElements (hd, 0);
  jfloat  *tr_elems = env->GetFloatArrayElements  (tr, 0);

  CpseisBase *module = (CpseisBase*)pointer;
  module->putTrace(itr, hd_elems, tr_elems);

  env->ReleaseDoubleArrayElements (hd, hd_elems, 0);
  env->ReleaseFloatArrayElements  (tr, tr_elems, 0);
}

//-------------------------- get trace -----------------------------//
//-------------------------- get trace -----------------------------//
//-------------------------- get trace -----------------------------//

JNIEXPORT void JNICALL Java_org_cpseis_util_CpseisBase_nativeGetTrace
  (JNIEnv *env, jobject obj, jlong pointer, jint itr,
   jdoubleArray hd, jfloatArray tr)
{
  if(!pointer) return;
  jdouble *hd_elems = env->GetDoubleArrayElements (hd, 0);
  jfloat  *tr_elems = env->GetFloatArrayElements  (tr, 0);

  CpseisBase *module = (CpseisBase*)pointer;
  module->getTrace(itr, hd_elems, tr_elems);

  env->ReleaseDoubleArrayElements (hd, hd_elems, 0);
  env->ReleaseFloatArrayElements  (tr, tr_elems, 0);
}

//--------------------------- execute ------------------------------//
//--------------------------- execute ------------------------------//
//--------------------------- execute ------------------------------//

JNIEXPORT jint JNICALL Java_org_cpseis_util_CpseisBase_nativeExecute
  (JNIEnv *env, jobject obj, jlong pointer, jint ntr)
{
  if(!pointer) return FATAL_ERROR;

  CpseisBase *module = (CpseisBase*)pointer;
  return module->execute(ntr);
}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
