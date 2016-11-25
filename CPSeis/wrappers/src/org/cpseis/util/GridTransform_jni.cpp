//------------------------ GridTransform_jni.cpp --------------------------//
//------------------------ GridTransform_jni.cpp --------------------------//
//------------------------ GridTransform_jni.cpp --------------------------//

#include "org_cpseis_util_GridTransform.h"
#include "GTW.hh"
#include "JniConverter.hh"
#include "named_constants.h"
#include <string.h>
#include <assert.h>

//--------------------------- casting helpers ---------------------------//
//--------------------------- casting helpers ---------------------------//
//--------------------------- casting helpers ---------------------------//

    // cast between jlong and pointer without getting warning messages.

static GTW *jj2pp(jlong pointer)
              { assert(pointer); return (GTW*)((long)pointer); }

static jlong pp2jj(GTW *grid)
              { assert(grid); return (jlong)((long)grid); }

//------------------------- create and destroy ----------------------------//
//------------------------- create and destroy ----------------------------//
//------------------------- create and destroy ----------------------------//

JNIEXPORT jlong JNICALL Java_org_cpseis_util_GridTransform_nativeCreate
  (JNIEnv *env, jobject obj)
{
  assert(sizeof(jint)    == sizeof(int));         // to simplify coding.
  assert(sizeof(jdouble) == sizeof(double));      // to simplify coding.
  assert(sizeof(jfloat)  == sizeof(float));       // to simplify coding.

  GTW *grid = new GTW();
  return pp2jj(grid);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeDestroy
  (JNIEnv *env, jobject obj, jlong pointer)
{
  if(!pointer) return;
  delete jj2pp(pointer);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeInitialize
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  grid->initialize();
}

//---------------------------- get values --------------------------------//
//---------------------------- get values --------------------------------//
//---------------------------- get values --------------------------------//

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetXorigin
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getXorigin();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetYorigin
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getYorigin();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetRotationAngle
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getRotationAngle();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetXgridWidth
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getXgridWidth();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetYgridWidth
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getYgridWidth();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_GridTransform_nativeGetHandedness
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jint)grid->getHandedness();
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_GridTransform_nativeIsRightHanded
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jboolean)grid->isRightHanded();
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_GridTransform_nativeIsLeftHanded
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jboolean)grid->isLeftHanded();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetCosineAngle
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getCosineAngle();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetSineAngle
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getSineAngle();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDx11
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDx11();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDx21
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDx21();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDx12
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDx12();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDx22
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDx22();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDn11
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDn11();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDn21
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDn21();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDn12
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDn12();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDn22
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDn22();
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetDeterminant
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getDeterminant();
}


//-------------------- get transformed coordinates ---------------------//
//-------------------- get transformed coordinates ---------------------//
//-------------------- get transformed coordinates ---------------------//


JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetXlocCoord
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xgrid, jdouble ygrid)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getXlocCoord((double)xgrid, (double)ygrid);
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetYlocCoord
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xgrid, jdouble ygrid)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getYlocCoord((double)xgrid, (double)ygrid);
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetXgridCoord
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xloc, jdouble yloc)
{
  assert(pointer);
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getXgridCoord((double)xloc, (double)yloc);
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_GridTransform_nativeGetYgridCoord
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xloc, jdouble yloc)
{
  GTW *grid = jj2pp(pointer);
  return (jdouble)grid->getYgridCoord((double)xloc, (double)yloc);
}


//------------------------- set values -----------------------------//
//------------------------- set values -----------------------------//
//------------------------- set values -----------------------------//


JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeinitialize
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  grid->initialize();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetXorigin
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xorigin)
{
  GTW *grid = jj2pp(pointer);
  grid->setXorigin(xorigin);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetYorigin
  (JNIEnv *env, jobject obj, jlong pointer, jdouble yorigin)
{
  GTW *grid = jj2pp(pointer);
  grid->setYorigin(yorigin);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetRotationAngle
  (JNIEnv *env, jobject obj, jlong pointer, jdouble angle)
{
  GTW *grid = jj2pp(pointer);
  grid->setRotationAngle(angle);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetXgridWidth
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xwidth)
{
  GTW *grid = jj2pp(pointer);
  grid->setXgridWidth(xwidth);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetYgridWidth
  (JNIEnv *env, jobject obj, jlong pointer, jdouble ywidth)
{
  GTW *grid = jj2pp(pointer);
  grid->setYgridWidth(ywidth);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetHandedness
  (JNIEnv *env, jobject obj, jlong pointer, jint hand)
{
  GTW *grid = jj2pp(pointer);
  grid->setHandedness(hand);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetRightHanded
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  grid->setRightHanded();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetLeftHanded
  (JNIEnv *env, jobject obj, jlong pointer)
{
  GTW *grid = jj2pp(pointer);
  grid->setLeftHanded();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetTransform
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xorigin, jdouble yorigin,
                             jdouble angle, jdouble xwidth, jdouble ywidth, jint hand)
{
  GTW *grid = jj2pp(pointer);
  grid->setTransform(xorigin, yorigin, angle, xwidth, ywidth, hand);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetRightHandedTransform
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xorigin, jdouble yorigin,
                             jdouble angle, jdouble xwidth, jdouble ywidth)
{
  GTW *grid = jj2pp(pointer);
  grid->setRightHandedTransform(xorigin, yorigin, angle, xwidth, ywidth);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetLeftHandedTransform
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xorigin, jdouble yorigin,
                             jdouble angle, jdouble xwidth, jdouble ywidth)
{
  GTW *grid = jj2pp(pointer);
  grid->setLeftHandedTransform(xorigin, yorigin, angle, xwidth, ywidth);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDx11
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dx11)
{
  GTW *grid = jj2pp(pointer);
  grid->setDx11(dx11);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDx21
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dx21)
{
  GTW *grid = jj2pp(pointer);
  grid->setDx21(dx21);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDx12
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dx12)
{
  GTW *grid = jj2pp(pointer);
  grid->setDx12(dx12);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDx22
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dx22)
{
  GTW *grid = jj2pp(pointer);
  grid->setDx22(dx22);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDn11
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dn11)
{
  GTW *grid = jj2pp(pointer);
  grid->setDn11(dn11);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDn21
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dn21)
{
  GTW *grid = jj2pp(pointer);
  grid->setDn21(dn21);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDn12
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dn12)
{
  GTW *grid = jj2pp(pointer);
  grid->setDn12(dn12);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetDn22
  (JNIEnv *env, jobject obj, jlong pointer, jdouble dn22)
{
  GTW *grid = jj2pp(pointer);
  grid->setDn22(dn22);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetForwardRotationMatrix
  (JNIEnv *env, jobject obj, jlong pointer,
   jdouble dx11, jdouble dx21, jdouble dx12, jdouble dx22)
{
  GTW *grid = jj2pp(pointer);
  grid->setForwardRotationMatrix(dx11, dx21, dx12, dx22);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_nativeSetReverseRotationMatrix
  (JNIEnv *env, jobject obj, jlong pointer,
   jdouble dn11, jdouble dn21, jdouble dn12, jdouble dn22)
{
  GTW *grid = jj2pp(pointer);
  grid->setReverseRotationMatrix(dn11, dn21, dn12, dn22);
}


//---------------------- define coordinate system ----------------------//
//---------------------- define coordinate system ----------------------//
//---------------------- define coordinate system ----------------------//


JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_defineOrigin
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xgrid, jdouble ygrid,
                                            jdouble  xloc, jdouble  yloc)
{
  GTW *grid = jj2pp(pointer);
  grid->defineOrigin(xgrid, ygrid, xloc, yloc);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_defineRotationAngle
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xloc1, jdouble yloc1,
                                            jdouble xloc2, jdouble yloc2)
{
  GTW *grid = jj2pp(pointer);
  grid->defineRotationAngle(xloc1, yloc1, xloc2, yloc2);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_defineOriginAndAngle
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xgrid, jdouble ygrid,
                                            jdouble xloc1, jdouble yloc1,
                                            jdouble xloc2, jdouble yloc2)
{
  GTW *grid = jj2pp(pointer);
  grid->defineOriginAndAngle(xgrid, ygrid, xloc1, yloc1, xloc2, yloc2);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_refineBinCenter
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xloc, jdouble yloc)
{
  GTW *grid = jj2pp(pointer);
  grid->refineBinCenter(xloc, yloc);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_refineRotationAngle
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xloc, jdouble yloc)
{
  GTW *grid = jj2pp(pointer);
  grid->refineRotationAngle(xloc, yloc);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_incrementGridCoords
  (JNIEnv *env, jobject obj, jlong pointer, jdouble xstep, jdouble ystep)
{
  GTW *grid = jj2pp(pointer);
  grid->incrementGridCoords(xstep, ystep);
}

// xresid and yresid can be NULL:

JNIEXPORT void JNICALL Java_org_cpseis_util_GridTransform_defineTransform
  (JNIEnv *env, jobject obj, jlong pointer, jint npoints,
                        jdoubleArray   xloc, jdoubleArray   yloc,    // input
                        jdoubleArray  xgrid, jdoubleArray  ygrid,    // input
                        jdoubleArray xresid, jdoubleArray yresid)    // output
{
  GTW *grid    = jj2pp(pointer);
  jdouble       *xloc9   = env->GetDoubleArrayElements (xloc, 0);
  jdouble       *yloc9   = env->GetDoubleArrayElements (yloc, 0);
  jdouble       *xgrid9  = env->GetDoubleArrayElements (xgrid, 0);
  jdouble       *ygrid9  = env->GetDoubleArrayElements (ygrid, 0);
  jdouble       *xresid9 = NULL;
  jdouble       *yresid9 = NULL;
  if(xresid)     xresid9 = env->GetDoubleArrayElements (xresid, 0);
  if(yresid)     yresid9 = env->GetDoubleArrayElements (yresid, 0);
  grid->defineTransform(npoints, xloc9, yloc9, xgrid9, ygrid9, xresid9, yresid9);
             env->ReleaseDoubleArrayElements (xloc, xloc9, 0);
             env->ReleaseDoubleArrayElements (yloc, yloc9, 0);
             env->ReleaseDoubleArrayElements (xgrid, xgrid9, 0);
             env->ReleaseDoubleArrayElements (ygrid, ygrid9, 0);
  if(xresid) env->ReleaseDoubleArrayElements (xresid, xresid9, 0);
  if(yresid) env->ReleaseDoubleArrayElements (yresid, yresid9, 0);
}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
