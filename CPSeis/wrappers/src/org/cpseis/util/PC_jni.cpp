//------------------------------ PC_jni.cpp -------------------------------//
//------------------------------ PC_jni.cpp -------------------------------//
//------------------------------ PC_jni.cpp -------------------------------//

#include "org_cpseis_util_PC.h"
#include "PCW.hh"
#include "GTW.hh"
#include "JniConverter.hh"
#include "named_constants.h"
#include <string.h>
#include <assert.h>

//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_exists
  (JNIEnv *env, jclass obj)
{
  return (jboolean)PCW::exists();
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_frontendUpdate
  (JNIEnv *env, jclass obj)
{
  assert(sizeof(jint)    == sizeof(int));         // to simplify coding.
  assert(sizeof(jdouble) == sizeof(double));      // to simplify coding.
  assert(sizeof(jfloat)  == sizeof(float));       // to simplify coding.
  assert(sizeof(jsize)   == sizeof(int));         // to simplify coding.

  PCW::frontendUpdate();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_backendUpdate
  (JNIEnv *env, jclass obj)
{
  assert(sizeof(jint)    == sizeof(int));         // to simplify coding.
  assert(sizeof(jdouble) == sizeof(double));      // to simplify coding.
  assert(sizeof(jfloat)  == sizeof(float));       // to simplify coding.
  assert(sizeof(jsize)   == sizeof(int));         // to simplify coding.

  PCW::backendUpdate();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_guiUpdate
  (JNIEnv *env, jclass obj)
{
  PCW::guiUpdate();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_quickUpdate
  (JNIEnv *env, jclass obj)
{
  PCW::quickUpdate();
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_frontendUpdateNoprint
  (JNIEnv *env, jclass obj)
{
  assert(sizeof(jint)    == sizeof(int));         // to simplify coding.
  assert(sizeof(jdouble) == sizeof(double));      // to simplify coding.
  assert(sizeof(jfloat)  == sizeof(float));       // to simplify coding.
  assert(sizeof(jsize)   == sizeof(int));         // to simplify coding.

  PCW::frontendUpdateNoprint();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_backendUpdateNoprint
  (JNIEnv *env, jclass obj)
{
  assert(sizeof(jint)    == sizeof(int));         // to simplify coding.
  assert(sizeof(jdouble) == sizeof(double));      // to simplify coding.
  assert(sizeof(jfloat)  == sizeof(float));       // to simplify coding.
  assert(sizeof(jsize)   == sizeof(int));         // to simplify coding.

  PCW::backendUpdateNoprint();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_guiUpdateNoprint
  (JNIEnv *env, jclass obj)
{
  PCW::guiUpdateNoprint();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_quickUpdateNoprint
  (JNIEnv *env, jclass obj)
{
  PCW::quickUpdateNoprint();
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clear
  (JNIEnv *env, jclass obj)
{
  PCW::clear();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_restore
  (JNIEnv *env, jclass obj)
{
  PCW::restore();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_next
  (JNIEnv *env, jclass obj)
{
  PCW::next();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_backendExecute
  (JNIEnv *env, jclass obj)
{
  PCW::backendExecute();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_continueBackendUpdate
  (JNIEnv *env, jclass obj)
{
  PCW::continueBackendUpdate();
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getUpdateState
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::getUpdateState();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_setBackendNoExec
  (JNIEnv *env, jclass obj)
{
  PCW::setBackendNoExec();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_setBackendYesExec
  (JNIEnv *env, jclass obj)
{
  PCW::setBackendYesExec();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getIpn
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::getIpn();
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_previousError
  (JNIEnv *env, jclass obj)
{
  return (jboolean)PCW::previousError();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_setIpn
  (JNIEnv *env, jclass obj, jint ipn)
{
  PCW::setIpn((int)ipn);
}

                          /////////////////////////////

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_doNotProcessTraces
  (JNIEnv *env, jclass obj)
{
  return (jboolean)PCW::doNotProcessTraces();
}

                          /////////////////////////////

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_updateError
  (JNIEnv *env, jclass obj)
{
  return (jboolean)PCW::updateError();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_error
  (JNIEnv *env, jclass obj, jstring msg)
{
  const char *msg9 = env->GetStringUTFChars(msg, 0);
  PCW::error(msg9);
  env->ReleaseStringUTFChars(msg, msg9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_warning
  (JNIEnv *env, jclass obj, jstring msg)
{
  const char *msg9 = env->GetStringUTFChars(msg, 0);
  PCW::warning(msg9);
  env->ReleaseStringUTFChars(msg, msg9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_info
  (JNIEnv *env, jclass obj, jstring msg)
{
  const char *msg9 = env->GetStringUTFChars(msg, 0);
  PCW::info(msg9);
  env->ReleaseStringUTFChars(msg, msg9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_print
  (JNIEnv *env, jclass obj, jstring msg)
{
  const char *msg9 = env->GetStringUTFChars(msg, 0);
  PCW::print(msg9);
  env->ReleaseStringUTFChars(msg, msg9);
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printAllDataCards
  (JNIEnv *env, jclass obj)
{
  PCW::printAllDataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printProcessCards
  (JNIEnv *env, jclass obj)
{
  PCW::printProcessCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printGlobalCards
  (JNIEnv *env, jclass obj)
{
  PCW::printGlobalCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printControlCards
  (JNIEnv *env, jclass obj)
{
  PCW::printControlCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printPdataCards
  (JNIEnv *env, jclass obj)
{
  PCW::printPdataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printJdataCards
  (JNIEnv *env, jclass obj)
{
  PCW::printJdataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_printGuiCards
  (JNIEnv *env, jclass obj)
{
  PCW::printGuiCards();
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_infoProcessCards
  (JNIEnv *env, jclass obj)
{
  PCW::infoProcessCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_infoGlobalCards
  (JNIEnv *env, jclass obj)
{
  PCW::infoGlobalCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_infoControlCards
  (JNIEnv *env, jclass obj)
{
  PCW::infoControlCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_infoPdataCards
  (JNIEnv *env, jclass obj)
{
  PCW::infoPdataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_infoJdataCards
  (JNIEnv *env, jclass obj)
{
  PCW::infoJdataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_infoGuiCards
  (JNIEnv *env, jclass obj)
{
  PCW::infoGuiCards();
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numElementsProcess
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint num = (jint)PCW::numElementsProcess(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return num;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numElementsGlobal
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint num = (jint)PCW::numElementsGlobal(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return num;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numElementsControl
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint num = (jint)PCW::numElementsControl(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return num;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numElementsGui
  (JNIEnv *env, jclass obj, jstring keyword, jstring action)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  jint num = (jint)PCW::numElementsGui(keyword9, action9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return num;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numElementsPdata
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint num = (jint)PCW::numElementsPdata(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return num;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numElementsJdata
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint num = (jint)PCW::numElementsJdata(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return num;
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_natureProcess
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint nature = (jint)PCW::natureProcess(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return nature;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_natureGlobal
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint nature = (jint)PCW::natureGlobal(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return nature;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_natureControl
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint nature = (jint)PCW::natureControl(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return nature;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_natureGui
  (JNIEnv *env, jclass obj, jstring keyword, jstring action)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  jint nature = (jint)PCW::natureGui(keyword9, action9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return nature;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_naturePdata
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint nature = (jint)PCW::naturePdata(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return nature;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_natureJdata
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint nature = (jint)PCW::natureJdata(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return nature;
}
                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_vartypeProcess
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint vartype = (jint)PCW::vartypeProcess(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return vartype;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_vartypeGlobal
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint vartype = (jint)PCW::vartypeGlobal(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return vartype;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_vartypeControl
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint vartype = (jint)PCW::vartypeControl(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return vartype;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_vartypeGui
  (JNIEnv *env, jclass obj, jstring keyword, jstring action)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  jint vartype = (jint)PCW::vartypeGui(keyword9, action9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return vartype;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_vartypePdata
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint vartype = (jint)PCW::vartypePdata(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return vartype;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_vartypeJdata
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jint vartype = (jint)PCW::vartypeJdata(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return vartype;
}
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::get(keyword9, grid);
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::get(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float scalar9 = (float)scalar;
  PCW::get(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double scalar9 = (double)scalar;
  PCW::get(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getL(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::get(keyword9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::getProcess(keyword9, grid);
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getProcess(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float scalar9 = (float)scalar;
  PCW::getProcess(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double scalar9 = (double)scalar;
  PCW::getProcess(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getProcessL(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::getProcess(keyword9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
/*
printf("PC.cpp before getGlobal: grid = %p\n", grid);
printf("PC.cpp before getGlobal: grid->getFpoint() = %p\n", grid->getFpoint());
printf("PC.cpp before getGlobal: xorigin = %lf\n", grid->getXorigin());
printf("PC.cpp before getGlobal: yorigin = %lf\n", grid->getYorigin());
printf("PC.cpp before getGlobal: xwidth = %lf\n", grid->getXgridWidth());
printf("PC.cpp before getGlobal: ywidth = %lf\n", grid->getYgridWidth());
printf("PC.cpp before getGlobal: angle = %lf\n", grid->getRotationAngle());
printf("PC.cpp before getGlobal: handedness = %d\n", grid->getHandedness());
*/
  PCW::getGlobal(keyword9, grid);
/*
printf("PC.cpp after getGlobal: xorigin = %lf\n", grid->getXorigin());
printf("PC.cpp after getGlobal: yorigin = %lf\n", grid->getYorigin());
printf("PC.cpp after getGlobal: xwidth = %lf\n", grid->getXgridWidth());
printf("PC.cpp after getGlobal: ywidth = %lf\n", grid->getYgridWidth());
printf("PC.cpp after getGlobal: angle = %lf\n", grid->getRotationAngle());
printf("PC.cpp after getGlobal: handedness = %d\n", grid->getHandedness());
*/
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getGlobal(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float scalar9 = (float)scalar;
  PCW::getGlobal(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double scalar9 = (double)scalar;
  PCW::getGlobal(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getGlobalL(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::getGlobal(keyword9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::getControl(keyword9, grid);
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getControl(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float scalar9 = (float)scalar;
  PCW::getControl(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double scalar9 = (double)scalar;
  PCW::getControl(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getControlL(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::getControl(keyword9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::getGui(keyword9, action9, grid);
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  int scalar9 = (int)scalar;
  PCW::getGui(keyword9, action9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  float scalar9 = (float)scalar;
  PCW::getGui(keyword9, action9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  double scalar9 = (double)scalar;
  PCW::getGui(keyword9, action9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  int scalar9 = (int)scalar;
  PCW::getGuiL(keyword9, action9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::getGui(keyword9, action9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::getPdata(keyword9, grid);
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getPdata(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float scalar9 = (float)scalar;
  PCW::getPdata(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double scalar9 = (double)scalar;
  PCW::getPdata(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getPdataL(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::getPdata(keyword9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jobject JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::getJdata(keyword9, grid);
/*
  jobject new_grid = JniConverter::putGTW(env, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete grid;
  return new_grid;
*/
  env->ReleaseStringUTFChars(keyword, keyword9);
  return scalar;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getJdata(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)scalar9;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float scalar9 = (float)scalar;
  PCW::getJdata(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)scalar9;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double scalar9 = (double)scalar;
  PCW::getJdata(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)scalar9;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int scalar9 = (int)scalar;
  PCW::getJdataL(keyword9, &scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)scalar9;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  char buffer[PCW::LENGTH];
  strcpy(buffer, scalar9);
  PCW::getJdata(keyword9, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         nsize2    = PCW::numElementsGui(keyword9, "ReplaceElements");
  int         nsize3    = PCW::numElementsProcess(keyword9);
  int         minsize   = MaximumValue(nsize2, nsize3);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::get(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         nsize2    = PCW::numElementsGui(keyword9, "ReplaceElements");
  int         nsize3    = PCW::numElementsProcess(keyword9);
  int         minsize   = MaximumValue(nsize2, nsize3);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::get(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         nsize2    = PCW::numElementsGui(keyword9, "ReplaceElements");
  int         nsize3    = PCW::numElementsProcess(keyword9);
  int         minsize   = MaximumValue(nsize2, nsize3);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::get(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         nsize2    = PCW::numElementsGui(keyword9, "ReplaceElements");
  int         nsize3    = PCW::numElementsProcess(keyword9);
  int         minsize   = MaximumValue(nsize2, nsize3);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getL(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}


JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_get__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         nsize2    = PCW::numElementsGui(keyword9, "ReplaceElements");
  int         nsize3    = PCW::numElementsProcess(keyword9);
  int         minsize   = MaximumValue(nsize2, nsize3);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::get(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsProcess(keyword9);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::getProcess(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsProcess(keyword9);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::getProcess(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsProcess(keyword9);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::getProcess(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsProcess(keyword9);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getProcessL(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getProcess__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsProcess(keyword9);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::getProcess(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsGlobal(keyword9);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::getGlobal(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsGlobal(keyword9);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::getGlobal(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsGlobal(keyword9);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::getGlobal(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsGlobal(keyword9);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getGlobalL(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getGlobal__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsGlobal(keyword9);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::getGlobal(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsControl(keyword9);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::getControl(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsControl(keyword9);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::getControl(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsControl(keyword9);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::getControl(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsControl(keyword9);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getControlL(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getControl__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsControl(keyword9);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::getControl(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  const char *action9   = env->GetStringUTFChars(action, 0);
  int         minsize   = PCW::numElementsGui(keyword9, action9);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::getGui(keyword9, action9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  const char *action9   = env->GetStringUTFChars(action, 0);
  int         minsize   = PCW::numElementsGui(keyword9, action9);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::getGui(keyword9, action9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  const char *action9   = env->GetStringUTFChars(action, 0);
  int         minsize   = PCW::numElementsGui(keyword9, action9);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::getGui(keyword9, action9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  const char *action9   = env->GetStringUTFChars(action, 0);
  int         minsize   = PCW::numElementsGui(keyword9, action9);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getGuiL(keyword9, action9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getGui__Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  const char *action9   = env->GetStringUTFChars(action, 0);
  int         minsize   = PCW::numElementsGui(keyword9, action9);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::getGui(keyword9, action9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsPdata(keyword9);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::getPdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsPdata(keyword9);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::getPdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsPdata(keyword9);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::getPdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsPdata(keyword9);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getPdataL(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getPdata__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsPdata(keyword9);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::getPdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jintArray JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsJdata(keyword9);
  int        *buffer    = JniConverter::getIntArray(env, array, &nelements, &nsize, minsize);
  PCW::getJdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jintArray new_array = JniConverter::putIntArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jfloatArray JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsJdata(keyword9);
  float      *buffer    = JniConverter::getFloatArray(env, array, &nelements, &nsize, minsize);
  PCW::getJdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jfloatArray new_array = JniConverter::putFloatArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jdoubleArray JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsJdata(keyword9);
  double     *buffer    = JniConverter::getDoubleArray(env, array, &nelements, &nsize, minsize);
  PCW::getJdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jdoubleArray new_array = JniConverter::putDoubleArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jbooleanArray JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsJdata(keyword9);
  int        *buffer    = JniConverter::getBooleanArray(env, array, &nelements, &nsize, minsize);
  PCW::getJdataL(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jbooleanArray new_array = JniConverter::putBooleanArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getJdata__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements, nsize;
  const char *keyword9  = env->GetStringUTFChars(keyword, 0);
  int         minsize   = PCW::numElementsJdata(keyword9);
  char       *buffer    = JniConverter::getStringArray(env, array, &nelements, &nsize, minsize);
  PCW::getJdata(keyword9, nsize, buffer, &nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  jobjectArray new_array = JniConverter::putStringArray(env, buffer, nelements);
  delete [] buffer;
  return new_array;
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getProcessElementInt
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getProcess(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)element;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getProcessElementFloat
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float element;
  PCW::getProcess(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)element;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getProcessElementDouble
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double element;
  PCW::getProcess(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)element;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getProcessElementBoolean
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getProcessL(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)element;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getProcessElementString
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char buffer[PCW::LENGTH];
  PCW::getProcess(keyword9, (int)indx, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getGlobalElementInt
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getGlobal(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)element;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getGlobalElementFloat
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float element;
  PCW::getGlobal(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)element;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getGlobalElementDouble
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double element;
  PCW::getGlobal(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)element;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getGlobalElementBoolean
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getGlobalL(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)element;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGlobalElementString
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char buffer[PCW::LENGTH];
  PCW::getGlobal(keyword9, (int)indx, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getControlElementInt
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getControl(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)element;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getControlElementFloat
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float element;
  PCW::getControl(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)element;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getControlElementDouble
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double element;
  PCW::getControl(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)element;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getControlElementBoolean
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getControlL(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)element;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getControlElementString
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char buffer[PCW::LENGTH];
  PCW::getControl(keyword9, (int)indx, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getGuiElementInt
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  int element;
  PCW::getGui(keyword9, action9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jint)element;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getGuiElementFloat
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  float element;
  PCW::getGui(keyword9, action9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jfloat)element;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getGuiElementDouble
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  double element;
  PCW::getGui(keyword9, action9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jdouble)element;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getGuiElementBoolean
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  int element;
  PCW::getGuiL(keyword9, action9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return (jboolean)element;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGuiElementString
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  char buffer[PCW::LENGTH];
  PCW::getGui(keyword9, action9, (int)indx, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getPdataElementInt
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getPdata(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)element;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getPdataElementFloat
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float element;
  PCW::getPdata(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)element;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getPdataElementDouble
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double element;
  PCW::getPdata(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)element;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getPdataElementBoolean
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getPdataL(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)element;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getPdataElementString
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char buffer[PCW::LENGTH];
  PCW::getPdata(keyword9, (int)indx, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_getJdataElementInt
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getJdata(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)element;
}

JNIEXPORT jfloat JNICALL Java_org_cpseis_util_PC_getJdataElementFloat
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float element;
  PCW::getJdata(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jfloat)element;
}

JNIEXPORT jdouble JNICALL Java_org_cpseis_util_PC_getJdataElementDouble
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double element;
  PCW::getJdata(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jdouble)element;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_getJdataElementBoolean
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int element;
  PCW::getJdataL(keyword9, (int)indx, &element);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jboolean)element;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getJdataElementString
  (JNIEnv *env, jclass obj, jstring keyword, jint indx)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char buffer[PCW::LENGTH];
  PCW::getJdata(keyword9, (int)indx, buffer);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_pressed
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean pressed = (jboolean)PCW::pressed(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return pressed;
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_activated
  (JNIEnv *env, jclass obj)
{
  char buffer[PCW::LENGTH];
  PCW::activated(buffer);
  return env->NewStringUTF(buffer);
}

                          /////////////////////////////

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_verifyScalar
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean verify = (jboolean)PCW::verifyScalar(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return verify;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_verifyElement
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int indx, action;
  jboolean verify = (jboolean)PCW::verifyElement(keyword9, &indx, &action);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return verify;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_verifyElementGetIndex
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int indx, action;
  PCW::verifyElement(keyword9, &indx, &action);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)indx;
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_verifyElementGetAction
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int indx, action;
  PCW::verifyElement(keyword9, &indx, &action);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return (jint)action;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_verifyArray
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean verify = (jboolean)PCW::verifyArray(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return verify;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_verifyArrayset
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean verify = (jboolean)PCW::verifyArrayset(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return verify;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_verifyScreen
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean verify = (jboolean)PCW::verifyScreen(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return verify;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_verifyEnd
  (JNIEnv *env, jclass obj)
{
  return (jboolean)PCW::verifyEnd();
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::put(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::put(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::put(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::put(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::put(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::putProcess(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putProcess(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putProcess(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putProcess(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putProcessL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putProcess(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
/*
printf("PC.cpp putGlobal: grid = %p\n", grid);
printf("PC.cpp putGlobal: grid->getFpoint() = %p\n", grid->getFpoint());
printf("PC.cpp putGlobal: xorigin = %lf\n", grid->getXorigin());
printf("PC.cpp putGlobal: yorigin = %lf\n", grid->getYorigin());
printf("PC.cpp putGlobal: xwidth = %lf\n", grid->getXgridWidth());
printf("PC.cpp putGlobal: ywidth = %lf\n", grid->getYgridWidth());
printf("PC.cpp putGlobal: angle = %lf\n", grid->getRotationAngle());
printf("PC.cpp putGlobal: handedness = %d\n", grid->getHandedness());
*/
  PCW::putGlobal(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGlobal(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGlobal(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGlobal(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGlobalL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putGlobal(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::putControl(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putControl(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putControl(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putControl(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putControlL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putControl(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::putGui(keyword9, action9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  PCW::putGui(keyword9, action9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  PCW::putGui(keyword9, action9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  PCW::putGui(keyword9, action9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  PCW::putGuiL(keyword9, action9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putGui(keyword9, action9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::putGuiOnly(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGuiOnly(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGuiOnly(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGuiOnly(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putGuiOnlyL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putGuiOnly(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::putPdata(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putPdata(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putPdata(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putPdata(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putPdataL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putPdata(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2Lorg_cpseis_util_GridTransform_2
  (JNIEnv *env, jclass obj, jstring keyword, jobject scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  GTW *grid = JniConverter::getGTW(env, scalar);
  PCW::putJdata(keyword9, grid);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2I
  (JNIEnv *env, jclass obj, jstring keyword, jint scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putJdata(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2F
  (JNIEnv *env, jclass obj, jstring keyword, jfloat scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putJdata(keyword9, (float)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2D
  (JNIEnv *env, jclass obj, jstring keyword, jdouble scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putJdata(keyword9, (double)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2Z
  (JNIEnv *env, jclass obj, jstring keyword, jboolean scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putJdataL(keyword9, (int)scalar);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring scalar)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *scalar9  = env->GetStringUTFChars(scalar, 0);
  PCW::putJdata(keyword9, scalar9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(scalar, scalar9);
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2_3I
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::put(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2_3F
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::put(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2_3D
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::put(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_put__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::put(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putProcess(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putProcess(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putProcess(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putProcessL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcess__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putProcess(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putGlobal(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putGlobal(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putGlobal(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putGlobalL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobal__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putGlobal(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putControl(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putControl(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putControl(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putControlL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControl__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putControl(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putGui(keyword9, action9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putGui(keyword9, action9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putGui(keyword9, action9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putGuiL(keyword9, action9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGui__Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jstring action, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putGui(keyword9, action9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putGuiOnly(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putGuiOnly(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putGuiOnly(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putGuiOnlyL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiOnly__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putGuiOnly(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putPdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putPdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putPdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putPdataL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdata__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putPdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, array, &nelements);
  PCW::putJdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, array, &nelements);
  PCW::putJdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, array, &nelements);
  PCW::putJdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, array, &nelements);
  PCW::putJdataL(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdata__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray array)
{
  int nelements;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, array, &nelements);
  PCW::putJdata(keyword9, buffer, nelements);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT void JNICALL Java_org_cpseis_util_PC_registerArrayNames
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray names)
{
  int num;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, names, &num);
  PCW::registerArrayNames(keyword9, buffer, num);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptions__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, options, &noptions);
  PCW::putOptions(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptions__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, options, &noptions);
  PCW::putOptions(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptions__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, options, &noptions);
  PCW::putOptions(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptions__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, options, &noptions);
  PCW::putOptionsL(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptions__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, options, &noptions);
  PCW::putOptions(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptionsA__Ljava_lang_String_2_3II
  (JNIEnv *env, jclass obj, jstring keyword, jintArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getIntArray(env, options, &noptions);
  PCW::putOptionsA(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptionsA__Ljava_lang_String_2_3FII
  (JNIEnv *env, jclass obj, jstring keyword, jfloatArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  float *buffer = JniConverter::getFloatArray(env, options, &noptions);
  PCW::putOptionsA(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptionsA__Ljava_lang_String_2_3DII
  (JNIEnv *env, jclass obj, jstring keyword, jdoubleArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  double *buffer = JniConverter::getDoubleArray(env, options, &noptions);
  PCW::putOptionsA(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptionsA__Ljava_lang_String_2_3Z
  (JNIEnv *env, jclass obj, jstring keyword, jbooleanArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  int *buffer = JniConverter::getBooleanArray(env, options, &noptions);
  PCW::putOptionsAL(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putOptionsA__Ljava_lang_String_2_3Ljava_lang_String_2
  (JNIEnv *env, jclass obj, jstring keyword, jobjectArray options)
{
  int noptions;
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  char *buffer = JniConverter::getStringArray(env, options, &noptions);
  PCW::putOptionsA(keyword9, buffer, noptions);
  env->ReleaseStringUTFChars(keyword, keyword9);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putSensitiveFieldFlag
  (JNIEnv *env, jclass obj, jstring keyword, jboolean sensitive)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putSensitiveFieldFlag(keyword9, (int)sensitive);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putSensitiveArrayFlag
  (JNIEnv *env, jclass obj, jstring keyword, jboolean sensitive)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putSensitiveArrayFlag(keyword9, (int)sensitive);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putSensitiveArraysetFlag
  (JNIEnv *env, jclass obj, jstring keyword, jboolean sensitive)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putSensitiveArraysetFlag(keyword9, (int)sensitive);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putSensitiveScreenFlag
  (JNIEnv *env, jclass obj, jstring keyword, jboolean sensitive)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putSensitiveScreenFlag(keyword9, (int)sensitive);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putVisibleFlag
  (JNIEnv *env, jclass obj, jstring keyword, jboolean visible)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putVisibleFlag(keyword9, (int)visible);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putMinsizeArray
  (JNIEnv *env, jclass obj, jstring keyword, jint minsize)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putMinsizeArray(keyword9, (int)minsize);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putMinsizeArrayset
  (JNIEnv *env, jclass obj, jstring keyword, jint minsize)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putMinsizeArrayset(keyword9, (int)minsize);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putMaxsizeArray
  (JNIEnv *env, jclass obj, jstring keyword, jint maxsize)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putMaxsizeArray(keyword9, (int)maxsize);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putMaxsizeArrayset
  (JNIEnv *env, jclass obj, jstring keyword, jint maxsize)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::putMaxsizeArrayset(keyword9, (int)maxsize);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numProcessCards
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numProcessCards();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numGlobalCards
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numGlobalCards();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numControlCards
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numControlCards();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numPdataCards
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numPdataCards();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numJdataCards
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numJdataCards();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numGuiCards
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numGuiCards();
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getProcessCards
  (JNIEnv *env, jclass obj)
{
  int         ncards    = PCW::numProcessCards();
  int         nsize     = ncards;
  char       *cards     = new char [ncards * PCW::LENGTH];
  PCW::getProcessCards(nsize, cards, &ncards);
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(ncards, element_class, initial_element);
  for(int i = 0; i < ncards; i++)
      {
      jstring card = env->NewStringUTF(&cards[i * PCW::LENGTH]);
      env->SetObjectArrayElement(new_array, i, card);
      }
  delete [] cards;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getGlobalCards
  (JNIEnv *env, jclass obj)
{
  int         ncards    = PCW::numGlobalCards();
  int         nsize     = ncards;
  char       *cards     = new char [ncards * PCW::LENGTH];
  PCW::getGlobalCards(nsize, cards, &ncards);
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(ncards, element_class, initial_element);
  for(int i = 0; i < ncards; i++)
      {
      jstring card = env->NewStringUTF(&cards[i * PCW::LENGTH]);
      env->SetObjectArrayElement(new_array, i, card);
      }
  delete [] cards;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getControlCards
  (JNIEnv *env, jclass obj)
{
  int         ncards    = PCW::numControlCards();
  int         nsize     = ncards;
  char       *cards     = new char [ncards * PCW::LENGTH];
  PCW::getControlCards(nsize, cards, &ncards);
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(ncards, element_class, initial_element);
  for(int i = 0; i < ncards; i++)
      {
      jstring card = env->NewStringUTF(&cards[i * PCW::LENGTH]);
      env->SetObjectArrayElement(new_array, i, card);
      }
  delete [] cards;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getPdataCards
  (JNIEnv *env, jclass obj)
{
  int         ncards    = PCW::numPdataCards();
  int         nsize     = ncards;
  char       *cards     = new char [ncards * PCW::LENGTH];
  PCW::getPdataCards(nsize, cards, &ncards);
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(ncards, element_class, initial_element);
  for(int i = 0; i < ncards; i++)
      {
      jstring card = env->NewStringUTF(&cards[i * PCW::LENGTH]);
      env->SetObjectArrayElement(new_array, i, card);
      }
  delete [] cards;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getJdataCards
  (JNIEnv *env, jclass obj)
{
  int         ncards    = PCW::numJdataCards();
  int         nsize     = ncards;
  char       *cards     = new char [ncards * PCW::LENGTH];
  PCW::getJdataCards(nsize, cards, &ncards);
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(ncards, element_class, initial_element);
  for(int i = 0; i < ncards; i++)
      {
      jstring card = env->NewStringUTF(&cards[i * PCW::LENGTH]);
      env->SetObjectArrayElement(new_array, i, card);
      }
  delete [] cards;
  return new_array;
}

JNIEXPORT jobjectArray JNICALL Java_org_cpseis_util_PC_getGuiCards
  (JNIEnv *env, jclass obj)
{
  int         ncards    = PCW::numGuiCards();
  int         nsize     = ncards;
  char       *cards     = new char [ncards * PCW::LENGTH];
  PCW::getGuiCards(nsize, cards, &ncards);
  jclass element_class = env->FindClass("java/lang/String");
  jstring initial_element = env->NewStringUTF("");
  jobjectArray new_array = (jobjectArray)env->NewObjectArray(ncards, element_class, initial_element);
  for(int i = 0; i < ncards; i++)
      {
      jstring card = env->NewStringUTF(&cards[i * PCW::LENGTH]);
      env->SetObjectArrayElement(new_array, i, card);
      }
  delete [] cards;
  return new_array;
}

                          /////////////////////////////

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getProcessCard
  (JNIEnv *env, jclass obj, jint icard)
{
  char card[PCW::LENGTH];
  PCW::getProcessCard((int)icard, card);
  return env->NewStringUTF(card);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGlobalCard
  (JNIEnv *env, jclass obj, jint icard)
{
  char card[PCW::LENGTH];
  PCW::getGlobalCard((int)icard, card);
  return env->NewStringUTF(card);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getControlCard
  (JNIEnv *env, jclass obj, jint icard)
{
  char card[PCW::LENGTH];
  PCW::getControlCard((int)icard, card);
  return env->NewStringUTF(card);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getPdataCard
  (JNIEnv *env, jclass obj, jint icard)
{
  char card[PCW::LENGTH];
  PCW::getPdataCard((int)icard, card);
  return env->NewStringUTF(card);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getJdataCard
  (JNIEnv *env, jclass obj, jint icard)
{
  char card[PCW::LENGTH];
  PCW::getJdataCard((int)icard, card);
  return env->NewStringUTF(card);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGuiCard
  (JNIEnv *env, jclass obj, jint icard)
{
  char card[PCW::LENGTH];
  PCW::getGuiCard((int)icard, card);
  return env->NewStringUTF(card);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcessCards
  (JNIEnv *env, jclass obj, jobjectArray cards)
{
  int ncards;
  char *buffer = JniConverter::getStringArray(env, cards, &ncards);
  PCW::putProcessCards(buffer, ncards);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobalCards
  (JNIEnv *env, jclass obj, jobjectArray cards)
{
  int ncards;
  char *buffer = JniConverter::getStringArray(env, cards, &ncards);
  PCW::putGlobalCards(buffer, ncards);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControlCards
  (JNIEnv *env, jclass obj, jobjectArray cards)
{
  int ncards;
  char *buffer = JniConverter::getStringArray(env, cards, &ncards);
  PCW::putControlCards(buffer, ncards);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdataCards
  (JNIEnv *env, jclass obj, jobjectArray cards)
{
  int ncards;
  char *buffer = JniConverter::getStringArray(env, cards, &ncards);
  PCW::putPdataCards(buffer, ncards);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdataCards
  (JNIEnv *env, jclass obj, jobjectArray cards)
{
  int ncards;
  char *buffer = JniConverter::getStringArray(env, cards, &ncards);
  PCW::putJdataCards(buffer, ncards);
  delete [] buffer;
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiCards
  (JNIEnv *env, jclass obj, jobjectArray cards)
{
  int ncards;
  char *buffer = JniConverter::getStringArray(env, cards, &ncards);
  PCW::putGuiCards(buffer, ncards);
  delete [] buffer;
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putProcessCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::putProcessCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGlobalCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::putGlobalCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putControlCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::putControlCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putPdataCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::putPdataCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putJdataCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::putJdataCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_putGuiCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::putGuiCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_addProcessCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::addProcessCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_addGlobalCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::addGlobalCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_addControlCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::addControlCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_addPdataCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::addPdataCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_addJdataCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::addJdataCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_addGuiCard
  (JNIEnv *env, jclass obj, jstring card)
{
  const char *card9 = env->GetStringUTFChars(card, 0);
  PCW::addGuiCard(card9);
  env->ReleaseStringUTFChars(card, card9);
}

                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////
                          /////////////////////////////


JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clearProcessCards
  (JNIEnv *env, jclass obj)
{
  PCW::clearProcessCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clearGlobalCards
  (JNIEnv *env, jclass obj)
{
  PCW::clearGlobalCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clearControlCards
  (JNIEnv *env, jclass obj)
{
  PCW::clearControlCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clearPdataCards
  (JNIEnv *env, jclass obj)
{
  PCW::clearPdataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clearJdataCards
  (JNIEnv *env, jclass obj)
{
  PCW::clearJdataCards();
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_clearGuiCards
  (JNIEnv *env, jclass obj)
{
  PCW::clearGuiCards();
}

                          /////////////////////////////

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_processKeywordPresent
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean present = (jboolean)PCW::processKeywordPresent(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return present;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_globalKeywordPresent
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean present = (jboolean)PCW::globalKeywordPresent(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return present;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_controlKeywordPresent
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean present = (jboolean)PCW::controlKeywordPresent(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return present;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_pdataKeywordPresent
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean present = (jboolean)PCW::pdataKeywordPresent(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return present;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_jdataKeywordPresent
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  jboolean present = (jboolean)PCW::jdataKeywordPresent(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  return present;
}

JNIEXPORT jboolean JNICALL Java_org_cpseis_util_PC_guiActionPresent
  (JNIEnv *env, jclass obj, jstring keyword, jstring action)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  jboolean present = (jboolean)PCW::guiActionPresent(keyword9, action9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
  return present;
}

                          /////////////////////////////

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numProcessKeywords
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numProcessKeywords();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numGlobalKeywords
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numGlobalKeywords();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numControlKeywords
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numControlKeywords();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numPdataKeywords
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numPdataKeywords();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numJdataKeywords
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numJdataKeywords();
}

JNIEXPORT jint JNICALL Java_org_cpseis_util_PC_numGuiKeywords
  (JNIEnv *env, jclass obj)
{
  return (jint)PCW::numGuiKeywords();
}

                          /////////////////////////////

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getProcessKeyword
  (JNIEnv *env, jclass obj, jint indx)
{
  char keyword[PCW::LENGTH];
  PCW::getProcessKeyword((int)indx, keyword);
  return env->NewStringUTF(keyword);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGlobalKeyword
  (JNIEnv *env, jclass obj, jint indx)
{
  char keyword[PCW::LENGTH];
  PCW::getGlobalKeyword((int)indx, keyword);
  return env->NewStringUTF(keyword);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getControlKeyword
  (JNIEnv *env, jclass obj, jint indx)
{
  char keyword[PCW::LENGTH];
  PCW::getControlKeyword((int)indx, keyword);
  return env->NewStringUTF(keyword);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getPdataKeyword
  (JNIEnv *env, jclass obj, jint indx)
{
  char keyword[PCW::LENGTH];
  PCW::getPdataKeyword((int)indx, keyword);
  return env->NewStringUTF(keyword);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getJdataKeyword
  (JNIEnv *env, jclass obj, jint indx)
{
  char keyword[PCW::LENGTH];
  PCW::getJdataKeyword((int)indx, keyword);
  return env->NewStringUTF(keyword);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGuiKeyword
  (JNIEnv *env, jclass obj, jint indx)
{
  char keyword[PCW::LENGTH];
  PCW::getGuiKeyword((int)indx, keyword);
  return env->NewStringUTF(keyword);
}

JNIEXPORT jstring JNICALL Java_org_cpseis_util_PC_getGuiAction
  (JNIEnv *env, jclass obj, jint indx)
{
  char action[PCW::LENGTH];
  PCW::getGuiAction((int)indx, action);
  return env->NewStringUTF(action);
}

                          /////////////////////////////

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_removeProcessKeyword
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::removeProcessKeyword(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_removeGlobalKeyword
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::removeGlobalKeyword(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_removeControlKeyword
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::removeControlKeyword(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_removePdataKeyword
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::removePdataKeyword(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_removeJdataKeyword
  (JNIEnv *env, jclass obj, jstring keyword)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  PCW::removeJdataKeyword(keyword9);
  env->ReleaseStringUTFChars(keyword, keyword9);
}

JNIEXPORT void JNICALL Java_org_cpseis_util_PC_removeGuiAction
  (JNIEnv *env, jclass obj, jstring keyword, jstring action)
{
  const char *keyword9 = env->GetStringUTFChars(keyword, 0);
  const char *action9  = env->GetStringUTFChars(action, 0);
  PCW::removeGuiAction(keyword9, action9);
  env->ReleaseStringUTFChars(keyword, keyword9);
  env->ReleaseStringUTFChars(action, action9);
}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
