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
#ifndef _JNICfeApiHelper_h
#define _JNICfeApiHelper_h

#include <jni.h>

/**
 * JNICfeApiHelper is a C++ Singleton class to store JNI related information
 * to help C functions call Java when called from inside the CFE code.
 * At a minimum a standalone C function needs the current Thread's JNIEnv ptr
 * and the instance object of the Java class to call.
 * I could dig the JNIEnv out from the VM directly, but it makes things simply
 * and more importantly quicker if I cache the information in this class
 */
class JNIEXPORT JNICfeApiHelper {
private:
	/* Standard Singleton pattern. private instance variable and constructor leaving
	 * a program no alternative but to use the static access methods below. Guarantees
	 * that there is one and only one instance of this class
	 */
	static JNICfeApiHelper *_instance;

	JNICfeApiHelper(JNIEnv *env, jclass cls) {

		/* Cache the two method IDs for later use */
		putValue = env->GetMethodID(cls, "putValue", "(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
		putArray = env->GetMethodID(cls, "putArray", "(ILjava/lang/String;Ljava/lang/String;[Ljava/lang/String;II)V");
	
		/* The String Class object is needed to construct the string array in put_array */
		stringClass = (jclass) env->NewGlobalRef(env->FindClass("java/lang/String"));
		
		lock = env->NewGlobalRef(env->FindClass("java/lang/Object"));
	}

public:
	/* Bad karma I know. You arent supposed to allow public access to instance variables
	 * but in this case it simplifies the calling code and you arent likely to ever subclass
	 * a singleton anyway
	 */

	/* Current JNI Thread environment pointer. This is transient */
	JNIEnv    *_env;

	/* Current Java instance handle. This is transient */
	jobject   _object;	
	
	jobject lock;

	/* Method IDs for putValue and putArray methods in Java class. These only need
	 * to be retrieved everything the class is loaded. Which should be once.
	 * PENDING(gww) Implement initIds methods from Java to handle initialization
	 */
	jmethodID putValue;
	jmethodID putArray;
	jclass    stringClass;


	/* Singleton accessor methods to get paws on instance variable */
	/* This should only be called from initIds */
	static JNICfeApiHelper *instance(JNIEnv *env, jclass cls) {
		if (_instance == 0) {
			_instance = new JNICfeApiHelper(env, cls);
		}
		
		return _instance;
	}

	static JNICfeApiHelper *instance() {
		return _instance;
	}

	/* record the current JNI environment and Java instance */
	void setState(JNIEnv *env, jobject obj) {
		_env = env;
		_object = obj;
	}

	JNIEnv *getJNIEnv() {
		return _env;
	}

	jobject getObjectInstance() {
		return _object;
	}
};
#endif
