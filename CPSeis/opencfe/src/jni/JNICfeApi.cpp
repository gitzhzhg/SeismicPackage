#include <jni.h>
#include <iostream>
/* #include <iostream.h> */ 
/* #include <c++/3.4.6/backward/iostream.h> */

#include <string.h>

#include <int_api.h>
#include <JNICfeApiHelper.h>

#ifdef __cplusplus
extern "C" {
#endif
	
/*
	Java API :-
	private static native void set_value(int processID, String action, String keyword, String value) throws MessagingException;
	private static native void set_array(int processID, String action, String keyword, String[] array, int start, int end) throws MessagingException;
*/

/*
 * Class:     com_conoco_cfe_server_messaging_JNICfeApi
 * Method:    initIds
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_conoco_cfe_server_messaging_JNICfeApi_initIds
(JNIEnv *env, jclass cls) {
	// Initialize the Helper class, which in turn initializes the JNI variables
	JNICfeApiHelper *helper = JNICfeApiHelper::instance(env, cls);
}


/*
 * Class:     com_conoco_cfe_server_messaging_JNICfeApi
 * Method:    set_array
 * Signature: (ILjava/lang/String;Ljava/lang/String;[Ljava/lang/String;II)V
 */
JNIEXPORT void JNICALL Java_com_conoco_cfe_server_messaging_JNICfeApi_set_1array
  (JNIEnv *env, jobject obj, jint processID, jstring action, jstring keyword, jobjectArray array, jint start, jint end) {
	
	// Make sure helper is initialized with current Thread state
	// and methodIDs are loaded for calling back into Java
	
	JNICfeApiHelper *helper = JNICfeApiHelper::instance();
	
	// synchronize on the helper object which 
	// contains the JNI env pointer
	env->MonitorEnter(helper->lock);
	
	helper->setState(env, obj);

	// Get the character data from action String, isActionCopy flag indicates whether or
	// not to release memory after use.
	jboolean isActionCopy = JNI_FALSE;
	const char *actionString = 0;
	
	if (action != 0) {
		actionString = env->GetStringUTFChars(action, &isActionCopy);
	}
	
	// Get the character data from keyword string
	jboolean isKeywordCopy = JNI_FALSE;
	const char *keywordString = 0;
	
	if (keyword != 0) {
		keywordString = env->GetStringUTFChars(keyword, &isKeywordCopy);
	}

	// The following code converts the Java String [] into a char ** array to pass
	// to the cfe interface function
	jint arraySize = 0;
	
	if (array != 0) {
		arraySize = env->GetArrayLength(array);
	}

	char **stringArray = 0;
	
	if (arraySize > 0) {
		stringArray = new char * [arraySize];

		for (int i = 0; i < arraySize; i++ ) {
			jboolean isCopy;

			// Get the next string object
			jstring str = (jstring) env->GetObjectArrayElement(array, i);

			// Retrieve the character data from the string
			const char *cstring = env->GetStringUTFChars(str, &isCopy);

			// Make space to store character data in 'C' heap
			stringArray[i] = new char[strlen(cstring) + 1];
			strcpy(stringArray[i], cstring);

			// Release character data if VM allocated
			if (isCopy == JNI_TRUE) {
				env->ReleaseStringUTFChars(str, cstring);
			}

			// Tidy up the local reference created by GetObjectArrayElement as some VMs
			// have a limited amount of local references.
			env->DeleteLocalRef(str);
		}
	}

	// Call the CFE interface function
	set_array((int) processID, actionString, keywordString, (const char **) stringArray, (int) start, (int) end);

	jthrowable exception;
	jboolean exceptionRaised = 0;

	// Check to see if the call to CFE (and subsequent calls to Java) generated an exception
	if ((exception = env->ExceptionOccurred()) != NULL) {
		exceptionRaised = 1;

		// Clear exception state from the VM, the exception is regenerated later
		env->ExceptionClear();
	}

	if (arraySize > 0) {
		// Tidy up the memory allocated for the string array
		for (int i=0; i < arraySize; i++) {
			delete stringArray[i];
		}

		delete stringArray;
	}

	// Free up an memory allocated for the string character data
	if (isActionCopy == JNI_TRUE) {
		env->ReleaseStringUTFChars(action, actionString);
	}

	if (isKeywordCopy == JNI_TRUE) {
		env->ReleaseStringUTFChars(keyword, keywordString);
	}

	// If we detected an exception from the CFE code then regenerate it and return
	// The reason why I attempt to tidy up the memory before throwing the exception
	// is in the vain hope that somehow this exception is not catastrophic and can be
	// dealt with else where.

	if (exceptionRaised) {
		env->Throw(exception);
		//return; // Okay, so I know that its just going to happen, but who knows who'll insert code after this point...
	}

	env->MonitorExit(helper->lock);

}

/*
 * Class:     com_conoco_cfe_server_messaging_JNICfeApi
 * Method:    set_value
 * Signature: (ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_conoco_cfe_server_messaging_JNICfeApi_set_1value
  (JNIEnv *env, jobject obj, jint processID, jstring action, jstring keyword, jstring value) {
	
	// Make sure helper is initialized with current Thread state
	// and methodIDs are loaded for calling back into Java
	JNICfeApiHelper *helper = JNICfeApiHelper::instance();
	
	// synchronize on the helper object which 
	// contains the JNI env pointer
	env->MonitorEnter(helper->lock);

	helper->setState(env, obj);

	// Get the character data from action String, isActionCopy flag indicates whether or
	// not to release memory after use.
	jboolean isActionCopy = JNI_FALSE;
	const char *actionString = 0;
	
	if (action != 0) {
		actionString = env->GetStringUTFChars(action, &isActionCopy);
	}

	// Get the character data from keyword string
	jboolean isKeywordCopy = JNI_FALSE;
	const char *keywordString = 0;
	
	if (keyword != 0) {
		keywordString = env->GetStringUTFChars(keyword, &isKeywordCopy);
	}

	// Get the character data from value string
	jboolean isValueCopy = JNI_FALSE;
	const char *valueString = 0;
	
	if (value != 0) {
		valueString = env->GetStringUTFChars(value, &isValueCopy);
	}

	// Call the CFE interface function
	set_value(processID, actionString, keywordString, valueString);
	
	jthrowable exception;
	jboolean exceptionRaised = 0;

	// Check to see if the call to CFE (and subsequent calls to Java) generated an exception
	if ((exception = env->ExceptionOccurred()) != NULL) {
		exceptionRaised = 1;

		// Clear exception state from the VM, the exception is regenerated later
		env->ExceptionClear();
	}

	// Free up an memory allocated for the string character data
	if (isActionCopy == JNI_TRUE) {
		env->ReleaseStringUTFChars(action, actionString);
	}

	if (isKeywordCopy == JNI_TRUE) {
		env->ReleaseStringUTFChars(keyword, keywordString);
	}
	
	if (isValueCopy == JNI_TRUE) {
		env->ReleaseStringUTFChars(value, valueString);
	}

	// If we detected an exception from the CFE code then regenerate it and return
	// The reason why I attempt to tidy up the memory before throwing the exception
	// is in the vain hope that somehow this exception is not catastrophic and can be
	// dealt with else where.

	if (exceptionRaised) {
		env->Throw(exception);
		//return; // Okay, so I know that its just going to happen, but who knows who'll insert code after this point...
	}	
	
	env->MonitorExit(helper->lock);

}

JNIEXPORT void JNICALL put_value(int processID, const char *action, const char *keyword, const char *value) {
	
	// Get the singleton helper class with the JNI access instances & parameters
	JNICfeApiHelper *helper = JNICfeApiHelper::instance();
		
	JNIEnv *env = helper->getJNIEnv();

	// Turns the 'C' strings into Java Strings
	// Need to change this if someone decides to write 2byte International stuff
	jstring actionString  = 0;
	jstring keywordString = 0;
	jstring valueString   = 0;

	if (action != 0) {
		actionString = env->NewStringUTF(action);
	}

	if (keyword != 0) {
		keywordString = env->NewStringUTF(keyword);
	}

	if (value != 0) {
		valueString = env->NewStringUTF(value);
	}

	// Call back into Java to send the message using the instance object stored in the
	// current setValue or setArray call.
	env->CallVoidMethod(helper->getObjectInstance(), helper->putValue, 
		processID, actionString, keywordString, valueString);

	jboolean exceptionRaised = 0;
	jthrowable exception;

	// Check to see if the call to CFE (and subsequent calls to Java) generated an exception
	if ((exception = env->ExceptionOccurred()) != NULL) {
		exceptionRaised = 1;
		env->ExceptionClear();
	}

	// Tidy up references to local String instances. I dont actually need to do this but its
	// good practice.
	if (actionString != 0) {
		env->DeleteLocalRef(actionString);
	}

	if (keywordString != 0) {
		env->DeleteLocalRef(keywordString);
	}

	if (valueString != 0) {
		env->DeleteLocalRef(valueString);
	}

	// If exception was raised by calling back to Java then throw it onward now
	if (exceptionRaised) {
		env->Throw(exception);
		return;
	}
}


JNIEXPORT void JNICALL put_array(int processID, const char *action, const char *keyword, const char **array, int start, int end) {

	// Get the singleton helper class with the JNI access instances & parameters
	JNICfeApiHelper *helper = JNICfeApiHelper::instance();
	JNIEnv *env = helper->getJNIEnv();

	// Turns the 'C' strings into Java Strings
	// Need to change this if someone decides to write 2byte International stuff
	jstring actionString  = 0;
	jstring keywordString = 0;

	if (action != 0) {
		actionString  = env->NewStringUTF(action);
	}

	if (keyword != 0) {
		keywordString = env->NewStringUTF(keyword);
	}

	jobjectArray stringArray = 0;

	if (end-start >= 0 && array != 0) {
		// Allocate array storage in Java for the array of strings
		stringArray = env->NewObjectArray(end-start+1, helper->stringClass, NULL);

		// For each string in the 'C' array allocate then assign to the Java string array
		for (int i = start; i <= end; i++) {
			jstring element = env->NewStringUTF(array[i-start]);
			env->SetObjectArrayElement(stringArray, i - start, element);
			env->DeleteLocalRef(element);
		}
	}

	// Call back into Java to send the message using the instance object stored in the
	// current setValue or setArray call.
	env->CallVoidMethod(helper->getObjectInstance(), helper->putArray, 
		processID, actionString, keywordString, stringArray, start, end);

	jboolean exceptionRaised = 0;
	jthrowable exception;

	// Check to see if the call to CFE (and subsequent calls to Java) generated an exception
	if ((exception = env->ExceptionOccurred()) != NULL) {
		exceptionRaised = 1;
		env->ExceptionClear();
	}

	// Tidy up references to local String instances. I dont actually need to do this but its
	// good practice.
	if (actionString != 0) {
		env->DeleteLocalRef(actionString);
	}

	if (keywordString != 0) {
		env->DeleteLocalRef(keywordString);
	}

	if (stringArray != 0) {
		env->DeleteLocalRef(stringArray);
	}

	// If exception was raised by calling back to Java then throw it onward now
	if (exceptionRaised) {
		env->Throw(exception);
		return;
	}
}

#ifdef __cplusplus
}
#endif

