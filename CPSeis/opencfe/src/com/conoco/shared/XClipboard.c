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
#include "com_conoco_shared_XClipboard.h"
#include "X11/Xlib.h"
#include <string.h>

#define WAITLOOPS 100

int errLevel = 0;

/*
     Modified by Steve Cook, January 2001.
       --added writeCutBuffer0 and writePrimarySelection
	    support to allow 2-way cut & paste.
       --note that this also entailed modifying XClipboard.java.
*/

/*
     XClipboard.c:
     	Implement some X-specific methods to access X selections.

     Copyright (c) 1999 Nathan Meyers

     Permission is hereby granted, free of charge, to any person obtaining
     a copy of this software and associated documentation files (the
     "Software"), to deal in the Software without restriction, including
     without limitation the rights to use, copy, modify, merge, publish,
     distribute, sublicense, and/or sell copies of the Software, and to
     permit persons to whom the Software is furnished to do so, subject
     to the following conditions:

     The above copyright notice and this permission notice shall be
     included in all copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
     KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
     WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
     NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
     BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
     AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
     IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
     THE SOFTWARE.
*/

/* Native component to XClipboard class */

/* We will store some platform-specific values - the Display* pointer
   and a Window ID - in a byte array. The following functions handle
   the conversions back and forth to native values. */

jbyteArray bytes_to_byte_array(JNIEnv *env, void *bytes, int nbytes)
{
    jbyteArray ptr_bytes = (*env)->NewByteArray(env, nbytes);
    jboolean is_copy;
    jbyte *ptr_bytes_elts =
    	(*env)->GetByteArrayElements(env, ptr_bytes, &is_copy);
    memcpy(ptr_bytes_elts, bytes, nbytes);
    (*env)->ReleaseByteArrayElements(env, ptr_bytes, ptr_bytes_elts, 0);
    return ptr_bytes;
}

void byte_array_to_bytes(JNIEnv *env, jbyteArray ptr_bytes, void *bytes,
			 int nbytes)
{
    jboolean is_copy;
    jbyte *ptr_bytes_elts =
    	(*env)->GetByteArrayElements(env, ptr_bytes, &is_copy);
    memcpy(bytes, ptr_bytes_elts, nbytes);
    (*env)->ReleaseByteArrayElements(env, ptr_bytes, ptr_bytes_elts, 0);
}

typedef struct
{
    Display *display;
    Window window;
} platform_data;

/* Open the connection at object construction. A pity the JVM won't
   share its connection with us. */
jbyteArray JNICALL Java_com_conoco_shared_XClipboard_openXConnection
    (JNIEnv *env, jobject obj)
{
    platform_data platform;
    platform.display = XOpenDisplay(0);
    platform.window = XCreateWindow(platform.display,
    				DefaultRootWindow(platform.display),
				0, 0, 1, 1, 0, CopyFromParent,
				CopyFromParent, CopyFromParent,
				0, 0);
    return bytes_to_byte_array(env, &platform, sizeof(platform));
}

/* Close the connection at object finalization */
jlong JNICALL Java_com_conoco_shared_XClipboard_closeXConnection
    (JNIEnv *env, jobject obj, jbyteArray pdata)
{
    platform_data platform;
    byte_array_to_bytes(env, pdata, &platform, sizeof(platform));
    return XCloseDisplay(platform.display);
}

/* Error level, primarily for debugging */
void JNICALL Java_com_conoco_shared_XClipboard_setErrorLevel
  (JNIEnv *env, jobject obj, jint val)
{
    errLevel=val;
}

/* writePrimarySelection (added by Steve Cook, January 2001) */
jint JNICALL Java_com_conoco_shared_XClipboard_writePrimarySelection
    (JNIEnv *env, jobject obj, jbyteArray pdata, jbyteArray writeThis)
{
    platform_data platform;
/*    char *bytes; */
    jbyte *bytes;
    int i,nbytes;
    jboolean is_copy;
    XEvent event;
    Atom selection,target;

    byte_array_to_bytes(env, pdata, &platform, sizeof(platform));

    selection = XInternAtom(platform.display, "PRIMARY", True);
    if(selection == None)
		{
		if(errLevel>4)printf("selection was None\n");
		return 1;
		}

    target = XInternAtom(platform.display, "STRING", False);
    if(target == None)
		{
		if(errLevel>4)printf("target was None\n");
		return 2;
		}

    XSetSelectionOwner(platform.display, selection, platform.window, CurrentTime);
    if(XGetSelectionOwner(platform.display,selection) != platform.window)
		{
		if(errLevel>4)printf("not owner\n");
		return 3;
		}

    if(errLevel>4)printf("selection,target=%li %li\n",selection,target);
    if(errLevel>4)printf("converting selection type PRIMARY to STRING \n");

    XConvertSelection(platform.display, selection, target, selection,
    		      platform.window, CurrentTime);

    XNextEvent(platform.display, &event);
    if (event.type == SelectionNotify)
	{
	if(errLevel>4)printf("write got SelectionNotify event at i=%i\n",i);
	}
    else if (event.type == SelectionRequest)
	{
	if(errLevel>4)printf("write got SelectionRequest event at i=%i\n",i);
	}

    nbytes = (*env)->GetArrayLength(env, writeThis);
    bytes=(*env)->GetByteArrayElements(env, writeThis, &is_copy);

    XChangeProperty(platform.display, platform.window,
	selection, target, 8, PropModeReplace, bytes, nbytes);

    return 0;
}

/* writeCutbuffer0 (added by Steve Cook, January 2001) */
jint JNICALL Java_com_conoco_shared_XClipboard_writeCutBuffer0
    (JNIEnv *env, jobject obj, jbyteArray pdata, jbyteArray writeThis)
{
    platform_data platform;
/*    char *bytes; */
    jbyte *bytes;
    int nbytes;
    jboolean is_copy;

    nbytes = (*env)->GetArrayLength(env, writeThis);
    byte_array_to_bytes(env, pdata, &platform, sizeof(platform));
    bytes=(*env)->GetByteArrayElements(env, writeThis, &is_copy);

    XStoreBytes(platform.display, bytes, nbytes);

    return 0;
}

/* Read cutbuffer0, in the unlikely event anyone cares about it */
jbyteArray JNICALL Java_com_conoco_shared_XClipboard_readCutBuffer0
    (JNIEnv *env, jobject obj, jbyteArray pdata)
{
    int nbytes;
    platform_data platform;
    char *result;
    jbyteArray result_array;
    jboolean is_copy;
    jbyte *result_array_elts;

    byte_array_to_bytes(env, pdata, &platform, sizeof(platform));
    result = XFetchBytes(platform.display, &nbytes);
    result_array = (*env)->NewByteArray(env, (jsize)nbytes);
    result_array_elts =
    	(*env)->GetByteArrayElements(env, result_array, &is_copy);
    memcpy(result_array_elts, result, nbytes);
    (*env)->ReleaseByteArrayElements(env, result_array, result_array_elts, 0);
    return result_array;
}

jobject JNICALL Java_com_conoco_shared_XClipboard_readSelection
  (JNIEnv *env, jobject obj, jbyteArray pdata, jbyteArray jselection,
   jbyteArray jtarget)
{
    platform_data platform;
    Atom selection, target;
    jbyte *bptr;
    jboolean is_copy;
    XSelectionEvent *sel_event;
    XEvent event;
    Atom return_type;
    long offset, length;
    int i, return_format, return_result;
    unsigned long return_nitems;
    unsigned long return_bytes_remaining;
    unsigned char *return_data;
    jobject result_array;
    void *result_bytes;

    /* Recover our platform data */
    byte_array_to_bytes(env, pdata, &platform, sizeof(platform));

    /* Get atom for the selection (e.g. "PRIMARY") */
    bptr = (*env)->GetByteArrayElements(env, jselection, &is_copy);
    selection = XInternAtom(platform.display, (char *)bptr, True);
    (*env)->ReleaseByteArrayElements(env, jselection, bptr, 0);
    if (selection == None) return 0;

    /* Get atom for the target (e.g. "STRING") */
    bptr = (*env)->GetByteArrayElements(env, jtarget, &is_copy);
    target = XInternAtom(platform.display, (char *)bptr, True);
    (*env)->ReleaseByteArrayElements(env, jtarget, bptr, 0);
    if (target == None) return 0;

    /* Convert the selection to the designated target type; we'll use
       the selection name as the target property name. */
    XConvertSelection(platform.display, selection, target, selection,
    		      platform.window, CurrentTime);

    XNextEvent(platform.display, &event);
    if (event.type == SelectionNotify)
	{
	if(errLevel>4)printf("read got SelectionNotify event at i=%i\n",i);
	}
    else if (event.type == SelectionRequest)
	{
	if(errLevel>4)printf("read got SelectionRequest event at i=%i\n",i);
	}

    sel_event = (XSelectionEvent *)&event;
    if (sel_event->property == None) return 0;

    /* We have a hit! How big? */
    return_result =
        XGetWindowProperty(platform.display, platform.window,
    		           sel_event->property, 0L, 0L, False,
		           AnyPropertyType, &return_type,
			   &return_format, &return_nitems,
			   &return_bytes_remaining, &return_data);
    if (return_result != Success) return 0;
    XFree(return_data);
    /* Allocate a result */
    switch (return_format)
    {
    	case 8:
	    result_array =
	    	(*env)->NewByteArray(env, (jsize)return_bytes_remaining);
	    result_bytes =
		(*env)->GetByteArrayElements(env, result_array, &is_copy);
	    break;
	case 16:
	    result_array =
	    	(*env)->NewShortArray(env, (jsize)return_bytes_remaining / 2);
	    result_bytes =
		(*env)->GetShortArrayElements(env, result_array, &is_copy);
	    break;
	case 32:
	    result_array =
	    	(*env)->NewIntArray(env, (jsize)return_bytes_remaining / 4);
	    result_bytes =
		(*env)->GetIntArrayElements(env, result_array, &is_copy);
	    break;
    }
    /* Grab the bits */
    offset = 0;
    length = 1024;
    while (return_bytes_remaining)
    {
        return_result =
            XGetWindowProperty(platform.display, platform.window,
    		               sel_event->property, offset, length, True,
		               AnyPropertyType, &return_type,
			       &return_format, &return_nitems,
			       &return_bytes_remaining, &return_data);
        if (return_result != Success) break;
	memcpy((char *)result_bytes + offset * 4,
	       return_data,
	       return_nitems * return_format / 8);
	offset += return_nitems * return_format / 32;
	XFree(return_data);
    }
    /* Release the result */
    switch (return_format)
    {
    	case 8:
	    (*env)->ReleaseByteArrayElements(env, result_array,
	    			(jbyte *) result_bytes, 0);
	    break;
	case 16:
	    (*env)->ReleaseShortArrayElements(env, result_array,
	    			(jshort *)result_bytes, 0);
	    break;
	case 32:
	    (*env)->ReleaseIntArrayElements(env, result_array,
	    			(jint *)  result_bytes, 0);
	    break;
    }
    return result_array;
}
