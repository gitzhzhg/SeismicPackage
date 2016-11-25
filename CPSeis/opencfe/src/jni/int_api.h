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
#ifndef _int_api_h
#define _int_api_h

#include  "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define set_value                    set_value_
#define set_array                    set_array_
#define get_debug_level              get_debug_level_
#define int_api_set_value            int_api_set_value_
#define int_api_set_array            int_api_set_array_
#define int_api_put_value            int_api_put_value_
#define int_api_put_array            int_api_put_array_
#define int_api_put_pointers         int_api_put_pointers_
#define put_array                    put_array_
#define put_value                    put_value_
#endif
 
#ifdef NEED_CAPITALS
#define set_value                    SET_VALUE
#define set_array                    SET_ARRAY
#define get_debug_level              GET_DEBUG_LEVEL
#define int_api_set_value            INT_API_SET_VALUE
#define int_api_set_array            INT_API_SET_ARRAY
#define int_api_put_value            INT_API_PUT_VALUE
#define int_api_put_array            INT_API_PUT_ARRAY
#define int_api_put_pointers         INT_API_PUT_POINTERS
#define put_array                    PUT_ARRAY
#define put_value                    PUT_VALUE
#endif

#ifndef JNIEXPORT
#define JNIEXPORT
#endif

#ifndef JNICALL
#define JNICALL
#endif


#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT void JNICALL put_value(int processID, const char *action, const char *keyword, const char *value);
JNIEXPORT void JNICALL put_array(int processID, const char *action, const char *keyword, const char **array, int start, int end);

JNIEXPORT void JNICALL set_value(int processID, const char *action, const char *keyword, const char *value);
JNIEXPORT void JNICALL set_array(int processID, const char *action, const char *keyword, const char **array, int start, int end);

JNIEXPORT int JNICALL get_debug_level();

void int_api_set_value(int *processID, const char *action, const char *keyword, const char *value);
void int_api_set_array(int *processID, const char *action, const char *keyword, const char *array, int *start, int *end, int *siz);

void int_api_put_value(int *processID, const char *action, const char *keyword, const char *value);
void int_api_put_array(int *processID, const char *action, const char *keyword, const char *array, int *dim1, int *dim2, int *start, int *end);

void int_api_put_pointers(void (*pv) (int, const char *, const char *, const char *), void (*pa) (int, const char *, const char *, const char **, int, int));

#ifdef __cplusplus
}
#endif

#endif
