/*<CPS_v1 type="PRIMITIVE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>
*/
#include <stdio.h>
#include <string.h>

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define set_value                    set_value_
#define set_array                    set_array_
#define int_api_set_value            int_api_set_value_
#define int_api_set_array            int_api_set_array_
#define int_api_put_value            int_api_put_value_
#define int_api_put_array            int_api_put_array_
#define int_api_put_pointers         int_api_put_pointers_
#endif

#ifdef NEED_CAPITALS
#define set_value                    SET_VALUE
#define set_array                    SET_ARRAY
#define int_api_set_value            INT_API_SET_VALUE
#define int_api_set_array            INT_API_SET_ARRAY
#define int_api_put_value            INT_API_PUT_VALUE
#define int_api_put_array            INT_API_PUT_ARRAY
#define int_api_put_pointers         INT_API_PUT_POINTERS
#endif

#ifdef __cplusplus
extern "C"
{
#endif

void int_api_set_value(int *processID, const char *action, const char *keyword, const char *value);
void int_api_set_array(int *processID, const char *action, const char *keyword, const char *array, int *start, int *end, int *siz);

void int_api_put_value(int *processID, const char *action, const char *keyword, const char *value);
void int_api_put_array(int *processID, const char *action, const char *keyword, const char *array, int *dim1, int *dim2, int *start, int *end);

void int_api_put_pointers(void (*pv) (int, const char *, const char *, const char *), void (*pa) (int, const char *, const char *, const char **, int, int));


main()
{
   char **array_ptr;
   char *array[100];
   int  i;

   for (i=0;i<100;i++) {
     array[i] = (char *) malloc(81);
   }
   array_ptr = &(array[0]);


/********************************************************************/
/*** Replace the following section with the results of            ***/
/***     grep int_debug /var/tmp/rmi.user.pid | cut -d" " -f2-80  ***/
/********************************************************************/

set_value(-1, "BackEndLibPath" , "   " , "/usr/app/user/vundedk/cfelib/sol/libCFE.so");
set_value(-1, "InitializeApp" , "   " , "   ");
set_value(1, "ButtonPress" , "EXIT" , "   ");

/**********************************/
/*** End of replacement section ***/
/**********************************/


}
void put_value (int windowID, const char *action, const char *keyword, const char *value)
{

  printf("putValue: windowID=%d Action=%s Keyword=%s Value=\"%s\"\n",windowID,action,keyword,value);

}
void put_array (int windowID, const char *action, const char *keyword, const char **array, int start, int end)
{
  int i;

  printf("putArray: windowID=%d Action=%s Keyword=%s Start=%d End=%d\n",windowID,action,keyword,start,end);
  for (i=start;i<=end;i++) {
    printf("Array Element %d =\"%s\"\n",i,array[i-start]);
  }

}


#ifdef __cplusplus
}
#endif

