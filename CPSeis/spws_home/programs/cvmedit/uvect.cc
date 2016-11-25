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
#include <string.h>
#include <stdlib.h>
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
int UVectNameList(void *ll,int *num ,int lsiz, char **names);
void UVectNameFree(char **names);
#ifdef __cplusplus
}                   // for C++
#endif

/***************
 * Return a list of unique names in a vector list.
 * Free memory for the members when you are done with the list
 * Returns number of names if ok. Returns -1 if bad argument.
 **************/
int UVectNameList(void *ll,int *num ,int lsiz, char **names)
{SeisVectLinkedList *vls;
 Vector   *vect;
 const char *name;
 int  j;

 vls = (SeisVectLinkedList *) ll;
 *num= 0;
 if(vls==NULL || names==NULL) return -1;
 names[*num]=NULL;

/* Find all the unique names in the list of vectors */
 vect = vls->top();
 if(vect == NULL) return 0;
 name = vect->getName();
 if(name != NULL)
  { names[*num] = (char *) malloc(strlen(name)+1);
    strcpy(names[*num],name);
    *num    += 1;
  }
 while((vect=vls->next()) != NULL)
  { name = vect->getName();
    if(name != NULL)
     { for(j=0;j<*num;j++)
        {if(strcmp(name, names[j])==0) break; }
       if(j==*num)
        { names[*num] = (char *) malloc(strlen(name)+1);
          strcpy(names[*num],name);
          *num    += 1;
          names[*num] = NULL;
          if(*num>= lsiz)
           { UVectNameFree(names);
             return 0;
           }
        }
     }
   }

 names[*num] = NULL;
 return *num;
}

void UVectNameFree(char **names)
{
  int num=0;
  if(names==NULL) return;
  while(names[num] != NULL)
   { free(names[num]);
     num++;
   }
}

