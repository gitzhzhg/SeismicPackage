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

/*
                             memory_cache.c

************************* COPYRIGHT NOTICE ****************************
*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
************************* COPYRIGHT NOTICE ****************************
C\USER DOC
-----------------------------------------------------------------------
                    SEISMIC PROCESSING WORKSTATION
                          C-LANGUAGE UTILITY 
                  designed to be called from C or C++

  Utility name:  memory_cache    (memory cache)

  Subdirectory:  cprim
  Library:       cprim.a
  Header file:   cprim.h
  Source file:   memory_cache.c

  Written:       94/11/04  by:  Tom Stoeckley
  Last revised:  94/11/04  by:  Tom Stoeckley

  Purpose:       To hold a selection of user data structures in memory
                 for later retrieval.

  Related Documentation:
-----------------------------------------------------------------------
                       GENERAL INFORMATION

  The purpose for writing this utility is to provide a way to
  hold onto copies of records in a disk file, to minimize repeated
  access to disk to get the same records.  Obviously, this utility
  can also be used for other purposes.

-----------------------------------------------------------------------
                   INPUT AND OUTPUT ARGUMENTS

  For each function documented here, each argument is flagged as
  follows:
      i = value required upon INPUT to the function.
      o = value set by the function upon OUTPUT.
      b = value BOTH required upon input and changed upon output.

  Values required for input may not necessarily be flagged in functions
  where all arguments are input arguments.

  For pointers, the flag (i,o,b) refers to the contents pointed to
  by the pointer, not to the value of the pointer itself.  The pointer
  value is required upon INPUT in all cases.
-----------------------------------------------------------------------
  To create a memory cache:

                o                          i       i
              cache = create_memory_cache(size, max_number)

  MemoryCache *cache = pointer to memory cache structure.
  long          size = size of each user data structure to keep in cache.
  long    max_number = maximum number of user data structures to keep in cache.

  The memory cache is allocated and initialized to contain no
    user data structures.
  NULL is returned if max_number <= 0 or the allocation is unsuccessful.
  It is OK to call the other functions with cache == NULL; the
    functions will simply behave as if there are no user data
    structures in the memory cache.

-----------------------------------------------------------------------
  To destroy the memory cache:

                o                            i
              cache = destroy_memory_cache(cache);

  MemoryCache *cache = pointer to memory cache structure.

  The memory cache is freed.
  Any user data structures contained in the cache are destroyed.
  A NULL is always returned.
  If this function is called with cache == NULL, nothing will happen.

-----------------------------------------------------------------------
  To add a user data structure to the memory cache:

                                          i     i      i
              void  add_to_memory_cache(cache, data, ident)

  MemoryCache *cache = pointer to memory cache structure.
  void         *data = user data structure to add to cache.
  long         ident = identification of user data structure.

  The contents of the specified user data structure are copied into the
    cache.
  If a user data structure of the specified ident is found, the user
    data structure inside the cache is replaced.
  If a user data structure of the specified ident cannot be found, the 
    user data structure is added to the cache.
  If the user data structure is to be added to the cache, and the cache 
    already contains the maximum number of user data structures, the user
    data structure which has not been accessed for the longest time is 
    destroyed before adding the new user data structure.
  If this function is called with cache == NULL, nothing will happen.

-----------------------------------------------------------------------
  To get a user data structure from the memory cache:

            o                             i     o      i
          point = get_from_memory_cache(cache, data, ident)

  MemoryCache *cache = pointer to memory cache structure.
  void         *data = user data structure to get from cache (can be NULL).
  long         ident = identification of user data structure.
  void        *point = pointer to user data structure inside cache.

  If a user data structure of the specified ident is found, a pointer to
    the user data structure inside the cache is returned.
  If no user data structure of the specified ident can be found, a NULL is
    returned.
  If the data argument is not NULL, and a user data structure of the
    specified ident is found, a copy of the user data structure inside
    the cache is copied to the location specified by the data argument.
  If this function is called with cache == NULL, NULL will be returned.

-----------------------------------------------------------------------
                        REVISION HISTORY

     Date      Author     Description
     ----      ------     -----------
  2.
  1. 94/11/04  Stoeckley  Initial version.
-----------------------------------------------------------------------
C\END DOC
*/


#include <string.h>
#include <limits.h>
#include <assert.h>
#include "cprim.h"




/*----------------------- hidden structure --------------------*/

struct _MemoryCache
{
  void **data;         /* list of pointers to user data structures */
  long  *ident;        /* list of idents of user data structures */
  long  *kount;        /* list of age counters */
  long   size;         /* size of each user data structure in bytes */
  long   max_number;   /* maximum number of user data structures to keep */
  long   number;       /* current number of user data structures */
  long   latest_kount; /* current age counter */
};




/*------------------ create memory cache ----------------------*/


MemoryCache *create_memory_cache(long size, long max_number)
{
  MemoryCache *cache;
  if(max_number <= 0) return NULL;
  cache = (MemoryCache*)malloc(sizeof(MemoryCache));
  if(!cache) return NULL;
  cache->data         = (void*)malloc(max_number * sizeof(void*));
  cache->ident        = (void*)malloc(max_number * sizeof(long));
  cache->kount        = (void*)malloc(max_number * sizeof(long));
  cache->size         = size;
  cache->max_number   = max_number;
  cache->number       = 0;
  cache->latest_kount = 0;
  if(!cache->data || !cache->ident || !cache->kount)
                            return destroy_memory_cache(cache);
  return cache;
}


/*----------------- destroy memory cache ---------------------*/

MemoryCache *destroy_memory_cache(MemoryCache *cache)
{
  int i;
  if(!cache) return NULL;
  for(i = 0; i < cache->number; i++)
     {
     if(cache->data[i]) free(cache->data[i]);
     }
  if(cache->data ) free(cache->data );
  if(cache->ident) free(cache->ident);
  if(cache->kount) free(cache->kount);
  free(cache);
  return NULL;
}


/*--------------- static boost kount ------------------------*/

static void static_boost_kount(MemoryCache *cache, int i)
{
  cache->kount[i] = cache->latest_kount;
  cache->latest_kount++;
  if(cache->latest_kount > LONG_MAX - 10)
      {
      long subtract = LONG_MAX / 2;
      int j;
      for(j = 0; j < cache->number; j++)
          {
          cache->kount[j] -= subtract;
          if(cache->kount[j] < 0) cache->kount[j] = 0;
          }
      cache->latest_kount -= subtract;
      }
}



/*
static void static_insert_data(MemoryCache *cache, void *data,
                                               long ident, int i)
{
  memcpy(cache->data[i], data, cache->size);
  cache->ident[i] = ident;
  static_boost_kount(cache, i);
}
*/



/*----------------- add to memory cache ---------------------*/

void add_to_memory_cache(MemoryCache *cache, void *data, long ident)
{
  int i, whichi=0, minimum=0;
  if(!cache) return;
  assert(cache->max_number > 0);
  assert(data);
  for(i = 0; i < cache->number; i++)
      {
      if(ident == cache->ident[i])
          {
          memcpy(cache->data[i], data, cache->size);
          static_boost_kount(cache, i);
          return;
          }
      }
  if(cache->number < cache->max_number)
      {
      cache->data[cache->number] = (void*)malloc(cache->size);
      if(!cache->data[cache->number]) return;
      memcpy(cache->data[cache->number], data, cache->size);
      cache->ident[cache->number] = ident;
      cache->number++;
 /*   static_boost_kount(cache, cache->number);    */   /* removed  7/22/96 */
      static_boost_kount(cache, cache->number - 1);     /* inserted 7/22/96 */
      return;
      }
  assert(cache->number > 0);
  for(i = 0; i < cache->number; i++)
      {
      if(i == 0 || cache->kount[i] < minimum)
          {
          minimum = cache->kount[i];
          whichi = i;
          }
      }
  memcpy(cache->data[whichi], data, cache->size);
  cache->ident[whichi] = ident;
  static_boost_kount(cache, whichi);
}



/*----------------- get from memory cache ---------------------*/

void *get_from_memory_cache(MemoryCache *cache, void *data, long ident)
{
  int i;
  if(!cache) return NULL;
  for(i = 0; i < cache->number; i++)
      {
      if(ident == cache->ident[i])
          {
          if(data) memcpy(data, cache->data[i], cache->size);
          static_boost_kount(cache, i);
          return cache->data[i];
          }
      }
  return NULL;
}



/*------------------------- end ------------------------------*/

