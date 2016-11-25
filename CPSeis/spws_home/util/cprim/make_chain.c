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
C      make_chain.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y
C             written in c -- designed to be called from c
C
C     Utility Name:  make_chain    (make linked chains)
C          Written:  93/02/08  by:  Tom Stoeckley
C     Last revised:  94/11/29  by:  Tom Stoeckley
C
C  Purpose:       Facilitate the management of linked chains of
C                 data structures.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            make_chain.c
C
C  static functions:       none
C
C  documented functions:   make_chain   get_chain    zero_link
C                          insert_link  remove_link  insert_link_before
C                          first_link   last_link    number_links
C                          prev_link    next_link
C                          free_chain   destroy_chain
C
C  macros (in cprim.h):    CpOffset     CpOffsetOf   MakeChain
C
C  The first two macros listed above are identical to XtOffset and 
C    XtOffsetOf, and are available in case Xlib/Xt is not available.
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C         (this utility does not reference X, Xt, and Motif)
C
C  libraries:     none
C  header files:  cprim.h
C  functions:     none
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  4. 94/11/29  Stoeckley  Add insert_link_before. Not yet tested.
C  3. 93/10/08  Stoeckley  Add free_chain and destroy_chain.
C  2. 93/06/07  Stoeckley  Move to library cprim.a.  Change to use 
C                            cprim.h and add three macros.
C  1. 93/02/08  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                       ARGUMENT DEFINITIONS
C
C ChainStruct *chain = pointer to opaque structure defining a linked chain.
C UserStruct   *ss   = pointer to ANY user structure to be linked into chains.
C LinkStruct   link  = structure containing the linking information.
C LinkStruct ss->link= the linking structure in the user structure.
C int       number   = number of links (user structures) in the chain.
C
C Note: The user never has to refer to the contents of chain or link.
C-----------------------------------------------------------------------
C              ROUTINES YOU ARE MOST LIKELY TO USE
C
C-------To create an empty linked chain:
C
C            chain = make_chain (XtOffsetOf(UserStruct, link))
C      -or-  chain = make_chain (CpOffsetOf(UserStruct, link))
C      -or-  chain = MakeChain  (           UserStruct, link )
C
C The last two options are available even if Xlib/Xt is not available.
C Returns NULL if not successful.
C
C-------To insert a user structure at the end of the chain:
C-------To insert a user structure at a specified location in the chain:
C-------To remove a user structure from the chain:
C
C            void insert_link        (chain, (void*)ss)
C            void insert_link_before (chain, (void*)ss, (void*)ss_in_chain)
C            void remove_link        (chain, (void*)ss)
C
C insert_link and insert_link_before initialize ss->link appropriately.
C insert_link        puts ss at the end of the chain.
C insert_link_before puts ss in the chain just before ss_in_chain.
C insert_link_before is not yet tested.
C remove_link zero's out ss->link to remove linking information.
C All three routines modify ss->link in the preceding and following
C   user structures in the chain, and modify information in chain.
C These routines do nothing if chain is NULL.
C
C-------To get a pointer to the first user structure in the chain:
C-------To get a pointer to the last user structure in the chain:
C
C            ss = (UserStruct*) first_link (chain)
C            ss = (UserStruct*)  last_link (chain)
C
C These routines both return NULL if the chain is NULL or empty.
C
C-------To get a pointer to the preceding user structure in the chain:
C-------To get a pointer to the next user structure in the chain:
C
C            ss = (UserStruct*) prev_link (chain, (void*)ss)
C            ss = (UserStruct*) next_link (chain, (void*)ss)
C
C These routines both return NULL if the chain is NULL or empty.
C prev_link returns NULL if ss is the first user structure in the chain.
C next_link returns NULL if ss is the last user structure in the chain.
C-----------------------------------------------------------------------
C                    LESS IMPORTANT ROUTINES 
C
C-------To get the number of user structures in the chain:
C
C             number = number_links (chain)
C
C Returns 0 if the chain is NULL or empty.
C
C-------To get a pointer to the structure defining a linked chain, from the
C       linking structure in a user structure:
C
C            chain = get_chain (ss->link)
C
C This function is provided in case the structure defining a linked chain
C   is not directly accessible.
C
C-------To zero out the linking structure in a user structure:
C
C                void zero_link (&ss->link)
C
C This is not required when insert_link or remove_link is called.
C This is dangerous when used by itself, since it breaks the chain; it
C   should be done only when the entire chain is being destroyed;
C   otherwise, remove_link should be used instead.
C Does nothing if &ss->link is NULL.
C
C-------To destroy a linked chain:
C
C               chain = free_chain    (chain)
C               chain = destroy_chain (chain)
C
C These routines call remove_link for each user structure in the chain,
C   and then free the chain.
C The first routine leaves the user structures intact.
C The second routine also frees the user structures in the chain.
C These routines always return NULL.
C-----------------------------------------------------------------------
C                          SAMPLE USE
C
C #include "make_chain.h"
C
C typedef struct _UserStruct
C   { int a;
C     float b;
C     LinkStruct link;     <-- this is needed in the structure
C     char c;
C     double d;
C   }   UserStruct;
C
C UserStruct *ss;
C ChainStruct *chain;
C
C---------------create a new linked chain:
C
C chain = make_chain(CpOffsetOf(UserStruct,link));
C
C---------------allocate user structure and add it to the chain:
C
C ss = (UserStruct*)calloc(1,sizeof(UserStruct));
C insert_link(chain, (void*)ss);
C
C---------------remove user structure from chain and de-allocate it:
C
C remove_link(chain, (void*)ss);
C free(ss);
C
C---------------loop through all user structures in the chain:
C
C for( ss = (UserStruct*)first_link(chain); ss; 
C      ss = (UserStruct*)next_link(chain, (void*)ss) )
C   {
C       >> work with ss here <<
C   }
C
C---------------or (less elegantly):
C
C ss = (UserStruct*)first_link(chain); 
C for( i = 0; i < number_links(chain); i++) 
C   {
C       >> work with ss here <<
C      ss = (UserStruct*)next_link(chain, (void*)ss);
C   }
C
C-----------------------------------------------------------------------
C\END DOC
*/

#include "cprim.h"


struct _ChainStruct
{
    void         *first;     /* pointer to first structure in chain */
    void         *last;      /* pointer to last structure in chain  */
    int          number;     /* number of structures in chain       */
    unsigned int offset;     /* offset of LinkStruct in user structure */
} ;



ChainStruct *make_chain(unsigned int offset)
{
  ChainStruct *chain;

  chain = (ChainStruct*)calloc(1,sizeof(ChainStruct));
  if(!chain) return NULL;
  chain->first  = NULL;
  chain->last   = NULL;
  chain->number = 0;
  chain->offset = offset;
  return chain;
}


ChainStruct *get_chain(LinkStruct link)
{
  return link.chain;
}



void zero_link(LinkStruct *link)
{
  if(!link) return;
  link->prev = NULL;
  link->next = NULL;
  link->chain= NULL;
}
  



void insert_link(ChainStruct *chain, void* ss)
{
  LinkStruct *link, *linkprev;

  if(!chain) return;
  link = (LinkStruct*)((char*)ss + chain->offset);
  if(chain->last)
       {
       linkprev = (LinkStruct*)((char*)chain->last + chain->offset);
       linkprev->next = ss;
       link->prev = chain->last;
       }
  else
       {
       chain->first = ss;
       link->prev = NULL;
       }
  link->next = NULL;
  link->chain= chain;
  chain->last = ss;
  chain->number++;
}



   /* the following routine is not yet tested */

void insert_link_before(ChainStruct *chain, void* ss, void *ss_in_chain)
{
  LinkStruct *link, *linkprev, *linknext;

  if(!chain) return;
  link     = (LinkStruct*)((char*)ss          + chain->offset);
  linknext = (LinkStruct*)((char*)ss_in_chain + chain->offset);
  if(linknext->prev)
       {
       linkprev = (LinkStruct*)((char*)linknext->prev + chain->offset);
       linkprev->next = ss;
       link->prev = linknext->prev;
       }
  else
       {
       chain->first = ss;
       link->prev = NULL;
       }
  linknext->prev = ss;
  link->next = ss_in_chain;
  link->chain= chain;
  chain->number++;
}




void remove_link(ChainStruct *chain, void* ss)
{
  LinkStruct *link, *linkprev, *linknext;

  if(!chain) return;
  link = (LinkStruct*)((char*)ss + chain->offset);
  if(link->prev)
       {
       linkprev = (LinkStruct*)((char*)link->prev + chain->offset);
       linkprev->next = link->next;
       }
  else
       {
       chain->first   = link->next;
       }
  if(link->next)
       {
       linknext = (LinkStruct*)((char*)link->next + chain->offset);
       linknext->prev = link->prev;
       }
  else
       {
       chain->last    = link->prev;
       }
  link->prev = NULL;
  link->next = NULL;
  link->chain= NULL;
  chain->number--;
}




void *first_link (ChainStruct *chain)
                     { if(!chain) return NULL; return chain->first;  }
void *last_link  (ChainStruct *chain)
                     { if(!chain) return NULL; return chain->last;  }
int number_links (ChainStruct *chain)
                     { if(!chain) return 0; return chain->number;  }




void *prev_link(ChainStruct *chain, void *ss)
{
  LinkStruct *link;

  if(!chain) return NULL;
  link = (LinkStruct*)((char*)ss + chain->offset);
  return link->prev;
}



void *next_link(ChainStruct *chain, void *ss)
{
  LinkStruct *link;

  if(!chain) return NULL;
  link = (LinkStruct*)((char*)ss + chain->offset);
  return link->next;
}



ChainStruct *free_chain(ChainStruct *chain)
{
  void *ss;

  if(!chain) return NULL;
  ss = first_link(chain);
  while(ss)
       {
       remove_link(chain, ss);
       ss = first_link(chain);
       }
  free(chain);
  return NULL;
}



ChainStruct *destroy_chain(ChainStruct *chain)
{
  void *ss;

  if(!chain) return NULL;
  ss = first_link(chain);
  while(ss)
       {
       remove_link(chain, ss);
       free(ss);
       ss = first_link(chain);
       }
  free(chain);
  return NULL;
}


