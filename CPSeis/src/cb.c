/*<CPS_v1 type="PRIMITIVE", pretag="!"/>
!------------------------------- cb.c ----------------------------------
!------------------------------- cb.c ----------------------------------
!------------------------------- cb.c ----------------------------------
!other files are:  cb.h
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
!
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : cb.c
! Category   : miscellaneous
! Written    : 2005-02-08   by: Bill Menger
! Revised    : 2005-12-05   by: Chuck Burch
! Maturity   : production
! Purpose    : Provides a circular buffer of pointers and functions with which
!              to access the buffer.
! Portability: Requires the pthreads (POSIX THREADS) library.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
! This is a simple circular FIFO buffer that only holds pointers to the data
! you wish to put and take from it.  This means that you (the programmer) must
! allocate memory and provide the buffer with the pointer when "putting" data
! on the buffer, then after "getting" it from the buffer, you must deallocate
! your memory.  You cannot re-use the same pointer unless you have finished
! with the data COMPLETELY!!! (i.e. taken it back off of the buffer).
! The buffer uses mutex locks and conditional waits so that you can send data
! to the buffer until it is full, at which time it will cause the sending oper-
! ation to wait until someone has removed data from the buffer.
!
! In a single threaded program, you will not find this to be of use unless you
! know how many items you need to store and you sequentially put everything on
! the buffer then sequentially remove the items.  
!
! In multi-threaded programs, you can have many threads putting and getting at
! the same time, which is where you will find the most useful application of
! the cb system.
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
! For pointers, the flag (i,o,b) refers to the contents pointed to
! by the pointer, not to the value of the pointer itself.  The pointer
! value (the address) is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
! See the header file for a description of what the cb_t "type" is.
!                           i
!    cb_t * cb_create(int qsize);
!           Purpose: Create the buffer.
!           qsize is the number of elements in the buffer.
! 
!                        i/o
!    void   cb_destroy(cb_t *);
!           Purpose: Destroy the buffer, return memory.
!
!                    i/o      i
!    void   cb_put(cb_t *, void *);
!           Purpose: To put a data pointer onto the buffer.
!           cb_t * is the instance of your buffer that was returned in the 
!                  create call.
!           void * is a pointer to your structure or array or item of data.
!
!                    i/o
!    void * cb_get(cb_t *);
!           Purpose: To return the next data pointer from the buffer using
!                    a FirstInFirstOut(FIFO) sequence.
!
!                           i
!    int    cb_numqueued(cb_t *);
!           Purpose: For diagnostics, to see how many items are on the queue.
!
!                          i
!    int    cb_getqsize(cb_t *);
!           Purpose: For diagnostics, to see how many elements are on the
!                    buffer.
!
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
! The main thing to remember with this software is that the queue is only
! holding a pointer to your data.  Therefore, you must ensure that your data
! has had memory allocated for it, and that you won't accidentally do something
! to the data at that memory location after putting it on the queue.  Sort of
! like putting your kid on a school bus.  You don't get to do anything with your
! kid until they come home off of the bus, but you had to create the kid before 
! the bus would take them. (bad analogy, but accurate).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2005-12-05  Chuck Burch  Added qsize to cb_t structure & some error checks
!                              Added unit test for debugging use
!  2. 2005-07-26  Bill Menger  Add return variables on pthread calls, added
!                              calls to cb_msg. 
!  1. 2005-02-08  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
! Uses the POSIX pthreads library.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS 
!
! must link with -pthreads
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
! The buffer is circular, and the number of elements in the circle is determined
! by the creation subroutine.  As a pointer is put onto one of the placeholders
! in the circle, the pointer is incremented to the next available placeholder.
! If all spots are taken, then a conditional wait is executed that keeps you
! from putting any more on the buffer until some process picks up the first
! item that was put on the buffer, freeing up a spot.  Similarly, if you are
! taking items from the buffer and you get to the last item, when you ask for
! one more, you are put in a conditional wait until som other process puts an
! item on the buffer.  Typically, these processes are threads that share the
! data space of the buffer.
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES 
! This is a good example of how conditional waits and mutex locks can work
! to keep threads synchronized in order to perform a function.
!
! This program contains a unit test that should be run with any changes 
! to the code.  If additional functionality is added, then a test for
! that functionality should be added to the unit test. If a bug is found with
! the code, then a test to catch the bug should be added to the unit test to
! prevent a future reoccurrence of the bug.
!
! To compile with the unit test: 
!            gcc -Wall -DcbUnitTest -o cbUnitTest cb.c -l pthread
! To run the unit test: cbUnitTest [#threads [size_circular_buffer [#packets]]]
!     [] means optional
!
!-------------------------------------------------------------------------------
!</programming_doc>
*/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char CB_IDENT[100] =
"$Id: cb.c,v 1.3 2005/12/07 12:37:16 Burch prod sps $";


#include "cb.h"
#ifdef linux
#include <asm/errno.h>  /* for ETIMEDOUT */
#else
#include <sys/errno.h>  /* for ETIMEDOUT */
#endif


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/************************** CB_CREATE *************************************/
cb_t * cb_create(int qsize) {
  int  i,prv;
  cb_t *cbp;
  
  if(qsize <= 0) return NULL;
  
  cbp = (cb_t *) malloc (sizeof (cb_t));
  if (cbp == NULL) return (NULL);
 
  prv=pthread_mutex_init(&cbp->buf_lock,NULL);
  cb_msg("cb_create:Error mutex_init",&prv);
  prv=pthread_cond_init(&cbp->notfull,NULL);
  cb_msg("cb_create:Error cond_init",&prv);
  prv=pthread_cond_init(&cbp->notempty,NULL);
  cb_msg("cb_create:Error cond_init",&prv);
  
  cbp->start_idx = 0;
  cbp->num_full  = 0;
  cbp->qsize     = qsize; 
  
  cbp->data = (void **) malloc(qsize * sizeof (void *) );
  if(cbp->data == NULL) {
    cb_destroy(cbp);
    return (NULL);
  }
  
  for(i=0;i<qsize;i++){
    cbp->data[i] = NULL;
  }
  
  return (cbp);
}

/************************** CB_GETQSIZE ***********************************/
/*** get the qsize from the cb system
 ***
 ***/

int cb_getqsize(cb_t * cbp) {
  if(cbp==NULL) return(0);
  return cbp->qsize;
}

/************************** CB_DESTROY ************************************/
/*** delete the circular buffer
 ***                    *call this after closing last file *
 ***/

void cb_destroy(cb_t * cbp) {
  int i,prv;

  if(cbp== NULL) return;
  
  prv=pthread_mutex_destroy(&cbp->buf_lock);
  cb_msg("cb_destroy:Error mutex_destroy",&prv);
  prv=pthread_cond_destroy(&cbp->notfull);
  cb_msg("cb_destroy:Error cond_destroy",&prv);
  prv=pthread_cond_destroy(&cbp->notempty);
  cb_msg("cb_destroy:Error cond_destroy",&prv);

  if(cbp->data != NULL) {  
    for (i=0;i<cbp->qsize;i++){
      if(cbp->data[i] != NULL) free(cbp->data[i]);
    }
    free(cbp->data);
  }
  
  free(cbp);
  return;
}

/************************** CB_PUT ****************************************/
/*** put data on the circular buffer queue
 ***
 ***/

void cb_put(cb_t * cbp, void * data) {
  int new_index,prv;

  prv=pthread_mutex_lock(&cbp->buf_lock);
  cb_msg("cb_put:Error mutex_lock",&prv);
  
  /* wait until buffer is not full */
  while(cbp->num_full == cbp->qsize) {
    prv=pthread_cond_wait(&cbp->notfull, &cbp->buf_lock);
    cb_msg("cb_put:Error cond_wait",&prv);
  }
  
  /* move the data into queue and incr the number of full index positions. */
  /* new_index = (cbp->start_idx + cbp->num_full) %cbp->qsize; */
  new_index = cbp->start_idx + cbp->num_full;
  if(new_index >= cbp->qsize) new_index -= cbp->qsize;
  cbp->data[new_index] = data;
  cbp->num_full++;
  
  /* let a waiting "get routine" know data is available */
  prv=pthread_cond_signal(&cbp->notempty);
  cb_msg("cb_put:Error cond_signal",&prv);
  
  prv=pthread_mutex_unlock(&cbp->buf_lock);
  cb_msg("cb_put:Error mutex_unlock",&prv);
}

/************************** CB_GET ****************************************/
/*** get data from the circular buffer queue
 ***
 ***/

void * cb_get(cb_t * cbp) {
  int prv;
  void *data;
  
  prv=pthread_mutex_lock(&cbp->buf_lock);
  cb_msg("cb_get:Error mutex_lock",&prv);
  
  /* wait until something shows up in the buffer */
  while (cbp->num_full == 0){
    prv=pthread_cond_wait(&cbp->notempty,&cbp->buf_lock);
    cb_msg("cb_get:Error cond_wait",&prv);
  }
  
  data = cbp->data[cbp->start_idx];
  cbp->data[cbp->start_idx]=NULL;
  /* cbp->start_idx = (cbp->start_idx+1)%cbp->qsize; */
  if((++cbp->start_idx) == cbp->qsize) cbp->start_idx=0;
  cbp->num_full--;
  
  /* let a waiting "put routine" know there is room. */
  prv=pthread_cond_signal(&cbp->notfull);
  cb_msg("cb_get:Error cond_signal",&prv);
  prv=pthread_mutex_unlock(&cbp->buf_lock);
  cb_msg("cb_get:Error mutex_unlock",&prv);

  return (data);
}

/************************** CB_NUMQUEUED **********************************/
int  cb_numqueued(cb_t * cbp) {
  int numqueued,prv;
  
  prv=pthread_mutex_lock(&cbp->buf_lock);
  cb_msg("cb_numqueued:Error mutex_lock",&prv);
  numqueued = (int) cbp->num_full;
  prv=pthread_mutex_unlock(&cbp->buf_lock);
  cb_msg("cb_numqueued:Error mutex_unlock",&prv);
  return numqueued;
}

/************************** CB_MSG ****************************************/
void cb_msg(char *caller,int *prv){
  char str[132];
  
  switch (*prv) {
    case(0): return; 
    case(EINVAL):
      strcpy(str,"EINVAL:The value specified is invalid.");
      break;  
    case(EBUSY):
      strcpy(str,"EBUSY:The lock is busy.");
      break;  
    case(EAGAIN):
      strcpy(str,"EAGAIN:System lacks necessary resources.");
      break;  
    case(EDEADLK):
      strcpy(str,"EDEADLK:Deadlock condition.");
      break;  
    case(EPERM):
      strcpy(str,"EPERM:The caller does not have permission.");
      break;  
    case(ESRCH):
      strcpy(str,"ESRCH:Not found.");
      break;  
    case(ETIMEDOUT):
      strcpy(str,"ETIMEDOUT:Timeout.");
      break;  
    case(EINTR):
      strcpy(str,"EINTR.");
      break;  
    default:
    strcpy(str,"Unknown error code.");
  }
  fprintf(stderr,"Thread#%d:%s: %s %d\n",(int) pthread_self(),caller,str,*prv);
  return; 
}

/******************** end of circular buffer routines *************************/
#ifdef __cplusplus
}
#endif

/********************************* cb unit test *******************************/
#ifdef cbUnitTest

/**********************************************************************
** basic unit test for cb.c,  Written 09/02/2005 by CC Burch
**
** Link: gcc -Wall -DcbUnitTest -o cbUnitTest cb.c
** Usage: cbUnitTest #threads #circular_buffer_entries #packets
**
** Main program will set up threads and the circular buffer,
**   send data to circular buffer, send signal for threads to quit and
**   print statistics on any data errors or entries not processed
**
** process_thread will get data from circular buffer, print data,
**   verify data, ensure no data processed more than once,
**   and collect performance statistics
************************************************************************/

#include <sys/stat.h>
#include <sys/types.h>
#include <stdlib.h>

#include "cb.h"

pthread_t **threads;
cb_t      *cbp, *cbp1;
int       *checks, npackets, packet_errors;       

/** thread for processing data from circular buffer **/
void* cb_process_thread(void *threadarg){
  char        *data, data_check[80];
  int         threadid, depth, nprocessed, tot_depth, n;

  threadid = *((int *)threadarg);
  nprocessed=0;
  tot_depth=0;

  /* Loop to process data from cb until told to shut down */
  while(1) {
    if( (data=(char *)cb_get(cbp)) == NULL) break;  /*see if to shut down */

    /* process data received from cb */
    depth = cb_numqueued(cbp);
    tot_depth += depth;
    nprocessed++;
    printf("process_thread(%d), str=%s, depth=%d\n",threadid,data,depth);

    /* Check validity of data and see if already processed*/
    n=-1;
    sscanf(data,"%s %d",data_check, &n);

    if(strcmp(data_check, "String") != 0 || n < 0 || n >= npackets) {
      printf("*** Error: Invalid data(%s) in process_thread\n", data);
      packet_errors++; /* really need lock, but if ==0 at end, no errors*/
    }
    
    if(n >= 0 && n < npackets){ 
      if(checks[n] != -1) {
        printf("*** Error: data(%s) previously processed by thread(%d)\n",
               data, checks[n]);
        packet_errors++; /* really need lock, but if ==0, no errors*/
      }
      checks[n]=threadid;
    }
    
    fflush(stdout);
    free(data);
  }

  /* print basic statistics and shut down */
  if( (n=nprocessed) == 0) n=1;
  printf("process_thread(%d) exit: processed=%d, avg depth=%5.2f\n",
    threadid, nprocessed, tot_depth/((float)n));
  fflush(stdout);
  pthread_exit((void *) (&threadid));
  return(threadarg);   /*not really executed */
}

int main(int argc, char *argv[]) {
  int nthreads, qsize, i, n, istat, *is;
  char *data;

  /* get # threads, qsize & # packets from command arguments or defaults*/
  nthreads=5;
  if(argc >= 2) nthreads=atoi(argv[1]);
  qsize=10;
  if(argc >=3) qsize=atoi(argv[2]);
  npackets=nthreads*qsize*5;
  if(argc >=4) npackets=atoi(argv[3]);

  packet_errors=0;
  printf("Running cbUnitTest with %d threads and qsize of %d\n",
    nthreads,qsize);
  
  if(nthreads < 2 || qsize < 2) {
    printf(
      "Error: Number threads(%d) and Circular_buffer_size(%d) must be >=2\n",
      nthreads, qsize);
    exit(-1);
  }    
  
  /* create cb and cbp1; verify qsize is proper with multiple cb creates */
  if( (cbp = cb_create(qsize)) == NULL) {
    printf("Error: cbp create failure with qsize=%d\n", qsize);
    exit(-1);
  }
  
  n=qsize+1;
  if( (cbp1 = cb_create(n)) == NULL) {    /*2nd cb with different size*/
    printf("Error: cbp1 create failure with qsize=%d\n", n);
    exit(-1);
  }
  
  if( (i=cb_getqsize(cbp)) != qsize) {
    printf("Error: cbp qsize: got=%d, should=%d\n",i,qsize);
    exit(-1);
  }
  
  if( (i=cb_getqsize(cbp1)) != n) {
    printf("Error: cbp1 qsize: got=%d, should=%d\n",i,n);
    exit(-1);
  }
  cb_destroy(cbp1);          /*get rid of 2nd cb*/
  
  /* Create nthreads threads to use process_threads */
  threads = (pthread_t **) malloc (nthreads * sizeof(pthread_t *));
  if(threads == NULL) {
    printf("Error: Unable to malloc threads with size=%d\n",nthreads);
    exit(-1);
  }
  
  /* Create space for arguments for started threads*/
  if( (is=(int*) malloc(nthreads*sizeof(int))) == NULL) {
    printf("Error: Unable to malloc is with size=%d\n",nthreads);
    exit(-1);
  }
  
  /* start the threads*/
  for(i=0; i<nthreads; i++){
    is[i]=i;
    if( (threads[i]=(pthread_t*)malloc(sizeof(pthread_t))) == NULL) {
      printf("Error: Unable to malloc threads[%d]\n",i);
      exit(-1);
    }
    
    if( (istat=pthread_create(threads[i], NULL, *cb_process_thread, 
                             (void*) &is[i])) != 0) {
      printf("Error: Unable to create thread(%d), istat=%d\n",i,istat);
      exit(-1);
    }
    
    printf("pthread_created: i=%d, thread=%d\n",i,(int)*threads[i]);
    fflush(stdout);
  }

  /* form array "checks" to ensure correct data proceesed by process_thread*/
  if( (checks=(int*) malloc(npackets*sizeof(int))) == NULL) {
    printf("Error: Unable to malloc checks in cb_basictest\n");
    exit(-1);
  }
  for(i=0; i<npackets; i++) {checks[i]=-1;}  /*-1 means not processed yet*/
  
  /* Send data to cb to be processed by process_thread */
  for(i=0; i<npackets; i++) {
    if( (data=(char*) malloc(80)) == NULL) {
      printf("Error: Unable to malloc data in cb_basictest\n");
      exit(-1);
    }
    sprintf(data,"String %d",i);
    cb_put(cbp, data); /*Note data gets freed by process_thread */
  }

  /* Send NULLs to circular buffer telling process_thread to shut down */
  for(i=0; i<nthreads; i++) {
    data=NULL;
    cb_put(cbp, data);
  }

  /* Done sending data, wait for process_thread to process the data */
  for(i=0; i<nthreads; i++) {
    printf("pthread_join: i=%d, thread=%d\n",i,(int)*threads[i]);
    fflush(stdout);
    if( (istat=pthread_join(*threads[i], NULL)) == 0){
      printf("pthread(%d) joined\n", (int)(*threads[i]));
    } else {
      printf("pthread_join error: thread=%d, stat=%d\n",
             (int)(*threads[i]),istat);
    }
    fflush(stdout);
  }
  printf("\n");

  /* check checks to see all data processed*/
  n=0;
  for(i=0; i<npackets; i++) {
    if(checks[i] == -1) {
      printf("Error: data packet[%d] was not processed\n",i);
      n++;
    }
  }
  
  if(n == 0) {
    printf("All data packets processed\n");
  } else {
    printf("Not all packets processed, # not processed=%d\n",n);
  }
  
  if(packet_errors == 0) {
    printf("No packet data errors found\n");
  } else {
    printf("Number of packet data errors found=%d\n",packet_errors);
  }
  
  /* clean up and exit */
  cb_destroy(cbp);
  free(is);
  free(threads);
  free(checks);
  
  return 0;
}
/***************************** end unit test ******************************/
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

