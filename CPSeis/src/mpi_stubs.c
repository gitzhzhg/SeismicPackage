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
/*!<CPS_v1 type="AUXILIARY_FILE",pretag="!"/>


!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : mpi_stubs 
! Category   : stand-alone
! Written    : 2001-03-21  by Charles C. Burch
! Revised    : 2006-07-18  by Bill Menger
! Maturity   : beta
! Purpose    : Dummy mpi routines for linking purposes on machines without mpi
! Portability: No known limitations.
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!  This primitive contains a collection of dummy mpi routines which are used 
!  in cps in cases where mpi is not actually used but are needed for linking.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
! These programs follow the mpi calling standards.  
! They are meant to satisfy linking requirements and not to be executed
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!  6. 2006-07-18  Menger         Added mpi_initialized
!  5. 2004-12-09  Stoeckley      Added mpi_comm_group, mpi_group_excl, and
!                                 mpi_comm_create.
!  4. 2002-04-22  Vunderink      Added mpi_abort
!  3. 2001-04-03  CC Burch       Added mpi_gather
!  2. 2001-03-22  CC Burch       Added mpi_reduce
!  1. 2001-03-21  CC Burch       Initial Version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
! No known problems.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! Requires -DNEED_UNDERSCORE on all platforms.
!
!-------------------------------------------------------------------------------
!</compile_doc>
*/


char *mpi_stubs_ident =
"$Id: mpi_stubs.c,v 1.5 2006/07/17 21:14:42 mengewm Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define INTEGER         long

#ifdef NEED_UNDERSCORE
#define mpi_comm_group   mpi_comm_group_
#define mpi_comm_null_copy_fn mpi_comm_null_copy_fn_
#define mpi_group_excl   mpi_group_excl_
#define mpi_comm_create  mpi_comm_create_
#define mpi_abort        mpi_abort_
#define mpi_allreduce    mpi_allreduce_
#define mpi_barrier      mpi_barrier_
#define mpi_bcast        mpi_bcast_
#define mpi_comm_rank    mpi_comm_rank_
#define mpi_comm_size    mpi_comm_size_
#define mpi_finalize     mpi_finalize_
#define mpi_gather       mpi_gather_
#define mpi_get_count    mpi_get_count_
#define mpi_init         mpi_init_
#define mpi_initialized  mpi_initialized_
#define mpi_iprobe       mpi_iprobe_
#define mpi_pack         mpi_pack_
#define mpi_pack_size    mpi_pack_size_
#define mpi_probe        mpi_probe_
#define mpi_recv         mpi_recv_
#define mpi_reduce       mpi_reduce_
#define mpi_send         mpi_send_
#define mpi_unpack       mpi_unpack_
#define mpi_comm_dup_fn  mpi_comm_dup_fn_
#define mpi_comm_null_delete_fn  mpi_comm_null_delete_fn_
#define mpi_dup_fn  mpi_dup_fn_
#define mpi_null_copy_fn  mpi_null_copy_fn_
#define mpi_null_delete_fn  mpi_null_delete_fn_
#define mpi_type_dup_fn  mpi_type_dup_fn_
#define mpi_type_null_copy_fn  mpi_type_null_copy_fn_
#define mpi_type_null_delete_fn  mpi_type_null_delete_fn_
#define mpi_win_dup_fn  mpi_win_dup_fn_
#define mpi_win_null_copy_fn  mpi_win_null_copy_fn_
#define mpi_win_null_delete_fn  mpi_win_null_delete_fn_
#define mpi_wtick  mpi_wtick_
#define mpi_wtime  mpi_wtime_
#define pmpi_wtick  pmpi_wtick_
#define pmpi_wtime  pmpi_wtime_

#endif

#ifdef NEED_CAPITALS
#define mpi_comm_group   MPI_COMM_GROUP
#define mpi_comm_null_copy_fn MPI_COMM_NULL_COPY_FN
#define mpi_group_excl   MPI_GROUP_EXCL
#define mpi_comm_create  MPI_COMM_CREATE
#define mpi_abort        MPI_ABORT
#define mpi_allreduce    MPI_ALLREDUCE 
#define mpi_barrier      MPI_BARRIER 
#define mpi_bcast        MPI_BCAST 
#define mpi_comm_rank    MPI_COMM_RANK 
#define mpi_comm_size    MPI_COMM_SIZE 
#define mpi_finalize     MPI_FINALIZE
#define mpi_gather       MPI_GATHER 
#define mpi_get_count    MPI_GET_COUNT 
#define mpi_init         MPI_INIT 
#define mpi_initialized  MPI_INITIALIZED
#define mpi_iprobe       MPI_IPROBE 
#define mpi_pack         MPI_PACK 
#define mpi_pack_size    MPI_PACK_SIZE 
#define mpi_probe        MPI_PROBE 
#define mpi_recv         MPI_RECV 
#define mpi_reduce       MPI_REDUCE
#define mpi_send         MPI_SEND 
#define mpi_unpack       MPI_UNPACK 
#define mpi_comm_null_delete_fn  MPI_COMM_NULL_DELETE_FN
#define mpi_dup_fn  MPI_DUP_FN
#define mpi_null_copy_fn  MPI_NULL_COPY_FN
#define mpi_null_delete_fn  MPI_NULL_DELETE_FN
#define mpi_type_dup_fn  MPI_TYPE_DUP_FN
#define mpi_type_null_copy_fn  MPI_TYPE_NULL_COPY_FN
#define mpi_type_null_delete_fn  MPI_TYPE_NULL_DELETE_FN
#define mpi_win_dup_fn  MPI_WIN_DUP_FN
#define mpi_win_null_copy_fn  MPI_WIN_NULL_COPY_FN
#define mpi_win_null_delete_fn  MPI_WIN_NULL_DELETE_FN
#define mpi_wtick  MPI_WTICK
#define mpi_wtime  MPI_WTIME
#define pmpi_wtick  PMPI_WTICK
#define pmpi_wtime  PMPI_WTIME
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

  void mpi_init(INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_init called\n");
    exit(1);
  }

  void mpi_initialized(INTEGER *flag, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_initialized called\n");
    exit(1);
  }

  void mpi_comm_size(INTEGER *comm, INTEGER *num_procs, INTEGER *istat){
    (*num_procs)=1;
    printf("Warning mpi_stub version mpi_comm_size called\n");
    exit(1);
  } 

  void mpi_comm_null_copy_fn(){
    printf("Warning mpi_stub version mpi_comm_null_copy_fn called\n");
    exit(1);
  }
  void mpi_comm_dup_fn(){
    printf("Warning mpi_stub version mpi_com_dup_fn called\n");
    exit(1);
  }

  void mpi_comm_null_delete_fn(){
    printf("Warning mpi_stub version mpi_comm_null_delete_fn called\n");
    exit(1);
  }

  void mpi_dup_fn(){
    printf("Warning mpi_stub version mpi_dup_fn called\n");
    exit(1);
  }

  void mpi_null_copy_fn(){
    printf("Warning mpi_stub version mpi_null_copy_fn called\n");
    exit(1);
  }

  void mpi_null_delete_fn(){
    printf("Warning mpi_stub version mpi_null_delete_fn called\n");
    exit(1);
  }

  void mpi_type_dup_fn(){
    printf("Warning mpi_stub version mpi_type_dup_fn called\n");
    exit(1);
  }

  void mpi_type_null_copy_fn(){
    printf("Warning mpi_stub version mpi_type_null_copy_fn called\n");
    exit(1);
  }

  void mpi_type_null_delete_fn(){
    printf("Warning mpi_stub version mpi_type_null_delete_fn called\n");
    exit(1);
  }

  void mpi_win_dup_fn(){
    printf("Warning mpi_stub version mpi_win_dup_fn called\n");
    exit(1);
  }

  void mpi_win_null_copy_fn(){
    printf("Warning mpi_stub version mpi_win_null_copy_fn called\n");
    exit(1);
  }

  void mpi_win_null_delete_fn(){
    printf("Warning mpi_stub version mpi_win_null_delete_fn called\n");
    exit(1);
  }

  void mpi_wtick(){
    printf("Warning mpi_stub version mpi_wtick called\n");
    exit(1);
  }

  void mpi_wtime(){
    printf("Warning mpi_stub version mpi_wtime called\n");
    exit(1);
  }

  void pmpi_wtick(){
    printf("Warning mpi_stub version pmpi_wtick called\n");
    exit(1);
  }

  void pmpi_wtime(){
    printf("Warning mpi_stub version pmpi_wtime called\n");
    exit(1);
  }

  void mpi_pack_size(INTEGER *n, INTEGER *type, INTEGER* comm, 
   INTEGER *size, INTEGER *istat) {
    printf("Warning mpi_stub version mpi_pack_size called\n");
    exit(1);
  }

  void  mpi_finalize(INTEGER *ierr){
    printf("Warning mpi_stub version mpi_finalize called\n");
    exit(1);
  }

  void mpi_comm_rank(INTEGER *comm, INTEGER *worker_num, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_comm_rank called\n");
    exit(1);
  }

  void mpi_barrier(INTEGER *comm, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_barrier called\n");
    exit(1);
  }

  void mpi_send(void *buffer, INTEGER *n, INTEGER *type, INTEGER *proc, 
   INTEGER *tag, INTEGER *comm, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_send called\n");
    exit(1);
  }

  void mpi_recv(void *buffer, INTEGER *n, INTEGER *type, INTEGER *proc, 
   INTEGER *tag, INTEGER *comm, INTEGER *status, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_recv called\n");
    exit(1);
  }

  void mpi_probe(INTEGER *source, INTEGER *tag, INTEGER *comm,  
   INTEGER *status, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_probe called\n");
    exit(1);
  }

  void mpi_iprobe(INTEGER *worker_no, INTEGER *tag, INTEGER *comm,  
   INTEGER *flag, INTEGER *status, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_iprobe called\n");
    exit(1);
  }

  void mpi_get_count(INTEGER *status, INTEGER *type, INTEGER *size, 
   INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_get_count called\n");
    exit(1);
  }

  void mpi_bcast(void *buff, INTEGER *n,INTEGER *type, INTEGER *proc, 
   INTEGER *comm, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_bcast called\n");
    exit(1);
  }

  void mpi_gather(void *send_buff, INTEGER *n_send, INTEGER *send_type, 
                  void *recv_buff, INTEGER *n_recv, INTEGER *recv_type, 
                  INTEGER *proc, INTEGER *comm, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_gather called\n");
    exit(1);
  }

  void mpi_pack(void *buff_in, INTEGER *n,INTEGER *type, void *buff_out,
   INTEGER *buff_size, INTEGER *n_packed, INTEGER *comm, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_pack called\n");
    exit(1);
  }

  void mpi_unpack(void *buff1, INTEGER *buff_size, INTEGER n_packed,
    void *buff2, INTEGER *n, INTEGER *type, INTEGER *comm, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_unpack called\n");
    exit(1);
  } 

  void  mpi_allreduce (void *x_inp, void *x_out, INTEGER *n_inp, INTEGER *type,
   INTEGER *op, INTEGER *comm, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_allreduce called\n");
    exit(1);
  }

  void  mpi_reduce (void *x_inp, void *x_out, INTEGER *n_inp, INTEGER *type,
   INTEGER *op, INTEGER root, INTEGER *comm, INTEGER *ierr) {
    printf("Warning mpi_stub version mpi_reduce called\n");
    exit(1);
  }

  void mpi_abort(INTEGER *comm, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_abort called\n");
    exit(1);
  }

  void mpi_comm_group(INTEGER *comm, INTEGER *group, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_comm_group called\n");
    exit(1);
  }

  void mpi_group_excl(INTEGER *comm, INTEGER *n, INTEGER *ranks,
                      INTEGER *newgroup, INTEGER *ierr){
    printf("Warning mpi_stub version mpi_group_excl called\n");
    exit(1);
  }

  void mpi_comm_create(INTEGER *comm, INTEGER *group, INTEGER *newcomm,
                       INTEGER *ierr){
    printf("Warning mpi_stub version mpi_comm_create called\n");
    exit(1);
  }

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
