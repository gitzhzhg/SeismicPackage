/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* include file for PVM - Parallel Virtual Machine */

#ifndef PVM_H
#define PVM_H

#include "cwp.h"

/* function prototypes for CWP interface to PVM */
int pvmEnroll (char *component);
void pvmLeave (void);
int pvmInitiate (char *exec, char *arch);
int pvmInitiateMachine (char *exec, char *machine);
int pvmTerminate (char *component, int instance);
int pvmCountMachines (void);
int pvmCountArchs (void);
int pvmActive (char *component, int instance);
int pvmIdentify (char *component, int *instance);
void pvmBeginMessage (void);
int pvmSend (int msgtype, char *component, int instance);
int pvmReceive (int msgtype);
int pvmNReceive (int n, int *msgtypes);
int pvmProbe (int msgtype);
int pvmMessageInfo (int *length, int *msgtype, char *component, int *instance);
int pvmPutNInt (int n, int *x);
int pvmPutNShort (int n, short *x);
int pvmPutNLong (int n, long *x);
int pvmPutNFloat (int n, float *x);
int pvmPutNDouble (int n, double *x);
int pvmPutNComplex (int n, complex *x);
int pvmPutNByte (int n, void *x);
int pvmPutString (char *x);
int pvmGetNInt (int n, int *x);
int pvmGetNShort (int n, short *x);
int pvmGetNLong (int n, long *x);
int pvmGetNFloat (int n, float *x);
int pvmGetNDouble (int n, double *x);
int pvmGetNComplex (int n, complex *x);
int pvmGetNByte (int n, void *x);
int pvmGetString (char *x);
int pvmBarrier (int n, char *name);
void pvmSignalEvent (char *event);
void pvmWaitUntil (char *event);

/* function prototypes for distribution PVM interface */
int barrier (char *name, int num);
int enroll (char *proc);
int getnint (int *np, int cnt);
int getnshort (short *np, int cnt);
int getnlong (long *np, int cnt);
int getnfloat (float *fp, int cnt);
int getndfloat (double *dp, int cnt);
int getncplx (float *xp, int cnt);
int getndcplx (double *zp, int cnt);
int getbytes (char *cp, int len);
int getstring (char *cp);
int initiate (char *aout, char *arch);
int initiateM (char *aout, char *host);
int initsend ();
int leave ();
int probe (int type);
int probemulti (int ntypes, int *types);
int pstatus (int *nproc, int *mixed);
int putnint (int *np, int cnt);
int putnshort (short *np, int cnt);
int putnlong (long *np, int cnt);
int putnfloat (float *fp, int cnt);
int putndfloat (double *dp, int cnt);
int putncplx (float *xp, int cnt);
int putndcplx (double *zp, int cnt);
int putbytes (char *cp, int len);
int putstring (char *cp);
int rcv (int type);
int rcvinfo (int *len, int *type, char *proc, int *inum);
int rcvmulti (int ntypes, int *types);
int ready (char *event);
int snd (char *proc, int inum, int type);
int status (char *proc, int inum);
int terminate (char *proc, int inum);
int waituntil (char *event);
int whoami (char *proc, int *inum);

#endif /* PVM_H */
