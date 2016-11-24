/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SeisLib.h */
#include "pvm3.h" 
  
/* INFO structure */
typedef struct _generalInfo {
   char recFile[100];
   int nR;
   int directWave;
   int nL;
   int nF;
   int nU;
   int hanningFlag;
   int nSamples;
   int nFreqProc;
   int verbose;
   float r1;
   float dR;
   float zs;
   float u1;
   float u2;
   float dU;
   float f1;
   float f2;
   float dF;
   float F1, F2, F3;
   float percU, percW;
   float wR;
   float tau;
} INFO;
   
/* Types definitions */
typedef struct	_seis_apl {
  char		*apl_name;
  char		*machine;
  int		apl_id;
  FILE		*fp_log;
  int		fd_log;

  /* Pvm definitions */
  int		info, bufid, n_bytes, msgtag, sender_tid;
  int		MasterTid;
  int		NodeTid;
  int		FatherTid;
  int		NextTid;

  /* Miscellaneous */
  int	DEBUG;
} SeisSlave;

int   InitLog(SeisSlave *zz, char * apl_name);
int   StartLog(SeisSlave *zz);	
void  PrintInfoLog(SeisSlave *zz);
void CleanLog();

/* Some forward declarations */
void	clips(char *);
/*void	gethostname(char *, long n);*/

/* PVM functions */

void BroadStr(char * str, int tids[], int ntids,  int MsgType);
void SendStr  (char *str, int tid, int MsgType);
char* RecvStr  (int tid, int MsgType);

int SendBytes(void * buff, int n, int tid, int MsgType);
void* RecvBytes(int tid, int MsgType);
int BroadBytes(void * str, int size, int tid[], int ntid, int MsgType);

void BroadInt(int IntBuf[], int n, int tid[], int ntid, int MsgType);
void SendInt  (int *IntBuf, int n, int tid, int MsgType);
int RecvInt  (int *IntBuf, int n, int tid, int MsgType);
int RecvOneInt  (int tid, int MsgType);
int RecvOneIntNB  (int tid, int MsgType);

void BroadFI(float *FloatBuf, int nf, int *IntBuf, int ni,
	     int tid[], int ntid, int MsgType);
void SendFI(float *FloatBuf, int nf, int *IntBuf, int ni, 
	    int tid, int MsgType);
int RecvFI(float *FloatBuf, int nf, int *IntBuf, int ni, 
	    int tid, int MsgType);

void BroadFloat(float *FloatBuf, int n, int tid[], int ntid, int MsgType);
void SendFloat(float *FloatBuf, int n, int tid, int MsgType);
int RecvFloat(float *FloatBuf, int n, int tid, int MsgType);

void SendCplx (complex *CplxBuf, int n, int tid, int MsgType);
int RecvCplx (complex *CplxBuf, int n, int tid, int MsgType);

void SendINFO (INFO *data, int n, int tid, int MsgType);
int RecvINFO (INFO *data, int n, int tid, int MsgType);
void BroadINFO(INFO *data, int n, int tid[], int ntid, int MsgType);

int CreateSlaves(int *SeisSlaves, char *SlaveName, int pn);
void EndOfSlave();
int CreatePipe(int *SeisSlaves, char *SlaveName, int n);
