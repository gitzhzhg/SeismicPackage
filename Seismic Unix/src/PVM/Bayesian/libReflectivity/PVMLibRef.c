/*
*	SeisLib.c
*
*	10 Oct 1994  Murillo: c
*	Modified: 23 Feb 1995. A. Murillo
*/
#include "refMaster.h"

int InitLog(SeisSlave *node, char * name)
{
	int	INFO;

	node -> fp_log   = 0;
	node -> DEBUG    = 0;
	node -> apl_name = name;
	node -> machine  = 0;
	INFO = StartLog(node);
	return(INFO);

} /* End of InitLog */

void CleanLog()
{
  char	command[512], *hd;
  
/* Clean PSU log directory */
  if ( !(hd = (char *)getenv("PVMLOG")) ) {
    hd = "/tmp"; /* Default value */
    fprintf(stderr, "Warning: Variable PVMLOG has not been  set\n");
    fprintf(stderr, "Warning: Using /tmp as log directory !! \n");
    fflush(stderr);
  }
  sprintf(command, "rm -rf %s/PSU*\0", hd);
  system(command);
  
} /* end of CleanLog() */

int StartLog(SeisSlave *node)
{
	char	log_file[256], myhost[256];
	char	*hd;
	FILE *fp;
	
	if ((node -> NodeTid = pvm_mytid()) < 0) {
		pvm_perror("Error enrolling");
		return(-1);
	} 

/* Get host info */
	(void) gethostname(myhost, sizeof(myhost));
	node -> machine = (char *)malloc(strlen(myhost));
	strcpy(node -> machine, myhost);

/* Open the log file */
	if ( !(hd = (char *)getenv("PVMLOG")) ) {
	    hd = "/tmp"; /* Default value */
            fprintf(stderr, "Warning: Variable PVMLOG has not been  set\n");
      	}

	/* creating log files */
	sprintf(log_file, "%s/PSU%s%d\0", hd, myhost, node -> NodeTid);
	/* if ( (node -> fd_log = open(log_file,O_CREAT)) == -1) { */
	if ( (node -> fp_log = freopen(log_file,"w", stderr)) == NULL) {
		fprintf(stderr, "Error opening the log file: %s\n", log_file);
		pvm_exit();
		return(-1);
	}
	
/* Write a header in the log file */
	fprintf(stderr, "%s\n", node -> apl_name);
	fprintf(stderr, 
		"\tFile\t--> %s\n\tapl-tid\t--> %d\n\tMachine\t--> %s \n",
		log_file, node -> NodeTid, node -> machine);
	fflush(stderr);

	return(0);
} /* end of StartLog() */

void	MsgLog(SeisSlave *node, char *msg)
{ 	
	fprintf(node -> fp_log, msg); 
	fflush(node -> fp_log); 
}

void 	PrintInfoLog(SeisSlave *zz) { 
	fprintf(zz -> fp_log, "\nProcess -->  %s\n", zz -> apl_name);
	fprintf(zz -> fp_log, "\tMachine    %s\n", zz -> machine);
	fprintf(zz -> fp_log, "\tapl_id        <%d>\n", zz -> NodeTid);
	fprintf(zz -> fp_log, "\n");
	fflush(zz -> fp_log);
} /* End of PrintInfoLog */

/*
	PVM functions (modified by Wences Gouveia)
*/

void BroadStr(char *str, int tids[], int ntids, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info) 
  {
     pvm_pkstr(str);
     pvm_mcast(tids,ntids, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with BroadStr : Message type %d\n", MsgType);
  }
}

void SendStr(char * str, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info) 
  {
     pvm_pkstr(str);
     pvm_send(tid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with SendStr : Message type %d\n", MsgType);
  }
}

char* RecvStr(int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, MasterTid;
  char *s1, tmp_char[200];

  BufId = pvm_recv(tid, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &MasterTid);
  pvm_upkstr(tmp_char);
  s1 = (char *)malloc(strlen(tmp_char)+1);
  strcpy(s1, tmp_char);
  return(s1);
}

void BroadInt(int IntBuf[], int n, int tid[], int ntid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info) 
  {
     pvm_pkint(IntBuf, n, 1);
     pvm_mcast(tid, ntid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with BroadInt: Message type %d\n", MsgType);
  }
}

void SendInt(int *IntBuf, int n, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info) 
  {
     pvm_pkint(IntBuf, n, 1);
     pvm_send(tid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with SendInt: Message type %d\n", MsgType);
  }
}

int RecvInt(int *IntBuf, int n, int tid, int MsgType)
{
  int info, BufId, NBytes, MsgTag, SenderTid;

  /*BufId = pvm_recv(-1, MsgType);*/
  BufId = pvm_recv(tid, MsgType);  /* wences */
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkint(IntBuf, n, 1);
  return(SenderTid);
}

void BroadFloat(float *FloatBuf, int n, int tid[], int ntid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  /*pvm_pkbyte((char *)FloatBuf, n*sizeof(float), 1);*/
  if (info)
  {
     pvm_pkfloat(FloatBuf, n, 1); /* wences */
     pvm_mcast(tid, ntid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with BroadFloat : Message type %d\n", MsgType);
  }
}

void SendFloat(float *FloatBuf, int n, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  /* pvm_pkbyte((char *)FloatBuf, n*sizeof(float), 1); */
  if (info)
  {
     pvm_pkfloat(FloatBuf, n, 1);
     pvm_send(tid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with SendFloat : Message type %d\n", MsgType);
  }
}

int RecvFloat(float *FloatBuf, int n, int tid, int MsgType)
{
  int info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkfloat(FloatBuf, n, 1);
  /* pvm_upkbyte((char *)FloatBuf, n*sizeof(float), 1); */
  return(SenderTid);
}

void SendCplx(complex *CplxBuf, int n, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info)
  {
     pvm_pkbyte((char *)CplxBuf, n*sizeof(complex), 1);
     pvm_send(tid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with SendCplx : Message type %d\n", MsgType);
  }
}

int RecvCplx(complex *CplxBuf, int n, int tid, int MsgType)
{
  int info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo(BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkbyte((char *) CplxBuf, n*sizeof(complex), 1);
  return(SenderTid);
}

void SendFI(float *FloatBuf, int nf, int *IntBuf, int ni, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info)
  {
     pvm_pkint(IntBuf, ni, 1);
     pvm_pkbyte((char *)FloatBuf, nf*sizeof(float), 1);
     pvm_send(tid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with SendFI : Message type %d\n", MsgType);
  }
}

int RecvFI(float *FloatBuf, int nf, int *IntBuf, int ni, int tid, int MsgType)
{
  int info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkint(IntBuf, ni, 1);
  pvm_upkbyte((char *)FloatBuf, nf*sizeof(float), 1);
  return(SenderTid);
}

int CreateSlaves(int *SeisSlaves, char *SlaveName, int pn)
{
  int info;

  info = pvm_spawn(SlaveName, (char **)0, 0,
                   (char *)0, pn, SeisSlaves);
  if (info != pn) {
        fprintf(stderr, "Error starting pvm applications %s \n", SlaveName);
        return(-1);
  }
  return(pn);
} /* End of CreateSlaves */

void EndOfSlave()
{
   pvm_exit();
}

void SendINFO(INFO *data, int n, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if (info)
  {
     pvm_pkbyte((char *)data, n*sizeof(INFO), 1);
     pvm_send(tid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with SendINFO : Message type %d\n", MsgType);
  }
}

int RecvINFO(INFO *data, int n, int tid, int MsgType)
{
  int info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo(BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkbyte((char *) data, n*sizeof(INFO), 1);
  return(SenderTid);
}

void BroadINFO(INFO *data, int n, int tid[], int ntid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);

  if (info)
  {
     pvm_pkbyte((char *)data, n*sizeof(INFO), 1);
     pvm_mcast(tid, ntid, MsgType);
  }
  else
  {
     fprintf(stderr, "Problems with BroadINFO : Message type %d\n", MsgType);
  }
}
