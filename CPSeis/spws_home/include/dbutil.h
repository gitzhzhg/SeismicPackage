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
#ifndef  _dbutil
#define  _dbutil

#include "c2f_interface.h"

#include <stdio.h>
/***********************************************************************

 	FILE: 		dbutil.h

	DESCRIPTION: 	Definitions and prototypes for dbutil.c

	AUTHOR:		Craig R. Dixon
	
***********************************************************************/


#ifdef NEED_CAPITALS
#define dbutil_del_             DBUTIL_DEL
#define dbutil_getnewindex_     DBUTIL_GETNEWINDEX
#define dbutil_getlun_          DBUTIL_GETLUN
#define dbutil_getreclength_    DBUTIL_GETRECLENGTH
#define dbutil_putlun_          DBUTIL_PUTLUN
#define dbutil_putreclength_    DBUTIL_PUTRECLENGTH
#define dbutil_putwordtype_     DBUTIL_PUTWORDTYPE
#define dbutil_putremotefile_   DBUTIL_PUTREMOTEFILE
#define dbutil_putlocalfile_    DBUTIL_PUTLOCALFILE
#define dbutil_putformattype_   DBUTIL_PUTFORMATTYPE
#define dbutil_putaccesstype_   DBUTIL_PUTACCESSTYPE
#define dbutil_putstatus_       DBUTIL_PUTSTATUS
#define dbutil_getrfile_        DBUTIL_GETRFILE
#define dbutil_getfast_         DBUTIL_GETFAST
#define dbutil_getlocalfile_    DBUTIL_GETLOCALFILE
#define dbutil_lun2index_       DBUTIL_LUN2INDEX
#define dbutil_rfile2key_       DBUTIL_RFILE2KEY
#define dbutil_getformattype_   DBUTIL_GETFORMATTYPE
#define dbutil_getstatus_       DBUTIL_GETSTATUS
#define dbutil_getwordtype_     DBUTIL_GETWORDTYPE
#define dbutil_key_available_   DBUTIL_KEY_AVAILABLE
#define dbutil_getnew_ckey_     DBUTIL_GETNEW_CKEY
#endif

#if (VMS || _AIX || __hpux)
#define dbutil_del_             dbutil_del
#define dbutil_getnewindex_     dbutil_getnewindex
#define dbutil_getlun_          dbutil_getlun
#define dbutil_getreclength_    dbutil_getreclength
#define dbutil_putlun_          dbutil_putlun
#define dbutil_putwordtype_     dbutil_putwordtype
#define dbutil_putreclength_    dbutil_putreclength
#define dbutil_putremotefile_   dbutil_putremotefile
#define dbutil_putlocalfile_    dbutil_putlocalfile
#define dbutil_putformattype_   dbutil_putformattype
#define dbutil_putaccesstype_   dbutil_putaccesstype
#define dbutil_putstatus_       dbutil_putstatus
#define dbutil_getrfile_        dbutil_getrfile
#define dbutil_getfast_         dbutil_getfast
#define dbutil_getlocalfile_    dbutil_getlocalfile
#define dbutil_lun2index_       dbutil_lun2index
#define dbutil_rfile2key_       dbutil_rfile2key
#define dbutil_getformattype_   dbutil_getformattype
#define dbutil_getstatus_       dbutil_getstatus
#define dbutil_getwordtype_     dbutil_getwordtype
#define dbutil_key_available_   dbutil_key_available
#define dbutil_getnew_ckey_     dbutil_getnew_ckey
#endif

typedef struct FileData
{
	FILE		*filePtr;
	int 		fortranUnit;
	
	char		localFile[80];
	char		remoteFile[132];
		
	char		accessType[16];
	char		status[16];
	char		formatType[16];
	
	int 		recordLength;
        int             word_type;
	
} FileData, *FileDataPtr;


/*	Prototypes
	----------*/

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void dbutil_add_(int  *indexNumber, FILE *filePtr, int  *fortranUnit, 
 	char *localFile, char *remoteFile, char *accessType, char *status, 
 	char *formatType, int  *recordLength);
 	
void dbutil_del_(int  *indexNumber);

void dbutil_allcaps (char *theString);

int  dbutil_getnewindex_(void);
int  dbutil_isopen (char *localFile);
	
void dbutil_swap_(int  *oldIndex, int  *newIndex);
int  dbutil_lun2index_(int  fortranUnit);

FILE *dbutil_getfileptr(int  *indexNumber);
int  dbutil_getlun_(int  *indexNumber);
int  dbutil_getreclength_(int  *indexNumber);
int  dbutil_getlocalfile_(int  *indexNumber,char *localFile);
char *dbutil_getremotefile (int  *indexNumber);
int  dbutil_getrfile_(int  *indexNumber, char *rfile);
int  dbutil_getfast_(int  *indexNumber,char *form,char *access, char *status);
char *dbutil_getaccesstype (int  *indexNumber);
int  dbutil_getstatus_(int  *indexNumber,char *status);
int  dbutil_getformattype_(int  *indexNumber,char *format);
int  dbutil_getwordtype_(int  *indexNumber);
int  dbutil_rfile2key_(char *rfile);

int  dbutil_putfileptr (int  *indexNumber, FILE *filePtr);
int  dbutil_putlun_(int  *indexNumber, int  *fortranUnit);
int  dbutil_putreclength_(int  *indexNumber, int  *recordLength);
int  dbutil_putlocalfile_(int  *indexNumber, char *localFile);
int  dbutil_putremotefile_(int  *indexNumber, char *remoteFile);
int  dbutil_putaccesstype_(int  *indexNumber, char *accessType);
int  dbutil_putstatus_(int  *indexNumber, char *status);
int  dbutil_putformattype_(int  *indexNumber, char *formatType);
int  dbutil_putwordtype_(int  *indexNumber, int  *wdtyp);
int  dbutil_key_available_(int  *indexNumber);
int  dbutil_getnew_ckey_();


#ifdef __cplusplus
}                   // for C++
#endif

#endif
