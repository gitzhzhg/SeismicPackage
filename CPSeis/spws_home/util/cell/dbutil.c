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
/***********************************************************************

 	FILE: 		dbutil.c

	DESCRIPTION: 	Shell for maintaining file data base for RMOD

	AUTHOR:		Craig R. Dixon
	
***********************************************************************/
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "dbutil.h"

static FileData		fileDataBase[200];

 	
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

/**********************************************************************/
 void dbutil_add_(int  *indexNumber, FILE *filePtr, int  *fortranUnit, 
 	char *localFile, char *remoteFile, char *accessType, char *status, 
 	char *formatType, int  *recordLength)
{
/*	Adds the information for a file to the database
	-----------------------------------------------  */
	
	dbutil_putfileptr (indexNumber, filePtr);
	dbutil_putlun_(indexNumber, fortranUnit);
	dbutil_putlocalfile_(indexNumber, localFile);
	dbutil_putremotefile_(indexNumber, remoteFile);
	dbutil_putaccesstype_(indexNumber, accessType);
	dbutil_putstatus_(indexNumber, status);
	dbutil_putformattype_(indexNumber, formatType);
	dbutil_putreclength_(indexNumber, recordLength);
}
	
/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 void dbutil_del_(int  *indexNumber)
/**********************************************************************/

{
/*	Deletes the information for the file associated with the 
	input index from the database.
	------------------------------  */
	FileData *chosen;
        chosen = &fileDataBase[*indexNumber];
	chosen->filePtr = 0L;
	chosen->fortranUnit = 0L;
	strcpy (chosen->localFile, "");
	strcpy (chosen->remoteFile, "");
	strcpy (chosen->accessType, "");
	strcpy (chosen->status, "");
	strcpy (chosen->formatType, "");
	chosen->recordLength = 0L;
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 void dbutil_allcaps (char *theString)
/**********************************************************************/

{
/*	Converts the input string to capital letters
	--------------------------------------------  */
	while (*theString)
	{ *theString = toupper (*theString);
          theString++;
	}
}	

/**********************************************************************/
int  dbutil_rfile2key_(char *rfile)
/**********************************************************************/
{int  count;
 if(rfile== NULL) return -1;
 if(strlen(rfile)==0) return -1;
 if(strcmp(rfile,"NONE")==0) return-1;
 for (count = 0; count <= 199; count++)
  {if (strcmp (fileDataBase[count].remoteFile, rfile) == 0)
    { return count; }
  }
 return -1;
}

/**********************************************************************/
int  dbutil_key_available_(int  *key)
/**********************************************************************/
{/*     Checks whether the requested Key is available for useage.
        if not available it returns a 0.
        ----------------  */
        int             isOpen = 0;
        if(*key< 0 || *key > 199) return isOpen;
        if (strcmp (fileDataBase[*key].localFile, "") == 0) isOpen = 1;
        return (isOpen);
}

/**********************************************************************/
 int  dbutil_getnewindex_()
/**********************************************************************/

{
/*	Gets the first free index which is available in the database;
	if none are available (there are already 199 files open,
	it returns a -1.
	----------------  */
	int			count;
	int 		firstOpenIndex = -1L;
	
	for (count = 0; count <= 199; count++)
	{
		if (strcmp (fileDataBase[count].localFile, "") == 0)
		{
			firstOpenIndex = count;
			return (firstOpenIndex);		
		}
	}
	
	return (firstOpenIndex);
}
/**********************************************************************/
 int  dbutil_getnew_ckey_()
/**********************************************************************/

{
/*      Gets the first free index which is available in the database;
        if none are available (there are already 199 files open,
        it returns a -1.
        ----------------  */
        int                     count;
        int             firstOpenIndex = -1L;
        for (count = 100; count <= 199; count++)
        {
         if (strcmp (fileDataBase[count].localFile, "") == 0)
          {firstOpenIndex = count;
           return (firstOpenIndex);
          }
        }
        return (firstOpenIndex);
}


/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_isopen (char *localFile)
/**********************************************************************/

{
/*	Tests whether the input file is already open; if it is, the database 
	index is returned.  Otherwise, -1 is returned.
	----------------------------------------------  */
	
	int 			indexNumber;
	
	
	for (indexNumber = 0; indexNumber <= 199; indexNumber ++)
	{
		if (strcmp (localFile, fileDataBase[indexNumber].localFile) == 0)
		{
			return (indexNumber);
		}
	}
		
	return (-1L);
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 void dbutil_swap_(int  *oldIndex, int  *newIndex)
/**********************************************************************/

{
/*	This routine allows you to move all the information listed in one
	database entry to another.  Then the old entries at the old index 
	location are deleted.
	---------------------  */
	
	fileDataBase[*newIndex].filePtr = fileDataBase[*oldIndex].filePtr;
	fileDataBase[*newIndex].fortranUnit = 
		fileDataBase[*oldIndex].fortranUnit;
	strcpy (fileDataBase[*newIndex].localFile, 
		fileDataBase[*oldIndex].localFile);
	strcpy (fileDataBase[*newIndex].remoteFile, 
		fileDataBase[*oldIndex].remoteFile);
	strcpy (fileDataBase[*newIndex].accessType, 
		fileDataBase[*oldIndex].accessType);
	strcpy (fileDataBase[*newIndex].status, 
		fileDataBase[*oldIndex].status);
	strcpy (fileDataBase[*newIndex].formatType, 
		fileDataBase[*oldIndex].formatType);
	fileDataBase[*newIndex].recordLength = 
		fileDataBase[*oldIndex].recordLength;

	dbutil_del_(oldIndex);

	return;
}

/**********************************************************************/
/**********************************************************************/

/**********************************************************************/
 int  dbutil_lun2index_(int  fortranUnit)
/**********************************************************************/

{
/*	Finds the database index which is associated with the input 
	fortran logical unit; returns a -1 if none is found.
	----------------------------------------------------  */
	
	int 		indexNumber = -1L;
	int 		count;
	
	
	for (count = 0; count <= 199; count++)
	{
		if (fortranUnit == fileDataBase[count].fortranUnit)
		{
			indexNumber = count;
			return (indexNumber);
		}
	}

	return (indexNumber);
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_getallfinfo (int  *indexNumber, FILE *filePtr, 
 	int  *fortranUnit, char*localFile, char *remoteFile, 
 	char *accessType, char *status, char *formatType, 
 	int  *recordLength)
/**********************************************************************/

{int  statl,statf,stats;
/*	Returns all the database information for the input index number.
	----------------------------------------------------------------  */
  statl = dbutil_getlocalfile_(indexNumber,localFile);	
  statf = dbutil_getformattype_(indexNumber,formatType);	
  stats = dbutil_getstatus_(indexNumber,status);	
	if (((filePtr = dbutil_getfileptr (indexNumber)) == 0) ||
		((*fortranUnit = dbutil_getlun_(indexNumber)) <= 0) ||
		(statl != 0) || (stats != 0) || (statf!=0) ||
		(strcmp (remoteFile = 
			dbutil_getremotefile (indexNumber), "") == 0) ||
		(strcmp (accessType = 
			dbutil_getaccesstype (indexNumber), "") == 0) ||
		((*recordLength = dbutil_getreclength_(indexNumber)) <= 0))
	{
		return (-1L);
	}

	return (0L);
}	

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 FILE *dbutil_getfileptr (int  *indexNumber)
/**********************************************************************/

{
/*	Returns a file pointer for the file associated with the index number
	in the database  
	---------------  */
	
	FILE		*filePtr;
	
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		return (fileDataBase[*indexNumber].filePtr);
	}
	else
	{
		return (0L);
	}
}

/**********************************************************************/
/**********************************************************************/

/**********************************************************************/
 int  dbutil_getlun_(int  *indexNumber)
/**********************************************************************/

{
/*	Returns fortran logical unit for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		return (fileDataBase[*indexNumber].fortranUnit);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
 int  dbutil_getwordtype_(int  *indexNumber)
/**********************************************************************/
{
/*      Returns word type for the file associated with the input
        index number in the database
        ----------------------------  */
        if (*indexNumber >= 0 && *indexNumber <= 199)
        { return (fileDataBase[*indexNumber].word_type); }
        else
        { return (-1L); }
}
/**********************************************************************/
 int  dbutil_putwordtype_(int  *indexNumber, int *wdtype)
/**********************************************************************/
{
/*      Sets word type for the file associated with the input
        index number in the database
        ----------------------------  */
       if (*indexNumber >= 0 && *indexNumber <= 199)
        { fileDataBase[*indexNumber].word_type = *wdtype;
          return (0L);
        }
       else
        { return (-1L); }
}
 

/**********************************************************************/
/**********************************************************************/
int  dbutil_getfast_(int  *indexNumber,char *form,char *access, char *status)
{ 
/*      Returns form,access, and status given the key
         index to the database 
        ----------------------------  */
       if (*indexNumber >= 0 && *indexNumber <= 199)
        {
           strcpy(form,fileDataBase[*indexNumber].formatType);
           strcpy(access,fileDataBase[*indexNumber].accessType);
           strcpy(status,fileDataBase[*indexNumber].status);
           return 0;
        }
        else
        {
           return (-1L);
        }
}


/**********************************************************************/
 int  dbutil_getlocalfile_(int  *indexNumber, char *localFile)
/**********************************************************************/

{
/*	Returns the local file name for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{strcpy(localFile,fileDataBase[*indexNumber].localFile);
         return 0;
	}
	else
	{ return 1;
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 char *dbutil_getremotefile (int  *indexNumber)
/**********************************************************************/

{
/*	Returns the remote file name for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		return (fileDataBase[*indexNumber].remoteFile);
	}
	else
	{
		return (0L);
	}
}
/**********************************************************************/
 int  dbutil_getrfile_(int  *indexNumber,char *rfile)
/**********************************************************************/

{
/*      Returns the remote file name for the file associated with the input
        index number in the database
        ----------------------------  */

        if (*indexNumber >= 0 && *indexNumber <= 199)
        {
         strcpy(rfile,fileDataBase[*indexNumber].remoteFile);
         return (0L);
        }
        else
        {
         return (1L);
        }
}
 

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 char *dbutil_getaccesstype (int  *indexNumber)
/**********************************************************************/

{
/*	Returns the access type for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		return (fileDataBase[*indexNumber].accessType);
	}
	else
	{
		return (0L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_getstatus_(int  *indexNumber,char *status)
/**********************************************************************/

{
/*	Returns the file status for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{strcpy(status,fileDataBase[*indexNumber].status);
         return 0;
	}
	else
	{return (1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_getformattype_(int  *indexNumber, char *format)
/**********************************************************************/

{
/*	Returns the format type for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{strcpy(format,fileDataBase[*indexNumber].formatType);
         return 0;
	}
	else
	{return (1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_getreclength_(int  *indexNumber)
/**********************************************************************/

{
/*	Returns the record length for the file associated with the input
	index number in the database  
	----------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		return (fileDataBase[*indexNumber].recordLength);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putfileptr (int  *indexNumber, FILE *filePtr)
/**********************************************************************/

{
/*	Places the input file pointer into the database at the location
	associated with the index number; if the index number is not a
	valid one, a -1 is returned  
	---------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		fileDataBase[*indexNumber].filePtr = filePtr;
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putlun_(int  *indexNumber, int  *fortranUnit)
/**********************************************************************/

{
/*	Places the file's fortran logical unit into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		fileDataBase[*indexNumber].fortranUnit = *fortranUnit;
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putlocalfile_(int  *indexNumber, char *localFile)
/**********************************************************************/

{
/*	Places the file's local file name into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		strcpy (fileDataBase[*indexNumber].localFile, localFile);
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putremotefile_(int  *indexNumber, char *remoteFile)
/**********************************************************************/

{
/*	Places the file's remote file name into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		strcpy (fileDataBase[*indexNumber].remoteFile, remoteFile);
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putaccesstype_(int  *indexNumber, char *accessType)
/**********************************************************************/

{
/*	Places the file's access type into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		strcpy (fileDataBase[*indexNumber].accessType, accessType);
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putstatus_(int  *indexNumber, char *status)
/**********************************************************************/

{
/*	Places the file's status into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		strcpy (fileDataBase[*indexNumber].status, status);
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putformattype_(int  *indexNumber, char *formatType)
/**********************************************************************/

{
/*	Places the file's format type into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		strcpy (fileDataBase[*indexNumber].formatType, formatType);
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/


/**********************************************************************/
 int  dbutil_putreclength_(int  *indexNumber, int  *recordLength)
/**********************************************************************/

{
/*	Places the file's record length into the database at the 
	location associated with the index number; if the index number is 
	not a valid one, a -1 is returned  
	---------------------------------  */
	
	if (*indexNumber >= 0 && *indexNumber <= 199)
	{
		fileDataBase[*indexNumber].recordLength = *recordLength;
		return (0L);
	}
	else
	{
		return (-1L);
	}
}

/**********************************************************************/
/**********************************************************************/
