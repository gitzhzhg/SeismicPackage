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
/****************************************************************************************
 *
 *	File:		sys_dirent.h
 *	Created:	7/3/93		By:	George T. Talbot
 *	Purpose:	Implements UNIX-like directory reading for the Macintosh.
 *				Filesystem-independent directory information.
 *
 *	Modifications:
 *
 *	Notes:
 *			1) These routines will NOT work under A/UX.
 *			2) WD = working directory
 *			3) CD = change directory
 *			4) FS = file system
 *			5) Mac filesystems allow spaces as part of pathnames!
 *			6) All routines which return a path use the default Macintosh path separator,
 *			   a colon (":").
 *
 ****************************************************************************************/

/* $Id: sys_dir_mac.h,v 1.3 1998/02/02 21:44:51 smitchel Exp $ */

#ifndef	__sys_dirent_h
#define	__sys_dirent_h

#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)

#include <Files.h>

struct	dirent {
	/* PRIVATE FIELDS.  Use fields after PUBLIC	*/
	struct dirent	**next;
	FSSpec			fsp;
	
	/*	PUBLIC.	*/
	long			d_off;					/* index (to seekdir()) of this entry */	
	long			d_fileno;				/* File number (dirID) of this entry	*/
#define	d_parent	fsp.parID				/* File number (dirID) of parent	*/
#define	d_reclen	sizeof(struct dirent)	/* Size of this record	*/
#define	d_namelen	strlen(fsp.name)		/*	Length of the name	*/
#define	d_name		fsp.name				/*	Name	*/
#define	d_volume	fsp.vRefNum
};

#define	DIRSIZ(dp) sizeof(struct dirent)

#endif /* defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)  */
#endif	/* !__sys_dirent_h */
