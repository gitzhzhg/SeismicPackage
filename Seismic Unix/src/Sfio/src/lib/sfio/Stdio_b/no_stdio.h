#ifndef _NO_STDIO_H
#define _NO_STDIO_H	1

/* While building the compatible stdio interface, we don't want <stdio.h>
** to be included. Fortunately, these files tend to be protected by a single
** macro defined at the top of each file to prevent multiple inclusion.
** This header file defines the same macros here.
** 
** Written by Kiem-Phong Vo.
*/

#ifndef __stdio_h__
#define __stdio_h__	1
#endif
#ifndef _stdio_h_
#define _stdio_h_	1
#endif
#ifndef _stdio_h
#define _stdio_h	1
#endif
#ifndef __h_stdio__
#define __h_stdio__	1
#endif
#ifndef _h_stdio_
#define _h_stdio_	1
#endif
#ifndef _h_stdio
#define _h_stdio	1
#endif
#ifndef __STDIO_H__
#define __STDIO_H__	1
#endif
#ifndef _STDIO_H_
#define _STDIO_H_	1
#endif
#ifndef _STDIO_H
#define _STDIO_H	1
#endif
#ifndef __H_STDIO__
#define __H_STDIO__	1
#endif
#ifndef _H_STDIO_
#define _H_STDIO_	1
#endif
#ifndef _H_STDIO
#define _H_STDIO	1
#endif
#ifndef _stdio_included
#define _stdio_included	1
#endif
#ifndef _included_stdio
#define _included_stdio	1
#endif
#ifndef _INCLUDED_STDIO
#define _INCLUDED_STDIO	1
#endif
#ifndef _STDIO_INCLUDED
#define _STDIO_INCLUDED	1
#endif
#ifndef _INC_STDIO
#define _INC_STDIO	1
#endif

#ifndef _FILE_DEFINED
#define _FILE_DEFINED	1	/* stop Windows from defining FILE	*/
#endif
#ifndef _FILEDEFED
#define _FILEDEFED	1	/* stop SUNOS5.8 ...			*/
#endif
#ifndef __FILE_defined
#define __FILE_defined	1	/* stop Linux ...			*/
#endif

#endif/*_NO_STDIO_H*/
