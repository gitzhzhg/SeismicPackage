#ifndef _SFSTDIO_H
#define _SFSTDIO_H	1

/********************************************************************************
*	This product contains certain software code or other information	*
*	("AT&T Software") proprietary to AT&T Corp. ("AT&T").			*
*	The AT&T Software is provided to you "AS IS". YOU ASSUME TOTAL		*
*	RESPONSIBILITY AND RISK FOR USE OF THE AT&T SOFTWARE.			*
*	AT&T DOES NOT MAKE, AND EXPRESSLY DISCLAIMS, ANY EXPRESS OR		*
*	IMPLIED WARRANTIES OF ANY KIND WHATSOEVER, INCLUDING,			*
*	WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTABILITY OR	*
*	FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF TITLE OR		*
*	NON-INFRINGEMENT OF ANY INTELLECTUAL PROPERTY RIGHTS,			*
*	ANY WARRANTIES ARISING BY USAGE OF TRADE, COURSE OF DEALING OR		*
*	COURSE OF PERFORMANCE, OR ANY WARRANTY THAT THE AT&T SOFTWARE		*
*	IS "ERROR FREE" OR WILL MEET YOUR REQUIREMENTS. 			*
*										*
*	All rights reserved. AT&T is a registered trademark of AT&T Corp.	*
********************************************************************************/

/* Set the below to 1 to get the function forms of putc() and getc() */
#ifndef SF_FUNCTION_PUTCGETC
#define SF_FUNCTION_PUTCGETC	0
#endif

#include		<sfio_t.h>

typedef Sfio_t		_sfFILE;
#undef	FILE
#define FILE		_sfFILE
#undef	_FILE_DEFINED
#define _FILE_DEFINED	1	/* stop Windows from defining FILE	*/
#undef	_FILEDEFED
#define _FILEDEFED	1	/* stop SUNOS5.8 ...			*/
#undef	__FILE_defined
#define __FILE_defined	1	/* stop Linux ...			*/


#define BUFSIZ		SF_BUFSIZE

#define _IOFBF		0
#define _IONBF		1
#define _IOLBF		2
#define L_ctermid	32
#define L_cuserid	32
#define P_tmpdir	"/tmp/"
#define L_tmpnam	(sizeof(P_tmpdir)+32)

#undef fpos_t
#define fpos_t		Sfoff_t

_BEGIN_EXTERNS_
#if _BLD_sfio && defined(__EXPORT__)
#define extern	__EXPORT__
#endif
#if !_BLD_sfio && defined(__IMPORT__) && defined(__EXPORT__)
#define extern	__IMPORT__
#endif

extern char*	ctermid _ARG_((char*));
extern char*	cuserid _ARG_((char*));
extern char*	tmpnam _ARG_((char*));
extern char*	tempnam _ARG_((const char*, const char*));

extern Sfio_t*	_stdfdopen _ARG_((int, const char*));
extern Sfio_t*	_stdfopen _ARG_((const char*, const char*));
extern Sfio_t*	_stdfreopen _ARG_((const char*, const char*, Sfio_t*));
extern Sfio_t*	_stdpopen _ARG_((const char*, const char*));
extern Sfio_t*	_stdtmpfile _ARG_((void));
extern int	_stdfflush _ARG_((Sfio_t*));

extern int	_stdprintf _ARG_((const char*, ...));
extern int	_stdfprintf _ARG_((Sfio_t* f, const char*, ...));
extern int	_stdsprintf _ARG_((char*, const char*, ...));
extern int	_stdsnprintf _ARG_((char*, size_t, const char*, ...));
extern int	_stdvsnprintf _ARG_((char*, size_t, const char*, _ast_va_list));
extern int	_stdasprintf _ARG_((char**, const char*, ...));
extern int	_stdvasprintf _ARG_((char**, const char*, _ast_va_list));
extern int	_stdscanf _ARG_((const char*, ...));
extern int	_stdfscanf _ARG_((Sfio_t* f, const char*, ...));
extern int	_stdsetvbuf _ARG_((Sfio_t*, char*, int, size_t));
extern int	_stdfputc _ARG_((int, Sfio_t*));
extern int	_stdputc _ARG_((int, Sfio_t*));
extern int	_stdfgetc _ARG_((Sfio_t*));
extern int	_stdgetc _ARG_((Sfio_t*));
extern int	_stdputw _ARG_((int, Sfio_t*));
extern int	_stdgetw _ARG_((Sfio_t*));
extern ssize_t	_stdfwrite _ARG_((const Void_t*, size_t, size_t, Sfio_t*));
extern ssize_t	_stdfread _ARG_((Void_t*, size_t, size_t, Sfio_t*));
extern char*	_stdgets _ARG_((Sfio_t*, char*, int n, int isgets));

#undef extern
_END_EXTERNS_

#define _STDSIZE(s)		(sizeof(s) != sizeof(char*) ? sizeof(s) : BUFSIZ)

#define	printf			_stdprintf
#define fprintf			_stdfprintf

#define sprintf			_stdsprintf
#define snprintf		_stdsnprintf
#define vsnprintf		_stdvsnprintf
#define asprintf		_stdasprintf
#define vasprintf		_stdvasprintf

#define scanf			_stdscanf
#define fscanf			_stdfscanf

#define sscanf			sfsscanf

#define fdopen			_stdfdopen
#define fopen			_stdfopen
#define popen			_stdpopen
#define freopen			_stdfreopen
#define tmpfile			_stdtmpfile

#define setvbuf			_stdsetvbuf

#define putw			_stdputw
#define fputc			_stdfputc
#define fputc_unlocked		_stdfputc
#define fwrite			_stdfwrite
#define fwrite_unlocked		_stdfwrite

#define getw			_stdgetw
#define fgetc			_stdfgetc
#define fgetc_unlocked		_stdfgetc
#define fread			_stdfread
#define fread_unlocked		_stdfread


#define _std_fclose(f)		sfclose(f)
#define _std_pclose(f)		sfclose(f)

#define _std_flockfile(f)	sfmutex((f), SFMTX_LOCK)
#define _std_ftrylockfile(f)	sfmutex((f), SFMTX_TRYLOCK)
#define _std_funlockfile(f)	sfmutex((f), SFMTX_UNLOCK)

#define _std_putc(c,f)		sfputc((f),(c))
#define _std_putchar(c)		sfputc(sfstdout,(c))
#define _std_fputs(s,f)		sfputr((f),(s),-1)
#define _std_puts(s)		sfputr(sfstdout,(s),'\n')
#define _std_vprintf(fmt,a)	(int)sfvprintf(sfstdout,(fmt),(a))
#define _std_vfprintf(f,fmt,a)	(int)sfvprintf((f),(fmt),(a))
#define _std_doprnt(fmt,a,f)	(int)sfvprintf((f),(fmt),(a))
#define _std_vsprintf(s,fmt,a)	(int)sfvsprintf((s),_STDSIZE(s),(fmt),(a) )
#define _std_vasprintf(s,fmt,a)	(int)sfvasprintf((s),(fmt),(a) )

#define _std_getc(f)		sfgetc(f)
#define _std_getchar()		sfgetc(sfstdin)
#define _std_ungetc(c,f)	sfungetc((f),(c))
#define _std_fgets(s,n,f)	_stdgets((f),(s),(n),0)
#define _std_gets(s)		_stdgets(sfstdin,(s),_STDSIZE(s),1)
#define _std_vscanf(fmt,a)	(int)sfvscanf(sfstdin,(fmt),(a))
#define _std_vfscanf(f,fmt,a)	(int)sfvscanf((f),(fmt),(a))
#define _std_doscan(f,fmt,a)	(int)sfvscanf((f),(fmt),(a))
#define _std_vsscanf(s,fmt,a)	(int)sfvsscanf(s,(fmt),(a))

#define _std_fpurge(f)		sfpurge(f)
#define _std_fflush(f)		_stdfflush(f)

#define _std_rewind(f)		(sfseek((f), (Sfoff_t)0, SEEK_SET|SF_SHARE), sfclrerr(f) )
#define _std_fseek(f,o,t)	(sfseek((f), (Sfoff_t)(o), (t)|SF_SHARE) < 0 ? -1 : 0 )
#define _std_ftell(f)		((long)sfseek((f), (Sfoff_t)0, SEEK_CUR) )
#define _std_fgetpos(f,p)	((*(p) = _std_ftell(f)) >= 0 ? 0 : -1 )
#define _std_fsetpos(f,p)	(sfseek((f), *(p), SEEK_SET|SF_SHARE) != *(p) ? -1 : 0 )

#define _std_fseeko(f,o,t)	(sfseek((f), (Sfoff_t)(o), (t)|SF_SHARE) < 0 ? -1 : 0 )
#define _std_ftello(f)		((off_t)sfseek((f), (Sfoff_t)0, SEEK_CUR) )

#define _std_setbuf(f,b)	(sfsetbuf((f),(b),(b) ? BUFSIZ : 0) )
#define _std_setbuffer(f,b,n)	(sfsetbuf((f),(b),(n)) ? 0 : -1)
#define _std_setlinebuf(f)	sfset((f),SF_LINE,1)

#define _std_fileno(f)		sffileno(f)
#define _std_feof(f)		sfeof(f)
#define _std_ferror(f)		sferror(f)
#define _std_clearerr(f)	(sfclrlock(f), sfclrerr(f) )

#if defined(__INLINE__) && !_BLD_sfio
__INLINE__ int fclose(FILE* f)				{ return _std_fclose(f);	}
__INLINE__ int pclose(FILE* f)				{ return _std_pclose(f);	}

__INLINE__ void flockfile(FILE* f)			{ (void) _std_flockfile(f);	}
__INLINE__ int ftrylockfile(FILE* f)			{ return _std_ftrylockfile(f);	}
__INLINE__ void funlockfile(FILE* f)			{ (void) _std_funlockfile(f);	}

__INLINE__ int putc(int c, FILE* f)			{ return _std_putc(c,f);	}
__INLINE__ int putc_unlocked(int c, FILE* f)		{ return _std_putc(c,f);	}
__INLINE__ int putchar(int c)				{ return _std_putchar(c);	}
__INLINE__ int putchar_unlocked(int c)			{ return _std_putchar(c);	}
__INLINE__ int fputs(const char* s, FILE* f)		{ return _std_fputs(s,f);	}
__INLINE__ int fputs_unlocked(const char* s, FILE* f)	{ return _std_fputs(s,f);	}
__INLINE__ int puts(const char* s)			{ return _std_puts(s);		}
__INLINE__ int puts_unlocked(const char* s)		{ return _std_puts(s);		}
__INLINE__ int vprintf(const char* fmt, va_list a)	{ return _std_vprintf(fmt,a);	}
__INLINE__ int vfprintf(Sfio_t* f, const char* fmt, va_list a)
							{ return _std_vfprintf(f,fmt,a);}
__INLINE__ int _doprnt(const char* fmt, va_list a, FILE* f)
							{ return _std_doprnt(fmt,a,f);	}
__INLINE__ int vsprintf(char* s, const char* fmt, va_list a)
							{ return _std_vsprintf(s,fmt,a);}

__INLINE__ int getc(FILE* f)				{ return _std_getc(f);		}
__INLINE__ int getc_unlocked(FILE* f)			{ return _std_getc(f);		}
__INLINE__ int getchar(void)				{ return _std_getchar();	}
__INLINE__ int getchar_unlocked(void)			{ return _std_getchar();	}
__INLINE__ int ungetc(int c, FILE* f)			{ return _std_ungetc(c,f);	}
__INLINE__ char* fgets(char* s, int n, FILE* f)		{ return _std_fgets(s,n,f);	}
__INLINE__ char* fgets_unlocked(char* s, int n, FILE* f){ return _std_fgets(s,n,f);	}
__INLINE__ char* gets_unlocked(char* s)			{ return _std_gets(s);		}
__INLINE__ int vscanf(const char* fmt, va_list a)	{ return _std_vscanf(fmt,a);	}
__INLINE__ int vfscanf(Sfio_t* f, const char* fmt, va_list a)
							{ return _std_vfscanf(f,fmt,a);	}
__INLINE__ int _doscan(Sfio_t* f, const char* fmt, va_list a)
							{ return _std_doscan(f,fmt,a);	}
__INLINE__ int vsscanf(char* s, const char* fmt, va_list a)
							{ return _std_vsscanf(s,fmt,a); }

__INLINE__ int fpurge(FILE* f)				{ return _std_fpurge(f);	}
__INLINE__ int fflush(FILE* f)				{ return _std_fflush(f);	}
__INLINE__ int fflush_unlocked(FILE* f)			{ return _std_fflush(f);	}

__INLINE__ void rewind(FILE* f)				{ (void) _std_rewind(f);	}
__INLINE__ int fseek(FILE* f, long o, int t)		{ return _std_fseek(f,o,t);	}
__INLINE__ long ftell(FILE* f)				{ return _std_ftell(f);		}
__INLINE__ int fsetpos(FILE* f, fpos_t* pos)		{ return _std_fsetpos(f,pos);	}
__INLINE__ int fgetpos(FILE* f, fpos_t* pos)		{ return _std_fgetpos(f,pos);	}

__INLINE__ int fseeko(FILE* f, off_t o, int t)		{ return _std_fseeko(f,o,t);	}
__INLINE__ off_t ftello(FILE* f)			{ return _std_ftello(f);	}

__INLINE__ void setbuf(FILE* f, char* b)		{ (void) _std_setbuf(f,b);	}
__INLINE__ int setbuffer(FILE* f, char* b, int n)	{ return _std_setbuffer(f,b,n);	}
__INLINE__ int setlinebuf(FILE* f)			{ return _std_setlinebuf(f);	}

__INLINE__ int fileno(FILE* f)				{ return _std_fileno(f);	}
__INLINE__ int feof(FILE* f)				{ return _std_feof(f);		}
__INLINE__ int feof_unlocked(FILE* f)			{ return _std_feof(f);		}
__INLINE__ int ferror(FILE* f)				{ return _std_ferror(f);	}
__INLINE__ int ferror_unlocked(FILE* f)			{ return _std_ferror(f);	}
__INLINE__ void clearerr(FILE* f)			{ (void) _std_clearerr(f);	}
__INLINE__ void clearerr_unlocked(FILE* f)		{ (void) _std_clearerr(f);	}

#else

#define fclose(f)					( _std_fclose(f)		)
#define pclose(f)					( _std_pclose(f)		)

#define flockfile(f)					( _std_flockfile(f)		)
#define ftrylockfile(f)					( _std_ftrylockfile(f)		)
#define funlockfile(f)					( _std_funlockfile(f)		)

#define putc(c,f)					( _std_putc(c,f)		)
#define putc_unlocked(c,f)				( _std_putc(c,f)		)
#define putchar(c)					( _std_putchar(c)		)
#define putchar_unlocked(c)				( _std_putchar(c)		)
#define fputs(s,f)					( _std_fputs(s,f)		)
#define fputs_unlocked(s,f)				( _std_fputs(s,f)		)
#define puts(s)						( _std_puts(s)			)
#define puts_unlocked (s)				( _std_puts(s)			)
#define vprintf(fmt,a)					( _std_vprintf(fmt,a)		)
#define vfprintf(f,fmt,a)				( _std_vfprintf(f,fmt,a)	)
#define _doprnt(fmt,a,f) 				( _std_doprnt(fmt,a,f)		)
#define vsprintf(s,fmt,a)				( _std_vsprintf(s,fmt,a)	)

#define getc(f)						( _std_getc(f)			)
#define getc_unlocked(f)				( _std_getc(f)			)
#define getchar()					( _std_getchar()		)
#define getchar_unlocked()				( _std_getchar()		)
#define ungetc(c,f)					( _std_ungetc(c,f)		)
#define fgets(s,n,f)					( _std_fgets(s,n,f)		)
#define fgets_unlocked(s,n,f)				( _std_fgets(s,n,f)		)
#define gets(s)						( _std_gets(s)			)
#define gets_unlocked(s)				( _std_gets(s)			)
#define vscanf(fmt,a)					( _std_vscanf(fmt,a)		)
#define vfscanf(f,fmt,a)				( _std_vfscanf(f,fmt,a)		)
#define vsscanf(s,fmt,a)				( _std_vsscanf(s,fmt,a)		)

#define fpurge(f)					( _std_fpurge(f)		)
#define fflush(f)					( _std_fflush(f)		)
#define fflush_unlocked(f)				( _std_fflush(f)		)

#define rewind(f)					( (void)_std_rewind(f)		)
#define fseek(f,o,t)					( _std_fseek(f,o,t)		)
#define ftell(f)					( _std_ftell(f)			)
#define fgetpos(f,pos)					( _std_fgetpos(f,pos)		)
#define fsetpos(f,pos)					( _std_fsetpos(f,pos)		)

#define fseeko(f,o,t)					( _std_fseeko(f,o,t)		)
#define ftello(f)					( _std_ftello(f)		)

#define setbuf(f,b)					( (void)_std_setbuf(f,b)	)
#define setbuffer(f,b,n) 				( _std_setbuffer(f,b,n)		)
#define setlinebuf(f)					( _std_setlinebuf(f)		)

#define fileno(f)					( _std_fileno(f)		)
#define feof(f)						( _std_feof(f)			)
#define feof_unlocked(f)				( _std_feof(f)			)
#define ferror(f)					( _std_ferror(f)		)
#define ferror_unlocked(f)				( _std_ferror(f)		)
#define clearerr(f)					( (void)_std_clearerr(f)	)
#define clearerr_unlocked(f)				( (void)_std_clearerr(f)	)

#endif

/* require putc&getc to be functions, not macros */
#if SF_FUNCTION_PUTCGETC
#undef putc
#define putc	_stdputc
#undef getc
#define getc	_stdgetc
#endif

/* standard streams */
#ifdef SF_FILE_STRUCT
#define sfstdin		(&_Sfstdin)
#define sfstdout	(&_Sfstdout)
#define sfstderr	(&_Sfstderr)
#endif
#define stdin		sfstdin
#define stdout		sfstdout
#define stderr		sfstderr

#endif /* _SFSTDIO_H */
