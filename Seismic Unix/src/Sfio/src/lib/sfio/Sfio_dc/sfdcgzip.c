#include	"sfdchdr.h"

/*
 * sfio gzip discipline
 */

#if !_hdr_zlib

#if __STD_C
int sfdcgzip(Sfio_t* sp, int flags)
#else
int sfdcgzip(sp, flags)
Sfio_t*		sp;
int		flags;
#endif
{
	return -1;
}

#else

#include <zlib.h>
#include <sfdisc.h>

typedef	 gzFile		Gz_t;

typedef struct
{
	Sfdisc_t	disc;		/* sfio discipline		*/
	Gz_t*		gz;		/* gz handle			*/
} Sfgzip_t;

#ifndef newof
#define newof(p,t,n,x) \
		((p) ? 	(t*)realloc((Void_t*)(p), sizeof(t)*(n)+(x)) : \
			(t*)calloc(1, sizeof(t)*(n)+(x)) )
#endif

/*
 * gzip exception handler
 * free on close
 */

#if __STD_C
static int sfgzexcept(Sfio_t* sp, int op, void* val, Sfdisc_t* dp)
#else
static int sfgzexcept(sp, op, val, dp)
Sfio_t*		sp;
int		op;
void*		val;
Sfdisc_t*	dp;
#endif
{
	Sfgzip_t*	gz = (Sfgzip_t*)dp;
	int		r;

	switch (op)
	{
	case SF_ATEXIT:
		sfdisc(sp, SF_POPDISC);
		return 0;
	case SF_CLOSING:
	case SF_DPOP:
	case SF_FINAL:
		if (gz->gz)
		{
			if (r = gzclose(gz->gz) ? -1 : 0)
				sp->_flags |= SF_ERROR;
			gz->gz = 0;
		}
		else
			r = 0;
		if (op != SF_CLOSING)
			free(dp);
		return r;
	}
	return 0;
}

/*
 * sfio gzip discipline read
 */

#if __STD_C
static ssize_t sfgzread(Sfio_t* fp, Void_t* buf, size_t size, Sfdisc_t* dp)
#else
static ssize_t sfgzread(fp, buf, size, dp)
Sfio_t*		fp;
Void_t*		buf;
size_t		size;
Sfdisc_t*	dp;
#endif
{
	Sfgzip_t*	gz = (Sfgzip_t*)dp;

	return gzread(gz->gz, buf, size);
}

/*
 * sfio gzip discipline write
 */

#if __STD_C
static ssize_t sfgzwrite(Sfio_t* fp, const Void_t* buf, size_t size, Sfdisc_t* dp)
#else
static ssize_t sfgzwrite(fp, buf, size, dp)
Sfio_t*		fp;
const Void_t*	buf;
size_t		size;
Sfdisc_t*	dp;
#endif
{
	Sfgzip_t*	gz = (Sfgzip_t*)dp;

	return (gzwrite(gz->gz, (void*)buf, size) == size) ? size : -1;
}

/*
 * create and push the sfio gzip discipline
 *
 * return
 *	>0	discipline pushed (gzip or lzw)
 *	 0	discipline not needed
 *	<0	error
 */

#if __STD_C
int sfdcgzip(Sfio_t* sp, int flags)
#else
int sfdcgzip(sp, flags)
Sfio_t*		sp;
int		flags;
#endif
{
	char*		m;
	Sfgzip_t*	gz;
	char		mode[10];

	if (sfset(sp, 0, 0) & SF_READ)
	{
		register unsigned char*	s;
		register int		n;

		/*
		 * peek the first 2 bytes to verify the magic
		 *
		 *	0x1f8b	sfdcgzip	gzip	
		 *	0x1f9d	sfdclzw		compress
		 */
		
		if (!(s = (unsigned char*)sfreserve(sp, 2, 1)))
			return -1;
		n = (s[0] != 0x1f || s[1] != 0x8b) ? -1 : 0;
		sfread(sp, s, 0);
		if (n < 0)
			return -1;
	}

	if (!(gz = newof(0, Sfgzip_t, 1, 0)))
		return -1;
	gz->disc.exceptf = sfgzexcept;
	if (sfset(sp, 0, 0) & SF_READ)
		gz->disc.readf = sfgzread;
	else
		gz->disc.writef = sfgzwrite;
	m = mode;
	*m++ = (sfset(sp, 0, 0) & SF_READ) ? 'r' : 'w';
	*m++ = 'b';
	*m++ = 'o';
	if ((flags &= 0xf) > 0 && flags <= 9)
		*m++ = '0' + flags;
	*m = 0;
	if (sfdisc(sp, &gz->disc) != &gz->disc || !(gz->gz = gzdopen(sffileno(sp), mode)))
	{
		free(gz);
		return -1;
	}

	sfsetbuf(sp, 0, SF_BUFSIZE);

	return 0;
}

#endif /*_hdr_zlib*/
