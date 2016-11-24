#define _in_flsbuf	1
#include	"sfstdio.h"

/*	Flush output buffer.
**	Written by Kiem-Phong Vo
*/

FLSBUF(c,f)
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	if((c = sfputc(sf,c)) < 0)
		_stdseterr(f,sf);
	else if(!(sf->flags&(SF_LINE|SF_MTSAFE)))
	{	/* fast access to buffer for putc benefit */
#if _FILE_writeptr
		f->std_writeptr = sf->next;
		f->std_writeend = sf->endb;
#endif
#if _FILE_readptr
		f->std_readptr = f->std_readend = NIL(uchar*);
#endif
#if _FILE_ptr || _FILE_p
		f->std_ptr = sf->next;
#endif
#if _FILE_cnt
		f->std_cnt = sf->endb - sf->next;
#endif
#if _FILE_w
		f->std_w = sf->endb - sf->next;
#endif
#if _FILE_r
		f->std_r = 0;
#endif
#if _FILE_writeptr || _FILE_cnt || _FILE_w
		/* internal protection against mixing of sfio/stdio */
		sf->mode |= SF_STDIO;
		sf->endr = sf->endw = sf->data;
#endif
	}

	return c;
}
