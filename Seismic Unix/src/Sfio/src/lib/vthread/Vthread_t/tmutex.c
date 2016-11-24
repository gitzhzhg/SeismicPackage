#include	"vttest.h"

#define NITEMS		100
#define NTHREADS	16

int		Buf[NTHREADS*NITEMS];
int		Count;
Vtmutex_t	Mutex;
int		Inverted;

#if __STD_C
void* stuff(void* arg)
#else
void* stuff(arg)
void*	arg;
#endif
{
	int	n;

	if(!Inverted && vtmtxlock(&Mutex) < 0)
		terror("Can't lock mutex");

	for(n = 0; n < NITEMS; ++n)
	{
		Buf[Count++] = (int)arg;
#if _WIN32
		Sleep(1);	/* give up thread control */
#endif
	}

	if(!Inverted && vtmtxunlock(&Mutex) < 0)
		terror("Can't unlock mutex");

	return NIL(Void_t*);
}

main()
{
	Vthread_t*	vt[NTHREADS];
	int		i, count[NTHREADS];

	if(vtmtxopen(&Mutex, VT_INIT) != &Mutex)
		terror("Can't create mutex");

do_inverted:
	if(Inverted)
		tmesg("\tTesting non-locking threads\n");
	else	tmesg("\tTesting locking threads\n");

	/* prepare buffer */
	for(i = 0; i < NTHREADS*NITEMS; ++i)
		Buf[i] = -1;
	Count = 0;

	for(i = 0; i < NTHREADS; ++i)
	{	if(!(vt[i] = vtopen(0,0)) )
			terror("Can't create thread handle");
		if(vtrun(vt[i], stuff, (Void_t*)i) < 0)
			terror("Can't run thread %d", i);
	}

	for(i = 0; i < NTHREADS; ++i)
		if(vtwait(vt[i]) < 0)
			terror("Wait failed");

	if(Count != NTHREADS*NITEMS)
	{	if(!Inverted)
			terror("Wrong Count");
		else	tsuccess("Wrong Count as expected.");
	}

	for(i = 0; i < NTHREADS; ++i)
		count[i] = 0;

	for(i = 0; i < NTHREADS*NITEMS; i += NITEMS)
	{	int	k;
		count[Buf[i]] += 1;
		for(k = 1; k < NITEMS; ++k)
		{	if(Buf[k+i] != Buf[i])
			{	if(!Inverted)
					terror("Buffer messed up");
				else	tsuccess("Buffer messed up as expected.");
			}
		}
	}

	for(i = 0; i < NTHREADS; ++i)
	{	if(count[i] != 1 )
		{	if(!Inverted)
				terror("Count wrong");
			else	tsuccess("Count wrong as expected.");
		}
	}

	if(!Inverted)
	{	tmesg("\t\tLocking threads work fine.");
		Inverted = 1;
		goto do_inverted;
	}
	else	tmesg("\t\tNon-locking threads also work - weird but ok!\n");

	return 0;
}
