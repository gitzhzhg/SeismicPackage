#include	"stdtest.h"
#include	<vthread.h>

#define N_STR	1000

static FILE*	F;
static char	Bigz[10*N_STR];
static char*	Str[26] =
	{	"aaaaaaaaa",
	 	"bbbbbbbbb",
	 	"ccccccccc",
	 	"ddddddddd",
	 	"eeeeeeeee",
	 	"fffffffff",
	 	"ggggggggg",
	 	"hhhhhhhhh",
	 	"iiiiiiiii",
	 	"jjjjjjjjj",
	 	"kkkkkkkkk",
	 	"lllllllll",
	 	"mmmmmmmmm",
	 	"nnnnnnnnn",
	 	"ooooooooo",
	 	"ppppppppp",
	 	"qqqqqqqqq",
	 	"rrrrrrrrr",
	 	"sssssssss",
	 	"ttttttttt",
	 	"uuuuuuuuu",
	 	"vvvvvvvvv",
	 	"wwwwwwwww",
	 	"xxxxxxxxx",
	 	"yyyyyyyyy",
	 	"zzzzzzzzz"
	};

#if _STD_C
void* writesmall(void* arg)
#else
void* writesmall(arg)
void*	arg;
#endif
{
	int	n;
	char	buf[16];

	strcpy(buf, Str[(int)arg]); buf[9] = '\n'; buf[10] = 0;
	for(n = 0; n < N_STR; ++n)
	{	if(fputs(buf, F) != 10)
			terror("fputs failed");
	}

	return arg;
}

#if _STD_C
void* writebig(void* arg)
#else
void* writebig(arg)
void*	arg;
#endif
{
	int	r = (rand()%3) + 1;	sleep(r);

	if(fwrite(Bigz,1,sizeof(Bigz),F) != sizeof(Bigz))
		terror("Writing bigz");

	return arg;
}

MAIN()
{
#if vt_threaded
	int		count[26];
	char*		s;
	int		i, k, n;
	char		buf[1024];
	Vthread_t*	thread[26];

	/* make the big z string */
	for(i = 0, s = Bigz; i < N_STR; ++i, s += 10)
		strcpy(s, "zzzzzzzzz\n");

	tmesg("\tTesting thread-safety\n");

	/* spin threads writing small chunks */
	F = fopen(tstfile(0),"w+");
	for(i = 0; i < 26; ++i)
	{	if(!(thread[i] = vtopen(0,0)) )
			terror("Creating thread %d", i);
		if(vtrun(thread[i], writesmall, (Void_t*)i) < 0)
			terror("Running thread %d", i);
	}

	for(i = 0; i < 26; ++i)
	{	count[i] = 0;
		vtwait(thread[i]);
	}

	if(fseek(F,0L,SEEK_SET) != 0)
		terror("Rewinding");

	for(n = 0;; ++n)
	{	if(!(s = fgets(buf, sizeof(buf), F)) )
			break;

		i = s[0] - 'a';
		if(i < 0 || i >= 26 || strlen(s) != 10)
			terror("Bad data s='%s' n=%d", s, n);
		s[9] = 0;
		if(strcmp(s, Str[i]) != 0)
			terror("Bad str s='%s' i=%d Str[i]='%s' n=%d", s, i, Str[i], n);

		count[i] += 1;
	}

	for(i = 0; i < 26; ++i)
		if(count[i] != N_STR)
			terror("Bad count[%d] = %d", i, count[i]);

	/* spin threads with one writing a big chunk */
	F = fopen(tstfile(0),"w+");
	for(i = 0; i < 25; ++i)
	{	if(!(thread[i] = vtopen(0,0)))
			terror("Creating thread %d", i);
		if(vtrun(thread[i],writesmall,(void*)i) < 0)
			terror("Running thread %d", i);
	}

	sleep(1);
	if(!(thread[i] = vtopen(0,0)) )
		terror("Creating big thread z");
	if(vtrun(thread[i],writebig,(void*)i) < 0)
		terror("Running big thread z");

	for(i = 0; i < 26; ++i)
	{	count[i] = 0;
		vtwait(thread[i]);
	}

	if(fseek(F,0L,SEEK_SET) != 0)
		terror("Rewinding");

	for(n = 0; ; ++n)
	{	if(!(s = fgets(buf, sizeof(buf), F)) )
			break;

		i = s[0] - 'a';
		if(i < 0 || i >= 26 || strlen(s) != 10)
			terror("Bad data s='%s' n=%d", s, n);
		s[9] = 0;
		if(strcmp(s, Str[i]) != 0)
			terror("Bad str s='%s' i=%d Str[i]='%s' n=%d", s, i, Str[i], n);
		count[i] += 1;

		if(i == 25) /* the 'z' */
		{	for(k = 1; k < N_STR; ++k, ++n)
			{	if(!(s = fgets(buf, sizeof(buf), F)) )
					terror("Premature eof n=%d", n);
				s[9] = 0;
				if(strcmp(s, Str[25]) != 0)
					terror("Bad str s='%s' n=%d", s, n);
				count[i] += 1;
			}
		}
	}

	for(i = 0; i < 26; ++i)
		if(count[i] != N_STR)
			terror("Bad count[%d] = %d", i, count[i]);
#endif

	TSTEXIT(0);
}
