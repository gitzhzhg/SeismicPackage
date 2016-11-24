/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/********************************************************************
VND - large out-of-core multidimensional block matrix transpose
*********************************************************************
The VND routines are intended for avoiding the need for virtual
memory when prototyping large out-of-core problems where 
multidimensional matrix transposes are required.  The data are 
stored in a multidimensional block matrix transpose format where 
the dimension of the matrix is user defined.  The data are accessed 
by the user as vectors for any given dimension with all the other 
dimensions fixed or defined as a key.  The vector elements all have 
the same user defined number of bytes per element (or node).  The user
specifies how much memory he would like to allocate for buffering
VND I/O operations.  The VND routines break up the multidimensional
matrix into chunks small enough to fit within this memory limitation
if at all possible.  If not, the routines use the smallest amount
of memory that they can.  If the data fits in memory, then it
is put into memory and no I/O operations are required except to 
save on disk.

A dimension over which the data need not be transposed can be defined as the
panel dimension.  If panels are used, then the file structure becomes
that of a sequence of block matrix transpose structures, one for 
each panel.  Normally, only one panel is stored in memory at a time.
The user has the option to specify 2 or more simultaneously open
panels at the expense of using more memory.  The variable "iopanel"
ranges from 0 to the number of open panels minus 1.  The variable
"ipanel" ranges from 0 to the number of panels in the VND files
minus 1.  Two calls to VNDrw with different values of "iopanel"
should not have the same value of "ipanel" unless both are only reading.

There is a preferred dimension for Input/Output efficiency.  That is
the first dimension (idim=0, all of the indicies are zero based).  In
the first dimension, a slice through the block matrix structure can
be read or written in one request.  In all all other dimensions, reading
or writing a slice requires multiple I/O requests.  Therefore, it
is best to choose the first dimension for the dimension with the
most I/O activity.

Real-to-Complex FFT's increase the number of bytes per node and
decrease the number of nodes in a give dimension by a factor
of 2.  To allow this option in dimension 0, the number of nodes
per block in dimension 0  is forced to be even.  This allows
one  to modify the number of bytes per node and number of nodes
in the first dimension to change the data from real to complex
should that be necessary.  This option is not possible for
other than the first dimension.  Clearly, which dimension is 
chosen to be the first is arbitrary other than for considerations
of Input/Output efficiency and this option.

For some applications, it may be desirable to have two dimensions
of size NA and NB in memory at a time to allow rapid changes in
access direction.  Define a single VND dimension with
size NA*NB and access values using the parameters defining starting
element and element increment in VNDrw.  This option keeps user
buffer sizes small and minimizes I/O overhead for some problems.
For implementing the Fowler DMO algorithm, one would use
temporal frequency and velocity together as a function of CMP
position in a single dimension.  This would allow the VND
routines to avoid swapping block matrix slices during the initial 
write of the VND files as the input CMP gathers are stacked for
a range of velocities, Fourier transformed from time to frequency,
and written out to the VND file.  
 
The Unix operating system typically has a 2 gigabyte limit for each 
file system.  When this limit is too small, the VND routines can spread 
the data out over multiple file systems as defined by the user.  Only 
one physical file is actually open at one time to avoid the Unix limitations
on the number of open files allowed.  The code is written in ANSI C and 
should not be limited to the Unix operating system.

The call to VNDop with mode equal to 2 can take a long time as the
first call initializes the file structure to have all zero bytes.
This may involve a significant amount of I/O for large VND files.

*********************************************************************
The VND user C interface consists of 4 routines:
*********************************************************************
VND *VNDop(int mode, long lwmax, int ndim, long *N, long npanels, 
		long nbytes, char *file, int ndir, char**dir, int noppanels)
	
	Open block matrix file structure.
*********************************************************************
	VND *VNDop	returns as pointer to VND I/O control structure
			or NULL if failed
 
	int mode	0 for read only
			1 for read and write (existing file)
			2 for write and read (create new file)
			3 for don't open, just compute buffer sizes 
			and fill out structure
	long lwmax	Maximum memory buffer size in bytes to use
	int ndim	Number of block matrix transpose dimensions
	long N[ndim]	List of number of nodes for each dimension
	long npanels	Number of panels of block matrix structure
	long nbytes	Number of bytes per node
	char *file	File name from which other file names will be 
			constructed.  The actual files used will have
			.VND0, .VND1, ... extentions.  This name
			should not have such an extention.
	int ndir	Number of file directories available for holding data.
			Set ndir=0 to use one file in current directory.
			Set ndir=5 to spread data out over 5 files of
			roughly equal size in the directories dir[j].
			Set ndir=-1 to look at .VND file for directories
			to use.  File format is "number of directories"
			on first line and then one line per directory name.
	char **dir	dir[j] = name of file directory for storing data.
			If dir[0]="/users/junk" and file="file", then the 
			first physical file will be "/users/junk/file.VND0"
	int noppanels	Max number of panels to be open at one time


	VNDop returns a value of NULL if open failed.

*********************************************************************
void VNDrw(char iop, int iopanel, VND *vnd, int idim, long *key, long ipanel, 
		char *values, long start, long incr, long nvalues,
		int msgnum, char *msg)

	Read or write a vector for dimension "idim" 
*********************************************************************
	char iop	'r' for read; 'w' for write
	int iopanel	Index of current open panel buffer 
			(Ranges from 0 to noppanels-1.  Used to
			allow access to multiple panels simultaneously.
			For most applications, noppanels = 1 and 
			iopanel will always equal 0.)	
	VND *vnd	Structure holding block matrix file information.
	int idim	Current dimension for read or write
                        (idim=0 is first dimension)
	long key[]	List of node positions for each dimension 
			(Value for dimension "idim" needs to be there
			but it's value is irrelevant.)
	long ipanel	Panel number for read or write.
			(ipanel=0 is first panel)
	char values[]	Returns as values read or input as values to be 
			written.
	long start	starting node (0 based, usually equals 0)
	long incr	node increment (usually 1)
	long nvalues	number of values to read or write 
			(usually same as number of values in 
			the specified dimension)
	int msgnum	A message number to be written out if error occurs.
	char *msg	A message to be written out if error occurs.
*********************************************************************
void VNDcl(VND *vnd, int mode)

	Close VND file structure.
*********************************************************************
	VND *vnd	Structure holding VND block matrix file information.
	int mode	0 means keep 
			1 means delete
*********************************************************************
void VNDflush(VND *vnd)

	Flush out VND buffers to file.  
*********************************************************************
	This may be important in applications where 
	checkpoint/restart capabilities are needed.  
	After VNDflush, the data written by the VNDrw
	routine are safely stored on disk rather than 
	just in a memory buffer where it could be lost in
	the event of a machine failure.

	VND *vnd	VND control structure

When doing real to complex FFT's, the following routines may
be helpful.
*********************************************************************
void VNDr2c(VND *vnd)

	Convert dimension 0 from real to complex by scaling
	the number of bytes/node by 2 and halving the
	number of nodes.  (Note: only works for dimension 0)
*********************************************************************
void VNDc2r(VND *vnd)

	Convert dimension 0 from complex to real by dividing
	the number of bytes/node by 2 and doubling the
	number of nodes.  (Note: only works for dimension 0)
*********************************************************************
When working in only 2 dimensions, the following routines may
be convenient as they have fewer parameters and don't require
the user to allocate arrays for N or key.  These routines
call the VND routines and are completely compatible.  A FORTRAN
equivalent routine exists with the same calling sequence for
all but the V2Dop routine.  The open routine assumes that
a single VND file can be opened in the current directory.
*********************************************************************
VND * V2Dop(int mode, long lwmax, long nbytes, char *file,long n0, long n1)

	Open 2-D VND file structure.
*********************************************************************
	int mode	0 for read only
			1 for read and write (existing file)
			2 for write and read (create new file)
			3 for don't open, just compute buffer sizes 
			and fill out structure
	long lwmax	Maximum memory buffer size in bytes to use
	long nbytes	Number of bytes per node
	char *file	File name from which VND file name will be 
			constructed.  The actual file used will have
			.VND0 extention.  This name
			should not have such an extention.
	long n0		number of nodes in dimension 0
	long n1		number of nodes in dimension 1
*********************************************************************
void V2Dr0(VND *vnd,long key,char *buf,int msgnum)	read dimension 0
void V2Dr1(VND *vnd,long key,char *buf,int msgnum)	read dimension 1
void V2Dw0(VND *vnd,long key,char *buf,int msgnum)	write dimension 0
void V2Dw1(VND *vnd,long key,char *buf,int msgnum)	write dimension 1
*********************************************************************
Memory management within the VND routines is based upon
the following routines.  These routines, when used together,
provide automatic error checking at the end of a job
during the VNDfree() step for any buffer overruns telling
the user if an overrun occured at the beginning of the
buffer or at the end.  To ensure that memory overruns
have not occured, a few extra bytes of information must
be stored for each buffer that is not available to the
user program.  Therefore, memory allocated
by VNDemalloc() must be freed by VNDfree() rather than
the standard free() routine.  It is sometimes convenient
to find out the total amount of currently allocated
VND memory.  A routine exists for that purpose.  At the
end of a job, you can ensure that all allocated VND 
buffers have been freed and checked for overruns by
seeing if the total allocated memory at that point
is zero.
*********************************************************************
void *VNDemalloc(size_t n,char *msg)

	Allocate memory and return a pointer.  
	Give error message and abort if fail.
*********************************************************************
	size_t n	number of bytes of memory to allocate
	char *msg	error message to print if abort
*********************************************************************
void VNDmemchk( void *p , char * msg)			

	check for memory overrun and abort if error found
	(can distinguish between overrun at beginning of
	allocated buffer versus overrun at end of allocated
	buffer)
*********************************************************************
	void *p		pointer returned from VNDemalloc
	char *msg	error message to print out if error found
*********************************************************************
long VNDtotalmem()

	return total current memory allocated by VNDemalloc
	in characters
*********************************************************************
void VNDfree( void *p, char *msg)

	free memory allocated by VNDemalloc
	(also calls VNDmemchk to check for memory 
	overflows during job)
*********************************************************************
	void *p		pointer returned from VNDemalloc
	char *msg	error message to print out if error found

	
The following routine returns a unique temporary file
root name for VND files. VNDtempname() 
returns a name made up from the input root string, the
process id, and a unique integer.  This is the only VND
routine calling a UNIX specific function or using the
header files <sys/types.h> or <sys/errno.h>.  The other
routines should be portable to non-Unix systems.
*********************************************************************
char *VNDtempname(char *root)

Memory associated with a VNDtempname() character string
must be freed using VNDfree() rather than free().
*********************************************************************
A short test program might check the VND_HOME file for
a .VND file for file systems over which to spread the data.
This has been accomplished by coding "ndir=-1" in VNDop.
A 3 dimensional 50x50x5 block matrix system is set up over
1 panel. The user wishes to use a memory buffer of 1000 
integers. The data are initialized to equal the first dimension
index and written out in vectors along the first dimension (idim=0).
Then, the data are then read from the file along vectors in 
the second dimension (idim=1) and dumped to check and see if 
they are correct.  Note that it would have required memory the
size of 12500 integers to hold the entire 3-D matrix in memory.

#include "VND.h"
int main(){
	int i;
	int j;      
	int k;      
	static long N[3]={50,50,5};
	long key[3];
	long buf[50];  
	VND *vnd;
	char **dir;

	fprintf(stdout,"Open the VND file and initialize values to 0\n");
	vnd=VNDop(2,9999,3,N,1,sizeof(long),"george",-1,dir,1);
	VNDdump(vnd,stdout);

	fprintf(stdout,"Write out data in first dimension slices\n");
	fprintf(stdout,
   		"Values = i+j*100+k*10000 where i,j,k are the keys\n");
	fprintf(stdout,"in the first, second, and third dimensions\n");
	for(k=0;k<N[2];k++) {
 		key[2]=k;
   		for(j=0;j<N[1];j++) {
			key[1]=j;
        		for(i=0;i<N[0];i++) buf[i]=i + j*100 + k*10000;
			VNDrw('w',0,vnd,0,key,0,(char *)buf,0,1,N[0],
				1,"writing 1st dimension (0)");
		}
   	}

	fprintf(stdout,"\nRead slices along second dimension with k=1 \n");
	fprintf(stdout,"Values should equal i+j*100+k*10000\n");
	fprintf(stdout,"so for this test, the values in buf should\n");
	fprintf(stdout,"increment by 100.  Each line should increment\n");
	fprintf(stdout,"by 1.  Since k=1, the starting value is 10000.\n");
	key[2]=1;
	for(i=0;i<N[0];i++) {
		key[0]=i;
		VNDrw('r',0,vnd,1,key,0,(char *)buf,0,1,N[1],
			2,"reading 2nd dimension (1)");
		printf(" First Dimension = %d  Values 0-4 = %ld %ld %ld %ld %ld\n",
			i,buf[0],buf[1],buf[2],buf[3],buf[4]);		
	}

	fprintf(stdout,"\nRead data slices in first dimension\n");
	fprintf(stdout,"so that values on a line increment by 1\n");
	key[2]=0;
	fprintf(stdout,"Third dimension is fixed with k=%ld\n",key[2]);
	fprintf(stdout,"Second dimension varies in reverse order\n");
	fprintf(stdout,"so lines will decrement by 100.\n");
	for(j=N[1]-1;j>=0;j--) {
		key[1]=j;
		VNDrw('r',0,vnd,0,key,0,(char *)buf,0,1,N[0],
			3,"reading 1st dimension in reverse order");
		printf(" Second Dimension = %d  Values 0-4 = %ld %ld %ld %ld %ld\n",
			j,buf[0],buf[1],buf[2],buf[3],buf[4]);		
	}


	fprintf(stdout,"\nRead data slices in third dimension\n");
	fprintf(stdout,"so values on a line should increment by 10000\n");
	key[0]=41;
	fprintf(stdout,"First dimension is fixed with i=%ld\n",key[0]);
	fprintf(stdout,"Second dimension varies so that successive\n");
	fprintf(stdout,"should increment by 100\n");
	for(j=0;j<N[1];j++) {
		key[1]=j;
		VNDrw('r',0,vnd,2,key,0,(char *)buf,0,1,N[2],
			4,"reading 3rd dimension");
		printf(" Second Dimension = %d  Values 0-4 = %ld %ld %ld %ld %ld\n",
			j,buf[0],buf[1],buf[2],buf[3],buf[4]);		
	}

	VNDcl(vnd,1);
	return (0);
}

*********************************************************************
Utility routines used internally by VNDop, VNDrw, and VNDcl:
*********************************************************************
int VNDGetDimensions(VND *vnd, long lwmax, int ich)
int VNDrwslice(VND *vnd, char *w, char mode, int idim, long islice, long ipanel)
int VNDseek(VND *vnd, long ib)
void VNDdump(VND *vnd,FILE *fp)
*********************************************************************
int VNDGetDimensions(VND *vnd, long lwmax, int ich)

	Set dimensions for block matrix transpose 
	in VND control structure.
*********************************************************************
	Initialize:
		vnd.NumBlocksPerPanel
		vnd.NumBytesMemBuf
		vnd.NumBytesPerBlock
		vnd.NumBytes1stDim
		vnd.NumNodesPerBlock
		vnd.NNodesPerBlock[k]   for k=0 to NumDim-1 
		vnd.NBlocks[k]
		vnd.ByteIncr[k]
		vnd.BlockIncr[k]


	VND *vnd 	VND control structure
			Values required to be set on input
			include vnd.N[k] and vnd.NumBytesPerNode
	long lwmax	number of characters of memory available	
	int ich 	1 if force even number of first dimension
			nodes per block (useful to allow
			real-to-complex ffts and make it
			possible to change number of bytes
			per node at a later point)

	return	-1 if all fits in memory
		0 if successful in partitioning to fit in memory
		1 if failed to partition so fits in memory

*********************************************************************
int VNDrwslice(VND *vnd,int iopanel, char mode, int idim, 
			long iblock, long ipanel)

	Read or write a slice for dimension idim into or from memory
*********************************************************************
	VND *vnd	pointer to VND control structure
	int iopanel	open panel counter (0 based. Always
			equals 0 if only one panel open at a time.)
	char mode	'r' for read; 'w' for write.
	int idim	Dimension of slice to be read or written.
			(0 based.  First dimension is idim=0.)
	long iblock	Starting block for block matrix transpose 
			slice associated with dimension idim.
			(Be careful--not sequential 0,1,2,...)
	long ipanel	Panel number (0 based) for desired slice.

	Function returns a value of 0 if successful.
*********************************************************************
int VNDseek(VND *vnd, long ib)

	Seek the desired block in VND file structure.
	(a) Go to the correct physical file, reopen if necessary
	(b) Seek the correct starting byte within the file
*********************************************************************
	VND *vnd	pointer to VND control structure
	long ib		desired block number (0 based,
			covers all panels and all files)

	The function returns 0 if successful, Nonzero if fail.
*********************************************************************
void VNDdump(VND *vnd,FILE *fp)

	Dump out values stored in VND structure to file.
*********************************************************************
	VND *vnd	pointer to VND control structure
	FILE *fp	FILE pointer associated with output file

*********************************************************************
void VNDerr(char *msg);

	Abort and print message.
	char *msg	message to print prior to abort.

***************************************************************************/

/***************************************************************************
This is the Fortran user interface to the VND routines.


*********************************************************************
VNDOP(vnd, mode, lwmax, ndim, N, npanels, 
		nbytes, file, ndir, dir, noppanels)
	
	Open block matrix file structure.

*********************************************************************
	int vnd		returns as pointer to filled out I/O 
			control structure 
	integer mode	0 for read only
			1 for read and write (existing file)
			2 for write and read (create new file)
			3 for don't open, just compute buffer sizes 
			and fill out structure
	integer lwmax	Maximum memory buffer size in nodes to use
	integer ndim	Number of block matrix transpose dimensions
	integer N[ndim]	List of number of nodes for each dimension
	integer npanels	Number of panels of block matrix structure
	integer nbytes	Number of bytes per node
	character*80 file File name from which other file names will be 
			constructed.  The actual files used will have
			.VND0, .VND1, ... extentions.  This name
			should not have such an extention.
	integer ndir	Number of file directories available for holding data.
			Set ndir=0 to use one file in current directory.
			Set ndir=-1 to look at .VND file for directories
			to use.  File format is "number of directories"
			on first line and then one line per directory name.
	character*80 dir[] list of directories for VND files
	int noppanels	Max number of panels to be open at one time


*********************************************************************
VNDRW(iop, iopanel, vnd, idim, key, ipanel, 
		values, start, incr, nvalues, msgnum)

	Read or write a vector for dimension "idim" 

*********************************************************************
	character*1 iop	"r" for read; "w" for write
	integer iopanel	Index of current open panel buffer 
			(Ranges from 1 to noppanels.  Used to
			allow access to multiple panels simultaneously.
			For most applications, noppanels = 1 and 
			iopanel will always equal 1.)	
	integer vnd	Structure holding block matrix file information.
	integer idim	Current dimension for read or write
			(idim=1 for first dimension.)
	integer key[]	List of node positions for each dimension 
			(Value for dimension "idim" needs to be there
			but it's value is irrelevant.)
	integer ipanel	Panel number for read or write.
			(ipanel=1 for first panel.)
	integer values[]Returns as values read or input as values to be 
			written.
	integer start	starting node (First node is 1, usually equals 1)
	integer incr	node increment (usually 1)
	integer nvalues	number of values to read or write 
			(usually same as number of values in 
			the specified dimension)
	integer msgnum	A message number to be written out if error occurs.

*********************************************************************
VNDCL(vnd, mode)

	Close VND file structure.

*********************************************************************
	integer vnd	Structure holding VND block matrix file information.
	integer mode	0 means keep 
			1 means delete


*********************************************************************
VNDFLUSH(vnd)

	Flush out VND buffers to file.  

*********************************************************************
	This may be important in applications where 
	checkpoint/restart capabilities are needed.  
	After VNDFLUSH, the data written by the VNDRW
	routine are safely stored on disk rather than 
	just in a memory buffer where it could be lost in
	the event of a machine failure.

	integer vnd	VND control structure

An example Fortran code similar to the C code example above
appears below.  On the Cray (or Convex in PD8 mode), be sure
to specify the bytes per node to be 8 instead of 4 in the
the VNDOP call.

	PROGRAM EXAMPLE
	INTEGER VND, N(3), KEY(3), BUF(50), I, J, K
	CHARACTER*80 FILE
	CHARACTER*80 DIR(2)
	FILE='GEORGE'
	N(1)=50
	N(2)=50
	N(3)=5
	OPEN(10,FILE='LISTING')
	CALL VNDOP(VND,2,1000,3,N,1,4,FILE,-1,DIR,1)

	WRITE(10,*) ' '
	WRITE(10,*) ' WRITE OUT THE DATA IN THE FIRST DIMENSION'
	WRITE(10,*) ' DATA VALUES = I + J*100 + K*10000'
	WRITE(10,*) ' WHERE I=1ST DIM, J=2ND DIM, AND K=3RD DIM'
	DO 300 K=1,N(3)
	   KEY(3)=K
	   DO 200 J=1,N(2)
		DO 100 I=1,N(1)
  100		BUF(I) = I + 100*J + 10000*K
		KEY(2)=J
		CALL VNDRW('W',1,VND,1,KEY,1,BUF,1,1,N(1),1)
  200      CONTINUE
  300	CONTINUE

	WRITE(10,*) ' '
	WRITE(10,*) ' READ THE DATA ALONG DIMENSION 2 '
	WRITE(10,*) ' THE 3RD DIMENSION IS HELD FIXED WITH K=2'
	WRITE(10,*) ' VALUES ON A LINE SHOULD INCREMENT BY 100'
	WRITE(10,*) ' VALUES SHOULD INCREMENT BY 1 FROM LINE TO LINE'
	KEY(3)=2
	DO 400 I=1,N(1)
		KEY(1)=I
		CALL VNDRW('R',1,VND,2,KEY,1,BUF,1,1,N(2),2)
		WRITE(10,*) ' I = ',I,' BUF(1-5)=',(BUF(J),J=1,5)
  400	CONTINUE

	WRITE(10,*) ' '
	WRITE(10,*) ' READ THE DATA ALONG DIMENSION 1'
	WRITE(10,*) ' THE 3RD DIMENSION IS HELD FIXED WITH K=1'
	WRITE(10,*) ' VALUES ON A LINE SHOULD INCREMENT BY 1'
	WRITE(10,*) ' VALUES SHOULD DECREMENT BY 100'
	KEY(3)=1
	DO 500 J=1,N(2)
		KEY(2)=N(2)+1-J
		CALL VNDRW('R',1,VND,1,KEY,1,BUF,1,1,N(1),3)
		WRITE(10,*) ' J = ',KEY(2),' BUF(1-5)=',(BUF(I),I=1,5)
  500	CONTINUE

	WRITE(10,*) ' '
	WRITE(10,*) ' READ THE DATA ALONG DIMENSION 3'
	WRITE(10,*) ' THE 1ST DIMENSION IS HELD FIXED WITH I=41 '
	WRITE(10,*) ' VALUES ON A LINE SHOULD INCREMENT BY 10000'
	WRITE(10,*) ' VALUES SHOULD INCREMENT BY 100 FROM LINE TO LINE'
	KEY(1)=41
	DO 600 J=1,N(2)
		KEY(2)=J
		CALL VNDRW('R',1,VND,3,KEY,1,BUF,1,1,N(3),4)
		WRITE(10,*) ' J = ',J,' BUF(1-5)=',(BUF(K),K=1,5)
  600	CONTINUE


	CALL VNDCL(VND,1)	
	CLOSE(10)
	STOP
	END

**************************************************************
Credits: J.E. Anderson (CSM Mobil Visiting Scientist, 1993)
***************************************************************

***************************************************************/
/**************** end self doc ********************************/


#include "VND.h"
static long total_VND_mem=0;

VND *VNDop(int mode, long lwmax, int ndim, long *N, 
		long npanels, long nbytes, char *file,
		int ndir, char**dir, int noppanels)

/**************************************************************************

VND *VNDop(int mode, long lwmax, int ndim, long *N, long npanels, 
		long nbytes, char *file, int ndir, char**dir, int noppanels)
	
	Open block matrix file structure.

	VND *VNDop	returns as pointer to VND I/O control structure
			or NULL if filespace not available
 
	int mode	0 for read only
			1 for read and write (existing file)
			2 for write and read (create new file)
			3 for don't open, just compute buffer sizes 
			and fill out structure
	long lwmax	Maximum memory buffer size in bytes to use
	int ndim	Number of block matrix transpose dimensions
	int N[ndim]	List of number of nodes for each dimension
	int npanels	Number of panels of block matrix structure
	int nbytes	Number of bytes per node
	char *file	File name from which other file names will be 
			constructed.  The actual files used will have
			.VND0, .VND1, ... extentions.  This name
			should not have such an extention.
	int ndir	Number of file directories available for holding data.
			Set ndir=0 to use one file in current directory.
			Set ndir=5 to spread data out over 5 files of
			roughly equal size in the directories dir[j].
			Set ndir=-1 to look at .VND file for directories
			to use.  File format is "number of directories"
			on first line and then one line per directory name.
	char **dir	dir[j] = name of file directory for storing data.
			If dir[0]="/users/junk" and file="file", then the 
			first physical file will be "/users/junk/file.VND0"
	int noppanels	Max number of panels to be open at one time

***************************************************************************/
{
	char file1[120];
	char dir1[120];
	int j;
	int k;
	int ndir1=-1;
	long iblock;
	long ipanel;
	long lcontrol;
	VND *vnd;
	FILE *fp=NULL, *fpdir=NULL;
	vnd = VNDemalloc(sizeof(VND),"VNDop: allocating VND");

	/* Build the name of the first VND file */
	if(ndir<0){
		if( (fpdir=fopen(VND_HOME,"r"))!=NULL ){
			j=fscanf(fpdir,"%d",&ndir1);
			j=fscanf(fpdir,"%s",dir1);
			(void)sprintf(file1,"%s/%s.VND0",dir1,file);
		}else{
			ndir1=0;
			(void)sprintf(file1,"%s.VND0",file);
			}
	}else{
		if(ndir==0){
			(void)sprintf(file1,"%s.VND0",file);
		}else{
			(void)sprintf(file1,"%s/%s.VND0",dir[0],file);
		}
	}

	if(mode<=1) {
		if( (fp = fopen(file1,"r"))==NULL) {
			fprintf(stderr,"Could not open file %s \n",
				file1);
			return(NULL);
		}
		j=(int) fread(vnd,sizeof(VND),1,fp);
		if(j!=1) VNDerr(file1);
	}else{
		vnd->NumDim          = ndim;
		vnd->NumBytesPerNode = nbytes;
		vnd->NumPanels       = npanels;
		if(mode!=3){
			fp = fopen(file1,"w+");
			if(fp == NULL) {
				fprintf(stderr,
				"Could not create file %s \n",
				file1);
				return(NULL);
			}
			if(ndir==0){vnd->NumFiles=1;}
			else if(ndir>0) {
				vnd->NumFiles=ndir;
			}else{
				if(ndir1==0) {
					vnd->NumFiles=1;
				}else{
					vnd->NumFiles=ndir1;
				}
			}
		}
	}

	vnd->LenFileNames  = (int *)VNDemalloc((vnd->NumFiles+1)*sizeof(int),
				"VNDopen: allocating LenFileNames");
	vnd->FileNames     = (char **)VNDemalloc((vnd->NumFiles+1)*sizeof(char *),
				"VNDopen: allocating FileNames");
	vnd->FirstDataByte = (long *)VNDemalloc((vnd->NumFiles+1)*sizeof(long),
				"VNDopen: allocating FirstDataByte");
	vnd->LenFile       = (long*)VNDemalloc((vnd->NumFiles+1)*sizeof(long),
				"VNDopen: allocating LenFile");

	if(mode==2){
		for(j=0;j<vnd->NumFiles;j++){
			if(ndir>0){
				(void)sprintf(file1,"%s/%s.VND%d",dir[j],file,j);
			}else if(ndir==0 || ndir1==0) {
				(void)sprintf(file1,"%s.VND%d",file,j);
			}else{
				(void)sprintf(file1,"%s/%s.VND%d",dir1,file,j);
				if(j<(vnd->NumFiles-1)) k=fscanf(fpdir,"%s",dir1);
			}
			vnd->LenFileNames[j]=(int)(1+strlen(file1));
			vnd->FileNames[j]=(char*)VNDemalloc(
				vnd->LenFileNames[j]*sizeof(char),
				"VNDopen: allocating FileNames");
			(void)sprintf(vnd->FileNames[j],"%s",file1);
			vnd->FirstDataByte[j]=0;
			vnd->FirstDataByte[0]+=+vnd->LenFileNames[j];
			vnd->LenFile[j]=0;
		}
		if(ndir1>0) k=fclose(fpdir);
	}
	vnd->CurrentFile      = 0;
	vnd->FileDescriptor   = fp;
	vnd->NumOpenPanels    = noppanels;

/*	allocate necessary space in structure  */
	vnd->CurrentPanel     = (long *)VNDemalloc(noppanels*sizeof(long),
				"VNDopen: allocating CurrentPanel");
	vnd->Modified         = (int *)VNDemalloc(noppanels*sizeof(int),
				"VNDopen: allocating Modified");
	vnd->CurrentDimension = (int *)VNDemalloc(noppanels*sizeof(int),
				"VNDopen: allocating CurrentDimension");
	vnd->CurrentBlock     = (long *)VNDemalloc(noppanels*sizeof(long),
				"VNDopen: allocating CurrentBlock");		

	vnd->N                = (long *)VNDemalloc(vnd->NumDim*sizeof(long),
				"VNDopen: allocating N");
	vnd->NNodesPerBlock   = (long *)VNDemalloc(vnd->NumDim*sizeof(long),
				"VNDopen: allocating NNodesPerBlock");
	vnd->NBlocks          = (long *)VNDemalloc(vnd->NumDim*sizeof(long),
				"VNDopen: allocating NBlocks");
	vnd->ByteIncr         = (long *)VNDemalloc(vnd->NumDim*sizeof(long),
				"VNDopen: allocating ByteIncr");
	vnd->BlockIncr        = (long *)VNDemalloc(vnd->NumDim*sizeof(long),
				"VNDopen: allocating BlockIncr");


	if(mode==0 || mode==1) {
		j=(int) fread(vnd->N,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: read error");
		j=(int) fread(vnd->NNodesPerBlock,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: read error");
		j=(int) fread(vnd->NBlocks,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: read error");
		j=(int) fread(vnd->ByteIncr,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: read error");
		j=(int) fread(vnd->BlockIncr,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: read error");

		j=(int) fread(vnd->LenFileNames,sizeof(long),vnd->NumFiles,fp);
		if(j!=vnd->NumFiles)VNDerr("VNDopen: read error");
		j=(int) fread(vnd->FirstDataByte,sizeof(long),vnd->NumFiles,fp);
		if(j!=vnd->NumFiles)VNDerr("VNDopen: read error");
		j=(int) fread(vnd->LenFile,sizeof(long),vnd->NumFiles,fp);
		if(j!=vnd->NumFiles)VNDerr("VNDopen: read error");

		for(j=0;j<vnd->NumFiles;j++){
			vnd->FileNames[j]=(char *)VNDemalloc(vnd->LenFileNames[j]*sizeof(char),
				"VNDopen: allocating FileNames");
			k=(int) fread(vnd->FileNames[j],sizeof(char),
				vnd->LenFileNames[j],fp);
			if(k!=vnd->LenFileNames[j])
				VNDerr("VNDopen: read error");
		}			
		/* allocate buffer space for VND routines */
		vnd->w = (char *)VNDemalloc(
			vnd->NumBytesMemBuf*noppanels*sizeof(char),
			"VNDopen: allocating w");	
		for(j=0;j<vnd->NumBytesMemBuf;j++) vnd->w[j]=0;
	}else{
		for(j=0;j<vnd->NumDim;j++) vnd->N[j]=N[j];
		j= VNDGetDimensions(vnd, lwmax/noppanels, 1);
		if(mode==3) return(vnd);
		vnd->FirstDataByte[0]+=sizeof(VND)+vnd->NumDim*sizeof(long)*5 +
					vnd->NumFiles*sizeof(long)*2 +
					vnd->NumFiles*sizeof(int);
		lcontrol=vnd->FirstDataByte[0];
		if(vnd->NumDim>1) vnd->FirstDataByte[0]=BytesPerSector*
			(1 +(vnd->FirstDataByte[0]-1)/BytesPerSector);
		for(j=1;j<vnd->NumFiles;j++){
			vnd->FirstDataByte[j]=0;
		}
		vnd->NumBytes1stDim=vnd->NumBytesPerBlock*vnd->NBlocks[0];
		/* Make the number of blocks/file a multiple of the
		number of blocks needed in 1st dimension so can do 
		long reads and writes in 1st dimension.			*/
		vnd->NumBlocksPerFile=(vnd->NumBlocksPerPanel*
				vnd->NumPanels)/vnd->NBlocks[0];		
		vnd->NumBlocksPerFile=1 + (vnd->NumBlocksPerFile-1)/
				vnd->NumFiles;
		vnd->NumBlocksPerFile*=vnd->NBlocks[0];
		/*  write out VND information to first file */
		j=(int) fwrite(vnd,sizeof(VND),1,fp);
		if(j!=1)VNDerr("VNDopen: write error");
		j=(int) fwrite(vnd->N,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: write error");
		j=(int) fwrite(vnd->NNodesPerBlock,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: write error");
		j=(int) fwrite(vnd->NBlocks,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: write error");
		j=(int) fwrite(vnd->ByteIncr,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: write error");
		j=(int) fwrite(vnd->BlockIncr,sizeof(long),vnd->NumDim,fp);
		if(j!=vnd->NumDim)VNDerr("VNDopen: write error");

		j=(int) fwrite(vnd->LenFileNames,sizeof(int),vnd->NumFiles,fp);
		if(j!=vnd->NumFiles)VNDerr("VNDopen: write error");
		j=(int) fwrite(vnd->FirstDataByte,sizeof(long),vnd->NumFiles,fp);
		if(j!=vnd->NumFiles)VNDerr("VNDopen: write error");
		for(j=0;j<vnd->NumFiles;j++){
			vnd->LenFile[j]=vnd->FirstDataByte[j]+
				vnd->NumBlocksPerFile*vnd->NumBytesPerBlock;
		}
		vnd->LenFile[vnd->NumFiles]=vnd->FirstDataByte[vnd->NumFiles]
			+ ( (vnd->NumPanels*vnd->NumBlocksPerPanel) -
			   (vnd->NumFiles*vnd->NumBlocksPerFile) )
			*vnd->NumBytesPerBlock;
		j=(int) fwrite(vnd->LenFile,sizeof(long),vnd->NumFiles,fp);
		if(j!=vnd->NumFiles)VNDerr("VNDopen: write error");
		for(j=0;j<vnd->NumFiles;j++){
			k=(int) fwrite(vnd->FileNames[j],sizeof(char),
				vnd->LenFileNames[j],fp);
		        if(k!=vnd->LenFileNames[j])
				VNDerr("VNDopen: write error");
		}
		/* allocate buffer space for VND routines */
		vnd->w = (char *)VNDemalloc(
			vnd->NumBytesMemBuf*noppanels*sizeof(char),
			"VNDopen: allocating w");	
		for(j=0;j<vnd->NumBytesMemBuf;j++) vnd->w[j]=0;

		lcontrol=vnd->FirstDataByte[0]-lcontrol;
		if(lcontrol>0)
			fwrite(vnd->w,sizeof(char),lcontrol,fp);
		/* initialize the VND files all zero bytes to check disk space*/
		vnd->Mode="w";

		for(j=0;j<vnd->NumOpenPanels;j++){
			vnd->CurrentPanel[j]     = -1;
			vnd->CurrentDimension[j] = -1;
			vnd->CurrentBlock[j]     = -1;
			vnd->Modified[j]         =  0;
		}	
		for(ipanel=0;ipanel<vnd->NumPanels;ipanel++){
			for(iblock=0;iblock<vnd->NumBlocksPerPanel;
				iblock+=vnd->NBlocks[0]){
				j=VNDrwslice(vnd,0,'w',0,iblock,ipanel);
			}
		}
	}
	for(j=0;j<vnd->NumOpenPanels;j++){
		vnd->CurrentPanel[j]     = -1;
		vnd->CurrentDimension[j] = -1;
		vnd->CurrentBlock[j]     = -1;
		vnd->Modified[j]         =  0;
	}	
	j=fclose(fp);

	if(mode==0){vnd->Mode="r";}
	else {vnd->Mode="r+";}

	vnd->FileDescriptor = fopen(vnd->FileNames[0],vnd->Mode);
/***********************
	if(vnd->NumDim>1){
		setbuf(vnd->FileDescriptor,NULL);
	}else{
		iblock = 10*BUFSIZ*(1+(vnd->NumBytesPerBlock-1)/BUFSIZ);
		j=setvbuf(vnd->FileDescriptor,NULL,_IOFBF,iblock);
	}
************************/
	
	/* Set file open mode for rest of processing */

	return  (vnd);
}
int VNDGetDimensions(VND *vnd, long lwmax, int ich)
/******************************************************************
int VNDGetDimensions(VND *vnd, long lwmax, int ich)

	Set dimensions for block matrix transpose 
	in VND control structure.

	Initialize:
		vnd.NumBlocksPerPanel
		vnd.NumBytesMemBuf
		vnd.NumBytesPerBlock
		vnd.NumBytes1stDim
		vnd.NumNodesPerBlock
		vnd.NNodesPerBlock[k]   for k=0 to NumDim-1 
		vnd.NBlocks[k]
		vnd.ByteIncr[k]
		vnd.BlockIncr[k]

	VND *vnd 	VND control structure
			Values required to be set on input
			include vnd.N[k] and vnd.NumBytesPerNode
	long lwmax	number of characters of memory available	
	int ich 	1 if force even number of first dimension
			nodes per block (useful to allow
			real-to-complex ffts and make it
			possible to change number of bytes
			per node at a later point)

	return	-1 if all fits in memory
		0 if successful in partitioning to fit in memory
		1 if failed to partition so fits in memory

*********************************************************************/
{
	long i;
	long j;
	long k;
	long TotalBytesPerPanel;
	long TotalNodesPerPanel;
	long TotalNodesPerBlock;
	long MaxN;
	long ByteIncr;
	long BlockIncr;

	TotalNodesPerPanel=1;
	MaxN=0;
	for(j=0;j<vnd->NumDim;j++){
		TotalNodesPerPanel*=vnd->N[j];
		if(vnd->N[j]>MaxN) MaxN=vnd->N[j];
	}
	TotalBytesPerPanel=TotalNodesPerPanel*vnd->NumBytesPerNode;
	if(vnd->NumDim>1) TotalBytesPerPanel=BytesPerSector*
		(1+(TotalBytesPerPanel-1)/BytesPerSector);

	if(TotalBytesPerPanel<=lwmax || vnd->NumDim==1 ){
		vnd->NumBlocksPerPanel=1;
		vnd->NumBytesMemBuf=TotalBytesPerPanel;
		vnd->NumBytesPerBlock=TotalBytesPerPanel;
		vnd->NumBytes1stDim=TotalBytesPerPanel;
		vnd->NumNodesPerBlock=TotalNodesPerPanel;

		ByteIncr=vnd->NumBytesPerNode;
		BlockIncr=1;
		for(j=0;j<vnd->NumDim;j++){
			vnd->NNodesPerBlock[j]=vnd->N[j];
			vnd->NBlocks[j]=1;
			vnd->ByteIncr[j]=ByteIncr;
			vnd->BlockIncr[j]=BlockIncr;
			ByteIncr*=vnd->NNodesPerBlock[j];
			BlockIncr*=vnd->NBlocks[j];
		}
		return(0);
	}

	for(k=1;k<=MaxN;k++) {
		ByteIncr=vnd->NumBytesPerNode;
		BlockIncr=1;
		TotalNodesPerBlock=1;
		for(j=0;j<vnd->NumDim;j++){
			vnd->NBlocks[j]=k;
			if(k>vnd->N[j])vnd->NBlocks[j]=vnd->N[j]; 
			vnd->NNodesPerBlock[j]=1 + 
				(vnd->N[j]-1)/vnd->NBlocks[j];
			if(j==0 && ich==1) {
				/* Force even number of 1st dim elements per block */
				i=vnd->NNodesPerBlock[j];
				if(i!=(2*(i/2))) i++;
				vnd->NNodesPerBlock[j]=i;
				vnd->NBlocks[j]=1 + (vnd->N[j]-1)/i;
			}
			vnd->ByteIncr[j]=ByteIncr;
			vnd->BlockIncr[j]=BlockIncr;
			ByteIncr*=vnd->NNodesPerBlock[j];
			BlockIncr*=vnd->NBlocks[j];
			TotalNodesPerBlock*=vnd->NNodesPerBlock[j];
		}
		vnd->NumBlocksPerPanel=BlockIncr;
		TotalNodesPerPanel=TotalNodesPerBlock*BlockIncr;
		ByteIncr=BytesPerSector*(1+(ByteIncr-1)/BytesPerSector);
		vnd->NumBytesPerBlock=ByteIncr;
		vnd->NumBytesMemBuf=k*ByteIncr;
		vnd->NumBytes1stDim=vnd->NBlocks[0]*ByteIncr;
		vnd->NumNodesPerBlock=TotalNodesPerBlock;
		if( ByteIncr==BytesPerSector ) return(0);		
		if( (k*ByteIncr<=lwmax) &&
			(ByteIncr<=MaxIOBuf) ) return(0);
	}
	return(-1);
}
int VNDrwslice(VND *vnd,int iopanel, char mode, int idim, 
	long iblock, long ipanel)
/********************************************************************
int VNDrwslice(VND *vnd,int iopanel, char mode, int idim, 
			long iblock, long ipanel)

	Read or write a slice for dimension idim into or from memory

	VND *vnd	pointer to VND control structure
	int iopanel	open panel counter (0 based. Always
			equals 0 if only one panel open at a time.)
	char mode	'r' for read; 'w' for write.
	int idim	Dimension of slice to be read or written.
			(0 based.  First dimension is idim=0.)
	long iblock	Starting block of block matrix transpose slice for dimension
			idim (0 based).
	long ipanel	Panel number (0 based) for desired slice.

	Function returns a value of 0 if successful.

*********************************************************************/
{
	char *w;
	int j;
	int n;
	int n2read;
	long ib;
	w=vnd->w+iopanel*vnd->NumBytesMemBuf;
	ib = ipanel*vnd->NumBlocksPerPanel + iblock;
	if(idim==0 && vnd->NumBytesMemBuf<MaxIOBuf ){
		if(VNDseek(vnd,ib)!=0) {
			fprintf(stderr,"VNDrwslice: seek failed on dim 0 \n");
			fprintf(stderr,
			" mode = %c iopanel = %d idim = %d iblock = %ld ipanel = %ld \n",
			mode,iopanel,idim,iblock,ipanel);
			VNDdump(vnd,stderr);
			abort();
			/*	return(-1); */
		}
		n2read=(int) (vnd->NumBytes1stDim);
		if(mode=='r'){
			n=(int) fread(w,sizeof(char),n2read,vnd->FileDescriptor);
		}else{
			n=(int) fwrite(w,sizeof(char),n2read,vnd->FileDescriptor);
		}
		if(n!=n2read){
			fprintf(stderr,"VNDrwslice: wrong number of values\n");
			if(mode=='r'){
				fprintf(stderr,
				" Read %d bytes out of %d on 1st dim",n,n2read);
			}else{
				fprintf(stderr,
				" Wrote %d bytes out of %d on 1st dim",n,n2read);
			}
			fprintf(stderr,
			" mode = %c iopanel = %d idim = %d iblock = %ld ipanel = %ld \n",
			mode,iopanel,idim,iblock,ipanel);
			VNDdump(vnd,stderr);
			abort();
			/*	return(-2);  */
		}
		return(0);
	}

	n2read=(int) (vnd->NumBytesPerBlock);
	for(j=0;j<vnd->NBlocks[idim];j++){
		if(VNDseek(vnd,ib)!=0) {
			fprintf(stderr,"VNDrwslice: seek failed");
			fprintf(stderr,
			" mode = %c iopanel = %d idim = %d iblock = %ld ipanel = %ld \n",
			mode,iopanel,idim,iblock,ipanel);
			abort();
			/*	return(-1);  */
		}
		if(mode=='r'){
			n=(int) fread(&w[j*n2read],sizeof(char),
				n2read,vnd->FileDescriptor);
		}else{
			n=(int) fwrite(&w[j*n2read],sizeof(char),
				n2read,vnd->FileDescriptor);
		}
		if(n!=n2read){
			fprintf(stderr,"VNDrwslice wrong number of values");
			if(mode=='r'){
				fprintf(stderr,
				" Read %d bytes out of %d on dim %d \n",
				n,n2read,idim);
			}else{
				fprintf(stderr,
				" Wrote %d bytes out of %d on dim %d \n",
				n,n2read,idim);
			}
			fprintf(stderr,
			" mode = %c iopanel = %d idim = %d iblock = %ld ipanel = %ld \n",
			mode,iopanel,idim,iblock,ipanel);
			VNDdump(vnd,stderr);
			abort();
			/*	return(-2);  */
		}
		ib=ib+vnd->BlockIncr[idim];
	}
	return(0);
}
int VNDseek(VND *vnd, long ib)
/****************************************************************
int VNDseek(VND *vnd, long ib)

	Seek the desired block in VND file structure.
	(a) Go to the correct physical file, reopen if necessary
	(b) Seek the correct starting byte within the file

	VND *vnd	pointer to VND control structure
	long ib		desired block number 

	The function returns 0 if successful, Nonzero if fail.

*****************************************************************/
{
	int ifile;

	ifile = (int) (ib/vnd->NumBlocksPerFile);
	if(vnd->CurrentFile==-1){
		vnd->FileDescriptor = fopen(vnd->FileNames[ifile],vnd->Mode);
		if(vnd->FileDescriptor==NULL) {
			fprintf(stderr,"fopen failed in VNDseek");
			return(-1);
		}
		vnd->CurrentFile=ifile;
	}else{
		if(ifile!=vnd->CurrentFile){
			(void) fflush(vnd->FileDescriptor);
			(void) fclose(vnd->FileDescriptor);
			vnd->FileDescriptor = freopen(vnd->FileNames[ifile],
					vnd->Mode,vnd->FileDescriptor);
			if(vnd->FileDescriptor==NULL) {
				fprintf(stderr,"freopen failed in VNDseek");
				return(-1);
			}
			vnd->CurrentFile=ifile;
		}
	}
	ib = ib - ifile*vnd->NumBlocksPerFile;
	ib = ib*vnd->NumBytesPerBlock+vnd->FirstDataByte[ifile];
	ib = fseeko(vnd->FileDescriptor,(off_t) ib,SEEK_SET);
	if(ib!=0){
		fprintf(stderr,"fseeko failed in VNDseek");
		return(-2);
	}
	return(0);
}
void VNDflush(VND *vnd)
/*********************************************************************
void VNDflush(VND *vnd)

	Flush out VND buffers to file

	VND *vnd	VND control structure

**********************************************************************/
{
	int j,k;
	for(j=0;j<vnd->NumOpenPanels;j++){
		if(vnd->Modified[j]==1){
			k=VNDrwslice(vnd,j,'w', 
				vnd->CurrentDimension[j], 
				vnd->CurrentBlock[j], 
				vnd->CurrentPanel[j]);
			if(k!=0){
				fprintf(stderr,
				"VNDflush: Error writing out old buffers");
				fprintf(stderr,
				"VNDflush: writing slice: idim=%d iblock=%ld ipanel=%ld\n",
				vnd->CurrentDimension[j], 
				vnd->CurrentBlock[j],
				vnd->CurrentPanel[j]);
				abort();
			}
		}
	}
	return;
}
void VNDrw(char iop, int iopanel,VND *vnd, int idim, 
		long *key, long ipanel, char *values, 
		long start, long incr, long nvalues,
		int msgnum, char *msg)
/********************************************************************

void VNDrw(char iop, int iopanel, VND *vnd, int idim, long *key, long ipanel, 
		char *values, long start, long incr, long nvalues,
		int msgnum, char *msg)

	Read or write a vector for dimension "idim" 

	char iop	'r' for read; 'w' for write
	int iopanel	Index of current open panel buffer 
			(Ranges from 0 to noppanels-1.  Used to
			allow access to multiple panels simultaneously.
			For most applications, noppanels = 1 and 
			iopanel will always equal 0.)	
	VND *vnd	Structure holding block matrix file information.
	int idim	Current dimension for read or write
			(idim=0 is first dimension)
	long key[]	List of node positions for each dimension.
			(Value for dimension "idim" needs to be there
			but it is irrelevant.) 
	long ipanel	Panel number for read or write.
			(ipanel=0 is first dimension)
	char values[]	Returns as values read or input as values to be 
			written.
	long start	starting node (0 based)
	long incr	node increment (usually 1)
	long nvalues	number of values to read or write 
			(usually same as number of values in 
			the specified dimension)
	int msgnum	A message number to be written out if error occurs.
	char *msg	A message to be written out if error occurs.

*********************************************************************/
{
	int j;
	int jj;
	int jjj;
	int k;
	int incore;
	long ib;
	long iblock;
	long ik;
	long ibyte;
	long iv;
	long iw;
	long kk;
	long NumNodes;
	long NBytes;
	long NBytesM;

	/* Find desired starting block and byte offset within block */
	iblock=0;
	ibyte=0;
	k=0;
	if(ipanel<0 || ipanel>(vnd->NumPanels-1)){
		fprintf(stderr,"VNDrw: ipanel is out of range \n");
		fprintf(stderr,"VNDrw: Message # %d %s \n",msgnum,msg);
		fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
			iopanel,idim,ipanel);
		for(jj=0;jj<vnd->NumDim;jj++){
			fprintf(stderr,"      Dimension = %d  key = %ld \n",
				jj,key[jj]);
		}
		abort();
	}

	if(vnd->NumDim==1 && start==0 && incr==1 && nvalues==vnd->N[0]){
		j=VNDseek(vnd,ipanel);
		if(j!=0){
			fprintf(stderr,"VNDrw: Seek error on trace %ld \n",
				ipanel);
			fprintf(stderr,
				"VNDrw: Message # %d %s \n",msgnum,msg);
			abort();
		}
		if(iop=='r') {
			j=(int) fread(&values[0],vnd->NumBytesPerNode,
				vnd->N[0],vnd->FileDescriptor);
		}else if(iop=='w'){
			 j=(int) fwrite(&values[0],vnd->NumBytesPerNode,
				vnd->N[0],vnd->FileDescriptor);
		}
		if(j!=vnd->N[0]){
			fprintf(stderr,"VNDrw: I/O error on trace %ld \n",
				ipanel);
			fprintf(stderr,
			"       Only read/wrote %d values out of %ld \n",
				j,vnd->N[0]);
			fprintf(stderr,
				"VNDrw: Message # %d %s \n",msgnum,msg);
			abort();
		}
		return;
	}

	if(iopanel<0 || iopanel>(vnd->NumOpenPanels-1)){
		fprintf(stderr,"VNDWR: iopanel is out of range \n");
		fprintf(stderr,"VNDrw: Message # %d %s \n",msgnum,msg);
		fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
			iopanel,idim,ipanel);
		for(jj=0;jj<vnd->NumDim;jj++){
			fprintf(stderr,"      Dimension = %d  key = %ld \n",
				jj,key[jj]);
		}
		abort();
	}

	for(j=0;j<vnd->NumDim;j++){
		if(j!=idim){
			if(key[j]<0 || key[j]>vnd->N[j]){
				fprintf(stderr,
				"VNDrw: key of %ld for dimension %d is out of range \n",
				key[j],j);
				fprintf(stderr,
				"VNDrw: Message # %d %s \n",msgnum,msg);
				fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
				iopanel,idim,ipanel);
				for(jj=0;jj<vnd->NumDim;jj++){
					fprintf(stderr,"      Dimension = %d  key = %ld \n",
					jj,key[jj]);
				}
				abort();
			}
			ib = key[j]/vnd->NNodesPerBlock[j];
			ik = key[j]-ib*vnd->NNodesPerBlock[j];
			iblock=iblock+ib*vnd->BlockIncr[j];
			ibyte =ibyte+ik*vnd->ByteIncr[j];
			k++;
		}
	}

	ibyte=ibyte+vnd->NumBytesMemBuf*iopanel;

/*********************************************************************** 
	Check to see if desired data already in memory.
	If not, 
	(a) write out current data in memory if it has been modified.
	(b) read in desired blocks
********************************************************************** */
	incore=1;
	if(ipanel!=vnd->CurrentPanel[iopanel]) incore=0;
	if(vnd->NumBlocksPerPanel>1){
		if(idim!=vnd->CurrentDimension[iopanel]) incore=0; 
		if(iblock!=vnd->CurrentBlock[iopanel]) incore=0;
	}
	if(incore==0){
		if(vnd->Modified[iopanel]==1){
/*
			fprintf(stdout,"writing slice: idim=%d iblock=%ld \n",
				vnd->CurrentDimension[iopanel], 
				vnd->CurrentBlock[iopanel]);
*/
			j=VNDrwslice(vnd,iopanel,'w', 
				vnd->CurrentDimension[iopanel], 
				vnd->CurrentBlock[iopanel], 
				vnd->CurrentPanel[iopanel]);
/*			VNDlook((int *)vnd->w,vnd->NumBytesMemBuf/sizeof(int) ); */
			if(j!=0){
				fprintf(stderr,"VNDrw: Error writing out old buffers");
				fprintf(stderr,
				"VNDrw: Message # %d %s \n",msgnum,msg);
				fprintf(stderr,
				"VNDrw: iop = %c byte = %ld iblock = %ld panel = %ld \n",
				iop,ibyte, iblock, ipanel);
				fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
				iopanel,idim,ipanel);
				for(jj=0;jj<vnd->NumDim;jj++){
					fprintf(stderr,"      Dimension = %d  key = %ld \n",
					jj,key[jj]);
				}
				abort();
			}
		}
	 	j=VNDrwslice(vnd, iopanel,'r', idim, iblock, ipanel);

		if(j!=0){
			fprintf(stderr,"VNDrw: Error reading in new buffers\n");
			fprintf(stderr,
			"VNDrw: Message # %d %s \n",msgnum,msg);
			fprintf(stderr,
			"VNDrw: iop = %c byte = %ld iblock = %ld panel = %ld \n",
			iop,ibyte, iblock, ipanel);	
			fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
			iopanel,idim,ipanel);
			for(jj=0;jj<vnd->NumDim;jj++){
				fprintf(stderr,"      Dimension = %d  key = %ld \n",
				jj,key[jj]);
			}
			abort();
		}
		vnd->CurrentDimension[iopanel] = idim; 
		vnd->CurrentBlock[iopanel]     = iblock; 
		vnd->CurrentPanel[iopanel]     = ipanel;
		vnd->Modified[iopanel]         = 0;
	}

	if(iop=='r') { /* copy desired data into read buffer */

		if(start==0 && incr==1 && nvalues==vnd->N[idim]){
			if(idim==0){
				NBytes = vnd->NumBytesPerNode * 
					vnd->NNodesPerBlock[0];
				NBytesM= vnd->NumBytesPerNode * vnd->N[0];
				jj=0;
				k=0;
				for(ib=0;ib<vnd->NBlocks[0];ib++){
					NBytes=MIN(NBytes,NBytesM-jj);
					for(j=0;j<NBytes;j++){
						values[k]=vnd->w[ibyte+j];
						k++;
					}
					ibyte+=vnd->NumBytesPerBlock;
					jj+=vnd->NNodesPerBlock[0]*
						vnd->NumBytesPerNode;
				}
			}else{
				jj=0;
				k=0;
				for(ib=0;ib<vnd->NBlocks[idim];ib++){
					NumNodes= MIN( vnd->N[idim]-jj,	
						vnd->NNodesPerBlock[idim]);
					jjj=(int) ibyte;
					for(j=0;j<NumNodes;j++){
						for(kk=0;kk<vnd->NumBytesPerNode;kk++){
							values[k]=vnd->w[jjj+kk];
							k++;
						}
						jjj+=vnd->ByteIncr[idim];
					}
					ibyte+=vnd->NumBytesPerBlock;
					jj+=vnd->NNodesPerBlock[idim];
				}
			}
		}else{
			iv=0;
			for(j=0;j<nvalues;j++){
				k=(int) (start + j*incr);
				if( k<0 || k>=vnd->N[idim] ){
					fprintf(stderr,"VNDrw: index out of range on read\n");
					fprintf(stderr,
						"VNDrw: Message # %d %s \n",msgnum,msg);
					fprintf(stderr,"VNDrw: dimension (0 based for C, add 1 for Fortran) = %d\n",idim);
					fprintf(stderr,"VNDrw: number of nodes = %ld\n",vnd->N[idim]);
					fprintf(stderr,"VNDrw: index (0 based for C, add 1 for Fortran) = %d\n",k);
					fprintf(stderr,
						"VNDrw: erroneous values for start, incr, or nvalues might cause this error\n");
					fprintf(stderr,"VNDrw: start (0 based for C, add 1 for Fortran) = %ld\n",start);
					fprintf(stderr,"VNDrw: incr (0 based for C, add 1 for Fortran) = %ld\n",incr);
					fprintf(stderr,"VNDrw: nvalues = %ld\n",nvalues);
					fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
						iopanel,idim,ipanel);
					for(jj=0;jj<vnd->NumDim;jj++){
						fprintf(stderr,"      Dimension = %d  key = %ld \n",
						jj,key[jj]);
					}
					abort();
				}
				ib = k/vnd->NNodesPerBlock[idim];
				iw = k - ib*vnd->NNodesPerBlock[idim];
				iw = ibyte + iw*vnd->ByteIncr[idim] +
				ib*vnd->NumBytesPerBlock;
				for(kk=0;kk<vnd->NumBytesPerNode;kk++){
					values[iv]=vnd->w[iw];
					iw++;
					iv++;
				}
			}
		}
		return;
	}

	/* copy desired data into write buffer */
	if(iop=='w') { /* copy desired data into read buffer */
		vnd->Modified[iopanel]=1;
		if(start==0 && incr==1 && nvalues==vnd->N[idim]){
			if(idim==0){
				NBytes = vnd->NumBytesPerNode * 
					vnd->NNodesPerBlock[0];
				NBytesM= vnd->NumBytesPerNode * vnd->N[0];
				jj=0;
				k=0;
				for(ib=0;ib<vnd->NBlocks[0];ib++){
					NBytes=MIN(NBytes,NBytesM-jj);
					for(j=0;j<NBytes;j++){
						vnd->w[ibyte+j]=values[k];
						k++;
					}
					ibyte+=vnd->NumBytesPerBlock;
					jj+=vnd->NNodesPerBlock[0]*
					vnd->NumBytesPerNode;
				}
			}else{
				jj=0;
				k=0;
				for(ib=0;ib<vnd->NBlocks[idim];ib++){
					NumNodes= MIN( vnd->N[idim]-jj,	
					vnd->NNodesPerBlock[idim]);
					jjj=(int) ibyte;
					for(j=0;j<NumNodes;j++){
						for(kk=0;kk<vnd->NumBytesPerNode;kk++){
							vnd->w[jjj+kk]=values[k];
							k++;
						}
						jjj+=vnd->ByteIncr[idim];
					}
					ibyte+=vnd->NumBytesPerBlock;
					jj+=vnd->NNodesPerBlock[idim];
				}
			}
		}else{
			iv=0;
			for(j=0;j<nvalues;j++){
				k=(int)(start + j*incr);
				if( k<0 || k>=vnd->N[idim] ){
					fprintf(stderr,"VNDrw: index out of range on write\n");
					fprintf(stderr,
					"VNDrw: Message # %d %s \n",msgnum,msg);
					fprintf(stderr,"VNDrw: dimension (0 based for C, add 1 for Fortran) = %d\n",idim);
					fprintf(stderr,"VNDrw: number of nodes = %ld\n",vnd->N[idim]);
					fprintf(stderr,"VNDrw: index (0 based for C, add 1 for Fortran) = %d\n",k);
					fprintf(stderr,
					"VNDrw: erroneous values for start, incr, or nvalues might cause this error\n");
					fprintf(stderr,"VNDrw: start (0 based for C, add 1 for Fortran) = %ld\n",start);
					fprintf(stderr,"VNDrw: incr (0 based for C, add 1 for Fortran) = %ld\n",incr);
					fprintf(stderr,"VNDrw: nvalues = %ld\n",nvalues);
					fprintf(stderr,"VNDrw: iopanel = %d idim = %d ipanel = %ld\n",
						iopanel,idim,ipanel);
					for(jj=0;jj<vnd->NumDim;jj++){
						fprintf(stderr,"      Dimension = %d  key = %ld \n",
						jj,key[jj]);
					}
					abort();
				}
				ib = k/vnd->NNodesPerBlock[idim];
				iw = k - ib*vnd->NNodesPerBlock[idim];
				iw = ibyte + iw*vnd->ByteIncr[idim] +
					ib*vnd->NumBytesPerBlock;
				for(kk=0;kk<vnd->NumBytesPerNode;kk++){
					vnd->w[iw]=values[iv];
					iw++;
					iv++;
				}
			}

		}
		return;
	}

	fprintf(stderr,"VNDrw: Invalid option.  Not  r  or  w   \n");
	abort();
}
void VNDcl(VND *vnd, int mode)
/*********************************************************************

void VNDcl(VND *vnd, int mode)

	Close VND file structure.  Flush I/O buffers.  Free memory.

	VND *vnd	Structure holding VND block matrix file information.
	int mode	0 means keep 
			1 means delete

***********************************************************************/
{
	int iopanel;
	int ifile;
	for(iopanel=0;iopanel<vnd->NumOpenPanels;iopanel++){
		if(vnd->Modified[iopanel]==1) {
			VNDrwslice(vnd,iopanel,'w', 
			vnd->CurrentDimension[iopanel], 
			vnd->CurrentBlock[iopanel], 
			vnd->CurrentPanel[iopanel]);
		}
	}
	fclose(vnd->FileDescriptor);
	if(mode==1){
		for(ifile=0;ifile<vnd->NumFiles;ifile++){
			remove(vnd->FileNames[ifile]);
			VNDfree(vnd->FileNames[ifile],"VNDcl: freeing vnd->FileNames[ifile]");
		}
	}
	VNDfree(vnd->CurrentPanel,"VNDcl: freeing vnd->CurrentPanel");
	VNDfree(vnd->Modified,"VNDcl: freeing vnd->Modified");
	VNDfree(vnd->CurrentDimension,"VNDcl: freeing vnd->CurrentDimension");
	VNDfree(vnd->CurrentBlock,"VNDcl: freeing vnd->CurrentBlock");
	VNDfree(vnd->N,"VNDcl: freeing vnd->N");
	VNDfree(vnd->NNodesPerBlock,"VNDcl: freeing vnd->NNodesPerBlock");
	VNDfree(vnd->NBlocks,"VNDcl: freeing vnd->NBlocks");
	VNDfree(vnd->ByteIncr,"VNDcl: freeing vnd->ByteIncr");
	VNDfree(vnd->BlockIncr,"VNDcl: freeing vnd->BlockIncr");
	VNDfree(vnd->LenFileNames,"VNDcl: freeing vnd->LenFileNames");
	VNDfree(vnd->FileNames,"VNDcl: freeing vnd->FileNames");
	VNDfree(vnd->FirstDataByte,"VNDcl: freeing vnd->FirstDataByte");
	VNDfree(vnd->LenFile,"VNDcl: freeing vnd->LenFile");
	VNDfree(vnd->w,"VNDcl: freeing vnd->w");
	VNDfree(vnd,"VNDcl: freeing vnd");
	return;
}
void VNDdump(VND *vnd,FILE *fp)
/*****************************************************************
void VNDdump(VND *vnd,FILE *fp)

	Dump out values stored in VND structure to file.

	VND *vnd	pointer to VND structure
	FILE *fp	FILE pointer associated with output file

******************************************************************/
{
	int j;
	fprintf(fp,"\nDump of VND structure\n");
	fprintf(fp,"NumDim = %ld\n",vnd->NumDim);
	fprintf(fp,"NumBytesPerNode = %ld\n",vnd->NumBytesPerNode);
	fprintf(fp,"NumBlocksPerPanel = %ld\n",vnd->NumBlocksPerPanel);
	fprintf(fp,"NumFiles = %ld\n",vnd->NumFiles);
	fprintf(fp,"NumPanels = %ld\n",vnd->NumPanels);
	fprintf(fp,"NumOpenPanels = %ld\n",vnd->NumOpenPanels);
	fprintf(fp,"NumBytesMemBuf = %ld\n",vnd->NumBytesMemBuf);
	fprintf(fp,"NumBytesPerBlock = %ld\n",vnd->NumBytesPerBlock);
	fprintf(fp,"NumBytes1stDim = %ld\n",vnd->NumBytes1stDim);
	fprintf(fp,"NumNodesPerBlock = %ld\n",vnd->NumNodesPerBlock);
	fprintf(fp,"NumBlocksPerFile = %ld\n",vnd->NumBlocksPerFile);
	fprintf(fp,"CurrentFile = %d\n",vnd->CurrentFile);
	fprintf(fp,"Mode = %s\n",vnd->Mode);
	fprintf(fp,"FileDescriptor will not be dumped\n");
	fprintf(fp,"\nFor each block matrix transpose dimension:\n");
	for(j=0;j<vnd->NumDim;j++){
		fprintf(fp,"Dimension = %d N = %ld NNodesPerBlock = %ld",
		j,vnd->N[j],vnd->NNodesPerBlock[j]);
		fprintf(fp," NBlocks = %ld ByteIncr = %ld BlockIncr = %ld\n",
		vnd->NBlocks[j],vnd->ByteIncr[j],vnd->BlockIncr[j]);
	}
	fprintf(fp,"\nFor each simultaneously open panel buffer:\n");
	for(j=0;j<vnd->NumOpenPanels;j++){
		fprintf(fp,"Open panel buffer = %d Current Panel = %ld",
		j,vnd->CurrentPanel[j]);	
		fprintf(fp," CurrentDimension = %d CurrentBlock = %ld",
		vnd->CurrentDimension[j],vnd->CurrentBlock[j]);
		fprintf(fp," Modified = %d \n",vnd->Modified[j]);
	}
	fprintf(fp,"\nFor each VND file:\n");
	for(j=0;j<vnd->NumFiles;j++){
		fprintf(fp,"File name = %s\n",vnd->FileNames[j]);
		fprintf(fp,"   LenFileNames = %d FirstDataByte = %ld",
		vnd->LenFileNames[j],vnd->FirstDataByte[j]);
		fprintf(fp," LenFile = %ld \n",vnd->LenFile[j]);
	}
	fprintf(fp,"\n \n");
	return;
}
void VNDlook(int *w, int lw)
{
	int j;
	for(j=0;j<lw;j++){
		fprintf(stdout," j=%d w[j]=%d \n",j,w[j]);		
	}
	return;
}
void *VNDemalloc(size_t n, char *s){
	void *p,*pp;
	long *j;
	n=1+(n-1)/sizeof(long);
	if((p = malloc((n+3)*sizeof(long)))==NULL) {
		fprintf(stderr,"Error allocating memory in VNDemalloc\n");
		fprintf(stderr,"%s\n",s);
		abort();
		/*	return (NULL);  */
	}
	j=(long *)p;
	j[0]=6789;
	j[1]=(int) n;
	pp = &j[2];
	j[n+2]=1234;
	total_VND_mem+=(n+3)*sizeof(long);
	return (pp);
}
void VNDmemchk( void *p , char * msg) 
{
	long *j;
	j = (long *)p;
	j = j-2;
	if(j[0]!=6789) {
		fprintf(stderr,"Beginning memory overrun detected by VNDmemchk\n");
		fprintf(stderr,"%s\n",msg);
		abort();
	}
	if(j[ j[1]+2 ]!=1234) {
		fprintf(stderr,"Ending memory overrun detected by VNDmemchk\n");
		fprintf(stderr,"%s\n",msg);
		abort();
	}	
}
long VNDtotalmem(void)
{
	return (total_VND_mem);
}
void VNDfree( void *p, char *msg)
{
	void *pp;
	long *j;
	j = (long *)p;
	j=j-2;
	pp = (void *)j;
	VNDmemchk(p,msg);
	total_VND_mem= total_VND_mem-((int) ((j[1]+3)*sizeof(long)) );
	free(pp);
}
void VNDerr(char *s)
{
	fprintf(stderr,"VNDerr: fatal error\n");
	fprintf(stderr,"%s\n",s);
	abort();
   /*	return;  */
}
void VNDr2c(VND *vnd)
{
	/* change vnd dimension 0 from real to complex */
	vnd->NumBytesPerNode*=2;
	vnd->N[0]/=2;
	vnd->NNodesPerBlock[0]/=2;
	vnd->ByteIncr[0]*=2;
	return;
}
void VNDc2r(VND *vnd)
{
	/* change vnd dimension 0 from complex to real */
	vnd->NumBytesPerNode/=2;
	vnd->N[0]*=2;
	vnd->NNodesPerBlock[0]*=2;
	vnd->ByteIncr[0]/=2;
	return;
}
VND * V2Dop(int mode, long lwmax, long nbytes, char *file,long n0, long n1)
{
	long N[2];
	char **dir=NULL;
	N[0]=n0;
	N[1]=n1;
	return(VNDop(mode,lwmax,2,N,1,nbytes,file,0,dir,1));	
}
void V2Dr0(VND *vnd,long key,char *buf,int msgnum)
{
	long k[2];
	k[0]=0;
	k[1]=key;
	VNDrw('r',0,vnd,0,k,0,buf,0,1,vnd->N[0],msgnum,"v2dr0");
	return;
}
void V2Dr1(VND *vnd,long key,char *buf,int msgnum)
{
	long k[2];
	k[1]=0;
	k[0]=key;
	VNDrw('r',0,vnd,1,k,0,buf,0,1,vnd->N[1],msgnum,"v2dr1");
	return;
}
void V2Dw0(VND *vnd,long key,char *buf,int msgnum)
{
	long k[2];
	k[0]=0;
	k[1]=key;
	VNDrw('w',0,vnd,0,k,0,buf,0,1,vnd->N[0],msgnum,"v2dw0");
	return;
}
void V2Dw1(VND *vnd,long key,char *buf,int msgnum)
{
	long k[2];
	k[1]=0;
	k[0]=key;
	VNDrw('w',0,vnd,1,k,0,buf,0,1,vnd->N[1],msgnum,"v2dw1");
	return;
}
/*
This is the Fortran user interface to the VND routines.


VNDOP(vnd, mode, lwmax, ndim, N, npanels, 
		nbytes, file, ndir, noppanels)
	
	Open block matrix file structure.

	int vnd		returns as pointer to filled out I/O 
			control structure 
	integer mode	0 for read only
			1 for read and write (existing file)
			2 for write and read (create new file)
			3 for don't open, just compute buffer sizes 
			and fill out structure
	integer lwmax	Maximum memory buffer size in nodes to use
	integer ndim	Number of block matrix transpose dimensions
	integer N[ndim]	List of number of nodes for each dimension
	integer npanels	Number of panels of block matrix structure
	integer nbytes	Number of bytes per node
	character*80 file File name from which other file names will be 
			constructed.  The actual files used will have
			.VND0, .VND1, ... extentions.  This name
			should not have such an extention.
	integer ndir	Number of file directories available for holding data.
			Set ndir=0 to use one file in current directory.
			Set ndir=-1 to look at ~/.VND file for directories
			to use.  File format is "number of directories"
			on first line and then one line per directory name.
	int noppanels	Max number of panels to be open at one time


VNDRW(iop, iopanel, vnd, idim, key, ipanel, 
		values, start, incr, nvalues, msgnum)

	Read or write a vector for dimension "idim" 

	character*1 iop	"r" for read; "w" for write
	integer iopanel	Index of current open panel buffer 
			(Ranges from 1 to noppanels.  Used to
			allow access to multiple panels simultaneously.
			For most applications, noppanels = 1 and 
			iopanel will always equal 1.)	
	integer vnd	Structure holding block matrix file information.
	integer idim	Current dimension for read or write
			(idim=1 for first dimension.)
	integer key[]	List of node positions for each dimension 
			(Value for dimension "idim" needs to be there
			but it's value is irrelevant.)
	integer ipanel	Panel number for read or write.
			(First panel is panel 1.)
	integer values[]Returns as values read or input as values to be 
			written.
	integer start	starting node (data starts at 1, usually equals 1)
	integer incr	node increment (usually 1)
	integer nvalues	number of values to read or write 
			(usually same as number of values in 
			the specified dimension)
	integer msgnum	A message number to be written out if error occurs.

VNDCL(vnd, mode)

	Close VND file structure.

	integer vnd	Structure holding VND block matrix file information.
	integer mode	0 means keep 
			1 means delete


VNDFLUSH(vnd)

	Flush out VND buffers to file.  

	This may be important in applications where 
	checkpoint/restart capabilities are needed.  
	After VNDflush, the data written by the VNDRW
	routine are safely stored on disk rather than 
	just in a memory buffer where it could be lost in
	the event of a machine failure.

	integer vnd	VND control structure


*/
#ifdef CRAY
void vndop_( VND **vnd, FORTINT *fort_mode, FORTINT *fort_lwmax, 
	FORTINT *fort_ndim, FORTINT *fort_N, FORTINT *fort_npanels,  
	FORTINT *fort_nbytes, char *fort_file,
	FORTINT *fort_ndir, char *fort_dir, FORTINT *fort_noppanels)
#else
void vndop_( VND **vnd, FORTINT *fort_mode, FORTINT *fort_lwmax, 
	FORTINT *fort_ndim, FORTINT *fort_N, FORTINT *fort_npanels,  
	FORTINT *fort_nbytes, char *fort_file,
	FORTINT *fort_ndir, char *fort_dir, FORTINT *fort_noppanels, 
	int lfile, int ldir)
#endif
{
#ifdef CRAY
	int j;
#else
	int j = (int) (ldir+lfile);
#endif
	int k;
	int mode;
	long lwmax;
	int ndim;
	long *N;
	long npanels;
	long nbytes;
	char file[81];
	int ndir;
	char **dir=NULL;
	int noppanels;

	mode=(int) fort_mode[0];
	lwmax=(long) fort_lwmax[0];
	ndim=(int) fort_ndim[0];
	npanels=(long) fort_npanels[0];
	nbytes=(long) fort_nbytes[0];
	ndir=(int) fort_ndir[0];
	noppanels=(int) fort_noppanels[0];
	N = (long *)VNDemalloc(ndim*sizeof(long),"vndop_ allocating N");
	for(j=0;j<ndim;j++)
		{
		N[j]=fort_N[j];
		}
	for(j=0;j<80;j++)
		{
		if(isgraph(fort_file[j])==0) break;
		file[j]=fort_file[j];
		}
	file[j]='\0';
	if(ndir>0)
		{
		dir=(char **)VNDemalloc(ndir*sizeof(char *),
			"vndopen_: allocating char pointers\n");
		for(k=0;k<ndir;k++)
			{
			dir[k]=(char *)VNDemalloc(81*sizeof(char),
			"vndopen_: allocating string for directory\n");
			for(j=0;j<80;j++)
				{
				if(isgraph(fort_dir[k*80+j])==0) break;
				dir[k][j]=fort_dir[k*80+j];
				}
			dir[k][j]='\0';
			}
		}
	*vnd=VNDop(mode, lwmax, ndim, N, npanels, 
		nbytes, file, ndir, dir, noppanels);
	for(k=0;k<ndir;k++) VNDfree(dir[k],"vndop_: freeing dir[k]");
	VNDfree(dir,"vndop_: freeing dir");
	VNDfree(N,"vndop_: freeing N");
	return;
}
#ifdef CRAY
void vndrw_(char *fort_iop, FORTINT *fort_iopanel,VND **vnd,
	FORTINT *fort_idim, FORTINT *fort_key, FORTINT *fort_ipanel,
	void *values, FORTINT *fort_start, FORTINT *fort_incr,
	FORTINT *fort_nvalues, FORTINT *fort_msgnum)
#else
void vndrw_(char *fort_iop, FORTINT *fort_iopanel,VND **vnd,
	FORTINT *fort_idim, FORTINT *fort_key, FORTINT *fort_ipanel,
	void *values, FORTINT *fort_start, FORTINT *fort_incr,
	FORTINT *fort_nvalues, FORTINT *fort_msgnum, long liop)
#endif
{
#ifdef CRAY
	int j;
#else
	int j = (int) liop; /* dummy */
#endif
	char iop=tolower( (int) fort_iop[0]);
	int iopanel=(int) (fort_iopanel[0]-1);
	int idim=(int)(fort_idim[0]-1);
	long key[20];	
	long ipanel=fort_ipanel[0]-1;
	long start=fort_start[0]-1;
	long incr=fort_incr[0];
	long nvalues=fort_nvalues[0];
	int msgnum = (int) fort_msgnum[0];
	char msg[1];
	msg[0]='\0';
	for(j=0;j<vnd[0]->NumDim;j++)
		{
		key[j]=fort_key[j]-1;
		}
	VNDrw(iop,iopanel,vnd[0],idim,key,ipanel,values,
		start,incr,nvalues,msgnum,msg);
	return;
}
void vndcl_(VND **vnd, FORTINT *fort_mode)
{
	int mode=(int) fort_mode[0];
	VNDcl(vnd[0],mode);
	return;
}
void vndflush_(VND **vnd)
{
	VNDflush(vnd[0]);
	return;
}
void vndr2c_(VND *vnd)
{
	VNDr2c(vnd);
	/* change vnd dimension 0 from real to complex */
	return;
}
void vndc2r_(VND *vnd)
{
	/* change vnd dimension 0 from complex to real */
	VNDc2r(vnd);
	return;
}
void V2Dr1_(VND *vnd,FORTINT key,void *buf,FORTINT msgnum)
{
	long k[2];
	int msg;
	msg=(int) msgnum;
	k[0]=0;
	k[1]=key-1;
	VNDrw('r',0,vnd,0,k,0,(char *)buf,0,1,vnd->N[0],msg,"V2DR1");
	return;
}
void v2dr2_(VND *vnd,FORTINT key,void *buf,FORTINT msgnum)
{
	long k[2];
	int msg;
	msg= (int) msgnum;
	k[1]=0;
	k[0]=key-1;
	VNDrw('r',0,vnd,1,k,0,(char *)buf,0,1,vnd->N[1],msg,"V2DR2");
	return;
}
void v2dw1_(VND *vnd,FORTINT key,void *buf,FORTINT msgnum)
{
	long k[2];
	int msg;
	msg= (int) msgnum;
	k[0]=0;
	k[1]=key-1;
	VNDrw('w',0,vnd,0,k,0,(char *)buf,0,1,vnd->N[0],msg,"V2DW1");
	return;
}
void v2dw2_(VND *vnd,FORTINT key,void *buf,FORTINT msgnum)
{
	long k[2];
	int msg;
	msg= (int) msgnum;
	k[1]=0;
	k[0]=key-1;
	VNDrw('w',0,vnd,1,k,0,(char *)buf,0,1,vnd->N[1],msg,"V2DW2");
	return;
}

char *VNDtempname(char *root)
/* 
	returns a unique temporary file name
*/
{
	static int count=0;
	char *file;
	count++;
	file=(char *)VNDemalloc(80*sizeof(char),"VNDtempname");
	sprintf(file,"%s%d%d",root,getpid(),count);
	return (file);
}

