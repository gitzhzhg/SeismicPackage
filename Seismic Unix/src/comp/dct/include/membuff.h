/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */



#ifndef _BUFFER_H
#define _BUFFER_H

/* buffer used for in memory operation */
typedef struct memBuffStruct{
        int mbound;
        int pos;
        unsigned char *code;
     } memBUFF;

#define MEM_EOB -1

/* define the buffering operation as macros to reduce the 
   overhead of function calls */

/* buffPutc(buff, c)	
	put a character _c_ to the buffer _buff_,
	returns 0 if successful, _MEM_EOB_ otherwise	

   buffWrite(buff, size, nitem, ptr)
	write _nitem_ elements of size _size_ in array 
	_ptr_ to the buffer _buff_, 
	returns _nitem_if successful,  _MEM_EOB_  otherwise
	
   buffGetc(buff, c) 
	get a character _c_ from the buffer _buff_,
	returns 0 if successful, _MEM_EOB_ otherwise	

   buffRead(buff, size, nitem, ptr)
	read _nitem_ elements of size _size_ from the buffer _buff_, 
	and store them in array _ptr_ 
	returns _nitem_if successful,  _MEM_EOB_  otherwise

   buffRewind(buff)
	rewind the buffer _buff_ to its starting position 

   buffMerge(buff1, buff2)
	append buffer _buff2_ starting from the begining up to the current 
	position to buffer _buff1_ and reset the pointer of _buff2_ to the
	begining, returns 0 if successful, _MEM_EOB otherwise

   buffPart(buff1, buff2, nitem)
	partition buffer _buff2_ and put _nitem_ elements starting from
	the current position to buffer _buff1_, returns 0 if successful,
	_MEM_EOB_ otherwise
*/	


/* Thanks for Dave Hale for the "," opertor, so that type conversion 
   from char to int is avoided	*/

#define buffPutc(buff, c)\
	(((buff)->pos == (buff)->mbound)? MEM_EOB :\
	((buff)->code[(buff)->pos ++] = (c), 0))

#define buffGetc(buff, c)\
	(((buff)->pos == (buff)->mbound)? MEM_EOB :\
	((c) = (buff)->code[(buff)->pos ++], 0))
  
/* The implementations here
   uses three multiplications, which might not be as efficient as 
   defining a function 
*/


#define buffWrite(buff, size, nitem, ptr)\
	(((buff)->pos + (size)*(nitem) > (buff)->mbound)? MEM_EOB :\
	(memcpy((buff)->code + (buff)->pos, (ptr), (size)*(nitem)),\
	(buff)->pos += (size)*(nitem), (nitem)))

#define buffRead(buff, size, nitem, ptr)\
	(((buff)->pos + (size)*(nitem) > (buff)->mbound)? MEM_EOB :\
	(memcpy((ptr), (buff)->code + (buff)->pos, (size)*(nitem)),\
	(buff)->pos += (size)*(nitem), (nitem)))

#define buffRewind(buff) ((buff)->pos = 0)

#define buffMerge(buff1, buff2)\
	(((buff1)->pos + (buff2)->pos > (buff1)->mbound)? MEM_EOB :\
	 (memcpy((buff1)->code + (buff1)->pos, (buff2)->code, (buff2)->pos),\
	 (buff1)->pos += (buff2)->pos, (buff2)->pos = 0, 0))

#define buffPart(buff1, buff2, len)\
	(((buff2)->pos + (len) > (buff2)->mbound)? MEM_EOB :\
	 (memcpy((buff1)->code, (buff2)->code + (buff2)->pos, (len)),\
	  (buff1)->pos = 0, (buff2)->pos += (len), 0))

#endif 
