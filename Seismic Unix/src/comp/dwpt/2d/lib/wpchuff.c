/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/* WPCHUFF: $Revision: 1.4 $ ; $Date: 2011/11/21 16:24:37 $	*/

/*********************** self documentation **********************/
/**************************************************************************
WPCHUFF -Routines for in memory Huffman coding/decoding

huffCompress - huffman compress a byte stream in a memory buffer and output 
		the bit stream into another memory buffer
huffDecompress - decompress a huffman coded bit stream in a memory buffer and 
		output the byte stream into another memory buffer   

***************************************************************************
Function Prototypes:
int huffCompress(void *inb, void *outb)
int huffDecompress(void *inb, void *outb)

***************************************************************************
huffCompress:
Input:
inb	buffer holding the input byte stream 
Output:
outb	buffer holding the output bit stream

huffDecompres:
inb	buffer holding the input bit stream 
outb	buffer holding the output byte stream

***************************************************************************
Note:
The algorithm is based on that in Mark Nelson's "Data 
Compression" book. The compression is performed in memory here.

***************************************************************************
Author:		Tong Chen, 08/01/94
**************************************************************************/
/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"
#include "wpcbuffer.h"
#include "wpcbitbuff.h"

/* definitions used internally */ 
typedef struct huffNodeStruct {
        unsigned int count;
        unsigned int saved_count;
        int child_0;
        int child_1;
} huffNODE;	/* one node in a Huffman tree */


typedef struct huffCodeStruct {
        unsigned int code;
        int code_bits;
} huffCODE;	/* one Huffman code */


#define HUFFNSYMBOL 256
#define HUFFNCODE (HUFFNSYMBOL) + 1 
#define HUFFNNODE 2*(HUFFNCODE) 
#define HUFFENDOFBLK 256
#define HUFFRUNMIN 3
#define HUFFTERM 0
#define HUFFMAXCOUNT 255

/* routines used internally */
void huffCountbytes (wpcBUFF *buff, unsigned int *counts);
void huffScalecounts(unsigned int *counts, huffNODE *nodes);
int huffBuildtree (huffNODE *nodes);
void huffConverttreetocode (huffNODE *nodes, huffCODE *codes,
        unsigned int code_so_far, int bits, int node);
int huffOutputcounts(wpcBITBUFF *bit_buff, huffNODE *nodes);
int huffInputcounts(wpcBITBUFF *bit_buff, huffNODE *nodes);
int huffEncoder(wpcBUFF *buff, wpcBITBUFF *bit_buff, huffCODE *codes);
int huffDecoder(wpcBITBUFF *bit_buff, wpcBUFF *buff,
        huffNODE *nodes, int root_node);


int huffCompress(void *inb, void *outb)
/*************************************************************************
static huffman coding
**************************************************************************
inb	buffer holding the input byte stream 
outb	buffer holding the output bit stream
*************************************************************************/
{
	wpcBUFF *inbuff = (wpcBUFF *)inb;
	wpcBUFF *outbuff = (wpcBUFF *)outb;
	unsigned int *counts;	
	huffNODE *nodes;
	huffCODE *codes;
	int root_node;
	int nbytes=0, retval;
	wpcBITBUFF *bit_buff;

/*
fprintf(stderr,"in Huffman\n");
*/

	/* initialize the bit buffer */
	bitInitbuff(bit_buff, outbuff);
	
	/* allocate and init spaces for hit counts, and tree */
	counts = (unsigned int *) calloc(HUFFNSYMBOL,sizeof(unsigned int));
	nodes = (huffNODE *) calloc (HUFFNNODE,sizeof(huffNODE));
	codes = (huffCODE *) calloc (HUFFNCODE,sizeof(huffCODE));

	/* hit counts of the input symbols */
	huffCountbytes(inbuff, counts);

	/* scale the hit counts to hold them in one byte */
	huffScalecounts(counts, nodes);


	/* save the counts */
	retval = huffOutputcounts(bit_buff, nodes);
/*
	retval = 0;
*/

	if(retval != WPC_EOB){

	    nbytes = retval; 

	    /* build the huffman tree */
	    root_node = huffBuildtree(nodes);


	    /* convert the tree to codes */
	    huffConverttreetocode(nodes, codes, 0, 0, root_node);

	    /* huffman encoding */
	    retval = huffEncoder(inbuff, bit_buff, codes);

	    if(retval != WPC_EOB){
		nbytes += retval;

		bitWindbuff(bit_buff, retval);

	    	if(retval != WPC_EOB) nbytes += retval;
	    }
	}

	/* free the spaces */
	bitFreebuff(bit_buff);

	free((void *) counts);
	free((void *) nodes);
	free((void *) codes);

	return (retval == WPC_EOB? retval : nbytes);
}


int huffDecompress(void *inb, void *outb)
/*************************************************************************
static huffman decoding
**************************************************************************
inb	buffer holding the input bit stream 
outb	buffer holding the output byte stream
*************************************************************************/
{
	wpcBUFF *inbuff = (wpcBUFF *) inb;
	wpcBUFF *outbuff = (wpcBUFF *) outb;
	huffNODE *nodes;
	int root_node;
	wpcBITBUFF *bit_buff;

	/* initialize the bit buffer */
	bitInitbuff(bit_buff, inbuff);

	/* space for the Huffman tree */
	nodes = (huffNODE *) calloc (HUFFNNODE,sizeof(huffNODE));

	/* input the hit counts */
	if(huffInputcounts(bit_buff, nodes) == WPC_EOB) 
		return WPC_EOB;

	/* build the Huffman tree */
	root_node = huffBuildtree(nodes);

	/* decoding */
	if(huffDecoder(bit_buff, outbuff, nodes, root_node) == WPC_EOB)
		return WPC_EOB;

	/* free the spaces */
	bitFreebuff(bit_buff);

	free((void *) nodes);

	return 0;
}


int huffOutputcounts (wpcBITBUFF *bit_buff, huffNODE *nodes)
/*************************************************************************
output the hit count table to the output buffer 
**************************************************************************
bit_buff	buffer holding the encoded bit stream
nodes		array[] of nodes hold the leaves (the symbols) of the
		Huffman tree 
**************************************************************************
Note:	All the side information needed for decoding is this hit count table,
	from which the Huffman tree that is neede for decoding can be built. 
	To reduce the overhead of the hit count table, a simplified 
	runlength coding is used. It contains the first and last symbols 
	in the run, followed by the hit counts of each symbol. The last
	symbol HUFFTERM is used as the terminator. Only runs longer than 
	HUFFRUNMIN is treated this way. 
*************************************************************************/
{
	int first, last, next, i;
	int nbytes = 0;

	/* the first symbol in a run */
	first = 0;
	while(first < (HUFFNSYMBOL-1) && nodes[first].count ==0) first ++;

	for(; first < HUFFNSYMBOL; first=next){

	    /* find the last symbol in this run and the first symbol of the
	     next run */
	    last = first + 1;
	    for (; ; ){
		for(; last < HUFFNSYMBOL; last ++)
		    if(nodes[last].count == 0) break;
		last -- ;
		for(next = last + 1; next < HUFFNSYMBOL; next ++)
		    if(nodes[next].count != 0) break;
		if(next > (HUFFNSYMBOL-1)) break;
		if((next - last) > HUFFRUNMIN) break;

		last = next;
	    }

	   /* output the first, last and hit counts */
	   if(buffPutc(bit_buff->buff, first) == WPC_EOB)
		return WPC_EOB;
	   nbytes ++;

	   if(buffPutc(bit_buff->buff, last) == WPC_EOB)
		return WPC_EOB;
	   nbytes ++;

	   for(i=first; i<=last; i++){ 
		if(buffPutc(bit_buff->buff, nodes[i].count) == WPC_EOB)
		    return WPC_EOB;
	  	nbytes ++;
	    }
	}

	/* output the terminator for the end of count table */
	if(buffPutc(bit_buff->buff, HUFFTERM) == WPC_EOB)
	    return WPC_EOB;
	nbytes ++;

	return (nbytes);
}

int huffInputcounts(wpcBITBUFF *bit_buff, huffNODE *nodes)
/*************************************************************************
input the hit count table from the input buffer 
**************************************************************************
bit_buff	buffer holding the encoded bit stream
nodes		array[] of nodes hold the leaves (the symbols) of the
		Huffman tree 
*************************************************************************/
{
	int first, last, i, c;

	for(i=0; i<HUFFNSYMBOL;i++) nodes[i].count = 0;
	
	if( buffGetc(bit_buff->buff, first) == WPC_EOB) 
	    return WPC_EOB;
	if( buffGetc(bit_buff->buff, last) == WPC_EOB) 
	    return WPC_EOB;

	for( ; ; ){
	    for(i=first; i <= last ; i++)
	      	if(buffGetc(bit_buff->buff, c) == WPC_EOB) 
	    	    return WPC_EOB;
		else nodes[i].count = (unsigned int ) c;
	    if(buffGetc(bit_buff->buff, first) == WPC_EOB) 
	    	return WPC_EOB;
	    if (first == 0) break;
	    if(buffGetc(bit_buff->buff, last) == WPC_EOB) 
	    	return WPC_EOB;
	}

	nodes[HUFFENDOFBLK].count = 1;

	return 0;
}


void huffCountbytes (wpcBUFF *buff, unsigned int *counts)
/*************************************************************************
count the hit count of each input symbols
**************************************************************************
buff		buffer holding the byte stream to be encoded
counts		array[] holding the hit counts
*************************************************************************/
{
	unsigned char c;
	int pos;

	pos = buff->pos;

	while(buffGetc(buff, c) != WPC_EOB)
	    counts[c] ++;

	/* rewind the buffer */
	buff->pos = pos;
}	


void huffScalecounts (unsigned int *counts, huffNODE *nodes)
/*************************************************************************
scale the hit counts
**************************************************************************
counts		array[] holding the hit counts
nodes		array[] holding the nodes of the Huffman tree
**************************************************************************
Note:		To hold the hit counts in a single byte, a scaling is
		perform on the counts. This scaling also ensures that the
		output codes can be hold in 2 bytes. Of course some 
		efficiency is lost by this scaling.   
*************************************************************************/
{
	unsigned int max_count;
	int i;

	max_count = 0;

	for(i=0; i<HUFFNSYMBOL; i++)
	    if(counts[i] > max_count) max_count = counts[i];

	/* avoid empty stream */
	if(max_count == 0){
	    counts[0] = 1;
	    max_count = 1;
	}

	max_count /= HUFFMAXCOUNT;
	max_count ++;

	for(i=0; i<HUFFNSYMBOL; i++){

	    nodes[i].count = (unsigned int) (counts[i]/max_count);
	    /* to avoid missing the very rare symbols, we have to give 
		them at least one hit, which is NOT efficient. */
	    if(nodes[i].count == 0 && counts[i] != 0)
		nodes[i].count = 1;
	}

	/* end of block symbol */ 
	nodes[HUFFENDOFBLK].count = 1;
}

int huffBuildtree ( huffNODE *nodes)
/*************************************************************************
build the Huffman tree
**************************************************************************
nodes		array[] holding the nodes of the Huffman tree
**************************************************************************
Note:		All of the active nodes are scanned to locate the two nodes
		with the minimum weights. These two weights are then added
		together and assigned to a new node. The new node make the
		two minimum nodes into its 0 and 1 children. Then the two
		minimum nodes are marked as inactive. Repeat this until
		only one node is left, which is the root node.
*************************************************************************/
{
	int next_free, i, min_1, min_2;

	/* node HUFFNNODE-1, which is not used in the coding, is used
	here to provide a node with a guranteed maximum value. */

	nodes[HUFFNNODE-1].count = 0xffff;	/* big enough  */

	for(next_free = HUFFENDOFBLK + 1; ; next_free ++){

	    min_1 = HUFFNNODE-1;
	    min_2 = HUFFNNODE-1;

	    for(i=0; i< next_free; i++)
		if(nodes[i].count != 0){
		    if(nodes[i].count < nodes[min_1].count){
			min_2 = min_1;
			min_1 = i;
		    }
		    else if(nodes[i].count < nodes[min_2].count) min_2 = i;
		}

	    if(min_2 == HUFFNNODE-1) break;

	    nodes[next_free].count = nodes[min_1].count + nodes[min_2].count;

	    nodes[min_1].saved_count = nodes[min_1].count;
	    nodes[min_1].count = 0;
	    nodes[min_2].saved_count = nodes[min_2].count;
	    nodes[min_2].count = 0;

	    nodes[next_free].child_0 = min_1;
	    nodes[next_free].child_1 = min_2;
	}

	next_free --;

	nodes[next_free].saved_count = nodes[next_free].count;

	return ( next_free);
}

void huffConverttreetocode( huffNODE *nodes, huffCODE *codes, 
	unsigned int code_so_far, int bits, int node)
/*************************************************************************
conver the Huffman tree into code
**************************************************************************
nodes		array[] holding the nodes of the Huffman tree
codes		array[] holding the corresponding codes
code_so_far	intermediate code
bits		# of bits of the intermediate code
node		current node that is up to
**************************************************************************
Note:		This routine walks through the Huffman tree recursively and
		adds the child bits to each code until reaching a leaf.
*************************************************************************/
{
	/* a leaf node is reached */
	if(node <= HUFFENDOFBLK){
	    codes[node].code = code_so_far;
	    codes[node].code_bits = bits;
	    return;
	}

	code_so_far <<= 1;
	bits ++;

	huffConverttreetocode (nodes, codes, code_so_far, bits, 
			nodes[node].child_0);
	huffConverttreetocode (nodes, codes, code_so_far | 1, bits, 
			nodes[node].child_1);
}


int huffEncoder(wpcBUFF *inbuff, wpcBITBUFF *bit_buff, huffCODE *codes)
/*************************************************************************
encode all the symbols in the byte stream buffer
**************************************************************************
inbuff		input buffer for the input byte stream
bit_buffer	output buffer for the output bit stream
codes		Huffman codes
**************************************************************************
Note:		After the Huffman codes are built, encoding is just a 
		table look up.
*************************************************************************/
{
	int c;
	int nbytes = (bit_buff->buff)->pos;
	int retval;

	while(buffGetc(inbuff, c) != WPC_EOB){
	    bitOutputbits(bit_buff, codes[c].code, codes[c].code_bits, retval);
	    if(retval == WPC_EOB) return retval;
	}


	bitOutputbits(bit_buff, codes[HUFFENDOFBLK].code,
		codes[HUFFENDOFBLK].code_bits, retval);
	if(retval == WPC_EOB) return retval;

	/* return this value just to check if the encoding is normal */
	nbytes = (bit_buff->buff)->pos - nbytes;

	return (nbytes);
}

int huffDecoder (wpcBITBUFF *bit_buff, wpcBUFF *outbuff,
	huffNODE *nodes, int root_node)
/*************************************************************************
decode all the symbols in the bit stream buffer
**************************************************************************
bit_buffer	input buffer for the bit stream
outbuff		output buffer for the byte stream
nodes		array[] holding the nodes of the Huffman tree
root_node	node to start decoding
**************************************************************************
Note:		Decoding is done on a bit-by-bit base. When a leaf
		node is reach, the symbol is written to the output. 
*************************************************************************/
{
	int node, tmp;

	for(; ; ){
	   /* start from the root */
	   node = root_node;

	   /* while going through the internal nodes */
	   do {
		bitInputbit(bit_buff, tmp);
		if(tmp == WPC_EOB) return WPC_EOB;
		else if(tmp) node = nodes[node].child_1;
		else node = nodes[node].child_0;
	   } while(node > HUFFENDOFBLK); 

	   if(node == HUFFENDOFBLK) break;

	   /* output the symbol */	
	   buffPutc(outbuff, node);
	}

	return 0;
}
