/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#ifndef WPC_CONFIG_H
#define WPC_CONFIG_H

/* tiling sizes */
#define WPC_TILESZT 256 
#define WPC_TILESZX 256 
#define WPC_POWT 8 
#define WPC_POWX 8 


/* stage of decomposition */
#define WPC_STAGET 3
#define WPC_STAGEX 3 

/* filter shift and length */
#define WPC_WTSHIFT -4
#define WPC_WTLEN 12  

/* size of each block of coefficients */
#define WPC_NWTBLK 64		/* 2^WAVESTAGEX  x  2^WAVESTAGET */
#define WPC_LWTBLK 1024		


/* size of each Huffman block */
#define WPC_NHUFFBLK 4
#define WPC_LHUFFBLK 16 


#endif /* WPC_CONFIG_H */
