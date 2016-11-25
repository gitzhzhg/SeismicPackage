/*<CPS_v1 type="AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>
!                 Related files:
!                      lbo.f90  lbo_crou.h
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
! Name       : lbo_crou (.c)
! Category   : io
! Revised    : 2006-08-29   by: SMCook
! Maturity   : production
! Purpose    : Compress seismic data.
! Portability: Possibly 4-byte word length only.
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2006-08-29  SMCook       Fixed left-over debug statement.
!  4. 2006-05-25  SMCook       Now can read/write version 2, which supports
!                               "geologic masking" wherein sample values are
!                               optionally flagged to show (for example) the
!                               location of a salt body.
!                              Also, caller no longer has to deal with the
!                               bitmask.  It's handled herein using a global,
!                               effectively hidden from the outside.
!                              Also other stuff like a bit dump for char.
!  3. 2005-04-21  SMCook       Added fix for precision problem with extremely
!                               low-amplitude portions of traces.
!  2. 2004-12-15  SMCook       Added workaround logic to allow bypassing
!                               corrupt traces.  Warning message is printed (up
!                               to 1000 times) if this has occurred.
!  1. 2004-08-23  SMCook       Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!  Please see lbo.f90.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
*/

#include <math.h>
#include <stdlib.h>
#include "lbo_crou.h"
#include "swap.h"
#include "named_constants.h"

/*
"$Id: lbo_crou.c,v 1.5 2006/08/30 13:15:17 SMCook prod sps $"
*/

#ifdef __cplusplus
extern "C" {
#endif


/*******************************************************************************
 Globals.
*******************************************************************************/
int lbo_ibuf_size   = 0;
int lbo_factor_size = 0;
float *lbo_factor = NULL;
int *lbo_ibuf = NULL;
int *_lbo_mask = NULL;

int VERBOSE = 0;  /* turns debug print statements on and off (0 = off) */

int lbo_workaround_count = 0;
int LBO_MAX_WORKAROUND_COUNT = 1000;

/*******************************************************************************
 Debug bit print functionality for lbo format
*******************************************************************************/
void lbo_print_charbits_c(char *val) {
  int ibyte = *val;
  printf("%4i: ", *val);
  if(0x80000000 & ibyte) printf("1"); else printf("0");
  if(0x00000040 & ibyte) printf("1"); else printf("0");
  if(0x00000020 & ibyte) printf("1"); else printf("0");
  if(0x00000010 & ibyte) printf("1"); else printf("0");
  if(0x00000008 & ibyte) printf("1"); else printf("0");
  if(0x00000004 & ibyte) printf("1"); else printf("0");
  if(0x00000002 & ibyte) printf("1"); else printf("0");
  if(0x00000001 & ibyte) printf("1"); else printf("0");
}

/*******************************************************************************
 Debug bit print functionality for lbo format
*******************************************************************************/
void lbo_print_intbits_c(int val, int swap) {
  int i, ival, j = 0;
  char *cbuf;
  char ctmp;
  if(swap) {
    cbuf = (char *)&ival;
    for(i=0; i<2; i++) {
      ctmp = cbuf[i];
      cbuf[i] = cbuf[3-i];
      cbuf[3-i] = ctmp;
    }
    val = ival;
  }
  for(i=0; i<32; i++) {
    if(val & (1 << (31-i)))
      printf("1");
    else
      printf("0");
    j++;
    if(j == 4) {
      printf(" ");
      j = 0;
    }
  }
}

/*******************************************************************************
 Calculate the number of packets based on nsamps and samps per packet
  (does not need to be called from Fortran).
*******************************************************************************/
int lbo_npackets(int nsamps, int samps_per_pack) {
  return (nsamps + samps_per_pack - 1) / samps_per_pack;
}


/*******************************************************************************
 Calculate the record length in bytes.  Contains small amount of padding at the
  end to assure enough space remains for bit-shifting.
*******************************************************************************/
int lbo_get_recl_version1_c(int nsamps, int samps_per_pack, int precision) {

  int recl;

  /* critical npackets calculation */
  int npackets = lbo_npackets(nsamps, samps_per_pack);

  /* critical ndatabytes calculation */
  int nbits = nsamps * precision;
  int ndatabytes = (nbits + 7) / 8;

  if(VERBOSE) {
    printf("lbo_crou.c: nsamps     = %i\n", nsamps);
    printf("lbo_crou.c: npackets   = %i\n", npackets);
    printf("lbo_crou.c: ndatabytes = %i\n", ndatabytes);
  }

  /* calculate the "basic record length" -- the final answer for version 1 */
  recl =
    1 +             /* 1 byte for version flag              */
    1 +             /* 1 byte for byte order flag           */
    1 +             /* 1 byte for precision                 */
    1 +             /* 1 byte not used (set to zero)        */
    4 +             /* 4 bytes for nsamps                   */
    4 +             /* 4 bytes for samps_per_pack           */
    4*npackets +    /* 4 bytes per packet for scale factors */
    ndatabytes;     /* space for the bitstream itself       */

  return ((recl + 7) / 4) * 4;  /* pad & round up to multiple of 4 */
}

/*******************************************************************************
 Calculate nflagbytes from nsamps.
*******************************************************************************/
int lbo_nflagbytes(int nsamps) {
  int nflagbytes = (nsamps + 7) / 8;  /* exact # of bytes needed for flags */
  return ((nflagbytes + 3)/ 4) * 4;   /* round up to word size             */
}

/*******************************************************************************
 Calculate the record length in bytes.  Length depends on format version.
*******************************************************************************/
void lbo_get_recl_excluding_header_c(
  int *version, int *nsamps, int *samps_per_pack, int *precision, int *recl) {

  int basic_recl =
    lbo_get_recl_version1_c(*nsamps, *samps_per_pack, *precision);
  if(VERBOSE)printf("lbo_crou.c: basic_recl = %i\n", basic_recl);

  switch(*version) {
    case 1:
      *recl = basic_recl;
    break;

    case 2:
      *recl = basic_recl + lbo_nflagbytes(*nsamps);
    break;

    default:
      printf("I can't calculate record length if version isn't 1 or 2.\n");
      exit(1);
      *recl = -1;
    break;
  }

  if(VERBOSE)printf("lbo_crou.c: *recl = %i\n", *recl);
}

/*******************************************************************************
 Create mask needed for lbo_compress_trace_c - step 1

  This mask is needed later by the pack_bits_c routine.  It is created
  once separately so as not to pay the cost of regenerating it each time.
*******************************************************************************/
void lbo_create_bitmask_c(int precision) {
  int i, imax, base = 0;
  int swap = swap_endian();

  if(VERBOSE)printf("lbo_crou.c: precision = %i\n", precision);
  for(i=0; i<precision; i++) {
    base |= (1 << i);
  }
  if(VERBOSE)printf("lbo_crou.c: base = %i\n", base);
  for(i=0; i<33; i++) {
    _lbo_mask[i] = 0;
  }
  imax = 33 - precision;
  for(i=0; i<imax; i++) {
    if(VERBOSE)printf("lbo_crou.c: i = %i\n", i);
    _lbo_mask[i] = base << i;
    if(VERBOSE) {
      lbo_print_intbits_c(_lbo_mask[i], swap); printf("\n");
    }
  }
}

/*******************************************************************************
 Pack arbitrary-precision integers into a charbuf - step 2

  This function handles the grouping and local rescaling (local bit
   optimization) of groups of samples.  Once the full stream of rescaled
   values is completed, it then calls lbo_compress_bits_c to put them all
   into one bitstream.  Only the sample values themselves are packed --
   the "control values" that store the number of samples, samples per group,
   and floating point scale factors are not bitpacked.
*******************************************************************************/
void lbo_compress_trace_c(
  int *version, float *fbuf, int *nsamps, char *cbuf, int *clen,
  int *precision, int *samps_per_pack, int *status) {

  int i, imax, j, k;
  int max_allowed = pow(2, *precision) / 2 - 1;
  float ftmp, absmax = 0;
  int ipacket, ofs, clen2;
  int swap = swap_endian();

  int *iptr = NULL;
  float *fptr = NULL;
  char *flags = NULL;

  int npackets;

  /* Addition for version 2:  the 'flags_are_present' variable will be set to 1
     if there are actually FNILs in the fbuf data.  This is for efficiency --
     when the data is read back in, the flag bitstream can be ignored if there
     are no FNILs on this particular trace (but the bitstream is always present,
     so the traces will always be the same predictable length). */
  int flags_are_present = 0;

  /* zero the output buf */
  for(i=0; i<*clen; i++) {
    cbuf[i] = 0;
  }

  if(VERBOSE) {
    printf("lbo_crou.c: version = %i\n", *version);
    printf("lbo_crou.c: *clen = %i\n", *clen);
  }

  /* possible malloc's needed */
  if(lbo_ibuf_size < 4 * *nsamps) {
    lbo_ibuf_size = 4 * *nsamps;
    lbo_ibuf = (int *)malloc((size_t)lbo_ibuf_size);
  }

  npackets = lbo_npackets(*nsamps, *samps_per_pack);
  if(lbo_factor_size < 4 * npackets) {
    lbo_factor_size = 4 * npackets;
    lbo_factor = (float *)malloc((size_t)lbo_factor_size);
  }

  if(*version == 2) {
    flags = (char *)malloc((size_t)*nsamps);
  }

  /* calculate the scale factors based on local absolute maxima */
  j = 0;
  ipacket = 0;
  for(i=0; i<*nsamps; i++) {
    ftmp = fbuf[i];

    if(*version == 2) {
      if(ftmp == FNIL) {
        flags_are_present = 1;
        flags[i] = 1;
      }
      else {
        flags[i] = 0;
      }
    }

    ftmp = fabs(ftmp);
    if(ftmp > absmax) absmax = ftmp;

    j++;
    if(j == *samps_per_pack || i == (*nsamps) - 1) {
      lbo_factor[ipacket] = absmax/max_allowed;
      if(VERBOSE)printf("lbo_crou.c: ipacket, lbo_factor[ipacket] = %i %f\n",
                                     ipacket, lbo_factor[ipacket]);
      for(k=i-j+1; k<=i; k++) {
        if(absmax == 0)
          lbo_ibuf[k] = 0;
        else if((ftmp=fbuf[k]) >= 0)
          lbo_ibuf[k] =  (int)(ftmp/lbo_factor[ipacket] + .5f);  /* nearest */
        else
          lbo_ibuf[k] = -(int)(.5f - ftmp/lbo_factor[ipacket]);  /* nearest */

        if(VERBOSE)printf("lbo_crou.c: %5i%12.6f%12.6f%10i\n",
          k, absmax, fbuf[k], lbo_ibuf[k]);
      }
      ipacket++;
      j = 0;
      absmax = 0;
    }
  }

  if(VERBOSE) {
    printf("lbo_crou.c: ratio check:\n");
    for(i=0; i<*nsamps; i++) {
      printf("lbo_crou.c: %5i%12.6f%10i%18.12f\n",
        i, fbuf[i], lbo_ibuf[i], fbuf[i]/lbo_ibuf[i]);
    }
  }

  /*
   * First, embed the following 3 bytes of information right into the trace:
   *  1 byte to record a format "version number"
   *  1 byte to record the byte order of the float scale factors
   *  1 byte for precision (num of bits)
   */

  cbuf[0] = *version;     /* file format version */
  cbuf[1] = swap;         /* big endian = 1      */
  cbuf[2] = *precision;
  if(*version == 1)
    cbuf[3] = 0;          /* not used if version == 1 */
  else
    cbuf[3] = flags_are_present;  /* may be set to 1 if version == 2 */

  if(VERBOSE) {
    for(i=0; i<4; i++) {
      printf("lbo_crou.c: w cbuf[%i] = %i\n", i, (int)cbuf[i]);
    }
  }

  /*
   * Second, write a 4-byte ints for nsamps and samps_per_pack.
   */
  ofs = 4; iptr = (int *)(cbuf+ofs); *iptr = *nsamps;
  ofs = 8; iptr = (int *)(cbuf+ofs); *iptr = *samps_per_pack;

  if(VERBOSE) {
    printf("lbo_crou.c: w int *nsamps         = %i\n", *nsamps);
    printf("lbo_crou.c: w *samps_per_pack = %i\n", *samps_per_pack);
  }

  /*
   * Third, write a float scale factor for each packet (and partial packet).
   */
  ofs = 12;
  for(i=0; i<npackets; i++) {
    if(VERBOSE) printf("lbo_crou.c: w lbo_factor[%i] = %f\n", i, lbo_factor[i]);
    fptr = (float *)(cbuf+ofs);
    *fptr = lbo_factor[i];
    ofs += 4;
  }

  /*
   * Fourth, pack the data bits portion.
   */
  clen2 = (*clen) - ofs;
  if(VERBOSE)
    printf("lbo_crou.c: call lbo_compress_bits, *clen, clen2 = %i %i\n",
                                                              *clen, clen2);
  lbo_compress_bits_c(
    lbo_ibuf, nsamps, (char *)(cbuf+ofs), &clen2, precision, status);
  if(*status != 0) {  // not good
    for(i=0; i<*nsamps; i++) {
      printf("lbo_crou.c: nonzero status fbuf[%i] = %20.15e\n", i, fbuf[i]);
    }
  }

  if(VERBOSE) {
    printf("lbo_crou.c: done lbo_compress_bits, *clen, clen2 = %i %i\n",
                                                              *clen, clen2);
    for(i=0; i<*clen; i++) {
      printf("lbo_crou.c: w cbuf[%i] = %i\n", i, (int)cbuf[i]);
    }
  }

  /*
   * Finally, write the flags bitstream and free the array.
   */
  if(*version == 2) {

    int ibyte = lbo_get_recl_version1_c(*nsamps, *samps_per_pack, *precision);
    char *cptr = cbuf+ibyte;

    if(VERBOSE)printf("lbo_crou.c: version 2 recl = %i\n", ibyte);

    j=0;
    for(i=0; i<*nsamps; i++) {
      if(VERBOSE)printf("lbo_crou.c: w flags[%i] = %i\n", i, flags[i]);
      if(flags[i] == 1) {
        switch(j) {
          case  0: (*cptr) |= 0x00000080; break;
          case  1: (*cptr) |= 0x00000040; break;
          case  2: (*cptr) |= 0x00000020; break;
          case  3: (*cptr) |= 0x00000010; break;

          case  4: (*cptr) |= 0x00000008; break;
          case  5: (*cptr) |= 0x00000004; break;
          case  6: (*cptr) |= 0x00000002; break;
          case  7: (*cptr) |= 0x00000001; break;
        }
      } // end if
      j++;
      if(j == 8) {
        j = 0;
        cptr++;
      }
    }

    if(flags != NULL) free(flags);

    if(VERBOSE) {
      printf("lbo_crou.c: w bit dump:\n");
      imax = ibyte + lbo_nflagbytes(*nsamps);
      for(i=0; i<imax; i++) {
        printf("lbo_crou.c: w %4i  ", i);
        lbo_print_charbits_c(cbuf+i); printf("\n");
      }
    }
  }
}

/*******************************************************************************
 Internal use only -- reads the bits that potentially flag FNILs.
*******************************************************************************/
void lbo_read_flag_bitstream_c(
  int nsamps, int samps_per_pack, int precision, char *cbuf, char *flags) {

  int ibyte = lbo_get_recl_version1_c(nsamps, samps_per_pack, precision);

  char ch = 0;
  int i, j = 0;
  char *cptr = cbuf+ibyte;

  if(VERBOSE)printf("lbo_crou.c: recl = %i\n", ibyte);
  for(i=0; i<nsamps; i++) {
    flags[i] = 0;
    switch(j) {
      case  0:  ch = (*cptr) & 0x00000080; break; 
      case  1:  ch = (*cptr) & 0x00000040; break; 
      case  2:  ch = (*cptr) & 0x00000020; break; 
      case  3:  ch = (*cptr) & 0x00000010; break; 
      case  4:  ch = (*cptr) & 0x00000008; break; 
      case  5:  ch = (*cptr) & 0x00000004; break; 
      case  6:  ch = (*cptr) & 0x00000002; break; 
      case  7:  ch = (*cptr) & 0x00000001; break; 
    }
    if(ch != 0) flags[i] = 1;
    if(VERBOSE)printf("lbo_crou.c: r flags[%i] = %i\n", i, flags[i]);

    j++;
    if(j == 8) {
      j = 0;
      if(VERBOSE) { lbo_print_charbits_c(cptr); printf("\n"); }
      cptr++;
    }
  }
}

/*******************************************************************************
 Pack arbitrary-precision integers into a charbuf - step 3

 This routine packs the data bits.  It does not handle the "control values" --
  in fact the control values MUST NOT be passed to this routine.  They would
  be erroneously interpreted as being seismic data.
*******************************************************************************/
void lbo_compress_bits_c(
  int *ibuf, int *ilen, char *cbuf, int *clen, int *precision, int *status) {

  int val, absval;
  int i, ibyte, ibit = 0;
  int itest, leftshift, rightshift;
  int *itemp;
  char ctemp[4];
  int max_allowed = pow(2, *precision) / 2 - 1;
  int swap = swap_endian();

  *status = -1;

  /* create mask if needed */
  if(_lbo_mask == NULL) {
    if(VERBOSE)printf("lbo_crou.c: creating bit mask\n");
    _lbo_mask = (int *)malloc(33 * sizeof(int));
    lbo_create_bitmask_c(*precision);
  }

  /* zero the output buffer */
  if(VERBOSE)printf("lbo_crou.c: zeroing\n");
  for(i=0; i<*clen; i++) {
    cbuf[i] = 0;
  }

  /* loop through each input val */
  for(i=0; i<*ilen; i++) {
    val    = ibuf[i];
    absval = abs(val);

    /* check for out-of-range values */
    if(absval > max_allowed) {
      printf("lbo_crou.c: i = %i, ratio = %i / %i\n", i, absval, max_allowed);
      if(val < 0)
        val = -max_allowed;
      else if(val > 0)
        val =  max_allowed;
/*
   technically this is perhaps an error condition, but only occurs in extremely
   rare circumstances and in extremely low-amplitude portions of traces
*/
/*
      *status = -2;
      return;
*/
    }

    /* left shift packs the bits so they're adjacent to the sign bit */
    leftshift = 32 - *precision;

    ibyte = ibit / 8;

    rightshift = ibit - (ibyte * 8);

    itest = val << leftshift;

    if(rightshift != 0) {
      itest = itest >> rightshift;
      itest &= _lbo_mask[32 - rightshift - *precision];
    }

    /* leftshift result shown diagrammatically:
     *  val
     *  0000 0000 0000 0000 0000 0000 0000 0011   suppose we have val = 3
     *
     *  itest
     *  0000 0000 0000 0011 0000 0000 0000 0000   sample for 16-bit precision
     *  0000 0000 0011 0000 0000 0000 0000 0000   sample for 12-bit precision
     *  0110 0000 0000 0000 0000 0000 0000 0000   sample for  3-bit precision
     */

    /* put the correct 4 bytes into ctemp (they may already have data) */
    if(!swap) {
      ctemp[0]=cbuf[ibyte+3];
      ctemp[1]=cbuf[ibyte+2];
      ctemp[2]=cbuf[ibyte+1];
      ctemp[3]=cbuf[ibyte  ];
    }
    else {
      ctemp[0]=cbuf[ibyte  ];
      ctemp[1]=cbuf[ibyte+1];
      ctemp[2]=cbuf[ibyte+2];
      ctemp[3]=cbuf[ibyte+3];
    }

    /* OR with the new value being added to the bit stream */
    itemp = (int *)&ctemp;
    *itemp |= itest;

    /* put the resulting bytes back in the output stream */
    if(!swap) {
      cbuf[ibyte+3]=ctemp[0];
      cbuf[ibyte+2]=ctemp[1];
      cbuf[ibyte+1]=ctemp[2];
      cbuf[ibyte  ]=ctemp[3];
    }
    else {
      cbuf[ibyte  ]=ctemp[0];
      cbuf[ibyte+1]=ctemp[1];
      cbuf[ibyte+2]=ctemp[2];
      cbuf[ibyte+3]=ctemp[3];
    }

    ibit += *precision;
  }

  *status = 0;
}

/*** Unpack lbo_compressed trace.
 *
 * The "control values" are handled here, whereas the data is unpacked by a
 * slightly more generic bit-unpacking algorithm.
 */
void lbo_uncompress_trace_c(
  char *cbuf, int *clen, float *fbuf, int *nsamps,
  int *version, int *precision, int *samps_per_pack, int *status) {

  int byte_order;
  int i, imax, ipacket, npackets, j, ofs, clen2, nsamps2;
  int *iptr;
  float *fptr;
  int swap = swap_endian();

  int flags_are_present = 0;
  char *flags = NULL;  /* for version 2 */

  if(VERBOSE){
    imax = (*clen) + (*nsamps)/8;  // approximate end
    for(i=0; i<imax; i++) {
      printf("lbo_crou.c: r cbuf[%i] = %i\n", i, (int)cbuf[i]);
    }
  }

  /*
   * First, recover the following 3 bytes of information:
   *  1 byte to record a format "version number"
   *  1 byte to record the byte order of the float scale factors
   *  1 byte for precision (num of bits)
   *  1 byte not used
   */
  *version   = cbuf[0];
  byte_order = cbuf[1];
  *precision = cbuf[2];
  flags_are_present = cbuf[3];

  if(VERBOSE) {
    printf("lbo_crou.c: r *version   = %i\n", *version);
    printf("lbo_crou.c: r byte_order = %i\n", byte_order);
    printf("lbo_crou.c:           (swap was %i)\n", swap);
    printf("lbo_crou.c: r *precision = %i\n", *precision);
    printf("lbo_crou.c: flags are present = %i\n", flags_are_present);
  }

  /*
   * Second, recover nsamps and samps_per_pack (4-byte ints).
   */
  ofs = 4;
  iptr = (int *)(cbuf+ofs);
  nsamps2 = *iptr;
  if(swap != byte_order) swap_int_4(&nsamps2);

  ofs = 8;
  iptr = (int *)(cbuf+ofs);
  *samps_per_pack = *iptr;
  if(swap != byte_order) swap_int_4(samps_per_pack);

  /* bug fix - if *samps_per_pack = 0, floating point exception occurs later */
  if(VERBOSE)printf("lbo_crou.c: r *samps_per_pack = %i\n", *samps_per_pack);
  if(*samps_per_pack == 0) {
    if(lbo_workaround_count++ < LBO_MAX_WORKAROUND_COUNT) {
      printf("lbo_crou.c: WARNING: *samps_per_pack, nsamps2 = %i %i\n",
                                   *samps_per_pack, nsamps2);
    }
    for(i=0; i<*nsamps; i++) {
      fbuf[i] = 0;
    }
    *status = *nsamps;
    return;
  }
 
  if(VERBOSE)
    printf("lbo_crou.c: r nsamps2 = %i (*nsamps = %i)\n", nsamps2, *nsamps);

  /*
   * Third, recover the float scale factors.
   */

  /* possible malloc's needed */
  if(lbo_ibuf_size < 4 * *nsamps) {
    lbo_ibuf_size = 4 * *nsamps;
    lbo_ibuf = (int *)malloc((size_t)lbo_ibuf_size);
  }

  /* printf("lbo_crou.c: (%i %i)\n", *nsamps, *samps_per_pack); */
  npackets = lbo_npackets(*nsamps, *samps_per_pack);
  if(lbo_factor_size < 4 * npackets) {
    lbo_factor_size = 4 * npackets;
    lbo_factor = (float *)malloc((size_t)lbo_factor_size);
  }

  ofs = 12;
  for(i=0; i<npackets; i++) {
    if(VERBOSE)printf("lbo_crou.c: i, npackets = %i %i\n", i, npackets);
    fptr = (float *)(cbuf+ofs);
    lbo_factor[i] = *fptr;
    if(swap != byte_order) swap_float_4(&lbo_factor[i]);
    if(VERBOSE)printf("lbo_crou.c: r lbo_factor[%i] = %f\n", i, lbo_factor[i]);
    ofs += 4;
  }

  /*
   * Fourth, get the integers from the bit stream.
   */
  clen2 = (*clen) - ofs;
  if(VERBOSE) {
    printf("lbo_crou.c: calling unpack_bits, clen2 = %i\n", clen2);
    printf("lbo_crou.c: allocating and zeroing lbo_ibuf\n");
  }
  for(i=0; i<*nsamps; i++) {
    lbo_ibuf[i] = 0;
  }
  lbo_uncompress_bits_c(cbuf+ofs, &clen2, lbo_ibuf, nsamps, precision, status);

  /**
   * Fifth, if version == 2, read the flag bitstream if flags are present.
   */
  if(flags_are_present) {
    flags = (char *)malloc(*nsamps * sizeof(char));
    lbo_read_flag_bitstream_c(
      *nsamps, *samps_per_pack, *precision, cbuf, flags);
  }

  /*
   * Final step -- apply the scale factors and (for version 2) restore FNILs.
   */
  i = 0;
  j = 0;
  ipacket = 0;
  while(1) {
    if(i == *nsamps) break;
    if(j == *samps_per_pack) {
      ipacket++;
      j = 0;
    }
    fbuf[i] = lbo_factor[ipacket] * lbo_ibuf[i];
    if(flags_are_present && flags[i] == 1) {
      fbuf[i] = FNIL;
    }
    if(VERBOSE) {
      printf("lbo_crou.c: r i, flags[i] fbuf[i] = %i %i %f\n",
                                                  i, (int)flags[i], fbuf[i]);
    }
    i++;
    j++;
  }

  if(flags != NULL) free(flags);

  *status = nsamps2;
}

/*******************************************************************************
 Unpack arbitrary-precision integers from a BIG ENDIAN charbuf -- step 4

  Reading the data is easier -- no masking, just bit shifts left then right.
*******************************************************************************/

void lbo_uncompress_bits_c(
  char *cbuf, int *clen, int *ibuf, int *ilen, int *precision, int *status) {

  int i, ibyte, ibit = 0;
  int *itemp;
  char ctemp[4];
  int leftshift, rightshift, swap = swap_endian();

  /* initialize */
  *status = -2;
  rightshift = 32 - *precision;

  /* zero the output buffer */
  if(VERBOSE)printf("lbo_crou.c: zeroing\n");
  for(i=0; i<*ilen; i++) {
    ibuf[i] = 0;
  }

  /* proceed down the bit stream */
  if(VERBOSE)printf("lbo_crou.c: unpacking loop\n");
  for(i=0; i<*ilen; i++) {
    ibyte = ibit / 8;
    if(!swap) {
      ctemp[0]=cbuf[ibyte+3];
      ctemp[1]=cbuf[ibyte+2];
      ctemp[2]=cbuf[ibyte+1];
      ctemp[3]=cbuf[ibyte  ];
    }
    else {
      ctemp[0]=cbuf[ibyte  ];
      ctemp[1]=cbuf[ibyte+1];
      ctemp[2]=cbuf[ibyte+2];
      ctemp[3]=cbuf[ibyte+3];
    }
    leftshift = ibit - 8*ibyte;
/*
    printf("ibyte, ilen, shifts = %i %i %i %i\n",
      ibyte, *ilen, leftshift, rightshift);
    printf("ctemps = %i %i %i %i\n",
      (int)ctemp[0], (int)ctemp[1], (int)ctemp[2], (int)ctemp[3]);
*/
    itemp = (int *)&ctemp[0];

    ibuf[i] = ((*itemp) << leftshift) >> rightshift;
    if(VERBOSE)printf("lbo_crou.c: r ibuf[%i] = %i\n", i, ibuf[i]);

    ibit += *precision;
  }

  *status = 0;
}

#ifdef __cplusplus
}
#endif
