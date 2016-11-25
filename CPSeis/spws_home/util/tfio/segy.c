/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

typedef struct _SegyHdr2
{ int jobid;
  int line;
  int reel;

  short traces_per_rec;
  short traces_aux;
  short dt;
  short field_dt;
  short nt;
  short field_nt;
  short format_code;
  short cdp_fold;
  short sort_code;
  short vert_sum;
  short sweep_start;
} SegyHdr2;

void segy_hdr1_in()
{

}

void segy_get_hdr2(unsigned char *buffer, SegyHdr2 *binhdr)
{/* convert data in segy buffer to values */
 wrdc_convstr(&buffer[0], (int *) &binhdr->jobid, 4);
 wrdc_convstr(&buffer[4],(int *) &binhdr->line, 4);
 wrdc_convstr(&buffer[8],(int *) &binhdr->reel, 4);
 wrdc_convstr(&buffer[12],(int *) &binhdr->traces_per_rec, 2);
 wrdc_convstr(&buffer[14],(int *) &binhdr->traces_aux ,2);
 wrdc_convstr(&buffer[16],(int *) &binhdr->dt, 2);
 wrdc_convstr(&buffer[18],(int *) &binhdr->field_dt, 2);
 wrdc_convstr(&buffer[20],(int *) &binhdr->nt,2);
 wrdc_convstr(&buffer[22],(int *) &binhdr->field_nt,2);
 wrdc_convstr(&buffer[24],(int *) &binhdr->format_code,2);
 wrdc_convstr(&buffer[26],(int *) &binhdr->cdp_fold, 2);
 wrdc_convstr(&buffer[28],(int *) &binhdr->sort_code, 2);

}

void segy_put_hdr2(unsigned char *buffer, SegyHdr2 *binhdr)
{/* convert data in segy buffer to values */
 wrdc_convstr(&buffer[0], (int *) &binhdr->jobid, 4);
 wrdc_convstr(&buffer[4],(int *) &binhdr->line, 4);
 wrdc_convstr(&buffer[8],(int *) &binhdr->reel, 4);
 wrdc_convstr(&buffer[12],(int *) &binhdr->traces_per_rec, 2);
 wrdc_convstr(&buffer[14],(int *) &binhdr->traces_aux ,2);
 wrdc_convstr(&buffer[16],(int *) &binhdr->dt, 2);
 wrdc_convstr(&buffer[18],(int *) &binhdr->field_dt, 2);
 wrdc_convstr(&buffer[20],(int *) &binhdr->nt,2);
 wrdc_convstr(&buffer[22],(int *) &binhdr->field_nt,2);
 wrdc_convstr(&buffer[24],(int *) &binhdr->format_code,2);
 wrdc_convstr(&buffer[26],(int *) &binhdr->cdp_fold, 2);
 wrdc_convstr(&buffer[28],(int *) &binhdr->sort_code, 2);

}
