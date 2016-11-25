#ifdef __cplusplus
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
extern "C" {
#endif
void b_(float *x,int *n){
	float *y;
	int i;
	y=x;
	for (i=0;i<n;i++,y++){
		*y=i*i;
	}
	printf("Done with b.\n");
}
#ifdef TEST_B
main () {
		float x[5], *y; 
		int   *n = 5, i;

	y=&x;
	for (i=0;i<n;i++){
		y[i] = i;
		printf("TEST: %f,%d\n",y[i],i);
	}
	b_(y,n);
	for (i=0;i<n;i++){
		printf("TEST: %f,%d\n",x[i],i);
	}

}
#endif
#ifdef __cplusplus
{
#endif
