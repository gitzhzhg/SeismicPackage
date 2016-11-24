// Complex.h - C++ include file for complex arithmetic

#ifndef COMPLEX_H
#define COMPLEX_H

#include <math.h>

class complex
{
public:
	float r;
	float i;
	inline complex() {r=0.0; i=0.0;}
	inline complex(float re, float im=0.0) {r=re; i=im;}
	inline float real(complex a);
	inline float imag(complex a);
	inline void operator+= (complex a);
	inline void operator+= (float a);
	inline void operator-= (complex a);
	inline void operator-= (float a);
	inline void operator*= (complex a);
	inline void operator*= (float a);
	inline void operator/= (complex a);
	inline void operator/= (float a);
};

// inline members
inline float complex::real(complex a) { return a.r; }
inline float complex::imag(complex a) { return a.i; }
inline void complex::operator+= (complex a)
{
	r += a.r; i += a.i;
}
inline void complex::operator+= (float a)
{
	r += a;
}
inline void complex::operator-= (complex a)
{
	r -= a.r; i -= a.i;
}
inline void complex::operator-= (float a)
{
	r -= a;
}
inline void complex::operator*= (complex a)
{
	float ar=a.r,ai=a.i,tr=r*ar-i*ai;
	i = r*ai+i*ar; r = tr;
}
inline void complex::operator*= (float a)
{
	r *= a; i *= a;
}
inline void complex::operator/= (complex a)
{
	float ar=a.r,ai=a.i,scale=1.0/(ar*ar+ai*ai);
	float tr = (r*ar+i*ai)*scale;
	i = (i*ar-r*ai)*scale; r = tr;
}
inline void complex::operator/= (float a)
{
	float scale=1.0/a;
	r *= scale; i *= scale;
}

// inline functions
inline int operator== (complex a, complex b)
{
	return a.r==b.r && a.i==b.i;
}
inline int operator== (complex a, float b)
{
	return a.r==b && a.i==0.0;
}
inline int operator== (float a, complex b)
{
	return a==b.r && b.i==0.0;
}
inline int operator!= (complex a, complex b)
{
	return a.r!=b.r || a.i!=b.i;
}
inline int operator!= (complex a, float b)
{
	return a.r!=b || a.i!=0.0;
}
inline int operator!= (float a, complex b)
{
	return a!=b.r || b.i!=0.0;
}
inline complex operator- (complex a)
{
	return complex(-a.r,-a.i);
}
inline complex conjg (complex a)
{
	return complex(a.r,-a.i);
}
inline float abs (complex a)
{
	return sqrt(a.r*a.r+a.i*a.i);
}
inline complex operator+ (complex a, complex b)
{
	return complex(a.r+b.r,a.i+b.i);
}
inline complex operator+ (complex a, float b)
{
	return complex(a.r+b,a.i);
}
inline complex operator+ (float a, complex b)
{
	return complex(a+b.r,b.i);
}
inline complex operator- (complex a, complex b)
{
	return complex(a.r-b.r,a.i-b.i);
}
inline complex operator- (complex a, float b)
{
	return complex(a.r-b,a.i);
}
inline complex operator- (float a, complex b)
{
	return complex(a-b.r,-b.i);
}
inline complex operator* (complex a, complex b)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i;
	return complex(ar*br-ai*bi,ar*bi+ai*br);
}
inline complex operator* (complex a, float b)
{
	return complex(a.r*b,a.i*b);
}
inline complex operator* (float a, complex b)
{
	return complex(a*b.r,a*b.i);
}
inline complex operator/ (complex a, complex b)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i,scale=1.0/(br*br+bi*bi);
	return complex((ar*br+ai*bi)*scale,(ai*br-ar*bi)*scale); 
}
inline complex operator/ (complex a, float b)
{
	float scale=1.0/b;
	return complex(a.r*scale,a.i*scale); 
}
inline complex operator/ (float a, complex b)
{
	float br=b.r,bi=b.i,scale=a/(br*br+bi*bi);
	return complex(br*scale,-bi*scale); 
}

// inline functions with destination argument (ugly, but efficient)
inline void cmplx(float a, float b, complex& c)
{
	c.r = a; c.i = b;
}
inline void conjg(complex& a, complex& b)
{
	b.r = a.r; b.i = -a.i;
}
inline void neg(complex& a, complex& b)
{
	b.r = -a.r; b.i = -a.i;
}
inline void add(complex& a, complex& b, complex& c)
{
	c.r = a.r+b.r; c.i = a.i+b.i;
}
inline void add(float a, complex& b, complex& c)
{
	c.r = a+b.r; c.i = b.i;
}
inline void add(complex& a, float b, complex& c)
{
	c.r = a.r+b; c.i = a.i;
}
inline void sub(complex& a, complex& b, complex& c)
{
	c.r = a.r-b.r; c.i = a.i-b.i;
}
inline void sub(float a, complex& b, complex& c)
{
	c.r = a-b.r; c.i = -b.i;
}
inline void sub(complex& a, float b, complex& c)
{
	c.r = a.r-b; c.i = a.i;
}
inline void mul(complex& a, complex& b, complex& c)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i;
	c.r = ar*br-ai*bi; c.i = ar*bi+ai*br;
}
inline void mul(float a, complex& b, complex& c)
{
	c.r = a*b.r; c.i = a*b.i;
}
inline void mul(complex& a, float b, complex& c)
{
	c.r = a.r*b; c.i = a.i*b;
}
inline void div(complex& a, complex& b, complex& c)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i,scale=1.0/(br*br+bi*bi);
	c.r = (ar*br+ai*bi)*scale; c.i = (ai*br-ar*bi)*scale; 
}
inline void div(float a, complex& b, complex& c)
{
	float br=b.r,bi=b.i,scale=a/(br*br+bi*bi);
	c.r = br*scale; c.i = -bi*scale; 
}
inline void div(complex& a, float b, complex& c)
{
	float scale=1.0/b;
	c.r = a.r*scale; c.i = a.i*scale; 
}

// non-inline function prototypes
complex sqrt(complex a);
complex cos(complex a);
complex sin(complex a);
complex cosh(complex a);
complex sinh(complex a);
complex exp(complex a);
complex log(complex a);
complex pow(complex a, int p);
complex pow(complex a, double p);
complex pow(double a, complex p);
complex pow(complex a, complex p);

#endif
