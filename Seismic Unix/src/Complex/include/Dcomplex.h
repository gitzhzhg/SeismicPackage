// Complex.h - C++ include file for dcomplex arithmetic

#ifndef DCOMPLEX_H
#define DCOMPLEX_H

#include <math.h>

class dcomplex
{
public:
	float r;
	float i;
	inline dcomplex() {r=0.0; i=0.0;}
	inline dcomplex(double re, double im=0.0) {r=re; i=im;}
	inline float real(dcomplex a);
	inline float imag(dcomplex a);
	inline void operator+= (dcomplex a);
	inline void operator+= (float a);
	inline void operator-= (dcomplex a);
	inline void operator-= (float a);
	inline void operator*= (dcomplex a);
	inline void operator*= (float a);
	inline void operator/= (dcomplex a);
	inline void operator/= (float a);
};

// inline members
inline float dcomplex::real(dcomplex a) { return a.r; }
inline float dcomplex::imag(dcomplex a) { return a.i; }
inline void dcomplex::operator+= (dcomplex a)
{
	r += a.r; i += a.i;
}
inline void dcomplex::operator+= (float a)
{
	r += a;
}
inline void dcomplex::operator-= (dcomplex a)
{
	r -= a.r; i -= a.i;
}
inline void dcomplex::operator-= (float a)
{
	r -= a;
}
inline void dcomplex::operator*= (dcomplex a)
{
	float ar=a.r,ai=a.i,tr=r*ar-i*ai;
	i = r*ai+i*ar; r = tr;
}
inline void dcomplex::operator*= (float a)
{
	r *= a; i *= a;
}
inline void dcomplex::operator/= (dcomplex a)
{
	float ar=a.r,ai=a.i,scale=1.0/(ar*ar+ai*ai);
	float tr = (r*ar+i*ai)*scale;
	i = (i*ar-r*ai)*scale; r = tr;
}
inline void dcomplex::operator/= (float a)
{
	float scale=1.0/a;
	r *= scale; i *= scale;
}

// inline functions
inline int operator== (dcomplex a, dcomplex b)
{
	return a.r==b.r && a.i==b.i;
}
inline int operator== (dcomplex a, float b)
{
	return a.r==b && a.i==0.0;
}
inline int operator== (float a, dcomplex b)
{
	return a==b.r && b.i==0.0;
}
inline int operator!= (dcomplex a, dcomplex b)
{
	return a.r!=b.r || a.i!=b.i;
}
inline int operator!= (dcomplex a, float b)
{
	return a.r!=b || a.i!=0.0;
}
inline int operator!= (float a, dcomplex b)
{
	return a!=b.r || b.i!=0.0;
}
inline dcomplex operator- (dcomplex a)
{
	return dcomplex(-a.r,-a.i);
}
inline dcomplex conjg (dcomplex a)
{
	return dcomplex(a.r,-a.i);
}
inline float abs (dcomplex a)
{
	return sqrt(a.r*a.r+a.i*a.i);
}
inline dcomplex operator+ (dcomplex a, dcomplex b)
{
	return dcomplex(a.r+b.r,a.i+b.i);
}
inline dcomplex operator+ (dcomplex a, float b)
{
	return dcomplex(a.r+b,a.i);
}
inline dcomplex operator+ (float a, dcomplex b)
{
	return dcomplex(a+b.r,b.i);
}
inline dcomplex operator- (dcomplex a, dcomplex b)
{
	return dcomplex(a.r-b.r,a.i-b.i);
}
inline dcomplex operator- (dcomplex a, float b)
{
	return dcomplex(a.r-b,a.i);
}
inline dcomplex operator- (float a, dcomplex b)
{
	return dcomplex(a-b.r,-b.i);
}
inline dcomplex operator* (dcomplex a, dcomplex b)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i;
	return dcomplex(ar*br-ai*bi,ar*bi+ai*br);
}
inline dcomplex operator* (dcomplex a, float b)
{
	return dcomplex(a.r*b,a.i*b);
}
inline dcomplex operator* (float a, dcomplex b)
{
	return dcomplex(a*b.r,a*b.i);
}
inline dcomplex operator/ (dcomplex a, dcomplex b)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i,scale=1.0/(br*br+bi*bi);
	return dcomplex((ar*br+ai*bi)*scale,(ai*br-ar*bi)*scale); 
}
inline dcomplex operator/ (dcomplex a, float b)
{
	float scale=1.0/b;
	return dcomplex(a.r*scale,a.i*scale); 
}
inline dcomplex operator/ (float a, dcomplex b)
{
	float br=b.r,bi=b.i,scale=a/(br*br+bi*bi);
	return dcomplex(br*scale,-bi*scale); 
}

// inline functions with destination argument (ugly, but efficient)
inline void cmplx(float a, float b, dcomplex& c)
{
	c.r = a; c.i = b;
}
inline void conjg(dcomplex& a, dcomplex& b)
{
	b.r = a.r; b.i = -a.i;
}
inline void neg(dcomplex& a, dcomplex& b)
{
	b.r = -a.r; b.i = -a.i;
}
inline void add(dcomplex& a, dcomplex& b, dcomplex& c)
{
	c.r = a.r+b.r; c.i = a.i+b.i;
}
inline void add(float a, dcomplex& b, dcomplex& c)
{
	c.r = a+b.r; c.i = b.i;
}
inline void add(dcomplex& a, float b, dcomplex& c)
{
	c.r = a.r+b; c.i = a.i;
}
inline void sub(dcomplex& a, dcomplex& b, dcomplex& c)
{
	c.r = a.r-b.r; c.i = a.i-b.i;
}
inline void sub(float a, dcomplex& b, dcomplex& c)
{
	c.r = a-b.r; c.i = -b.i;
}
inline void sub(dcomplex& a, float b, dcomplex& c)
{
	c.r = a.r-b; c.i = a.i;
}
inline void mul(dcomplex& a, dcomplex& b, dcomplex& c)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i;
	c.r = ar*br-ai*bi; c.i = ar*bi+ai*br;
}
inline void mul(float a, dcomplex& b, dcomplex& c)
{
	c.r = a*b.r; c.i = a*b.i;
}
inline void mul(dcomplex& a, float b, dcomplex& c)
{
	c.r = a.r*b; c.i = a.i*b;
}
inline void div(dcomplex& a, dcomplex& b, dcomplex& c)
{
	float ar=a.r,ai=a.i,br=b.r,bi=b.i,scale=1.0/(br*br+bi*bi);
	c.r = (ar*br+ai*bi)*scale; c.i = (ai*br-ar*bi)*scale; 
}
inline void div(float a, dcomplex& b, dcomplex& c)
{
	float br=b.r,bi=b.i,scale=a/(br*br+bi*bi);
	c.r = br*scale; c.i = -bi*scale; 
}
inline void div(dcomplex& a, float b, dcomplex& c)
{
	float scale=1.0/b;
	c.r = a.r*scale; c.i = a.i*scale; 
}

// non-inline function prototypes
dcomplex sqrt(dcomplex a);
dcomplex cos(dcomplex a);
dcomplex sin(dcomplex a);
dcomplex cosh(dcomplex a);
dcomplex sinh(dcomplex a);
dcomplex exp(dcomplex a);
dcomplex log(dcomplex a);
dcomplex pow(dcomplex a, int p);
dcomplex pow(dcomplex a, double p);
dcomplex pow(double a, dcomplex p);
dcomplex pow(dcomplex a, dcomplex p);

#endif
