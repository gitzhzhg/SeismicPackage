/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUGASSMAN: $Revision: 1.3 $ ; $Date: 2011/11/12 00:40:42 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUGASSMAN - Model reflectivity change with rock/fluid properties	",
"									",
"	sugassman [optional parameters] > data.su			",
"									",
" Optional parameters:							",
" nt=500 	number of time samples					",
" ntr=200	number of traces					",
" dt=0.004 	time sampling interval in seconds			",
" mode=0	model isolated gassmann refl coefficient		",
"		=1 embed gassmann RC in random RC series		",
"		=2 R0 parameter sensitivity output			",
" p=0.15 	parameter sensitivity test range (if mode=2)		",
" .... Environment variables ...					",
" temp=140 	Temperature in degrees C				",
" pres=20 	Pressure in megaPascals					",
" .... Caprock variables ....						",
" v1=37900 	caprock P-wave speed (m/s)				",
" r1=44300 	caprock mass density (g/cc)				",
" .... Reservoir fluid variables ....					",
" g=0.56	Gas specific gravity 0.56 (methane)-1.8 (condensate)	",
" api=50 	Gas specific gravity 10 (heavy)-50 (ultra light)	",
" s=35		Brine salinity in ppm/(1000 000				",
" so=.7 	Oil saturation (0-1)					",
" sg=.2 	Gas saturation (0-1)					",
" .... Reservoir rock frame variables ....				",
" kmin=37900 	Bulk modulus (MPa) of rock frame mineral(s) [default=quartz]",
" mumin=44300 	Shear modulus (MPa) of rock frame mineral(s) [default=quartz]",
" rmin=2.67 	Mass density (g/cc) of rock frame mineral(s) [default=quartz]",
" phi=.24 	Rock frame porosity (0-1)				",
" a=1 		Fitting parameters: Mdry/Mmineral ~ 1/(a + b phi^c)	",
" b=15 		... where M is P-wave modulus and defaults are for	",
" c=1 		... Glenn sandstone [see Liner (2nd ed, table 26.2)]	",
"	h=20 			Reservoir thickness (m)			",
"									",
" Notes:								",
" Creates a reflection coefficient series based on Gassmann		",
" theory of velocity and density for porous elastic media		",
NULL};

/* 
 * Credits: UHouston: Chris Liner	9/23/2009
 *
 * trace header fields set: 
 */
/**************** end self doc ***********************************/

/* Function prototypes */
float rockT0(float h, float kmin, float mumin, float rmin, 
		float phi, float a, float b, float c, float api, 
		float g, float s, float so, float sg, float t, float p);
float rockR0(float v1, float r1, float kmin, float mumin, 
		float rmin, float phi, float a, float b, float c, 
		float api, float g, float s, float so, float sg, 
		float t, float p);
float rockIp(float kmin, float mumin, float rmin, float phi, float a,
		float b, float c, float api, float g, float s, 
		float so, float sg, float t, float p);
float rockVp(float kmin, float mumin, float rmin, float phi, 
		float a, float b, float c, float api, float g, 
		float s, float so, float sg, float t, float p);
float rockDensity(float rmin, float phi, float api, float g, 
			float s, float so, float sg, float t, float p);
float fluidVelocity(float api, float g, float s, float so, 
			float sg, float t, float p);
float fluidModulus(float api, float g, float s, float so, 
			float sg, float t, float p);
float fluidDensity(float api, float g, float s, float so, float sg, 
			float t, float p);
float brineVelocity(float s, float t, float p);
float brineModulus(float s, float t, float p);
float brineDensity(float s, float t, float p);
float oilModulus(float api, float g, float t, float p);
float oilVelocity(float api, float g, float t, float p);
float oilDensity(float api, float g, float t, float p);
float gasVelocity(float g, float t, float p);
float gasModulus(float g, float t, float p);
float gasDensity(float g, float t, float p);

/* C language definitions for use with Mathematica output */
#define Power(x, y)	(pow((double)(x), (double)(y)))
#define Sqrt(x)	 (sqrt((double)(x)))
#define Abs(x)		(fabs((double)(x)))
#define Exp(x)		(exp((double)(x)))
#define Log(x)		(log((double)(x)))
#define Sin(x)		(sin((double)(x)))
#define Cos(x)		(cos((double)(x)))
#define tan(x)		(tan((double)(x)))
#define ArcSin(x)	(asin((double)(x)))
#define ArcCos(x)	(acos((double)(x)))
#define Arctan(x)	(atan((double)(x)))
#define Sinh(x)		(sinh((double)(x)))
#define Cosh(x)		(cosh((double)(x)))
#define tanh(x)		(tanh((double)(x)))
#define E		2.71828182845904523536029
#define pi		3.14159265358979323846264
#define Degree		0.01745329251994329576924

segy tr;

int
main(int argc, char **argv)
{
	int nt;		/* number of time samples		*/
	int ntr;	/* number of traces			*/
	int itr;	/* trace counter			*/
	int mode;	/* operation mode (see selfdoc)		*/
	float dt;	/* time sampling interval		*/
	int it, it1;	/* time sample of gassmann spike	*/
	/* environment variables */
	float temp;	/* temperature in deg C			*/
	float pres;	/* pressure in megaPascals		*/
	/* caprock variables */
	float v1;	/* p-wave speed (m/s) 			*/
	float r1;	/* mass density (g/cc)			*/
	/* reservoir fluid properties */
	float g;	/* gas specific gravity			*/
			/*     0.56 (methane) - 1.8 (condensate)*/
	float api;	/* oil API gravity... 			*/
			/*	10 (heavy) - 50 (ultra light)	*/	
	float s;	/* brine salinity ppm/(1 000 000)	*/ 
	float so, sg;	/* oil and gas saturation 		*/
	/* reservoir rock properties */
	float kmin;	/* bulk modulus (MPa) of frame mineral(s) */
	float mumin;	/* shear modulus (MPa) of frame mineral(s)*/
	float rmin;	/* density (g/cc) of frame mineral(s) 	*/
	float phi;	/* rock frame porosity (0-1)		*/
	float a,b,c;	/* fitting params: 			*/
			/*	Mdry/Mmineral ~ 1/(a + b phi^c) */
	float h;	/* reservoir thickness (m)		*/
	float p;	/* parameter sensitivity test % (if mode=2)*/
	float x, y;	/* temporary variable */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */

	/* get parameters */
	nt = 500;	getparint("nt", &nt);
	CHECK_NT("nt",nt);				tr.ns = nt;
	ntr = 200;	getparint("ntr", &ntr);
	dt = 0.004;	getparfloat("dt", &dt);		tr.dt = dt*1000000;
	mode = 0;	getparint("mode", &mode);
	if (mode != 0 && mode != 1) mode = 0;
	it1 = nt/2;	getparint("it1", &it1);

	/* environment vars */
	temp = 140;		getparfloat("temp", &temp);
	pres = 200;		getparfloat("pres", &pres);
	/* caprock vars */
	v1 = 3300;		getparfloat("v1", &v1);
	r1 = 2.6;		getparfloat("r1", &r1);
	/* fluid vars */
	g = 0.56;		getparfloat("g", &g);
	api = 50;		getparfloat("api", &api);
	s = .35;		getparfloat("s", &s);
	so = .7;		getparfloat("so", &so);
	sg = .2;		getparfloat("sg", &sg);
	/* rock frame vars */
	kmin = 37900;	getparfloat("kmin", &kmin);
	mumin = 44300;	getparfloat("mumin", &mumin);
	rmin = 2.67;	getparfloat("rmin", &rmin);
	phi = 0.24;		getparfloat("phi", &phi);
	a = 1;			getparfloat("a", &a);
	b = 15;			getparfloat("b", &b);
	c = 1;			getparfloat("c", &c);
	h = 20;			getparfloat("h", &h);
	/* mode opertation vars */
	p = .15;		getparfloat("p", &p);
	

	/* select mode */
	if (mode == 0) {
		ntr	= 1;
		nt	= 200;
		tr.ns = nt;
		memset( (void *) tr.data, 0, nt * FSIZE);
		x = sg;
		y = 0.0;
		for (it = 0; it < nt; it++) {
			y = (1 - p) * x + 2 * (float) it * p * x / (float) nt ;
			tr.data[it] = 100 * rockR0(v1,r1,kmin, mumin, rmin, phi, a, b, c, api, g, s, so, y, temp, pres)/
					rockR0(v1,r1,kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, temp, pres);
		}
		tr.f1 = ( 1 - p ) * x;
		tr.d1 = 2 * p * x / nt;
		puttr(&tr);

	}

	if (mode == 2) {
		temp = 0;
		for (itr = 0; itr < ntr; itr++) {
			memset( (void *) tr.data, 0, nt * FSIZE);
			temp += 350 / ntr;
			pres = 100;
			for (it = 0; it < nt; it++) {
				pres -= (100 - .1) / nt;
				tr.data[it] = rockT0(h,kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, temp, pres);
				/* tr.data[it] = rockR0(v1,r1,kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, temp, pres);
				 tr.data[it] = rockIp(kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, temp, pres);
				 tr.data[it] = rockVp(kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, temp, pres);
				 tr.data[it] = rockDensity(rmin, phi, api, g, s, so, sg, temp, pres);
				 tr.data[it] = fluidVelocity(api, g, s, so, sg, temp, pres);
				 tr.data[it] = fluidModulus(api, g, s, so, sg, temp, pres);
				 tr.data[it] = fluidDensity(api, g, s, so, sg, temp, pres);
				 tr.data[it] = brineVelocity(s, temp, pres);
				 tr.data[it] = brineModulus(s, temp, pres);
				 tr.data[it] = brineDensity(s, temp, pres);
				 tr.data[it] = oilModulus(api, g, temp, pres);
				 tr.data[it] = oilVelocity(api, g, temp, pres);
				 tr.data[it] = oilDensity(api, g, temp, pres);
				 tr.data[it] = gasVelocity(g, temp, pres);
				 tr.data[it] = gasModulus(g, temp, pres);
				 tr.data[it] = gasDensity(g, temp, pres); */
			}
			puttr(&tr);
		}
		
	}
	

	return(CWP_Exit());
}


float rockT0(float h, float kmin, float mumin, float rmin, 
		float phi, float a, float b, float c, 
		float api, float g, float s, float so, 
		float sg, float t, float p)
/*******************************************************************
rockTO - Calculate P-wave normal incidence reflection coefficient of 
	 a fluid saturated reservoir rock. 
*******************************************************************
Notes:
Caprock velocity is v1 (m/s) and density is r1 (g/cc).
Constituent mineral properties are bulk modulus kmin (MPa),
shear modulus \[Mu]min (MPa), and density rmin (g/cc).	Porosity 
is phi (Range: 0-1) and (a,b,c) are empirical frame stiffness 
constants used as mdry/mmin=(a+b phi^c). The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float v;
	float x;

	v = rockVp(kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, t, p);
	x = 2 * h / v;

	return x;
}

float rockR0(float v1, float r1, float kmin, float mumin,
		float rmin, float phi, float a, float b, float c, 
		float api, float g, float s, float so, 
		float sg, float t, float p)
/*******************************************************************
rockRO - Calculate P-wave normal incidence reflection coefficient of a fluid saturated reservoir rock. 
*******************************************************************
Notes:
Caprock velocity is v1 (m/s) and density is r1 (g/cc).
Constituent mineral properties are bulk modulus kmin (MPa),
shear modulus \[Mu]min (MPa), and density rmin (g/cc).	Porosity 
is phi (Range: 0-1) and (a,b,c) are empirical frame stiffness 
constants used as mdry/mmin=(a+b phi^c). The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float ip1, ip2;
	float x;

	ip1 = v1 * r1;
	ip2 = rockIp(kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, t, p);
	x = (ip2 - ip1)/(ip2 + ip1);

	return x;
}


float rockIp(float kmin, float mumin, float rmin, float phi,
		float a, float b, float c, float api, float g,
		float s, float so, float sg, float t, float p)
/*******************************************************************
rockIp - Calculate P-wave acoustic impedance (m/s)*(g/cc) of a fluid 
	 saturated reservoir rock. 
********************************************************************
Notes:
Constituent mineral properties are bulk modulus kmin (MPa),
shear modulus \[Mu]min (MPa), and density rmin (g/cc).	Porosity 
is phi (Range: 0-1) and (a,b,c) are empirical frame stiffness 
constants used as mdry/mmin=(a+b phi^c). The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float rsat, vsat;
	float x;

	rsat = rockDensity(rmin, phi, api, g, s, so, sg, t, p);
	vsat = rockVp(kmin, mumin, rmin, phi, a, b, c, api, g, s, so, sg, t, p);
	x = rsat * vsat;

	return x;
}

float rockVp(float kmin, float mumin, float rmin, float phi,
		float a, float b, float c, float api, float g,
		float s, float so, float sg, float t, float p)
/*******************************************************************
rockVp - Calculate P-wave speed (m/s) of a fluid saturated reservoir rock.	
********************************************************************
Notes:
Constituent mineral properties are bulk modulus kmin (MPa), 
shear modulus \[Mu]min (MPa), and density rmin (g/cc).	Porosity 
is phi (Range: 0-1) and (a,b,c) are empirical frame stiffness 
constants used as mdry/mmin=(a+b phi^c). The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float kf, mmin, mdry, rhs, msat, rsat;
	float x;

	kf	= fluidModulus(api, g, s, so, sg, t, p);
	mmin = kmin + 4 * mumin / 3;
	mdry = mmin /(a + b * Power(phi,c));
	rhs	= mdry / (mmin - mdry) + kf / ( phi * (mmin - kf) );
	msat = mmin * rhs / (1 + rhs);
	rsat = rockDensity(rmin, phi, api, g, s, so, sg, t, p);
	x = Sqrt(1000 * msat / rsat);

	return x;
}

float rockDensity(float rmin, float phi, float api, float g, float s, 
			float so, float sg, float t, float p)
/*******************************************************************
rockDensity - Calculate mass density (g/cc) of a fluid 
		saturated reservoir rock.	\[Continuation]
********************************************************************
Notes:
The mineral density of the rock is rmin, and the porosity \[Continuation]
is phi (Range: 0-1).	The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight 
fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is 
Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x1, x;

	x1 = fluidDensity(api, g, s, so, sg, t, p);
	x = rmin * (1 - phi) + phi * x1;

	return x;
}

float fluidVelocity(float api, float g, float s, float so,
			float sg, float t, float p)
/*******************************************************************
fluidVelocity - Calculate sound speed (m/s) for a reservoir fluid mix. 
********************************************************************
The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight 
fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is 
Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x1, x2, x;

	x1 = fluidModulus(api, g, s, so, sg, t, p);
	x2 = fluidDensity(api, g, s, so, sg, t, p);
	x = Sqrt( 1000 * x1 / x2 );

	return x;
}

float fluidModulus(float api, float g, float s, float so, 
			float sg, float t, float p)
/*******************************************************************
fluidModulus - Calculate bulk modulus (MPa) of a reservoir fluid mix. 
********************************************************************
The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight 
fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is 
Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float brine, gas, oil;
	float x;

	if ( so + sg > 1 ) err("so + sg > 1...impossible");

	brine = brineModulus(s,t,p);
	gas	= gasModulus(g,t,p);
	oil	= oilModulus(api,g,t,p);

	x = 1.0 / ( (1 - so - sg)/brine + so/oil + sg/gas );

	return x;
}

float fluidDensity(float api, float g, float s, float so,
			float sg, float t, float p)
/*******************************************************************
fluidDensity - Calculate mass density (g/cc) of a reservoir fluid mix. 
********************************************************************
Notes:
The oil component is characterized by oil gravity 
API, gas by specific gravity G, and brine by weight 
fraction of sodium chloride S (ppm/1,000,000).	Oil 
saturation is So (Range: 0-1) and gas saturation is 
Sg (Range: 0-1).	Water saturation is calculated 
from 1-So-Sg. T is temperature (deg C) and P is pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float brine, gas, oil;
	float x;

	if ( so + sg > 1 ) err("so + sg > 1...impossible");

	brine = brineDensity(s,t,p);
	gas	= gasDensity(g,t,p);
	oil	= oilDensity(api,g,t,p);

	x = (1 - so - sg)*brine + so*oil + sg*gas;

	return x;
}

float brineModulus(float s, float t, float p)
/*******************************************************************
brineModulus - Calculate bulk modulus (MPa) of saline solution characterized 
	  by weight fraction of sodium chloride S (ppm/1,000,000) as 
	  a function of temperature (deg C) and pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = (Power(1402.85 + 1.524*p + 0.003437*Power(p,2) - 
	0.000011970000000000002*Power(p,3) + 
	(780 - 10*p + 0.16*Power(p,2))*Power(s,1.5) - 
	1820*Power(s,2) + 4.871*t - 0.0111*p*t + 
	0.00017390000000000003*Power(p,2)*t - 
	1.628e-6*Power(p,3)*t - 0.04783*Power(t,2) + 
	0.0002747*p*Power(t,2) - 
	2.135e-6*Power(p,2)*Power(t,2) + 
	1.2370000000000001e-8*Power(p,3)*Power(t,2) + 
	0.0001487*Power(t,3) - 
	6.502999999999999e-7*p*Power(t,3) - 
	1.4550000000000001e-8*Power(p,2)*Power(t,3) + 
	1.327e-10*Power(p,3)*Power(t,3) - 
	2.197e-7*Power(t,4) + 7.987e-10*p*Power(t,4) + 
	5.23e-11*Power(p,2)*Power(t,4) - 
	4.614e-13*Power(p,3)*Power(t,4) + 
	s*(1170 + 2.6*p - 0.0476*Power(p,2) - 9.6*t - 
		0.0029*p*t + 0.055*Power(t,2) - 
		0.000085*Power(t,3)),2)*
	(1 + (489*p - 0.333*Power(p,2) - 80*t - 2*p*t - 
		0.002*Power(p,2)*t - 3.3*Power(t,2) + 
		0.016*p*Power(t,2) + 0.00175*Power(t,3) - 
		0.000013000000000000001*p*Power(t,3))/1.e6 + 
	s*(0.668 + 0.44*s + 
		(300*p - 2400*p*s + 
		t*(80 - 13*p - 3300*s + 47*p*s + 3*t))/1.e6
		)))/
	(1000.*(1 + 0.0494*Power(10,
	 -4 - (7.786*s)/Power(17.78 + t,0.306))*
	(3676*Power(p,0.64) + 
		0.712*p*Power(Abs(-76.71 + t),1.5))));

	return x;
}

float brineVelocity(float s, float t, float p)
/*******************************************************************
brineVelocity - Calculate sound speed (m/s) of saline solution characterized 
	  by weight fraction of sodium chloride S (ppm/1,000,000) as 
	  a function of temperature (deg C) and pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = 1402.85 + 1.524*p + 0.003437*Power(p,2) - 
	0.000011970000000000002*Power(p,3) + 
	(780 - 10*p + 0.16*Power(p,2))*Power(s,1.5) - 
	1820*Power(s,2) + 4.871*t - 0.0111*p*t + 
	0.00017390000000000003*Power(p,2)*t - 
	1.628e-6*Power(p,3)*t - 0.04783*Power(t,2) + 
	0.0002747*p*Power(t,2) - 
	2.135e-6*Power(p,2)*Power(t,2) + 
	1.2370000000000001e-8*Power(p,3)*Power(t,2) + 
	0.0001487*Power(t,3) - 
	6.502999999999999e-7*p*Power(t,3) - 
	1.4550000000000001e-8*Power(p,2)*Power(t,3) + 
	1.327e-10*Power(p,3)*Power(t,3) - 
	2.197e-7*Power(t,4) + 7.987e-10*p*Power(t,4) + 
	5.23e-11*Power(p,2)*Power(t,4) - 
	4.614e-13*Power(p,3)*Power(t,4) + 
	s*(1170 + 2.6*p - 0.0476*Power(p,2) - 9.6*t - 
	0.0029*p*t + 0.055*Power(t,2) - 
	0.000085*Power(t,3));

	return x;
}


float brineDensity(float s, float t, float p)
/*******************************************************************
brineDensity - Calculate mass density (g/cc) of saline solution characterized 
		by weight fraction of sodium chloride S (ppm/1,000,000) as 
		a function of temperature (deg C) and pressure (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = 1 + (489*p - 0.333*Power(p,2) - 80*t - 2*p*t - 
	0.002*Power(p,2)*t - 3.3*Power(t,2) + 
	0.016*p*Power(t,2) + 0.00175*Power(t,3) - 
	0.000013000000000000001*p*Power(t,3))/1.e6 + 
	s*(0.668 + 0.44*s + 
	(300*p - 2400*p*s + 
	 t*(80 - 13*p - 3300*s + 47*p*s + 3*t))/1.e6);

	return x;
}


float oilModulus(float api, float g, float t, float p)
/*******************************************************************
oilModulus - Calculate bulk modulus (MPa) of live oil (including solution gas)
	characterized by oil gravity API, gas specific gravity G, 
	at temperature T (deg C) and pressure P (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = oilDensity(api,g,t,p) * Power( oilVelocity(api,g,t,p), 2) / 1000.;

	return x;
}

float oilVelocity(float api, float g, float t, float p)
/*******************************************************************
oilVelocity - Calculate sound speed (m/s) of live oil (including solution gas)
	characterized by oil gravity API, gas specific gravity G, 
	at temperature T (deg C) and pressure P (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = 4.64*p - 3.7*t + 24932.71072306419*
	Power(1/
	((131.5 + api)*(1 + 
		0.0020299999999999997*g*
		Power(Power(E,0.02878*api - 0.00377*t)*p,
		1.205))*(0.972 + 
		0.00038*Power(17.8 + 
		0.409570869105443*g*Sqrt((131.5 + api)*g)*
		Power(Power(E,0.02878*api - 0.00377*t)*p,
		1.205) + t,1.175))*
	(2.6 - 141.5/
		((131.5 + api)*
		(1 + 0.0020299999999999997*g*
		Power(Power(E,0.02878*api - 0.00377*t)*
			p,1.205))*
		(0.972 + 0.00038*
		Power(17.8 + 
			0.409570869105443*g*
			Sqrt((131.5 + api)*g)*
			Power(Power(E,
			0.02878*api - 0.00377*t)*p,1.205) + 
			t,1.175))))),0.5) + 
	0.0115*p*t*(-1 + 4.12*
	Sqrt(-1 + 0.007632508833922262*(131.5 + api)*
		(1 + 0.0020299999999999997*g*
		Power(Power(E,0.02878*api - 0.00377*t)*p,
		1.205))*(0.972 + 
		0.00038*Power(17.8 + 
		0.409570869105443*g*
		Sqrt((131.5 + api)*g)*
		Power(Power(E,0.02878*api - 0.00377*t)*
			p,1.205) + t,1.175))));

	return x;
}


float oilDensity(float api, float g, float t, float p)
/*******************************************************************
oilDensity - Calculate mass density (g/cc) of live oil (including solution gas) 
	characterized by oil gravity API, gas specific gravity G, 
	at temperature T (deg C) and pressure P (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = 0.000349*p + (141.5/(131.5 + api) + 
	0.0024359999999999994*Power(g,2)*
	Power(Power(E,0.02878*api - 0.00377*t)*p,1.205))/
	(0.972 + 0.00038*Power(17.8 + 
	 0.409570869105443*g*Sqrt((131.5 + api)*g)*
		Power(Power(E,0.02878*api - 0.00377*t)*p,
		1.205) + t,1.175)) + 
	(0.00277*p - 1.7099999999999998e-7*Power(p,3))*
	Power(-1.15 + (141.5/(131.5 + api) + 
	 0.0024359999999999994*Power(g,2)*
		Power(Power(E,0.02878*api - 0.00377*t)*p,
		1.205))/
	(0.972 + 0.00038*
		Power(17.8 + 
		0.409570869105443*g*Sqrt((131.5 + api)*g)*
		Power(Power(E,0.02878*api - 0.00377*t)*p,
		1.205) + t,1.175)),2);

	return x;
}


float gasVelocity(float g, float t, float p)
/*******************************************************************
gasVelocity - Calculate sound speed (m/s) of natural gas characterized by 
	specific gravity G, at temperature T (deg C) and pressure P (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = sqrt( ( 1000. * gasModulus(g,t,p) ) / gasDensity(g,t,p) );

	return x;
}


float gasModulus(float g, float t, float p)
/*******************************************************************
gasModulus - Calculate bulk modulus (MPa) of natural gas characterized by 
	specific gravity G, at temperature T (deg C) and pressure P (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = (p*(0.85 - 8.7/
	Power(E,0.65*(1 + p/(4.892 - 0.4048*g))) + 
	5.6/(2 + p/(4.892 - 0.4048*g)) + 
	27.1/Power(3.5 + p/(4.892 - 0.4048*g),2)))/
	(1 - (p*(0.03 + 0.00527*
		Power(3.5 - (273.15 + t)/(94.72 + 170.75*g),
		3) + (0.0228*
		Power(E,((94.72 + 170.75*g)*
		 Power(p/(4.892 - 0.4048*g),1.2)*
		 (-0.45 - 
			8*Power(0.56 - 
			(94.72 + 170.75*g)/(273.15 + t),2)))
		/(273.15 + t))*(94.72 + 170.75*g)*
		Power(p/(4.892 - 0.4048*g),0.2)*
		Power(3.85 - 
		(273.15 + t)/(94.72 + 170.75*g),2)*
		(-0.45 - 8*
		Power(0.56 - 
			(94.72 + 170.75*g)/(273.15 + t),2)))/
		(273.15 + t)))/
	((4.892 - 0.4048*g)*
	(-0.52 + (0.642*(273.15 + t))/
		(94.72 + 170.75*g) - 
		(0.007*Power(273.15 + t,4))/
		Power(94.72 + 170.75*g,4) + 
		0.019*Power(3.85 - 
		(273.15 + t)/(94.72 + 170.75*g),2) + 
		(p*(0.03 + 0.00527*
		Power(3.5 - 
			(273.15 + t)/(94.72 + 170.75*g),3)))/
		(4.892 - 0.4048*g))));

	return x;
}


float gasDensity(float g, float t, float p)
/*******************************************************************
gasDensity - Calculate mass density (g/cc) of natural gas 
	characterized by specific gravity G, at temperature T 
	(deg C) and pressure P (MPa)
*******************************************************************
Credits: UHouston: Chris Liner	9/23/2009
*******************************************************************/
{
	float x;

	x = (3.4638699124410666*g*p)/
	((273.15 + t)*(-0.52 + 
	(0.642*(273.15 + t))/(94.72 + 170.75*g) - 
	(0.007*Power(273.15 + t,4))/
	Power(94.72 + 170.75*g,4) + 
	0.109*Power(E,((94.72 + 170.75*g)*
		Power(p/(4.892 - 0.4048*g),1.2)*
		(-0.45 - 8*
		Power(0.56 - 
		 (94.72 + 170.75*g)/(273.15 + t),2)))/
		(273.15 + t))*
	Power(3.85 - (273.15 + t)/(94.72 + 170.75*g),2)
	+ (p*(0.03 + 0.00527*
		Power(3.5 - 
		(273.15 + t)/(94.72 + 170.75*g),3)))/
	(4.892 - 0.4048*g)));

	return x;
}
