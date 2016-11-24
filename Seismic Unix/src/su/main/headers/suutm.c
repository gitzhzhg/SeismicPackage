/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUUTM: $Revision: 1.5 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" SUUTM - UTM projection of longitude and latitude in SU trace headers  ",
"                                                                       ",
" suutm <stdin >stdout [optional parameters]                            ",
"                                                                       ",
" Optional parameters:                                                  ",
"    counit=(from header) input coordinate units code:                  ",
"                    =1: length (meters or feet; no UTM projection)     ",
"                    =2: seconds of arc                                 ",
"                    =3: decimal degrees                                ",
"                    =4: degrees, minutes, seconds                      ",
"    idx=23          reference ellipsoid index (default is WGS 1984)    ",
"    a=(from idx)    user-specified semimajor axis of ellipsoid         ",
"    f=(from idx)    user-specified flattening of ellipsoid             ",
"    zkey=           if set, header key to store UTM zone number        ",
"    verbose=0       =1: echo ellipsoid parameters                      ",
"                                                                       ",
"    lon0=           central meridian for TM projection in degrees      ",
"                    (default uses the 60 standard UTM longitude zones) ",
"    xoff=500000     false Easting (default: UTM)                       ",
"    ysoff=10000000  false Northing, southern hemisphere (default: UTM) ",
"    ynoff=0         false Northing, northern hemisphere (default: UTM) ",
"                                                                       ",
" Notes:                                                                ",
"    Universal Transverse Mercator (UTM) coordinates are defined between",
"    latitudes 80S (-80) and 84N (84). Longitude values must be between ",
"    -180 degrees (west) and 179.999... degrees (east).                 ",
"                                                                       ",
"    Latitudes are read from sy and gy (N positive), and longitudes     ",
"    are read from sx and gx (E positive).                              ",
"    The UTM zone is determined from the receiver coordinates gy and gx.",
"                                                                       ",
"    Use suazimuth to calculate shot-receiver azimuths and offsets.     ",
"                                                                       ",
" Reference ellipsoids:                                                 ",
"    An ellipsoid may be specified by its semimajor axis a and its      ",
"    flattening f, or one of the following ellipsoids may be selected   ",
"    by its index idx (semimajor axes in meters):                       ",
"     0  Sphere with radius of 6371000 m                                ",
"     1  Airy 1830                                                      ",
"     2  Australian National 1965                                       ",
"     3  Bessel 1841 (Ethiopia, Indonesia, Japan, Korea)                ",
"     4  Bessel 1841 (Namibia)                                          ",
"     5  Clarke 1866                                                    ",
"     6  Clarke 1880                                                    ",
"     7  Everest (Brunei, E. Malaysia)                                  ",
"     8  Everest (India 1830)                                           ",
"     9  Everest (India 1956)                                           ",
"    10  Everest (Pakistan)                                             ",
"    11  Everest (W. Malaysia, Singapore 1948)                          ",
"    12  Everest (W. Malaysia 1969)                                     ",
"    13  Geodetic Reference System 1980 (GRS 1980)                      ",
"    14  Helmert 1906                                                   ",
"    15  Hough 1960                                                     ",
"    16  Indonesian 1974                                                ",
"    17  International 1924 / Hayford 1909                              ",
"    18  Krassovsky 1940                                                ",
"    19  Modified Airy                                                  ",
"    20  Modified Fischer 1960                                          ",
"    21  South American 1969                                            ",
"    22  World Geodetic System 1972 (WGS 1972)                          ",
"    23  World Geodetic System 1984 (WGS 1984) / NAD 1983               ",
"                                                                       ",
NULL};

/* UTM grid:
 * The Universal Transverse Mercator (UTM) system is a world wide
 * coordinate system defined between 80S and 84N. It divides the
 * Earth into 60 six-degree zones. Zone number 1 has its central
 * meridian at 177W (-177 degrees), and numbers increase eastward.
 *
 * Within each zone, an Easting of 500,000 m is assigned to its 
 * central meridian to avoid negative coordinates. On the northern
 * hemisphere, Northings start at 0 m at the equator and increase 
 * northward. On the southern hemisphere a false Northing of 
 * 10,000,000 m is applied, i.e. Northings start at 10,000,000 m at 
 * the equator and decrease southward.
 *
 * Coordinate encoding (sx,sy,gx,gy):
 *    counit=1  units of length (coordinates are not converted)
 *    counit=2  seconds of arc
 *    counit=3  decimal degrees 
 *    counit=4  degrees, minutes and seconds encoded as integer DDDMMSS 
 *              with scalco=1 or DDDMMSS.ss with scalco=-100 (see segy.h)
 * Units of length are also assumed, if counit <= 0 or counit >= 5.
 *
 *
 * Author: 
 *    Nils Maercklin, RISSC, University of Naples, Italy, March 2007
 *
 * References:
 * NIMA (2000). Department of Defense World Geodetic System 1984 - 
 *    its definition and relationships with local geodetic systems.
 *    Technical Report TR8350.2. National Imagery and Mapping Agency, 
 *    Geodesy and Geophysics Department, St. Louis, MO. 3rd edition.
 * J. P. Snyder (1987). Map Projections - A Working Manual. 
 *    U.S. Geological Survey Professional Paper 1395, 383 pages.
 *    U.S. Government Printing Office.
 *
 *
 * Trace header fields accessed: sx, sy, gx, gy, scalco, counit
 * Trace header fields modified: sx, sy, gx, gy, scalco, counit
 */
/**************** end self doc ***********************************/

/* Number of pre-defined reference ellipsoids */
#define N_ELLIPSOIDS 24

/* Data structure for reference ellipsoid */
typedef struct {
    double a;       /* semimajor axis in m */
    double f;       /* flattening */
    char name[48];  /* ellipsoid name */
} RefEllipsoid;


/* Function prototypes */
void setval( cwp_String type_out, Value *val_out, double dval_out);
void decodeCoordinates(double *xout, double *yout, 
    double x, double y, int scalco, int counit);
void convLLtoTM(double a, double f, double lat, double lon, \
    double lon0, double *x, double *y);
short getUTMZone(double lat, double lon);
RefEllipsoid getRefEllipsoid(int idx);


segy tr;

int 
main (int argc, char **argv)
{
    int idx;                /* reference ellipsoid index */
    RefEllipsoid ellip;     /* reference ellipsoid */
    double a;               /* semimajor axis of ellipsoid */
    double f;               /* flattening of ellipsoid */
    short zone;             /* UTM zone number */
    double lon0;            /* central meridian in degrees */
    double xoff;            /* false Easting */
    double ynoff;           /* false Northing on N hemisphere */
    double ysoff;           /* false Northing on S hemisphere */
    double sx, sy;          /* Shot coordinates (lat/lon, UTM) */
    double gx, gy;          /* Receiver coordinates (lat/lon, UTM) */
    int verbose=0;          /* verbose flag */
    int counit=0;           /* user-specified coordinate units code */
    int userlon0=0;         /* flag for user-specified central meridian */
    cwp_String zkey=NULL;   /* output key for UTM zone number */
    cwp_String ztype=NULL;  /* ...type for output key */
    int zindex=0;           /* ...index for output key */
    Value zval;             /* ...value of output key */

    /* hook up getpar */
    initargs(argc, argv);
    requestdoc(1);

    /* Get parameters */
    if (!getparint("idx", &idx))         idx=23;
    if (!getparint("verbose", &verbose)) verbose=0;
    if (!getparint("counit", &counit))   counit=0;
    if (!getparstring("zkey", &zkey))    zkey=NULL;
    if (!getpardouble("xoff", &xoff))    xoff=500000.0;
    if (!getpardouble("ynoff", &ynoff))  ynoff=0.0;
    if (!getpardouble("ysoff", &ysoff))  ysoff=10000000.0;
    if (getpardouble("lon0", &lon0))     userlon0=1;

    /* Check parameters */
    if (idx<0 || idx>=N_ELLIPSOIDS) err("unknown ellipsoid idx=%d", idx);
    if (userlon0 && (lon0<-180.0 || lon0>180.0)) \
        err("lon0=%g must be between -180 and 180", lon0);

    /* Get key type and index for output zkey */
    if (zkey) {
        ztype  = hdtype(zkey);
        zindex = getindex(zkey);
    }

    /* Select reference ellipsoid by index */
    ellip = getRefEllipsoid(idx);
    a = ellip.a;
    f = ellip.f;

    /* Get user-specified ellipsoid parameters */
    if (getpardouble("a", &a)) {
        if (a<=0.0) err("semimajor axis a=%g must be positive", a);
        strcpy(ellip.name, "User-specified\0");
    }
    if (getpardouble("f", &f)) {
        if (f<0.0) err("flattening f=%g must be non-negative", f);
        strcpy(ellip.name, "User-specified\0");
    }    
    checkpars();

    /* Print ellipsoid parameters (and lon0, if specified) */
    if (verbose) {
        warn("ellipsoid: %s", ellip.name);
        if (userlon0) warn("a=%.12g f=%.12g lon0=%g", a, f, lon0);
        else warn("a=%.12g f=%.12g", a, f);
    }

    /* Loop over traces and convert latitude/longitude to UTM */
    while (gettr(&tr)) {

        /* Read source and receiver coordinates from header */
        if (counit) tr.counit=counit;
        decodeCoordinates(&sx, &sy, (double) tr.sx, (double) tr.sy, \
            tr.scalco, tr.counit);
        decodeCoordinates(&gx, &gy, (double) tr.gx, (double) tr.gy, \
            tr.scalco, tr.counit);

        /* Coordinates are converted only if tr.counit>=2 */
        if (tr.counit>=2 && tr.counit<=4) {

            /* Check coordinates */
            if (gx==180.0) gx=-180;
            if (sx==180.0) sx=-180;

            if (gy<-89.999 || gy>89.999 || gx<-180. || gx>=180.) {
                err("invalid coordinates: tracl=%d gx=%g gy=%g", \
                    tr.tracl, gx, gy);
            }
            if (sy<-89.999 || sy>89.999 || sx<-180. || sx>=180.) {
                err("invalid coordinates: tracl=%d sx=%g sy=%g", \
                    tr.tracl, sx, sy);
            }

            /* UTM zone determined from receiver coordinates */
            zone = getUTMZone(gy, gx);

            /* UTM zone and central meridian in degrees */
            if (!userlon0) {
                zone = getUTMZone(gy, gx);
                lon0 = (double) ((zone-1)*6 - 180 + 3);

                /* Write (signed) zone number to header */
                if (zkey) {
                    if (gy<0.0) zone *= -1;
                    setval(ztype,&zval,(double) zone);
                    puthval(&tr, zindex, &zval);
                }
            }

            /* Transverse Mercator projection */
            convLLtoTM(a, f, gy, gx, lon0,  &gx, &gy);
            convLLtoTM(a, f, sy, sx, lon0,  &sx, &sy);

            /* Add false Easting and Northing (standard UTM) */
            gx += xoff;
            sx += xoff;
            gy += (gy<0.0) ? ysoff : ynoff;
            sy += (gy<0.0) ? ysoff : ynoff;

            /* Change trace header */
            tr.counit = 1;
            tr.scalco = 1;
            tr.gx = NINT(gx);
            tr.gy = NINT(gy);
            tr.sx = NINT(sx);
            tr.sy = NINT(sy);
        }

        /* Write trace to stdout */
        puttr(&tr);
    }

    return (CWP_Exit());

} /* end of main */


/************************************************************************/
/* Functions used internally                                            */
/************************************************************************/


RefEllipsoid getRefEllipsoid(int idx)
/************************************************************************
getRefEllipsoid - select a reference ellipsoid by index

*************************************************************************
Input:
idx     index identifying reference ellipsoid

Output:
        returns the ellipsoid parameters

*************************************************************************
Notes:
Does computations as doubles. The index idx is a number between 
0 and N_ELLIPSOIDS. The ellipsoid data structure is defined as
typedef struct {
    double a;        semimajor axis in m
    double f;        flattening
    char name[48];   ellipsoid name
} RefEllipsoid;

Reference:
NIMA (2000). Department of Defense World Geodetic System 1984 - Its 
    definition and relationships with local geodetic systems.
    Technical Report TR8350.2. National Imagery and Mapping Agency, 
    Geodesy and Geophysics Department, St. Louis, MO. 3rd edition.
*************************************************************************
Author: Nils Maercklin, 30 March 2007
*************************************************************************/
{
    /* Pre-defined reference ellipsoids */
    const RefEllipsoid ellip[N_ELLIPSOIDS]={
/* 0*/  { 6371000.,    0.0,              "Sphere\0" }, 
/* 1*/  { 6377563.396, 1./299.3249646,   "Airy 1830\0" }, 
/* 2*/  { 6378160.,    1./298.25,        "Australian National 1965\0" }, 
/* 3*/  { 6377397.155, 1./299.1528128,   \
             "Bessel 1841 (Ethiopia, Indonesia, Japan, Korea)\0" }, 
/* 4*/  { 6377483.865, 1./299.1528128,   "Bessel 1841 (Namibia)\0" }, 
/* 5*/  { 6378206.4,   1./294.9786982,   "Clarke 1866\0" }, 
/* 6*/  { 6378249.145, 1./293.465,       "Clarke 1880\0" }, 
/* 7*/  { 6377298.556, 1./300.80178,     \
             "Everest (Brunei, E. Malaysia)\0" }, 
/* 8*/  { 6377276.345, 1./300.80178,     "Everest (India 1830)\0" }, 
/* 9*/  { 6377301.243, 1./300.80178,     "Everest (India 1956)\0" }, 
/*10*/  { 6377309.613, 1./300.80178,     "Everest (Pakistan)\0" }, 
/*11*/  { 6377304.063, 1./300.80178,     \
             "Everest (W. Malaysia, Singapore 1948)\0" }, 
/*12*/  { 6377295.664, 1./300.80178,     "Everest (W. Malaysia 1969)\0" }, 
/*13*/  { 6378137.,    1./298.257222101, \
             "Geodetic Reference System 1980 (GRS 1980)\0" }, 
/*14*/  { 6378200.,    1./298.3,         "Helmert 1906\0" }, 
/*15*/  { 6378270.,    1./297.,          "Hough 1960\0" }, 
/*16*/  { 6378160.,    1./298.247,       "Indonesian 1974\0" }, 
/*17*/  { 6378388.,    1./297.,          \
             "International 1924 / Hayford 1909\0" }, 
/*18*/  { 6378245.,    1./298.3,         "Krassovsky 1940\0" }, 
/*19*/  { 6377340.189, 1./299.3249646,   "Modified Airy\0" }, 
/*20*/  { 6378155.,    1./298.3,         "Modified Fischer 1960\0" }, 
/*21*/  { 6378160.,    1./298.25,        "South American 1969\0" }, 
/*22*/  { 6378135.,    1./298.26,        "WGS 1972\0" }, 
/*23*/  { 6378137.,    1./298.257223563, "WGS 1984 / NAD 1983\0" }
    };

    /* Set idx to default, if outside range */
    if (idx<0 || idx>=N_ELLIPSOIDS) idx=23;

    /* Return reference ellipsoid parameters */
    return ellip[idx];
}


short getUTMZone(double lat, double lon)
/************************************************************************
getUTMZone - get the UTM zone number

*************************************************************************
Input:
lat     geographical latitude in degrees
lon     geographical longitude in degrees

Output:
        returns the UTM zone number

*************************************************************************
Notes:
Does computations as doubles. The latitude is positive on the northern
hemisphere and negative on the southern hemisphere. UTM coordinates
are defined between 80S and 84N. 
The longitude is negative west of the zero-meridian (Greenwich), i.e. 
its range of values is -180.0 ... 179.99999.
*************************************************************************
Author: Nils Maercklin, 30 March 2007
*************************************************************************/
{
    short zone; /* UTM zone number */

    /* Make sure the longitude is between -180 and 179.999 deg */
    lon = (lon+180.)-floor((lon+180.)/360.)*360.-180.;

    /* Zone number */
    zone = (short)((lon + 180.)/6.) + 1;
    if (lat >= 56.0 && lat < 64.0 && lon >= 3.0 && lon < 12.0) zone = 32;

    /* Svalbard zones */
    if (lat >= 72.0 && lat < 84.0) {
        if      (lon >= 0.0  && lon <  9.0) zone = 31;
        else if (lon >= 9.0  && lon < 21.0) zone = 33;
        else if (lon >= 21.0 && lon < 33.0) zone = 35;
        else if (lon >= 33.0 && lon < 42.0) zone = 37;
    }

    /* Return zone number */
    return zone;
}


void convLLtoTM(double a, double f, double lat, double lon, \
    double lon0, double *x, double *y)
/************************************************************************
convLLtoTM - convert latitude and longitude to Transverse Mercator 
             Easting and Northing (e.g. UTM)
*************************************************************************
Input:
a       ellipsoid simimajor axis (in m)
f       ellipsoid flattening
lat     geographical latitude in degrees
lon     geographical longitude in degrees
lon0    central meridian (longitude) in degrees

Output:
x       Easting (Transverse Mercator grid)
y       Northing (Transverse Mercator grid)

*************************************************************************
Notes:
Does computations as doubles. The longitude is negative west of the 
zero-meridian (Greenwich), i.e. its range of values is -180.0 ... 179.99999.
The latitude is positive on the northern hemisphere and negative on the 
southern hemisphere. 
For standard UTM coordinates add always a false Easting of 500 km and a 
false Northing of 10000 km on the southern hemisphere. UTM coordinates 
are only defined between 80S and 84N. 

Reference:
J. P. Snyder (1987). Map Projections - A Working Manual. 
    U.S. Geological Survey Professional Paper 1395, 383 pages.
    U.S. Government Printing Office.

This function is adopted from a Perl routine in Geo-Coordinates-UTM-0.06
by G. Crookham (CPAN, March 2007).
*************************************************************************
Author: Nils Maercklin, 30 March 2007
*************************************************************************/
{
    double e2;   /* eccentricity squared, e2 = f(2-f) */
    double ep2;  /* eccentricity prime squared */
    double k0, cn, ct, cc, ca, cm;

    /* Convert latitude and longitudes to radians */
    lat  *= PI / 180.0;
    lon  *= PI / 180.0;
    lon0 *= PI / 180.0;

    /* Ellipsoid parameters */
    e2  = f * (2.0 - f);
    ep2 = e2 / (1.0 - e2);

    /* Some constants */
    k0 = 0.9996;
    cn = a / sqrt(1.0-e2*sin(lat)*sin(lat));
    ct = tan(lat)*tan(lat);
    cc = ep2*cos(lat)*cos(lat);
    ca = cos(lat)*(lon-lon0);

    cm = a * ((1.0 - e2/4. - 3.*e2*e2/64. - 5.*e2*e2*e2/256.)*lat \
         - (3.*e2/8. + 3.*e2*e2/32.+  45.*e2*e2*e2/1024.)*sin(2.*lat) \
         + (15.*e2*e2/256. + 45*e2*e2*e2/1024.)*sin(4.*lat) \
         - (35.*e2*e2*e2/3072.)*sin(6.*lat));

    /* Transverse Mercator Easting */
    (*x) = k0 * cn * (ca + (1.-ct+cc) * ca*ca*ca/6. \
           + (5. - 18.*ct + ct*ct + 72.*cc - 58.*ep2) * ca*ca*ca*ca*ca/120.);

    /* Transverse Mercator Northing */
    (*y) = k0 * (cm + cn*tan(lat) * (ca*ca/2. + \
           (5. - ct + 9.*cc + 4.*cc*cc) * ca*ca*ca*ca/24. \
           + (61. - 58.*ct + ct*ct + 600.*cc - 330.*ep2) * \
             ca*ca*ca*ca*ca*ca/720.));
}


void decodeCoordinates(double *xout, double *yout, 
            double x, double y, int scalco, int counit) 
/************************************************************************
decodeCoordinates - decode and scale coordinates sx,sy,gx,gy from trace
                    header according to values of scalco and counit
*************************************************************************
Input:
x       x coordinate from header, cast to double
y       y coordinate from header, cast to double
scalco  scaling factor (>0 means multiplication, <0 means division)
counit  coordinate units code as defined in segy.h

Output:
xout    x coordinate in units of length or degrees
yout    y coordinate in units of length or degrees

*************************************************************************
Notes:
Does computations as doubles. If counit=0 or scalco=0, counit=1 or
scalco=1 is assumed, respectively. Coordinate encoding:
counit=1  units of length
counit=2  seconds of arc
counit=3  decimal degrees 
counit=4  degrees, minutes and seconds encoded as integer DDDMMSS 
          with scalco=1 or DDDMMSS.ss with scalco=-100 (see segy.h)
Geographical coordinates should be positive North of the equator and 
East of Greenwich.
*************************************************************************
Author: Nils Maercklin, March 2006
*************************************************************************/
{
    /* Internal variables */
    double factor;  /* scaling factor */
    double d,m,s;   /* degrees, minutes, and seconds */

    /* If tr.scalco not set, use 1 as the value */
    factor = (!scalco) ? 1.0 : (double) scalco;

    /* Factor < 0 means divide; factor > 0 means to multiply */
    if (factor < 0.0) factor = -1.0/factor;

    /* Apply scaling according to counit value */
    switch (counit) {
        /* Units of length */
        case 1:
            *xout = factor * x;
            *yout = factor * y;
        break;
        /* Lat and lon in seconds of arc */
        case 2:
            *xout = (factor * x) / 3600.0;
            *yout = (factor * y) / 3600.0;
        break;
        /* Lat and lon decimal degrees */
        case 3:
            *xout = factor * x;
            *yout = factor * y;
        break;
        /* Lat and lon in DMS */
        case 4:
            x *= factor;
            m = 10000.0 * modf(x/10000.0, &d);
            s =  100.0 * modf(m/100.0, &m);
            *xout = d + m/60.0 + s/3600.0;

            y *= factor;
            m = 10000.0 * modf(y/10000.0, &d);
            s =   100.0 * modf(m/100.0, &m);
            *yout = d + m/60.0 + s/3600.0;
        break;
        /* Else assume units of length without warning */
        default:
            *xout = factor * x;
        break;
    }
}


void setval( cwp_String type_out, Value *val_out, double dval_out)
/************************************************************************
setval - set value of a trace header word 

*************************************************************************
Input:
type_out    type of header word
dval_out    value of header word (double)

Output:
val_out     value according to type of header word

*************************************************************************
Credits: Einar Kajartansson, Jack K. Cohen, John Stockwell (sushw)
*************************************************************************/
{
    switch (*type_out) {
    case 's':
        err("can't change char header word");
    break;
    case 'h':
        val_out->h = (short) NINT(dval_out);
    break;
    case 'u':
        val_out->u = (unsigned short) NINT(dval_out);
    break;
    case 'l':
        val_out->l = (long) NINT(dval_out);
    break;
    case 'v':
        val_out->v = (unsigned long) NINT(dval_out);
    break;
    case 'i':
        val_out->i = (int) NINT(dval_out);
    break;
    case 'p':
        val_out->p = (unsigned int) NINT(dval_out);
    break;
    case 'f':
        val_out->f = (float) dval_out;
    break;
    case 'd':
        val_out->d = (double) dval_out;
    break;
    default:
        err("unknown type %s", type_out);
    break;
    }
}

