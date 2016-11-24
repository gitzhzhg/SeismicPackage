/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* UTMCONV: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" UTMCONV - CONVert longitude and latitude to UTM, and vice versa       ",
"                                                                       ",
" utmconv <stdin >stdout [optional parameters]                          ",
"                                                                       ",
" Optional parameters:                                                  ",
"    idx=23          reference ellipsoid index (default is WGS 1984)    ",
"    format=%.3f     output number format (printf style for one float)  ",
"    a=(from idx)    user-specified semimajor axis of ellipsoid         ",
"    f=(from idx)    user-specified flattening of ellipsoid             ",
"    letter=0        =1: use UTM letter designator for latitude/Northing",
"    invert=0        =0: convert latitude and longitude to UTM          ",
"                    =1: convert UTM to latitude and longitude          ",
"    verbose=0       =1: echo parameters and number of converted coords ",
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
"    Input and output is in ASCII format. For a conversion from lon/lat ",
"    to UTM (invert=0), input is a two-column table of longitude and    ",
"    latitude in decimal degrees. Output is a three-column table of     ",
"    UTM Easting, UTM Northing, and UTM zone. The zone is given either  ",
"    by the zone number only (default, negative on southern hemisphere) ",
"    or by the positive zone number plus a letter designator (letter=1).",
"                                                                       ",
" Example:                                                              ",
"    Convert 40.822N, 14.125E to UTM with zone number and letter,       ",
"    output values rounded to nearest integer:                          ",
"        echo 14.125 40.822 | utmconv letter=1 format=%.0f              ",
"    The output is \"426213 4519366 33T\" (Easting, Northing, UTM zone).",
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
 * the equator and decrease southward. Letters are sometimes used
 * to identify different zones of latitude. The letters C-M 
 * indicate zones on the southern and the letters N-X zones on 
 * the northern hemisphere.
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
void convLLtoTM(double a, double f, double lat, double lon, \
    double lon0, double *x, double *y);
void convTMtoLL(double a, double f, double x, double y, \
    double lon0, double *lat, double *lon);
char getUTMLetter(double lat);
short getUTMZone(double lat, double lon);
RefEllipsoid getRefEllipsoid(int idx);


int 
main (int argc, char **argv)
{
    int idx;                /* reference ellipsoid index */
    RefEllipsoid ellip;     /* reference ellipsoid */
    double a;               /* semimajor axis of ellipsoid */
    double f;               /* flattening of ellipsoid */
    short zone;             /* UTM zone number */
    char letter;            /* UTM letter designator */
    double x;               /* UTM Easting */
    double y;               /* UTM Northing */
    double lat;             /* latitude in degrees */
    double lon;             /* longitude in degrees */
    double lon0;            /* central meridian in degrees */
    int verbose=0;          /* verbose flag */
    int invert=0;           /* invert flag */
    int useletter=0;        /* use-letter flag */
    int userlon0=0;         /* flag for user-specified central meridian */
    double xoff;            /* false Easting */
    double ynoff;           /* false Northing on N hemisphere */
    double ysoff;           /* false Northing on S hemisphere */
    cwp_String format=NULL; /* output number format */
    char fmt[60];           /* internal format string */
    int n=0;                /* coordinate record counter */
    float tmp;              /* temporary variable */


    /* hook up getpar */
    initargs(argc, argv);
    requestdoc(1);

    /* Get parameters */
    if (!getparstring("format", &format)) format="%.3f";
    if (!getparint("idx", &idx))          idx=23;
    if (!getparint("verbose", &verbose))  verbose=0;
    if (!getparint("invert", &invert))    invert=0;
    if (!getparint("letter", &useletter)) useletter=0;
    if (!getpardouble("xoff", &xoff))     xoff=500000.0;
    if (!getpardouble("ynoff", &ynoff))   ynoff=0.0;
    if (!getpardouble("ysoff", &ysoff))   ysoff=10000000.0;
    if (getpardouble("lon0", &lon0))      userlon0=1;

    /* Check parameters */
    if (idx<0 || idx>=N_ELLIPSOIDS) err("unknown ellipsoid idx=%d", idx);
    if (userlon0 && (lon0<-180.0 || lon0>180.0)) \
        err("lon0=%g must be between -180 and 180", lon0);
    if (strlen(format)>20) err("format string too long");

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

    /* Coordinate conversion from UTM to longitude/latitude */
    if (invert) {
        sprintf(fmt, "%s %s\n", format, format);
        while (2==fscanf(stdin, "%lf %lf", &x, &y)) {
            n++;

            /* Interpret zone number and letter ...*/
            if (useletter) {
                if (!(2==fscanf(stdin, "%f%c", &tmp, &letter)))
                    err("missing zone number/letter in record %d", n);
                if (letter>='A' && letter<='Z') {
                    if ((letter-'N')<0) tmp=-tmp;
                }
                else if (letter>='a' && letter<='z') {
                    if ((letter-'n')<0) tmp=-tmp;
                }
                else {
                    err("missing or invalid zone letter in record %d", n);
                }
            }
            /* ...OR get signed zone number */
            else {
                if (!(1==fscanf(stdin, "%f", &tmp)))
                    err("missing zone number for record %d", n);
            }
            zone = (short) tmp;

            /* Calculate central meridian in degrees */
            if (!userlon0) {
                if (abs(zone)<1 || abs(zone)>60) \
                    err("invalid zone=%d in record %d", zone, n);
                lon0 = (double) ((abs(zone)-1)*6 - 180 + 3);
            }


            /* Remove false Easting and Northing */
            x -= xoff;
            y -= (zone<0) ? ysoff : ynoff;

            /* Inverse Transverse Mercator projection */
            convTMtoLL(a, f, x, y, lon0, &lat, &lon);

            /* Print converted coordinates */
            printf(fmt, lon, lat);
        }
    }

    /* Coordinate conversion from longitude/latitude to UTM */
    else {
        if (useletter) sprintf(fmt, "%s %s %%2d%%c\n", format, format);
        else sprintf(fmt, "%s %s %%3d\n", format, format);

        while (2==fscanf(stdin, "%lf %lf", &lon, &lat)) {
            n++;

            /* Check coordinates */
            if (lon==180.0) lon=-180.0;
            if (lat<-89.999 || lat>89.999 || lon<-180. || lon>=180.) {
                err("invalid coordinates in record %d: lat=%g lon=%g", \
                    n, lat, lon);
            }

            /* UTM zone number and letter designator */
            zone   = getUTMZone(lat, lon);
            letter = getUTMLetter(lat);

            /* Central meridian of the zone in degrees */
            if (!userlon0) lon0 = (double) ((zone-1)*6 - 180 + 3);
            
            /* Transverse Mercator projection */
            convLLtoTM(a, f, lat, lon, lon0, &x, &y);

            /* Add false Easting and Northing */
            x += xoff;
            y += (lat<0.0) ? ysoff : ynoff;

            /* Print converted coordinates */
            if (useletter) {
                printf(fmt, x, y, zone, letter);
            }
            else {
                if (lat<0.0) zone *= -1;
                printf(fmt, x, y, zone);
            }
        }
    }

    /* Print number of coordinate pairs converted */
    if (verbose) warn("converted %d coordinate pair%s", n, (n==1)?"":"s");

    return(CWP_Exit());

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


char getUTMLetter(double lat)
/************************************************************************
getUTMLetter - get the UTM letter designator

*************************************************************************
Input:
lat     geographical latitude in degrees

Output:
        returns the letter designator as a single character

*************************************************************************
Notes:
Does computations as doubles. The latitude is positive on the northern
hemisphere and negative on the southern hemisphere. UTM coordinates
are defined between 80S and 84N. If the latitude is outside this range, 
the letter A or Z is returned.
*************************************************************************
Author: Nils Maercklin, 30 March 2007
*************************************************************************/
{
    char l;

    if      (  84 < lat) l = 'Z';
    else if (( 84 >=lat) && (lat >=  72)) l = 'X';
    else if (( 72 > lat) && (lat >=  64)) l = 'W';
    else if (( 64 > lat) && (lat >=  56)) l = 'V';
    else if (( 56 > lat) && (lat >=  48)) l = 'U';
    else if (( 48 > lat) && (lat >=  40)) l = 'T';
    else if (( 40 > lat) && (lat >=  32)) l = 'S';
    else if (( 32 > lat) && (lat >=  24)) l = 'R';
    else if (( 24 > lat) && (lat >=  16)) l = 'Q';
    else if (( 16 > lat) && (lat >=   8)) l = 'P';
    else if ((  8 > lat) && (lat >=   0)) l = 'N';
    else if ((  0 > lat) && (lat >=  -8)) l = 'M';
    else if (( -8 > lat) && (lat >= -16)) l = 'L';
    else if ((-16 > lat) && (lat >= -24)) l = 'K';
    else if ((-24 > lat) && (lat >= -32)) l = 'J';
    else if ((-32 > lat) && (lat >= -40)) l = 'H';
    else if ((-40 > lat) && (lat >= -48)) l = 'G';
    else if ((-48 > lat) && (lat >= -56)) l = 'F';
    else if ((-56 > lat) && (lat >= -64)) l = 'E';
    else if ((-64 > lat) && (lat >= -72)) l = 'D';
    else if ((-72 > lat) && (lat >= -80)) l = 'C';
    else l = 'A';

    /* Return letter designator */
    return l;
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


void convTMtoLL(double a, double f, double x, double y, \
    double lon0, double *lat, double *lon)
/************************************************************************
convTMtoLL - convert Transverse Mercator Easting and Northing (e.g. UTM)
             to latitude and longitude
*************************************************************************
Input:
a       ellipsoid simimajor axis (in m)
f       ellipsoid flattening
x       Easting (Transverse Mercator grid)
y       Northing (Transverse Mercator grid)
lon0    central meridian (longitude) in degrees

Output:
lat     geographical latitude in degrees
lon     geographical longitude in degrees

*************************************************************************
Notes:
Does computations as doubles. Remember to remove any false Northing
and Easting from the input coordinates x and y. Standard UTM uses
a false Easting of 500 km and a false Northing of 10000 km on the 
southern hemisphere

The latitude is positive on the northern hemisphere and negative on the 
southern hemisphere. The longitude is negative west of the zero-meridian 
(Greenwich), i.e. its range of values is -180.0 ... 179.99999.

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
    double e1;
    double e2;   /* eccentricity squared, e2 = f(2-f) */
    double ep2;  /* eccentricity prime squared */
    double k0, mu, cn1, ct1, cc1, cr1, cd, cm, phi1;

    /* Ellipsoid parameters */
    e2  = f * (2.0 - f);
    ep2 = e2 / (1.0 - e2);
    e1  = (1.0 - sqrt(1.0-e2)) / (1.0 + sqrt(1.0-e2));

    /* Some constants */
    k0 = 0.9996;
    cm = y / k0;
    mu = cm / (a * (1. - e2/4. - 3.*e2*e2/64. - 5.*e2*e2*e2/256.));

    phi1 = mu + (3.*e1/2. - 27.*e1*e1*e1/32.) * sin(2.*mu) \
           + (21.*e1*e1/16. - 55.*e1*e1*e1*e1/32.) * sin(4.*mu) \
           +(151.*e1*e1*e1/96.) * sin(6.*mu);

    cn1 = a / sqrt(1. - e2*sin(phi1)*sin(phi1));
    ct1 = tan(phi1) * tan(phi1);
    cc1 = ep2*cos(phi1)*cos(phi1);
    cr1 = a * (1. - e2) / pow(1.-e2*sin(phi1)*sin(phi1), 1.5);
    cd  = x / (cn1*k0);

    /* Latitude */
    (*lat) = phi1 - (cn1*tan(phi1)/cr1) * (cd*cd/2. \
             - (5. + 3.*ct1 + 10.*cc1 - 4.*cc1*cc1 - 9.*ep2)*cd*cd*cd*cd/24.\
             + (61. + 90. *ct1 + 298.*cc1 + 45.*ct1*ct1 - 252.*ep2 \
             - 3*cc1*cc1) * cd*cd*cd*cd*cd*cd/720.);
    (*lat) *= 180.0 / PI;

    /* Longitude */
    (*lon) = (cd - (1. + 2.*ct1 + cc1)*cd*cd*cd/6. \
             + (5. - 2.*cc1 + 28.*ct1-3.*cc1*cc1 + 8*ep2 + 24.*ct1*ct1) \
             * cd*cd*cd*cd*cd/120.) / cos(phi1);
    (*lon) = lon0 + ((*lon) * 180.0 / PI);
}

