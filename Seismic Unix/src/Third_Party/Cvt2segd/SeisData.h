#ifdef __cplusplus
extern "C" {
#endif
/* Define a structure to hold all the parameters for the CSeisData class
 * We put all the parameters in a structure to make reading and writing
 * data to file much easier.
 */

typedef struct tagSEISDATAPARMS
    {
    int    m_ffid;       /* FFID of shot record, only valid if data is shot rec. */
    int    m_itype;      /* Data type */
    int    m_units;      /* Units, 1=English, 2=metric */
    int    m_softrls;    /* Software release number */
    int    m_geoassn;    /* Geometry assigned flag (0=no, 1=yes) */
    int    m_ntraces;    /* Total Number of Traces in this dataset */
    int    m_year;       /* Year data recorded */
    int    m_day;        /* Day data recoreded */
    int    m_hour;       /* Hour data recorded */
    int    m_min;        /* Minute data recorded */
    int    m_sec;        /* Second data recorded */
    int    m_nsegtrchead; /* No. of bytes in the orginal SEG-D trace header */
    int    m_format_code; /* Orginal fromat code from SEG-D header */
    BOOL   m_attrcalc;   /* Attributes calculated or not  */
    int    m_dataunits;  /* Normally volts or microbars. */
    int    m_recsystype; /* Recorder system type, e.g, I/O system II */
    int    m_objecttype; /* Type of data object, shot record, etc. */
    int    m_ispare[83]; /* Spare integer parameters */
    float  m_scale;      /* Scale that has been applied to all traces */
    float  m_datum;      /* Datum for all the traces */
    float  m_minxsrc;    /* Minimum x-value for source positions */
    float  m_maxxsrc;    /* Maximum x-value for source positions */
    float  m_minysrc;    /* Minimum y-value for source positions */
    float  m_maxysrc;    /* Maximum y-value for source positions */
    float  m_minzsrc;    /* Minimum z=value for source positions */
    float  m_maxzsrc;    /* Maximum z-value for source positions */
    float  m_minxrec;    /* Minimum x-value for receiver positions */
    float  m_maxxrec;    /* Maximum x-value for receiver positions */
    float  m_minyrec;    /* Minimum y-value for receiver positions */
    float  m_maxyrec;    /* Maximum y-value for receiver positions */
    float  m_minzrec;    /* Minimum z-value for receiver positions */
    float  m_maxzrec;    /* Maximum z-value for receiver positions */
    float  m_grpint;     /* Group interval */
    float  m_srcint;     /* Source interval */
    float  m_fspare[84]; /* Spare float parameters */
    unsigned char m_uspare[224];  /* Spare bytes to make main header 1024 bytes */
	} SEISDATAPARMS;

/* Define a structure to hold trace header parameters.  We define
 * a structure to hold the trace header values because it makes
 * the reading and writing of data to hard disk much easier.
 */
typedef struct tagSEISTRACEHEADER
    {
    int     m_trcseq;      /* Trace sequence number */
    int     m_ffid;        /* Field File ID number */
    int     m_cdp;         /* CDP ensemble number */
    int     m_trcid;       /* Trace idenfification code */
    int     m_nvert;       /* No. of vertically summed traces to produce this trace */
    int     m_test;        /* Data use */
    int     m_nsamp;       /* No. of samples in this trace */
    int     m_scantype;    /* Scan type from SEG-D trace header */
	int     m_channset;    /* Channel set number for SEG-D trace header */
	int     m_chantrcno;   /* Trace number from SEG-D trace header */
	int     m_nsegtrchead; /* No. of bytes in orginal SEG-D trace header */
	BOOL    m_badtrace;    /* Flag to indicate a bad trace (TRUE or FALSE) */
    int     m_isrclineint;    /* Source line number (integer) */
    int     m_isrclinefract;  /* Source line number (fraction) */
    int     m_isrcptnoint;    /* Source point number (integer) */
    int     m_isrcptnofract;  /* Source point number (fraction) */
    int     m_ireclineint;    /* Receiver line number (integer) */
    int     m_ireclinefract;  /* Receiver line number (fraction) */
    int     m_irecptnoint;    /* Receiver point number (integer) */
    int     m_irecptnofract;  /* Receiver point number (fraction) */
    int     m_fold;        /* No. of traces stacked to produce this trace  */
    COLORREF m_colorwintop; /* Color of time pick at top of window */
    COLORREF m_colorwinbot; /* Color of time pick at bottom of window */
    COLORREF m_colorfirstbreak; /* Color of first break pick */
    COLORREF m_colortheotime; /* Color of the theortical pick */
    int     m_ispare[39];  /* Spare integer parameters */
    float   m_samp;        /* Sampling interval (in seconds) for this trace */
    float   m_offset;      /* Distance from source to receiver */
    double  m_recelv;      /* Receiver group elevation */
    double  m_srcelv;      /* Source elevation */
    float   m_recdatum;    /* Datum for receiver */
    float   m_srcdatum;    /* Dataum for source */
    float   m_scale;       /* Scale factor applied to data */
    float   m_pad04_1;     /* alignment padding */
    double  m_srcx;        /* Source X-Coordinate, Easting */
    double  m_srcy;        /* Source Y-Corrdinate, Northing */
    double  m_recx;        /* Receiver X-Coordinate, Easting */
    double  m_recy;        /* Receiver Y-Coordinate, Northing */
    float   m_units;       /* Coordinate units */
    float   m_rms1;        /* RMS value in 1st time window */
    float   m_abs1;        /* Maximum absolute value in 1st time window */
    float   m_peaktime1;   /* Time when the peak occured in 1st time window */
    float   m_time1start;  /* Start time of 1sttime window */
    float   m_time1end;    /* End time of 1st time window */
    float   m_rms2;        /* RMS value in 2nd RMS time window */
    float   m_abs2;        /* Maximum absolute value in 2nd time window */
    float   m_peaktime2;   /* Time when the peak occured in 2nd time window */
    float   m_time2start;  /* Start time of 2nd time window */
    float   m_time2end;    /* End time of 2nd time window */
    float   m_firstbreak;  /* Time of first break */
    float   m_pickwintop;  /* Pick time for top of window */
    float   m_pickwinbot;  /* Pick time for bottom of window */
    float   m_picktheotime; /* Pick time for theortical first break */
    float   m_weath;       /* Weathering velocity */
    float   m_subweath;    /* Subweathering velocity */
    float   m_upsrc;       /* Uphole time at source */
    float   m_uprec;       /* Uphole time at receiver */
    float   m_srcstatic;   /* Source static correction */
    float   m_recstatic;   /* Receiver static correction */
    float   m_wthstatic;   /* Weathering static */
    float   m_totalstatic; /* Total static applied */
    float   m_timefirst;   /* Time of 1st sample */
	float   m_timelast;    /* Time of last sample */
	float   m_scaler;      /* An external gain term, normally user supplied */
	float   m_alias_filter_frequency; /* Alias filter frequency */
	float   m_alias_filter_slope;     /* Alias filter slope */
	float   m_lowcut_frequency;       /* Lowcut filter's frequency */
	float   m_lowcut_slope;           /* Lowcut filter's slope */
	float   m_first_notch;            /* First notch filter */
	float   m_second_notch;           /* Second notch filter */
	float   m_third_notch;            /* Third notch filter */
	float   m_srclineno;              /* Source Line Number */
	float   m_srcstationno;           /* Source Station Number */
	float   m_reclineno;              /* Receiver Line Number */
	float   m_recstationno;           /* Receiver Station Number */
	float   m_fspare[9];  /* Spare floating parameters */
	} SEISTRACEHEADER;

/* Declaration of a structure for handling the trace header */

typedef struct tagTRACEDATA
	{
	SEISTRACEHEADER *thdr;
	unsigned char *segd;
	float *trace;
	} TRACEDATA;

/* Declaration of a structure used for reading and writing data objects
 * from and to hard disk.
 */

typedef struct tagSEISFILEHEADER
    {
    DWORD ID;      /* Identifier to represent a Seis Data object. */
    int ntraces;   /* Total number of seismic traces in this data object */
    DWORD seghead; /* Offset from start of file to location of SEG-D record header. */
    int nseghead;  /* Number of bytes in the SEG-D record header. */
    DWORD offtraces; /* Offset from start of file to location to find information */
                   /* about the location of where the traces are at in the file */
    } SEISFILEHEADER;

/* Declaration of a structure used to pinpoint the location of seismic trace
 * and its associated trace header.
 */

typedef struct tagSEISTRACELOCATION
    {
    DWORD trcoffset;  /* Offset from start of file to location of trace. */
    int nsamp;        /* Number of samples in trace. */
    } SEISTRACELOCATION;

#ifdef __cplusplus
}
#endif
