/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#ifndef REFLPSV_H
#define REFLPSV_H

void compute_reflectivities (int int_type, int verbose, int wtype, int wfield,
	int vsp, int flt, int win, int nx, int nt, int ntc, int nor, int nf,
	int nlayers, int lsource, int layern, int nfilters, int *filters_phase,
 	int nw, int np, float bp, float tlag, float red_vel, float w1,float w2,
 	float fx, float dx, float bx, float fs, float decay, float p2w,
	float tsec, float fw, float wrefp, float wrefs, float epsp, float epss,
	float sigp, float sigs, float pw1, float pw2, float pw3, float pw4,
	float h1, float h2, float m1, float m2, float m3, float fref, int *lobs,
	int *filters_type, float *dbpo, float *f1, float *f2, float *cl,
	float *ct, float *ql, float *qt, float *rho, float *t,
	float **wavefield1, float **wavefield2, float **wavefield3,
	FILE *outfp);

void convolve_wavelet (int wavelet_type, int nx, int nt, float dt, float fpeak, 
	float **wfieldx);

void compute_synthetics (int verbose, int nt, int ntc, int nx, int nor, int nw,
    int nlayers, int lsource, int nf, int flt, int vsp, int win, int wtype,
    float tlag, float red_vel, float decay, float tsec, float bx, float dx,
    float w1, float w2, int *fil_phase, int nfilters, int *fil_type,
    float *dbpo, float *f1, float *f2, int *lobs, float *cl, float *t, 
    complex ***response, float **reflectivity, FILE *outfp);

void red_vel_factor (float x, float red_vel, float tlag, complex wpie,
    complex wsq, complex *rvfac);

void construct_tx_trace (int nw, int nt, int ntc, int ntfft, int nfilters,
    int *min_phase, float tsec, float unexp, float sphrd, complex *refw,
    int *fil_type, float *f1, float *f2, float *dbpo, float *reft);

void compute_Hanning_window (int iwin, int iw, int if1, int if2, int nw,
    complex *win);

void apply_filters (int *min_phase, int nfilters, int nw, float tsec, float *f1,
    float *f2, float *dbpo, int *filtype, complex *refw);

void compute_w_aux_arrays (int wtype, int layern, int nlayers, int nor,
    int lsource, int *np, int *block_size, int *nblock, int *left, float fw,
    float wrefp, float wrefs, int *lobs, float tsec, float p2w, float fs,
    float xmax, float w, float decay, float epsp, float epss, float sigp,
    float sigs, float *pw1, float *pw2, float *pw3, float *pw4, float *dp,
    float *fp, complex wpie, complex *cdp, float *cl, float *ct, float*ql,
    float *qt, float *rho, float *t, complex *al, complex *at, complex *prs);

void compute_p_aux_arrays (int wtype, int nlayers, int lsource, int block_size,
    float bp, float dp, float w, float decay, float pw1, float pw2,
    float pw3, float pw4, complex *pwin, float m1, float m2, float m3, float h1,
    float h2, complex wpie, complex *divfac, complex *p, complex *pp,
    complex *al, complex *at, float *rho, complex *gl, complex *gt,
    complex *gam, complex *alpha, complex *betha, complex *sigmad1,
    complex *sigmad2, complex *sigmau1, complex *sigmau2) ;

void source_receiver_type (int nor, int nlayers, int lsource, int  *lobs,
    float *cl, float *ct, int *acoust, int *flag);

void compute_slowness (int wtype, int lsource, float w, float p2w, int *np,
    float *fp, float fs, float e4, float dk, float decay, float xmax, float *dp,
    float *pw1, float *pw2, float *pw3, float *pw4, complex *cdp, float *cl,
    float *ct, float *t);
   
void compute_al_at (int wtype, int nlayers, int layern, float eps, float epsp,
    float epss, float sigma, float sigp, float sigs, float *wrefp, float *wrefs,
    float fw, float *cl, float *ct, float *ql, float *qt, float *rho,
    complex wpie, complex *al, complex *at);

void compute_prs (int nor, int *lobs, complex *al, complex *at, float *rho,
    complex *prs);

void compute_block_size (int np, int *block_size, int *nblock, int *left);

void compute_pwin (int wtype, int block_size, float bp, float dp, float w,
    float decay, float pw1, float pw2, float pw3, float pw4, complex *pwin,
    complex *p, complex *pp);

void compute_gl_gt_gam (int wtype, int nlayers, int block_size, complex *al,
    complex *at, float *rho, complex *pp, complex *gl, complex *gt,
    complex *gam);

void compute_alpha_betha (int lsource, int block_size, complex ats, float rhos,
    complex *gl, complex *gt, complex *alpha, complex *betha);

void compute_sigmas (int wtype, int block_size, int lsource, float m1,
    float m2, complex meu, complex *alpha, complex *betha, complex *p,
    complex *gl, complex *gt, complex *gam, complex a2, complex a3,
    complex *pwin, complex s1, complex s2, complex s4, complex *sigmau1,
    complex *sigmau2, complex *sigmad1, complex *sigmad2);

void compute_moment_tensor (int wtype, float phi, float lambda, float delta,
    float phis, float m0, float *m1, float *m2, float *m3);

void parameter_interpolation (int nlayers, int *intlayers, int *nintlayers,
    float *intlayth, float *cl, float *ql, float *ct, float *qt, float *rho,
    float *t) ;

void random_velocity_layers (int *nlayers, int *lsource, int nrand_layers,
    float sdcl, float sdct, float layer, float zlayer, float *cl, float *ql,
    float *ct, float *qt, float *rho, float *t);

void apply_earth_flattening (int nlayers, float z0, float *cl, float *ct,
    float *rho, float *t);

void psv_reflectivities (int int_type, int verbose, int wtype, int nw,
    int nlayers, int nx, int layern, int nor, int np, float bp1, float m1,
    float m2, float m3, float h1, float h2, int lsource, float bx, float dx,
    float xmax, float decay, float fref, float wrefp, float wrefs, float tsec,
    float p2w, float fs, float epsp, float epss, float sigp, float sigs,
    float pw1, float pw2, float pw3, float pw4, int *acoustic, int *flag,
    int *lobs, float *rho, float *t, float *cl, float *ct, float *ql, float *qt,
    complex ***response1, complex ***response2, complex ***response3,
    FILE *outfp);

void sh_reflectivities (int int_type, int verbose, int wtype, int nw,
    int layers, int nx, int layern, int nor, int np, float bp1, float m1,
    float m2, float m3, float h1, float h2, int lsource, float bx, float dx,
    float xmax, float decay, float fref, float wrefp, float wrefs, float tsec,
    float p2w, float fs, float epsp, float epss, float sigp, float sigs,
    float pw1, float pw2, float pw3, float pw4, int *flag, int *lobs,
    float *rho, float *t, float *cl, float *ct, float *ql, float *qt,
    complex ***response1, FILE *outfp);

#define RSO 6371.0

#endif /* reflpsvsh_h */
