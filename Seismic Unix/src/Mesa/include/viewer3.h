/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#ifndef VIEWER3_H
#define VIEWER3_H

#include "GL/glu.h"
#include "GL/glut.h"
#include "MGL/trackball.h"

typedef struct {
      float **x;  /*nonuniform grid for x on top horizon*/
      float **y;  /*nonuniform grid for y on top horizon*/
      float **z;  /*nonuniform grid for z on top horizon*/
      float **v0; /*velocity on the top horizon*/
      float **v1; /*velocity above the base horizon*/
      int nrays;  /*number of rays*/
      int *nseg;  /*number of raysegments for each ray*/   
} Layer;

#define DIAMETER 10 
#define RADIUS (DIAMETER*0.55)
#define EPS 0.001
#define OUT -99999

GLfloat angle=-150; 	/* in degrees */
GLint glb_spinning=0; 	/*flag: set when glb_spinning*/
GLint glb_moving=0;	/*flag: set when glb_moving*/
GLint glb_beginx; 	/*the last point (glb_beginx,glb_beginy)*/
GLint glb_beginy;
GLint glb_W=60;		/*window width in pixels*/ 
GLint glb_H=60;		/*window height in pixels*/
GLint glb_newmodel=1;	/*flag: set when the model is updated*/
GLint glb_scaling;	/*flag: set when glb_scaling*/
GLfloat glb_scalefactor=1.0;
GLfloat glb_alpha=1;
GLint glb_hue;
GLfloat tbs;

GLfloat curquat[4];       /*current quaternion*/
GLfloat lastquat[4];      /*last quaternion*/

enum H_L_T{HORZ,TRI,TETRA,LAYER};
enum H_L_T hlt=TRI;
enum WC_RAYS{WHITE,COLORED};
enum WC_RAYS wc_rays=COLORED;
enum WFS{WIRED,SOLID};
enum WFS wfs=WIRED;
enum On_or_Off{OFF,ON};
enum On_or_Off *glb_on_or_off;
enum Plot_Rays{DO_NOT_PLOT_RAYS,PLOT_RAYS};
enum Plot_Wf{DO_NOT_PLOT_WF,PLOT_WF};
enum Plot_Rays glb_plot_rays=DO_NOT_PLOT_RAYS;
enum Plot_Wf glb_plot_wf=DO_NOT_PLOT_WF;
enum Plot_ST{DO_NOT_PLOT_SURFACE_TRAVELTIMES,PLOT_SURFACE_TRAVELTIMES};
enum Plot_ST plot_st=DO_NOT_PLOT_SURFACE_TRAVELTIMES;

/*number of interfaces except the surface*/
int nhz;

void showHorz(int ihz,float ***databot, int nx,int ny,
	float ***emis);
void showLayer(int ihz,float ***databot,float ***datatop,
	int nx,int ny,float ***emisbot,float ***emistop);
void showTetra(int ihz,float ***databot,float ***datatop,
	int nx,int ny,float ***emisbot,float ***emistop);
void showTri(int ihz,float ***data,int nx,int ny,float ***emis);
void recalcModelView(void);
void showMessage(GLfloat x, GLfloat y, GLfloat z, char *message);
void redraw();
void myReshape(int w, int h);
void mouse(int button, int state, int x, int y);
void animate(void);
void motion(int x,int y);
void controlLights(int value);
void vis(int visible);
void vEmission(float v,float vmin,float vmax,float *emission);
void tEmission(float t,float tmin,float tmax,float *emission);
static void normalize_quat(float q[4]);

#endif /* VIEWER3_H */
