/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRIP: $Revision: 1.6 $ ; $Date: 2011/11/21 16:29:34 $	*/

#define DIAMETER 10
#define RADIUS (DIAMETER*0.5)

#include "par.h"
#include "GL/glu.h"
#include "GL/glut.h"
#include "MGL/trackball.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"									",
" TRIP - TRI-Plane 3D data viewer					",
"									",
" trip < datain [parameters] 		 				",
" 									",
" Required parameters:							",
" n1=	   	number of x samples (1st dimension)		     	",
" n2=	   	number of y samples (2nd dimension) 			",
" n3=		number of z samples (3nd dimension) 			",
" 									",
" Optional Parameters:							",
" cx=n2/n2s/2 (integer) x-intercept of view plane facing x-axis		",	
" cy=n3/n3s/2 (integer) y-intercept of view plane facing y-axis		",	
" cz=n1/n1s/2 (integer) z-intercept of view plane facing z-axis		",
" n1s=1	stride in the fastest dimension					",
" n2s=1	stride in the second dimension					",
" n3s=1	stride in the third dimension					",
" hue=1		for hue and 0 for bw 					",
" q=-0.6,0.06,-0.06,0.8 define the quaternion				",
" tbs=0.8 the lager the slower it rotates                     	 	",
" verbose=0     =1 print some useful information			",NULL};

/*
 * Credits:
 *  	CWP: Zhaobo Meng, 1996
 */
/**************** end self doc *******************************************/

#define EPS 0
GLfloat angle=-150;	/*in degrees */
GLint glb_spinning=0; 	/*flag: set when spinning*/
GLint glb_moving=0;	/*flag: set when glb_moving*/
GLint glb_beginx; 	/*the last point (glb_beginx,glb_beginy)*/
GLint glb_beginy;
GLint glb_W=300;	/*window width in pixels*/ 
GLint glb_H=300;	/*window height in pixels*/
GLfloat curquat[4];	/*current quaternion*/
GLfloat lastquat[4];	/*last quaternion*/
GLint glb_newModel=1;	/*flag: set when the model is updated*/
GLint glb_scaling;	/*flag: set when glb_scaling*/
GLfloat glb_scalefactor=1.0;
GLint glb_hue;
GLfloat glb_alpha=1.0;
GLfloat tbs;

enum On_or_Off{OFF,ON};
enum On_or_Off *glb_plane_flag;
enum Plot_Axis{DO_NOT_PLOT_AXIS,PLOT_AXIS};
enum Plot_Axis glb_plot_axis;

void showPlane(int list,float ***data,float ***emis, int nx,int ny); 
void recalcModelView(void);
void showMessage(GLfloat x, GLfloat y, GLfloat z, char *message);
void redraw();
void myReshape(int w, int h);
void mouse(int button, int state, int x, int y);
void animate(void);
void motion(int x,int y);
void controlPanel(int value);
void vis(int visible);
void zmEmission(float v,float vmin,float vmax,float *emission);

static void normalize_quat(float q[4])
{
    int i;
    float mag;

    mag = (q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3]);
    for (i = 0; i < 4; i++) q[i] /= mag;
}

int 
main(int argc, char **argv)
{
	int n1;	    	/*number of samples in the fastest direction.*/
	int n2;     	/*number of samples in the 2nd direction*/
	int n3;     	/*number of samples in slownest direction*/

	int n1s;  	/*stride in the fastest direction.*/
	int n2s;  	/*stride in the 2nd direction*/
	int n3s;  	/*stride in slownest direction*/

	int i1,i2,i3;
	int i1min,i1max; /*indice for min max in n1*/
	int i2min,i2max; /*indice for min max in n2*/
	int i3min,i3max; /*indice for min max in n3*/

	int newn1,newn2,newn3;
	int i1new,i2new,i3new;
	float v0;

	float emission[4];

	float ***data;	/*data for plotting*/
	float ***emis;  /*emission for plotting*/

	int verbose;    /*if =1 print some useful information*/

	float ***cube;	/*the 3D data set*/

	float vmin;
	float vmax;

	float xmin,xmax;
        float ymin,ymax;
        float zmin,zmax;

	float eyez;

	float q0[4];

	int cx,cy,cz;	/*center of the 1st, 2nd and 3rd view plane*/

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("n1",&n1)) 	    	err("Must specify n1");
	if (!getparint("n2",&n2))	    	err("Must specify n2\n");
	if (!getparint("n3",&n3)) 	    	err("Must specify n3\n");
	if (!getparint("n1s",&n1s)) 	n1s=1;
	if (!getparint("n2s",&n2s)) 	n2s=1;
	if (!getparint("n3s",&n3s)) 	n3s=1;
	if (!getparint("hue",&glb_hue)) 	glb_hue=1; /*1 for glb_hue*/
        if (!getparfloat("tbs",&tbs)) 	tbs=0.8;

	if (glb_hue!=0) glb_hue=1;

	if (n1<1) err("n1=%d < 1",n1);
	if (n2<1) err("n2=%d < 1",n2);
	if (n3<1) err("n3=%d < 1",n3);

	n1s=MAX(1,n1s);
	n1s=MIN(n1,n1s);
	n2s=MAX(1,n2s);
	n2s=MIN(n2,n2s);
	n3s=MAX(1,n3s);
	n3s=MIN(n3,n3s);

	newn1=MAX(2,n1/n1s);	
	newn2=MAX(2,n2/n2s);
	newn3=MAX(2,n3/n3s);

	if (!getparint("verbose",&verbose)) verbose=0;

	if (!getparint("cx",&cx)) cx=n2/2;
	if (!getparint("cy",&cy)) cy=n3/2;
	if (!getparint("cz",&cz)) cz=n1/2;

	cx=MAX(0,MIN(newn2-1,cx/n2s));
	cy=MAX(0,MIN(newn3-1,cy/n3s));
	cz=MAX(0,MIN(newn1-1,cz/n1s));

	if (verbose) {
		warn("newn1=%d\nnewn2=%d\nnewn3=%d\ncx=%d\ncy=%d\ncz=%d",
			newn1,newn2,newn3,cx,cy,cz);
		warn("hue=%d",glb_hue);
	}

	cube=ealloc3float(newn1,newn2,newn3);

	for (i3=0;i3<n3;i3++) {
		for (i2=0;i2<n2;i2++) {
			for (i1=0;i1<n1;i1++) {
				if (fread(&v0,sizeof(float),1,stdin)!=1)
				 	err("Can not read in cube");
				if (	i3%n3s==0 &&
					i2%n2s==0 &&
					i1%n1s==0) {
					i3new=MIN(newn3-1,i3/n3s);
					i2new=MIN(newn2-1,i2/n2s);
					i1new=MIN(newn1-1,i1/n1s);
					cube[i3new][i2new][i1new]=v0;
					if (n1/n1s<2) 
					cube[i3new][i2new][1]=cube[i3new][i2new][0];
					if (n2/n2s<2)
					cube[i3new][1][i1new]=cube[i3new][0][i1new];
					if (n3/n3s<2)
					cube[1][i2new][i1new]=cube[0][i2new][i1new];
				}
			}
		}
	}


	n1=newn1;
	n2=newn2;
	n3=newn3;

	zmin=0; zmax=MAX(n1-1,1);
	ymin=0; ymax=MAX(n3-1,1);
	xmin=0; xmax=MAX(n2-1,1);

	glb_plane_flag=(enum On_or_Off *)ealloc1int(3);
	glb_plane_flag[2]=OFF;
	glb_plane_flag[1]=ON;
	glb_plane_flag[0]=OFF;
	glb_plot_axis=DO_NOT_PLOT_AXIS;

	vmin=cube[0][0][0];
	vmax=vmin;
	i1min=i2min=i3min=0;
	i1max=i2max=i3max=0;
	for (i1=0;i1<n1;i1++) {
		for (i2=0;i2<n2;i2++) {
			for (i3=0;i3<n3;i3++) {
				if (vmin<cube[i3][i2][i1]) {
					i3min=i3;
					i2min=i2;
					i1min=i1;
					vmin=cube[i3][i2][i1];
				}
				if (vmax>cube[i3][i2][i1]) {
					i3max=i3;
					i2max=i2;
					i1max=i1;
					vmax=cube[i3][i2][i1];
				}
			}
		}
	}

	fprintf(stderr,
		"max value=%e, at i3=%d i2=%d i1=%d\n",vmin,i3min,i2min,i1min);
	fprintf(stderr,
		"min value=%e, at i3=%d i2=%d i1=%d\n",vmax,i3max,i2max,i1max);

	glutInit(&argc, argv);
	glutInitWindowSize(512, 512);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
	glutCreateWindow("trip");
	glutDisplayFunc(redraw);
	glutIdleFunc(NULL);

	if (!getparfloat("q",q0)){
		q0[0]=-0.6; 
		q0[1]=0.05;
		q0[2]=-0.06;
		q0[3]=0.8;
	}	
        checkpars();

	normalize_quat(q0);

	curquat[0]=q0[0];
	curquat[1]=q0[1];
	curquat[2]=q0[2];
	curquat[3]=q0[3];

	glutReshapeFunc(myReshape);
	glutVisibilityFunc(vis);
	glutMouseFunc(mouse);
	glutMotionFunc(motion);
	glutCreateMenu(controlPanel);
	glutAddMenuEntry("Full screen",1);
	glutAddMenuEntry("Quit", 2);
	glutAddMenuEntry("First vertical plane",3);
	glutAddMenuEntry("Second vertical plane",4);
	glutAddMenuEntry("Horizontal plane",5);
	glutAddMenuEntry("Plot Axes",6);
	glutAttachMenu(GLUT_RIGHT_BUTTON);

	glShadeModel(GL_SMOOTH); /* colors will be continuous */
	glEnable(GL_LIGHTING);
	glEnable(GL_DEPTH_TEST);
	glLineWidth(1.0);

	glMatrixMode(GL_PROJECTION);

	eyez=25;

	gluPerspective( 
		40.0,	/*fovy: view angle in y direction*/
		1.0,	/*aspect: ratio of width (x) to y (height)*/
		eyez-DIAMETER,	/*near clipping plane*/
		eyez+DIAMETER);	/*far clipping plane*/
	glMatrixMode(GL_MODELVIEW);
	gluLookAt(
		0.0, 0.0, eyez, /*(eyex,eyey,eyez): the eye position*/
		0.0, 0.0, 0.0,  /*(centerx,centery,centerz): the center*/
		0.0, 1.0, 0.0); /*(upx,upy,upz): the up direction*/
	glPushMatrix(); 

	/****************************************************
	Let's plot the first vertical plane (facing x-axis):
	****************************************************/
	data=ealloc3float(3,n1,n3);
	emis=ealloc3float(4,n1,n3);
	for (i1=0;i1<n1;i1++) {
		for (i3=0;i3<n3;i3++) {
			data[i3][i1][0]=((cx-xmin)/(xmax-xmin)-0.5)*DIAMETER;
			data[i3][i1][1]=((i3-ymin)/(ymax-ymin)-0.5)*DIAMETER;
			data[i3][i1][2]=((i1-zmin)/(zmax-zmin)-0.5)*DIAMETER;
			v0=cube[i3][cx][i1];
			zmEmission(v0,vmin,vmax,emis[i3][i1]);
		}
	}

	showPlane(
		3,	/*list of plot*/
		data,	/*2-D plane data*/
		emis,	/*emission*/
		n3,	/*slow dimension in the 2D data*/
		n1);	/*fast dimension in the 2D data*/
	free3float(data);
	free3float(emis);

	/****************************************************
	Let's plot the second vertical plane (facing y-axis):
	****************************************************/
	data=ealloc3float(3,n1,n2);
	emis=ealloc3float(4,n1,n2);
	for (i1=0;i1<n1;i1++) {
		for (i2=0;i2<n2;i2++) {
			data[i2][i1][0]=((i2-xmin)/(xmax-xmin)-0.5)*DIAMETER;
			data[i2][i1][1]=((cy-ymin)/(ymax-ymin)-0.5)*DIAMETER;
			data[i2][i1][2]=((i1-zmin)/(zmax-zmin)-0.5)*DIAMETER;
			v0=cube[cy][i2][i1];

			zmEmission(v0,vmin,vmax,emis[i2][i1]);
		}
	}

	showPlane(
		4,	/*list of plot*/
		data,	/*2-D plane data*/
		emis,	/*emission*/
		n2,	/*slow dimension in the 2D data*/
		n1);	/*fast dimension in the 2D data*/
	free3float(data);
	free3float(emis);

	/****************************************************
	Let's plot the horizontal plane:
	****************************************************/
	data=ealloc3float(3,n3,n2);
	emis=ealloc3float(4,n3,n2);
	for (i3=0;i3<n3;i3++) {
		for (i2=0;i2<n2;i2++) {
			data[i2][i3][0]=((i2-xmin)/(xmax-xmin)-0.5)*DIAMETER;
			data[i2][i3][1]=((i3-ymin)/(ymax-ymin)-0.5)*DIAMETER;
			data[i2][i3][2]=((cz-zmin)/(zmax-zmin)-0.5)*DIAMETER;
			v0=cube[i3][i2][cz];
			zmEmission(v0,vmin,vmax,emis[i2][i3]);
		}
	}
	showPlane(
		5,	/*list of plot*/
		data,	/*2-D plane data*/
		emis,	/*emission*/
		n2,	/*slow dimension in the 2D data*/
		n3);	/*fast dimension in the 2D data*/
	free3float(data);
	free3float(emis);

	/*show axes*/
	glNewList(6,GL_COMPILE);
	glLineWidth(1.5);
	emission[0]=1.0;
	emission[1]=1.0;
	emission[2]=1.0;
	emission[3]=1.0;
	glMaterialfv(GL_FRONT,GL_EMISSION,emission);

	glBegin(GL_LINES);
	glVertex3f(0.0,0.0,0.0);
	glVertex3f(RADIUS,0.0,0.0);
	glEnd();

        glBegin(GL_LINES);
        glVertex3f(0.0,0.0,0.0);
        glVertex3f(0.0,RADIUS,0.0);
        glEnd();

        glBegin(GL_LINES);
        glVertex3f(0.0,0.0,0.0);
        glVertex3f(0.0,0.0,-RADIUS);
        glEnd();

	glEndList();

	glutMainLoop();
	return 0;
}

void
showPlane(int list,float ***data,float ***emis,int nx,int ny)
{
	int ix,iy;

	glNewList(list,GL_COMPILE);
	glBegin(GL_QUAD_STRIP);
	for (ix=0;ix<nx-1;ix++) {
		for (iy=0;iy<ny;iy++) {
			glMaterialfv(GL_FRONT,GL_EMISSION,emis[ix][iy]);
			glVertex3fv(data[ix][iy]);
			glMaterialfv(GL_FRONT,GL_EMISSION,emis[ix+1][iy]);
			glVertex3fv(data[ix+1][iy]);
		}
		for (iy=ny-1;iy>=0;iy--) {
			glMaterialfv(GL_FRONT,GL_EMISSION,emis[ix][iy]);
			glVertex3fv(data[ix][iy]);
			glMaterialfv(GL_FRONT,GL_EMISSION,emis[ix+1][iy]);
			glVertex3fv(data[ix+1][iy]);
		}
	}

	glEnd();
	glEndList();
}

void
recalcModelView(void)
{
	GLfloat m[4][4];		/*the matrix*/

	/* The pop and push are useful */
	glPopMatrix();
	glPushMatrix();

	build_rotmatrix(m, curquat);
	glMultMatrixf(m[0]);
	glScalef(glb_scalefactor, glb_scalefactor, glb_scalefactor);
	glb_newModel=0;
}

/***************************************************************
function to print a message in the window
***************************************************************/
void
showMessage(GLfloat x, GLfloat y, GLfloat z, char *message)
{
	glPushMatrix();
	glDisable(GL_LIGHTING);
	glTranslatef(x, y, z);
	glScalef(.01, .01, .01);
	while (*message) {
		glutStrokeCharacter(GLUT_STROKE_ROMAN, *message);
		message++;
	}
	glEnable(GL_LIGHTING);
	glPopMatrix();
}

void
redraw(void)
{
	int iplane;
	if (glb_newModel) recalcModelView();
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	for (iplane=0;iplane<3;iplane++) {
		if (glb_plane_flag[iplane]==ON)
			glCallList(iplane+3);
	}
	if (glb_plot_axis==PLOT_AXIS) {
		glCallList(6);
		showMessage(RADIUS, 0, 0, "x");
		showMessage(0, RADIUS, 0, "y");
		showMessage(0, 0,-RADIUS, "z");
	}
	glFlush();
	glutSwapBuffers();
}

void
myReshape(int w, int h)
{
	glViewport(0, 0, w, h);
	glb_W=w;
	glb_H=h;
}

void
mouse(int button, int state, int x, int y)
{
	if (button==GLUT_LEFT_BUTTON && state==GLUT_DOWN) {
		glb_spinning=0;
		glutIdleFunc(NULL);
		glb_moving=1;
		glb_beginx=x;
		glb_beginy=y;
		if(glutGetModifiers() & GLUT_ACTIVE_SHIFT) 
			glb_scaling=1;
		else 
			glb_scaling=0;
	}	
	if (button==GLUT_LEFT_BUTTON && state==GLUT_UP) 
		glb_moving=0;
}

void
animate(void)
{
	add_quats(lastquat, curquat, curquat);
	glb_newModel=1;
	glutPostRedisplay();
}

void
motion(int x,int y)
{
	if (glb_scaling) {
		glb_scalefactor=glb_scalefactor*(1.0+(((float)(glb_beginy-y))/glb_H));
		glb_beginx=x;
		glb_beginy=y;
		glb_newModel=1;
		glutPostRedisplay();
		return;
	}

	if (glb_moving) {
		trackball(lastquat,
		(2.0*glb_beginx-glb_W)/glb_W,
		(glb_H-2.0*glb_beginy)/glb_H,
		(2.0*x-glb_W)/glb_W,
		(glb_H-2.0*y)/glb_H,tbs);
		fprintf(stderr,"q=%f,%f,%f,%f\n",curquat[0],curquat[1],
                        curquat[2],curquat[3]);

		glb_beginx=x;
		glb_beginy=y;
		glb_spinning=1;
		glutIdleFunc(animate);
	}
}


void
controlPanel(int value)
{
	switch (value) {
		case 6:
			if (glb_plot_axis==PLOT_AXIS)
				glb_plot_axis=DO_NOT_PLOT_AXIS;
			else
				glb_plot_axis=PLOT_AXIS;
			break;
		case 1:
			glutFullScreen();
			break;
		case 2:
			exit(0);
			break;
		default:
			if (glb_plane_flag[value-3]==ON)
				glb_plane_flag[value-3]=OFF;
			else if (glb_plane_flag[value-3]==OFF)
				glb_plane_flag[value-3]=ON;
			break;	
	}
	glutPostRedisplay();
}

void
vis(int visible)
{
	if (visible==GLUT_VISIBLE) {
		if (glb_spinning)
			glutIdleFunc(animate);
	} else {
		if (glb_spinning)
			glutIdleFunc(NULL);
	}
}

void
zmEmission(float v,float vmin,float vmax,
	float *emission)
{
	float clr;
	clr=(v-vmin)/(vmax-vmin);
	clr=MAX(0.0,MIN(1.0,clr));
	if (glb_hue==1) {
		emission[0]=0.5;
		emission[1]=clr;
		emission[2]=0.3;
	} else {
		emission[0]=clr;
		emission[1]=clr;
		emission[2]=clr;
	}
	emission[3]=glb_alpha;
}
