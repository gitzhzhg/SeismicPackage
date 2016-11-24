
#include <cwp.h>

int
main()
{
  int i,j,nx,nz;
  float pi,x,dx,z,dz,*a;
  float blvl,refr;
  float **vel,base;
  FILE *gp,*hp,*ip;

  pi = 3.1415927;
  dx = 50.0;
  dz = 2.0;
  
  gp = fopen("blvl_file.dat","w");
  hp = fopen("refr_file.dat","w");
  ip = fopen("vel_file.dat","w");

  nx = 657;
  nz = 76;

  vel = (float **)malloc(nx*sizeof(float *));
  a = (float *)malloc(nx*nz*sizeof(float));
  for(i=0;i<nx;i++) vel[i] = &(a[i*nz]);

  for(i=0;i<nx;i++)
  {
    x = i*dx;
    blvl = 150.0;
    base = 100.0+40.0*sin(2.0*pi*x/6000.0);
    refr = 0.0;
    fprintf(gp,"%f\n",blvl);
    fprintf(hp,"%f\n",refr);

    for(j=0;j<nz;j++)
    {
      z = j*dz;
      if(z <= base)
      {
        vel[i][j] = 1640.0;
        fprintf(ip,"%f\n",vel[i][j]);
      }
      if(z > base)
      {
        vel[i][j] = 9840.0;
        fprintf(ip,"%f\n",vel[i][j]);
      } 
    }
  }

  fclose(gp);
  fclose(hp);
  fclose(ip);
	return EXIT_SUCCESS;
}
