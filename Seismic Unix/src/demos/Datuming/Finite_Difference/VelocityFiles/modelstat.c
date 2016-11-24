#include "cwp.h"

int
main()
{
  int i,j,nx,nz;
  float f,x,dx,diff,V_low,z,dz;
  float V_1,V_2,V_very_low,f1,f2;
  FILE *zsp,*zrp,*vfp1,*vfp2;

  nx = 320;
  dx = 50.0; 
  V_low = 3280.0;

  zsp=fopen("zsou.dat","w");
  zrp=fopen("zrec.dat","w");
  vfp1=fopen("vfp1.dat","w");
  vfp2=fopen("vfp2.dat","w");

  for(i=0;i<nx;i++)
  {
    x = i*dx + 100.0;
    f = x/18.0 - 1000.0;
    diff = f + 1000.0;
    fprintf(zsp,"%f\n",1000.0*diff/(V_low));
    fprintf(zrp,"%f\n",1000.0*diff/(V_low));
  }

  nz = 161;
  dz = 10.0;
  V_1 = 1000.0*3.28;
  V_2 = 3000.0*3.28;
  V_very_low = 10.0*3.28;

  for(i=0;i<nx+40;i++)
  {
    x = i*dx;
    f1 = x/18.0 - 1000.0;
    f2 = -5.0*x/180.0 + 600.0;
    for(j=0;j<nz;j++)
    {
      z = j*dz - 1000.0;
      if(z <= f1)
      {
        fprintf(vfp1,"%f\n",V_1);
        fprintf(vfp2,"%f\n",V_very_low);
      }
      if(z <= f2 && z > f1)
      {
        fprintf(vfp1,"%f\n",V_1);
        fprintf(vfp2,"%f\n",V_1);
      }
      if(z > f2)
      {
        fprintf(vfp1,"%f\n",V_2);
        fprintf(vfp2,"%f\n",V_2);
      }
    }/*end for j*/
  }/*end for i*/

  fclose(zsp);
  fclose(zrp);
  fclose(vfp1);
  fclose(vfp2);
   return EXIT_SUCCESS;
}
