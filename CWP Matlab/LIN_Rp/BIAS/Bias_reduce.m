%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code computes a hypothetical
% medium parameters satisfying, however,
% the small-incidence-angle conditions
% !!current version supports ORT aligned medium only!!
% !! and Vs corresponds to A55!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all;
clear all;

% Required unique imput:

b_a=0.5572;

Da_a=0.2857;
%Dr_r=0.1816;

%Db_b=0.4400;
%Dd2=-0.3323;

%Dg=-0.19;
%Dd1=0.20;

Dr_r=0.1805;
Db_b=0.4420;
Dg=-0.2161;
Dd2=-0.3057;
Dd1=0.3033;

kappa=0;

% Auxiliar quantities:

D1=-1;
D2=-1;
TERM=1;

% Non-unique input upon request:

while (D1 < 0) | (D2 < 0) & (TERM ~= 0)

  a1=input('\n \nEnter P-wave velocity of the incidence halfspace Vp1=');
  r1=input('Enter density of the incidence halfspace rho1=');
  g1=input('Enter gamma1S of the incidence halfspace gamma1=');
  d11=input('Enter delta1(1) of the incidence halfspace delta1=');
  d12=input('Enter delta1(2) of the incidence halfspace delta2=');
  
  % Determination of the sotropic parameters: 

  a2=(a1*(0.5*Da_a+1))/(1-0.5*Da_a);
  b1=-b_a*(a2+a1)*(0.25*Db_b-0.5);
  b2=b_a*(a2+a1)-b1;
  r2=(r1*(0.5*Dr_r+1))/(1-0.5*Dr_r);
  
  % Determination of the anisotropic parameters: 

  g2=Dg+g1;
  d21=Dd1+d11;
  d22=Dd2+d12;
  e11=0;
  e12=0;
  e21=0;
  e22=0;
  d13=0;
  d23=0;
  
  % Get the Aij for both halfspaces:
  
  A11_1=a1^2;
  A22_1=a1^2;
  A33_1=a1^2;
  A44_1=b1^2*(2*g1+1);
  A55_1=b1^2;
  A66_1=A44_1;
  A12_1=sqrt((A11_1-A66_1)^2)-A66_1;
  A13_1=sqrt(2*A33_1*(A33_1-A55_1)*d12+(A33_1-A55_1)^2)-A55_1;
  A23_1=sqrt(2*A33_1*(A33_1-A44_1)*d11+(A33_1-A44_1)^2)-A44_1;
  A16_1=0;
  A26_1=0;
  A36_1=0;
  
  A11_2=a2^2;
  A22_2=a2^2;
  A33_2=a2^2;
  A44_2=b2^2*(2*g2+1);
  A55_2=b2^2;
  A66_2=A44_2;
  A12_2=sqrt((A11_2-A66_2)^2)-A66_2;
  A13_2=sqrt(2*A33_2*(A33_2-A55_2)*d22+(A33_2-A55_2)^2)-A55_2;
  A23_2=sqrt(2*A33_2*(A33_2-A44_2)*d21+(A33_2-A44_2)^2)-A44_2;
  A16_2=0;
  A26_2=0;
  A36_2=0;
  
  
  % Checking if the stifness tensor theoretically exists

  aij=[A11_1, A12_1, A13_1, A16_1, A22_1, A23_1, A26_1, A33_1, A36_1, A44_1, A55_1, A66_1];
  D1=EnergyTest(aij);
  aij=[A11_2, A12_2, A13_2, A16_2, A22_2, A23_2, A26_2, A33_2, A36_2, A44_2, A55_2, A66_2];
  D2=EnergyTest(aij);
  if (D1 < 0) | (D2 < 0)
    fprintf('\n*** Condition of positive energy is not satisfied \n');
    TERM=input('*** Terminate? (0=yes, 1=no)  \n');
  end;
  if (imag(A12_1)~=0.) | (imag(A13_1)~=0.) | (imag(A23_1)~=0.)
    fprintf('\n***  A negative number under square root: program terminated  ***\n');
    break;
  end;
  if (imag(A12_2)~=0.) | (imag(A13_2)~=0.) | (imag(A23_2)~=0.)
    fprintf('\n***  A negative number under square root: program terminated  ***\n');
    break;
  end;
  
%%%%%%%%%%%%%%%%%%%%
% Print the results:
%%%%%%%%%%%%%%%%%%%%

fprintf('\n \nHASPS 1: Vp=%f \t V44=%f (V55=%f) \t rho=%f \n',a1,sqrt(A44_1), ...
	b1,r1);
fprintf('         eps1=%f \t delta1=%f \t gamma1=%f \n',e11,d11,g1);
fprintf('         eps2=%f \t delta2=%f \t gamma2=%f \n',e12,d12,0);
fprintf('                         delta3=%f              \n',0);
fprintf('\n A11=%f \t A12=%f \t A13=%f \n \t \t A22=%f \t A23=%f \n \t \t \t \t A33=%f \n',A11_1, ...
	 A12_1,A13_1,A22_1,A23_1,A33_1);
fprintf(' \t \t \t \t \t \t A44=%f \t A55=%f \t A66=%f \n',A44_1,A55_1,A66_1);
fprintf('\n --------------------------------------------------------------------------------------\n');

fprintf('\nHASPS 2: Vp=%f \t V44=%f (V55=%f) \t rho=%f \n',a2,sqrt(A44_2), ...
	b2,r2);
fprintf('         eps1=%f \t delta1=%f \t gamma1=%f \n',e21,d21,g2);
fprintf('         eps2=%f \t delta2=%f \t gamma2=%f \n',e22,d22,0);
fprintf('                         delta3=%f              \n',0);
fprintf('\n A11=%f \t A12=%f \t A13=%f \n \t \t A22=%f \t A23=%f \n \t \t \t \t A33=%f \n',A11_2, ...
	 A12_2,A13_2,A22_2,A23_2,A33_2);
fprintf(' \t \t \t \t \t \t A44=%f \t A55=%f \t A66=%f \n',A44_2,A55_2,A66_2);

% print into a file:

fid=fopen('model_bias.in','w');
fprintf(fid,'%d \n',0);
fprintf(fid,'\n%g \n',r1);
fprintf(fid,'%7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g \n',...
	A11_1,A12_1,A13_1,0.0,A22_1,A23_1,0.0,A33_1,0.0,A44_1,A55_1,A66_1);
fprintf(fid,'%g \n',0.0);
fprintf(fid,'\n%g \n',r2);
fprintf(fid,'%7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g %7.3g \n',...
	A11_2,A12_2,A13_2,0.0,A22_2,A23_2,0.0,A33_2,0.0,A44_2,A55_2,A66_2);
fprintf(fid,'%g \n',kappa);

end;


