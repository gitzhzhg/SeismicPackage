%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inversion of azimuthally invariant Rpp and Rps 
% (i.e., ISO or VTI media) - PHASE2: 
%
% the surface parameters from PHASE1, corresponding 
% to the small-medium incidence angles, are further
% inverted for more convenient combinations of medium 
% parameters, knowing vp/vs ratio and either inc. medium 
% parameters or the angle kappa (kappa>=15deg, kappa=<75deg).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;

global data
global b_a

% additional information: b_a ratio is necessary

%b_a=0.520457;        % vs/vp ratio
b_a=0.40;
%control=0;           % ISO medium
control=1;           % VTI medium
in_file='Result_phase1_paper.out';  % the name of input file with Ap,Bp and As
                                      % parameters
%in_file='Result_exact_phase1.out';
%in_file='Result_phase1.out'

% get the data first:

fid=fopen(in_file,'r');
fscanf(fid,'%s',1);
AAp=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBp=fscanf(fid,'%f');
fscanf(fid,'%s',1);
CCp=fscanf(fid,'%f');
fscanf(fid,'%s',1);
AAs=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBs=fscanf(fid,'%f');
fscanf(fid,'%s',1);
CCs=fscanf(fid,'%f');
fscanf(fid,'%s',1);
DDs=fscanf(fid,'%f');

% transfer data into more convenient variables:

Ap=AAp(1,1);
Ap_err=AAp(2,1);
%Ap_err=0.0009;
Bp=BBp(1,1);
Bp_err=BBp(2,1);
%Bp_err=0.0131;
Cp=CCp(1,1);
Cp_err=CCp(2,1);
As=AAs(1,1);
As_err=AAs(2,1)
%As_err=0.0028;
Bs=BBs(1,1);
Bs_err=BBs(2,1);
Cs=CCs(1,1);
Cs_err=CCs(2,1);
Ds=DDs(1,1);
Ds_err=DDs(2,1);

%%%%%%%%%%%%
% INVERSION:
%%%%%%%%%%%%%%%%%%%%%%
% A) isotropic medium:
%%%%%%%%%%%%%%%%%%%%%%
if control==0
  %first, just linear estimate:
  %Ap=0.233766;
  %Bp=-0.075;
  A=[1      1       0;
      1      0  -4*(b_a)^2;
      0    -0.5    -b_a ];
  [u,s,v]=svd(A);
  ss=diag(1./diag(s));
  %ss(3,3)=[50];
  PI=v*ss*u';
  result=PI*[2*Ap;2*Bp;As];
  
  COVtemp=[4*(Ap_err)^2       0         0;
	    0            4*(Bp_err)^2     0;
	    0                  0    (As_err)^2];
  COVARIANCE=PI*COVtemp*PI';
  error=sqrt(diag(COVARIANCE));
  Da_1st=[result(1,1) error(1,1)];
  Dr_1st=[result(2,1) error(2,1)];
  DG_1st=[result(3,1) error(3,1)];

  %% NOTICE! There is a non-linearity in Ap data between Da and Dr. The
  %% non-linearity is, however, very weak. Therefore, there is really no
  %% sence in trying some non-linear inversion of the Ap data.

  %% Here are some stable combinations estimated:

  Da_Dr=[Ap Ap_err];                          % 0.5*Da_a+0.5*Dr_r
  Dr_DG_1=[Ap-Bp sqrt(Ap_err^2+Bp_err^2)];    % 0.5*Dr_r+2(b_a)^2*DG_G
  Dr_DG_2=[As As_err];                        % -0.5*Dr_r-(b_a)*DG_G
					      % ... this is almost the same
					      % combination as Dr_DG_1

					 
  %% Further separation, if additional information available:

  U=input('ISO CONTRAST AVAILABLE: \n 0=Da_a \n 1=Db_b \n 2=Drho_rho \n 3=none \n');

  %% Da_a is known: %%
  if U==0
    Da_a=input('enter the contrast Da_a= ');
    DZ_Z=2*Ap;
    Z_Zerr=2*Ap_err;
    Dr_r(1,1)=(DZ_Z-Da_a)/(1-0.25*Da_a*DZ_Z);
    temp=[abs(Dr_r(1,1)-((DZ_Z+Z_Zerr-Da_a)/(1-0.25*Da_a*(DZ_Z+Z_Zerr))));
	  abs(Dr_r(1,1)-((DZ_Z-Z_Zerr-Da_a)/(1-0.25*Da_a*(DZ_Z-Z_Zerr))))];
    Dr_r(1,2)=max(temp);
    
    A1=[ -4*(b_a)^2;
	        -b_a];
    DATA1=[2*Bp-Da_a; As+0.5*Dr_r(1,1)];
    COVtemp=[4*Bp_err^2 0; 0 As_err^2+0.25*(Dr_r(1,2))^2]; 
    PI1=pinv(A1);
    result1=PI1*DATA1;
    COV1=PI1*COVtemp*PI1';
    
    DG_G=[result1 sqrt(COV1)];
    Db_b(1,1)=G_inv(0,DG_G(1,1),Dr_r(1,1));     % ecaxt inversion
    Db_b_err(1)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r(1,1)+Dr_r(1,2));
    Db_b_err(2)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r(1,1)-Dr_r(1,2));
    Db_b_err(3)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r(1,1)+Dr_r(1,2));
    Db_b_err(4)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r(1,1)-Dr_r(1,2));
    Db_b(1,2)=0.5*(abs(min(Db_b(1,1)-Db_b_err))+abs(max(Db_b(1,1)-Db_b_err)));
    %  Db_b=[0.5*DG_G(1,1)-Dr_r(1,1) 0.5*sqrt(DG_G(1,2)^2+Dr_r(1,2)^2)];
    result1=[Da_a Db_b(1,1) Dr_r(1,1) DG_G(1,1)  DZ_Z  0]
    result1_err=[0 Db_b(1,2) Dr_r(1,2) DG_G(1,2)  Z_Zerr  0]

  %% Db_b is known: %%
  elseif U==1
    Db_b=input('enter the contrast Db_b= ');
    DZ_Z=2*Ap;
    DZ_Z_err=2*Ap_err;
    
    A1=[1        1;
	1  -4*(b_a)^2;
	0  -(1+2*b_a)/2];
    DATA1=[DZ_Z; 2*Bp+8*(b_a)^2*Db_b; As+2*(b_a)*Db_b];
    COVtemp=[DZ_Z_err^2 0 0; 0 4*Bp_err^2 0; 0 0 As_err^2]; 
    PI1=pinv(A1);
    result1=PI1*DATA1;
    COV1=PI1*COVtemp*PI1';
    Dr_r=[result1(2,1) sqrt(COV1(2,2))];
    Da_a(1,1)=(DZ_Z-Dr_r(1,1))/(1-0.25*Dr_r(1,1)*DZ_Z);
    Da_a_err(1)=((DZ_Z+DZ_Z_err)-(Dr_r(1,1)+Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)+Dr_r(1,2))*(DZ_Z+DZ_Z_err));
    Da_a_err(2)=((DZ_Z+DZ_Z_err)-(Dr_r(1,1)-Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)-Dr_r(1,2))*(DZ_Z+DZ_Z_err));
    Da_a_err(3)=((DZ_Z-DZ_Z_err)-(Dr_r(1,1)+Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)+Dr_r(1,2))*(DZ_Z-DZ_Z_err));
    Da_a_err(4)=((DZ_Z-DZ_Z_err)-(Dr_r(1,1)-Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)-Dr_r(1,2))*(DZ_Z-DZ_Z_err));
    Da_a(1,2)=0.5*(abs(min(Da_a(1,1)-Da_a_err))+abs(max(Da_a(1,1)-Da_a_err)));
    %   Da_a=[DZ_Z-Dr_r(1,1) sqrt(DZ_Z_err^2+Dr_r(1,2)^2)];
    DG_G(1,1)=(0.25*Dr_r(1,1)*Db_b^2+Dr_r(1,1)+2*Db_b)/(0.5*Dr_r(1,1)*Db_b+0.25*Db_b^2+1);
    DG_G_err(1)=(0.25*(Dr_r(1,1)+Dr_r(1,2))*Db_b^2+(Dr_r(1,1)+Dr_r(1,2))+2*Db_b)/(0.5*(Dr_r(1,1)+Dr_r(1,2))*Db_b+0.25*Db_b^2+1);
    DG_G_err(2)=(0.25*(Dr_r(1,1)-Dr_r(1,2))*Db_b^2+(Dr_r(1,1)-Dr_r(1,2))+2*Db_b)/(0.5*(Dr_r(1,1)-Dr_r(1,2))*Db_b+0.25*Db_b^2+1);
    DG_G(1,2)=0.5*(abs(min(DG_G(1,1)-DG_G_err))+abs(max(DG_G(1,1)-DG_G_err)));
    %   DG_G=[Dr_r(1,1)+2*Db_b Dr_r(1,2)];
    result1=[Da_a(1,1) Db_b Dr_r(1,1) DG_G(1,1) DZ_Z  0]
    result1_err=[Da_a(1,2) 0 Dr_r(1,2) DG_G(1,2) DZ_Z_err  0]

  %% Dr_r is known: %%
  elseif U==2
    Dr_r=input('enter the contrast Drho_rho= ');
    DZ_Z=2*Ap;
    Z_err=2*Ap_err;
    Da_a(1,1)=(DZ_Z-Dr_r)/(1-0.25*Dr_r*DZ_Z);
    temp=[abs(Da_a(1,1)-((DZ_Z+Z_err-Dr_r)/(1-0.25*Dr_r*(DZ_Z+Z_err))));
	  abs(Da_a(1,1)-((DZ_Z-Z_err-Dr_r)/(1-0.25*Dr_r*(DZ_Z-Z_err))))];
    Da_a(1,2)=max(temp);
    % Da_a(1,1)=2*Ap-Dr_r;
    
    A1=[-4*(b_a)^2;
	 -b_a];
    DATA1=[2*Bp-Da_a(1,1); As+0.5*Dr_r];
    COVtemp=[4*Bp_err^2+Da_a(1,2)^2 0; 0 As_err^2]; 
    PI1=pinv(A1);
    result1=PI1*DATA1;
    COV1=PI1*COVtemp*PI1';
    DG_G=[result1 sqrt(COV1)];
    Db_b(1,1)=G_inv(0,DG_G(1,1),Dr_r(1,1));     % ecaxt inversion
    Db_b_err(1)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r);
    Db_b_err(2)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r);
    Db_b(1,2)=0.5*(abs(min(Db_b(1,1)-Db_b_err))+abs(max(Db_b(1,1)-Db_b_err)));
    %  Db_b=[0.5*(DG_G(1,1)-Dr_r) 0.5*DG_G(1,2)];
    result_abs=[Da_a(1,1) Db_b(1,1) Dr_r DG_G(1,1) DZ_Z 0]
    result_abs_err=[Da_a(1,2) Db_b(1,2) 0 DG_G(1,2) Z_err 0]
  
  else U==3
    result1=[-100 -100 -100 -100 -100 -100];
    result1_err=[-100 -100 -100 -100 -100 -100];
  end;
  
end; 

%%%%%%%%%%
% B) VTI :
%%%%%%%%%%
  
if control==1  

  % some stable combinations first %
  Da_Dr=[Ap Ap_err];
  Da_DG_Dd=[2*Bp 2*Bp_err];
  Dr_DG_Dd=[As As_err];

  %% further separation, if additional information available: %%
  U=input('ISO CONTRAST AVAILABLE: \n 0=Da_a \n 1=Db_b \n 2=Drho_rho \n 3=none \n');

  %% Da_a is known: %%
  if U==0
    Da_a=input('enter the contrast Da_a= ');
    DZ_Z=2*Ap;
    Z_Zerr=2*Ap_err;
    Dr_r(1,1)=(DZ_Z-Da_a)/(1-0.25*Da_a*DZ_Z);
    temp=[abs(Dr_r(1,1)-((DZ_Z+Z_Zerr-Da_a)/(1-0.25*Da_a*(DZ_Z+Z_Zerr))));
	  abs(Dr_r(1,1)-((DZ_Z-Z_Zerr-Da_a)/(1-0.25*Da_a*(DZ_Z-Z_Zerr))))];
    Dr_r(1,2)=max(temp);
    
    A1=[1 -4*(b_a)^2;
	1/(2*(1+b_a)) -b_a];
    DATA1=[2*Bp-Da_a; As+0.5*Dr_r(1,1)];
    COVtemp=[4*Bp_err^2 0; 0 As_err^2+0.25*(Dr_r(1,2))^2]; 
    PI1=pinv(A1);
    result1=PI1*DATA1;
    COV1=PI1*COVtemp*PI1';
    d2_d1=[result1(1,1) sqrt(COV1(1,1))];
    
    DG_G=[result1(2,1) sqrt(COV1(2,2))];
    Db_b(1,1)=G_inv(0,DG_G(1,1),Dr_r(1,1));     % ecaxt inversion
    Db_b_err(1)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r(1,1)+Dr_r(1,2));
    Db_b_err(2)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r(1,1)-Dr_r(1,2));
    Db_b_err(3)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r(1,1)+Dr_r(1,2));
    Db_b_err(4)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r(1,1)-Dr_r(1,2));
    Db_b(1,2)=0.5*(abs(min(Db_b(1,1)-Db_b_err))+abs(max(Db_b(1,1)-Db_b_err)));
    %  Db_b=[0.5*DG_G(1,1)-Dr_r(1,1) 0.5*sqrt(DG_G(1,2)^2+Dr_r(1,2)^2)];
    result1=[Da_a Db_b(1,1) Dr_r(1,1) DG_G(1,1)  DZ_Z  d2_d1(1,1)];
    result1_err=[0 Db_b(1,2) Dr_r(1,2) DG_G(1,2)  Z_Zerr  d2_d1(1,2)];

    %% Db_b is known: %
  elseif U==1
    Db_b=input('enter the contrast Db_b= ');
    DZ_Z=2*Ap;
    DZ_Z_err=2*Ap_err;
    
    A1=[1 -1-4*(b_a)^2;
	1/(2*(1+b_a)) -(1+2*b_a)/2];
    DATA1=[2*Bp-DZ_Z+8*(b_a)^2*Db_b; As+2*(b_a)*Db_b];
    COVtemp=[4*Bp_err^2+DZ_Z_err^2 0; 0 As_err^2]; 
    PI1=pinv(A1);
    result1=PI1*DATA1;
    COV1=PI1*COVtemp*PI1';
    d2_d1=[result1(1,1) sqrt(COV1(1,1))];
    Dr_r=[result1(2,1) sqrt(COV1(2,2))];
    Da_a(1,1)=(DZ_Z-Dr_r(1,1))/(1-0.25*Dr_r(1,1)*DZ_Z);
    Da_a_err(1)=((DZ_Z+DZ_Z_err)-(Dr_r(1,1)+Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)+Dr_r(1,2))*(DZ_Z+DZ_Z_err));
    Da_a_err(2)=((DZ_Z+DZ_Z_err)-(Dr_r(1,1)-Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)-Dr_r(1,2))*(DZ_Z+DZ_Z_err));
    Da_a_err(3)=((DZ_Z-DZ_Z_err)-(Dr_r(1,1)+Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)+Dr_r(1,2))*(DZ_Z-DZ_Z_err));
    Da_a_err(4)=((DZ_Z-DZ_Z_err)-(Dr_r(1,1)-Dr_r(1,2)))/(1-0.25*(Dr_r(1,1)-Dr_r(1,2))*(DZ_Z-DZ_Z_err));
    Da_a(1,2)=0.5*(abs(min(Da_a(1,1)-Da_a_err))+abs(max(Da_a(1,1)-Da_a_err)));
    %   Da_a=[DZ_Z-Dr_r(1,1) sqrt(DZ_Z_err^2+Dr_r(1,2)^2)];
    DG_G(1,1)=(0.25*Dr_r(1,1)*Db_b^2+Dr_r(1,1)+2*Db_b)/(0.5*Dr_r(1,1)*Db_b+0.25*Db_b^2+1);
    DG_G_err(1)=(0.25*(Dr_r(1,1)+Dr_r(1,2))*Db_b^2+(Dr_r(1,1)+Dr_r(1,2))+2*Db_b)/(0.5*(Dr_r(1,1)+Dr_r(1,2))*Db_b+0.25*Db_b^2+1);
    DG_G_err(2)=(0.25*(Dr_r(1,1)-Dr_r(1,2))*Db_b^2+(Dr_r(1,1)-Dr_r(1,2))+2*Db_b)/(0.5*(Dr_r(1,1)-Dr_r(1,2))*Db_b+0.25*Db_b^2+1);
    DG_G(1,2)=0.5*(abs(min(DG_G(1,1)-DG_G_err))+abs(max(DG_G(1,1)-DG_G_err)));
    %   DG_G=[Dr_r(1,1)+2*Db_b Dr_r(1,2)];
    result1=[Da_a(1,1) Db_b Dr_r(1,1) DG_G(1,1) DZ_Z d2_d1(1,1)];
    result1_err=[Da_a(1,2) 0 Dr_r(1,2) DG_G(1,2) DZ_Z_err d2_d1(1,2)];

  %% Dr_r is known: %%
  elseif U==2
    Dr_r=input('enter the contrast Drho_rho= ');
    DZ_Z=2*Ap;
    Z_err=2*Ap_err;
    Da_a(1,1)=(DZ_Z-Dr_r)/(1-0.25*Dr_r*DZ_Z);
    temp=[abs(Da_a(1,1)-((DZ_Z+Z_err-Dr_r)/(1-0.25*Dr_r*(DZ_Z+Z_err))));
	  abs(Da_a(1,1)-((DZ_Z-Z_err-Dr_r)/(1-0.25*Dr_r*(DZ_Z-Z_err))))];
    Da_a(1,2)=max(temp);
    % Da_a(1,1)=2*Ap-Dr_r;
    
    A1=[1 -4*(b_a)^2;
	1/(2*(1+b_a)) -b_a];
    DATA1=[2*Bp-Da_a(1,1); As+0.5*Dr_r];
    COVtemp=[4*Bp_err^2+Da_a(1,2)^2 0; 0 As_err^2]; 
    PI1=pinv(A1);
    result1=PI1*DATA1;
    COV1=PI1*COVtemp*PI1';
    d2_d1=[result1(1,1) sqrt(COV1(1,1))];
    DG_G=[result1(2,1) sqrt(COV1(2,2))];
    Db_b(1,1)=G_inv(0,DG_G(1,1),Dr_r(1,1));     % ecaxt inversion
    Db_b_err(1)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r);
    Db_b_err(2)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r);
    Db_b(1,2)=0.5*(abs(min(Db_b(1,1)-Db_b_err))+abs(max(Db_b(1,1)-Db_b_err)));
    %  Db_b=[0.5*(DG_G(1,1)-Dr_r) 0.5*DG_G(1,2)];
    result1=[Da_a(1,1) Db_b(1,1) Dr_r DG_G(1,1) DZ_Z d2_d1(1,1)];
    result1_err=[Da_a(1,2) Db_b(1,2) 0 DG_G(1,2) Z_err d2_d1(1,2)];

  else U==3
    result_abs=[-100 -100 -100 -100 -100 -100];
    result_abs_err=[-100 -100 -100 -100 -100 -100];
  
  end;

end;
  
  %now, with this first estimate, run a non-linear inversion
  % IT DOES MAKE NO SENCE SINCE THE PROBLEM IS ALMOST LINEAR -
  % the results from linear inversion will not change almost
  % at all. Look at the errors.
%  data=[2*Ap Bp As];
%  INITIAL=[Da_1st(1,1)  Dr_1st(1,1)  DG_1st(1,1)];  % the initial estimate
%  LB=[-2 -2 -2];                     % the lower boundary on Da,Dr and DG
%  UB=[2 2 2];                        % the upper boundary on Da,Dr and DG
%  tolParam = eps;
%  OPTIONS = optimset('DerivativeCheck','off','Diagnostics','off',  'LargeScale', 'on', 'Display', 'on', ...
%		     'LevenbergMarquardt','off','Jacobian', 'off','MaxFunEvals',500,'MaxIter', 200, ...
%		     'TolX', tolParam,   'TolFun', tolParam);

% [result,resnorm,residual,exitflag,output] = lsqnonlin('ISO_objective',INITIAL,LB,UB,OPTIONS,b_a);
% exitflag
% result


%%%%%%%%%%%%%%%%%%
% C) write the results into a table file
%%%%%%%%%%%%%%%%%%
fid=fopen('Result_phase2.out','w');
fprintf(fid,'PHASE II INVERSION RESULTS FOR ISO-VTI MEDIA (Invert_R_I_phase2 output): \n');
fprintf(fid,'\nVs/Vp is known (Vs/Vp=%f): \n',b_a);
fprintf(fid,'\n \n');

fprintf(fid,'\nStable combinations:\n');
fprintf(fid,'-----------------------------\n');
if control==0
  fprintf(fid,'0.5*Da_a + 0.5*Dr_r = %f +- %f\n',Da_Dr(1,1), Da_Dr(1,2));
  fprintf(fid,'0.5*Dr_r + 2*(b_a)^2*DG_G = %f +- %f\n',Dr_DG_1(1,1), Dr_DG_1(1,2));
  fprintf(fid,'-0.5*Dr_r - (b_a)*DG_G = %f +- %f\n',Dr_DG_2(1,1), Dr_DG_2(1,2));

  fprintf(fid,'\nUnstable splitting of ISO contrasts:\n');
  fprintf(fid,'-----------------------------------\n'); 
  fprintf(fid,'Da_a = %f +- %f\n',Da_1st(1,1), Da_1st(1,2));
  fprintf(fid,'Dr_r = %f +- %f\n',Dr_1st(1,1), Dr_1st(1,2));
  fprintf(fid,'DG_G = %f +- %f\n',DG_1st(1,1), DG_1st(1,2));
end;
if control==1
  fprintf(fid,'0.5*Da_a + 0.5*Dr_r = %f +- %f\n',Da_Dr(1,1), Da_Dr(1,2));
  fprintf(fid,'Da_a - 4(b_a)^2*DG_G + Dd = %f +- %f\n',Da_DG_Dd(1,1), Da_DG_Dd(1,2));
  fprintf(fid,'-0.5*Dr_r - (b_a)*DG_G + 1/(2*(1+b_a))*Dd = %f +- %f\n',Dr_DG_Dd(1,1), Dr_DG_Dd(1,2));
end;

fprintf(fid,'\nSplitting after an additional input of an isotropic contrast:\n');
fprintf(fid,'------------------------------------------------------------\n'); 
fprintf(fid,'\n!!! if the following quantities are equal to -100, no enough information was available !!! \n');
fprintf(fid,'\nDVp/Vp = %f +- %f \n',result1(1,1),result1_err(1,1));
fprintf(fid,'DVs/Vs = %f +- %f \n',result1(1,2),result1_err(1,2));
fprintf(fid,'Drho/rho = %f +- %f \n',result1(1,3),result1_err(1,3));
fprintf(fid,'DG/G = %f +- %f \n',result1(1,4),result1_err(1,4));
fprintf(fid,'DZ/Z = %f +- %f \n',result1(1,5),result1_err(1,5));
fprintf(fid,'delta2_2-delta1_2 = %f +- %f \n',result1(1,6),result1_err(1,6));

fclose(fid);
