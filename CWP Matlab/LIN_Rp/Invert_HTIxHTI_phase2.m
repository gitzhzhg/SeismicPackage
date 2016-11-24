%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inversion of Rpp and Rps from data file - PHASE2
% (this version supports the HTI/HTI_misalligned interface - 
% do not use it for ORT interfaces):
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

% additional information: b_a ratio is necessary

%b_a=0.55715;        % vs/vp ratio
b_a=0.3578;
nevim=1;
%inc_known=0;
%kappa_known=0;
%delta1_1=-0.2; inc_known=1;
%delta1_2=0.1;
%gamma1_1=0.1;
%gamma1_2=0.0;
SG2=-100;             % sign of gamma2_s: necessary for unique estimate of
                    % kappa; if not provided, kappa is not estimated uniquelly!
		    % if you do not know SG2, put SG2=100;
%kappa=20*pi/180; kappa_known=1;  % provided kappa must be from <-90,90>

in_file='Result_phase1.out';  % the name of input file with Ap,Bp and As
                        % parameters
%in_file='Result_exact_phase1.out';

%control=1;
%if SG2==1
%  SG2=1;
%elseif SG2==-1
%  SG2=-1;
%else
%  SG2=100;
%end;
% get the data first:

fid=fopen(in_file,'r');
fscanf(fid,'%s',1);
AAp=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBp1=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBp2=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBp3=fscanf(fid,'%f');
fscanf(fid,'%s',1);
AAs1=fscanf(fid,'%f');
fscanf(fid,'%s',1);
AAs2=fscanf(fid,'%f');
fscanf(fid,'%s',1);
AAs3=fscanf(fid,'%f');

% transfer data into more convenient variables:

Ap=AAp(1,1);
Ap_err=AAp(2,1);
Bp1=BBp1(1,1);
Bp1_err=BBp1(2,1);
Bp2=BBp2(1,1);
%Bp2=0.035;
Bp2_err=BBp2(2,1);
Bp3=BBp3(1,1);
%Bp3=0.097;
Bp3_err=BBp3(2,1);
As1=AAs1(1,1);
As1_err=AAs1(2,1);
As2=AAs2(1,1);
%As2=0.05833;
As2_err=AAs2(2,1);
As3=AAs3(1,1);
As3_err=AAs3(2,1);


%%%%%%%%%%%%
% INVERSION:
%%%%%%%%%%%%

%
% Vs/Vp ratio is known only:
%

A1=[0.5  4*(b_a)^2; 1/(2*(1+b_a)) 2*b_a];
[u1,s1,v1]=svd(A1);
ss1=diag(1./diag(s1));
PI1=v1*ss1*u1';

COV1temp_cos=[(Bp3_err)^2 0;
	     0 (As3_err)^2];
COVARIANCE1_cos=PI1*COV1temp_cos*PI1';
RESOL1=PI1*A1;

result1_cos=PI1*[Bp3;As3];      
error1_cos=sqrt(diag(COVARIANCE1_cos));
d1_d2cos=[result1_cos(1,1) error1_cos(1,1)];              % this is d1_(V)-d2_(V)cos(2*kappa)
g1_g2cos=[result1_cos(2,1) error1_cos(2,1)];	         % this is g1_s-g2_s*cos(2*kappa)					
COV1temp_sin=[(Bp2_err)^2 0;
	     0 (As2_err)^2];
COVARIANCE1_sin=PI1*COV1temp_sin*PI1';

result1_sin=PI1*[Bp2;As2];      
error1_sin=sqrt(diag(COVARIANCE1_sin));
d2sin=[result1_sin(1,1) error1_sin(1,1)];                % this is d2_(V)*sin(2*kappa)
g2sin=[result1_sin(2,1) error1_sin(2,1)];                % this is
                                                         % g2_s*sin(2*kapp)
%
% gamma1 plus Vs/Vp is known only:
%

  gamma1=-100;
  gamma1_err=-100;
  g2cos1st=-100;
  g2cos1st_err=-100;
  kappa1st=-100;
  kappa1st_err=-100;
  g2=-100;
  g2_err=-100;
  d1=-100;
  d1_err=-100;
  d2=-100;
  d2_err=-100;
  
  gamma1=input('enter the gamma1, if unknown, enter -100: gamma1 = ');
  if gamma1 ~=-100
    gamma1_err=0;
    g2cos1st=-g1_g2cos(1,1)+gamma1;
    g2cos1st_err=g1_g2cos(1,2);

    kappa_error(1)=0.5*atan(g2sin(1,1)/g2cos1st)*180/pi;
    kappa_error(2)=0.5*atan((g2sin(1,1)+g2sin(1,2))/(g2cos1st+g2cos1st_err))*180/pi;
    kappa_error(3)=0.5*atan((g2sin(1,1)+g2sin(1,2))/(g2cos1st-g2cos1st_err))*180/pi;
    kappa_error(4)=0.5*atan((g2sin(1,1)-g2sin(1,2))/(g2cos1st+g2cos1st_err))*180/pi;
    kappa_error(5)=0.5*atan((g2sin(1,1)-g2sin(1,2))/(g2cos1st-g2cos1st_err))*180/pi;
    kappa1st=mean(kappa_error);
    kappa1st_err=sqrt(var(kappa_error));
    
    kappa=kappa1st;
    kappa_err=kappa1st_err;
    g2_error1(1)=g2cos1st/cos(2*kappa*pi/180);
    g2_error1(2)=(g2cos1st+g2cos1st_err)/cos(2*(kappa+kappa_err)*pi/180);
    g2_error1(3)=(g2cos1st+g2cos1st_err)/cos(2*(kappa-kappa_err)*pi/180);
    g2_error1(4)=(g2cos1st-g2cos1st_err)/cos(2*(kappa+kappa_err)*pi/180);
    g2_error1(5)=(g2cos1st-g2cos1st_err)/cos(2*(kappa-kappa_err)*pi/180);
    g2_error2(1)=g2sin(1,1)/sin(2*kappa*pi/180);
    g2_error2(2)=(g2sin(1,1)+g2sin(1,2))/sin(2*(kappa+kappa_err)*pi/180);
    g2_error2(3)=(g2sin(1,1)+g2sin(1,2))/sin(2*(kappa-kappa_err)*pi/180);
    g2_error2(4)=(g2sin(1,1)-g2sin(1,2))/sin(2*(kappa+kappa_err)*pi/180);
    g2_error2(5)=(g2sin(1,1)-g2sin(1,2))/sin(2*(kappa-kappa_err)*pi/180);
    g21=mean(g2_error1);
    g21_err=sqrt(var(g2_error1));
    g22=mean(g2_error2);
    g22_err=sqrt(var(g2_error2));
    if min([g21_err g22_err])==g21_err
      g2=g21;
      g2_err=g2_error1;
    else
      g2=g22;
      g2_err=g22_err;
    end;
    
    d2_error(1)=d2sin(1,1)/sin(2*kappa*pi/180);
    d2_error(2)=(d2sin(1,1)+d2sin(1,2))/sin(2*(kappa+kappa_err)*pi/180);
    d2_error(3)=(d2sin(1,1)+d2sin(1,2))/sin(2*(kappa-kappa_err)*pi/180);
    d2_error(4)=(d2sin(1,1)-d2sin(1,2))/sin(2*(kappa+kappa_err)*pi/180);
    d2_error(5)=(d2sin(1,1)-d2sin(1,2))/sin(2*(kappa-kappa_err)*pi/180);
    d2=mean(d2_error);
    d2_err=sqrt(var(d2_error));
    
    d1=d1_d2cos(1,1)+d2*cos(2*kappa*pi/180);
    d1_err=sqrt(d1_d2cos(1,2)^2 + (d2_err^2*cos(2*kappa_err*pi/180)^2+d2^2*cos(2* ...
						  kappa_err*pi/180)^2 ...
					+d2_err^2*cos(2*kappa*pi/180)^2));
    
  
    % this was ment to be for the case when I know gamma1 and one iso
    % contrast, but no delta1. It would give just a little bit worse
    % estimates as with delta1 provided.
    
    %d2_g2sinP2(1,1)=0.5*(d2+8*(b_a)^2*g2)*sin(kappa*pi/180)^2;
    %d2_g2sinS2(1,1)=(1/(2*(1+b_a))*d2+2*b_a*g2)*sin(kappa*pi/180)^2;
    %err_temp2=0.25*d2_err^2+16*(b_a)^4*g2_err^2;
    %d2_g2sinP2(1,2)=err_temp2*sin blblblblaaaa
  end;						 
  
%
% gamma1 and delta1 and Vs/Vp are known:
%
  delta1=input('enter the delta1(V), if unknown, enter -100: delta1(V) = ');

  if gamma1 ~=-100 & delta1~=-100

    gamma1_s=gamma1;
    delta1_2=delta1;
    delta1_1=0;
    %gamma1_s=(gamma1_1-gamma1_2)/(1+2*gamma1_2);
    d2cos=[-d1_d2cos(1,1)+(delta1_2-delta1_1) error1_cos(1,1)];           % this is d2(V)*cos(2*kappa)
    g2cos=[-g1_g2cos(1,1)+gamma1_s error1_cos(2,1)];           % this is g2_s*cos(2*kappa)
    
    
    % estimate the angle kappa: 
    % you should know the sign of gamma to do that uniquely
    
    d2_g2_cosP=[-Bp3+0.5*(delta1_2-delta1_1+8*(b_a)^2*gamma1_s)  Bp3_err];  
    % this is
    % 0.5*[(d2_2-d2_1+8*(b_a)^2*g2_s)*cos(2*kappa)
    
    d2_g2_cosS=[-As3+1/(2*(1+b_a))*(delta1_2-delta1_1)+2*b_a*gamma1_s As3_err];
    % this is
    % 1/(2*(1+b_a))*(d2_2-d2_1+2*b_a*g2_s)*cos(2*kappa)
						 
    kappa1=0.5*atan(Bp2/d2_g2_cosP(1,1))*180/pi;
    kappa1_err(1)=0.5*atan((Bp2+Bp2_err)/(d2_g2_cosP(1,1)+d2_g2_cosP(1,2)))*180/pi;
    kappa1_err(2)=0.5*atan((Bp2+Bp2_err)/(d2_g2_cosP(1,1)-d2_g2_cosP(1,2)))*180/pi;
    kappa1_err(3)=0.5*atan((Bp2-Bp2_err)/(d2_g2_cosP(1,1)+d2_g2_cosP(1,2)))*180/pi;
    kappa1_err(4)=0.5*atan((Bp2-Bp2_err)/(d2_g2_cosP(1,1)-d2_g2_cosP(1,2)))*180/pi;
    kappa1=mean(kappa1_err);
    kappa1_error=sqrt(var(kappa1_err));

    kappa2=0.5*atan(As2/d2_g2_cosS(1,1))*180/pi;
    kappa2_err(1)=0.5*atan((As2+As2_err)/(d2_g2_cosS(1,1)+d2_g2_cosS(1,2)))*180/pi;
    kappa2_err(2)=0.5*atan((As2+As2_err)/(d2_g2_cosS(1,1)-d2_g2_cosS(1,2)))*180/pi;
    kappa2_err(3)=0.5*atan((As2-As2_err)/(d2_g2_cosS(1,1)+d2_g2_cosS(1,2)))*180/pi;
    kappa2_err(4)=0.5*atan((As2-As2_err)/(d2_g2_cosS(1,1)-d2_g2_cosS(1,2)))*180/pi;
    kappa2=mean(kappa2_err);
    kappa2_error=sqrt(var(kappa2_err));

    if SG2~=-100
      if (sign(tan(2*kappa1*pi/180))*SG2) ~= sign(g2cos(1,1))
	kappa=kappa1+90.;
      end;
      if (sign(tan(2*kappa2*pi/180))*SG2) ~= sign(g2cos(1,1))
	kappa2=kappa2+90.;
      end;   
    end;

    if min([kappa1_error kappa2_error])==kappa1_error
      kappa=[kappa1  kappa1_error];
    else
      kappa=[kappa2  kappa2_error];
    end;
    
    if SG2 ==-100
      fprintf('Sign of gamma2_s is unknown: \n1 ... kappa=%f deg \n2 ... kappa=%f deg\n',kappa(1,1), kappa(1,1)+90);
      pick=input('\nPick one of two possible kappas\n');
      if pick==2
	kappa=kappa+90;
      end;
    end;
  
  % get delta2 and gamma2 here:
  
    g2b_error1(1)=g2cos(1,1)/cos(2*kappa(1,1)*pi/180);
    g2b_error1(2)=(g2cos(1,1)+g2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
    g2b_error1(3)=(g2cos(1,1)+g2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
    g2b_error1(4)=(g2cos(1,1)-g2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
    g2b_error1(5)=(g2cos(1,1)-g2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
    g2b_error2(1)=g2sin(1,1)/sin(2*kappa(1,1)*pi/180);
    g2b_error2(2)=(g2sin(1,1)+g2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
    g2b_error2(3)=(g2sin(1,1)+g2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
    g2b_error2(4)=(g2sin(1,1)-g2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
    g2b_error2(5)=(g2sin(1,1)-g2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
    g21b=mean(g2b_error1);
    g21b_err=sqrt(var(g2b_error1));
    g22b=mean(g2b_error2);
    g22b_err=sqrt(var(g2b_error2));
    if min([g21b_err g22b_err])==g21b_err
      g2b=g21b;
      g2b_err=g2b_error1;
    else
      g2b=g22b;
      g2b_err=g22b_err;
    end;

    d2b_error1(1)=d2cos(1,1)/cos(2*kappa(1,1)*pi/180);
    d2b_error1(2)=(d2cos(1,1)+d2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
    d2b_error1(3)=(d2cos(1,1)+d2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
    d2b_error1(4)=(d2cos(1,1)-d2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
    d2b_error1(5)=(d2cos(1,1)-d2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
    d2b_error2(1)=d2sin(1,1)/sin(2*kappa(1,1)*pi/180);
    d2b_error2(2)=(d2sin(1,1)+d2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
    d2b_error2(3)=(d2sin(1,1)+d2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
    d2b_error2(4)=(d2sin(1,1)-d2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
    d2b_error2(5)=(d2sin(1,1)-d2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
    d21b=mean(d2b_error1);
    d21b_err=sqrt(var(d2b_error1));
    d22b=mean(d2b_error2);
    d22b_err=sqrt(var(d2b_error2));
    if min([d21b_err d22b_err])==d21b_err
      d2b=d21b;
      d2b_err=d2b_error1;
    else
      d2b=d22b;
      d2b_err=d22b_err;
    end;
 


  
%  
% one iso contrast is known as well:  
%  

    sga=sign(d2_g2_cosP(1,1))*sign(cos(2*kappa(1,1)*pi/180));
    d2_g2_sinP2(1,1)=0.5*(sga*sqrt(Bp2^2+d2_g2_cosP(1,1)^2) - d2_g2_cosP(1,1)) 
                                                 % this is
                                                 % 0.5*[(d2(V)+8*(b_a)^2*g2_s)*sin(kappa)^2]
    d2_g2_sinP2_err(1)=0.5*(sga*sqrt((Bp2+Bp2_err)^2+(d2_g2_cosP(1,1)+d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)+d2_g2_cosP(1,2)));
    d2_g2_sinP2_err(2)=0.5*(sga*sqrt((Bp2+Bp2_err)^2+(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))); 
    d2_g2_sinP2_err(3)=0.5*(sga*sqrt((Bp2-Bp2_err)^2+(d2_g2_cosP(1,1)+d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)+d2_g2_cosP(1,2))); 
    d2_g2_sinP2_err(4)=0.5*(sga*sqrt((Bp2-Bp2_err)^2+(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))); 
    d2_g2_sinP2(1,2)=max(abs(d2_g2_sinP2(1,1)-d2_g2_sinP2_err));
						 
    sga=sign(d2_g2_cosS(1,1))*sign(cos(2*kappa(1,1)*pi/180));
    d2_g2_sinS2(1,1)=0.5*(sga*sqrt(As2(1,1)^2+d2_g2_cosS(1,1)^2) - d2_g2_cosS(1,1)) 
                                                 % this is
                                                 % (1/(2*(1+b_a))*d2(V)+2*b_a*g2_s)*sin(kappa)^2
    d2_g2_sinS2_err(1)=0.5*(sga*sqrt((As2+As2_err)^2+(d2_g2_cosS(1,1)+d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)+d2_g2_cosS(1,2)));
    d2_g2_sinS2_err(2)=0.5*(sga*sqrt((As2+As2_err)^2+(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))); 
    d2_g2_sinS2_err(3)=0.5*(sga*sqrt((As2-As2_err)^2+(d2_g2_cosS(1,1)+d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)+d2_g2_cosS(1,2))); 
    d2_g2_sinS2_err(4)=0.5*(sga*sqrt((As2-As2_err)^2+(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))); 
    d2_g2_sinS2(1,2)=max(abs(d2_g2_sinS2(1,1)-d2_g2_sinS2_err));


    % P-wave absolute term: (d2(V)-d1(V)+Da_a-(2*b_a)^2*DG_G):
    P_abs(1,1)=2*(Bp1+d2_g2_sinP2(1,1));
    P_abs(1,2)=2*sqrt(Bp1_err^2+d2_g2_sinP2(1,2)^2);

    % S-wave absolute term: 1/(2*(1+b_a))*(d22-d12) - (1+2*b_a)/2*Dr_r - 2*b_a*Db_b:
    S_abs(1,1)=As1+d2_g2_sinS2(1,1);
    S_abs(1,2)=sqrt(As1_err^2+d2_g2_sinS2(1,2)^2);
    
    % further separation, if additional information available:
    U=input('ISO CONTRAST AVAILABLE: \n 0=Da_a \n 1=Db_b \n 2=Drho_rho \n 3=none \n');
    
    % Da_a is known:
    if U==0
      Da_a=input('enter the contrast Da_a= ');
      DZ_Z=2*Ap;
      Z_Zerr=2*Ap_err;
      Dr_r(1,1)=(DZ_Z-Da_a)/(1-0.25*Da_a*DZ_Z);
      temp=[abs(Dr_r(1,1)-((DZ_Z+Z_Zerr-Da_a)/(1-0.25*Da_a*(DZ_Z+Z_Zerr))));
	    abs(Dr_r(1,1)-((DZ_Z-Z_Zerr-Da_a)/(1-0.25*Da_a*(DZ_Z-Z_Zerr))))];
      Dr_r(1,2)=max(temp);
      
      A3=[1 -4*(b_a)^2;
	  1/(2*(1+b_a)) -b_a];
      DATA3=[P_abs(1,1)-Da_a; S_abs(1,1)+0.5*Dr_r(1,1)];
      COVtemp=[P_abs(1,2)^2 0; 0 S_abs(1,2)^2+0.25*(Dr_r(1,2))^2]; 
      PI3=pinv(A3);
      result3=PI3*DATA3;
      COV3=PI3*COVtemp*PI3';
      d2_d1=[result3(1,1) sqrt(COV3(1,1))];
      
      DG_G=[result3(2,1) sqrt(COV3(2,2))];
      Db_b(1,1)=G_inv(0,DG_G(1,1),Dr_r(1,1));     % ecaxt inversion
      Db_b_err(1)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r(1,1)+Dr_r(1,2));
      Db_b_err(2)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r(1,1)-Dr_r(1,2));
      Db_b_err(3)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r(1,1)+Dr_r(1,2));
      Db_b_err(4)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r(1,1)-Dr_r(1,2));
      Db_b(1,2)=0.5*(abs(min(Db_b(1,1)-Db_b_err))+abs(max(Db_b(1,1)-Db_b_err)));
      %  Db_b=[0.5*DG_G(1,1)-Dr_r(1,1) 0.5*sqrt(DG_G(1,2)^2+Dr_r(1,2)^2)];
      result_abs=[Da_a Db_b(1,1) Dr_r(1,1) DG_G(1,1)  DZ_Z  d2_d1(1,1)];
      result_abs_err=[0 Db_b(1,2) Dr_r(1,2) DG_G(1,2)  Z_Zerr  d2_d1(1,2)];
    end;
    
    % Db_b is known:
    if U==1
      Db_b=input('enter the contrast Db_b= ');
      DZ_Z=2*Ap;
      DZ_Z_err=2*Ap_err;
      
      A3=[1 -1-4*(b_a)^2;
	  1/(2*(1+b_a)) -(1+2*b_a)/2];
      DATA3=[P_abs(1,1)-DZ_Z+8*(b_a)^2*Db_b; S_abs(1,1)+2*(b_a)*Db_b];
      COVtemp=[P_abs(1,2)^2+DZ_Z_err^2 0; 0 S_abs(1,2)^2]; 
      PI3=pinv(A3);
      result3=PI3*DATA3;
      COV3=PI3*COVtemp*PI3';
      d2_d1=[result3(1,1) sqrt(COV3(1,1))];
      Dr_r=[result3(2,1) sqrt(COV3(2,2))];
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
      result_abs=[Da_a(1,1) Db_b Dr_r(1,1) DG_G(1,1) DZ_Z d2_d1(1,1)];
      result_abs_err=[Da_a(1,2) 0 Dr_r(1,2) DG_G(1,2) DZ_Z_err d2_d1(1,2)];
    end;

    % Dr_r is known:
    if U==2
      Dr_r=input('enter the contrast Drho_rho= ');
      DZ_Z=2*Ap;
      Z_err=2*Ap_err;
      Da_a(1,1)=(DZ_Z-Dr_r)/(1-0.25*Dr_r*DZ_Z);
      temp=[abs(Da_a(1,1)-((DZ_Z+Z_err-Dr_r)/(1-0.25*Dr_r*(DZ_Z+Z_err))));
	    abs(Da_a(1,1)-((DZ_Z-Z_err-Dr_r)/(1-0.25*Dr_r*(DZ_Z-Z_err))))];
      Da_a(1,2)=max(temp);
      % Da_a(1,1)=2*Ap-Dr_r;
      
      A3=[1 -4*(b_a)^2;
	  1/(2*(1+b_a)) -b_a];
      DATA3=[P_abs(1,1)-Da_a(1,1); S_abs(1,1)+0.5*Dr_r];
      COVtemp=[P_abs(1,2)^2+Da_a(1,2)^2 0; 0 S_abs(1,2)^2]; 
      PI3=pinv(A3);
      result3=PI3*DATA3;
      COV3=PI3*COVtemp*PI3';
      d2_d1=[result3(1,1) sqrt(COV3(1,1))];
      DG_G=[result3(2,1) sqrt(COV3(2,2))];
      Db_b(1,1)=G_inv(0,DG_G(1,1),Dr_r(1,1));     % ecaxt inversion
      Db_b_err(1)=G_inv(0,DG_G(1,1)+DG_G(1,2),Dr_r);
      Db_b_err(2)=G_inv(0,DG_G(1,1)-DG_G(1,2),Dr_r);
      Db_b(1,2)=0.5*(abs(min(Db_b(1,1)-Db_b_err))+abs(max(Db_b(1,1)-Db_b_err)));
      %  Db_b=[0.5*(DG_G(1,1)-Dr_r) 0.5*DG_G(1,2)];
      result_abs=[Da_a(1,1) Db_b(1,1) Dr_r DG_G(1,1) DZ_Z d2_d1(1,1)];
      result_abs_err=[Da_a(1,2) Db_b(1,2) 0 DG_G(1,2) Z_err d2_d1(1,2)];
      
      
    end;
    if U==3
      result_abs=[-100 -100 -100 -100 -100 -100];
      result_abs_err=[-100 -100 -100 -100 -100 -100];
    end;
  else
    result_abs=[-100 -100 -100 -100 -100 -100];
    result_abs_err=[-100 -100 -100 -100 -100 -100];
  end;

if result_abs(1,6)~=-100
  d2bb=result_abs(1,6)+delta1;
  d2bb_err=result_abs_err(1,6);
  d2best=(d2b*d2b_err^2 + d2bb*d2bb_err^2)/ ...
	 (d2b_err^2+d2bb_err^2);
  d2best_err=sqrt((d2b_err^2*d2bb_err^2)/(d2b_err^2+d2bb_err^2));
  result_abs(1,6)=d2best-delta1;
  result_abs_err(1,6)=d2best_err;
else
  d2bb=-100;
  d2bb_err=-100;
  d2best=-100;
  d2best_err=-100;
end;

%%%%%%%%%%%%%%%%%%
% E) write the results into a table file
%%%%%%%%%%%%%%%%%%
fid=fopen('Result_phase2.out','w');
fprintf(fid,'PHASE II INVERSION RESULTS (Invert_R_I_phase2 output): \n');
fprintf(fid,'\nMost stable combinations:\n');
fprintf(fid,'-----------------------------\n');
fprintf(fid,'Ap = 0.5*Da_a + 0.5*Drho_rho = %f +- %f\n',Ap,Ap_err);
fprintf(fid,'Bp1 = 0.5*[(delta2_(V)+8*(Vs/Vp)^2*gamma2_s)*sin(kappa)^2 \n   + delta2(V)+Da_a-4(b_a)^2*DG_D] = %f +- %f \n',Bp1,Bp1_err);
fprintf(fid,'As1 = 1/(2*(1+Vs/Vp))*[(delta2_2-delta2_1)+2*(Vs/Vp)* gamma2_s)*sin(kappa)^2 \n    + delta2(V)] - 0.5*Dr_r -(b_a)*DG_G = %f +- %f \n',As1,As1_err);
fprintf(fid,'Bp2 = 0.5*(delta2_(V)+8*(Vs/Vp)^2*gamma2_s)*sin(2*kappa) = %f +- %f\n',Bp2,Bp2_err);
fprintf(fid,'As2 = (1/(2*(1+Vs/Vp))*(delta2_(V)+2*(Vs/Vp)*gamma2_s)*sin(2*kappa) = %f +- %f\n',As2,As2_err);
fprintf(fid,'Bp3 = 0.5*(delta1_(V)+8(b_a)^2*gamma1_s) \n    - 0.5*(delta2_(V)+8*(Vs/Vp)^2*gamma2_s)*cos(2*kappa) = %f +- %f\n',Bp3,Bp3_err);
fprintf(fid,'As3 = (1/(2*(1+Vs/Vp))*(delta1(V)+2*(b_a)*gamma1_s)-\n      (1/(2*(1+Vs/Vp))*(delta2_(V)+2*(Vs/Vp)*gamma2_s)*sin(2*kappa) = %f +- %f\n',As3,As3_err);


fprintf(fid,'\nVs/Vp is known only (Vs/Vp=%f): \n',b_a);
fprintf(fid,'-------------------------------------------------------- \n');
fprintf(fid,'delta1_(V)-delta2_(V)*cos(2*kappa) = %f +- %f \n',d1_d2cos(1,1),d1_d2cos(1,2));
fprintf(fid,'delta2_(V)*sin(2*kappa) = %f +- %f \n',d2sin(1,1), d2sin(1,2));
fprintf(fid,'gamma1_s-gamma2_s*cos(2*kappa) = %f +- %f \n',g1_g2cos(1,1),g1_g2cos(1,2));
fprintf(fid,'gamma2_s*sin(2*kappa) = %f +- %f \n',g2sin(1,1), g2sin(1,2));

fprintf(fid,'\ngamma1 is known besides Vs/Vp: \n',b_a);
fprintf(fid,'-------------------------------------------------------- \n');
fprintf(fid,'gamma1 = %f +- %f \n',gamma1,gamma1_err);
fprintf(fid,'gamma2*cos(2kapa) = %f +- %f \n',g2cos1st,g2cos1st_err);
fprintf(fid,'gamma2 = %f +- %f \n',g2,g2_err);
fprintf(fid,'kappa = %f +- %f \n',kappa1st,kappa1st_err);
fprintf(fid,'delta2(V) = %f +- %f \n',d2,d2_err);
fprintf(fid,'delta1(V) = %f +- %f \n',d1,d1_err);

if gamma1~=-100 & delta1~=-100
  fprintf(fid,'\nAnisotropic parameters of the inc. medium are known \n');
  fprintf(fid,'(delta1(V)=%f \t gamma1=%f) \n',delta1_2,gamma1_s);
  fprintf(fid,'-------------------------------------------------------- \n');
  fprintf(fid,'0.5*(delta2(V)+8*(Vs/Vp)^2*gamma2_s)*cos(2*kappa) = %f +- %f\n',d2_g2_cosP(1,1),d2_g2_cosP(1,2));
  fprintf(fid,'(delta2(V)+8*(Vs/Vp)^2*gamma2_s)*sin(2*kappa) = %f +- %f\n',2*Bp2,2*Bp2_err);
  fprintf(fid,'(1/(2*(1+Vs/Vp))*delta2(V)+2*(Vs/Vp)*gamma2_s)*cos(2*kappa) = %f +- %f\n',d2_g2_cosS(1,1),d2_g2_cosS(1,2));
  fprintf(fid,'(1/(2*(1+Vs/Vp))*delta2(V)+2*(Vs/Vp)*gamma2_s)*sin(2*kappa) = %f +- %f\n',As2,As2_err);

  fprintf(fid,'\ndelta2(V)*cos(2*kappa) = %f +- %f\n',d2cos(1,1),d2cos(1,2));
  fprintf(fid,'gamma2_s*cos(2*kappa) = %f +- %f\n',g2cos(1,1),g2cos(1,2));
  fprintf(fid,'\nKnowing the sign of gamma2_s = %f, estimate of kappa follows: \n', SG2);
  fprintf(fid,'(if SG2=-100, the sign is unknown and kappa is non-unique) \n');
  fprintf(fid,'kappa = %f +- %f \n',kappa(1,1),kappa(1,2));
  fprintf(fid,'gamma2_better= %f +- %f \n',g2b,g2b_err);
  fprintf(fid,'delta2(V)_better= %f +- %f \n',d2b,d2b_err);

end;
if gamma1~=-100 & delta1~=-100
  fprintf(fid,'\nAbsolute terms inversion:\n');
  fprintf(fid,'-----------------------------\n');
  fprintf(fid,'0.5*(delta2_(V)+8*(Vs/Vp)^2*gamma2_s)*sin(kappa)^2 = %f +- %f \n',d2_g2_sinP2(1,1),d2_g2_sinP2(1,2));
  fprintf(fid,'1/(2*(1+Vs/Vp))*(delta2_2-delta2_1)+2*(Vs/Vp)*gamma2_s)*sin(kappa)^2 = %f +- %f \n',d2_g2_sinS2(1,1),d2_g2_sinS2(1,2));
  fprintf(fid,'delta2_2-delta1_2+DVp/Vp-(2*Vs/Vp)^2*DG/DG = %f +- %f \n',P_abs(1,1),P_abs(1,2));
  fprintf(fid,'1/(2*(1+Vs/Vp))*(delta2_2-delta1_2)-0.5*Drho/rho-(Vs/Vp)*DG/DG = %f +- %f \n',S_abs(1,1),S_abs(1,2));
  fprintf(fid,'\n!!! if the following quantities are equal to -100, no enough information was available !!! \n');
  fprintf(fid,'\nDVp/Vp = %f +- %f \n',result_abs(1,1),result_abs_err(1,1));
  fprintf(fid,'DVs/Vs = %f +- %f \n',result_abs(1,2),result_abs_err(1,2));
  fprintf(fid,'Drho/rho = %f +- %f \n',result_abs(1,3),result_abs_err(1,3));
  fprintf(fid,'DG/G = %f +- %f \n',result_abs(1,4),result_abs_err(1,4));
  fprintf(fid,'DZ/Z = %f +- %f \n',result_abs(1,5),result_abs_err(1,5));
  fprintf(fid,'delta2(V)-delta1(V) = %f +- %f \n',result_abs(1,6),result_abs_err(1,6));
  fprintf(fid,'delta2(V)_better2 = %f +- %f \n',d2bb,d2bb_err);
  fprintf(fid,'\ndelta2(V)_best = %f +- %f \n',d2best,d2best_err);
end;

fclose(fid);
