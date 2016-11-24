%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inversion of Rpp and Rps from data file - PHASE2
% (this is the most general version of PHASE2 inversion,
% supportint any combinations of misaligned halfspaces
% up to orthorhombic - you will get better results using
% a more specific version of this code if your model has
% higher symmetry):
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

b_a=0.55715;        % vs/vp ratio
%b_a=0.6129;
inc_known=0;
kappa_known=0;
delta1_1=-0.2; inc_known=1;
delta1_2=0.1;
gamma1_1=0.1;
gamma1_2=0.0;
SG2=-1;             % sign of gamma2_s: necessary for unique estimate of
                    % kappa; if not provided, kappa is not estimated uniquelly!
		    % if you do not know SG2, put SG2=100;
%kappa=20*pi/180; kappa_known=1;  % provided kappa must be from <-90,90>

in_file='Result_phase1.out';  % the name of input file with Ap,Bp and As
                        % parameters
%in_file='Result_exact_phase1.out';

control=1;
if SG2==1
  SG2=1;
elseif SG2==-1
  SG2=-1;
else
  SG2=100;
end;
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
Bp2_err=BBp2(2,1);
Bp3=BBp3(1,1);
Bp3_err=BBp3(2,1)
As1=AAs1(1,1);
As1_err=AAs1(2,1);
As2=AAs2(1,1);
As2_err=AAs2(2,1);
As3=AAs3(1,1);
As3_err=AAs3(2,1);
%%%%%%%%%%%%
% INVERSION:
%%%%%%%%%%%%
% A) b_a ratio is known only:
%%%%%%%%%%%%

A1=[0.5  4*(b_a)^2; 1/(2*(1+b_a)) 2*b_a];
[u1,s1,v1]=svd(A1);
ss1=diag(1./diag(s1));
%ss(2,2)=[0];
PI1=v1*ss1*u1';

COV1temp_cos=[(Bp3_err)^2 0;
	     0 (As3_err)^2];
COVARIANCE1_cos=PI1*COV1temp_cos*PI1';
RESOL1=PI1*A1;

result1_cos=PI1*[Bp3;As3];      
error1_cos=sqrt(diag(COVARIANCE1_cos));
d1_d2cos=[result1_cos(1,1) error1_cos(1,1)];              % this is (d1_2-d1_1)-(d2_2-d2_1)cos(2*kappa)
g1_g2cos=[result1_cos(2,1) error1_cos(2,1)];	         % this is g1_s-g2_s*cos(2*kappa)					
COV1temp_sin=[(Bp2_err)^2 0;
	     0 (As2_err)^2];
COVARIANCE1_sin=PI1*COV1temp_sin*PI1';

result1_sin=PI1*[Bp2;As2];      
error1_sin=sqrt(diag(COVARIANCE1_sin));
d2sin=[result1_sin(1,1) error1_sin(1,1)];                 % this is (d2_2-d2_1)sin(2*kappa)
g2sin=[result1_sin(2,1) error1_sin(2,1)];                % this is
                                                         % g2_s*sin(2*kapp)
							 
%%%%%%%%%%%%%%
% B-1) rotation angle kappa is known:
%%%%%%%%%%%%%%
if kappa_known==1
  if abs(kappa)<15*pi/180 
    control=-1;
    if abs(kappa)<0.1*pi/180
      control=-2;
    end;
  end;
  if abs(kappa)>75*pi/180 
    control=-1;
    if abs(kappa)>89.9*pi/180
      control=-2;
    end;
  end;
  if control==-1
    fprintf('!!! kappa angle is close to 0deg or 90deg => resuls inverted \n');
    fprintf('!!! from azimuthal variations are strongly ill-posed !!! \n');
  end;
  if control ~=-2
    d2_g2P=[2*Bp2/sin(2*kappa)  2*1/sin(2*kappa)*Bp2_err]; % this is (d22-d21+8*(b_a)^2*g2s)
    d2_g2S=[As2/sin(2*kappa)  1/sin(2*kappa)*As2_err]; % this is 1/(2*(1+b_a))*(d22-d21)+2*(b_a)*g2s
    d1_g1P=[2*Bp3+d2_g2P(1,1)*cos(2*kappa) sqrt(4*Bp3_err^2+(cos(2*kappa)* ...
						  d2_g2P(1,2))^2)];
                                                     % this is
                                                     % (d12-d11)+8*(b_a)^2*g1s
    d1_g1S=[(As3+d2_g2S(1,1)*cos(2*kappa)) sqrt(As3_err^2+(cos(2*kappa)* ...
						  d2_g2S(1,2))^2)];
                                                     % this is
                                                     % 1/(2*(1+b_a))*(d12-d11)+2*(b_a)*g1s
  end;

  % finally, separate deltas and gammas:
  if control~=-2
    A2=[1        8*(b_a)^2; 
	1/(2*(1+b_a))  2*b_a];
    PI2=pinv(A2);
    
    DATA1=[d1_g1P(1,1);   d1_g1S(1,1)];
    COVtemp=[d1_g1P(1,2)^2  0;
	     0 d1_g1S(1,2)^2 ];
    result1=PI2*DATA1;
    COV1=PI2*COVtemp*PI2';
    
    DATA2=[d2_g2P(1,1);  d2_g2S(1,1)];
    COVtemp=[d2_g2P(1,2)^2  0;
	     0 d2_g2S(1,2)^2 ];
    result2=PI2*DATA2;
    COV2=PI2*COVtemp*PI2';
    
    d12_d11=[result1(1,1) sqrt(COV1(1,1))];  % this is (d12-d11)
    gs1=[result1(2,1) sqrt(COV1(2,2))];      % this is gamma1s
    d22_d21=[result2(1,1) sqrt(COV2(1,1))];  % this is (d22-d21) 
    gs2=[result2(2,1) sqrt(COV2(2,2))];      % this is gamma2s
  end;
  if control==-2
    A2=[0.5 4*(b_a)^2;
	1/(2*(1+b_a))  2*b_a];
    PI2=pinv(A2);
    
    DATA1=[Bp3; As3];
    COVtemp=[Bp3_err^2 0;
	     0 As3_err^2];
    result1=PI2*DATA1;
    COV1=PI2*COVtemp*PI2';
    d1_d2=[result1(1,1) sqrt(COV1(1,1))];  % this is d1_2-d1_1-d2_2+d2_1
    g1_g2=[result1(2,1) sqrt(COV1(2,2))];  % this is gamma1_s-gamma2_s
  end;
end;

%%%%%%%%%%%%%%
% B-2) incident halfspace parameters are known (i.e., d1_1, d1_2, g1_s):
%%%%%%%%%%%%%%
if inc_known==1
  gamma1_s=(gamma1_1-gamma1_2)/(1+2*gamma1_2);
  d2cos=[-d1_d2cos(1,1)+(delta1_2-delta1_1) error1_cos(1,1)]   % this is (d2_2-d2_1)cos(2*kappa)
  g2cos=[-g1_g2cos(1,1)+gamma1_s error1_cos(2,1)]              % this is g2_s*cos(2*kappa)
  
  
  % estimate the angle kappa: 
  % you should know the sign of gamma to do that uniquely

  d2_g2_cosP=[-Bp3+0.5*(delta1_2-delta1_1+8*(b_a)^2*gamma1_s)  Bp3_err];  
                                                 % this is
                                                 % 0.5*[(d2_2-d2_1+8*(b_a)^2*g2_s)*cos(2*kappa)

  d2_g2_cosS=[-As3+1/(2*(1+b_a))*(delta1_2-delta1_1)+2*b_a*gamma1_s As3_err];
                                                 % this is
                                                 % 1/(2*(1+b_a))*(d2_2-d2_1+2*b_a*g2_s)*cos(2*kappa)
						 
  kappa1_err(1)=0.5*atan(Bp2/d2_g2_cosP(1,1))*180/pi;
  kappa1_err(2)=0.5*atan((Bp2+Bp2_err)/(d2_g2_cosP(1,1)+d2_g2_cosP(1,2)))*180/pi;
  kappa1_err(3)=0.5*atan((Bp2+Bp2_err)/(d2_g2_cosP(1,1)-d2_g2_cosP(1,2)))*180/pi;
  kappa1_err(4)=0.5*atan((Bp2-Bp2_err)/(d2_g2_cosP(1,1)+d2_g2_cosP(1,2)))*180/pi;
  kappa1_err(5)=0.5*atan((Bp2-Bp2_err)/(d2_g2_cosP(1,1)-d2_g2_cosP(1,2)))*180/pi;
  %temp=sort(abs(kappa1_err-kappa1));
  %kappa1_error=0.5*(temp(1,3)+temp(1,4)); % less conservatine error estimate
  kappa1=mean(kappa1_err);
  kappa1_error=sqrt(var(kappa1_err));
  %kappa1_error=(temp(1,4));
  kappa2_err(1)=0.5*atan(As2/d2_g2_cosS(1,1))*180/pi;
  kappa2_err(2)=0.5*atan((As2+As2_err)/(d2_g2_cosS(1,1)+d2_g2_cosS(1,2)))*180/pi;
  kappa2_err(3)=0.5*atan((As2+As2_err)/(d2_g2_cosS(1,1)-d2_g2_cosS(1,2)))*180/pi;
  kappa2_err(4)=0.5*atan((As2-As2_err)/(d2_g2_cosS(1,1)+d2_g2_cosS(1,2)))*180/pi;
  kappa2_err(5)=0.5*atan((As2-As2_err)/(d2_g2_cosS(1,1)-d2_g2_cosS(1,2)))*180/pi;
  %temp=sort(abs(kappa2_err-kappa2));
  %kappa2_error=0.5*(temp(1,3)+temp(1,4)); % less conservative error estimate
  %kappa2_error=(temp(1,4));
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
  
  
%  if SG2==100
%    pick=input('Sign of gamma2_s is unknown, pick one of two possible kappas: \n1 ... kappa=%f deg \n2 ... kappa=%f deg\n',0.5*(kappa1+kappa2), 0.5*(kappa1+kappa2)+90);
%    if pick==2
%      kappa1=kappa1+90;
%      kappa2=kappa2+90;
%    end;
%  else
%    if (sign(tan(2*kappa1*pi/180))*SG2) ~= sign(g2cos(1,1))
%      kappa1=kappa1+90.;
%    end;
%    if (sign(tan(2*kappa2*pi/180))*SG2) ~= sign(g2cos(1,1))
%      kappa2=kappa2+90.;
%    end;
%    kappa=[0.5*(kappa1+kappa2)  0.5*sqrt(kappa1_error^2+kappa2_error^2)]
%  end;

   % get the g2 and d2 separately %
   g2c_err(1)=g2cos(1,1)/cos(2*kappa(1,1)*pi/180);
   g2c_err(2)=(g2cos(1,1)+g2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
   g2c_err(3)=(g2cos(1,1)+g2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
   g2c_err(4)=(g2cos(1,1)-g2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
   g2c_err(5)=(g2cos(1,1)-g2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
   g2c=mean(g2c_err);
   g2c_error=sqrt(var(g2c_err));
   
   g2s_err(1)=g2sin(1,1)/sin(kappa(1,1)*pi/180);
   g2s_err(2)=(g2sin(1,1)+g2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
   g2s_err(3)=(g2sin(1,1)+g2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
   g2s_err(4)=(g2sin(1,1)-g2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
   g2s_err(5)=(g2sin(1,1)-g2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
   g2s=mean(g2s_err);
   g2s_error=sqrt(var(g2s_err));
   
   if min([g2c_error g2s_error])==g2c_error
    g2=[g2c  g2c_error];
  else
    g2=[g2s  g2s_error];
  end;
   
   d2c_err(1)=d2cos(1,1)/cos(2*kappa(1,1)*pi/180);
   d2c_err(2)=(d2cos(1,1)+d2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
   d2c_err(3)=(d2cos(1,1)+d2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
   d2c_err(4)=(d2cos(1,1)-d2cos(1,2))/cos(2*(kappa(1,1)+kappa(1,2))*pi/180);
   d2c_err(5)=(d2cos(1,1)-d2cos(1,2))/cos(2*(kappa(1,1)-kappa(1,2))*pi/180);
   d2c=mean(d2c_err);
   d2c_error=sqrt(var(d2c_err));
   
   d2s_err(1)=d2sin(1,1)/sin(kappa(1,1)*pi/180);
   d2s_err(2)=(d2sin(1,1)+d2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
   d2s_err(3)=(d2sin(1,1)+d2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
   d2s_err(4)=(d2sin(1,1)-d2sin(1,2))/sin(2*(kappa(1,1)+kappa(1,2))*pi/180);
   d2s_err(5)=(d2sin(1,1)-d2sin(1,2))/sin(2*(kappa(1,1)-kappa(1,2))*pi/180);
   d2s=mean(d2s_err);
   d2s_error=sqrt(var(d2s_err));
   
   if min([d2c_error d2s_error])==d2c_error
    d2=[d2c  d2c_error];
  else
    d2=[d2s  d2s_error];
  end;
   
    

end;

%%%%%%%%%%%%%%%%%%%%%%%%
% C) absolute terms analysis
%%%%%%%%%%%%%%%%%%%%%%%%
if kappa_known==1
  if control~=-2
    d2_g2_sinP2=[0.5*d2_g2P(1,1)*sin(kappa)^2  0.5*sin(kappa)^2*d2_g2P(1,2)];
                                                 % this is
                                                 % 0.5*[(d2_2-d2_1+8*(b_a)^2*g2_s)*sin(kappa)^2]
    d2_g2_sinS2=[d2_g2S(1,1)*sin(kappa)^2  sin(kappa)^2*d2_g2S(1,2)];
                                                 % this is
						 % (1/(2*(1+b_a))*(d2_2-d2_1)+2*b_a*g2_s)*sin(kappa)^2
  else
    d2_g2_sinP2=[0 0];
    d2_g2_sinS2=[0 0];
  end;
end;

if inc_known==1
  sga=sign(d2_g2_cosP(1,1))*sign(cos(2*kappa(1,1)*pi/180));
  d2_g2_sinP2(1,1)=0.5*(sga*sqrt(Bp2^2+d2_g2_cosP(1,1)^2) - d2_g2_cosP(1,1)) 
                                                 % this is
                                                 % 0.5*[(d2_2-d2_1+8*(b_a)^2*g2_s)*sin(kappa)^2]
  d2_g2_sinP2_err(1)=0.5*(sga*sqrt((Bp2+Bp2_err)^2+(d2_g2_cosP(1,1)+d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)+d2_g2_cosP(1,2)));
  d2_g2_sinP2_err(2)=0.5*(sga*sqrt((Bp2+Bp2_err)^2+(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))); 
  d2_g2_sinP2_err(3)=0.5*(sga*sqrt((Bp2-Bp2_err)^2+(d2_g2_cosP(1,1)+d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)+d2_g2_cosP(1,2))); 
  d2_g2_sinP2_err(4)=0.5*(sga*sqrt((Bp2-Bp2_err)^2+(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))^2)-(d2_g2_cosP(1,1)-d2_g2_cosP(1,2))); 
  d2_g2_sinP2(1,2)=max(abs(d2_g2_sinP2(1,1)-d2_g2_sinP2_err));

  sga=sign(d2_g2_cosS(1,1))*sign(cos(2*kappa(1,1)*pi/180));
  d2_g2_sinS2(1,1)=0.5*(sga*sqrt(As2(1,1)^2+d2_g2_cosS(1,1)^2) - d2_g2_cosS(1,1)) 
                                                 % this is
                                                 % (1/(2*(1+b_a))*(d2_2-d2_1)+2*b_a*g2_s)*sin(kappa)^2
  d2_g2_sinS2_err(1)=0.5*(sga*sqrt((As2+As2_err)^2+(d2_g2_cosS(1,1)+d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)+d2_g2_cosS(1,2)));
  d2_g2_sinS2_err(2)=0.5*(sga*sqrt((As2+As2_err)^2+(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))); 
  d2_g2_sinS2_err(3)=0.5*(sga*sqrt((As2-As2_err)^2+(d2_g2_cosS(1,1)+d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)+d2_g2_cosS(1,2))); 
  d2_g2_sinS2_err(4)=0.5*(sga*sqrt((As2-As2_err)^2+(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))^2)-(d2_g2_cosS(1,1)-d2_g2_cosS(1,2))); 
  d2_g2_sinS2(1,2)=max(abs(d2_g2_sinS2(1,1)-d2_g2_sinS2_err));
end;

% P-wave absolute term: (d22-d12+Da_a-(2*b_a)^2*DG_G):
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

%%%%%%%%%%%%%%%%%%
% E) write the results into a table file
%%%%%%%%%%%%%%%%%%
fid=fopen('Result_phase2.out','w');
fprintf(fid,'PHASE II INVERSION RESULTS (Invert_R_I_phase2 output): \n');
fprintf(fid,'\nVs/Vp is known only (Vs/Vp=%f): \n',b_a);
fprintf(fid,'-------------------------------------------------------- \n');
fprintf(fid,'delta1_2-delta1_1-(delta2_2-delta2_1)*cos(2*kappa) = %f +- %f \n',d1_d2cos(1,1),d1_d2cos(1,2));
fprintf(fid,'(delta2_2-delta2_1)*sin(2*kappa) = %f +- %f \n',d2sin(1,1), d2sin(1,2));
fprintf(fid,'gamma1_s-gamma2_s*cos(2*kappa) = %f +- %f \n',g1_g2cos(1,1),g1_g2cos(1,2));
fprintf(fid,'gamma2_s*sin(2*kappa) = %f +- %f \n',g2sin(1,1), g2sin(1,2));
if inc_known==1
  fprintf(fid,'\nAnisotropic parameters of the inc. medium are known \n');
  fprintf(fid,'(delta1_1=%f \t delta1_2 = %f \t gamma1_s=%f) \n',delta1_1,delta1_2,gamma1_s);
  fprintf(fid,'-------------------------------------------------------- \n');
  fprintf(fid,'(delta2_2-delta2_1+8*(Vs/Vp)^2*gamma2_s)*cos(2*kappa) = %f +- %f\n',d2_g2_cosP(1,1),d2_g2_cosP(1,2));
  fprintf(fid,'(delta2_2-delta2_1+8*(Vs/Vp)^2*gamma2_s)*sin(2*kappa) = %f +- %f\n',2*Bp2,2*Bp2_err);
  fprintf(fid,'(1/(2*(1+Vs/Vp))*(delta2_2-delta2_1)+2*(Vs/Vp)*gamma2_s)*cos(2*kappa) = %f +- %f\n',d2_g2_cosS(1,1),d2_g2_cosS(1,2));
  fprintf(fid,'(1/(2*(1+Vs/Vp))*(delta2_2-delta2_1)+2*(Vs/Vp)*gamma2_s)*sin(2*kappa) = %f +- %f\n',As2,As2_err);

  fprintf(fid,'\n(delta2_2-delta2_1)*cos(2*kappa) = %f +- %f\n',d2cos(1,1),d2cos(1,2));
  fprintf(fid,'gamma2_s*cos(2*kappa) = %f +- %f\n',g2cos(1,1),g2cos(1,2));
  fprintf(fid,'\nKnowing the sign of gamma2_s = %f, estimate of kappa follows: \n', SG2);
  fprintf(fid,'(if SG2=100, the sign is unknown and kappa is non-unique) \n');
  fprintf(fid,'kappa = %f +- %f \n',kappa(1,1),kappa(1,2));
  fprintf(fid,'\ndelta2_2-delta2_1 = %f +- %f \n',d2(1,1),d2(1,2));
  fprintf(fid,'gamma2_s = %f +- %f \n',g2(1,1),g2(1,2));
 
end;
if kappa_known==1
  fprintf(fid,'\n \nAngle kappa = %f is known as well:\n',kappa*180/pi);
  fprintf(fid,'(this option results in very unstable estimates of the following \nazimuth-controlling parameters) \n');
  fprintf(fid,'---------------------------------------------------------------------------------\n');
  if control~=-2 
    fprintf(fid,'delta2_2-delta2_1+8*(Vs/Vp)^2*gamma2_s = %f +- %f \n', d2_g2P(1,1),d2_g2P(1,2));
    fprintf(fid,'delta1_2-delta1_1+8*(Vs/Vp)^2*gamma1_s = %f +- %f \n', d1_g1P(1,1),d1_g1P(1,2));
    fprintf(fid,'1/(2*(1+Vs/Vp))*(delta2_2-delta2_1)+2*(Vs/Vp)*gamma2_s = %f +-  %f \n', d2_g2S(1,1),d2_g2S(1,2));
    fprintf(fid,'1/(2*(1+Vs/Vp))*(delta1_2-delta1_1)+2*(Vs/Vp)*gamma1_s = %f +- %f \n', d1_g1S(1,1),d1_g1S(1,2));
    fprintf(fid,'delta1_2-delta1_1 = %f +- %f \n',d12_d11(1,1),d12_d11(1,2));
    fprintf(fid,'delta2_2-delta2_1 = %f +- %f \n',d22_d21(1,1),d22_d21(1,2));
    fprintf(fid,'gamma1_s = %f +- %f \n',gs1(1,1),gs1(1,2));
    fprintf(fid,'gamma2_s = %f +- %f \n',gs2(1,1),gs2(1,2));
  end;
  if control==-2
    fprintf(fid,'delta1_2-delta2_2 -(delta1_1-delta2_1) = %f +- %f \n',d1_d2(1,1),d1_d2(1,2));
    fprintf(fid,'gamma1_s-gamma2_s = %f +- %f \n',g1_g2(1,1),g1_g2(1,2));
  end;
end;
fprintf(fid,'\nAbsolute terms inversion:\n');
fprintf(fid,'-----------------------------\n');
fprintf(fid,'0.5*(delta2_2-delta2_1+8*(Vs/Vp)^2*gamma2_s)*sin(kappa)^2 = %f +- %f \n',d2_g2_sinP2(1,1),d2_g2_sinP2(1,2));
fprintf(fid,'1/(2*(1+Vs/Vp))*(delta2_2-delta2_1)+2*(Vs/Vp)*gamma2_s)*sin(kappa)^2 = %f +- %f \n',d2_g2_sinS2(1,1),d2_g2_sinS2(1,2));
fprintf(fid,'delta2_2-delta1_2+DVp/Vp-(2*Vs/Vp)^2*DG/DG = %f +- %f \n',P_abs(1,1),P_abs(1,2));
fprintf(fid,'1/(2*(1+Vs/Vp))*(delta2_2-delta1_2)-0.5*Drho/rho-(Vs/Vp)*DG/DG = %f +- %f \n',S_abs(1,1),S_abs(1,2));
fprintf(fid,'\n!!! if the following quantities are equal to -100, no enough information was available !!! \n');
fprintf(fid,'\nDVp/Vp = %f +- %f \n',result_abs(1,1),result_abs_err(1,1));
fprintf(fid,'DVs/Vs = %f +- %f \n',result_abs(1,2),result_abs_err(1,2));
fprintf(fid,'Drho/rho = %f +- %f \n',result_abs(1,3),result_abs_err(1,3));
fprintf(fid,'DG/G = %f +- %f \n',result_abs(1,4),result_abs_err(1,4));
fprintf(fid,'DZ/Z = %f +- %f \n',result_abs(1,5),result_abs_err(1,5));
fprintf(fid,'delta2_2-delta1_2 = %f +- %f \n',result_abs(1,6),result_abs_err(1,6));

fclose(fid);



