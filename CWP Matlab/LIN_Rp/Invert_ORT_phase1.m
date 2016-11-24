%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inversion of Rpp and Rps from data file - PHASE1 
% (this version assumes a generally oriented ORT or
% HTI halfspaces with misaligned symmetry planes above
% and bellow the interface):
%
% parameters of the surfaces Rpp(inc_angle,azim)  
% and Rpsv(inc_angle,azim) are recovered, together with 
% the corresponding error estimates.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;

% some useful constants:

n_inv_p=9;      % number of inverting variables in PP (9)
n_inv_s=18;     % number of inverting variables in PS (18)
std_p=0.2;        % standard deviation for PP (in %)
std_s=0.2;        % standard deviation for PS (in %)

                 % for plotting
xmin=0;
xmax=30;        
ymin=0;
ymax=90;

% get the data first:

load Rpp.out   % Rpp data
load Rsvsh.out % Rps data

%load Rpp1.out   % Rpp data
%load Rsvsh1.out % Rps data
%Rpp=Rpp1;
%Rsvsh=Rsvsh1;

Pdata=Rpp(:,3);
Sdata=Rsvsh(:,3);
P_inc_deg=Rpp(:,1);
P_az_deg=Rpp(:,2);
S_inc_deg=Rpp(:,1);
S_az_deg=Rpp(:,2);

P_inc=P_inc_deg*pi/180;
P_az=P_az_deg*pi/180;
S_inc=S_inc_deg*pi/180;
S_az=S_az_deg*pi/180;


% INVERSION: PHASE 1:
% definition of the inversion matrices:

SZP=size(P_inc);
SZS=size(S_inc);
AP=eye(SZP(1,1),n_inv_p);       % Rpp matrix
AS=eye(SZS(1,1),n_inv_s);       % Rpsv matrix

AP(:,1)=[1];
AP(:,2)=[sin(P_inc).^2];
AP(:,3)=[sin(P_az).*cos(P_az).*sin(P_inc).^2];
AP(:,4)=[sin(P_az).*sin(P_az).*sin(P_inc).^2];
AP(:,5)=[(sin(P_inc).^2).*(tan(P_inc).^2)];
AP(:,6)=[sin(2*P_az).*cos(2*P_az).*(sin(P_inc).^2).*(tan(P_inc).^2)];
AP(:,7)=[sin(P_az).*cos(P_az).*(sin(P_inc).^2).*(tan(P_inc).^2)];
AP(:,8)=[(sin(P_az).^2).*(cos(P_az).^2).*(sin(P_inc).^2).*(tan(P_inc).^2)];
AP(:,9)=[sin(P_az).*sin(P_az).*(sin(P_inc).^2).*(tan(P_inc).^2)];

AS(:,1)=[sin(S_inc)];
AS(:,2)=[sin(S_az).*cos(S_az).*sin(S_inc)];
AS(:,3)=[sin(S_az).*sin(S_az).*sin(S_inc)];
AS(:,4)=[sin(S_inc).^3];
AS(:,5)=[sin(2*S_az).*cos(2*S_az).*(sin(S_inc).^3)];
AS(:,6)=[sin(S_az).*cos(S_az).*(sin(S_inc).^3)];
AS(:,7)=[(sin(S_az).^2).*(cos(S_az).^2).*(sin(S_inc).^3)];
AS(:,8)=[(sin(S_az).^2).*(sin(S_inc).^3)];
AS(:,9)=[sin(S_inc).^5];
AS(:,10)=[sin(2*S_az).*cos(2*S_az).*(sin(S_inc).^5)];
AS(:,11)=[sin(S_az).*cos(S_az).*(sin(S_inc).^5)];
AS(:,12)=[(sin(S_az).^2).*(cos(S_az).^2).*(sin(S_inc).^5)];
AS(:,13)=[(sin(S_az).^2).*(sin(S_inc).^5)];
AS(:,14)=[sin(S_inc).^7];
AS(:,15)=[sin(2*S_az).*cos(2*S_az).*(sin(S_inc).^7)];
AS(:,16)=[sin(S_az).*cos(S_az).*(sin(S_inc).^7)];
AS(:,17)=[(sin(S_az).^2).*(cos(S_az).^2).*(sin(S_inc).^7)];
AS(:,18)=[(sin(S_az).^2).*(sin(S_inc).^7)];


% here I join the matrices:  switched off here
%
%A=AP;
%for i=1:SZS(1,1)
%  A(SZP(1,1)+i,:)=AS(i,:);
%end;
%

% solving the system: pseudo-inverse, resolution and pre-accuracy:

%%% Rpp %%%
[UP,SP,VP]=svd(AP);
PIP=pinv(AP);        % pseudo-inverse
RESP=PIP*AP;         % resolution
COVP=PIP*PIP';       % pre-covariance
errP=diag(COVP)';
resolutionP=diag(RESP)';
PIPexact=PIP;

%%% Rps %%%
[US,SS,VS]=svd(AS);
PIS=pinv(AS);        % pseudo-inverse
RESS=PIS*AS;         % resolution
COVS=PIS*PIS';       % pre-covariance
errS=diag(COVS)';
resolutionS=diag(RESS)';
PISexact=PIS;


% biasing the system -> dumping small eigenvalues: 

SSP=diag(1./diag(SP));
[row,cul]=size(SP);
SSP(:,n_inv_p+1:1:row)=[0];      
SP_b=SSP;                   % pseudo-inverse is now: V*S_b*U'

SSS=diag(1./diag(SS));
[row,cul]=size(SS);
SSS(:,n_inv_s+1:1:row)=[0];      
SS_b=SSS;                   % pseudo-inverse is now: V*S_b*U'


% choose the dumping here:
%%% Rpp %%%
% SP_b(9,9)=[0];           % a complete dump
% SP_b(8,8)=[30];           % a partial dump
% SP_b(7,7)=[25];
% SP_b(6,6)=[0];
% SP_b(5,5)=[0];
% SP_b(4,4)=[0];
% SP_b(3,3)=[0];
% SP_b(2,2)=[0];
% SP_b(1,1)=[0];

%%% Rps %%%
% SS_b(18,18)=[0];           % a complete dump
% SS_b(17,17)=[0];           % a partial dump
% SS_b(16,16)=[0];
% SS_b(15,15)=[0];
% SS_b(14,14)=[0];
% SS_b(13,13)=[0];
% SS_b(12,12)=[0];
% SS_b(11,11)=[80];
% SS_b(10,10)=[0];
% SS_b(9,9)=[0];
% SS_b(8,8)=[0];
% SS_b(7,7)=[0];
% SS_b(6,6)=[0];
% SS_b(5,5)=[0];
% SS_b(4,4)=[0];
% SS_b(3,3)=[0];
% SS_b(2,2)=[0];
% SS_b(1,1)=[0];


PIP_b=VP*SP_b*UP';
COVP_b=PIP_b*PIP_b';
RESP_b=PIP_b*AP;
errP_b=diag(COVP_b)';
resolutionP_b=diag(RESP_b)';

PIS_b=VS*SS_b*US';
COVS_b=PIS_b*PIS_b';
RESS_b=PIS_b*AS;
errS_b=diag(COVS_b)';
resolutionS_b=diag(RESS_b)';

%data games:

IN_DATA_P=[Pdata];
IN_DATA_S=[Sdata];
n_data_P=size(IN_DATA_P);
n_data_S=size(IN_DATA_S);
IP(1:n_data_P(1,1),1)=[1];
IS(1:n_data_S(1,1),1)=[1];

%error contamination of the data (std%)
TEMP=rand(n_data_P(1,1),1);
DATAP=IN_DATA_P.*(IP+std_p*(0.5-TEMP));
TEMP=rand(n_data_S(1,1),1);
DATAS=IN_DATA_S.*(IS+std_s*(0.5-TEMP));
%  load dataP_20.out; load dataS_20.out;  % unmark in case of using the previous erroneous data
%     DATAP=dataP_20';
%     DATAS=dataS_20';                          % if you read in data from a file
   %foutP = fopen('dataP.out', 'w');
   %fprintf(foutP, ['%7.4f'], DATAP);
   %foutS = fopen('dataS.out', 'w');
   %fprintf(foutS, ['%7.4f '], DATAS);
Pdata=DATAP;
Sdata=DATAS;

% invert the data
% unmark this if you want the result with dumping:
 PIP=PIP_b;
 RESP=RESP_b;
 PIS=PIS_b;
 RESS=RESS_b;
 
resultP=PIP*DATAP;
resultS=PIS*DATAS;
result_exactP=PIPexact*IN_DATA_P;
result_exactS=PISexact*IN_DATA_S;
[resultP'; result_exactP'];
[resultS'; result_exactS'];

% error evaluation:

DATA2P=AP*resultP;                                        % reproduction of the original data
P_RESID=DATAP-DATA2P;                                     % get the Rpp residua
DATA2S=AS*resultS;                                        % reproduction of the original data
S_RESID=DATAS-DATA2S;                                     % get the Rps residua

P_VAR=sum(P_RESID.^2)/(SZP(1,1)-1);                    % Rpp variance
S_VAR=sum(S_RESID.^2)/(SZS(1,1)-1);                    % Rpsv variance

COVtempP(1:SZP(1,1),1:SZP(1,1))=[P_VAR];                   % setting up the covariance matrix
COVtempS(1:SZS(1,1),1:SZS(1,1))=[S_VAR];
COVtempP=diag(diag(COVtempP));
COVtempS=diag(diag(COVtempS));
COVARIANCE1_P=PIP*COVtempP*PIP';
COVARIANCE1_S=PIS*COVtempS*PIS';
diag(COVARIANCE1_P);
RESOL1_P=RESP;
diag(COVARIANCE1_S);
RESOL1_S=RESS;

%%%%%%%%% look here for important semi-results %%%%%%%%%%%%%

[resultP(1:4,:)'; result_exactP(1:4,:)'];
[resultP'; result_exactP'];
[resultS(1:3,:)'; result_exactS(1:3,:)']
[resultS'; result_exactS'];
RP=diag(RESOL1_P)';
RS=diag(RESOL1_S)';
[COVARIANCE1_P(4,4) 0; 0 COVARIANCE1_S(3,3)];
CP=diag(COVARIANCE1_P)';
CS=diag(COVARIANCE1_S)';
chyba_p=sqrt(CP);
chyba_s=sqrt(CS);
chyba_p(1,2);
chyba_s(1,3)
RESOL1_P(3,3);
resoluce_u_P=RP(1:9);
resoluce_u_P33=RESOL1_P(2,:);
resoluce_u_S=RS(1:3);
resoluce_u_S44=RESOL1_S(3,:)

% bias estimate: N is the order number of parameter being estimated:
N=3;
%res_bias=[ RESOL1_P(N,1:(N-1)) RESOL1_P(N,N+1:n_inv_p)];
res_bias=[ RESOL1_S(N,1:(N-1)) RESOL1_S(N,N+1:n_inv_p)];

res_bias=sqrt(res_bias.^2);
BIAS=0.6*sum(res_bias)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% plot the results - residua %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pos0=[270 380 500 400];
pos1=[300 360 500 400];
pos2=[330. 340. 500 400];
pos3=[360 320 500 400];

figure('Position',pos0);
plot(P_inc_deg,P_az_deg,'.','MarkerSize',15);
hold;
plot(S_inc_deg,S_az_deg,'ro','MarkerSize',8);
title('Data coverage', 'FontSize', 20,'FontWeight','bold');
grid;
xlabel('Incidence angle (deg)', 'FontSize', 15,'FontWeight','bold');
ylabel('Azimuth (deg)', 'FontSize', 15, 'FontWeight','bold');


figure('Position',pos1);
plot3([P_inc_deg],[P_az_deg],P_RESID,'.','MarkerSize',15)
hold;
plot3([S_inc_deg],[S_az_deg],S_RESID,'.','MarkerSize',15)
title('Residua', 'FontSize', 20,'FontWeight','bold');
grid;
xlabel('Incidence angle (deg)', 'FontSize', 15,'FontWeight','bold');
ylabel('Azimuth (deg)', 'FontSize', 15, 'FontWeight','bold');
zlabel('Residua (deg)', 'FontSize', 15, 'FontWeight','bold');


dx=xmin:1:xmax;
dy=ymin:5:ymax;
[XI,YI]=meshgrid(dx,dy);           %this sets up the regular grid%

figure('Position',pos2);
Pplot=griddata(P_inc_deg,P_az_deg,DATA2P,XI,YI);
mesh(XI,YI,Pplot);
hold;
plot3(P_inc_deg,P_az_deg,Pdata,'.','MarkerSize',15);
title('Rpp  fit', 'FontSize', 20,'FontWeight','bold');
xlabel('Incidence angle (deg)', 'FontSize', 15,'FontWeight','bold');
ylabel('Azimuth (deg)', 'FontSize', 15, 'FontWeight','bold');
zlabel('Coefficient (deg)', 'FontSize', 15, 'FontWeight','bold');


figure('Position',pos3);
Splot=griddata(S_inc_deg,S_az_deg,DATA2S,XI,YI);
mesh(XI,YI,Splot);
hold;
plot3(S_inc_deg,S_az_deg,Sdata,'.','MarkerSize',15);
title('Rpsv  fit', 'FontSize', 20,'FontWeight','bold');
xlabel('Incidence angle (deg)', 'FontSize', 15,'FontWeight','bold');
ylabel('Azimuth (deg)', 'FontSize', 15, 'FontWeight','bold');
zlabel('Coefficient (deg)', 'FontSize', 15, 'FontWeight','bold');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%This is the place to write your results:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

U=input('UPDATE RESULTS: 0=YES 1=NO \n')
if U == 0
  Ap=resultP(1,1);
  Bp1=resultP(2,1);
  Bp2=resultP(3,1);
  Bp3=resultP(4,1);
  As1=resultS(1,1);
  As2=resultS(2,1);
  As3=resultS(3,1);
  UU=input('UPDATE THE VARABLE: \n 1...Ap \n 2...Bp1 \n 3...Bp2 \n 4...Bp3 \n 5...As1 \n 6...As2 \n 7...As3 \n');
  fid=fopen('Result_phase1.out','r+');
  if fid==-1
    fid=fopen('Result_phase1.out','w');
    fprintf(fid,'Ap: \t %f \t %f \nBp1: \t %f \t %f \nBp2: \t %f \t %f \nBp3: \t %f \t %f \nAs1: \t %f \t %f \nAs2: \t %f \t %f \nAs3: \t %f \t %f \n',Ap,sqrt(COVARIANCE1_P(1,1)),Bp1,sqrt(COVARIANCE1_P(2,2)),Bp2,sqrt(COVARIANCE1_P(3,3)),Bp3,sqrt(COVARIANCE1_P(4,4)),As1,sqrt(COVARIANCE1_S(1,1)),As2,sqrt(COVARIANCE1_S(2,2)),As3,sqrt(COVARIANCE1_S(3,3)));
    fclose(fid);
    fid=fopen('Result_phase1.out','r+');
  end;
  fscanf(fid,'%s',1);
  App=fscanf(fid,'%f');
  fscanf(fid,'%s',1);
  Bpp1=fscanf(fid,'%f');
  fscanf(fid,'%s',1);
  Bpp2=fscanf(fid,'%f');
  fscanf(fid,'%s',1);
  Bpp3=fscanf(fid,'%f');
  fscanf(fid,'%s',1);
  Ass1=fscanf(fid,'%f');
  fscanf(fid,'%s',1);
  Ass2=fscanf(fid,'%f');
  fscanf(fid,'%s',1);
  Ass3=fscanf(fid,'%f');
  switch UU 
   case 1
    App=[resultP(1,1) sqrt(COVARIANCE1_P(1,1))];
   case 2
    Bpp1=[resultP(2,1) sqrt(COVARIANCE1_P(2,2))];
   case 3
    Bpp2=[resultP(3,1) sqrt(COVARIANCE1_P(3,3))];;
   case 4
    Bpp3=[resultP(4,1) sqrt(COVARIANCE1_P(4,4))];
   case 5
    Ass1=[resultS(1,1) sqrt(COVARIANCE1_S(1,1))];;
   case 6
    Ass2=[resultS(2,1) sqrt(COVARIANCE1_S(2,2))];
   case 7
    Ass3=[resultS(3,1) sqrt(COVARIANCE1_S(3,3))];
  end;
  frewind(fid);
  fprintf(fid,'Ap: \t %f \t %f \n',App);
  fprintf(fid,'Bp1: \t %f \t %f \n',Bpp1);
  fprintf(fid,'Bp2: \t %f \t %f \n',Bpp2);
  fprintf(fid,'Bp3: \t %f \t %f \n',Bpp3);
  fprintf(fid,'As1: \t %f \t %f \n',Ass1);
  fprintf(fid,'As2: \t %f \t %f \n',Ass2);
  fprintf(fid,'As3: \t %f \t %f \n',Ass3);  

end;






