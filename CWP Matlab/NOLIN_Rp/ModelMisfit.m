% AUTHOR: Petr Jilek, Colorado School of Mines, Center for Wave
% Phenomena, January 2002.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ModelMisfit.m is an auxiliar but important routine. For a given model
% (characterized by a certain value of the objective function) and
% tolerance, the routine returns all data points in which the model exceeds
% the prescribed tolerance. If the model fits the data within the tolerance,
% the result is an empty matrix. The routine also returns mean misfit value
% with its standard deviation (in perfect fit, both are equal zero). The sum
% of these two quntities (in absolute sence) is a valuable measure of how
% the model, recovered using L2 "average" norm, fits the data. If the sum
% exceeds the prescribed tolerance (which is the measure of our confidence
% in our data quality), the model can be rejected as "model that does not
% fit the data".
%
% After unmarking the corresponding section denoted as "CIJ section 1" or
% "CIJ section 2" below, the routine also can give stifnesses Cij_1 and
% Cij_2 of the halfspaces of the given model.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This routine runs fast enough even in Matlab environment, so there is
% no need for compilation of the routine into an executable.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INPUT: see the initial settings
% OUTPUT: most output is self-explanatory text and numbers on the screen,
%         the parameter wave_ID: =1 ... P-wave, =2 ... S1-wave, =3 ... S2-wave
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all;
clear all;

global Pdata
global Sdata
global S1data
global S2data
global RECORD

%Make sure the path is correct!
path(path,'/Net/dobrin/home1/pjilek/src/Matlab/NOLIN_Rp/LIB');      %Obj_Rcf library M routines

% ==========================
% START OF INITIAL SETTINGS:
% ==========================

model_ID=4;           % model identification, see README.MAIN
load Data.in;         % name of the file containing data (such as Data.in file)
load RESULT.out;      % name of the result file (such as RESULT.out)
data=Data;              % the right side of the relation must be
                        % identical to the name of the loaded in data
                        % file without the suffix.
model=RESULT;           % the right side of the relation must be
                        % identical to the name of the loaded in result
                        % file without the suffix.

% =======================
% END OF INITIAL SETTINGS
% =======================

% ---------------
% PROGRAM STARTS:
% ---------------

N=input('Provide the ordinary model number from RESULT.out file (do not count first two lines): \n');
N=N+2;
misfit=0.05;
misfit1=input('Enter the required data-model misfit (0.05 default): \n');
if isempty(misfit1)==0
  misfit=misfit1;
end;
RECORD=1;

inc_deg=data(:,1);
azim_deg=data(:,2);
Pdata=data(:,3);
S1data=data(:,4);
S2data=data(:,5);
inc=inc_deg*pi/180;
azim=azim_deg*pi/180;


Drho=model(N,2);
DV33=model(N,3);
DV55=model(N,4);
b_a=model(N,5);

if model_ID==0

  medium=[Drho DV33 DV55 b_a];
  F=ISO_objective(medium,inc,azim,1,0);
  output=[inc_deg azim_deg; inc_deg azim_deg];
  output(:,3)=F';
  
end;  

if model_ID==3 | model_ID==4
  ev_1=model(N,6);
  dv_1=model(N,7);
  g_1=model(N,8);
  ev_2=model(N,9);
  dv_2=model(N,10);
  g_2=model(N,11);
  
  if model_ID==3
    
    medium=[Drho DV33 DV55 b_a ev_1 dv_1 g_1 ev_2 dv_2 g_2];
    F=HTI_align_objective(medium,inc,azim,1,0);
    output=[inc_deg azim_deg; inc_deg azim_deg; inc_deg azim_deg];
    output(:,3)=F';
    
  end;

  if model_ID==4
    
    kap=model(N,12);
    medium=[Drho DV33 DV55 b_a ev_1 dv_1 g_1 ev_2 dv_2 g_2 kap];
    F=HTI_objective(medium,inc,azim,1,0);
    output=[inc_deg azim_deg; inc_deg azim_deg; inc_deg azim_deg];
    output(:,3)=F';
    
  end;

%% CIJ section 1: UNMARK HERE if the Cij's are desired %%%%%%%%
  
%  a1=3.5;                     % THIS IS MY CHOICE!!! (should be arbitrary)
%  rho1=2.5;                   % THIS IS MY CHOICE!!! (should be arbitrary)
%  a2=a1*(1+0.5*DV33)/(1-0.5*DV33);
%  b1=b_a*(a2+a1)*(0.5-0.25*DV55);
%  b2=b_a*(a2+a1)-b1;
%  rho2=rho1*(1+0.5*Drho)/(1-0.5*Drho);
%  
%  Cij_1=HTI_Cij([a1 b1 ev_1 dv_1 g_1]);
%  Cij_2=HTI_Cij([a2 b2 ev_2 dv_2 g_2]);

%  rho1
%  Cij_1
%  rho2
%  Cij_2
%  kap

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
end;

if model_ID==5 | model_ID==6
  e1_1=model(N,6);
  e2_1=model(N,7);
  d1_1=model(N,8);
  d2_1=model(N,9);
  g1_1=model(N,10);
  g2_1=model(N,11);
  d3_1=model(N,12);
  e1_2=model(N,13);
  e2_2=model(N,14);
  d1_2=model(N,15);
  d2_2=model(N,16);
  g1_2=model(N,17);
  g2_2=model(N,18);
  d3_2=model(N,19);
  
  if model_ID==5
    
    medium=[Drho DV33 DV55 b_a e1_1 e2_1 d1_1 d2_1 g1_1 g2_1 d3_1 e1_2 e2_2 ...
	  d1_2 d2_2 g1_2 g2_2 d3_2];
    F=ORT_align_objective(medium,inc,azim,1,0);
    output=[inc_deg azim_deg; inc_deg azim_deg; inc_deg azim_deg];
    output(:,3)=F';
    
  end;

  if model_ID==6
    
    kap=model(N,20);
    medium=[Drho DV33 DV55 b_a e1_1 e2_1 d1_1 d2_1 g1_1 g2_1 d3_1 e1_2 e2_2 ...
	  d1_2 d2_2 g1_2 g2_2 d3_2 kap];
    F=ORT_objective(medium,inc,azim,1,0);
    output=[inc_deg azim_deg; inc_deg azim_deg; inc_deg azim_deg];
    output(:,3)=F';
        
  end;

%% CIJ section 2: UNMARK HERE if the Cij's are desired %%%%%%%%
  
%  a1=3.5;                     % THIS IS MY CHOICE!!! (should be arbitrary)
%  rho1=2.5;                   % THIS IS MY CHOICE!!! (should be arbitrary)
%  a2=a1*(1+0.5*DV33)/(1-0.5*DV33);
%  b1=b_a*(a2+a1)*(0.5-0.25*DV55);
%  b2=b_a*(a2+a1)-b1;
%  rho2=rho1*(1+0.5*Drho)/(1-0.5*Drho);
%  
%  Cij_1=ORT_Cij([a1 b1 e1_1 e2_1 d1_1 d2_1 g1_1 g2_1 d3_1]);
%  Cij_2=ORT_Cij([a2 b2 e1_2 e2_2 d1_2 d2_2 g1_2 g2_2 d3_2]);

%  rho1
%  Cij_1
%  rho2
%  Cij_2
%  kap

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
end;

row=find(output(:,3)>misfit);

for J=1:length(row)
  index(J)=1;
  if row(J) > length(Pdata)
    index(J)=2;
  end;
  if row(J) > (length(Pdata)+length(S1data))
    index(J)=3;
  end;
end;

if isempty(row)
  fprintf('\n MODEL FITS THE DATA WITHIN THE PRESCRIBED TOLERANCE \n');
else
  fprintf('\n    wave_ID | inc_angle |  azim | misfit \n');
  fprintf('    ------------------------------------ \n');
  [index' output(row,:)]
end
fprintf('\n MEAN MISFIT VALUE: %f \n',mean(abs(output(:,3))));
fprintf(' STANDARD DEVIATION OF THE MISFIT: %f \n',std(abs(output(:,3))));

