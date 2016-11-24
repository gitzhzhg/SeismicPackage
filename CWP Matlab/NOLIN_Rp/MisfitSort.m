% AUTHOR: Petr Jilek, Colorado School of Mines, Center for Wave
% Phenomena, January 2002.  

function MisfitSort;
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MISFITSORT generates a pool of starting models PSM (used by the NonLinGrad
% programs in PHASE2) sorted from the best- to the worst-fitting
% models, based on a misfit function evaluation between computed Rpp, Rps1 and Rps2
% reflection coefficients and the corresponding data. The models are also
% generated according to a priori information that may be available.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   You should be familiar with the meanings of basic variables, 
%   as described in README.MAIN file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INPUT: 
% I)  Data file organized in the matrix as follows: each row represents the
%     input data in the form [*  *  inc_angle  azimuth  Rpp  Rps1  Rps2 
%     *  *  *  *  *  *  *  *  *]. The "*" symbol represents an arbitrary
%     real number. The number of rows corresponds to the number of
%     (inc_angle;azimuth) pairs for which are provided the data
%     (Rpp;Rps1:Rps2). The Rpp, Rps1 and Rps2 can be either real data
%     (containing errors) or synthetically generated exact reflection
%     coefficients (with no errors). In the later case, the reflection
%     coefficients will be contaminated by random errors as defined below.
% II) Initial settings which control the program behavior as described below.
%     Those settings include a priori information that may be available.
%
% OUTPUT:
% I)  Data.in ... output data file [reformatted error-contaminated data
%     file from INPUT I)] used as input data file in PHASE2. The rows of Data.in
%     file have the following form: [inc_angle  azimuth  Rpp  Rps1  Rps2].
% II) InitModels.in ... output PSM file containing the sorted
%     pool of starting models. The rows of InitModels.in
%     file may have the following forms:  
%
%     if model_ID=0: 
%     [ON  Fobj  Drho  Dv33  Dv55  v55_v33] 
%     if model_Id=3: 
%     [ON  Fobj  Drho  Dv33  Dv55  v55_v33  ev_1  dv_1  g_1 ev_2 dv_2 g_2]
%     if model_Id=4: 
%     [ON  Fobj  Drho  Dv33  Dv55  v55_v33  ev_1  dv_1  g_1  ev_2 dv_2 g_2 kappa]
%     if model_Id=5: 
%     [ON  Fobj  Drho  Dv33  Dv55  v55_v33  e1_1  e2_1 d1_1
%     d2_1 g1_1 g2_1 d3_1 e1_2 e2_1 d1_2 d2_2 g1_2 g2_2 d3_2]
%     if model_Id=6: 
%     [ON  Fobj  Drho  Dv33  Dv55  v55_v33  e1_1  e2_1 d1_1
%     d2_1 g1_1 g2_1 d3_1 e1_2 e2_1 d1_2 d2_2 g1_2 g2_2 d3_2 kappa]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;

%-------
% Unmark this if the routine is to be run under Matlab,
% mark this if the routine is to be compiled into an executable.
% If used, adjust the path of the libraries accordingly.
%
%close all;
%path(path,'/Net/dobrin/home1/pjilek/src/Matlab/NOLIN_Rp/LIB');      %Obj_Rcf library M routines
% this library should be installed - Matlab Optimization Toolbox
%path(path,'.../Optim');                                              %Optim library M routines
%-------

global Pdata
global Sdata
global S1data
global S2data
global RECORD

% ==========================
% START OF INITIAL SETTINGS:
% ==========================

%
% 1) symmetry of the model and the data file:	       
% -------------------------------------------
%
model_ID=4;         % (0=ISO)  
                    % (3=HTI) 
		    % (4=HTIxHTI) 
                    % (5=ORT) 
		    % (6=ORTxORT)

% 2) A priori information may be sometimes available. Some most likely types
%    of a priori information for different symmetries are encoded here. Initial
%    models will be generated within the ranges defined below. NOTE: the ranges
%    of the model parameters below apply to PSM models only; the finally
%    inverted models obtained in PHASE2 may be outside of these ranges, if this
%    is required to fit the data (see the input in NonLinGrad programs). It is
%    usually good to define the ranges below "as good as you can" but to be
%    neither too narrow nor too relaxed. Too narrow ranges may generate INITIAL
%    models outside the zone of right solution, too relaxed ranges may generate
%    too exotic INITIAL models with a sparse coverage of the model
%    space. However, if a priori information is unknown, more relaxed
%    ranges are preferable.
% --------------------------------------------------------------------
%
n_group=117;  % Number of BASIC GROUPS OF MODELS (BGM) being randomly
            % generated; BGM is a group of M models generated in a certain
            % predefined pattern. The higher n_group is chosen, the denser
            % coverage of the model space is achieved, but also the longer
            % computational time is needed. The pattern of generated BGM is
            % defined individually for each anisotropic symmetry:
	    %
	    % ISO symmetry (model_ID=0): M=1.
	    % n_group determines directly the number of all isotropic
            % initial models generated in PSM. The ISO BGM consists of one
            % model vector [Drho Dv33 Dv55 v55/v33] randomly generated
            % according to the provided ranges below. 
	    %
	    % HTI symmetry (model_ID=3,4): M=35.
	    % The anisotropy parameters of the incidence and reflecting
            % hafspaces are grouped into triplets [ev_1 dv_1 g_1] and [ev_2 dv_2
            % g_2] respectively. Each parameter X from each triplet attains
            % three main values V1=[MX MX+TX MX-TX] (where TX is the
            % tolerance and MX the mean of X as prescribed below) and then
            % two complementary values V2=[MX+0.5TX MX-0.5TX]. All possible
            % combinations of [ev_1 dv_1 g_1] reaching the values V1 are then
            % created (i.e., 27 combinations), plus all combinations
            % reaching the values V2 (8 additional combinations) = 35
            % combinations alltogether. Similarly, 35 combinations are
            % created for the parameters [ev_2 dv_2 g_2] of the reflecting
            % halfspace. Then the 35 triplets [ev_1 dv_1 g_1] and 35 triplets
            % [ev_2 dv_2 g_2] are randomly combined resulting in 35 vectors
            % [ev_1 dv_1 g_1 ev_2 dv_2 g_2]. Alongside, the isotropic parameters
            % and the parameter kappa are generated randomly, using a priori
            % information provided below, resulting in 35 model vectors
            % [Drho Dv33 Dv55 v55/v33 ev_1 dv_1 g_1 ev_2 dv_2 g_2 kappa] which
            % define the basic group. This process is repeated n_group
            % times, so the final number of all models in PSM thus generated
            % is n_group*35. It is ensured that there are not generated any
            % two identical model vectors.
	    %
	    % ORT symmetry (model_ID=5,6): M=54.
	    % For orthorhombic media, it is more reasonable to assume a
            % priori estimates of the differences [(e1_1-e2_1) (d1_1-d2_1)
            % (gs_1=~g1_1-g2_1)] rather than the individual anisotropy
            % parameters. Therefore, each PSM model is generated such that
            % the differences of its incident-medium anisotropy parameters
            % X1_1-X2_1=Dx1 attain the values [MDx1 MDx1+TDx1 MDx1-TDx1],
            % where MDx1 is the mean value of Dx1, and TDx1 is the
            % tolerance, as defined bellow. Each single value MDx1 in the
            % basic group is achieved twice: once with X1_1=0 and then with
            % X2_1=0. Therefore, there are 2x27 basic combinations of
            % different triplets [(e1_1-e2_1) (d1_1-d2_1)
            % (gs_1=~g1_1-g2_1)]. The remaining medium parameters are
            % generated randomly, however, with no repetition, resulting in
            % a basic group of 54 model vectors [Drho Dv33 Dv55 v55/v33 e1_1
            % e2_1 d1_1 d2_1 g1_1 g2_1 d3_1 e1_2 e2_2 d1_2 d2_2 g1_2 g2_2
            % d3_2 kappa]. The final number of all models in PSM is
            % n_group*54

R0_range=[0.150 0.150];    % give the range of P-wave AVO intercepts;
RP_range=[-0.13 -0.13];    % give the range of P-wave AVO gradients; NOTE:
                           % usually, it is beneficial to set the range with
                           % a zero length (i.e., [min max=min]), even if
                           % only approximate values are known.
ISO=[0 1 1]; % indicate by +1 the weight on a priori knowledge of either
             % [Drho Dv33 Dv55] parameter; this a priori knowledge has
             % priority in the model building process. If ISO=[1 1 1],
             % than all parameters Drho Dv33 and Dv55 will be taken from
             % the corresponding values specified bellow. If ISO=[0 0 0],
             % then the values will fully honor the data-driven
             % quantities R0 (P-AVO intercept) and RP (P-AVO gradient) in
             % the "isotropic sense". In that case, Dv33 will be always taken
             % randomly from the range specified bellow, Drho will be 
             % determined from Dv33 and the R0 AVO intercept, and, finally, 
             % Dv55 will be determined using Dv33, Drho and RP AVO
             % gradient. Notice the redundancy: ISO=[0 0 0]=[0 1 0].
	     
r_range=[-0.8 0.8];  % give the range of Drho;
a_range=[-0.8 0.8];  % give the range of Dv33;
b_range=[-0.8 0.8];  % give the range of Dv55;

b_a_range=[0.5 0.5]; % give the range of possible v55/v33 ratios;
kap_range=[0. 90.];  % give the range of kappa IN DEGREES!!!

%
% For HTI media only (model_ID=3 or model_ID=4):
%

if model_ID==3 | model_ID==4
 
 M_ev1=-0.15;  % medium value of ev_1;
 T_ev1=0.15;  % tolerance in the value;
 M_dv1=-0.15;  % medium value of dv1_1;
 T_dv1=0.15;  % tolerance in the value;
 M_g1=-0.15;  % medium value of g_1;
 T_g1=0.15;  % tolerance in the value;
 M_ev2=-0.15;  % medium value of ev_2;
 T_ev2=0.15;  % tolerance in the value;
 M_dv2=-0.15;  % medium value of dv1_2;
 T_dv2=0.15;  % tolerance in the value;
 M_g2=-0.15;  % medium value of g_2;
 T_g2=0.15;  % tolerance in the value;

end;

%
% For ORT media only (model_ID=5 or model_ID=6):
%

if model_ID==5 | model_ID==6
 
 M_De1=-0.15;  % medium value of (e1_1-e2_1);
 T_De1=0.1;  % tolerance in the value;
 M_Dd1=-0.30;  % medium value of (d1_1-d2_1);
 T_Dd1=0.05;  % tolerance in the value;
 M_gs1=0.1;  % medium value of (gs_1~=g1_1-g2_1);
 T_gs1=0.05;  % tolerance in the value;

 rest=0.15;  % the tolerance of the remaining anisotropy parameters
            % <-rest;+rest>, the mean value is always zero;
	
end;

%
% 3) data error contamination:
% ----------------------------
%
cont=0;       % 0...input data will be contaminated by an error defined bellow
              %     (applies to synthetically generated Rp, Rps1 and Rps2
              %     data) 
              % 1...input data are already contaminated by errors
	      %     (applies to real data)

std_p=0.2;    % standard deviation (uniformly distributed error) - P-data
std_s1=0.2;   % standard deviation (uniformly distributed error) - S1-data
std_s2=0.2;   % standard deviation (uniformly distributed error) - S2-data
stt=10;       % the initial state of the random-number generator used for
              % data contamination (but, not for PSM semi-random
              % generation); this way, it is possible to repeatedly generate
              % identical random-error-contaminated data sets. This can
              % be useful for repeating inversion of the same synthetic
              % data using different initial settings, or to carry out
              % the inversion (PHASE1 and PHASE2) on several processors
              % to speed up the procedure. 

% =======================
% END OF INITIAL SETTINGS
% =======================



% --------------------
% DATA PRE-PROCESSING:
% --------------------

% 1) read in the data from a file:

Dname=input('Enter input data-file name: \n','s');
fid0=fopen(Dname);
D=fscanf(fid0,'%f',[16 Inf]);  
fclose(fid0);
model=D';

inc_deg=model(:,3);
azim_deg=model(:,4);
% THIS IS THE PLACE TO RECALIBRATE THE DATA: used for testing purposes,
% such as to study influence of a wong calibration of the reflection
% coefficients. 
XP=1.;
XS1=1.;
XS2=1.;
% END of the recalibration.
Rpp=XP*model(:,5);
Rps1=XS1*model(:,6);
Rps2=XS2*model(:,7);

inc=inc_deg*pi/180;
azim=azim_deg*pi/180;

% 2) error contamination of the data (if synthetic example):

if cont==1
  Pdata=Rpp;
  S1data=Rps1;
  S2data=Rps2;
else
  n_data_P=length(Rpp);
  n_data_S1=length(Rps1);
  n_data_S2=length(Rps2);
  IP(1:n_data_P,1)=[1];
  IS1(1:n_data_S1,1)=[1];
  IS2(1:n_data_S2,1)=[1];
  
  rand('state',stt);                    % set the state of the generator
  %rand('state',sum(100*clock));        % random state of the generator
  TEMP=rand(n_data_P,1);                % unirormly distributed random numbers
  Pdata=Rpp.*(IP+std_p*(0.5-TEMP));
  TEMP=rand(n_data_S1,1);
  S1data=Rps1.*(IS1+std_s1*(0.5-TEMP));
  TEMP=rand(n_data_S1,1);
  S2data=Rps2.*(IS2+std_s2*(0.5-TEMP));
end;

fout = fopen('Data.in', 'w');
fprintf(fout, '%f \t %f \t %f \t %f \t %f \n', [inc_deg azim_deg Pdata S1data S2data]');
fclose(fout);
    
  % At this point, the error-contaminated data (Pdata, S1data,
  % S2data) are loaded in and ready for further analysis. 
  % The inc. angles and azimuths are in "inc" and "azim".
  


% -----------------------------------
% BUILDING LSM (Large Set of Models):
% -----------------------------------
% (The rows of the LSM matrix represent individual models)

% setup the ranges of ISO parameters and kappa:

R0_mean=mean(R0_range);
R0_t=(max(R0_range)-min(R0_range))/2;
RP_mean=mean(RP_range);
RP_t=(max(RP_range)-min(RP_range))/2;

r_mean=mean(r_range);
r_t=(max(r_range)-min(r_range))/2;
a_mean=mean(a_range);
a_t=(max(a_range)-min(a_range))/2;
b_mean=mean(b_range);
b_t=(max(b_range)-min(b_range))/2;

b_a_mean=mean(b_a_range);
b_a_t=(max(b_a_range)-min(b_a_range))/2;
kap_mean=mean(kap_range);
kap_t=(max(kap_range)-min(kap_range))/2;

MEAN=[R0_mean RP_mean r_mean a_mean b_mean b_a_mean kap_mean];
T=[R0_t RP_t r_t a_t b_t b_a_t kap_t];

%
% model: ISO [Drho Da Db b_a]
%

LSM=[];

if model_ID==0
  LSM=LSM_build_ISO(ISO,MEAN,T,n_group);
end;

%
% model: HTI [Drho Da Db b_a ev_1 dv_1 g_1 ev_2 dv_2 g_2 kappa]
%

if model_ID==3 | model_ID==4
  LSM=LSM_build_HTI(ISO,MEAN,T,M_ev1,T_ev1,M_dv1,T_dv1,M_g1,T_g1,M_ev2, ...
		    T_ev2,M_dv2,T_dv2,M_g2,T_g2,n_group);
  if model_ID==3
    LSM(:,11)=[];
  end;
end;

%
% model: ORTHORHOMBIC [Drho Da Db b_a e1_1 e2_1 d1_1 d2_1 g1_1 g2_1 d3_1 
%                            e1_2 e2_2 d1_2 d2_2 g1_2 g2_2 d3_2 kappa]

if model_ID==5 | model_ID==6
  LSM=LSM_build_ORT(ISO,MEAN,T,M_De1,T_De1,M_Dd1,T_Dd1,M_gs1,T_gs1,n_group,rest);
  if model_ID==5
    LSM(:,19)=[];
  end;
end;

% -------------------------------
% OBJECTIVE FUNCTION EVALUATIONS:
% -------------------------------

% model prototypes:
%
% ISOTROPIC ......medium=[Drho DV33 DV55 b_a];
% HTI ............medium=[Drho DV33 DV55 b_a ev_1 dv_1 gv_1 ev_2 dv_2 gv_2 kappa]; 
% ORTHORHOMBIC ...medium=[Drho DV33 DV55 b_a ev1_1 ev2_1 dv1_1 dv2_1 g1_1 g2_1 d3_1 
%                               ev1_2 ev2_2 dv1_2 dv2_2 g1_2 g2_2 d2_3 kappa];

%LSM(5:length(LSM),:)=[];

[x y]=size(LSM);

for I=1:x
  RECORD=-2;
  if model_ID==0   % ISO:
    Ftemp = ISO_objective(LSM(I,:),inc,azim,I,1); 
  end;
  if model_ID==3   % HTI aligned:
    Ftemp = HTI_align_objective(LSM(I,:),inc,azim,I,1); 
  end;
  if model_ID==4   % HTI misaligned:
    Ftemp = HTI_objective(LSM(I,:),inc,azim,I,1);
  end;
  if model_ID==5   % ORTHORHOMBIC aligned:
    Ftemp = ORT_align_objective(LSM(I,:),inc,azim,I,1); 
  end;
  if model_ID==6   % ORTHORHOMBIC misaligned:
    Ftemp = ORT_objective(LSM(I,:),inc,azim,I,1);
  end;
  F(I)=sum(Ftemp.^2);
end;


% -----------------------------------------------------------------------------------
% CREATE PSM = Sorted models from the best to the worst fit:
% -----------------------------------------------------------------------------------

MSort=[F' LSM];
MSort=sortrows(MSort,[1]);
[x y]=size(MSort);

for I=1:x
  PSM(I,:)=[I MSort(I,:)];
end;


% -------------------------------------
% STORE THE RESULTS PSM IN OUTUP FILES:
% -------------------------------------

fid=fopen('InitModels.in','w');           % Final Sorted Models, starting with the best

if model_ID==0
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \n',PSM');
elseif model_ID==3
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',PSM');
elseif model_ID==4
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',PSM');
elseif model_ID==5
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',PSM');
elseif model_ID==6
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',PSM');
end;
fclose(fid);

fprintf('*** PROGRAM FINISHED SUCCESFULLY *** \n');
  
%%%%%%%% END OF STORY %%%%%%%%%%%%%%%%%%%%%%%%%%%

