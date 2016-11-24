function MisfitSort;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MISFITSORT generates a set of initial models (used by the NonLinGrad
% program in the next phase) sorted from the best-to-worst fitting
% models, based on a misfit function evaluation between predicted Rpp, Rps1 and Rps2
% reflection coefficients and the corresponding data. The models are
% generated according to the a priori information that may be available.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INPUT: 
% I)  Data file organized in the matrix as follows: each row represents the
%     input data in the form [*; *; inc_angle; azimuth; Rpp; Rps1; Rps2;
%     *; *; *; *; *; *; *; *; *]. The "*" symbol represents an arbitrary number. 
% II) Initial settings which control the program behavior as described bellow.
%     Those settings include a priori information that may be available.
%
% OUTPUT:
% I)  Data.in ... output->input data file (either real data or synthetically generated
%     data) used in the next phase by the NonLinGrad program.
% II) InitModels.in ... output->input file for NonLinGrad containing the sorted
%     set of initial models from MisfitSort.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;
%path(path,'/Net/dobrin/home1/pjilek/Matlab_lib');
%path(path,'/Net/dobrin/home1/pjilek/Matlab_lib/Optim');

global Pdata
global Sdata
global S1data
global S2data
global RECORD

% -----------------
% INITIAL SETTINGS:
% -----------------

% 1) symmetry of the model and the data file:	       

model_ID=6;         % (0=ISO)  (3=HTI aligned) (4=HTIxHTI) (6=ORTxORT)

% 2) a priori information; based on the fact, that there are some initial
%    estimates of the incidence anisotropic parameters (or their differences
%    in the case of ORT media) usually available; initial models will be
%    generated within the ranges defined bellow;
%    NOTE: the ranges of model parameters bellow apply to initial models only;
%    the finally inverted  models obtained in PHASE2 may be outside of these ranges,
%    see the input in NonLinGrad. It is usually good to define the ranges
%    bellow "as best as you can" but to be neither too exact nor too
%    relaxed. Too narrow ranges may generate initial models outside of
%    the zone of interest, too relaxed ranges may generate too exotic
%    initial models with a sparse coverage of the model space. 

n_group=6;  % number of basic groups of models being randomly generated; the basic
            % group is a group of all possible combinations defined
            % particularly for a specific anisotropic symmetry:
	    %
	    % HTI symmetry: 
	    % the incidence and reflecting medium
            % anisotropy parameters are grouped into triplets [ev1 dv1 g1]
            % and [ev2 dv2 g2]. Each parameter X from the triplets attains
            % the values V1=[MX MX+TX MX-TX] and V2=[MX+0.5TX MX-0.5TX], where TX is
            % the tolerance and MX the mean of X prescribed bellow. 
	    % The basic group consists of
            % all possible combinations of [ev1 dv1 g1] reaching the
            % values V1 and then V2, i.e., 35 combinations. The same type
            % of 35 combinations is created for [ev2 dv2 g2]. Then the
            % triplets [ev1 dv1 g1] and [ev2 dv2 g2] are randomly
            % combined. Alongside, the isotropic parameters and the
            % parameter kappa are generated randomly, using a priori
            % information provided bellow. The final number of all models
            % thus generated is (n_group*35).
	    %
	    % ORT symmetry:
	    % the triplets of differences [e1_1-e2_1 d1_1-d2_1 gs_1] are
            % created (using zero and non-zero values of the individual
            % x1_1 and x2_1 incidence-medium anisotropy parameters) to
            % attain all values [MDx MDx+TDX MDx-TDx], where Dx is the
            % difference of the incidence-medium anisotropy
            % parameter with the mean value MDx and tolerance TDx, as
            % defined bellow. The randomness of the group results from
            % random generations of the rest of the anisotropy 
            % and isotropy medium parameters. The final number of models
            % thus generated is (n_group*54). 

R0_range=[0.25 0.25];    % give the range of P-wave AVO intercepts;
RP_range=[-0.7 -0.7];    % give the range of P-wave AVO gradients;

ISO=[0 1 1]; % indicate by +1 the weight on an apriori knowledge of either
             % [Drho Dv33 Dv55] parameter; this a priori knowledge has
             % priority in the model building process. If ISO=[1 1 1],
             % than all parameters Drho Dv33 and Dv55 will be taken from
             % the corresponding values specified bellow. If ISO=[0 0 0],
             % then the values will fully honor the data-driven
             % quantities R0 (P-AVO intercept) and RP (P-AVO gradient) in
             % the "isotropic sense". 
	     
r_range=[0.0 0.8] ;  % give the range of Drho;
a_range=[0.25 0.3];  % give the range of Dv33;
b_range=[0.0 0.8];  % give the range of Dv55;

b_a_range=[0.5 0.5]; % give the range of possible beta/alpha ratios;
kap_range=[0. 90.];  % give the range of kappa IN DEGREES!!!

 % For HTI media only (model_ID=3 or model_ID=4):

if model_ID==3 | model_ID==4
 
 M_ev1=-0.1;  % medium value of ev_1;
 T_ev1=0.1;  % tolerance in the value;
 M_dv1=-0.15;  % medium value of dv1_1;
 T_dv1=0.1;  % tolerance in the value;
 M_g1=0.1;  % medium value of g_1;
 T_g1=0.05;  % tolerance in the value;
 M_ev2=-0.15;  % medium value of ev_1;
 T_ev2=0.1;  % tolerance in the value;
 M_dv2=-0.15;  % medium value of dv1_1;
 T_dv2=0.1;  % tolerance in the value;
 M_g2=0.1;  % medium value of g_1;
 T_g2=0.05;  % tolerance in the value;

end;

if model_ID==5 | model_ID==6
 
 % For ORT media only (model_ID=5 or model_ID=6):

 M_De1=-0.05;  % medium value of (e1_1-e2_1);
 T_De1=0.1;  % tolerance in the value;
 M_Dd1=-0.30;  % medium value of (d1_1-d2_1);
 T_Dd1=0.05;  % tolerance in the value;
 M_gs1=0.1;  % medium value of (gs_1~=g1_1-g2_1);
 T_gs1=0.05;  % tolerance in the value;

 rest=0.15;  % the tolerance of the remaining anisotropy parameters
            % <-rest;+rest> around zero;
	
end;

% 3) data error contamination:

cont=0;       % 0...input data will be contaminated by an error defined bellow
              %     (applies to all synthetic examples) 
              % 1...already error contaminated input data
	      %     (applies to real data)

std_p=0.2;    % standard deviation (uniformly distributed error) - P-data
std_s1=0.2;   % standard deviation (uniformly distributed error) - S1-data
std_s2=0.2;   % standard deviation (uniformly distributed error) - S2-data
stt=10;       % the initial state of the random-number generator used for
              % data contamination (but, not for LSM semi-random generation)



% --------------------
% DATA PRE-PROCESSING:
% --------------------

% 1) read in the data from a file:

Dname=input('Enter input data-file name: \n','s');
fid0=fopen(Dname);
D=fscanf(fid0,'%f',[16 Inf]);  
fclose(fid0);
model=D';

fprintf('Running...\n');

inc_deg=model(:,3);
azim_deg=model(:,4);
Rpp=model(:,5);
Rps1=model(:,6);
Rps2=model(:,7);

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
    
  % At this point, the error-contaminated (synthetic) data (Pdata, S1data,
  % S2data) are loaded in and ready for further analysis. 
  % The inc. angles and azimuths are in "inc" and "azim".
  


% -----------------------------------
% BUILDING LSM (Large Set fo Models):
% -----------------------------------
% (The rows of the LSM matrix represent individual models)

% setup the ranges of ISO parameters and kappa:

RECORD=-1;

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
  if model_ID==5   % ORTHORHOMBIC aligned:
    Ftemp = ORT_align_objective(LSM(I,:),inc,azim,I,1); 
  end;
  if model_ID==6   % ORTHORHOMBIC misaligned:
    Ftemp = ORT_objective(LSM(I,:),inc,azim,I,1);
  end;
  F(I)=sum(Ftemp.^2);
end;


% -----------------------------------------------------------------------------------
% DETERMINE FSM (Final Set of Models) = Sorted models from the best to the worst fit:
% -----------------------------------------------------------------------------------

MSort=[F' LSM];
MSort=sortrows(MSort,[1]);
[x y]=size(MSort);

for I=1:x
  FSM(I,:)=[I MSort(I,:)];
end;


% -------------------------------------
% STORE THE RESULTS FSM IN OUTUP FILES:
% -------------------------------------

fid=fopen('InitModels.in','w');           % Final Sorted Models, starting with the best

if model_ID==0
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \n',FSM');
elseif model_ID==3
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',FSM');
elseif model_ID==4
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',FSM');
elseif model_ID==5
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',FSM');
elseif model_ID==6
  fprintf(fid,'%i \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n',FSM');
end;
fclose(fid);

fprintf('*** PROGRAM FINISHED SUCCESFULLY *** \n');
  
%%%%%%%% END OF STORY %%%%%%%%%%%%%%%%%%%%%%%%%%%

