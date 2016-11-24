%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inversion of Rpp and Rps from data file - PHASE2
% (this version supports the ORT reflecting halfspace and ISO, VTI, 
% HTI or ORT ALIGNED incidence halfspace):
%
% the surface parameters from PHASE1, corresponding 
% to the small-medium incidence angles, are further
% inverted for more convenient combinations of medium 
% parameters, knowing vp/vs ratio and maybe something else. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;

global b_a;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% first some basic information: 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_a=0.55715;                        % vs/vp ratio: NECESSARY INPUT!
%b_a=0.3578;
%in_file='Result_phase1.out';            % the name of input file with Ap,Bp and As
in_file='Result_phase1_bias.out';            % the name of input file with Ap,Bp and As
%in_file='Result_phase1_exact.out';                                     % parameters

control=3;                           % type of the incidence halfspace:
                                     % 0 ... ISO
				     % 1 ... VTI
				     % 2 ... HTI alligned
				     % 3 ... ORT alligned
				    
%%%%%%%%%%%%%%
% get the data:
%%%%%%%%%%%%%%

fid=fopen(in_file,'r');
fscanf(fid,'%s',1);
AAp=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBp1=fscanf(fid,'%f');
fscanf(fid,'%s',1);
BBp2=fscanf(fid,'%f');
fscanf(fid,'%s',1);
AAs1=fscanf(fid,'%f');
%AAs1(1,1)=-0.7464;
fscanf(fid,'%s',1);
AAs2=fscanf(fid,'%f');

%%%%%%%%%%%%
% INVERSION:
%%%%%%%%%%%%

switch control
 case 0
  ORT(0,AAp,BBp1,BBp2,AAs1,AAs2);
 case 1
  ORT(1,AAp,BBp1,BBp2,AAs1,AAs2);
 case 2
  ORT(2,AAp,BBp1,BBp2,AAs1,AAs2);
 case 3
  ORT(3,AAp,BBp1,BBp2,AAs1,AAs2);
 otherwise
  fprintf('WRONG CHOICE OT THE INCIDENCE MEDIUM \n');
end;

break;
%%%%%%%%%%%%%
% END OF FILE:
%%%%%%%%%%%%%
