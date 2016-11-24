%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read_in.m is a very trivial routine to read in resutls of the inversion
% into Matlab environment. The results then can be analysed using various
% Matlab routines (sum, std, ...).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

clear all;
close all;

load RESULT.out  % Here you load in the RESULT file to be analysed.
file=RESULT;     % The right side must be the same as the name of the
                 % RESULT file above excluding the suffix.
%load EXACT.out
%file=EXACT;

line=file(:,1);
rho=file(:,2);
a=file(:,3);
b=file(:,4);
b_a=file(:,5);
e1_1=file(:,6);
e2_1=file(:,7);
d1_1=file(:,8);
d2_1=file(:,9);
g1_1=file(:,10);
g2_1=file(:,11);
d3_1=file(:,12);
e1_2=file(:,13);
e2_2=file(:,14);
d1_2=file(:,15);
d2_2=file(:,16);
g1_2=file(:,17);
g2_2=file(:,18);
d3_2=file(:,19);
kap=file(:,20);

v33_1(1:length(a),1)=[3.0];
rho_1(1:length(rho),1)=[2.1];
v33_2=(-(2+a).*v33_1)./(a-2);
rho_2=(-(2+rho).*rho_1)./(rho-2);
v55_1=(b_a.*(b-2).*(v33_2+v33_1))./((-b-2).*(1+(b-2)./(-b-2)));
v55_2=b_a.*(v33_2+v33_1)-v55_1;

% some useful combinations: will be different for different aniso
% symmetries. 
G=(rho+2*b+0.25*rho.*b.^2)./(1+0.25*b.^2+0.5*rho.*b);
gs_1=(g1_1-g2_1)./(1+2*g2_1);
gs_2=(g1_2-g2_2)./(1+2*g2_2);
P=d2_2-d1_2+8*b_a.^2.*gs_2;                   
S=(1./(2*(1+b_a))).*(d2_2-d1_2)+2*b_a.*gs_2;




