% Testing forward fgft
% Author: Mostafa Naghizadeh; Copyright (C) 2010
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

clc
%close all;
clear all;

% Chirp example
dt = 4/1000;,
N = 256;
% Compute a test signal (a hyperbolic chirp)
[d,t,cf] = mychirp(10,110,dt,N);
% Add noise to the chirp
%d = d + 0.6*randn(size(d));
%d=[d;flipud(d)];
N=256;

d_orig=d;

% % Irregular interpolation
% % Randomizing the offset locations	
% kn1=randperm(N);
% % Randomly eliminating some traces
% percent=0.5;
% kn2=kn1(1:ceil(percent*N));
% % sorting random chosen traces in ascending order
% h=[sort(kn2)];
h=[1:2:N];  % Regular decimation
%h=[90:100];  % Gap
d(h)=0;

%Finding the indices of known components
h1=[1:N];
[a,b]=wcommon(h,h1);
H=find(b-1);

% Forward FGFT
D=forward_fgft(d');

% Tresholding smaller values of D
D1=forward_fgft(d_orig');
pctg=0.75;
cfs = sort(abs(D1));
nb = round(pctg*length(cfs));
cutoff = cfs(nb);
% Set small coefficients to zero
for w=1:length(D1)
    MASK(w)= (abs(D1(w))>cutoff);
end

D=scale_fgft(D);
PK=plot_fgft(D);

d3=d(H);
M=MASK;
nh=N;
iter_cg=3;
iter_bl=30;

% For irregular sampling
%INTD=irls_fitting_fgft(d3.',H,nh,iter_cg,iter_bl);

% % For regular sampling provided that we know the mask
INTD=ls_mask_fitting_fgft(d3.',H,M,nh,iter_cg);

INTD=INTD/max(abs(INTD));
figure
subplot(411)
h2=plot(linspace(1,N,N),d,'k');axis tight;
setit(h2,'Time samples','Amplitude');

subplot(412)
colormap(flipud(gray));
h2=imagesc([1:N],linspace(0,.51,129),abs(PK(1:129,:)));
setit02(h2,'Time samples','Frequency');

subplot(413)
D2=forward_fgft(INTD);
D2=scale_fgft(D2);
PK3=plot_fgft(D2);

colormap(flipud(gray));
h2=imagesc([1:N],linspace(0,.51,129),abs(PK3(1:129,:)));
setit02(h2,'Time samples','Frequency');

subplot(414)
h2=plot(linspace(1,N,N),real(INTD),'k');axis tight;
setit(h2,'Time samples','Amplitude');

PMASK=plot_fgft(MASK);