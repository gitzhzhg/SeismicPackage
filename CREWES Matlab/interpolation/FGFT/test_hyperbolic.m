% Testing regularly sampled seismic data 
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

close all
clear all
clc

%% hyperbolic events
load hyperbolic_data.mat

[nt nh]=size(d);

% Decimating data
d_orig=d;
d=d_orig(:,1:2:nh);

[nt nh]=size(d);

% Parameters for LPF interpolation
pctg=0.65;

% Interpolation using different values of lambda
Y=fx_fgft_interpolation(d,pctg);

% Replacing original traces
Y(:,1:2:2*nh)=d;

figure
subplot(121)
imagesc(d); title("Original T-X");
subplot(122)
imagesc(abs(fftshift(fft2(d,512,512)))); title("Original F-K");
figure
subplot(121)
imagesc(Y);title("Interpolated T-X");
subplot(122)
imagesc(abs(fftshift(fft2(Y,512,512)))); title("Interpolated F-K");