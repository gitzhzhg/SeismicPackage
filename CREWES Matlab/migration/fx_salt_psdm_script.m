clear
%Please see 'Ferguson_Margrave_2005.pdf' or 'Ferguson and Margrave, 2005,
%Planned seismic imaging using explicit one-way operators, Geophysics, V70,
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

%NO5, S101 - S109' for details.

dip=70;%maximum dip (degrees).
fmax=38;%maximum frequency (Hz) of interest.
fmin=4;%minumum frequency (fmin > 0 Hz) of interest.
load Pro_salt_vel%EAGE/SEG salt model.
[rm cm]=size(model);
load shot_rec
dt=t(2);
dx=xrec(2);
dz=4;

%***get sizes of things***
[rs,cs]=size(seis);%seis is rs rows by cs columns.
%*************************

%***build freqeuncy axis of interest***
f=1/2/dt/rs*[0:2:rs-2];%positive frequency axis in Hz.
nf=min(find(f>=fmax));%index in f for fmax.
f1=max(find(f<=fmin));%index in f for fmin.
f=f(f1:nf);%desired range of positive frequencies.
%**************************************

%***2D FFT***
temp=ifft(fft(seis),[],2);clear seis%fft t->f and ifft x-> kx.
fseis=temp(f1:nf,:);%spectrum band-limited between fmin and fmax.
%   NOTE f axis is band-limited and +ve only.
%   NOTE kx axis is uncentred.
%************

%***migrate***
disp('Your output file will be "fx_salt_mig"')
pause(3)
fx_psdm=fx_mig(fseis,f,model,dx,dz,dip);
%*************

%***output***
save fx_salt_mig fx_psdm
%************