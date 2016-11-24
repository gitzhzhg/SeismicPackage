function snew=modifysonic(s,z,t,to,tw,ts)
% MODIFYSONIC is a program that will add or subtract a bulk sonic shift 
%    from the sonic log in order to match top picks.  It is the algoritim 
%    used in the GUI program STRETCHWELL   
%
% snew=modifysonic(s,z,t,to,tw,ts)
%
%   s = sonic
%   z =depth
%   t =time vector
%   to=time at top of section where seismic and well agree
%   tw=bottom pick in well
%   ts= bottom pick in seismic
% Variables out
%   snew = New sonic log
%
% H.J.E. Lloyd, December 2012
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
dzz=z(2)-z(1);
tnew=(1/10^6)*cumsum(s.*dzz)*2;
tnew=tnew-tnew(1);
znew=interp1(tnew,z,t);
% [tz,zt,vins]=sonic2tz(s,z,-10000);
% tnew=interp1(zt,tz,z);
% znew=interp1(tz,zt,t);
alim=t(2)-t(2)/2;

io=near(t,to);
is=near(t,ts);
iw=near(t,tw);
zo=znew(io);zo=zo(1);
zs=znew(is);zs=zs(end);
zw=znew(iw);zw=zw(end);




iz1=near(z,zo);iz1=iz1(1);
iz2=near(z,zw);iz2=(iz2(end));

%%
dt=(ts-tw)/2;
a=(10^6*dt)/((zw-zo)); % assume ds=a

snew=s;
snew(iz1:iz2)=s(iz1:iz2)+a;

tt=(1/10^6)*cumsum(snew.*dzz)*2;
tt=tt-tt(1);
k=1;
tn(k)=tt(iz2);
disp(['New Time:',num2str(tn(end)),' , Number of Iterations:',num2str(k),' , Alpha:',num2str(a)]);