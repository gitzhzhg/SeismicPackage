figure('menubar','none')
%first break vs offset
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

%shotpickm=median(shotpick);
%[b,a]=fir1(6,[0.1,0.2]);

%shotpickf=filtfilt(b,a,shotpick);
plot(shotoffset(1:189,:),shotpick(1:189,:),'o');
xlabel('offset (m)')
ylabel('traveltime (ms)')
title('Refracted P-wave picks for shots #1-189')
%first derivative
[m,n]=size(shotoffset);
delx=shotoffset(:,1:n-50)-shotoffset(:,51:n);
delt=shotpick(:,1:n-50)-shotpick(:,51:n);  
avgx=(shotoffset(:,1:n-50)+shotoffset(:,51:n))/2;
deriv=delx./delt;
absavgx=abs(avgx);
absderiv=abs(deriv);
figure('menubar','none')
plot(absavgx(1:189,:),absderiv(1:189,:),'o');
axis([0,3500,2,4]);                     
xlabel('offset (m)')
ylabel('velocity (1000 m/s)')
title('First derivative of the P-wave  picks for shots #1-189')
%second derivative
[m2,n2]=size(deriv);
delx2=delx(:,1:n2-10)-delx(:,11:n2);
delt2=delt(:,1:n2-10)-delt(:,11:n2);
avgx2=(avgx(:,1:n2-10)+avgx(:,1:n2-10))/2;
deriv2=delx2./delt2;
absavgx2=abs(avgx2);
%absderiv2=abs(deriv2);
figure('menubar','none')
plot(absavgx2(1:189,:),deriv2(1:189,:),'o');
axis([0,4000,-0.5,0.5]);  
xlabel('offset (m)')
ylabel('acceleration (1000 m/s2)')
title('Second derivative of the P-wave picks for shots #1-189')