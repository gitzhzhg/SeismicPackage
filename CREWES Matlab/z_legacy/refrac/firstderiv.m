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

[m,n]=size(shotoffset)
delx=shotoffset(:,1:n-50)-shotoffset(:,51:n);
delt=shotpick(:,1:n-50)-shotpick(:,51:n);  
avgx=(shotoffset(:,1:n-50)+shotoffset(:,51:n))/2;
deriv=delx./delt;
absavgx=abs(avgx);
absderiv=abs(deriv);
figure('menubar','none')
plot(absavgx(1:189,:),absderiv(1:189,:),'o');
axis([0,3500,2,4]);                     
%axis([-4000,4000,-20,20]);  
%axis([-4000,4000,-3,3]);  
%plot(avgx(1:1,:),deriv(1:1,:),'o');
%figure
%plot(shotoffset(1:1,:),shotpick(1:1,:),'o')
%plot(delt(1:1,:),shotpick(1:1,:),'o')