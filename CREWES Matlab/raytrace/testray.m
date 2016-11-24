% TESTRAY: demo the v(x,z) raytrace code
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

%ray test
dg=10;

tstep=0:.004:3;
x=0:dg:5000;z=x';
v=1800+.6*(z*ones(1,length(x)));

rayvelmod(v,dg);

theta=pi*45/180;
r0=[0,0,sin(theta)/1800,cos(theta)/1800]';

tic
[t,r]=shootrayvxz(tstep,r0);
toc

tic
[tg,rg]=shootrayvxz_g(tstep,r0);
toc

tic
[tm,rm]=ode45('drayvec',tstep,r0);
toc

plot(rm(:,1),rm(:,2)+2*dg,rg(:,1),rg(:,2)+dg,r(:,1),r(:,2));flipy
legend('drayvec','shootrayvxz\_g','shootrayvxz')