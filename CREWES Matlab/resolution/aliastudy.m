% ALIASTUDY: study aliasing versus depth
%
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

% Just run the script
ls{1}='-';ls{2}='-.';ls{3}=':';ls{4}='--';
figure;
vo=1800;c=.6;
dx=[20 40 80 160];
z=0:25:20000;
f=60;
vo1=3500;c=01;

for k=1:length(dx)
	thetac=thalias(dx(k),f,vo1,c1,z);
    theta=thalias(dx(k),f,vo,c,z);
    h(2*k-1)=plot(z,thetac,['b' ls{k}]);
    if(k==1) hold; end
    h(2*k)=plot(z,theta,['r' ls{k}]);
end

%p=get(gcf,'position');
%set(gcf,'position',[p(1:2) 700 700])
ylabel('scattering angle in degrees')
xlabel('depth in meters')
set(gca,'xtick',[0:2500:20000])
set(gca,'ytick',[0:30:90])
legend(h,['Const v \Delta x=' int2str(dx(1))],['Linear v(z) \Delta x=' int2str(dx(1))],...
    ['Const v \Delta x=' int2str(dx(2))],['Linear v(z) \Delta x=' int2str(dx(2))],....
    ['Const v \Delta x=' int2str(dx(3))],['Linear v(z) \Delta x=' int2str(dx(3))],....
    ['Const v \Delta x=' int2str(dx(4))],['Linear v(z) \Delta x=' int2str(dx(4))])
%whitefig
%grid
	