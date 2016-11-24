% APERSTUDY: study the aperture effect
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
A=[1000  4000  12000  20000];
z=0:25:20000;
vo1=3500;c1=0;
h=zeros(1,8);
for k=1:length(A)
    theta=thaper(A(k),vo,c,z);
	thetac=thaper(A(k),vo1,c1,z);
    h(2*k-1)=plot(z,thetac,['b' ls{k}]);
    if(k==1) hold; end
    h(2*k)=plot(z,theta,['r' ls{k}]);
end




%p=get(gcf,'position');
%set(gcf,'position',[p(1:2) 700 700])
ylabel('scattering angle in degrees')
xlabel('depth in meters')
set(gca,'xtick',[0:5000:20000])
set(gca,'ytick',[0:30:180])
set(gca,'ylim',[0 180])
legend(h,['Const v A=' int2str(A(1))],['Linear v(z) A=' int2str(A(1))],...
    ['Const v A=' int2str(A(2))],['Linear v(z) A=' int2str(A(2))],....
    ['Const v A=' int2str(A(3))],['Linear v(z) A=' int2str(A(3))],....
    ['Const v A=' int2str(A(4))],['Linear v(z) A=' int2str(A(4))])
%whitefig
%grid
	