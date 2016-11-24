function dipspect(a,t,dx,f,vo,c,z,thrline,thapline,thaline)
% DIPSPECT: make dip spectral analysis plot for v(z)
%
% dipspect(a,t,dx,f,vo,c,z,thrline,thapline,thaline)
% dipspect(a,t,dx,f,vo,c,z)
%
% DIPSPECT makes a dip spectral analysis plot for v=vo+cz
% If z is not provided, then it is computed with 
% z=linspace(0,zmax,nz). Here, zmax is computed as the maximum
% depth for the supplied record length and nz=500. (If z is
% provided as a single number, then it is taken to be nz).
%
% a ... aperture
% t ... maximum recording time
% dx ... spatial sampling interval
% f ... frequency of interest
% vo ... initial velocity
% c ... accelerator ( v(z) = vo +c*z )
% z ... vector of depths for which limits are computed
% thrline ... color to plot the record length limit with
%    ********* default 'r' *********
% thapline ... color to plot the aperture limit with
%    ********** default 'c' ********
% thaline .... color to plot with alias limit with
%    ********* default 'g' ********
%
%
% G.F. Margrave, CREWES Project, 1997
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

if(nargin<10)
	thaline='g';
end
if(nargin<9)
	thapline='c';
end
if(nargin<8)
	thrline='r';
end

if(nargin<7)
	z=500;
end

if(length(z)==1)
	[thetar,z]=threc(t,vo,c,z);
else
	thetar=threc(t,vo,c,z);
end
ir=find(imag(thetar)==0);

thap=thaper(a,vo,c,z);
inda=find(imag(thap)==0);
thal = thalias(dx,f,vo,c,z);
indal=find(imag(thal)==0);

figure;

plot(z(ir),thetar(ir),thrline,z,thap,thapline,z(indal),thal(indal),thaline);
legend(['Record length limit, T=' num2str(t) ' sec'],...
	['Aperture limit, A=' int2str(a) ' lu'],...
	['Spatial aliasing limit, dx=' int2str(dx) ' lu, f=' ...
	int2str(f) ' Hz']);
	
title([' Dipspect chart for vo= ' int2str(vo) ' c= ' num2str(c)])
grid