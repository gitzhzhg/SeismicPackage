function eventraymod(fignoz,fignot,pts,clr)
% EVENTRAYMOD: raytrace model a picked event assuming normal incidence
%
% eventraymod(fignoz,fignot,pts,clr)
%
% EVENTRAYMOD uses a velocity model initialized by RAYVELMOD and a
% vector of points picked using the picking facility in PLOTIMAGE.
% For each pair of npoints, a normal incidence ray is determined 
% and traced from the picked point to the surface. The traveltime is
% determined and plotted (2-way) in the figure indicated.
%
% fignoz ... figure number to get the depth (dip) picks from
% fignot ... figure number to plot the times in
% pts ... n x 2 matrix of (x,t) vaues from plotimage or equivalent
% ********** default is to use the global PICKS ***************
% ********** a value of nan for pts will invoke the default *************
% clr ... color to plot rays with
%
% G.F. Margrave, CREWES, June 2000
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
global PICKS

if(nargin<1)
	error('You must give a figure number for picks')
end
if(nargin<2)
	error('You must give a figure number to plot times in')
end

%resolve figure numbers
[nfigs,nnn]=size(PICKS);
doit=0;
for kkp=1:nfigs
    pickfig=PICKS{kkp,1};
    if(pickfig==fignoz)
        doit=1;
        break
    end
end

if(~doit)
    error('invalid figure number for picks')
end

doit=0;
for kkt=1:nfigs
    tfig=PICKS{kkt,1};
    if(tfig==fignot)
        doit=1;
        break
    end
end

if(~doit)
    error('invalid figure number for times')
end

if(nargin<4)
    clr='r';
end

if(nargin<3 || isnan(pts) )
	global PICKS PICKCOLOR
	pts=PICKS{kkp,2};
end

% 
% if(isempty(PICKCOLOR))
% 	clr='r';
% else
% 	clr=PICKCOLOR;
% end

[npts,nc]=size(pts);
params=[fignot, fignoz, .004, 2, nan];
for k=1:npts
	dip=180*atan((pts(k,4)-pts(k,2))/(pts(k,3)-pts(k,1)))/pi;
	x0=.5*(pts(k,3)+pts(k,1));
	z0=.5*(pts(k,4)+pts(k,2));
	if(~isnan(dip))
        normray(x0,z0,dip,params,clr);
    end
end