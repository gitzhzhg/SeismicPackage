function eventraymig(figno1,figno2,pts,clr)
% EVENTRAYMIG: raytrace migrate a picked event assuming normal incidence
%
% eventraymig(figno1,figno2,pts,clr)
%
% EVENTRAYMIG uses a velocity model initialized by RAYVELMOD and a
% set of events picked either using plotimage and automatically harvested from
% a plotimage figure or picked using GINPUT and passed in as an argument. 
% For each pick, a normal incidence ray is determined and projected down 
% into the velocity model.
%
% figno1 ... figure number to take picks from
% figno2 ... figure number to plot the rays in
% pts ... n x 2 matrix of (x,t) vaues from ginput or equivalent
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
	error('You must give a figure number to plot rays in')
end

%resolve figure numbers
[nfigs,nnn]=size(PICKS);
doit=0;
for kkp=1:nfigs
    pickfig=PICKS{kkp,1};
    if(pickfig==figno1)
        doit=1;
        break
    end
end

if(~doit)
    error('invalid figure number for picks')
end

% doit=0;
% for kkr=1:nfigs
%     rayfig=PICKS{kkr,1};
%     if(rayfig==figno2)
%         doit=1;
%         break
%     end
% end
rayfig=figno2;
% if(~doit)
%     error('invalid figure number for rays')
% end

if(nargin<4)
    clr='r';
end

if(nargin<3 || isnan(pts) )
	global PICKS PICKCOLOR
	pts=PICKS{kkp,2};
end

% if(isempty(PICKCOLOR))
% 	clr='r';
% else
% 	clr=PICKCOLOR;
% end

[npts,nc]=size(pts);

for k=1:npts
	%dtdx=(pts(k+1,2)-pts(k,2))/(pts(k+1,1)-pts(k,1));
    dtdx=(pts(k,4)-pts(k,2))/(pts(k,3)-pts(k,1));
	%x0=.5*(pts(k+1,1)+pts(k,1));
    x0=.5*(pts(k,1)+pts(k,3));
	t0=.5*(pts(k,2)+pts(k,4));
	params=[rayfig .004 4];
    if(~isnan(dtdx))
        normraymig(x0,t0,dtdx,params,clr);
    end
end