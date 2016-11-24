function editvel(edit,v,win)
% Edit the first and second layer velocities by dragging  
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

% (values at each receivers)  
v1rec=refdata('get','v1rec');
v2rec=refdata('get','v2rec');
recelev=refdata('get','recelev');
coord=recelev(1,:);
set(gcf,'units','pixels','position',[0 0 864 576]);
% Optional median filter applicable to velocity values
if (edit==0)
  % First layer velocity only
  if (v==1)
	v1rec=medfilt1(v1rec,win);
  end
  % Second layer velocity only
  if (v==2)
	v2rec=medfilt1(v2rec,win);
  end
  % Both first and second layer velocities
  if (v==3)
	v1rec=medfilt1(v1rec,win);
	v2rec=medfilt1(v2rec,win);
  end
end
hold on
v1handle = [];
if(length(v1rec)>1)
	v1handle = plot(coord,1000*v1rec,'g-.')
end
v2handle = [];
if(length(v2rec)>1)
	v2handle = plot(coord,1000*v2rec,'r-.')
end
xlabel('Coordinate (m)')
ylabel('Velocity (m/s)')
title('Velocity model (first layer=green; second layer=red)')
% Call the edit function
editveldepth('init',[v1handle v2handle],'vel');