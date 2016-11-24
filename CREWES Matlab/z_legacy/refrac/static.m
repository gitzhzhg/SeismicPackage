function [recstat,shotstat] =static(depth, shotcoord, shotelev, recelev, uphole, datum, repvel,psd, v1rec, v2rec)
% Calculate the receiver and the shot static corrections according to a depth 
% and velocity model. Three static corrections: the weathering correction,
%  the elevation correction and the total correction.  
% The weathering correction correspond to the time needed to bring the 
% receiver or the shot at the base of the first layer using the depth and the
% velocity of the first layer.  In method 1, the elevation correction
% correspond to the time needed to bring back the shot or the receiver from 
% the base of the first layer to a flat datum using a replacement velocity. 
% In method 2, the elevation correction include the time to bring the shot or
% the receiver from the base of the first layer to a pseudo datum below the 
% first layer using the second layer velocity and the time to bring the shot
% or the receiver back to the final datum using a replacement velocity. 
% The total correction is a summation of the weathering and the elevation 
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

% corrections.
if (psd==0) % method 1
	% Weathering static computation
	hg = depth(2,:);
	wsrec = -hg./v1rec;
	hs = interpextrap(depth(1,:),depth(2,:),shotcoord);
	hs=hs';
	v1shot = interpextrap(depth(1,:),v1rec,shotcoord);
	v1shot=v1shot';
	wsshot = (-hs./v1shot) + uphole';
	% Elevation static computation
	
	repvel=repvel/1000;
	esrec = (datum-(recelev(2,:)-hg))./repvel;
	esshot = (datum-(shotelev-hs))./repvel;
else % method 2
	% Weathering static computation
	hg = depth(2,:);
	wsrec = -hg./v1rec;
	hs = interpextrap(depth(1,:),depth(2,:),shotcoord);
	hs=hs';
	v1shot = interpextrap(depth(1,:),v1rec,shotcoord);
	v1shot=v1shot';
	wsshot = (-hs./v1shot) + uphole';
	% Elevation static computation
	depthelev(1,:)=recelev(2,:)-depth(2,:);
	pseudo=min(depthelev);
	repvel=repvel/1000;
	esrec = (datum-pseudo)/repvel  -((recelev(2,:)-hg)-pseudo)./v2rec;
	v2shot = interpextrap(depth(1,:),v2rec,shotcoord);
	v2shot=v2shot';
	esshot = (datum-pseudo)/repvel  -((shotelev-hs)-pseudo)./v2shot;
end
recstat(1,:)=wsrec;
recstat(2,:)=esrec;
recstat(3,:)=wsrec+esrec;
shotstat(1,:)=wsshot;
shotstat(2,:)=esshot;
shotstat(3,:)=wsshot+esshot;
% Update menus
PMTsetmenus;