function [tz,zt,vins]=sonic2tz(sonic,z,nlegs,tnot,titlestring,flag,wrnflag)
% [tz,zt,vins]=sonic2tz(sonic,z,nlegs,tnot)
% [tz,zt,vins]=sonic2tz(sonic,z,nlegs)
% sonic2tz(sonic,z,nlegs,tnot,title,flag) ... for analysis display
% sonic2tz(sonic,z,nlegs) ... for analysis display
%
% SONIC2TZ computes an approximate 2-way time depth curve from a sonic log for
% use with depth conversion of logs. The approximation is a piecewise linear
% curve whos points are taken directly from the exact t-z curve (which is
% computed from directly integrating the sonic.) The sample points occur
% at depths defined by the location of the largest peaks of the Hilbert 
% envelope of the derivative of the sonic (the derivative of the sonic is
% proportional to reflection coeficient at constant density.) At these sample
% points the time depth approximation will give zero error compared to the
% exact curve. The error between control points is reduced by computing
% more legs on the approximate curve; however, many legged curves will lead
% to long time-depth conversion run-times. To get help in choosing the number
% of legs, run the function with no return arguments and it will produce a
% display showing the fit of the approximation with estimated depth conversion
% error.
%
% sonic = a vector containing the sonic log. Sonic should be in units of
%   microseconds per length unit.
% z = vector of depth's for the sonic log.
% ******** length(z) must equal length(sonic) *********
% nlegs = number of legs on the piecewise linear approximation
% NOTE: If sonic2tz is unable to find the requested number of legs, it
%       will terminate with an error message. This can be avoided by
%       entering nlegs as a negative number in which case execution will
%       proced with a warning that fewer than nlegs were found.
% tnot = starting time corresponding to z(1)
% *********** default = 0.0 **********
% title = string with a title for the analysis displays
% flag = 0 ... compute estimated error in time for analysis displays
%      = 1 ... compute estimated error in depth for analysis displays
% *********** default = 1 ***********
% wrnflag = 0 ... do not show warning message if fewer than nlegs are found
%         = 1 ... show warning message if fewer than nlegs are found
% *********** default = 1 ***********
% tz = vector of length nlegs+2 containing the 2-way times of the  
%	approximate t-z curve
% zt = vector of length nlegs+2 containing the depths of the approximate
%	t-z curve
% vins = vector of length nlegs+2 containing the instantaneous velocities 
%	implied by the approximate t-z curve
%
% G.F. Margrave, March 1994
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
%
% $Id: sonic2tz.m,v 1.5 2009/07/24 19:51:13 gary Exp $
if(nargin<4)
	tnot=0;
end
if(nargin < 6)
	flag=1;
end
if(nargin < 7)
	wrnflag=1;
end
iwarn=0;
if( nlegs<0 )
	iwarn=1;
	nlegs=abs(nlegs);
end
%convert to column vectors
[m,n]=size(sonic);
sonic=sonic(:);
z=z(:);
%compute detailed tz curve
%compute delta z's
nz=length(z);
dz=diff(z);
%compute the mean of each pair of sonic samples
%s2=(sonic(1:nz-1)+sonic(2:nz))*.5;
%integrate
t=int_sonic(sonic,z)+tnot;
if(length(t)<=abs(nlegs) & iwarn)
	disp(['Log too short for approximations, returning exact curve, length= '...
		int2str(length(t))])
	tz=t;
	zt=z;
	vins= 1.e06 ./sonic;
	return;
end
%compute approx rc's and then hilbert env and mask
%(transpose is necessary to force gradient to give a real result)
rcs=gradient(sonic',z');
[mask,henv]=hmask(rcs,.005);
henv=abs(henv);
spikes=mask.*henv;
%find the nlegs largest
%kzone=nz/3;
%nlegs=3*round(nlegs/3);
%mlegs=nlegs/3;
%ilegs=[];
%for k=1:3
% iz1=1+(k-1)*kzone;
% iz2=min([iz1+kzone-1,nz]);
%	spikezone=spikes(iz1:iz2);
%	[spikezone,is]=sort(spikezone);
%	spikezone=flipud(spikezone);
%	is=flipud(is);
%	ilegs=[ilegs (iz1+is(1:mlegs-1)-1)];
%end
%[spikes,is]=sort(spikes);
%spikes=flipud(spikes);
%is=flipud(is);
%ilegs=is(1:nlegs-1);
nlegsasked=nlegs;
izone=nz*.06;      % Does anyone know what izone means? Please describe it here.
                   % It -feels- like an overlap window or something to smooth
                   % between legs. I don't really know though. 
                   
ilegs=zeros(1,round(2*nlegs));
jlegs=1;
while(jlegs<nlegs)
    spike2=spikes;       % take a copy of the spikes
    ind=find(ilegs~=0);  % find all the bits of ilegs that are nonzero.
                         % the first time through, there are none, so ind =
                         % 0. 
                         
    for k=1:length(ind) 
        i1=max([1,ilegs(ind(k))-izone]);  % The minimum index of the ileg, including overlap
        i2=min([ilegs(ind(k))+izone,nz]); % The max index of the ileg, including overlap
        spike2(i1:i2)=zeros(size(i1:i2)); % set everything in this ileg range to zero on spike2.
                                          % I think this part protects the
                                          % ilegs that we have already
                                          % determined. So it sort of masks
                                          % them out on the spike2 set,
                                          % which then constrains us to
                                          % setting only those parts of
                                          % ilegs which haven't been set
                                          % before.
    end
    
    kmax=nlegs-length(ind);	              % we can only deal with the number of legs we have left.
    
    for k=1:kmax
        if( sum(spike2)==0.0) % If we have wiped out the entire spike2 in
                              % that last operation, then we have filled
                              % all of the slots in ilegs and so we're done. I
                              % think so, anyhow.
            break;
        end
        ii=find( spike2==max(spike2) ); % look for the biggest spike(s). 
        
        ilegs(jlegs:jlegs+length(ii)-1)=ii; % set ilegs to be those highest spikes. 
                                            % If there are n spikes of
                                            % the same height within the
                                            % spikes, then n elements
                                            % of ilegs are set.
       for kk=1:length(ii)  % for each of the 'n' elements we just set
           i1=max([1,ilegs(jlegs+kk-1)-izone]);  % find the minimum of the ileg including overlap
           i2=min([ilegs(jlegs+kk-1)+izone,nz]); % find the max of the ileg including overlap
           spike2(round(i1):round(i2))=zeros(size(round(i1):round(i2))); % now set that bit to zero 
                                                                         % so we ignore it next time around
           
        end
        jlegs=jlegs+length(ii);
    end
    izone=floor(izone/2); % Now we shrink the smoothing window. 
    if (izone==0) break; end % If we have no more window, then stop.
end
nlegs=jlegs;
% disp([ int2str(nlegs) ' legs found '])
if( nlegs<nlegsasked & ~iwarn)
    errordlg({' Unable to find requested number of legs';
        ['Only ' num2str(nlegs) ' Available']})
	error(' Unable to find requested number of legs');
elseif( nlegs<nlegsasked & iwarn )
    if wrnflag
	disp(['WARNING: SONIC2TZ found only ' int2str(nlegs) ' legs where '...
		int2str(nlegsasked) ' were requested'])
    end
end
%make sure nlegs is exactly nlegsasked
if( nlegs > nlegsasked )
	nlegs=nlegsasked;
end
	
nlegs=nlegs-1;
ilegs=ilegs(1:round(nlegs));
	
%now the legged tz function
i1=find(t(1)==t(ilegs));
i2=find(t(nz)==t(ilegs));
if(isempty(i1) & isempty(i2))
	tz=[t(1);t(ilegs);t(nz)];
	zt=[z(1);z(ilegs);z(nz)];
elseif(~isempty(i1))
	tz=[t(ilegs);t(nz)];
	zt=[z(ilegs);z(nz)];
elseif(~isempty(i2))
	tz=[t(1);t(ilegs)];
	zt=[z(1);z(ilegs)];
end
% sort into order
[zt,is]=sort(zt);
tz=tz(is);
% the instantaneous velocity
% transpose for a real result, 2 for 2 way time
vins=2*gradient(zt',tz');
vins=vins(:);
if(m==1)
	tz=tz';
	zt=zt';
	vins=vins';
end
if( nargout==0 )
% create analysis plots
	hfig=figure('visible','off','menubar','none');
	% first the tz curve
	subplot(1,3,1);
	hax1=gca;
	plot(t,z,'b');
	line(tz,zt,'color','r','marker','.','markersize',12);
	title('TZ curves');
	set(gca,'ylabel',text(0,0,'Depth'),'xlabel',text(0,0,'Time'),'Ydir','reverse');
	%text(0,-35,'MB1->draw box to zoom. MB1->single click to unzoom',...
	%		'units','pixels');
	grid;
	%now the instantaneous velocity comparison
	subplot(1,3,2);
	hax2=gca;
	plot(1.e06 ./sonic,z,'b')
	line(vins,zt,'color','r','linewidth',2);
	title('Vins');
	set(gca,'xlabel',text(0,0,'Velocity'),'ylabel',text(0,0,'Depth'),'ydir','reverse');
	grid;
	% now the error
	subplot(1,3,3);
	hax3=gca;
	if( flag )
		tt=t(1):.008:t(length(t));
		zz=pwlint(tz,zt,tt);
		t=linspace(min(t),max(t),length(t));
		zzex=interp1(t,z,tt);
		plot(zz(:)-zzex(:),tt)
		grid
		title('Estimated error')
		set(gca,'xlabel',text(0,0,'Depth Error'),'ylabel',text(0,0,'Time'),'ydir','reverse');
	else
		zz=linspace(z(1),z(length(z)),200);
		tt=pwlint(zt,tz,zz);
		ttex=interp1(z,t,zz);
		plot(tt(:)-ttex(:),zz,'b')
		grid
		title('Estimated error')
		set(gca,'xlabel',text(0,0,'Time Error'),'ylabel',text(0,0,'Depth'),'ydir','reverse');
	end
	simplezoom;
	tz=[];
	zt=[];
	vins=[];
	if(nargin>=5)
		%a figure title
		pos=get(hfig,'position');
		ht=pos(4);
		Ht=pos(4)+41;
		htit=uicontrol('style','text','string',titlestring,'units','normalized', ...
			'position',[1/pos(3),(pos(4)+21)/Ht,(pos(3)-2)/pos(3),20/Ht]);
		htit2=uicontrol('style','text','string',...
			'Blue=exact, Red=approx, MB1->draw box to zoom. MB1->single click to unzoom',...
			'units','normalized','position',[1/pos(3),pos(4)/Ht,(pos(3)-2)/pos(3),20/Ht]);
		%shift each axes down
		pa=get(hax1,'position');
		set(hax1,'position',[pa(1) ht*pa(2)/Ht pa(3) ht*pa(4)/Ht]);
		pa=get(hax2,'position');
		set(hax2,'position',[pa(1) ht*pa(2)/Ht pa(3) ht*pa(4)/Ht]);
		pa=get(hax3,'position');
		set(hax3,'position',[pa(1) ht*pa(2)/Ht pa(3) ht*pa(4)/Ht]);
		%resize the figure
		set(hfig,'position',[pos(1:3) Ht]);
	end
	
	% a close button
	uicontrol('style','pushbutton','string','close','position',...
		[1,1,45,20],'callback','close(gcf)');
		
	set(hfig,'visible','on');
end