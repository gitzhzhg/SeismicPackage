function [h,hva]=wtva(s,t,kolor,snot,polarity,dir,resampfact)
% [h,hva]=wtva(s,t,kolor,snot,polarity,dir,resampfact)
% hs=wtva(s,t,kolor,snot,polarity,dir,resampfact)
% wtva ... with no input arguments for a demo plot
%
% WTVA draws a vector using wiggle trace variable area display. The
% vector is drawn by calling LINE and the variable area shading is
% done with a call to PATCH. This results in 2 handles for the resultant
% display, one for the line and the other for the va. WTVA draws only
% one vector at a time. Plots of multiple traces should be done with
% a loop calling WTVA separately for each trace.
% If two return arguments are specified, then the first is the handle
% of the wiggle trace and the second is the va. If only one is 
% specified then it is returned as a two element vector.
% NOTE: In order to accurately create the variable area display, additional
% 	samples must be inserted in the trace precisly at the "zero crossings".
%	This means that if you use the trace's handle to get its samples (e.g.
%	samples=get(h,'ydata') ) that you will get something different from
%	your input trace.
%
% s = the vector to be plotted
% t = the sample 'times' for s. s and t must be the same size
% kolor = the color to be used for the display. Any MATLAB color spec will
%		work here such as 'g' or 'green' or [0 1 0]. See Matlab reference manual
%		under ColorSpec for more information.
% ************ default = 'g' ************
% snot = amplitude value which determines the shading. That is, with polarity
%		of 1, all s>snot will have va shading
% ************ default = mean of the live samples of s *************
% polarity determines whether peaks or troughs are shaded 
% polarity = +1 shade peaks (s>snot)
% polarity = -1 shade troughs (s<snot)
% ************** default: polarity =1 **************
% dir=1 ... vertical plot ( line(s,t) is called )
% dir=-1 ... horizontal plot ( line(t,s) is called )
% ************** default = 1 *********************
% resampfact ... resampling factor for a smoother, but slower, plot
%   Must be a positive integer. 
% ************** default = 4 *******************
%
% G.F. Margrave, April 1994
%        revised June 1995, Oct 1995
% The CREWES Project
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
	if(nargin == 0)
	% do a demo
		%Make a fake reflectivity
		t=0:.002:2.0;
		r=randn(size(t)).^5;
		%make a ricker wavelet
		tw=-.1:.002:.1;
		arg=(pi*15*tw).^2;
		w=(1-2.*arg).*exp(-arg);
		%convolve
		s=conv(r,w);
		s=s(51:length(t)+50);
		s=s/max(s); %normalize
		%open up a figure
		figure
	end
	if(nargin<7)
		resampfact=4;
	end
	if(nargin<6)
		dir=1;
	end
	
	if(nargin<5)
		polarity=1;
	end
	if(nargin<4)
		ilive=find(~isnan(s));
		snot=mean(s(ilive));
	end
	
	if(nargin<3)
		kolor='g';
	end
	
	if(abs(polarity)~=1)
		error('invalid polarity value');
	end
	
	%find the parts to shade
	% Method:
	%		1) Find all points not to be shaded
	%		2) find the boundary points of the groups of 1). That is
	%			determine how the points fall into contiguous groups and
	%			where these groups start and end
	%		3) Find any groups of 1) which are single isolated points and
	%			add an extra point so that the minimum group size is 2
	%		4) For all groups, interpolate in the zero crossings at the 
	%			beginning and end of the group and assign the first and
	%			last points in the group to these values
	%		5) assign all other points in the group to the value 0 (or snot)
	%		6) plot the polygon using patch with no edgecolor
	%
	%test for dead trace
	test=sum(abs(s-snot));
	if(test>1000*eps)
		% resample
		if(resampfact>1)
			% my resampling
			[s,t]=resamp(s,t,(t(2)-t(1))/resampfact,[t(1) t(length(t))],0);
			% SIgnal toolbox resampling
			%s=resample(s,resampfact,1);
			%tmax=t(1)+(length(s)-1)*(t(2)-t(1))/resampfact;
			%t=t(1):(t(2)-t(1))/resampfact:tmax;
		end
		
	
		dt=t(2)-t(1);
		ss=polarity*[snot;s(:);snot];
		snot=polarity*snot;
		n=length(ss);
		tt=[t(1)-dt;t(:);t(length(t))+dt];
		% put snot on both ends. This forces the detection of isolated singular
		% points at the ends
		
		%find all points which won't be shaded
		il=find(ss<=snot);
		ilow=[0;il;n+1]; %end conditions
		%now find the beginning and ends of these zones
		ind=diff(ilow);
		ibdy=find(ind>1);
		% find singular groups
		ind=find(diff(ibdy)==1);
		ising=ilow(ibdy(ind)+1);
		if(~isempty(ising))
			if(ising(1)==0)
				ising(1)=[];
			end
			if(ising(length(ising))==n+1)
				ising(length(ising))=[];
			end
			if(~isempty(ising))
				for k=1:length(ising)
					%duplicate singular points
					ss=[ss(1:ising(k)); ss(ising(k)); ss(ising(k)+1:length(ss))];
					tt=[tt(1:ising(k)); tt(ising(k)); tt(ising(k)+1:length(tt))];
					ising=ising+1;
				end
			end
			% refind the points because the point count may have changed
			%find all points which won't be shaded
			il=find(ss<=snot);
			ilow=[0;il;n+1]; %end conditions
			%now find the beginning and ends of these zones
			ind=diff(ilow);
			ibdy=find(ind>1);
		end
					
		
		ibdy1=ilow(ibdy);
		ibdy2=ilow(ibdy+1);
		if(ibdy1(1)==0)
			ibdy1(1)=[];
		end
		if(ibdy1(length(ibdy1))==n+1)
			ibdy1(length(ibdy1))=[];
		end
		if(ibdy2(1)==0)
			ibdy2(1)=[];
		end
		if(ibdy2(length(ibdy2))==n+1)
			ibdy2(length(ibdy2))=[];
		end
		
		%interpolate in zero crossings at the beginnings
		tnot=(snot-ss(ibdy1))./(ss(ibdy1+1)-ss(ibdy1));
		tnot=tnot.*(tt(ibdy1+1)-tt(ibdy1))+tt(ibdy1);
		tt(ibdy1)=tnot;
		%interpolate in zero crossings at the ends
		tnot=(snot-ss(ibdy2))./(ss(ibdy2-1)-ss(ibdy2));
		tnot=tnot.*(tt(ibdy2-1)-tt(ibdy2))+tt(ibdy2);
		tt(ibdy2)=tnot;
		% set all troughs to snot
		ss(il)=snot*ones(size(il));
		
		% make sure we have snot on both ends
		ss=[snot;ss(2:length(ss)-1);snot];
		tt=[t(1);tt(2:length(tt)-1);t(length(t))];
	else
		ss=s;
		tt=t;
	end
	
	if(dir==1)
		hva=patch('xdata',polarity*ss,'ydata',tt,...
			'edgecolor','none','facecolor',kolor);
	else
		hva=patch('xdata',tt,'ydata',polarity*ss,...
		'edgecolor','none','facecolor',kolor);
	end
	%plot with line
	if(dir==1)
		h=line(s,t,'color',kolor);
	else
		h=line(t,s,'color',kolor);
	end
	if(nargout==1)
		h=[h hva];
		clear hva;
	end