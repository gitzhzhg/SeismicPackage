function fact=fmhorplot(fmat,hors,vaflag,fact,flipy,kolor, horkols,tflag,hax)

% fact=fmhorplot(fmat,hors,vaflag,fact,flipy,kolor,...
%	horkols,tflag,hax)
%
% FMHORPLOT calls FMPLOT for a quick plot of a flexi-mat in 
% figure window (made by fmplot). In addition, it expects a
% LOGSEC style horizon object which it then plots on top of the
% fleximat display.
%
%  fmat ... the flexi-mat to be plotted
%  hors ... LOGSEC style horizon object
%  vaflag ... if 0, then traces are plotted at wt (wiggle traces) 
%     if 1, then the traces are plotted wtva
%      if 2 the rev. pol. wtva
%     ************* default = 1 *********
%  fact ... scaling factors. Make fact(1) bigger for bigger wiggle
%     traces. fact(2) controls the overall plot scale. If not
%     provided, it is computed as max(abs(matrix)). To scale two
%     fmplots the same, capture the return value from the first and
%     provide is as fact(2) for the second
%     ************* default 1.5 ***********
%  flipy ... if 1, then the y axis is reverse so that it goes from top
%     of window to the bottom
%     ************* default 1 ***********
%  kolor ... color to plot the traces
%     ************* default = [1 0 0] (red) ************
%  horkols ... either a single color or an nhors x 3 matrix of
%     rgb values for each horizon. If a single color, then all
%     horizons has that color
%	************ default = [ 1 1 1 ] (white) *************
%  tflag ... if 1, then horizons are in time, 0 for depth
%   ************ default =1 **********
%  hax ... handle of the axis to plot the fleximat in. 
%		If -1, then a new figure is created.
%     ************* default = -1 **************
%
% G.F. Margrave, June 1994
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

if(nargin < 9)
	hax=-1;
end
if(nargin<8)
	tflag=1;
end
if(nargin<7)
	horkols=[1 1 1];
end
if(nargin<6)
	kolor=[1 0 0];
end
if(nargin<5)
	flipy=1;
end
if(nargin<4)
	fact=1.5;
end
if(nargin<3)
	vaflag=1;
end

%call fmplot to plot the fleximat
fact=fmplot(fmat,vaflag,fact,flipy,kolor,hax);

hornames=objget(hors,'namesmatrix','xhor');
nhors=size(hornames,1);
nkols=size(horkols,1);
if(nkols<nhors)
	horkols=ones(nhors,1)*horkols(1,:);
end

%loop over horizons and plot
xlim=get(gca,'xlim');
del=.02*abs(diff(xlim));
xtst1=min(xlim)+del;
xtst2= max(xlim)-del;
for k=1:nhors
 if(~strcmp(hornames(k,1:2),'__'))
		hor=objget(hors,hornames(k,:));
		x=objget(hor,'inline');
		if(tflag)
			y=objget(hor,'time');
		else
			y=objget(hor,'depth');
		end

		line(x,y,'color',horkols(k,:));
		%plot the name
		ilive=find(~isnan(x));
		ind=find(x==max(x(ilive)));
		xlbl=x(ind)+del;
		if(between(xtst1,xtst2,xlbl))
			ind=find(x==min(x(ilive)));
			xlbl=x(ind)-del;
			text('position',[xlbl,y(ind)],'string',strunpad(hornames(k,:)),...
				'color',horkols(k,:),'horizontalalignment','right');
		else
			text('position',[xlbl,y(ind)],'string',strunpad(hornames(k,:)),...
				'color',horkols(k,:),'horizontalalignment','left');
		end
		
	end
end