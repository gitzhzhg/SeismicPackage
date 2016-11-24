function plotseisva(hax,smat,t,x,ampflag,clip,scaleopt,c)
% PLOTSEISVA ... Image display utility for seismic matrices
%
% plotimage(hax,smat,t,x,ampflag)
% plotimage(hax,smat,t,x)
% plotimage(hax,smat,t)
% plotimage(hax,smat)
%
% PLOTIMAGE does a quick plot of a seismic matrix in a figure window
% (made by plotimage). By default, it plots the seismic matrix in gray levels
% using the seisclrs colormap.
%
%	smat ... the seismic matrix to be plotted. Traces are assumed stored in
%       the columns smat.
%	t ... time coordinates of traces.
%       ****** default 1:nrows where nrows = number of rows in smat ****
%	x ... x coordinates of the traces
%       ****** default 1:ncols where ncols=number of columns *****
%   ampflag ... 1->independent, 2->master, 3->slave
%       ****************** default =0 *******************
% 
%
% NOTE: Scaling options, colormap, and picking are controlled by defining 8 global variables in
% your base workspace and setting their value as needed. The variables are SCALE_OPT, 
% NUMBER_OF_COLORS, GRAY_PCT, CLIP, COLOR_MAP, NOBRIGHTEN. The easiest thing to do is to put their 
% declarations in your startup.m file:
%
% global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP 
% You can also simply type this command at the matlab prompt. And then assign values to these
% variables. The default for these parameters is
% is 2, 64, 50, 4, 'seisclrs', 0, [], 'r', 0
%
% SCALE_OPT=1 ... the plotimage window begins in mean scaling mode with a clip level of 4
%         =2 ... the plotimage window begins in maximum scaling mode (no clipping).
%       Note: only SCALE_OPT=2 is a true amplitude display, but see also GRAY_PCT.
%  *************** defaults to 2 if no global is found ***************
% NUMBER_OF_COLORS ... this integer value sets the number of colors (gray levels usually)
%       Use a bigger number for more detail, but beware that it takes longer to compute.
%  *************** defaults to 64 if no global is found ***************
% GRAY_PCT=x ... Should be a number between 1 and 100. This affects how the seisclrs colomap
%       is built. When GRAY_PCT=100, you get a true linear gray scale. For a true-amplitude
%       display, you should use GRAY_PCT=100 and SCALE_OPT=2. When GRAY_PCT is say, 20, it
%       means that then first 40% of the gray levels are set to black and the last 40% to white.
%       Only in the middle 20% to you get a gradient from black to white. This accomplishes a
%       display that is visually similar to wiggle trace in that the positives tend to be all
%       black and the negatives all white. It is a kind of clipping.
%  *************** defaults to 50 if no global is found ***************
%  CLIP ... data clip level in standard deviations from the mean
%  *************** defaults to 4 if no global is found ***************
%  COLOR_MAP ... string with the name of the colormap to use
%  *************** defaults to SEISCLRS if no global is found ***************
%  NOBRIGHTEN ... 0 means the seisclrs colormap is automatically brightened
%             ... 1 means the seisclrs colormap is left as a clipped linear ramp 
%   *************** defaults to 0 if no global is found ***************
%  PICKCOLOR ... color to draw picks in. The default red works well with the seisclrs
%				colormap. Other colors may be needed for other colormaps.
%   *************** defaults to 'r' if no global is found ***************
%  PICKS ... as you make picks, they accumulate in this array. The array is 2*n-by-2 where n
%            is the number of picks. A given pick adds two rows to PICKS for the two points
%           that define the start and end of the line segment. The first column containes the
%           x (horizontal) coordinates and the second is the y (vertical) coordinates.
%           Unlike the other globals, it is not wise to assign values to PICKS. Rather, simply
%           retrieve the coordinates in PICKS by assigning them to local variables.
%  XAXISTOP ... if 1, put the x axis on top. 0 means on bottom.
%   *************** defaults to 0 if no global is found ***************
%
% There are two picking modes, Pick(O) and Pick(N). The 'O' and 'N' stand for old and new. This
% signifies that invoking Picks(N) begins a new pickset by clearing the existing PICKS matrix 
% and deleting any picks drawn on top of the plot. Picks(O) simply continues picking by adding 
% to the existing pickset. This is useful when picking is interrupted to zoom the display and
% then it is desired to resume picking.
%
% The two basic plot modes, mean and maximum scaling, are described below. Mean scaling is
% best for real data (with a large dynamic range) while maximum scaling is preferred for 
% synthetic when you want an accurate display of amplitudes.
%Mean scaling (SCALE_OPT=1)... The mean and standard deviation of all samples are computed.
%		samples>= mean+CLIP*stddev are clipped (set equal to mean+CLIP*stddev).
%		(The same is done for negative samples which are set to mean -CLIP*stddev).
%		These two extremes are mapped to the ends of the color map and all intermed
%		values are displayed linearly.
%Maximum scaling (SCALE_OPT=2) ... The maximum absolute value of the data is computed (say mxs).
%		The extremes of the colormap are then assigned to +/- mxs and all intermed
%		values are displayed linearly. There is no clipping but the display may
%		be dominated by any large values.
%
% Each plotimage window can also be set to the status of "independent", "master", or "slave". This
% refers to the method by which the maximum absolute value and standard deviations of the data
% are obtained. For both "independent" and "master" these numbers are measured from the input
% data while for the "slave" case the numbers are the same as for the most recent plotimage window
% that is declared as "master". This allows two plotimage windows to be displayed in true relative
% amplitude with respect to one another by setting one to be "master" and the other to be "slave". Note
% that if the identity of the "master" window is changed, any "slave" windows will not automatically refresh
% themselves. To do this, you must reselect the "slave" option after the new "master" window is declared.
%
% G.F. Margrave, CREWES Project, U of Calgary, 1996, 1999, and 2000
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

global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP

if(isempty(NOBRIGHTEN)) nobrighten=0; else nobrighten=NOBRIGHTEN; end
if (isempty(SCALE_OPT)) scaleopt=2; else scaleopt=SCALE_OPT; end %default to max scaling
if (isempty(NUMBER_OF_COLORS)) number_of_colors=64; 
	else number_of_colors=NUMBER_OF_COLORS; end %default to 64 gray levels
if (isempty(GRAY_PCT)) gray_pct=50; else gray_pct=GRAY_PCT; end %default to 50% gray transition
if (isempty(CLIP)) clip=4; else clip=CLIP; end %default to clip of 4
if (isempty(COLOR_MAP)|strcmp(COLOR_MAP,'seisclrs'))
	clrmap=seisclrs(number_of_colors,gray_pct);
else
	eval(['clrmap=' COLOR_MAP '(' int2str(number_of_colors) ');']);
end
if (isempty(PICKCOLOR))
	PICKCOLOR='r';
end
if (isempty(XAXISTOP))
	XAXISTOP=0;
end


gray_pct=round(gray_pct); %force an integer

if( nargin < 1 | ~isstr(smat) )
	action='init';
else
	action = smat;
end

if(strcmp(action,'init'))
	if(nargin<1)
		% do a demo
			%Make a fake reflectivity
			t=0:.002:1.0;
			r=randn(size(t)).^5;
			%make a ricker wavelet
			tw=-.1:.002:.1;
			arg=(pi*15*tw).^2;
			w=(1-2.*arg).*exp(-arg);
			%convolve
			s=conv(r,w);
			s=s(51:length(t)+50)';
			s=s/max(s); %normalize
		
			smat=s*ones(1,20);
	end
	if(nargin<4)
	  ampflag=1;
	end
	if(nargin<3)
		ncols=size(smat,2);
		x=1:ncols;
	end
	if(nargin<2)
		nrows=size(smat,1);
		t=1:nrows;
	end
	
	if(length(t)~=size(smat,1))
	    error(' length of ''time'' coordinate vector incompatible with seismic matrix');
	end
	if(length(x)~=size(smat,2))
	    error(' length of ''space'' coordinate vector incompatible with seismic matrix');
	end
	
	clips=[30 25 20 15 10 9 8 7 6 5 4 3 2 1 .5 .25 .1 .05 .01 .005 .001];
	iclip=near(clips,clip);
	clip=clips(iclip); %make sure we have a sanctioned value

	if(length(x)>1)
		bnds=(max(x)-min(x))/(length(x)+1);
	else
		bnds=max(smat-min(smat))/2;
	end

	figure;
	%

	colormap(clrmap)

	%set(gca,'ydir','reverse');

	%scale the image
	clrmap=get(gcf,'colormap');
	[nkols,m]=size(clrmap);
	
	
	    mxs=full(max(max(abs(smat))));
	    %determine clipping
	    smean=full(mean(mean(smat)));
	    stddev=full(sqrt( sum(sum((smat-smean).^2 ) )...
				/prod(size(smat))));
							
	    disp(['data maximum: ' num2str(full(mxs))])
	    disp(['data mean: ' num2str(full(smean))])
	    disp(['data stddev: ' num2str(full(stddev))])
	 smean2=smean;
	 mxs2=mxs;
	 stddev2=stddev;
	 %smean2, mxs2, and stddev2 are actually used for scaling.
	 if(ampflag==2)
	    global SMEAN STDDEV MXS
	    SMEAN=smean;
	    STDDEV=stddev;
	    MXS=mxs;
	 elseif(ampflag==3)
	    global SMEAN STDDEV MXS
	    smean2=SMEAN;
	    stddev2=STDDEV;
	    mxs2=MXS;
	  end
	    disp(['number of gray levels ' int2str(number_of_colors)])
	    disp(['Percentage of gray transition ' int2str(gray_pct)]) 
	
	if( scaleopt ==1) %mean scaling
		if(~isnan(clip))
			mxsprime=min([smean2+clip*stddev2,mxs2]);
		end
		mns=-mxsprime;
		disp(['mean scaling ']);
		disp(['data clipped outside or mean +/- '...
			num2str(clip) ' standard deviations ']);
		disp(['sigma = ' num2str(full(stddev2))])
		seis = (smat -mns)/(mxsprime-mns)*(nkols-1)+1;
		clear smat
	elseif( scaleopt==2 )
		mns=-mxs;
        %mns=0;
		disp(['max scaling ']);
		seis = (smat -mns)/(mxs-mns)*(nkols-1)+1;
		clear smat
	else
		error('invalid scaling option');
	end

	smat=[];
	
   		ix=1:length(x);
		dx=x(2)-x(1);
		dt=t(2)-t(1);
		if(dx~=(x(3)-x(2)))
			dx=0;
		end
		if(dt~=(t(3)-t(2)))
			dt=0;
		end
		
		hi=image(x,t,seis(:,ix));
		

 %install zooming
 selboxinit('plotimage(''zoom'')',1);

 set(gcf,'name','Seismic Image Plot, Simplezooming installed (Use MB1)')

%put the x axis on top
if(XAXISTOP)
	set(gca,'xaxislocation','top')
end

 %make a few buttons
 sep=.005;
 ht=.05;
 wd=.11;
 x=sep;
 hzoompick=uicontrol('style','popupmenu',...
		'string','Zoom|Pick(O)|Pick(N)',...
 	'units','normalized','tooltipstring','Define mouse action as zoom or pick',...
	'position',[x 0 wd ht],'callback','plotimage(''zoompick'')');
 x=x+wd+sep;
 wd=.18;
 hflip=uicontrol('style','popupmenu',...
		'string','Normal Polarity|Reverse Polarity',...
 	'units','normalized','tooltipstring','Set display polarity',...
	'position',[x 0 wd ht],'callback','plotimage(''flip'')',...
	'userdata',1);
 x=x+wd+sep;
 wd=.05;
 fsize=get(0,'factoryuicontrolfontsize');
 black=[0 0 0];white=[1 1 1];
 hbrighten=uicontrol('style','pushbutton','fontsize',fsize/3,'string','brt',...
 	'units','normalized','tooltipstring','Brighten the image','backgroundcolor',white,'foregroundcolor',black,...
	'position',[x 0 wd ht],'callback','brighten(.5)','visible','off');
 x=x+wd+sep;
 hdarken=uicontrol('style','pushbutton','fontsize',fsize/3,'string','drk',...
 	'units','normalized','tooltipstring','Darken the image','backgroundcolor',black,'foregroundcolor',white,...
	'position',[x 0 wd ht],'callback','brighten(-.5)','visible','off');
	
x=x-wd-sep;
        wd=.1;
        fsize=get(0,'factoryuicontrolfontsize');
        hlabel=uicontrol('style','text','fontsize',fsize/2,'string','Bright 0','units','normalized',...
        'position', [x,ht/2,wd,2*ht/3],'tooltipstring','Current brightness level','userdata',0);
        hslider=uicontrol('style','slider','string','Bright','units','normalized','position',...
            [x,0,wd,ht/2],'callback','plotimage(''brighten'')',...
            'tooltipstring','Set image brightness','max',10,'min',-10,...
            'tag','phase','value',0,'userdata',hlabel,'sliderstep',[1/20 1/20]);
            
%  wd=.1;
%  fsize=get(0,'factoryuicontrolfontsize');
%  hbrighten=uicontrol('style','pushbutton','fontsize',fsize/3,'string','brighten',...
%  	'units','normalized',...
% 	'position',[x ht/2 wd ht/2],'callback','brighten(.5)');
% 	 %x=x+wd+sep;
%  hdarken=uicontrol('style','pushbutton','fontsize',fsize/3,'string','darken',...
%  	'units','normalized',...
% 	'position',[x 0 wd ht/2],'callback','brighten(-.5)');
 x=x+wd+sep;
 wd=.1;
 hcmap=uicontrol('style','pushbutton','string','Colormap',...
 	'units','normalized',...
	'position',[x 0 wd ht],'callback','plotimage(''colormap'')',...
	'visible','off');
 %x=x+wd+sep;
 wd=.17;
 hmsg=uicontrol('style','text','string','Polarity Normal',...
 	'units','normalized',...
	'position',[x 0 wd ht],'visible','off');
 %x=x+wd+sep;
 x=x+sep;
 wd=.1;
 hmaster=uicontrol('style','popupmenu','string','Ind.|Master|Slave',...
 	'units','normalized','position',[x,0,wd,ht],'tooltipstring','Define amplitude control',...
 	'callback','plotimage(''rescale'')','value',ampflag);
 x=x+wd+sep;
 wd=.16;
 hscale=uicontrol('style','popupmenu','string',str2mat('Mean scaling',...
 	'Max scaling'),'units','normalized','position',[x,0,wd,ht],'tooltipstring','Define data scaling mechanism',...
 	'callback','plotimage(''rescale'')','value',scaleopt);
 vis='on'; if(scaleopt==2) vis='off'; end
 x=x+wd+sep;
 wd=.15;
 nclips=length(clips);
 clipmsg=num2strmat(clips);
 clipmsg=[ones(nclips,1)*'Cliplevel: ' num2strmat(clips)];  
 hclip=uicontrol('style','popupmenu','string',clipmsg,...
 	'units','normalized','position',[x,0,wd,ht],'tooltipstring','Set clip level in std deviations',...
 	'callback','plotimage(''rescale'')','value',iclip,...
 	'visible',vis);

	set(gcf,'userdata',[hflip,hbrighten,hdarken,hmsg,hi,hscale,hclip,hcmap,...
			hzoompick hmaster hlabel hslider])
	
	set(hscale,'userdata',[scaleopt mxs2 mns smean2 stddev2]);
	set(hmaster,'userdata',[mxs smean stddev]);
	set(hclip,'userdata',iclip);
	set(hmsg,'userdata',clips)

 %colorview(gca,hi,mns,mxs,0)

if(~nobrighten) brighten(.5); end

global NAME_
global NOSIG
if(isempty(NOSIG))nosig=0;else nosig=NOSIG; end
if(~nosig)
	signature(NAME_);
end

 return;
end

if(strcmp(action,'zoom'))


 h=get(gcf,'userdata');
 hi=h(5);

 box=selboxfini;
 try
    delete(box{2});
 catch
    %no selbox to delete
 end
 box = box{1};
 
 if(isempty(box)) return; end

	xmin=min([box(1) box(3)]);
	xmax=max([box(1) box(3)]);
	ymin=min([box(2) box(4)]);
	ymax=max([box(2) box(4)]);
	%get the current axis settings
	xlim=get(gca,'xlim');
	ylim=get(gca,'ylim');
	test1=xmin-xlim(1)+xmax-xlim(2)+ymin-ylim(1)+ymax-ylim(2);
	test2=(xmin-xmax)*(ymin-ymax);
	if(abs(test1)<10*eps | abs(test2)< 10*eps)
		x=get(hi,'xdata');
		y=get(hi,'ydata');
		set(gca,'xlim',[min(x) max(x)],'ylim',[min(y) max(y)]);
	else
		set(gca,'xlim',[xmin xmax],'ylim',[ymin ymax]);
	end
	return;
end

if(strcmp(action,'flip'))
 h=get(gcf,'userdata');
 hflip=h(1);
 hmsg=h(4);
 pol=get(hflip,'userdata');
 pol=-1*pol;
	clr=get(gcf,'colormap');
	colormap(flipud(clr));
	set(hflip,'userdata',pol);
	if(pol==1)
		set(hmsg,'string','Polarity Normal');
	elseif(pol==-1)
		set(hmsg,'string','Polarity Reversed');
	end
	return;
end;

if(strcmp(action,'zoompick'))
 h=get(gcf,'userdata');
 hzoompick=h(9);
 value=get(hzoompick,'value');
 switch value
 case 1 
	selboxinit('plotimage(''zoom'')',1);
	set(gcf,'name','Seismic Image Plot, Simplezooming installed (Use MB1)');
 case 2
	drawlineinit('plotimage(''pick'')',1);
	set(gcf,'name','Seismic Image Plot, Picking resummed (Use MB1)');
 case 3
	drawlineinit('plotimage(''pick'')',1);
	set(gcf,'name','Seismic Image Plot, Picking new (Use MB1)');
	PICKS=[];
   hp=get(hzoompick,'userdata');
   hkids=get(gca,'children');
   for k=1:length(hp)
		ind=find(hp(k)==hkids);
		if(~isempty(ind))
			delete(hp(k));
		end
	end
	set(hzoompick,'userdata',[]);
 end
 return;
end;

if(strcmp(action,'pick'))
 pick=drawlinefini;
 h=get(gcf,'userdata');
 hzoompick=h(9);

 if(length(pick)==5)
		if(pick(5)>0)
      	delete(pick(5));
		end
		test=pick(1)-pick(3)+pick(4)-pick(2);%zero for a double click
		if(test)
			hpick=line([pick(1) pick(3)],[pick(2) pick(4)],[1 1],'linewidth',2,...
				'color',PICKCOLOR);
			PICKS = [PICKS; pick(1) pick(2); pick(3) pick(4)];
      	hp=get(hzoompick,'userdata');
      	set(hzoompick,'userdata',[hp hpick]);
		else
			if(~isempty(PICKS))
				%make sure click was in axes
				xlim=get(gca,'xlim');
				ylim=get(gca,'ylim');
				if(between(xlim(1),xlim(2),pick(1)) & between(ylim(1),ylim(2),pick(2)) )
					PICKS(end-1:end,:)=[];
					hp=get(hzoompick,'userdata');
					if(~isempty(hp))
						delete(hp(end));
						hp(end)=[];
						set(hzoompick,'userdata',hp);
					end
				end
			end
		end
 end

 return;
end

if(strcmp(action,'colormap'))
	h=get(gcf,'userdata');
	hmsg=h(4);
	hi=h(5);
	hscale=h(6);
	hclip=h(7);
	
	%get the data
	seis=get(hi,'cdata');
	
	%determine old scaling
	dat=get(hscale,'userdata');
	oldscaleopt=dat(1);
	mxs=dat(2); mns=dat(3); smean=dat(4); stddev=dat(5);
	
	colorview(gca,hi,mns,mxs,0)
	
	return
end

if(strcmp(action,'brighten'))
   h=get(gcf,'userdata');
   hlabel=h(11);
   hslider=h(12);
   currlvl=get(hlabel,'userdata');
   newlvl=get(hslider,'value');
   if(currlvl>newlvl)
      nsteps=currlvl-newlvl;
      for k=1:nsteps
        brighten(-.25);
      end
   elseif(currlvl<newlvl)
      nsteps=newlvl-currlvl;
      for k=1:nsteps
        brighten(.25);
      end
   end
   set(hlabel,'string',['Bright ' int2str(newlvl)])
   set(hlabel,'userdata',newlvl);
   return;
end

if(strcmp(action,'rescale'))
	h=get(gcf,'userdata');
	hmsg=h(4);
	hi=h(5);
	hscale=h(6);
	hclip=h(7);
	hmaster=h(10);
	
	ampflag=get(hmaster,'value');
	stuff=get(hmaster,'userdata');
	mxs=stuff(1);
	smean=stuff(2);
	stddev=stuff(3);
	
	%get the data
	seis=get(hi,'cdata');
	
	%determine old scaling
	dat=get(hscale,'userdata');
	oldscaleopt=dat(1);
	mxsold=dat(2); mnsold=dat(3); smeanold=dat(4); stddevold=dat(5);
	%new opt
	newscaleopt=get(hscale,'value');
	dat(1)=newscaleopt;
	
	
	%get clip value
	inewclip=get(hclip,'value');
	ioldclip=get(hclip,'userdata');
	set(hclip,'userdata',inewclip);
	clips=get(hmsg,'userdata');
	clipold=clips(ioldclip);
	clipnew=clips(inewclip);
	
	%get number of columns in colormap
	clrmap=get(gcf,'colormap');
	nkols=size(clrmap,1);
	
	flag=computer;
	flag='shit';
	
	if(ampflag==1)
	   stddev2=stddev;
	   mxs2=mxs;
	   smean2=smean;
	elseif(ampflag==2)
	   global SMEAN STDDEV MXS
	   stddev2=stddev;
	   mxs2=mxs;
	   smean2=smean;
	   SMEAN=smean;
	   STDDEV=stddev;
	   MXS=mxs;
	else
	   global SMEAN STDDEV MXS
	   stddev2=STDDEV;
	   mxs2=MXS;
	   smean2=SMEAN;
	end
	
	% 		%undo the old scaling
	if( oldscaleopt == 1 ) %undo mean scaling
		mxsprime = min([smeanold+clipold*stddevold,mxsold]);
		mns=-mxsprime;
		smat = (seis-1)*(mxsprime-mns)/(nkols-1) + mns;
	elseif( oldscaleopt == 2) %undo max scaling
		mns=-mxsold;
		smat = (seis-1)*(mxsold-mns)/(nkols-1) + mns;
	end
	
	%apply new scaling
	if(newscaleopt==1)
		mxsprime = min([smean2+clipnew*stddev2,mxs2]);
		mns=-mxsprime;
		disp(['mean scaling ']);
		seis = (smat -mns)/(mxsprime-mns)*(nkols-1)+1;
		set(hclip,'visible','on');
	elseif(newscaleopt==2)
		disp(['max scaling ']);
		mns=-mxs2;
		seis = (smat -mns)/(mxs2-mns)*(nkols-1)+1;
		set(hclip,'visible','off');
	end
	
	dat(1)=newscaleopt;
	dat(2)=mxs2;
	dat(3)=mns;
	dat(4)=smean2;
	dat(5)=stddev2;
	set(hscale,'userdata',dat);
	
% 	if(strcmp(flag,'MAC2'))
% 		ntr=size(seis,2);
% 		for k=1:ntr
% 			%undo the old scaling
% 			if( oldscaleopt == 1 ) %undo mean scaling
% 				if(k==1) mxsprime = min([smean+clipold*stddev,mxs]);
% 					mns=-mxsprime;
% 				end
% 				tmp = (seis(:,k)-1)*(mxsprime-mns)/(nkols-1) + mns;
% 			elseif( oldscaleopt == 2) %undo max scaling
% 				if(k==1)	mns=-mxs; end
% 				tmp = (seis(:,k)-1)*(mxs-mns)/(nkols-1) + mns;
% 			end
% 			
% 			%apply new scaling
% 			if(newscaleopt==1)
% 				if(k==1) disp(['mean scaling ']);
% 					mxsprime = min([smean+clipnew*stddev,mxs]);
% 					mns=-mxsprime;
% 					set(hclip,'visible','on');
% 				end
% 				seis(:,k) = (tmp -mns)/(mxsprime-mns)*(nkols-1)+1;
% 			elseif(newscaleopt==2)
% 				if(k==1) disp(['max scaling ']);
% 					mns=-mxs;
% 					set(hclip,'visible','off');
% 				end
% 				seis(:,k) = (tmp -mns)/(mxs-mns)*(nkols-1)+1;
% 				
% 			end
% 		end
% 	else
% 		%undo the old scaling
% 		if( oldscaleopt == 1 ) %undo mean scaling
% 			mxsprime = min([smean+clipold*stddev,mxs]);
% 			mns=-mxsprime;
% 			smat = (seis-1)*(mxsprime-mns)/(nkols-1) + mns;
% 		elseif( oldscaleopt == 2) %undo max scaling
% 			mns=-mxs;
% 			smat = (seis-1)*(mxs-mns)/(nkols-1) + mns;
% 		end
% 		
% 		%apply new scaling
% 		if(newscaleopt==1)
% 			mxsprime = min([smean+clipnew*stddev,mxs]);
% 			mns=-mxsprime;
% 			disp(['mean scaling ']);
% 			seis = (smat -mns)/(mxsprime-mns)*(nkols-1)+1;
% 			set(hclip,'visible','on');
% 		elseif(newscaleopt==2)
% 			disp(['max scaling ']);
% 			mns=-mxs;
% 			seis = (smat -mns)/(mxs-mns)*(nkols-1)+1;
% 			set(hclip,'visible','off');
% 		end
% 	end

	
	set(hi,'cdata',seis);
	return;
end