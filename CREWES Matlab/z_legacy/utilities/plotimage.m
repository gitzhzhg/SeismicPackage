function plotimage(smat,t,x,clip,clrmap)
%
% plotimage(smat,t,x,clip,clrmap)
%
% PLOTIMAGE does a quick plot of a seismic matrix in a figure window
% (made by plotimage). It plots the the seismic matrix as a color image
% using the current color map.
%
%	smat ... the seismic matrix to be plotted. Traces are assumed stored in
%       the columns smat.
%	t ... time coordinates of traces.
%       ****** default 0:.002:(nrows-1)*.002 where nrows = number of rows in smat ****
%	x ... x coordinates of the traces
%       ****** default 1:ncols where ncols=number of columns *****
% NOTE: if only a simgle argument is provided, it is assumed to be a fleximat
%  from which seis,t,and x can be extracted.
%  clip ... data clip level in standard deviations from the mean
%   ***** default =4 ******* (enter nan for no clipping)
%  clrmap ... the colormap to use
%    ***** defaults to seisclrs(64) ******
% 
%
% NOTE: Scaling options are controlled by defining three global variables in your
% base workspace and setting their value as needed. The variables are scaleopt, 
% number_of_colors, and gray_pct. The easiest thing to do is to put their declarations in
% your startup.m file:
% global scaleopt number_of_colors gray_pct
% You can also simply type this command at the matlab prompt. The default for these values
% is 2, 64, and 20.
% scaleopt=1 ... the plotimage window begins in mean scaling mode with a clip level of 4
%         =2 ... the plotimage window begins in maximum scaling mode (no clipping).
%       Note: only scaleopt=2 is a true amplitude display, but see also gray_pct.
% number_of_colors ... this integer value sets the number of colors (gray levels usually)
%       Use a bigger number for more detail, but beware that it takes longer to compute.
% gray_pct=x ... Should be a number between 1 and 100. This affects how the seisclrs colomap
%       is built. When gray_pct=100, you get a true linear gray scale. For a ture-amplitude
%       display, you should use gray_pct=100 and scaleopt=2. When gray_pct is say, 20, it
%       means that then first 40% of the gray levels are set to black and the last 40% to white.
%       Only in the middle 20% to you get a gradient from black to white. This accomplishes a
%       display that is visually similar to wiggle trace in that the positive tend to be all
%       black and the negatives all white. It is a kind of clipping.
%
% 
% The two basic plot modes, mean and maximum scaling, are described below. Mean scaling is
% best for real data (with a large dynamic range) while maximum scaling is preferred for 
% synthetic when you want an accurate display of amplitudes.
%Mean scaling (scaleopt=1)... The mean and standard deviation of all samples are computed.
%		samples>= mean+clip*stddev are clipped (set equal to mean+clip*stddev).
%		(The same is done for negative samples which are set to mean -clip*stddev).
%		These two extremes are mapped to the ends of the color map and all intermed
%		values are displayed linearly.
%Maximum scaling (scaleopt=2) ... The maximum absolute value of the data is computed (say mxs).
%		The extremes of the colormap are then assigned to +/- mxs and all intermed
%		values are displayed linearly. There is no clipping but the display may
%		be dominated by any large values.
%
% Plotimage can automatically put a signature on your plot if you desire. To enable this option,
% define two more global variables: NOSIG and NAME_ . Set the value of NOSIG to 0 and set NAME_
% equal to a string which you wish to have on every plot.
%
% G.F. Margrave, CREWES Project, U of Calgary, 1996, and 1999
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


global scaleopt
global number_of_colors
global gray_pct

if (isempty(scaleopt)) scaleopt=2; end %default to max scaling
if (isempty(number_of_colors)) number_of_colors=64; end %default to 64 gray levels
if (isempty(gray_pct)) gray_pct=20; end %default to 20% gray transition

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
   if(nargin<2)
      [nt,nx]=size(smat);
      x=1:nx;
      t=1:nt;
		%t=fmget(smat,'y');
      %x=fmget(smat,'x');
		%smat=fmget(smat,'mat');
	end
	if(nargin<3)
		nx=size(smat,2);
		x=1:nx;
   end
   if(nargin<4)
		clip=4;
	end
	if(nargin<5)
		clrmap=seisclrs(number_of_colors,gray_pct);
	end
	
	clips=[30 25 20 15 10 9 8 7 6 5 4 3 2 1 .5 .25 .1];
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
	disp(['number of gray levels ' int2str(number_of_colors)])
	disp(['Percentage of gray transition ' int2str(gray_pct)]) 
	
	if( scaleopt ==1) %mean scaling
		if(~isnan(clip))
			mxsprime=min([smean+clip*stddev,mxs]);
		end
		mns=-mxsprime;
		disp(['mean scaling ']);
		disp(['data clipped outside or mean +/- '...
			num2str(clip) ' standard deviations ']);
		disp(['sigma = ' num2str(full(stddev))])
		seis = (smat -mns)/(mxsprime-mns)*(nkols-1)+1;
		clear smat
	elseif( scaleopt==2 )
		mns=-mxs;
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

 %make a few buttons
 sep=.005;
 ht=.05;
 wd=.25;
 hflip=uicontrol('style','popupmenu',...
		'string','Normal Polarity|Reverse Polarity',...
 	'units','normalized',...
	'position',[sep 0 wd ht],'callback','plotimage(''flip'')',...
	'userdata',1);
 x=wd+sep;
 wd=.1;
 hbrighten=uicontrol('style','pushbutton','string','Brighten',...
 	'units','normalized',...
	'position',[x 0 wd ht],'callback','brighten(.5)');
	 x=x+wd+sep;
 hdarken=uicontrol('style','pushbutton','string','Darken',...
 	'units','normalized',...
	'position',[x 0 wd ht],'callback','brighten(-.5)');
 x=x+wd+sep;
 wd=.15;
 hcmap=uicontrol('style','pushbutton','string','Colormap',...
 	'units','normalized',...
	'position',[x 0 wd ht],'callback','plotimage(''colormap'')');
 x=x+wd+sep;
 wd=.17;
 hmsg=uicontrol('style','text','string','Polarity Normal',...
 	'units','normalized',...
	'position',[x 0 wd ht],'visible','off');
 %x=x+wd+sep;
 x=x+sep;
 wd=.2;
 hscale=uicontrol('style','popupmenu','string',str2mat('Mean scaling',...
 	'Max scaling'),'units','normalized','position',[x,0,wd,ht],...
 	'callback','plotimage(''rescale'')','value',scaleopt);
 vis='on'; if(scaleopt==2) vis='off'; end
 x=x+wd+sep;
 wd=.15;
 nclips=length(clips);
 clipmsg=num2strmat(clips);
 clipmsg=[ones(nclips,1)*'Cliplevel: ' num2strmat(clips)];  
 hclip=uicontrol('style','popupmenu','string',clipmsg,...
 	'units','normalized','position',[x,0,wd,ht],...
 	'callback','plotimage(''rescale'')','value',iclip,...
 	'visible',vis);

	set(gcf,'userdata',[hflip,hbrighten,hdarken,hmsg,hi,hscale,hclip,hcmap])
	
	set(hscale,'userdata',[scaleopt mxs mns smean stddev]);
	set(hclip,'userdata',iclip);
	set(hmsg,'userdata',clips)

 %colorview(gca,hi,mns,mxs,0)

 global NAME_
 global NOSIG
 if(~NOSIG)
 	signature(NAME_);
 end

 return;
end

if(strcmp(action,'zoom'))
 box=selboxfini;

 h=get(gcf,'userdata');
 hi=h(5);
 if(length(box)==5)
		delete(box(5));
	end

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

if(strcmp(action,'rescale'))
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
	%new opt
	newscaleopt=get(hscale,'value');
	dat(1)=newscaleopt;
	set(hscale,'userdata',dat);
	
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
	
	if(strcmp(flag,'MAC2'))
		ntr=size(seis,2);
		for k=1:ntr
			%undo the old scaling
			if( oldscaleopt == 1 ) %undo mean scaling
				if(k==1) mxsprime = min([smean+clipold*stddev,mxs]);
					mns=-mxsprime;
				end
				tmp = (seis(:,k)-1)*(mxsprime-mns)/(nkols-1) + mns;
			elseif( oldscaleopt == 2) %undo max scaling
				if(k==1)	mns=-mxs; end
				tmp = (seis(:,k)-1)*(mxs-mns)/(nkols-1) + mns;
			end
			
			%apply new scaling
			if(newscaleopt==1)
				if(k==1) disp(['mean scaling ']);
					mxsprime = min([smean+clipnew*stddev,mxs]);
					mns=-mxsprime;
					set(hclip,'visible','on');
				end
				seis(:,k) = (tmp -mns)/(mxsprime-mns)*(nkols-1)+1;
			elseif(newscaleopt==2)
				if(k==1) disp(['max scaling ']);
					mns=-mxs;
					set(hclip,'visible','off');
				end
				seis(:,k) = (tmp -mns)/(mxs-mns)*(nkols-1)+1;
				
			end
		end
	else
		%undo the old scaling
		if( oldscaleopt == 1 ) %undo mean scaling
			mxsprime = min([smean+clipold*stddev,mxs]);
			mns=-mxsprime;
			smat = (seis-1)*(mxsprime-mns)/(nkols-1) + mns;
		elseif( oldscaleopt == 2) %undo max scaling
			mns=-mxs;
			smat = (seis-1)*(mxs-mns)/(nkols-1) + mns;
		end
		
		%apply new scaling
		if(newscaleopt==1)
			mxsprime = min([smean+clipnew*stddev,mxs]);
			mns=-mxsprime;
			disp(['mean scaling ']);
			seis = (smat -mns)/(mxsprime-mns)*(nkols-1)+1;
			set(hclip,'visible','on');
		elseif(newscaleopt==2)
			disp(['max scaling ']);
			mns=-mxs;
			seis = (smat -mns)/(mxs-mns)*(nkols-1)+1;
			set(hclip,'visible','off');
		end
	end

	
	set(hi,'cdata',seis);
	return;
end
	
		
		
		
	