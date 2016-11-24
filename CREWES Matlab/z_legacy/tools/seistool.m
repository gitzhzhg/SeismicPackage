function seistool(arg1,plotmode,xwells,wnames,wlet,tw,tit,...
			hhors,kolor,xsc,ysc,datumname,wletname)
% seistool(fmseis,xwells,wnames,wlet,tw,tit,hhors,kolor,xsc,ysc,...
%		datumname,wletname)
%
% fmseis ... fleximat containing the seismic section
% plotmode ... 1=matrix plot; 2=wtva; 3=wt
%       ***** default = 1 **********
% xwells ... vector of x coordinates of the wells
% wnames ... string matrix of well names
% wlet ... wavelet
% tw ... time coordinate for the wavelet
% tit ... figure title
% hhors ... vector of horizon handles in time window
% kolor ... color to plot the seismic in
% xsc ... initial x scale for hardcopy
% ysc ... initial y scale for hardcopy
% datumname ... name of the datum
% wletname ... waveletname
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
if(nargin<1)
	arg1=[];
end
if(~isstr(arg1))
	action='init';
	fmseis=arg1;
	clear arg1
else
	action=arg1;
end
if(nargin<2)
	plotmode=1;
end
if(nargin<3)
	xwells=[];
end
if(nargin<4)
	wnames=[];
end
if(nargin<5)
	wlet=[];
end
if(nargin<6)
	tw=[];
end
if(nargin<7)
	tit='';
end
if(nargin<8)
	hhors=[];
end
if(nargin<9)
	kolor=[0 0 0];
end
if(nargin<10)
	xsc=500;
end
if(nargin<11)
	ysc=.1;
end
if(nargin<12)
	datumname = 'Datum';
end
if(nargin<13)
	wletname='';
end
if(strcmp(action,'init'))
	%add the wavelet to the fleximat
	%if(~isempty(wlet))
	%	x=fmget(fmseis,'x');
	%	t=fmget(fmseis,'y');
	%	t=t(1:length(wlet));
	%	delx=median(diff(x));
	%	xm=max(x)+delx;
	%	fmseis=fmset(fmseis,xm,t,wlet);
	%end
	
	%make a figure and an axis
	
	hfig=figcent(.7,.66);
	set(gcf,'menubar','none');
 pos=get(gcf,'position');
 posit=pos;
 if(isempty(hhors))
		pos=[.1 .1 .75 .8];
	else
		pos=[.1 .1 .8 .8];
	end
	hax=axes('position',pos);
	set(hfig,'pointer','watch');
   %make an options menu
 hopt=uimenu(hfig,'label','Options');
 
 %fileio
 hreadsegy=uimenu(hopt,'label','Read SEGY','callback',...
 		'seistool(''readsegy'')');
 hreadobj=uimenu(hopt,'label','Read MATLAB','callback',...
 		'seistool(''readobj'')','enable','off');
 hwritesegy=uimenu(hopt,'label','Write SEGY','callback',...
 		'seistool(''writesegy'')');
 hwriteobj=uimenu(hopt,'label','Write MATLAB','callback',...
 		'seistool(''writeobj'')','enable','off');
   
   hhard=uimenu(hopt,'label','Hard Copy',...
      'callback','seistool(''hardcopy'')');
   hclose=uimenu(hopt,'label','Close',...
      'callback','seistool(''close'')');
      
   % the view menu
   hview=uimenu(gcf,'label','View');
   hzoom=uimenu(hview,'Label','Zoom','callback',...
   	'seistool(''zoom'')');
   hunzoom=uimenu(hview,'label','unZoom','callback',...
   	'seistool(''unzoom'')');
   hplotmode=uimenu(hview,'label','Plotmode','userdata',plotmode);
	  if(plotmode==1) chk='on'; else chk='off'; end
   hmatrix=uimenu(hplotmode,'label','Matrix','callback',...
   	'seistool(''matrix'')','checked',chk);
	  if(plotmode==4) chk='on'; else chk='off'; end
   hmatrixwt=uimenu(hplotmode,'label','Matrix/WT','callback',...
   	'seistool(''matrixwt'')','checked',chk);
	  if(plotmode==2) chk='on'; else chk='off'; end
   hwtva=uimenu(hplotmode,'label','WTVA','callback',...
   	'seistool(''wtva'')','checked',chk);
	  if(plotmode==3) chk='on'; else chk='off'; end
   hwt=uimenu(hplotmode,'label','WT','callback',...
   	'seistool(''wt'')','checked',chk);
   hwlet=uimenu(hview,'label','Hide Wavelet','callback',...
   	'seistool(''togglewlet'')');
   if(isempty(wlet)) set(hwlet,'visible','off'); 
   else set(hwlet,'userdata',[wlet(:) tw(:)]); 
   end
    
 hamp=uimenu(hview,'label','Trace Amplitude Factor');
  hamp1 = uimenu(hamp,'label','.5','callback','seistool(''tramp'')',...
      'userdata',.5,'checked','off');
  hamp2 = uimenu(hamp,'label','1.0','callback','seistool(''tramp'')',...
      'userdata',1.0,'checked','off');
  hamp3 = uimenu(hamp,'label','2.0','callback','seistool(''tramp'')',...
      'userdata',2.0,'checked','on');
  hamp4 = uimenu(hamp,'label','4.0','callback','seistool(''tramp'')',...
      'userdata',4.0,'checked','off');
  hamp5 = uimenu(hamp,'label','8.0','callback','seistool(''tramp'')',...
      'userdata',8.0,'checked','off');
  hamp6 = uimenu(hamp,'label','12.0','callback','seistool(''tramp'')',...
      'userdata',12.0,'checked','off');
  hamp7 = uimenu(hamp,'label','18.0','callback','seistool(''tramp'')',...
      'userdata',18.0,'checked','off');
  hamp8 = uimenu(hamp,'label','24.0','callback','seistool(''tramp'')',...
      'userdata',24.0,'checked','off');
  hamp9 = uimenu(hamp,'label','32.0','callback','seistool(''tramp'')',...
      'userdata',32.0,'checked','off');
 set(hamp,'userdata',[2 hamp1 hamp2 hamp3 hamp4 hamp5 hamp6 hamp7...
		hamp8 hamp9]);
	hcolors=uimenu(hview,'label','Colors','callback',...
		'seistool(''colors'')');
	hmatinterp=uimenu(hview,'label','Matrix Interpolation Factor');
	hint1=uimenu(hmatinterp,'label','1','callback','seistool(''matint'')',...
		'userdata',1,'checked','on');
	hint2=uimenu(hmatinterp,'label','2','callback','seistool(''matint'')',...
		'userdata',2,'checked','off');
	hint3=uimenu(hmatinterp,'label','4','callback','seistool(''matint'')',...
		'userdata',4,'checked','off');
	hint4=uimenu(hmatinterp,'label','8','callback','seistool(''matint'')',...
		'userdata',8,'checked','off');
	hint5=uimenu(hmatinterp,'label','12','callback','seistool(''matint'')',...
		'userdata',12,'checked','off');
	hint6=uimenu(hmatinterp,'label','18','callback','seistool(''matint'')',...
		'userdata',18,'checked','off');
	hint7=uimenu(hmatinterp,'label','24','callback','seistool(''matint'')',...
		'userdata',24,'checked','off');
	set(hmatinterp,'userdata',[1 hint1 hint2 hint3 hint4 hint5 hint6 hint7]);
		
% the actions menu
	hactions=uimenu(gcf,'label','Actions','enable','off',...
		'visible','off');
	hdraw=uimenu(hactions,'label','Draw New Event',...
		'callback','seistool(''drawnew'')','enable','off');
	hsnap=uimenu(hactions,'label','Snap to','enable','on');
	hsnappeak=uimenu(hsnap,'label','Peak','callback',...
		'seistool(''snappeak'')','enable','off');
	hsnaptrof=uimenu(hsnap,'label','Trough','callback',...
		'seistool(''snaptrof'')','enable','off');
	hsnappmzc=uimenu(hsnap,'label','+ -> - Zero Xing',...
		'callback','seistool(''snappmzc'')','enable','off');
	hsnapmpzc=uimenu(hsnap,'label','- -> + Zero Xing',...
		'callback','seistool(''snapmpzc'')','enable','off');
	
	hmodify=uimenu(hactions,'label','Modify Events',...
		'callback','seistool(''modify'')','enable','off');
		
	hextract=uimenu(hactions,'label','Extract Attributes',...
		'callback','seistool(''extract'')','enable','off');
   
    % Display Options ContextMenu
   hdisp=uimenu('parent',gcf, 'Label','Display Options');
   hcmenu=uicontextmenu;
   menuss=[hdisp, hcmenu];
   for k=1:2
       menus=menuss(k);
   uimenu('parent',menus, 'Label','Increase Font Size','Separator', 'off',...
       'Callback','bigfont(gcf,1.25,1)');
   uimenu('parent',menus, 'Label','Decrease Font Size','Separator', 'off',...
       'Callback','bigfont(gcf,(1/1.25),1)');
   uimenu('parent',menus, 'Label','Bold Font','Separator', 'off',...
       'Callback','bigfont(gcf,1,2)');
   uimenu('parent',menus, 'Label','Normal Font','Separator', 'off',...
       'Callback','bigfont(gcf,1,1)');
    uimenu('parent',menus, 'Label','Increase Line Weight','Separator', 'on',...
       'Callback','boldlines(gcf,2,1)');
   uimenu('parent',menus, 'Label','Decrease Line Weight','Separator', 'off',...
       'Callback','boldlines(gcf,(1/2),1)');
   uimenu('parent',menus, 'Label','White Background','Separator', 'on',...
       'Callback','whitefig');
   uimenu('parent',menus, 'Label','Grey Background','Separator', 'off',...
       'Callback','greyfig');
   uimenu('parent',menus, 'Label','Large Figure','Separator', 'off',...
       'Callback','bigfig');
   uimenu('parent',menus, 'Label','Small Figure','Separator', 'off',...
       'Callback','smallfig(gcf,get(gcbo,''Userdata''))','Userdata',posit);
   uimenu('parent',menus, 'Label','All Display Options ON','Separator', 'on',...
       'Callback','bigfig(gcf);boldlines(gcf,4,2);bigfont(gcf,2,2);whitefig');
   uimenu('parent',menus, 'Label','All Display Options OFF','Separator', 'off',...
       'Callback',['smallfig(gcf,get(gcbo,''Userdata''));boldlines(gcf,.25,.5);',...
       'bigfont(gcf,.5,1);greyfig'],'Userdata',posit);
   uimenu('parent',menus, 'Label','Copy Figure to Clipboard','Separator', 'on',...
       'Callback','print(gcf,''-dbitmap'')');
   end
   set(gcf,'uicontextmenu',hcmenu)
   %make some uicontrols
   sep=1;
   xnow=sep;
   ynow=sep;
   width=50;
	height=20;
	%some scroll buttons initially invisible
	  hright=uicontrol('style','pushbutton','string','Right',...
		'callback','seistool(''scroll'')',...
		'position',[xnow,ynow,width,height],'visible','off');
	  ynow=ynow+height+sep;
	  hleft=uicontrol('style','pushbutton','string','Left',...
		'callback','seistool(''scroll'')',...
		'position',[xnow,ynow,width,height],'visible','off');
	  ynow=ynow+height+sep;
	  hdown=uicontrol('style','pushbutton','string','Down',...
		'callback','seistool(''scroll'')',...
		'position',[xnow,ynow,width,height],'visible','off');
	  ynow=ynow+height+sep;
	  hup=uicontrol('style','pushbutton','string','Up',...
		'callback','seistool(''scroll'')',...
		'position',[xnow,ynow,width,height],'visible','off');
	  
   xnow=xnow+width+sep;
	  ynow=sep;
   width=700;
   hmsg=uicontrol('style','text','string','',...
      'position',[xnow ynow width height]);
		
	hstor1=uicontrol('style','text','string','','visible','off');
	hstor2=uicontrol('style','text','string','','visible','off');
	hstor3=uicontrol('style','text','string','','visible','off');
	hstor4=uicontrol('style','text','string','','visible','off');
	hstor5=uicontrol('style','text','string','','visible','off');
		
    
    
    
	%save the userdata
	
set(gcf,'userdata',[hclose,hhard,hmsg,hstor1,hstor2,hstor3,hstor4,...
      hstor5,hopt,hamp,hup,hdown,hleft,hright...
			hreadsegy hreadobj hwritesegy hwriteobj hview hzoom hunzoom...
			hplotmode hmatrix hwtva hwt hactions hdraw hsnap...
			hsnappeak hsnaptrof hsnappmzc hsnapmpzc hmodify hextract hcolors...
			hwlet hmatrixwt hmatinterp]);
	% 
	% user data assignments
	%  hclose h(1) ... the seismic matrix
	%  hhard h(2) ... xsc ysc (the latest hard copy scales
	%  hmsg h(3) ... xwells = the x coordinates of any wells to be posted
	%  hstor1 h(4) ... string matrix of well names to go with xwells
	%  hstor2 h(5) ... kolor = color to plot the sonic
	%  hstor3 h(6) ... hhors = handle of horizons to be copied as events
	%  hstor4 h(7) ... handles of text labels that need to be 
	%						reposit. after zooming
	%  hstor5 h(8) ... the wavelet name
	%  hopt h(9) ... the handle of the main plot axes
	%  hamp h(10) ... current amp factor and vector of submenus
	%  hup h(11) ... the initial axes limits
	%  hdown h(12) ... the seismic x coordinates
	%  hleft h(13) ... the seismic t coordinates
	%  hright h(14) ... min and max data values
	%  hreadsegy h(15) ... geometry matrix: [xm;xs;ys;xr;yr;offs;cdps]
	%  hreadobj h(16) ... geometry flags for plotting
	%  hwritesegy h(17) ... the line name if provided
	%  hwriteobj h(18) ... [ntr nrec] = number of traces per record 
	%			and number of records
	%  hview h(19) ... [haxw hw1 hw2] handle of the wavelet axes and
	%			handles of the wavlet trace and va fill
	%  hzoom h(20) ... used during zooming
	%  hunzoom h(21) ... vector of handles of all the non-seismic stuff
	%			in the primary axes
	%  hplotmode h(22) ... integer denoting the plotmode
	%  hmatrix h(23) ... handle of the image plot
	%  hwtva h(24) ... vector of handles of wiggle traces and their VA fills
	%  hwt h(25) ... handle of the color axes
	%  hactions h(26) ... not used
	%  hdraw h(27) ... not used
	%  hsnap h(28) ... not used
	%  hsnappeak h(29) ... not used
	%  hsnaptrof h(30) ... not used
	%  hsnappmzc h(31) ... not used
	%  hsnapmpzc h(32) ... not used
	%  hmodify h(33) ... not used
	%  hextract h(34) ... not used
	%  hcolors h(35) ... handles of the color figure
	%  hwlet h(36) ... [wlet tw]
	%  hmatrixwt h(37) ... not used
	%  hmatinterp h(38) ... matrix interpolation factor and vector of submenus
	%unpack the fleximat
	seis=fmget(fmseis,'mat');
	x=fmget(fmseis,'x');
	[x,ix]=sort(x);
	if( ~isempty(ix) )
	  	test=any(diff(ix)~=1);
		if(test)
			seis=seis(:,ix);
		end
	end
	t=fmget(fmseis,'y');
	fmseis=[];
	
	set(hclose,'userdata',seis);
	set(hdown,'userdata',x);
	set(hleft,'userdata',t);
	set(hhard,'userdata',[xsc ysc]);
	set(hmsg,'userdata',xwells);
	set(hstor1,'userdata',wnames);
	set(hstor2,'userdata',kolor);
	set(hstor3,'userdata',hhors);
	set(hopt,'userdata',gca);
	% hstor4 is used for the handles of text labels that need reposit. after
	% a zoom
	set(hstor5,'userdata',wletname);
% hopt is unused
 % hamp is used for the current amp factor and vector of submenus
 % hup contains the initial axes limits
	set(gcf,'name',tit);
	
	%plot
	colormap('jet');
	seistool('plot');
	
	if(~isempty(wlet))
		seistool('plotwlet');
	end
	
	whitefig;
	set(hfig,'pointer','arrow');
	return;
end
%plot things
if( strcmp(action,'plot'))
	h=get(gcf,'userdata');
	hclose=h(1);
	hmsg=h(3);
	hstor1=h(4);
	hstor2=h(5);
	hstor3=h(6);
	hstor4=h(7);
	hstor5=h(8);
	hopt=h(9);
	hamp=h(10);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hview=h(19);
	hplotmode=h(22);
	hunzoom=h(21);
	hmatrix=h(23);
	hwtva=h(24);
	hwt=h(25);
	hcolors=h(35);
	hwlet=h(36);
	hmatinterp=h(38);
 
 set(gcf,'pointer','watch');
 hax=get(hopt,'userdata');
 set(gcf,'currentaxes',hax);
 %holdstat=get(gcf,'nextplot');
 %if(~strcmp(holdstat,'add'))
	 cla;
	%end
	
	seis=get(hclose,'userdata');
	x=get(hdown,'userdata');
	t=get(hleft,'userdata');
	xwells=get(hmsg,'userdata');
	wellnames=get(hstor1,'userdata');
	kolor=get(hstor2,'userdata');
 
 %get the amp factor
 ampdat=get(hamp,'userdata');
	
	%plot the seismic
	plotmode=get(hplotmode,'userdata');
	sex=get(hright,'userdata');
	if(isempty(sex))
		ilive=find(~isnan(seis));
		mxs=max(seis(ilive));
		mns=min(seis(ilive));
		set(hright,'userdata',[mns mxs]);
	else
		mns=sex(1);
		mxs=sex(2);
	end
	% see if we are in zoom mode
	if(strcmp(get(hup,'visible'),'on'))
		xlim=get(hax,'xlim');
		ylim=get(hax,'ylim');
	else
		xlim=[];
	end
	if( plotmode==1 | plotmode==4 )
		% scale the image
		clrmap=get(gcf,'colormap');
		[nkols,m]=size(clrmap);
		seis=(seis-mns)/(mxs-mns)*(nkols-1) +1;
		%see if the colorfigure exists and use its scaling
		hcolorfig=get(hcolors,'userdata');
		cupdate=0;
		if( ~isempty(hcolorfig) )
		   if( colorview('iscolor',hcolorfig) )
			vals=colorview('getvalues',hcolorfig);
			seis=(seis-vals(1))/(vals(2)-vals(1))*(nkols-1) +1;
			cupdate=1;
		   end
		end
		%interpolate new columns if requested
		matint=get(hmatinterp,'userdata');
		nint=matint(1)-1;
		if(nint>0)
		  [nr,nc]=size(seis);
		  seisold=seis;
		  seis=zeros(nr,nc+(nc-1)*nint);
		  xold=x;
		  x=zeros(1,nc+(nc-1)*nint);
		  for k=1:length(x)-1
			m1=ceil(k/(nint+1));
			m2=m1+1;
			kinc=rem(k-1,nint+1);
			x(k)=xold(m1)+(xold(m2)-xold(m1))*kinc/(nint+1);
		  end
		  x(length(x))=xold(length(xold));
		  seis=(interp1(xold,seisold.',x).');
		end
		hi=image(x,t,seis);
		if(~isempty(xlim))
			set(hax,'xlim',xlim,'ylim',ylim);
		end
		set(hamp,'enable','off');
		set(hcolors,'enable','on');
		set(hmatrix,'userdata',hi);
		if(cupdate)
			colorview('refresh',hi,hcolorfig);
		end
	end
	if( plotmode==2 | plotmode==3 | plotmode==4 )
		if(plotmode==4) %unscale the seismic
			seis=get(hclose,'userdata');
			x=get(hdown,'userdata');
			t=get(hleft,'userdata');
		end
		bnds=(max(x)-min(x))/(length(x)+1);
		s=max([abs(mxs) abs(mns)]);
		if(plotmode==2 | plotmode==4)
			htrcs=zeros(1,2*length(x));
		else
			htrcs=zeros(1,length(x));
		end
		for k=1:length(x)
			trc=seis(:,k)/s;
			ilive2=find(~isnan(trc));
			%m=mean(trc(ilive2));
			m=0;
			trc=(trc-m)*bnds*ampdat(1)+x(k);
			if( plotmode==2)
				[h1,h2]=wtva(trc(ilive2),t(ilive2),kolor,x(k),1,1);
				htrcs(2*k-1:2*k)=[h1 h2];
			elseif( plotmode == 3 | plotmode== 4)
				htrcs(k)=line('xdata',trc(ilive2),'ydata',t(ilive2)...
				,'zdata',ones(size(ilive2)),'color',kolor);
			end
		end
		set(gca,'ydir','reverse');
		set(hwtva,'userdata',htrcs);
		%if(strcmp(get(hup,'visible'),'off'))
		%	axis('auto');
		%end
		%set(hup,'userdata',[]);
		set(hamp,'enable','on');
		if(plotmode~=4)
			set(hcolors,'enable','off');
			%hide color figure if needed
			hcolorfig=get(hcolors,'userdata');
			if( ~isempty(hcolorfig) )
			   if(colorview('iscolor',hcolorfig))
				set(hcolorfig,'visible','off');
			   end
			end   
			%delete coloraxes
			hcax=get(hwt,'userdata');
			if(~isempty(hcax))
				colorview('deletecolor',hcax);
				set(hwt,'userdata',[]);
			end
		end
	end
	
	%plot the wells
	if(isempty(get(hup,'userdata')))
		ylim=get(gca,'ylim');
	else
		lims=get(hup,'userdata');
		ylim=lims(3:4);
	end
	
	htext=zeros(size(xwells));
	hstuff=zeros(1,2*length(xwells));
	
	for k=1:length(xwells)
		xw=[xwells(k) xwells(k)];
		yw=[t(1) max(ylim)];
		
		hstuff(2*k-1)=line('xdata',xw,'ydata',yw,...
				'zdata',ones(size(xw)),...
				'color','w','linestyle','-.');
            ttle=wellnames(k,:);
            ttle((ttle=='_'))='-';
		htext(k)=text('position',[xw(1) yw(1) 1],...
			'rotation',45,...
			'string',ttle,...
			'fontsize',10);
		hstuff(2*k)=htext(k);
	end
	set(hstor4,'userdata',htext);
 knot=length(hstuff);
 hstuff=[hstuff zeros(1,2*length(hhors))];
 hhors=get(hstor3,'userdata');
	%plot the horizons
	for k=1:length(hhors)
		xh=get(hhors(k),'xdata');
		yh=get(hhors(k),'ydata');
		kh=get(hhors(k),'color');
		name=get(hhors(k),'userdata');
		ind=find(name==':');
		name=name(ind+2:length(name));
		name=strunpad(name);
		if(~strcmp(name(1:2),'__'))
			ttle=name;
            ttle((ttle=='_'))='-';
            hstuff(knot+2*k-1)=line('xdata',xh,'ydata',yh,...
				'zdata',ones(size(xh)),'color',kh,'linewidth',3);
			ilive=find(~isnan(xh));
			xmax=max(xh(ilive));
			ylbl=yh(find(xh==xmax));
			hstuff(knot+2*k)=text('position',[xmax ylbl(1) 1],...
				'string',ttle,...
        		'color',kh,'fontsize',8);
        end
	end
	if(~isempty(hstuff))
		ind=find(hstuff==0);
		hstuff(ind)=[];
		set(hunzoom,'userdata',hstuff);
	end
 
 %save the axes limits if needed
 if(isempty(get(hup,'userdata')))
		xlim=get(gca,'xlim');
		ylim=get(gca,'ylim');
		set(hup,'userdata',[xlim ylim]);
 end
	set(gca,'ygrid','on','xgrid','on');
    tit=get(gcf,'name');
    ttle=tit;
    ttle((ttle=='_'))='-';
    xlabel(ttle);
    
    ylabel(['Time (sec) from ' datumname]);
    
    set(gcf,'pointer','arrow');
    
    view(2);
	whitefig;
	
	return;
end
%plot the wavelet in its own axes
if(strcmp(action,'plotwlet'))
	h=get(gcf,'userdata');
	hclose=h(1);
	hmsg=h(3);
	hstor1=h(4);
	hstor2=h(5);
	hstor3=h(6);
	hstor4=h(7);
	hstor5=h(8);
	hopt=h(9);
	hamp=h(10);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hview=h(19);
	hplotmode=h(22);
	hmatrix=h(23);
	hcolors=h(35);
	hwlet=h(36);
	
	dat=get(hwlet,'userdata');
	w=dat(:,1);
	tw=dat(:,2);
 
 	set(gcf,'pointer','watch');
 	haxmain=get(hopt,'userdata');
 	hax=get(hview,'userdata');
 	if(isempty(hax))
 		%determine position and size
 		t=get(hleft,'userdata');
 		pos=get(haxmain,'position');
 		wmain=pos(3);
 		hmain=pos(4);
 		sep=.15;
 		wthis=.1;
 		wmain=wmain-wthis-sep;
 		hthis=length(tw)*hmain/length(t);
 		posthis=zeros(1,4);
 		posthis(1)=pos(1)+wmain+sep;
 		posthis(2)=pos(2)+hmain-hthis;
 		posthis(3)=wthis;
 		posthis(4)=hthis;
 		
 		hax=axes('position',posthis);
 		set(haxmain,'position',[pos(1:2) wmain hmain],'box','on',...
			'ygrid','on');
 		
 		set(gcf,'currentaxes',hax);
 	end
 	
 	%draw the wavelet
 		
 	set(gcf,'currentaxes',hax);
 	kolor=get(hstor2,'userdata');
 	[hw1,hw2]=wtva(w,tw,kolor,0,1,1);
		set(hview,'userdata',[hax hw1 hw2]);
 	%label the wavelet
 	wletname=get(hstor5,'userdata');
 	xlabel(wletname);flipy;
		title('Wavelet');
		set(gcf,'currentaxes',haxmain);
 	
 	return;
 end
%
% toggle the wavelet on and off
%
if(strcmp(action,'togglewlet'))
	h=get(gcf,'userdata');
	hstor5=h(8);
	hopt=h(9);
	hview=h(19);
	hax=get(hopt,'userdata');
	hws=get(hview,'userdata');
	haxw=hws(1);
	hw1=hws(2);
	hw2=hws(3);
	hmenu=gcbo;
	pos=get(hax,'position');
	posw=get(haxw,'position');
	flag=get(haxw,'visible');
	if(strcmp(flag,'on')) %then hide it
		sep=.15;
		pos(3)=pos(3)+sep+posw(3);
		set(hax,'position',pos);
		set(haxw,'visible','off');
		set(hw1,'visible','off');
		set(hw2,'visible','off');
		set(hmenu,'label','Show Wavelet');
		htit=get(haxw,'title');
		set(htit,'visible','off');
		delete(get(haxw,'xlabel'));
	else
		sep=.15;
		pos(3)=pos(3)-sep-posw(3);
		set(hax,'position',pos);
		set(haxw,'visible','on');
		set(hw1,'visible','on');
		set(hw2,'visible','on');
		set(hmenu,'label','Hide Wavelet');
		htit=get(haxw,'title');
		set(htit,'visible','on');
		name=get(hstor5,'userdata');
		set(gcf,'currentaxes',haxw);
		set(haxw,'xlabel',text(0,0,name));
		set(gcf,'currentaxes',hax);
	end
	return;
end
		
%close the figure
if(strcmp(action,'close'))
	h=get(gcf,'userdata');
	hcolors=h(35);
	%see if the color widget exists
	hcolorfig=get(hcolors,'userdata');
	if(~isempty(hcolorfig))
			close(hcolorfig);
	end
	close(gcf);
	return;
end
%
% change the plotmode
%
if( strcmp(action,'matrix') )
	h=get(gcf,'userdata');
	hplotmode=h(22);
	hmatrix=h(23);
	hwtva=h(24);
	hwt=h(25);
	hmatrixwt=h(37);
	set(hmatrix,'checked','on');
	set(hmatrixwt,'checked','off');
	set(hwtva,'checked','off');
	set(hwt,'checked','off');
	set(hplotmode,'userdata',1);
	seistool('plot');
	return;
end
if( strcmp(action,'matrixwt') )
	h=get(gcf,'userdata');
	hplotmode=h(22);
	hmatrix=h(23);
	hwtva=h(24);
	hwt=h(25);
	hmatrixwt=h(37);
	set(hmatrix,'checked','off');
	set(hmatrixwt,'checked','on');
	set(hwtva,'checked','off');
	set(hwt,'checked','off');
	set(hplotmode,'userdata',4);
	seistool('plot');
	return;
end
if( strcmp(action,'wtva') )
	h=get(gcf,'userdata');
	hplotmode=h(22);
	hmatrix=h(23);
	hwtva=h(24);
	hwt=h(25);
	hmatrixwt=h(37);
	set(hmatrix,'checked','off');
	set(hmatrixwt,'checked','off');
	set(hwtva,'checked','on');
	set(hwt,'checked','off');
	set(hplotmode,'userdata',2);
	seistool('plot');
	return;
end
if( strcmp(action,'wt') )
	h=get(gcf,'userdata');
	hplotmode=h(22);
	hmatrix=h(23);
	hwtva=h(24);
	hwt=h(25);
	hmatrixwt=h(37);
	set(hmatrix,'checked','off');
	set(hmatrixwt,'checked','off');
	set(hwtva,'checked','off');
	set(hwt,'checked','on');
	set(hplotmode,'userdata',3);
	seistool('plot');
	return;
end
% hardcopy
if(strcmp(action,'hardcopy') )
   h=get(gcf,'userdata');
   hhard=h(2);
   
   scales=get(hhard,'userdata');
 
   shardcopy(gcf,'seistool(''hardcopy2'')',scales(1),scales(2),...
      'temp.ps');
   return;
end
 
if(strcmp(action,'hardcopy2') )
   h=get(gcf,'userdata');
   hhard=h(2);
   hmessage = h(3);
   
   % get the dialog answers and test for reasonableness
   [ps_scale,xlength,ylength,xscale,yscale]=shardcopyfini;
 
   if( ps_scale== -999.) %test for a cancel
      set(hmessage,'string','Plot cancelled');
      return;
   end
   
%put out a message
set(hmessage,'string',...
      ['plotsize is ' num2str(xlength) ' by ' num2str(ylength) ...
      ' inches. use ps_scale = ' num2str(ps_scale) ' in CHVSUB']);
      
   %remember the scales
   set(hhard,'userdata',[xscale yscale]);
   
   return
end
%hardcopy
if(strcmp(action,'hardcopy'))
	h=get(gcf,'userdata');
	hhard=h(2);
	hmsg=h(3);
	%$$$$
	return;
end
%
%
%
if( strcmp(action,'zoom') )
	h=get(gcf,'userdata');
	hmsg=h(3);
	hopt=h(9);
	hzoom=h(20);
	set(hmsg,'string','Drag zoom box with left button');
	hax=get(hopt,'userdata');
	set(gcf,'currentaxes',hax);
	dat=get(hax,'userdata');
	fcn1=get(gcf,'windowbuttondownfcn');
	fcn2=get(gcf,'windowbuttonmotionfcn');
	fcn3=get(gcf,'windowbuttonupfcn');
	[m,n]=size(dat);
	stuff=[ m n dat(:)' nan abs(fcn1) nan abs(fcn2) nan abs(fcn3)];
	set(hzoom,'userdata',stuff);
	set(hax,'userdata',[]);
	selboxinit('seistool(''zoom2'')');
	return;
end
if( strcmp(action,'zoom2') )
	h=get(gcf,'userdata');
	hmsg=h(3);
	hopt=h(9);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hzoom=h(20);
	hunzoom=h(21);
	hmatrix=h(23);
 hax=get(hopt,'userdata');
	stuff=get(hzoom,'userdata');
	ind=find(isnan(stuff));
	dat=stuff(3:ind(1)-1);
	dat=reshape(dat',abs(stuff(1)),abs(stuff(2)));
	fcn1=setstr(stuff(ind(1)+1:ind(2)-1));
	fcn2=setstr(stuff(ind(2)+1:ind(3)-1));
	fcn3=setstr(stuff(ind(3)+1:length(stuff)));
	set(gcf,'windowbuttondownfcn',fcn1,'windowbuttonmotionfcn',fcn2,...
	  'windowbuttonupfcn',fcn3);
	
    box=selboxfini;
    try
       delete(box{2});
    catch
       %no selbox to delete
    end
    box = box{1};
    
	xmin=min([box(1) box(3)]);
	xmax=max([box(1) box(3)]);
	ymin=min([box(2) box(4)]);
	ymax=max([box(2) box(4)]);
	set(hax,'xlim',[xmin xmax],'ylim',[ymin ymax]);
	set(hax,'userdata',dat);
	set(hmsg,'string','<<-- Note scroll buttons at left');
	set(hup,'visible','on');
	set(hdown,'visible','on');
	set(hleft,'visible','on');
	set(hright,'visible','on');
 %reposition text
 seistool('reposit');
 return;
end
if(strcmp(action,'unzoom'))
	h=get(gcf,'userdata');
	hmsg=h(3);
	hopt=h(9);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hzoom=h(20);
	hplotmode=h(22);
	hax=get(hopt,'userdata');
 %if(get(hplotmode,'userdata')==1)
	 lims=get(hup,'userdata');
	 set(hax,'xlim',lims(1:2),'ylim',lims(3:4));
	%else
	% axis('auto');
	%end
	set(hup,'visible','off');
	set(hdown,'visible','off');
	set(hleft,'visible','off');
	set(hright,'visible','off');
 %reposition text
 seistool('reposit');
 set(hmsg,'string','');
 return;
end
		
%
% reposition the well labels after a zoom or scroll
%
if(strcmp(action,'reposit'))
	h=get(gcf,'userdata');
	hstor4=h(7);
	htext=get(hstor4,'userdata');
	if(~isempty(htext))
		ylim=get(gca,'ylim');
		ynew=min(ylim)+.1*abs(diff(ylim));
		for k=1:length(htext)
			pos=get(htext(k),'position');
			pos(2)=ynew;
			set(htext(k),'position',pos);
		end
	end
	return;
end
%
% change trace amplitude
%
if(strcmp(action,'tramp'))
 h=get(gcf,'userdata');
 hamp=h(10);
	set(gcf,'pointer','watch');
 
 ampdat=get(hamp,'userdata');
 hampmenus=ampdat(2:length(ampdat));
 
 for hk=hampmenus
      set(hk,'checked','off');
 end
 
 hmenu=gcbo;
 set(hmenu,'checked','on');
 
 ampdat(1)=get(hmenu,'userdata');
 set(hamp,'userdata',ampdat);
 
 seistool('plot');
	set(gcf,'pointer','arrow');
 
 return;
end
%
% set the matrix interpolation factor
%
if(strcmp(action,'matint'))
	h=get(gcf,'userdata');
	hmatinterp=h(38);
	set(gcf,'pointer','watch');
 
 intdat=get(hmatinterp,'userdata');
 hintmenus=intdat(2:length(intdat));
 
 for hk=hintmenus
      set(hk,'checked','off');
 end
 
 hmenu=gcbo;
 set(hmenu,'checked','on');
 
 intdat(1)=get(hmenu,'userdata');
 set(hmatinterp,'userdata',intdat);
 
 seistool('plot');
	set(gcf,'pointer','arrow');
 
 return;
end
%
% a scroll
% 
if(strcmp(action,'scroll'))
	h=get(gcf,'userdata');
 hbut=gco;
 dir=get(hbut,'string');
 
 xlim=get(gca,'xlim');
 ylim=get(gca,'ylim');
 if(strcmp(dir,'Right'))
		xwid=abs(diff(xlim));
		xlim=xlim+.5*xwid;
		set(gca,'xlim',xlim);
 elseif( strcmp(dir,'Left') )
		xwid=abs(diff(xlim));
		xlim=xlim-.5*xwid;
		set(gca,'xlim',xlim);
 elseif( strcmp(dir,'Up') )
		ywid=abs(diff(ylim));
		ylim=ylim-.5*ywid;
		set(gca,'ylim',ylim);
		 seistool('reposit');
 elseif( strcmp(dir,'Down') )
		ywid=abs(diff(ylim));
		ylim=ylim+.5*ywid;
		set(gca,'ylim',ylim);
		 seistool('reposit');
 end
 
 return;
end
%
% popup the color window
%
if(strcmp(action,'colors'))
	h=get(gcf,'userdata');
	hmsg=h(3);
	hopt=h(9);
	hright=h(14);
	hmatrix=h(23);
	hwt=h(25);
	hcolors=h(35);
	set(gcf,'pointer','watch');
	hmasterfig=gcf;
	hi=get(hmatrix,'userdata');
	%see if the color widget exists
	hcolorfig=get(hcolors,'userdata');
	done=0;
	if(~isempty(hcolorfig))
		if(colorview('iscolor',hcolorfig))
			set(hcolorfig,'visible','on');
			colorview('refresh',hi,hcolorfig);
			figure(hcolorfig);
			done=1;
		end
	end
	if(~done)
		hax=get(hopt,'userdata');
		sex=get(hright,'userdata');
		hcolorfig=colorview(hax,hi,sex(1),sex(2),1);
		set(hcolors,'userdata',hcolorfig);
	end
	%turn on color axes if needed
	hcax=get(hwt,'userdata');
	if(isempty(hcax))
		hcax=colorview('coloraxes',hmasterfig,hcolorfig);
		whitefig;
		set(hwt,'userdata',hcax);
		figure(hcolorfig);
	end
	set(hmasterfig,'pointer','arrow');
	return;
end
%%READ IN A segy dataset
if(strcmp(action,'readsegy'))
	h=get(gcf,'userdata');
	hclose=h(1);
	hmsg=h(3);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hreadsegy=h(15);
	hreadobj=h(16);
	hwritesegy=h(17);
	hwriteobj=h(18);
	hcolors=h(35);
	
	%get the file name
	[filename,path]=uigetfile('*.sgy','Select Input SEGY File');
	if( isempty(filename)|filename==0 )
		set(hmsg,'string','no file name given');
		set(gcf,'pointer','arrow');
		return;
	end
	set(gcf,'pointer','watch');
	fullfilename = [path filename];
	disp(' now reading file...');
	
	[seis,t,line_name,ntr,nrec,xs,ys,xr,yr,offs,selevs,relevs,...
		sdepths,cdps]=readsegy(fullfilename);
	%save seismic
	set(hclose,'userdata',seis);
	%save line_name
	set(hwritesegy,'userdata',line_name);
	%save ntr and nrec
	set(hwriteobj,'userdata',[ntr nrec]);
	%hide the colorfig
	hcolorfig=get(hcolors,'userdata');
	if(colorview('iscolor',hcolorfig))
		set(hcolorfig,'visible','off');
	end
		
	%compute midpoint coordinates
	xm=.5*(xs+xr);
	ym=.5*(ys+yr);
	
	%make a big geometry matrix
	geomat=[xm;xs;ys;xr;yr;offs;cdps;selevs;relevs;sdepths];
	set(hreadsegy,'userdata',geomat);
	
	%determine which coordinates are non-constant and therefore
	%useful as xcoords
	geoflags=ones(1,7);
	if(abs(sum(diff(xm)))<10000*eps)
		geoflags(1)=0;
	end
	if(abs(sum(diff(xs)))<10000*eps)
		geoflags(2)=0;
	end
	if(abs(sum(diff(ys)))<10000*eps)
		geoflags(3)=0;
	end
	if(abs(sum(diff(xr)))<10000*eps)
		geoflags(4)=0;
	end
	if(abs(sum(diff(yr)))<10000*eps)
		geoflags(5)=0;
	end
	if(abs(sum(diff(offs)))<10000*eps)
		geoflags(6)=0;
	end
	if(abs(sum(diff(cdps)))<10000*eps)
		geoflags(7)=0;
	end
	set(hreadobj,'userdata',geoflags);
	
	%cdps is the default x
	set(hdown,'userdata',cdps);
	
	set(hleft,'userdata',t);
	
	%plot
	set(hup,'userdata',[]);
	set(hright,'userdata',[]);
	axis('auto');
	seistool('plot');
	
	return;
end
%%write out segy
if(strcmp(action,'writesegy'))
 h=get(gcf,'userdata');
 hclose=h(1);
	hmsg=h(3);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hreadsegy=h(15);
	hreadobj=h(16);
	hwritesegy=h(17);
	hwriteobj=h(18);
	hcolors=h(35);
	
	%get the file name
	[filename,path]=uiputfile('*.sgy','Select Output SEGY File');
	if( isempty(filename)|filename==0 )
		set(hmsg,'string','no file name given');
		set(gcf,'pointer','arrow');
		return;
	end
	set(gcf,'pointer','watch');
	fullfilename = [path filename];
	disp(' now exporting file...');
 geomat=get(hreadsegy,'userdata');
	line_name=get(hwritesegy,'userdata');
	nn=get(hwriteobj,'userdata');
	if(isempty(nn))
		ntr=24;
	else
		ntr=nn(1);
	end
	t=get(hleft,'userdata');
	x=get(hdown,'userdata');
	%make a geomat if we don't have one
	if(isempty(geomat))
		geomat=zeros(10,length(x));
		geomat(2,:)=x;
		geomat(4,:)=x;
		geomat(7,:)=1:length(x);
	end
	%geomat=[xm;xs;ys;xr;yr;offs;cdps;selevs;relevs;sdepths];
	seis=get(hclose,'userdata');
	code=writesegy(fullfilename,...
		line_name,seis,geomat(2,:),t,0.0,ntr,...
		geomat(2,:),geomat(3,:),geomat(4,:),geomat(5,:),...
		geomat(6,:),geomat(8,:),geomat(9,:),geomat(10,:),...
		geomat(7,:));
	if(code==0)
		set(hmsg,'string','Write FAILED!!');
	else
		set(hmsg,'string',['SEGY write successful to: ' fullfilename]);
	end
	set(gcf,'pointer','arrow');
 
	return;
end
%%READ IN A Matlab dataset
if(strcmp(action,'readobj'))
	h=get(gcf,'userdata');
	hclose=h(1);
	hmsg=h(3);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hreadsegy=h(15);
	hreadobj=h(16);
	hwritesegy=h(17);
	hwriteobj=h(18);
	hcolors=h(35);
	
	%get the file name
	[filename,path]=uigetfile('*.mat','Select Input Matlab File');
	if( isempty(filename)|filename==0 )
		set(hmsg,'string','no file name given');
		set(gcf,'pointer','arrow');
		return;
	end
	set(gcf,'pointer','watch');
	fullfilename = [path filename];
	disp(' now reading file...');
	
   l=load(fullfilename);
   names=fieldnames(l);
   prompt={'Please Select the Time Vector','Please Select the Inline Vector',...
       'Please Select the Seismic Section Variable'};
   name='Please Select the following Variables';
   numlines=1;
   defaultanswer={names,names,names};
   answer=inputdlg(prompt,name,numlines,defaultanswer);
   
if isempty(answer)
   set(hmsg,'string','Import Matlab Cancelled');
		set(gcf,'pointer','arrow');
		return;
end
   
if size(answer{3})==[length(answer{1}),length(answer{2})]
    set(hmsg,'string','Variables do not support Seismic Section');
		set(gcf,'pointer','arrow');
		return;
end
seis=getfield(l,answer{3});
t=getfield(l,answer{1});
x=getfield(l,answer{2});
line_name='Unknown';
	%save seismic
	set(hclose,'userdata',seis);
	%save line_name
	set(hwritesegy,'userdata',line_name);
    
    %save x and t vectors
    set(hleft,'userdata',t);
	set(hdown,'userdata',x);
	%make a geomat
		geomat=zeros(10,length(x));
		geomat(2,:)=x;
		geomat(4,:)=x;
		geomat(7,:)=1:length(x);
    
    %save Geomat
    set(hreadsegy,'userdata',geomat);
    
    ntr=[];
    nrec=[];
	set(hwriteobj,'userdata',[ntr nrec]);
	%hide the colorfig
	hcolorfig=get(hcolors,'userdata');
	if(colorview('iscolor',hcolorfig))
		set(hcolorfig,'visible','off');
	end
		
% 	%compute midpoint coordinates
% 	xm=.5*(xs+xr);
% 	ym=.5*(ys+yr);
% 	
% 	%make a big geometry matrix
% 	geomat=[xm;xs;ys;xr;yr;offs;cdps;selevs;relevs;sdepths];
% 	set(hreadsegy,'userdata',geomat);
% 	
% 	%determine which coordinates are non-constant and therefore
% 	%useful as xcoords
% 	geoflags=ones(1,7);
% 	if(abs(sum(diff(xm)))<10000*eps)
% 		geoflags(1)=0;
% 	end
% 	if(abs(sum(diff(xs)))<10000*eps)
% 		geoflags(2)=0;
% 	end
% 	if(abs(sum(diff(ys)))<10000*eps)
% 		geoflags(3)=0;
% 	end
% 	if(abs(sum(diff(xr)))<10000*eps)
% 		geoflags(4)=0;
% 	end
% 	if(abs(sum(diff(yr)))<10000*eps)
% 		geoflags(5)=0;
% 	end
% 	if(abs(sum(diff(offs)))<10000*eps)
% 		geoflags(6)=0;
% 	end
% 	if(abs(sum(diff(cdps)))<10000*eps)
% 		geoflags(7)=0;
% 	end
% 	set(hreadobj,'userdata',geoflags);
% 	
% 	%cdps is the default x
% 	set(hdown,'userdata',cdps);
% 	
% 	set(hleft,'userdata',t);
	
	%plot
	set(hup,'userdata',[]);
	set(hright,'userdata',[]);
	axis('auto');
	seistool('plot');
	
	return;
end
%%write out MATLAB
if(strcmp(action,'writeobj'))
 h=get(gcf,'userdata');
 hclose=h(1);
	hmsg=h(3);
	hup=h(11);
	hdown=h(12);
	hleft=h(13);
	hright=h(14);
	hreadsegy=h(15);
	hreadobj=h(16);
	hwritesegy=h(17);
	hwriteobj=h(18);
	hcolors=h(35);
	
	%get the file name
	[filename,path]=uiputfile('*.mat','Select Output Matlab File');
	if( isempty(filename)|filename==0 )
		set(hmsg,'string','no file name given');
		set(gcf,'pointer','arrow');
		return;
	end
	set(gcf,'pointer','watch');
	fullfilename = [path filename];
	disp(' now exporting file...');
	t=get(hleft,'userdata');
	x=get(hdown,'userdata');
	seis=get(hclose,'userdata');
	save(fullfilename,'t','x','seis');
	set(hmsg,'string',['MATLAB write successful to: ' fullfilename]);	
	set(gcf,'pointer','arrow');
 
	return;
end