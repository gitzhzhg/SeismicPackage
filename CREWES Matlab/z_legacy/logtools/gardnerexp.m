function gardnerexp(arg1,arg2,arg3,arg4,arg5)
% gardnerexp(v,rho,z,tops,ztops)
%
% GARDNEREXP allows interactive estimation of the Gardner parameters 
% empirically linking density and instantaneous p-wave velocity.
% Vectors of velocity and density values must be available and both
% may contain NaNs. The program pops up a figure window displaying
% the vp and density logs on the left (vp is blue) and a density
% versus velocity crossplot on the right. You may then interactively
% fit Gardner's rule to portions of the log using least squares.
% The Gardner relation is
% 		rho=a*v.^b (a times v to the bth power)
% 
% Mouse button assignments:
% MB1: use to draw a box in either axes to select the fit range.
%		If zoom as been selected from the option menu, then the 
%		button is temporally reassigned to draw a zoom box
% MB2: Click on any fit result on the crossplot axes to delete it
% MB3: Click on any fit result on the crossplot axes to see the
%		Gardner parameters and the points used for that fit.
%
% G.F. Margrave
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
if( nargin<3 & ~isstr(arg1) )
		  error('you must supply velocity, density, and depth vectors')
end
if( nargin>=3 )
	action='init';
	v=arg1(:);
	rho=arg2(:);
	z=arg3(:);
	if(nargin<4)
		tops=[];
		ztops=[];
	else
		tops=arg4;
		ztops=arg5;
	end
	indv=~isnan(v);
	indrho=~isnan(rho);
	iuse=find( indv & indrho );
	if( length(iuse)< 2)
		error([' there must be at least 2 live'...
		' velocity/density pairs']);
	end
else
 if(~isstr(arg1))
		disp('there must be three input vectors for gardnerexp');
		error('invalid argument(s) for gardnerexp');
	end
		
	action=arg1;
end
if(strcmp(action,'init'))
 % a new figure
 hfig=figure('visible','off');
 pos=get(gcf,'position');
 ss=get(0,'screensize');
 pos=[.1*ss(3) .1*ss(4) .7*ss(3) .8*ss(4)];
 set(hfig,'position',pos);
 set(gcf,'name','Gardners Relation Tool');
 set(hfig,'visible','on');
	%make an options menu
	hopt=uimenu(gcf,'label','Options');
	hsave=uimenu(hopt,'label','Save Results',...
		'callback','gardnerexp(''save'')');
	hzoom=uimenu(hopt,'label','Zoom',...
		'callback','gardnerexp(''zoom'')');
	hunzoom=uimenu(hopt,'label','unZoom');
	hunzoom1=uimenu(hunzoom,'label','Log axes',...
		'callback','gardnerexp(''unzoom1'')');
	hunzoom2=uimenu(hunzoom,'label','Crossplot',...
		'callback','gardnerexp(''unzoom2'')');
	hunzoom3=uimenu(hunzoom,'label','Both axes',...
		'callback','gardnerexp(''unzoom2'');gardnerexp(''unzoom1'')');
	%message panel
	hmsg=uicontrol('style','text','string',...
		'Select fit region with MB1',...
		'units','normalized','position',[0 0 1 .025]);
	%plot logs
	subplot(1,2,1)
	ilive=find(~isnan(v));
	vmin=min(v(ilive));
	vmax=max(v(ilive));
	vv=(v-vmin)/(vmax-vmin);
	hv=line(vv,z,'color','c');
	ilive=find(~isnan(rho));
	rhomin=min(rho(ilive));
	rhomax=max(rho(ilive));
	rhorho=(rho-rhomin)/(rhomax-rhomin);
	hrho=line(rhorho,z,'color','g');
	set(gca,'ydir','reverse');
	ylabel('depth');
	xlabel('normalized');
	hax1=gca;
	
	%plot tops
	xlim=get(gca,'xlim');
	  ntops=length(ztops);
	  htopsline=ones(1,ntops);
	  htopstext=ones(1,ntops);
	  for ii=1:ntops
		col=[1 .5 0];
		lbl=sprintf('%5.2f',ztops(ii));
		lbl=sprintf('%2.5f',ztops(ii));
	     htopstext(ii)=text(xlim(2),ztops(ii),tops(ii,:),...
	         'verticalalignment','baseline',...
	         'horizontalalignment','left',...
	          'color',col,'fontsize',10);
	     
	     htopsline(ii)=line(xlim, [ztops(ii) ztops(ii)],...
	          'color',col,'linestyle','-.',...
				 'userdata',['Top: ' tops(ii,:) ' depth ' lbl]);
	  end
	%crossplot
	subplot(1,2,2);
	hdata=line(v,rho,'linestyle','.','color','m');
	set(hdata,'userdata',z);
	xlabel('Velocity');
	ylabel('Density');
	hax2=gca;
 
 %save things
 	set(hfig,'userdata',[hopt hsave hmsg hdata hax1 hax2 hzoom ...
		hunzoom hunzoom1 hunzoom2 hunzoom3]);
 %
 % userdata
 % 1 hopt = handles of the red curves (data used in fit)
 % 2 hsave = not used
 % 3 hmsg = tops info
 % 4 hdata = z
 % 5 hax1 = not used
 % 6 hax2 = not used
 % 7 hzoom = temp storage during zoom
 % 8 hunzoom = htopstext htopsline
 % 9 hunzoom1 = not used
 % 10 hunzoom2 = not used
 % 11 hunzoom3 = not used
	 [m,n]=size(tops);
	 set(hmsg,'userdata',[m; n; ztops(:); nan; tops(:)]);
	 set(hunzoom,'userdata',[htopstext htopsline]);
	 set(hopt,'userdata',[]);
	
	 selboxinit('gardnerexp(''fit'')',1)
	
	 return;
end
if(strcmp(action,'fit'))
 %determine which button was pressed
 button=get(gcf,'selectiontype');
 if(~strcmp(button,'normal'))
		return;
 end
	%get the zoombox
    box=selboxfini;
    try
       delete(box{2});
    catch
       %no selbox to delete
    end
    box = box{1};
    
	h=get(gcf,'userdata');
	hopt=h(1);
	hmsg=h(3);
	hdata=h(4);
	hax1=h(5);
	hax2=h(6);
	v=get(hdata,'xdata');
	ilive=find(~isnan(v));
	vmin=min(v(ilive));
	vmax=max(v(ilive));
	rho=get(hdata,'ydata');
	ilive=find(~isnan(rho));
	rhomin=min(rho(ilive));
	rhomax=max(rho(ilive));
	rho=get(hdata,'ydata');
	z=get(hdata,'userdata');
	hl=get(hopt,'userdata');
	if(~isempty(hl))
		set(hopt,'userdata',[]);
		delete(hl);
	end
 if(gca==hax2) %selection drawn on crossplot
		indx=between(box(1),box(3),v,2);
		indy=between(box(2),box(4),rho(indx),2);
		ind=indx(indy);
		
		%determine the fit
		% I seem to be letting nans into the fit
		lv=log(v(ind));
		lr=log(rho(ind));
		p=polyfit(lv,lr,1);
		a=exp(p(2));
		b=p(1);
		vfit=linspace(vmin,vmax,100);
		rhop= a*vfit.^(b);
		%draw the fit curve
		% this type of fit is drawn in magenta with a solod line and is 
		% referred to as a global fit
		hp=line(vfit,rhop,'color','m','userdata',[a;b;ind(:)],...
		'buttondownfcn','gardnerexp(''sayhey'')',...
		'linestyle','-');
		% make the selected points red
		hl1=line(v(ind),rho(ind),'color','r','linestyle','.');
		set(gcf,'currentaxes',hax1);
		vv=(v(ind)-vmin)/(vmax-vmin);
		rhorho=(rho(ind)-rhomin)/(rhomax-rhomin);
		hl2=line(vv,z(ind),'color','r','marker','+');
		hl3=line(rhorho,z(ind),'color','r','marker','+');
		set(gcf,'currentaxes',hax2);
		set(hmsg,'string',['Global Gardner parameters: a= ',num2str(a),...
			' b= ',num2str(b)]);
		set(hopt,'userdata',[hl1 hl2 hl3]);
	else %selection drawn on log display
		zmin=min(box([2 4]));
		zmax=max(box([2 4]));
		ind=between(box(2),box(4),z,2);
		
		%determine the fit
		lv=log(v(ind));
		lr=log(rho(ind));
		p=polyfit(lv,lr,1);
		a=exp(p(2));
		b=p(1);
		vfit=linspace(vmin,vmax,100);
		rhop= a*vfit.^(b);
		
		%draw the fit
		% this type of fit is drawn in pumpkin with a dash-dot line and is
		% referred to as local fit
		set(gcf,'currentaxes',hax2);
		hp=line(vfit,rhop,'color',[1 .5 0],'userdata',[a;b;zmin;zmax;ind(:)] ,...
		'buttondownfcn','gardnerexp(''sayhey'')',...
		'linestyle','-.');
		%highlight the point involved in the fit in red
		hl1=line(v(ind),rho(ind),'color','r','linestyle','.');
		set(gcf,'currentaxes',hax1);
		vv=(v(ind)-vmin)/(vmax-vmin);
		rhorho=(rho(ind)-rhomin)/(rhomax-rhomin);
		hl2=line(vv,z(ind),'color','r','marker','+');
		hl3=line(rhorho,z(ind),'color','r','marker','+');
		set(gcf,'currentaxes',hax2);
		set(hmsg,'string',['for (zmin,zmax)= ' num2str(zmin)...
			',' num2str(zmax) ...
			' Gardner parameters: a= ',num2str(a),...
			' b= ',num2str(b)]);
		set(hopt,'userdata',[hl1 hl2 hl3]);
	end
	
	return;
end
if(strcmp(action,'sayhey'))
 %determine the selectiontype
 	flag=get(gcf,'selectiontype');
 	if(strcmp(flag,'normal'))
		return;
	end
	hobj=gco;
	dat=get(gco,'userdata');
	h=get(gcf,'userdata');
	hopt=h(1);
	hmsg=h(3);
	hdata=h(4);
	hax1=h(5);
	hax2=h(6);
	v=get(hdata,'xdata');
	ilive=find(~isnan(v));
	vmin=min(v(ilive));
	vmax=max(v(ilive));
	rho=get(hdata,'ydata');
	ilive=find(~isnan(rho));
	rhomin=min(rho(ilive));
	rhomax=max(rho(ilive));
	z=get(hdata,'userdata');
	%see if we are deleting a fit
	if(strcmp(flag,'extend'))
		delete(gco);
		return;
	end
	hl=get(hopt,'userdata');
	if(~isempty(hl))
		set(hopt,'userdata',[]);
		delete(hl);
	end
	if(~isempty(dat))
		flag=get(hobj,'linestyle');
		if(flag=='-')
			a=dat(1);
			b=dat(2);
			ind=dat(3:length(dat));
			hl1=line(v(ind),rho(ind),'color','r','linestyle','.');
			set(gcf,'currentaxes',hax1);
			vv=(v(ind)-vmin)/(vmax-vmin);
			rhorho=(rho(ind)-rhomin)/(rhomax-rhomin);
			hl2=line(vv,z(ind),'color','r','marker','+');
			hl3=line(rhorho,z(ind),'color','r','marker','+');
			set(gcf,'currentaxes',hax2);
			set(hmsg,'string',['Global Gardner parameters: a= ',num2str(a),...
				' b= ',num2str(b)]);
			set(hopt,'userdata',[hl1 hl2 hl3])
		else
			a=dat(1);
			b=dat(2);
			zmin=dat(3);
			zmax=dat(4);
			ind=dat(5:length(dat));
			hl1=line(v(ind),rho(ind),'color','r','linestyle','.');
			set(gcf,'currentaxes',hax1);
			vv=(v(ind)-vmin)/(vmax-vmin);
			rhorho=(rho(ind)-rhomin)/(rhomax-rhomin);
			hl2=line(vv,z(ind),'color','r','marker','+');
			hl3=line(rhorho,z(ind),'color','r','marker','+');
			set(gcf,'currentaxes',hax2);
			set(hmsg,'string',['for (zmin,zmax)= ' num2str(zmin)...
				',' num2str(zmax) ...
				' Gardner parameters: a= ',num2str(a),...
				' b= ',num2str(b)]);
			set(hopt,'userdata',[hl1 hl2 hl3])
		end
	end
	return;
end
% zoom
if(strcmp(action,'zoom'))
	h=get(gcf,'userdata');
	hmsg=h(3);
	hax1=h(5);
	hax2=h(6);
	hzoom=h(7);
	
	set(hmsg,'string','draw zoombox with MB1');
	
	fcn1=get(gcf,'windowbuttondownfcn');
	fcn2=get(gcf,'windowbuttonmotionfcn');
	fcn3=get(gcf,'windowbuttonupfcn');
	dat1=get(hax1,'userdata');
	[m1,n1]=size(dat1);
	dat2=get(hax2,'userdata');
	[m2,n2]=size(dat2);
	stuff=[m1 n1 dat1(:)' m2 n2 dat2(:)' nan abs(fcn1) nan ...
	abs(fcn2) nan abs(fcn3)];
	set(hzoom,'userdata',stuff);
	set(hax1,'userdata',[]);
	set(hax2,'userdata',[]);
	selboxinit('gardnerexp(''zoom2'')',1);
	return;
end
%
%*******REPOSITION THE TOPS LABELS*********
%
if(strcmp(action,'repositops'))
	h=get(gcf,'userdata');
  	hunzoom=h(8);
	info=get(hunzoom,'userdata');
	if(~isempty(info))
		xlim=get(gca,'xlim');
		ntops=length(info)/2;
		htext=info(1:ntops);
		hline=info(ntops+1:2*ntops);
		for k=1:ntops
			pos=get(htext(k),'position');
			set(htext(k),'position',[xlim(2) pos(2)]);
			set(hline(k),'xdata',xlim);
		end
	end
	return;
end
% zoom2
if(strcmp(action,'zoom2'))
	h=get(gcf,'userdata');
	hmsg=h(3);
	hax1=h(5);
	hax2=h(6);
 	hzoom=h(7);
    
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
	set(gca,'xlim',[xmin xmax],'ylim',[ymin ymax]);
	 stuff=get(hzoom,'userdata');
	 m=abs(stuff(1));
	 n=abs(stuff(2));
	 mn=m*n;
	 dat1=stuff(3:2+mn);
	 dat1=reshape(dat1,m,n);
	 m=abs(stuff(mn+3));
	 n=abs(stuff(mn+4));
	 dat2=stuff(mn+5:mn+4+m*n);
	 dat2=reshape(dat2,m,n);
	
	 set(hax1,'userdata',dat1);
	 set(hax2,'userdata',dat2);
	
	 ind=find(isnan(stuff));
	 fcn1=setstr(stuff(ind(1)+1:ind(2)-1));
	 fcn2=setstr(stuff(ind(2)+1:ind(3)-1));
	 fcn3=setstr(stuff(ind(3)+1:length(stuff)));
	 set(gcf,'windowbuttondownfcn',fcn1,'windowbuttonmotionfcn',fcn2,...
		'windowbuttonupfcn',fcn3);
		
	if(gca==hax1)
		gardnerexp('repositops');
	end
	set(hmsg,'string','Select fit region with MB1');
 return;
end
if(strcmp(action,'unzoom1'))
	h=get(gcf,'userdata');
	hmsg=h(3);
	hax1=h(5);
	hax2=h(6);
	hzoom==h(7);
	set(hax1,'xlimmode','auto','ylimmode','auto');
	set(hmsg,'string','Select fit region with MB1');
	gardnerexp('repositops');
	return;
end
if(strcmp(action,'unzoom2'))
	h=get(gcf,'userdata');
	hmsg=h(3);
	hax1=h(5);
	hax2=h(6);
	hzoom==h(7);
	set(hax2,'xlimmode','auto','ylimmode','auto');
	set(hmsg,'string','Select fit region with MB1');
	return;
end