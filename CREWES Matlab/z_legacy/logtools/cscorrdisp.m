function [cson,tshft,tcsp,zcsp]=cscorrdisp(s,z,tcs,zcs,transfer)
% cscorrdisp(s,z,tcs,zcs,transfer) ... initiate the display
% [cs,tnot,tcs2,zcs2]=cscorrdisp('fini') ... finish the display
%
% CSCORRDISP puts up an interactive display allowing the the check shot
% correction of a sonic log. The algorithm used in any iteration is that
% found in CSCORR_SONIC. Facilities are provided to edit the check shot,
% view the corrected log next to the original, and view the applied
% delta-t's.
%
% s = vector of input sonic log
% z = vector of depth for s
% tcs = vector of check shot times
% zcs = vector of check shot depths
% transfer = string vector containing any legal matlab command to be called
%		when the user signals completion
% cs = corrected sonic
% tnot = one-way time constant that must be added to integrated sonic
%			one-way times to match check shot times.
% tcs2 = vector of edited check shot times
% zcs2 = vector of edited check shot depths
%
% G.F. Margrave November 1994
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
if(nargin>1)
	action='init';
else
	action=s;
end
%initialize the display
if(strcmp(action,'init'))
	hmasterfig=gcf;
	hfig=figure('visible','off');
	%set the figure size
	pos=get(hfig,'position');
	ss=get(0,'screensize');
	xscreen=ss(3);
	yscreen=ss(4);
	xdefsize=1000*xscreen/1152;
	ydefsize=750*yscreen/900;
	if( xscreen== 1)
   xscreen = 1000;
	end
	if(yscreen==1)
   yscreen=800;
	end
 
	if( (xdefsize < 200 ) | (xdefsize > 1100) )
   xdefsize=1000;
	end
	if( (ydefsize < 200 ) | (ydefsize > 900) )
   xdefsize=750;
	end
 
	if( pos(1)+xdefsize> xscreen-10)
   pos(1)= pos(1) - (pos(1)+xdefsize -xscreen)-50;
	end
	if( pos(2)+ydefsize> yscreen-10)
   pos(2)= pos(2) - (pos(2)+ydefsize -yscreen)-50;
	end
	fight=ydefsize;
	figwd=xdefsize;
	set(hfig,'position',[pos(1:2) figwd fight]);
	
	%make the menu
	hopt=uimenu(gcf,'label','Options');
	
	%edit the check shot
	hedit=uimenu(hopt,'label','Edit C.S. Times','callback',...
		'cscorrdisp(''editon'')');
	hundo=uimenu(hopt,'label','Undo Last Edit','callback',...
		'editlines(''undo'')','enable','off');
		
	%compute the corrections
	hcompute=uimenu(hopt,'label','Compute Corrected Log','callback',...
		'cscorrdisp(''compute'')');
		
	%zoom
	hzoom = uimenu(hopt,'label','Zoom','callback',...
			'cscorrdisp(''zoom'')');
	hunzoom = uimenu(hopt,'label','Un-Zoom','callback',...
			'cscorrdisp(''unzoom'')');
		
	%Set check shot times
	hcstimes=uimenu(hopt,'label','C.S. Times are...');
	honeway=uimenu(hcstimes,'label','ONE WAY Traveltime','callback',...
		'cscorrdisp(''settimes'')','checked','on');
	htwoway=uimenu(hcstimes,'label','TWO WAY Traveltime','callback',...
		'cscorrdisp(''settimes'')','checked','off');
		
	%cancel
	hcancel=uimenu(hopt,'label','Cancel','Callback',...
		'cscorrdisp(''cancel'')');
	
	%done
	hdone=uimenu(hopt,'label','Done','Callback',...
		'cscorrdisp(''done'')');
		
	%initialize the plotting
	sep=.05;
	xnow=sep;
	ynow=2*sep;
	width=.5*(1-4*sep);
	ht=.85;
	hax1=axes('position',[xnow ynow width ht]);
	grid
	%hax1=subplot(1,3,1);
	hcst=line(tcs,zcs,'color','r','marker','*','markersize',12);
	set(gca,'ydir','reverse');
	ylim=get(gca,'ylim');
	ylabel('depth');
	xlabel('Red: c.s. times  Green: sonic times  Blue: corr. sonic times');
	
	%hax2=subplot(1,3,2);
	xnow=xnow+width+sep;
	width=.5*width;
	hax2=axes('position',[xnow ynow width ht]);
	grid
	hson=line(1.e06 ./s,z,'color',[.5 0 .5]);
	set(gca,'ydir','reverse');
	set(gca,'ylim',ylim);
	xlabel('Purple: input sonic  Blue: corr. sonic');
 text('string','Check Shot Correction Display','units','normalized',...
		'position',[0 ynow+ht+1.6*sep],...
		'horizontalalignment','center','fontsize',18);
	
	%hax3=subplot(1,3,3);
	xnow=xnow+width+sep;
	hax3=axes('position',[xnow ynow width ht]);
	grid
	set(gca,'ydir','reverse');
	set(gca,'ylim',ylim);
	xlabel('Red: dt at c.s.  Blue: applied dt');
	
	%make a message panel
	hmsg=uicontrol('style','text','string','Edit check shot or Compute corrections',...
		'units','normalized','position',[sep .01 1-2*sep .5*sep]);
	
	%save things
	set(hfig,'userdata',[hopt hedit hcompute hzoom hunzoom hcstimes honeway...
			htwoway hcancel hdone hmsg hundo]);
	%
	% userdata assignments
	%	hopt = h(1) ... [z s] the origonal depth and sonic vectors
	%	hedit = h(2) ... the current anchors for the cs times
	%	hcompute = h(3) ... [hax1 hax2 hax3] the three figure axes
	%	hzoom = h(4) ... [hcst hint hint2 hson hcs hdt hdts...] curve handles 
	%			hcst= current check shot times
	%			hint= original integrated sonic times
	%			hint2 = current integrated sonic times
	%			hson = original sonic
	%			hcs = corrected sonic
	%			hdt = check shot delta t's
	%			hdts = splined delta t's
	% hunzoom = h(5) ... handle of the master figure
	% hcstimes = h(6) ...[zcs tcs] the current (edited) check shot corrections
	% honeway = h(7) ...the latest corrected sonic
	% htwoway = h(8) ...the constant time shift, tshft
	% hcancel = h(9) ...the three window button functions before zooming: [f1 nan f2 ...
	% hdone = h(10) ...the transfer function
	% hmsg = h(11) ...unused
	% hundo = h(12) ... notused
	%
	set(hopt,'userdata',[z s]);
	set(hedit,'userdata',0);
	set(hcompute,'userdata',[hax1 hax2 hax3]);
	set(hzoom,'userdata',[hcst 0 0 hson 0 0 0]);
	set(hunzoom,'userdata',hmasterfig);
	set(hcstimes,'userdata',[zcs tcs]);
	set(hdone,'userdata',transfer);
	
	%make the figure visible
	set(hfig,'visible','on');
	
	return;
end
%turn on editing
if(strcmp(action,'editon'))
	h=get(gcf,'userdata');
	hedit=h(2);
	hcompute=h(3);
	hzoom=h(4);
	hmsg=h(11);
	hundo=h(12);
	%se if we are really turning it off
	flag=get(hedit,'checked');
	if(strcmp(flag,'on'));
		cscorrdisp('editoff');
		return;
	end
	
	handles=get(hzoom,'userdata');
	hcst=handles(1);
	
	anch=get(hedit,'userdata');
	
	axes1=get(hcompute,'userdata');
	hax1=axes1(1);
	
	set(gcf,'currentaxes',hax1);
	editlinesinit([-hcst anch]);
	editlines('locate');
 %modify the window button down function
 fcn=get(gcf,'windowbuttondownfcn');
 set(gcf,'windowbuttondownfcn',['cscorrdisp(''setaxe'');' fcn]);
 set(hedit,'checked','on');
 set(hundo,'enable','on');
	
	set(hmsg,'string',['MB1(click): new pt MB1(drag): move pt MB2: delete pt'...
			' MB3(click): set/free anchors MB3(drag): move pt groups']);
	
	return;
end
%turn off editing
if(strcmp(action,'editoff'))
	h=get(gcf,'userdata');
	hedit=h(2);
	hzoom=h(4);
	hcstimes=h(6);
	honeway=h(7);
	hmsg=h(11);
	hundo=h(12);
	if(strcmp(get(hedit,'checked'),'on'))
		flag=get(honeway,'checked');
		if(strcmp(flag,'on'))
			fact=1.0;
		else
			fact=.5;
		end
	
		anch=editlinesfini;
		set(hedit,'userdata',anch(2:length(anch)));
	
		handles=get(hzoom,'userdata');
		hcst=handles(1);
		tcs=get(hcst,'xdata')/fact;
		zcs=get(hcst,'ydata');
		set(hcstimes,'userdata',[zcs(:) tcs(:)]);
		set(hedit,'checked','off');
		set(hundo,'enable','off');
	end
	set(hmsg,'string','Editing turned off');
	
	return;
end
%compute the correction
if(strcmp(action,'compute'))
	h=get(gcf,'userdata');
	hopt=h(1);
	hedit=h(2);
	hcompute=h(3);
	hzoom=h(4);
	hcstimes=h(6);
	honeway=h(7);
	htwoway=h(8);
	hmsg=h(11);
	set(gcf,'pointer','watch');
	cscorrdisp('editoff');
	flag=get(honeway,'checked');
	if(strcmp(flag,'on'))
		fact=1.0;
	else
		fact=.5;
	end
	
	dat=get(hopt,'userdata');
	z=dat(:,1);
	s=dat(:,2);
	
	dat=get(hcstimes,'userdata');
	zcs=dat(:,1);
	tcs=fact*dat(:,2);
	
	[cs,tnot,delt,zcs2,tcs2,deltcs]=cscorr_sonic(s,z,tcs,zcs);
	tson2=.5*int_sonic(cs,z);
	set(honeway,'userdata',cs);
	set(htwoway,'userdata',tnot);
	
	%plot the results
	handles=get(hzoom,'userdata');
	%[hcst hint hint2 hson hcs hdt hdts...] curve handles
	hcst=handles(1);
	hint=handles(2);
	hint2=handles(3);
	hcs=handles(5);
	hdt=handles(6);
	hdts=handles(7);
	
	haxes=get(hcompute,'userdata');
	
	set(gcf,'currentaxes',haxes(1));
	if(hint) delete(hint); end
	tson=.5*int_sonic(s,z);
	hint=line(tson+tnot,z,'color','g');
	if(hint2) delete(hint2); end
	hint2=line(tson2+tnot,z,'color','b');
	%delete and redraw the cs times so that they are on top
	x=get(hcst,'xdata');
	y=get(hcst,'ydata');
	ls=get(hcst,'linestyle');
	ms=get(hcst,'markersize');
	kol=get(hcst,'color');
	delete(hcst);
	hcst=line('xdata',x,'ydata',y,'linestyle',ls,'markersize',ms,...
		'color',kol);
	set(gcf,'currentaxes',haxes(2));
	if(hcs) delete(hcs); end
	hcs=line(1.e06 ./cs,z,'color','b');
	set(gcf,'currentaxes',haxes(3));
	if(hdt) delete(hdt); end
	hdt=line(deltcs,zcs2,'color','r','marker','*','markersize',12);
	if(hdts) delete(hdts); end
	hdts=line(delt,z,'color','b');
	%save the handles
	set(hzoom,'userdata',[hcst hint hint2 handles(4) hcs hdt hdts]);
	%message
	set(hmsg,'string','Done to finish... else Edit and re-Compute');
	set(gcf,'pointer','arrow');
	return;
end
if(strcmp(action,'setaxe'))
	h=get(gcf,'userdata');
 hcompute=h(3);
 haxes=get(hcompute,'userdata');
	set(gcf,'currentaxes',haxes(1));
	return;
end	
%
% initiate a zoom
%
if(strcmp(action,'zoom'))
	h=get(gcf,'userdata');
	hcompute=h(3);
 hcancel=h(9);
 hmsg=h(11);
 %save the window button functions
 f1=get(gcf,'windowbuttondownfcn');
 f2=get(gcf,'windowbuttonupfcn');
 f3=get(gcf,'windowbuttonmotionfcn');
 set(hcancel,'userdata',[abs(f1) nan abs(f2) nan abs(f3)]);
 %initiate
 selboxinit('cscorrdisp(''zoom2'')');
 set(hmsg,'string','MB1: draw zoom box');
 return;
end
if(strcmp(action,'zoom2'))
	h=get(gcf,'userdata');
	hcompute=h(3);
	hcancel=h(9);
	%reset the functions
	fs=get(hcancel,'userdata');
	ind=find(isnan(fs));
	set(gcf,'windowbuttondownfcn',setstr(fs(1:ind(1)-1)));
	set(gcf,'windowbuttonupfcn',setstr(fs(ind(1)+1:ind(2)-1)));
	set(gcf,'windowbuttonmotionfcn',setstr(fs(ind(2)+1:length(fs))));

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
	%get the current axis settings
	xlim=get(gca,'xlim');
	ylim=get(gca,'ylim');
	test1=xmin-xlim(1)+xmax-xlim(2)+ymin-ylim(1)+ymax-ylim(2);
	test2=(xmin-xmax)*(ymin-ymax);
	if(abs(test1)<10*eps | abs(test2)< 10*eps)
		cscorrdisp('unzoom');
	else
		set(gca,'xlim',[xmin xmax],'ylim',[ymin ymax]);
		%do the other two axes
		haxes=get(hcompute,'userdata');
		ind=find(haxes~=gca);
		%xrnge=abs(diff(xlim));
		%xminpct=(xmin-min(xlim))/xrnge;
		%xmaxpct=(xmax-min(xlim))/xrnge;
		for k=1:2
			hax=haxes(ind(k));
			%xlim=get(hax,'xlim');
			%xrnge=abs(diff(xlim));
			%xmin=min(xlim)+xminpct*xrnge;
			%xmax=min(xlim)+xmaxpct*xrnge;
			set(hax,'xlimmode','auto','ylim',[ymin ymax]);
		end
	end
	return;
end
if(strcmp(action,'unzoom'))
	h=get(gcf,'userdata');
	hcompute=h(3);
	haxes=get(hcompute,'userdata');
	set(haxes(1),'xlimmode','auto','ylimmode','auto');
	ylim=get(haxes(1),'ylim');
	set(haxes(2),'xlimmode','auto','ylim',ylim);
	set(haxes(3),'xlimmode','auto','ylim',ylim);
	return;
end
%set for one-way or two way times
if(strcmp(action,'settimes'))
	h=get(gcf,'userdata');
	hzoom=h(4);
	honeway=h(7);
	htwoway=h(8);
	hmenu=gcbo;
	%determine current mode
	flag=get(honeway,'checked');
	if(strcmp(flag,'on'))
		modenow=1;
	else
		modenow=2;
	end
	%determine desired mode
	if(hmenu==honeway)
		modethen=1;
	else
		modethen=2;
	end
	if(modenow==modethen)%do nothing
		return;
	end
	handles=get(hzoom,'userdata');
	hcs=handles(1);
	if(modethen==1)% display used to assume 2-way now its assumed 1-way
		set(honeway,'checked','on');
		set(htwoway,'checked','off');
		tcs=get(hcs,'xdata');
		set(hcs,'xdata',2*tcs);
	else %display used to assume 1-way now its 2-way
		set(honeway,'checked','off');
		set(htwoway,'checked','on');
		tcs=get(hcs,'xdata');
		set(hcs,'xdata',tcs/2);
	end
	return;
end
		
%
% cancel
%
if( strcmp(action,'cancel') )
	h=get(gcf,'userdata');
	hunzoom=h(5);
	set(gcf,'pointer','watch');
	hmasterfig=get(hunzoom,'userdata');
	yesnoinit('cscorrdisp(''cancel2'')','Really Cancel??');
	return;
end
if( strcmp(action,'cancel2') )
	h=get(gcf,'userdata');
	hunzoom=h(5);
	hdone=h(10);
	hmsg=h(11);
	hfig=gcf;
	ans=yesnofini;
	if(ans==1)
		hmasterfig=get(hunzoom,'userdata');
		hax=get(hmasterfig,'currentaxes');
		set(hax,'userdata',-1);
		transfer=get(hdone,'userdata');
		figure(hmasterfig);
		eval(transfer);
		close(hfig);
	else
		set(hmsg,'string','Cancel Cancelled');
		set(hfig,'pointer','arrow');
	end
	return;
end
%
% the done button
%
if(strcmp(action,'done'))
	h=get(gcf,'userdata');
	hunzoom=h(5);
	hcstime=h(6);
	honeway=h(7);
	htwoway=h(8);
	hdone=h(10);
 hfig=gcf;
	hmasterfig=get(hunzoom,'userdata');
	hax=get(hmasterfig,'currentaxes');
	cs=get(honeway,'userdata');
	if(isempty(cs))
		set(hax,'userdata',-1);
	else
		cs=cs(:);
		tnot=get(htwoway,'userdata');
		dat=get(hcstime,'userdata');
		zcs=dat(:,1);
		tcs=dat(:,2);
		set(hax,'userdata',[cs' nan tnot nan zcs' nan tcs']);
	end
	transfer=get(hdone,'userdata');
	figure(hmasterfig);
	eval(transfer);
	close(hfig);
end
%
% the fini step
%
if(strcmp(action,'fini'))
	hax=get(gcf,'currentaxes');
	dat=get(hax,'userdata');
	if(dat==-1)
		cson=-1;
		tshft=[];
		tcsp=[];
		zcsp=[];
	else
		ind=find(isnan(dat));
		cson=dat(1:ind(1)-1);
		tshft=dat(ind(1)+1);
		zcsp=dat(ind(2)+1:ind(3)-1);
		tcsp=dat(ind(3)+1:length(dat));
		cson=cson(:);
		zcsp=zcsp(:);
		tcsp=tcsp(:);
	end
	return;
end