function eqndef(action,arg2)

% eqndef is the central part of a dialog used by LOGEDIT to define
% the algebraic equation used to compute a new log from existing ones.
% Inititate the dialog by calling eqndefINIT and finish
% it by calling EQNDEFINI.
%
% G.F. Margrave Jan 1996
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

if( strcmp(action,'init'))
	stuff=get(gca,'userdata');
% 	ind=find(isnan(stuff));
% 	if( isempty(ind) )
% 		ind=0;
% 	end
% 	transfer=setstr(stuff(1:ind(1)-1));
% 	lognames=setstr(stuff(ind(1)+1:ind(2)-1));
% 	
% 	hmasterfig=stuff(ind(2)+1);
% 	logdefaults=stuff(ind(2)+2:ind(2)+5);
% 	coefs=stuff(ind(2)+6:ind(2)+9);
% 	exps=stuff(ind(2)+10:ind(2)+14);
% 	
% 	newlogdes=setstr(stuff(ind(3)+1:length(stuff)));
    transfer=stuff{1};
    lognames=stuff{2};
    hmasterfig=stuff{3};
    logdefaults=stuff{4};
    coefs=stuff{5};
    exps=stuff{6};
    newlogdes=stuff{7};

	
	fgkol=[0 0 0];
	bgkol=[0 1 1];

	%make a new figure
	hfig=figure('visible','off');
    set(hfig,'menubar','none');
    set(hfig,'name','LOGEDIT: equation definition','numbertitle','off');

	pos=get(hfig,'position');

	height=20;
	figheight=8.5*height;
	
	figwidth=402;
	sep=1;
	xnow=sep;
	ynow=figheight-height-sep;
	width=figwidth - 2*sep;
	htitle=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Log Algebra Equation Specifications',...
		'foregroundcolor','r');

	xnow=sep;
	ynow=ynow-height-sep;
	msg=' Eqn form: newlog = ((a*log1^k + b*log2^m)/(c*log3^n+d*log4^o))^p ';
	
	hmsg=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		msg);

	xnow=sep;
	ynow=ynow-height-sep;
	width=50;
	hlog1lbl=uicontrol('style','text','string','Log1: ','position',...
		[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=150;
	hlog1=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',logdefaults(1),'backgroundcolor',bgkol,'foregroundcolor',fgkol);
		
	xnow=xnow+width+sep;
	width=50;
	hlog2lbl=uicontrol('style','text','string','Log2: ','position',...
		[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=150;
	hlog2=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',logdefaults(2),'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	
	xnow=sep;
	ynow=ynow-height-sep;
	width=50;
	hlog3lbl=uicontrol('style','text','string','Log3: ','position',...
		[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=150;
	hlog3=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',logdefaults(3),'backgroundcolor',bgkol,'foregroundcolor',fgkol);
		
	xnow=xnow+width+sep;
	width=50;
	hlog4lbl=uicontrol('style','text','string','Log4: ','position',...
		[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=150;
	hlog4=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',logdefaults(4),'backgroundcolor',bgkol,'foregroundcolor',fgkol);		


	xnow=sep;
	ynow=ynow-height-sep;
 	width=50;
 	hcoeflbl=uicontrol('style','text','string','a,b,c,d: ','position',...
		[xnow,ynow,width,height]);

 	xnow=xnow+width+sep;
 	width=180;
 	hcoef=uicontrol('style','edit','string',...
 		[num2str(coefs(1)) ' ' num2str(coefs(2)) ' ' num2str(coefs(3)) ' '...
 		num2str(coefs(4)) ' '],...
 		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		
	
	xnow=xnow+width+sep;
 	width=60;
 	hexplbl=uicontrol('style','text','string','k,m,n,o p: ','position',...
		[xnow,ynow,width,height]);

 	xnow=xnow+width+sep;
 	width=110;
 	hexp=uicontrol('style','edit','string',...
 		[num2str(exps(1)) ' ' num2str(exps(2)) ' ' num2str(exps(3)) ' '...
 		num2str(exps(4)) ' ' num2str(exps(5)) ' '],...
 		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);

 	xnow=sep;
	ynow=ynow-height-sep;
 	width=100;
	htypelbl=uicontrol('style','text','string','New Log Type:','Position',...
		[xnow,ynow,width,height]);
	
	xnow=xnow+sep+width;
	width=200;
	[lm,ln]=logtype2las;
	htypes=uicontrol('style','popupmenu','string',ln,'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		
	xnow=sep;
	ynow=ynow-height-sep;
 	width=150;
	hdeslbl=uicontrol('style','text','string','New Log Description:','Position',...
		[xnow,ynow,width,height]);
	
	xnow=xnow+sep+width;
	width=250;
	hdes=uicontrol('style','edit','string',newlogdes,'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		
	%the buttons
	xnow=sep;
	ynow=ynow-height-sep;
	width=60;
	% the done button is enabled when the mode is switched to compute
	hdone=uicontrol('style','pushbutton','string','Done','position',...
		[xnow,ynow,width,height],'callback','eqndef(''done'');',...
		'userdata',transfer,'enable','on');

	xnow=xnow+width+sep;
 	width=60;
	hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
		[xnow,ynow,width,height],'callback','eqndef(''cancel'');');

	set(hfig,'userdata',[hmasterfig,htitle,hlog1,hlog2,hlog3,hlog4,...
		hcoef, hexp, htypes, hdone, hdes]);
	
	pospar=get(hmasterfig,'position');

	px=pospar(1)+pospar(3)/2-figwidth/2;
	py=pospar(2)+pospar(4)/2-figheight/2;			
	set(hfig,'position',[px py figwidth figheight]);
	set(hfig,'visible','on');

	return;

end

%
% handle the done button
%
if(strcmp(action,'done'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	hmsg=h(2);
	hlog1=h(3);
	hlog2=h(4);
	hlog3=h(5);
	hlog4=h(6);
	hcoef=h(7);
	hexp=h(8);
	htypes=h(9);
	hdone=h(10);
	hdes=h(11);
	
	hax=get(hmasterfig,'currentaxes');
	
	bgkol=[1 0 0];
	fgkol=[1 1 0];
	
	%get the logs
	log1=get(hlog1,'value');
	log2=get(hlog2,'value');
	log3=get(hlog3,'value');
	log4=get(hlog4,'value');
	
	%determine coefficients
	cstr=get(hcoef,'string');
	cs=sscanf(cstr,'%g');
	
	if(length(cs)~=4)
		set(hmsg,'string','You must provide four coeficients (a,b,c,d)',...
		'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		return;
	end
	
	% determine the exponents
	estr=get(hexp,'string');
	es=sscanf(estr,'%g');
	
	if(length(es)~=5 )
		set(hmsg,'string','You must provide five exponents (k,m,n,o,p)',...
		'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		return;
	end
	
	%check for zero divide
	test=cs(3)*log3^es(3) + cs(4)*log4^es(4);
	if( abs(test) < 10000*eps )
		set(hmsg,'string','Invalid equations. Zero divide will result',...
		'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		return;
	end
	
	%get the log type
	type=get(htypes,'value')-2;
	
	%get the descriptor
	des=get(hdes,'string');
	if( strcmp(des,'') )
		set(hmsg,'string','Provide a non-blank descriptor',...
		'backgroundcolor',bgkol,...
		'foregroundcolor',fgkol);
		return;
	end
	
	%store this stuff
		
	set(hax,'userdata',[[log1 log2 log3 log4] cs' es' type abs(des)]);
	 
	%call the transfer function
	transfer=get(hdone,'userdata');
	close(gcf);

	eval(transfer);
	
	return;
end

% 
% do a cancel
% 
if(strcmp(action,'cancel'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hdone=h(10);
	
	transfer=get(hdone,'userdata');
	
	hax=get(hmasterfig,'currentaxes');
	
	set(hax,'userdata',-1);
	
	close(gcf);
	
	eval(transfer);
	
	return;
	
end