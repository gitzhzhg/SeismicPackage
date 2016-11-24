function savething(action)
% G.F. Margrave Feb 1994
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
if(strcmp(action,'init'))
		hax=gca;
		qs=get(hax,'userdata');
		%build the dialog box
		hdial=figure('visible','off','menubar','none');
		pos=get(hdial,'position');
		sep=1;
		nrows=5;
		%
		% assume 6 chars in 50 pixesl
		%
		charpix=6;
		%
		% now the first question
		%
		q=qs(1,:);
		ind=find(q==1);
		if(~isempty(ind))
			q=q(1:ind(1)-1);
		end
		width=50*ceil(length(q)/charpix);
		height=20;
		figheight=(height+sep)*(nrows);
		maxwidth=width;
		ynow=figheight-height-sep;
		xnow=sep;
		hq=uicontrol('style','text','string',char(q),...
					'position',[xnow ynow width height],...
					'foregroundcolor','r');
		% the yes/no toggles
		width=70;
		ynow=ynow-sep-height;
		hyes=uicontrol('style','checkbox','string','Yes','position',...
			[xnow,ynow,width,height],'callback','savething(''toggle'')');
		xnow=xnow+sep+width;
		hno=uicontrol('style','checkbox','string','No','position',...
			[xnow,ynow,width,height],'callback','savething(''toggle'')');
		if(xnow+width>maxwidth) maxwidth=xnow+width; end
		% a message
		ynow=ynow-sep-height;
		xnow=sep;
		width=200;
		hmsg=uicontrol('style','text','string','Change name if desired:',...
			'position',[xnow,ynow,width,height],'visible','off',...
			'foregroundcolor','r');
		if(xnow+width>maxwidth) maxwidth=xnow+width; end
		ynow=ynow-height-sep;
		q=qs(2,:);
		ind=find(q==1);
		if(~isempty(ind))
			q=q(1:ind(1)-1);
		end
		hname=uicontrol('style','edit','string',char(q),'position',...
			[xnow,ynow,width,height],'visible','off',...
			'backgroundcolor','c');
		%set user data on the toggles
		set(hyes,'userdata',[hno hmsg hname]);
		set(hno,'userdata',[hyes hmsg hname]);
		% ok button
		ynow=ynow-height-sep;
		width=40;
		hok=uicontrol('style','pushbutton','string','OK','position',...
			[xnow,ynow,width,height],'callback','savething(''ok'')',...);
			'foregroundcolor','r');
		% cancel button
		xnow=xnow+width+sep;
		width=60;
		hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
			[xnow,ynow,width,height],'callback','savething(''ok'')');
		transfer=qs(3,:);
		ind=find(transfer==1);
		if(~isempty(ind))
			transfer=transfer(1:ind(1)-1);
		end
		set(gcf,'userdata',[hax hyes,hno,hname,hq hmsg nan abs(transfer)]);
		%get the position of the calling figure
		hparent=get(hax,'parent');
		pospar=get(hparent,'position');
		px=pospar(1)+pospar(3)/2;
		py=pospar(2)+pospar(4)/2;
		figwidth=maxwidth;
		pp=get(hq,'position');
		pp(3)=figwidth;
		set(hq,'position',pp);
		set(gcf,'position',[px py figwidth figheight],'visible','on');
		return;
	end
	%
	% toggle the yes no buttons
	%
	if(strcmp(action,'toggle'))
		htoggle=gco;
		flag=get(htoggle,'string');
		dat=get(htoggle,'userdata');
		if( strcmp(flag,'Yes') )
			set(dat(1),'value',0);
			set(dat(2),'visible','on');
			set(dat(3),'visible','on');
		else
			set(dat(1),'value',0);
			set(dat(2),'visible','off');
			set(dat(3),'visible','off');
		end
		return;
	end
	%
	% handle the OK button
	%
	if(strcmp(action,'ok'))
		h=get(gcf,'userdata');
		hbutton=gco;
		flag=get(hbutton,'string');
		if(strcmp(flag,'OK'))
			y=get(h(2),'value');
			n=get(h(3),'value');
			if( ~y & ~n )
				set(h(5),'string','Please CHOOSE Yes or No');
				return;
			end
			if( y )
				% get the name
				name=get(h(4),'string');
				%remove leading or trailing blanks
				iii=find(abs(name)~=32);
				ib=find(abs(name)==32);
				ib2=find(ib<iii(1));
				if(~isempty(name))
					name(ib(ib2))=[];
					iii=find(abs(name)~=32);
					ib=find(abs(name)==32);
					ib2=find(ib>iii(length(iii)));
					if(~isempty(name))
						name(ib(ib2))=[];
					end
				end
				if(isempty(name))
					set(h(6),'string','Provide a NON-BLANK name');
					return;
				end
			else
				name=0;
			end
		else
			name=-1;
		end
		set(h(1),'userdata',name);
		close(gcf);
	% call the transfer expression
		transfer=setstr(h(find(isnan(h))+1:length(h)));
		eval(transfer);
		return;
	end