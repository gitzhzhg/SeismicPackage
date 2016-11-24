function yesno(action)
%
%
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

%

if(strcmp(action,'init'))
		hax=gca;
		qs=get(hax,'userdata');

		%build the dialog box
		hdial=figure('visible','off','menubar','none');
		pos=get(hdial,'position');
		sep=1;
		nrows=2;

		%
		% assume 6 chars in 50 pixesl
		%
		charpix=6;
		%
		% now the question
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
					'position',[xnow ynow width height]);

		% the yes/no buttons

		width=40;
		ynow=ynow-sep-height;

		hyes=uicontrol('style','pushbutton','string','Yes','position',...
			[xnow,ynow,width,height],'callback','yesno(''button'')');
		xnow=xnow+sep+width;

		hno=uicontrol('style','pushbutton','string','No','position',...
			[xnow,ynow,width,height],'callback','yesno(''button'')');

		
		% cancel button

		xnow=xnow+sep+width;
		width=60;
		hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
			[xnow,ynow,width,height],'callback','yesno(''button'')');

		if(xnow+width>maxwidth) maxwidth=xnow+width; end

		transfer=qs(3,:);
		ind=find(transfer==1);
		if(~isempty(ind))
			transfer=transfer(1:ind(1)-1);
		end
		set(gcf,'userdata',{hax char(transfer)});

		%get the position of the calling figure
		hparent=get(hax,'parent');
		pospar=get(hparent,'position');
		figwidth=maxwidth;
		px=pospar(1)+pospar(3)/2-figwidth/2;
		py=pospar(2)+pospar(4)/2-figheight/2;
		

		set(gcf,'position',[px py figwidth figheight],'visible','on');

		return;

	end

	%
	% handle a button
	%
	if(strcmp(action,'button'))
		h=get(gcf,'userdata');

		hbutton=gco;

		flag=get(hbutton,'string');

		if(strcmp(flag,'Yes'))
			reply=1;
		elseif(strcmp(flag,'No'))
			reply=0;
		else
			reply=-1;
		end
		
		set(h{1},'userdata',reply);

		close(gcf);

	% call the transfer expression
		transfer=h{2};

		eval(transfer);

		return;

	end