function textmove(action)
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
	set(gcf,'windowbuttondownfcn','textmove(''init'')');
	return;
end
if(strcmp(action,'init'))
	hobj=gco;
	if( strcmp(get(hobj,'type'),'text') )
			set(gcf,'windowbuttonmotionfcn','textmove(''move'')');
			set(gcf,'windowbuttonupfcn','textmove(''fini'')');
			dat1=get(hobj,'units');
			dat2=get(gcf,'units');
			set(hobj,'units','pixels');
			set(gcf,'units','pixels');
			pos=get(hobj,'position');
			xobj=pos(1);
			yobj=pos(2);
			pt=get(gcf,'currentpoint');
			xmouse=pt(1,1);
			ymouse=pt(1,2);
			dx=xobj-xmouse;
			dy=yobj-ymouse;
			set(gca,'userdata',[hobj dx dy abs(dat1)...
			abs(nan) abs(dat2)]);
			return;
	end
	return;
end
if(strcmp(action,'move'))
 objdat=get(gca,'userdata');
 hobj=objdat(1);
 dx=objdat(2);
 dy=objdat(3);
 pt=get(gcf,'currentpoint');
 pos=get(hobj,'position');
 pos(1)=pt(1,1)+dx;
 pos(2)=pt(1,2)+dy;
 set(hobj,'position',pos);
 return;
end
if(strcmp(action,'fini'))
	objdat=get(gca,'userdata');
	set(gca,'userdata',[]);
	hobj=objdat(1);
	%dat=objdat(4:length(objdat));
	%ii=find(isnan(dat));
	set(hobj,'units','pixels');
	set(gcf,'units','pixels');
	set(gcf,'windowbuttonmotionfcn','');
	set(gcf,'windowbuttonupfcn','');
	return;
end