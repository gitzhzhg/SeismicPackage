function simplezoom(arg,arg2)
% SIMPLEZOOM: figure zooming utility using SELBOX
%
% simplezoom(button,transfer)
%
% Installs simple zooming action on the current figure. To
% Zoom any 2-D axes, simply make sure the axes parent figure is
% current and type: simplezoom
% at the matlab prompt to install the zooming function. Then simply
% draw a rectangle by clicking and dragging the mouse. The axis will
% 'zoom' at the completion of the draw. To unzoom, enter a rectangle
% of zero size which is done by a simple mouse click without any 
% motion.
% button ... refers to the mouse button to be used to draw
%		the zoom box. Any other button will cause no action.
%  ********* Button default is 1 ***********
% Transfer ... is a string containing any legal matlab command
% 		which is to be called at the completion of the zoom. 
%		This is key to using SIMPLEZOOM in a larger program so that
%		control can be returned to the larger program following
%		the zoom.
% ********** Default is '' ************
%
% G.F. Margrave, Feb 1994
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

if(nargin<2)
	button=1;
	arg2='';
else
	button=arg2;
end

if(nargin<1)
	arg=1;
else
	action=arg;
end

if( ~ischar(arg) )
	% initialize
	%
	if(~ischar(arg2)) cmd=''; else cmd=arg2; end
	if(arg==1)
		selboxinit(['simplezoom(''zoomit'',1);' arg2],arg);
	elseif(arg==2)
		selboxinit(['simplezoom(''zoomit'',2);' arg2],arg);
	elseif(arg==3)
		selboxinit(['simplezoom(''zoomit'',3);' arg2],arg);
	end
	return;
end

if( strcmp(action,'zoomit') )
 % determine the button type
 flag=get(gcf,'selectiontype');
 go=0;
 if( strcmp(flag,'normal') && button==1)
		go=1;
	elseif( strcmp(flag,'extend') && button==2)
		go=1;
	elseif( strcmp(flag,'alt') && button==3)
		go=1;
	end
	if(go)
		box=selboxfini;
        
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
		if(abs(test1)<10*eps || abs(test2)< 10*eps)
			axis('auto')%unzoom
		else
			set(gca,'xlim',[xmin xmax],'ylim',[ymin ymax]);
		end
	end
	return;
end