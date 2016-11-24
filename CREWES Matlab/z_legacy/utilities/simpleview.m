function simpleview( arg )
% simpleview
%
% simpleVIEW provides an interactive, point&click interface to viewing 
% any 3-D axes and altering its azimuth and elevation of the view
%
% G.F. Margrave, September 1994
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
% determine input mode
 if(nargin < 1)
		action='initialize';
 elseif( nargin==1 & isstr(arg) )
		action = arg;
 end
% user data assignments
%
% figure... the handles of all of the uicontrols:
%	[hdoit,hazLabel,haz,helLabel,hel,hquit]
%
% hdoit ... [x y z]
%
% hazLabel ... not used
%
% haz ... not used
%
% helLabel ... not used
%
% hel ... not used
%
% hquit ... not used
%
% initialize the viewer
if( strcmp(action,'initialize') )
% get the orientation of the current view
 [az,el]=view;
	if( abs(az) > 180. )
		az = az-360.;
	end
	hfig1 = gcf;
	
	% make some controls
	sep = 2;
	xnow = sep;
	ynow = sep;
	width = 50;
	height = 20;
% store the input grid in the doit button
	hdoit = uicontrol('style','pushbutton','string','Doit',...
		'position', [xnow,ynow,width,height],'callback',...
		'simpleview(''plot'')','visible','off');
	%xnow = xnow+sep+width;
	width = 75;
	hazLabel = uicontrol('style','text','position',...
		[xnow,ynow,width,height]);
	
	xnow = xnow+sep+width;
	width = 90; height = 20;
	haz = uicontrol('style','slider','min',-180,'max',180,...
	'position',[xnow,ynow,width,height],'callback',...
	'simpleview(''az'');simpleview(''doit'')','value',az);
 
	xnow = xnow+sep+width;
	width = 75;
	height = 20;
	helLabel = uicontrol('style','text','position',...
		[xnow,ynow,width,height]);
	
	xnow = xnow+sep+width;
	width = 90;
	height = 20;
	hel = uicontrol('style','slider','min',0,'max',90,...
	'position',[xnow,ynow,width,height],'callback',...
	'simpleview(''el'');simpleview(''doit'')','value',el);
	xnow = xnow+width+sep;
	width = 50;
	height = 20;
	hquit = uicontrol('style','pushbutton','string','Close','position',...
		[xnow,ynow,width,height],'callback','simpleview(''quit'')');
		
% store the uicontrol handles in the figure
	set(gcf,'userdata',[hdoit,hazLabel,haz,helLabel,hel,hquit]);
	% set the slider labels
	simpleview('az');
	simpleview('el');
	set(gca,'box','on');
	grid on
	return;
end
% the doit button
if( strcmp( action,'doit') )
	h = get(gcf,'userdata');
	haz = h(3);
	hel = h(5);
	az = get(haz,'value');
	el = get(hel,'value');
	view(az,el);
	set(gca,'box','on');
end
% change the azimuth display
if( strcmp(action,'az') )
	h = get(gcf,'userdata');
	hazLabel = h(2);
	haz=h(3);
	az = get(haz,'value');
	if( abs(az) > 180. )
		az = az-360.;
	end
	az = round(az);
	azLabel = sprintf('Az= %d',az);
	set( hazLabel,'string',azLabel);
end
% change the elevation display
if( strcmp(action,'el') )
	h = get(gcf,'userdata');
	helLabel = h(4);
	hel=h(5);
	el = get(hel,'value');
	el = round(el);
	elLabel = sprintf('Elev= %d',el);
	set( helLabel,'string',elLabel);
end
% the quit callback
if( strcmp(action,'quit') )
	  close(gcf);
end