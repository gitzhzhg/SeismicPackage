function matace(action)
% matace
%
% MATACE takes and returns no arguments. It simple puts up a panel
% of pushbuttons designed to run chevron applications
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
if(nargin<1)
	action='init';
end
if(strcmp(action,'init'))
	hfig=figure('visible','off');
	p=get(hfig,'position');
	nrows=4;
	% 
	% current applications:
	%	logedit
	%	logsec
	%	slicetool
	%	slicetool('extract')
	%	waveleted
	%
	sep=.01;
	ht=(1-6*sep)/nrows;
	width=(1-3*sep)/2;
	xnow=sep;
	ynow=1-ht;
 wd=2*width+sep;
	hmode=uicontrol('style','popupmenu','string',...
		'Click button to launch program|Click button for help on program',...
		'units','normalized',...
		'position',[xnow,ynow,wd,.9*ht],'foregroundcolor','r');
	ynow=ynow-ht-sep;
	hlogedit=uicontrol('style','pushbutton','string','logedit',...
		'units','normalized',...
		'position',[xnow,ynow,width,ht],'callback',...
		'matace(''logedit'')');
	xnow=xnow+width+sep;
	hwaveleted=uicontrol('style','pushbutton','string','waveleted',...
		'units','normalized',...
		'position',[xnow,ynow,width,ht],'callback',...
		'matace(''waveleted'')');
	ynow=ynow-sep-ht;
	xnow=sep;
	hlogsec=uicontrol('style','pushbutton','string','logsec',...
		'units','normalized',...
		'position',[xnow,ynow,width,ht],'callback',...
		'matace(''logsec'')');
	xnow=xnow+width+sep;
	hlogsecnew=uicontrol('style','pushbutton','string','logsec(''new'')',...
		'units','normalized',...
		'position',[xnow,ynow,width,ht],'callback',...
		'matace(''logsecnew'')');
	ynow=ynow-sep-ht;
	xnow=sep;
	hslicetool=uicontrol('style','pushbutton','string','slicetool',...
		'units','normalized',...
		'position',[xnow,ynow,width,ht],'callback',...
		'matace(''slicetool'')');
	xnow=xnow+width+sep;
	hslicextract=uicontrol('style','pushbutton',...
		'string','slicetool(''extract'')',...
		'units','normalized',...
		'position',[xnow,ynow,width,ht],'callback',...
		'matace(''slicextract'')');
	p=[p(1:2) 400 100];
	set(hfig,'userdata',[hmode hlogedit hwaveleted hlogsec hlogsecnew...
		hslicetool hslicextract]);
	set(hfig,'position',p,'visible','on');
	return;
end
if( strcmp(action,'logedit') )
	%determine mode
	h=get(gcf,'userdata');
	hmode=h(1);
	mode=get(hmode,'value');
	if(mode==1)
	
		disp('Running logedit... Please wait a moment')
		eval('logedit');
	else
		disp(' ')
		disp(['LOGEDIT is an interactive log editor which reads and writes'])
		disp('logs in LAS format. It allows general editing of log samples')
		disp('as well as such tasks as resampling, median filtering, units')
		disp('change and more. It will also convert logs from depth to time')
		disp('and output them in a form which SEISLINE can read. For more')
		disp('info or personal instruction call Gary Margrave @ 5393')
		disp(' ')
	end
	return;
end
if( strcmp(action,'logsec') )
	%determine mode
	h=get(gcf,'userdata');
	hmode=h(1);
	mode=get(hmode,'value');
	if(mode==1)
	
		disp('Running logsec... Please wait a moment')
		eval('logsec');
	else
		disp(' ')
		disp('Use this button to run LOGSEC when you are opening a saved')
		disp('file from a previous LOGSEC session')
		disp('LOGSEC is an interactive program designed to propagate well')
		disp('logs along a cross section, convert them from depth to time')
		disp('and eventually make synthetic seismograms. It contains a')
		disp('complete cross section construction package, interactive')
		disp('log propagation, datum shifting, time converssion. It ')
		disp('invokes the wavelet editor, waveleted, to construct')
		disp('wavelets for the theograms. For more information or personal')
		disp('instruction, call Gary Margrave @ 5393')
		disp(' ')
	end
	return;
end
if( strcmp(action,'logsecnew') )
	%determine mode
	h=get(gcf,'userdata');
	hmode=h(1);
	mode=get(hmode,'value');
	if(mode==1)
	
		disp('Running logsec(''new'')... Please wait a moment')
		eval('logsec(''new'')');
	else
		disp(' ')
		disp('Use this button to run LOGSEC when you are beginning a new')
		disp('LOGSEC model')
		disp('LOGSEC is an interactive program designed to propagate well')
		disp('logs along a cross section, convert them from depth to time')
		disp('and eventually make synthetic seismograms. It contains a')
		disp('complete cross section construction package, interactive')
		disp('log propagation, datum shifting, time converssion. It ')
		disp('invokes the wavelet editor, waveleted, to construct')
		disp('wavelets for the theograms. For more information or personal')
		disp('instruction, call Gary Margrave @ 5393')
		disp(' ')
	end
	return;
end
if( strcmp(action,'slicetool') )
	%determine mode
	h=get(gcf,'userdata');
	hmode=h(1);
	mode=get(hmode,'value');
	if(mode==1)
	
		disp('Running slicetool... Please wait a moment')
		eval('slicetool');
	else
		disp(' ')
		disp('Use this button to run SLICETOOL when you are opening a saved')
		disp('file from a previous SLICETOOL session')
		disp('SLICETOOL is an interactive cross plotting and analysis tool')
		disp('for columnar (i.e. vector) data. It provides facilities for')
		disp('crossplotting vectors, fitting polynomials to them (and ')
		disp('computing residuals if desired), performing basic algebra')
		disp('on the vectors and a whole lot more.')
		disp('For more information or personal instruction call Gary')
		disp('Margrave @ 5393')
		disp(' ')
	end
	return;
end
if( strcmp(action,'slicextract') )
	%determine mode
	h=get(gcf,'userdata');
	hmode=h(1);
	mode=get(hmode,'value');
	if(mode==1)
	
		disp('Running slicetool(''extract'')... Please wait a moment')
		eval('slicetool(''extract'')');
	else
		disp(' ')
		disp('Use this button to run SLICETOOL when you are opening a saved')
		disp('file from a previous SLICETOOL session')
		disp('SLICETOOL is an interactive cross plotting and analysis tool')
		disp('for columnar (i.e. vector) data. It provides facilities for')
		disp('crossplotting vectors, fitting polynomials to them (and ')
		disp('computing residuals if desired), performing basic algebra')
		disp('on the vectors and a whole lot more.')
		disp('For more information or personal instruction call Gary')
		disp('Margrave @ 5393')
		disp(' ')
	end
	return;
end
	
if( strcmp(action,'waveleted') )
	%determine mode
	h=get(gcf,'userdata');
	hmode=h(1);
	mode=get(hmode,'value');
	if(mode==1)
	
		disp('Running waveleted... Please wait a moment')
		eval('waveleted');
	else
		disp(' ')
		disp('WAVELETED is an interactive waveleteditor.')
		disp('It provides the ability to create new wavelets from theory')
		disp('to model simple impulsive or vibroseis sources or to import')
		disp('a wavelet from an ASCII flat file. Existing wavelets can be')
		disp('altered in a variety of ways including phase rotation,')
		disp('inversion, ghosting, minimum or zero phasing etc.')
		disp('For more information or personal instruction call Gary')
		disp('Margrave @ 5393')
		disp(' ')
	end
	return;
end
	