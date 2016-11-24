function slicetool(arg1,arg2)
% SLICETOOL provides interactive 2-D viewing and manipulation of a random 
% Earth Object. Slicetool may be invoked in any of the following ways:
% 1) slicetool 
%    If no input, SLICETOOL puts up a file input panel which can be used
%    to direct the program to an object saved on disk. This should be a
%    MATLAB .mat file which can contain an object of any of the three types
%    mentioned above.
%
% 2) slicetool('extract') 
%    causes SLICETOOL to put up a file input panel which is used to direct
%    the program to an ASCII file containing SEISLINE data and created by
%    EXTRACT_RASTERS. SLICETOOL will read this file and create and object
%    of type iii) (see below) which is then used to initiate automatically
%    a SLICEMASTER panel.
% This method has been disabled by comment out the line which calls
% the extract function.
%
% 3) slicetool(object) 
%    invokes SLICETOOL for work on "object" which may be any of: 
%       i) a random earth object, ii) a container object containing a single
%          random object whose datatype is 'line' and whose container label 
%          (name) is 'randobj' and any number of other objects with auxiliary 
%          information 
%     iii) a container object containing any number of containers of type 'ii' 
%          whose datatype has been set to 'slce'. In the latter two cases, 
%          the containers should also store a container named 'file' which 
%          has fields called 'filename' and 'pathname' to direct the saveing
%          of datasets. In case iii) SLICETOOL launches a control panel 
%          called 'SLICEMASTER' from which any of the contained	slices can
%          be launched in SLICETOOL windows and then pauses.
%
% Related MATLAB programs include EXTRACT, SEIS2WELL, GRIDTOOL, READZMAP, 
% and LOGSEC.
%
% by G.F. Margrave, Jan-March 1994 
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
%note that arg2 is used by SLICEMASTER to provde its figure number to SLICETOOL.
if( nargin < 2)
	smfig=[];
else
	smfig=arg2;
end
if( nargin < 1)
   % get the input file
   [filename,path] = uigetfile('*.mat','Select Saved Session');
   
        if( isempty(filename) )
                error('no file name given');
        end
        if( filename==0 )
                error('no file name given');
        end
        ind = findstr(filename,'.mat');
        if( length(ind)>0 )
       filename=filename(1:ind-1);
   else
      error(' invalid file extension');
   end
        fullfilename = [path filename];
   % load the file
   if( strcmp(computer,'MAC2') )
      eval(['load ' filename]);
   else
      eval(['load ' fullfilename]);
   end
   %copy it into object
   eval(['object=' filename ';']);
   eval([filename '=[];']);
   % because all relevant variables are stored in this file, this will
   % restore them under their proper name.
   % set action to indicate this status
   action = 'initialize';
	  arg1=[];
 end
if( isstr(arg1) )
	action = arg1;
elseif( ~isempty(arg1) )
	action='initialize';
	object=arg1;
end
%test for extract invocation
if(strcmp(action,'extract'))
		% make a temp figure
		htmp=figure('visible','off');
		%ask for the extraction increment
		askthingsinit('slicetool(''extract2'')','Extract_rasters trace increment?',...
			'1');
		return;
end
	if(strcmp(action,'extract2'))
	  a=askthingsfini;
	  % test for cancel
	  if( a== -1 )
		error(' Trace increment not supplied');
	  end
	  inc=sscanf(a,'%d');
% get the input file
   [filename,path] = uigetfile('*.dat','Select extract_raster file');
   
    if( isempty(filename) )
                error('no file name given');
    end
    if( filename==0 )
                error('no file name given');
    end
   ind = findstr(filename,'.dat');
   if( isempty(length(ind)) )
      error(' invalid file extension. Filename must be *.dat');
   end
   fullfilename = [path filename];
   % run extract
	  %object=extract(fullfilename,inc);
   % EXTRACT creates a container object with multiple lines inside which will
   % cause slicetool to launch SLICEMASTER
   % set action to indicate this status
   action = 'initialize';
   arg1=[];
	  delete(gcf);
 end	
if( strcmp(action,'initialize') )
% test what kind of object we have
flag=objget(object,'objtype');
if( strcmp(flag,'cont') )
 % see if it has any slices in it
 test=objget(object,'namesmatrix','slce');
 if( ~isempty(test) )
		slicemaster(object);
		return;
 else
		test=objget(object,'namesmatrix','line');
		if( isempty(test) )
			error('unknown object in slicetool');
		end
	end
end
	% open up a new figure
	hfig = figcent(.6,.6); set(hfig,'menubar','none');
%
% userdata assignments
%
%hfig = handle of the main figure ... 
%		[hdoit hoptions hflipx hflipy hhold hactions ...
%		hxaxis hyaxis hquit hmode hmessage htranstore...
%		hcmplxstore hgrid hcomplex hpolyorder houtput houtopts...
%		hpolystore hscale hslopetools hautopro halgebra hpromote...
%		hdefineqn hscatter hshowfile hsaveas hup hdown...
%		hleft hright hpolydisp hautoident haltlabels haltx...
%		halty hpolyeval hptsize hhardcopy hnumcolors hpolygen...
%		hsmooth hdefinesm hderiv]);
%
% hdoit = handle of the doit button ... the input object
%
% hoptions = handle of the options menu ... temp storage of windowbutton functions
%			while zooming
%
% hactions = handle of the actions menu ... a number indicating the current action
%	and a list of the submenu handles: [1 hplot,hzoom,hunzoom,hpickslopes,hpolyfit
%	,hidentify]
%
% hidentify = handle of the identify action menu ... the number 7 followed by up
%	to 2n graphics handles (n== the number of data fields in the object). If ndisp
%	is the number of displayed fields, then there will be ndisp curve handles and
%   ndisp text label handles for the first subplot and the same for the second in a 
%	second row of the userdata. If the is i subplot, then the size of the userdata
%	is [1,2*ndisp+1] and for 2 subplots it is [2,2*ndisp+1]. Position (2,1) is not
%	used.
%
% hzoom = handle of zoom menu ... the coordinates of the zoom box
% 
% hpickslopes = handle of the pickslopes menu ... latest slope pick
%
% hpolyfit = handle of the polyfit menu ... athe number 6 followed by a vector of curve
%	handles and a vector of id numbers for the polynomila fits displayed on screen
%	Gets reset everytime a plot with no hold is made
%
% hpolystore = handle of the polynomial coeficients storage bin. ... a curve id number,
%	followed by an integer giving the number of coeficients, followed by that many 
%       coeficients. This is repeated for any number of curves on screen. 
%	Gets reset everytime a new polynomial fit is done.
%
% htransform = handle of the transform menu ... transforms of the displayed variables???
%
% hxaxis = handle of the x axis menu ... the number of data items, the handles of the
% 	submenus, option menu handle, and the numbers of any that are checked (on)
%        [num vector_of_handles option_handle first_item second_item ...]
%    If there are n data fields in the object being displayed, and if the userdata is
%	called xinfo, then the numbers of the displayed data fields are given by
%		xinfo(n+3:2n+3) or equivalently xinfo(xinfo(1)+3:length(xinfo)) since 
%		xinfo(1)==n.
%
% hyaxis = handle of the y axis menu ... the number of data items, the handles of the
% 	submenus, and the numbers of any that are checked (on)
%        [num vector_of_handles first_item second_item ...]
%
% hmode = handle of the mode menu ... a number indicating the mode (1=normal, 
% 	2=transform) and the handles of the submenus
%
% hmessage = handle of the message panel ...the original container object
%
% htranstore = handle of storage bucket for transfoms ... nx +ny columns of data
%	containing the complex transforms of the selected data
%
% hcmplxstore = handle of storage bucket for complex data ... nx +ny columns of data
%	processed as per the complex data option
%
% hgrid = handle of "grid on" menu ... the handle of the SLICEMASTER window (if any)
%	which is in control
%
% hcomplex = handle of the complex options menu ... integer indicating the current 
%		option and handles of all of the sub menus:
%			[flag hamplin hampdb hphase hampphs hrealimag]
%
% hpolyorder = handle of the polynomial order menu ... number indicating the 
%		current order and	a vector of submenu handles
%
% hautopro = handle of the auto promotion option menu ... command to be called
%		after auto promotion
% 
% halgebra = handle of the algebra menu ... integer indicationg the action and 
%		handle of curve1 in the current equation
%
% hpromote = handle of the promote menu ... interger indicating the action and
%		handle of the curve to be promoted
%
% hdefineqn = handle of the define equation menu ... [a b m n flag] where tha
%	allowed 	equation is of the form: a*(curve1).^m +|* b*(curve2).^n
%   If flag ==1, then + is used or if flag==2 then * is used.
%
% hscatter = handle of the scatterplot boolean menu ... the string 'slicetool' to allow
% 	positive id of the figure
%
% hshowfile ... not used
%
% hsaveas ... the SLICETOOL parameters
%
% hup ... not used
%
% hdown ... not used
%
% hleft ... not used
%
% hright ... not used
%
% hpolydisp ... not used
%
% hautoident ... not used
%
% haltlabels ... not used
%
% haltx ... name of the alternate label vector for the x axis
%
% halty ... name of the alternate label vector for the y axis
%
% hpolyeval ... not used
%
% hptsize ... the current scatterplot point size
%
% hhardcopy ... the last used scales for x & y
%
% hnumcolors ... the current number of colors
%
% hpolygen ... not used
%
% hsmooth ... handle of the smooth menu ... integer indicating the action and 
%		handle of curve to be smoothed
%
% hdefinesm = handle of the define smoother menu ... [n flag] where 
%   n is the number of points in the smoother, and flag for smoother type.
%   If flag ==1, then boxcar is used or if flag==2 then triangle is used.
%
% hderiv ... handle of the derivative menu ... integer indicating the action and 
%		handle of curve to be derivatived
% SLICETOOL saved parameters definition
%
% p(1) ... last xscale setting for hardcopy
% p(2) ... last yscale setting for hardcopy
% p(3) ... flipx setting
% p(4) ... flipy setting
% p(5) ... grid setting
% p(6) ... scatterplot setting
% p(7) ... scatterplot pt size
% p(8) ... 1-0 scaling setting
% p(9) ... auto promote setting
% p(10) ... display poly coeffs setting
% p(11) ... eval poly locally setting
% p(12) ... alt x axis setting
% p(13) ... number of the field proving the alternate x axis setting
% p(14) ... alt y axis setting
% p(15) ... number of the field proving the alternate y axis setting
% p(16) ... complex data option number
% p(17) ... mode option
% p(18) ... polynomial order
% p(19) ... number of the slope tool
% p(20) ... algebra eqn 'a'
% p(21) ... algebra eqn 'b'
% p(22) ... algebra eqn 'm'
% p(23) ... algebra eqn 'n'
% p(24) ... algebra eqn type (1... add 2...mult)
% p(25) ... y data option (0 for exclusive, 1 for accumulate)
% p(26) ... x data option (0 for exclusive, 1 for accumulate)
% p(27) ... autoidentify setting
% p(28) ... number of colors
% p(29) ... smoother length (number of points)
% p(30) ... smoother type (1... box 2... triangle)
%
% These will be stored in a container object called 'parameters' which is in 
% another container called 'slicetool_settings'
% Also in slicetool_settings will be 'data_settings' or dset 
% which will be a vector whose first entry tells the number of data fields and
% where length(dset)=1+dset(1)*nentries where nentries is the number of saved
% entries for each data vector. Currently, I plan to save:
% 	xflag ... whether the menu is checked in xdata
%   yflag ... whether the menu is checked in ydata
%   r ... the red color value for this data 
%   g ... the green color value for this data
%   b ... the blue color value for this data
% Or five entries for each data vector.
% see if there is a saved settings object
	sliceset=objget(object,'slicetool_settings');
	if( ~isempty(sliceset) )
		p=objget(sliceset,'parameters');
		olp=length(p);
		if(olp<30)
			p=[p zeros(1,30-length(p))];
		end
		if(olp<28) p(28)=6; end
		if(olp<29) p(29)=7; end
		if(olp<30) p(30)=1; end
	else
		% generate default parameters
		p=zeros(1,28);
		p(7)=12;
		p(16)=2;
		p(18)=1;
		p(19)=1;
		p(20)=1;
		p(21)=-1;
		p(22)=1;
		p(23)=1;
		p(24)=1;
		p(25:26)=[2 2];
		p(28)=6;
		p(29)=7;
		p(30)=1;
	end
		
% make a doit button
	sep=1;
	xnow=sep;
	ynow=sep;
	width=50;
	height=20;
	hdoit = uicontrol('style','pushbutton','string','Doit','callback',...
		'slicetool(''doit'')','position',[xnow,ynow,width,height]);
	% make a quit button
	xnow=sep+width+xnow;
	hquit = uicontrol('style','pushbutton','string','Quit','callback',...
		'slicetool(''quit'')','position',[xnow,ynow,width,height]);
		
	% make a message box
	
	xnow=sep+width+xnow;
	ynow=sep;
	figGeom = get(gcf,'position');
	width = figGeom(3)-width-5*sep;
	height=20;
	hmessage = uicontrol('style','text','string','','position',[xnow,ynow,width,height]);
		
	%make zoom scroll buttons
	ynow=ynow+height+sep;
	xnow=sep;
	width=40;
	hright=uicontrol('style','pushbutton','string','Right',...
		'callback','slicetool(''right'')','visible','off',...
		'position',[xnow,ynow,width,height]);
	ynow=ynow+height+sep;
	hleft=uicontrol('style','pushbutton','string','Left',...
		'callback','slicetool(''left'')','visible','off',...
		'position',[xnow,ynow,width,height]);
	ynow=ynow+height+sep;
	hdown=uicontrol('style','pushbutton','string','Down',...
		'callback','slicetool(''down'')','visible','off',...
		'position',[xnow,ynow,width,height]);
	ynow=ynow+height+sep;
	hup=uicontrol('style','pushbutton','string','Up',...
		'callback','slicetool(''up'')','visible','off',...
		'position',[xnow,ynow,width,height]);
	%see what kind of an object we have
	test=objget(object,'objtype');
	if(strcmp(test,'cont'))
		set(hmessage,'userdata',object);
		object=objget(object,'randobj');
	else
		co=contobj(objget(object,'name'),'slce');
		co=objset(co,'randobj',object);
		file=contobj('file','prvt');
		file=objset(file,'filename','undefined');
		file=objset(file,'pathname','   ');
		co=objset(co,'file',file);
		set(hmessage,'userdata',co);
		co=[];
	end
	set(hdoit,'userdata',object);
% make the output menu
	houtput = uimenu(gcf,'label','File');
	hallcurves = uimenu(houtput,'Label','Save','callback',...
		'slicetool(''save'')');
	hsaveas = uimenu(houtput,'Label','Save As','callback',...
		'slicetool(''saveas'')','userdata',p);
	hcurrcurves = uimenu(houtput,'Label','Save Current Curves','callback',...
		'slicetool(''out'')');
	hcurrpoly = uimenu(houtput,'Label','Save Current Poly Coeffs','callback',...
		'slicetool(''out'')','enable','off');
	houtopts = uimenu(houtput,'label','Output Options');
	houtcurve = uimenu(houtopts,'label','Output as Simple Curve','callback',...
		'slicetool(''exboolean'')','checked','off','enable','off');
	houtobj = uimenu(houtopts,'label','Output as Object','callback',...
		'slicetool(''exboolean'')','checked','on','userdata',...
		[2 houtcurve]);
	set(houtcurve,'userdata',[1 houtobj]);
	hmatfile = uimenu(houtopts,'label','In Matlab File Format','callback',...
		'slicetool(''exboolean'')','checked','on');
	hascii = uimenu(houtopts,'label','In ASCII File Format','callback',...
		'slicetool(''exboolean'')','checked','off','userdata',...
		[2 hmatfile],'enable','off');
	hshowfile= uimenu(houtopts,'label','Show Save File','callback',...
		'slicetool(''showfile'')');
	set(hmatfile,'userdata',[1 hascii]);
	set(houtopts,'userdata',[houtcurve,houtobj,hmatfile,hascii]);
	hhardcopy = uimenu(houtput,'Label','Hard Copy','callback',...
		'slicetool(''Hardcopy'')','userdata',p(1:2));
	
	% make the options menu
	hoptions = uimenu(gcf,'Label','Options');
	hhold=uimenu(hoptions,'label','Hold Plot','callback',...
		'slicetool(''boolean'')');
	if(p(3)) chk='on'; else chk='off'; end
	hflipx=uimenu(hoptions,'label','Flip X','callback','slicetool(''boolean'')',...
		'checked',chk);
	if(p(4)) chk='on'; else chk='off'; end
	hflipy=uimenu(hoptions,'label','Flip Y','callback','slicetool(''boolean'')',...
		'checked',chk);
	if(p(5)) chk='on'; else chk='off'; end
	hgrid=uimenu(hoptions,'label','Grid','callback','slicetool(''boolean'')',...
		'userdata',smfig,'checked',chk);
	if(p(6)) chk='on'; else chk='off'; end
	hscatter=uimenu(hoptions,'label','Scatter Plot','callback',...
		'slicetool(''boolean'')','userdata','slicetool','checked',chk,...
		'separator','on');
	hptsize=uimenu(hoptions,'label','Scatter Plot Pt Size','userdata',p(7));
		if(p(7)==6) chk='on'; else chk='off'; end
		hpt6=uimenu(hptsize,'label','6','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(7)==9) chk='on'; else chk='off'; end
		hpt9=uimenu(hptsize,'label','9','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(7)==12) chk='on'; else chk='off'; end
		hpt12=uimenu(hptsize,'label','12','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(7)==18) chk='on'; else chk='off'; end
		hpt18=uimenu(hptsize,'label','18','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(7)==28) chk='on'; else chk='off'; end
		hpt28=uimenu(hptsize,'label','28','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(7)==50) chk='on'; else chk='off'; end
		hpt50=uimenu(hptsize,'label','50','checked',chk,...
			'callback','slicetool(''exboolean'')');
		set(hpt6,'userdata',[-6 hptsize hpt9 hpt12 hpt18 hpt28 hpt50]);
		set(hpt9,'userdata',[-9 hptsize hpt6 hpt12 hpt18 hpt28 hpt50]);
		set(hpt12,'userdata',[-12 hptsize hpt6 hpt9 hpt18 hpt28 hpt50]);
		set(hpt18,'userdata',[-18 hptsize hpt6 hpt9 hpt12 hpt28 hpt50]);
		set(hpt28,'userdata',[-28 hptsize hpt6 hpt9 hpt12 hpt18 hpt50]);
		set(hpt50,'userdata',[-50 hptsize hpt6 hpt9 hpt12 hpt18 hpt28]);
	hnumcolors=uimenu(hoptions,'label','Number of Colors','userdata',p(28));
		if(p(28)==6) chk='on'; else chk='off'; end
		hc6=uimenu(hnumcolors,'label','6','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(28)==9) chk='on'; else chk='off'; end
		hc9=uimenu(hnumcolors,'label','9','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(28)==12) chk='on'; else chk='off'; end
		hc12=uimenu(hnumcolors,'label','12','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(28)==18) chk='on'; else chk='off'; end
		hc18=uimenu(hnumcolors,'label','18','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(28)==24) chk='on'; else chk='off'; end
		hc24=uimenu(hnumcolors,'label','24','checked',chk,...
			'callback','slicetool(''exboolean'')');
		if(p(28)==30) chk='on'; else chk='off'; end
		hc30=uimenu(hnumcolors,'label','30','checked',chk,...
			'callback','slicetool(''exboolean'')');
		set(hc6,'userdata',[-6 hnumcolors hc9 hc12 hc18 hc24 hc30]);
		set(hc9,'userdata',[-9 hnumcolors hc6 hc12 hc18 hc24 hc30]);
		set(hc12,'userdata',[-12 hnumcolors hc6 hc9 hc18 hc24 hc30]);
		set(hc18,'userdata',[-18 hnumcolors hc6 hc9 hc12 hc24 hc30]);
		set(hc24,'userdata',[-24 hnumcolors hc6 hc9 hc12 hc18 hc30]);
		set(hc30,'userdata',[-30 hnumcolors hc6 hc9 hc12 hc18 hc24]);
	if(p(8)) chk='on'; else chk='off'; end
	hscale=uimenu(hoptions,'label','1-0 (Max-Min) Scaling','callback',...
		'slicetool(''boolean'')','checked',chk);
	if(p(9)) chk='on'; else chk='off'; end
	hautopro=uimenu(hoptions,'label','Auto Promote','checked',chk,...
		'callback','slicetool(''boolean'')','separator','on');
	if(p(27)) chk='on'; else chk='off'; end
	hautoident=uimenu(hoptions,'label','Auto Identify','checked',chk,...
		'callback','slicetool(''boolean'')');
	if(p(10)) chk='on'; else chk='off'; end
	hpolydisp=uimenu(hoptions,'label','Display Poly Coefs','checked',chk,...
		'callback','slicetool(''boolean'')');
	if(p(11)) chk='on'; else chk='off'; end
	hpolyeval=uimenu(hoptions,'label','Evaluate Poly Locally','checked',chk,...
		'callback','slicetool(''boolean'')');
	haltlabels=uimenu(hoptions,'label','Alt Axis Labels','separator','on');
	if(p(12)) chk='on'; else chk='off'; end
	haltx=uimenu(haltlabels,'label','X Axis','callback','slicetool(''altlabelx'')',...
		'checked',chk,'userdata',p(13));
	if(p(14)) chk='on'; else chk='off'; end
	halty=uimenu(haltlabels,'label','Y Axis','callback','slicetool(''altlabely'')',...
		'checked',chk,'userdata',p(15));
	hcomplex=uimenu(hoptions,'label','Complex Data');
	if(p(16)==1) chk='on'; else chk='off'; end
	hamplin=uimenu(hcomplex,'label','Linear Amplitude Spectrum','callback',...
		'slicetool(''complex'')','userdata',1,'checked',chk);
	if(p(16)==2) chk='on'; else chk='off'; end
	hampdb=uimenu(hcomplex,'label','Decibel Amplitude Spectrum','callback',...
		'slicetool(''complex'')','userdata',2,'checked',chk);
	if(p(16)==3) chk='on'; else chk='off'; end
	hphase=uimenu(hcomplex,'label','Phase Spectrum','callback',...
		'slicetool(''complex'')','userdata',3,'checked',chk);
	if(p(16)==4) chk='on'; else chk='off'; end
	hampphs=uimenu(hcomplex,'label','Amp&Phase Spectra','callback',...
		'slicetool(''complex'')','userdata',4,'checked',chk);
	if(p(16)==5) chk='on'; else chk='off'; end
	hrealimag=uimenu(hcomplex,'label','Real&Imag Spectra','callback',...
		'slicetool(''complex'')','userdata',5,'checked',chk);
	set(hcomplex,'userdata',[2 hamplin hampdb hphase hampphs hrealimag]);
	
	% make the actions menu
	hactions = uimenu(gcf,'label','Actions');
%	
% to add a new action to the list, you must do the following
% 1) make a menu item for it by modifying one of those below. If its setting is to be
%	exclusive (like plot,zoom,...identify) make sure its user data is the next largest
% integer not already used as user data. It can have other userdata, but the first 
% element of the userdata matrix must be this integer. Otherwise make it like the mode setting.
% 2) If it's an exclusive action, enter its handle at the end of the list of the userdata
% 	items for hactions. Otherwise, put it's handle in the list of the figure's user data
%	like hmode.
% 3) Modify the if-elseif switch in the 'actions' code block to take the action appropriate
%	when the menu item is selected. This may be just setting some flags and enabling the
%	doit button (as in plot or unzoom) or it might require setting the windowbutton functions
% 	as in zoom, pickslope,... identify.
% 4) If the action requires the user to press the doit button, then add an appropriate case
% 	to the if-elseif block in the 'doit' code block
% 5) if the action uses the mouse, then add appropriate code to the 'buttondown' & 
%	'buttonup' code blocks
% 6) if it affect the plot, you may have to change the 'plot' code block
% 
	
	hplot = uimenu(hactions,'label','Plot','callback',...
		'slicetool(''actions'')','userdata',1,'checked','on');
	hpickslopes = uimenu(hactions,'label','Pick Slopes','callback',...
		'slicetool(''actions'')','userdata',4,'checked','off');
	hpolyfit=uimenu(hactions,'label','Poly Fit','callback',...
		'slicetool(''actions'')','userdata',5,'checked','off');
	hpolygen=uimenu(hactions,'label','Poly Gen','callback',...
		'slicetool(''polygen'')','userdata',9,'checked','off');
	hidentify=uimenu(hactions,'label','Identify','callback',...
		'slicetool(''actions'')','userdata',6,'checked','off');
	halgebra=uimenu(hactions,'label','Curve Algebra','userdata',7,...
		'callback','slicetool(''algebra'')','checked','off');
	hsmooth=uimenu(hactions,'label','Smooth','userdata',10,...
		'callback','slicetool(''smooth'')','checked','off');
	hderiv=uimenu(hactions,'label','Derivative','userdata',11,...
		'callback','slicetool(''deriv'')','checked','off');
	hpromote=uimenu(hactions,'label','Promote/Delete','userdata',8,...
		'callback','slicetool(''promote'')');
	hzoom = uimenu(hactions,'label','Zoom','callback',...
		'slicetool(''zoom'')','userdata',2,'checked','off',...
		'separator','on');
	hunzoom = uimenu(hactions,'label','unZoom','callback',...
		'slicetool(''unzoom'')','userdata',3,'checked','off');
	hmode=uimenu(hactions,'label','Mode','userdata',1,...
		'separator','on');
		if(p(17)==1) chk='on'; else chk='off'; end
        hnormal = uimenu(hmode,'label','Normal','userdata',1,'callback',...
                'slicetool(''mode'')','checked',chk);
        if(p(17)==2) chk='on'; else chk='off'; end
        htransform = uimenu(hmode,'label','Transform','userdata',2,'callback',...
                'slicetool(''mode'')','checked',chk);
        htransoptions = uimenu(hmode,'label','Transform Options');
        hzeromean = uimenu(htransoptions,'label','Remove Mean','checked','on',...
        	'callback','slicetool(''boolean''),slicetool(''zerotranstore'')');
        hzeropad = uimenu(htransoptions,'label','Zero Pad','checked','on',...
        	'callback','slicetool(''boolean''),slicetool(''zerotranstore'')');
        hburg = uimenu(htransoptions,'label','Burg Spectrum','checked','off',...
        	'callback','slicetool(''boolean''),slicetool(''zerotranstore'')');
        hburgorder = uimenu(htransoptions,'label','Burg Order');
        hburg6 = uimenu(hburgorder,'label','L=6','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',6);
        hburg8 = uimenu(hburgorder,'label','L=8','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',8);
        hburg10 = uimenu(hburgorder,'label','L=10','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',10);
        hburg12 = uimenu(hburgorder,'label','L=12','checked','on','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',12);
        hburg16 = uimenu(hburgorder,'label','L=16','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',16);
        hburg20 = uimenu(hburgorder,'label','L=20','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',20);
        hburg26 = uimenu(hburgorder,'label','L=26','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',26);
        hburg32 = uimenu(hburgorder,'label','L=32','checked','off','callback',...
        	'slicetool(''setorder''),slicetool(''zerotranstore'')','userdata',32);
        	
    % the bizarre userdata below allows the use of order values that are not simple
    % sequential integers. When the second integer is negative, that is a flag
    % for non-sequential ordering.
    set(hburgorder,'userdata',[12 -8 [6 8 10 12 16 20 26 32] hburg6 hburg8 hburg10 hburg12 hburg16 hburg20...
    		hburg26 hburg32]);
    	
    set(htransoptions,'userdata',[hzeromean hzeropad hburg hburgorder]);
    set(hmode,'userdata',[1 hnormal htransform htransoptions]);
 
    hpolyorder=uimenu(hactions,'label','Poly Order');
                
    % make a shitload of submenus for hpolyorder
    if(p(18)==1) chk='on'; else chk='off'; end
    hpoly1=uimenu(hpolyorder,'label','order 1','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',1);
    if(p(18)==2) chk='on'; else chk='off'; end
    hpoly2=uimenu(hpolyorder,'label','order 2','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',2);
    if(p(18)==3) chk='on'; else chk='off'; end
    hpoly3=uimenu(hpolyorder,'label','order 3','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',3);
    if(p(18)==4) chk='on'; else chk='off'; end
    hpoly4=uimenu(hpolyorder,'label','order 4','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',4);
    if(p(18)==5) chk='on'; else chk='off'; end
	hpoly5=uimenu(hpolyorder,'label','order 5','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',5);
    if(p(18)==6) chk='on'; else chk='off'; end
    hpoly6=uimenu(hpolyorder,'label','order 6','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',6);
    if(p(18)==7) chk='on'; else chk='off'; end
    hpoly7=uimenu(hpolyorder,'label','order 7','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',7);
    if(p(18)==8) chk='on'; else chk='off'; end
    hpoly8=uimenu(hpolyorder,'label','order 8','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',8);
    if(p(18)==9) chk='on'; else chk='off'; end
    hpoly9=uimenu(hpolyorder,'label','order 9','checked',chk,'callback',...
    	'slicetool(''setorder'')','userdata',9);
	set(hpolyorder,'userdata',[1 hpoly1 hpoly2 hpoly3 hpoly4 hpoly5 hpoly6 hpoly7...
		hpoly8 hpoly9]);
	set(hactions,'userdata',[1 hplot,hzoom,hunzoom,hpickslopes,hpolyfit,...
		hidentify, halgebra, hpromote hpolygen hsmooth hderiv]);
	hslopetools = uimenu(hactions,'label','Slope Tools','Userdata',1);
	if(p(19)==1) chk='on'; else chk='off'; end
	hslopedisplay = uimenu(hslopetools,'label','Simple Display',...
		'callback','slicetool(''exboolean'');slicetool(''resetslopetool'')'...
		,'checked',chk);
	if(p(19)==2) chk='on'; else chk='off'; end
	hmagdepth = uimenu(hslopetools,'label','Magnetic Depth (Peters)','checked',...
		chk,'callback',...
		'slicetool(''exboolean'');slicetool(''resetslopetool'')');
		
	set(hslopedisplay,'userdata',[-1 hslopetools hmagdepth]);
	set(hmagdepth,'userdata',[-2 hslopetools hslopedisplay]);
	%
	% userdata for hdefineqn is [a b m n] where the equation is of the form
	% a*(curve1).^m + b*(curve2).^n
	%
	hdefineqn= uimenu(hactions,'label','Algebra Equation','callback',...
		'slicetool(''defineqn'')','userdata',p(20:24));
	
	%
	% userdata for hdefinesm is [n flag] 
	%     n is no.of pts., flag is 1 for box, 2 for triangle 
	%
	hdefinesm= uimenu(hactions,'label','Smoother Type','callback',...
		'slicetool(''definesm'')','userdata',p(29:30));
	
	% make the xaxis & yaxis menu
	hxaxis = uimenu(gcf,'label','X_Data');
	hyaxis = uimenu(gcf,'label','Y_Data');
	
	% loop over number of items in object and load into menu
	names=objget(object,'namesmatrix');
	[ndata,c]=size(names);
	xhandles=zeros(1,ndata); yhandles=zeros(1,ndata);
	for k=1:ndata
		thisname=names(k,:);
		ind=find(abs(thisname)==1);
		if( ~isempty(ind) )
			thisname=thisname(1:ind(1)-1);
		end
		xhandles(k) = uimenu(hxaxis,'label',thisname,...
			'callback','slicetool(''xdata'')');
			%'userdata',k,'callback','slicetool(''xdata'')');
		yhandles(k) = uimenu(hyaxis,'label',thisname,...
			'callback','slicetool(''ydata'')');
			%'userdata',k,'callback','slicetool(''ydata'')');
	end
	hxoption = uimenu(hxaxis,'label','Option');
	if(p(25)==1) chk='on'; else chk='off'; end
	hxaccum = uimenu(hxoption,'label','Accumulate Selections',...
		'callback','slicetool(''dataoption'')','userdata',1,...
		'checked',chk);
	if(p(25)==2) chk='on'; else chk='off'; end
	hexclude =  uimenu(hxoption,'label','Exclusive Selections',...
		'callback','slicetool(''dataoption'')','userdata',2,...
		'checked',chk);
	set(hxoption,'userdata',[p(25) hxaccum hexclude]);
	hyoption = uimenu(hyaxis,'label','Option');
	if(p(26)==1) chk='on'; else chk='off'; end
	hyaccum = uimenu(hyoption,'label','Accumulate Selections',...
		'callback','slicetool(''dataoption'')','userdata',1,...
		'checked',chk);
	if(p(26)==2) chk='on'; else chk='off'; end
	heyclude =  uimenu(hyoption,'label','Exclusive Selections',...
		'callback','slicetool(''dataoption'')','userdata',2,...
		'checked',chk);
	set(hyoption,'userdata',[p(26) hyaccum heyclude]);
	%set x and y axis defaults
	% make the first item in the object the x axis
	set(xhandles(1),'checked','on');
	set(hxaxis,'userdata',[ndata xhandles hxoption 1]); 
	% make the third item in the object the y axis
	if( length(yhandles) >= 3 )
		set(yhandles(3),'checked','on');
		ynum=3;
	elseif( length(yhandles) >= 2 )
		set(yhandles(2),'checked','on');
		ynum=2;
	else
		set(yhandles(1),'checked','on');
		ynum=1;
	end
	set(hyaxis,'userdata',[ndata yhandles hyoption ynum]); 
	
	% a couple of storage buckets
	htranstore = uicontrol('style','text','userdata',[],'visible','off');
	hcmplxstore = uicontrol('style','text','userdata',[],'visible','off');
	hpolystore = uicontrol('style','text','userdata',[],'visible','off');
	% set the user data of the figure
	set(hfig,'userdata',[hdoit hoptions hflipx hflipy hhold hactions ...
		hxaxis hyaxis hquit hmode hmessage htranstore...
		hcmplxstore hgrid hcomplex hpolyorder houtput houtopts...
		hpolystore hscale hslopetools hautopro halgebra hpromote...
		hdefineqn hscatter hshowfile hsaveas hup hdown...
		hleft hright hpolydisp hautoident haltlabels haltx...
		halty hpolyeval hptsize hhardcopy hnumcolors hpolygen...
		hsmooth hdefinesm hderiv]);
			
	% define the buttondown function
	
	set(hfig,'windowbuttondownfcn',''); % because the initial action is plot, these
										% are just dummy definitions
	
	% define the buttonup function
	
	set(hfig,'windowbuttonupfcn','');
	% make a plot
	slicetool('plot');
	
	return;
end
	
% plot the curves
if( strcmp(action,'plot') )
	h=get(gcf,'userdata');
	hdoit=h(1);
	hflipx=h(3);
	hflipy=h(4);
	hhold=h(5);
	hactions=h(6);
	hxaxis=h(7);
	hyaxis=h(8);
	hmode=h(10);
	hmsg=h(11);
	htranstore=h(12);
	hcmplxstore=h(13);
	hgrid=h(14);
	hcomplex=h(15);
	hscale=h(20);
	hscatter=h(26);
	hautoident=h(34);
	haltx=h(36);
	halty=h(37);
	hptsize=h(39);
	hnumcolors=h(41);
	
	actioninfo = get(hactions,'userdata');
	hzoom = actioninfo(3);
	hpolyfit=actioninfo(6);
	hident= actioninfo(7);
	
	% clear the axes userdata
	set(gca,'userdata',[]);
	% set the msg string to zip
	set(hmsg,'string','');
	% set some graphics flags
	flag=get(hflipx,'checked');
	xflip=0;
	if( strcmp(flag,'on')) xflip=1; end
	
	flag=get(hflipy,'checked');
	yflip=0;
	if( strcmp(flag,'on')) yflip=1; end
	
	flag=get(hhold,'checked');
	holdit=0;
	if( strcmp(flag,'on')) holdit=1; end
	
	flag=get(hgrid,'checked');
	gridon=0;
	if( strcmp(flag,'on')) gridon=1; end
	
	% reset the axes
	if( ~holdit ) cla reset; set(gca,'visible','off'); drawnow; end
	% clear out the polyfit info
	if( ~holdit ) 
		polyid=get(hpolyfit,'userdata');
		set(hpolyfit,'userdata',polyid(1));
	end
	
	%get the object
	obj=get(hdoit,'userdata');
	
	% get the x axis vectors
	
	xinfo = get(hxaxis,'userdata');
	xdisp=xinfo(3+xinfo(1):length(xinfo) );
	
	% get the y axis vectors
	
	yinfo = get(hyaxis,'userdata');
	ydisp=yinfo(3+yinfo(1):length(yinfo) );
	
	% make sure either xinfo or yinfo is of length 1
	if( length(xdisp)~=1 & length(ydisp)~=1 )
		msg=' The number of x displays or y displays must equal 1';
		set(hmsg,'string',msg);
		error(msg);
	end
	
	% make the x axis displays
	
	xvectors = [];
	nx=length(xdisp);
	for k=1:nx
	    x=objget(obj,xdisp(k) );
		xvectors = [ xvectors x(:) ];
	end
	
	% make the y axis displays
	
	yvectors = [];
	ny=length(ydisp);
	for k=1:ny
	    y=objget(obj,ydisp(k) );
		yvectors = [ yvectors y(:) ];
	end
	% check transform flag and do so if needed
	flag=get(hmode,'userdata');
	flag=flag(1);
	if( flag == 2 ) %do a transform
		% check the htranstore userdata to see if a transform already exists
		dat=get(htranstore,'userdata');
		if(length(dat)==0)
			set(htranstore,'userdata',[xvectors yvectors]);
			slicetool('transform');
			dat=get(htranstore,'userdata');
		end
		xvectors = dat(:,1:nx);
		yvectors = dat(:,nx+1:nx+ny);	
	end
	% see if data is complex and needs processing
	complexflag=0;
	if( iscomplex(xvectors) | iscomplex(yvectors) )
		complexflag=1; % set a flag for later use
		%check hcmplxstore to see if it has already been done
		dat =get(hcmplxstore,'userdata');
		if( length(dat)==0)
			set(hcmplxstore,'userdata',[xvectors yvectors]);
			slicetool('cmplxfmt');
			dat = get(hcmplxstore,'userdata');
		end
		xvectors=dat(:,1:nx);
		yvectors=dat(:,nx+1:nx+ny);
	end
	% perform scaling if needed
	flag = get(hscale,'checked');
	if(strcmp(flag,'on')) % off means independent scaling is off so we
				% scale everything into the range +/- 1
		if( ~complexflag ) % only do this on real data
			if( nx==1) % scale the y vectors
				for k=1:ny
					ilive=find(~isnan(yvectors(:,k)));
					factor=max(abs(yvectors(ilive,k)));
					term=min(abs(yvectors(ilive,k)));
					yvectors(:,k)=(yvectors(:,k)-term)/(factor-term);
				end
			else
				for k=1:nx
					ilive=find(~isnan(xvectors(:,k)));
					factor=max(abs(xvectors(ilive,k)));
					term=min(abs(yvectors(ilive,k)));
					xvectors(:,k)=(xvectors(:,k)-term)/(factor-term);
				end
			end
		end
	end
	
	%generate some colors
	nkol=get(hnumcolors,'userdata');
	kolorset=zeros(nkol,3);
	%kolorset(:,1)=((nkol:-1:1)/nkol)';%red 1-0 ramp
	%kolorset(:,2)=((1:nkol)')/nkol;%green 0-1 ramp
	%kolorset(:,3)=([2:2:nkol nkol-2:-2:0]')/nkol;%blue 0-1-0 ramp
	if( 2*(floor(nkol/2))==nkol )
		kolorset(:,1)=([nkol:-2:2 0:2:nkol-2]/nkol)';%red 1-0-1 ramp
		kolorset(:,2)=([2:2:nkol nkol-2:-2:0]')/nkol;%green 0-1-0 ramp
		kolorset(:,3)=((1:nkol)')/nkol;%blue 0-1 ramp
	else
		kolorset(:,1)=([nkol:-2:2 0:2:nkol-1]/nkol)';%red 1-0-1 ramp
		kolorset(:,2)=([2:2:nkol nkol-1:-2:0]')/nkol;%green 0-1-0 ramp
		kolorset(:,3)=((1:nkol)')/nkol;%blue 0-1 ramp
	end
	
	% for each curve, determine its unique color by indexing into the color
	% set
	if(nx==1)
		kolors=zeros(ny,3);
		for k=1:ny
			index=rem(ydisp(k),nkol);
			if(~index) index=nkol; end
			kolors(k,:)=kolorset(index,:);
		end
	else
		kolors=zeros(nx,3);
		for k=1:nx
			index=rem(xdisp(k),nkol);
			if(~index) index=nkol; end
			kolors(k,:)=kolorset(index,:);
		end
	end
	%get the scatterplot flag
	scatter=get(hscatter,'checked');
	if(strcmp(scatter,'on'))
		linestyle='.';
	else
		linestyle='-';
	end
	%get the markersize
	ptsize=get(hptsize,'userdata');
	
	% make a plot. This will depend on the complex data option
	flag=get(hcomplex,'userdata');
	flag=flag(1);
	
	idinfo = get(hident,'userdata');
	subplots=0;
	if( ~complexflag )
		subplot(1,1,1);
		set(gca,'colororder',kolors,'visible','on');
		% use line to get the handles
		hcurves=line(xvectors,yvectors,'linestyle',linestyle,'markersize',ptsize); 
		% set the id info. The basic idea here is that hident userdata 
		% will provide the handles of the plotted curves and the handles
		% of an optional text label naming the curve. If there are 2 subplots,
		% then this is repeatedi in a second row.
		% so if ndisp is the number of displayed curves,
		% then the size of the user data is [1,2*ndisp+1]for 1 subplot and
		% [2,2*ndisp+1] for 2 subplots.
		set(hident,'userdata',[idinfo(1),hcurves',zeros(size(hcurves'))]);
		set(actioninfo(3),'enable','on'); % enable zooming
		set(actioninfo(4),'enable','on'); % enable unzooming
		set(actioninfo(5),'enable','on'); % enable slopepicking
		set(actioninfo(6),'enable','on'); % enable polyfitting
		set(actioninfo(7),'enable','on'); % enable identifying
		axis('auto'); %autoscale
	elseif( flag < 4)
		subplot(1,1,1)
		set(gca,'colororder',kolors,'visible','on');
		% use line to get the handles
		hcurves=line(xvectors,yvectors,'linestyle',linestyle,'markersize',ptsize); 
		set(hident,'userdata',[idinfo(1),hcurves',zeros(size(hcurves'))]);
		set(actioninfo(3),'enable','on'); % enable zooming
		set(actioninfo(4),'enable','on'); % enable unzooming
		set(actioninfo(5),'enable','on'); % enable slopepicking
		set(actioninfo(6),'enable','on'); % enable polyfitting
		set(actioninfo(7),'enable','on'); % enable identifying
		axis('auto'); %autoscale
	elseif( flag > 3 & nx == 1)
		subplots=1;
		subplot(2,1,1);
		set(gca,'colororder',kolors,'visible','on');
		% use line to get the handles
		hcurves=line(xvectors,real(yvectors),'linestyle',linestyle,'markersize',ptsize); 
		idinfo=[idinfo(1) hcurves' zeros(size(hcurves'))];
		set(hident,'userdata',idinfo);
		axis('auto'); %autoscale
		title1='Amplitude Spectra';
		subplot(2,1,2);
		set(gca,'colororder',kolors,'visible','on');
		hcurves=line(xvectors,imag(yvectors),'linestyle',linestyle,'markersize',ptsize); 
		idinfo=[idinfo;0 ,hcurves',zeros(size(hcurves'))];
		set(hident,'userdata',idinfo);
		axis('auto'); %autoscale
		title2='Phase Spectra';
		set(actioninfo(3),'enable','off'); % disable zooming
		set(actioninfo(4),'enable','off'); % disable unzooming
		set(actioninfo(5),'enable','off'); % disable slopepicking
		set(actioninfo(6),'enable','off'); % disable polyfitting
		set(actioninfo(7),'enable','off'); % disable identifying
	elseif( flag > 3 & ny==1 & nx ~= 1)
		subplots=1;
		subplot(2,1,1);
		set(gca,'colororder',kolors,'visible','on');
        hcurves=line(real(xvectors),yvectors,'linestyle',linestyle,'markersize',ptsize);
        idinfo=[idinfo(1) hcurves' zeros(size(hcurves'))];
		set(hident,'userdata',idinfo);
		axis('auto'); %autoscale
		title1='Real Spectra';
        subplot(2,1,2);
        set(gca,'colororder',kolors,'visible','on');
        hcurves=line(imag(xvectors),yvectors,'linestyle',linestyle,'markersize',ptsize);
		idinfo=[idinfo;0 ,hcurves',zeros(size(hcurves'))];
		set(hident,'userdata',idinfo);
		axis('auto'); %autoscale
		title2='Imaginary Spectra';
		set(actioninfo(3),'enable','off'); % disable zooming
		set(actioninfo(4),'enable','off'); % disable unzooming
		set(actioninfo(5),'enable','off'); % disable slopepicking
		set(actioninfo(6),'enable','off'); % disable polyfitting
		set(actioninfo(7),'enable','off'); % disable identifying
	end
	
if( ~subplots )
		% set the plot directions and grid
		xlabels=[]; ylabels=[];
		if(xflip) set(gca,'xdir','reverse'); end
		if(yflip) set(gca,'ydir','reverse'); end
		if(gridon) set(gca,'xgrid','on','ygrid','on'); end
		if(holdit) hold on;
			else hold off;
		% check for alternate x axis tick labels
		if(nx==1)
			flag=get(haltx,'checked');
			if( strcmp(flag,'on') )
				xlabelname=get(haltx,'userdata');
				if( xlabelname~=0 )
					xlabelvalues=objget(obj,xlabelname);
					xtick=get(gca,'xtick');
					xlabels=[];
					for k=1:length(xtick)
						ind=near(xvectors,xtick(k));
						xlabels=strmat(xlabels,num2str(xlabelvalues(ind)));
					end
					set(gca,'xticklabel',xlabels);
				end
			end
		end
		% check for alternate y axis tick labels
		if(ny==1)
			flag=get(halty,'checked');
			if( strcmp(flag,'on') )
				ylabelname=get(halty,'userdata');
				if(ylabelname ~= 0)
					ylabelvalues=objget(obj,ylabelname);
					ytick=get(gca,'ytick');
					ylabels=[];
					for k=1:length(ytick)
						ind=near(yvectors,ytick(k));
						ylabels=strmat(ylabels,num2str(ylabelvalues(ind)));
					end
					set(gca,'yticklabel',ylabels);
				end
			end
		end
	end
  
	% add a title
	title(objget(obj,'name'));
	% label the x axis
	if( length(xdisp)>1 )
		xlabel('Multiple Curves');
	else
		if( isempty(xlabels) )
			xlabel(objget(obj,'dataname',xdisp));
		else
			if(~isstr(xlabelname))
				xlabelname=objget(obj,'dataname',xlabelname);
			end
			str=[objget(obj,'dataname',xdisp),' (labeled by ' xlabelname ')'];
			xlabel(str);
		end
	end
	% label the y axis
	if( length(ydisp)>1 )
		ylabel('Multiple Curves');
	else
		if( isempty(ylabels) )
			ylabel(objget(obj,'dataname',ydisp));
		else
			if(~isstr(ylabelname))
				ylabelname=objget(obj,'dataname',ylabelname);
			end
			str=[objget(obj,'dataname',ydisp),' (labeled by ' ylabelname ')'];
			ylabel(str);
		end
	end
   else
	% add a title
	subplot(2,1,1)
	% set the plot directions and grid
	if(xflip) set(gca,'xdir','reverse'); end
	if(yflip) set(gca,'ydir','reverse'); end
	if(gridon) set(gca,'xgrid','on','ygrid','on'); end
	if(holdit) hold on;
	else hold off;
	end
	title(title1);
	% label the x axis
	if( length(xdisp)>1 )
		xlabel('Multiple Curves');
	else
		xlabel(objget(obj,'dataname',xdisp));
	end
	% label the y axis
	if( length(ydisp)>1 )
		ylabel('Multiple Curves');
	else
		ylabel(objget(obj,'dataname',ydisp));
	end
	% add a title
	subplot(2,1,2)
	% set the plot directions and grid
	if(xflip) set(gca,'xdir','reverse'); end
	if(yflip) set(gca,'ydir','reverse'); end
	if(gridon) set(gca,'xgrid','on','ygrid','on'); end
	if(holdit) hold on;
	else hold off;
	end
	title(title2);
	% label the x axis
	if( length(xdisp)>1 )
		xlabel('Multiple Curves');
	else
		xlabel(objget(obj,'dataname',xdisp));
	end
	% label the y axis
	if( length(ydisp)>1 )
		ylabel('Multiple Curves');
	else
		ylabel(objget(obj,'dataname',ydisp));
	end
	set(hmsg,'string',objget(obj,'name'));
   end
 %see if we are in autoident mode
 if(strcmp(get(hautoident,'checked'),'on'))
		slicetool('autoident')
	end
	return;
end
% actions switchboard
if( strcmp(action,'actions') )
        hthisaction = gcbo;
        %unset all button functions
        set(gcf,'WindowButtonDownFcn','');
        set(gcf,'WindowButtonmotionfcn','');
        set(gcf,'WindowButtonUpFcn','');
        h=get(gcf,'userdata');
        haction=h(6);
	hmsg=h(11);
        flag = get(hthisaction,'userdata');
        if( ~isempty(flag) )
		flag=flag(1);
	end
		% uncheck previous action and check this one
        dat = get(haction,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hthisaction,'checked','on');
        if( size(dat(1))==size(flag) ) 
		dat(1)=flag;
	end
        set(haction,'userdata',dat);
	% set the doit button
	if( ~isempty(flag) )
			  if( flag == 1) % a plot
                % enable the doit button
                set(h(1),'enable','on');
						set(hmsg,'string','Push DOIT when ready to plot');
			  elseif( flag == 3 ) % an unzoom
                 
                % enable the doit button
                set(h(1),'enable','on');
			  elseif( flag==2 ) % a zoom
					% zoom no longer works through 'actions'
					
			  elseif( flag==4 ) % a pickslope
					% see which slopetool is active
					hslopetools=h(21);
					toolid=get(hslopetools,'userdata');
					if(toolid==1) %simple slope display
						drawlineinit('slicetool(''buttonup'')');
						set(hmsg,'string','MB1: draw slope');
					elseif( toolid==2 ) %peters depth estimate
						hmsg=h(11);
						set(hmsg,'string','MB1: Select a zone of interest');
						selboxinit('slicetool(''buttonup'')');
					end
			
					% disable the doit button
						set(h(1),'enable','off');
				  elseif( flag == 5) % a polyfit
					% initial the selectionbox
						selboxinit('slicetool(''buttonup'')');
						% enable the doit button
						set(h(1),'enable','on');
						set(hmsg,'string',...
							'MB1: draw fit range (default is entire curve) ... then DOIT');
				  elseif (flag == 6 | flag==9)% initiate the identify sequence
						% define the buttondown function
						set(gcf,'WindowButtonDownFcn','slicetool(''identify'')');
						% define the buttonup function
						set(gcf,'WindowButtonUpFcn','');
						% disable the doit button
						set(h(1),'enable','off');
						set(hmsg,'string',...
							'MB1: identify curve  MB2: change text  MB3: move text');
						if( flag==9 )
							slicetool('autoident');
						end
				  end
	end
        return;
end
% set the mode
if( strcmp(action,'mode') )
        h = get(gcf,'userdata');
        hthismode = gcbo;
        hmode = h(10);
        dat = get(hmode,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hthismode,'checked','on');
        dat(1)=get(hthismode,'userdata');
        set(hmode,'userdata',dat);
        
        % set the zoom state to unzoom
        
        haction = h(6);
			actdat=get(haction,'userdata');
			hzoom=actdat(3);
			zinfo=get(hzoom,'userdata');
			set(hzoom,'userdata',[zinfo(1)]);
        return;
end
% here is the doit button callback. This is basically a switchboard
if( strcmp(action,'doit') )
        h=get(gcf,'userdata');
        % determine the action state
        haction = h(6);
        actdat = get(haction,'userdata');
        flag = actdat(1);
        if( flag == 1 | flag == 2 ) % plot & zoom call the same thing
                set(gcf,'WindowButtonDownFcn','');
                set(gcf,'WindowButtonmotionfcn','');
                set(gcf,'WindowButtonUpFcn','');
                slicetool('plot');
        elseif (flag == 3) % an unzoom
                slicetool('unzoom');
        elseif ( flag == 4) % pickslopes
                slicetool('pickslopes');
        elseif( flag == 5) % polyfit
               slicetool('polyfit');
        elseif( flag==7) % algebra
        		slicetool('algebra3');
        elseif( flag==10) % smooth
        		slicetool('smooth3');
        elseif( flag==11) % deriv
        		slicetool('deriv3');
        end
        % reset action to plot
        if(flag ~= 7)
        	set(actdat(flag+1),'checked','off');
        	set(actdat(2),'checked','on');
        	actdat(1)=1;
        	set(haction,'userdata',actdat);
        end
        return;
end
% set the x data or the y data
if( strcmp(action,'xdata') | strcmp(action,'ydata') )
h = get(gcf,'userdata');
hmsg=h(11);
hautopro=h(22);
	if( strcmp(action,'xdata') )
        	hdata = h(7);
	else
		hdata = h(8);
	end
  % get the menu infor
  menuinfo = get(hdata,'userdata');
  %get the menu that triggered this call
  hthismenu =gcbo;
	% get the options flag
	hdataopt = menuinfo(2+menuinfo(1));
	flag = get(hdataopt,'userdata');
	flag=flag(1);
	if( flag == 1)
		% check or uncheck it
		flag=get(hthismenu,'checked');
		if( strcmp(flag,'on') )
			set(hthismenu,'checked','off');
		else
			set(hthismenu,'checked','on');
		end
	elseif(flag == 2)
		% uncheck all selections
		for k=1:menuinfo(1)
			set(menuinfo(1+k),'checked','off');
		end
		% check this one
		set(hthismenu,'checked','on');
	end
	% build the data info
	flagvec = [];
	for k=1:menuinfo(1)
		thismenu = menuinfo(1+k);
		flag=get(thismenu,'checked');
		if( strcmp(flag,'on') )
			flagvec = [flagvec get(thismenu,'position')];
		end
	end
  % set the information into the data userinformation
  menuinfo= [menuinfo(1:2+menuinfo(1)) flagvec];
  set(hdata,'userdata',menuinfo);
	% set the transform store to zero
	slicetool('zerotranstore');
	%set the action to plot unless we are in an auto-promote sequence
	%see if there is a command in hautopro to execute
	cmd=get(hautopro,'userdata');
	if( isempty(cmd) )
	  haction=h(6);
	  dat = get(haction,'userdata');
	  hplot=dat(2);
	  %set(gcf,'currentmenu',hplot);
	  slicetool('actions');
		set(hmsg,'string','Push DOIT when ready for new plot');
	end
return;
end
% zero the transform & complex storage bins
if(strcmp( action,'zerotranstore' ) )
	h=get(gcf,'userdata');
	htranstore=h(12);
	set(htranstore,'userdata',[]);
	slicetool('zerocmplxstore');
	return;
end
if(strcmp( action,'zerocmplxstore' ) )
	h=get(gcf,'userdata');
	hstore=h(13);
	set(hstore,'userdata',[]);
	return;
end
if( strcmp(action,'quit') | strcmp(action,'quit2'))
	if(strcmp(action,'quit'))
		%put up a save first dialog
		yesnoinit('slicetool(''quit2'')','Save changes first?');
		return;
	elseif(strcmp(action,'quit2'))
		reply=yesnofini;
		
		if(reply==1)
			slicetool('save');
		elseif(reply==-1)
			return;
		end
		
		close(gcf);
	end
end
% set the dataoption
if( strcmp(action,'dataoption') )
        h = get(gcf,'userdata');
        hthisoption = gcbo;
        hdataopt = get(hthisoption,'parent');
        dat = get(hdataopt,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hthisoption,'checked','on');
        dat(1)=get(hthisoption,'userdata');
        set(hdataopt,'userdata',dat);
        return;
end
%
% change the setting on a boolean flag menu item.
%
if( strcmp(action,'boolean') )
	hthis = gcbo;
	% get the current flag setting
	flag = get(hthis,'checked');
	% toggle it the other way
	if( strcmp(flag,'on') )
		set(hthis,'checked','off');
	else
		set(hthis,'checked','on');
	end
	return;
end
% set any exclusive boolean option
% the userdata of such an option is assumed to consist of
% an integer flag (ignored by this action) followed by a list of 
% handles which are to have the opposite boolean setting
% if the integer flag is negative, this signals special action:
%	0) let h=userdata of the optionmenu
%	1) the absolute value of h(1) is stored as user data in h(2)
%	2) h(3)...h(length(h)) are set to the opposite settings of the option
%
if( strcmp(action,'exboolean') )
	hflag = gcbo;
	flag=get(hflag,'checked');
	list=get(hflag,'userdata');
	if( length(list)>1 )
		intflag=list(1);
		list=list(2:length(list));
	else
		list=[];
	end
	if( strcmp(flag,'on') )
		set(hflag,'checked','off');
		if( length(list)>0 )
			if(intflag>0)
				for k=1:length(list)
					set(list(k),'checked','on');
				end
			else
				d=get(list(1),'userdata');
				if( isempty(d)) 
					d=abs(intflag);
				 else 
					d(1) = abs(intflag);
				end
				set(list(1),'userdata',d);
				for k=2:length(list)
					set(list(k),'checked','on');
				end
			end
		end
	else
		set(hflag,'checked','on');
		if( length(list)>0 )
			if(intflag>0)
				for k=1:length(list)
					set(list(k),'checked','off');
				end
			else
				d=get(list(1),'userdata');
				if( isempty(d)) 
					d=abs(intflag);
				 else 
					d(1) = abs(intflag);
				end
				set(list(1),'userdata',d);
				for k=2:length(list)
					set(list(k),'checked','off');
				end
			end
		end
	end
	return;
end
% set the complex data flags
if( strcmp(action,'complex') )
	hthismenu=gcbo;
	hcomplex=get(hthismenu,'parent');
	cdata=get(hcomplex,'userdata');
	% uncheck the previous option
	set(cdata(cdata(1)+1),'checked','off');
	% check this one
	set(hthismenu,'checked','on');
	% update the user data in the hcomplex
	cdata(1)=get(hthismenu,'userdata');
	set(hcomplex,'userdata',cdata);
	% set the complex storage bucket to zero
	h=get(gcf,'userdata');
	hcmplxstore=h(13);
	set(hcmplxstore,'userdata',[]);
	return;
end
% process a button up event
if( strcmp(action,'buttonup') )
	
	h=get(gcf,'userdata');
	hdoit=h(1);
	
	% determine the action state
	haction = h(6);
	actdat=get(haction,'userdata');
	state=actdat(1);
	
	if( state == 2 | state == 5)	% a zoom or a polynomial fit
        set(gcf,'windowbuttonmotionfcn','');
        
        box=selboxfini;
        try
            delete(box{2});
        catch
            %no selbox to delete
        end
        box = box{1};
        
        xmin = min(box(1), box(3));
        xmax = max(box(1), box(3));
        ymin = min(box(2), box(4));
		ymax = max(box(2), box(4));
		hzoom=actdat(3);
		zinfo=get(hzoom,'userdata');
		set(hzoom,'userdata',[zinfo(1),xmin,xmax,ymin,ymax,box(5)]);
		set(hdoit,'enable','on');
		
	elseif (state == 4) % save slope info
		% determine which slope tool
		hslopetools=h(21); hzoom=actdat(3); hmessage=h(11);
		toolid=get(hslopetools,'userdata');
		if(toolid==1) %simple slope display
			set(gcf,'windowbuttonmotionfcn','');
			lineinfo=get(gca,'userdata');
			if( length(lineinfo) < 4 )
				set(hmessage,'string','invalid slope pick');
			else
				p1=lineinfo(1:2);
				p2=lineinfo(3:4);
				hpickslopes=actdat(5);
				pinfo=get(hpickslopes,'userdata');
				set(hzoom,'userdata',[pinfo(1),p1,p2]);
				hmessage = h(11);
				slope=(p2(2)-p1(2))/(p2(1)-p1(1));
				mess=sprintf(' slope =%g ',slope);
				set(hmessage,'string',[mess ' .... MB1: draw slope']);
			end
		elseif(toolid==2) % peters depth estimate
			set(gcf,'windowbuttonmotionfcn','');
			slicetool('petersdepth');
		end
	end
return;
end
	
% the identify sequence
if( strcmp(action,'identify') )
	
	h=get(gcf,'userdata');
	hdoit=h(1);
	hmsg=h(11);
	
	% determine the action state
	haction = h(6);
	actdat=get(haction,'userdata');
	state=actdat(1);
	%determine which mouse button was pressed
	flag=get(gcf,'selectiontype');
	if( strcmp(flag,'alt') )
		% move an existing text label around
		textmove('init');
	elseif( strcmp(flag,'extend') )
		hobj=gco;
		if(strcmp(get(gco,'type'),'text'))
			set(hmsg,'string','Type in new string in MATLAB window');
			newstr=input('Type in new string: ','s');
			if( ~isempty(newstr) )
				set(gco,'string',newstr);
			end
			set(hmsg,'string','MB1: identify curve  MB2: change text  MB3: move text');
		end
	else
		% get the current object's handle
		hobj = get(gcf,'currentobject');
		
		% see if it is one of the curves
		hident=actdat(7);
		% The basic idea here is that hident userdata 
		% will provide the handles of the plotted curves and the handles
		% of an optional text label naming the curve. If there are 2 subplots,
		% then this is repeated in a second row.
		% so if ndisp is the number of displayed curves,
		% then the size of the user data is [1,2*ndisp+1]for 1 subplot and
		% [2,2*ndisp+1] for 2 subplots.
		idinfo=get(hident,'userdata');
		curvenum=-999;
		
		[l,m]=size(idinfo);
		
%
% NOTE: This id mechanism does not work for a subdivided plot
%
		for k=2:m
			if( hobj==idinfo(k) )
				curvenum=k; % we found the curve
				break;
			end
		end
		if( strcmp(get(hobj,'type'),'text') )
			hmsg=h(11);
			set(hmsg,'string','Select curve not text object');
			return;
		end
		
		
		%return if the curve was not found
		if( curvenum== -999)
			hmsg=h(11);
			set(hmsg,'string','unable to identify curve');
			return;
		else
			hmsg=h(11);
			set(hmsg,'string','');
		end
		
		hxaxis=h(7);
        hyaxis=h(8);
        % get the number of x axis vectors
        xinfo = get(hxaxis,'userdata');
        xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
        % get the number of y axis vectors
        yinfo = get(hyaxis,'userdata');
        ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
		
		if( nx==1 )
			numcurves=ny;
			if( curvenum <= numcurves+1 )
				curveid=ydisp(curvenum-1);
			elseif( curvenum> 2*numcurves+1 & curvenum <= 3*numcurves+1)
				curveid=ydisp(curvenum-2*numcurves-1);
			end
		else
			numcurves=nx;
			if( curvenum <= numcurves+1 )
				curveid=xdisp(curvenum-1);
			elseif( curvenum> 2*numcurves+1 & curvenum <= 3*numcurves+1)
				curveid=xdisp(curvenum-2*numcurves-1);
			end
		end
		
		% see if there is already a label to delete
		if(idinfo(curvenum+numcurves)>0)
			delete(idinfo(curvenum+numcurves));
		end
		
		% get the data information
        hdoit=h(1);
		
		% get the data object
		obj=get(hdoit,'userdata');
		
		% get the dataname
		
		dataname=objget(obj,'dataname',curveid);
		
		% get the current point
		pt=get(gca,'currentpoint');
		% get the color
		kolor=get(hobj,'color');
		
		% write the name on the screen
		
		htext=text(pt(1,1),pt(1,2),dataname,'color',kolor);
		
		% update the idinfo
		idinfo(curvenum+numcurves)=htext;
		set(hident,'userdata',idinfo);
		set(hmsg,'string','MB1: identify curve  MB2: change text  MB3: move text');
	end
	
	return;
	
end
% the identify sequence
if( strcmp(action,'autoident') )
	
	h=get(gcf,'userdata');
	hdoit=h(1);
	hmsg=h(11);
	
	% determine the action state
	haction = h(6);
	actdat=get(haction,'userdata');
	state=actdat(1);
	%determine which mouse button was pressed
	flag=get(gcf,'selectiontype');
	if( strcmp(flag,'alt') )
		% move an existing text label around
		textmove('init');
	elseif( strcmp(flag,'extend') )
		hobj=gco;
		if(strcmp(get(gco,'type'),'text'))
			set(hmsg,'string','Type in new string in MATLAB window');
			newstr=input('Type in new string: ','s');
			if( ~isempty(newstr) )
				set(gco,'string',newstr);
			end
			set(hmsg,'string','MB1: identify curve  MB2: change text  MB3: move text');
		end
	else
		
		% get the id info
		hident=actdat(7);
		% The basic idea here is that hident userdata 
		% will provide the handles of the plotted curves and the handles
		% of an optional text label naming the curve. If there are 2 subplots,
		% then this is repeated in a second row.
		% so if ndisp is the number of displayed curves,
		% then the size of the user data is [1,2*ndisp+1]for 1 subplot and
		% [2,2*ndisp+1] for 2 subplots.
		idinfo=get(hident,'userdata');
		curvenum=-999;
		
		[l,m]=size(idinfo);
		
		ncurves=(m-1)/2;
		hcurves=idinfo(2:ncurves+1);
		htexts=idinfo(ncurves+2:m);
		
		hxaxis=h(7);
        hyaxis=h(8);
		% determin x(y) or y(x)
        % get the number of x axis vectors
        xinfo = get(hxaxis,'userdata');
        xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
        % get the number of y axis vectors
        yinfo = get(hyaxis,'userdata');
        ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
		
		if( nx==1 )
			numcurves=ny;
			curveids=ydisp;
		else
			numcurves=nx;
			curveids=xdisp;
		end
		
		% get the data information
        hdoit=h(1);
		
		% get the data object
		obj=get(hdoit,'userdata');
		
		% get the datanames
		
		names=objget(obj,'namesmatrix');
		
		%loop over the curves
		for k=1:ncurves
			% see if there is already a label to delete
			if(htexts(k)>0)
				delete(htexts(k));
				htexts(k)=0;
			end
		
			% put the label at the last point on the curve
			x=get(hcurves(k),'xdata');
			y=get(hcurves(k),'ydata');
			ilive=find(~isnan(x));
			ilive2=find(~isnan(y(ilive)));
			ilive=ilive(ilive2);
			x=x(ilive);
			y=y(ilive);
			xpt=x(length(x));
			ypt=y(length(y));
			% get the color
			kolor=get(hcurves(k),'color');
			
			%get the name
			dataname=names(curveids(k),:);
			ind=find(abs(dataname)==1);
			if( ~isempty(ind) )
				dataname=dataname(1:ind(1)-1);
			end
		
			% write the name on the screen
			htexts(k)=text(xpt,ypt,dataname,'color',kolor);
		end
		
	% update the idinfo
	idinfo=[idinfo(1) hcurves htexts];
	set(hident,'userdata',idinfo);
	end
	
	return;
	
end
% a zoom
if(strcmp(action,'zoom'))
	h=get(gcf,'userdata');
	hoptions=h(2);
	fcn1=get(gcf,'windowbuttondownfcn');
	fcn2=get(gcf,'windowbuttonmotionfcn');
	fcn3=get(gcf,'windowbuttonupfcn');
	set(hoptions,'userdata',[abs(fcn1) nan abs(fcn2) nan abs(fcn3) ]);
	% initial the selectionbox
	selboxinit('slicetool(''zoom2'')');
    % disable the doit button
    set(h(1),'enable','off');
    return;
end
if(strcmp(action,'zoom2'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hoptions=h(2);
	hup=h(29);
	hdown=h(30);
	hleft=h(31);
	hright=h(32);
	
	% get the action data
	haction = h(6);
	actdat=get(haction,'userdata');
	
	fcndat=get(hoptions,'userdata');
	ind=find(isnan(fcndat));
	set(gcf,'windowbuttondownfcn',setstr(fcndat(1:ind(1)-1)));
	set(gcf,'windowbuttonmotionfcn',setstr(fcndat(ind(1)+1:ind(2)-1)));
    set(gcf,'windowbuttonupfcn',setstr(fcndat(ind(2)+1:length(fcndat))));
    
    box=selboxfini;
    try
        delete(box{2});
    catch
        %no selbox to delete
    end
    box = box{1};
    
    xmin = min(box(1), box(3));
	xmax = max(box(1), box(3));
	ymin = min(box(2), box(4));
	ymax = max(box(2), box(4));
	hzoom=actdat(3);
	zinfo=get(hzoom,'userdata');
	set(hzoom,'userdata',[zinfo(1),xmin,xmax,ymin,ymax]);
	set(hdoit,'enable','on');
	if(length(box)==5) delete(box(5)); end
	set(gca,'xlim',[xmin xmax],'ylim',[ymin ymax]);
	
	set(hup,'visible','on');
	set(hdown,'visible','on');
	set(hleft,'visible','on');
	set(hright,'visible','on');
	return;
end
% an unzoom
if(strcmp(action,'unzoom') )
	h=get(gcf,'userdata');
	hup=h(29);
	hdown=h(30);
	hleft=h(31);
	hright=h(32);
	
	haction = h(6);
	actdat=get(haction,'userdata');
	hzoom=actdat(3);
	zinfo=get(hzoom,'userdata');
	set(hzoom,'userdata',[zinfo(1)]);
	
	axis('auto');
	set(hup,'visible','off');
	set(hdown,'visible','off');
	set(hleft,'visible','off');
	set(hright,'visible','off');
	
	return;
end
% do a transform
if( strcmp(action,'transform') )
	h=get(gcf,'userdata');
        hxaxis=h(7);
        hyaxis=h(8);
        hmode=h(10);
		hmsg=h(11);
        htranstore=h(12);
        hcmplxstore=h(13);
        
        % get the transform options
        info=get(hmode,'userdata');
        htransoptions = info(4);
        transoptions=get(htransoptions,'userdata');
        
        flag=get(transoptions(1),'checked');
        zeromean=0;
        if( strcmp(flag,'on') ) zeromean=1; end
        flag=get(transoptions(2),'checked');
        zeropad=0;
        if( strcmp(flag,'on') ) zeropad=1; end
        flag=get(transoptions(3),'checked');
        doburg=0;
        if( strcmp(flag,'on') )
        	 doburg=1;
        	 burgorder=get(transoptions(4),'userdata');
        	 burgorder=burgorder(1);
       	end
        % get the number of x axis vectors
        xinfo = get(hxaxis,'userdata');
        xdisp=xinfo(3+xinfo(1):length(xinfo) );
	nx=length(xdisp);
        % get the number of y axis vectors
        yinfo = get(hyaxis,'userdata');
        ydisp=yinfo(3+yinfo(1):length(yinfo) );
	ny=length(ydisp);
	dat=get(htranstore,'userdata');
	% determine the transform coordinates. Note that if both nx and ny are 1
	% then the transform of y(x) is done and not the transform of x(y)
	% This might be a problem for people who plot things sideways
	if( nx==1 )
		t=dat(:,nx);
		s=dat(:,nx+1:nx+ny);
	elseif( ny == 1 & nx~= 1)
		s=dat(:,1:nx);
		t=dat(:,nx+1:nx+ny);
	else
		error(' improper transform specifications');
	end
	% see if the data needs to be regularized
	% first toss out nan's
	idead=find(isnan(s));
	test=sum(abs(diff(diff(t))))/median(diff(t));
	if( test > 10^(-6) | ~isempty(idead) ) %this means irregular
		% we must regularize each curve separately
		[npts,ncurves]=size(s);
		for k=1:ncurves
			if(k==1)
				% make a new regular t
				dt=median(diff(t)); % use the most common
				tmin = min(t);
				tmax=max(t);
				nt=round((tmax-tmin)/dt+1);
				tnew=xcoord(tmin,dt,nt)';	
			% see if we need to sort
			% at this time, sorting is "handled" by aborting
				[t,ind]=sort(t);
				if( sum(diff(diff(ind))) > eps )
				msg='transform only possible for single valued functions';
					set(hmsg,'string',msg);
					error(msg);
				end
				snew=zeros(length(tnew),ncurves);
			end
		% now interpolate using splines
			ilive=find(~isnan(s(:,k)));
			snew(:,k)=interp1(t(ilive),s(ilive,k),tnew,'spline');
		end
		s=snew;
		t=tnew;
		snew=[];
		tnew=[];
		set(hmsg,'string','Data regularized prior to transform');
	end
	% now the transform
	
	if( zeromean )
		[rs,cs]=size(s);
		mn=mean(s);
		mn=ones(rs,1)*mn;
		s=s-mn;
		mn=[];
	end
	if( zeropad ) % zero pad only affects fft's
		n2=nextpow2(t);
		n=2^n2;
	else
		n-length(t);
	end
	
	if(doburg)
		[rs,cs]=size(s);
		S=zeros(size(s+i*s));
		for k=1:cs
			[tmp,f]=burg(s(:,k),t,burgorder);
			S(:,k)=tmp+ i*eps*ones(size(tmp)); % make it complex so its treated as 
											% a spectrum
		end
	else
		[S,f]=fftrl(s,t,10,n);
	end
	% put the transform back where it came from
	if( nx == 1)
		dat=zeros(length(f),nx+ny);
		dat(:,1)=f(:);
		dat(:,2:ny+1)=S;
	elseif( ny == 1 & nx~= 1)
		dat=zeros(length(f),nx+ny);
		dat(:,1:nx)=S;
		dat(:,nx+1:nx+1)=f(:);
	end
	set(htranstore,'userdata',dat);
  
	% set the complex store to zero
	set(hcmplxstore,'userdata',[]);
	return;
end
% reformat complex data
if( strcmp(action,'cmplxfmt') )
	h=get(gcf,'userdata');
        hxaxis=h(7);
        hyaxis=h(8);
	hmsg=h(11);
        htranstore=h(12);
        hcmplxstore=h(13);
	hcomplex=h(15);
        % get the number of x axis vectors
        xinfo = get(hxaxis,'userdata');
        xdisp=xinfo(3+xinfo(1):length(xinfo) );
	nx=length(xdisp);
        % get the number of y axis vectors
        yinfo = get(hyaxis,'userdata');
        ydisp=yinfo(3+yinfo(1):length(yinfo) );
	ny=length(ydisp);
	dat=get(hcmplxstore,'userdata');
	if( nx == 1)
		S=dat(:,nx+1:nx+ny);
		f=dat(:,nx);
		ns=ny;
	elseif( ny== 1 & nx~= 1)
		S=dat(:,1:nx);
		f=dat(:,nx+1);
		ns=nx;
	end
	% get the complex formatting flag
	flag=get(hcomplex,'userdata');
	flag=flag(1);
	% the meaning of the flag is:
	%	1 ... linear amplitude spectrum
	%	2 ... decibel amplitude spectrum
	%	3 ... phase spectrum
	%	4 ... amplitude and phase spectra
	%	5 ... real and imaginary parts	
	if( flag == 1 )
		S=abs(S);
	elseif( flag == 2)
		% each spectrum will be in dbdown from its own max
		for k=1:ns
			S(:,k) = real(todb(S(:,k)));
		end 
	elseif( flag == 3)	
		% phase spectra are returned as the imaginary part by TODB
		for k=1:ns
			S(:,k) = imag(todb(S(:,k)));
		end 
	elseif( flag == 4)
		%
		for k=1:ns
			S(:,k) = todb(S(:,k));
		end 
	elseif( flag == 5)
		% this one is easy
	end
	% put the data back
	if( nx == 1)
		dat=zeros(size(dat));
		dat(:,nx+1:nx+ny)=S;
		dat(:,1)=f;
	elseif( ny== 1 & nx~= 1)
		dat=zeros(size(dat));
		dat(:,1:nx)=S;
		dat(:,nx+1)=f;
	end
	set(hcmplxstore,'userdata',dat);
	return;
end
% set the polynomial order menu or the Burg order menu
if( strcmp(action,'setorder') )
        hthisorder = gcbo;
        h=get(gcf,'userdata');
 
        hmainorder=get(hthisorder,'parent');
        maininfo = get(hmainorder,'userdata');
        % check for special ordering information
        if( maininfo(2) < 0 )
        	num=abs(maininfo(2));
        	ind=maininfo(3:num+2);
        	offset=num+1;
        else
        	offset=0;
        end
        
        thisorder=get(hthisorder,'userdata');
        
        % uncheck the previous order
        if( offset== 0) 
        	set( maininfo(maininfo(1)+1),'checked','off');
        else
        	k=find(ind==maininfo(1));
        	set( maininfo( offset+1+k ),'checked','off');
        end
        
        % check this one
        set(hthisorder,'checked','on');
        
        % update the maininfo
        maininfo(1)=thisorder;
        set(hmainorder,'userdata',maininfo);
        
        return;
end
% do a polynomial fit
if( strcmp(action,'polyfit') )
	% get the data to be fit
	% This is done by using the 'identify' information to get the handles
	% to the displayed curves and then getting their xdata and ydata
		h=get(gcf,'userdata');
        haction=h(6);
        actinfo=get(haction,'userdata');
		hpolyfit = actinfo(6);
        hident=actinfo(7);
        hzoom=actinfo(3);
        hpolyorder=h(16);
		hpolystore=h(19);
		hpolydisp=h(33);
		hpolyeval=h(38);
		ptsize=get(h(39),'userdata');
        idinfo=get(hident,'userdata');
        
        dispflag=get(hpolydisp,'checked');
        if(strcmp(dispflag,'on'))
        	dispflag=1;
        else
        	dispflag=0;
        end
        
        % in order to interprete idinfo, we need to know the number of curves
        % displayed and whether we are doing y(x) or x(y)
        
        hxaxis=h(7);
        hyaxis=h(8);
			hmsg=h(11);
        htranstore=h(12);
        hcmplxstore=h(13);
        % get the number of x axis vectors
        xinfo = get(hxaxis,'userdata');
        xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
        % get the number of y axis vectors
        yinfo = get(hyaxis,'userdata');
        ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
		% determine number of curves and y(x) or x(y)
		
		if( nx==1 )
			numcurves=ny;
			yofx=1;
			curvedat=get(hyaxis,'userdata');
			curvedisp=ydisp;
		else
			numcurves=nx;
			yofx=0;
			curvedat=get(hxaxis,'userdata');
			curvedisp=xdisp;
		end
		
		%get the curve handles
		
		hcurves=idinfo(2:numcurves+1);
		
		% now get the zoom info to determine the range of the fit
		zoominfo = get(hzoom, 'userdata');
		xmin=-999;
		if( length(zoominfo)>1 )
			xmin=zoominfo(2);
			xmax=zoominfo(3);
			ymin=zoominfo(4);
			ymax=zoominfo(5);
			if( length(zoominfo)>5 )
				delete(zoominfo(6));
				set(hzoom,'userdata',zoominfo(1:5));
			end
		else
			ylim=get(gca,'ylim');
			xlim=get(gca,'xlim');
			xmin=min(xlim);
			xmax=max(xlim);
			ymin=min(ylim);
			ymax=max(ylim);
		end
		
		% get the order of the fit
		polyinfo = get(hpolyorder,'userdata');
		
		order=polyinfo(abs(1));
		
		% freeze the axis
		
		axis(axis);
		
		% now loop over curves
		
		polyid=get(hpolyfit,'userdata');
		pstore=[];
		for k=1:numcurves
			
			xdata=get(hcurves(k),'xdata');
			ydata=get(hcurves(k),'ydata');
			
			%make sure the curve is in the window
			itestx=between(xmin,xmax,xdata,2);
			itesty=between(ymin,ymax,ydata(itestx),2);
			pcoefs=[];
			if( itesty )
			
				% use only live data
				ixlive=find(~isnan(xdata));
				iylive=find(~isnan(ydata));
			
			
			
				kolor=get(hcurves(k),'color');
				set(hcurves(k),'linestyle','.','markersize',ptsize);
				if( yofx )
					% fit y to x
					ilive=find(~isnan(ydata(ixlive)));
					xlivedata=xdata(ixlive(ilive));
					ylivedata=ydata(ixlive(ilive));
					if( xmin ~= -999 )
						% window the x coordinates
						ind=between(xmin,xmax,xlivedata,2); 
					else
						ind=1:length(xlivedata);
					end
				
					pcoefs=polyfit(xlivedata(ind),ylivedata(ind),order);
					% save the coeficients
					pstore=[pstore ydisp(k) length(pcoefs) pcoefs];
					%evaluate the polynomial. Evaluate it wherever xdata is live
					%unless hployeval is checked which means we only evaluate
					% over the range we computed
				
					ypoly=nan*ones(size(xdata));
					if( strcmp(get(hpolyeval,'checked'),'off') )
						ypoly(ixlive)=polyval(pcoefs,xdata(ixlive));
					else
						ind=between(xmin,xmax,xdata(ixlive),2);
						ypoly(ixlive(ind))=polyval(pcoefs,xdata(ixlive(ind)));
					end
				
					% plot with line
				
					hpoly=line(xdata,ypoly,'color','w','linestyle','-');
					% save the id info of the polyfit in hpolyfit
					polyid=[polyid hpoly ydisp(k) order];
			
				else
				
					% fit x to y
					ilive=find(~isnan(xdata(iylive)));
					xlivedata=xdata(iylive(ilive));
					ylivedata=ydata(iylive(ilive));
					if( xmin ~= -999 )
					% window the y coordinates
						ind=between(ymin,ymax,ylivedata,2); 
					else
						ind=1:length(ylivedata);
					end
				
					pcoefs=polyfit(ylivedata(ind),xlivedata(ind),order);
 
					% save the coeficients
					pstore=[pstore xdisp(k) length(pcoefs) pcoefs];
					%evaluate the polynomial. Evaluate it wherever xdata is live
					%unless hployeval is checked which means we only evaluate
					% over the range we computed
				
					xpoly=nan*ones(size(ydata));
					if( strcmp(get(hpolyeval,'checked'),'off') )
						xpoly(iylive)=polyval(pcoefs,ydata(iylive));
					else
						ind=between(ymin,ymax,ydata(iylive),2);
						xpoly(iylive(ind))=polyval(pcoefs,ydata(iylive(ind)));
					end
				
					% plot with line
				
					hpoly=line(xpoly,ydata,'color','w','linestyle','-');
					% save the id info of the polyfit in hpolyfit
					polyid=[polyid hpoly xdisp(k) order];
			end
		end
			
			%write out the polynomial coefficients if requested
			if( dispflag & ~isempty(pcoefs) )
				%determine curve name
				hcurvemenu=curvedat(1+curvedisp(k));
				curvename=get(hcurvemenu,'label');
				disp(['Polynomial coefficients for ' curvename ':']);
				disp('order n , order n-1 ... constant term');
				pcoefs
			end
			
		end
		% save the polynomial information
		 
		set(hpolyfit,'userdata',polyid);
		set(hpolystore,'userdata',pstore);
		
		% get the axis scaling information
		
		v=axis;
		
		if( xmin ~= v(1) | xmax ~= v(2) | ymin ~= v(3) | ymax ~= v(4) )
		% zero the zoom info because it was just used to set the fitting bounds
			set(hzoom,'userdata',zoominfo(1));
		end
		set(hmsg,'string','fit completed... ACTION reset to PLOT');
		
		return;
end
% output section
if( strcmp(action,'out') | strcmp(action,'save') | strcmp(action,'saveas'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	houtput=h(17);
	houtopts=h(18);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	haction=h(6);
	actinfo=get(haction,'userdata');
	hident=actinfo(7);
	hpolyfit=actinfo(6);
	hcmplxstore=h(13);
	hgrid=h(14);
	hpolystore=h(19);
	set(hmsg,'string','');
	if( strcmp(action,'save') | strcmp(action,'saveas'))
		outmode='Save Slice Object';
	else
		hout=gcbo;
		outmode=get(hout,'label');
	end
% get the handles of the options submenus
% houtopts userdata = [houtcurve,houtobj,hmatfile,hascii]
	
	dat = get(houtopts,'userdata');
	houtcurve=dat(1);
	hmatfile=dat(3);
	flag=get(houtcurve,'checked');
	if( strcmp(flag,'on') )
		curveout = 1;
		objout=0;
	else
		objout=1;
		curveout=0;
	end
	flag=get(hmatfile,'checked');
	if( strcmp(flag,'on') )
		matout = 1;
		asciiout=0;
		fname='*.mat';
	else
		asciiout=1;
		matout=0;
		fname='*.dat';
	end
% get the container object
	cobj=get(hmsg,'userdata');
% get the file object
	fileobj=objget(cobj,'file');
	
	filename=setstr(objget(fileobj,'filename'));
	path=setstr(objget(fileobj,'pathname'));
	
% see is we need to ask for a filename
	if( ~strcmp(outmode,'Save Slice Object') | (strcmp(outmode,'Save Slice Object') & ...
		strcmp(filename,'undefined') ) | strcmp(action,'saveas'))
		% get the output file name
		pos=get(gcf,'position');
		%xpopup=pos(1)+pos(3)/2;
		%ypopup=pos(2)+pos(4);
		%[filename,path]=uiputfile(fname,'Output File Selection',xpopup,ypopup);
		[filename,path]=uiputfile(fname,'Output File Selection');
		if( isempty(filename) )
			set(hmsg,'string','Output aborted: no file name given');
			return;
		end
		if( filename==0 )
			set(hmsg,'string','Output aborted');
			return;
		end
		
		%save the defined file and path
		if(strcmp(outmode,'Save Slice Object'))
			fileobj=objset(fileobj,'filename',filename);
			fileobj=objset(fileobj,'pathname',path);
			cobj=objset(cobj,'file',fileobj);
			set(hmsg, 'userdata',cobj);
		end
	end
		matfilename=filename;
		ind = findstr(filename,'.mat');
		if( length(ind)>0 ) filename=filename(1:ind-1); end
		ind = findstr(filename,'.dat');
		if( length(ind)>0 ) filename=filename(1:ind-1); end
		filenamelabel= [path matfilename];
		fullfilename = [path filename];
	
	% get the data object
	obj=get(hdoit,'userdata');
% now, a different case for each possible output		
%
% first output current curves
%
	if( strcmp(outmode,'Save Current Curves') )
	% 
	% steps: 	1) determine if one or 2 subplots and branch. Do the following for each
	%		2) Get the handles of the regular curves from hident
	%		3) Get the handles of any polynomials from hpolyfit
	%		4) Determine y(x) or x(y) and the curves id numbers
	%		5) if objout, then build an object. Put polynomial curves right after
	%		their parents and name them Pn: parentName. Where n is the order of
	%			the fit.
	%		6) if curvesout, write out x,y's for each curve. Preface each curve
	%		with: number of points, name of x variable & name of y variable
	%     		if ascii format. If matlab format, then make variable whose names
	%		are the first 10 letters of the curve name (minus blanks).
	%	
				
	% get the curve information	
	idinfo=get(hident,'userdata');
	polyinfo=get(hpolyfit,'userdata');
	% determine number of subplots
	[nsubs,m]=size(idinfo);
	
	% if there are two subplots then this is essentially complex output. We
	% get the numbers from the complexstore.
    if( nsubs> 1)
		data=get(hcmplxstore,'userdata');
	% ********* unfinished **********
    else
	
		% get the number of curves and the curve handles
			ncurves = (length(idinfo)-1)/2;
			hcurves = idinfo(2:ncurves+1);
			% see if we have any polynomials
			np=length(polyinfo);
			npolys = np/3;
	
			if( npolys ) 
				hpolys=polyinfo(2:3:np);
				polyids=polyinfo(3:3:np);
				orders=polyinfo(4:3:np);
			end
	
			% get the number of x axis vectors
				 xinfo = get(hxaxis,'userdata');
			 xdisp=xinfo(3+xinfo(1):length(xinfo) );
			 nx=length(xdisp);
			 % get the number of y axis vectors
			 yinfo = get(hyaxis,'userdata');
			 ydisp=yinfo(3+yinfo(1):length(yinfo) );
			ny=length(ydisp);
			% determine number of curves and y(x) or x(y)
		
			if( nx==1 )
				yofx=1;
				curveids=ydisp;
				xid=xdisp(1);
			else
				yofx=0;
				curveids=xdisp;
				xid=ydisp(1);
			end
		
		% branch if object or not
	
			if( objout )
			% get the data from the first curve
				x=get(hcurves(1),'xdata');
				y=get(hcurves(1),'ydata');
		
				if( ~yofx ) tmp=x; x=y; y=tmp; tmp=[]; end % swap if needed
		
				%build an object. Use the dependent variable to initialize
				%use eval to make sure the name is the filename
				objname=['Slicetool: ' objget(obj,'name')];
				xname=objget(obj,'dataname',xid);
		
				eval([filename '=randobj(''' objname ''',x,''' xname ''');']);
		
				% now loop over the number of curves and add them
				if( yofx ) 
						cdata='ydata';
				else
						cdata='xdata';
				end
		
				for k=1:ncurves 
			
					% add the curve to the object
					y=get(hcurves(k),cdata);
			
					thisname = objget(obj,'dataname',curveids(k));
					eval([ filename '=objset(' filename ',''' thisname ...
						''',y);']);
			
					% now any polynomial fits for this curve
			
					if( npolys )
						ind=find( polyids==curveids(k) );
				
						for kk=1:length(ind)
				
							polyname= ['P' int2str(orders(ind(kk))) ...
								 ':' thisname];
							ypoly = get(hpolys(ind(kk)),cdata);
							eval([ filename '=objset(' filename ...
								 ',''' polyname ''',ypoly);']);
					
						end
					end
				end
		
				% ok, now write it out
		
				if( matout )
					eval(['save ' fullfilename ' ' filename]);
				elseif( asciiout )
					eval(['save ' fullfilename ' ' filename ' -ascii']);
				end
		
			 elseif( curveout )
				set(hmsg,'string','Current Curves must be output as an object');
				return;
			 end
			end %close fucked nsubs block
		
	elseif(strcmp(outmode,'Save Slice Object'))
	
		%make sure the random object has a datatype of 'line'
		obj=objset(obj,'datatype','line');
		
		%put the slice object in the container
		cobj=objset(cobj,'randobj',obj);
		% save the parameters settings
		p=get(h(28),'userdata');
		% hardcopy scales
		p(1:2)=get(h(40),'userdata');
		% flipx
		flag=get(h(3),'checked');
		if(strcmp(flag,'on'))
			p(3)=1;
		else
			p(3)=0;
		end
		% flipy
		flag=get(h(4),'checked');
		if(strcmp(flag,'on'))
			p(4)=1;
		else
			p(4)=0;
		end
		% grid
		flag=get(h(14),'checked');
		if(strcmp(flag,'on'))
			p(5)=1;
		else
			p(5)=0;
		end
		% scatterplot
		flag=get(h(26),'checked');
		if(strcmp(flag,'on'))
			p(6)=1;
		else
			p(6)=0;
		end
		% scatterplot pt size
		p(7)=get(h(39),'userdata');
		% 1-0 scaling
		flag=get(h(20),'checked');
		if(strcmp(flag,'on'))
			p(8)=1;
		else
			p(8)=0;
		end
		% autopromote
		flag=get(h(22),'checked');
		if(strcmp(flag,'on'))
			p(9)=1;
		else
			p(9)=0;
		end
		% display poly
		flag=get(h(33),'checked');
		if(strcmp(flag,'on'))
			p(10)=1;
		else
			p(10)=0;
		end
		% eval poly
		flag=get(h(38),'checked');
		if(strcmp(flag,'on'))
			p(11)=1;
		else
			p(11)=0;
		end
		% alt x axis 
		xalt=get(h(36),'userdata');
		if(isstr(xalt))
			%resolve the name to a number
			hxmenus=get(h(7),'userdata'); %the x axis menus
			for k=1:hxmenus(1)
				if(strcmp(get(hxmenus(k+1),'label'),xalt))
					xalt=k;
				end
			end
			if(isstr(xalt)) xalt=0; end
		end
		flag=get(h(36),'checked');
		if(strcmp(flag,'on')&xalt)
			p(12)=1;
			p(13)=xalt;
		else
			p(12)=0;
			p(13)=xalt;
		end
		% alt y axis 
		yalt=get(h(37),'userdata');
		if(isstr(yalt))
			%resolve the name to a number
			hymenus=get(h(8),'userdata'); %the y axis menus
			for k=1:hymenus(1)
				if(strcmp(get(hymenus(k+1),'label'),yalt))
					yalt=k;
				end
			end
			if(isstr(yalt)) yalt=0; end
		end
		flag=get(h(37),'checked');
		if(strcmp(flag,'on')&yalt)
			p(14)=1;
			p(15)=yalt;
		else
			p(14)=0;
			p(15)=yalt;
		end
		%complex data option
		dat=get(h(15),'userdata');
		p(16)=dat(1);
		% mode option
		dat=get(h(10),'userdata');
		p(17)=dat(1);
		%polynomial order
		dat=get(h(16),'userdata');
		p(18)=dat(1);
		%number of the slope tool
		p(19)=get(h(21),'userdata');
		%algebra equation
		p(20:24)=get(h(25),'userdata');
		%x data option
		hxmenus=get(h(7),'userdata'); %the x axis menus
		dat=get(hxmenus(hxmenus(1)+2),'userdata');
		p(25)=dat(1);
		hymenus=get(h(8),'userdata'); %the y axis menus
		dat=get(hymenus(hxmenus(1)+2),'userdata');
		p(26)=dat(1);
		% the auto identify option
		flag=get(h(34),'checked');
		if(strcmp(flag,'on'))
			p(27)=1;
		else
			p(27)=0;
		end
		%the number of colors
		p(28)=get(h(41),'userdata');
		sliceset=contobj('slicetool_settings','prvt');
		sliceset=objset(sliceset,'parameters',p);
		cobj=objset(cobj,'slicetool_settings',sliceset);
		
		% see if we are saving via slicemaster
		if( strcmp(filename,'slicemaster') )
			smfig=get(hgrid,'userdata');
			slicemaster('saveslice',cobj,smfig);
		else
		
			%copy the object into a variable whose name is the filename
			eval([filename '=cobj;']);
			% ok, now write it out
		
			if(strcmp(computer,'MAC2') )
				if( matout )
					eval(['save ' filename ' ' filename]);
				elseif( asciiout )
					eval(['save ' filename ' ' filename ' -ascii']);
				end
			else
				if( matout )
					eval(['save ' fullfilename ' ' filename]);
				elseif( asciiout )
					eval(['save ' fullfilename ' ' filename ' -ascii']);
				end
			end
		end
	end % thats all for now
 %end
	if( strcmp(filename,'slicemaster') )
		set(hmsg,'string','Output successful to Slice Master');
	else
		set(hmsg,'string',['Output successful to ' filenamelabel]);
	end
	
	return;
	
end
if( strcmp(action,'petersdepth') )
	h=get(gcf,'userdata');
	hmsg=h(11);
	%get the selection box
    box=selboxfini;
    try
       delete(box{2});
    catch
       %no selbox to delete
    end
    box = box{1};
    
	xmin = min(box(1), box(3));
	xmax = max(box(1), box(3));
	ymin = min(box(2), box(4));
	ymax = max(box(2), box(4));
	% find the maximum slope in the box
		
% get the curve information	
	haction=h(6);
	actinfo=get(haction,'userdata');
	hident=actinfo(7);
	idinfo=get(hident,'userdata');
	
% get the number of curves and the curve handles
	ncurves = (length(idinfo)-1)/2;
	hcurves = idinfo(2:ncurves+1);
	
% get the number of x axis vectors
	 hxaxis=h(7);
   	 xinfo = get(hxaxis,'userdata');
   	 xdisp=xinfo(3+xinfo(1):length(xinfo) );
	 nx=length(xdisp);
% determine y(x) or x(y)
	if(nx==1) yofx=1; else yofx=0; end
	
	if( yofx ) 
		xd='xdata'; 
		yd='ydata';
	else
		xd='ydata';
		yd='xdata';
		xmin=ymin;
		xmax=ymax;
	end
	
% loop over curves
	for k=1:ncurves
		kolor=get(hcurves(k),'color');
		xdata=get(hcurves(k),xd);
		ydata=get(hcurves(k),yd);
		ndata=length(xdata);
		
		%compute slopes
		slopes=diff(ydata)./diff(xdata);
		xslopes=(xdata(1:ndata-1)+xdata(2:ndata))/2;
		
		% find the max(abs(slope)) in the selction box
		ind = between(xmin,xmax,xslopes,2);
		
		sm= max(abs(slopes(ind)));
		im=find( abs(slopes(ind))==sm);
		
		sm=slopes(ind(im));
		xm=xslopes(ind(im));
		ym=interp1(xdata,ydata,xm);
		
		% compute the half slope value
		
		sm2=sm/2.;
		
		% find the half slope points
		sdiff=slopes-sm2;	
		ind = surround(sdiff,0.0);
		sd2=sdiff(2:length(sdiff));
		% ind will point to half slope pts. Now interpolate their
		% x coordinates
		x2=xslopes(ind)+(sm2-slopes(ind)).*(xslopes(ind+1)-...
			xslopes(ind)) ./(slopes(ind+1)-slopes(ind));
		% find the half slope points nearest the max slope pt
		% must be at least 2 half slope pts
		if( length(ind)<2 )
			set(hmsg,'string','Unable to find 2 half slope pts');
		end
		ind=surround(x2,xm);
		y2=interp1(xdata,ydata,x2);
		% ok ind should contain 1 index, pointing to the half slope
		% point one one side of xm while ind+1 points to the other
		%
		% compute the depth
		fudge=1.6;
		magdepth = abs(x2(ind)-x2(ind+1))/fudge;
		
		% display the max and half slope lines
		linelen=(xmax-xmin)/5;
		hmax=putline(xm,ym,sm,linelen,kolor);
		h21=putline(x2(ind),y2(ind),sm2,linelen,kolor);
		h22=putline(x2(ind+1),y2(ind+1),sm2,linelen,kolor);
		% set the curve to dots
		set(hcurves(k),'linestyle',':');
		% display the result in the msg window
		set(hmsg,'string',sprintf(' depth = %g ',magdepth));
	end
end
if(strcmp(action,'resetslopetool') )
	h=get(gcf,'userdata');
	haction=h(6);
	actdat=get(haction,'userdata');
	hpickslopes=actdat(5);
	%set(gcf,'currentmenu',hpickslopes);
	slicetool('actions');
	return;
end
if(strcmp(action,'Hardcopy') )
	h=get(gcf,'userdata');
	hhardcopy=h(40);
	
	set(gcf,'pointer','watch');
	scales=get(hhardcopy,'userdata');
	
        tit='(Do not use alt axis labels for scales)';
	shardcopy(gcf,'slicetool(''Hardcopy2'')',...
                         scales(1),scales(2),'temp.ps',tit)
    return;
end
if(strcmp(action,'Hardcopy2') )
        h = get(gcf,'userdata');
        hhardcopy=h(40);
        hmessage = h(11);
        [ps_scale,xlength,ylength,xscale,yscale] = shardcopyfini;
	set(hmessage,'string',...
	      ['plotfile name is temp.ps, use ps_scale = ' num2str(ps_scale) ' in CHVSUB']);
      
	%remember the scales
	set(hhardcopy,'userdata',[xscale yscale]);
	%give message of expected plot size
	telluserinit( [' Plot width is ' num2str(xlength) ...
                       ', Plot height is ' num2str(ylength)]);
	set(gcf,'pointer','arrow');
	return
end
if(strcmp(action,'defineqn') )
	% put up an askthings dialog
	% 
	h=get(gcf,'userdata');
	hdefineqn=h(25);
	eqn=get(hdefineqn,'userdata');
	q=strmat('Coefficient ''a'':','Coefficient ''b'':');
	q=strmat(q,'Exponent ''m'':');
	q=strmat(q,'Exponent ''n'':');
	q=strmat(q,'Equation type: ');
	if( eqn(5) == 1)
		eqnstr='add|multiply';
	else
		eqnstr='multiply|add';
	end
	a=strmat(num2str(eqn(1)),num2str(eqn(2)));
	a=strmat(a,num2str(eqn(3)));
	a=strmat(a,num2str(eqn(4)));
	a=strmat(a,eqnstr);
	askthingsinit('slicetool(''defineqn2'')',q,a,[1 1 1 1 1],...
		'Define: (a*curve1^m)(+ or *)(b*curve2^n)');
	return;
end
if(strcmp(action,'defineqn2'))
	h=get(gcf,'userdata');
	hdefineqn=h(25);
	aa=askthingsfini;
	if(aa==-1)
		return;
	end
	a=sscanf(aa(1,:),'%f');
	b=sscanf(aa(2,:),'%f');
	m=sscanf(aa(3,:),'%f');
	n=sscanf(aa(4,:),'%f');
	type=aa(5,:);
	if( strcmp(type(1:3),'add') ) 
		type=1;
	else
		type=2;
	end
	set(hdefineqn,'userdata',[a b m n type]);
	return;
end
if(strcmp(action,'definesm') )
	% put up an askthings dialog
	% 
	h=get(gcf,'userdata');
	hdefinesm=h(44);
	sm=get(hdefinesm,'userdata');
	q=strmat('Smooth length ''n'':','Smoother type: ');
	if( sm(2) == 1)
		eqnstr='box|triangle';
	else
		eqnstr='triangle|box';
	end
%       a=strmat(num2str(sm(1)),eqnstr);
	if     ( sm(1) == 3)
        	a=strmat('3|5|7|11|17|31|51|91',eqnstr);
	elseif ( sm(1) == 5)
        	a=strmat('5|3|7|11|17|31|51|91',eqnstr);
	elseif ( sm(1) == 7)
        	a=strmat('7|3|5|11|17|31|51|91',eqnstr);
	elseif ( sm(1) == 11)
        	a=strmat('11|3|5|7|17|31|51|91',eqnstr);
	elseif ( sm(1) == 17)
        	a=strmat('17|3|5|7|11|31|51|91',eqnstr);
	elseif ( sm(1) == 31)
        	a=strmat('31|3|5|7|11|17|51|91',eqnstr);
	elseif ( sm(1) == 51)
        	a=strmat('51|3|5|7|11|17|31|91',eqnstr);
	elseif ( sm(1) == 91)
        	a=strmat('91|3|5|7|11|17|31|51',eqnstr);
	else 
        	a=strmat('3|5|7|11|17|31|51|91',eqnstr);
	end
	askthingsinit('slicetool(''definesm2'')',q,a,[1 1],...
		'Define: n = number of points in the smoother');
	return;
end
if(strcmp(action,'definesm2'))
	h=get(gcf,'userdata');
	hdefinesm=h(44);
	hmsg=h(11);
	aa=askthingsfini;
	if(aa==-1)
		return;
	end
	n=sscanf(aa(1,:),'%f');
	type=aa(2,:);
	if( strcmp(type(1:3),'box') ) 
		type=1;
	else
		type=2;
	end
%	make sure n is odd
	nodd = 2*fix(n/2)+1;
	if(nodd ~= n)
		n = nodd;
		set(hmsg,'string','Warning, n changed to an odd number');
	end
	set(hdefinesm,'userdata',[n type]);
	return;
end
if(strcmp(action,'algebra'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	halgebra=h(23);
	hmsg=h(11);
		  % uncheck previous action and check this one
        haction=h(6);
        flag = get(halgebra,'userdata');
			 flag=flag(1);
        dat = get(haction,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(halgebra,'checked','on');
			  dat(1)=flag;
        set(haction,'userdata',dat);
	set(gcf,'windowbuttondownfcn','slicetool(''algebra1'')');
	set(gcf,'windowbuttonmotionfcn','');
	set(gcf,'windowbuttonupfcn','');
	set(hmsg,'string','Algebra ... MB1: select curve 1');
	
	set(hdoit,'enable','off');
	return;
end
if(strcmp(action,'algebra1'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	halgebra=h(23);
	hmsg=h(11);
	hobj=gco;
	if( ~strcmp(get(hobj,'type'),'line') )
		set(hmsg,'string','You must select a line object for curve 1, try again');
		return;
	end
	ud=get(halgebra,'userdata');
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);
	
	%double the linewidth to indicate selection
	if(length(ud)>1) 
		hkids=get(gca,'children');
		itest=find(hkids==ud(2))
		if( isempty(itest) )
			hold1=[];
			htext1=[];
		else
			hold1=ud(2);
			htext1=ud(5);
		end
	else
		hold1=[];
		htext1=[];
	end
	if( hold1~=hobj )
		lw=get(hobj,'linewidth');
		ms=get(hobj,'markersize');
		set(hobj,'linewidth',4*lw,'markersize',2*ms);
		
		if(length(ud)>1) 
			if( length(ud)>5 )
				if(hold1==ud(6))%same curve selected twice
					set(hold1,'linewidth',4*ud(3),'markersize',2*ud(4));
				else
					set(hold1,'linewidth',ud(3),'markersize',ud(4));
				end
			else
				set(hold1,'linewidth',ud(3),'markersize',ud(4));
			end
			ud(2:5)=[hobj lw ms 0];
		else
			ud=[ud hobj lw ms 0];
		end
	%a text label
		delete(htext1);
		kol=get(hobj,'color');
		ud(5)=text(xpt,ypt,'curve1','color',kol);
		set(halgebra,'userdata',ud);
	end
	set(gcf,'windowbuttondownfcn','slicetool(''algebra2'')');
	set(hmsg,'string','Algebra ... MB1: select curve 2');
	return;
end
if(strcmp(action,'algebra2'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hautopro=h(22);
	halgebra=h(23);
	hdefineqn=h(25);
	hpromote=h(24);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	hobj2=gco;
	if( ~strcmp(get(hobj2,'type'),'line') )
		set(hmsg,'string','You must select a line object for curve 2, try again');
		return;
	end
	ud=get(halgebra,'userdata');
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);
	hobj1=ud(2);
	
	%double the linewidth to indicate selection
	if(length(ud)>5) 
		hkids=get(gca,'children');
		itest=find(hkids==ud(6));
		if( isempty(itest) )
			hold2=[];
			htext2=[];
		else
			hold2=ud(6);
			htext2=ud(9);
		end
	else
		hold2=[];
		htext2=[];
	end
	if( hobj2~=hold2 )
		if( hobj2 ~= ud(2) )% test for same curve selected twice
			lw=get(hobj2,'linewidth');
			ms=get(hobj2,'markersize');
			set(hobj2,'linewidth',4*lw,'markersize',2*ms);
		else
			lw=ud(3); ms=ud(4);
			set(hobj2,'linewidth',8*lw,'markersize',4*ms);
		end
		if( ~isempty(hold2) )
			set(hold2,'linewidth',ud(7),'markersize',ud(8));
		end	
		%textlabel
		delete(htext2);
		kol=get(hobj2,'color');
		htext2=text(xpt,ypt,'curve2','color',kol);
		set(halgebra,'userdata',[ud(1:5) hobj2 lw ms htext2]);
	end
	set(gcf,'windowbuttondownfcn','slicetool(''algebra1'')');
	set(hmsg,'string','Algebra ... Doit to compute or MB1: select curve 1');
	set(hdoit,'enable','on');
	return;
end
if(strcmp(action,'algebra3'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hautopro=h(22);
	halgebra=h(23);
	hdefineqn=h(25);
	hpromote=h(24);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	
	ud=get(halgebra,'userdata');
	hobj1=ud(2);
	hobj2=ud(6);
	set(hobj1,'linewidth',ud(3),'markersize',ud(4));
	set(hobj2,'linewidth',ud(7),'markersize',ud(8));
	delete(ud(5));
	delete(ud(9));
		
	
	% now form the new curve
	x1=get(hobj1,'xdata');
	y1=get(hobj1,'ydata');
	x2=get(hobj2,'xdata');
	y2=get(hobj2,'ydata');
	eqn=get(hdefineqn,'userdata');
% determine y(x) or x(y)
% get the number of x axis vectors
  xinfo = get(hxaxis,'userdata');
  xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
  % get the number of y axis vectors
  yinfo = get(hyaxis,'userdata');
  ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
	% determine number of curves and y(x) or x(y)
		
	if( nx==1 )
		yofx=1;
	else
		yofx=0;
	end
	if( yofx )
		if(eqn(5)==1)
			y3= eqn(1)*y1.^eqn(3) + eqn(2)*y2.^eqn(4);
		elseif(eqn(5)==2)
			y3= (eqn(1)*y1.^eqn(3)).*(eqn(2)*y2.^eqn(4));
		end
		%plot
		hnew=line(x1,y3,'color','w');
	else
		if(eqn(5)==1)
			x3= eqn(1)*x1.^eqn(3) + eqn(2)*x2.^eqn(4);
		elseif(eqn(5)==2)
			x3= (eqn(1)*x1.^eqn(3)).*(eqn(2)*x2.^eqn(4));
		end
		%plot
		hnew=line(x3,y1,'color','w');
	end
	%Expand the axes to make sure we see things
	if(yofx)
		xlim=get(gca,'xlim');
		set(gca,'xlimmode','manual','xlim',xlim,'ylimmode','auto');
	else
		ylim=get(gca,'ylim');
		set(gca,'ylimmode','manual','ylim',ylim,'xlimmode','auto');
	end
	
	set(gcf,'windowbuttondownfcn','slicetool(''algebra1'')');
	set(hmsg,'string','Algebra ... MB1: select curve 1');
	
	set(hdoit,'enable','off');
	set(halgebra,'userdata',ud(1));
	%see if we are in auto promote mode
	if(strcmp(get(hautopro,'checked'),'on'))
		hfig=gcf;
		set(hfig,'currentobject',hnew);
		set(hfig,'currentmenu',hpromote);
		set(hautopro,'userdata','slicetool(''algebra'')');
		slicetool('promote2');
	end
	return;
end
	
%
if(strcmp(action,'smooth'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hsmooth=h(43);
	hmsg=h(11);
		  % uncheck previous action and check this one
        haction=h(6);
        flag = get(hsmooth,'userdata');
			 flag=flag(1);
        dat = get(haction,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hsmooth,'checked','on');
			  dat(1)=flag;
        set(haction,'userdata',dat);
	set(gcf,'windowbuttondownfcn','slicetool(''smooth1'')');
	set(gcf,'windowbuttonmotionfcn','');
	set(gcf,'windowbuttonupfcn','');
	set(hmsg,'string','Smooth ... MB1: select curve to smooth');
	
	set(hdoit,'enable','off');
	return;
end
if(strcmp(action,'smooth1'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hsmooth=h(43);
	hmsg=h(11);
	hobj=gco;
	if( ~strcmp(get(hobj,'type'),'line') )
		set(hmsg,'string','You must select a line object for the curve, try again');
		return;
	end
	ud=get(hsmooth,'userdata');
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);
	
	%double the linewidth to indicate selection
	if(length(ud)>1) 
		hkids=get(gca,'children');
		itest=find(hkids==ud(2))
		if( isempty(itest) )
			hold1=[];
			htext1=[];
		else
			hold1=ud(2);
			htext1=ud(5);
		end
	else
		hold1=[];
		htext1=[];
	end
	if( hold1~=hobj )
		lw=get(hobj,'linewidth');
		ms=get(hobj,'markersize');
		set(hobj,'linewidth',4*lw,'markersize',2*ms);
		
		if(length(ud)>1) 
			if( length(ud)>5 )
				if(hold1==ud(6))%same curve selected twice
					set(hold1,'linewidth',4*ud(3),'markersize',2*ud(4));
				else
					set(hold1,'linewidth',ud(3),'markersize',ud(4));
				end
			else
				set(hold1,'linewidth',ud(3),'markersize',ud(4));
			end
			ud(2:5)=[hobj lw ms 0];
		else
			ud=[ud hobj lw ms 0];
		end
	%a text label
		delete(htext1);
		kol=get(hobj,'color');
		ud(5)=text(xpt,ypt,'curve1','color',kol);
		set(hsmooth,'userdata',ud);
	end
	set(gcf,'windowbuttondownfcn','slicetool(''smooth1'')');
	set(hmsg,'string','Doit to compute or MB1: select curve');
	set(hdoit,'enable','on');
	return;
end
if(strcmp(action,'smooth3'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hautopro=h(22);
	hsmooth=h(43);
	hpromote=h(24);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	hdefinesm=h(44);
	
	ud=get(hsmooth,'userdata');
	hobj1=ud(2);
	set(hobj1,'linewidth',ud(3),'markersize',ud(4));
	delete(ud(5));
		
	
	% now form the new curve
	x1=get(hobj1,'xdata');
	y1=get(hobj1,'ydata');
%   sm(1) is length of smoother
%   if sm(2)=1, use box, if sm(2)=2, triangle
	sm = get(hdefinesm,'userdata');
% determine y(x) or x(y)
% get the number of x axis vectors
  xinfo = get(hxaxis,'userdata');
  xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
  % get the number of y axis vectors
  yinfo = get(hyaxis,'userdata');
  ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
	% determine number of curves and y(x) or x(y)
		
	if( nx==1 )
		yofx=1;
	else
		yofx=0;
	end
	if( yofx )
		if(sm(2)==1)
			y3= filtboxnan(y1,sm(1));
		elseif(sm(2)==2)
			y3= filttrinan(y1,sm(1));
		end
		%plot
		hnew=line(x1,y3,'color','w');
	else
		if(sm(2)==1)
			x3= filtboxnan(x1,sm(1));
		elseif(sm(2)==2)
			x3= filttrinan(x1,sm(1));
		end
		%plot
		hnew=line(x3,y1,'color','w');
	end
	%Expand the axes to make sure we see things
	if(yofx)
		xlim=get(gca,'xlim');
		set(gca,'xlimmode','manual','xlim',xlim,'ylimmode','auto');
	else
		ylim=get(gca,'ylim');
		set(gca,'ylimmode','manual','ylim',ylim,'xlimmode','auto');
	end
	
	set(gcf,'windowbuttondownfcn','slicetool(''smooth1'')');
	set(hmsg,'string','Smooth ... MB1: select curve 1');
	
	set(hdoit,'enable','off');
	set(hsmooth,'userdata',ud(1));
	%see if we are in auto promote mode
	if(strcmp(get(hautopro,'checked'),'on'))
		hfig=gcf;
		set(hfig,'currentobject',hnew);
		set(hfig,'currentmenu',hpromote);
		set(hautopro,'userdata','slicetool(''smooth'')');
		slicetool('promote2');
	end
	return;
end
	
%
if(strcmp(action,'deriv'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hderiv=h(45);
	hmsg=h(11);
		  % uncheck previous action and check this one
        haction=h(6);
        flag = get(hderiv,'userdata');
			 flag=flag(1);
        dat = get(haction,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hderiv,'checked','on');
			  dat(1)=flag;
        set(haction,'userdata',dat);
	set(gcf,'windowbuttondownfcn','slicetool(''deriv1'')');
	set(gcf,'windowbuttonmotionfcn','');
	set(gcf,'windowbuttonupfcn','');
	set(hmsg,'string','Derivative ... MB1: select curve');
	
	set(hdoit,'enable','off');
	return;
end
if(strcmp(action,'deriv1'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hderiv=h(45);
	hmsg=h(11);
	hobj=gco;
	if( ~strcmp(get(hobj,'type'),'line') )
		set(hmsg,'string','You must select a line object for the curve, try again');
		return;
	end
	ud=get(hderiv,'userdata');
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);
	
	%double the linewidth to indicate selection
	if(length(ud)>1) 
		hkids=get(gca,'children');
		itest=find(hkids==ud(2))
		if( isempty(itest) )
			hold1=[];
			htext1=[];
		else
			hold1=ud(2);
			htext1=ud(5);
		end
	else
		hold1=[];
		htext1=[];
	end
	if( hold1~=hobj )
		lw=get(hobj,'linewidth');
		ms=get(hobj,'markersize');
		set(hobj,'linewidth',4*lw,'markersize',2*ms);
		
		if(length(ud)>1) 
			if( length(ud)>5 )
				if(hold1==ud(6))%same curve selected twice
					set(hold1,'linewidth',4*ud(3),'markersize',2*ud(4));
				else
					set(hold1,'linewidth',ud(3),'markersize',ud(4));
				end
			else
				set(hold1,'linewidth',ud(3),'markersize',ud(4));
			end
			ud(2:5)=[hobj lw ms 0];
		else
			ud=[ud hobj lw ms 0];
		end
	%a text label
		delete(htext1);
		kol=get(hobj,'color');
		ud(5)=text(xpt,ypt,'curve1','color',kol);
		set(hderiv,'userdata',ud);
	end
	set(gcf,'windowbuttondownfcn','slicetool(''deriv1'')');
	set(hmsg,'string','Doit to compute or MB1: select curve');
	set(hdoit,'enable','on');
	return;
end
if(strcmp(action,'deriv3'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hautopro=h(22);
	hderiv=h(45);
	hpromote=h(24);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	
	ud=get(hderiv,'userdata');
	hobj1=ud(2);
	set(hobj1,'linewidth',ud(3),'markersize',ud(4));
	delete(ud(5));
		
	
	% now form the new curve
	x1=get(hobj1,'xdata');
	y1=get(hobj1,'ydata');
% determine y(x) or x(y)
% get the number of x axis vectors
  xinfo = get(hxaxis,'userdata');
  xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
  % get the number of y axis vectors
  yinfo = get(hyaxis,'userdata');
  ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
	% determine number of curves and y(x) or x(y)
		
	if( nx==1 )
		yofx=1;
	else
		yofx=0;
	end
	if( yofx )
		y3= gradient(y1);
		%plot
		hnew=line(x1,y3,'color','w');
	else
		x3= gradient(x1);
		%plot
		hnew=line(x3,y1,'color','w');
	end
	%Expand the axes to make sure we see things
	if(yofx)
		xlim=get(gca,'xlim');
		set(gca,'xlimmode','manual','xlim',xlim,'ylimmode','auto');
	else
		ylim=get(gca,'ylim');
		set(gca,'ylimmode','manual','ylim',ylim,'xlimmode','auto');
	end
	
	set(gcf,'windowbuttondownfcn','slicetool(''deriv1'')');
	set(hmsg,'string','Derivative ... MB1: select curve 1');
	
	set(hdoit,'enable','off');
	set(hderiv,'userdata',ud(1));
	%see if we are in auto promote mode
	if(strcmp(get(hautopro,'checked'),'on'))
		hfig=gcf;
		set(hfig,'currentobject',hnew);
		set(hfig,'currentmenu',hpromote);
		set(hautopro,'userdata','slicetool(''deriv'')');
		slicetool('promote2');
	end
	return;
end
	
%
% promote a temporary computed curve to full status
% or delete a full curve
%
if(strcmp(action,'promote'))
	h=get(gcf,'userdata');
	hmsg=h(11);
	hpromote=h(24);
		  % uncheck previous action and check this one
        haction=h(6);
        flag = get(hpromote,'userdata');
			 flag=flag(1);
        dat = get(haction,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hpromote,'checked','on');
			  dat(1)=flag;
        set(haction,'userdata',dat);
	set(hmsg,'string','MB1: select curve to promote  MB2: delete selected curve');
	set(gcf,'windowbuttondownfcn','slicetool(''promote2'')');
	set(gcf,'windowbuttonmotionfcn','');
	set(gcf,'windowbuttonupfcn','');
	return;
end
if(strcmp(action,'promote2'))
	h=get(gcf,'userdata');
	hmsg=h(11);
	hpromote=h(24);
	hobj=gco;
	%determine which mouse button was pressed
	flag=get(gcf,'selectiontype');
	%see if we are deleteing
	if( strcmp(flag,'extend') )
		slicetool('delete');
		return;
	end
	if( ~strcmp(get(hobj,'type'),'line') )
		set(hmsg,'string','You must select a line with MB1, Please try again');
		return;
	end
	%see if its already a full-status line
		% see if it is one of the curves
		% determine the action state
		haction = h(6);
		actdat=get(haction,'userdata');
		hident=actdat(7);
		% The basic idea here is that hident userdata 
		% will provide the handles of the plotted curves and the handles
		% of an optional text label naming the curve. If there are 2 subplots,
		% then this is repeated in a second row.
		% so if ndisp is the number of displayed curves,
		% then the size of the user data is [1,2*ndisp+1]for 1 subplot and
		% [2,2*ndisp+1] for 2 subplots.
		idinfo=get(hident,'userdata');
		curvenum=-999;
		
		[l,m]=size(idinfo);
		
%
% NOTE: This id mechanism does not work for a subdivided plot
%
		for k=2:m
			if( hobj==idinfo(k) )
				curvenum=k; % we found the curve
				break;
			end
		end
		if( curvenum~=-999 )
			hmsg=h(11);
			set(hmsg,'string','Selected curve is already promoted... Try again');
			return;
		end
	%save the curve handle
	ud=get(hpromote,'userdata');
	set(hpromote,'userdata',[ud(1) hobj]);
	%ok, ask for a name for the promoted curve
	askthingsinit('slicetool(''promote3'')','Name of new curve:');
	set(gcf,'windowbuttondownfcn','');
	return;
end
if(strcmp(action,'promote3')|strcmp(action,'promote4'))
	
	h=get(gcf,'userdata');
	hdoit=h(1);
	hpromote=h(24);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	hautopro=h(22);
	set(gcf,'windowbuttondownfcn','slicetool(''promote2'')');
	set(hmsg,'string','MB1: select curve to promote  MB2: delete selected curve');
	ud=get(hpromote,'userdata');
	hobj=ud(2);
	% now form the new curve
	x=get(hobj,'xdata');
	y=get(hobj,'ydata');
% determine y(x) or x(y)
% get the number of x axis vectors
  xinfo = get(hxaxis,'userdata');
  xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
  % get the number of y axis vectors
  yinfo = get(hyaxis,'userdata');
  ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
	% determine number of curves and y(x) or x(y)
		
	if( nx==1 )
		yofx=1;
	else
		yofx=0;
	end
	% get the name of the object
	name=askthingsfini;
	if( name== -1)
		return; %the user cancels
	end
	%get the object
	sliceobj=get(hdoit,'userdata');
	%see if a curve already exists by that name
	names=objget(sliceobj,'namesmatrix');
	[ndata,c]=size(names);
	item_num=ndata+1;
	nl=length(name);
	if( nl<=c)
		for k=1:ndata
			if(strcmp(name,names(k,1:nl)) )
				item_num=k;
				break;
			end
		end
	end
		
	if( item_num<=ndata & strcmp(action,'promote3') )
		askthings('slicetool(''promote4'')','Name of new curve:',name,1,...
		'Curve already exists, push done to replace it');
	end
	if( yofx )
		sliceobj=objset(sliceobj,name,y);
		names=objget(sliceobj,'namesmatrix');
		[ndatanew,c]=size(names);
		% if we are replacing an item, then we need not make a new menu
		if( ndatanew==ndata )
			% check the menu item of the replaced data
			hmenu=yinfo(1+item_num);
			%set(gcf,'currentmenu',hmenu);
			slicetool('ydata');
		else
			%make a new menu
			hymenu=uimenu(hyaxis,'label',name,'checked','off','userdata',ndatanew,...
				'callback','slicetool(''ydata'')','position',ndatanew);
			hxmenu=uimenu(hxaxis,'label',name,'checked','off','userdata',ndatanew,...
				'callback','slicetool(''xdata'')','position',ndatanew);
			yinfo=[ndatanew yinfo(2:ndata+1) hymenu yinfo(ndata+2:length(yinfo))];
			xinfo=[ndatanew xinfo(2:ndata+1) hxmenu xinfo(ndata+2:length(xinfo))];
			%make sure options menu stays at end
			set(yinfo(ndatanew+2),'position',ndatanew+1);
			set(xinfo(ndatanew+2),'position',ndatanew+1);
			set(hyaxis,'userdata',yinfo);
			set(hxaxis,'userdata',xinfo);
			%set(gcf,'currentmenu',hymenu);
			slicetool('ydata');
		end
	else
		sliceobj=objset(sliceobj,name,x);
		names=objget(sliceobj,'namesmatrix');
		[ndatanew,c]=size(names);
		% if we are replacing an item, then we need not make a new menu
		if( ndatanew==ndata )
			% check the menu item of the replaced data
			hmenu=yinfo(1+item_num);
			%set(gcf,'currentmenu',hmenu);
			slicetool('xdata');
		else
			%make a new menu
			hymenu=uimenu(hyaxis,'label',name,'checked','off','userdata',ndatanew,...
				'callback','slicetool(''ydata'')','position',ndatanew);
			hxmenu=uimenu(hxaxis,'label',name,'checked','off','userdata',ndatanew,...
				'callback','slicetool(''xdata'')','position',ndatanew);
			yinfo=[ndatanew yinfo(2:ndata+1) hymenu yinfo(ndata+2:length(yinfo))];
			xinfo=[ndatanew xinfo(2:ndata+1) hxmenu xinfo(ndata+2:length(xinfo))];
			%make sure options menu stays at end
			set(yinfo(ndatanew+2),'position',ndatanew+1);
			set(xinfo(ndatanew+2),'position',ndatanew+1);
			set(hyaxis,'userdata',yinfo);
			set(hxaxis,'userdata',xinfo);
			%set(gcf,'currentmenu',hxmenu);
			slicetool('xdata');
		end
	end
	%put the object back
	set(hdoit,'userdata',sliceobj);
	% modify the message
	set(hmsg,'string','Continue promoting or Plot/Doit for new plot');
	%see if there is a command in hautopro to execute
	cmd=get(hautopro,'userdata');
	if( ~isempty(cmd) )
		set(hautopro,'userdata',[]);
		eval(cmd);
	end
	return;
end
		
if(strcmp(action,'delete'))
	
	h=get(gcf,'userdata');
	hdoit=h(1);
	hpromote=h(24);
	hmsg=h(11);
	hxaxis=h(7);
	hyaxis=h(8);
	set(gcf,'windowbuttondownfcn','slicetool(''promote2'')');
	set(hmsg,'string','MB1: select curve to promote  MB2: delete selected curve');
	hobj=gco;
	if( ~strcmp(get(hobj,'type'),'line'))
		set(hmsg,'string','You must select a line, MB1: promote MB2: delete');
		return;
	end
	%
	% now invoke the Rube-Goldberg id mechanism to determine the curve's id
	% get the curve information	
	haction=h(6);
	actinfo=get(haction,'userdata');
	hident=actinfo(7);
		
		% The basic idea here is that hident userdata 
		% will provide the handles of the plotted curves and the handles
		% of an optional text label naming the curve. If there are 2 subplots,
		% then this is repeated in a second row.
		% so if ndisp is the number of displayed curves,
		% then the size of the user data is [1,2*ndisp+1]for 1 subplot and
		% [2,2*ndisp+1] for 2 subplots.
		idinfo=get(hident,'userdata');
		curvenum=-999;
		
		[l,m]=size(idinfo);
		
%
% NOTE: This id mechanism does not work for a subdivided plot
%
		for k=2:m
			if( hobj==idinfo(k) )
				curvenum=k; % we found the curve
				break;
			end
		end
		if( strcmp(get(hobj,'type'),'text') )
			hmsg=h(11);
			set(hmsg,'string','Select curve not text object');
			return;
		end
		
		
		%return if the curve was not found
		if( curvenum== -999)
			hmsg=h(11);
			set(hmsg,'string',...
				'Temporary curves need not be deleted ... MB1 promote  MB2 delete');
			return;
		end
		%update the idinfo
		idinfo(:,curvenum)=[];
		idinfo(:,curvenum-1+(m-1)/2)=[];
		set(hident,'userdata',idinfo);
		
		hxaxis=h(7);
        hyaxis=h(8);
        % get the number of x axis vectors
        xinfo = get(hxaxis,'userdata');
        xdisp=xinfo(3+xinfo(1):length(xinfo) );
		nx=length(xdisp);
        % get the number of y axis vectors
        yinfo = get(hyaxis,'userdata');
        ydisp=yinfo(3+yinfo(1):length(yinfo) );
		ny=length(ydisp);
		
		if( nx==1 )
			numcurves=ny;
			if( curvenum <= numcurves+1 )
				curveid=ydisp(curvenum-1);
			elseif( curvenum> 2*numcurves+1 & curvenum <= 3*numcurves+1)
				curveid=ydisp(curvenum-2*numcurves-1);
			end
		else
			numcurves=nx;
			if( curvenum <= numcurves+1 )
				curveid=xdisp(curvenum-1);
			elseif( curvenum> 2*numcurves+1 & curvenum <= 3*numcurves+1)
				curveid=xdisp(curvenum-2*numcurves-1);
			end
		end
	%get the object
	sliceobj=get(hdoit,'userdata');
		%delete the curve from the object
		names=objget(sliceobj,'namesmatrix');
		[ndata,c]=size(names);
		sliceobj=objset(sliceobj,names(curveid,:),[]);
		% delete the menu
			delete(yinfo(1+curveid));
			delete(xinfo(1+curveid));
		%update the menuinfo
			ndata=ndata-1;
			yinfo=[ndata yinfo(2:curveid) yinfo(curveid+2:length(yinfo))];
			ydisp=yinfo(ndata+3:length(yinfo));
			ind=find(ydisp==curveid);
			if( ~isempty(ind) )
				ydisp(ind)=[];
			end
			if( ~isempty(ydisp) )
				ind=find(ydisp>curveid);
				ydisp(ind)=ydisp(ind)-1;
			end
			yinfo=[yinfo(1:ndata+2) ydisp];
			xinfo=[ndata xinfo(2:curveid) xinfo(curveid+2:length(xinfo))];
			xdisp=xinfo(ndata+3:length(xinfo));
			ind=find(xdisp==curveid);
			if( ~isempty(ind) )
				xdisp(ind)=[];
			end
			if( ~isempty(xdisp) )
				ind=find(xdisp>curveid);
				xdisp(ind)=xdisp(ind)-1;
			end
			xinfo=[xinfo(1:ndata+2) xdisp];
			set(hyaxis,'userdata',yinfo);
			set(hxaxis,'userdata',xinfo);
		%	slicetool('ydata');
		%	slicetool('xdata');
			
		%delete the curve
		delete(hobj);
	%put the object back
	set(hdoit,'userdata',sliceobj);
	% modify the message
	%set(gcf,'currentmenu',hpromote)
	set(hmsg,'string','Continue promoting/deleting or Plot/Doit for replot');
	return;
end
if(strcmp(action,'showfile'))
	h=get(gcf,'userdata');
	hmsg=h(11);
	
	obj=get(hmsg,'userdata');
	
	fileobj=objget(obj,'file');
	if( isempty(fileobj) )
		msg='Save file: undefined';
	else
		filename=objget(fileobj,'filename');
		pathname=objget(fileobj,'pathname');
		
		if(strcmp(filename,'slicemaster'))
			msg='Save to Slice Master';
		elseif( strcmp(pathname,'undefined') )
			msg='Save file: undefined';
		else
			msg=['Save file: ' pathname filename];
		end
	end
	
	set(hmsg,'string',msg);
	
	return;
end
if(strcmp(action,'down'))
	scroll=.5;
	ylim=get(gca,'ylim');
	yspan=abs(ylim(1)-ylim(2));
	shift=scroll*yspan;
	set(gca,'ylim',[ylim(1)-shift ylim(2)-shift]);
	return;
end
if(strcmp(action,'left'))
	scroll=.5;
	xlim=get(gca,'xlim');
	xspan=abs(xlim(1)-xlim(2));
	shift=scroll*xspan;
	set(gca,'xlim',[xlim(1)-shift xlim(2)-shift]);
	return;
end
if(strcmp(action,'right'))
	scroll=.5;
	xlim=get(gca,'xlim');
	xspan=abs(xlim(1)-xlim(2));
	shift=scroll*xspan;
	set(gca,'xlim',[xlim(1)+shift xlim(2)+shift]);
	return;
end
if(strcmp(action,'up'))
	scroll=.5;
	ylim=get(gca,'ylim');
	yspan=abs(ylim(1)-ylim(2));
	shift=scroll*yspan;
	set(gca,'ylim',[ylim(1)+shift ylim(2)+shift]);
	return;
end
% 
% establish the alternate axis labels
%
if(strcmp(action,'altlabelx')|strcmp(action,'altlabelx2'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hmsg=h(11);
	haltx=h(36);
	%get the slice object
	slice=get(hdoit,'userdata');
	if(strcmp(action,'altlabelx'))
		%off or on?
		flag=get(haltx,'checked');
		if(strcmp(flag,'on'))%turn it off
			set(haltx,'checked','off');
			return;
		else
			set(haltx,'checked','on');
		end
		q='Alternate X axis label:';
		a=get(haltx,'userdata');
		if(~isstr(a))
			if(~a)
				a=[];
			else
				a=objget(slice,'dataname',a);
			end
		end
		choices=objget(slice,'fieldnames');
		if( isempty(a) )
			ichoice=1;
		else
			it=findstr(choices,a);
			ind=findstr(choices,'|');
			ichoice=find(ind>it);
			if( isempty(ichoice) )
				ichoice=length(ind)+1;
			else
				ichoice=ichoice(1);
			end
		end
		askthingsinit('slicetool(''altlabelx2'')',q,choices,[ichoice]);
		return;
	end
	% get the response
	a=askthingsfini;
	if(a==-1) 
		set(haltx,'checked','off');
		return;
	end
	set(haltx,'userdata',a);
	return;
end
if(strcmp(action,'altlabely')|strcmp(action,'altlabely2'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hmsg=h(11);
	halty=h(37);
	%get the slice object
	slice=get(hdoit,'userdata');
	if(strcmp(action,'altlabely'))
		%off or on?
		flag=get(halty,'checked');
		if(strcmp(flag,'on'))%turn it off
			set(halty,'checked','off');
			return;
		else
			set(halty,'checked','on');
		end
		q='Alternate Y axis label:';
		a=get(halty,'userdata');
		if(~isstr(a))
			if(~a)
				a=[];
			else
				a=objget(slice,'dataname',a);
			end
		end
		choices=objget(slice,'fieldnames');
		if( isempty(a) )
			ichoice=1;
		else
			it=findstr(choices,a);
			ind=findstr(choices,'|');
			ichoice=find(ind>it);
			if( isempty(ichoice) )
				ichoice=length(ind)+1;
			else
				ichoice=ichoice(1);
			end
		end
		askthingsinit('slicetool(''altlabely2'')',q,choices,[ichoice]);
		return;
	end
	% get the response
	a=askthingsfini;
	if(a==-1) 
		set(halty,'checked','off');
		return;
	end
	set(halty,'userdata',a);
	return;
end
%
% generate a polynomial
%
if(strcmp(action,'polygen'))
	h=get(gcf,'userdata');
	hmsg=h(11);
	hpolygen=h(42);
	hpolyorder=h(16);
	
	% see if we got a valid response
	
	order=get(hpolyorder,'userdata');
	order=order(1);
	
		  % uncheck previous action and check this one
        haction=h(6);
        flag = get(hpolygen,'userdata');
			 flag=flag(1);
        dat = get(haction,'userdata');
        set(dat(dat(1)+1),'checked','off');
        set(hpolygen,'checked','on');
			  dat(1)=flag;
        set(haction,'userdata',dat);
	set(hmsg,'string',['Provide ' int2str(order+1) ' polynomial coefficients']);
	
	askthingsinit('slicetool(''polygen2'')','order n, order n-1 ... constant:',...
		' ',1,['Provide ' int2str(order+1) ' poly coeffs separated by spaces or commas']);
	set(gcf,'windowbuttondownfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	set(gcf,'windowbuttonupfcn','');
	return;
end
%
%
%
if(strcmp(action,'polygen2'))
	h=get(gcf,'userdata');
	hdoit=h(1);
	hmsg=h(11);
	hpolygen=h(42);
	hpolyorder=h(16);
	haction=h(6);
   actinfo=get(haction,'userdata');
   hident=actinfo(7);
   hxaxis=h(7);
   hyaxis=h(8);
	
	% see if we got a valid response
	
	order=get(hpolyorder,'userdata');
	order=order(1);
	
	a=askthingsfini;
	
	%test for cancel
	if(a==-1)
		return;
	end
	
	pcoefs=sscanf(a,'%f');
	
	if(length(pcoefs)~=order+1)
		set(hmsg,'string',['Error Incorrect number of coefficients']);
	
		askthingsinit('slicetool(''polygen2'')',...
			'order n, order n-1 ... constant:',a,1,...
			['Provide ' int2str(order+1) ' poly coeffs separated by spaces or commas']);
	
		return;
	end
	
	% determine the independent variable
	idinfo=get(hident,'userdata');
	
	% get the number of x axis vectors
   xinfo = get(hxaxis,'userdata');
   xids=xinfo(3+xinfo(1):length(xinfo) );
	nx=length(xids);
	
	% get the number of y axis vectors
	yinfo = get(hyaxis,'userdata');
	yids=yinfo(3+yinfo(1):length(yinfo) );
	ny=length(yids);
	
	% get the data object
	obj=get(hdoit,'userdata');
		
	%different cases for yofx or xofy
	if( nx==1 ) %yofx case
		x=objget(obj,xids);
	else
		x=objget(obj,yids);
	end
	
	%generate the polynomial
	y=polyval(pcoefs,x);
		
	% plot with line
				
	hpoly=line(x,y,'color','w','linestyle','-');
	
	set(hmsg,'string','');
	
	return;
end