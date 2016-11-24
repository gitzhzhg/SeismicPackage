function shardcopy(hfig,transfer,xscale,yscale,plotfile,tit)
% shardcopy(hfig,transfer,xscale,yscale,plotfile,tit);
% shardcopy(hfig,transfer,xscale,yscale,plotfile);
% shardcopy(hfig,transfer,xscale,yscale);
% shardcopy(hfig,transfer);
% shardcopy(hfig);
% shardcopy;
%
% shardcopy and shardcopyfini provide a dialog to determine the scale
% factor required in CHVSUB to produce an arbitrary sized plot
% from a MATLAB postscript file. This allows any MATLAB figure to
% be plotted in hardcopy at arbitrary specific x and y scales. (Note
% this has only been tested for 2-D axes). If precisely scaled hardcopy
% is not needed then it is usually simpler to just use MATLABs print
% command.
% To initiate the dialog, the master program makes sure the figure to
% be plotted is the current figure and then calls shardcopy with the
% noted arguments described below:
% hfig ... the handle of the figure to be plotted
% transfer ... a string containing a syntactically correct MATLAB command
%	to be called by shardcopy when done. This allows shardcopy to return
%	control to the calling program. For example, if program MYPROG wants
%	a scaled hardcopy, it could call shardcopy with transfer set to
%	something like 'myprog(''shardcopydone'')', where the single argument
%	is a flag to MYPROG that shardcopy is done
% xscale ... suggested (default) value for xscale to be shown in the 
%	popup dialog. Units are xdataunits/inch
%  ********** default is no scale ***********
% yscale ... suggested (default) value for yscale to be shown in the 
%	popup dialog. Units are ydataunits/inch
%  ********** default is no scale ***********
% plotfile ... name of the output plotfile
%	********* default is temp.ps **********
% tit ... title of the dialog
%	********* default is 'Specify Hardcopy Scales' *******
%
% When shardcopy finishes, the calling program calls shardcopyfini to 
% determine the final plotting information.
%
% [ps_scale,xlength,ylength,xscale,yscale]=shardcopyfini
%
% shardcopyfini terminates the hardcopy dialog and returns the
% user supplied scales for x and y as well as the final plot 
% size after scaling (xlength and ylength) and the scale factor
% needed for CHVSUB (ps_scale). The calling program will probably
% want to report these to the user somehow as it is up to her to
% actually submit the plotfile to CHVSUB and type the ps_scale
% value into CHVSUB
% *NOTE* If the user cancels, then ps_scale will return as -999.
%
% by G.F. Margrave, T.N. Bishop May 1994 
% additional hacks by Darren Foltinek, Oct, 1995
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
if( nargin < 6)
   tit='Specify Hardcopy Scales';
end
if( nargin < 5)       % Default the filename
   plotfile='temp.ps';
end
if( nargin < 4)
   a=[' ';' '];
else
   a=strmat(num2str(xscale),num2str(yscale));
end
if( nargin < 2)
    % Default the transfer function to nothing
    transfer = 'disp(''Postscript file made'');';
end
if( nargin < 1)
   hfig = gcf;        % Default figure is the current one
end
if(~isstr(hfig))
	action='hardcopy';
else
	action=hfig;
end
if(strcmp(action,'hardcopy') )
	figure(hfig);
	%make a temporary storage bin
	uicontrol('style','text','visible','off','string','__**$$',...
		'userdata',strmat(transfer,plotfile));
	% put up a dialog to ask for questions
    bad = 1;
    while bad
        a={num2str(xscale), num2str(yscale), plotfile};
        q={'xunits/inch?','yunits/inch?','plotfile name?'};
        a = askthingsle('questions',q,'answers',a);
        if isempty(a) %test for a cancel
  		    ps_scale=-999.;   %set to clearly a bogus value
  		    return;
  	    end
        xscale = sscanf(strunpad(a{1}),'%f');
        yscale = sscanf(strunpad(a{2}),'%f');
    	if(xscale<=0 | isempty(xscale) | yscale<=0 | isempty(yscale))
            uiwait(errordlg('Scales must be greater than zero'));
            return
        else
        	plotfile=strunpad(a{3});
        	if isempty(deblank(plotfile))
                uiwait(errordlg('Please set plotfile to something'));
                return
            else
            % save original settings for later
        	po=get(gcf,'paperorientation');
        	pt=get(gcf,'papertype');
        	pp=get(gcf,'paperposition');
        	pu=get(gcf,'paperunits');
        	fp=get(gcf,'position');
        	fu=get(gcf,'units');
        	pa=get(gca,'position');
        	xlimmode = get(gca,'xlimmode');
        	ylimmode = get(gca,'ylimmode');
        	set(gca,'xlimmode', 'manual');
        	set(gca,'ylimmode', 'manual');
        	% compute the aspect ratio desired
        	hax = get(gcf,'currentaxes');
        	aunits=get(hax,'units');
        	set(hax,'units','inches');
        	xlim=get(hax,'xlim');
        	ylim=get(hax,'ylim');
        	xlength = abs(xlim(2)-xlim(1))/xscale;
        	ylength = abs(ylim(2)-ylim(1))/yscale;
        	if xlength <= 250 && ylength <= 40
                bad = 0;   % EXIT THE LOOP!  RELEASE THE HOUNDS!
            else
                uiwait(errordlg(...
                    ['Scale results in an image that is too large ('...
                    num2str(xlength) ' x ' num2str(ylength) ' inches)'], ...
                    'Plot too large error'));
        	end
        end
    end
%
	% set figure properties
	% pos=get(hax,'position');
	% old algorithm is below
	%  twelve = 12;
 %	seven=12*ylength/xlength; 
	%  if(seven > 7)
	%    ratio=7/seven;
	%    twelve=twelve*ratio;
	%    seven=seven*ratio;
	%  end
	%  xmargin=14-twelve;
	%  xmargin=min([xmargin 2.5]);
	%  ymargin=8.5-seven;
	%  ymargin=min([ymargin 2.5]);
 %	set(gcf,'paperposition',[xmargin ymargin twelve seven]);
	%  ps_scale=xlength/(pos(3)*twelve);
	%
% new algorithm is to simply set the aspect ratio on the axes and not 
% set any figure properties. let them default.
% new stuff to get aspect ratio correct
	aspect=xlength/ylength;
	aspectsave = get(hax,'dataaspectratio');
%	% save original axeslengths
	[xlenold,ylenold]=axeslength(hax);
%	set(hax,'aspectratio',[aspect aspectsave(2)]);
%	% compute new axeslengths
%	[xlennew,ylennew]=axeslength(hax);
% set the figure and paper size to their desired values
        oldpaperx = pp(3);
	oldpapery = pp(4);
        paperx = max(xlength * 1.2, oldpaperx);
	papery = max(ylength * 1.2, oldpapery);
	fprintf(1,'Paper x=%f y=%f aspect=%f\n',paperx,papery, aspect);
	fprintf(1,'And figure x=%f y=%f\n',xlength, ylength);
	if(aspect>1)
	  fprintf(1,'Setting paper to landscape\n');
          set(gcf,'paperorientation','landscape','papertype','usletter');
	else
	  fprintf(1,'Setting paper to portrait\n');
	  set(gcf,'paperorientation','portrait','papertype','usletter');
	end
        set(gcf,'paperunits','inches');
        fprintf(1,'Setting paper width and height to %f, %f\n', paperx, papery);
	set(gcf,'paperposition',[.25 .25 paperx papery]);
	set(gcf,'units','inches');
	set(gcf,'position',[.25 .25 paperx papery]);
	
	% Set the axes size to their desired values (real units)
	set(gca,'units','inches');
	set(gca,'position',[ 0.4 0.4 xlength ylength]);
	%search for other axes. The x and y axes of other axes will be
	%scaled by the same factors as the primary axis
	
%	hkids=get(gcf,'children');
%	idax=zeros(size(hkids));
%	for k=1:length(hkids)
%		if(strcmp(get(hkids(k),'type'),'axes'))
%			idax(k)=1;
%		end
%	end
%	haxes=hkids(find(idax==1));
%	% loop over axes
%	xfact=xlennew/xlenold;
%	yfact=ylennew/ylenold;
%	for k=1:length(haxes)
%		au=get(haxes(k),'units');
%		set(haxes(k),'units','inches');
%		posax=get(haxes(k),'position');
%		set(haxes(k),'position',[posax(1:2) posax(3)*xfact ...
%			posax(4)*yfact]);
%		set(haxes(k),'units',au);
%	end
	%find the temp storage bin
	hkids=get(gcf,'children');
	for k=hkids'
		if( strcmp(get(k,'type'),'uicontrol') )
			if( strcmp(get(k,'style'),'text') )
				if(strcmp(get(k,'string'),'__**$$'))
					htmp=k;
					break;
				end
			end
		end
	end
    %make the plot
	dat=get(htmp,'userdata');
	%plotfile=strunpad(dat(2,:));
	eval(['print -dpsc ' plotfile])
	string = sprintf('Done plot. Output is in %s', plotfile );
	disp(string);
	
%	reset original settings 
%	set(hax,'aspectratio',[aspectsave],'visible','on');
	set(gcf,'paperorientation',po);
	set(gcf,'papertype',pt);
	set(gcf,'paperposition',pp);
	set(gcf,'paperunits',pu);
	set(gca,'units',aunits);
	set(gca,'position',pa);
	set(gcf,'units',fu);
	set(gcf,'position',fp);
	set(gca,'xlimmode', xlimmode);
	set(gca,'ylimmode', ylimmode);
%	%restore any secondary axes
%	for k=1:length(haxes)
%		au=get(haxes(k),'units');
%		set(haxes(k),'units','inches');
%		posax=get(haxes(k),'position');
%		set(haxes(k),'position',[posax(1:2) posax(3)/xfact ...
%			posax(4)/yfact]);
%		set(haxes(k),'units',au);
%	end
	ps_scale = 1.0;
	transfer=strunpad(dat(1,:));
	delete(htmp);
	set(hax,'userdata',[ps_scale,xlength,ylength,xscale,yscale]);
	eval(transfer);
%
	return
end
end