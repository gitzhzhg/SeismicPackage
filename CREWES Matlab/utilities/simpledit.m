function simpledit(arg)
% SIMPLEDIT: can be used for simple graphical editing of line graphs
% hh=simpledit(fig)
%
% SIMPLEDIT is intended primarily to demonstrate how the EDITLINES functions
% can be hooked to GUI components. For a description of the mouse button 
% assignments for EDITLINES, type help editlines
% An example of using it is:
% x1=1:2:100;
% y1=sin(x1/10);
% x2=2:2:100;
% y2=cos(x2/10);
% hh=plot(x1,y1,x2,y2);
% simpledit;
%
% After editing, retrive the edited results by:
% x1=get(hh(1),'xdata');
% y1=get(hh(1),'ydata');
% x2=get(hh(2),'xdata');
% y2=get(hh(2),'ydata');
%
% G.F. Margrave, Chevron Canada Resources, Jan 1994
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

% we assume an unadorned figure so that we donot have to worry about clobbering
% any exisitng user components.

% examine the input argument and act accordingly
if(nargin<1)
		arg=gcf;
end

if(isstr(arg))
		action=arg;
	else
		action='init';
		fig=arg;
end

% ok, now initialize
if( strcmp(action,'init') )

% make the figure current
		figure(fig);

% make an edit mode button

		sep=1;
		xnow=sep;
		ynow=sep;
		width=80;
		height=30;
		hmode=uicontrol('style','popupmenu','string','Modify|Link|Smooth',...
			'position',[xnow,ynow,width,height],'callback','simpledit(''mode'')');

		% now another popup for the motion options
		xnow=xnow+sep+width;
		width=100;
		hmotion=uicontrol('style','popupmenu','string','x&y motion|x only|y only',...
				'position',[xnow,ynow,width,height],'callback','simpledit(''motion'')');

		% now a drag popup
		xnow=xnow+sep+width;
		width=120;
		hdrag=uicontrol('style','popupmenu','string','elastic drag|constant drag',...
				'position',[xnow,ynow,width,height],'callback',...
				'simpledit(''dragmode'')');

		% now a locate checkbox

		xnow=xnow+sep+width;
		width=100;
		hlocate=uicontrol('style','checkbox','string','Locate Info','position',...
				[xnow,ynow,width,height],'callback','editlines(''locate'')');

		% now a fastopt checkbox

		xnow=xnow+sep+width;
		width=100;
		hfastopt=uicontrol('style','checkbox','string','Fast Option','position',...
				[xnow,ynow,width,height],'callback','simpledit(''fastopt'')');


		% and an undo button

		xnow=xnow+width+sep;
		width=50;
		hundo=uicontrol('style','pushbutton','string','Undo','position',...
				[xnow,ynow,width,height],'callback','editlines(''undo'')');

		% a zoom button

		xnow=1;
		ynow=ynow+height+sep;
		width=55;
		hzoom=uicontrol('style','pushbutton','string','Zoom','position',...
			[xnow,ynow,width,height],'callback','simpledit(''zoom'')');

		% an unzoom button

		xnow=1;
		ynow=ynow+height+sep;
		hunzoom=uicontrol('style','pushbutton','string','unZoom','position',...
			[xnow,ynow,width,height],'callback','simpledit(''unzoom'')');


		% now initiate editlines

		editlinesinit;

		return;

end

% switch the motion options

if( strcmp(action,'motion') )

		h=gco;

		val=get(h,'value');

		if( val==1 )
			editlines('xandy');
		elseif( val==2 )
			editlines('xandy');
			editlines('xonly');
		elseif( val==3 )
			editlines('xandy');
			editlines('yonly');
		end

		return;
	end

	%switch the edit mode
	if(strcmp(action,'mode'))
		hmode=get(gcf,'currentobject');

		val=get(hmode,'value');

		if(val==1)
			editlines('smoothoff');
			editlines('linkoff');
		elseif(val==2)
			editlines('linkon');
		elseif(val==3)
			editlines('smoothon');
		end

		return;
	end

	%switch the dragmode
	if(strcmp(action,'dragmode'))
		hmenu=get(gcf,'currentobject');

		val=get(hmenu,'value');

		if(val==1)
			editlines('elasticdrag');
		elseif(val==2)
			editlines('constantdrag');
		end

		return;

	end

	if( strcmp(action,'zoom') )
		hzoom=gco;
		dat=get(gca,'userdata');
		fcn1=get(gcf,'windowbuttondownfcn');
		fcn2=get(gcf,'windowbuttonmotionfcn');
		fcn3=get(gcf,'windowbuttonupfcn');

		[m,n]=size(dat);
		stuff=[ m n dat(:)' nan abs(fcn1) nan abs(fcn2) nan abs(fcn3)];
		set(hzoom,'userdata',stuff);
		set(gca,'userdata',[]);
		selboxinit('simpledit(''zoom2'')');
		return;
	end
	
	
	
if(	strcmp(action,'zoom2') )

		%find the zoom button
		kids=get(gcf,'children');
		for k=1:length(kids)
			if( strcmp('uicontrol',get(kids(k),'type')))
				if( strcmp('Zoom',get(kids(k),'string')))
					hzoom=kids(k);
				end
			end
		end

		stuff=get(hzoom,'userdata');
		ind=find(isnan(stuff));
		dat=stuff(3:ind(1)-1);
		%if( stuff(1)*stuff(2) )
			dat=reshape(dat',stuff(1),stuff(2));
		%end
		fcn1=setstr(stuff(ind(1)+1:ind(2)-1));
		fcn2=setstr(stuff(ind(2)+1:ind(3)-1));
		fcn3=setstr(stuff(ind(3)+1:length(stuff)));

		set(gcf,'windowbuttondownfcn',fcn1,'windowbuttonmotionfcn',fcn2,...
			'windowbuttonupfcn',fcn3);

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

		set(gca,'xlim',[xmin xmax],'ylim',[ymin ymax]);
		set(gca,'userdata',dat);

		return;

	end

	if(strcmp(action,'unzoom') )
		
		axis('auto');
		return;
	end
		
	if( strcmp(action,'fastopt') )
		hfastopt=gco;

		flag=get(hfastopt,'value');

		if( flag )
			editlines('faston');
		else
			editlines('fastoff');
		end

		return;
	end