function editlines(action)
% EDITLINES is a general digital editing tool for line data.
% mouse action definitions:
% 
% When in normal edit mode ('modify' mode) the mouse buttons act as follows:
% button 1 click - select line for editing or add a point.
%                    If the click occurs on a line which is not currently
%                    being edited, then it becomes the current editable
%                    line. If the click line is being edited, then a new
%                    point is added at the clicked location. If the click
%                    is not on any editable line, then all lines are
%                    unselected for editing.
% button 1 drag  - move a single point.
%                    The point on the current line nearest the clicked
%                    point is dragged as the cursor is dragged. Anchor
%                    points cannot be moved. 
% button 2 click - kill a single point.
%                    The point on the current line nearest the clicked
%                    point is deleted. Anchor points cannot be deleted.
% button 2 drag  - drag-kill many points
%                    Points are killed as the cursor is dragged near them
% button 3 click - set or free an anchor point.
%                    Anchor points may not be moved or killed. They are
%                    denoted as points with a double circle around them. 
%                    If the point clicked is a normal point, it becomes an
%                    anchor. If it is already an anchor, it is freed.
% button 3 drag  - move many points
%                     All points between the clicked point and anchors on 
%                     either side are dragged together. If in elastic drag
%                     mode, the magnitude of the displacement tapers to zero
%                     at the anchors and is a maximum only at the clicked
%                     point.  If the line has no anchors, then the entire
%                     line is dragged. If in constant drag mode, then all
%                     points between two anchors are dragged with the same
%                     displacement. When no anchors are present, the two
%                     drag modes are identical.
%
% When in link mode, the button actions are:
%    MB1 click ... select a line for link editing.
%    MB1 drag ... move a point. If it is released over another line, then
%                 a link is formed. A link is defined as an anchored point
%                 which exists on both lines
%    MB2 click ... break a line into segments
%    MB3 click or drag same as modify mode
%
% EDITLINES is intended to be incorporated within larger tools with specific
% editing tasks. Someone using it within such a larger tool need read no
% further. For further information on how to build EDITLINES into a tool
% of your own, type: help editlinesinit
%  
% by G.F. Margrave December 1993
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
% mouse button assignments
% button1 up ... current line becomes selected. If it is already selected, then a
%		point is added to the line at the clicked location.
% button1 motion ... point on current line closest to the pointer location is 
%		dragged
% button2 up ... point closest to the pointer is deleted.
% button2 motion ... points near the pointer are deleted
% button3 up ... Point closest to the pointer location becomes an anchor point 
%		unless it already is, in which case it is freed.
% button3 motion ... point closest to the pointer is dragged. Other points on the
%		line are dragged with the magnitude of the displacement vector tapering to
%		zero at the closest anchor points. If no anchor points, then the entire line
%		is dragged.
%
% the anchors vector [hcurve1 n1 x1(1) y1(1) ... x1(n) y1(n) hcurve2 n2 x2(1)
% 	y2(1) ... x2(n2) y2(n2) .... ]
% Is passed via the current axis userdata. At the end of editing, it may be 
% retrieved from there for future reference.
%
% EDITLINES actions:
% strcmp(action,'init') ... initialize
% strcmp(action,'fini') ... terminate editing
%strcmp(action,'tempfini') ... temporarily terminate when the user has clicked 
%           outside of all lines
% strcmp(action,'buttondown') ... a general button down handler
% strcmp(action,'button1up') ... button 1 up to create a single point
% strcmp(action,'button2up') ... delete a single point
% strcmp(action,'button3up') ... toggle anchor status
% strcmp(action,'button1motion') ... drag a single point
% strcmp(action,'button2motion') ... drag kill
%  strcmp(action,'button3motion') ... group moves
% strcmp(action,'stopmotion') ... turn off the motion function
% strcmp(action,'xonly') ... toggle x only
% strcmp(action,'yonly') ... toggle y only
% strcmp(action,'xandy') ... full free motion
% strcmp(action,'locate') ... toggle locate
% strcmp(action,'dragmode') ... toggle dragmode
% strcmp(action,'elasticdrag') ... set dragmode to elastic
% strcmp(action,'constantdrag') ... set dragmode to  constant
% strcmp(action,'undo') ... undo the last whatever
% strcmp(action,'smoothon') ... turn on smooth mode
% strcmp(action,'smoothoff') ... turn off smoothmode
% strcmp(action,'linkmode') ... toggle linkmode
% strcmp(action,'polymode') ... toggle polymode
%         (note linkmode-> pdata(3)==1 & polymode-> pdata(3)==-1)
% strcmp(action,'link') ... link 2 curves
% strcmp(action,'linkpoly') ... make or break a polygon
% strcmp(action,'autoseg') ... autmatically segment a curve
% strcmp(action,'linkon') ... turn link mode on
% strcmp(action,'linkoff') ... link mode off
% strcmp(action,'smoothon') ... smooth mode on
% strcmp(action,'smoothoff') ... smooth mode off
% strcmp(action,'smooth') ... smooth the current line between anchors
% strcmp(action,'smooth3') ... smooth the entire current line
% strcmp(action,'multnanon') ... all the presence of multiple nans in a row
% strcmp(action,'multnanoff') ... do not allow multiple nans. Any time 2
%		consecutive nans occur, toss one. Also get rid of any leading and
%		trailing nans.
% ***NOTE*** default behavior is to allow multiple nans
% strcmp(action,'deleteon') ... allow deletion of points
% strcmp(action,'deleteoff') ... do not allow deletion of points
%	***** default is allow ****
% strcmp(action,'addon') ... allow addition of new of points
% strcmp(action,'addoff') ... do not allow addition of new of points
%	***** default is allow ****
% strcmp(action,'clearanch') ... clear anchors on active line. Undo will unclear.
%		Has no effect if nothing is active.
% strcmp(action,'clearallanch') ... clear all anchors. Works even if nothing is
%		active. No true undo. If one line is active, then undo will restore
%		anchors for that line ONLY.
% 
valid_anchors=[]; line_anchors=[]; kclose=[]; 
if( strcmp(action,'init') )
	anchors=get(gca,'userdata');
	if(isempty(anchors)) % search the figure for the handles of all lines
		h=get(gca,'children');
		for k=1:length(h)
			if( strcmp(get(h(k),'type'),'line') )
				anchors = [anchors h(k) 0];
			end
		end
	end
	set(gca,'userdata',[]);
	% make an invisible storage bucket 
	hstore=uicontrol('style','text','visible','off','string','yrag');
	hparams=uicontrol('style','text','visible','off','string','yrag_params');
	hundo=uicontrol('style','text','visible','off','string','yrag_undo');
	hhor=uicontrol('style','text','visible','off','string','yrag_hors');
	% parameters
	% pdat(1) == xonly ... if 1 then y is not changed
	% pdat(2) == yonly ... if 1 then x is not changed
	% pdat(3) == linkmode ... if 1 thenwe are in link mode
	% pdat(4) == locate ... if 1 then we write the cursor location out
	% pdat(5) == dragmode ... if 0 then group drag is elastic, else it is constant
	% pdat(6) == smoothmode ... if 1 then we are in smooth mode
	% pdat(7) == fastopt ... if 1, then dragging a single point is done with a
	% 				less annoying display which, though less accurate, is much faster
	%				graphically
	% pdat(8) == multiple nans ... if 0 then we allow multiple nans, otherwise,
	%				we cull them
	% pdat(9) == nodelete ... if 0 then points can be delete if 1 they cannot
	%				defaults to 0
	% pdat(10) == noadd ... if 0 then points can be added, if 1 they cannot
	%				defaults to 0
	pdat=zeros(1,10);
	set(hparams,'userdata',pdat);
	
	% search for a negative anchor which indicates we should go ahead an select that
	% line for editing. Also in this loop, we create a vector of the handles alone
	% to facilitate quickly searching all lines in link mode
	% We also check to see that any handles found are children of the current
	% axes. If not, they are deleted.
	h=anchors(1);
	nh=1;
	hstart=0;
	hors=[];
	hkids=get(gca,'children')
	while( h ~= 0 )
		test=find(abs(h)==hkids);
		if(isempty(test))
			%its bogus, toss it
			npts=anchors(nh+1);
			anchors(nh:nh+1+2*npts)=[];
			if(nh>length(anchors) ) h=0;
			else h=anchors(nh);
			end
		else
			hors=[hors abs(h)];
			if( h<0 )
				if(hstart==0); % only the first negative handle is honored
					hstart=abs(h);
				end
				anchors(nh)=abs(h);
			end
			npts=anchors(nh+1);
			nh=nh+2*npts+2;
			if( nh >length(anchors) )h=0;
			else h=anchors(nh);
			end
		end
	end
	% make another storage bucket for the anchors	
	hanchors=uicontrol('style','text','visible','off','string','yrag_anchors',...
		'userdata',anchors);
		
	% put the horizon vector in its bucket
	set(hhor,'userdata',hors);
	%make sure pointer is an arrow
	set(gcf,'pointer','arrow');
		
	% start editing if requested
	if(hstart)
		set(gcf,'currentobject',hstart);
		editlines('buttondown');
	end
	return;
end
if(strcmp(action,'fini') )
	% clean up graphics if needed
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                                        hundo=h(k);
                                        found=found+1;
                elseif( strcmp(get(h(k),'string'),'yrag_hors') )
                						hhors=h(k);
                						found=found+1;
				end
				if( found== 5)
					break;
				end
			end
		end
	end
	% delete any locate information that may be on
		stuff=get(gca,'userdata');
		if( length(stuff)>6 )
			htext=stuff(2,6);
		else
			htext=0;
		end
		
		if(htext)
			delete(htext);
			htext=0;
			stuff(2,6)=0;
			set(gca,'userdata',stuff);
		end
	dat=get(hstor,'userdata');
	% get the anchor info 
	anchors=get(hanchors,'userdata');
	% reset and exit
	if( length(dat)>0 )
		delete(dat(7));
		if(dat(8)>0) delete(dat(8)); end
		if(dat(2)==1)
			ls='-';
		elseif(dat(2)==2)
			ls='--';
		elseif(dat(2)==3)
			ls=':';
		elseif(dat(2)==4)
			ls='-.';
		elseif(dat(2)==5)
			ls='o';
		elseif(dat(2)==6)
			ls='+';
		elseif(dat(2)==7)
			ls='.';
		elseif(dat(2)==8)
			ls='*';
		elseif(dat(2)==9)
			ls='x';
		end
		
		if (dat(2) < 5)
		set(dat(1),'linestyle',ls,'linewidth',dat(3),...
			'color',dat(4:6),'erasemode','normal');
		elseif (dat(2) >= 5)
		set(dat(1),'marker',ls,'linewidth',dat(3),...
			'color',dat(4:6),'erasemode','normal');
		end
		
		set(hstor,'userdata',[]);
		% save the anchor information
		ind=find( anchors==dat(1) );
		line_anchors=dat(9:length(dat));
		nanchors=length(line_anchors)/2;
		front=anchors(1:ind);
		back=anchors(ind+2+2*anchors(ind+1):length(anchors));
		anchors=[front nanchors line_anchors back];
		if( anchors==0 )anchors=[]; end
	end
	% put the anchor info in gca
	set(gca,'userdata',anchors);
	delete(hstor);
	delete(hanchors);
	delete(hparams);
	delete(hundo);
	delete(hhors);
	
	% make sure pointer is an arrow
	set(gcf,'pointer','arrow');
end
if(strcmp(action,'tempfini'))
% this causes a temporary termination of editing. To fully terminate,
	% editlines('fini') must be called
	% reset and exit
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                                        hundo=h(k);
                                        found=found+1;
				end
				if( found== 4)
					break;
				end
			end
		end
	end
	% delete any locate information that may be on
		stuff=get(gca,'userdata');
		if( length(stuff)>6 )
			htext=stuff(2,6);
		else
			htext=0;
		end
		
		if(htext)
			delete(htext);
			htext=0;
			stuff(2,6)=0;
			set(gca,'userdata',stuff);
		end
	dat=get(hstor,'userdata');
	% get the anchor info 
	anchors=get(hanchors,'userdata');
	if( length(dat)>0 )
		delete(dat(7));
		if(dat(8)) delete(dat(8)); end
		if(dat(2)==1)
			ls='-';
		elseif(dat(2)==2)
			ls='--';
		elseif(dat(2)==3)
			ls=':';
		elseif(dat(2)==4)
			ls='-.';
		elseif(dat(2)==5)
			ls='o';
		elseif(dat(2)==6)
			ls='+';
		elseif(dat(2)==7)
			ls='.';
		elseif(dat(2)==8)
			ls='*';
		elseif(dat(2)==9)
			ls='x';
		end
		
		if (dat(2) < 5)
		set(dat(1),'linestyle',ls,'linewidth',dat(3),...
			'color',dat(4:6),'erasemode','normal');
		elseif (dat(2) >= 5)
		set(dat(1),'marker',ls,'linewidth',dat(3),...
			'color',dat(4:6),'erasemode','normal');
		end
		
		% save the anchor information
		ind=find( anchors==dat(1) );
		line_anchors=dat(9:length(dat));
		nanchors=length(line_anchors)/2;
		front=anchors(1:ind);
		back=anchors(ind+2+2*anchors(ind+1):length(anchors));
		anchors=[front nanchors line_anchors back];
		if( anchors==0 )anchors=[]; end
		set(hanchors,'userdata',anchors);
		set(hstor,'userdata',[]);
	end
	%set(gca,'userdata',[]);
	set(hstor,'userdata',[]);
	set(hundo,'userdata',[]);
	return;
end
if( strcmp(action,'buttondown') )
	% Get the storage bucket
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
					 hundo=h(k);
					 found=found+1;
				end
				if( found== 4)
					break;
				end
			end
		end
	end
	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
	
	% get the parameters
	% pdata(1) ... xonly flag
	% pdata(2) ... yonly flag
	% pdata(3) ... linkmode flag
	% pdata(4) ... locate flag
	% pdata(5) ... dragmode flag
	% pdata(6) ... smoothmode
	% pdata(7) ... fastopt
	% pdata(8) ... multiple nans
	% pdata(9) ... nodelete
	% pdata(10) ... noadd
	pdata=get(hparams,'userdata');
	xonly=pdata(1);
	yonly=pdata(2);
	linkmode=pdata(3);
	locate=pdata(4);
	dragmode=pdata(5);
	smoothmode=pdata(6);
	fastopt=pdata(7);
	singnan=pdata(8);
	nodelete=pdata(9);
	noadd=pdata(10);
	% get the anchor vector
	anchors=get(hanchors,'userdata');
	% if provided, anchor information indicates which curves are editable and
	% what their anchors are. If provided, only curves whose handles are found in
	% this  vector will be edited regardless of whether or not thay have anchors.
	% anchor vector is defined as:
	% [hcurve1 num_anchors_curve1 (x,y)s_of_curve1_anchors hcurve2 ... ]
	%
	obj=get(gcf,'currentobject');
	hline=obj;
	if( ~strcmp('line',get(obj,'type')) ) % the user has clicked outside of all lines
		editlines('tempfini');
  		return;
	end
		
	% at this point we know the user has selected a line. If the line is 
	% aleady selected, then we add a point or delete a point. If not, then
	% we change the line style and thickness to indicat that it has been 
	% selected.
	%
	% delete any locate information that may be on
		stuff=get(gca,'userdata');
		if( length(stuff)>6 )
			htext=stuff(2,6);
		else
			htext=0;
		end
		
		if(htext)
			delete(htext);
			htext=0;
			stuff(2,6)=0;
			set(gca,'userdata',stuff);
		end
	
	newline=0;
	if(isempty(dat))
		newline=1;
	elseif( dat(1)~= hline & dat(7)~= hline & dat(8)~=hline )
		newline=1;
	end
	if( newline ) % we have selected a new line
	% see if this is an editable curve
		canedit=1;
		nanchors=0;
		if( length(anchors)>0 )
			canedit=0;
			k=1;
			while(k<length(anchors) )
				if( anchors(k) == hline )
					canedit=1;
					nanchors=anchors(k+1);
					len=nanchors*2;
					line_anchors=anchors(k+2:k+1+len);
					break;
				else
					n=anchors(k+1);
					k=k+2+2*n;
				end
			end
		end
		if( ~canedit )
			return;
		end
					
		if(~isempty(dat)) % reset the previous line
			delete(dat(7));
			if(dat(8)) delete(dat(8)); end
			if(dat(2)==1)
				ls='-';
			elseif(dat(2)==2)
				ls='--';
			elseif(dat(2)==3)
				ls=':';
			elseif(dat(2)==4)
				ls='-.';
			elseif(dat(2)==5)
				ls='o';
			elseif(dat(2)==6)
				ls='+';
			elseif(dat(2)==7)
				ls='.';
			elseif(dat(2)==8)
				ls='*';
			elseif(dat(2)==9)
				ls='x';
			end
			
			if (dat(2) < 5)
			set(dat(1),'linestyle',ls,'linewidth',dat(3),...
				'color',dat(4:6),'erasemode','normal');
			elseif (dat(2) >= 5)
			set(dat(1),'marker',ls,'linewidth',dat(3),...
				'color',dat(4:6),'erasemode','normal');
			end
				
			% store the anchor info
			ind=find( anchors==dat(1) );
			la=dat(9:length(dat));%get the previous anchors
			na=length(la)/2;% number of prev anchors
			front=anchors(1:ind);
			back=anchors(ind+2+2*anchors(ind+1):length(anchors));
			anchors=[front na la back];
			
			set(hanchors,'userdata',anchors);
			
		end
		dat=zeros(1,8);
		dat(1)=hline;
		ls=get(hline,'linestyle');
		if( strcmp(ls,'-'))
			ls=1;
		elseif(strcmp(ls,'--'))
			ls=2;
		elseif(strcmp(ls,':'))
			ls=3;
		elseif(strcmp(ls,'-.'))
			ls=4;
		elseif(strcmp(ls,'o'))
			ls=5;
		elseif(strcmp(ls,'+'))
			ls=6;
		elseif(strcmp(ls,'.'))
			ls=7;
		elseif(strcmp(ls,'*'))
			ls=8;
		elseif(strcmp(ls,'x'))
			ls=9;
		end
		dat(2) =ls;
		dat(3) =get(hline,'linewidth');
		dat(4:6) =get(hline,'color');
		
	% set the properties on the line
		set(hline,'erasemode','xor','color',[.5 .5 .5],'linewidth',.2,...
			'linestyle','-');
			
		x=get(hline,'xdata');
		y=get(hline,'ydata');
		npts=length(x);
		
	% see if it is a polygon (closed curve)
		ispoly=0;
		if( x(1)==x(npts) & y(1)==y(npts) & npts>1)
			ispoly=1;
		end
			
	% draw the points
		if(ispoly)
			dat(7)=line(x(2:npts),y(2:npts),'erasemode','xor','color',dat(4:6),...
				'marker','o');
		else
			dat(7)=line(x,y,'erasemode','xor','color',dat(4:6),...
				'marker','o');
		end
	% test the anchors for validity
		for k=1:nanchors
			ind=find( x==line_anchors(2*k-1) );
			if( length(ind)>0 )
				i2=find( y==line_anchors(2*k));
				if(~isempty(i2))
					valid_anchors=[valid_anchors ...
						line_anchors(2*k-1:2*k)];
				end
			end
		end
		line_anchors=valid_anchors;
		nanchors=length(line_anchors)/2;
			
	% draw the anchors
		dat(8)=0;
		if( nanchors )
			dat(8)=line(line_anchors(1:2:2*nanchors),line_anchors...
			(2:2:2*nanchors),'color',[1 0 0],...
			'marker','o','markersize',12,'linestyle','none','erasemode','xor');
		end
	% store the anchors
		dat=[dat line_anchors];
				
		set(hstor,'userdata',dat);
	% if here then we are not selecting a new line (or we are in link mode)
	else % its an already defined line so we turn on the buttonup and motion
			% functions
			
		% get the selection type
	
		flag=get(gcf,'selectiontype');
		  %
		  % button 1 assignments
		  %
		  % Note: linkmode has all buttons off except number 1. This allows 
		  % movement and link only. Note that the button1up function assigned
		  % here only makes or breaks polygons. Linking of 1 curve to another
		  % is only activated by the button1motion function which re-defines the
		  % button1up function to editlines('link')
		  %
		  if( strcmp(flag,'normal') ) 
		  	if(~linkmode & ~smoothmode)
				set(gcf,'windowbuttonupfcn','editlines(''button1up'')');
			elseif( linkmode == -1) % normal linking does not use the poly option
				set(gcf,'windowbuttonupfcn','editlines(''linkpoly'')');
			elseif( smoothmode )
				set(gcf,'windowbuttonupfcn','editlines(''smooth'')');
			end
			if( ~smoothmode )
				set(gcf,'windowbuttonmotionfcn','editlines(''button1motion'')');
			end
		  %
		  % button 2 assignments
		  %	
		  elseif( strcmp(flag,'extend') ) %button
		  if(~linkmode & ~smoothmode & ~nodelete)
			set(gcf,'windowbuttonupfcn','editlines(''button2up'')');
			set(gcf,'windowbuttonmotionfcn',...
				'editlines(''button2motion'')');
		  elseif( linkmode )
		  	set(gcf,'windowbuttonupfcn','editlines(''autoseg'')');
		  	set(gcf,'windowbuttonmotionfcn','');
		  end
		  %
		  % button 3 assignments
		  %	
		  elseif( strcmp(flag,'alt') ) %button 3
		  if( ~smoothmode)
			set(gcf,'windowbuttonupfcn','editlines(''button3up'')');
			set(gcf,'windowbuttonmotionfcn',...
				'editlines(''button3motion'')');
		  %elseif( linkmode )
		  %	set(gcf,'windowbuttonupfcn','editlines(''autoseg'')');
		  %	set(gcf,'windowbuttonmotionfcn','');
		  elseif( smoothmode )
			set(gcf,'windowbuttonupfcn','editlines(''smooth3'')');
		  	set(gcf,'windowbuttonmotionfcn','');
		  end
		   end
			
		% now get the current point and store it in user data
		pt=get(gca,'currentpoint');
		pt=pt(1,1:2);
			
		% get the lines data and determine the index of the closest point
			
		% get the lines data	
		x=get(dat(1),'xdata');
		y=get(dat(1),'ydata');
		npts=length(x);
			
		live=find(~isnan(x));
		d=abs(x(live)-pt(1)) + abs(y(live)-pt(2));
		it=find( d==min(d) );
		it=live(it);
% flags and parameters:
% xonly ... if 1 then only x coordinate is changed
% yonly ... if 1 then ony y coordinate is changed
% killrd ... max distance to kill with a drag kill
	% set the kill radius at .2% of the xaxis length
	%
		xlim=get(gca,'xlim');
		killrd=.002*abs(xlim(1)-xlim(2));
		
	% see if it is a polygon (closed curve)
		ispoly=0;
		if( x(1)==x(npts) & y(1)==y(npts) )
			ispoly=1;
		end
			
		stuff=[length(it) it hstor pt dat(1) dat(7:length(dat))]; % first row
		stuff=[stuff;zeros(2,length(stuff))];
		stuff(2,1)=xonly;
		stuff(2,2)=yonly;
		stuff(2,3)=killrd;
		stuff(2,4)=ispoly;
		stuff(2,5)=locate;
		stuff(2,6)=htext; % reserved for text handle of locate display
		stuff(2,7)=dragmode;
		stuff(2,8)=linkmode;
		stuff(3,1)=fastopt;
		stuff(3,2)=singnan;
		stuff(3,3)=noadd;
		set(gca,'userdata',stuff);
	end
	% store the undo information
	% [npts x(1:npts) y(1:npts) dat]
	if(~linkmode)
		set(hundo,'userdata',[npts x y dat]);
	else
		if(newline)
			x2=get(hline,'xdata');
			y2=get(hline,'ydata');
			n2=length(x2);
			ustuff=[npts x y dat inf hline n2 x2 y2];
			set(hundo,'userdata',ustuff);
		else
			set(hundo,'userdata',[npts x y dat]);
		end
	end
			
	
return;
end
if( strcmp(action,'button1up') | strcmp(action,'button2up') ...
		| strcmp(action,'button3up')  )
	
	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
				
	% we are adding (or deleting) a point to the current line
	% or toggling the anchor status of a point
	% find the clicked point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hstor=stuff(1,lit+2);
	xpt=stuff(1,lit+3);
	ypt=stuff(1,lit+4);
	hline=stuff(1,lit+5);
	hline2=stuff(1,lit+6);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
	else
		nanchors=0;
		line_anchors=[];
	end
	xonly=stuff(2,1);
	yonly=stuff(2,2);
	killrd=stuff(2,3);
	ispoly=stuff(2,4);
	noadd=stuff(3,3);
	% get the lines data	
	x=get(hline,'xdata');
	y=get(hline,'ydata');
	npts=length(x);
	if( strcmp(action,'button1up') ) % add a point
		if(noadd)
			return;
		end
	% check to see if the point already exists
		dtest=abs(x(it)-xpt)+abs(y(it)-ypt);
		if( dtest<.5*killrd )
			return;
		end
	% find all those points which surround xpt
		ind=surround(x,xpt);
	% now for each pair of surrounding points compute the perpendicular
	% distance from xpt,ypt to the line segment connecting the pair
		m=(y(ind+1)-y(ind))./(x(ind+1)-x(ind));
		b=y(ind)-m.*x(ind);
		d=abs(m*xpt -ypt +b)./sqrt(m.*m+1);
	% find the minimum distance
		live = find(~isnan(d));
		if(isempty(live))
			editlines('tempfini');
			return;
		end
		
		it=find(d(live)==min(d(live)));
		
	% the next lines are there to trap an apparent MATLAB bug. Occaisionally, it
	% seems possible to click outside all lines and still have the current object
	% be a line. This leads to the current circumstance where we are inserting a
	% point which is way off the line. The intended behavior is a temporary fini
	% so we do that
		if(min(d)>2*killrd)
			editlines('tempfini');
			return;
		end
		% insert the point
		x=[x(1:ind(it)) xpt x(ind(it)+1:npts)];
		y=[y(1:ind(it)) ypt y(ind(it)+1:npts)];
		npts=length(x);
	elseif( strcmp(action,'button2up') )  % delete a point
		
		% make sure its not an anchor. To delete an anchor you must first free it 
		if( nanchors )
			ind=find( x(it)==line_anchors(1:2:2*nanchors) );
			if( length(ind)> 0)
				ohoh = find(y(it)==line_anchors(2*ind));
				if( length(ohoh) > 0)
					return;
				end
			end
		end
		
		%check for accidental deletion of a segment separator
		if( isnan(x(it)) | isnan(y(it)) )
			return;
		end
		
		% check for deletion of polygon endpoint
		done=0;
		if( it(1)==1 | it(1)==npts ) % it might be an end point
			if( ispoly ) % see if it is a polygon
				x=[x(2:npts-1) x(2)];
				y=[y(2:npts-1) y(2)];
				npts=length(x);
				done=1;
			end
		end
		
		if( ~done)	
			if( length(it)>1 )
				error('click closer to the point to delete it');
			end
			
			x=[x(1:it-1) x(it+1:npts)];
			y=[y(1:it-1) y(it+1:npts)];
			npts=length(x);
		end
		
	elseif( strcmp(action,'button3up') ) % toggle the points anchor status
		% see if its a polygon and we have selected an endpoint
		if(ispoly)
			if(it(1)==1 | it(1)==npts)
				it=1; % make sure only one of the endpoints is an anchor
			end
		end
		
		%check for accidental selection of a segment separator
		if( isnan(x(it)) | isnan(y(it)) )
			return;
		end
		
		if( length(it)>1 )
			it=it(1);
		end
		
		% see if it already is an anchor
		found=0; 
		if( nanchors )
			ind=find( x(it)==line_anchors(1:2:2*nanchors) );
			if( length(ind)> 0)
				ooohh = find(y(it)==line_anchors(2*ind));
				if( length(ooohh) > 0)
					% ok remove it from anchor status.
					% 2*ind(ooohh) points to the
					%  ycoord of the selected anchor
					nanchors=nanchors-1;
			 	     line_anchors=[line_anchors(1:2*ind(ooohh)-2)...
				            line_anchors(2*ind(ooohh)+1:length(line_anchors))];
					  % remove any line anchors within .5*killrd of this one
					  kmax=nanchors;
					  nanchors=0;
					  la=[];
					  for k=1:kmax
							xa=line_anchors(2*k-1);
							ya=line_anchors(2*k);
							d=sqrt( (xa-x(it))^2 + (ya-y(it))^2 );
							if( d> killrd )
								nanchors=nanchors+1;
								la=[la xa ya];
							end
						end
						line_anchors=la;
					found=1;
				end
			end
		end
		if( ~found )
			% make the point an anchor
			nanchors=nanchors+1;
			line_anchors=[line_anchors x(it) y(it)];
		end
		
		% store the anchor information
		% the user data of hstor is:
		% dat(1) = handle of the selected line
		% dat(2) = original linestyle
		% dat(3) = original linewidth
		% dat(4:6) = original color
		% dat(7) = handle of duplicate line
		% dat(8) = handle of the anchor line
		% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
		dat=get(hstor,'userdata');
		dat=[dat(1:8) line_anchors];
		set(hstor,'userdata',dat);
		
	end
				
	% check for occurance of multiple nans in a row or at the beginning &/or end of
	% the line
 singnan=stuff(3,2);
 if(singnan)
		[x,ikeep]=nanclear(x);
		if( length(x)<npts )
			npts=length(x);
			y=y(ikeep);
		end
	end
	set(hline,'xdata',x,'ydata',y);
	if( ispoly )
		set(hline2,'xdata',x(2:npts),'ydata',y(2:npts));
	else
		set(hline2,'xdata',x,'ydata',y);
	end
	
	%fart with the anchors display
	if( nanchors )
		if( hline3 ) set(hline3,'xdata',line_anchors(1:2:2*nanchors),'ydata',...
			line_anchors(2:2:2*nanchors));
		else
			hline3 = line(line_anchors(1:2:2*nanchors),line_anchors...
				(2:2:2*nanchors),'color',[1 0 0],'marker','o','markersize',12,...
			'linestyle','none','erasemode','xor');
			dat(8)=hline3;
			set(hstor,'userdata',dat);
		end
	elseif( ~nanchors & hline3 )
		delete(hline3);
		hline3=0;
		dat(8)=hline3;
		set(hstor,'userdata',dat);
	end
				
return;
end
if( strcmp(action,'button1motion') | strcmp(action,'button2motion') |...
	strcmp(action,'button3motion') )
	 % we are dragging a point of the current line
	 % find the clicked point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hstor=stuff(1,lit+2);
	%xpt=stuff(1,lit+3);
	%ypt=stuff(1,lit+4);
	hline=stuff(1,lit+5);
	hline2=stuff(1,lit+6);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
	else
		nanchors=0;
		line_anchors=[];
	end
	xonly=stuff(2,1);
	yonly=stuff(2,2);
	killrd=stuff(2,3);
	ispoly=stuff(2,4);
	locate=stuff(2,5);
	htext=stuff(2,6);
	dragmode=stuff(2,7);
	linkmode=stuff(2,8);
	fastopt=stuff(3,1);
	
	if(linkmode)
		set(gcf,'windowbuttonupfcn','editlines(''link'')');
	else
		set(gcf,'windowbuttonupfcn','editlines(''stopmotion'')');
	end
	
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);		
	% get the lines data	
	x=get(hline,'xdata');
	y=get(hline,'ydata');
	npts=length(x);
		
	if( strcmp(action,'button1motion') ) % drag a single point
	% see if we are trying to drag an anchor
		if( nanchors )
			ind=find( x(it(1))==line_anchors(1:2:2*nanchors) );
			if( length(ind)> 0)
				ooohh = find(y(it(1))==line_anchors(2*ind));
				if( length(ooohh) > 0)
					% simply return
					return;
				end
			end
		end
		
		%check for accidental drag of a segment separator
		if( isnan(x(it)) | isnan(y(it)) )
			return;
		end
		% correct for xonly and yonly
		xpt = xpt*(1-yonly)+x(it(1))*yonly;
		ypt = ypt*(1-xonly)+y(it(1))*xonly;
 
		if(htext)
			delete(htext);
			htext=0;
			stuff(2,6)=0;
			set(gca,'userdata',stuff);
		end
		if(locate)
			htext=text(xpt,ypt,['(' num2str(xpt) ',' num2str(ypt) ')'],'erasemode',...
			'xor','color',[.5 .5 .5]);
			stuff(2,6)=htext;
			set(gca,'userdata',stuff);
		end
		% see if its a polygon
		done=0;
		if(ispoly)
			if(it(1)==1 | it(1)==npts)
				xpt = xpt(1)*(1-yonly)+x(it(1))*yonly;
				ypt = ypt(1)*(1-xonly)+y(it(1))*xonly;
				x=[xpt x(2:npts-1) xpt];
				y=[ypt y(2:npts-1) ypt];
				done=1;
			end
		end
		if(~done)
			if(lit>1)
				if( x(it(1))==x(it(2)) & y(it(1))==y(it(2)) )
					it=it(1);
					stuff(1,3:2+lit-1)=stuff(1,2)*ones(size(stuff(1,3:2+lit-1)));
					set(gca,'userdata',stuff);
				else
					error(' you may only move nodes not segments ');
				end
			end
			xpt = xpt*(1-yonly)+x(it)*yonly;
			ypt = ypt*(1-xonly)+y(it)*xonly;
			x=[x(1:it-1) xpt x(it+1:npts)];
			y=[y(1:it-1) ypt y(it+1:npts)];
		end
		if(fastopt & ~linkmode)
			xmoved=xpt;
			ymoved=ypt;
		end
		
	elseif( strcmp(action,'button2motion') ) %kill points the pointer drags by
		% find the points in a local box
		ind=between(xpt-10*killrd,xpt+10*killrd,x,2);
		if(isempty(ind)|ind==0) return; end
		ind2=between(ypt-10*killrd,ypt+10*killrd,y(ind),2);
		if(isempty(ind2)|ind2==0) return; end
		
		% find the point closest to the current point
		d=abs(x(ind(ind2))-xpt) + abs(y(ind(ind2))-ypt);
		dkill=min(d);
		
		ik=find( d==min(d) );
		itkill=ind(ind2(ik));
		% see if we are trying to drag-kill an anchor
		if( nanchors )
			ind=find( x(itkill)==line_anchors(1:2:2*nanchors) );
			if( length(ind)> 0)
				ooohh = find(y(itkill)==line_anchors(2*ind));
				if( length(ooohh) > 0)
					% simply return
					return;
				end
			end
		end
		
		%check for accidental deletion of a segment separator
		if( isnan(x(itkill)) | isnan(y(itkill)) )
			return;
		end
		
		% see if it is a polygon endpoint
		done=0;
		if(ispoly)
			if( itkill ==1 | itkill==npts )
				x=[x(2:npts-1) x(2)];
				y=[y(2:npts-1) y(2)];
				npts=length(x);
				done=1;
			end
		end
		
		if(~done)
			x=[x(1:itkill-1) x(itkill+1:npts)];
			y=[y(1:itkill-1) y(itkill+1:npts)];
			npts=length(x);
		end
	elseif( strcmp(action,'button3motion') ) % move all points between anchors
	% see if we are trying to drag an anchor
		if( nanchors )
			ind=find( x(it(1))==line_anchors(1:2:2*nanchors) );
			if( length(ind)> 0)
				ooohh = find(y(it(1))==line_anchors(2*ind));
				if( length(ooohh) > 0)
					% simply return
					return;
				end
			end
		end
		
	%check for accidental drag of a segment separator
		if( isnan(x(it)) | isnan(y(it)) )
			return;
		end
	% display locate information if required
		if(htext)
			delete(htext);
			htext=0;
			stuff(2,6)=0;
			set(gca,'userdata',stuff);
		end
		if(locate)
			htext=text(xpt,ypt,['(' num2str(xpt) ',' num2str(ypt) ')'],'erasemode',...
			'xor','color',[.5 .5 .5]);
			stuff(2,6)=htext;
			set(gca,'userdata',stuff);
		end
		% see if its a polygon
		
		% compute the displacement
			xdisp = (xpt-x(it))*(1-yonly);
			ydisp = (ypt-y(it))*(1-xonly);
		
		if( ~nanchors ) % see if the curve is free floating
			
			x=x+xdisp(1);
			y=y+ydisp(1);
			if(fastopt)
				xmoved=x;
				ymoved=y;
			end
		
		elseif(ispoly) % treat polygons as a special case
		
			if( length(it)>1 )
				if(it(1)==1 & it(2) ==npts)|(it(1)==npts & it(2)==1 )
					it=1;
					xdisp = xdisp(1);
					ydisp = ydisp(1);
				end
			end
		
		% determine the indicies of the anchors
			ia=[];
			for k=1:nanchors
				ind=find( x == line_anchors(2*k-1) );
				ii = find( y(ind) == line_anchors(2*k) );
				ia=[ia ind(ii)];
			end
			% don't count endpoints twice
			iend=find(ia==npts);
			if( iend )
				ia=[ia(1:iend-1) ia(iend+1:length(ia))];
			end
			
		% find the anchors on either side of it
			% first go ccw
			a1=0;
			itest=it;
			wrap1=0;
			while(~a1)
				%decrement
				itest=itest-1;
				if(itest==0) %check for wrap around
					itest=npts;
					wrap1=1;
				end
				% see if itest is an anchor
				ind=find(itest==ia);
				if(ind)
					a1=itest;
				end
			end
			
			% now go clockwise
			a2=0;
			itest=it;
			wrap2=0;
			while(~a2)
				%increment
				itest=itest+1;
				if(itest==npts+1)
					itest=1;
					wrap2=1;
				end
				%see if test is an anchor
				ind=find(itest==ia);
				if(ind)
					a2=itest;
				end
			end
			
		% now the cases.
		% case1 occurs when the drag point ('it')
		% is on the opposite side of the polygon from the endpoints
		% Case 2 is when they are on the same side. Case 2 is the only possibility
		% when there is a single anchor
		%if( a1~=a2 )
			case1=0;
			if(between(a1,a2,it)) case1=1;
			%elseif( a1==1 & between(npts,a2,it) ) case1=0;
			%elseif( a2==1 & between(a1,npts,it) ) case1=0;
			end
			
			done=0;
			if( case1 ) % see if 'it' is opposite the endpoints
				if(dragmode) % constant drag
					xdvec=zeros(1,npts);
					xdvec(a1+1:a2-1)=xdisp*ones(size(a1+1:a2-1));
					ydvec=zeros(1,npts);
					ydvec(a1+1:a2-1)=ydisp*ones(size(a1+1:a2-1));
				else % elastic drag
					 % compute the x displacement
					xdvec = zeros(1,npts); % start with zero
					xinc1=xdisp/(it-a1);
					xinc2=xdisp/(a2-it);
					xdvec(a1+1:a2-1) = [(1:it-a1)*xinc1 xdisp-(1:a2-it-1)*xinc2];
				
					% compute the y displacement
					ydvec = zeros(1,npts); % start with zero
						yinc1=ydisp/(it-a1);
					yinc2=ydisp/(a2-it);
					ydvec(a1+1:a2-1) = [(1:it-a1)*yinc1 ydisp-(1:a2-it-1)*yinc2];
				end
				
				done=1;
			else % case 2
				% case two is complicated because 'it' is on the same side of
				% the polygon as the endpoints
				xdvec=zeros(1,npts); % start with zero
				ydvec=zeros(1,npts); % start with zero
					
				% determine ccw distance to a1
				if(wrap1) 
					d1=npts+it-1-a1;
				else
					d1=(it-a1);
				end
				
				%cw distance to anchor a2
				if(wrap2)
					d2=npts+a2-1-it;
				else
					d2=a2-it;
				end
				
				xinc1=xdisp/d1;
				xinc2=xdisp/d2;
				yinc1=ydisp/d1;
				yinc2=ydisp/d2;
				
				% determine ccw displacment vector
				if(wrap1)
					if(dragmode) %do constant drag
						xdvec(1:it)=xdisp*ones(size(1:it));
						xdvec(a1+1:npts)=xdisp*ones(size(a1+1:npts));
						ydvec(1:it)=ydisp*ones(size(1:it));
						ydvec(a1+1:npts)=ydisp*ones(size(a1+1:npts));
					else
						xdvec(1:it) =[xdisp+((-it+1):0)*xinc1];
						xdvec(npts-1:-1:a1)=[((npts-a1-1):-1:0)*xinc1];
						xdvec(npts)=xdvec(1);
						ydvec(1:it) =[ydisp+((-it+1):0)*yinc1];
						ydvec(npts-1:-1:a1)=[((npts-a1-1):-1:0)*yinc1];
						ydvec(npts)=ydvec(1);
					end
				else
					if(dragmode)
						xdvec(a1+1:it)=xdisp*ones(size(a1+1:it));
						ydvec(a1+1:it)=ydisp*ones(size(a1+1:it));
					else
						xdvec(a1:it) = [(0:(it-a1))*xinc1];
						ydvec(a1:it) = [(0:(it-a1))*yinc1];
					end
				end
				
				% determine cw displacement vector
				if(wrap2)
					if (dragmode)% do constant drag
						if(a2==1)
							xdvec(it:npts-1)=xdisp*...
								ones(size(it:npts-1));
							ydvec(it:npts-1)=ydisp*...
								ones(size(it:npts-1));
						else
						   xdvec(it:npts)=xdisp*ones(size(it:npts));
						   xdvec(1:a2-1)=xdisp*ones(size(1:a2-1));
						   ydvec(it:npts)=ydisp*ones(size(it:npts));
						   ydvec(1:a2-1)=ydisp*ones(size(1:a2-1));
						end
					else	
						xdvec(it:npts) = [xdisp-(0:(npts-it))*xinc2];
						xdvec(2:a2) = [((a2-2):-1:0)*xinc2];
						xdvec(1)=xdvec(npts);
						ydvec(it:npts) = [ydisp-(0:(npts-it))*yinc2];
						ydvec(2:a2) = [((a2-2):-1:0)*yinc2];
						ydvec(1)=ydvec(npts);
					end
				else
					if(dragmode)
						xdvec(it:a2-1)=xdisp*ones(size(it:a2-1));
						ydvec(it:a2-1)=ydisp*ones(size(it:a2-1));
					else
						xdvec(it:a2) = [xdisp-(0:(a2-it))*xinc2];
						ydvec(it:a2) = [ydisp-(0:(a2-it))*yinc2];
					end
				end
				done=1;
			end
		
		
		x=x+xdvec;
		y=y+ydvec;	
		if(fastopt)
			ind=find( (xdvec+ydvec)~= 0 );
			xmoved=x(ind);
			ymoved=y(ind);
		end
				
		else % group drag for non-polygons
		
			%determine the indicies of the anchors
			ia=[];
			for k=1:nanchors
				ind=find( x == line_anchors(2*k-1) );
				ii = find( y(ind) == line_anchors(2*k) );
				ia=[ia ind(ii)];
			end
			
			%determine if we are between anchors or off the end
			
			offend=0; offbeg=0;
			[iasort,ias]=sort(ia); % sort the anchors based on index
			if( it< iasort(1) ) offbeg=1; 
			elseif( it> iasort(nanchors) ) offend=1;
			end
			
			% offbeg... we are before the first anchor
			if( offbeg )
				numshifted = ia(ias(1))-1;
				
				% compute the x displacement
				xdvec = xdisp*ones(1,numshifted);% constant shift vector
				ydvec = ydisp*ones(1,numshifted);% constant shift vector
				if(~dragmode) %modify for elastic drag
					xincdisp=xdisp/(ia(ias(1))-it);
					ind=it+1:ia(ias(1))-1;% find the points to receive ramped displacement
					xdvec(ind) = xdvec(ind) - (1:length(ind))*xincdisp;
					%compute the y displacement
					yincdisp=ydisp/(ia(ias(1))-it);
					ydvec(ind) = ydvec(ind) - (1:length(ind))*yincdisp;
				end
				
				x= [x(1:numshifted)+xdvec x(ia(ias(1)):npts)];
				y= [y(1:numshifted)+ydvec y(ia(ias(1)):npts)];
				if(fastopt)
					xold=get(hline,'xdata');
					yold=get(hline,'ydata');
					xdvec=x-xold;
					ydvec=y-yold;
					ind=find( (xdvec+ydvec)~= 0 );
					xmoved=x(ind);
					ymoved=y(ind);
				end
			elseif( offend ) % we are after the last anchor
				numshifted = npts-ia(ias(nanchors));
				
				
				xdvec = zeros(1,npts); % start with zero
				ydvec = zeros(1,npts); % start with zero
				if( dragmode)
					xdvec(ia(ias(nanchors))+1:npts)=xdisp*...
						ones(size(ia(ias(nanchors))+1:npts));
					ydvec(ia(ias(nanchors))+1:npts)=ydisp*...
						ones(size(ia(ias(nanchors))+1:npts));
				else
					% compute the x displacement
					xincdisp=xdisp/(it-ia(ias(nanchors)));
					xdvec(ia(ias(nanchors))+1:it)=(1:it-ia(ias(nanchors)))*xincdisp;
					xdvec(it+1:npts)=xdisp*ones(1,length(it+1:npts));
					%compute the y displacement
					yincdisp=ydisp/(it-ia(ias(nanchors)));
					ydvec(ia(ias(nanchors))+1:it)=(1:it-ia(ias(nanchors)))*yincdisp;
					ydvec(it+1:npts)=ydisp*ones(1,length(it+1:npts));
				end
				
				x=x+xdvec;
				y=y+ydvec;
				if(fastopt)
					ind=find( (xdvec+ydvec)~= 0 );
					xmoved=x(ind);
					ymoved=y(ind);
				end
				
			else % we are between anchors
				% determine which 2 anchors
				ind=surround(iasort,it);
				ia1=iasort(ind);
				ia2=iasort(ind+1);
				
				
				xdvec = zeros(1,npts); % start with zero
				ydvec = zeros(1,npts); % start with zero
				if( dragmode )%constant displacement
					xdvec(ia1+1:ia2-1)=xdisp*ones(size(ia1+1:ia2-1));
					ydvec(ia1+1:ia2-1)=ydisp*ones(size(ia1+1:ia2-1));
				else
				% compute the x displacement
					xinc1=xdisp/(it-ia1);
					xinc2=xdisp/(ia2-it);
					xdvec(ia1+1:ia2-1) = [(1:it-ia1)*xinc1 xdisp-(1:ia2-it-1)*xinc2];
				% compute the y displacement
					yinc1=ydisp/(it-ia1);
					yinc2=ydisp/(ia2-it);
					ydvec(ia1+1:ia2-1) = [(1:it-ia1)*yinc1 ydisp-(1:ia2-it-1)*yinc2];
				end
				
				x=x+xdvec;
				y=y+ydvec;
				if(fastopt)
					ind=find( (xdvec+ydvec)~= 0 );
					xmoved=x(ind);
					ymoved=y(ind);
				end
				
			end
		end
	end
				
	% check for occurance of multiple nans in a row or at the beginning &/or end of
	% the line
	singnan=stuff(3,2);
	if(singnan)
		[x2,ikeep]=nanclear(x);
	else
		x2=x;
		ikeep=1:length(x);
	end
	if( length(x2)<npts )
		if(~strcmp(action,'button2motion'))
			xit=x(it);
			yit=y(it);
			npts=length(x2);
			x=x2;
			y=y(ikeep);
			% if we changed the number of points then change 'it' as well
			lit=stuff(1,1);
			for k=1:lit
				ind=find( x==xit(k) );
				i2=find( y(ind)==yit(k) );
				it(k)=ind(i2);
			end
			stuff(1,2:lit+1)=it;
			set(gca,'userdata',stuff);
		else
			npts=length(x2);
			x=x2;
			y=y(ikeep);
		end
	end
if( fastopt & ~linkmode & ~strcmp(action,'button2motion') )			
		set(hline2,'xdata',xmoved,'ydata',ymoved);
		set(hline2,'userdata',[x;y]);
	else
		set(hline,'xdata',x,'ydata',y);
		if( ispoly )
			set(hline2,'xdata',x(2:npts),'ydata',y(2:npts));
		else
			set(hline2,'xdata',x,'ydata',y);
		end
	end
			
  return;
end
if(strcmp(action,'stopmotion') )
 % see if a significant motion occured. If it did not, call the appropriate
 % button up function. This is done to decrease the sensitivity of the 
 % program to accidental small mouse motions
	% find the clicked point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hstor=stuff(1,lit+2);
	xit=stuff(1,lit+3);
	yit=stuff(1,lit+4);
	hline=stuff(1,lit+5);
	hline2=stuff(1,lit+6);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
	else
		nanchors=0;
		line_anchors=[];
	end
	xonly=stuff(2,1);
	yonly=stuff(2,2);
	killrd=stuff(2,3);
	ispoly=stuff(2,4);
	locate=stuff(2,5);
	htext=stuff(2,6);
	dragmode=stuff(2,7);
	linkmode=stuff(2,8);
	fastopt=stuff(3,1);
	
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);		
	%delete any locate text that may be on
	if(htext)
		delete(htext)
		htext=0;
		stuff(2,6)=0;
		set(gca,'userdata',stuff);
	end
	% get the lines data	
	%x=get(hline,'xdata');
	%y=get(hline,'ydata');
	%npts=length(x);
	% get the button
	flag=get(gcf,'selectiontype');
	if( strcmp(flag,'normal') )
		d=sqrt( (xpt-xit)^2 + (ypt-yit)^2 );
		if( d>.1*killrd )
			set(gcf,'windowbuttonmotionfcn','');
			set(gcf,'windowbuttonupfcn','');
			% test for fastopt
			if(fastopt)
				xy=get(hline2,'userdata');
				set(hline,'xdata',xy(1,:),'ydata',xy(2,:));
				if( ispoly )
					set(hline2,'xdata',xy(1,2:npts),'ydata',xy(2,2:npts));
				else
					set(hline2,'xdata',xy(1,:),'ydata',xy(2,:));
				end
			end
			return;
		else
			set(gcf,'windowbuttonmotionfcn','');
			set(gcf,'windowbuttonupfcn','');
			editlines('undo');
			if(~linkmode & ~smoothmode )
				editlines('button1up');
			elseif( linkmode==-1)
				editlines('linkpoly');
			elseif( smoothmode )
				editlines('smooth');
			end
		end
	elseif( strcmp(flag,'alt') )
		d=sqrt( (xpt-xit)^2 + (ypt-yit)^2 );
		if( d>.1*killrd )
			set(gcf,'windowbuttonmotionfcn','');
			set(gcf,'windowbuttonupfcn','');
			% test for fastopt
			if(fastopt)
				xy=get(hline2,'userdata');
				set(hline,'xdata',xy(1,:),'ydata',xy(2,:));
				if( ispoly )
					set(hline2,'xdata',xy(1,2:npts),'ydata',xy(2,2:npts));
				else
					set(hline2,'xdata',xy(1,:),'ydata',xy(2,:));
				end
			end
			return;
		else
			editlines('undo');
			if(~linkmode & ~smoothmode )
				editlines('button3up');
			elseif( linkmode)
				editlines('autoseg');
			elseif( smoothmode )
				editlines('smooth3');
			end
		end
	end
	set(gcf,'windowbuttonmotionfcn','');
	return;
end
if(strcmp(action,'xonly')) %toggle xonly
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	xonly=pdata(1);
	if( xonly ) 
		pdata(1)=0;
	else
		pdata(1)=1;
	end
	
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'yonly')) %toggle yonly
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	yonly=pdata(2);
	if( yonly ) 
		pdata(2)=0;
	else
		pdata(2)=1;
	end
	
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'xandy')) %toggle yonly
        % Get the storage bucket
        h=get(gcf,'children');
        for k=1:length(h)
                if( strcmp(get(h(k),'type'),'uicontrol') )
                        if( strcmp(get(h(k),'style'),'text') )
                                if( strcmp(get(h(k),'string'),'yrag_params') )
                                        hparams=h(k);
                                        break;
                                end
                        end
                end
        end
        % get the parameters
        pdata=get(hparams,'userdata');
	xonly=pdata(1);
        yonly=pdata(2);
                pdata(1)=0;
                pdata(2)=0;
        set(hparams,'userdata',pdata);
        return;
end
if(strcmp(action,'locate')|strcmp(action,'locateon')|strcmp(action,'locateoff'))
%toggle locate
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	if(strcmp(action,'locate'))
		locate=pdata(4);
		if( locate ) 
			pdata(4)=0;
		else
			pdata(4)=1;
		end
	elseif(strcmp(action,'locateon'))
		pdata(4)=1;
	else
		pdata(4)=0;
	end
	
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'dragmode')|strcmp(action,'elasticdrag')|...
		strcmp(action,'constantdrag')) %toggle dragmode
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	dragmode=pdata(5);
	if(strcmp(action,'dragmode'))
		if( dragmode ) 
			pdata(5)=0;
		else
			pdata(5)=1;
		end
	elseif(strcmp(action,'elasticdrag'))
		pdata(5)=0;
	elseif(strcmp(action,'constantdrag'))
		pdata(5)=1;
	end
	
	set(hparams,'userdata',pdata);
		
	return;
end
if( strcmp(action,'undo') )
% the previous action is undone by restroing things to the state saved at the last 
% buttondown
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                                        hundo=h(k);
                                        found=found+1;
				end
				if( found== 4)
					break;
				end
			end
		end
	end
	% get the undo information
	undostuff=get(hundo,'userdata');
	if(isempty(undostuff))
		return;
 end
	flag=isinf(undostuff);
	flag=find(flag==1);
	if(isempty(flag))
		otherstuff=[];
	else
		otherstuff=undostuff(flag+1:length(undostuff));
		undostuff=undostuff(1:flag-1);
	end
	% get the line data
	npts=undostuff(1);
	x=undostuff(2:npts+1);
	y=undostuff(npts+2:2*npts+1);
	ispoly=0;
	if( x(1)==x(npts) & y(1)==y(npts) )ispoly=1;end
	% get dat
	dat=undostuff(2*npts+2:length(undostuff));
	% get baddat
	baddat=get(hstor,'userdata');
	% dat is the user data of hstor and is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
	if(dat(8))
		line_anchors=dat(9:length(dat));
		nanchors=length(line_anchors)/2;
	end
	set(dat(1),'xdata',x,'ydata',y);
	if( ispoly )
		set(dat(7),'xdata',x(2:npts),'ydata',y(2:npts));
	else
		set(dat(7),'xdata',x,'ydata',y);
	end
	
	if( dat(8) )
		%make sure dat(8) still exists
		hkids=get(gca,'children');
		ind=find(dat(8)==hkids);
		if(isempty(ind))
			hline3 = line(line_anchors(1:2:2*nanchors),line_anchors...
				(2:2:2*nanchors),'color',[1 0 0],'marker','o','markersize',12,...
			'linestyle','none','erasemode','xor');
			dat(8)=hline3;
		end
		set(dat(8),'xdata',line_anchors(1:2:2*nanchors),'ydata',...
			line_anchors(2:2:2*nanchors));
	elseif(baddat(8))
		delete(baddat(8));
	end
	set(hstor,'userdata',dat);
	% deal withthe otherstuff
	if(~isempty(otherstuff))
		hline=otherstuff(1);
		n2=otherstuff(2);
		x2=otherstuff(3:2+n2);
		y2=otherstuff(3+n2:2*n2+2);
		set(hline,'xdata',x2,'ydata',y2);
	end
				
return;
end
if(strcmp(action,'smoothon')|strcmp(action,'smoothoff')|...
	strcmp(action,'linkon')|strcmp(action,'linkoff') ) %toggle smoothmode
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	if( strcmp(action,'smoothon') )
		%see if linkmode is on and turn it off
		if(pdata(3))
			editlines('linkoff');
		end
		pdata(6)=1;
		set(gcf,'pointer','circle');
	elseif( strcmp(action,'smoothoff') )
		pdata(6)=0;
		set(gcf,'pointer','arrow');
	elseif( strcmp(action,'linkon') )
		%see if smoothmode is on and turn it off
		if(pdata(6))
			editlines('smoothoff');
		end
		pdata(3)=1;
		set(gcf,'pointer','crosshair');
	elseif( strcmp(action,'linkoff') )
		pdata(3)=0;
		set(gcf,'pointer','arrow');
	end
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'faston')|strcmp(action,'fastoff') )%toggle fastopt
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	if( strcmp(action,'faston') )
		pdata(7)=1;
	elseif( strcmp(action,'fastoff') )
		pdata(7)=0;
	end
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'multnanon')|strcmp(action,'multnanoff') )%toggle multnan's
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	if( strcmp(action,'multnanon') )
		pdata(8)=1;
	elseif( strcmp(action,'multnanoff') )
		pdata(8)=0;
	end
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'deleteon')|strcmp(action,'deleteoff') )%toggle delete mode
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	if( strcmp(action,'deleteon') )
		pdata(9)=0;
	elseif( strcmp(action,'deleteoff') )
		pdata(9)=1;
	end
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'addon')|strcmp(action,'addoff') )%toggle add mode
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	if( strcmp(action,'addon') )
		pdata(10)=0;
	elseif( strcmp(action,'addoff') )
		pdata(10)=1;
	end
	set(hparams,'userdata',pdata);
		
	return;
end
if(strcmp(action,'linkmode')|strcmp(action,'polymode')) %toggle linkmode
	% Get the storage bucket
	h=get(gcf,'children');
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					break;
				end
			end
		end
	end
	
	% get the parameters
	pdata=get(hparams,'userdata');
	linkmode=pdata(3);
	if( linkmode ) 
		pdata(3)=0;
		set(gcf,'pointer','arrow')
	elseif( strcmp(action,'linkmode') )
		pdata(3)=1;
		set(gcf,'pointer','crosshair');
	elseif( strcmp(action,'polymode') )
		pdata(3)=-1;
		set(gcf,'pointer','crosshair');
	end
	
	set(hparams,'userdata',pdata);
		
	return;
end
% In linkmode, a singleclick can signify one of the following 
% 1) if it occurs on a new line, then the line is selected as the active line
% 2) if it occurs on the active line and the active line is a polygon, then
%     the polygon is broken at the clicked point
% 3) if it occurs on the active line and it is not a polygon, then it becomes
%     one by linking its endpoints
if( strcmp(action,'link') )
% the plan for linking curves is as follows:
% Switch to linkmode and select a line by clicking on it.
% move (with button 1) a point to lie on the second curve (to be linked with)
% and release. This code is then invoked to form the link. The first task is that
% the second curve must be identified. This requires examining all curves for those
% which come close to the release point. The second curve is the one with the closest
% point (as defined by the algorithm in closestpt.m) to the release pt. 
% A new point is added to the second curve with the same coordinates
%	as the moved point on the first curve. 
% Both points become anchors. 
% 
	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                	hundo=h(k);
                	found=found+1;
                elseif( strcmp(get(h(k),'string'),'yrag_hors') )
                	hhors=h(k);
                	found=found+1;
				end
				if( found== 5)
					break;
				end
			end
		end
	end
	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
	
	% get the axes userdata which has the information on the last point
	% moved
	stuff=get(gca,'userdata');
	
	% find the last moved point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
	else
		nanchors=0;
		line_anchors=[];
	end
	%get the data from the current curve
	hline1=dat(1);
	x1=get(hline1,'xdata');
	y1=get(hline1,'ydata');
	npts1=length(x1);
	ispoly=0;
	if(x1(1)==x1(npts1) & y1(1)==y1(npts1) )
		ispoly=1;
	end
	
	% get the last moved point
	% test it for validity
	if( it>length(x1) )
		error(' please move something on primary curve, then link');
	end
	xit=x1(it);
	yit=y1(it);
	
	% search all lines for the closest one to the last moved point
	hors=get(hhors,'userdata');
	d=inf;
	for k=1:length(hors)
		if(hline1~=hors(k))
			x2=get(hors(k),'xdata');
			y2=get(hors(k),'ydata');
		
			%compute closest point
			[xpt,ypt,inode,dtest]=closestpt(x2,y2,xit(1),yit(1));
			if(dtest<d)
				d=dtest;
				kclose=k;
			end
		end
	end
	
	%recompute for the closest curve
	hline2=hors(kclose);
	if(~isempty(hline2))
		x2=get(hline2,'xdata');
		y2=get(hline2,'ydata');
		npts2=length(x2);
		[xpt,ypt,inode,d]=closestpt(x2,y2,xit(1),yit(1));
	end
	
	%test other segments of hline1 as well
	isegs=find(isnan(x1));
	
	nsegs=length(isegs)+1;
	
	if(nsegs>1)
		i1=1;
		i2=isegs(1)-1;
		dseg=inf;
		for k=1:nsegs
			%test for oncurve
			if(~between(i1,i2,it(1),2) )
				[xptsg,yptsg,inodesg,dtst]=closestpt(x1(i1:i2),y1(i1:i2),xit(1),yit(1));
				if(dtst<dseg)
					dseg=dtst;
					kclose=k;
					i1close=i1;
					i2close=i2;
					inodeseg=inodesg;
					xptseg=xptsg;
					yptseg=yptsg;
				end
			end
			if(k< nsegs)
				i1=isegs(k)+1;
				if(k<nsegs-1)
					i2=isegs(k+1)-1;
				else
					i2=length(x1);
				end
			end
		end
		%compare to the best result from the other horizons
		if( dseg< d)
			hline2=hline1;
			xpt=xptseg;
			ypt=yptseg;
			d=dseg;
			inode=inodeseg+i1close-1;
			x2=x1;
			y2=y1;
			npts2=npts1;
		end
	end
		
	% now, if d is not less than 2 times the kill radius, then we just return
	killrd=stuff(2,3);
	if(d>4*killrd)
		return;
	end
	
	% Now we make the link
	% We know the insertion point to be (xpt,ypt)
	
	% see if the point already exists in the newline (or nearly so)
	alreadythere=0;
	testdist=sqrt((x2(inode)-xpt)^2 + (y2(inode)-ypt)^2);
	if( testdist < .001*killrd )
		xpt=x2(inode);
		ypt=y2(inode);
		alreadythere=1;
	end
	
	% a special test for the case of linking two segments. If inode points to 
	% the end of a segment, then we definitly do not insert a new point
	if( hline1==hline2 )
		if( inode-1 >= 1)
			if( isnan(x1(inode-1)) )
				xpt=x1(inode);
				ypt=y1(inode);
				alreadythere=1;
			end
		end
		if( inode+1 <= npts2 )
			if( isnan(x1(inode+1)) )
				xpt=x1(inode);
				ypt=y1(inode);
				alreadythere=1;
			end
		end
	end
		
	% insert the point. We do this by honoring the coordinates of the link on
	% line 2 over those of (xit,yit)
	xit=xpt*ones(size(xit));
	yit=ypt*ones(size(xit));
	if(~alreadythere)
		x2=[x2(1:inode) xpt x2(inode+1:npts2)];
		y2=[y2(1:inode) ypt y2(inode+1:npts2)];
		npts2=length(x2);
	end
	% make the inserted point an anchor in the new line
	anchors=get(hanchors,'userdata');
	ind=find(anchors==hline2);
	nanch=anchors(ind+1);% number of current anchors
	front=anchors(1:ind);
	back=anchors(ind+2+2*nanch:length(anchors));
	anch2=anchors(ind+2:ind+1+2*nanch); %the current anchors
		
	done=0;
	if(alreadythere) % see if it already is an anchor
		ia=find(anch2(1:2:2*nanch)==xpt);
		if( length(ia)>0 )
			ia2=find(anch2(2*ia)==ypt);
			if( length(ia2)>0 )
				done=1;
			end
		end
	end
	if(~done)
		nanch=nanch+1;
		anch2=[anch2 xpt ypt];
		anchors=[front nanch anch2 back];
		set(hanchors,'userdata',anchors);
	end
			
	if(hline1==hline2)
		x1=x2;
		y1=y2;
	end
	% set xit,yit in the current line and make sure it is an anchor
	x1(it)=xit;
	y1(it)=yit;
		
	% see if it is an anchor already. It definitly is if hline1==hline2
	%if(hline1==hline2)
	%	done=1;
	%else
		done=0;
	%end
	
	if(nanchors & ~done)
		ind=find(xpt==line_anchors(1:2:2*nanchors));
		if(length(ind)>0)
			ia2=find(ypt==line_anchors(2*ind));
			if(length(ia2)>0)
					done=1;
			end
		end
	end
	
	if(~done)
			nanchors=nanchors+1;
			line_anchors=[line_anchors xpt ypt];
			% line_anchors get updated
			dat=[dat(1:8) line_anchors];
			set(hstor,'userdata',dat);
	end
	% redisplay the lines
	if(~alreadythere)
			set(hline2,'xdata',x2,'ydata',y2);% the new line
	end
	set(hline1,'xdata',x1,'ydata',y1);% the current line
	if( ispoly )
			set(dat(7),'xdata',x1(2:npts1),'ydata',y1(2:npts1));
	else
			set(dat(7),'xdata',x1,'ydata',y1);
	end
	
	
	%fart with the anchors display
	if( nanchors )
		if( dat(8) ) set(dat(8),'xdata',line_anchors(1:2:...
				2*nanchors),'ydata',line_anchors(2:2:2*nanchors));
		else
			dat(8) = line(line_anchors(1:2:2*nanchors),...
					line_anchors(2:2:2*nanchors),'color',...
					[1 0 0],'marker','o','markersize',12,...
					'linestyle','none','erasemode','xor');
			set(hstor,'userdata',dat);
		end
	end
	
	return;
	
end
	
%the next section makes or breaks a polygon link
if(strcmp(action,'linkpoly'))
	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                	hundo=h(k);
                	found=found+1;
                elseif( strcmp(get(h(k),'string'),'yrag_hors') )
                	hhors=h(k);
                	found=found+1;
				end
				if( found== 5)
					break;
				end
			end
		end
	end
	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
	
	% get the axes userdata which has the information on the last point
	% moved
	stuff=get(gca,'userdata');
	
	% find the last moved point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
	else
		nanchors=0;
		line_anchors=[];
	end
	%get the data from the current curve
	hline1=dat(1);
	x1=get(hline1,'xdata');
	y1=get(hline1,'ydata');
	npts1=length(x1);
	ispoly=0;
	if(x1(1)==x1(npts1) & y1(1)==y1(npts1) )
		ispoly=1;
	end
	
	% get the last moved point
	% test it for validity
	if( it>length(x1) )
		error(' please move something on primary curve, then link');
	end
	xit=x1(it);
	yit=y1(it);
	if(~ispoly) % handle the polygon link
		npts1=npts1+1;
		x1=[x1 x1(1)];
		y1=[y1 y1(1)];
		set(dat(1),'xdata',x1,'ydata',y1);
	else % handle polygon breaking
		npts1=npts1-1;
		x1=x1(1:npts1);
		x1=[x1(it:npts1) x1(1:it-1)]; % break at 'it'
		y1=y1(1:npts1);
		y1=[y1(it:npts1) y1(1:it-1)]; % break at 'it'
		set(dat(1),'xdata',x1,'ydata',y1);
	end
return
end
%the next section breaks a line into two segments and anchors them together
if(strcmp(action,'autoseg'))
	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                	hundo=h(k);
                	found=found+1;
                elseif( strcmp(get(h(k),'string'),'yrag_hors') )
                	hhors=h(k);
                	found=found+1;
				end
				if( found== 5)
					break;
				end
			end
		end
	end
	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
	
	% get the axes userdata which has the information on the clicked point
	stuff=get(gca,'userdata');
	
	% find the clicked point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
	else
		nanchors=0;
		line_anchors=[];
	end
	%get the data from the current curve
	hline=dat(1);
	x=get(hline,'xdata');
	y=get(hline,'ydata');
	npts=length(x);
	ispoly=0;
	if(x(1)==x(npts) & y(1)==y(npts) )
		ispoly=1;
	end
	
	% get the clicked point
	xit=x(it);
	yit=y(it);
	
	% return if the point is the first or last
	if( it==1 | it==npts )
		return;
	end
	
	% make sure it is not already segmented
	if( isnan(x(it+1)) | isnan(x(it-1)) )
		return;
	end
	
	% duplicate the point and put a nan between
	
	x=[x(1:it-1) xit nan xit x(it+1:npts)];
	y=[y(1:it-1) yit nan yit y(it+1:npts)];
	
	% see if the point is already an anchor
		
	done=0;
	if(nanchors)
		ind=find(xit==line_anchors(1:2:2*nanchors));
		if(length(ind)>0)
			ia2=find(yit==line_anchors(2*ind));
			if(length(ia2)>0)
					done=1;
			end
		end
	end
	
	if(~done)
			nanchors=nanchors+1;
			line_anchors=[line_anchors xit yit];
			% line_anchors get updated
			dat=[dat(1:8) line_anchors];
			set(hstor,'userdata',dat);
	end
	% redisplay the line
	set(hline,'xdata',x,'ydata',y);% the current line
	if( ispoly )
			set(dat(7),'xdata',x(2:npts),'ydata',yl(2:npts));
	else
			set(dat(7),'xdata',x,'ydata',y);
	end
	
	
	%fart with the anchors display
	if( nanchors )
		if( dat(8) ) set(dat(8),'xdata',line_anchors(1:2:...
				2*nanchors),'ydata',line_anchors(2:2:2*nanchors));
		else
			dat(8) = line(line_anchors(1:2:2*nanchors),...
					line_anchors(2:2:2*nanchors),'color',...
					[1 0 0],'marker','o','markersize',12,...
					'linestyle','none','erasemode','xor');
			set(hstor,'userdata',dat);
		end
	end
	
	return;
	
end
if( strcmp(action,'smooth')|strcmp(action,'smooth3') )
	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                	hundo=h(k);
                	found=found+1;
                elseif( strcmp(get(h(k),'string'),'yrag_hors') )
                	hhors=h(k);
                	found=found+1;
				end
				if( found== 5)
					break;
				end
			end
		end
	end
	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
	
	% get the axes userdata which has the information on the clicked point
	stuff=get(gca,'userdata');
	
	% find the clicked point
	stuff=get(gca,'userdata');
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hline3=stuff(1,lit+7);
	if( hline3 )
		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
		nanchors=length(line_anchors)/2;
		xa=line_anchors(1:2:2*nanchors);
		ya=line_anchors(2:2:2*nanchors);
	else
		nanchors=0;
		line_anchors=[];
	end
	%get the data from the current curve
	hline=dat(1);
	x=get(hline,'xdata');
	y=get(hline,'ydata');
	npts=length(x);
	ispoly=0;
	if(x(1)==x(npts) & y(1)==y(npts) )
		ispoly=1;
	end
	
	% get the clicked point
	
	xit=x(it);
	yit=y(it);
	
	% determine the indicies of the anchor points
	ia=[];
	xanch=[];
	yanch=[];
	if(nanchors)
		for k=1:nanchors
			indx=find(x==xa(k));
			indy=find(y(indx)==ya(k));
			if(~isempty(indy))
				ia=[ia indx(indy) ];
				xanch=[xanch xa(k)*ones(size(indy))];
				yanch=[yanch ya(k)*ones(size(indy))];
			end
		end
	end
	
	% now merge these with segment boundaries
	iseg=[1 find(isnan(x)) npts];
	xseg=x(iseg);
	yseg=y(iseg);
	for k=1:length(iseg)
		iseg(k) %%%%%%%%%%
		ind=find(ia==iseg(k));
		if(isempty(ind)) % accumulate if it is not already an anchor
			ia=[ia iseg(k)];
			xanch=[xanch xseg(k)];
			yanch=[yanch yseg(k)];
		end
	end
	
	% sort them
	[ia,ind]=sort(ia);
	xanch=xanch(ind);
	yanch=yanch(ind);
	
	% determine the range of points to be smoothed
	% first we smooth only locally between anchors
	if(strcmp(action,'smooth') )
		i1=find(ia<it);
		if(~isempty(i1)) i1=ia(i1(length(i1))); end
		i2=find(ia>it);
		if(~isempty(i2)) i2=ia(i2(1)); end
	else
		i1=surround(iseg,it);
		if(~isempty(i1))
			i2=iseg(i1+1);
			i1=iseg(i1);
		end
	end
	%adjust for nans
	while(isnan(x(i1)))
		i1=i1+1;
	end
	while(isnan(x(i2)))
		i2=i2-1;
	end
	
	if(isempty(i1) | isempty(i2) | i1>=i2)
		return;
	end
	
	% now smooth
	% first form a replacement x which has a new point between every input point
	x1=x(i1:i2);
	x2=zeros(1,2*length(x1)-1);
	x2(1:2:length(x2))=x1;
	x2(2:2:length(x2))=.5*(x1(1:length(x1)-1)+x1(2:length(x1)));
	
	%y2=spline(x(i1:i2),y(i1:i2),x2);
	theta=45.;
	xint= cos(theta*pi/180.)*x2 + sin(theta*pi/180.)*(i1:.5:i2);
	xfake=  cos(theta*pi/180.)*x(i1:i2) + sin(theta*pi/180.)*(i1:i2);
	y2=spline(xfake,y(i1:i2),xint);
	xnew=[x(1:i1-1) x2 x(i2+1:npts)];
	ynew=[y(1:i1-1) y2 y(i2+1:npts)];
	npts=length(xnew);
	
	%
	% redisplay the line
	set(hline,'xdata',xnew,'ydata',ynew);% the current line
	if( ispoly )
			set(dat(7),'xdata',xnew(2:npts),'ydata',ynew(2:npts));
	else
			set(dat(7),'xdata',xnew,'ydata',ynew);
	end
	
	return;
	
end
			
		
%clear any and all anchors
if(strcmp(action,'clearanch')|strcmp(action,'clearallanch'))
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                                        hundo=h(k);
                                        found=found+1;
				end
				if( found== 4)
					break;
				end
			end
		end
	end
	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
 if(isempty(dat) & strcmp(action,'clearanch'))
		return;
	end
 if(~isempty(dat))
		% store the undo information
		% [npts x(1:npts) y(1:npts) dat]
		x=get(dat(1),'xdata');
		y=get(dat(1),'ydata');
		set(hundo,'userdata',[length(x) x y dat]);
		if(dat(8))
			delete(dat(8));
			dat(8)=0;
		end
		dat(9:length(dat))=[];
		set(hstor,'userdata',dat);
	end
	anchors=get(hanchors,'userdata');
	h=anchors(1);
	if(strcmp(action,'clearallanch'))
		nh=1;
		hstart=0;
		newanch=[];
		while( h ~= 0 )
			newanch=[newanch h 0];
			npts=anchors(nh+1);
			nh=nh+2*npts+2;
			if( nh >length(anchors) )h=0;
			else h=anchors(nh);
			end
		end
	else
		ind=find(dat(1)==anchors);
		inext=ind+2+anchors(ind+1)*2;
		newanch=[anchors(1:ind) 0 anchors(inext:length(anchors))];
	end
	set(hanchors,'userdata',newanch);
	return;
end