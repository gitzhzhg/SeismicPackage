function editlines(action)
% EDITLINES: a general digital editing tool for line data.
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
% NOTE ON MOUSE BUTTONS: On the PC, button 1 is the left mouse button. 
%      Button 2 is the middle button but can be simulated as shift(key)-button-1. 
%      Button 3 is the right button but can be simulated as ctrl(key)-button-1.
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
% The anchors cell array {hcurve1 n1 pts1 hcurve2 n2 pts2 ...}
% is passed via the current axis userdata. At the end of editing, it may be 
% retrieved from there for future reference. Here pts1 is a n1-by-2 array
% contining the x and y coordinates for hcurve1 with the x coordinates in
% column 1 and the y coordinates in column 2. Similarly for the other
% lines.
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
% strcmp(action,'undo') ... undo the last (latest) whatever
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
   el_init
	return;
end

if(strcmp(action,'fini') )
	el_fini
    return % $$$$ possibly extra
end

if(strcmp(action,'tempfini'))
    el_tempfini
	return;
end


if( strcmp(action,'buttondown') )
    el_buttondown
return;
end



if( strcmp(action,'button1up') || strcmp(action,'button2up') ...
		|| strcmp(action,'button3up')  )
	
	el_buttonup(action)			
return;
end

if( strcmp(action,'button1motion') || strcmp(action,'button2motion') ||...
	strcmp(action,'button3motion') )
	
	el_buttonmotion(action)
			
  return;
end

if(strcmp(action,'stopmotion') )

   el_stopmotion
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


if(strcmp(action,'locate')||strcmp(action,'locateon')||strcmp(action,'locateoff'))
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

if(strcmp(action,'dragmode')||strcmp(action,'elasticdrag')||...
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
    el_undo			
return;

end



if(strcmp(action,'smoothon')||strcmp(action,'smoothoff')||...
	strcmp(action,'linkon')||strcmp(action,'linkoff') ) %toggle smoothmode
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

if(strcmp(action,'faston')||strcmp(action,'fastoff') )%toggle fastopt
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

if(strcmp(action,'multnanon')||strcmp(action,'multnanoff') )%toggle multnan's
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

if(strcmp(action,'deleteon')||strcmp(action,'deleteoff') )%toggle delete mode
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

if(strcmp(action,'addon')||strcmp(action,'addoff') )%toggle add mode
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


if(strcmp(action,'linkmode')||strcmp(action,'polymode')) %toggle linkmode
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

    el_link
return
end

%the next section breaks a line into two segments and anchors them together
if(strcmp(action,'autoseg'))

    el_autoseg
    

	
	return;
	
end

if( strcmp(action,'smooth')||strcmp(action,'smooth3') )

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
% 				elseif( strcmp(get(h(k),'string'),'yrag_params') )
% 					hparams=h(k);
% 					found=found+1;
% 				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
% 					hanchors=h(k);
% 					found=found+1;
% 				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
%                 	hundo=h(k);
%                 	found=found+1;
%                 elseif( strcmp(get(h(k),'string'),'yrag_hors') )
%                 	hhors=h(k);
%                 	found=found+1;
				end
				if( found== 1)
					break;
				end
			end
		end
	end

	dat=get(hstor,'userdata');
	% the user data of hstor is:
	% PREVIOUS
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
    % NEW
	% dat{1} = handle of the selected line
	% dat{2} = original linestyle
	% dat{3} = original linewidth
	% dat{4} = original color (formerly dat(4:6))
	% dat{5} = handle of duplicate line (formerly dat(7))
	% dat{6} = handle of the anchor line (formerly dat(8))
	% dat{7} = (x,y)'s of the anchors of the selected line (formerly dat(9:length(dat))
	
	% get the axes userdata which has the information on the clicked point
	newstuff=get(gca,'userdata');
    stuff=newstuff{1};
    %stuff=newstuff{1}
    %htext=newstuff{2}
    %hstor=newstuff{3}
    %hline1=newstuff{4}
    %hline2=newstuff{5}
    %hline3=newstuff{6}
	% find the clicked point
% 	lit=stuff{1,1};
	it=stuff(1,2);
	hline3=newstuff{6};
	if( isgraphics(hline3) )
		line_anchors=dat{7};
		nanchors=length(line_anchors);
		xa=line_anchors(:,1);
		ya=line_anchors(:,2);
	else
		nanchors=0;
% 		line_anchors=[];
	end

	%get the data from the current curve
	hline=dat{1};
	x=get(hline,'xdata');
	y=get(hline,'ydata');
	npts=length(x);
	ispoly=0;
	if(x(1)==x(npts) && y(1)==y(npts) )
		ispoly=1;
	end
	
	% get the clicked point
	
% 	xit=x(it);
% 	yit=y(it);
	
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
		ind=find(ia==iseg(k), 1);
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
		if(~isempty(i1)); i1=ia(i1(length(i1))); end
		i2=find(ia>it);
		if(~isempty(i2)); i2=ia(i2(1)); end
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
	
	if(isempty(i1) || isempty(i2) || i1>=i2)
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
			set(dat{5},'xdata',xnew(2:npts),'ydata',ynew(2:npts));
	else
			set(dat{5},'xdata',xnew,'ydata',ynew);
	end
	
	return;
	
end
			
		
%clear any and all anchors
if(strcmp(action,'clearanch')||strcmp(action,'clearallanch'))
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
    
    if(isempty(dat) && strcmp(action,'clearanch'))
        return;
    end
    
    if(~isempty(dat))
        % store the undo information
        % [npts x(1:npts) y(1:npts) dat]
        x=get(dat{1},'xdata');
        y=get(dat{1},'ydata');
        set(hundo,'userdata',[length(x) x y dat]);
        
        if(dat{6})
            delete(dat{6});
            dat{6}=[];
        end
        dat{7}=[];
        set(hstor,'userdata',dat);
    end
    
    anchors=get(hanchors,'userdata');
    if(strcmp(action,'clearallanch'))
%         nh=1;
%         hstart=0;
%         newanch=[];
%         while( h ~= 0 )
%             newanch=[newanch h 0];
%             npts=anchors(nh+1);
%             nh=nh+2*npts+2;
%             if( nh >length(anchors) )h=0;
%             else h=anchors(nh);
%             end
%         end
        newanch=anchors;
        for k=1:size(anchors,1)
            newanch{k,2}=0;
            newanch{k,3}=[];
        end
    else
        ind=el_cellfind(anchors{:,1},dat{1});
        newanch=anchors;
        newanch{ind,2}=0;
        newanch{ind,3}=[];
    end
    
    set(hanchors,'userdata',newanch);
    
    return;
end