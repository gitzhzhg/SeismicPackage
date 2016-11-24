function el_link
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
	newstuff=get(gca,'userdata');
    stuff=newstuff{1};
	
	% find the last moved point
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hline3=dat{6};
	if( hline3 )
		line_anchors=dat{7};
		nanchors=size(line_anchors,1);
	else
		nanchors=0;
		line_anchors=[];
	end

	%get the data from the current curve
	hline1=dat{1};
	x1=get(hline1,'xdata');
	y1=get(hline1,'ydata');
	npts1=length(x1);
	ispoly=0;
	if(x1(1)==x1(npts1) && y1(1)==y1(npts1) )
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
    %figure;hold;
    dtestc=[];
    nsamp=[];
    
    for k=1:length(hors)
        if(hline1~=hors(k))
            x2=get(hors(k),'xdata');
            y2=get(hors(k),'ydata');
            %Heather Fix???
            p=100/(length(x2)-1);
            x2b=[];
            y2b=[];
            if((length(unique(x2))~=length(x2))&&(length(unique(y2))~=length(y2)))||max(isnan(x2)|isnan(y2))
                for mm=1:length(x2)-1
                    diffx=(x2(mm+1)-x2(mm))/p;
                    diffy=(y2(mm+1)-y2(mm))/p;
                    for mmm=0:p-1
                        x2b=[x2b x2(mm)+diffx*mmm];
                        y2b=[y2b y2(mm)+diffy*mmm];
                    end
                end
                
            elseif min(~isnan(x2)&~isnan(y2))
                if((length(unique(x2))~=length(x2)))
                    diffy=(y2(end)-y2(1))/100;
                    y2b=y2(1):diffy:y2(end);
                    x2b=interp1(y2,x2,y2b);
                    
                elseif ((length(unique(x2))==length(x2)))
                    diffx=(x2(end)-x2(1))/100;
                    x2b=x2(1):diffx:x2(end);
                    y2b=interp1(x2,y2,x2b);
                    
                end
            end
            %compute closest point
            [xpt,ypt,inode,dtest]=closestpt(x2b,y2b,xit(1),yit(1));
            %plot(x2b,y2b,'o');
            dtestc=[dtestc dtest];
            nsamp=[nsamp length(x2b)];
            if(dtest<d)
                d=dtest;
                kclose=k;
            end
        end
    end
	%plot(xit(1),yit(1),'d', 'markersize',10,'markerfacecolor','r');flipy;
	%recompute for the closest curve
	hline2=hors(kclose);
    if(~isempty(hline2))
        x2=get(hline2,'xdata');
        y2=get(hline2,'ydata');
        npts2=length(x2);
        dline=d;
        [xpt,ypt,inode,d]=closestpt(x2,y2,xit(1),yit(1));
    end
	
    if length(xit)>1
        it=find(isnan(x1));
        xit=xit(1);
        yit=yit(1);
    end
    
    if length(xit)>1
        it=find(isnan(x1));
        xpt=xpt(1);
        ypt=ypt(1);
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
	if(dline>4*killrd)
		return;
	end
	
	% Now we make the link
	% We know the insertion point to be (xpt,ypt)
	
	% see if the point already exists in the newline (or nearly so)
	alreadythere=0;
	testdist=sqrt((x2(inode)-xit(1)).^2 + (y2(inode)-yit(1)).^2);
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
    
    

	
    %
    ind=[];
    y2c=y2;
    x2c=x2;
    % Check to see if line segment is in segments and extract appropriate
    % segment
    if any(isnan(x2)|isnan(y2))
        y2c=[];
        x2c=[];
        if find(isnan(x2))==find(isnan(y2))
            ind=find(isnan(x2));
            for ii=1:length(ind)
                if ind(ii)>inode
                    sp=ind(ii);
                    break;
                end
               sp=length(x2); 
            end
            if sp==ind(1)
                sp=sp-1;
                st=1;
            elseif sp==length(x2)
                st=ind(ii)+1;
            else st=ind(ii-1)+1;
                sp=sp-1;
            end
            
            y2c=y2(st:sp);
            x2c=x2(st:sp);
            
        end
        
    end
    
    % Interpret new x and y points for the connection
    if (length(unique(x2c))==length(x2c))
        ypt=interp1(x2c,y2c,xit(1));
        xpt=xit(1);
    elseif (length(unique(y2c))==length(y2c))
        xpt=interp1(y2c,x2c,yit(1));
        ypt=yit(1);
    end;
    
    %Put segmented line back together
    if ~isempty(ind)
        x2b=x2;
        y2b=y2;
        x2=[x2(1:st-1) x2c x2(sp+1:end)];
        y2=[y2(1:st-1) y2c y2(sp+1:end)];
    end;  
   
    % insert the point. We do this by honoring the coordinates of the link on
	% line 2 over those of (xit,yit)
    xit=xpt.*ones(size(xit));
 	yit=ypt.*ones(size(xit));
   
    if(~alreadythere)
        if inode==length(x2);
            inode=inode-1;
        end
        if min(xpt<x2(inode)&inode~=1)
            inode=inode-1;
        end
        x2=[x2(1:inode) xpt x2(inode+1:npts2)];
        y2=[y2(1:inode) ypt y2(inode+1:npts2)];
        npts2=length(x2);
    end
	

	% make the inserted point an anchor in the new line
	anchors=get(hanchors,'userdata');
% 	ind=find(anchors==hline2);
    ind=el_cellfind(anchors(:,1),hline2);
% 	nanch=anchors(ind+1);% number of current anchors
% 	front=anchors(1:ind);
% 	back=anchors(ind+2+2*nanch:length(anchors));
% 	anch2=anchors(ind+2:ind+1+2*nanch); %the current anchors
    anch2=anchors{ind,3};
		
	done=0;
	if(alreadythere) % see if it already is an anchor
% 		ia=find(anch2(1:2:2*nanch)==xpt);
% 		if( ~isempty(ia) )
% 			ia2=find(anch2(2*ia)==ypt, 1);
% 			if( ~isempty(ia2) )
% 				done=1;
% 			end
% 		end
        ind=find(xpt==anch2(:,1));
        if(~isempty(ind))
            for kk=1:length(ind)
                if(ypt==anch2(ind(k),2));
                    done=1;
                end
            end
        end
	end

	if(~done)
% 		nanch=nanch+1;
		anch2=[anch2; xpt ypt];
		anchors{ind,3}=anch2;
        anchors{ind,2}=size(anch2,1);
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
	
	if(nanchors && ~done)
		ind=find(xpt==line_anchors(:,1));
		if(~isempty(ind))
			ia2=find(ypt==line_anchors(:,2), 1);
			if(~isempty(ia2))
					done=1;
			end
		end
	end
	
	if(~done)
% 			nanchors=nanchors+1;
			line_anchors=[line_anchors; xpt ypt];
            nanchors=size(line_anchors,1);
			% line_anchors get updated
			dat{7}=line_anchors;
			set(hstor,'userdata',dat);
	end

	% redisplay the lines
	if(~alreadythere)
			set(hline2,'xdata',x2,'ydata',y2);% the new line
	end

	set(hline1,'xdata',x1,'ydata',y1);% the current line

	if( ispoly )
			set(dat{5},'xdata',x1(2:npts1),'ydata',y1(2:npts1));
	else
			set(dat{5},'xdata',x1,'ydata',y1);
	end
	
	
	%fart with the anchors display
    if( nanchors )
        if( isgraphics(dat{6}) ); set(dat{6},'xdata',line_anchors(1:2:...
                2*nanchors),'ydata',line_anchors(2:2:2*nanchors));
        else
            dat{6} = line(line_anchors(1:2:2*nanchors),...
                line_anchors(2:2:2*nanchors),'color',...
                [1 0 0],'marker','o','markersize',12,...
                'linestyle','none','erasemode','xor');
            set(hstor,'userdata',dat);
        end
    end

   
   
    