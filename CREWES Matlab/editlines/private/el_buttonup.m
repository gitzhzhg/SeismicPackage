function el_buttonup(action)
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

	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');

	
				
	% we are adding (or deleting) a point to the current line
	% or toggling the anchor status of a point
	% find the clicked point
	newstuff=get(gca,'userdata');
    stuff=newstuff{1};
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hstor=newstuff{3};
    dat=get(hstor,'userdata');
	xpt=stuff(1,lit+3);
	ypt=stuff(1,lit+4);
    hline=newstuff{4};
    hline2=newstuff{5};
    hline3=newstuff{6};
	if( isgraphics(hline3) )
		line_anchors=dat{7};
		nanchors=size(line_anchors,1);
	else
		nanchors=0;
		line_anchors=[];
	end
% 	xonly=stuff(2,1);
% 	yonly=stuff(2,2);
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
		if( nanchors>0 )
			ind=find( x(it)==line_anchors(:,1));
			if( ~isempty(ind))
				if( y(it)==line_anchors(ind,2))
					return;
				end
			end
		end
		
		%check for accidental deletion of a segment separator
		if( isnan(x(it)) || isnan(y(it)) )
			return;
		end
		
		% check for deletion of polygon endpoint
		done=0;
		if( it(1)==1 || it(1)==npts ) % it might be an end point
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
			if(it(1)==1 || it(1)==npts)
				it=1; % make sure only one of the endpoints is an anchor
			end
		end
		
		%check for accidental selection of a segment separator
		if( isnan(x(it)) || isnan(y(it)) )
			return;
		end
		
		if( length(it)>1 )
			it=it(1);
		end
		
		% see if it already is an anchor
		found=0; 
        if( nanchors )
            ind=find( x(it)==line_anchors(:,1) );
            if( ~isempty(ind))
                if( y(it)==line_anchors(ind,2))
                    % ok remove it from anchor status.
                    % ind points to the selected anchor
                    nanchors=nanchors-1;
                    line_anchors(ind,:)=[];
                    ikill=zeros(nanchors,1);
                    % remove any line anchors within .5*killrd of this one
                    for k=1:nanchors
                        xa=line_anchors(k,1);
                        ya=line_anchors(k,2);
                        d=sqrt((xa-x(it))^2 + (ya-y(it))^2 );
                        if( d< killrd )
                            ikill(k)=1;
                        end
                    end
                    ind=ikill==1;
                    line_anchors(ind,:)=[];
                    nanchors=size(line_anchors,1);
%                     kmax=nanchors;
%                     nanchors=0;
%                     la=[];
%                     for k=1:kmax
%                         xa=line_anchors(2*k-1);
%                         ya=line_anchors(2*k);
%                         d=sqrt( (xa-x(it))^2 + (ya-y(it))^2 );
%                         if( d> killrd )
%                             nanchors=nanchors+1;
%                             la=[la xa ya];
%                         end
%                     end
%                     line_anchors=la;
                    found=1;
                end
            end
        end
		if( found==0 )
			% make the point an anchor
			nanchors=nanchors+1;
			line_anchors=[line_anchors; x(it) y(it)];
		end
		
		% store the anchor information
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
		dat=get(hstor,'userdata');
		dat{7}=line_anchors;
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
		if( isgraphics(hline3) ); set(hline3,'xdata',line_anchors(:,1),'ydata',...
			line_anchors(:,2));
		else
			hline3 = line(line_anchors(:,1),line_anchors(:,2),...
            'color',[1 0 0],'marker','o','markersize',12,'linestyle','none');
			dat{6}=hline3;
			set(hstor,'userdata',dat);
		end
	elseif( nanchors==0 && ~isempty(isgraphics(hline3)) )
		delete(hline3);
		dat{6}=[];
		set(hstor,'userdata',dat);
	end