function el_buttondown
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

% Get the storage bucket
	h=get(gcf,'children');
	found=0;
    hstor=[];
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
    if(isempty(hstor))
        set(gcf,'windowbuttondownfcn','')
        return;
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
    if(isempty(pdata))
        set(gcf,'windowbuttondownfcn','')
        return;
    end
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

	% get the anchor cell array
	anchors=get(hanchors,'userdata');

	% if provided, anchor information indicates which curves are editable and
	% what their anchors are. If provided, only curves whose handles are found in
	% this  vector will be edited regardless of whether or not thay have anchors.
	% anchor vector is defined as:
	% [hcurve1 num_anchors_curve1 (x,y)s_of_curve1_anchors hcurve2 ... ]
	%

	hline=get(gcf,'currentobject');

	if( ~strcmp('line',get(hline,'type')) ) % the user has clicked outside of all lines
		editlines('tempfini');
  		return;
    end
		
    %
	% at this point we know the user has selected a line. If the line is 
	% aleady selected, then we add a point or delete a point. If not, then
	% we change the line style and thickness to indicate that it has been 
	% selected.
	%

	% delete any locate information that may be on
		newstuff=get(gca,'userdata');
        if(~isempty(newstuff))
            stuff=newstuff{1};
            htext=newstuff{2};
        else
            stuff=[];
            htext=[];
        end
% 		if( length(stuff)>6 )
% 			htext=stuff(2,6);
% 		else
% 			htext=0;
% 		end
		
		if(~isempty(htext))
			delete(htext);
			htext=[];
			newstuff{2}=[];
			set(gca,'userdata',newstuff);
		end
	
	newline=0;
	if(isempty(dat))
		newline=1;
	elseif( dat{1}~= hline && dat{5}~= hline)
        if(isempty(dat{6}))
            newline=1;
        elseif(dat{6}~=hline)
            newline=1;
        end
	end


	if( newline ) % we have selected a new line

	% see if this is an editable curve
		canedit=1;
		nlines=size(anchors,1);
        if(nlines>0)
            canedit=0;
            for k=1:nlines
                if(anchors{k,1}==hline)
                    canedit=1;
                    nanchors=abs(round(anchors{k,2}));
                    line_anchors=anchors{k,3};
                    break;
                end
            end
        end
                
            
% 		if( ~isempty(anchors) )
% 			canedit=0;
% 			k=1;
% 			while(k<length(anchors) )
% 				if( anchors(k) == hline )
% 					canedit=1;
% 					nanchors=anchors(k+1);
% 					len=nanchors*2;
% 					line_anchors=anchors(k+2:k+1+len);
% 					break;
% 				else
% 					n=anchors(k+1);
% 					k=k+2+2*n;
% 				end
% 			end
% 		end

		if( ~canedit )
			return;
		end
					
		if(~isempty(dat)) % reset the previous line
			if(isgraphics(dat{5}) ); delete(dat{5}); end
			if(isgraphics(dat{6}) ); delete(dat{6}); end
			if(dat{2}==1)
				ls='-';
			elseif(dat{2}==2)
				ls='--';
			elseif(dat{2}==3)
				ls=':';
			elseif(dat{2}==4)
				ls='-.';
			elseif(dat{2}==5)
				ls='o';
			elseif(dat{2}==6)
				ls='+';
			elseif(dat{2}==7)
				ls='.';
			elseif(dat{2}==8)
				ls='*';
			elseif(dat{2}==9)
				ls='x';
			end
			
			if (dat{2} < 5)
			set(dat{1},'linestyle',ls,'linewidth',dat{3},...
				'color',dat{4});
			elseif (dat{2} >= 5)
			set(dat{1},'marker',ls,'linewidth',dat{3},...
				'color',dat{4});
			end
				
			% store the anchor info
			ind=el_cellfind(anchors(:,1),dat{1} );
			la=dat{7};%get the previous anchors
			na=length(la)/2;% number of prev anchors
% 			front=anchors(1:ind);
% 			back=anchors(ind+2+2*anchors(ind+1):length(anchors));
% 			anchors=[front na la back];
            anchors{ind,2}=na;
            anchors{ind,3}=la;
			
			set(hanchors,'userdata',anchors);
			
		end
		dat=cell(1,7);
		dat{1}=hline;
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
		dat{2} =ls;
		dat{3} =get(hline,'linewidth');
		dat{4} =get(hline,'color');
		
	% set the properties on the line
		set(hline,'color',[.5 .5 .5],'linewidth',.2,...
			'linestyle','-');
			
		x=get(hline,'xdata');
		y=get(hline,'ydata');
		npts=length(x);
		
	% see if it is a polygon (closed curve)
		ispoly=0;

		if( x(1)==x(npts) && y(1)==y(npts) && npts>1)
			ispoly=1;
		end
			
	% draw the points
		if(ispoly)
			dat{5}=line(x(2:npts),y(2:npts),'color',dat{4},...
				'marker','o');
		else
			dat{5}=line(x,y,'color',dat{4},...
				'marker','o');
		end


	% test the anchors for validity. This means that the anchor point must
	% be a point on the line in order to be valid
    if(~isempty(line_anchors))
        valid_anchors=nan*zeros(size(line_anchors));
        for k=1:nanchors
            ind=find( x==line_anchors(k,1) );
            if( ~isempty(ind) )
                for kk=1:length(ind)
                    if(y(ind(kk))==line_anchors(k,2))
                        valid_anchors(k,:)=line_anchors(k,:);
                    end
                end
            end
        end
        ind= isnan(valid_anchors(:,1));
        valid_anchors(ind,:)=[];
		line_anchors=valid_anchors;
		nanchors=size(line_anchors,1);
    end
			

	% draw the anchors

		dat{6}=[];
		if( nanchors>0 )
			dat{6}=line(line_anchors(:,1),line_anchors(:,2),'color',[1 0 0],...
			'marker','o','markersize',12,'linestyle','none');
		end


	% store the anchors
		dat{7}=line_anchors;
				
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
              if(~linkmode && ~smoothmode)
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
              if(~linkmode && ~smoothmode && ~nodelete)
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
		x=get(dat{1},'xdata');
		y=get(dat{1},'ydata');
		npts=length(x);
			
		live=find(~isnan(x));
		d=abs(x(live)-pt(1)) + abs(y(live)-pt(2));

		it= d==min(d) ;
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
		if( x(1)==x(npts) && y(1)==y(npts) )
			ispoly=1;
		end
			
% 		stuff=[length(it) it hstor pt dat(1) dat(7:length(dat))]; % first row
        datcrap=dat{7}';%this is purely a legacy thing
        if(isempty(datcrap)); datcrap=0; end
        %datcrap(:) is the anchors with the x's and y's as alternating
        %samples [x1 y1 x2 y2 ...]. This was how it was done initially
        stuff=[length(it) it 0 pt 0 0 0 datcrap(:)']; % first row
		stuff=[stuff;zeros(2,length(stuff))];
		stuff(2,1)=xonly;
		stuff(2,2)=yonly;
		stuff(2,3)=killrd;
		stuff(2,4)=ispoly;
		stuff(2,5)=locate;
		stuff(2,6)=0; % formerly for text handle of locate display, now is empty
		stuff(2,7)=dragmode;
		stuff(2,8)=linkmode;
		stuff(3,1)=fastopt;
		stuff(3,2)=singnan;
		stuff(3,3)=noadd;
        newstuff={stuff htext hstor dat{1} dat{5} dat{6}};%here are the text handle, the hstor handle, and selected line handle
        %stuff=newstuff{1}
        %htext=newstuff{2}
        %hstor=newstuff{3}
        %hline1=newstuff{4}
        %hline2=newstuff{5}
        %hline3=newstuff{6}
		set(gca,'userdata',newstuff);
	end

	% store the undo information
	% [npts x(1:npts) y(1:npts) dat]

	if(~linkmode)
		set(hundo,'userdata',{npts x y dat});
	else
		if(newline)
			x2=get(hline,'xdata');
			y2=get(hline,'ydata');
			n2=length(x2);
			ustuff={npts x y dat hline n2 x2 y2};
			set(hundo,'userdata',ustuff);
		else
			set(hundo,'userdata',{npts x y dat});
		end
	end