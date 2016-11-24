function el_buttonmotion(action)

% we are dragging a point of the current line
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

% find the clicked point
newstuff=get(gca,'userdata');
stuff=newstuff{1};
hstor=newstuff{3};
dat=get(hstor,'userdata');
lit=stuff(1,1);
it=stuff(1,2:lit+1);
% 	hstor=stuff(1,lit+2);
%xpt=stuff(1,lit+3);
%ypt=stuff(1,lit+4);
% 	hline=stuff(1,lit+5);
% 	hline2=stuff(1,lit+6);
% 	hline3=stuff(1,lit+7);
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
xonly=stuff(2,1);
yonly=stuff(2,2);
killrd=stuff(2,3);
ispoly=stuff(2,4);
locate=stuff(2,5);
htext=newstuff{2};
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
    if( nanchors>0 )
        ind=find( x(it(1))==line_anchors(:,1) );
        if( ~isempty(ind))
            if( y(it(1))==line_anchors(ind,2));
                % simply return
                return;
            end
        end
    end
    
    %check for accidental drag of a segment separator
    if( isnan(x(it)) || isnan(y(it)) )
        return;
    end
    
    % correct for xonly and yonly
    
    xpt = xpt*(1-yonly)+x(it(1))*yonly;
    ypt = ypt*(1-xonly)+y(it(1))*xonly;
    
    if(isgraphics(htext))
        delete(htext);
        newstuff{2}=0;
        set(gca,'userdata',newstuff);
    end
    if(locate)
        htext=text(xpt,ypt,['(' num2str(xpt) ',' num2str(ypt) ')'],...
            'color',[.5 .5 .5]);
        newstuff{2}=htext;
        set(gca,'userdata',newstuff);
    end
    % see if its a polygon
    done=0;
    if(ispoly)
        if(it(1)==1 || it(1)==npts)
            xpt = xpt(1)*(1-yonly)+x(it(1))*yonly;
            ypt = ypt(1)*(1-xonly)+y(it(1))*xonly;
            x=[xpt x(2:npts-1) xpt];
            y=[ypt y(2:npts-1) ypt];
            done=1;
        end
    end
    
    if(~done)
        if(lit>1)
            if( x(it(1))==x(it(2)) && y(it(1))==y(it(2)) )
                it=it(1);
                stuff(1,3:2+lit-1)=stuff(1,2)*ones(size(stuff(1,3:2+lit-1)));
                newstuff{1}=stuff;
                set(gca,'userdata',newstuff);
            else
                error(' you may only move nodes not segments ');
            end
        end
        xpt = xpt*(1-yonly)+x(it)*yonly;
        ypt = ypt*(1-xonly)+y(it)*xonly;
        x=[x(1:it-1) xpt x(it+1:npts)];
        y=[y(1:it-1) ypt y(it+1:npts)];
    end
    
    if(fastopt && ~linkmode)
        xmoved=xpt;
        ymoved=ypt;
    end
    
elseif( strcmp(action,'button2motion') ) %kill points the pointer drags by
    % find the points in a local box
    ind=between(xpt-10*killrd,xpt+10*killrd,x,2);
    if(isempty(ind)||ind==0); return; end
    ind2=between(ypt-10*killrd,ypt+10*killrd,y(ind),2);
    if(isempty(ind2)||ind2==0); return; end
    
    % find the point closest to the current point
    d=abs(x(ind(ind2))-xpt) + abs(y(ind(ind2))-ypt);
    dkill=min(d);
    
    ik= d==dkill ;
    itkill=ind(ind2(ik));
    % see if we are trying to drag-kill an anchor
    if( nanchors )
        ind=find( x(itkill)==line_anchors(:,1) );
        if( ~isempty(ind))
            if( y(itkill)==line_anchors(ind,2))
                % simply return
                return;
            end
        end
    end
    
    %check for accidental deletion of a segment separator
    if( isnan(x(itkill)) || isnan(y(itkill)) )
        return;
    end
    
    % see if it is a polygon endpoint
    done=0;
    if(ispoly)
        if( itkill ==1 || itkill==npts )
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
        ind=find( x(it(1))==line_anchors(:,1) );
        if( ~isempty(ind))
            if( y(it(1)==line_anchors(ind,2)))
                % simply return
                return;
            end
        end
    end
    
    %check for accidental drag of a segment separator
    if( isnan(x(it)) || isnan(y(it)) )
        return;
    end
    
    % display locate information if required
    if(isgraphics(htext))
        delete(htext);
        newstuff{2}=0;
        set(gca,'userdata',newstuff);
    end
    if(locate)
        htext=text(xpt,ypt,['(' num2str(xpt) ',' num2str(ypt) ')'],'color',[.5 .5 .5]);
        newstuff{2}=htext;
        newstuff{1}=stuff;
        set(gca,'userdata',newstuff);
    end
    
    % see if its a polygon
    
    % compute the displacement
    xdisp = (xpt-x(it))*(1-yonly);
    ydisp = (ypt-y(it))*(1-xonly);
    
    if( nanchors==0 ) % see if the curve is free floating
        
        x=x+xdisp(1);
        y=y+ydisp(1);
        
        if(fastopt)
            xmoved=x;
            ymoved=y;
        end
        
    elseif(ispoly) % treat polygons as a special case
        
        if( length(it)>1 )
            if(it(1)==1 && it(2) ==npts)||(it(1)==npts && it(2)==1 )
                it=1;
                xdisp = xdisp(1);
                ydisp = ydisp(1);
            end
        end
        
        % determine the indicies of the anchors
        ia=[];
        for k=1:nanchors
            ind=find( x == line_anchors(k,1) );
            ii =  y(ind) == line_anchors(k,2) ;
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
        while(a1==0)
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
        while(a2==0)
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
        if(between(a1,a2,it)); case1=1;
            %elseif( a1==1 & between(npts,a2,it) ) case1=0;
            %elseif( a2==1 & between(a1,npts,it) ) case1=0;
        end
        
%         done=0;
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
            
%             done=1;
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
                    xdvec(1:it) =xdisp+((-it+1):0)*xinc1;
                    xdvec(npts-1:-1:a1)=((npts-a1-1):-1:0)*xinc1;
                    xdvec(npts)=xdvec(1);
                    ydvec(1:it) =ydisp+((-it+1):0)*yinc1;
                    ydvec(npts-1:-1:a1)=((npts-a1-1):-1:0)*yinc1;
                    ydvec(npts)=ydvec(1);
                end
            else
                if(dragmode)
                    xdvec(a1+1:it)=xdisp*ones(size(a1+1:it));
                    ydvec(a1+1:it)=ydisp*ones(size(a1+1:it));
                else
                    xdvec(a1:it) = (0:(it-a1))*xinc1;
                    ydvec(a1:it) = (0:(it-a1))*yinc1;
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
                    xdvec(it:npts) = xdisp-(0:(npts-it))*xinc2;
                    xdvec(2:a2) = ((a2-2):-1:0)*xinc2;
                    xdvec(1)=xdvec(npts);
                    ydvec(it:npts) = ydisp-(0:(npts-it))*yinc2;
                    ydvec(2:a2) = ((a2-2):-1:0)*yinc2;
                    ydvec(1)=ydvec(npts);
                end
            else
                if(dragmode)
                    xdvec(it:a2-1)=xdisp*ones(size(it:a2-1));
                    ydvec(it:a2-1)=ydisp*ones(size(it:a2-1));
                else
                    xdvec(it:a2) = xdisp-(0:(a2-it))*xinc2;
                    ydvec(it:a2) = ydisp-(0:(a2-it))*yinc2;
                end
            end
%             done=1;
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
            ind=find( x == line_anchors(k,1) );
            ii =  y(ind) == line_anchors(k,2) ;
            ia=[ia ind(ii)];
        end
        
        %determine if we are between anchors or off the end
        
        offend=0; offbeg=0;
        [iasort,ias]=sort(ia); % sort the anchors based on index
        if( it< iasort(1) ); offbeg=1;
        elseif( it> iasort(nanchors) ); offend=1;
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
%             numshifted = npts-ia(ias(nanchors));
            
            
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
            i2= y(ind)==yit(k) ;
            
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

if( fastopt && ~linkmode && ~strcmp(action,'button2motion') )
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