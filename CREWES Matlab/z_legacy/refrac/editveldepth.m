function editveldepth(arg,linehandles,datatype)
% Function allowing to edit the velocity or the depth by dragging points
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

% from a curve (values at each receivers) 
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
	% make an edit mode button
        sep=1;
        xnow=sep;
        ynow=sep;
        width=80;
        height=30;
        % now a drag popup
        xnow=xnow+sep+width;
        width=120;
        hdrag=uicontrol('style','popupmenu','string','elastic drag|constant drag',...
                               'position',[xnow,ynow,width,height],'callback',...
                                'editveldepth(''dragmode'')');
        % now a locate checkbox
        xnow=xnow+sep+width;
        width=100;
        hlocate=uicontrol('style','checkbox','string','Locate Info','position',...
                          [xnow,ynow,width,height],'callback','editlines(''locate'')');
        % now a fastopt checkbox
        xnow=xnow+sep+width;
        width=100;
        hfastopt=uicontrol('style','checkbox','string','Fast Option','position',...
                           [xnow,ynow,width,height],'callback','editveldepth(''fastopt'')');
        % and an undo button
        xnow=xnow+width+sep;
        width=50;
        hundo=uicontrol('style','pushbutton','string','Undo','position',...
                        [xnow,ynow,width,height],'callback','editlines(''undo'')');
	% Add a 'done' button
	xnow=xnow+width+sep;
	width=50;
	hdone=uicontrol('style','pushbutton','string','Done',...
			'position',[xnow,ynow,width,height],...
			'callback','editveldepth(''done'')');
        % a zoom button
        xnow=1;
        ynow=ynow+height+sep;
        width=55;
        hzoom=uicontrol('style','pushbutton','string','Zoom','position',...
                        [xnow,ynow,width,height],'callback','editveldepth(''zoom'')');
        % an unzoom button
        xnow=1;
        ynow=ynow+height+sep;
        hunzoom=uicontrol('style','pushbutton','string','unZoom','position',...
                         [xnow,ynow,width,height],'callback','editveldepth(''unzoom'')');
        % now initiate editlines, first build an anchor vector
	if( length(linehandles) == 1 )
     	   av = [linehandles(1) 0];
	else
	   av = [linehandles(1) 0 linehandles(2) 0];
	end
        editlinesinit(av);
        editlines('yonly');
	editlines('smoothoff');
	editlines('linkoff');
	editlines('deleteoff');
	editlines('addoff');
	% Save the datatype into the 'Done' button userdata
	set(hdone, 'userdata', datatype);
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
                selboxinit('editveldepth(''zoom2'')');
                return;
        end
if(     strcmp(action,'zoom2') )
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
	if( strcmp(action,'done') )
	   % The format of the returned vector from editlinesfini is:
	   % [linehandle1  #anchors  anchor1x anchor1y anchor2x anchor2y ... 
           %  linehandle2 ...]
	   moddata = editlinesfini;
	   % Find the 'Done' button, where the datatype is stored
	   c = get(gcf, 'children');
	   for i=1:length(c)
	      type = get(c(i), 'type');
	      if( strcmp(type, 'uicontrol') )
	         if( strcmp(get(c(i),'string'),'Done') )
		   datatype = get(c(i),'userdata');
		 end
	      end
	   end
	   if( strcmp(datatype, 'vel') )
	      data1h = moddata(1);
              % To find the next line handle, we need to skip any anchors on the
       	      % first line.
	      nanchors = moddata(2);
	      data2h = moddata(3+nanchors*2);
 	      if(~isempty(data1h))
		 refdata('set','v1rec',get(data1h,'ydata')/1000 );
	      end
	      if(~isempty(data2h))
		 refdata('set','v2rec',get(data2h,'ydata')/1000 );
	      end
	   end
	   if( strcmp(datatype, 'depth') )
   	      datah = moddata(1);
	      recelev = refdata('get','recelev');
	      depth = refdata('get', 'depth');
	      depth(2,:) = recelev(2,:)  - get(datah, 'ydata');
	      refdata('set','depth', depth);
	   end
	   % Now, erase all the editing buttons and clear the axis
	   % Find the 'Done' button, where the datatype is stored
	   c = get(gcf, 'children')
	   for i=1:length(c)
	      type = get(c(i), 'type');
	      if( strcmp(type, 'uicontrol') )
		 delete(c(i));
	      end
	   end
	   cla;
	   return;
	   
	end	   
		