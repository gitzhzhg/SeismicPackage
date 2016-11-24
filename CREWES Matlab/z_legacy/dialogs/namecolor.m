function namecolor(action)

% Please use "namecolorle" when developing new programs.
%
% by G.F. Margrave, November 1993
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

if(strcmp(action,'init') )
		hax=gca;
        qs=get(hax,'userdata');
		[m,n]=size(qs);
        % see if we have a namelist supplied
        nrows=8;
        if( m > 5)
        	q3=qs(6,:);
        	ind=find(q3==1);
        	if(~isempty(ind))
        		q3=q3(1:ind(1)-1);
        	end
        	namelist=qs(7,:);
        	ind=find(namelist==1);
        	if(~isempty(ind))
        		namelist=namelist(1:ind(1)-1);
        	end
        	nrows=9;
        end
        
        %build the dialog box and the questions
        hdial=figure('visible','off','menubar','none');
        pos=get(hdial,'position');
        sep=1;
        %
        % assume 8 chars in 50 pixels
        %
        [r,c]=size(qs);
        q0='Please supply this information:';
        width=50*ceil(length(q0)/8);
        height=20;
        figheight=(height+sep)*(nrows-1);
        maxwidth=width;
        % figwidth=2*(width+sep);
        ynow=figheight-height;
        xnow=sep;
        hmsg=uicontrol('style','text','string',q0,...
        	'position',[xnow ynow width height]);
        	
        % the toggle button
        hmenu=0;
        if(m>5)
        	width=50*ceil(length(q3)/8);
        	if(xnow+width>maxwidth)
        		maxwidth=xnow+width;
        	end
        	ynow=ynow-sep-height;
        	hswitch=uicontrol('style','checkbox','string',char(q3),'position',...
        		[xnow,ynow,width,height],'callback','namecolor(''switch'')');
        	ynow=ynow-sep-height;
        	hmenu=uicontrol('style','popupmenu','string',char(namelist),'position',...
        		[xnow,ynow,width,height],'visible','off');
        	ynow=ynow+sep+height;
        end
        % the name question
        	ynow=ynow-sep-height;
        	q=qs(1,:);
			v1=qs(2,:);
        	ind=find(q==1);
        	if(~isempty(ind))
        		q=q(1:ind(1)-1);
        	end
			ind=find(v1==1);
                if(~isempty(ind))
                        v1=v1(1:ind(1)-1);
                end
			width=50*ceil(length(q)/8);
        	
        	hq1=uicontrol('style','text','string',char(q),'position',...
        		[xnow,ynow,width,height]);
           xnow=xnow+width+sep;
           
           % This is now checks to finf a _ (underscore) in front of the
           % char(v1).  IF _ then hal is not editable
           
           check=v1(1);
           checkit='_';
           checkit=abs(checkit);
           
           if(check==checkit)
              % Removing the _ from v1 is imperative
              v1=v1(2:end);
              ha1=uicontrol('style','Text','string',char(v1),'position',...
                 [xnow,ynow,width,height]);
           else
              ha1=uicontrol('style','edit','string',char(v1),'position',...
                 [xnow,ynow,width,height]);
           end
           
           if(xnow+width>maxwidth)
              maxwidth=xnow+width;
           end
           
	% second question if there
	q2=qs(3,:);
	q=qs(3,:);
	ind=find(q==1);
	if(~isempty(ind))
		q=q(1:ind(1)-1);
	end

	if(~isempty(qs))
		xnow=sep;
		ynow=ynow-height-sep;
		width=50*ceil(length(q)/8);
		hq2=uicontrol('style','text','string',char(q),'position',...
			[xnow,ynow,width,height]);
		if(xnow+width>maxwidth)
        		maxwidth=xnow+width;
        end
	end
        	
        % now the sliders
        
		c=qs(5,1:3);

		axheight=ynow-6*sep;
        ynow=ynow-height-sep;
        xnow=sep;
        height=15;
        rval=abs(c(1));
		width=80;
        redslide=uicontrol('style','slider','min',0,'max',1,'value',rval,...
        	'position',[xnow,ynow,width,height],'callback','namecolor(''color'')');
        xnow=xnow+width+sep;
        height=20;
        width=40;
        red=uicontrol('style','text','string','red','position',...
        	[xnow,ynow,width,height]);
        	
        ynow=ynow-height-sep;
        xnow=sep;
        height=15;
        width=80;
		gval=abs(c(2));
        greenslide=uicontrol('style','slider','min',0,'max',1,'value',gval,...
        	'position',[xnow,ynow,width,height],'callback','namecolor(''color'')');
        xnow=xnow+width+sep;
        height=20;
        width=40;
        green=uicontrol('style','text','string','green','position',...
        	[xnow,ynow,width,height]);
        	
        ynow=ynow-height-sep;
        xnow=sep;
        height=15;
        width=80;
		bval=abs(c(3));
        blueslide=uicontrol('style','slider','min',0,'max',1,'value',bval,...
        	'position',[xnow,ynow,width,height],'callback','namecolor(''color'')');
        xnow=xnow+width+sep;
        height=20;
        width=40;
        blue=uicontrol('style','text','string','blue','position',...
        	[xnow,ynow,width,height]);
        

		  %the done and cancel buttons
        ynow=ynow-sep-height;
		xnow=sep;
        hdone=uicontrol('style','pushbutton','string','Done','position',...
        	[xnow ynow width height],'callback','namecolor(''done'')');
		xnow=xnow+width+sep;
        hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
        	[xnow ynow 1.7*width height],'callback','namecolor(''done'')');

	% make an axes
	xnow=120+3*sep;
	ynow=3*sep;
	figwidth=maxwidth;
	width=figwidth-xnow-sep;
	height=axheight;
	hcolorax=axes('units','pixels','position',[xnow,ynow,width,height],...
		'visible','off','xtick',[],'ytick',[],'xticklabel',[],...
		'yticklabel',[],'yticklabelmode','manual','xticklabelmode','manual');
	hpoly=fill([0 1 1 0 0],[0 0 1 1 0],[rval gval bval]);
	set(hcolorax,'visible','off');
        
	% get the position of the calling figure
	hparent=get(hax,'parent');
	pospar=get(hparent,'position');

	px=pospar(1)+pospar(3)/2;
	py=pospar(2)+pospar(4)/2;

        set(hdial,'position',[px py figwidth figheight]);
        set(hdial,'visible','on');
        
        transfer=qs(4,:);
        ind=find(transfer==1);
        if(~isempty(ind))
        	transfer=transfer(1:ind(1)-1);
        end
        
        hstore=uicontrol('style','text','visible','off','userdata',...
        	[hax abs(transfer)]);
        	
         
        set(hdial,'userdata',[ha1 hstore redslide greenslide blueslide hpoly ...
        	hmenu hq1 hq2 red green blue]);
        
        return;
        
end
if(strcmp(action,'done'))
   % get the answers and put them in gca userdata

	  %check for cancel
	  test=get(gco,'string');

	if(strcmp(test,'Cancel'))
		name=-1;
		dat=get(gcf,'userdata');
		hstore=dat(2);
	  dat=get(hstore,'userdata');
	  set(dat(1),'userdata',[name nan 0 0 0]);
 else 
		% see if the menu is visible
		dat=get(gcf,'userdata');
		hmenu=dat(7);
		if( hmenu )
			flag=get(hmenu,'visible');
		else
			flag='off';
		end
		if( strcmp(flag,'off') )
   		nq=length(dat)-1;
   		ha=dat(1);
   		a=get(ha,'string');% get the name

			hpoly=dat(6);
			kolor=get(hpoly,'facecolor');
        
        hstore=dat(2);
        dat=get(hstore,'userdata');
        set(dat(1),'userdata',[abs(a) nan kolor]);
        
		else
   
   		name=get(hmenu,'value');% an integer flag
   		hstore=dat(2);
        dat=get(hstore,'userdata');
        set(dat(1),'userdata',[name]);
		end
	end
   		
        
        close(gcf);
        
        % call the transfer expression
        transfer=setstr(dat(2:length(dat)));
        
        eval(transfer);
        
        return;
        	
end

if(strcmp(action,'color'))
	% change the color
	h=get(gcf,'userdata');
	hred=h(3);
	hgreen=h(4);
	hblue=h(5);
	hpoly=h(6);
	red=get(hred,'value');
	green=get(hgreen,'value');
	blue=get(hblue,'value');

	set(hpoly,'facecolor',[red green blue]);

	return;

end

if(strcmp(action,'switch'))
	h=get(gcf,'userdata');
	hred=h(3);
	hgreen=h(4);
	hblue=h(5);
	hpoly=h(6);
	hmenu=h(7);
	hq1=h(8);
	hq2=h(9);
	ha1=h(1);
	red=h(10);
	green=h(11);
	blue=h(12);
	
	flag=get(hmenu,'visible');
	if( strcmp(flag,'off') )
	
		set(ha1,'visible','off');
		set(hq1,'visible','off');
		set(hq2,'visible','off');
		set(hred,'visible','off');
		set(hblue,'visible','off');
		set(hgreen,'visible','off');
		set(hblue,'visible','off');
		set(hpoly,'visible','off');
		set(red,'visible','off');
		set(green,'visible','off');
		set(blue,'visible','off');
	
		set(hmenu,'visible','on');
	else
		set(ha1,'visible','on');
		set(hq1,'visible','on');
		set(hq2,'visible','on');
		set(hred,'visible','on');
		set(hblue,'visible','on');
		set(hgreen,'visible','on');
		set(hblue,'visible','on');
		set(hpoly,'visible','on');
		set(red,'visible','on');
		set(green,'visible','on');
		set(blue,'visible','on');
	
		set(hmenu,'visible','off');
	end
	
	return;
	
end
	