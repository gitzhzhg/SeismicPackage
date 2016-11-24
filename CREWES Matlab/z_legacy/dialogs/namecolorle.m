function ansfini=namecolorle(masterfig,nmqst,nmans,colorqst,colorans,titlestr,optqst,optlist);

% ansfini=namecolorle(masterfig,nmqst,nmans,colorqst,colorans);
% ansfini=namecolorle(masterfig,nmqst,nmans,colorqst,colorans,titlestr);
% ansfini=namecolorle(masterfig,nmqst,nmans,colorqst,colorans,titlestr,optqst,optlist)
%
% NAMECOLORLE is a replacement for namecolorinit, namecolor and 
% namecolorfini dialogs.   namecolorle needs only be called once with
% desired questions and answers unlike the previous version.
%
% masterfig - namecolorle can only be called if there is a master figure
% present.  Inputting "gcf" is usually acceptable.
%
% nmqst - A simple string question pertaining to the desired user answer
% string.
%   nmqst='Type New Name'
%
% nmans - Predefined answer associated with the nmqst.  This can be an
% empty string, a predefined name, or an uneditable name by placing an '_'
% (underscore) at the beginning of the string.
%   nmans='This is Editable';  nmans='_This is Not Editable';
%
% colorqst - A simple string question pertaining to the desired user answer
% to the colorans
%   colorqst='Change Color Too';
%
% colorans - A color matrix of the initial color for the user to change.
%   colorans=[.5 .5 .5]
%
% titlestr - A title string for the question box.
%   titlestr='This is a title';
%
% optqst - Optional question that was avaiable in the old namecolor.  
%   optqst='Multiple Choice Question'
%
% optlist - Popup list associated with optional question.  Must be set up
% in the following format with '|' inbetween options.
%   optlist='One|front|JumpJump|ENDFORTY';
%
% ansfini - The answers are returned in cell format.  There is always 3
% cells.
%       {'New Typed Name' [1 1 1] 'One'} 
%
%
% C.B. Harrison 2002
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

try
    nrows=8;
    if(nargin<=4)
        errordlg('Need More Arguments, See "namecolorle" Help For Details');
        return
    elseif(nargin==5)
        if(~strcmp(get(masterfig,'type'),'figure'))
            errordlg('Need a Masterfigure To Configure namecolorle Interface, See "namecolorle" Help For Details');
            return
        end
        if(~ischar(nmqst))|(~ischar(nmans))|(~ischar(colorqst))|(~isnumeric(colorans))
            errordlg('Invalid Input Argument(s), See "namecolorle" Help For Details');
            return
        end
        titlestr='Please Supply This Information';
        optqst='';
        optlist='';
    elseif(nargin==6)
        if(~ischar(nmqst))|(~ischar(nmans))|(~ischar(colorqst))|(~isnumeric(colorans))|(~ischar(titlestr))
            errordlg('Invalid Input Argument(s), See "namecolorle" Help For Details');
            return
        end
        optqst='';
        optlst='';
    elseif(nargin==8)
        if(~ischar(optqst))
            errordlg('Invalid Optional Question');
            return
        elseif(~ischar(optlist))
            errordlg('Invalid Optional List');
            return
        end
        nrows=9;
    end
    nrows=9;
    %build the dialog box and the questions
    hdial=figure('visible','off','menubar','none','resize','off','closerequestfcn',@namecolorleCancel);
    pos=get(hdial,'position');
    sep=1;
    
    % assume 10 chars in 50 pixels
    width=50*ceil(length(titlestr)/10);
    height=20;
    figheight=(height+sep)*(nrows-1);
    maxwidth=width;
    % figwidth=2*(width+sep);
    ynow=figheight-height;
    xnow=sep;
    hmsg=uicontrol('style','text','string',titlestr,...
        'position',[xnow ynow width height]);
    
    % the toggle button
    hmenu=0;
    if(~isempty(optqst))
        width=50*ceil(length(optqst)/10);
        if(xnow+width>maxwidth)
            maxwidth=xnow+width;
        end
        ynow=ynow-sep-height;
        hswitch=uicontrol('style','checkbox','string',char(optqst),'position',...
            [xnow,ynow,width+width,height],'callback',@namecolorleSwitch);
        ynow=ynow-sep-height;
        hmenu=uicontrol('style','popupmenu','string',char(optlist),'position',...
            [xnow,ynow,width,height],'visible','off');
        ynow=ynow+sep+height;
    else
        hswitch=uicontrol('style','checkbox','string',' ','position',...
            [xnow,ynow,width,height],'callback',@namecolorleSwitch,'visible','off');
        hmenu=uicontrol('style','popupmenu','string','','position',...
            [xnow,ynow,width,height],'visible','off','value',1);
    end
    % the name question
    ynow=ynow-sep-height;
    if(isempty(nmqst))
        % creating genaric question
        nmqst='Please Type Information';
    end
    width=50*ceil(length(nmqst)/10);
    
    hq1=uicontrol('style','text','string',char(nmqst),'position',...
        [xnow,ynow,width,height]);
    xnow=xnow+width+sep;
    
    % This is now checks to finf a _ (underscore) in front of the
    % char(v1).  IF _ then hal is not editable
    
    check=nmans(1);
    checkit='_';
    checkit=abs(checkit);
    
    if(check==checkit)
        % Removing the _ from v1 is imperative
        nmans=nmans(2:end);
        width=50*ceil(length(nmans)/10);
        ha1=uicontrol('style','Text','string',char(nmans),'position',...
            [xnow,ynow,width,height]);
    else
        if(isempty(nmans))
            width=50;
        else
            width=50*ceil(length(nmans)/10);
        end
        ha1=uicontrol('style','edit','string',char(nmans),'position',...
            [xnow,ynow,width,height]);
    end
    xnow=xnow+width+sep;
    if(xnow+width>maxwidth)
        maxwidth=xnow+width;
    end
    
    % second question if there
    if(isempty(colorqst))
        % creating generic question
        colorqst='Please Choose Color';
    end
    
    xnow=sep;
    ynow=ynow-height-sep;
    width=50*ceil(length(colorqst)/10);
    hq2=uicontrol('style','text','string',char(colorqst),'position',...
        [xnow,ynow,width,height]);
    if(xnow+width>maxwidth)
        maxwidth=xnow+width;
    end
    
    
    % now the sliders
    if(isempty(colorans))
        colorans=[1 0 1];
    else
        if(size(colorans,2)>=4)|size(colorans,2)<=2
            % problem with color, will automatically change it
            colorans=[1 0 1];
        end
    end        
    axheight=ynow-6*sep;
    ynow=ynow-height-sep;
    xnow=sep;
    height=15;
    rval=abs(colorans(1));
    width=80;
    redslide=uicontrol('style','slider','min',0,'max',1,'value',rval,...
        'position',[xnow,ynow,width,height],'callback',@namecolorleColor);
    xnow=xnow+width+sep;
    height=20;
    width=40;
    red=uicontrol('style','text','string','red','position',...
        [xnow,ynow,width,height]);
    
    ynow=ynow-height-sep;
    xnow=sep;
    height=15;
    width=80;
    gval=abs(colorans(2));
    greenslide=uicontrol('style','slider','min',0,'max',1,'value',gval,...
        'position',[xnow,ynow,width,height],'callback',@namecolorleColor);
    xnow=xnow+width+sep;
    height=20;
    width=40;
    green=uicontrol('style','text','string','green','position',...
        [xnow,ynow,width,height]);
    
    ynow=ynow-height-sep;
    xnow=sep;
    height=15;
    width=80;
    bval=abs(colorans(3));
    blueslide=uicontrol('style','slider','min',0,'max',1,'value',bval,...
        'position',[xnow,ynow,width,height],'callback',@namecolorleColor);
    xnow=xnow+width+sep;
    height=20;
    width=40;
    blue=uicontrol('style','text','string','blue','position',...
        [xnow,ynow,width,height]);
    
    
    %the done and cancel buttons
    ynow=ynow-sep-height;
    xnow=sep;
    hdone=uicontrol('style','pushbutton','string','Done','position',...
        [xnow ynow width height],'callback',@namecolorleDone);
    xnow=xnow+width+sep;
    hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
        [xnow ynow 1.7*width height],'callback',@namecolorleCancel);
    
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
    pospar=get(masterfig,'position');
    
    px=pospar(1)+pospar(3)/2;
    py=pospar(2)+pospar(4)/2;
    
    set(hdial,'position',[px py figwidth figheight]);
    set(hdial,'visible','on');
    
    set(hdial,'userdata',[ha1 0 redslide greenslide blueslide hpoly hmenu hq1 hq2 red green blue]);
    
    uiwait;
    ansfini=get(gcf,'userdata');
    delete(gcf);
catch
    errordlg('Something Is Wrong With The Coding');
end


function namecolorleSwitch(hObject, eventdata, handles)
% Swithcing looking of interface for optional question
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

function namecolorleColor(hObject, eventdata, handles)
% Changing Color
h=get(gcf,'userdata');
hred=h(3);
hgreen=h(4);
hblue=h(5);
hpoly=h(6);
red=get(hred,'value');
green=get(hgreen,'value');
blue=get(hblue,'value');

set(hpoly,'facecolor',[red green blue]);

function namecolorleDone(hObject, eventdata, handles)
% Acquiring user choices for export
% see if the menu is visible
dat=get(gcf,'userdata');
ha=dat(1);
hmenu=dat(2);
hpoly=dat(6);
hmenu=dat(7);
kolor=get(hpoly,'facecolor');
ans1=get(ha,'string');
nm=get(hmenu,'string');
if(isempty(nm))
    ans2=nm;
else
    val=get(hmenu,'value');
    ans2=deblank(nm(val,:));
end
ansout={ans1 kolor ans2};
set(gcf,'userdata',ansout);
uiresume;

function namecolorleCancel(hObject, eventdata, handles)
% quiting without accepting user choices.
set(gcf,'userdata',{});
uiresume;