function cfig=PI_init_image()
%
% function PI_init_image()
%
% Checks global values, then instantiates a new plotimage figure window
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

% Declare globals used in this function
global SCALE_OPT CLIP XAXISTOP IMCONTROLS PICKS

% global CHECK_GLOBAL AMPFLAG NUMBER_OF_COLORS GRAY_PCT ...
%     NOBRIGHTEN COLOR_MAP PICKCOLOR CLOSEREQUEST 

% Predeclare variables

mxs2=[]; % set(hscale,'userdata',[scaleopt mxs2 mns smean2 stddev2]);
mns=[]; % set(hmaster,'userdata',[mxs smean stddev]);
smean2=[];
stddev2=[];
mxs=[];
smean=[];
stddev=[];
hi=[];     % no image yet

enb='off';   % buttons not allowed on yet

% Get initial width for plotimage window
checkscreen=get(0,'screensize');
if(checkscreen(3)>=1000)
    wid=.8;
    ht=.6;
else
    wid=.8;
    ht=.6;
end

% Create plotimage figure
cfig=figcent(wid,ht);
posax_x0=.02;%x coord of lower left corner of posaxis
posax_y0=.72;%y coord of ll corner of posaxis
posax_wid=.15;%width of posax
posax_ht=.2;%ht of posax
mainax_x0=.23;%lower left corner of main axis at launch
mainax_x0_alt=.1;%LL corner of main axis when image controls are hidden
mainax_y0=.12;%lower left corner of main axis
mainax_wid=.7;%width of main axis
mainax_wid_alt=.85;%width of main axis with imagecontrols hidden
mainax_ht=.78;%height of main axis
widim=posax_wid;%width of image control
htim_all=.588;%ht of all image controls together
htim=.03;%height of single image control
sepim=htim;%space between image controls
x0im=posax_x0;%left edge of image control
y0im=mainax_y0;%bottom left corner of image controls


set(cfig,'name',['Seismic Image Plot(' num2str(gethandlenum(gcf)) '), Simplezooming installed (Use MB1)'],...
   'closerequestfcn',@PI_Close,'tag','PLOTIMAGEFIGURE',...
   'menubar','none','numbertitle','off');
% hposax=subplot('position',[.029 .721 .158 .18]);
hposax=subplot('position',[posax_x0 posax_y0 posax_wid posax_ht]);
set(hposax,'hittest','off','visible','off','tag','POSITIONAXES');

%hmainax=subplot('position',[.291 .117 .686 .783]);
hmainax=subplot('position',[mainax_x0 mainax_y0 mainax_wid mainax_ht]);
set(hmainax,'visible','off','tag','MAINAXES');
set(hmainax,'XAxisLocation',XAXISTOP)
selboxinit('plotimage(''zoom'')',1);

%set(gcf,'currentaxes',hmainax);

%% Create File Menus
hfile1=uimenu(gcf,'Label','File','enable','on');
uimenu(hfile1,'Label','Open .mat','visible','on','callback','plotimage(''OpenFile'')',...
    'userdata',{1 gcf []});
uimenu(hfile1,'Label','Open Segy','visible','on','callback','plotimage(''OpenFile'')',...
    'userdata',{2 gcf []});
uimenu(hfile1,'label','Save','callback','plotimage(''SaveFile'')','separator','on',...
    'userdata',{1 gcf []});
uimenu(hfile1,'label','Save As','callback','plotimage(''SaveFile'')',...
    'userdata',{2 gcf []});
% hf=uimenu(hfile1,'label','Open Properties','callback','ChangePropertiesMenu',...
%     'userdata',{[4] gcf []},'separator','on');
% hf=uimenu(hfile1,'label','Save Properties','callback','plotimage(''SaveFile'')',...
%     'userdata',{[3] gcf []});
if(length(findobj(0,'type','figure','tag','PLOTIMAGEFIGURE'))<=1)
    uimenu(hfile1,'label','Spawn New Plot Image','callback','plotimage(''SpawnPlotImage'')','separator','on',...
        'tag','PLOTIMAGEMASTERFIGURE');
else
    masterfigchild=findobj(0,'type','uimenu','tag','PLOTIMAGEMASTERFIGURE');
    if(isempty(masterfigchild))
        uimenu(hfile1,'label','Spawn New Plot Image','callback','plotimage(''SpawnPlotImage'')','separator','on',...
        'tag','PLOTIMAGEMASTERFIGURE');
    else
        masterfigchild=get(masterfigchild,'parent');
        masterfig=get(masterfigchild,'parent');
        pos=get(masterfig,'position');
        set(gcf,'position',[pos(1)+20 pos(2)-20 pos(3) pos(4)]);
    end    
end
filenames=[];
lastpath=pwd;
originalpath=pwd;
try
    load('plotimagedata.mat');
catch
end
previousload=[];
if(isempty(filenames))
    % if there is no previous loaded session, this is creating blank
    % menu items
    previousload = uimenu(hfile1,'separator','on','callback','plotimage(''OpenFile'')',...
        'visible','off','userdata',{3 gcf []},'tag','QUICK_OPENFILE');
    for ii=2:4
        previousload(ii) =uimenu(hfile1,'callback','plotimage(''OpenFile'')',...
            'visible','off','userdata',{3 gcf []});
    end
    set(previousload(ii),'userdata',{3 gcf previousload originalpath lastpath});
elseif(~isempty(filenames))
    % this is creating menu items for the files that were previously load
    % in other sessions
    for ii=1:4
        vis='on';
        if(isempty(strunpad(filenames{ii,:})))
            vis='off';
        end
        if(size(filenames,1)>=ii)
            lbl=strunpad(filenames{ii,:});
            xx1=uimenu(hfile1,'label',strunpad(lbl),...
                'callback','plotimage(''OpenFile'')','visible',vis,...
                'userdata',{3 gcf []});
        else
            xx1=uimenu(hfile1,'callback','plotimage(''OpenFile'')',...
                'visible',vis,'userdata',{3 gcf []},'userdata');
        end
        previousload=[previousload xx1];
        set(previousload(1),'userdata',{3 gcf previousload originalpath lastpath},...
            'separator','on','tag','QUICK_OPENFILE');
    end
end
uimenu(hfile1,'label','Close','callback',@PI_Close,'separator','on',...
    'userdata',gcf);  % closing will be different for master figs and spawned figs

%% Option Menus
hfile2=uimenu(gcf,'label','Options','tag','PLOTIMAGEOPTIONS','enable',enb);
uimenu(hfile2,'label','Limit Box: ON','enable','on','tag','LIMITBOXMENU',...'
    'callback','plotimage(''LmLnActivation'')','UserData',1);
uimenu(hfile2,'label','Visibility of Data Figure','callback','plotimage(''limlnfigurevis'')',...
    'enable','off','tag','LIMITBOXDATAFIGUREMENU','visible','off');
if(length(findobj(0,'type','figure','tag','PLOTIMAGEFIGURE'))<=1)
%     hf3=uimenu(hfile2,'label','Change Properties','callback','plotimage(''ChangeProperties'')',...
%         'separator','on');
    
    uimenu(gcf,'label',['              Plot Image Master Figure ' date]);
end
uimenu(hfile2,'label','Open Pick File','callback','plotimage(''PicksOpen'')','separator','on');
uimenu(hfile2,'label','Save Picks','callback','plotimage(''PicksSave'')');
hamp_picks_lbl=uimenu(hfile2,'label','Copy AMP_PICKS from another window','callback','plotimage(''copyamppicks'')');
uimenu(hfile2,'label','Show AMP_PICKS event names','callback','plotimage(''showamppicks'')','checked','on');

%%%% Margrave May 30 2006 Hack begins
posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
sep='on';
if(length(findobj(gcf,'type','uimenu','tag','PLOTIMAGEMASTERFIGURE'))==1)
    sep='on';
    uimenu(hfile2,'label','Change Globals','separator','on','callback',@PI_Global);
    uimenu(hfile2,'label','Spawn New Plot Image','callback','plotimage(''SpawnPlotImage'')','separator',sep);
end
checkcolor=get(gcf,'color');
if(sum(checkcolor)==3)
    ii=2;
else
    ii=1;
end
uimenu(hfile2,'label','Send to Clipboard','callback',@PI_axis_options,'separator','on');
if(strcmp(get(posax,'visible'),'on'))
    uimenu(hfile2,'label','Hide Image Controls','callback',@PI_axis_options);
else
    uimenu(hfile2,'label','Hide Image Controls','callback',@PI_axis_options);
end
uimenu(hfile2,'label','Data Stats','callback',@PI_axis_options);
sizequestion={'Publishable Figure: ON' 'Publishable Figure: OFF'};
uimenu(hfile2,'label',sizequestion{ii},'callback','plotimage(''figuresizechange'')',...
    'separator',sep);
%%%%% end of Margrave Hack


%% Image Controls
enb='off';

% Text backing
hbak=uicontrol('style','text','string','Image Controls',...
    'units','normalized','position',[x0im y0im widim htim_all],...
    'backgroundcolor',[.502 .502 .502],'foregroundcolor',[1 1 1],...
    'tag','BACKING');
ynow=htim_all+y0im-htim-sepim;
hcolormap=uicontrol('style','popupmenu',...
    'string',listcrcolormaps(),...
    'units','normalized','tooltipstring','Pick a color map',...
    'position',[x0im ynow widim htim],'callback','plotimage(''setcolormap'')',...
    'value',PI_ColorMapValue,'tag','COLORMAP','enable',enb);
ynow=ynow-htim-sepim;
hzoompick=uicontrol('style','popupmenu',...
    'string',['Zoom|Pick time dips(Old PICK buffer)|Pick time dips(New PICK buffer)'...
        '|Pick amplitudes(Old AMP_PICK buffer)|Pick amplitudes(New AMP_PICK buffer)'],...
    'units','normalized','tooltipstring','Define mouse action as zoom or pick',...
    'position',[x0im ynow widim htim],'callback','plotimage(''zoompick'')',...
    'enable',enb);
ynow=ynow-htim-sepim;
hflip=uicontrol('style','popupmenu',...
    'string','Normal Polarity|Reverse Polarity',...
    'units','normalized','tooltipstring','Set display polarity',...
    'position',[x0im ynow widim htim],'callback','plotimage(''flip'')',...
    'userdata',1,'enable',enb);
fsize=get(0,'factoryuicontrolfontsize');
ynow=ynow-htim-sepim;
hlabel=uicontrol('style','text','fontsize',fsize,'string','Bright 0','units','normalized',...
    'position', [x0im ynow widim htim],'tooltipstring','Current brightness level','userdata',0,'enable',enb);
ynow=ynow-htim;
hslider=uicontrol('style','slider','string','Bright','units','normalized','position',...
    [x0im ynow widim htim],'callback','plotimage(''brighten'')',...
    'tooltipstring','Set image brightness','max',10,'min',-10,...
    'value',0,'userdata',hlabel,'sliderstep',[0.05 0.05],...
    'tag','brightslider','enable',enb);
% set(hslider,'tag','brightslider')

% user data for below is being used
hmsg=uicontrol('style','text','string','Polarity Normal',...
    'units','normalized',...
    'position',[0 0 .15 .05],'visible','off','enable',enb);
ynow=ynow-htim-sepim;       
hmaster=uicontrol('style','popupmenu','string','Independent|Master|Slave',...
    'units','normalized','position',[x0im ynow widim htim],'tooltipstring','Define amplitude control',...
    'callback','plotimage(''rescale'')','value',PI_AmpFlagValue,'enable',enb);
ynow=ynow-htim-sepim;
hscale=uicontrol('style','popupmenu','string',char('Mean scaling',...
    'Max scaling'),'units','normalized','position',[x0im ynow widim htim],'tooltipstring','Define data scaling mechanism',...
    'callback','plotimage(''rescale'')','value',SCALE_OPT,'enable',enb);

vis='on'; 
if(SCALE_OPT==2) 
    vis='off'; 
end

clips=PI_GetClipLevels;
nclips=length(clips);
iclip=near(clips,CLIP);
clipmsg=[ones(nclips,1)*'Cliplevel: ' num2strmat(clips)];
ynow=ynow-htim-sepim;
hclip=uicontrol('style','popupmenu','string',clipmsg,...
    'units','normalized','position',[x0im ynow widim htim],'tooltipstring','Set clip level in std deviations',...
    'callback','plotimage(''rescale'')','value',iclip,...
    'visible',vis,'enable',enb,'userdata',clips);

ynow=ynow-htim-sepim;
hpicktool=uicontrol('style','pushbutton','string','Launch PickTool',...
    'units','normalized','position',[x0im ynow widim htim],...
    'tooltipstring','Initiate horizon picking',...
    'callback','plotimage(''launchpicktool'')','tag','picktool',...
    'visible','on','enable','on');

boxstr=strmat('Limit Box: OFF','Limit Box: ON ');
hlimbox=uicontrol('style','popupmenu','string',boxstr,...
    'units','normalized','position',[0 0 widim .05],...
    'tooltipstring','Constrains master values to box',...
    'callback','LmLnActivation','value',1,...
    'visible','off','enable',enb);
% settign all buttons into for quick access
set(hbak,'userdata',[hbak hcolormap hzoompick hflip hlabel hslider hmaster hscale hclip hpicktool]);
% hlimbox visibility is now set off and actions will be accessed
% through a uicontext menu.  This context menu is built in PI_HeadOff. THis
% is a bit of a problem since the context menu is an attribute of the image
% and the image is supposed to respond to clicks for zooming and picking.
% Since MB3 invokes the context menu but is also involved in picking, this
% is a bit of a conflict. At present, the context menu only displays in
% "zoom" mode.

% zoom controls
hvrtscrol=uicontrol('style','slider','units','normalized','position',[.957 .139 .021 .762],...
    'callback','PI_zoom_slider','userdata',{1 [] []},'visible','off','sliderstep',[.05 .2]);
hhorscrol=uicontrol('style','slider','units','normalized','position',[.291 .117 .664 .021],...
    'callback','PI_zoom_slider','userdata',{2 [] []},'visible','off','sliderstep',[.05 .2]);

% message box, 
hmsgmain=uicontrol('style','text','units','normalized','string','Welcome to Plotimage',...
    'position',[0 0 1 .035],'backgroundcolor',[1 1 1],'visible','on','tag','messages');

%band aid [why is hi empty sometimes??]
if isempty(hi)
    hi=-1;
end

set(gcf,'userdata',{hflip hmsgmain -1 hmsg hi hscale hclip -1 ,...
    hzoompick hmaster hlabel hslider hclip hlimbox hfile2 hvrtscrol, ...
    hhorscrol hamp_picks_lbl hcolormap});

%Margrave hack to get around 2015 release changing figure size on copy and
%paste
set(gcf,'menubar','figure');

set(hscale,'userdata',[SCALE_OPT mxs2 mns smean2 stddev2]);
set(hmaster,'userdata',[mxs smean stddev]);
set(hclip,'userdata',iclip);
set(hmsg,'userdata',clips)
set(hcolormap,'userdata',[mainax_x0 mainax_y0 mainax_wid mainax_ht mainax_x0_alt mainax_wid_alt])
 
% 13. PICKS ... picks
if(isempty(PICKS))
    PICKS={gcf [] []};
else
    PICKS{size(PICKS,1)+1,1}=gcf;
end

if(strcmpi(IMCONTROLS,'off'))
    PI_axis_options;
end