function dataout=lascreator(arg1,arg2,arg3,arg4);
%-------------------------------
%--------- Las Creator --------- 
%-------------------------------
%
% lascreator
% dataout=lascreator(Anything);
% 
% LASCREATOR allows users to create their own VS, VP and density well logs.
% The VS and VP is converted to slowness when saving to exporting the
% created file.  Future releases may include more properties able to be
% exported.  Exported files are Version 2.0 LAS files.
%
% LASCREATOR can be called in two ways.  Simply typing lascreator will pop
% up the lascretor figure allowing for las files to be created and saved.
% The second way is to add a string or number as the the only arguement in
% to the routine.  This last way is best used in conjuction with other
% programs and utilizes.  This way uses uiwait and releases dataout in the
% following way.
%
% dataout={lasheader logdata};
% 
% C.B. Harrison 2003
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


if(nargin<1)
    action='init';
elseif(nargin==1)
    action='two';
elseif(nargin==2)
    action='init';
elseif(nargin==3)
    action='init';
end

switch action
    case 'init'
        lascreator_figure_init;
        controlout=lascreator_datacheck;    % checking data
    case 'two'
        lascreator_figure_init('EXPORT');
        controlout=lascreator_datacheck;    % checking data
        uiwait;
        % Pulling data out
        h=get(gcf,'userdata');
        if(~isstruct(h))
            return    
        end
        LayerData=getfield(h,'Data');
        GeneralData=getfield(h,'GeneralData');
        BlockHandles=getfield(h,'BlockHandles');
        BlockButtons=getfield(h,'BlockButtons');
        FigureHandles=getfield(h,'FigureHandles');
        hmsg=getfield(FigureHandles,'Message');
        %--------------------------------------
        hSend=getfield(FigureHandles,'Save');
        dataout=get(hSend,'userdata');
        dataout={dataout{2} dataout{3}};
        delete(gcf);
    case 'three'
    case 'four'        
end


function lascreator_figure_init(hObject, eventdata, handles)
% this was a guide generated code with modifications made

hfig=figcent(811,570,'pixels');
set(hfig,...
    'Name','LAS Creator',...
    'menu','none','numbertitle','off',...
    'closerequestfcn',@lascreator_cancel,'units','normalized',...
    'windowbuttondownfcn','');
cmenu=uicontextmenu;

haxes = axes(...
    'Color',get(0,'defaultaxesColor'),...
    'Position',[0.5881 0.1192 0.4069 0.7894],...
    'Tag','axes1','xtick',[],'ytick',[]);
title('Well Log');
ylabel('Depth');
flipy;
hold;
% Message Handle
%----------------
hmsg = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0 0 1 0.0350],...
'String','Figure Message Bar',...
'Style','text',...
'Tag','text3');

% Main Figure Applications
%--------------------------

hmainbacking = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.5019 0.5019 0.5019],...
'Position',[.402 0.114 0.1 0.679],...
'String',{ 'Layer Controls' },'tooltipstring','',...
'Style','text',...
'foregroundcolor',[1 1 1],...
'userdata',[1]);

hMainUpDate = uicontrol(...
'Units','normalized','position',[.408 .723 .086 .04],...
'Callback',@lascreator_datacheck,'string','Update',...
'tooltipstring','Updating will occure when any action is taken');

hMainHiddenData = uicontrol(...
'Units','normalized','position',[.408 .677 .086 .04],...
'Callback',@lascreator_gcfcontextmenu,'string','Hidden Data',...
'tooltipstring','Edit Company, Field, Service, Country, ZStep, KB, UWID');


hMainScrollText = uicontrol('style','text',...
'Units','normalized','position',[.402 .611 .1 .026],...
'string','Scroll Layers');

hMainOneUp = uicontrol(...
'Units','normalized','position',[.408 .561 .086 .04],...
'Callback',@lascreator_master_move,'string','Up','userdata',{'oneup' 1});

hMainOneDown = uicontrol(...
'Units','normalized','position',[.408 .511 .086 .04],...
'Callback',@lascreator_master_move,'string','Down','userdata',{'onedown' 1});

hmainreset = uicontrol(...
'Units','normalized','position',[.408 .123 .086 .04],...
'Callback',@lascreator_master_reset,'string','Reset All','userdata',[]);

% checking number of arguments, if there is one agrument, that means
% lascreator is being called from another program or routine so instead
if(nargin==1)
    snd='Done';
    vis='on';
    exp=[1];
else
    snd='Save';
    vis='off';
    exp=[2];
end
set(hfig,'uicontextmenu',cmenu)
m1=uimenu(cmenu,'label','Change Hidden Data','callback',@lascreator_gcfcontextmenu);
m1=uimenu(cmenu,'label',snd,'callback',@lascreator_save,'separator','on');
m1=uimenu(cmenu,'label','Cancel','callback',@lascreator_cancel,'visible',vis);

hSend = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_save,...
'Position',[0.2207 0.0385 0.1331 0.0438],...
'String',snd,...
'Tag','pushbutton2','userdata',{exp [] []});

hCancel = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_cancel,...
'Position',[0.3834 0.0385 0.1331 0.0438],...
'String','Cancel',...
'Tag','pushbutton3','userdata',[1],...
'visible',vis);

FigureHandles.Axes=haxes;
FigureHandles.Message=hmsg;
FigureHandles.OneUp=hMainOneUp;
FigureHandles.OneDown=hMainOneDown;
FigureHandles.Save=hSend;
FigureHandles.Cancel=hCancel;

% General Las Info
%-------------------
hWellName = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 0.5019 0],...
'Position',[0.0308 0.9543 0.1541 0.0368],...
'String','Well Name',...
'Style','text',...
'Tag','text4');

hWellNameData = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1948 0.9543 0.2983 0.0368],...
'String','',...
'Style','edit',...
'Tag','edit1');

hLocation = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 0.5019 0],...
'Position',[0.0308 0.9087 0.1541 0.0368],...
'String','Location',...
'Style','text',...
'Tag','text5');

hLocationData = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1948 0.9070 0.2983 0.0368],...
'String','',...
'Style','edit',...
'Tag','edit2');

hDepthUnits = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 0.5019 0],...
'Position',[0.0320 0.8666 0.0912 0.0333],...
'String','Depth Units',...
'Style','text',...
'Tag','text6');

hDepthUnitsData = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_general_buttons,...
'Position',[0.1319 0.8596 0.1146 0.0438],...
'String',{ 'Meters' 'Feet' 'Seconds' },...
'Style','popupmenu','userdata',[1],...
'Value',1,...
'Tag','popupmenu1');

hNullValue= uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 0.5019 0],...
'Position',[0.0320 0.8210 0.09129 0.0333],...
'String','Null Value',...
'Style','text',...
'Tag','text7');

hNullValueData = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1331 0.8210 0.0912 0.0333],...
'String','-999.25',...
'Style','edit',...
'Tag','text8');

hDensity = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 0.5019 0],...
'Position',[0.2577 0.8649 0.0912 0.0333],...
'String','Density Units',...
'Style','text',...
'Tag','text30');

hDensityUnits = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_general_buttons,...
'Position',[0.3551 0.8578 0.1146 0.0438],...
'String',{ 'RHOB' 'RHGF' 'RHGA' 'PHID' 'DPHI' 'DPSS' },...
'Style','popupmenu','userdata',[2],...
'Value',1,...
'Tag','popupmenu2');

GeneralData.Name=hWellNameData;
GeneralData.Location=hLocationData;
GeneralData.DepthUnits=hDepthUnitsData;
GeneralData.NullValue=hNullValueData;
GeneralData.DensityUnits=hDensityUnits;

% SendData is to identify what data is being saved or exported, this also
% identifies the headers of the block data.  Expansion here to identify new
% data to be exported
GeneralData.SendData={'Depth' 'Vp' 'Vs' 'Density'};

% This data is telling LASCREATOR what the files are goign to be when
% saving to the las header.  The blank values show that the export is
% dependant on a unit that is controled with either presently in general
% data or somehwere else inthe future
GeneralData.LASHeaders={'VP' 'VS' []}; 

% MiscPull is the field names of data that is included in the exported las
% file but not intergral to the LASCREATOR.  This data can be accessed
% using the contextmenu of the figure.  There are 5 sections described
% belew that are integral
FieldNames={'Company' 'Field' 'Service' 'Country' 'ZStep' 'KB' 'UWID'};
MiscPull.FieldNames=FieldNames;
% The Field names in GeneralData where data is going ot be stored
defaultdat={'Unspecified Company' 'Unspecified Field' 'Unspecified Service Company',...
        'Unspecified Country' {'1' '1' '0.01'} '0' '1000001'};
MiscPull.Default=defaultdat;
% Defailt data for when all data is reset by user.  Any data that changes
% due to unit changes (depth) needs to have a number of default values
% equal to the number of different depth units.  Only 3 at this point
MiscPull.Questions={'Company' 'Field' 'Service Company' 'Country' 'Depth Step (numeric)',...
        'Height of Kelly Bushing (numeric)' 'Unique Well ID'};
% Questions to be asked when changing values for askthingsle
MiscPull.AskFlags=[0 0 0 0 1 1 0];
% Flags for askthingsle, see askthingsle for details
MiscPull.Flags=[0 0 0 0 2 2 0];
% Flags for checking data user has finialzed after askthingle has kicked
% in.  0 - Optional Anything 1 - NOT Optional Anything 2 - NOT OPT Number
MiscPull.LasID={'cm' 'fld' 'srv' 'ctr' 'zstep' 'kb' 'uwid'};
% las IDs, this is higly important because they have to match the ones
% recognized by 
GeneralData.MiscPull=MiscPull;
% adding to these will allow for more properties to edited.
% the following is creating GeneralData Info
for ii=1:size(FieldNames,2)
    dat=defaultdat{ii};
    if(iscell(dat))
        % this is getting the default unit value
        dat=dat{1};
    end
    GeneralData=setfield(GeneralData,FieldNames{ii},dat);
end
% generalize col
col=[0.8 0.8 0.8];

%----------------------------
%---------- BLOCK1 ----------
%----------------------------
%
% Block Uicontrol
%-----------------
cmenu=uicontextmenu;
m1=uimenu(cmenu,'label','Block 1');
m1=uimenu(cmenu,'label','Shift Data:','separator','on');
m2=uimenu(m1,'label','Up','callback',@lascreator_block_controls,'userdata',[1]);
m2=uimenu(m1,'label','Down','callback',@lascreator_block_controls,'userdata',[1]);
% m2=uimenu(m1,'label','Choose','callback',@lascreator_block_controls,'userdata',[1]);
m1=uimenu(cmenu,'label','Clear Layer','callback',@lascreator_block_controls,'userdata',[1]);
m1=uimenu(cmenu,'label','Delete Layer','callback',@lascreator_block_controls,'userdata',[1]);
m1=uimenu(cmenu,'label','Insert Layer:');
m2=uimenu(m1,'label','Above','callback',@lascreator_block_controls,'userdata',[1]);
m2=uimenu(m1,'label','Below','callback',@lascreator_block_controls,'userdata',[1]);

% Block Buttons
%---------------
hbacking = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.5019 0.5019 0.5019],...
'Position',[0.0123 0.6385 0.385 0.1736],...
'String',{ '' },'tooltipstring','Use mouse button two to activate block buttons.',...
'Style','frame',...
'userdata',[1],'uicontextmenu',cmenu);

hslide = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_layer_slider,...
'Position',[0.3785 0.6403 0.0135 0.1701],...
'String',{ '' },...
'Style','slider',...
'Tag','slider2',...
'UserData',1,...
'Visible','off','uicontextmenu',cmenu);

BlockButtons1.Slide=hslide;
BlockButtons1.Backing=hbacking;

% Block Handles
%---------------
hLayerNumber = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.01602 0.7719 0.0690 0.0333],...
'String','Layer: 1',...
'Style','text',...
'Tag','edit5','uicontextmenu',cmenu);

hLayerName = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.0900 0.7719 0.0850 0.0350],...
'String','',...
'Style','edit',...
'Tag','text1','uicontextmenu',cmenu);

hQuickInfo = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.0147 0.6421 0.1639 0.1245],...
'String',{ '0-1000 Meters' 'Vp:       Vs:' 'Density:' },...
'Style','text',...
'Tag','text2','uicontextmenu',cmenu);

hheader1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.7649 0.0924 0.0315],...
'String','Depth',...
'Style','text',...
'Tag','text16','uicontextmenu',cmenu);

hdata1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.7280 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','text20','uicontextmenu',cmenu);

hheader2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.6912 0.0924 0.0315],...
'String','Vp',...
'Style','text',...
'Tag','text25','uicontextmenu',cmenu);

hdata2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.6543 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','text24','uicontextmenu',cmenu);

hheader3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.7649 0.0924 0.0315],...
'String','Vs',...
'Style','text',...
'Tag','text23','uicontextmenu',cmenu);

hdata3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.72807 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','text26','uicontextmenu',cmenu);

hheader4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.6912 0.0924 0.0315],...
'String','Density',...
'Style','text',...
'Tag','text27','uicontextmenu',cmenu);

hdata4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.65431 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','text28','uicontextmenu',cmenu);

BlockHandle1.Number=hLayerNumber;
BlockHandle1.Name=hLayerName;
BlockHandle1.Info=hQuickInfo;
BlockHandle1.Header1=hheader1;
BlockHandle1.Data1=hdata1;
BlockHandle1.Header2=hheader2;
BlockHandle1.Data2=hdata2;
BlockHandle1.Header3=hheader3;
BlockHandle1.Data3=hdata3;
BlockHandle1.Header4=hheader4;
BlockHandle1.Data4=hdata4;
BlockHandle1.Backing=hbacking;

%----------------------------
%---------- BLOCK2 ----------
%----------------------------
%
% Block Uicontrol
%-----------------
cmenu=uicontextmenu;
m1=uimenu(cmenu,'label','Block 2');
m1=uimenu(cmenu,'label','Shift Data:','separator','on');
m2=uimenu(m1,'label','Up','callback',@lascreator_block_controls,'userdata',[2]);
m2=uimenu(m1,'label','Down','callback',@lascreator_block_controls,'userdata',[2]);
% m2=uimenu(m1,'label','Choose','callback',@lascreator_block_controls,'userdata',[2]);
m1=uimenu(cmenu,'label','Clear Layer','callback',@lascreator_block_controls,'userdata',[2]);
m1=uimenu(cmenu,'label','Delete Layer','callback',@lascreator_block_controls,'userdata',[2]);
m1=uimenu(cmenu,'label','Insert Layer:');
m2=uimenu(m1,'label','Above','callback',@lascreator_block_controls,'userdata',[2]);
m2=uimenu(m1,'label','Below','callback',@lascreator_block_controls,'userdata',[2]);

% Block Buttons
%---------------
hbacking = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.5019 0.5019 0.5019],...
'Position',[0.0123 0.4561 0.385 0.1736],...
'String',{ '' },'tooltipstring','Use mouse button two to activate block buttons.',...
'Style','frame',...
'userdata',[2],'uicontextmenu',cmenu);

hslide = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_layer_slider,...
'Position',[0.3785 0.4578 0.0135 0.1701],...
'String',{ '' },...
'Style','slider',...
'Tag','slider3',...
'UserData',2,...
'Visible','off','uicontextmenu',cmenu);

BlockButtons2.Slide=hslide;
BlockButtons2.Backing=hbacking;


% Block Handles
%---------------
hLayerNumber = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.0160 0.5894 0.0690 0.0333],...
'String','Layer: 2',...
'Style','text',...
'Tag','text36',...
'UserData',zeros(1,0),'uicontextmenu',cmenu);

hLayerName = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.0900 0.5894 0.0850 0.0350],...
'String','',...
'Style','edit',...
'Tag','edit6','uicontextmenu',cmenu);

hQuickInfo = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.0147 0.4596 0.1639 0.1245],...
'String',{ 'Meters' 'Vp:       Vs:' 'Density:' },...
'Style','text',...
'Tag','text31','uicontextmenu',cmenu);

hheader1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.5824 0.0924 0.0315],...
'String','Depth',...
'Style','text',...
'Tag','text32','uicontextmenu',cmenu);

hdata1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.5456 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit7','uicontextmenu',cmenu);

hheader2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.5087 0.0924 0.0315],...
'String','Vp',...
'Style','text',...
'Tag','text33','uicontextmenu',cmenu);

hdata2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.4719 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit8','uicontextmenu',cmenu);

hheader3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.5824 0.0924 0.0315],...
'String','Vs',...
'Style','text',...
'Tag','text34','uicontextmenu',cmenu);

hdata3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.5456 0.09247 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit9','uicontextmenu',cmenu);

hheader4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.5087 0.0924 0.0315],...
'String','Density',...
'Style','text',...
'Tag','text35','uicontextmenu',cmenu);

hdata4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.47192 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit10','uicontextmenu',cmenu);

BlockHandle2.Number=hLayerNumber;
BlockHandle2.Name=hLayerName;
BlockHandle2.Info=hQuickInfo;
BlockHandle2.Header1=hheader1;
BlockHandle2.Data1=hdata1;
BlockHandle2.Header2=hheader2;
BlockHandle2.Data2=hdata2;
BlockHandle2.Header3=hheader3;
BlockHandle2.Data3=hdata3;
BlockHandle2.Header4=hheader4;
BlockHandle2.Data4=hdata4;
BlockHandle2.Backing=hbacking;

%----------------------------
%---------- BLOCK3 ----------
%----------------------------
%
% Block Uicontrol
%-----------------
cmenu=uicontextmenu;
m1=uimenu(cmenu,'label','Block 3');
m1=uimenu(cmenu,'label','Shift Data:','separator','on');
m2=uimenu(m1,'label','Up','callback',@lascreator_block_controls,'userdata',[3]);
m2=uimenu(m1,'label','Down','callback',@lascreator_block_controls,'userdata',[3]);
% m2=uimenu(m1,'label','Choose','callback',@lascreator_block_controls,'userdata',[3]);
m1=uimenu(cmenu,'label','Clear Layer','callback',@lascreator_block_controls,'userdata',[3]);
m1=uimenu(cmenu,'label','Delete Layer','callback',@lascreator_block_controls,'userdata',[3]);
m1=uimenu(cmenu,'label','Insert Layer:');
m2=uimenu(m1,'label','Above','callback',@lascreator_block_controls,'userdata',[3]);
m2=uimenu(m1,'label','Below','callback',@lascreator_block_controls,'userdata',[3]);

% Block Buttons
%---------------
hbacking = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.5019 0.5019 0.5019],...
'Position',[0.0123 0.2736 0.385 0.1736],...
'String',{ '' },'tooltipstring','Use mouse button two to activate block buttons.',...
'Style','frame',...
'userdata',[3],'uicontextmenu',cmenu);

hslider = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_layer_slider,...
'Position',[0.3785 0.2754 0.0135 0.1701],...
'Style','slider',...
'Tag','slider4',...
'UserData',3,...
'Visible','off','uicontextmenu',cmenu);

BlockButtons3.Slide=hslide;
BlockButtons3.Backing=hbacking;

% Block Handles
%---------------
hLayerNumber = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.01602 0.4070 0.0690 0.03333],...
'String','Layer: 3',...
'Style','text',...
'Tag','text42','uicontextmenu',cmenu);

hLayerName = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.0900 0.4070 0.0850 0.0350],...
'String','',...
'Style','edit',...
'Tag','edit11','uicontextmenu',cmenu);

hQuickInfo = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.0147 0.2771 0.1639 0.1245],...
'String',{ 'Meters' 'Vp:       Vs:' 'Density:' },...
'Style','text',...
'Tag','text37','uicontextmenu',cmenu);

hheader1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.4 0.0924 0.0315],...
'String','Depth',...
'Style','text',...
'Tag','text38','uicontextmenu',cmenu);

hdata1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.36315 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit12','uicontextmenu',cmenu);

hheader2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.3263 0.0924 0.0315],...
'String','Vp',...
'Style','text',...
'Tag','text39','uicontextmenu',cmenu);

hdata2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.2894 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit13','uicontextmenu',cmenu);

hheader3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.4 0.0924 0.0315],...
'String','Vs',...
'Style','text',...
'Tag','text40','uicontextmenu',cmenu);

hdata3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.3631 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit14','uicontextmenu',cmenu);

hheader4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.3263 0.0924 0.0315],...
'String','Density',...
'Style','text',...
'Tag','text41','uicontextmenu',cmenu);

hdata4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.2894 0.09247 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit15','uicontextmenu',cmenu);

BlockHandle3.Number=hLayerNumber;
BlockHandle3.Name=hLayerName;
BlockHandle3.Info=hQuickInfo;
BlockHandle3.Header1=hheader1;
BlockHandle3.Data1=hdata1;
BlockHandle3.Header2=hheader2;
BlockHandle3.Data2=hdata2;
BlockHandle3.Header3=hheader3;
BlockHandle3.Data3=hdata3;
BlockHandle3.Header4=hheader4;
BlockHandle3.Data4=hdata4;
BlockHandle3.Backing=hbacking;

%----------------------------
%---------- BLOCK4 ----------
%----------------------------
%
% Block Uicontrol
%-----------------
cmenu=uicontextmenu;
m1=uimenu(cmenu,'label','Block 4');
m1=uimenu(cmenu,'label','Shift Data:','separator','on');
m2=uimenu(m1,'label','Up','callback',@lascreator_block_controls,'userdata',[4]);
m2=uimenu(m1,'label','Down','callback',@lascreator_block_controls,'userdata',[4]);
% m2=uimenu(m1,'label','Choose','callback',@lascreator_block_controls,'userdata',[4]);
m1=uimenu(cmenu,'label','Clear Layer','callback',@lascreator_block_controls,'userdata',[4]);
m1=uimenu(cmenu,'label','Delete Layer','callback',@lascreator_block_controls,'userdata',[4]);
m1=uimenu(cmenu,'label','Insert Layer:');
m2=uimenu(m1,'label','Above','callback',@lascreator_block_controls,'userdata',[4]);
m2=uimenu(m1,'label','Below','callback',@lascreator_block_controls,'userdata',[4]);

% Block Buttons
%---------------
hbacking = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.5019 0.5019 0.5019],...
'Position',[0.0123 0.0912 0.385 0.1736],...
'String',{ '' },'tooltipstring','Use mouse button two to activate block buttons.',...
'Style','frame',...
'userdata',[4],'uicontextmenu',cmenu);

hslide = uicontrol(...
'Units','normalized',...
'Callback',@lascreator_layer_slider,...
'Position',[0.3785 0.0929 0.0135 0.1701],...
'String',{ '' },...
'Style','slider',...
'Tag','slider7',...
'UserData',4,...
'Visible','off','uicontextmenu',cmenu);

BlockButtons4.Slide=hslide;
BlockButtons4.Backing=hbacking;

% Block Handles
%---------------
hLayerNumber = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.0160 0.2245 0.0690 0.0333],...
'String','Layer: 4',...
'Style','text',...
'Tag','text60','uicontextmenu',cmenu);

hLayerName = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.0900 0.2245 0.0850 0.0350],...
'String','',...
'Style','edit',...
'Tag','edit26','uicontextmenu',cmenu);

hQuickInfo = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.0147 0.0947 0.1639 0.1245],...
'String',{ 'Meters' 'Vp:       Vs:' 'Density:' },...
'Style','text',...
'Tag','text55','uicontextmenu',cmenu);

hheader1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.2175 0.0924 0.0315],...
'String','Depth',...
'Style','text',...
'Tag','text56','uicontextmenu',cmenu);

hdata1 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.1807 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit27','uicontextmenu',cmenu);

hheader2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.1849 0.1438 0.0924 0.0315],...
'String','Vp',...
'Style','text',...
'Tag','text57','uicontextmenu',cmenu);

hdata2 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.1849 0.1070 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit28','uicontextmenu',cmenu);

hheader3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.2175 0.0924 0.0315],...
'String','Vs',...
'Style','text',...
'Tag','text58','uicontextmenu',cmenu);

hdata3 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.1807 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit29','uicontextmenu',cmenu);

hheader4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[0.8 0.8 0.8],...
'Position',[0.2799 0.1438 0.0924 0.0315],...
'String','Density',...
'Style','text',...
'Tag','text59','uicontextmenu',cmenu);

hdata4 = uicontrol(...
'Units','normalized',...
'BackgroundColor',[1 1 1],...
'Position',[0.2799 0.1070 0.0924 0.0315],...
'String','',...
'Style','edit',...
'Tag','edit30','uicontextmenu',cmenu);

BlockHandle4.Number=hLayerNumber;
BlockHandle4.Name=hLayerName;
BlockHandle4.Info=hQuickInfo;
BlockHandle4.Header1=hheader1;
BlockHandle4.Data1=hdata1;
BlockHandle4.Header2=hheader2;
BlockHandle4.Data2=hdata2;
BlockHandle4.Header3=hheader3;
BlockHandle4.Data3=hdata3;
BlockHandle4.Header4=hheader4;
BlockHandle4.Data4=hdata4;
BlockHandle4.Backing=hbacking;

BlockButtons.Block1=BlockButtons1;
BlockButtons.Block2=BlockButtons2;
BlockButtons.Block3=BlockButtons3;
BlockButtons.Block4=BlockButtons4;

BlockHandles.Block1=BlockHandle1;
BlockHandles.Block2=BlockHandle2;
BlockHandles.Block3=BlockHandle3;
BlockHandles.Block4=BlockHandle4;

% creating structure for data
% layer zero is a template for all subsuquent layers and for future layers
defaultlayersource={'LayerNumber' 'LayerName' 'Depth' 'DepthUnit' 'Vp' 'VpUnit',...
        'Vs' 'VsUnit' 'Density' 'DensityUnit'};
defaultlayerdata={0 [] [] [] [] [] [] [] [] [] };
Layer0=[];
for ii=1:length(defaultlayersource)
    % this is creating the default layer zero
    Layer0=setfield(Layer0,defaultlayersource{ii},defaultlayerdata{ii});
end
LayerData=[];
for ii=1:5
    % this is putting Layers into LayerData, with Layer0 at the start
    newlayername=['Layer' num2str(ii-1)];
    Layer0=setfield(Layer0,'LayerNumber',[ii-1]);
    LayerData=setfield(LayerData,['Layer' num2str(ii-1)],Layer0);
end
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;

set(gcf,'userdata',AllInfo);

%--------------------------------
%----- UNIVERSAL DATA CHECK -----
%--------------------------------
%
% this routine checks data that is in the blocks to make sure user has not
% placed characters where numbers should be.  It will also stop the cycling
% of the blocks if a depth has not been added to a block where it should
% have a depth.  Only checks data that is visibly located in the figure

function controlout=lascreator_datacheck(hObject, eventdata, handles)
controlout={'Please ensure there is at least one depth at layer 1' -1};
%
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
% -----------

nuldat=getfield(GeneralData,'NullValue');
nullval=get(nuldat,'string');
% checking nullval
checknul=str2num(nullval);
if(isempty(checknul))
    % not a number buddy!
    nullval='-999.25';
end
welldat=getfield(GeneralData,'Name');
wellname=get(welldat,'string');
% Checking name to see if testing.
if(findstr(wellname,'LASTEST:'))
    lascreator_master_reset;
    checkwellname=get(welldat,'string');
    if(findstr(checkwellname,'LASTEST:'))
        % has not been reset, must stop
        return
    end
    rmov=findstr(wellname,':');
    num=str2num(wellname(rmov+1:end));
    if(isnumeric(num))
        if(num<1)
            num=2;
        end
        % Making a fake well log based on number input by user
        testmat=[];
        rot1={'Depth'    'Vp'    'Vs'    'Density'};
        % reseting data
        defaultlayersource={'LayerNumber' 'LayerName' 'Depth' 'DepthUnit' 'Vp' 'VpUnit',...
                'Vs' 'VsUnit' 'Density' 'DensityUnit'};
        defaultlayerdata={0 [] [] [] [] [] [] [] [] [] };
        Layer0=[];
        for ii=1:length(defaultlayersource)
            % this is creating the default layer zero
            Layer0=setfield(Layer0,defaultlayersource{ii},defaultlayerdata{ii});
        end
        LayerData=[];
        for ii=1:5
            % this is putting Layers into LayerData, with Layer0 at the start
            newlayername=['Layer' num2str(ii-1)];
            Layer0=setfield(Layer0,'LayerNumber',[ii-1]);
            LayerData=setfield(LayerData,['Layer' num2str(ii-1)],Layer0);
        end
        for ii=1:abs(round(num))
            testnums=100*ii;
            try
                lay=getfield(LayerData,['Layer' num2str(ii)]);
            catch 
                % need to create a new layer
                layerdataget=getfield(LayerData,'Layer0');
                layerdataget=setfield(layerdataget,'LayerNumber',[ii]);
                LayerData=setfield(LayerData,['Layer' num2str(ii)],layerdataget);
            end
            for jj=1:size(rot1,2)
                LayerData=setfield(LayerData,['Layer' num2str(ii)],rot1{jj},testnums);
                if(ii==1|ii==2|ii==3|ii==4)
                    % Setting the block info
                    blocknumerx=getfield(BlockHandles,['Block' num2str(ii)]);
                    for kk=1:4
                        set(getfield(blocknumerx,['Data' num2str(kk)]),'string',num2str(testnums)); 
                    end
                end
            end
        end
        % Must now chance test name slightly to stop major problems
        set(welldat,'string',['Testing: ' num2str(ii) ' Layers']);
    else
    end
end
hdepthunit=getfield(GeneralData,'DepthUnits');
dat=get(hdepthunit,'string');
val=get(hdepthunit,'value');
depthunit=dat(val,:);
zstep=getfield(GeneralData,'ZStep');
Block1=getfield(BlockHandles,'Block1');
LayerNumber=getfield(Block1,'Number');
nm=get(LayerNumber,'string');
% nm has -1 taken from it for looping purposes later on
nm=str2num(nm(findstr(' ',nm)+1:end))-1;

% the following may eventually be part of block data
hDepthUnit=getfield(GeneralData,'DepthUnits');
hDensityUnit=getfield(GeneralData,'DensityUnits');
depunit1=get(hDepthUnit,'string');
denunit1=get(hDensityUnit,'string');
alldep=[];
alldat=cell(1,4);
checkdep=[];    % as long as checkdep is empty, different colours 
                % will be attempted to be created for blocks
                % No longer blank means that there is an empty 
                % depth, which means subsuqent layers are ignored
for ii=1:4
    % eventually, the block slider will have to be referenced when
    % lascreator has expanded to allow more options
    % getting blocks
    blocknumerx=getfield(BlockHandles,['Block' num2str(ii)]);
    hLayerNumber=getfield(blocknumerx,'Number');
    hLayerName=getfield(blocknumerx,'Name');
    hQuickInfo=getfield(blocknumerx,'Info');
    hheader1=getfield(blocknumerx,'Header1');
    hdata1=getfield(blocknumerx,'Data1');
    hheader2=getfield(blocknumerx,'Header2');
    hdata2=getfield(blocknumerx,'Data2');
    hheader3=getfield(blocknumerx,'Header3');
    hdata3=getfield(blocknumerx,'Data3');
    hheader4=getfield(blocknumerx,'Header4');
    hdata4=getfield(blocknumerx,'Data4');
    % making groups of data
    hdatas=[hdata1 hdata2 hdata3 hdata4];
    hheaders=[hheader1 hheader2 hheader3 hheader4];
    % getting depth and density units
    hdepunit=getfield(GeneralData,'DepthUnits');
    dat=get(hdepunit,'string');
    val=get(hdepunit,'value');
    depunit=dat(val,:);
    hdenunit=getfield(GeneralData,'DensityUnits');
    dat=get(hdenunit,'string');
    val=get(hdenunit,'value');
    denunit=dat(val,:);
    % getting names of data to export, this whole section should be rewiten
    % to work regardless of how many inputs the user has added
    setdat=getfield(GeneralData,'SendData');
    holdat={'dep' 'Vp' 'Vs' 'den'};
    dep=get(hdatas(1),'string');
    Vp=get(hdatas(2),'string');
    Vs=get(hdatas(3),'string');
    den=get(hdatas(4),'string');
    for jj=1:4
        holdat{jj}=get(hdatas(jj),'string');
    end
    if(dep==str2num(nullval)|~isempty(checkdep))
        col1=0.8;
        col2=0.8;
        col3=0.8;
        checkdep='STOP';
    else
        try
            % this section is creating colours for different depths
            swdat1=sort([str2num(Vp) str2num(dep)]);
            col1=swdat1(1)/swdat1(2);
            col2=1;
            swdat1=sort([str2num(Vp) str2num(Vs)]);
            col3=swdat1(1)/swdat1(2);
            checkcol=[col1 col3];
            for kk=1:2
                if(isinf(checkcol(kk))|isnan(checkcol(kk)))
                    col1=0.8;
                    col2=0.8;
                    col3=0.8;
                    break
                end  
            end
        catch
            col1=0.8;
            col2=0.8;
            col3=0.8;
        end
    end
    col=[col1 col2 col3];
    for jj=1:4
        dat=get(hdatas(jj),'string');
        dat=str2num(dat);
        if(isempty(dat))
            % making sure that data boxes are empty
            set(hdatas(jj),'string','');
            dat=[];   
        end
        % making sure data boxes are white
        set(hdatas(jj),'backgroundcolor',[1 1 1]);
        % making sure header boxes are right colour
        set(hheaders(jj),'backgroundcolor',col);
        layerdataset=['Layer' num2str(nm+ii)];
        LayerData=setfield(LayerData,layerdataset,setdat{jj},dat);
        alldat{jj}=dat;
    end
    x1=[num2str(alldat{1}) '  ' depthunit{1}];
    x2=['Vp: ' num2str(alldat{2}) '  Vs: ' num2str(alldat{3})];
    x3=['Density: ' num2str(alldat{4})];
    set(hQuickInfo,'string',{x1;x2;x3},'backgroundcolor',col); 
    set(hLayerNumber,'backgroundcolor',col);
    nam=deblank(get(hLayerName,'string'));
    % checking for special characters
    checkchar={'[' ']' '(' ')' '{' '}' '=' '.'  ',' ';' '%' '!',...
            '+' '-' '*' '/' '\' '^' '<' '>' '=' '~' '&' '|' '@',...
            '#' '$' '`' ':' '"'};
    for kk=1:length(checkchar)
        if(~isempty(findstr(checkchar{kk},nam)))
            stringinfo=['Can Not Use "' checkchar{kk} '" In Name'];
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
            return
        end
    end
    layerdataget=getfield(LayerData,['Layer' num2str(nm+ii)]);
    if(~isempty(nam))
        layerdataget=setfield(layerdataget,'LayerName',nam);
    else
        layerdataget=setfield(layerdataget,'LayerName',[]);
    end
    LayerData=setfield(LayerData,['Layer' num2str(nm+ii)],layerdataget);
    % getting depths for checking consitancy
    dep=getfield(LayerData,['Layer' num2str(nm+ii)],'Depth');
    if(isempty(dep))
        adep=str2num(nullval);
    else
        adep=dep;
    end
    alldep=[alldep;adep];
    LayerData=setfield(LayerData,['Layer' num2str(nm+ii)],'DepthUnit',depunit{1});
    LayerData=setfield(LayerData,['Layer' num2str(nm+ii)],'DensityUnit',denunit{1});
end
alldep2=alldep;
for ii=1:3
    % Checking to see if scrolling should allowed, depths have to increase
    % as layer number increase.
    num1=alldep(ii);    num2=alldep(ii+1);
    if(num1==str2num(nullval)|num2==str2num(nullval))
        if(num1==str2num(nullval))
            if(ii==3)
                jj=ii;
            else
                jj=1;
            end
        else
            jj=ii+1;
        end
        controlout={['Layer: ' num2str(nm+ii+1) ' needs a depth or be deleted before scrolling can continue'] jj};
        break
    end
    if(num1>=num2)
        if(ii==3)
            jj=ii;
        else
            jj=1;
        end
        controlout={['Layer: ' num2str(nm+ii+1) ' needs to be deeper then than Layer: ' num2str(nm+ii)] jj};
        break
    end
end
if(alldep(1)==nullval&controlout{2}==0)
    controlout={['Please insert data into blocks before scrolling layers'] [-1]};
end
% Saving data
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;
set(gcf,'userdata',AllInfo);
% ----------- 
% plotting da junk
lascreator_plot;

%-------------------------------
%----- UNIVERSAL DATA PULL -----
%-------------------------------
% 
% This routine pulls important data out of the various structures and
% exports them in an easy to use structure
% Pulling data out

function datapull=lascreator_datapull(hObject, eventdata, handles)
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
haxes=getfield(FigureHandles,'Axes');
% -----------
% General Data
welldat=getfield(GeneralData,'Name');
wellname=get(welldat,'string');
locdat=getfield(GeneralData,'Location');
lc=get(locdat,'string');
nuldat=getfield(GeneralData,'NullValue');
nullval=get(nuldat,'string');
unitdat=getfield(GeneralData,'DepthUnits');
val=get(unitdat,'value');
units=get(unitdat,'string');
units=units(1,1);
den=getfield(GeneralData,'DensityUnits');
val=get(den,'value');
den=get(den,'string');
den=den(val,1);
lg=getfield(GeneralData,'LASHeaders');
lg{3}=den{1};
% getting names of fields that are going to be exported.  Eventually rot1
% will be more organized than it presently is, this new organization will
% go hand in hand with lascreators future ability to add more log
% properties.
rot1=getfield(GeneralData,'SendData');
topnames={};
ztops=[];
holdat1=[];
holdat2=[];
holdat3=[];
holdat4=[];
rot2={holdat1 holdat2 holdat3 holdat4};
stploop=[];
kk=1;
for ii=1:size(fieldnames(LayerData),1)-1 % since layers start at 0
    layerdataget=['Layer' num2str(ii)];
    lay=getfield(LayerData,layerdataget);
    for jj=1:size(rot1,2)
        laydat=getfield(lay,rot1{jj});
        if(isempty(laydat)&jj==1)
            % stopping data aquisition on first blank depth
            stploop='stop';
            break
        end
        if(isempty(laydat))
            laydat=str2num(nullval);  % nullval
        end
        rot2{jj}=[rot2{jj};laydat];
    end 
    if(isempty(stploop))
        tpn=getfield(lay,'LayerName');  % aquiring tops names  
        dep=getfield(lay,'Depth');      % acquiring tops depths
        if(isempty(tpn))  
        else
            topnames{kk}=tpn;
            ztops=[ztops;dep];
            kk=kk+1;
        end
    else
        break
    end
end
allwelllogdata=rot2;
datapull={allwelllogdata topnames ztops};   % sending data out

%--------------------------
%----- PLOT WELL LOGS -----
%--------------------------
%
% this routine will plot the well log data on the axes.  Three plots will
% only be seen at once, eventually when more data is added, plots will be
% cycled through.

function lascreator_plot(hObject, eventdata, handles)
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
haxes=getfield(FigureHandles,'Axes');
% -----------
Block1=getfield(BlockHandles,'Block1');
LayerNumber=getfield(Block1,'Number');
nm=get(LayerNumber,'string');
nm=str2num(nm(findstr(' ',nm)+1:end));
nuldat=getfield(GeneralData,'NullValue');
nullval=get(nuldat,'string');
den=getfield(GeneralData,'DensityUnits');
val=get(den,'value');
den=get(den,'string');
den=den(val,1);
dep=getfield(GeneralData,'DepthUnits');
val=get(dep,'value');
dep=get(dep,'string');
dep=dep(val,1);
lasheader=getfield(GeneralData,'LASHeaders');
lasheader{3}=den{1};
% using data pull to aquire data for plotting purposes
datapull=lascreator_datapull;
rot2=datapull{1};   % array of log data, depths is first cell
topnames=datapull{2};
ztops=datapull{3};

alldep=rot2{1};
if(isempty(rot2{2}))
else
    % setting y axis
    cla;
    % creaing a patch to show users where exactly they are on the axis as
    % they cycle through the layers
    alldep=[rot2{1}];
    nm=[nm:1:nm+4];
    nm=nm(find(size(alldep,1)>=nm));
    try
        ydat=[alldep(nm(1)) alldep(nm(end)) alldep(nm(end)) alldep(nm(1))];
    catch
        % ydat=[alldep(nm(1)) alldep(nm(end-1)) alldep(nm(end-1)) alldep(nm(1))];
        % something doesn't work sometimes with the above... so were going
        % with the following
        ydat=[0 0 0 0];
    end
    patch('xdata',[-.25 -.25 3.25 3.25],'ydata',ydat,...
            'facecolor',[.95 .95 .95],'edgecolor','none');
    alldep2=zeros(2*size(alldep,1),1);
    for ii=1:size(alldep,1)
        % this is going to allow for bar graph
        alldep2(2*ii-1)=alldep(ii);
        alldep2(2*ii)=alldep(ii);
    end
    cols={'r-' 'm-' 'g-'};
    for ii=2:size(rot2,2)
        % the following line is for reference purposes, they are being
        % plotted first so information lines are on top
        line([ii-2 ii-2],[alldep2(1) alldep2(end)],'color',[.8 .8 .8],'buttondownfcn',@lascreator_well_lines);
    end
    for ii=2:size(rot2,2)
        dat=rot2{ii};
        if(dat(1)==str2num(nullval))
            dat=dat.*0;
        end
        mx=max(abs(dat));   % shouldn't need to do the abs... but what the hell
        if(mx==0)
        else
            dat=dat/mx;
        end
        dat=dat+ii-2;
        dat2=zeros(2*size(dat,1),1);
        for jj=1:size(dat,1)
            % this is going to allow for bar graph
            dat2(2*jj-1)=dat(jj);
            dat2(2*jj)=dat(jj);
        end
        % the following line shows the normalized values
        welline=plot([dat2],alldep2,cols{ii-1});
        set(welline,'buttondownfcn',@lascreator_well_lines);
    end
    if(alldep(1)==alldep(end))
        % ploting will not occure
    else
        set(haxes,'ylim',[alldep(1) alldep(end)],'ytick',[alldep(1):alldep(end)/20:alldep(end)],...
            'xlim',[-.25 3.25],'xtick',[0:1:3]);
    end
    title('Well Log');
    ylabel(['Depth (' dep{1} ')']);
    % setting tops lines
    xxlim=get(gca,'xlim');
    for ii=1:size(topnames,2)
        % plotting tops with names and depths
        hhtext1(ii)=text(xxlim(1),ztops(ii),['*' topnames{ii} '*'],...	%text on right
            'verticalalignment','baseline',...
            'horizontalalignment','left',...
            'color',[1 .6 0],'fontsize',8);
        lbl=sprintf('%2.3f',ztops(ii));
        hhtext2(ii)=text(xxlim(2),ztops(ii),lbl,...	%Positon 
            'verticalalignment','baseline',...		%One day left
            'horizontalalignment','right',...
            'color',[1 .6 0],'fontsize',8);
        hrl=line(xxlim,[ztops(ii) ztops(ii)],'color',[1 .6 0],...
            'linewidth',[1],'userdata','',...
            'buttondownfcn',@lascreator_well_lines,'linestyle','-.');
    end
    for ii=2:size(rot2,2)
        % text to identify what is being plotted
        newtext(ii)=text([ii-2],[alldep2(1)],lasheader{ii-1},...	%text on right
            'verticalalignment','top',...
            'horizontalalignment','left',...
            'color',[0 0 0],'fontsize',8);
    end
end

%------------------------------
%----- MASTER BLOCK SHIFT -----
%------------------------------
%
% This routine allows for the cycling through of layer data.  New layers
% are created if data is shifted down with no layer data present.

function lascreator_master_move(hObject, eventdata, handles)
% checking data
controlout=lascreator_datacheck;
% -------------
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
hMainOneUp=getfield(FigureHandles,'OneUp');
hMainOneDown=getfield(FigureHandles,'OneDown');
% -----------
nuldat=getfield(GeneralData,'NullValue');
nullval=get(nuldat,'string');
if(controlout{2}==0)
    stringinfo=controlout{1};
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
    lascreator_plot;
    return
end
hdepthunit=getfield(GeneralData,'DepthUnits');
dat=get(hdepthunit,'string');
val=get(hdepthunit,'value');
depthunit=dat(val,:);

Block1=getfield(BlockHandles,'Block1');
LayerNumber=getfield(Block1,'Number');
nm=get(LayerNumber,'string');
nm=str2num(nm(findstr(' ',nm)+1:end));
% the following colours are for the backing of the block, a single
% block is only high lighted when layer selected from well log lines
backingcol={[0.5019 0.5019 0.5019] [0.5019 0.5019 0.5019] [0.5019 0.5019 0.5019],...
        [0.5019 0.5019 0.5019]};
if(isempty(gcbo))
    % this fucntion is being called from a well log line click, the data
    % transfered contains tghe layers that will now show up in the blocks
    % and the layer that has been clicked will be high lighted
    backingdat=hObject;
    nm=backingdat{1};
    backingcol{backingdat{2}}=[1 0.5019 0];
    udat={[] nm};
else
    % function is being called through layer controls
    udat=get(gcbo,'userdata');
    whichbut=get(gcbo,'string');
    switch whichbut
        case 'Up'
            if(nm<=2)
                % taking no action
                stringinfo='Already at the top of section';
                set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                if(nm==1)
                    return
                else
                    nm=nm-1;
                end
            else
                dat=get(gcbo,'userdata');
                if(controlout{2}==1)
                    stringinfo=controlout{1};
                    col=[1 1 0];
                    set(hmsg,'string',stringinfo,'backgroundcolor',col);
                    return
                else
                    stringinfo='Scrolling Layers';
                    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                end
                nm=nm-1;
                for ii=nm-1:nm+2
                    layerdataget=getfield(LayerData,['Layer' num2str(ii)]);
                end
            end
            udat{2}=udat{2}-1;
            if(udat{2}==0)
                udat{2}=4;
            end
        case 'Down'
            h=get(gcf,'userdata');
            LayerData=getfield(h,'Data');
            GeneralData=getfield(h,'GeneralData');
            if(controlout{2}==0)
            elseif(controlout{2}==1|controlout{2}==2|controlout{2}==3)
                stringinfo=controlout{1};
                col=[1 1 0];
                set(hmsg,'string',stringinfo,'backgroundcolor',col);
                return
            end
            stringinfo='Scrolling Layers';
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
            nm=nm+1;
            udat{2}=udat{2}+1;
            if(udat{2}>=5)
                udat{2}=1;
            end
        case 'xxx'
        case 'xxx'
    end
end
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
checkdep=[];    % as long as checkdep is empty, different colours 
                % will be attempted to be created for blocks
                % No longer blank means that there is an empty 
                % depth, which means subsuqent layers are ignored
for ii=1:4
    % getting block backings for highlight purposes
    BlockBacking=getfield(BlockButtons,['Block' num2str(ii)]);
    hbacking=getfield(BlockBacking,'Backing');
    set(hbacking,'backgroundcolor',backingcol{ii});
    % getting blocks
    blockst=getfield(BlockHandles,['Block' num2str(ii)]);
    % getting handles
    hLayerNumber=getfield(blockst,'Number');
    hLayerName=getfield(blockst,'Name');
    hQuickInfo=getfield(blockst,'Info');
    hheader1=getfield(blockst,'Header1');
    hdata1=getfield(blockst,'Data1');
    hheader2=getfield(blockst,'Header2');
    hdata2=getfield(blockst,'Data2');
    hheader3=getfield(blockst,'Header3');
    hdata3=getfield(blockst,'Data3');
    hheader4=getfield(blockst,'Header4');
    hdata4=getfield(blockst,'Data4');
    % getting data
    try
        lay=getfield(LayerData,['Layer' num2str(nm+ii-1)]);
    catch 
        % need to create a new layer
        layerdataget=getfield(LayerData,'Layer0');
        layerdataget=setfield(layerdataget,'LayerNumber',[nm+ii-1]);
        LayerData=setfield(LayerData,['Layer' num2str(nm+ii-1)],layerdataget);
        lay=getfield(LayerData,['Layer' num2str(nm+ii-1)]);
    end
    lay=getfield(LayerData,['Layer' num2str(nm+ii-1)]);
    num=getfield(lay,'LayerNumber');
    nam=getfield(lay,'LayerName');
    dep=getfield(lay,'Depth');
    depunit=getfield(lay,'DepthUnit');
    Vp=getfield(lay,'Vp');
    Vpunit=getfield(lay,'VpUnit');
    Vs=getfield(lay,'Vs');
    Vsunit=getfield(lay,'VsUnit');
    den=getfield(lay,'Density');
    denunit=getfield(lay,'DensityUnit');
    %     numrotation={[1 2 3 4] [2 3 4 1] [3 4 1 2] [4 1 2 3]};
    %     numrotation=numrotation{udat{2}};
    %     colrotation={[1 1 0] [1 0.5019 0] [1 1 0.5019] [1 0.5019 0.2509]};
    % the above was for none unique colors
    
    % creating unique colors for each layer
    if(dep==str2num(nullval)|~isempty(checkdep))
        col1=0.8;
        col2=0.8;
        col3=0.8;
        checkdep='STOP';
    else
        try
            % this section is creating colours for different depths
            swdat1=sort([(Vp) (dep)]);
            col1=swdat1(1)/swdat1(2);
            col2=1;
            swdat1=sort([(Vp) (Vs)]);
            col3=swdat1(1)/swdat1(2);
            checkcol=[col1 col3];
            for kk=1:2
                if(isinf(checkcol(kk))|isnan(checkcol(kk)))
                    col1=0.8;
                    col2=0.8;
                    col3=0.8;
                    break
                end  
            end
        catch
            col1=0.8;
            col2=0.8;
            col3=0.8;
        end
    end
    col=[col1 col2 col3];
    set(hLayerNumber,'string',['Layer: ' num2str(nm+ii-1)],'backgroundcolor',col);
    set(hLayerName,'string',nam);
    set(hdata1,'string',num2str(dep));
    set(hdata2,'string',num2str(Vp));
    set(hdata3,'string',num2str(Vs));
    set(hdata4,'string',num2str(den));
    x1=[num2str(dep) '  ' depthunit{1}];
    x2=['Vp: ' num2str(Vp) '  Vs: ' num2str(Vs)];
    x3=['Density: ' num2str(den)];
    set(hQuickInfo,'string',{x1;x2;x3},'backgroundcolor',col); 
    set(hheader1,'backgroundcolor',col);
    set(hheader2,'backgroundcolor',col);
    set(hheader3,'backgroundcolor',col);
    set(hheader4,'backgroundcolor',col);
end
nms=fieldnames(LayerData);
for ii=val:val+3
end
% Saving data
dat=get(hMainOneUp,'userdata');
dat{2}=udat{2};
set(hMainOneUp,'userdata',dat);
dat=get(hMainOneDown,'userdata');
dat{2}=udat{2};
set(hMainOneDown,'userdata',dat);
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;
set(gcf,'userdata',AllInfo);
lascreator_plot;

%------------------------
%----- MASTER RESET -----
%------------------------
%
% This routine is resetting all block data to empty strings as well as
% clearing all stored data.  A question is of course possed before clearing
% all data is done. 

function lascreator_master_reset(hObject, eventdata, handles)
% -------------
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
MiscPull=getfield(GeneralData,'MiscPull');
hmsg=getfield(FigureHandles,'Message');
hMainOneUp=getfield(FigureHandles,'OneUp');
hMainOneDown=getfield(FigureHandles,'OneDown');
% -----------
stringinfo='Clear and reset all layer data?';
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);

welldat=getfield(GeneralData,'Name');
wellname=get(welldat,'string');
% Checking name to see if testing.
if(findstr(wellname,'LASTEST:'))
    addon='LASTEST ';
else
    addon='Reseting data ';
    % checking data
    controlout=lascreator_datacheck;
end
qststring={[addon ' will reset all general well log info'],...
        'as well clear all layer data.',...
        'Do you want to continue?'};
button = questdlg(qststring,'Reset?','Continue','Cancel','Continue');

switch button
    case 'Continue'
    case 'Cancel'
        stringinfo='Action canceled';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
        return
end
LayerData=[];
% creating structure for data
% layer zero is a template for all subsuquent layers and for future layers
defaultlayersource={'LayerNumber' 'LayerName' 'Depth' 'DepthUnit' 'Vp' 'VpUnit',...
        'Vs' 'VsUnit' 'Density' 'DensityUnit'};
defaultlayerdata={0 [] [] [] [] [] [] [] [] [] };
Layer0=[];
for ii=1:length(defaultlayersource)
    % this is creating the default layer zero
    Layer0=setfield(Layer0,defaultlayersource{ii},defaultlayerdata{ii});
end
LayerData=[];
for ii=1:5
    % this is putting Layers into LayerData, with Layer0 at the start
    newlayername=['Layer' num2str(ii-1)];
    Layer0=setfield(Layer0,'LayerNumber',[ii-1]);
    LayerData=setfield(LayerData,['Layer' num2str(ii-1)],Layer0);
end
% reseting general data
rot1={'Name' 'Location' 'DepthUnits' 'NullValue' 'DensityUnits'};
dep={ 'Meters' 'Feet' 'Seconds' };
den={ 'RHOB' 'RHGF' 'RHGA' 'PHID' 'DPHI' 'DPSS' };
rot2={'' '' dep '-999.25' den};
for ii=1:size(rot1,2)
    rot1name=rot1{ii};
    hndle=getfield(GeneralData,rot1name);
    rot2name=rot2{ii};
    set(hndle,'string',rot2{ii},'value',1);
end
fnames=getfield(MiscPull,'FieldNames');
dfault=getfield(MiscPull,'Default');
for ii=1:size(fnames,2)
    df=dfault{ii};
    checkdf=1;
    while checkdf==1
        if(iscell(df))
            df=df{1};
        else
            checkdf=0;
        end
    end
    GeneralData=setfield(GeneralData,fnames{ii},df);
end

% reseting all block handles
for ii=1:4
    % getting blocks
    blockst=getfield(BlockHandles,['Block' num2str(ii)]);
    rot1={'Number' 'Name' 'Info'};
    rot2={'hLayerNumber' 'hLayerName' 'hQuickInfo'};
    x1=['  Meters'];
    x2=['Vp: '  '  Vs: '];
    x3=['Density: '];
    x4={x1 x2 x3};
    lay=['Layer: ' num2str(ii)];
    rot3={lay '' {x1 x2 x3}};
    rot4={[.8 .8 .8] [1 1 1] [.8 .8 .8]};
    for jj=1:size(rot1,2)
        rotdataset=getfield(blockst,rot1{jj});
        set(rotdataset,'string',rot3{jj},'backgroundcolor',rot4{jj});
    end
    rot5=getfield(GeneralData,'SendData');
    for jj=1:4
        hheaderget=getfield(blockst,['Header' num2str(jj)]);
        hdataget=getfield(blockst,['Data' num2str(jj)]);
        set(hheaderget,'string',rot5{jj},'backgroundcolor',[.8 .8 .8]);
        set(hdataget,'string',[],'backgroundcolor',[1 1 1]);
    end
end
stringinfo='Data has been reset and cleared';
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
% almost forgot to clear gca
cla;

% Saving data
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;
set(gcf,'userdata',AllInfo);
% ----------- 

%---------------------------
%----- GENERAL BUTTONS -----
%---------------------------
%
% This routine at present is only responsible for two buttons, those being
% the depth units and density units.  

function lascreator_general_buttons(hObject, eventdata, handles)
% checking data
controlout=lascreator_datacheck;
% -------------
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
hMainOneUp=getfield(FigureHandles,'OneUp');
hMainOneDown=getfield(FigureHandles,'OneDown');
% -----------

dat=get(gcbo,'userdata');
val=get(gcbo,'value');
str=get(gcbo,'string');
str=str{val};
col=[1 1 1];
if(dat==2)
    stringinfo=['Density units have been changed to ' str '.  Please ensure layer values are correct.'];
elseif(dat==1)
    stringinfo=['Depth units have been changed to ' str '. Please ensure layer values are correct.'];
    % if depth is changed, need to set kelly bushing and zstep to default for
    % respective depth units
    MiscPull=getfield(GeneralData,'MiscPull');
    fnames=getfield(MiscPull,'FieldNames');
    default=getfield(MiscPull,'Default');
    % any celled default data needs changing due to change in depth units
    for ii=1:size(fnames,2)
        checkdefault=default{ii};
        if(iscell(checkdefault))
            switch str
                case 'Meters'
                    dat=checkdefault{1};
                case 'Feet'
                    dat=checkdefault{2};
                case 'Seconds'
                    dat=checkdefault{3};
            end 
            GeneralData=setfield(GeneralData,fnames{ii},dat);
        end
    end
else
    strinfinfo='Something is wrong';
    col=[1 1 0];
end

set(hmsg,'string',stringinfo,'backgroundcolor',col);
% Saving data
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;
set(gcf,'userdata',AllInfo);
% ----------- 

%----------------------------
%---- SAVE / EXPORT LAS -----
%----------------------------
% 
% This routine is called both when saveing or exporting data.  When saving
% routine simply asks where the user wants to save LAS data.  Export is
% mainly used inconjuction with other programs and uses uiresume to allow
% for data to be sent out.

function lascreator_save(hObject, eventdata, handles)
% checking data
controlout=lascreator_datacheck;
% -------------
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
hMainOneUp=getfield(FigureHandles,'OneUp');
hMainOneDown=getfield(FigureHandles,'OneDown');
% -----------
if(controlout{2}==1)
    % saving or exporting will not occure if something is not right with
    % the data.
    stringinfo=[controlout{1} ' before saving can continue.'];
    col=[1 1 0];
    set(hmsg,'string',stringinfo,'backgroundcolor',col);
    return
end

% General Data
welldat=getfield(GeneralData,'Name');
wellname=get(welldat,'string');
locdat=getfield(GeneralData,'Location');
lc=get(locdat,'string');
nuldat=getfield(GeneralData,'NullValue');
nullval=get(nuldat,'string');
unitdat=getfield(GeneralData,'DepthUnits');
val=get(unitdat,'value');
units=get(unitdat,'string');
units=units(1,1);
den=getfield(GeneralData,'DensityUnits');
val=get(den,'value');
den=get(den,'string');
den=den(1,1);
logtypes=getfield(GeneralData,'LASHeaders');
logtypes{3}=den{1};
rot1=getfield(GeneralData,'SendData');
datapull=lascreator_datapull;
rot2=datapull{1};
topnames=datapull{2};
ztops=datapull{3};

holdat1=rot2{1};
if(isempty(holdat1))
    exp=get(gcbo,'string');
    stringinfo=['There is no data to be ' exp 'ed.'];
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
    return
end
zstart=holdat1(1);
zend=holdat1(end);
zstep=getfield(GeneralData,'ZStep');
if(ischar(zstep))
    zstep=str2num(zstep);
end
% acquiring 
MiscPull=getfield(GeneralData,'MiscPull');
fnames=getfield(MiscPull,'FieldNames');
LasID=getfield(MiscPull,'LasID');
% setting structure
exportstruct=struct;
for ii=1:size(fnames,2)
    dat=getfield(GeneralData,fnames{ii});
    if(iscell(dat))
        % When this loop occures, zstep is being checked for, whether its
        % meters, feet or seconds.
        switch units
            case 'M'
                dat=dat{1};
            case 'F'
                dat=dat{2};
            case 'S'
                dat=dat{3};
        end
    end
    exportstruct=setfield(exportstruct,LasID{ii},dat);
end
srvdate=date;       % Date
% setting up structure for export.
exportstruct.zstart=zstart;
exportstruct.zend=zend;
exportstruct.nullval=nullval;
exportstruct.units=units;
exportstruct.topnames=topnames;
exportstruct.ztops=ztops;
exportstruct.wellname=wellname;
exportstruct.lc=lc;
exportstruct.srvdate=srvdate;

% creating depths for logmat
%holdat1=[0;holdat1];
holdat1=[holdat1;holdat1(end)+100];
dt=[];
for ii=1:length(holdat1)-1
    % finding smallest layer thickness
    try
        % Just a quick fix 'cause really... this might blow up, I am not
        % sure
        dt=min([dt holdat1(ii+1)-holdat1(ii)]);
    catch
    end
end
% checking to make sure 
while (dt<zstep)
    stringinfo='Your depth step is too big for one or more of your layer thicknesses';
    col=[1 1 0];
    set(hmsg,'string',stringinfo,'backgroundcolor',col);
    qst={['Depth Step must be less then: ' num2str(dt),...
                ' or cancel and fix your depths']};
    a={''};
    flags=[1];
    titlestr=['Present Depth Step: ' num2str(zstep)];
    ansfini=askthingsle(gcf,qst,a,flags,titlestr); 
    if(isempty(ansfini))
        stringinfo='Action Canceled';
        col=[1 1 0];
        set(hmsg,'string',stringinfo,'backgroundcolor',col);
        return
    end
    zstep=str2num(ansfini{1});   
end
exportstruct.zstep=zstep;
dep=[];
rot3={};
rot4=rot1;
for ii=1:size(rot1,2)-1
    % building a cell to hold data for acquiring
    rot3{ii}=[];
end

logmathold=[];
% the following loop seems to be broken at this point. Gary Margrave put
% in the loop after this, and it fixed the problem.  For now... I am
% leaving this 'cause in all truth, unless more values are added, there is
% no need to find the exact problem in the loop.  Love.. Chris.
for ii=1:size(holdat1,1)-1
    % first part here is creating properly stepped depths
    rot1=rot4;
    xx=[holdat1(ii):zstep:holdat1(ii+1)]';
    dep=[dep;xx(1:end-1)];
    for jj=2:size(rot2,2)
        dat=rot2{jj};
        if(strcmp(rot1{jj},'Vs'))
            % this is now changing the Vs and Vp to slowness rather then
            % strickly velocity, if velocity zero, 
            rot1{jj}='SSON';
            dat=10^6./dat;
        elseif(strcmp(rot1{jj},'Vp'))
            rot1{jj}='PSON';
            dat=10^6./dat;
        elseif(strcmp(rot1{jj},'Density'))
            rot1{jj}=den{1};
        end
        logmathold=[logmathold dat];
        yy=dat(ii)*ones(size(xx(1:end-1),1),1);
        expdat=rot3{(jj-1)};
        expdat=[expdat;yy];
        rot3{(jj-1)}=expdat;
    end
end
logmat=dep;
%previously this had been fixed with this sonic ...
    %to velocity conversion in the loop however this created problems with
    %the first layer or the log.  The previous loop has now been fixed so
    %further conversions are not needed.
for ii=1:size(rot3,2)
%     if(ii<3)
%         tmp=10^6./rot3{ii};
%     else
%         tmp=rot3{ii};
%     end
logmat=[logmat rot3{ii}];
%     logmat=[logmat tmp];
 end
% finalizing structure incase changes have occured
exportstruct.logtypes=rot1(:,2:end);
lash=makelasheader(exportstruct);
hSend=getfield(FigureHandles,'Save');
dat=get(hSend,'userdata');
if(isstruct(dat))
    % this occures part of the close request function to save data
elseif(dat{1}==1)
    % exporting data, exporting closes figure
    set(gcbo,'userdata',{dat{1} lash logmat});
    uiresume;
elseif(dat{1}==2)
    % saveing data
    stringinfo='Save LAS file.  NOTE: Well log cut off at first blank depth.';
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
    [file,path]=myuifile(gcf,'*.las','Save File As','put');
    if(file==0)
        stringinfo='Action canceled';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
        return
    end
    if(isempty(findstr(file,'.las')))
        file=[path file '.las'];
    end
    writelas(file,lash,logmat);
else
    % if all else fails!
    errorstring='Something is wrong';
    errordlg(errorstring);
    return
end


%-------------------------
%---- BLOCK CONTROLS -----
%-------------------------
%
% This routine is fired up when the user uses Mouse button 2 to click on
% any uicontrol in any of the blocks (uicontextmenu).  The various controls
% are described below.  If more log properties are added, this section will
% have to undergo a major surgery.

function lascreator_block_controls(hObject, eventdata, handles)
controlout=lascreator_datacheck;    % checking data
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
% -----------
hdepthunit=getfield(GeneralData,'DepthUnits');
dat=get(hdepthunit,'string');
val=get(hdepthunit,'value');
depthunit=dat(val,:);

Block1=getfield(BlockHandles,'Block1');
LayerNumber1=getfield(Block1,'Number');
nm1=get(LayerNumber1,'string');
nm1=str2num(nm1(findstr(' ',nm1)+1:end));
Block4=getfield(BlockHandles,'Block4');
LayerNumber4=getfield(Block1,'Number');
nm4=get(LayerNumber4,'string');
nm4=str2num(nm4(findstr(' ',nm4)+1:end));
checkdep=getfield(LayerData,['Layer' num2str(nm4)],'Depth');
blocknum=get(gcbo,'userdata');
Blockget=getfield(BlockHandles,['Block' num2str(blocknum)]);
LayerNumber=getfield(Blockget,'Number');
nm=get(LayerNumber,'string');
nm=str2num(nm(findstr(' ',nm)+1:end));
% getting block handles
blockget=Blockget;
hLayerNumber=getfield(blockget,'Number');
hLayerName=getfield(blockget,'Name');
hQuickInfo=getfield(blockget,'Info');
hheader1=getfield(blockget,'Header1');
hdata1=getfield(blockget,'Data1');
hheader2=getfield(blockget,'Header2');
hdata2=getfield(blockget,'Data2');
hheader3=getfield(blockget,'Header3');
hdata3=getfield(blockget,'Data3');
hheader4=getfield(blockget,'Header4');
hdata4=getfield(blockget,'Data4');
% getting layer info
lay=getfield(LayerData,['Layer' num2str(nm)]);
num=getfield(lay,'LayerNumber');
nam=getfield(lay,'LayerName');
dep=getfield(lay,'Depth');
depunit=getfield(lay,'DepthUnit');
Vp=getfield(lay,'Vp');
Vpunit=getfield(lay,'VpUnit');
Vs=getfield(lay,'Vs');
Vsunit=getfield(lay,'VsUnit');
den=getfield(lay,'Density');
denunit=getfield(lay,'DensityUnit');
% initial switch number is empty
switchnm=[];
LayerData2=LayerData;
Layer0=getfield(LayerData,'Layer0');
blockoperation=get(gcbo,'label');
depswitch='YES';    % this controls whether the depth is switched or not
switch blockoperation
    case 'Up'
        % Shifts the selected block data up 1
        if(nm1==blocknum)
            stringinfo='Can not shift layer 1 up any higher';
            col=[1 1 0];
        else
            stringinfo=['Shifting layer ' num2str(blocknum) ' up'];
            col=[1 1 1];
            layerdataget=getfield(LayerData2,['Layer' num2str(nm)]);
            LayerData=setfield(LayerData,['Layer' num2str(nm-1)],layerdataget);
            layerdataget=getfield(LayerData2,['Layer' num2str(nm-1)]);
            LayerData=setfield(LayerData,['Layer' num2str(nm)],layerdataget);
        end
        % Depth is not being switched
        depswitch='NO';
    case 'Down'
        % shifts the selected block data down 1
        if(isempty(checkdep)&nm4==nm)
            stringinfo=['Can not shift layer ' num2str(nm4) ' down without depth specified.'];
            col=[1 1 0];
        else
            stringinfo=['Shifting layer ' num2str(blocknum) ' down'];
            col=[1 1 1];
            layerdataget=getfield(LayerData2,['Layer' num2str(nm)]);
            LayerData=setfield(LayerData,['Layer' num2str(nm+1)],layerdataget);
            layerdataget=getfield(LayerData2,['Layer' num2str(nm+1)]);
            LayerData=setfield(LayerData,['Layer' num2str(nm)],layerdataget);
        end
        % Depth is not being switched
        depswitch='NO';
    case 'Clear Layer'
        % clears block data without question
        stringinfo=['Clearing layer ' num2str(nm) ];
        col=[1 1 1];
        set(hLayerName,'string',[]);
        set(hdata1,'string',[]);
        set(hdata2,'string',[]);
        set(hdata3,'string',[]);
        set(hdata4,'string',[]);
        LayerData=setfield(LayerData,['Layer' num2str(nm)],Layer0);
        nm=nm1;
    case 'Above'
        stringinfo=['Layer is being inserted above ' num2str(nm)];
        col=[1 1 1];
        lyrs=fieldnames(LayerData);
        szlyrs=size(lyrs,1);
        LayerData2=[];
        LayerData2.Layer0=Layer0;
        jj=1;
        for ii=1:szlyrs;
            if(ii==nm)
                layerdataget=getfield(LayerData,'Layer0');
                LayerData2=setfield(LayerData2,['Layer' num2str(nm)],layerdataget);
            else
                layerdataget=getfield(LayerData,['Layer' num2str(jj)]);
                LayerData2=setfield(LayerData2,['Layer' num2str(ii)],layerdataget);
                jj=jj+1;
            end
        end
        LayerData=LayerData2;
        nm=nm1;
    case 'Below'
        stringinfo=['Layer is being inserted below ' num2str(nm)];
        col=[1 1 1];
        lyrs=fieldnames(LayerData);
        szlyrs=size(lyrs,1);
        LayerData2=[];
        LayerData2.Layer0=Layer0;
        jj=1;
        for ii=1:szlyrs;
            if(ii==nm+1)
                layerdataget=getfield(LayerData,'Layer0');
                LayerData2=setfield(LayerData2,['Layer' num2str(nm+1)],layerdataget);
            else
                layerdataget=getfield(LayerData,['Layer' num2str(jj)]);
                LayerData2=setfield(LayerData2,['Layer' num2str(ii)],layerdataget);
                jj=jj+1;
            end
        end
        LayerData=LayerData2;
        nm=nm1;
    case 'Choose'
        % not enabled, and wont be enabled... well... it might one day
    case 'Delete Layer'
        % this will delete layers without asking user again.
        stringinfo=['Deleting ' num2str(nm)];
        col=[1 1 1];
        lyrs=fieldnames(LayerData);
        szlyrs=size(lyrs,1);
        LayerData2=[];
        LayerData2.Layer0=Layer0;
        jj=1;
        for ii=1:szlyrs-1;
            if(ii==nm)
            else
                layerdataget=getfield(LayerData,['Layer' num2str(ii)]);
                LayerData2=setfield(LayerData2,['Layer' num2str(jj)],layerdataget);
                jj=jj+1;
            end
        end
        LayerData=LayerData2;
        nm=nm1;
end

for ii=1:4
    % getting blocks
    blk=getfield(BlockHandles,['Block' num2str(ii)]);
    % getting handles
    hbacking=getfield(blk,'Backing');
    hLayerNumber=getfield(blk,'Number');
    hLayerName=getfield(blk,'Name');
    hQuickInfo=getfield(blk,'Info');
    hheader1=getfield(blk,'Header1');
    hdata1=getfield(blk,'Data1');
    hheader2=getfield(blk,'Header2');
    hdata2=getfield(blk,'Data2');
    hheader3=getfield(blk,'Header3');
    hdata3=getfield(blk,'Data3');
    hheader4=getfield(blk,'Header4');
    hdata4=getfield(blk,'Data4');
    % getting data
    try
        lay=getfield(LayerData,['Layer' num2str(nm1)]);
    catch 
        % need to create a new layer
        layerdataget=getfield('LayerData','Layer0');
        LayerData=setfield(LayerData,['Layer' str2num(nm1+ii-1)],layerdataget);
        lay=getfield(LayerData,['Layer' num2str(nm1+ii-1)]);
    end
    lay=getfield(LayerData,['Layer' num2str(nm1+ii-1)]);
    num=getfield(lay,'LayerNumber');
    nam=getfield(lay,'LayerName');
    dep=getfield(lay,'Depth');
    depunit=getfield(lay,'DepthUnit');
    Vp=getfield(lay,'Vp');
    Vpunit=getfield(lay,'VpUnit');
    Vs=getfield(lay,'Vs');
    Vsunit=getfield(lay,'VsUnit');
    den=getfield(lay,'Density');
    denunit=getfield(lay,'DensityUnit');
    set(hLayerNumber,'string',['Layer: ' num2str(nm1+ii-1)]);
    set(hLayerName,'string',nam);
    % set(hdata1,'string',num2str(dep));    % depth is being left out of
    % shifting.  If more properties are added, this whole section will have
    % to be changed... I am not sure how at this point, but this is a note
    % to future folks
    if(strcmp(depswitch,'YES'))
        % depth is being switched
        set(hdata1,'string',num2str(dep));
    end
    set(hdata2,'string',num2str(Vp));
    set(hdata3,'string',num2str(Vs));
    set(hdata4,'string',num2str(den));
    set(hbacking,'backgroundcolor',[.5019 .5019 .5019]);
    x1=[num2str(dep) '  ' depthunit{1}];
    x2=['Vp: ' num2str(Vp) '  Vs: ' num2str(Vs)];
    x3=['Density: ' num2str(den)];
    set(hQuickInfo,'string',{x1;x2;x3}); 
    nm=nm+1;
end  
set(hmsg,'string',stringinfo,'backgroundcolor',col);
% Saving data
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;
set(gcf,'userdata',AllInfo);
% ----------- 
% checking data again, to save changes
controlout=lascreator_datacheck;    % checking data

%-------------------------------------------
%----- WELL LINES BUTTON DOWN FUNCTION -----
%-------------------------------------------
%
% this routine allows user to  click on a specific areas on the
% plotted well log graph and have the corresponding layer highlighed for
% close inspection or adjustment. Layer blocks may have to be shifted for
% desired layer to be highlighted

function lascreator_well_lines(hObject, eventdata, handles)
pt=get(gca,'currentpoint');
checkpt=pt(1,2);
% checking data
controlout=lascreator_datacheck;    % checking data
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
% -----------
datapull=lascreator_datapull;
% checking to see where click has taken place and setting up to highlight
% the correct block for user
dat=datapull{1};
alldep=dat{1};
findlayer=find(checkpt<alldep);
if(isempty(findlayer))
    % data will not shift due to depths being incorrect 
    stringinfo='No Depths';
    col=[1 1 0];
else
    stringinfo=['Layer ' num2str(findlayer(1))];
    col=[1 1 1];
    keylayer=findlayer(1);
    if(size(findlayer,1)<=3)
        findlayer=[findlayer(end)-size(findlayer,2)-2:1:findlayer(end)];
        if(findlayer(1)<=0)
            findlayer=[1 2 3 4];
        end
    else
        findlayer=[findlayer(1):1:findlayer(1)+3];
    end
    lascreator_master_move({findlayer(1) find(keylayer==findlayer)});
end
set(hmsg,'string',stringinfo,'backgroundcolor',col);

%----------------------------
%----- CLOSE LASCREATOR -----
%----------------------------
%
% This routine is allowing for the closing of lascreator

function lascreator_cancel(hObject, eventdata, handles)
% checking data
controlout=lascreator_datacheck;
% -------------
% Pulling data out
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
hMainOneUp=getfield(FigureHandles,'OneUp');
hMainOneDown=getfield(FigureHandles,'OneDown');
% -----------
hSend=getfield(FigureHandles,'Save');
dat=get(hSend,'userdata');
if(isstruct(dat))
    % means a close request has been attempted while save is enabled
    rtn=questdlg('Are you sure you want to quit?','Quit?',...
        'Save and Quit','Quit','Cancel','Quit');
else
    % means a close request has been attempted while exporting is enalbed
    rtn=questdlg('Are you sure you want to quit?','Quit?',...
        'Quit','Cancel','Quit');
end

switch rtn
    case 'Save and Quit'
        lascreator_save;
    case 'Quit'
    case 'Cancel'
        stringinfo='Action canceled';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
        return
    case 'xxx'
end
hsend=getfield(FigureHandles,'Save');
% checking to see which mode lascreator is in and closeing the figure
% repsectivly
if(strcmp(get(hsend,'string'),'Export'))
    uiresume;
else
    delete(gcf)
end

%-----------------------------------
%----- GCF CONTEXT MENU ITEMS ------
%-----------------------------------
%
% This routine controls all items associated with the GCF context menu.

function lascreator_gcfcontextmenu(hObject, eventdata, handles)
% checking data
controlout=lascreator_datacheck;    % checking data
% Pulling data out
hmasterfig=gcf;
h=get(gcf,'userdata');
LayerData=getfield(h,'Data');
GeneralData=getfield(h,'GeneralData');
BlockHandles=getfield(h,'BlockHandles');
BlockButtons=getfield(h,'BlockButtons');
FigureHandles=getfield(h,'FigureHandles');
hmsg=getfield(FigureHandles,'Message');
% ---------------

MiscPull=getfield(GeneralData,'MiscPull');
fnames=getfield(MiscPull,'FieldNames');
qst=getfield(MiscPull,'Questions');
askflags=getfield(MiscPull,'AskFlags');
flags=getfield(MiscPull,'Flags');
ans={};
for ii=1:size(fnames,2)
    ans{ii}=getfield(GeneralData,fnames{ii});
end
stringinfo=['Changing General data'];
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
ansfini=askthingsle(hmasterfig,qst,ans,askflags);
if(isempty(ansfini))
    stringinfo='Action Cancelled';
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
    return
end
% checking to see if user has answered properly
for ii=1:size(fnames,2)
    if(flags(ii)==0)
        % skipping 'cause it don't matter!
    elseif(flags(ii)==2)
        % making sure that output is a number
        if(~isempty(str2num(ansfini{ii})))
        else
            stringinfo='Please ensure that you put numeric values where needed';
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
            return
        end
    end
end    
% setting new values
for ii=1:size(ans,2)
    GeneralData=setfield(GeneralData,fnames{ii},ansfini{ii});
end
stringinfo=['General data has been changed and saved'];
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);

% Saving data
AllInfo.Data=LayerData;
AllInfo.GeneralData=GeneralData;
AllInfo.BlockHandles=BlockHandles;
AllInfo.BlockButtons=BlockButtons;
AllInfo.FigureHandles=FigureHandles;
set(gcf,'userdata',AllInfo);
% ----------- 
% checking data again, to save changes
controlout=lascreator_datacheck;    % checking data

%-------------------------
%----- LAYER SLIDER ------
%-------------------------
%
% Future Development

function lascreator_layer_slider()