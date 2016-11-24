function afd_velcreate(vel,dx,dz)
%% AFD_VELCREATE : build a model using polygonal shapes that are created
% graphically by the user
%
% afd_velcreate(vel,dx,dz)
% afd_velcreate(vel,dx)
% afd_velcreate
%
% This function creates a gui interface where the user can create polygonal
% shapes and add them to a velocity model.This model is intended to be used
% with the finite difference toolbox
%
% vel ... velocity model (z by x in size)
%
% dx ... grid interval (distance between grid points in x)
%
% dz ... grid interval (distance between grid points in z)
%  *********** default dx ************
%
% if no arguments are provided the gui will open where the user can then
% create a new section of constant velocity or create a model including,
% flatmodel, channelmodel, and wedgemodel.
%
% by H.J.E Lloyd, July 2009, Revised June 2010
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

%% Deal with Arguments
if nargin<1
    vel=[];
end
if(nargin<2)
    
    dx=10;
end
if(nargin<3)
    dz=dx;
end

%% Initialize the gui

hmasterfig=figure('Menu','none','units','normalized','position',...
    [0.25 0.225 0.5 0.55],'closerequestfcn',@exitmodel,'Name',...
    'AFD_VELCREATE','NumberTitle','off');
handles.hmasterfig=hmasterfig;
set(0,'DefaultUicontrolBackgroundColor',[0.9373,0.9373,0.9373]);
set(0,'DefaultUipanelBackgroundColor',[0.9373,0.9373,0.9373]);
handles.hpanel=uipanel(hmasterfig,'units','normalized','position',[0 0 1 1]);
handles.hmainaxes=axes('units','normalized','position',[0.1 0.3 0.8 0.60],...
    'Visible','off','Tag','axes');
handles.hmsg=uicontrol(hmasterfig,'Style','text','units','normalized',...
    'position',[0.1 0.025 0.8 0.05],'Fontsize',10,'fontweight','normal');
handles.title=uicontrol(hmasterfig,'Style','edit','units','normalized',...
    'position',[0.1 0.925 0.8 0.05],'Fontsize',14,'fontweight','bold');
handles.hsvel=uicontrol(hmasterfig,'Style','slider','units','normalized',...
    'position',[0.075 0.195 0.85 0.02],'Tooltipstring','Object velocity',...
    'Visible','off','Callback',@settext,'string','3500','backgroundcolor',...
    [.5 .5 .5],'Sliderstep',[.1,.000001]);
handles.txvel= uicontrol(hmasterfig,'Style','text','units','normalized',...
    'position',[0.3 0.075 0.15 0.05],'Tooltipstring','Object velocity',...
    'Visible','off','string','Object Velocity','Fontsize',12);
handles.hxvel=uicontrol(hmasterfig,'Style','text','units','normalized',...
    'position',[0.55 0.075 0.15 0.05],'Tooltipstring','Object velocity',...
    'Visible','off','string','3500','Fontsize',12);

%menus
%File Menu
hfilem=uimenu(hmasterfig,'Label','File');handles.hfilem=hfilem;
handles.hnew=uimenu(hfilem,'Label','New Model','Callback',@newmodel);
handles.hopen=uimenu(hfilem,'Label','Open *.mat Model','Callback',@openmodel);
handles.hopenv=uimenu(hfilem,'Label','Open *.vel Model','Callback',@openvelmodel);
handles.himpshp=uimenu(hfilem,'Label','Import Shapes from *.shp file','Callback',@importshapes);
handles.hprint=uimenu(hfilem,'Label','Print Model','Callback','printdlg(gcf)');
handles.hgenm=uimenu(hfilem,'Label','Generate *.m File','Callback',@generatem,'separator','on');
handles.hsave=uimenu(hfilem,'Label','Save Model as *.mat','Callback',@savemodel,'separator','on');
handles.hsavev=uimenu(hfilem,'Label','Save Model as *.vel','Callback',@savevelmodel,'separator','off');
handles.hsaveas=uimenu(hfilem,'Label','Save Model As','Callback',@savemodelas);
handles.hexpshp=uimenu(hfilem,'Label','Export Shapes to *.shp file','Callback',@exportshapes);
handles.hexit=uimenu(hfilem,'Label','Exit','Callback',@exitmodel,'separator','on');

%Model Menu
hmodel=uimenu(hmasterfig,'Label','Model');handles.hmodel=hmodel;
handles.hbgmodel=uimenu(hmodel,'Label','Background Model');
handles.hrflat=uimenu(handles.hbgmodel,'Label','Flat Model','Callback',@fltmodel);
handles.hchan=uimenu(handles.hbgmodel,'Label','Channel Model','Callback',@chanmodel);
handles.hwedge=uimenu(handles.hbgmodel,'Label','Wedge Model','Callback',@wedgmodel);
handles.hcrtobj=uimenu(hmodel,'Label','Add New Object','Callback',@crtobj,'enable','off');

% Format Menu
hform=uimenu(hmasterfig,'Label','Format Model');handles.hform=hform;
handles.hresampj=uimenu(hform,'Label','Resample Model','Callback',@resamps1,'enable','on');
hrmvnan=uimenu(hform,'Label','Remove Nans');handles.hrmvnan=hrmvnan;
handles.hconstfill=uimenu(hrmvnan,'Label','Replace Nans With Constant Fill','Callback',@constfill,'enable','on');
handles.hminmaxfill=uimenu(hrmvnan,'Label','Replace Nans With Vertical Variation','Callback',@maxminfill,'enable','on');
handles.hlinearfill=uimenu(hrmvnan,'Label','Replace Nans With Linear Function','Callback',@linearfill,'enable','on');

%Settings Menu
hsetting=uimenu(hmasterfig,'Label','Settings');handles.hsetting=hsetting;
handles.hlineset=uimenu(hsetting,'Label','Line Settings','Callback',@lineset,'enable','on');
handles.hcolbarset=uimenu(hsetting,'Label','Colorbar Settings','Callback',@colorbarset,'enable','on');
handles.hvellimset=uimenu(hsetting,'Label','Change Velocity Limits','Callback',@chngvellim,'enable','on');
handles.hmachfmt=uimenu(hsetting,'Label','Change Machine Format','Callback',@chngmachfmt,'enable','on');
handles.hmattog=uimenu(hsetting,'Label','Matrix Save Format','Callback',@chngmatfmt,'enable','on',...
    'userdata',1);

%Display Options Menu & ContextMenu
hdisp=uimenu('parent',hmasterfig, 'Label','Display Options');
hcmenu=uicontextmenu;
menuss=[hdisp, hcmenu];
for k=1:2
    menus=menuss(k);
    uimenu('parent',menus, 'Label','Increase Font Size','Separator', 'off',...
        'Callback','bigfont(gcf,1.125,1)');
    uimenu('parent',menus, 'Label','Decrease Font Size','Separator', 'off',...
        'Callback','bigfont(gcf,(1/1.125),1)');
    uimenu('parent',menus, 'Label','Bold Font','Separator', 'off',...
        'Callback','bigfont(gcf,1,2)');
    uimenu('parent',menus, 'Label','Normal Font','Separator', 'off',...
        'Callback','bigfont(gcf,1,1)');
    uimenu('parent',menus, 'Label','Increase Line Weight','Separator', 'on',...
        'Callback','boldlines(gcf,2,1)');
    uimenu('parent',menus, 'Label','Decrease Line Weight','Separator', 'off',...
        'Callback','boldlines(gcf,(1/2),1)');
    uimenu('parent',menus, 'Label','White Background','Separator', 'on',...
        'Callback','handles=guidata(gcf);set(handles.hpanel,''BackgroundColor'',[1 1 1])');
    uimenu('parent',menus, 'Label','Grey Background','Separator', 'off',...
        'Callback','handles=guidata(gcf);set(handles.hpanel,''BackgroundColor'',[0.941176 0.941176 0.941176])');
    uimenu('parent',menus, 'Label','Large Figure','Separator', 'off',...
        'Callback',@makebig);
    uimenu('parent',menus, 'Label','Small Figure','Separator', 'off',...
        'Callback',@makesmall);
    uimenu('parent',menus, 'Label','All Display Options ON','Separator', 'on',...
        'Callback',@makebig);
    uimenu('parent',menus, 'Label','All Display Options OFF','Separator', 'off',...
        'Callback',@makesmall);
    uimenu('parent',menus, 'Label','Copy Figure to Clipboard','Separator', 'on',...
        'Callback','print(gcf,''-dbitmap'')');
end
set(handles.hpanel,'uicontextmenu',hcmenu)

% Help Menu
hhelp=uimenu(hmasterfig,'Label','Help');handles.hhelp=hhelp;
handles.hvers=uimenu(hhelp,'Label','AFD_velcreate Version','Callback',@helps);
handles.hlicence=uimenu(hhelp,'Label','AFD_velcreate Licence','Callback',@helps);
handles.hhelps=uimenu(hhelp,'Label','AFD_velcreate Help','Callback',@helps,'separator','on');


%Toolbar
iconpath=iconslocator;
htoolbar=uitoolbar(hmasterfig);handles.htoolbar=htoolbar;
handles.hnewpush=uipushtool(htoolbar,'tooltipstring','New Model','cData',...
    imread(fullfile(iconpath,'tool_new.png'),'png'),'Clickedcallback',@newmodel);
handles.hopenpush=uipushtool(htoolbar,'tooltipstring','Open Model','cData',...
    imread(fullfile(iconpath,'tool_open.png'),'png'),'Clickedcallback',@openmodel);
handles.hsavepush=uipushtool(htoolbar,'tooltipstring','Save Model','cData',...
    imread(fullfile(iconpath,'tool_save.png'),'png'),'Clickedcallback',@savemodel);
handles.hzoominpush=uipushtool(htoolbar,'tooltipstring','Zoom In','cData',...
    imread(fullfile(iconpath,'tool_zoom_in.png'),'png'),'Clickedcallback',...
    @zoommode,'separator','on');
handles.hpanpush=uipushtool(htoolbar,'tooltipstring','Pan','cData',...
    imread(fullfile(iconpath,'tool_pan.png'),'png'),'Clickedcallback','pan');
handles.hzoomoutpush=uipushtool(htoolbar,'tooltipstring','Zoom Out','cData',...
    imread(fullfile(iconpath,'tool_zoom_out.png'),'png'),'Clickedcallback',...
    'zoom out;zoom off');
handles.hundoobjpush=uipushtool(htoolbar,'tooltipstring','Undo Last Object','cData',...
    imread(fullfile(iconpath,'tool_undoobj.png'),'png'),'Clickedcallback',...
    @undoobj,'separator','on','enable','off');
handles.hcrtobjpush=uipushtool(htoolbar,'tooltipstring','Create Object','cData',...
    imread(fullfile(iconpath,'tools_crtobj.png'),'png'),'Clickedcallback',...
    @crtobj,'separator','on','enable','off');
handles.hcpyobjpush=uipushtool(htoolbar,'tooltipstring','Copy Last Object','cData',...
    imread(fullfile(iconpath,'tools_cpyobj.png'),'png'),'Clickedcallback',...
    @cpyobj,'separator','off','enable','off');
handles.hmodifypush=uipushtool(htoolbar,'tooltipstring','Modify','cData',...
    imread(fullfile(iconpath,'tool_modify.png'),'png'),'Clickedcallback',...
    @modifyobj,'separator','on','visible','off');
handles.hsmoothpush=uipushtool(htoolbar,'tooltipstring','Smooth','cData',...
    imread(fullfile(iconpath,'tool_smooth.png'),'png'),'Clickedcallback',...
    @smoothobj,'separator','off','visible','off');
handles.hundoeditpush=uipushtool(htoolbar,'tooltipstring','Undo Last Edit','cData',...
    imread(fullfile(iconpath,'tool_undo.png'),'png'),'Clickedcallback',...
    @undoeditobj,'separator','off','visible','off');
handles.hpickvelpush=uipushtool(htoolbar,'tooltipstring','Pick Velocity From Model','cData',...
    imread(fullfile(iconpath,'tools_pickvel.png'),'png'),'Clickedcallback',...
    @pickvel1,'separator','on','visible','off');
handles.haddobjpush=uipushtool(htoolbar,'tooltipstring','Add Object','cData',...
    imread(fullfile(iconpath,'tools_addobj'),'png'),'Clickedcallback',...
    @addobj,'separator','on','visible','off');

handles.vel=vel;
handles.x=[];
handles.z=[];
handles.dx=dx;
handles.dz=dz;
handles.file=[];
handles.tmp=cd;handles.tmp=[handles.tmp,'\.afd_velcreatevel.tmp'];
handles.shp=cd;handles.shp=[handles.shp,'\.afd_velcreateshp.tmp'];
handles.mfile=cd;handles.mfile=[handles.mfile,'\.afd_velcreatem.tmp'];
handles.fid=fopen(handles.tmp,'w');
handles.fidshp=fopen(handles.shp,'w');
handles.fidm=fopen(handles.mfile,'w');
handles.machineformat='n';
handles.shpcount=1;
handles.lc=0;
handles.velobj=str2double(get(handles.hxvel,'String'));
handles.tempvel=handles.vel;
handles.editlines=0;
handles.zoomflag=0;
handles.linesettings.color='k';
handles.linesettings.style='-';
handles.linesettings.weight=1;
handles.max=4000;
handles.min=2000;
handles.mean=3000;
set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
set(handles.hxvel,'String',num2str(handles.mean));
guidata(hmasterfig,handles);
if ~isempty(handles.vel)
    modelinit(hmasterfig);
end
end

%% Sub Functions

function modelinit(hmasterfig)
handles=guidata(hmasterfig);
s=size(handles.vel);
dx=handles.dx;
dz=handles.dz;
x=0:dx:(s(2)-1)*dx;
z=0:dz:(s(1)-1)*dz;
handles.x=x;
handles.z=z;
hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
guidata(hmasterfig,handles);handles=colorbarsetup;zoom reset;
if any(any(isnan(handles.vel)))
    set(handles.hmsg,'string','Nans Must be removed from the model before resampling can continue');
    set(handles.hcrtobjpush,'enable','off');
    set(handles.hundoobjpush,'enable','off');
    set(handles.hcrtobj,'enable','off');
    set(hmasterfig,'pointer','arrow');
    return
end

if handles.dx~=handles.dz
    a=askthingslev2('Name','Gridspacing Mismatch','title',...
        {'To use AFD_VELCREATE the gridspacing in both'; 'x and z directions must be the same.';
        ['dx is currently ' num2str(handles.dx)];['dz is currently ' num2str(handles.dz)];
        'Suggested Resampling Rate:'},'questions',{'Resampling Rate'},...
        'answers',{num2str(mean([handles.dx,handles.dz]))});
    if isempty(a)
        set(handles.hmsg,'string','File does not have proper gridspacing');
        set(gcf,'pointer','arrow');
        set(handles.hcrtobjpush,'enable','off');
        set(handles.hundoobjpush,'enable','off');
        set(handles.hcrtobj,'enable','off');
        return
    else handles=resamps(str2double(a{1}));
    end
    
end
set(handles.hcrtobjpush,'enable','on');
set(handles.hundoobjpush,'enable','on');
set(handles.hcrtobj,'enable','on');
set(handles.hmsg,'string','Section Loaded');
handles.max=max(max(handles.vel));
handles.min=min(min(handles.vel));
handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
set(handles.hxvel,'String',num2str(handles.mean));
handles.tempvel=handles.vel;
guidata(hmasterfig,handles);
end

function newmodel(hObject, eventdata)
handles=guidata(hObject);
if ~isempty(handles.vel)
    warn = questdlg('Would you like to save your changes?', ...
        'Save Changes','Yes', 'No','Cancel', 'Yes');
    
    switch warn,
        case 'Yes',
            savemodel(hObject, eventdata);
        case 'No',
        case 'Cancel',
            return;
    end% switch
end
q={'End Inline Coordinate','End Depth Coordinate','Grid Spacing',...
    'Background Velocity'};
a={'3000','2000','10','2500'};
tooltps={'Maximum Horizontal Coordinate of Model',...
    'Maximum Depth Coordinate of Model','Grid Spacing',...
    'Constant Velocity for Model'};
a=askthingsle('Name','Set Model Parameters','questions',q,'answers',a,...
    'tooltips',tooltps);
if isempty(a)
    set(handles.hmsg,'string','Constant Model Cancelled');
    set(gcf,'pointer','arrow');
    return
end
vconst=str2double(a{4});
dx=str2double(a{3});
x=str2double(a{1}); x=0:dx:x;
z=str2double(a{2}); z=0:dx:z;
vel=vconst*ones(length(z),length(x));
handles.x=x;
handles.z=z;
handles.vel=vel;
handles.dx=dx;
handles.dz=dx;
hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
guidata(hObject,handles);handles=colorbarsetup;zoom reset;
set(handles.hcrtobjpush,'enable','on');
set(handles.hundoobjpush,'enable','on');
set(handles.hcrtobj,'enable','on');
handles.tempvel=handles.vel;
set(handles.colorbar,'XLim',[handles.min,handles.max]);
set(handles.colorbar,'Xtick',[handles.min,handles.max]);
set(handles.colorbar,'Xticklabel',[num2str(handles.min);num2str(handles.max)]);
startnewfiles(hObject, eventdata)
guidata(hObject,handles);
writecommand(hObject,['fvel=',num2str(vconst),'*ones(',num2str(length(z)),',',num2str(length(x)),');']);
end

function openmodel(hObject, eventdata)
handles=guidata(hObject);
if ~isempty(handles.vel)
    warn = questdlg('Would you like to save your changes?', ...
        'Save Changes','Yes', 'No','Cancel', 'Yes');
    
    switch warn,
        case 'Yes',
            savemodel(hObject, eventdata);
        case 'No',
        case 'Cancel',
            return;
    end% switch
end
%get the file name
[filename,path]=uigetfile('*.mat','Select Input velocity file');
if(filename(1)==0 )
    set(handles.hmsg,'string','no file name given');
    set(gcf,'pointer','arrow');
    return;
end
set(gcf,'pointer','watch');
fullfilename = [path filename];
set(handles.hmsg,'string',' now reading file...');
l=load(fullfile(fullfilename));
if isfield(l,'x')&&isfield(l,'z')&&(isfield(l,'vel')||isfield(l,'sec')||isfield(l,'velocity'))
    if (isfield(l,'velocity'))
        handles.vel=l.velocity;
        fiel='velocity';
    elseif(isfield(l,'vel'))
        handles.vel=l.vel;
        fiel='vel';
    else
        handles.vel=l.sec;
        fiel='sec';
    end
    handles.dx=l.x(2)-l.x(1);
    handles.dz=l.z(2)-l.z(1);
    handles.x=0:handles.dx:handles.dx*(length(l.x)-1);
    handles.z=(0:handles.dz:handles.dz*(length(l.z)-1))';
    
    hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
    guidata(hObject,handles);handles=colorbarsetup;zoom reset;
    %filename=find(strcmp(handles.file,'\');filename=handles.file(filename(end+1):end);
    set(handles.hmsg,'string',['File ',filename,' is now loaded']);
    set(gcf,'pointer','arrow');
    startnewfiles(hObject, eventdata)
    
    set(handles.title,'String',filename);
    handles.file=filename;
    guidata(hObject,handles);
    if any(any(isnan(handles.vel)))
        set(handles.hmsg,'string','Nans Must be removed from the model before resampling can continue');
        set(handles.hcrtobjpush,'enable','off');
        set(handles.hundoobjpush,'enable','off');
        set(handles.hcrtobj,'enable','off');
        set(gcf,'pointer','arrow');
        return
    end
    if handles.dx~=handles.dz
        a=askthingslev2('Name','Gridspacing Mismatch','title',...
            {'To use AFD_VELCREATE the gridspacing in both';'x and z directions must be the same.';
            ['dx is currently ' num2str(handles.dx)];['dz is currently ' num2str(handles.dz)];
            'Suggested Resampling Rate:'},'questions',{'Resampling Rate'},...
            'answers',{num2str(mean([handles.dx,handles.dz]))});
        if isempty(a)
            set(handles.hmsg,'string','File does not have proper gridspacing');
            set(gcf,'pointer','arrow');
            set(handles.hcrtobjpush,'enable','off');
            set(handles.hundoobjpush,'enable','off');
            set(handles.hcrtobj,'enable','off');
            return
        else handles=resamps(str2double(a{1}));
            
        end
        
    end
    set(handles.hcrtobjpush,'enable','on');
    set(handles.hundoobjpush,'enable','on');
    set(handles.hcrtobj,'enable','on');
    set(handles.hmsg,'string','Section Loaded');
    handles.max=max(max(handles.vel));
    handles.min=min(min(handles.vel));
    handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
    set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
    set(handles.hxvel,'String',num2str(handles.mean));
    handles.tempvel=handles.vel;
    guidata(hObject,handles);
    
    if get(handles.hmattog,'userdata')
        com=['bkmdl=load(''',fullfilename,''');',char(13),...
            'fdx=bkmdl.x(2)-bkmdl.x(1);',char(13),...
            'fdz=bkmdl.z(2)-bkmdl.z(1);',char(13),...
            'fx=0:fdx:fdx*(length(bkmdl.x)-1);',char(13),...
            'fz=0:fdz:fdz*(length(bkmdl.z)-1);',char(13),...
            'fvel=bkmdl.',fiel,';',char(13)];
        writecommand(hObject,com)
    else
        writematrix(hObject,eventdata);
    end
    
    set(handles.hmsg,'string','File is not a valid velocity model');
    set(gcf,'pointer','arrow');
    return
end

end

function openvelmodel(hObject, eventdata)
handles=guidata(hObject);
if ~isempty(handles.vel)
    warn = questdlg('Would you like to save your changes?', ...
        'Save Changes','Yes', 'No','Cancel', 'Yes');
    
    switch warn,
        case 'Yes',
            savemodel(hObject, eventdata);
        case 'No',
        case 'Cancel',
            return;
    end% switch
end
%get the file name
[filename,path]=uigetfile('*.vel','Select Input velocity file');
if(filename(1)==0 )
    set(handles.hmsg,'string','no file name given');
    set(gcf,'pointer','arrow');
    return;
end
set(gcf,'pointer','watch');
startnewfiles(hObject, eventdata)

fullfilename = [path filename];
set(handles.hmsg,'string',' now reading file...');
fidin=fopen(fullfilename,'r',handles.machineformat);
ck=1;
velfile.cnt.m=0;
velfile.cnt.c=0;
velfile.cnt.s=0;
order='';
while ~isempty(ck)
    type=(fread(fidin,7,'*char'));type=type';
    if strcmp(type,'matrix:')
        velfile.cnt.m=velfile.cnt.m+1;
        order=[order,'m',num2str(velfile.cnt.m),'%'];
        fseek(fidin,4,'cof');
        velfile.matrix{velfile.cnt.m}.dx=fread(fidin,1,'float');
        fseek(fidin,4,'cof');
        velfile.matrix{velfile.cnt.m}.dz=fread(fidin,1,'float');
        fseek(fidin,6,'cof');
        velfile.matrix{velfile.cnt.m}.sz=(fread(fidin,2,'int'))';
        fseek(fidin,5,'cof');
        velfile.matrix{velfile.cnt.m}.vel=fread(fidin,velfile.matrix{velfile.cnt.m}.sz,'float');
        ck=fread(fidin,1,'*char');
    elseif strcmp(type,'comman:');
        velfile.cnt.c=velfile.cnt.c+1;
        order=[order,'c',num2str(velfile.cnt.c),'%'];
        fseek(fidin,6,'cof');
        velfile.comman{velfile.cnt.c}.sz=fread(fidin,2,'int');
        fseek(fidin,1,'cof');
        velfile.comman{velfile.cnt.c}.command=(fread(fidin,velfile.comman{velfile.cnt.c}.sz(2),'*char'))';
        ck=fread(fidin,1,'*char');
    elseif strcmp(type,'shapes:');
        velfile.cnt.s=velfile.cnt.s+1;
        order=[order,'s',num2str(velfile.cnt.s),'%'];
        fseek(fidin,10,'cof');
        velfile.shapes{velfile.cnt.s}.shapenumber=fread(fidin,1,'int');
        fseek(fidin,6,'cof');
        velfile.shapes{velfile.cnt.s}.sz=fread(fidin,2,'int');
        fseek(fidin,3,'cof');
        velfile.shapes{velfile.cnt.s}.xpoly=fread(fidin,velfile.shapes{velfile.cnt.s}.sz','float');
        fseek(fidin,3,'cof');
        velfile.shapes{velfile.cnt.s}.zpoly=fread(fidin,velfile.shapes{velfile.cnt.s}.sz','float');
        fseek(fidin,3,'cof');
        velfile.shapes{velfile.cnt.s}.vpoly=fread(fidin,1,'float');
        ck=fread(fidin,1,'*char');
    else
        ck=[];
    end
end
order=['%',order];p=findstr(order,'%');
velfile.order=order;
[nulflag velfile]=editvelfile(velfile);
if nulflag
    for l=1:length(p)-1
        otype=order(p(l)+1);
        n=str2num(order((p(l)+2):(p(l+1)-1)));
        if strcmp(otype,'m')
            handles.vel=velfile.matrix{n}.vel;
            handles.dx=velfile.matrix{n}.dx;
            handles.dz=velfile.matrix{n}.dz;
            handles.x=0:handles.dx:handles.dx*(velfile.matrix{n}.sz(1));
            handles.z=(0:handles.dz:handles.dz*(velfile.matrix{n}.sz(2)))';
            handles.max=max(max(handles.vel));
            handles.min=min(min(handles.vel));
            handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
            set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
            set(handles.hxvel,'String',num2str(handles.mean));
            handles.tempvel=handles.vel;
            guidata(hObject,handles);
            writematrix(hObject,eventdata);
            handles=guidata(hObject);
        elseif strcmp(otype,'c');
            eval(velfile.comman{n}.command);
            handles.vel=fvel;
            handles.dx=fx(2)-fx(1);
            handles.dz=fz(2)-fz(1);
            handles.x=fx;
            handles.z=fz;
            guidata(hObject,handles);
            writecommand(hObject,velfile.comman{n}.command);
            handles=guidata(hObject);
        elseif strcmp(otype,'s');
            handles.vel=afd_vmodel(handles.dx,handles.vel,velfile.shapes{n}.vpoly,...
                velfile.shapes{n}.xpoly,velfile.shapes{n}.zpoly);
            guidata(hObject,handles);
            writeshape(hObject,velfile.shapes{n});
            handles=guidata(hObject);
        end
        
    end
    handles=guidata(hObject);
    handles.max=max(max(handles.vel));
    handles.min=min(min(handles.vel));
    handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
    set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
    set(handles.hxvel,'String',num2str(handles.mean));
    handles.tempvel=handles.vel;
    set(handles.hcrtobjpush,'enable','on');
    set(handles.hundoobjpush,'enable','on');
    set(handles.hcrtobj,'enable','on');
    set(handles.hmsg,'string','Vel File Has Been Loaded');
    set(gcf,'pointer','arrow');
    set(handles.title,'String',filename);
    hold off;imagesc(handles.x,handles.z,handles.vel);
    guidata(hObject,handles);handles=colorbarsetup;hold on;
    guidata(hObject,handles);
    
else
    set(handles.hmsg,'string','Load Vel File Was Cancelled');
    set(gcf,'pointer','arrow');
end

end

function savemodel(hObject, eventdata)
handles=guidata(hObject);
if isempty(handles.vel)
    set(handles.hmsg,'string','There is No data to save');
    set(gcf,'pointer','arrow');
    return
end
fullfilename=handles.file;
if isempty(handles.file)
    savemodelas(hObject, eventdata)
    return
end

%replace max and min corners
min=mean([handles.vel(1,end-1),handles.vel(2,end),handles.vel(2,end-1)]);
max=mean([handles.vel(end,end-1),handles.vel(end-1,end),handles.vel(end-1,end-1)]);
handles.vel(1,end)=min;
handles.vel(end,end)=max;

velocity=handles.vel;
x=handles.x;
z=handles.z;
dx=handles.dx;
dz=handles.dx;

save(fullfile(fullfilename),'velocity','x','z','dx','dz');
%ind=find(fullfilename=='\');
%filename=fullfilename((ind(end)+1):end);
% set(handles.hmsg,'string',['File ',filename ' is now saved.']);
% set(handles.title,'String',filename);
set(handles.hmsg,'string',['File ',fullfilename ' is now saved.']);
set(handles.title,'String',fullfilename);
set(gcf,'pointer','arrow');
guidata(hObject,handles);
end

function savevelmodel(hObject,eventdata)
handles=guidata(hObject);
[filename,path]=uiputfile('*.vel','Save As');
if(filename(1)==0 )
    set(handles.hmsg,'string','no file name given');
    set(gcf,'pointer','arrow');
    return;
end
set(gcf,'pointer','watch');
fullfilename = [path filename];
copyfile(handles.tmp,fullfilename);
set(gcf,'pointer','arrow');
end

function savemodelas(hObject, eventdata)
handles=guidata(hObject);
[filename,path]=uiputfile('*.mat','Save As');
if(filename(1)==0 )
    set(handles.hmsg,'string','no file name given');
    set(gcf,'pointer','arrow');
    return;
end
set(gcf,'pointer','watch');
fullfilename = [path filename];
handles.file=fullfilename;
guidata(hObject,handles);
savemodel(hObject, eventdata);
end

function exitmodel(hObject, eventdata)
handles=guidata(hObject);
currfig=gcf;
if isfield(handles,'help')
    if ~isempty(handles.help)
        delete(handles.help);
    end
end

if isempty(handles.vel)
    delete(currfig);
else
    warn = questdlg('Would you like to save your changes?', ...
        'Save Changes','Yes', 'No','Cancel', 'Yes');
    
    switch warn,
        case 'Yes',
            savemodel(hObject, eventdata);
            delete(currfig);
        case 'No',
            delete(currfig); %close was causing the dialog to appear twice
        case 'Cancel',
            return;
    end% switch
end
end

function fltmodel(hObject, eventdata)
handles=guidata(hObject);
set(gcf,'pointer','watch');
q={'Grid Spacing','Maximum Inline Coordinate','Maximum Depth',...
    'Maximum Velocity','Minimum Velocity','Number of Sedimentary Layers',...
    'Vector of Layer Depths'};
if ~isempty(handles.dx),dx=handles.dx;else dx=10;end
if ~isempty(handles.x),x=handles.x(end);else x=2500;end
if ~isempty(handles.z),z=handles.z(end);else z=1000;end
a={num2str(dx),num2str(x),num2str(z),'4000','2000','4','[]'};
tooltps={'Grid Spacing','Maximum Inline Coordinate','Maximum Depth',...
    'Maximum Velocity','Minimum Velocity','Number of Sedimentary Layers',...
    'Must be a vector'};
a=askthingsle('Name','Get Flat Model Parameters','questions',q,'answers',a,'tooltips',tooltps);

if isempty(a)
    set(handles.hmsg,'string','Flat Model Cancelled');
    set(gcf,'pointer','arrow');
    return
end

dx=str2double(a{1});
xmax=str2double(a{2});
zmax=str2double(a{3});
vhigh=str2double(a{4});
vlow=str2double(a{5});
nlayers=str2double(a{6});
zlay=str2num(a{7});
if(isnan(zlay))
    zlay=[];
    com=['[fvel,fx,fz,fzlay,fvlay]=flatmodel(',num2str(dx),',',num2str(xmax),',',num2str(zmax),...
    ',',num2str(vhigh),',',num2str(vlow),',',num2str(nlayers),');'];
else
    nlayers=[];
    com=['[fvel,fx,fz,fzlay,fvlay]=flatmodel(',num2str(dx),',',num2str(xmax),',',num2str(zmax),...
    ',',num2str(vhigh),',',num2str(vlow),',[],','[',num2str(zlay),']',');'];
end

startnewfiles(hObject, eventdata)
writecommand(hObject,com);

[vel,x,z,zlay,vlay]=flatmodel(dx,xmax,zmax,vhigh,vlow,nlayers,zlay);
handles.dx=dx;
handles.vel=vel;
handles.x=x;
handles.z=z;
if strcmp(get(gcf,'Name'),'Error Dialog');delete(gcf);end
hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
guidata(hObject,handles);handles=colorbarsetup;zoom reset;
set(handles.hcrtobjpush,'enable','on');
set(handles.hundoobjpush,'enable','on');
set(handles.hcrtobj,'enable','on');
set(handles.hmsg,'string','Flat Model is now Initialized');
set(gcf,'pointer','arrow');
handles.max=max(max(handles.vel));
handles.min=min(min(handles.vel));
handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
set(handles.hxvel,'String',num2str(handles.mean));
handles.tempvel=handles.vel;
guidata(hObject,handles);
end

function chanmodel(hObject, eventdata)
handles=guidata(hObject);
set(gcf,'pointer','watch');
q={'Grid Spacing','Maximum Inline Coordinate','Maximum Depth',...
    'Maximum Velocity','Minimum Velocity','Depth to Channel',...
    'Width of Channel','Thickness of Channel','Velocity of Channel',...
    'Number of Sedimentary Layers','Distribution of Layers','Vector of Layer Depths'};
if ~isempty(handles.dx),dx=handles.dx;else dx=10;end
if ~isempty(handles.x),x=handles.x(end);else x=2500;end
if ~isempty(handles.z),z=handles.z(end);else z=1000;end
a={num2str(dx),num2str(x),num2str(z),'4000','2000','500',num2str(dx*5),...
    num2str(dx*5),'2500','4','Random|Mostly Above','[]'};
tooltps={'Grid Spacing','Maximum Inline Coordinate','Maximum Depth',...
    'Maximum Velocity','Minimum Velocity','Depth to Channel',...
    'Width of Channel','Thickness of Channel','Velocity of Channel',...
    'Number of Sedimentary Layers','Distribution of Layers','Must be a vector with at least 2 entries'};
a=askthingsle('Name','Get Channel Model Parameters','questions',q,'answers',a,'tooltips',tooltps);

if isempty(a)
    set(handles.hmsg,'string','Channel Model Cancelled');
    set(gcf,'pointer','arrow');
    return
end

dx=str2double(a{1});
xmax=str2double(a{2});
zmax=str2double(a{3});
vhigh=str2double(a{4});
vlow=str2double(a{5});
zchannel=str2double(a{6});
wchannel=str2double(a{7});
hchannel=str2double(a{8});
vchannel=str2double(a{9});
nlayers=str2double(a{10});
if strcmp(a{11},'Mostly Above'),flag=1;else flag=0;end
zlay=str2num(a{12});

if isempty(zlay)
    [vel,x,z,zlay,vlay]=channelmodel(dx,xmax,zmax,vhigh,vlow,zchannel,wchannel,hchannel,vchannel,nlayers,flag);
    com=['[fvel,fx,fz,fzlay,fvlay]=channelmodel(',num2str(dx),',',num2str(xmax),',',num2str(zmax),...
        ',',num2str(vhigh),',',num2str(vlow),',',num2str(zchannel),',',...
        num2str(wchannel),',',num2str(hchannel),',',num2str(vchannel),',',...
        num2str(nlayers),',',num2str(flag),')'];
else
    [vel,x,z,zlay,vlay]=channelmodel(dx,xmax,zmax,vhigh,vlow,zchannel,wchannel,hchannel,vchannel,nlayers,flag,zlay);
    com=['[fvel,fx,fz,fzlay,fvlay]=channelmodel(',num2str(dx),',',num2str(xmax),',',num2str(zmax),...
        ',',num2str(vhigh),',',num2str(vlow),',',num2str(zchannel),',',...
        num2str(wchannel),',',num2str(hchannel),',',num2str(vchannel),',',...
        '[],',num2str(flag),',','[',num2str(zlay),']',')'];
end
startnewfiles(hObject, eventdata)

writecommand(hObject,com);
handles.dx=dx;
handles.vel=vel;
handles.x=x;
handles.z=z;
if strcmp(get(gcf,'Name'),'Error Dialog');delete(gcf);end
hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
guidata(hObject,handles);handles=colorbarsetup;zoom reset;
set(handles.hcrtobjpush,'enable','on');
set(handles.hundoobjpush,'enable','on');
set(handles.hcrtobj,'enable','on');
set(handles.hmsg,'string','Channel Model is now Initialized');
set(gcf,'pointer','arrow');
handles.max=max(max(handles.vel));
handles.min=min(min(handles.vel));
handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
set(handles.hxvel,'String',num2str(handles.mean));
handles.tempvel=handles.vel;
guidata(hObject,handles);

end

function wedgmodel(hObject, eventdata)
handles=guidata(hObject);
set(gcf,'pointer','watch');
q={'Grid Spacing','Maximum Inline Coordinate','Maximum Depth',...
    'Maximum Velocity','Minimum Velocity'};
if ~isempty(handles.dx),dx=handles.dx;else dx=10;end
if ~isempty(handles.x),x=handles.x(end);else x=2500;end
if ~isempty(handles.z),z=handles.z(end);else z=1000;end
a={num2str(dx),num2str(x),num2str(z),'4000','2000'};
tooltps={'Grid Spacing','Maximum Inline Coordinate','Maximum Depth',...
    'Maximum Velocity','Minimum Velocity'};
a=askthingsle('Name','Get Wedge Model Parameters','questions',q,'answers',a,'tooltips',tooltps);

if isempty(a)
    set(handles.hmsg,'string','Wedge Model Cancelled');
    set(gcf,'pointer','arrow');
    return
end

dx=str2double(a{1});
xmax=str2double(a{2});
zmax=str2double(a{3});
vhigh=str2double(a{4});
vlow=str2double(a{5});
[vel,x,z]=wedgemodel(dx,xmax,zmax,vhigh,vlow);
com=['[fvel,fx,fz]=wedgemodel(',num2str(dx),',',num2str(xmax),',',num2str(zmax),...
    ',',num2str(vhigh),',',num2str(vlow),');'];

startnewfiles(hObject, eventdata)

writecommand(hObject,com);
handles.dx=dx;
handles.vel=vel;
handles.x=x;
handles.z=z;
if strcmp(get(gcf,'Name'),'Error Dialog');delete(gcf);end

hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
guidata(hObject,handles);handles=colorbarsetup;zoom reset;
set(handles.hcrtobjpush,'enable','on');
set(handles.hundoobjpush,'enable','on');
set(handles.hcrtobj,'enable','on');
set(handles.hmsg,'string','Wedge Model is now Initialized');
set(gcf,'pointer','arrow');
handles.max=max(max(handles.vel));
handles.min=min(min(handles.vel));
handles.mean=mean(mean(handles.vel(~isnan(handles.vel))));
set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
set(handles.hxvel,'String',num2str(handles.mean));
handles.tempvel=handles.vel;
guidata(hObject,handles);

end

function crtobj(hObject, eventdata)
handles=guidata(hObject);
zoom off; handles.zoomflag=0;pan off;
handles.tempvel=handles.vel;
set(handles.hvellimset,'enable','off')
set(handles.hcrtobj,'enable','off');
set(handles.hcpyobjpush,'enable','off');
set(handles.hcrtobjpush,'enable','off');
set(handles.hmodifypush,'Visible','on');
set(handles.hsmoothpush,'Visible','on');
set(handles.hundoeditpush,'Visible','on');
set(handles.hpickvelpush,'Visible','on');
set(handles.haddobjpush,'Visible','on');
set(handles.hxvel,'Visible','on');
set(handles.hsvel,'Visible','on');
set(handles.txvel,'Visible','on');
hold on;
handles.lc=handles.lc+1;
xmid=(max(handles.x)+ min(handles.x))/2;
zmid=(max(handles.z)+ min(handles.z))/2;
xwid=(max(handles.x)-min(handles.x))/8;
zwid=(max(handles.z)-min(handles.z))/8;
xpoly=[xmid-xwid,xmid+xwid,xmid+xwid,xmid-xwid,xmid-xwid];
zpoly=[zmid-zwid,zmid-zwid,zmid+zwid,zmid+zwid,zmid-zwid];
handles.shape(handles.lc).lineh=plot(xpoly,zpoly,'color',handles.linesettings.color,...
    'linestyle',handles.linesettings.style,'linewidth',handles.linesettings.weight);
handles.shape(handles.lc).xpoly=xpoly;
handles.shape(handles.lc).zpoly=zpoly;
editlinesinit;
handles.editlines=1;
guidata(hObject,handles);
end

function cpyobj(hObject, eventdata)
handles=guidata(hObject);
zoom off; handles.zoomflag=0;pan off;
handles.tempvel=handles.vel;
set(handles.hvellimset,'enable','off')
set(handles.hcrtobj,'enable','off');
set(handles.hcpyobjpush,'enable','off');
set(handles.hcrtobjpush,'enable','off');
set(handles.hmodifypush,'Visible','on');
set(handles.hsmoothpush,'Visible','on');
set(handles.hundoeditpush,'Visible','on');
set(handles.hpickvelpush,'Visible','on');
set(handles.haddobjpush,'Visible','on');
set(handles.hxvel,'Visible','on');
hold on;
xpoly=(handles.shape(handles.lc).xpoly);
zpoly=(handles.shape(handles.lc).zpoly);
handles.lc=handles.lc+1;
handles.shape(handles.lc).lineh=plot(xpoly,zpoly,'color',handles.linesettings.color,...
    'linestyle',handles.linesettings.style,'linewidth',handles.linesettings.weight);
handles.shape(handles.lc).xpoly=xpoly;
handles.shape(handles.lc).zpoly=zpoly;
editlinesinit;
handles.editlines=1;
guidata(hObject,handles);
end

function addobj(hObject, eventdata)
handles=guidata(hObject);
if handles.editlines;
    editlinesfini;
    handles.editlines=0;
end
xpoly=get(handles.shape(handles.lc).lineh,'XData');
zpoly=get(handles.shape(handles.lc).lineh,'YData');
handles.velobj=str2double(get(handles.hxvel,'String'));
handles.shape(handles.lc).xpoly=xpoly;
handles.shape(handles.lc).zpoly=zpoly;
handles.shape(handles.lc).vpoly=handles.velobj;
handles.vel=afd_vmodel(handles.dx,handles.vel,handles.velobj,...
    xpoly,zpoly);
hold off;imagesc(handles.x,handles.z,handles.vel);
guidata(hObject,handles);handles=colorbarsetup;hold on;
set(handles.hmodifypush,'Visible','off');
set(handles.hsmoothpush,'Visible','off');
set(handles.hundoeditpush,'Visible','off');
set(handles.hpickvelpush,'Visible','off');
set(handles.haddobjpush,'Visible','off');
set(handles.hcrtobj,'enable','on');
set(handles.hcrtobjpush,'enable','on');
set(handles.hcpyobjpush,'enable','on');
set(handles.hvellimset,'enable','on')
set(handles.hmsg,'string','New Object Added');
guidata(hObject,handles);
writeshape(hObject,handles.shape(handles.lc));
end

function pickvel1(hObject, eventdata)
set(gcf,'WindowButtonDownFcn', @pickvel)
set(gcf,'Pointer','crosshair');
end

function pickvel(hObject, eventdata)
%editlinesfini;
handles=guidata(hObject);
if strcmp(get(gco,'Tag'),'Colorbar')
    clickpt=get(gco,'currentpoint');
    handles.velobj=clickpt(1,1);
elseif strcmp(get(gco,'Type'),'image')
    clickpt=get(gca,'currentpoint');
    x=near(handles.x,clickpt(1,1));
    z=near(handles.z,clickpt(1,2));
    velpt=handles.vel(z,x);
    handles.velobj=velpt;
else
    set(handles.hmsg,'String','Please Click on the Plot or the Colorbar');
    set(gcf,'windowButtonDownFcn', '');
    set(gcf,'pointer','arrow');
    return;
end;
set(handles.hxvel,'String',num2str(handles.velobj));
set(handles.hsvel,'Value',handles.velobj);
set(gcf,'windowButtonDownFcn', '');
set(gcf,'pointer','arrow');
guidata(hObject,handles);
end

function modifyobj(hObject, eventdata)
handles=guidata(hObject);
zoom off; handles.zoomflag=0;pan off;
if handles.editlines==0;
    editlinesinit;
    handles.editlines=1;
end
editlines('smoothoff');
editlines('linkoff');
set(handles.hmsg,'string','MB1: Move/Add Points, MB2: Delete Points, MB3: Anchor Points');
guidata(hObject,handles);
end

function smoothobj(hObject, eventdata)
handles=guidata(hObject);
zoom off; handles.zoomflag=0;pan off;
if handles.editlines==0;
    editlinesinit;
    handles.editlines=1;
end
editlines('smoothon');
set(handles.hmsg,'string','MB1: Smooth Between Points, Tip: Anchor points before smoothing');
guidata(hObject,handles);
end

function undoeditobj(hObject, eventdata)
editlines('undo');
end

function settext(hObject, eventdata)
handles=guidata(hObject);
velobj=get(handles.hsvel,'Value');
handles.velobj=velobj;
set(handles.hxvel,'String',num2str(velobj));
guidata(hObject,handles);
end

function undoobj(hObject, eventdata)
handles=guidata(gcbo);
if handles.editlines;
    editlinesfini;
    handles.editlines=0;
end
handles.vel=handles.tempvel;
hold off;imagesc(handles.x,handles.z,handles.tempvel);
guidata(hObject,handles);handles=colorbarsetup;hold on;
set(handles.hmodifypush,'Visible','off');
set(handles.hsmoothpush,'Visible','off');
set(handles.hundoeditpush,'Visible','off');
set(handles.hpickvelpush,'Visible','off');
set(handles.haddobjpush,'Visible','off');
set(handles.hcrtobj,'enable','on');
set(handles.hcrtobjpush,'enable','on');
set(handles.hcpyobjpush,'enable','on');
set(handles.hmsg,'string','Last Object Removed From Velocity Model');
guidata(hObject,handles);
end

function handles=resamps(rate)
handles=guidata(gcf);
if any(any(isnan(handles.vel)))
    set(handles.hmsg,'string','Nans Must be removed from the model before resampling can continue');
    set(gcf,'pointer','arrow');
    return
end
xlim=handles.x(end)/rate;
zlim=handles.z(end)/rate;
if (xlim*zlim >=1000000)
    warn = questdlg({'With current resample rate the model will be';
        ['( ',num2str(zlim),' , ',num2str(xlim),' )'];
        'Are You Sure You Want to Continue'}, ...
        'Save Changes','Yes', 'No', 'Yes');
    
    switch warn,
        case 'Yes',
            
        case 'No',
            set(gcf,'pointer','arrow');
            return;
    end% switch
end
xi=0:rate:handles.x(end);
zi=(0:rate:handles.z(end))';
handles.vel = interp2(handles.x,handles.z,handles.vel,xi,zi);
handles.z=zi;
handles.x=xi;
handles.dx=rate;
handles.dz=rate;
guidata(gcf,handles);
handles.max=max(max(handles.vel));
handles.min=min(min(handles.vel));
hold off;imagesc(handles.x,handles.z,handles.vel);hold on;
guidata(gcf,handles);handles=colorbarsetup;zoom reset;
end

function resamps1(hObject, eventdata)
handles=guidata(hObject);
if isempty(handles.vel)
    set(handles.hmsg,'string','Please Load a Velocity Model');
    set(gcf,'pointer','arrow');
    return
end
if handles.dx~=handles.dz
    a=askthingslev2('Name','Gridspacing Mismatch','title',...
        {'To use AFD_VELCREATE the gridspacing in both';'x and z directions must be the same.';
        ['dx is currently ' num2str(handles.dx)];['dz is currently ' num2str(handles.dz)];
        'Suggested Resampling Rate:'},'questions',{'Resampling Rate'},...
        'answers',{num2str(mean([handles.dx,handles.dz]))});
else
    a=askthingslev2('Name','Resampling Model','title',...
        {'Currently the Model has the following gridspacing:';
        ['dx and dz are currently ' num2str(handles.dx)];
        'Choose the new Resampling Rate:'},'questions',{'Resampling Rate'},...
        'answers',{num2str(mean([handles.dx,handles.dz]))});
end
if isempty(a)
    set(handles.hmsg,'string','Resampling Cancelled');
    set(gcf,'pointer','arrow');
    return
end
rate=str2double(a{1});
handles=resamps(rate);
guidata(hObject,handles);
end

function zoommode(hObject, eventdata)
handles=guidata(hObject);
if handles.editlines
    editlinesfini;
    handles.editlines=0;
end
if handles.zoomflag
    zoom off;
    handles.zoomflag=0;
else
    zoom on;
    handles.zoomflag=1;
end
guidata(hObject,handles);
end

function constfill(hObject, eventdata)
handles=guidata(hObject);
if isempty(handles.vel)
    set(handles.hmsg,'string','Please Load a Velocity Model');
    set(gcf,'pointer','arrow');
    return
end

if ~any(any(isnan(handles.vel)))
    set(handles.hmsg,'string','There are no NaNs to be replaced in this model');
    set(gcf,'pointer','arrow');
    return
end
a=askthingsle('Name','Replace NaNs with Constant','title',...
    'NaNs must be removed to use this tool','questions',...
    {'Constant Replacement Velocity'},'answers',{num2str(mean(mean(handles.vel(~isnan(handles.vel)))))});
if isempty(a)
    set(handles.hmsg,'string','Replace NaNs with Constant was Cancelled');
    set(gcf,'pointer','arrow');
    return
else
    k=(str2double(a{1}));
end

tempsec=zeros(size(handles.vel));
sm=size(handles.vel);
for ii=1:sm(2)
    int=handles.vel( : ,ii);
    int(isnan(int))=k;
    tempsec(:,ii)=int;
end;
handles.vel=tempsec;
set(handles.hmsg,'string','Nans have been removed from section');
set(gcf,'pointer','arrow');
guidata(hObject,handles);
if handles.dx~=handles.dz
    a=askthingslev2('Name','Gridspacing Mismatch','title',...
        {'To use AFD_VELCREATE the gridspacing in both x and z directions must be the same.';
        ['dx is currently ' num2str(handles.dx)];['dz is currently ' num2str(handles.dz)];
        'Suggested Resampling Rate:'},'questions',{'Resampling Rate'},...
        'answers',{num2str(mean([handles.dx,handles.dz]))});
    if isempty(a)
        set(handles.hmsg,'string','File does not have proper gridspacing');
        set(gcf,'pointer','arrow');
        set(handles.hcrtobjpush,'enable','off');
        set(handles.hundoobjpush,'enable','off');
        set(handles.hcrtobj,'enable','off');
        guidata(hObject,handles);
        return
    else
        handles=resamps(str2double(a{1}));
    end
end
writematrix(hObject,eventdata);
guidata(hObject,handles);
end

function maxminfill(hObject, eventdata)
handles=guidata(hObject);
if isempty(handles.vel)
    set(handles.hmsg,'string','Please Load a Velocity Model');
    set(gcf,'pointer','arrow');
    return
end

if ~any(any(isnan(handles.vel)))
    set(handles.hmsg,'string','There are no NaNs to be replaced in this model');
    set(gcf,'pointer','arrow');
    return
end
indz=find(~isnan(handles.vel(:,ceil(length(handles.x)/4))));
under=ones(21,1);
l=1;
for k=indz(end-20):indz(end)
    sec=handles.vel(k,:);
    under(l,1)=mean(sec(~isnan(sec)));
    l=l+1;
end
under=mean(under);

a=askthingslev2('Name','Replace NaNs with Vertical Variation','title',...
    {'NaNs must be removed to use this tool';
    ['Vertical Variation Limits:',...
    '   Will replace NaNs above model starting'];['   with surface value',...
    ' and ending with the mean of the top of the model.'];...
    '   Will replace NaNs below the model with a Underburden Value'},...
    'questions',{'Surface Velocity','Underburden Velocity'},...
    'answers',{'1800',num2str(under)});
if isempty(a)
    set(handles.hmsg,'string','Replace NaNs with Vertical Variation was Cancelled');
    set(gcf,'pointer','arrow');
    return
else
    ksur=(str2double(a{1}));
    kund=(str2double(a{2}));
end
vmin=ones(20,1);
l=1;
for k=indz(1):indz(20)
    sec=handles.vel(k,:);
    vmin(l,1)=mean(sec(~isnan(sec)));
    l=l+1;
end
vmin=mean(vmin);

k=kund*ones(size(handles.z));
d=handles.z(indz(1))-handles.z(1);
indend=find(handles.z==indz(end)*handles.dz);
k(1:indend)=((((vmin-ksur)/d).*handles.z(1:indend))+ksur);
k(indend:end)=kund;
tempsec=zeros(size(handles.vel));
sm=size(handles.vel);
for ii=1:sm(2)
    int=handles.vel( : ,ii);i=find(isnan(int));
    int(i)=k(i);
    tempsec(:,ii)=int;
end;
handles.vel=tempsec;
set(handles.hmsg,'string','Nans have been removed from section');
set(gcf,'pointer','arrow');
guidata(hObject,handles);
if handles.dx~=handles.dz
    a=askthingslev2('Name','Gridspacing Mismatch','title',...
        {['To use AFD_VELCREATE the gridspacing in',...
        'both x and z directions must be the same.'];
        ['dx is currently ' num2str(handles.dx)];['dz is currently ' num2str(handles.dz)];
        'Suggested Resampling Rate:'},'questions',{'Resampling Rate'},...
        'answers',{num2str(mean([handles.dx,handles.dz]))});
    if isempty(a)
        set(handles.hmsg,'string','File does not have proper gridspacing');
        set(gcf,'pointer','arrow');
        set(handles.hcrtobjpush,'enable','off');
        set(handles.hundoobjpush,'enable','off');
        set(handles.hcrtobj,'enable','off');
        guidata(hObject,handles);
        return
    else
        handles=resamps(str2double(a{1}));
    end
end
guidata(hObject,handles);
writematrix(hObject,eventdata);
end

function linearfill(hObject, eventdata)
handles=guidata(hObject);
if isempty(handles.vel)
    set(handles.hmsg,'string','Please Load a Velocity Model');
    set(gcf,'pointer','arrow');
    return
end

if ~any(any(isnan(handles.vel)))
    set(handles.hmsg,'string','There are no NaNs to be replaced in this model');
    set(gcf,'pointer','arrow');
    return
end
a=askthingsle('Name','Replace NaNs with Linear Function','title',...
    {'NaNs must be removed to use this tool';...
    'Vo+Cz'},'questions',{'Vo                         ','C                 '},'answers',{'1800','0.6'});
if isempty(a)
    set(handles.hmsg,'string','Replace NaNs with Linear Function was Cancelled');
    set(gcf,'pointer','arrow');
    return
else
    vo=(str2double(a{1}));
    c=(str2double(a{2}));
end

k=vo+(c.*handles.z);
tempsec=zeros(size(handles.vel));
sm=size(handles.vel);
for ii=1:sm(2)
    int=handles.vel( : ,ii);i=find(isnan(int));
    int(i)=k(i);
    tempsec(:,ii)=int;
end;
handles.vel=tempsec;
set(handles.hmsg,'string','Nans have been removed from section');
set(gcf,'pointer','arrow');
guidata(hObject,handles);
if handles.dx~=handles.dz
    a=askthingslev2('Name','Gridspacing Mismatch','title',...
        {'To use AFD_VELCREATE the gridspacing in both x and z directions must be the same.';
        ['dx is currently ' num2str(handles.dx)];['dz is currently ' num2str(handles.dz)];
        'Suggested Resampling Rate:'},'questions',{'Resampling Rate'},...
        'answers',{num2str(mean([handles.dx,handles.dz]))});
    if isempty(a)
        set(handles.hmsg,'string','File does not have proper gridspacing');
        set(gcf,'pointer','arrow');
        set(handles.hcrtobjpush,'enable','off');
        set(handles.hundoobjpush,'enable','off');
        set(handles.hcrtobj,'enable','off');
        guidata(hObject,handles);
        return
    else
        handles=resamps(str2double(a{1}));
    end
end
guidata(hObject,handles);
writematrix(hObject,eventdata);
end

function lineset(hObject, eventdata)
handles=guidata(hObject);
a={'Black|White|Blue|Green|Red|Cyan|Magenta|Yellow','Solid Line|Dashed Line',...
    '0.5|1|2|4|6|8'};
a=askthingsle('Name','Object Line Settings','title',...
    {'Object Line Settings'},'questions',{'Choose Line Color',...
    'Choose Line Style','Choose Line Weight'},'answers',a);
if isempty(a)
    set(handles.hmsg,'string','Line Settings was Cancelled');
    set(gcf,'pointer','arrow');
    return
else
    handles.linesettings.color=a{1};
    if strcmp(a{2},'Dashed Line'),handles.linesettings.style='--';
    else handles.linesettings.style='-';end;
    handles.linesettings.weight=str2double(a{3});
end

if ~handles.lc==0
    if ishandle(handles.shape(handles.lc).lineh)
        set(handles.shape(handles.lc).lineh,'color',handles.linesettings.color,...
            'linestyle',handles.linesettings.style,'linewidth',handles.linesettings.weight);
    end
end
guidata(hObject,handles);
end

function colorbarset(hObject, eventdata)
a={'Jet|HSV|Hot|Cool|Spring|Summer|Autumn|Winter|Gray|Bone|Copper|Pink|Lines'};
a=askthingsle('Name','Colorbar Settings','title',...
    {'Colorbar Settings'},'questions',{'Choose Color Variations'},'answers',a);
if isempty(a)
    set(handles.hmsg,'string','Colorbar Cancelled');
    set(gcf,'pointer','arrow');
    return
else
    colormap(a{1});
end
end

function chngvellim(hObject, eventdata)
handles=guidata(hObject);
a=askthingsle('Name','Model Velocity Limits','title',...
    {'Choose Model Velocity  Limits'},'questions',{'Choose Minimum Velocity',...
    'Choose Maximum Velocity'},'answers',{num2str(handles.min),num2str(handles.max)});
if isempty(a{1})|| any( a{1}==-1)
    set(handles.hmsg,'string','Change Velocity Limits Cancelled');
    set(gcf,'pointer','arrow');
    return
else
    handles.max=str2double(a{2});
    handles.min=str2double(a{1});
    handles.mean=mean([handles.max,handles.min]);
    set(handles.hsvel,'Max',handles.max,'Min',handles.min,'Value',handles.mean);
    set(handles.hxvel,'String',num2str(handles.mean));
    handles.vel(1,end)=handles.min;
    handles.vel(end,end)=handles.max;
    guidata(hObject,handles);handles=colorbarsetup;
end
guidata(hObject,handles);
end

function handles=colorbarsetup
handles=guidata(gcf);
if isfield(handles,'colorbar')
    if ~ishandle(handles.colorbar)
        handles.colorbar=colorbar('southoutside');
    end
else handles.colorbar=colorbar;
end
set(handles.colorbar,'UIContextMenu',[]);
if get(handles.colorbar,'xlim')~=[handles.min , handles.max]
    set(handles.colorbar,'CLimMode','manual','Clim',[handles.min,handles.max]);
    handles.vel(1,end)=handles.min;
    handles.vel(end,end)=handles.max;
    hold off;imagesc(handles.x,handles.z,handles.vel);
    handles.colorbar=colorbar('southoutside');hold on;
    set(handles.colorbar,'UIContextMenu',[]);
end
set(handles.colorbar,'units','normalized','Position',[0.1 0.175 0.80 0.06]);
set(handles.colorbar,'units','pixels');
set(handles.hsvel,'units','pixels');
colsz=get(handles.colorbar,'Position');
slisz=get(handles.hsvel,'Position');
slisz=[colsz(1)-16,slisz(2),colsz(3)+32,slisz(4)];
set(handles.hsvel,'position',slisz);
set(handles.colorbar,'units','normalized');
set(handles.hsvel,'units','Normalized');
colsz=get(handles.colorbar,'Position');
slisz=get(handles.hsvel,'Position');
slisz=[slisz(1),colsz(2)+colsz(4)/3,slisz(3),colsz(4)/3];
set(handles.hsvel,'position',slisz);
guidata(gcf,handles);
end

function makebig(hObject, eventdata)
handles=guidata(hObject);
bigfig(gcf);
set(handles.colorbar,'units','pixels');
set(handles.hsvel,'units','pixels');
colsz=get(handles.colorbar,'Position');
slisz=get(handles.hsvel,'Position');
slisz=[colsz(1)-16,slisz(2),colsz(3)+32,slisz(4)];
set(handles.hsvel,'position',slisz);
set(handles.colorbar,'units','normalized');
set(handles.hsvel,'units','Normalized');

if strcmp(get(hObject,'label'),'All Display Options ON')
    boldlines(gcf,4,2);
    bigfont(gcf,2,2);
    set(handles.hpanel,'BackgroundColor',[1 1 1]);
end
end

function makesmall(hObject, eventdata)
handles=guidata(hObject);
smallfig(gcf,[0.25 0.225 0.5 0.55])
set(handles.colorbar,'units','pixels');
set(handles.hsvel,'units','pixels');
colsz=get(handles.colorbar,'Position');
slisz=get(handles.hsvel,'Position');
slisz=[colsz(1)-16,slisz(2),colsz(3)+32,slisz(4)];
set(handles.hsvel,'position',slisz);
set(handles.colorbar,'units','normalized');
set(handles.hsvel,'units','Normalized');

if strcmp(get(hObject,'label'),'All Display Options OFF')
    boldlines(gcf,.25,.5);
    bigfont(gcf,.5,1);
    set(handles.hpanel,'BackgroundColor',[0.941176 0.941176 0.941176]);
end
end

function writematrix(hObject, eventdata)
handles=guidata(hObject);
% write matrix to temporary vel file
sz=size(handles.vel);
fwrite(handles.fid,'matrix:%dx=','char');fwrite(handles.fid,handles.dx,'float');
fwrite(handles.fid,'%dz=','char');fwrite(handles.fid,handles.dz,'float');
fwrite(handles.fid,'%size=','char');fwrite(handles.fid,sz,'int');
fwrite(handles.fid,'%vel=','char');
fwrite(handles.fid,handles.vel,'float');
fwrite(handles.fid,'%','char');

% write matrix to temporary m file
z=0:handles.dz:length(handles.z)*handles.dz;
fwrite(handles.fidm,['%% Background Matrix:',char(13)],'char');
fwrite(handles.fidm,['fdx=',num2str(handles.dx),';',char(13)],'char');
fwrite(handles.fidm,['fdz=',num2str(handles.dz),';',char(13)],'char');
fwrite(handles.fidm,['fx=[',num2str(handles.x),'];',char(13)],'char');
fwrite(handles.fidm,['fz=[',num2str(z),'];',char(13)],'char');
fwrite(handles.fidm,'fvel=[','char');
for k=1:sz(1)-1;
fwrite(handles.fidm,[num2str(handles.vel(k,:)),';',char(13)],'char');
end
fwrite(handles.fidm,[num2str(handles.vel(sz(1),:)),'];',char(13)],'char');

guidata(hObject,handles);

end

function writecommand(hObject,com)
handles=guidata(hObject);
% write command to temporary vel file
sz=size(com);
fwrite(handles.fid,'comman:%size=','char');fwrite(handles.fid,sz,'int');
str=['%',com,'%'];
fwrite(handles.fid,str,'char');
% write command to temporary m file
fwrite(handles.fidm,['%% Background Model:',char(13)],'char');
if ~findstr(com,'l=load')
z=0:handles.dz:length(handles.z)*handles.dz;
fwrite(handles.fidm,['fdx=',num2str(handles.dx),';',char(13)],'char');
fwrite(handles.fidm,['fdz=',num2str(handles.dz),';',char(13)],'char');
fwrite(handles.fidm,['fx=[',num2str(handles.x),'];',char(13)],'char');
fwrite(handles.fidm,['fz=[',num2str(z),'];',char(13)],'char');
end
fwrite(handles.fidm,[com,char(13)],'char');
guidata(hObject,handles);
end

function writeshape(hObject,velstruct)
handles=guidata(hObject);
sz=size(velstruct.xpoly);
% write shape to temporary vel file
fwrite(handles.fid,'shapes:%','char');
fwrite(handles.fid,'shapenum=','char');fwrite(handles.fid,handles.shpcount,'int');
fwrite(handles.fid,'%size=','char');fwrite(handles.fid,sz,'int');
fwrite(handles.fid,'%x=','char');
fwrite(handles.fid,velstruct.xpoly,'float');
fwrite(handles.fid,'%z=','char');
fwrite(handles.fid,velstruct.zpoly,'float');
fwrite(handles.fid,'%v=','char');
fwrite(handles.fid,velstruct.vpoly,'float');
fwrite(handles.fid,'%','char');

% write shape to temporary shp file
fwrite(handles.fidshp,[num2str(velstruct.vpoly),',']);
fwrite(handles.fidshp,['[',num2str(velstruct.xpoly),']',',']);
fwrite(handles.fidshp,['[',num2str(velstruct.zpoly),']',char(13)]);

% write shape to temporary m file

str=['fvel=afd_vmodel(',num2str(handles.dx),',fvel,',num2str(velstruct.vpoly),',[',...
    num2str(velstruct.xpoly),'],[',num2str(velstruct.zpoly),']);',char(13)];
fwrite(handles.fidm,['%Shape',num2str(handles.shpcount),':',char(13)],'char');
fwrite(handles.fidm,str,'char');

handles.shpcount=handles.shpcount+1;

guidata(hObject,handles);

end

function [nulflag velfilestruct]=editvelfile(velfilestruct)
handles=guidata(gcf);h.masterfig=handles.hmasterfig;

p=findstr(velfilestruct.order,'%');
h.data=cell(length(p)-1,4);
order=velfilestruct.order;
for l=1:length(p)-1
    otype=order(p(l)+1);
    n=str2num(order((p(l)+2):(p(l+1)-1)));
    if strcmp(otype,'m')
        h.data{l,1}=false;
        h.data{l,2}=['Matrix: ',num2str(n)];
        h.data{l,3}=['Matrices Cannot Be Edited'];
        h.data{l,4}=[' '];
        h.data{l,5}=[' '];
    elseif strcmp(otype,'c');
        h.data{l,1}=false;
        h.data{l,2}=['Command: ',num2str(n)];
        h.data{l,3}=[velfilestruct.comman{n}.command];
        h.data{l,4}=[' '];
        h.data{l,5}=[' '];
    elseif strcmp(otype,'s');
        h.data{l,1}=false;
        h.data{l,2}=['Shape: ',num2str(n)];
        h.data{l,3}=['[',num2str(velfilestruct.shapes{n}.vpoly),']'];
        h.data{l,4}=['[',num2str(velfilestruct.shapes{n}.xpoly),']'];
        h.data{l,5}=['[',num2str(velfilestruct.shapes{n}.zpoly),']'];
    end
end
h.dlg=figure('Menu','none','units','normalized','position',...
    [0.225 0.225 0.55 0.55],'closerequestfcn',@closedlg,'Name',...
    'Change','NumberTitle','off');
h.table=uitable(h.dlg,'units','normalized','position',[.02 .60 .96 .38],...
    'data',h.data,'columnname',{'Plot?','Data Type','Velocity','Coordinates in X','Coordinates in Z'},...
    'rowname',[],'columneditable',[true,false,true,false,false],...
    'columnwidth',{50,80 200 200 200});
h.axes=subplot('position',[.10 .05 .5 .45]);axis ij ;

h.plotbut=uicontrol('units','normalized','position',[.70 .40 .25 .1],...
    'style','push','string','Plot Shapes','callback',@plotshape);
h.okbut=uicontrol('units','normalized','position',[.70 .225 .25 .1],...
    'style','push','string','OK','callback',@clickok);
h.canbut=uicontrol('units','normalized','position',[.70 .05 .25 .1],...
    'style','push','string','Cancel','callback',@closedlg);
nulflag=0;
guidata(h.dlg,h);
uiwait(h.dlg)
delete(h.dlg)
return;

    function closedlg(hObject, eventdata)
        h=guidata(hObject);
        nulflag=0;
        uiresume;
    end

    function clickok(hObject,eventdata)
        h=guidata(hObject);
        
        tmpdata=get(h.table,'data');
        sz=size(tmpdata);
        for k=1:sz(1)
            if findstr(tmpdata{k,2},'Shape:')
                tmpvel=str2num(tmpdata{k,3});
                if ~isempty(tmpvel)
                    h.data{k,3}=tmpvel(1);
                end
            elseif findstr(tmpdata{k,2},'Command:')
                h.data{k,3}=tmpdata{k,3};
            end
        end
        
        p=findstr(velfilestruct.order,'%');
        order=velfilestruct.order;
        for k=1:length(p)-1
            otype=order(p(k)+1);
            n=str2num(order((p(k)+2):(p(k+1)-1)));
            if strcmp(otype,'m')
            elseif strcmp(otype,'c');
                velfilestruct.comman{n}.command=h.data{k,3};
            elseif strcmp(otype,'s');
                h.data{k,1}=['Shape: ',num2str(n)];
                velfilestruct.shapes{n}.vpoly=(h.data{k,3});
                velfilestruct.shapes{n}.xpoly=str2num(h.data{k,4});
                velfilestruct.shapes{n}.zpoly=str2num(h.data{k,5});
            end
        end
        nulflag=1;
        uiresume;
    end

    function plotshape(hObject,eventdata)
        h=guidata(hObject);
        dat=get(h.table,'data');
        sz=size(dat);
        ckv=ones(sz(1),1);
        for m=1:sz(1)
            ckv(m,1)=dat{m,1};
        end
        ck=find(ckv==true);
        flag=0;
        hold off;
        for m=1:length(ck)
            if ~isempty(findstr(dat{ck(m),2},'Command:')) || ~isempty(findstr(dat{ck(m),2},'Matrix:'))
                flag=1;
            else
                hold on;
                v=str2num(dat{ck(m),3});
                if isempty(v);
                    warndlg(['Shape:',num2str(ck(m)),' Has an Invalid Velocity']);
                end
                fill(str2num(dat{ck(m),4}),str2num(dat{ck(m),5}),v(1));
            end
        end;
        if flag
            warndlg('Commands and Matricies Cannot be Plotted','Plotting Error');
        end
        colorbar;
        guidata(hObject,h);
    end
end

function importshapes(hObject,eventdata)
handles=guidata(hObject);
if isempty(handles.vel)
    warndlg('Background Velocity Model Must Be Created First',...
        'Missing Background Velocity');
    return
end
%get the file name
[filename,path]=uigetfile('*.shp','Select Shapes File');
if(filename(1)==0 )
    set(handles.hmsg,'string','no file name given');
    set(gcf,'pointer','arrow');
    return;
end
set(gcf,'pointer','watch');

fullfilename = [path filename];
set(handles.hmsg,'string',' now reading file...');
fidin=fopen(fullfilename,'r',handles.machineformat);
all=fread(fidin,'char')';

eol=find(all==13);
if ~isempty(eol)
    k=[];
    for l=2:length(eol)
        if eol(l)-eol(l-1)>2
            k=[k l];
        end
    end
    if eol(1)~=1
        k=[1 k];
    end
    eol=eol(k);
    all(eol)=44;
end
comma=find(all==44);
rot=length(comma)/3;
if round(rot)~=rot
    warndlg('File does not have proper formating');
    return;
end
all=[44 all];
comma=find(all==44);
if comma(end)~=length(all)
    comma=[comma length(all)+1];
end
shapes.order='%';
for l=1:rot
    m=(l-1)*3;
    shapes.shapes{l}.vpoly=str2num(char(all(comma(1+m)+1:comma(2+m)-1)));
    shapes.shapes{l}.xpoly=str2num(char(all(comma(2+m)+1:comma(3+m)-1)));
    shapes.shapes{l}.zpoly=str2num(char(all(comma(3+m)+1:comma(4+m)-1)));
    shapes.order=[shapes.order,'s',num2str(l),'%'];
end
[nulflag shapes]=editvelfile(shapes);
if nulflag
    for l=1:length(shapes.shapes)
        if length(shapes.shapes{l}.xpoly)>length(shapes.shapes{l}.zpoly)
            shapes.shapes{l}.xpoly=shapes.shapes{l}.xpoly(1:length(shapes.shapes{l}.zpoly));
        elseif length(shapes.shapes{l}.xpoly)<length(shapes.shapes{l}.zpoly)
            shapes.shapes{l}.zpoly=shapes.shapes{l}.zpoly(1:length(shapes.shapes{l}.xpoly));
        end
        if (shapes.shapes{l}.xpoly(1) ~= shapes.shapes{l}.xpoly(end)) || shapes.shapes{l}.zpoly(1) ~= shapes.shapes{l}.zpoly(end)
            shapes.shapes{l}.xpoly=[shapes.shapes{l}.xpoly , shapes.shapes{l}.xpoly(1)];
            shapes.shapes{l}.zpoly=[shapes.shapes{l}.zpoly , shapes.shapes{l}.zpoly(1)];
        end
        if isnan(shapes.shapes{l}.vpoly)
            shapes.shapes{l}.vpoly=2500;
        end
    end
    p=findstr(shapes.order,'%');
    for l=1:length(p)-1
        n=str2num(shapes.order((p(l)+2):(p(l+1)-1)));
        handles.vel=afd_vmodel(handles.dx,handles.vel,shapes.shapes{n}.vpoly,...
            shapes.shapes{n}.xpoly,shapes.shapes{n}.zpoly);
        guidata(hObject,handles);
        writeshape(hObject,shapes.shapes{n});
        handles=guidata(hObject);
    end
    set(handles.hmsg,'string','Shape File Has Been Loaded');
    set(gcf,'pointer','arrow');
    hold off;imagesc(handles.x,handles.z,handles.vel);
    guidata(hObject,handles);handles=colorbarsetup;hold on;
    guidata(hObject,handles);
    
else
    set(handles.hmsg,'string','Load Shape File Was Cancelled');
    set(gcf,'pointer','arrow');
end
end

function exportshapes(hObject,eventdata)
        handles=guidata(hObject);
        [filename,path]=uiputfile('*.shp','Save Shape file As');
        if(filename(1)==0 )
            set(handles.hmsg,'string','no file name given');
            set(gcf,'pointer','arrow');
            return;
        end
        set(gcf,'pointer','watch');
        fullfilename = [path filename];
        copyfile(handles.shp,fullfilename);
        set(gcf,'pointer','arrow');
end

function generatem(hObject,eventdata)
handles=guidata(hObject);
%get the file name
[filename,path]=uiputfile('*.m','Select *.m File Name');
if(filename(1)==0 )
    set(handles.hmsg,'string','no file name given');
    set(gcf,'pointer','arrow');
    return;
end
set(gcf,'pointer','watch');
fullfilename=[path,filename];
fid=fopen(fullfilename,'w',handles.machineformat);
fwrite(fid,['% ',filename,char(13),'% Velocity model created by AFD_VELCREATE on ',...
    datestr(clock),char(13)],'char');
fclose(handles.fidm);
handles.fidm=fopen(handles.mfile,'r');

c=fread(handles.fidm,1,'*char');
while ~isempty(c)
    c=fread(handles.fidm,1,'*char');
    if ischar(c)
    fwrite(fid,c,'char');
    end
end
fclose(fid);
fclose(handles.fidm);
handles.fidm=fopen(handles.mfile,'a');
fseek(handles.fidm,0,'eof');
set(handles.hmsg,'string',[filename,' has been generated']);
    set(gcf,'pointer','arrow');

end

function startnewfiles(hObject, eventdata)
handles=guidata(hObject);
fclose(handles.fid);
fclose(handles.fidshp);
fclose(handles.fidm);
handles.fid=fopen(handles.tmp,'w');
handles.fidshp=fopen(handles.shp,'w');
handles.fidm=fopen(handles.mfile,'w');
handles.shpcount=1;
guidata(hObject,handles);
end

function chngmachfmt(hObject, eventdata)
handles=guidata(hObject);

str={'Normal (current format of this computer)';...
    'ieee–be (big-endian)';'ieee-le (little-endian)';...
    'ieee-be.l64 (big-endian 64-bit data type)';...
    'ieee-le.l64 (little-endian 64-bit data type)'};
ps={'The Precision Format should be chosen for the computer that the file was created';...
    'ieee-be can be selected for most PC''s';' ieee-le can be selected for most Linux machines'};
[s,v] = listdlg('PromptString','Select Precision Format Of File:',...
    'SelectionMode','single','ListSize',[500 200],...
    'ListString',str,'PromptString',ps);

if v==0
    fmt='n';
elseif s==1
    fmt='normal';
elseif s==2
    fmt='ieee-be';
elseif s==3
    fmt='ieee-le';
elseif s==4
    fmt='ieee-be.l64';
elseif s==5
    fmt='ieee-le.l64';
end
handles.machineformat=fmt;
guidata(hObject,handles);
end

function chngmatfmt(hObject, eventdata)
handles=guidata(hObject);

str={'Save Matrix in Load Format (Requires *.mat File)';...
     'Save Matrix in Matrix Format (Produces Larger File Self-Sufficent';};
ps={'This allows the user to select the format that they would like *.mat files',...
    'to be defined as in generate m code and save *.vel file'};
[s,v] = listdlg('PromptString','Select Save Format Of Matricies:',...
    'SelectionMode','single','ListSize',[500 200],...
    'ListString',str,'PromptString',ps);

if v==0
    set(handles.hmattog,'userdata',1);
elseif s==1
    set(handles.hmattog,'userdata',1);
elseif s==2
    set(handles.hmattog,'userdata',0);
end
guidata(hObject,handles);
end

%% Help Functions
    function helps(hObject, eventdata)
        handles=guidata(hObject);
        if hObject==handles.hvers
            msgbox('AFD_Velcreate Version 2.0');
        end
        
        if hObject==handles.hlicence
            msgbox({'This SOFTWARE is maintained by the CREWES Project at the Department';
                'of Geology and Geophysics of the University of Calgary, Calgary';
                'Alberta, Canada.  The copyright and ownership is jointly held by';
                'its author (identified above) and the CREWES Project.  The CREWES ';
                'project may be contacted via email at:  crewesinfo@crewes.org';
                ' ';
                'The term ''SOFTWARE'' refers to the Matlab source code, translations to';
                'any other computer language, or object code';
                ' ';
                'Terms of use of this SOFTWARE';
                '';
                '1) Use of this SOFTWARE by any for-profit commercial organization is';
                '   expressly forbidden unless said organization is a CREWES Project';
                '   Sponsor.';
                '';
                '2) A CREWES Project sponsor may use this SOFTWARE under the terms of the';
                '   CREWES Project Sponsorship agreement.';
                '';
                '3) A student or employee of a non-profit educational institution may ';
                '   use this SOFTWARE subject to the following terms and conditions:';
                '   - this SOFTWARE is for teaching or research purposes only.';
                '   - this SOFTWARE may be distributed to other students or researchers';
                '     provided that these license terms are included.';
                '   - reselling the SOFTWARE, or including it or any portion of it, in any';
                '     software that will be resold is expressly forbidden.';
                '   - transfering the SOFTWARE in any form to a commercial firm or any';
                '     other for-profit organization is expressly forbidden.'},'Licence Information');
            
        end
        
        if hObject==handles.hhelps
            if isfield(handles,'help')
                if ~isempty(handles.help)
                    close(handles.help);
                    handles.help=[];
                end
            end
            handles.help=figure('menu','none','Numbertitle','off','name','Help Menu',...
                'units','normalized','position',[.3 .25 .4 .5],'CloseRequestFcn',@closehelp);
            uipanel(handles.help,'units','normalized','position',[0 0 1 1]);
            handles.helptopics=uicontrol(handles.help,'Style','text','units','normalized',...
                'position',[0 .9 .30 .1],'Fontsize',14,'Fontweight','Bold','String',{' ';'Help Topics:'});
            handles.helplist=uicontrol(handles.help,'Style','listbox','units','normalized',...
                'position',[0 0 .30 .9],'Callback',@gethelp,'Fontsize',10,'Value',1);
            set(handles.helplist,'string',{'About AFD_Velcreate';...
                'Saving and Loading Files';'Edit Tools';'Vel Files';'Shape Files'});
            handles.helpinfotlt=uicontrol(handles.help,'Style','text','units',...
                'normalized','position',[.33 .9 .67 .1],'Fontsize',14,'Fontweight','Bold');
            handles.helpinfo=uicontrol(handles.help,'Style','listbox','units',...
                'normalized','position',[.33 0 .67 .9],'Fontsize',10);
            
        end
        
        guidata(handles.hmasterfig,handles);
        
        function gethelp(hObject, eventdata)
            val=get(hObject,'value');
            if val==1
                set(handles.helpinfotlt,'string',{' ';'About AFD_Velcreate'});
                helpinfo={'AFD_Velcreate helps the user build a velocity model ';
                    '  using polygonal shapes that are created and then modifed ';
                    '  using the modify and smooth functions of editlines.';
                    ' ';
                    'As a tool of the finite differencing toolbox, AFD_Velcreate ';
                    '  adheres to the following assumptions:';
                    '          - That the model begins at (0,0)';
                    '          - That the gridspacing is the same in both the x'
                    '               and z directions';
                    '';};
                
                set(handles.helpinfo,'String',helpinfo);
            end
            if val==2
                set(handles.helpinfotlt,'string',{' ';'Saving and Loading Files'});
                helpinfo={'AFD_Velcreate can open files that have been created';
                    '  with velcreate or have been saved in the folowing format:';
                    '';
                    '       vel or sec = the velocity section that has'
                    '                         dimensions (z,x)';
                    '       x  = the inline vector';
                    '       z  = the depth vector';
                    '       dx = grid spacing in the x direction';
                    '       dz = grid spacing in the z direction';
                    '';
                    'If the file is not saved with these variables call'
                    '  afd_velcreate with the following syntax';
                    '       afd_velcreate(''velocity model'',''dx'',''dz'')';
                    '';
                    'When a file is saved using afd_velcreate the above ';
                    '  variables are saved.';
                    '';
                    '';
                    'To load a velocity model for use with another function it '
                    '  is suggested that the following be used such that other ';
                    '  variables are not overridden';
                    '';
                    '        k=load( '' filename.mat '' );';
                    '        section=k.sec;';
                    '        xvec=k.x;';
                    '        zvec=k.z;';
                    '        dx=k.dx;';
                    '';
                    '';
                    ''};
                set(handles.helpinfo,'String',helpinfo);
            end
            if val==3
                set(handles.helpinfotlt,'string',{' ';'Edit Tools'});
                helpinfo={'The editing toolbox consists of two main tools:';
                    '      -MODIFY is the function that allows the user to add, ';
                    '             delete and move points';
                    '      -SMOOTH is the function that will create curvature in ';
                    '             the object';
                    '';
                    'The MODIFY function has several functions.';
                    '     Mouse Button 1:';
                    '         -The first click on an object, selects that object.';
                    '         -Additonal clicks will add points to the object.';
                    '         -Click and Drag motion will move a point.';
                    '     Mouse Button 2:';
                    '         -Clicking on a point will anchor that point.';
                    '         -Click and Drag motion will move the entire Object';
                    ' ';
                    '     To delete a point, click on the point with the third ';
                    '        mouse button.  If the mouse only has two buttons press';
                    '        them both simutaneously';
                    '';
                    'The SMOOTH fuction can be tricky and ocasionally do random'
                    '  things To control it anchor at least three points before using'
                    '  the smooth function and click step by step such that if it'
                    '  does go astray it will be easier to correct.  To distribute '
                    '  the points evenly along two sides click the midpoint';
                    '';
                    ''};
                set(handles.helpinfo,'String',helpinfo);
            end
            if val==4
                set(handles.helpinfotlt,'string',{' ';'Vel Files'});
                helpinfo={'Vel files contain all the information that is needed to';
                    '  recreate a model.  When a file is saved as a vel all of ';
                    '  the data is saved in a non-editable format.  ';
                    '';
                    'Warning: Please do not try to edit or save this file out of';
                    '                       AFD_VELCREATE.';
                    '';
                    'When loading a vel file you can change backround comands ';
                    '  and shape velocities only.  ';'';
                    'IT IS NOT RECOMENDED TO CHANGE COMMAND LINES';
                    ' UNLESS YOU ARE EXPERIENCED IN USING THE TOOL.';
                    '';
                    'If you would like to have more control over the positions ';
                    '  of the shapes it is recomended that you use the ';
                    '  import/export shape tools.';
                    '';
                    ''};
                set(handles.helpinfo,'String',helpinfo);
            end
            if val==5
                set(handles.helpinfotlt,'string',{' ';'Shape Files'});
                helpinfo={'Shape files contain shape data in a comma delimiated file.';
                    '  This data can be created using any text editor or ';
                    '  spreadsheet program as long as the file is saved as comma ';
                    '  delimited.  ';'';
                    'Shape files must be formatted in the following format:';
                    '    -Every new shape must be on a separate line'; 
                    '    -The data must be in this order velocity, X coordinates, ';
                    '                Z coordinates.';
                    '    -Commas must separate velocity, X coordinates, ';
                    '                Z coordinates.';
                    '    -No commas must be used to separate values in the ';
                    '                X coordinates, or Z coordinates.';
                    '    -X and Z coordinate vectors must be the same length';
                    '    -X and Z coordinates must be contained by square brackets';
                    '    -No other data can exist in the file';
                    '    -Both the X and Z coordinate vector must end where';
                    '                they started';
                    '';
                    'Example:';
                    '   2400,[300 300 200 200 300],[400 500 500 400 400]';
                    '   3000,[400 500 700 900 200 400],[200 100 700 300 100 200]';
                    '';
                    ''};
                set(handles.helpinfo,'String',helpinfo);
            end
        end
        
        function closehelp(hObject, eventdata)
            delete(handles.help);
            handles.help=[];
            guidata(handles.hmasterfig,handles);
        end
    end