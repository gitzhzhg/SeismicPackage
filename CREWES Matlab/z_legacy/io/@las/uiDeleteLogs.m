function obj = uiDeleteLogs(obj, sn, msg)
%
% function obj = uideletelogs(obj,sn,msg)
%
% UIDELETELOGS is a GUI-based utility designed to allow the deletion of logs
% in LAS files. This is useful for files that contain large numbers of logs
% because LOGEDIT does not work well with more than about 20 logs.
%
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


if nargin < 2 || nargin > 3
    warning('crewes:las:uideletelogs','Not enough input parameters');
    return
elseif isequal(nargin,2)
    msg = '';
end

hfig=figure('name','las.uideletelogs','numbertitle','off','units','normalized','position',[.3 .3 .6 .4]);
set(hfig,'menubar','none');
set(hfig,'closerequestfcn',@closereq_callback);

sidx = obj.getSectionIndices(sn);

%%only accept one data section for now
if numel(sidx) > 1
    delete(hfig)
    warning('crewes:las:uideletelogs',...
        ['String ''' sn ''' matches more than one section']);
    return   
end

if ~obj.isDataSection(sidx)
    delete(hfig)
    warning('crewes:las:uideletelogs',...
        ['String ''' sn ''' does not match a data section']);
    return
end

tabledata = cell(1,0);
for k = 1:numel(sidx)
   
    td=obj.getSectionData(obj.getSectionAssociation(sidx(k)))';

    td(:,3)=td(:,2);
    td(:,2)=td(:,1);
    td(:,1)={false};
    if (k > 1)
        td(:,1)={true};
    end
    td(:,5)=obj.sectionNames(1,sidx(k));
    td(:,6)=obj.sectionNames(2,sidx(k));
    
    tabledata = vertcat(tabledata, td);
end

%Find indices of DEPTH and ETIM logs
dscale = strncmpi(td(:,2),'DEPT',4) | strncmpi(td(:,2),'ETIM',4);

if isequal(numel(tabledata),0)
   delete(hfig) 
   warning('crewes:las:uideletelogs','No logs found in data section');
   return
end

dwidth = max(cellfun('length',tabledata(:,4:6)));
ld  = 'Description';
das = 'Data Section';
des = 'Definition Section';
ld  = [ld  char(32*ones(1,2*dwidth(1)-length(ld)))];
das = [das char(32*ones(1,2*dwidth(2)-length(das)))];
des = [des char(32*ones(1,2*dwidth(3)-length(des)))];

colnames={'Delete','Mnemonic','Units',ld,das,des};
colformat={'logical','char','char','char','char','char'};
columneditable =  [true false false false false false];
posn=[.1 .1 .7 .8];
uitable('Parent',hfig,'Tag','logs_table','data',tabledata,...
    'userdata',dscale,...
    'columnname',colnames,...
    'columnformat',colformat,'columneditable',columneditable,...
    'rowname',[],'units','normalized','position',posn,...
    'celleditcallback',@celledit_callback);

%add some buttons
xnow=.8;ynow=.8;width=.1;height=.1;
uicontrol('Parent',hfig,'Tag','checkall_button','style','pushbutton','string','Check All',...
    'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height],...
    'callback',@check_all_callback);

xnow=.8;ynow=.7;width=.1;height=.1;
uicontrol('Parent',hfig,'Tag','uncheckall_button','style','pushbutton','string','Uncheck All',...
    'callback',@uncheck_all_callback,'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height]);

xnow=.8;ynow=.6;width=.1;height=.1;
uicontrol('Parent',hfig,'Tag','next_button','style','pushbutton','string','Next...',...
    'callback',@next_callback,'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height]);

xnow=.8;ynow=.5;width=.1;height=.1;
uicontrol('Parent',hfig,'Tag','cancel_button','style','pushbutton','string','Cancel',...
    'callback',@closereq_callback,'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height]);

xnow=.8;ynow=.1;width=.1;height=.4;
uicontrol('Parent',hfig,'Tag','msg_text','style','text','string',msg,...
    'visible','on','units','normalized','position',[xnow,ynow,width,height])

%put a title at the top
xnow=.1;ynow=.95;width=.7;height=.05;
uicontrol('Parent',hfig,'Tag','filename_text','style','text','string',['File:' ' ' obj.fileName],...
    'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height]);
xnow=.1;ynow=.9;width=.7;height=.05;

if str2double(obj.version) < 3.0
    well_parameter = '~w';
else
    well_parameter = '~well';
end
name   = obj.getMnemonicValue(well_parameter,'WELL');
wellid = obj.getMnemonicValue(well_parameter,'UWI');
uicontrol('Parent',hfig,'Tag','wellid_text','style','text','string',[name ' ' wellid],...
    'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height]);
xnow=.1;ynow=.05;width=.7;height=.05;

nlogs = obj.numLogs(sn);

uicontrol('Parent',hfig,'Tag','nlogs_to_delete_text','style','text','string',...
    ['0 of ' int2str(nlogs) ' logs have been selected for deletion'],...
    'visible','on',...
    'units','normalized','position',[xnow,ynow,width,height]);

uicontrol('Parent',hfig,'Tag','done_text','style','text','string',...
    '',...
    'visible','off',...
    'units','normalized','position',[xnow,ynow,width,height]);

handles = guihandles(hfig); %create struct based on 'Tag' property
guidata(hfig,handles);      %save struct in hfig

set(handles.done_text,'UserData',0);
set(handles.next_button,'UserData',obj);

waitfor(handles.done_text,'UserData');

t = get(handles.done_text,'UserData');
switch t
    case 1 %cancel button clicked, or figure close
        warning('crewes:las:uideletelogs','User canceled log deletion')
    case 2 %logs edited
        obj = get(handles.next_button,'UserData');
end
delete(hfig);
    
end %end function

function closereq_callback(varargin)
handles = guidata(gcbo);
logs2delete = get(handles.nlogs_to_delete_text,'UserData');

if(logs2delete~=0)
    selection = questdlg(char('Are you sure you want to close this window?','No logs will be deleted.'),...
        'Exit confirmation',...
        'Yes','No','Yes');
    switch selection,
        case 'Yes',
            set(handles.done_text,'UserData',1);
        case 'No'
            return
    end
else
    set(handles.done_text,'UserData',1);
end
    guidata(gcbo);
end %end function closereq_callback

function check_all_callback(varargin)
    handles = guidata(gcbo);
    %check all
    d = get(handles.logs_table,'data');
    ds = get(handles.logs_table,'userdata');    

    d(:,1) = {true};
    d(ds,1) = {false}; %Make sure we can't delete depth of time logs
    
    nlogs = length(d);

    set(handles.logs_table,'data',d);
    %update message
    set(handles.nlogs_to_delete_text,'string',...
        [num2str(nlogs-sum(ds)) ' of ' num2str(nlogs) ' logs have been selected for deletion']);
    set(handles.nlogs_to_delete_text,'UserData',nlogs);
    guidata(gcbo);
end %end function check_all_callback

function uncheck_all_callback(varargin)
    handles = guidata(gcbo);
    d = get(handles.logs_table,'data');
    nlogs = length(d);
    d(:,1) = {false};
    set(handles.logs_table,'data',d);
    set(handles.nlogs_to_delete_text,'string',...
        ['0 of ' num2str(nlogs) ' logs have been selected for deletion']);
    set(handles.nlogs_to_delete_text,'UserData',0);
    guidata(gcbo);
end %end function uncheck_all_callback

function celledit_callback(varargin)
    handles = guidata(gcbo);
    d  = get(handles.logs_table,'data');
    ds = get(handles.logs_table,'userdata');   
    
    d(ds,1) = {false}; %Make sure we can't delete any depth or time logs
    set(handles.logs_table,'data',d);
    
    nlogs = length(d);
    ndel_logs = sum(cell2mat((d(:,1))));
    
    set(handles.nlogs_to_delete_text,'string',...
        [num2str(ndel_logs) ' of ' num2str(nlogs) ' logs have been selected for deletion']);
    set(handles.nlogs_to_delete_text,'UserData',ndel_logs);
    guidata(gcbo);
end %end function celledit_callback

function next_callback(varargin)
    handles = guidata(gcbo);
    las_obj = get(handles.next_button,'UserData');

    tabledata = get(handles.logs_table,'data');
     
    %Get log(s) to delete
    logs_to_delete = cell2mat(tabledata(:,1));
    
    %Get section name(s)    
    section_names = unique(tabledata(:,5));
        
    %Delete logs
    for n = 1:numel(section_names)
        b = strcmp(section_names(n),tabledata(:,5));
        las_obj = ...
            las_obj.deleteLogs(logs_to_delete(b),section_names(n));
    end
    
    set(handles.next_button,'UserData',las_obj);
    set(handles.done_text,'UserData',2);
    guidata(gcbo);
        
end %end function next_callback