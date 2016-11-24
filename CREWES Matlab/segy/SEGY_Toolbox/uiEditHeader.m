function header=uiEditHeader(varargin)
% header=uiEditHeader();
% texthead=uiEditHeader(texthead);
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

% create the Gui
handles.figure1=figure('visible','off','numberTitle','off','Name','Edit Header',...
    'position',[300 200 640 420],'menubar','none','closerequestfcn',@close_Callback);

%menus
handles.file=uimenu(handles.figure1,'label','File');
handles.newsegy=uimenu(handles.file,'label','New Header From Template','Callback',@newsegy_Callback);
handles.newblank=uimenu(handles.file,'label','New Blank Header','Callback',@newblank_Callback);
handles.open=uimenu(handles.file,'label','Open','callback',@open_Callback);
handles.save=uimenu(handles.file,'label','Save','callback',@save_Callback);
handles.close=uimenu(handles.file,'label','Exit','callback',@close_Callback,...
    'separator','on');
%table, buttons and panels
handles.uitable1=uitable(handles.figure1,'position',[5 5 630 350]);
handles.rbpanel=uipanel(handles.figure1,'units','pixels','position',[60 370 520 40]);
handles.rb_ascii=uicontrol(handles.rbpanel,'style', 'radio','callback',...
    @rb_ascii_Callback,'String','ASCII Format','position',[100 10 100 20]);
handles.rb_ebcdic=uicontrol(handles.rbpanel,'style', 'radio','callback',...
    @rb_ebcdic_Callback,'String','EBCDIC Format','position',[300 10 100 20]);

% comands before opening
handles.obj=[];
if nargin>0
    handles.obj=varargin{1};
    handles.header=handles.obj.header;
    handles.format=handles.obj.format;
    handles.header(:,81) = '.';
    set(handles.uitable1,'Data',cellstr(handles.header));
    if strcmpi(handles.format,'ascii');
        set(handles.rb_ascii,'Value',1.0);
    else
        set(handles.rb_ebcdic,'Value',1.0);
    end
else
    [handles.header handles.format] = TextHeader.newHeader;
    handles.header = TextHeader.reshapeHeader(handles.header,'2D');
    set(handles.uitable1,'Data',[]);
end
set(handles.uitable1,'FontName','fixedwidth');
set(handles.uitable1,'ColumnWidth',{600},'columneditable',[true],'columnname',...
    'When editing this header please ensure there are ONLY 40 characters per line', ...
    'rowname',[]);

set(handles.figure1,'visible','on');
guidata(gcf,handles);
uiwait(handles.figure1)
handles=guidata(gcf);

th=char(get(handles.uitable1,'Data'));
if ~isempty(th) 
    th=th(:,1:80);
    if ~isempty(handles.obj)
        handles.obj.header=th;
        handles.obj.format=handles.format;
        header=handles.obj;
    else
        header=th;
    end
else
    header=[];
end
close all hidden
delete(handles.figure1);




% UIWAIT makes uiEditHeader wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = uiEditHeader_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --------------------------------------------------------------------
function file_Callback(hObject, eventdata, handles)
% hObject    handle to file (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --------------------------------------------------------------------
function save_Callback(hObject, eventdata, handles)
% hObject    handle to save (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
[fname, pname] = uiputfile( ...
    {'*.txt;','SEGY Textual header file (*.txt)';
    '*.*','All Files (*.*)'}, ...
    'Save SEG-Y textual header file as...');
% If "Cancel" is selected then return
if isequal([fname,pname],[0,0])
    return
    % Otherwise construct the fullfilename and write the file.
else
    try
        fid = fopen(fullfile(pname,fname),'w');
        if(fid ~= -1)
            th=char(get(handles.uitable1,'Data'));
            for i = 1:40
                fprintf(fid,'%s\n',th(i,:)');
            end
            fclose(fid);
        end
    catch me
        %catch me
        %    error(me.message);
    end
end

% --------------------------------------------------------------------
function newsegy_Callback(hObject, eventdata)
% hObject    handle to newsegy (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles=guidata(hObject);
[handles.header handles.format] = TextHeader.newHeader;
handles.header = TextHeader.reshapeHeader(handles.header,'2D');
handles.header(:,81)='.'; %Add dots in column 81 as visual cue for editing
set(handles.uitable1,'Data',cellstr(handles.header));
set(handles.rb_ascii,'Value',1.0);
guidata(hObject,handles);

% --------------------------------------------------------------------
function newblank_Callback(hObject, eventdata)
% hObject    handle to newblank (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles=guidata(hObject);
handles.header(1:40,1:81)=' ';
handles.header(:,81) = '.';
set(handles.uitable1,'Data',cellstr(handles.header));
set(handles.rb_ascii,'Value',1.0);

% --- Executes on button press in rb_ebcdic.
function rb_ebcdic_Callback(hObject, eventdata)
% hObject    handle to rb_ebcdic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of rb_ebcdic
handles=guidata(hObject);
th=char(get(handles.uitable1,'Data'));
th=TextHeader.ascii2ebcdic(th);
handles.format='ebcdic';
set(handles.uitable1,'Data',cellstr(th));
set(handles.rb_ascii,'Value',0.0);
set(handles.rb_ebcdic,'Value',1.0);

% --- Executes on button press in rb_ascii.
function rb_ascii_Callback(hObject, eventdata)
% hObject    handle to rb_ascii (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of
handles=guidata(hObject);
th=char(get(handles.uitable1,'Data'));
th=TextHeader.ebcdic2ascii(th);
handles.format='ascii';
set(handles.uitable1,'Data',cellstr(th));
set(handles.rb_ascii,'Value',1.0);
set(handles.rb_ebcdic,'Value',0.0);


% --------------------------------------------------------------------
function close_Callback(hObject, eventdata)
% hObject    handle to close (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
uiresume


% --------------------------------------------------------------------
function new_Callback(hObject, eventdata, handles)
% hObject    handle to new (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function open_Callback(hObject, eventdata)
% hObject    handle to open (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles=guidata(hObject);
[fname, pname] = uigetfile( ...
    {'*.txt;','SEGY Textual header file (*.txt)';
    '*.*','All Files (*.*)'}, ...
    'Select SEG-Y textual header file (text)');
% If "Cancel" is selected then return
if isequal([fname,pname],[0,0])
    return
    % Otherwise construct the fullfilename and read the file.
else
    try
        fid = fopen(fullfile(pname,fname),'r');
        if(fid ~= -1)
            %read entire file, ignoring new lines
            i=1;
            while 1
                th(i,:)=fgetl(fid);
                i=i+1;
                if ~ischar(th), break, end
            end
            
            %check what we've got
            asdf
            fclose(fid);
        end
    catch me
        error(me.message);
    end
    
    set(handles.uitable1,'Data',th);
end


% --------------------------------------------------------------------
function text_Callback(hObject, eventdata)
% hObject    handle to text (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function rf_ascii_Callback(hObject, eventdata)
% hObject    handle to rf_ascii (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles=guidata(hObject);
handles.format='ascii';
set(handles.rb_ascii,'Value',1.0);
set(handles.rb_ebcdic,'Value',0.0);

% --------------------------------------------------------------------
function rf_ebcdic_Callback(hObject, eventdata, handles)
% hObject    handle to rf_ebcdic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles.format='ebcdic';
set(handles.rb_ascii,'Value',0.0);
set(handles.rb_ebcdic,'Value',1.0);

% --------------------------------------------------------------------
function c_ascii_Callback(hObject, eventdata, handles)
% hObject    handle to c_ascii (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
th=char(get(handles.uitable1,'Data'));
th=TextHeader.ebcdic2ascii(th);
handles.format='ascii';
set(handles.uitable1,'Data',cellstr(th));
set(handles.rb_ascii,'Value',1.0);
set(handles.rb_ebcdic,'Value',0.0);

% --------------------------------------------------------------------
function c_ebcdic_Callback(hObject, eventdata, handles)
% hObject    handle to c_ebcdic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
th=char(get(handles.uitable1,'Data'));
th=TextHeader.ascii2ebcdic(th);
handles.format='ebcdic';
set(handles.uitable1,'Data',cellstr(th));
set(handles.rb_ascii,'Value',0.0);
set(handles.rb_ebcdic,'Value',1.0);