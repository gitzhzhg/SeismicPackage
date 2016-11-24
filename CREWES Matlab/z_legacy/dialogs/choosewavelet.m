function varargout = choosewavelet(varargin)
% CHOOSEWAVELET M-file for choosewavelet.fig
%
% Choosewavelet allows interactive selection of a wavelet from a file of
% wavelets made by WAVELETED. 
% wavenum = choosewavelet('WaveletNames', waveletnames, ...
%                         'WaveMatrix', wavemtx, ...
%                         'WaveNum', wavenum);
%
% For now, put them in this order. 'waveletnames' is an array of the names
% of the wavelets in the 'wavemtx'. 'wavenum' is the default number to
% display. Returned will be the index of the wavelet chosen. This function
% was built for READWVLETFILE so you can see how it is used there. 
% 
% Chad Hogan, 2004.
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

% $Id: choosewavelet.m,v 1.2 2004/07/30 21:24:41 kwhall Exp $

% Edit the above text to modify the response to help choosewavelet

% Last Modified by GUIDE v2.5 04-May-2004 11:46:12

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @choosewavelet_OpeningFcn, ...
    'gui_OutputFcn',  @choosewavelet_OutputFcn, ...
    'gui_LayoutFcn',  [], ...
    'gui_Callback',   []);
if nargin & isstr(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before choosewavelet is made visible.
function choosewavelet_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   unrecognized PropertyName/PropertyValue pairs from the
%            command line (see VARARGIN)

% Choose default command line output for choosewavelet
% handles.output    = hObject;
handles.output = 1;

% store our wavenames.
% wavestuff = varargin{1};
% store our actual wavelets.
%namesarray         = wavestuff{1};
%handles.wavematrix = wavestuff{2};

% Update handles structure

handles.output     = varargin{6};  % This is our default choice
handles.wavematrix = varargin{4};  % contains our wavelets.
namesarray         = varargin{2};  % Is the names of the wavelets.

guidata(hObject, handles);

[numofwaves, junk] = size(namesarray);
for i=1:numofwaves
    popupcells(i) = {namesarray(i,:)};
end
set(handles.wvpopup, 'String', popupcells);     % fill in the popup.
set(handles.wvpopup, 'Value',  handles.output); % and select the default.

makeallplots(handles);

% UIWAIT makes choosewavelet wait for user response (see UIRESUME)
uiwait(handles.chooserwindow);
% --- Outputs from this function are returned to the command line.
function varargout = choosewavelet_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;

delete(handles.chooserwindow);


% --- Executes during object creation, after setting all properties.
function wvpopup_CreateFcn(hObject, eventdata, handles)
% hObject    handle to wvpopup (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

% Now we create our popup list.
% numofwaves = length(handles.wavenames);
% for i=1:numofwaves
%     popupcells(i) = {handles.wavenames(i)};
% end
% set(handles.wvpopup, 'String', popupcells);
% --- Executes on selection change in wvpopup.
function wvpopup_Callback(hObject, eventdata, handles)
% hObject    handle to wvpopup (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns wvpopup contents as cell array
%        contents{get(hObject,'Value')} returns selected item from wvpopup

% First we pick which wavelet is selected.
selected = get(hObject, 'Value');
handles.output = selected;
guidata(hObject, handles);

% Now we plot it. 

makeallplots(handles);

% wavemtx = handles.wavematrix;
% wavetime = wavemtx(:, 2 * selected - 1);
% wavelet  = wavemtx(:, 2 * selected);
% 
% [spec, f] = fftrl(wavelet, wavetime);
% 
% axes(handles.display1);
% plot(wavetime, wavelet);
% set(handles.display1,'XMinorTick','on')
% 
% axes(handles.display2);
% plot(f, spec);
% set(handles.display2,'XMinorTick','on')

% --- Executes on button press in okbutton.
function okbutton_Callback(hObject, eventdata, handles)
% hObject    handle to okbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
uiresume(handles.chooserwindow);

% --- Executes on button press in cancelbutton.
function cancelbutton_Callback(hObject, eventdata, handles)
% hObject    handle to cancelbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles.output = 0;
guidata(hObject, handles);
uiresume(handles.chooserwindow);

% --- Executes when user attempts to close figure1.
function choosewavelet_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if isequal(get(handles.choosewavelet, 'waitstatus'), 'waiting')
    % The GUI is still in UIWAIT, us UIRESUME
    uiresume(handles.choosewavelet);
else
    % The GUI is no longer waiting, just close it
    delete(handles.choosewavelet);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
function makeallplots(handles)

wavemtx  = handles.wavematrix;
selected = handles.output;
wavetime = wavemtx(:, 1);
wavelet  = wavemtx(:, selected + 1);

wtsz = length(wavetime);
wlsz = length(wavelet);

% Gotta chop off the first sample.
wavelet  = wavelet(2:wlsz);
wavetime = wavetime(2:wtsz);

% get the live samples
ilive  =  find(~isnan(wavelet));
wavelet = wavelet(ilive);
tw = wavetime(ilive);

axes(handles.display1);
plot(tw, wavelet);
set(handles.display1,'XMinorTick','on')

wp  = wavelet;
twp = tw;

% adjust for time zero

izero = near(twp,0);

% make sure its close
if(abs(twp(izero))<twp(2)-twp(1))
    wp = [wp(izero:length(wp));wp(1:izero-1)];
else
    disp('***WARNING*** unable to find time zero, phase may be inaccurate')
end
[W,f] = fftrl(wp,twp);
W     = todb(W);

axes(handles.display2);
plot(f, real(W));
set(handles.display2, 'XMinorTick', 'on');

axes(handles.display3);
plot(f, 180*imag(W)/pi);
set(handles.display3, 'XMinorTick', 'on');