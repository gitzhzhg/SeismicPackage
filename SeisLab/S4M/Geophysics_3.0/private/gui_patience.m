function varargout = gui_patience(varargin)
% Create a message window; the message does not halt execution of the program
% Written by: E. Rietsch: October 2003
% Last updated: July 22, 2007: "mlint" compliant
%
%            varargout = gui_patience(varargin)
% INPUT
% varargin(1)   cell containing string with message to be displayed
% varargin(2)   optional cell with the handle of a figure; the message is 
%               centered on this figure
% OUTPUT
% varargout  handle of the message window
%
% EXAMPLE
%            msg_handle=gui_patience({'Please be patient ...'})

%
% GUI_PATIENCE M-file for gui_patience.fig
%      GUI_PATIENCE, by itself, creates a new GUI_PATIENCE or raises the existing
%      singleton*.
%
%      H = GUI_PATIENCE returns the handle to a new GUI_PATIENCE or the handle to
%      the existing singleton*.
%
%      GUI_PATIENCE('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in GUI_PATIENCE.M with the given input arguments.
%
%      GUI_PATIENCE('Property','Value',...) creates a new GUI_PATIENCE or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before gui_patience_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to gui_patience_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help gui_patience

% Last Modified by GUIDE v2.5 19-Oct-2003 13:06:52

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @gui_patience_OpeningFcn, ...
                   'gui_OutputFcn',  @gui_patience_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- Executes just before gui_patience is made visible.
function gui_patience_OpeningFcn(hObject, eventdata, handles, varargin)  %#ok
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to gui_patience (see VARARGIN)

% Choose default command line output for gui_patience
handles.output = hObject;

% global S4M

set(hObject,'WindowStyle','normal','Name','Patience')

if ~isempty(varargin)
   set(handles.patience,'String',varargin{1});
end

if length(varargin) > 1
   try
      centerfig(hObject,varargin{2})
   catch
   end
end

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes gui_patience wait for user response (see UIRESUME)
% uiwait(handles.figure1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- Outputs from this function are returned to the command line.
function varargout = gui_patience_OutputFcn(hObject, eventdata, handles)  %#ok
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.figure1;
drawnow

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Executes during object creation, after setting all properties.
function patience_CreateFcn(hObject, eventdata, handles)  %#ok
% hObject    handle to patience (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Do nothing
