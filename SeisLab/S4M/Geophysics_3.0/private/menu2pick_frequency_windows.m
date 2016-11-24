function MenuHandle=menu2pick_frequency_windows(figure_handle)
% Create menu button to pick rectangular windows; the spectrum of the data 
% is posted to a second window. The callback function that does
% all this is also in this file ("g_ispectrum") and passed on to the
% "outside world" via a "function handle".
%
% Written by: E. Rietsch: November 23, 2003
% Last updated: February 8, 2006: Fix bug with non-normalized data;
%                                 add box around spectrum plot
%
%                MenuHandle=menu2pick_frequency_windows(figure_handle)
% INPUT
% figure_handle  handle of the figure to which the menu button is
%                to be attached; default: gcf
% OUTPUT
% MenuHandle     handle of the menu item

global S4M

if nargin == 0
   figure_handle=gcf;
end

%	Create menu button
MenuHandle=uimenu(figure_handle,'Label','Pick windows','ForeGroundColor',[1 0 0]);

set(MenuHandle,'CallBack',@g_ispectrum,'Tag','Pick_window_menu')
if S4M.experience < 0
   display_help('s_ispectrum')
end

% waitfor(MenuHandle)  % Don't wait 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function g_ispectrum(varargin)
% Interactively select windows on a seismic data set and compute the spectra of
% the data in these windows
%
% Written by: E. Rietsch: November 23, 2003
% Last updated: November 27, 2003: Bug fix with window plot

% global S4M

SeisHandle=gcf;
userdata=get(SeisHandle,'Userdata');
param=userdata.param;
seismic=userdata.seismic;
ntr0=size(seismic.traces,2);

%       Remove figure button functions
try
   state = uisuspend(SeisHandle);
catch
end

%	If button is pressed in Window check if it was over the Picking button
set(SeisHandle,'WindowButtonDownFcn','g_check_menu_button', ...
               'CloseRequestFcn',@g_closerequest4ispectrum)

MenuHandle=findobj(SeisHandle,'Tag','Pick_window_menu');
arg='findobj(gcf,''Tag'',''Pick_window_menu'')';
set(MenuHandle,'Label','Done picking windows','CallBack',['delete(',arg,')'])

if isfield(seismic,'null')  
   null=1;       % Set logical value null to true if there are NaNs in the seismic
else
   null=0;       % Set logical value null to false if there are no NaNs in the seismic
end


nspectra=length(param.colors);
book=cell(nspectra,4);  % Book-keeping array
                        % color, handle of box, handle of spectrum, box coordinates
book(:,1)=param.colors';
index=1:nspectra;         
fnyquist=500/seismic.step;
if param.frequencies{2} > fnyquist
   param.frequencies{2}=fnyquist;
end

SpecHandle=lfigure;
if strcmpi(param.scale,'log')
   axis([param.frequencies{1},param.frequencies{2},param.minamp,0])
else
   axis([param.frequencies{1},param.frequencies{2},0,1])
end


%	Shift figure position to make it visible below seismic window
msg=['This figure can be closed once the Button "',get(MenuHandle,'Label'), ...
    '" in the corresponding seismic window (Figure ',num2str(SeisHandle),') has been pressed.'];
set(SpecHandle,'Position',get(SpecHandle,'Position')+[-40,40,0,0], ...
        'CloseRequestFcn',{@closedlg,msg},'Tag',['Spectrum_4_',num2str(SeisHandle)])

%figure_export_menu(SpecHandle); % Create menu button to export figure as emf/eps file

if strcmpi(param.normalize,'yes')
   axis manual
end
grid on
zoom

mytitle('Amplitude spectra of selected windows of seismic data set')
xlabel('Frequency (Hz)')
if strcmpi(param.scale,'linear')
   atext='Amplitude (linear)';
else
   atext='Amplitude (dB)';
end
ylabel(atext)

type='continue';


          while ~strcmpi(type,'quit') && ishandle(MenuHandle)

if isempty(index)
   warndlg(' Not enough colors to display additional spectrum')
end
figure(SeisHandle)
ia=index(1);

axis_handle=gca;
set(SeisHandle,'WindowButtonDownFcn',{@pick_or_delete_a_box,axis_handle})
waitfor(MenuHandle,'UserData')
if ishandle(MenuHandle)
   set(MenuHandle,'UserData','wait')
else
   break
end

userdata1=get(axis_handle,'UserData');
type=userdata1.type;
handle=userdata1.handle;
databox=userdata1.databox;

refresh

switch type
                case 'pick'          % A box has been picked



%	Round coordinates of box to the nearest samples
if strcmpi(param.plottype,'color')
   hvals=s_gh(seismic,param.annotation);
else
   hvals=1:ntr0;
end
idxh=find(hvals >= databox(1) & hvals <= databox(2));
idxt1=round((min(databox(3:4))-seismic.first)/seismic.step)+1;
idxt2=round((max(databox(3:4))-seismic.first)/seismic.step)+1;

		if isempty(idxh) || idxt2-idxt1 < 4
type='continue';
%delete(handle)
% keyboard

		else
databox(1:2)=sort([hvals(idxh(1)),hvals(idxh(end))]);
databox(3:4)=([idxt1,idxt2]-1)*seismic.step+seismic.first;
handle=plot_box(databox,'r',param.linewidth);
book{ia,2}=handle;
book{ia,4}=databox;

index(1)=[];

ntr=length(idxh);
traces=seismic.traces(max([1,idxt1]):min([end,idxt2]),idxh);
if null
   idx_null=find(isnan(traces));
   if ~isempty(idx_null)
      traces(idx_null)=0;
   end
end
nsamp=size(traces,1);

%       Apply a window to the traces
wind=mywindow(nsamp,param.window);

for ii=1:ntr
   traces(:,ii)=traces(:,ii).*wind;
end
% traces=spdiags(wind,0,nsamp,nsamp)*traces;
ft=fft(traces,max([param.padding,nsamp]));

nfft=size(ft,1);
f=(0:2:nfft)*fnyquist/nfft;

amp=mean(abs(ft(1:length(f),:)),2);

if strcmpi(param.scale,'log')
   amp=amp/max(amp);  
   amp(amp < 1.0e-5)=1.0e-5;   % Constrain the possible values of the amplitude spectrum
   amp=20*log10(amp/max(amp));
end

if strcmpi(param.scale,'linear') && strcmpi(param.normalize,'yes')
   amp=amp/(max(amp)+eps);
end
color=book{ia,1};
if length(color) > 1
   linestyle=color(2:end);
else
   linestyle='-';
end
set(handle,'Color',color(1),'LineStyle',linestyle, ...
        'LineWidth',param.linewidth,'EraseMode','none');

figure(SpecHandle)
v=axis;
if ia == 1
   v(4)=max(amp);
else
   v(4)=max(v(4),max(amp));
end
axis(v)
hl=line(f,amp,'Color',color(1),'LineStyle',linestyle, ...
        'LineWidth',param.linewidth,'EraseMode','none');

book{ia,3}=hl;
		end


                case 'delete'
idx=find(cat(1,book{:,2}) == handle);
if isempty(idx)
   disp(' No box that can be deleted has been selected.')
else
   figure(SeisHandle)
   delete(handle)         % Delete box
   refresh

   figure(SpecHandle)
   temp=book(idx,:);
   delete(temp{3})        % Delete spectrum curve
   refresh

   book(idx,:)=[];
   idx1=index(1)-1;
   book=[book(1:idx1-1,:);temp;book(idx1:end,:)];
%   book=[temp;book];
   index=[idx1,index];       %#ok First value of "index" is next available row in bookkeeping array
end


                case 'break'
% keyboard
          
                otherwise
% Continue

end

          end      % End while

figure(SpecHandle)
set(SpecHandle,'CloseRequestFcn','closereq')
set(SeisHandle,'WindowButtonDownFcn',[])
hold off
ncurves=index(1)-1;
ltext1=cell(ncurves,1);
ltext2=cell(ncurves,1);

headervals=s_gh(seismic,param.annotation);
for ii=1:ncurves
   if strcmpi(param.plottype,'wiggle_color')
      firsttrace=num2str(headervals(min(book{ii,4}(1:2))));
      lasttrace=num2str(headervals(max(book{ii,4}(1:2))));
   else
      firsttrace=num2str(min(book{ii,4}(1:2)));
      lasttrace=num2str(max(book{ii,4}(1:2)));
   end     
   ltext1(ii)={[strrep(param.annotation,'_','\_'),': ',firsttrace,'-', ...
                                       lasttrace,';']};
   ltext2(ii)={[' time: ',num2str(min(book{ii,4}(3:4))),'-', ...
                        num2str(max(book{ii,4}(3:4))),' ',seismic.units]};
end

ltext=[char(ltext1),char(ltext2)];

%       Avoid error message if no spectrum curves have been created
try
   legend(ltext,param.lloc)
catch
end
refresh

grid on
bgGray  % Create a gray background

box on


%	Return data to calling program
userdata=get(SeisHandle,'UserData');
SpecWindows=reshape([book{1:ncurves,4}],4,ncurves)';
userdata=struct('SpecWindows',SpecWindows,'exit',userdata.exit);
figure(SeisHandle)
try
   uirestore(state);
catch
end
set(SeisHandle,'Userdata',userdata,'CloserequestFcn','closereq')
refresh

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function g_closerequest4ispectrum(varargin)

% Save "userdata" in case the close-figure button has been pressed

% global TABLE_FROM_ISPECTRUM

SeisHandle=gcf;
SpecHandle=findobj('Tag',['Spectrum_4_',num2str(SeisHandle)]);
set(SpecHandle,'CloseRequestFcn','closereq')

MenuHandle=findobj(SeisHandle,'Tag','Pick_window_menu');
delete(MenuHandle)

userdata=get(SeisHandle,'UserData');
userdata.exit=1;
% TABLE_FROM_ISPECTRUM.userdata=userdata;

% uiresume(SeisHandle)

set(SeisHandle,'CloserequestFcn','closereq','UserData',userdata)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function pick_or_delete_a_box(varargin)
% Select a rectangular area/box in a figure or find the handle of an existing
% box (for deletion)
% Written by: E. Rietsch: November 23, 2003
% Last updated: August 27, 2004: add input arguments for use via a call-back function handle
%
%              pick_or_delete_a_box(hObject,evdata,axis_handle)
% INPUT
% hObject          used by Matlab
% evdata           used by Matlab
% axis_handle      axis handle
% OUTPUT
% userdata.type    type of action ('pick' or 'delete', or 'continue')
% userdata.databox     corners of box selected (if "type" is "pick", [] otherwise)
% userdata.handle  handle of box selected (if "type" is "delete",[] otherwise)

axis_handle=varargin{3};

FigureHandle=gcf;
userdata=get(axis_handle,'UserData');
type=get(FigureHandle,'SelectionType');
set(FigureHandle,'WindowButtonDownFcn','')
if strcmp(type,'normal')
   databox=gg_pickbox;
   userdata.handle=[];
   userdata.type='pick';
   userdata.databox=databox;

elseif strcmp(type,'alt')
   userdata.type='delete';
   userdata.databox=[];
   userdata.handle=gco;
   if isempty(userdata.handle)
      userdata.type='continue';
   end

else
   userdata.type='continue';
   userdata.databox=[];
   userdata.handle=[];
end

set(axis_handle,'UserData',userdata)
MenuHandle=findobj(FigureHandle,'Tag','Pick_window_menu');
set(MenuHandle,'UserData','proceed')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function closedlg(varargin)
% Message to display if the window close box of the spectral window is pushed.

msgdlg(varargin{3})
