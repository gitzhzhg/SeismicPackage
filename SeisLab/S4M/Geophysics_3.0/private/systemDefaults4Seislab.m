function systemDefaults4Seislab
% Establish default values for parameters used by SeisLab
%
% Written by: E. Rietsch: January 2000
% Last updated: March 5, 2008: Fix dual-monitor field
%
%       systemDefaults

% UPDATE HISTORY
%       February 28, 2007: Add field "log_depth".
%       October 25, 2007: Replace field "log_depth" by field "diff_location".
%       January 27, 2008: Add field "batch"


global CURVES CURVE_TYPES S4M TABLES 

%	Find the name of the calling script and save it in variable SAM.script
try
   dbstk=dbstack; 

   if length(dbstk) > 2         % Presets is called from inside another macro/script
      [dummy,name]=fileparts(dbstk(3).name); %#ok First output variable is not required
      S4M.script=name;
   else
      S4M.script='';
   end

catch
   S4M.script='';
   dbstk=1;
end

%     Get dimensions of the monitor(s)
monitorsize=get(0,'MonitorPositions');
 
S4M.alert=true; 	    % Warning messages ("alerts") are printed.   
                            %    If S4M.alert == false, then warnings 
                            %    are not printed
S4M.backgroundcolor=[0.9,0.9,0.9];  % Color of backgound in menus, etc.
S4M.case_sensitive=false;   % Header selection is case-insensitive (change 
                            %    from 0 to 1 to make it case-sensitive)
S4M.dd=10.0301;             % Distribution date
S4M.deployed=isdeployed;    % True if "systemDefaults" is in a compiled package
                            % Uses interactive features
S4M.dualmonitor=(size(monitorsize,1) > 1);  % Fields set to false if SeisLab does 
                            % not run on a dual-screen system 
S4M.experience=1;           % Level of experience (possible values -1, 0, 1)

S4M.font_name='Arial';      % Font for plot annotations
S4M.fontsize4params=9;      % Font size for parameter menus
S4M.fixed_font='FixedWidth';% Fixed-pitch font 
S4M.figure_labels=true;     % Plot plot label and date/time if available
                            % see "S4M.plot_label" and "S4M.time"

%               Color schemes for Graphical User Interfaces                           
S4M.gui.backgroundcolor=[0.8,0.8,0.8];  % Color of background in menus, etc.
S4M.gui.titlebackground=[0.8,0.8,0.9];  % Color of background of titles of UIC groups
S4M.gui.quitbackground=[0.8,0.8,0.9];   % Color of background of the Quit/Done/Cancel buttons
S4M.gui.buttonbackground=[0.8,0.8,0.8]; % Color of background of buttons
S4M.gui.color4tasks=[0.25,0.5,0.5];     % Color of background for tasks


S4M.history=true;           % Create history field for seismic structures 
                            %    (set S4M.history to false to disable history
                            %    field)
S4M.history_level=1;        % Set the deepest level below the START_LEVEL from 
                            %    which to add entries into the history field

S4M.batch=true;             % Scipts are run in batch mode. This means pop-up 
                            % windows with messages are replaced by messages
			                   % in the command window.
			                   % This setting is changed to false if S4M.deployed

S4M.invert_hardcopy='off';  % Invert figure background when printing 

v=tokens(version,'.');
S4M.matlab_version=str2double([v{1},'.',v{2}]);  % Matlab version number with major 			  
                            %    and minor release digits

S4M.diff_location='base';   % Depths of a log or pseudowell may refer to either:
                            % the center of an interval, its top, or its base.
                            % If S4M.diff_location='center' then the first reflection 
                            % coefficient would be at depth/time 
                            % (depth(1)+depth(2))/2. 
                            % If S4M.diff_location == 'base' then a log sample,
                            % say rho(i), is constant in an interval that begins
                            % at depth depth(i-1) and ends at depth(i); thus
                            % the first reflection coefficients would be at
                            % depth/time depth(1). The first log sample has
                            % an implied interval start.
                            % If S4M.diff_location == 'top' then a log sample,
                            % say rho(i), is constant in an interval that begins
                            % at depth depth(i) and ends at depth(i+1); thus
                            % the first reflection coefficients would be at
                            % depth/time depth(2). The last log sample has an 
                            % implied interval end.
                            % See also function: "reflectionStartTime"
S4M.log_step_error=0.005;   % Upper limit on the relative depth increment
                            % deviation for a log to still be considered uniformly
                            % sampled.

temp=fileparts(which('presets'));
S4M.toolbox_functions=temp; % Directory with Seislab-specific files 
S4M.toolbox=fileparts(temp);% Directory with Seislab-toolbox files
S4M.mymatlab=fileparts(S4M.toolbox); % Directory with my Matlab files
%keyboard
%idx=strfind(temp,filesep);
%S4M.mymatlab=temp(1:idx(end)-1); % Directory with SeisLab files 
S4M.helpfiles=fullfile(S4M.toolbox,'HelpFiles');  % Directory with help files

S4M.keyword_expansion=true; % Allow unique abbreviations of keywords

S4M.myseislab=temp;         % Directory with Matlab files (to be personalized
                            %    in "userDefaults")

S4M.name='SeisLab';         % Name for Figure windows in compiled version
S4M.no_units='n/a';         % Indicator that a quantity is dimensionless
S4M.ntr_wiggle2color=101;   % Number of traces for which the default seismic
                            % display switches from wiggle to color
S4M.pd=true;                % PD version

S4M.plot_label=S4M.script;  % Label for the lower left corner of plots 
                            % (see also "S4M.figure_labels")
S4M.precision='double';     % Use double precision for seismic and well-log datasets
S4M.pp_directory=fullfile('C:\Documents and Settings',getenv('USERNAME'), ...
                 'My Documents','My Pictures'); % Directory for PowerPoint files
                            
S4M.eps_directory=S4M.pp_directory;   % Directory for EPS files


S4M.start_time=clock;       % Date and time as 6-element array       

S4M.start_level=size(dbstk,1); % Set the reference level relative which to 
                            %    count the level of a function to determine if
                            %    it can make an entry into the history field.
                            
S4M.testdata=fullfile(S4M.toolbox,'TestData');  % Directory with TestData
S4M.time=datestr(now,0);    % Date and time as string
S4M.title=S4M.name;         % Title in figures and dialogs

S4M.seislab_version='3.0';  % SeisLab version
			                   % and minor release digits

if S4M.deployed
   S4M.batch=false;
end

%	GUI colors (deprecated)
S4M.quitbackground= [0.8,0.8,0.9];
S4M.titlebackground=[0.8,0.8,0.9];
S4M.backgroundcolor=[0.8,0.8,0.8];


%       Should be overridden by user-specific file paths in function "userDefaults"
S4M.figure_path=tempdir;    % Path to saved figures
S4M.seismic_path=tempdir;   % Path to seismic data with extension "sgy" 
                            % (starting point for interactive data selection)
S4M.log_path=tempdir;       % Path to log data  with extension "log" 
                            % (starting point for interactive data selection)
S4M.mat_path=tempdir;       % Path to mat-files with extension "mat"
                            % (starting point for interactive data selection)
S4M.table_path=tempdir;     % Path to table data  with extension "tbl" 
                            % (starting point for interactive data selection)
S4M.default_path=tempdir;   % Path for all other files
S4M.function_directories={'ToolboxFunctions','Beta','Incomplete'};    % Used in 
                            % "efind" and "hfind"; should be customized in 
                            % function "userDefaults4Seislab"

if S4M.matlab_version >= 7
%   feature('jitallow','structs','off'); % Fix bug in Matlab Version 7
end

%       Default figure sizes/positions for portrait and landscape (based on screen size)

bdwidth=5;                  % Border width
topbdwidth=60;              % Top border width
xl=monitorsize(1,4)*0.75;      % Long side of figure
yl=xl*0.66;                 % Shorter side of figure
x0=monitorsize(1,3)*0.15;
y0=monitorsize(1,4)-yl-5*(topbdwidth+bdwidth);
LANDSCAPE=round([x0,y0,xl*1.05,yl]);   % Figure position information for landscape format
                      % The four elements in vector LANDSCAPE are: x and y coordinates of the
                      % lower left corner and length and height of the figure (in pixels)
S4M.landscape=LANDSCAPE;

x0=monitorsize(1,3)*0.22;
y0=monitorsize(1,4)/7-0.5*(topbdwidth+bdwidth);
PORTRAIT=round([x0,y0,yl*1.1,xl*0.9]);    % Figure position information for portrait format
                      % The four elements in vector PORTRAIT are: x and y coordinates of the
                      % lower left corner and length and height of the figure (in pixels)
S4M.portrait=PORTRAIT;


%	Default mnemonics for tables
TABLES.depth='depth';	       % Depth
TABLES.owt='owt';	             % One-way time
TABLES.twt='twt';	             % Two-way time
TABLES.vint='vint';	          % Interval velocity
TABLES.vp='vp';	             % P-velocity
TABLES.vrms='vrms';            % RMS velocity
TABLES.vs='vs';	             % S-velocity


%       Default mnemonics for log curves
CURVES.aimp='aImp';            % Acoustic (pressure) impedance (rho*Vp)
CURVES.arefl='aRefl';          % Acoustic reflectivity
CURVES.bd='BS';                % Bit size
CURVES.cal='caliper';          % Caliper
CURVES.delta='delta';          % Thomsen parameter
CURVES.depth='depth';          % Depth column (generally first column)
CURVES.drho='drho';            % Density correction
CURVES.dtp='DTp';              % Sonic log (Pressure)
CURVES.dts='DTs';              % Shear log
CURVES.epsilon='epsilon';      % Thomsen anisotropic parameter
CURVES.gr='GR';                % Gamma ray
CURVES.md='MD';                % Measured depth
CURVES.owt='OWT';              % One-way time
CURVES.phie='Phie';            % Effective porosity
CURVES.phit='Phit';            % Total porosity
CURVES.pr='PR';                % Poisson's ratio
CURVES.qp='Qp';                % Q for P-waves
CURVES.qs='Qs';                % Q for S-waves
CURVES.rho='rho';              % Density
CURVES.sbrine='SBrine';        % Brine saturation
CURVES.sgas='SGas';            % Gas saturation
CURVES.shc='SHC';              % Hydrocarbon saturation
CURVES.simp='sImp';            % Shear impedance (rho*Vs)
CURVES.soil='SOil';            % Oil saturation
CURVES.tvd='TVD';              % True vertical depth
CURVES.tvdbml='TVDbML';        % True vertical depth below mud line
CURVES.tvdbsd='TVDbSD';        % True vertical depth below seismic datum
CURVES.twt='TWT';              % Two-way time
CURVES.vclay='Vclay';          % Clay volume
CURVES.vshale='Vshale';        % Shale volume
CURVES.vint='Vint';            % Interval velocity
CURVES.vrms='Vrms';            % RMS velocity
CURVES.vp='Vp';                % Compressional velocity
CURVES.vs='Vs';                % Shear velocity

%       Lithology
CURVES.coal='coal';            % Logical for coal
CURVES.dolomite='dolomite';    % Logical for dolomite
CURVES.gas_sand='gas_sand';    % Logical for gas sand
CURVES.hc_sand='hc_sand';      % Logical for hydrocarbon sand
CURVES.limestone='limestone';  % Logical for limestone
CURVES.oil_sand='oil_sand';    % Logical for oil sand
CURVES.salt='salt';            % Logical for salt
CURVES.sand='sand';            % Logical for sand
CURVES.sh_sand='sh_sand';      % Logical for shaly sand
CURVES.shale='shale';          % Logical for shale
CURVES.volcanics='volcanics';  % Logical for volcanics
CURVES.wet_sand='wet_sand';    % Logical for wet sand

%       Pore pressure
CURVES.ep='EP';                % Excess pressure
CURVES.epg='EPG';              % Excess pressure gradient
CURVES.fp='FP';                % Fracture pressure
CURVES.fpg='FPG';              % Fracture pressure gradient
CURVES.obp='OBP';              % Overburden pressure
CURVES.obpg='OBPG';            % Overburden pressure gradient
CURVES.pp='PP';                % Pore pressure
CURVES.ppg='PPG';              % Pore pressure gradient


%	Curve types
% The 4 columns are:
%    Description
%    Units of measurements usually associated with it
%          "standard" curve mnemonic
%    Curve type
%    Indicator if a curve type is not related to the one on the next line in the list
%         ('P-sonic' is related to 'P-velocity' in the next row; hence the indicator is 0
%          'P-velocity' is not related to 'Density' in the next row; hence the indicator is 1)
%         This is used to structure a GUI
CURVE_TYPES = ...
[{'P-sonic',      '|us/ft|us/m|',    'DTp',   'sonic',            0};
 {'P-velocity',   '|m/s|ft/s|',      'Vp',    'sonic velocity',   1};
 {'Density',      '|g/cm3|kg/m3|',   'rho',   'density',          1};
 {'Impedance',    '|imp|',           'Imp',   'impedance',        0};
 {'Reflection coefficients', '|n/a|','Refl',  'reflection coefficients',1};
 {'S-sonic',      '|us/ft|us/m|',    'DTs',   'shear sonic',      0};
 {'S-velocity',   '|m/s|ft/s|',      'Vs'   , 'shear velocity',   1};
 {'Clay volume',  '|fraction|%|',    'Vclay', 'clay volume',      1};
 {'Water saturation','|fraction|%|', 'Sbrine','brine saturation', 1};
 {'Gamma ray',    '|API|gamma|',     'GR',    'gamma ray',        1};
 {'Two-way time', '|s|ms|',          'TWT',   'two-way time',     0};
 {'One-way time', '|s|ms|',          'OWT',   'one-way time',     1};
 {'Depth',        '|ft|m|'           'depth', 'depth',            0}];
