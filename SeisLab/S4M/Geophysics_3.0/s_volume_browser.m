function s_volume_browser(seismic,varargin)
% Interactive display of a 3-D seismic data set (volume) via 
%
% Written by: E. Rietsch: October 17, 2006
% Last updated: October 29, 2006: Debug option
%
%          s_volume_browser(seismic,varargin)
% INPUT
% seismic  3-D seismic dataset with headers such as "iline_no" and 
%          "xline_no", "CDP" and "offset" that represent surface coordinates
% varargin  one or more cell arrays; the first element of each cell array is a keyword,
%          the other elements are parameters. Presently, keywords are:
%     'headers' two strings or a cell vector with two strings representing two 
%          mnemonics that describe the two horizontal dimensions of the dataset. 
%          Examples are {'iline_no','xline_no'} or {'cdp','offset'}.
%          Default: {headers','iline_no','xline_no'}
%     'times'       2-element or 3-element cell array 
%          {'times',vector of first and last time to plot} or ('times',first,last}. 
%          Default: {'times',seismic.first,seismic.last}    This is equivalent to 
%                   {'times',[seismic.first,seismic.last]}
%     'title' Title of figures
%          Default: {'title',seismic.name}
%     'traces'  2-element or 3-element cell array. The second element can be an
%          array of trace numbers or it can be a string. If it is a string  
%          it can be a header mnemonic or it can contain a logical expression 
%          involving header values to include. 
%          A "pseudo-header" 'trace_no' can also be used. If the second 
%          element is a string containing a header mnemonic there must be a 
%          third element containing a vector of values. (see "s_select").
%          Example: {'traces','mod(iline_no,2) == 0  &&  mod(xline_no,2) == 1}
%          which selects traces with even inline numbers and odd 
%          cross-line numbers.
%          Default:  {'traces',[]} which is equivalent to 
%                    {'traces',1:ntr} where ntr denotes the number of traces in the 
%                            input data set (ntr = size(seismic.traces,2))
%     
% EXAMPLE
%       seismic=s_data3d;
%       s_volume_browser(seismic,{'headers','iline_no','xline_no'})

global S4M

%       Check if function "volume_browser" is present
temp=exist('volume_browser','file');
if temp ~= 2  &&  temp ~= 3  &&  temp ~= 6
   disp(' The volume browser function, "volume_browser", has not been found.')
   disp(' You need to download it (and functions it calls) from the Matlab File exchange,')
   disp('    http://www.mathworks.com/matlabcentral/fileexchange/13526')
   disp(' and add it to the Matlab Path.')
   drawnow
   error('Abnormal termination')
end

%	Set defaults of input parameters
param.debug=false;
param.headers={'iline_no','xline_no'};
param.title=mnem2tex(seismic.name);
param.times=[];
param.traces=[];

%	Replace defaults by actual input parameters
param=assign_input(param,varargin);

if iscell(param.headers{1})
   param.headers=param.headers{1};
end

%seismic=double(seismic); % Some graphic functions cannot yet handle single-precision

%	Select subset of seismic data
if ~isempty(param.traces)  ||  ~isempty(param.times)
   history=S4M.history;       % Preserve value of global variable S4M.history
   S4M.history=false;

   if iscell(param.traces)
      param.traces=cell2num(param.traces);
   end

   if isempty(param.times)
      seismic=s_select(seismic,{'traces',param.traces});

   elseif iscell(param.times)
      seismic=s_select(seismic,{'traces',param.traces},{'times',param.times{1},param.times{2}});

   else
      seismic=s_select(seismic,{'traces',param.traces},{'times',param.times(1),param.times(2)});

   end

   S4M.history=history;		% Restore history setting
end

%	Create 3D matrix "vol" for input to the actual volume browser
[vol,x,y,z,options.yinfo,options.xinfo,options.zinfo] = ...
                      traces2volume(seismic,param.headers{1},param.headers{2});
vol=permute(vol,[3,2,1]);

options.debug=param.debug;
options.zdir='reverse';		% z-direction downward
options.plot_title=param.title;
options.plot_time=S4M.time;
options.plot_label=S4M.plot_label;

%	Volume_browser
volume_browser(vol,x,y,z,options)
