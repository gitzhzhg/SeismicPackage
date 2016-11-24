function aux=s_compare(seis1,seis2,varargin)
% Function compares two seismic data sets by plotting one on top of the other
%
% Written by: E. Rietsch, January 12, 2001
% Last updated: October 31, 2008: Handle spike display (property "quality"
%                                 is 'spikes')
%
%         aux=s_compare(seis1,seis2,varargin)
% INPUT
% seis1   first data set    (required)
%         must be a seismic structure or a matrix
% seis2   second data set   (required)
%         must be a seismic structure or a matrix (whatever the first input argument is)
% varargin   one or more cell arrays; the first element of each cell array is a keyword,
%         the other elements are parameters. The keywords correspond to those used 
%         by "s_wplot" with the exception of the keyword 'same_scale'.
%
%         'same_scale'  indicates if the same scale should be used for both data sets. 
%                Possible values are 'yes' and 'no' (or true and false)
%                Default: {'same_scale','yes'}
%
%         The following keywords apply to both data sets:
%         'annotation', 'direction', 'figure', 'orient', 'quality', 'times', 'title'
%         Their defaults are:
%
%         {'annotation','trace_no'}
%         {'direction','l2r'}
%         {'figure','new'}
%         {'orient','landscape'} for more than 5 traces in data set with more traces
%         {'orient','portrait'}  for 5 and fewer traces in data set with more traces
%         {'orient','landscape'}
%         {'quality','draft'}
%         {'times',ta,te} where "ta" is the smaller of the start times and "te" is the larger of 
%                         the end times of "seis1" and "seis2"
%         {'title',title} where "title" is 
%                         "name-of-first-input-data-setseis1" vs. name-of-second-input-data-set"
%
%         The following keywords, if given, apply to both data sets but can also be given
%         separately (see below).
%
%         {'deflection',[]}
%         {'interpol',[]}
%         {'peak_fill',[]}
%         {'polarity',[]}
%         {'scale,[]}
%         {'trough_fill',[]}
%         {'wiggle_color',[]}
%         Set {'peak_fill','w'},{'trough_fill','w'} if only wiggles are desired
%
%         The following additional keywords can be given separately for "seis1" and "seis2":
%
%               Defaults for "seis1" are:
%         {'deflection1',1}
%         {'interpol1','v5cubic'}
%         {'peak_fill1','black'}
%         {'polarity1',1}
%         {'scale1','no'}
%         {'trough_fill1',''}
%         {'wiggle_color1','k'}
%               Defaults for "seis2" are:
%         {'deflection2',1}
%         {'interpol2','v5cubic'}
%         {'peak_fill2','red'}
%         {'polarity2',1}
%         {'scale2','no'}
%         {'trough_fill2',''}
%         {'wiggle_color2','r'}
%
% OUTPUT
% aux     optional structure with fields 'scale1' and 'scale2', the scale 
%         factors used for the two data sets plotted
%         
% EXAMPLE
%         seis1=s_data;
%         seis2=seis1*0.5;
%         seis2.name=[seis2.name,' - scaled'];
%         s_compare(seis1,seis2)
%         s_compare(seis1,seis2,{'annotation','CDP'},{'polarity1',-1},{'deflection2',2})

% UPDATE HISTORY
%         October 26, 2006: use v5cubic' as default interpolation method
%         November 27, 2007: compute the default scale factor based on 
%                                  both input datasets
%         March 20, 2008: Fix bug in scaling


%     Handle case when seis1 and seis2 are matrices (and not seismic datasets)
if ~(istype(seis1,'seismic') && istype(seis2,'seismic'))
   if isnumeric(seis1)  &&  isnumeric(seis2)
      seis1=s_convert(seis1,1,1,' ','samples');
      seis2=s_convert(seis2,1,1,' ','samples');
   else
      error('The two input datasets must either both be of type "seismic" or must be matrices.')
   end
end

ntr=max([size(seis1.traces,2),size(seis2.traces,2)]);

%     Set common default values
param.same_scale='yes';
param.annotation='trace_no';
param.direction='l2r';
param.figure='new';
param.figure_only='no';
if ntr > 5
  param.orient='landscape';
else
  param.orient='portrait';
end
param.pixels=600;
param.quality='draft';
param.times={min([seis1.first,seis2.first]),max([seis1.last,seis2.last])};
param.title=[];
param.wiggle_width=1.5;

%     Set defaults for parameters that can also defined saparately for each data set
param.deflection=[];
param.interpol=[];
param.peak_fill=[];
param.polarity=[];
param.scale=[];
param.trough_fill=[];
param.wiggle_color=[];

%     Set separate default values for first data set
param.deflection1=1.00;
param.interpol1='v5cubic';
param.peak_fill1='black';
param.polarity1=1;
param.scale1='no';
param.trough_fill1='';
param.wiggle_color1='black';

%     Set separate default values for second data set
param.deflection2=1.00;
param.interpol2='v5cubic';
param.peak_fill2='red';
param.polarity2=1;
param.scale2='no';
param.trough_fill2='';
param.wiggle_color2='red';

%       Decode and assign input arguments
param=assign_input(param,varargin);


if isempty(param.title)
   param.title=[strrep(seis1.name,'_','\_'),' (',param.peak_fill1,') vs. ', ...
                strrep(seis2.name,'_','\_'),' (',param.peak_fill2,')'];
   param.title(1)=upper(param.title(1));
end

%       Set separately definable parameters if they have been specified in the argument
%       list to be equal
if ~isempty(param.deflection)
   param.deflection1=param.deflection;
   param.deflection2=param.deflection;
end
if ~isempty(param.interpol)
   param.interpol1=param.interpol;
   param.interpol2=param.interpol;
end
if ~isempty(param.peak_fill)
   param.peak_fill1=param.peak_fill;
   param.peak_fill2=param.peak_fill;
end
if ~isempty(param.polarity)
   param.polarity1=param.polarity;
   param.polarity2=param.polarity;
end
if ~isempty(param.scale)
   param.scale1=param.scale;
   param.scale2=param.scale;
else
   if isyes(param.same_scale)
      trace_max=max(abs([seis1.traces(:);seis2.traces(:)]));
      param.scale1=param.deflection1/(trace_max+eps);
      param.scale2=param.deflection2/(trace_max+eps);
   else
      param.scale1=param.deflection1/(max(abs(seis1.traces(:)))+eps);
      param.scale2=param.deflection2/(max(abs(seis2.traces(:)))+eps);
   end
end

if ~isempty(param.trough_fill)
   param.trough_fill1=param.trough_fill;
   param.trough_fill2=param.trough_fill;
end
if ~isempty(param.wiggle_color)
   param.wiggle_color1=param.wiggle_color;
   param.wiggle_color2=param.wiggle_color;
end

switch param.quality
   case {'high','draft'}
      aux1=s_wplot(seis1, ...
       {'annotation',param.annotation}, ...
       {'direction',param.direction}, ...
       {'figure',param.figure}, ...
       {'orient',param.orient}, ...
       {'pixels',param.pixels}, ...
       {'quality',param.quality}, ...
       {'times',param.times{1},param.times{2}}, ...
       {'title',param.title}, ...
       {'deflection',param.deflection1}, ...
       {'interpol',param.interpol1}, ...
       {'peak_fill',param.peak_fill1}, ...
       {'polarity',param.polarity1}, ...
       {'scale',param.scale1}, ...
       {'trough_fill',param.trough_fill1}, ...
       {'wiggle_color',''});
      hold on

      aux2=s_wplot(seis2, ...
       {'annotation',param.annotation}, ...
       {'direction',param.direction}, ...
       {'figure','old'}, ...
       {'orient',param.orient}, ...
       {'pixels',param.pixels}, ...
       {'quality',param.quality}, ...
       {'times',param.times{1},param.times{2}}, ...
       {'title',[]}, ...
       {'deflection',param.deflection2}, ...
       {'interpol',param.interpol2}, ...
       {'peak_fill',param.peak_fill2}, ...
       {'polarity',param.polarity2}, ...
       {'scale',param.scale2}, ...
       {'trough_fill',param.trough_fill2}, ...
       {'wiggle_color',param.wiggle_color2}, ...
       {'wiggle_width',param.wiggle_width});

      s_wplot(seis1, ...
       {'annotation',param.annotation}, ...
       {'direction',param.direction}, ...
       {'figure','old'}, ...
       {'orient',param.orient}, ...
       {'pixels',param.pixels}, ...
       {'quality','draft'}, ...
       {'times',param.times{1},param.times{2}}, ...
       {'title',[]}, ...
       {'deflection',param.deflection1}, ...
       {'interpol',param.interpol1}, ...
       {'peak_fill',''}, ...
       {'polarity',param.polarity1}, ...
       {'scale',param.scale1}, ...
       {'trough_fill',''}, ...
       {'wiggle_color',param.wiggle_color1}, ...
       {'wiggle_width',param.wiggle_width});

      
   case {'spike','spikes'}
      s_wplot(seis1, ...
       {'annotation',param.annotation}, ...
       {'direction',param.direction}, ...
       {'figure',param.figure}, ...
       {'orient',param.orient}, ...
       {'quality','spikes'}, ...
       {'times',param.times{1},param.times{2}}, ...
       {'title',[]}, ...
       {'deflection',param.deflection1}, ...
       {'polarity',param.polarity1}, ...
       {'scale',param.scale1}, ...
       {'wiggle_color',param.peak_fill1}, ...
       {'wiggle_width',3*param.wiggle_width});
     hold on
     
     s_wplot(seis2, ...
       {'annotation',param.annotation}, ...
       {'direction',param.direction}, ...
       {'figure','old'}, ...
       {'orient',param.orient}, ...
       {'quality','spikes'}, ...
       {'times',param.times{1},param.times{2}}, ...
       {'title',[]}, ...
       {'deflection',param.deflection2}, ...
       {'polarity',param.polarity2}, ...
       {'scale',param.scale2}, ...
       {'wiggle_color',param.peak_fill2}, ...
       {'wiggle_width',param.wiggle_width});
   otherwise
      error(['Unknown quality option: ',param.quality])
end

hold off
drawnow

if nargout
   aux.figure_handle=aux1.figure_handle;
   aux.scale1=aux1.scale;
   aux.scale2=aux2.scale;
end
