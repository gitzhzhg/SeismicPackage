function wlog=read_las_file(filename,varargin)
% Function reads well logs from a disk file in LAS format (Log ASCII Standard) 
% Versions 2.0 or 3.0 as specified by the Canadian Well Logging Society.
% See also: "read_las2_file"
%
% Written by: E. Rietsch: December 15, 2006;  
% Last updated: October 12, 2007: remove fields with text strings that 
%                                 appear to provide no real information
%
%          wlog=read_las_file(filename,varargin)
% INPUT
% filename  string with the file name (and full path if desired);
%         if the file is not found a file selection box will pop up to
%         allow interactive file selection
% varargin  one or more cell arrays; the first element of each cell array 
%         is a keyword, the other elements are parameters (only used 
%         if the file is in LAS 3.0 format).
%         Presently, keywords are:
%     'section'   string with names of sections that schould be read in 
%         The actual log curves (Section ~ASCII) and the parameters
%         (Section ~Parameter) are always read.
%         Example: {'section','tops','inclinometry'}; 
%         the sections can also be written as one comma-separated string
%         Example: {'section','tops,inclinometry'}  reads sections "tops" and
%                                                "inclinometry", if they exist.
%                  {'section','all'}       reads all sections.
%         Default: {'section',[]}   No additional sections are read.
% OUTPUT
% wlog    structure containing the log curves and ancillary information
%    wlog.curves   Matrix of curve values
%         Each log curve is in a column and the first column is usually the depth.
%         Thus, if w1log.curves has n columns, then there are n-1 log curves.
%         If necessary, the rows of curve data are resorted so that
%         depth increases from one row to the next
%         
%    wlog.curve_info Cell array (3 x number of curves) with curve
%         mnemonics, curve units of measurement, and curve descriptions
%    wlog.first  Start of log (first depth in file)
%    wlog.last           End of log (last depth in file)
%    wlog.step   Depth increment (0 if unequal)
%    wlog.units  Units of measurement for depth
%    wlog.null   Null value (set to NaN) if there are no-data values
%                otherwise, this field is not set
%    wlog.wellname  Name of well
%    wlog.location  Location of well
%    wlog.company  Oil/gas company which drilled the well
%    wlog.field   Field in which well is located
% 
%         The following components, while desirable, may or may not be present 
%         (depending on LAS file), and others may be present. 
%         They include well identification such as
%    wlog.country  Country in which well is located            
%    wlog.wellid   Well Identifier
%    wlog.api      API number of the well
%    wlog.service  Service company which logged the well
%
%          Also included are all parameters in the Parameter Section of the
%          LAS header which have numerical values other than the null value. 
%          Each parameter is stored in a field of the log structure; its 
%          units of measurement and a description are stored in a row of
%          the cell array "wlog.parameter_info" 
%          Examples illustrating such parameters are 
%    wlog.ekb=84        Kelly bushing elevation     
%    wlog.egl=0         Ground level elevation
%    wlog.parameter_info={'ekb','ft','Kelly bushing elevation';
%                         'egl','ft','Ground level elevation'}

run_presets_if_needed

%       Sections of LAS 3.0 file to read in addition to the standard ones
param.section=[];

%       Replace defaults by actual input parameters
param=assign_input(param,varargin);

if ~isempty(param.section)
   param.section=tokens(param.section,',');
end

if nargin == 0  || isempty(filename);
   [fid,filename]=open_file('rt','.las');
else
   [fid,filename]=open_file('rt',filename);
end

tlines=textscan(fid,'%s','delimiter','\n');
fclose(fid);
tlines=tlines{1};
bool=~cellfun(@isempty,tlines);  % Matlab version > 7.04
% bool=~cellfun('isempty',tlines);
tlines=tlines(bool);
bool=cellfun(@isempty,regexp(tlines,'^#')); % Matlab version > 7.04
% bool=cellfun('isempty',regexp(tlines,'^#'));
tlines=tlines(bool);


%       Check version
temp=tokens(tlines{1},' ');
tlines(1)=temp(1);
if strcmpi(tlines{1},'~VERSION')
   comp=split_line(tlines{2});
   if strcmp(comp{1},'VERS')
      if str2double(comp{2}) == 3
         wlog=interpret_las3(tlines(3:end),param);

      elseif str2double(comp{2}) == 2
         wlog=interpret_las2(tlines(3:end));
 
      elseif str2double(comp{2}) == 1.2
         warning(warnid,'LAS-file version is 1.2')

      else
         error(['Unknown LAS-file version ',comp{2}])
      end
   end
else
    error(['The file specified, "',filename,'", is not a LAS file.'])
end

%       Remove unnecessary fields
wlog=clean_up_well_log(wlog);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=clean_up_well_log(wlog)
%       Remove certain text fields if they are either empty or their value
%       is "unknown". 

fields={'wellname','country','county','state','api','wellid', ...
        'location','field','company','service','date'};

lfields=length(fields);
bool=false(1,lfields);

for ii=1:lfields
   try
      txt=wlog.(fields{ii});
      if isempty(txt) || ~isempty(findstr('unknown',lower(txt)))
         bool(ii)=true;
      end
   catch
   %    Do nothing
   end
end

if any(bool)
   wlog=rmfield(wlog,fields(bool));
end
