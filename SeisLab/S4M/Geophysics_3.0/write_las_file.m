function write_las_file(wlog,filename,iprint)
% Function creates a disk file in LAS (Log ASCII Standard)format, 
% Version 2.0, as specified by the Canadian Well Logging Society
%
% Written by: E. Rietsch: February 20, 2000
% Last updated: October 10, 2007: Use function "open_file" to handle file initiation
%
%       write_las_file(wlog,filename,iprint)
% INPUT
% wlog  structure containing the log curves and ancillary information
%              Required fields
%       wlog.type       'well-log'
%       wlog.tag        Tag, e.g. 'unspecified'
%       wlog.name       Name of the log
%	wlog.curves   	Matrix of curve values
%		 Each log curve is in a c"wlog.curves" has n columns, then there
%                are n-1 log curves. The LAS format does not allow exponents in 
%                the curve values. Hence care should should be taken to ascertain
%                that the log values can be represented accurately enough with a
%                total no more than 2-3 decimal digits.      
%       wlog.curve_info Cell array (3 x number of curves) with curve mnemonics, 
%               curve units of measurement, and curve descriptions
%       wlog.null	Null value (set to NaN); only required if there are null values
%       wlog.first	Start of log (first depth in file)
%       wlog.last	Stop of log (last depth in file)
%       wlog.step	Depth increment (0 if unequal)
%       wlog.units      Units of measurement for depth
%       wlog.wellname	Well name
%       wlog.field      Name of the field (this could be WILDCAT)
%       wlog.location   Location of well
%       wlog.company    Oil/gas company which drilled the well
%
%              The following components, while desirable, are either optional 
%              or are replaced by defaults. They include
%                    Well information      
%       wlog.country	 Country in which well is located
%       wlog.wellid	 Well Identifier
%       wlog.service     Service company which logged the well
%       wlog.province    Province
%       wlog.county      County
%       wlog.state       State
%       wlog.date        Date (default: current data)
%       wlog.api         API number
%       wlog.uwi         Unique well ID
%
%              Parameters which have numerical values other than the null value are
%              written to the Parameter Section of the LAS header.
%              In the log structure a parameter is represented by a field with 
%              the parameter name and a row in the cell array "wlog.parameter_info".
%              Such a row consists of three strings: the first is the parameter 
%              mnemonic, the second the units of measurement, and the third the 
%              parameter description. An example is:
%    wlog.ekb=84	Kelly bushing elevation     
%    wlog.egl=0		Ground level elevation
%    wlog.parameter_info={'ekb','ft','Kelly bushing elevation';
%                         'egl','ft','Ground level elevation'}
%
%
% filename     name of the file; if empty or omitted a file selector box will be opened
%              for interactive file selection
%
% iprint       print control parameter (iprint = 0, no printout,
%                                       iprint = 1; LAS header is printed, default)
%
% EXAMPLE
%             wlog=l_data;
%             write_las_file(wlog)


if ~istype(wlog,'well_log');
   error(' The first input argument must be a well log.')
end

if nargin  <= 3;
   iprint=0;
   if nargin < 2
      filename='';
   end
end  

line_feed=char(10);

% 	Replace NaNs (if they exist) by new null value
if isnull(wlog)
   wlog.curves(isnan(wlog.curves))=-999.25;
end
wlog.null=-999.25;

%      	If a 4-decimal representation of wlog.step is not sufficient, 
%       replace wlog.step by 0
temp=wlog.step*10000;
if temp-fix(temp) > 1.0e-6
   wlog.step=0;
end

lasheader=make_las20_header(wlog,line_feed,iprint);

if iprint == 1
   disp(lasheader)
end

%	Open file for writing
if ~isempty(filename)
   fid=open_file('wt',filename);
else
   fid=open_file('wt','*.las');
end

fprintf(fid,'%s',lasheader);

% 	ASCII data
mnems=char(wlog.curve_info{:,1});
[n,m]=size(mnems);
nchar=12;	% Number of characters per column
if m < nchar    % Append nchar-m blanks to mnemonics
   stemp=blanks(nchar-m);
   stemp=stemp(ones(n,1),:);
   mnems=[mnems,stemp];
else
   mnems=mnems(:,1:nchar);
end

lash1=['~A ', reshape(mnems',1,nchar*n),line_feed];
fprintf(fid,'%s',lash1);
if iprint == 1; fprintf('%s',lash1); end 

[n,m]=size(wlog.curves);


% Create format for ASCII data (minimum four digits)
 
% ac=abs(wlog.curves);
textformat=cell(m+1,1);
textformat{end,1}='\n';
for ii=1:m
   nd1=ceil(log10(max(abs(wlog.curves(:,ii))+eps)))+2;
   nd2=max(10-nd1,0);
   if nd1+nd2 < nchar-1
      nd1=nchar;
   else
      nd2=2;
      nd1=nd1+4;
   end
   if ii == 1 
      if wlog.step == 0 || fix(wlog.step*nd2) ~= wlog.step*nd2
         nd2=3; 
      end
   end
   textformat{ii}=['%',num2str(nd1),'.',num2str(nd2),'f'];
end
  
% textformat=[txtformat,'\n'];
textformat=cell2str(textformat,'');
fprintf(fid,textformat,wlog.curves');
if iprint == 1
   fprintf('%d of %d lines of curve data written\n',n,n)
 end
fclose(fid);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function lash=make_las20_header(wlog,line_feed,iprint)
% Function creates simple header for LAS file
% Date Feb. 19, 2000;  written by E. Rietsch
% INPUT
%  wlog           Log structure
%                     Required fields  
%  wlog.curves    Matrix of curve values
%		 Each log is in a column and the first column is usually the depth.
%                Thus, if logmat has n columns, then there are n-1 log curves.
%                The LAS format does not allow exponents in the curve values.
%                Hence care should should be taken to ascertain that the log values 
%                can be represented accurately enough with a total no more than 
%                2-3 decimal digits.      
%  wlog.mnem	Cell array with curve mnemonics
%  wlog.units	Cell  array with curve units of measurement
%  wlog.description Cell array with curve description
%  wlog.null	Null value (set to NaN)
%  wlog.first	Start of log (first depth in file)
%  wlog.last	Stop of log (last depth in file)
%  wlog.step	Depth increment (0 if unequal)
%  wlog.wellname	Well name
%  wlog.field       Name of the field (this could be WILDCAT)
%  wlog.location    Location of well
%  wlog.company     Oil/gas company which drilled the well
%
%              The following components, while desirable, are either optional 
%              or are replaced by defaults. They include
%                    Well information      
%   wlog.country	Country in which well is located
%   wlog.wellid	Well Identifier
%   wlog.service     Service company which logged the well
%   wlog.province    Province
%   wlog.county      County
%   wlog.state       State
%   wlog.date        Date
%   wlog.api         API number
%   wlog.uwi         Unique well ID
%
%              Parameters which have numerical values other than the null value are
%              written to the Parameter Section of the LAS header.
%              In the log structure a parameters is represented by field with 
%              the parameter name and a row in the cell array wlog.parameter_info.
%              Such a row consists of three strings: the first is the parameter 
%              mnemonic, the second the units of measurement and the third the 
%              parameter description.
%    wwlog.ekb=84	Kelly bushing elevation     
%    wwlog.egl=0		Ground level elevation
%    wwlog.parameter_info={'ekb','ft','Kelly bushing elevation';
%                         'egl','ft','Ground level elevation'}
%
% line_feed   line-feed character
% iprint   Control diagnostic output (optional)
%          iprint = 0  ==> no output; DEFAULT
%          iprint ~= 0 ==> diagnostic output
% OUTPUT   
% lash     LAS header
%
%            lash=make_las20_header(wlog,line_feed,iprint)

if nargin == 2, iprint=0; end

%  	Version information section
lash=['~VERSION INFORMATION SECTION',line_feed, ...
          'VERS.           2.0  :CWLS Log ASCII Standard - 2.0',line_feed, ...
          'WRAP.           NO   :One line per depth',line_feed, ...
          '#               Created in SeisLab',line_feed];
if iprint == 1, fprintf('... start well information section\n'), end

%	Well information section
stemp=wlog.curve_info{1,2};
start=['STRT.',stemp,'  ',num2str(wlog.first)];
stop=['STOP.',stemp,'  ',num2str(wlog.last)];
step=['STEP.',stemp,'  ',num2str(wlog.step)];
null=['NULL.    ',num2str(wlog.null)];
lash1=char('#MNEM.UNIT  VALUE/NAME',start,stop,step,null);
lash2=char('   :DESCRIPTION','   :Start Depth','   :Stop Depth','   :Step', ...
          '   :Null Value');

if isfield(wlog,'wellname') 
   lash1=char(lash1,['WELL.    ',wlog.wellname]);
else
   lash1=char(lash1,['WELL.    ','UNKNOWN']);
end
lash2=char(lash2,'   :Well Name');

if isfield(wlog,'country'), 
   lash1=char(lash1,['CTRY.    ',wlog.country]);
else
   lash1=char(lash1,['CTRY.    ','UNKNOWN']);
end
lash2=char(lash2,'   :Country');
 
if isfield(wlog,'county'), 
   lash1=char(lash1,['CNTY.    ',wlog.county]);
else
   lash1=char(lash1,['CNTY.    ','UNKNOWN']);
end
lash2=char(lash2,'   :County');

if isfield(wlog,'state'), 
   lash1=char(lash1,['STAT.    ',wlog.state]);
else
   lash1=char(lash1,['STAT.    ','UNKNOWN']);
end
lash2=char(lash2,'   :State');

if isfield(wlog,'api'), 
   lash1=char(lash1,['API .    ',wlog.api]);
else
   lash1=char(lash1,['API .    ','UNKNOWN']);
end
lash2=char(lash2,'   :API number');

if isfield(wlog,'uwi'), 
   lash1=char(lash1,['UWI .    ',wlog.uwi]);
else
   lash1=char(lash1,['UWI .    ','UNKNOWN']);
end
lash2=char(lash2,'   :Unique well ID');

if isfield(wlog,'location'),  
   lash1=char(lash1,['LOC .    ',wlog.location]);
else
  lash1=char(lash1,['LOC .    ','UNKNOWN']);
end
lash2=char(lash2,'   :Well Location');

if isfield(wlog,'field'), 
   lash1=char(lash1,['FLD .    ',wlog.field]);
else
   lash1=char(lash1,['FLD .    ','UNKNOWN']);
end
lash2=char(lash2,'   :Field');

if isfield(wlog,'company'), 
   lash1=char(lash1,['COMP.    ',wlog.company]);
else
   lash1=char(lash1,['COMP.    ','UNKNOWN']);
end
lash2=char(lash2,'   :Company');

if isfield(wlog,'service'),  
   lash1=char(lash1,['SRVC.    ',wlog.service]);
else
   lash1=char(lash1,['SRVC.    ','UNKNOWN']);
end
lash2=char(lash2,'   :Service Company');

if isfield(wlog,'date'), 
   lash1=char(lash1,['DATE.    ',wlog.date]);
else
   lash1=char(lash1,['DATE.    ',date]);
end
lash2=char(lash2,'   :Date');

lash=[lash,'~WELL INFORMATION SECTION',line_feed];
for ii=1:size(lash1,1)
   lash=[lash,lash1(ii,:),lash2(ii,:),line_feed]; %#ok
end
if iprint == 1, fprintf('... completed well information section\n'), end

%      Parameter information section (parameters are fields of the log structure
%      which matching entry in the first column of the cell array 
%      wlog.parameter_info
if ~isfield(wlog,'parameter_info')
   np=0;
else 
   parameters=wlog.parameter_info(:,1);
   np=length(parameters);
end

temps1='#MNEM';
temps1a='.UNITS';
temps2='  VALUE';
temps3='   :DESCRIPTION OF PARAMETER';
for ii=1:np
   temps=wlog.parameter_info(ii,2:3);
   temps1=char(temps1,parameters{ii});
   temps1a=char(temps1a,['.',temps{1}]);
%   temps2=char(temps2,['  ',num2str(getfield(wlog,parameters{ii}))]);
   temps2=char(temps2,['  ',num2str(wlog.(parameters{ii}))]);
   temps3=char(temps3,['   :',temps{2}]);
end
lash=[lash,'~PARAMETER INFORMATION SECTION',line_feed];

for ii=1:size(temps1,1)
   lash=[lash,temps1(ii,:),temps1a(ii,:),temps2(ii,:),temps3(ii,:),line_feed]; %#ok
end

if iprint == 1 
   fprintf('... completed parameter information section\n')
end

%	Curve information section
if iprint == 1
   fprintf('... start curve information section\n')
 end
ncurves=size(wlog.curves,2);

temps1='#MNEM';
temps1a='.UNITS';
temps1b='  API CODES';
temps2='   :DESCRIPTION OF MNEMONIC';

for ii=1:ncurves
   temps1=char(temps1,wlog.curve_info{ii,1});
   temps1a=char(temps1a,['.',wlog.curve_info{ii,2}]);
   temps1b=char(temps1b,' ');
   temps2=char(temps2,['   :',wlog.curve_info{ii,3}]);
end
  
% lash=char(lash,'~CURVE INFORMATION SECTION',[temps1,temps2]); 
lash=[lash,'~CURVE INFORMATION SECTION',line_feed];
for ii=1:size(temps1,1)
   lash=[lash,temps1(ii,:),temps1a(ii,:),temps1b(ii,:),temps2(ii,:),line_feed]; %#ok
end       
if iprint == 1, fprintf('... completed curve information section\n'), end


%	Other information
if isfield(wlog,'comments')
%  lash=char(lash,'~Other information',wlog.comments);
  lash=[lash,'~Other information',wlog.comments,line_feed];
end
