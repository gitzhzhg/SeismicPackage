function [wlog,aux]=interpret_las3(tlines,param)
% Interpret the lines of a file in LAS 3.0 format and create a well-log
% structure. Called by "read_las_file".
%
% Written by: E. Rietsch: December 16, 2006
% Last updated: January 31, 2007: Write parameters only if they are not empty
%
%          [wlog,aux]=interpret_las3(tlines)      
% INPUT
% tlines   non-blank lines of the LAS 3.0 file (except for the first two lines)
% param    parameters passed on by the calling program "read_las_file"
% OUTPUT
% wlog     well log structure
% aux      auxiliary information (nothing yet)

global S4M L_WPLOG_REPEAT

aux=[];
L_WPLOG_REPEAT=true;   % Used to avoid repeating a specific message

%       Wrap info
comp=split_line(tlines{1});
if strcmp(comp{1},'WRAP')
   if ~strcmp(strtrim(comp{2}),'NO')
      error('Line wrapping is not allowed in LAS 3.0')
   end
else
   error(['Line ',tlines{1},' should have wrap information.'])
end

%       Delimiter info
comp=split_line(tlines{2});
if strcmp(comp{1},'DLM')
   delim=strtrim(comp{2});
   bool=ismember(delim,{'SPACE','COMMA','TAB'});
  if ~any(bool)
      error(['Unknown delimiter "',delim,'"'])
   end
else
   disp([' Line ',tlines{2}])
   disp(' should have delimiter information.')
   error('Abnormal termination')
end

[dummy,filename]=fileparts(S4M.filename);
wlog=struct('type','well_log','tag','unspecified','name',filename, ...
            'first',[],'last',[],'step',[],'units','','null',-999.25, ...
            'from',fullfile(S4M.pathname,S4M.filename));

%       Section-start lines
index=find(~cellfun(@isempty,strfind(tlines','~'))); % Requires Matlab version > 7.04
% index=find(~cellfun('isempty',strfind(tlines','~')));
index=[index,length(tlines)+1];
for ii=1:length(index)-1
   tline=tlines{index(ii)};
   idx=strfind(tline,'|');
   if isempty(idx)
      temp=tokens(tline,' ');
%      association=[];
   else
      temp1=tokens(tline,'|');
      temp=tokens(temp1{1},' ');
%      association=strtrim(temp1{2});
   end

%   temp=split_units_values(tlines{index(ii)});
   switch_parameter=upper(temp{1});

   switch switch_parameter

   case '~WELL'
      wlog=well_section_no1(wlog,tlines(index(ii)+1:index(ii+1)-1));

   case '~PARAMETER'
      wlog=parameter_section_no9(wlog,tlines(index(ii)+1:index(ii+1)-1));

   case '~CURVE'
      wlog=curve_section_no3(wlog,tlines(index(ii)+1:index(ii+1)-1));

   case {'~ASCII','~LOG'}
%      if ~isempty(association)  &&  strcmpi(association,'CURVE')
       wlog=data_section_no4(wlog,tlines(index(ii)+1:index(ii+1)-1),delim);
%      end
 
   otherwise
      wlog=section_analysis_no6(wlog,tlines(index(ii)+1:index(ii+1)-1), ...
           delim,switch_parameter,param);
      
   end
end

wlog=fix_las_file_log(wlog);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=well_section_no1(wlog,tlines)
% Read well-section information

global S4M L_WPLOG_REPEAT

for ii=1:length(tlines)
   comp=split_line(tlines{ii});
   comp{2}=strtrim(comp{2});
   switch lower(comp{1})

   case 'strt'
      comp1=split_units_values(comp{2});
      wlog.first=str2double(comp1{2});
      units=unit_substitution(comp1{1});
      if isempty(wlog.units)
         wlog.units=units;
      elseif ~strcmp(units,wlog.units)
         error(['Depth units are incompatible: ',wlog.units,' and ',units])
      end

   case 'stop'
      comp1=split_units_values(comp{2});
      wlog.last=str2double(comp1{2});
      units=unit_substitution(comp1{1});
      if isempty(wlog.units)
         wlog.units=units;
      elseif ~strcmp(units,wlog.units)
         error(['Depth units are incompatible: ',wlog.units,' and ',units])
      end
  
   case 'step'
      comp1=split_units_values(comp{2});
      wlog.step=str2double(comp1{2});
      units=unit_substitution(comp1{1});
      if isempty(wlog.units)
         wlog.units=units;
      elseif ~strcmp(units,wlog.units)
         error(['Depth units are incompatible: ',wlog.units,' and ',units])
      end

   case 'null'
       wlog.null=str2double(comp{2});
  
   case 'comp'
       if ~isempty(comp{2})
          wlog.company=comp{2};
       end

   case 'well'
       if ~isempty(comp{2})
          wlog.wellname=comp{2};
       end

   case 'loc'
       if ~isempty(comp{2})
          wlog.location=comp{2};
       end

   case 'fld'
       if ~isempty(comp{2})
          wlog.field=comp{2};
       end
 
   case 'ctry'
       if ~isempty(comp{2})
          wlog.country=comp{2};
       end


%       For Canada
   case 'prov'
       if ~isempty(comp{2})
          wlog.province=comp{2};
       end

   case 'uwi'
       if ~isempty(comp{2})
          wlog.wellid=comp{2};
       end

   case 'lic'
       wlog.license_number=comp{2};

%       For US
   case 'stat'
       if ~isempty(comp{2})
          wlog.state=comp{2};
       end

   case 'cnty'
       if ~isempty(comp{2})
          wlog.county=comp{2};
       end

   case 'api'
       if ~isempty(comp{2})
          wlog.api=comp{2};
       end
%      end of "For US"
   
   case 'srvc'
       if ~isempty(comp{2})
          wlog.service=comp{2};
       end

   case 'date'
       if ~isempty(comp{2})
          wlog.date=comp{2};
       end

 
 %      Either 
   case 'lat'
       comp1=split_units_values(comp{2});
       if ~isempty(comp{2}) ||  ~isempty(comp{2})
          wlog.latitude=[comp1{2},' ',comp1{1}];
       end

   case 'long'
       comp1=split_units_values(comp{2});
       if ~isempty(comp{2}) ||  ~isempty(comp{2})
          wlog.longitude=[comp1{2},' ',comp1{1}];
       end
 
  %     or 
   case 'x'
       if ~isempty(comp{2})
          wlog.x=comp{2};
       end

   case 'y'
       if ~isempty(comp{2})
          wlog.y=comp{2};
       end

   case 'hzcs'
       if ~isempty(comp{2})
          wlog.hor_coord_system=comp{2};
       end
   %    end of "or"
  
   case 'gdat'
       comp1=split_units_values(comp{2});
       if ~isempty(comp{2}) ||  ~isempty(comp{2})
          wlog.geodetic_datum=strtrim([comp1{2},' ',comp1{1}]);
       end


   case 'utm'
       if ~isempty(comp{2})
          wlog.utm=comp{2};
       end

 
   otherwise
       % warning(['Unexpected parameter: ',comp{1}])
      disp([' Unexpected keyword in ~WELL INFORMATION section: ',comp{1}])
      if L_WPLOG_REPEAT
         disp(' Assume that the ~PARAMETER INFORMATION line is missing.')
         L_WPLOG_REPEAT=false;
      end
      wlog=parameter_section_no9(wlog,tlines(ii));


   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [descr,fmt]=extract_format_no2(str)
% Extract the format from the description section

idx1=strfind(str,'{');
if ~isempty(idx1)
   idx2=strfind(str,'}');
   if ~isempty(idx2)  &&  idx2 > idx1+1
      fmt=strtrim(str(idx1+1:idx2-1));
      descr=strtrim(str(1:idx1-1));
   else
      warning(warnid,['Error in description: ',str])
      descr=strtrim(str);
      fmt=[];  
   end
else
   descr=strtrim(str);
   fmt=[];
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=curve_section_no3(wlog,tlines)
% Read curve-section information

nlines=length(tlines);
wlog.curve_info=cell(nlines,5);

for ii=1:nlines
   comp=split_line(tlines{ii});
   comp1=split_units_values(comp{2});
   [descr,fmt]=extract_format_no2(comp{3});
   %    If no format has been defined set it depending on the units of measurement
   if isempty(fmt)
      if isempty(comp1{1})
         fmt='S';
      else
         fmt='F';
      end
   end
   mnem=fix_mnem_no5(comp{1});  % Remove blanks and brackets from mnemonics
   units=unit_substitution(comp1{1}); % Create standardized units of measurement
   wlog.curve_info(ii,:)={mnem,units,descr,fmt,comp1{2}};
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=data_section_no4(wlog,tlines,delim)
% Read data in data section

fmt0=wlog.curve_info(:,4);

[wlog,curves,bool,nlines,ncols]=data_section_no10(wlog,tlines,delim,fmt0);

if all(bool)   % In case there are no strings
   wlog.curves=cellfun(@str2double,curves);  % Requires Matlab version > 7.04
   % wlog.curves=cellfun('str2double',curves);

else           % In case there are strings
   wlog.curves=zeros(nlines,ncols);
   idx=find(bool);
   lidx=length(idx);
   wlog.row_label_info=wlog.curve_info(~bool,:);

   %  Make mnemonics for string columns lower-case
   wlog.row_label_info(:,1)=lower(wlog.row_label_info(:,1)); 
   
   wlog.curves(:,1:length(idx))=cellfun(@str2double,curves(:,idx));  % Requires Matlab version > 7.04
   % wlog.curves(:,1:length(idx))=cellfun('str2double',curves(:,idx));
   wlog.curve_info=[wlog.curve_info(bool,:);wlog.row_label_info];
   wlog.row_labels=curves(:,~bool);
   for ii=1:size(wlog.row_label_info,1)
      temp=unique(wlog.row_labels(:,ii));
      nlabels=length(temp);
      wlog.row_label_codes.(wlog.row_label_info{ii,1})= ...
                               [temp,num2cell((1:nlabels)')];
      for jj=1:nlabels
         bool1=~cellfun(@isempty,strfind(wlog.row_labels(:,ii),temp{jj})); % Requires Matlab version > 7.04
         % bool1=~cellfun('isempty',strfind(wlog.row_labels(:,ii),temp{jj}));
         wlog.curves(bool1,lidx+ii)=jj;
      end
   end
   wlog.row_label_info=wlog.row_label_info(:,1:3);
end

wlog.curve_info=wlog.curve_info(:,1:3);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function mnem=fix_mnem_no5(mnem)
% Remove blanks and bracketed indices from mnemonic of LAS file
% e.g. mnemonic[3] ==> mnemonic_3

%       Find blanks and replace by underscores
mnem=strrep(mnem,' ','_');

%       Find bracketed indices and replace by underscores followed by indices
idx1=strfind(mnem,'[');
if isempty(idx1)
   return
else
   num=mnem(idx1+1:end-1);
   mnem=[mnem(1:idx1-1),'_',num];
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=section_analysis_no6(wlog,tlines,delim,switch_parameter,param)
% Check selected less-conventional sections

comp=tokens(switch_parameter,'_');
section=lower(comp{1}(2:end));

if (~isempty(param.section) && ismember({section},param.section))  ||   ...
     strcmpi(param.section,'all')
   switch upper(comp{2})
   case 'PARAMETER'
      wlog=info4section_no7(wlog,tlines,section,'parameters');
   case 'DEFINITION'
      wlog=info4section_no7(wlog,tlines,section,'info');
   case 'DATA'
      wlog=data4section_no8(wlog,tlines,delim,section);  
   otherwise
      % One should never get here
   end
   
 else
   if length(comp) == 1 || strcmp(upper(comp{2}),'DATA')
      disp([' Section "',section,'" ignored.'])
      keyboard
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=info4section_no7(wlog,tlines,first_name,last_name)
% Read parameter-section information

field=[first_name,'_',last_name];
nlines=length(tlines);
wlog.(field)=cell(nlines,5);

for ii=1:nlines
   comp=split_line(tlines{ii});
   comp1=split_units_values(comp{2});
   [descr,fmt]=extract_format_no2(comp{3});
   %    If no format has been defined set it depending on the units of measurement
   if isempty(fmt)
      if isempty(comp1{1})
         fmt='S';
      else
         fmt='F';
      end
   end
   mnem=fix_mnem_no5(comp{1});  % Remove blanks and brackets from mnemonics
   units=unit_substitution(comp1{1}); % Create standardized units of measurement
   wlog.(field)(ii,:)={mnem,units,descr,fmt,comp1{2}};
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=data4section_no8(wlog,tlines,delim,first_name)
% Read data in data section

%label=[first_name,'_label'];      % Field name for label
data=[first_name,'_data'];         % Field name for data
label_info=[first_name,'_info'];   % Field name for info
fmt0=wlog.(label_info)(:,4);

[wlog,curves,bool]=data_section_no10(wlog,tlines,delim,fmt0);

%{
nlines=length(tlines);
ncols=length(fmt0);
nerrors=0;

%       Convert cell vector of lines into cell array of matrix elements
curves=cell(nlines,ncols);

%       Set the delimiter
switch lower(delim)
case 'blank'
   sep=' ';
   %    Remove consecutive blanks so that they are not interpreted as 
   %    indicating missing data values
   tlines=remove_consecutive_blanks(tlines);
case 'tab';
   sep=sprintf('\t');
case 'comma'
   sep=',';
otherwise
   error([' Unknown delimiter :',delim])
end

for ii=1:nlines
   temp=tokens(tlines{ii},sep);
   ltemp=length(temp);
   if ltemp == ncols
      curves(ii,:)=temp;
   elseif ltemp > ncols  % Combine strings
      index=find(~cellfun(@isempty,strfind(temp,'"'))); % Requires Matlab version > 7.04
      % index=find(~cellfun('isempty',strfind(temp,'"')));
      if ~mod(length(index),2) == 0
         nerrors=nerrors+1;
         if nerrors <= 10
            disp([' Problem with a string in line ',num2str(ii),' of the data'])
            disp(' String is not enclosed in double quotes.')
            disp(tlines{ii})
            if nerrors == 10
               disp(' Any additional errors are not displayed')
            end
         end  
      else
         for jj=1:2:length(index)
            temp1=cell2str(temp(index(jj):index(jj+1)),', ');
            temp{index(jj)}=temp1(2:end-1);
            temp(index(jj)+1:index(jj+1))=[];
         end
      end
   
      if length(temp) ~= ncols
         nerrors=nerrors+1;
         if nerrors <= 10
            disp([' Problem with a string in data line ',num2str(ii)])
            disp(tlines{ii})
            if nerrors == 10
               disp(' Any additional errors are not displayed')
            end
         end
      else
         curves(ii,:)=temp;
      end
   else
      nerrors=nerrors+1;
      if nerrors <= 10
         disp([' There are fewer than ',num2str(ncols),' data values in line ',num2str(ii)])
         disp(tlines{ii})
         if nerrors == 10
            disp(' Any additional errors are not displayed')
         end
      end
   end
end

%       Find columns with strings
bool=true(1,ncols);
for ii=1:ncols
   if strcmpi(fmt0{ii}(1),'S')  ||  ...
      (length(fmt0{ii}) > 1 && strcmpi(fmt0{ii}(1:2),'AS'))
      bool(ii)=false;
   end
end
%}

if all(bool)   % In case there are no strings
   wlog.(data)=cellfun(@str2double,curves); % Requires Matlab version > 7.04
   % wlog.(data)=cellfun('str2double',curves);

else           % In case there are strings
   wlog.(data)=[num2cell(cellfun(@str2double,curves(:,bool))),curves(:,~bool)]; % Requires Matlab version > 7.04
   % wlog.(data)=[num2cell(cellfun('str2double',curves(:,bool))),curves(:,~bool)];
end

wlog.(label_info)=wlog.(label_info)(:,1:3);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=parameter_section_no9(wlog,tlines)
% Read parameter-section information pf LAS 3.0 file

nlines=length(tlines);
wlog.parameter_info=cell(nlines,3);

ik=0;
for ii=1:nlines
   comp=split_line(tlines{ii});
   if isempty(comp{4})   % No vertical bar
      comp1=split_units_values(comp{2});
      values=str2num(comp1{2});   %#ok More than one numeric value is possible
      if ~isempty(values)
         [descr,fmt]=extract_format_no2(comp{3});
   %    If no format has been defined set it depending on the units of measurement
         if isempty(fmt)
            if isempty(comp1{1})
               fmt='S';
            else
               fmt='F';
            end
         end
         mnem=lower(fix_mnem_no5(comp{1}));  % Remove blanks and brackets from mnemonics
         units=unit_substitution(comp1{1}); % Create standardized units of measurement
         if ~strcmpi(fmt(1),'S')
            ik=ik+1;
            wlog.parameter_info(ik,:)={mnem,units,descr};
            wlog.(mnem)=values;   
         else
            % Disregard parameters that have no numeric values
         end
      end
   else
         % Disregard parameter definitions that refer to different runs
   end
end

if ik > 0
   wlog.parameter_info=wlog.parameter_info(1:ik,:);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [wlog,curves,bool,nlines,ncols]=data_section_no10(wlog,tlines,delim,fmt0)

% Used in "data_section_no4" and in "data4section_no8"

nlines=length(tlines);
ncols=length(fmt0);
nerrors=0;

%       Convert cell vector of lines into cell array of matrix elements
curves=cell(nlines,ncols);

%       Set the delimiter
switch lower(delim)
case 'blank'
   sep=' ';
   %    Remove consecutive blanks so that they are not interpreted as 
   %    indicating missing data values
   tlines=remove_consecutive_blanks(tlines);
case 'tab';
   sep=sprintf('\t');
case 'comma'
   sep=',';
otherwise
   error([' Unknown delimiter :',delim])
end

for ii=1:nlines
   temp=tokens(tlines{ii},sep);
   ltemp=length(temp);
   if ltemp == ncols
      curves(ii,:)=temp;
   elseif ltemp > ncols  % Combine strings
      index=find(~cellfun(@isempty,strfind(temp,'"'))); % Requires Matlab version > 7.04
%      index=find(~cellfun('isempty',strfind(temp,'"')));
      if ~mod(length(index),2) == 0
         nerrors=nerrors+1;
         if nerrors <= 10
            disp([' Problem with a string in line ',num2str(ii),' of the data'])
            disp(' String is not enclosed in double quotes.')
            disp(tlines{ii})
            if nerrors == 10
               disp(' Any additional errors are not displayed')
            end
         end  
      else
         for jj=1:2:length(index)
            temp1=cell2str(temp(index(jj):index(jj+1)),', ');
            temp{index(jj)}=temp1(2:end-1);
            temp(index(jj)+1:index(jj+1))=[];
         end
      end
   
      if length(temp) ~= ncols
         nerrors=nerrors+1;
         if nerrors <= 10
            disp([' Problem with a string in data line ',num2str(ii)])
            disp(tlines{ii})
            if nerrors == 10
               disp(' Any additional errors are not displayed')
            end
         end
      else
         curves(ii,:)=temp;
      end
   else
      nerrors=nerrors+1;
      if nerrors <= 10
         disp([' There are fewer than ',num2str(ncols),' data values in line ',num2str(ii)])
         disp(tlines{ii})
         if nerrors == 10
            disp(' Any additional errors are not displayed')
         end
      end
   end
end

%       Find columns with strings
bool=true(1,ncols);
for ii=1:ncols
   if strcmpi(fmt0{ii}(1),'S')  ||  ...
      (length(fmt0{ii}) > 1 && strcmpi(fmt0{ii}(1:2),'AS'))
      bool(ii)=false;
   end
end
