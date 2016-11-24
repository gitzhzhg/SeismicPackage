function [wlog,aux]=interpret_las2(tlines,wrap)
% Interpret the lines of a file in LAS 3.0 format and create a well-log
% structure.
%
% Written by: E. Rietsch: December 19, 2006
% Last updated: February 7, 2007: Handle parameters without a preceding ~PARAMETER line
%
%          [wlog,aux]=interpret_las2(tlines)      
% INPUT
% tlines   non-blank lines of the LAS 2.0 file (except for the first two lines
% OUTPUT
% wlog     well log structure
% aux      auxiliary information

global S4M L_WPLOG_REPEAT

aux=[];
L_WPLOG_REPEAT=true;   % Used to avoid repeating a specific message

%       Wrap info
comp=split_line(tlines{1});
if strcmp(comp{1},'WRAP')
   if ~strcmp(strtrim(comp{2}),'NO')
      disp('LAS file is wrapped.')
      wrap=true;
   else
      wrap=false;
   end
else
   error(['Line ',tlines{1},' should have wrap information.'])
end

[dummy,filename]=fileparts(S4M.filename);
wlog=struct('type','well_log','tag','unspecified','name',filename, ...
            'first',[],'last',[],'step',[],'units','','null',-999.25, ...
            'from',S4M.filename,'curve_info',[],'curves',[]);

%       Section-start lines
index=find(~cellfun(@isempty,strfind(tlines','~')));
index=[index,length(tlines)+1];
for ii=1:length(index)-1
   tline=tlines{index(ii)};
   switch tline(1:2)

   case '~W'
      wlog=well_section_no1(wlog,tlines(index(ii)+1:index(ii+1)-1));

   case '~C'
      wlog=curve_section_no3(wlog,tlines(index(ii)+1:index(ii+1)-1));

   case '~P'
      wlog=parameter_section_no2(wlog,tlines(index(ii)+1:index(ii+1)-1));

   case '~A'
       wlog=data_section_no4(wlog,tlines(index(ii)+1:index(ii+1)-1),wrap);
 
   otherwise
      % Disregard section
      
   end
end

wlog=fix_las_file_log(wlog);
clear global L_WPLOG_REPEAT


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=well_section_no1(wlog,tlines)
% Read well-section information

global L_WPLOG_REPEAT

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
      if ~isempty(comp{2})
        wlog.license_number=comp{2};
      end

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
 
   otherwise
       disp([' Unexpected keyword in ~WELL INFORMATION section: ',comp{1}])
       if L_WPLOG_REPEAT
          disp(' Assume that the ~PARAMETER INFORMATION line is missing.')
          L_WPLOG_REPEAT=false;
       end
       wlog=parameter_section_no2(wlog,tlines(ii));

   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=parameter_section_no2(wlog,tlines)
% Read parameter section (or misplaced parameters)

nlines=length(tlines);
if ~isfield(wlog,'parameter_info')
   wlog.parameter_info=cell(nlines,3);
   ik=0;
else
   ik=size(wlog.parameter_info,1); 
end

for ii=1:nlines
   comp=split_line(tlines{ii});
   comp1=split_units_values(comp{2});
   values=str2num(comp1{2});             %#ok  Can be more than one value
   if ~isempty(values)
      ik=ik+1;
      mnem=lower(strrep(comp{1},' ','_'));
      units=unit_substitution(comp1{1}); % Create standardized units of measurement
      wlog.parameter_info(ik,:)={mnem,units,comp{3}};
      wlog.(mnem)=values;      
   end
end
if ik == 0
   wlog=rmfield(wlog,'parameter_info');
else
   wlog.parameter_info(ik+1:end,:)=[];
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=curve_section_no3(wlog,tlines)
% Read curve-section information

global S4M

nlines=length(tlines);
wlog.curve_info=cell(nlines,3);

for ii=1:nlines
   comp=split_line(tlines{ii});
   comp1=split_units_values(comp{2});
   if S4M.case_sensitive
      mnem=strrep(comp{1},' ','_');
   else
      mnem=lower(strrep(comp{1},' ','_'));
   end
   units=unit_substitution(comp1{1}); % Create standardized units of measurement
   wlog.curve_info(ii,:)={mnem,units,comp{3}};
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=data_section_no4(wlog,tlines,wrap)
% Read data in data section

global S4M

nlines=length(tlines);
ncols=size(wlog.curve_info,1);

if wrap
   curves=zeros(nlines*ncols,1,S4M.precision);
   ia=1;
   for ii=1:nlines
      [temp,nnum]=sscanf(tlines{ii},'%g');
      ie=ia+nnum-1;
      curves(ia:ie)=temp;
      ia=ie+1;
   end
   wlog.curves=reshape(curves(1:ie),ncols,[])';

else
   curves=zeros(nlines,ncols,1,S4M.precision);
   for ii=1:nlines
      curves(ii,:)=sscanf(tlines{ii},'%g')';
   end
   wlog.curves=curves;
end
