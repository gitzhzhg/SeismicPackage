function wlog=l_combine(wlog1,wlog2,varargin)
% Combine two logs in such a way that the curves of the second log follow 
% "below" the curves of the first log. There are two options:
%  1.    Only curves common to both logs are used (common means they have the
%        same curve mnemonics).
%  2.    All curves are used. Values of curves not present in one log are set
%        to the null value.
% An index curve is added; the depth columns become normal curves
% A well log identifier is added to the output log.
% Written by: E. Rietsch: August 16, 2003
% Last updated: September 17, 2003: copy parameters
%
%          wlog=l_combine(wlog1,wlog2,varargin)
% INPUT
% wlog1    first well log
% wlog2    second well log
% varargin  one or more cell arrays; the first element of each cell array is a 
%          keyword, the other elements are parameters. Presently, keywords are:
%          'option'  defines what to do with curves that are not present in both
%                logs. Possible values are 'all' (curves of both wells are used)
%                and 'common' (only the curves common to both logs are used)
%                Default: {'option','common'}
% OUTPUT
% wlog     new well log where the curves of the second log are "below"
%          the curves of the first log

%	Set defaults for input parameters
param.option='common';

%	Assign values of input parameters
param=assign_input(param,varargin);

wlog=wlog1;

%       Combine the parameters from the two logs
wlog=combine_parameters(wlog1,wlog2,wlog);

mnems1=wlog1.curve_info(:,1);
mnems2=wlog2.curve_info(:,1);

%	Create curves with well identifier (if not yet present)
idx1=find(ismember(mnems1,'wellid'));
idx2=find(ismember(mnems2,'wellid'));
if isempty(idx1) && isempty(idx2)
   wlog1=l_curve(wlog1,'add','wellid',1,'n/a','Well identifier');
   wlog2=l_curve(wlog2,'add','wellid',2,'n/a','Well identifier');
   mnems1=wlog1.curve_info(:,1);
   mnems2=wlog2.curve_info(:,1);
   wlog.wellname='multi-well data';
   wlog.wellnames={wlog1.wellname;wlog2.wellname};
   wlog.wellids=[1;2];

elseif ~isempty(idx1) && isempty(idx2)
   wellid=max(wlog1.curves(:,idx1))+1;
   wlog2=l_curve(wlog2,'add','wellid',wellid,'n/a','Well identifier');
   mnems2=wlog2.curve_info(:,1);
   wlog.wellnames=[wlog1.wellnames;{wlog2.wellname}];
   wlog.wellids=[wlog1.wellids;wellid];

elseif isempty(idx1) && ~isempty(idx2)
   wellid=max(wlog2.curves(:,idx2))+1;
   wlog1=l_curve(wlog1,'add','wellid',wellid,'n/a','Well identifier');
   mnems1=wlog1.curve_info(:,1);
   wlog.wellnames=[{wlog1.wellname},wlog2.wellnames];
   wlog.wellids=[wlog2.wellids;wellid];

else
   wellids1=unique(wlog1.curves(:,idx1));
   wellids2=unique(wlog1.curves(:,idx2)); 
   if ~isempty(intersect(wellids1,wellids2))
      wlog2.curves(:,idx2)=wlog2.curves(:,idx2)+max(wellids1);
   end
   wlog.wellnames=[wlog1.wellnames;wlog1.wellnames];
   wlog.wellids=[wlog1.wellids;wlog2.wellids+max(wellids1)];
end
%idx=find(ismember('wellid',mnems2))

[mnems,index1,index2]=intersect(mnems1,mnems2);
%[index1,idx]=sort(index1);
%index2=index1(idx);

if isempty(mnems) 
   if strcmpi(param.option,'common')
      disp(' Mnemonics of first well log:')
      disp(mnems1)
      disp(' Mnemonics of second well log:')
      disp(mnems2)
      error(' No common curves found')
   end
else
   info1=wlog1.curve_info(index1,:);
   info2=wlog2.curve_info(index2,:);
   for ii=1:length(index1)
      if ~strcmpi(info1{ii,2},info2{ii,2})
         wlog2=ds_unit_conversion(wlog2,{info2{ii,2},info1{ii,2}});
	 alert([' Units "',info2{ii,2},'" of second log converted to units "',info1{ii,2}])	
      end
   end
end

if strcmpi(param.option,'common')
    curves=[wlog1.curves(:,index1);wlog2.curves(:,index2)];    
    wlog.curve_info=wlog1.curve_info(index1,:);

elseif strcmpi(param.option,'all')
%   index2=find(~ismember(mnems2,mnems1));
   mnems=[mnems1;mnems2(~ismember(mnems2,mnems1))];
   lmnems1=length(mnems1);
   index1=1:length(mnems1);
   [dummy,index,index2]=intersect(mnems(lmnems1+1:end),mnems2); %#ok  First output argument not required
   wlog.curve_info=cell(length(mnems),3);
   index=index+lmnems1;
   wlog.curve_info(index,:)=wlog2.curve_info(index2,:);
   wlog.curve_info(index1,:)=wlog1.curve_info;
   [nsamp1]=size(wlog1.curves,1);
   [nsamp2]=size(wlog2.curves,1);
   curves=NaN*zeros(nsamp1+nsamp2,length(mnems));  
   curves(1:nsamp1,index1)=wlog1.curves;
   [dummy,index,index2]=intersect(mnems,mnems2); %#ok  First output argument not required
   curves(nsamp1+1:nsamp1+nsamp2,index)=wlog2.curves(:,index2);
else
   disp(' Unknown option')
   error('Abnormal termination')
end

wlog.curves=curves;
wlog=l_depth2index(wlog);
