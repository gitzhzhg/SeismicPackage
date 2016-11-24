function wlog=l_switch_depth(wlog,new_depth)
% Function replaces "depth" column (1st column) of the log structure with another column
% Neither column may contain null values. 
% Generally used to convert from depth to time and vice versa
%
% Written by: E. Rietsch: November 7, 2000
% Last update: January 1, 2006: Add/replace field units
%
%	      wlog=l_switch_depth(wlog,new_depth)
% INPUT
% wlog        log structure whose depth column needs to be switched
% new_depth   mnemonic of column to be used as new depth
% OUTPUT
% wlog 	      log structure with the with the new "depth" column

if ~isstruct(wlog)
  error(' First input data set must be log structure')
end

index=curve_index1(wlog,new_depth);
if index == 1           % Requested depth column is already the first column in matrix "wlog.curves"
   return
end

temp=wlog.curves(:,index);
if isfield(wlog,'null')
   ilog=isnan(temp);
%    idx=find(ilog);
   if sum(ilog) > 0
      idx=find(~ilog);
      wlog.curves=wlog.curves(idx,:);
      temp=temp(idx);
      disp([' Alert from "l_switch_depth": Rows with null values in new depth column "',new_depth,'" have been dropped'])
   end
end
temp_info=wlog.curve_info(index,:);

wlog.curves(:,index)=wlog.curves(:,1);
wlog.curve_info(index,:)=wlog.curve_info(1,:);
wlog.curves(:,1)=temp;
wlog.curve_info(1,:)=temp_info;

wlog.first=temp(1);
wlog.last=temp(end);
wlog.units=wlog.curve_info{1,2};

%	Check if new depth is uniformly sampled
dd=diff(temp);
bool=isconstant(dd,0.002);
if bool
   wlog.step=mean(dd);
else
   wlog.step=0;
end
            
