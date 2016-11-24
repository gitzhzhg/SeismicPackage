function wlog=l_merge(wlog,curves,varargin)
% Function replaces the null values of one curve with the corresponding 
% samples of another
%
% Written by: E. Rietsch: October 4, 2002
% Last updated: August 16, 2003: Renamed
%
%         wlog=l_merge(wlog,curves,varargin)
% INPUT
% wlog    well log
% curves  cell array with two curve mnemonics; null values of the curve
%         represented by the first curve mnemonic are replaced by the
%         corresponding values of the curve represented by the second 
%         curve mnemonic         
% varargin
% OUTPUT
% wlog    well log with the new curve

global S4M

%    Set default values for input parameters
param.mnem=[];

%       Decode and assign input arguments
param=assign_input(param,varargin);

if isempty(param.mnem)
  param.mnem=curves{1};
end

curve1=l_gc(wlog,curves{1});
curve2=l_gc(wlog,curves{2});

idx=find(isnan(curve1));
curve1(idx)=curve2(idx);

if S4M.case_sensitive
   if strcmp(param.mnem,curves{1})
      wlog=l_curve(wlog,'replace',curves{1},curve1);
   elseif strcmp(param.mnem,curves{2})
      wlog=l_curve(wlog,'replace',curves{2},curve1);
   else
      wlog=l_curve(wlog,'add_ne',param.mnem,curve1,l_gu(wlog,curves{1}), ...
         [l_gd(wlog,curves{1}),' (corrected)']);
   end

else
   if strcmpi(param.mnem,curves{1})
      wlog=l_curve(wlog,'replace',curves{1},curve1);
   elseif strcmpi(param.mnem,curves{2})
      wlog=l_curve(wlog,'replace',curves{2},curve1);
   else
      wlog=l_curve(wlog,'add_ne',param.mnem,curve1,l_gu(wlog,curves{1}), ...
         [l_gd(wlog,curves{1}),' (corrected)']);
  end
end  

