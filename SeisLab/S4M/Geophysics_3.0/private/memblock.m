function mem=memblock
% Compute the size of the largest contiguous memory block; works only on PC's
%
% Written by: E. Rietsch: December 31, 2006
% Last updated:
%
%       mem=memblock
% OUTPUT
% mem   size of the largest contiguous memory block in bytes

if ~ispc
   error(' This function works only on PC''s.')
end


% temp=tokens(evalc('feature(''memstats'')'),'\n');
temp=textscan(evalc('feature(''memstats'')'),'%s','delimiter','\n'); % "feature" 
                                          % is an undocumented built-in function
temp=temp{1};

for ii=length(temp)-10:-1:1
   if strcmpi(temp(ii),'Largest Contiguous Free Blocks:')
%      text=tokens(temp{ii+1},' ');
      text=textscan(temp{ii+1},'%s');
      text=text{1};
      break
   end
end


str=text{end};
mem=hex2dec(str(2:end-1));
