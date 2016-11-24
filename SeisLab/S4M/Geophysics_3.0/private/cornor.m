function [cc,lag]=cornor(s1,s2,type)
%  Function computes normalized crosscorrelation between s1 and s2
%  for all possible shifts.
%
%      [cc,lag]=cornor(s1,s2,type)
% INPUT
% s1      vector
% s2      vector
% type    denotes the type of output; 
%          type='max' output maximum correlation only.
%          type='maxabs' output maximum of absolute value of correlation 
%                only 
%          type='absmax' output correlation with the largest absolute value only
%                (unlike for 'maxabs', this can be positive or negative; actually,
%                the value obtained with 'maxabs' is the absolute value of that 
%                obtained with 'absmax'). If type is not given, this is the default. 
%          type='all'    output all correlation coefficients; in this case the 
%                output parameter lag is empty.
% OUTPUT
% cc       normalized crosscorrelations as requested via parameter type
% lag      shift between s1 and s2; if lag == 0 then s1 and s2 are aligned;
%          positive values indicate that s2 is delayed with respect to s1.
%          if type == 'all'  lag is empty   

[ns1,ms1]=size(s1);
[ns2,ms2]=size(s2);

if norm(s1) == 0 || norm(s2) == 0
   cc=0;
   lag=0;
   return
end

if nargin < 3
   type='absmax';
end

% keyboard
% s1=s1*sparse(diag(1./sqrt(sum(s1.*s1))));   s2=s2*sparse(diag(1./sqrt(sum(s2.*s2))));
s1=full(s1*spdiags(1./sqrt(sum(s1.*s1))',0,ms1,ms1));   
s2=full(s2*spdiags(1./sqrt(sum(s2.*s2))',0,ms2,ms2)); 

 
if strcmp(type,'max') == 1 || strcmp(type,'maxabs') == 1
  cc=zeros(max(ms1,ms2),1);
  if ms1==ms2
     lag=zeros(1,ms1);
     for ii=1:ms1
       temp=correlation(s1(:,ii),s2(:,ii));
       if strcmp(type,'maxabs') == 1; temp=abs(temp); end
       cc(ii)=max(temp);
       temp1=find(temp==cc(ii));
       lag(ii)=temp1(1)-ns2;
       if length(temp1) > 1; fprintf('In cornor: %d maxima found\n',length(temp1))
       end
     end

   elseif ms1 == 1
     lag=zeros(1,ms2);
     for ii=1:ms2
        temp=correlation(s1,s2(:,ii));
        if strcmp(type,'maxabs') == 1; temp=abs(temp); end
        cc(ii)=max(temp);
        temp1=find(temp==cc(ii));
        lag(ii)=temp1(1)-ns2;
        if length(temp1) > 1; fprintf('In cornor: %d maxima found\n',length(temp1))
        end
     end

   elseif ms2 == 1
      lag=zeros(1,ms1);
      for ii=1:ms1
        temp=correlation(s1(:,ii),s2);
        if strcmp(type,'maxabs') == 1; temp=abs(temp); end
        cc(ii)=max(temp);
        temp1=find(temp==cc(ii));
        lag(ii)=temp1(1)-ns2;
        if length(temp1) > 1; fprintf('In cornor: %d maxima found\n',length(temp1))
        end
      end
   else
      fprintf(['Error in cornor: input data have incompatible number of ',...
         'columns (%d, %d)\n'],ms1,ms2)
      return
   end

elseif strcmp(type,'all') == 1
  cc=zeros(ns1+ns2-1,max(ms1,ms2));
  if ms1==ms2
     for ii=1:ms1
       cc(:,ii)=correlation(s1(:,ii),s2(:,ii));
     end
  else
    if ms1 == 1
      for ii=1:ms2
        cc(:,ii)=correlation(s1,s2(:,ii));
      end
    elseif ms2 == 1
      for ii=1:ms1
        cc(:,ii)=correlation(s1(:,ii),s2);
      end
    else
      fprintf(['Error in cornor: input data have incompatible number of ',...
         'columns (5d, %d)\n'],ms1,ms2)
      return
    end
  end
  lag=[];

elseif strcmp(type,'absmax') == 1
  cc=zeros(max(ms1,ms2),1);
  if ms1==ms2
     lag=zeros(1,ms1);
     for ii=1:ms1
       temp=correlation(s1(:,ii),s2(:,ii));
       ma=max(temp); mi=min(temp);
       if abs(ma) > abs(mi); 
          cc(ii)=ma; 
       else
          cc(ii)=mi;
       end
       temp1=find(temp==cc(ii));
       lag(ii)=temp1(1)-ns2;
     end

  else
    if ms1 == 1
      lag=zeros(1,ms2);
      for ii=1:ms2
        temp=correlation(s1,s2(:,ii));
        ma=max(temp); mi=min(temp);
        if abs(ma) > abs(mi); 
           cc(ii)=ma; 
        else 
           cc(ii)=mi; 
        end
        temp1=find(temp==cc(ii));
        lag(ii)=temp1(1)-ns2;
      end

    elseif ms2 == 1
      lag=zeros(1,ms1);
      for ii=1:ms1
        temp=correlation(s1(:,ii),s2);
        ma=max(temp); mi=min(temp);
        if abs(ma) > abs(mi);
           cc(ii)=ma; 
        else
           cc(ii)=mi; 
        end
        temp1=find(temp==cc(ii));
        lag(ii)=temp1(1)-ns2;
      end
    else
      fprintf(['Error in cornor: input data have incompatible number of ',...
         'columns (5d, %d)\n'],ms1,ms2)
      return
    end
  end
 
else
  disp(['Error in CORNOR: wrong value of type (',type,')'])
end
