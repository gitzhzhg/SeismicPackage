function corr=xcorr_corresp(a,b,up,down,normalize)
% Function computes the crosscorrelation of corresponding columns of matrices "a" and "b".
% The function does no input error checking!
%           corr=xcorr_corresp(a,b,up,down,normalize)
% INPUT
% a     matrix
% b     matrix ; both matrices must have the same 
% up    number of samples "b" must be shifted up with regard to "a" for the
%       computation of the first lag of the crosscorrelation
% down  number of samples "b" must be shifted down with regard to "a" for the
%       last sample of the crosscorrelation
% normalize  Type of normalization to apply prior to computing the crosscorrelation
%       Possible values are:    'no'    ==> no normalization
%                               'traces' ==> columns of "a" and "b" are normalized to have
%                                   L2 norm 1
%                               'segment" ==>
%                     in this case the condition "up >= 0 && down <= abs(na-nb)" must be satisfied
%                     where "na" and "nb" denote the number of rows of "a" and "b".
% OUTPUT
% corr  correlation coefficients; "corr" has dimension (down-up) x ma where ma denotes the number
%       of columns of "a"

[na,ma]=size(a);
[nb,mb]=size(b);
if ma ~= mb
   error(' Input matrices must have the same number of columns')
end

if strcmpi(normalize,'segment') && (up < 0 || down > abs(na-nb))
   disp([char(13),' For segment normalization the correlation shifts (lags) must'])
   disp(' not move one trace beyond the beginning or the end of the other')
   error(' Abnormal termination')
end

% endmm=ma^2;

corr=zeros(down-up+1,ma);

if strcmpi(normalize,'traces')
   for ii=1:ma
      a(:,ii)=a(:,ii)/(norm(a(:,ii))+eps);
   end
      
   for ii=1:mb
      b(:,ii)=b(:,ii)/(norm(b(:,ii))+eps);
   end
end

counter=0;

if na >= nb
   if strcmpi(normalize,'segment')
      for ii=1:mb
         b(:,ii)=b(:,ii)/(norm(b(:,ii))+eps);
      end
      
      for ii=up:down
         counter=counter+1;
         if nb+ii < 1
            % Do nothing
         elseif ii < 0
            temp=a(1:nb+ii,:);
            for jj=1:ma
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
               corr(counter,jj)=temp(:,jj)'*b(1-ii:nb,jj);
            end
            %corr(counter,:)=reshape(temp'*b(1-ii:nb,:),1,mm);
         elseif ii <= na-nb
            temp=a(1+ii:nb+ii,:);
            for jj=1:ma
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
               corr(counter,jj)=temp(:,jj)'*b(:,jj);
            end
            
         elseif ii < na
            temp=a(1+ii:na,:);
            for jj=1:ma
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
               corr(counter,jj)=temp(:,jj)'*b(1:na-ii,jj);
            end
         end
      end
      
   else  
      for ii=up:down
         counter=counter+1;
         if nb+ii < 1
            % Do nothing
         elseif ii < 0
           for jj=1:ma
              corr(counter,jj)=a(1:nb+ii,jj)'*b(1-ii:nb,jj);
           end
         elseif ii <= na-nb
            for jj=1:ma
               corr(counter,jj)=a(1+ii:nb+ii,jj)'*b(:,jj);
            end
         elseif ii < na
            for jj=1:ma
               corr(counter,jj)=a(1+ii:na,jj)'*b(1:na-ii,jj);
            end
         end
      end
   end
   
else                        % nb > na
   if strcmpi(normalize,'segment')
      for ii=1:mb
         a(:,ii)=a(:,ii)/(norm(a(:,ii))+eps);
      end
    
      for ii=up:down
         counter=counter+1;
         if nb+ii < 1
           % Do nothing
         elseif nb+ii <= na
            temp=b(1-ii:nb,:);
            for jj=1:mb
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
               corr(counter,jj)=a(1:nb+ii,jj)'*temp(:,jj);
            end
            
         elseif ii > na-nb && ii <= 0
            temp=b(1-ii:na-ii,:);
            for jj=1:mb
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
               corr(counter,jj)=a(:,jj)'*temp(:,jj);
            end
            
         elseif ii < na
            temp=b(1:na-ii,:);
            for jj=1:mb
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
               corr(counter,jj)=a(1+ii:na,jj)*temp(:,jj);
            end
         end
      end
     
   else  
      for ii=up:down
        counter=counter+1;
        if nb+ii < 1
             % Do nothing
        elseif nb+ii <= na
           for jj=1:ma
              corr(counter,jj)=a(1:nb+ii,jj)'*b(1-ii:nb,jj);
           end
        elseif ii > na-nb && ii <= 0
           for jj=1:ma
              corr(counter,jj)=a(:,jj)'*b(1-ii:na-ii,jj);
           end
        elseif ii < na
           for jj=1:ma
              corr(counter,jj)=a(1+ii:na,jj)'*b(1:na-ii,jj);
           end
        end
      end
   end
end
