function structout=combine_parameters(structin1,structin2,structout)
% Combine parameters from "structin1" and "structin2" and add them to
% "structout".
%
% Written by: E. Rietsch: September 12, 2003
% Last updated: July 22, 2007: "mlint" compliant.
%
%        structout=combine_parameters(structin1,structin2,structout)
% INPUT
% structin1  structure which might have a field "parameter_info" (examples are 
%            "seismic","well_log", etc.
% structin2  structure which might have a field "parameter_info"
% structout  structure to which the combined parameters of "structin1" and 
%            "structin2" will be added.
% OUTPUT
% structout

if isfield(structin1,'parameter_info') && isfield(structin2,'parameter_info')
   par1=structin1.parameter_info(:,1);
   par2=structin2.parameter_info(:,1);
   [dummy,idx1,idx2]=intersect(par1,par2);

   if isempty(dummy)    % No commmon parameters
      structout.parameter_info=[structin1.parameter_info;structin2.parameter_info];
      for ii=1:length(par1)
%         structout=setfield(structout,par1{ii},getfield(structin1,par1{ii}));
	 structout.(par1{ii})=structin1.(par1{ii});
      end
      for ii=1:length(par2)
%         structout=setfield(structout,par2{ii},getfield(structin2,par2{ii}));
	 structout.(par2{ii})=structin2.(par2{ii});
      end 

   else                 % Common parameters

%       Add the parameters that the two structures do not have in common
      idx1a=find(~ismember(par1,dummy));
      idx2a=find(~ismember(par2,dummy));
      if ~isempty(idx1) && ~isempty(idx2a)
         structout.parameter_info=[structin1.parameter_info(idx1a,:); ...
                                   structin2.parameter_info(idx2a,:)];

      elseif ~isempty(idx1a)
         structout.parameter_info=structin1.parameter_info(idx1a,:);
      
      elseif ~isempty(idx2a)
         structout.parameter_info=structin2.parameter_info(idx2a,:);
      
      else
         structout.parameter_info=[];
      end
      for ii=1:length(idx1a)
%         structout=setfield(structout,structin1.parameter_info{idx1a(ii)}, ...
%                   getfield(structin1,structin1.parameter_info{idx1a(ii)}));
         structout.(structin1.parameter_info{idx1a(ii)}) = ...
	            structin1.parameter_info{idx1a(ii)};
      end
      for ii=1:length(idx2a)
%         structout=setfield(structout,structin2.parameter_info{idx2a(ii)}, ...
%                   getfield(structin1,structin2.parameter_info{idx2a(ii)}));
         structout.(structin2.parameter_info{idx2a(ii)}) = ...
	            structin2.parameter_info{idx2a(ii)};
      end
%       Check if the common parameters are the same
      for ii=1:length(idx1)
         par=par1{idx1(ii)};
%         val1=getfield(structin1,par);
%         val2=getfield(structin2,par);
	 val1=structin1.(par);
         val2=structin2.(par);
         if all(size(val1) == size(val2))    % Both parameters have the same dimension
            if all(val1 == val2)             % Both parameters have the same value
%               structout=setfield(structout,par,val1);
	       structout.(par)=val1;
               structout.parameter_info=[structout.parameter_info;
                                         structin1.parameter_info(idx1(ii),:)];
            else
%               structout=setfield(structout,[par,'_1'],val1);
%               structout=setfield(structout,[par,'_2'],val2);
	       structout.([par,'_1'])=val1;
               structout.([par,'_2'])=val2;
               structout.parameter_info=[structout.parameter_info;
                                         structin1.parameter_info(idx1(ii),:)];        
               structout.parameter_info{end,1}=[par,'_1'];
               structout.parameter_info=[structout.parameter_info;
                                         structin1.parameter_info(idx2(ii),:)];                  
               structout.parameter_info{end,1}=[par,'_2'];                       
            end
         else
%            structout=setfield(structout,[par,'_1'],val1);
%            structout=setfield(structout,[par,'_2'],val2);
	    structout.([par,'_1'])=val1;
            structout.([par,'_2'])=val2;

            structout.parameter_info=[structout.parameter_info;
                                      structin1.parameter_info(idx1(ii),:)];           
            structout.parameter_info{end,1}=[par,'_1'];
            structout.parameter_info=[structout.parameter_info;
                                      structin1.parameter_info(idx2(ii),:)];                     
            structout.parameter_info{end,1}=[par,'_2'];
         end
      end
   end

elseif isfield(structin1,'parameter_info')
   structout=copy_parameters(structin1,structout);

elseif isfield(structin2,'parameter_info')
   structout=copy_parameters(structin2,structout);
end

