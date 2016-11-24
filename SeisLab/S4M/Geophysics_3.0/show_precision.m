function show_precision(ds)
% Function checks numeric fields of specific structures (seismic, well log, 
% table, pseudo-wells) to see if they have all the same precision.
%
% Written by: E. Rietsch: December 31, 2006
% Last updated: January 7, 2007: Generalize for dataset vectors
% 
%      ds=show_precision(ds)
% INPUT
% ds   dataset of type "seismic', "well_log", "table",' or "pseudo-well"
%
% EXAMPLE
%    show_precision(s_data)

if nargin ~= 1
   error(' One input argument is required.')
end

if strcmp(ds(1).type,'seismic')	 ||  strcmp(ds(1).type,'well_log') ...
                                 ||  strcmp(ds(1).type,'table') ...
                                 ||  strcmp(ds(1).type,'pseudo-wells') 
   lds=length(ds);
   for jj=1:lds
      fnames=fieldnames(ds(jj));
      lfnames=length(fnames);
      bool1=false(lfnames,1);
      bool2=false(lfnames,1);
      for ii=1:lfnames
         if isa(ds(jj).(fnames{ii}),'double')
            bool2(ii)=true;
         elseif isa(ds(jj).(fnames{ii}),'single')
            bool1(ii)=true;
         end
      end
      if lds > 1
         disp([' Dataset # ',num2str(jj),':'])
      end
      if any(bool1)
         if any(bool2)   % Mixed precision
            disp(' Single precision:')
       	    disp(['   ',cell2str(fnames(bool1),', ')])
            disp(' Double precision:')
	    disp(['   ',cell2str(fnames(bool2),', ')])
         else
            disp(' Single precision')
         end
      else
         disp(' Double precision')
      end
   end 
else
   error('Function "same_precision" is not defined for this input argument.')
end
