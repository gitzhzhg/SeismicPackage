function ier=l_check(wlog)
% Function checks if log structure set is consistent and complies with
% specifications
%
% Written by: E. Rietsch: August 26, 2001
% Last updated: August 22, 2008: Fix errors found in fields 'first" and "last"
%
%         ier=l_check(wlog)
% INPUT
% wlog    log structure
% OUTPUT
% ier     error indicator; true if there is an error and false if not

% UPDATE HISTORY
%        January 1, 2006: Check field "units"


global S4M

param.verbose=false;

verbose=param.verbose;

ier=false;

if nargin ~= 1
   disp(' One argument (log structure) required.')
   ier=true;
   return
end

if ~istype(wlog,'well_log')
   dispdlg('Input to "l_check" is not a well log.')
   ier=true;
   return
end

%       Establish name of input structure for reference later

dataset=['"',wlog.name,'"'];

if ~isstruct(wlog)
   dispdlg([' ',dataset,' is not a structure.'])
   return
end 

%       Check if required fields are present
fields=fieldnames(wlog);
required={'first','last','step','units','curves','curve_info'};
index=find(~ismember(required,fields));
if ~isempty(index)
   dispdlg([' ',dataset,' does not have required field(s): ',cell2str(required(index))])
   return
else
   if verbose
      dispdlg([' ',dataset,' has the required fields: ',cell2str(required)])
   end
end

%       Check if curves are empty
[nsamp,ncurves]=size(wlog.curves);
if nsamp*ncurves == 0
   dispdlg([' curves of ',dataset,' are empty.'])
   ier=true;
   return
else
   if verbose
      dispdlg([' ',dataset,' has ',num2str(ncurves),' curve(s) with ',num2str(nsamp), ...
                  ' sample(s).'])
   end  
end 

%       Check if start depth, end depth, sample interval and # of samples agree
if wlog.step < 0
   dispdlg(' Sample interval must be non-negative.')
   ier=true;
   return
end
if wlog.first ~= wlog.curves(1,1)
   dispdlg([' Field "first" (',num2str(wlog.first),') differs from first value', ...
      ' of depth curve (',num2str(wlog.curves(1,1)),').'])
   wlog.first=wlog.curves(1,1);
   disp(' Now corrected.')
end
if wlog.last ~= wlog.curves(end,1)
   dispdlg([' Field "last" (',num2str(wlog.last),') differs from last value', ...
      ' of depth curve (',num2str(wlog.curves(end,1)),').'])
   wlog.last=wlog.curves(end,1);
   disp(' Now corrected.')
end

ddd=diff(wlog.curves(:,1));
if any(isnan(ddd))
% if ~isempty(find(isnan(ddd)));
   dispdlg(' The first column of the log curves must not contain NaNs.')
   ier=true;
else
   middd=min(ddd);
   if middd <= 0
      dispdlg(' Depth values must be monotonically increasing.')
      ier=true;
      if wlog.step > 0
         dispdlg(' "wlog.step" must be zero for nonuniform depth values.')
      end
   else
      bool=isconstant(ddd,S4M.log_step_error);
      maddd=max(ddd);
      mddd=0.5*(maddd+middd);
      if wlog.step == 0  &&  bool
         dispdlg([' Depth increment is uniform (',num2str(ddd(1)),'), but "wlog.step" is zero.'])
         ier=true;
      elseif wlog.step > 0  &&  ~bool
         dispdlg([' Depth increment not uniform (',num2str(middd),' - ',num2str(maddd), ...
             '), but "wlog.step" is not zero'])
         ier=true;
      elseif wlog.step > 0  &&  bool
         if abs(wlog.step-ddd(1)) > S4M.log_step_error*wlog.step
            dispdlg([' Depth increment (',num2str(mddd),') and "wlog.step" (', ...
                num2str(wlog.step),') do not agree.'])
            ier=true;
         end
      end
   end
end


%       Check field "curve_info"
[nci,mci]=size(wlog.curve_info);
if ~iscell(wlog.curve_info)
   dispdlg(' Field "curve_info" must be a cell array.')

else
   if nci ~= ncurves
      dispdlg([' The number of log curves (',num2str(ncurves),') is not equal to ', ...
          'the number of rows (',num2str(nci),') of cell array "curve_info".'])
     ier=true;
   end

   if mci ~= 3
      dispdlg(' The cell array of field "curve_info" must have 3 columns.')
   end

   temp=wlog.curve_info(:);
   ix=0;
   for ii=1:3*nci
      if ~ischar(temp{ii})
         ix=1;
      end
   end
   if ix
      dispdlg(' Elements of field "curve_info" must be character strings.')
      ier=true;
   end
end

%	Check parameters
ier=max(param_check(wlog),ier);
if verbose && ~ier
   dispdlg(' No formal errors with parameters.')
end


%       Check for NaNs
idx=find(isnan(wlog.curves));
if isempty(idx) && isnull(wlog)
   mywarning(' Field "null" is not empty, but curves have no null values.')
   ier=true;
elseif ~isempty(idx) && ~isnull(wlog)
   dispdlg(' Field "null" does not exist, but curves do have null values.')
   ier=true;
else
   if verbose
      dispdlg(' No problem with null values.')
   end
end

% 	Check for identical mnemonics
if S4M.case_sensitive
  mnems=unique(wlog.curve_info(:,1));
  if length(mnems) < ncurves
    dispdlg(' Curve mnemonics are not unique (mnemonics are case-sensitive).')
    ier=true;
    for ii=1:length(mnems)
      idx=find(ismember(wlog.curve_info(:,1),mnems{ii}));
      if length(idx) > 1
        dispdlg(['     ',cell2str(wlog.curve_info(idx,1),', ')])
      end
    end
  end
  
else
   mnems=unique(lower(wlog.curve_info(:,1)));
   if length(mnems) < ncurves
      dispdlg(' Curve mnemonics are not unique (mnemonics are not case-sensitive):')
      ier=true;
      for ii=1:length(mnems)
         idx=find(ismember(lower(wlog.curve_info(:,1)),mnems{ii}));
         if length(idx) > 1
            dispdlg(['     ',cell2str(wlog.curve_info(idx,1),', ')])
         end
      end
   end 
end

if ~ier && nargout == 0
   if S4M.deployed
      dispdlg([' No formal errors found in ',dataset,'.'])
   else
      disp([' No formal errors found in ',dataset,'.'])
   end
end 

%	Check field "units"
if ~strcmp(wlog.units,wlog.curve_info{1,2})
   ier=true;
   if S4M.deployed
      dispdlg(['Depth units in field "units" ( ',wlog.units, ...
       ' ) differ from those in field "curve_info" ( ',wlog.curve_info{1,2},' ).'])
   else
      disp(['Depth units in field "units" ( ',wlog.units, ...
       ' ) differ from those in field "curve_info" ( ',wlog.curve_info{1,2},' ).'])
   end
end

if nargout == 0
   clear ier
end

