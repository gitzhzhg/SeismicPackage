function [var,ier,handle]=check_numeric(str,var_name,varargin)
% Check if string represents numeric variables and satisfies specified constraints
%
% Written by: E. Rietsch: August 21, 2004
% Last updated: December 4, 2005: Bug fix in error message
%
%           [var,ier,handle]=check_numeric(str,var_name,varargin)
% INPUT
% str       string of numbers
% var_name  name of the variable (for error messages)
% varargin  one or more cell arrays; the first element of each cell array is
%           a keyword, the other elements are parameters. Presently, keywords are:
%           'minmax'  minimum and maximum number of entries
%                  Default: not checked
%           'bounds'  lower and upper bound of entries; not checked if bounds(1)=[]
%                  Default: not checked
%           'monotonic' monotonicity of numbers (if more than one). Possible 
%                  values are: 
%                  '<'    strictly monotonicly increasing
%                  '<='   monotonicly increasing
%                  '>='   monotonicly decreasing      
%                  '>'    strictly monotonicly decreasing 
%                  []     not checked (default)
%           'modulo'  two numbers "m1", and "m2": input must satify the condition 
%                     mod(input,m2)+m2 == 0
%                     not checked if modulo == []
%                     numbers are integers if {'modulo',1,0}
%                                     even if {'modulo',2,0}
%                                      odd if {'modulo',2,1}
% OUTPUT
% var       scalar or numeric vector 
% ier       Logical vector with 5 entries
%           ier(1) == true    string is not numeric
%           ier(2) == true    number of input parameters is not within limits
%           ier(3) == true    input parameters are not within bounds
%           ier(4) == true    input parameters do not satisfy monotonicity requirements
%           ier(5) == true    input parameters do not satisfy the "modulo" requirement
% handle    handle of message window (so it can be deleted)
%
% EXAMPLES
%         [var,ier]=check_numeric('22','test_data',{'minmax',1 3})
%         [var,ier]=check_numeric('22 23 24 35','test_data',{'minmax',1 3})
%         [var,ier]=check_numeric('22 23','test_data',{'minmax',1 3},{'bounds',0 30}, ...
%                   {'monotonic','>'},{'modulo',2,0})
%         [var,ier]=check_numeric('22 23','frequencies',{'minmax',1 3},{'bounds',0 30}, ...
%                   {'modulo',2,1})


global S4M

%keywords={'number')
handle=[];
ier=false(1,5);

var=str2num(str);   %#ok String with numbers to be checked

if isempty(var)
   msgdlg([var_name,' is not numeric.'])
   ier(1)=true;
   return
end

ik=0;
lvar=length(var);
message=cell(10,1);

for ii=1:length(varargin)
   if ier(2)
      break
   end
   temp=varargin{ii};

                if ~isempty(temp{2})

   switch temp{1}

        % Check if the number of entries is correct
   case 'minmax' 
      if length(temp) == 3
         minno=temp{2};
         maxno=temp{3};
      else
         try
           minno=temp{2}(1);
           maxno=temp{2}(2);
         catch
           minno=temp{2};
           maxno=minno;
         end
      end
      if lvar < minno  ||  lvar > maxno
         ier(2)=true;
         if minno < maxno
            ik=ik+1;
            message{ik}=['"',var_name,'" must be a vector with ',num2str(minno), ...
                    ' to ',num2str(maxno),' entries.'];

            handle=msgdlg(['"',var_name,'" must be a vector with ',num2str(minno), ...
                    ' to ',num2str(maxno),' entries.']);
            add_handle2delete(handle)
         elseif temp{2} > 1
            ik=ik+1;
            message{ik}=['"',var_name,'" must be a vector with ',num2str(minno),' entries.'];
%            handle=msgdlg(['"',var_name,'" must be a vector with ',num2str(minno),' entries.']);
%           add_handle2delete(handle)
         else
            ik=ik+1;
            message{ik}=['"',var_name,'" must be a scaler.'];
%           handle=msgdlg(['"',var_name,'" must be a scaler.']);
%           add_handle2delete(handle)
         end
      end

%       Check if the entries are within bounds
   case 'bounds'
      if ~isempty(temp(2))
         if length(temp) == 3
            minno=temp{2};
            maxno=temp{3};
         else
            minno=temp{2}(1);
            maxno=temp{2}(2);
         end
         minvar=min(var(:));
         maxvar=max(var(:));
         if minvar < minno  ||  maxvar > maxno
            ier(3)=true;
            ik=ik+1;
            message{ik}=['Entries of "',var_name,'" must be a between ',num2str(minno), ...
                    ' and ',num2str(maxno)];
%            handle=msgdlg(['"',var_name,'" must be between ',num2str(minno), ...
%                   ' and ',num2str(maxno)]);
%            add_handle2delete(handle)
         end
      end

%       Check if entries are monotonic
   case 'monotonic'
      mtype=temp{2};
      if lvar > 1  &&  ~isempty(mtype)
         test=diff(var(:));
         if strcmp(mtype,'<')
            if any(test <= 0)
               ik=ik+1;
               message{ik}=['Entries of "',var_name,'" must be strictly increasing.'];
%              handle=msgdlg(['Entries of vector "',var_name,'" must be strictly increasing.']);
%              add_handle2delete(handle)               
               ier(4)=true;
            end
         elseif strcmp(mtype,'<=')
            if any(test < 0)
               ik=ik+1;
               message{ik}=['Entries of "',var_name,'" must be non-decreasing.'];
%              handle=msgdlg(['Entries of vector "',var_name,'" must be non-decreasing.']);
%              add_handle2delete(handle)
               ier(4)=true;
            end
         elseif strcmp(mtype,'>=')
            if any(test > 0)
               ik=ik+1;
               message{ik}=['Entries of "',var_name,'" must be non-increasing.'];
%              handle=msgdlg(['Entries of vector "',var_name,'" must be non-increasing.']);
%              add_handle2delete(handle)
               ier(4)=true;
            end
         elseif strcmp(mtype,'>')
            if any(test >= 0)
               ik=ik+1;
               message{ik}=['Entries of "',var_name,'" must be strictly decreasing.'];
%              handle=msgdlg(['Entries of vector "',var_name,'" must be strictly decreasing.']);
%              add_handle2delete(handle)
               ier(4)=true;
            end
         else
            handle=msgdlg(['Unknown option: ',mtype]);
            add_handle2delete(handle)
         end
      end
 
   case 'modulo'
      if length(temp) == 3
         modul=temp{2};
         remain=temp{3};
      else
         modul=temp{2}(1);
         remain=temp{2}(2);
      end
      bool=(mod(var,modul) ~= remain);
      if any(bool)
%         var=round(var/modul)*modul+remain;
         ier(5)=true;
         if modul == 1   &&   remain == 0
            ik=ik+1;
            message{ik}=['Entries of "',var_name,'" must be integer.'];
%           handle=msgdlg(['Entries of "',var_name,'" must be integer.']);
%           add_handle2delete(handle)
         elseif modul == 2   &&   remain == 0
            ik=ik+1;
            message{ik}=['Entries of "',var_name,'" must be even.'];
%            handle=msgdlg(['Entries of "',var_name,'" must be even.']);
%           add_handle2delete(handle)
         elseif modul == 2   &&   remain == 1
            ik=ik+1;
            message{ik}=['Entries of "',var_name,'" must be odd.'];
%            handle=msgdlg(['Entries of "',var_name,'" must be odd.']);
%           add_handle2delete(handle)
         elseif remain  == 0
            ik=ik+1;
           message{ik}=['Entries of "',var_name,'" must be an integer multiples of ' ...
                      ,num2str(modul),'.'];
%            handle=msgdlg(['Entries of "',var_name,'" must be integer multiples of ' ...
%                       ,num2str(modul),'.']);
%            add_handle2delete(handle)
         else
            ik=ik+1;
            message{ik}=['Entries of "',var_name,'" must be integer multiples of ' ...
                     ,num2str(modul),' incemented by ',num2str(remain),'.'];
%           handle=msgdlg(['Entries of "',var_name,'" must be integer multiples of ' ...
%                    ,num2str(modul),' incemented by ',num2str(remain),'.']);
%           add_handle2delete(handle)
         end
      end

   otherwise
      warndlg(['Unrecognized constraint "',temp{1},'"'])
   
   end

                end  
 
end  
   
if ik > 0
   if S4M.batch
      alert(message)
   else
      handle=warndlg(message(1:ik));
      add_handle2delete(handle);
   end
end
