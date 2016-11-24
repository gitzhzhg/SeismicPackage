function ds=moperation(i1,i2,operator)
% Function multiplies a constant or matrix elementwise with traces 
% of a seismic dataset
% 
% Written by: E. Rietsch: December 7, 2006
% Last updated:
%
%       ds=moperation(i1,i2,operator)
% INPUT
% i1    first operand 
% i2    second operand
%       At least one of the two operands is a dataset.
% operation  string defining operation to perform. Possible values are:
%      '+', '-', '*', '/', '^'
% OUTPUT
% ds    result of the operation

try
   if isstruct(i1)
      type=i1.type;
   else
      type=i2.type;
   end
catch
   error(['Operation "',operator,'" is not defined for these two objects'])
end


switch type
case 'seismic'
   ds=moperation4seismic(i1,i2,operator);

case 'pdf'
   ds=moperation4pdf(i1,i2,operator);

otherwise
   error(['Operation "',operator,'" is not defined for these two arguments'])

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=moperation4pdf(i1,i2,operator)
% Perform the operation defined by input argument "operator" to the first two 
% input arguments
%
% Written by: E. Rietsch: July 12, 2006
% Last updated: September 16, 2006: Add handling of PDF's
%
%       ds=moperation4pdf(i1,i2,operator)
% INPUT
% i1    first operand 
% i2    second operand
%       At least one of the two operands is a PDF.
% operation  string defining operation to perform. Possible values are:
%      '+', '-', '*', '/', '^'
% OUTPUT
% ds    result of the operation

if istype(i1,'pdf')  % First input argument is a PDF
   if isnumeric(i2)                      % and second input dataset is numeric
      ds=i1;
      if numel(i2) == 1

         switch operator

	 case '+'
            ds.pdf=i1.pdf+i2;

	 case '-'
            ds.pdf=i1.pdf-i2;
	 
	 case '*'
            ds.pdf=i1.pdf*i2;
	 
	 case '/'
            ds.pdf=i1.pdf/i2;
	 
	 case '^'
            ds.pdf=i1.pdf.^i2;
	 
	 otherwise
            disp([' Unknown operator "',operator,'".'])
            error(' Abnormal termination.')
	 
         end

      else
	 disp([' The operator "',operator, ...
	       '" is not defined for these arguments (PDF and vector or matrix).'])
         error(' Abnormal termination.')
	 
      end
     
         
   elseif istype(i2,'pdf') % Both input datasets are PDF's
      
      ds=pd_operation(i1,i2,operator);

   else
      disp([' The operator "',operator,'" is not defined for these arguments.'])
      error(' Abnormal termination.')
	 
   end


elseif  istype(i2,'pdf')  &&  ...      % Second input dataset is a PDF
        isnumeric(i1)                  % and first input dataset is numeric
   ds=i2;
   if numel(i1) == 1
       switch operator

       case '+'
          ds.pdf=i1+ds.pdf;

       case '-'
          ds.pdf=i1-ds.pdf;
	 
       case '*'
          ds.pdf=i1*ds.pdf;
	 
       case '/'
          ds.pdf=i1./ds.pdf;
	 
       case '^'
          ds.pdf=i1.^ds.pdf;
	 
       otherwise
	  disp([' Unknown operator "',operator,'".'])
          error(' Abnormal termination.')

       end
    
   else
      error(['Operator "',operator, ...
      '" is not defined for these argument (vector or matrix and PDF).'])
   end

else
   error(['The operator "',operator,'" is not defined for these arguments.'])
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=moperation4seismic(i1,i2,operator)
% Perform the operation defined by input argument "operator" to the first two 
% input arguments
%
% Written by: E. Rietsch: December 7, 2006
% Last updated:
%
%       ds=moperation4seismic(i1,i2,operator)
% INPUT
% i1    first operand 
% i2    second operand
%       At least one of the two operands is a seismic dataset.
% operation  string defining operation to perform. Possible values are:
%      '+', '-', '*', '/', '^'
% OUTPUT
% ds    result of the operation

switch operator

case '+'
   ds=plus_no1(i1,i2);

case '-'
   ds=minus_no2(i1,i2);

case '*'
   ds=times_no3(i1,i2);

case '/'
   ds=mrdivide_no4(i1,i2);

case '^'
   ds=power_no5(i1,i2);

otherwise
        disp([' Unknown operator "',operator,'".'])
        error(' Abnormal termination.')

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=plus_no1(i1,i2)
% Function adds a constant or matrix to traces of seismic dataset 
% Written by: E. Rietsch: September 11, 2005
% Last updated:

if isstruct(i1)  &&  strcmp(i1.type,'seismic')  &&  isnumeric(i2)
   ds=i1;
   sz=size(i2);
   [nsamp,ntr]=size(i1.traces);

   if prod(sz) == 1  ||  all(sz == [nsamp,ntr])
      ds.traces=ds.traces+i2;

   else     
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=ds.traces(ii,:)+i2;
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=ds.traces(:,ii)+i2;
         end
      
      else
         error('Operator "+" is not defined for this size of summand.')
      
      end
   end   
         

elseif  isstruct(i2)  &&  strcmp(i2.type,'seismic')  &&  isnumeric(i1)
   ds=i2;
   sz=size(i1);
   [nsamp,ntr]=size(i2.traces);

   if prod(sz) == 1   ||  all(sz == [nsamp,ntr])
      ds.traces=ds.traces+i1;

   else
      
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=ds.traces(ii,:)+i1;
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=ds.traces(:,ii)+i1;
         end
            
      else
         error('Operator "+" is not defined for this size of summand.')
      
      end
   end   
         

else
   error('Operator "+" is not defined for these arguments.')
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=minus_no2(i1,i2)
% Function adds a constant or matrix to traces of seismic dataset 
% Written by: E. Rietsch: September 11, 2005
% Last updated:

if isstruct(i1)  &&  strcmp(i1.type,'seismic')  &&  isnumeric(i2)
   ds=i1;
   sz=size(i2);
   [nsamp,ntr]=size(i1.traces);
   if prod(sz) == 1  ||  all(sz == [nsamp,ntr])
      ds.traces=ds.traces-i2;

   else
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=ds.traces(ii,:)-i2;
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=ds.traces(:,ii)-i2;
         end
      
      else
         error('Operator "-" is not defined for this size of subtrahend.')
      
      end
   end   
         

elseif  isstruct(i2)  &&  strcmp(i2.type,'seismic')  &&  isnumeric(i1)
   ds=i2;
   sz=size(i1);
   [nsamp,ntr]=size(i2.traces);
   if prod(sz) == 1   ||  all(sz == [nsamp,ntr])
      ds.traces=i1-ds.traces;

   else
      
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=i1-ds.traces(ii,:);
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=i1-ds.traces(:,ii);
         end

      else
         error('Operator "-" is not defined for this size of subtrahend.')
      
      end
   end   
         

else
   error('Operator "-" is not defined for these arguments.')
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=times_no3(i1,i2)
% Function multiplies a constant or matrix elementwise with traces 
% of a seismic dataset
%
% Written by: E. Rietsch: September 11, 2005
% Last updated:

if isstruct(i1)  &&  strcmp(i1.type,'seismic')  &&  isnumeric(i2)
   ds=i1;
   sz=size(i2);
   [nsamp,ntr]=size(i1.traces);

   if numel(i2) == 1
      ds.traces=ds.traces.*i2;

   else
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=ds.traces(ii,:).*i2;
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=ds.traces(:,ii).*i2;
         end
      
      elseif all(sz == [nsamp,ntr])
         ds.traces=ds.traces.*i2;
      
      else
         error('The operator "*" is not defined for this size of matrix.')
      
      end
   end   
         
elseif  isstruct(i2)  &&  strcmp(i2.type,'seismic')  &&  isnumeric(i1)
   ds=i2;
   sz=size(i1);
   [nsamp,ntr]=size(i2.traces);
   if prod(sz) == 1   ||  all(sz == [nsamp,ntr])
      ds.traces=ds.traces.*i1;

   else
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=ds.traces(ii,:).*i1;
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=ds.traces(:,ii).*i1;
         end
      
      elseif all(sz == [nsamp,ntr])
         ds.traces=ds.traces.*i1;
      
      else
         error('Operator "*" is not defined for this size of matrix.')
      
      end
   end   
         

else
   error('The operator "*" is not defined for these two arguments.')
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=mrdivide_no4(i1,i2)
% Function multiplies a constant or matrix elementwise with traces 
% of a seismic dataset
%
% Written by: E. Rietsch: September 11, 2005
% Last updated:

if isstruct(i1)  &&  strcmp(i1.type,'seismic')  &&  isnumeric(i2)
   ds=i1;
   sz=size(i2);
   [nsamp,ntr]=size(i1.traces);

   if numel(i2) == 1
      ds.traces=ds.traces./i2;

   else
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=ds.traces(ii,:)./i2;
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=ds.traces(:,ii)./i2;
         end
      
      elseif all(sz == [nsamp,ntr])
         ds.traces=ds.traces./i2;
      
      else
         error('The operator "/" is not defined for this size of matrix.')
      
      end
   end   
         
elseif  isstruct(i2)  &&  strcmp(i2.type,'seismic')  &&  isnumeric(i1)
   ds=i2;
   sz=size(i1);
   [nsamp,ntr]=size(i2.traces);
   if prod(sz) == 1   ||  all(sz == [nsamp,ntr])
      ds.traces=i1./ds.traces;

   else
      if all(sz == [1,ntr])
         for ii=1:nsamp
            ds.traces(ii,:)=i1./ds.traces(ii,:);
         end
      
      elseif all(sz == [nsamp,1])
         for ii=1:ntr
            ds.traces(:,ii)=i1./ds.traces(:,ii);
         end
      
      elseif all(sz == [nsamp,ntr])
         ds.traces=i1./ds.traces;
      
      else
         error('Operator "*" is not defined for this size of matrix.')
      
      end
   end   
         
else
   error('The operator "*" is not defined for these two arguments.')
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

function ds=power_no5(i1,i2)
% Function takes the power of the traces of a seismic dataset
%
% Written by: E. Rietsch: September 11, 2005
% Last updated:

if isstruct(i1)  &&  isnumeric(i2)  && numel(i2) == 1
   ds=i1;
   ds.traces=ds.traces.^i2;
elseif isstruct(i2)  &&  isnumeric(i1) && numel(i1) == 1
   ds=i2;
   ds.traces=i1.^ds.traces;
else
   error('Operator ".^" is not defined for these arguments.')
end
