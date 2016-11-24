function data=myhilbert(data)
% Compute Hilbert transform of input data, If "data" is a matrix, "myhilbert"
% operates on each column 
% Written by: E. Rietsch: December 13, 2003
% Last updated: January 24, 2005: Change polarity of imaginary pa
%
%         data=myhilbert(data)
% INPUT
% data    vector or matrix; each column represents a signal
% OUTPUT
% hdata   matrix; each column represents the Hilbert transform (complex) 
%         of the corresponding column of the input matrix "data"
%         same dimension as input data "data"

if ~isreal(data)
   error(' Input data must be real')
end

[data,ndim]=shiftdim(data);  % Make sure that first dimension is not singleton
nsamp=size(data,1);

if nsamp == 1  
   return
end

data=fft(data);
if mod(nsamp,2) == 0        % Nyquist frequency present
   ntr2=nsamp/2+1;
else
   ntr2=(nsamp+1)/2;
end 
%data(1,:)=-data(1,:);
data(1,:)=0;
data(2:ntr2,:)=-2*(data(2:ntr2,:));
data(ntr2+1:end,:)=0;
% hdata=-conj(ifft(data));
data=-ifft(data);
data=shiftdim(data,-ndim); % Undo dimension change, if there was one
