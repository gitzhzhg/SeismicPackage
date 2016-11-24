function [ca,ia,ib,ic]=approx_intersect(a,b,tol)
% Find the values of numeric vector "a" that are closest to values of 
% numeric vector "b".
% Analog to Matlab function "intersect" for numeric data; does not require 
% identity but only approximate equality.
% It is important to note that the result "c" may change if the input arguments
% "a" and "b" are swapped. This would not be the case for "intersect".
%
% Written by: E. R.: July 7, 2003
% Last updated: January 18, 2007: Add fourth output argument
%
%       [ca,ia,ib,ic]=approx_intersect(a,b,tol)
% INPUT
% a     numeric vector
% b     numeric vector
% tol   tolerance; a constant 
% OUTPUT
% ca    elements of "a" that are close to "b"
% ia    index vector so that ca=a(ia)
% ib    index vector so that |ca-b(ib)| <= tol
% ic    index vector such that for bool=~isnan(ic)
%                            |a(ic(bool))-b(bool)] <= tol
%
% EXAMPLE
%       a=1:5;
%       b=[2.3 2.9 3.1];
%       [ca,ia,ib,ic]=approx_intersect(a,b,0.2)
%       bool=~isnan(ic);
%       a(ic(bool))-b(bool)


% na=length(a);
nb=length(b);
% ntol=length(tol);
ia=NaN(nb,1);
ib=NaN(nb,1);
% ic=NaN(nb,1);
ca=NaN(nb,1);

for ii=1:nb
   [am,idxa]=min(abs(a-b(ii)));
   if am <= tol
      ca(ii)=a(idxa);
      ib(ii)=ii;
      ia(ii)=idxa;
   end
end

bool=~isnan(ca);
ic=ia;
ca=ca(bool);
ia=ia(bool);
ib=ib(bool);
  
