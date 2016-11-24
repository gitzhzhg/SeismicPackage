function sout=predlowf(s,f,flower,fupper,n)
% sout=predlowf(s,f,flower,fupper,n)
%
% PREDLOWF uses Burg prediction filtering to predict the low portion of
% a frequency spectrum.
% Algorithm:
%	- Copy the input spectrum into the output spectrum and zero
%	all values below flower
%	- Loop over zero'd frequencies starting with the highest
%	- Design a 1 lag Burg prediction filter to predict the
%	first zero'd frequency. Use only frequencies between the
%	first non-zero'd one and fupper in the design of the filter.
%	- predict the highest zero'd spectral component.
%	- use the predictions of previous iterations in the current
%	prediction
%
% s ... input complex spectrum
% f ... frequency coordinate vector for s
% flower ... lowest frequency to keep
% fupper ... higest frequency to use in the prediction filter design
% n ... number of points in the prediction operator
% sout ... returned spectrum with low-end replaced with predicted
%	samples.
%
% G.F. Margrave May 1995
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
% check for row vectors and transpose if needed
aa=size(s);
bb=size(f);
if aa(1)==1
	s=s';
end
if bb(1)==1
	f=f';
end
ind=find(f<flower);
ind=flipud(ind); %start with the higher f's
iup=find(f<=fupper);
iup=max(iup);
sout=s;
%zero the frequencies to be predicted
sout(ind)=zeros(size(ind));
%handle real and imaginary part separately
tempr=real(sout);
tempi=imag(sout);
for k=1:length(ind)
	%design filter
	pfilt=(burgpr(tempr(ind(k):iup).',n)).';
	%convert from prediction error to prediction filter
	pfilt(1)=pfilt(1)+1;
	
	%predict tempr(ind(k))
	p= sum(tempr(ind(k):ind(k)+n-1).*pfilt);
	tempr(ind(k))=p;
end
for k=1:length(ind)
	%design filter
	pfilt=(burgpr(tempi(ind(k):iup).',n)).';
	%convert from prediction error to prediction filter
	pfilt(1)=pfilt(1)+1;
	
	%predict tempi(ind(k))
	p= sum(tempi(ind(k):ind(k)+n-1).*pfilt);
	tempi(ind(k))=p;
end
sout=tempr+i*tempi;