function [tvs,fout,t]=fborga(signal,t,fwidth,finc,padflag)
% FBORGA: forward Borga transform with Gaussian analysis windowing
%
% [tvs,fout]=fborga(signal,t,fwidth,finc,padflag)
% 
% FBORGA performs a forward Borga transform of a seismic trace using
% modified Gaussian analysis windows. The Borga transform is implemented by
% a forward Fourier transform, frequency slicing using modified Gaussian
% windows, and then inverse Fourier transfoming each slice. The output is a
% 2D matrix, called tvs, with the row coordinate being time and the column
% coordinate being the center frequency of each frequency slice. This tvs,
% or time variant spectrum, is also called the Borga spectrum. The Borga
% spectrum is a time-frequency decomposition that is the formal adjoint of
% the Gabor transform (hint: the name Borga is a joke). While the Gabor
% transform is complex valued, the Borga transform is real valued.
% Essentially each column (trace) of the Borga transform is a filter slice
% of the input signal. The Borga spectrum may be inverted s=sum(tvs,2);
% where s will be the reconstructed trace.
%
% signal= input trace 
% t= time coordinate vector for signal.
% fwidth= width (Hertz) of the Gaussian window.
% finc= shift (Hertz) between windows.
% Note: Generally finc < fwidth.
% padflag= if 0, the trace is transformed without padding. If 1,
%   it is padded with zeros to the next power of 2 (unless it already is
%   a power of 2)
% ************** default padflag = 1 ***************
% tvs= output time-variant spectrum (complex valued). Each column is a time
%       series for a particular frequency.
% fout= row vector giving the column coordinate of tvs
% NOTE: the column vector giving the row coordinate of tvs is the same as
%   the input t
%
% by G.F. Margrave, July 2009-2014
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

if(nargin<5); padflag=1; end

p=1;

nt=length(signal);
if(padflag)
    nt=2^(nextpow2(nt));
end

%forward Fourier transform
[Spectrum,f]=fftrl(signal,t,0,nt);

fmin=f(1);

%make sure we have column vectors
m=size(Spectrum,1);
if(m==1); Spectrum=Spectrum.'; end
m=size(f,1);
if(m==1); f=f'; end

%test for even and odd lengths of spectrum
%need this because the Gaussian POU function always returned an even length
%window. If Gaussian_pou is modified to remove this "feature" then this
%code should be dropped.
ls=length(Spectrum);
iuse=1:ls;
if(isodd(ls))
    Spectrum=[Spectrum;0];%remove this later
    f=[f;f(end)+f(2)-f(1)];
end

%build first window and POU norm factor
gdb=inf;
[g,norm_factor,fnotvec,nwin]=gaussian_upou(f,fmin,fwidth,finc,0,0,gdb,0);


%now loop over windows and build the tvs
tvs=zeros(nt,nwin);
fout=zeros(1,nwin);

for k=1:nwin
    fnow=fnotvec(k);
    fout(k)=fnow;
    %make the windows
    g=gaussian_upou(f,fnow,fwidth,finc,norm_factor,fnotvec,gdb,0);
    if(p~=1)
        if(p==0)
            g=ones(size(f));
        else
            g=g.^p;
        end
    end
    %inverse Fourier transform
    S=Spectrum.*g;
    tvs(:,k)=ifftrl(S(iuse),f(iuse));
end