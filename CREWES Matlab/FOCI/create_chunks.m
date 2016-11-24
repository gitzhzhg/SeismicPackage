function [seischunk,fchunk,xchunk,ichunk]=create_chunks(seisf,f,x,dt,vcrit,eta,beta,fmin,fmax,iprint)
% CREATE_CHUNKS ... Spatially resample a seismic wavefield
%
% [seischunk,fchunk,xchunk,ichunk]=create_chunks(seisf,f,x,dt,vcrit,eta,beta,fmin,fmax,iprint)
%
% seisf ... input seismic matrix in the frequency domain. Each row is a
%   constant frequency, each column is a constant spatial position. seisf
%   should contain only the non-negative frequencies as created by fftrl.
% f ... frequency coordinate vector for the rows of seisf (Hz)
% x ... spatial coordinate vector for the columns of seisf (must be regularly sampled and increasing)
% dt ... temporal sample rate of the original time domain data
% vcrit ... critical velocity for spatial resampling. This determines the
%       largest useful spatial nyquist wavenumber (called the evanescent wavenumber) at each frequency.
% eta ... a new frequency chunk is defined whenever the evanescent wavenumber falls below eta*nyquist
%       Choose .5 if you are unsure.
% beta ... a new frequency chunk is resampled such that its highest evanescent wavenumber is beta*new_nyquist
%       Choose .9 if you are unsure.
% fmin, fmax ... minimum and maximum frequencies desired. Frequencies outside this range will be lost.
% iprint ... make 0 to suppress printing resmapling info, 1 otherwise.
% seischunk ... cell array of the seismic data as partitioned into frequency chunks. 
%       Length(seischunck) is the number of chuncks. seisfchunk=seischunk{j} is a seimic matrix for the 
%       jth frequency band.
% fchunk ... cell array of the frequency coordinate vectors for each chunk.
% xchunk ... cell array of the x coordinate vectors for each chunk. These
%       will each have a different sample rate. This means that
%       seisfchunk=seischunk{j} is a matrix of size nf-by-nx where
%       nf=length(fchunk{j}) and nx=length(xchunk{j}).
% ichunk ... cell array of the frequency coordinate indicies of each chunk.
%       That is ifreqs=ichunk{k} gives the row numbers of the position of
%       seisfchunk=seischunk{j} in the re-assembled matrix.
% vcrit ... the 'critical' velocity that drives the spatial resampling. If
%       you have no better choice, make vcrit=min(v(:)), i.e. the slowest
%       velocity in your model.
%
% NOTE: See assemble_chunks for the inverse process.
% 
%
% G.F. Margrave, CREWES/POTSI 2008
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

small=100*eps;
dx=abs(x(2)-x(1));
df=f(2)-f(1);
nchunks=0;
knyquist=.5/dx;
ftop=fmax;
while ftop>fmin
    kcrit=eta*knyquist;
    fcrit=kcrit*vcrit;
    if(fcrit>ftop-df) fcrit=ftop-df; end %need at least one sample
    fcrit=df*floor(fcrit/df);
    if(fcrit<fmin)
        fcrit=fmin; 
    end
    ich=near(f,fcrit,ftop);
    nchunks=nchunks+1;
    fchunk{nchunks}=f(ich);
    seischunk{nchunks}=seisf(ich,:);
    
    xchunk{nchunks}=x;
    ichunk{nchunks}=ich;
    knyquist=kcrit/beta;
    ftop=fcrit-df;
end
if(iprint)
    disp(['Seismic broken into ' int2str(nchunks) ' frequency bands'])
end

%resample each chunk
for k=1:nchunks
    seistmp=seischunk{k};
    ftmp=fchunk{k};
    nyqflag=0;
    dcflag=0;
    if(abs((max(ftmp)-.5/dt))<small); nyqflag=1; end
    if(abs(min(ftmp))<small); dcflag=1; end
    [seistmpr,xr]=kdesample(seistmp,x,ftmp,100*beta,vcrit,dcflag,nyqflag);
    
    xchunk{k}=xr;
    seischunk{k}=seistmpr;
    
    if(iprint)
        disp(['Seismic chunk ' int2str(k) ' resampled from ' num2str(x(2)-x(1),3) '(m) to ' num2str(xr(2)-xr(1),3) '(m)'])
        disp(['frequency band for this chunk is ' num2str(max(ftmp),3) '(Hz) to ' num2str(min(ftmp),3) '(Hz)'])
        disp(['Number of traces dropped from ' int2str(length(x)) ' to ' int2str(length(xr))])
    end
end