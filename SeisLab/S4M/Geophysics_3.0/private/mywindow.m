function wndw=mywindow(lwindow,name,param,param2)
% Compute a window for signal-processing purposes
%
% Written by: E. Rietsch:
% Last updated: October 10, 2006: Add cosine window
%
%        wndw=mywindow(n,name,param,param2)
% INPUT
% lwindow   window length
% name   name of the window; possible values are:
%       'Bartlett'   Triangular window (same as 'triang', 'triangle')
%       'Blackman'
%       'cosine'     rectangular window with cosine taper (input argument param 
%                    used to define length of taper as a fraction of the total
%                    window length; 0 <= param <= 0.5)
%       'Dolph'
%       'Gauss'      an optional parameter "param" sets both end values. 
%                    Default: param=0.005
%  ,    'Hamming'    raised cosine window
%       'Hanning'    cosine-shaped window
%       'Harris', 
%       'none'       same as 'rect'
%       'Nutbess'
%       'Nuttall'
%       'Papoulis'
%       'Parzen'
%       'rect',      rectangular window; same as 'none'
%       'sine'       rectangular window with sine taper (input argument param 
%                    used to define length of taper as a fraction of the total
%                    window length; 0 <= param <= 0.5)
%       'trapezoid'  rectangular window with linear taper (input argument param 
%                    used to define length of taper as a fraction of the total
%                    window length; 0 <= param <= 0.5)
%	'triang'     same as 'Bartlett'
%	'triangle'   same as 'Bartlett'
%
%        Default: name='Hamming';
% param   additional optional parameter
%       cosine window: fraction of "lwindow" used for the cosine taper 
%                      0<=param<=0.5; default: param=0.5
%       Gauss window:  value (step) at the end of the window
%                      param < 1; default: 0.005
%       sine window:   fraction of "lwindow" used for the sine taper 
%                      0<=param<=0.5; default: param=0.5
%       trapezoid window: fraction of "lwindow" used for the linear taper 
%                      0<=param<=0.5; default: param=0.5
% param2  second optional parameter
% OUTPUT
% wndw  column vector of length "lwindow" with samples of the requested window
%
% EXAMPLE
%       wndw=mywindow(256,'Gauss',0.005);
%       figure; plot(wndw);


if nargin == 0
   error ('At least 1 input parameter is required.')
end

if lwindow <= 0
   error('Window length "lwindow" should be strictly positive.')
end;

if nargin == 1
   name= 'HAMMING';
else
   name=upper(name);
end


switch name

case 'BARTHANN'
   wndw=0.38*(1.0-cos(2.0*pi*(1:lwindow)/(lwindow+1))') ...
       +0.48*min((1:lwindow),(lwindow:-1:1))'/(lwindow+1);


case {'BARTLETT','TRIANG','TRIANGLE'},
   wndw=2.0*min((1:lwindow),(lwindow:-1:1))'/(lwindow+1);


case 'BLACKMAN'
   ind=(-(lwindow-1)/2:(lwindow-1)/2)' *2.0*pi/lwindow;
   wndw=0.42 + 0.50*cos(ind) + 0.08*cos(2.0*ind);


case 'COSINE'
   if nargin < 3  || isempty(param) ||  param > 0.5  ||  param < 0
      param=0.25;
   end    
   n1=round(lwindow*param);
   wndw=ones(lwindow,1);
   wndw(1:n1)=sin(0.5*pi*(1:n1)/n1).^2;
   wndw(end-n1+1:end)=wndw(n1:-1:1);


case {'DOLPH','DOLF'}
   if rem(lwindow,2) == 0
      oddlength=1; 
      lwindow=2*lwindow+1; 
   else 
      oddlength=0; 
   end;
   if nargin < 3  ||  isempty(param) 
      A=1e-3; 
   else 
      A=10^(param/20); 
   end;
   lw1=lwindow-1; 
   Z0=cosh(acosh(1.0/A)/lw1); 
   x0=acos(1/Z0)/pi; 
   x=(0:lw1)/lwindow; 
   indices1=find((x<x0) | (x>1-x0));
   indices2=find((x>=x0) & (x<=1-x0));
   wndw(indices1)=cosh(lw1*acosh(Z0*cos(pi*x(indices1))));
   wndw(indices2)=cos(lw1*acos(Z0*cos(pi*x(indices2))));
   wndw=fftshift(real(ifft(lw1*real(wndw))));
   wndw=wndw'/wndw(lw1/2+1);
   if oddlength
      wndw=wndw(2:2:lw1); 
   end;

%{
case 'FEJER'
   nh=fix(lwindow/2);
   ind=(1:nh-1)/nh;
   h1=sin(pi*ind*(nh+1)).^2./(sin(pi*ind).^2*(nh+1));
   wndw=h1;
   error('Fejer window not yet implemented')
%  wndw= needs checking
%}

case 'GAUSS'
   if nargin < 3  ||  isempty(param) 
      param=0.005; 
   end;
   wndw=exp(log(param) * linspace(-1,1,lwindow)'.^2 );


case 'HAMMING'
   wndw=0.54 - 0.46*cos(2.0*pi*(1:lwindow)'/(lwindow+1));


case {'HANNING','HANN'}
   wndw=(sin((pi/(lwindow+1))*(1:lwindow))').^2;


case 'HARRIS'
   ind=(1:lwindow)' *2.0*pi/(lwindow+1);
   wndw= 0.35875 ...
     -0.48829 *cos(    ind) ...
     +0.14128 *cos(2.0*ind) ...
     -0.01168 *cos(3.0*ind);


case 'KAISER'
  if nargin < 3  ||  isempty(param) 
     beta=3.0*pi; 
  else 
     beta=param; 
  end;
  ind=(-(lwindow-1)/2:(lwindow-1)/2)' *2/lwindow;
  wndw=bessel(0,j*beta*sqrt(1.0-ind.^2))/real(bessel(0,j*beta));


case {'NONE','RECTANG','RECT'} 
   wndw=ones(lwindow,1);


case 'NUTBESS'
   if nargin < 3
      beta=3*pi; 
   elseif isempty(param) 
      beta=3*pi;
   else
      beta=param;
   end
   if nargin == 4
      nu=param2;
   else 
      nu=0.5;
   end;
   ind=(-(lwindow-1)/2:(lwindow-1)/2)' *2/lwindow; 
   wndw=sqrt(1-ind.^2).^nu .* ...
     real(bessel(nu,j*beta*sqrt(1.0-ind.^2)))/real(bessel(nu,j*beta));


case 'NUTTALL'
 ind=(-(lwindow-1)/2:(lwindow-1)/2)' *2.0*pi/lwindow;
 wndw=0.3635819             +0.4891775*cos(    ind) + ...
      0.1363995*cos(2.0*ind)+0.0106411*cos(3.0*ind) ;


case 'PAPOULIS'
   ind=(1:lwindow)'*pi/(lwindow+1); 
   wndw=sin(ind);


case 'PARZEN'
   ind=abs(-(lwindow-1)/2:(lwindow-1)/2)'*2/lwindow; 
   temp=2*(1.0-ind).^3;
   wndw=min(temp-(1-2.0*ind).^3,temp);


case 'SINE'
   if nargin < 3  ||  isempty(param) ||  param > 0.5  ||  param < 0
      param=0.25;
   end    
   n1=round(lwindow*param);
   wndw=ones(lwindow,1);
   wndw(1:n1)=sin(0.5*pi*(1:n1)/n1);
   wndw(end-n1+1:end)=wndw(n1:-1:1);


case 'TRAPEZOID'
   if nargin < 3  ||  isempty(param) ||  param > 0.5  ||  param < 0
      param=0.25;
   else
      if param < 0  || param > 0.5
         error('Parameter "param" must be between 0 and 0.5.')
      end
   end
   n1=fix(lwindow*param);
   wndw=ones(lwindow,1);
   wndw(1:n1)=sin(pi*0.5*(1:n1)/(n1+1)).^2;
   wndw(end:-1:end-n1+1)=wndw(1:n1);


otherwise
   error([' Unknown window: ',name]);

end
