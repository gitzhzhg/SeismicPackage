function wavelet=s_roots2wavelet(zroots,first,step,maxamp)
% Compute wavelet from the zeros of its z-transform
% Written by: E. Rietsch: June 13, 2004
% Last updated:
%
%          wavelet=s_roots2wavelet(zroots,first,step)
% INPUT
% zroots   roots of the z-transform
% first    time of first sample
% step     sample interval
% maxamp   maximum amplitude of resulting wavelet
% OUTPUT
% wavelet  one-trace seismic structure

temp=real(poly(zroots));
temp=temp*(maxamp/max(temp));
wavelet=s_convert(flipud(temp(:)),first,step);
wavelet.tag='wavelet';
