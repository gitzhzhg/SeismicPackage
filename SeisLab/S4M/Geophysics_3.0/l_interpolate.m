function wlog=l_interpolate(wlog,mnemonics)
% Function interpolates null values of all curves specified by an optional list 
% of mnemonics. The function assumes that null values are represented by NaNs.
% DEPRECATED: Replace by "l_fill_gaps".
%
% Date April 29, 2000;  written by E. Rietsch
% Last update: June 8, 2004: use "wlog" instead of "log" and "log_out"
%
%             wlog=l_interpolate(wlog,mnemonics)
% INPUT
% wlog        log structure
% mnemonics   mnemonic or cell array of mnemonics to interpolate
%             if mnemonic is not given or empty all curves are interpolated
% OUTPUT
% wlog       log with interpolated curves; curves not in the list of mnemonics are
%            copied unchanged

alert('DEPRECATED: Replace by "l_fill_gaps".')

wlog=l_fill_gaps(wlog,mnemonics);
