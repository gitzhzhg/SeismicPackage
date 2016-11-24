%==============================================================================
% 2-point raytracing based on ``shooting''
%==============================================================================

function misfit = shooting2(slow, ipair);

%==============================================================================

global Source Receiv Npair

xReceiv = shooting(slow, ipair);
if isnan(xReceiv);  
   misfit = 1.e+4*sqrt(slow(1)^2 + slow(2)^2) + 1.;
else
   misfit = sqrt(abs(xReceiv(1) - Receiv(1,ipair))^2 + ...
                 abs(xReceiv(2) - Receiv(2,ipair))^2);
end;

fprintf(' slowness = %g %g,  misfit = %g \r', [slow, misfit]);
%fprintf(' slowness = %g %g,  misfit = %g \n', [slow, misfit]);

%==============================================================================

