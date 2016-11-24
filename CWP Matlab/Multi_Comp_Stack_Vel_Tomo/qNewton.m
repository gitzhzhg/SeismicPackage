%==============================================================================
% 2-point raytracing based on ``shooting'' solwed with quasi-Newton method
%==============================================================================

function slowout = qNewton(slow, ipair, tol);

%==============================================================================

global Source Receiv Npair 

%==============================================================================

shift = 0.002;
xReceiv   = shooting(slow, ipair);
   misfit = sqrt(abs(xReceiv(1) - Receiv(1,ipair))^2 + ...
                 abs(xReceiv(2) - Receiv(2,ipair))^2);

while misfit > tol
  slow1 = [slow(1)-shift/2, slow(2)-shift*sqrt(3)/2];
  xReceiv1 = shooting(slow1, ipair);

  slow2 = [slow(1), slow(2)+shift];
  xReceiv2 = shooting(slow2, ipair);

  slow3 = [slow(1)+shift/2, slow(2)-shift*sqrt(3)/2];
  xReceiv3 = shooting(slow3, ipair);

% Built the eqs to be solved to find dslow/dxReceiv
  A = [xReceiv1(1)-xReceiv(1), xReceiv1(2)-xReceiv(2), 0, 0; ...
       0, 0, xReceiv1(1)-xReceiv(1), xReceiv1(2)-xReceiv(2); ...
       xReceiv2(1)-xReceiv(1), xReceiv2(2)-xReceiv(2), 0, 0; ...
       0, 0, xReceiv2(1)-xReceiv(1), xReceiv2(2)-xReceiv(2); ...
       xReceiv3(1)-xReceiv(1), xReceiv3(2)-xReceiv(2), 0, 0; ...
       0, 0, xReceiv3(1)-xReceiv(1), xReceiv3(2)-xReceiv(2)];
  B = [slow1(1)-slow(1), slow1(2)-slow(2), ...
       slow2(1)-slow(1), slow2(2)-slow(2), ...
       slow3(1)-slow(1), slow3(2)-slow(2)];
% dsdR(1) --> dslow(1)/dxReceiv(1),   dsdR(2) --> dslow(1)/dxReceiv(2),
% dsdR(3) --> dslow(2)/dxReceiv(1),   dsdR(4) --> dslow(2)/dxReceiv(2) 
  dsdR = pinv(A)*B';

% New slowness vector -- quasi-Newton
  slow(1) = slow(1) + dsdR(1)*(Receiv(1,ipair)-xReceiv(1)) + ...
                      dsdR(2)*(Receiv(2,ipair)-xReceiv(2));
  slow(2) = slow(2) + dsdR(3)*(Receiv(1,ipair)-xReceiv(1)) + ...
                      dsdR(4)*(Receiv(2,ipair)-xReceiv(2));

  xReceiv   = shooting(slow, ipair);
     misfit = sqrt(abs(xReceiv(1) - Receiv(1,ipair))^2 + ...
                   abs(xReceiv(2) - Receiv(2,ipair))^2);
  fprintf(' slowness = %g %g,  misfit = %g \r', [slow, misfit]);
end;

slowout = slow;

%==============================================================================

