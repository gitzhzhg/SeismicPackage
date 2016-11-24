% Select model interfaces 
close all;   clear all;

xx = [0: 0.01: 5.0];   N = 4;

x  = [0.00, 0.50, 1.00, 1.50, 2.00, 2.50, 3.00, 3.50, 4.00, 4.50, 5.00];
z1 = [0.90, 0.85, 0.80, 0.90, 0.90, 0.80, 0.75, 0.65, 0.60, 0.50, 0.40];
z2 = [1.80, 1.85, 1.90, 1.95, 2.00, 1.80, 1.65, 1.50, 1.30, 1.10, 0.80]-0.2;
z3 = [2.80, 2.75, 2.62, 2.90, 2.68, 2.46, 2.25, 2.08, 1.95, 1.40, 1.20];

p1 = polyfit(x, z1, N)
xz1 = polyval(p1, xx);
p2 = polyfit(x, z2, N)
xz2 = polyval(p2, xx);
p3 = polyfit(x, z3, N)
xz3 = polyval(p3, xx);

figure;   hold on;   grid;
plot(x, z1, 'o');
plot(xx, xz1);
plot(x, z2, 'o');
plot(xx, xz2);
plot(x, z3, 'o');
plot(xx, xz3);

