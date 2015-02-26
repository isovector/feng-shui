orig = csvread('data.csv');

function result = toMinutes(time)
    ipart = fix(time);
    fpart = time - ipart;
    result = ipart * 60 + fpart * 100;
end


for i = 2 : size(orig)
    orig(i,4) = toMinutes(orig(i,4));
    orig(i,6) = toMinutes(orig(i,6));
%    stats(i,:) = orig(i,:);
    stats(i,:) = [ orig(i,:), orig(i-1,:) ];
end

stats(1,:) = [];
avgCols = mean(stats);
for i = 1 : size(stats)
    stats(i,:) -= avgCols;
end

covM = stats * stats.';

[eigVecs, eigVals] = eigs(covM, size(covM, 1));

(stats.' * eigVecs)(:,1)
