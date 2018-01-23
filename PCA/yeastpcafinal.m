load yeastdata.mat
numel(genes)

mapcaplot(yeastvalues,genes)

[pc, zscores, pcvars] = pca(yeastvalues);
pcvars./sum(pcvars) * 100
cumsum(pcvars./sum(pcvars) * 100)

figure
scatter(zscores(:,1),zscores(:,2));
xlabel('First Principal Component');
ylabel('Second Principal Component');
title('Principal Component Scatter Plot');

figure
pcclusters = clusterdata(zscores(:,1:2),'maxclust',10,'linkage','av');
gscatter(zscores(:,1),zscores(:,2),pcclusters)
xlabel('First Principal Component');
ylabel('Second Principal Component');
title('Principal Component Scatter Plot with Colored Clusters');

gname(genes)