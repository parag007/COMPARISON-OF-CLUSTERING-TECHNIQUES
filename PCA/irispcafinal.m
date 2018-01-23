load fisheriris
gscatter(meas(:,1), meas(:,2),  species,'rgb'); hold on
gscatter(meas(:,3), meas(:,4),  species,'rgb');hold on
xlabel('Sepal length');
ylabel('Sepal width');


[pc,score,latent,tsquare] = princomp(meas);
pc,latent
cumsum(latent)./sum(latent)