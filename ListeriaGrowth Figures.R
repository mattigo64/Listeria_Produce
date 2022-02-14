jpeg('lis Growth~temp', units ='in', width = 5, height = 5, res = 500)
plot(rate~temp,main = 'Growth Rate vs Temperature', ylab = 'Growth Rate (log CFU/day)', xlab = 'Temperature (°F)')
abline(lm(rate~temp), lwd=2,col='red')
dev.off()

jpeg('lis Growth~start', units ='in', width = 5, height = 5, res = 500)
plot(rate~time0, main = 'Growth Rate vs Starting Concentration', ylab = 'Growth Rate (log CFU/day)', xlab = 'Starting Concentration (log CFU)')
abline(lm(rate~time0), lwd=2,col='red')
dev.off()


jpeg('lis Growth~end', units ='in', width = 5, height = 5, res = 500)
plot(rate~timeEnd, main = 'Growth Rate vs Final Concentration', ylab = 'Growth Rate (log CFU/day)', xlab = 'Final Concentration (log CFU)')
abline(lm(rate~timeEnd), lwd=2,col='red')
dev.off()
