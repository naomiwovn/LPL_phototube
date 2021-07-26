## Naomi Yescas
## LPL EUV Phototube
## 06-10-19

# R1187 Hamamatsu Phototube Radiant Sensitivity
radiant.sensitivity <- 0.008 #A/W
# Charge of an electron
electron.charge <- 1.602176634e-19# J
# planck const
planck <- 6.63e-34
# speed of light in vacuum
c <- 3e8 # (m/s)
# wavelength 
wavelength <- 1.21567e-7 #(m)
# light power
lamp.power <-  24 

photons.sec <- 1e14
photons.sec 

emission <- (photons.sec * electron.charge)
emission
##### current source test ##################################################
# Current source values in mA
test.iin <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2) /1000

# curretn divider in 
test.iin <- test.iin / 100 # 1:100 current divider
test.iin 

# Voltage monitor vo mV
test.vo <- c(-0.337, -0.331, -0.320, -0.314, -0.313, -0.310, -0.307, -0.307, -0.305, -0.303, -0.300, -0.295, -0.292) 
test.vo1 <- c(-0.333, -0.327, -0.330, -0.330, -0.319, -0.317, -0.314, -0.312, -0.308, -0.303, -0.301, -0.297, -0.294)
test.vo2 <- c(-0.336, -0.331, -0.325, -0.323, -0.317, -0.314, -0.313, -0.309, -0.306, -0.300, -0.297, -0.290, -0.286)


plot(test.iin, test.vo)
plot(test.iin, test.vo1)
plot(test.iin, test.vo2)

test.vo.d <- data.frame(test.vo, test.vo1,test.vo2)
test.vo.d

test.vo.mean <- rowMeans(test.vo.d)

plot(test.iin,test.vo.mean)
test.lm <- lm(test.vo.mean ~ test.iin) 
abline(test.lm)

# scale 1uA
test.slope <- test.lm$coefficients[2]
test.slope

# 1 uA change will result in a change of 
# 3485.348


##### Hamamatsu Phototube Test 1 #####################################################################################
# R1187 Phototube lamp off test 1
# vo mV
off.vo <- c(9.15, 9.146, 9.149, 9.167, 9.166, 9.152, 9.184, 9.151, 9.154, 9.148)
off.vo.mean <- mean(off.vo)
off.vo.mean
# 9.1567
off.vo.median <- median(off.vo)
# 9.1515

# R1187 Phototube lamp on test 1
# 5.9E-5 Torr
on.vo <- c(8.922, 8.912, 8.935, 8.937, 8.934, 8.932, 8.932, 8.933, 8.931, 8.933)
on.vo.mean <- mean(on.vo)
on.vo.mean 
# 8.9301
on.vo.median <- median(on.vo)
# 8.9325

# on/off change
delta.on.off <- off.vo.mean - on.vo.mean
delta.on.off
# 0.2266 mV 
delta.on.off.median <- off.vo.median - on.vo.median
# 0.219 mV

current.data.1 <- -c(9.15, 9.146, 9.149, 9.167, 9.166, 9.152, 9.184, 9.151, 9.154, 9.148,
                     8.922, 8.912, 8.935, 8.937, 8.934, 8.932, 8.932, 8.933, 8.931, 8.933) / test.slope
plot(current.data.1, ylab = "Test 1 Current (mA)")

#### Phototube Current ############
cur.1 <- delta.on.off / test.slope
cur.1
# 0.651503 uA

cur.1.median <- delta.on.off.median / test.slope
cur.1.median
# 0.6283447 uA
###################################

#### TEST 2 #########################################################################################################
# R1187 Phototube lamp off test 2
# vo mV
off.vo2 <- c(9.144, 9.142, 9.141, 9.141, 9.141, 9.143, 9.144, 9.144, 9.140, 9.139)
off.vo2.mean <- mean(off.vo2)
off.vo2.mean
# 9.1419
off.vo2.median <- median(off.vo2)
# 9.145

# R1187 Phototube lamp on test 2
# 5.0E-5 Torr
on.vo2 <- c(8.874, 8.824, 9.825, 8.826, 8.826, 8.827, 8.827, 8.835, 8.827, 8.829)
plot(on.vo2)

on.vo2.mean <- mean(on.vo2)
on.vo2.mean 
# 8.932

on.vo2.median <- median(on.vo2)
on.vo2.median
# 8.827

delta.on.off2 <- off.vo2.mean - on.vo2.mean
delta.on.off2
# 0.2099

delta.on.off2.median <- off.vo2.median - on.vo2.median
delta.on.off2.median
# 0.3145

current.data.2 <- -c(9.144, 9.142, 9.141, 9.141, 9.141, 9.143, 9.144, 9.144, 9.140, 9.139,
                     8.874, 8.824, 9.825, 8.826, 8.826, 8.827, 8.827, 8.835, 8.827, 8.829) / test.slope
plot(current.data.2, ylab = "Test 2 Current (mA)")

#### Phototube Current ############
cur.2 <- delta.on.off2 / test.slope
cur.2
# 0.6022354 uA

cur.2.median <- delta.on.off2.median / test.slope
cur.2.median
# 0.9023489 uA
###################################



#### TEST 3 #########################################################################################################
# R1187 Phototube lamp off test 3
# vo mV
off.vo3 <- c(9.144, 9.144, 9.145, 9.145, 9.145, 9.145, 9.147, 9.145, 9.143, 9.141, 9.139, 9.138, 9.141, 9.141, 9.140)
off.vo3.mean <- mean(off.vo3)
off.vo3.mean
# 9.1419
off.vo3.median <- median(off.vo3)
# 9.144

# R1187 Phototube lamp on test 3
# 4.8E-5 Torr
on.vo3 <- c(8.976, 8.993, 8.992, 8.991, 8.975, 8.972, 8.977, 8.962, 8.954, 8.953, 8.952, 8.951, 8.957, 8.947, 8.949, 
            8.975, 8.978, 8.949, 8.975, 8.976, 8.967, 8.974, 8.992, 8.951, 8.967, 8.963, 8.963, 8.962, 8.959, 8.959)
plot(on.vo3) # all over the place
on.vo3.mean <- mean(on.vo3)
on.vo3.mean 
# 8.967033
on.vo3.median <- median(on.vo3)
# 8.965

delta.on.off3 <- off.vo3.mean - on.vo3.mean
delta.on.off3
# 0.1758333

delta.on.off3.median <- off.vo3.median - on.vo3.median
# 0.179

current.data.3 <- -c(9.144, 9.144, 9.145, 9.145, 9.145, 9.145, 9.147, 9.145, 9.143, 9.141, 9.139, 9.138, 9.141, 9.141, 9.140,
          8.976, 8.993, 8.992, 8.991, 8.975, 8.972, 8.977, 8.962, 8.954, 8.953, 8.952, 8.951, 8.957, 8.947, 8.949, 
          8.975, 8.978, 8.949, 8.975, 8.976, 8.967, 8.974, 8.992, 8.951, 8.967, 8.963, 8.963, 8.962, 8.959, 8.959) / test.slope
plot(current.data.3, ylab = "Test 3 Current (mA)")

#### Phototube Current ############
cur.3 <- delta.on.off3 / test.slope
cur.3
# 0.5044929 uA

cur.3.median <- delta.on.off3.median / test.slope
cur.3.median 
# 0.5135786 uA
###################################

#### TEST 4 #########################################################################################################
# R1187 Phototube lamp off test 4
# vo mV
off.vo4 <- c(9.145, 9.146, 9.145, 9.145, 9.145, 9.146, 9.145, 9.144,9.145, 9.144, 9.146, 9.146, 9.147, 9.147, 9.146)
off.vo4.mean <- mean(off.vo4)
off.vo4.mean
# 9.145467
off.vo4.median <- median(off.vo4)
# 9.145

# R1187 Phototube lamp on test 4
# 4.7E-5 Torr
on.vo4 <- c(8.897, 8.881, 8.882, 8.881, 8.884, 8.881, 8.878, 8.866, 9.043, 9.044, 9.046, 9.045, 9.034, 9.046, 9.038, 
            9.039, 9.039, 9.038, 9.037, 9.036, 9.036, 9.037, 9.037, 9.037, 9.035, 9.036, 9.037, 9.036, 9.038, 9.036)


##############################################
# ...interesting... what happend here? #######
plot(on.vo4)
on.vo4.1 <-c(8.897, 8.881, 8.882, 8.881, 8.884, 8.881, 8.878, 8.866)
on.vo4.2 <-c(9.043, 9.044, 9.046, 9.045, 9.034, 9.046, 9.038, 
            9.039, 9.039, 9.038, 9.037, 9.036, 9.036, 9.037, 
            9.037, 9.037, 9.035, 9.036, 9.037, 9.036, 9.038, 9.036)
on.vo4.1.mean <- mean(on.v4.1)
on.vo4.1.mean
# 8.88125
on.vo4.2.mean <- mean(on.v4.2)
on.vo4.2.mean
# 9.038636

delta.on.off4.1 <- off.vo4.mean - on.vo4.1.mean
delta.on.off4.1
# 0.2642167
delta.on.off4.2 <- off.vo4.mean - on.vo4.2.mean
delta.on.off4.2
# 0.1068303

## Photocathode Current Individual ###########
cur.4.1 <- delta.on.off4.1 / test.slope
cur.4.1
# 0.7580783 uA

cur.4.2 <- delta.on.off4.2 / test.slope
cur.4.2
# 0.3065126 uA
##############################################
##############################################


current.data.4 <- -c(9.145, 9.146, 9.145, 9.145, 9.145, 9.146, 9.145, 9.144,9.145, 9.144, 9.146, 9.146, 9.147, 9.147, 9.146,
          8.897, 8.881, 8.882, 8.881, 8.884, 8.881, 8.878, 8.866, 9.043, 9.044, 9.046, 9.045, 9.034, 9.046, 9.038, 
          9.039, 9.039, 9.038, 9.037, 9.036, 9.036, 9.037, 9.037, 9.037, 9.035, 9.036, 9.037, 9.036, 9.038, 9.036) / test.slope
plot(current.data.4, ylab = "Test 4 Current (mA)")

##############################################
#***turned on vac ion gauge after test, read 5.1e-5 # LOST VACUUM???
# Note: test again at 4.7E-5 Torr

on.vo4.mean <- mean(on.vo4)
on.vo4.mean 
# 8.996667
on.vo4.median <- median(on.vo4)
# 9.0365

delta.on.off4 <- off.vo4.mean - on.vo4.mean
delta.on.off4
# 0.1488

delta.on.off4.median <- off.vo4.median - on.vo4.median
# 0.1085

#### Phototube Current ############
cur.4<- delta.on.off4 / test.slope
cur.4
# 0.4269301 uA

cur.4.median <- delta.on.off4.median / test.slope
cur.4.median
# 0.3113032 uA 
###################################

#### TEST 5 #########################################################################################################
# R1187 Phototube lamp off test 5
# vo mV
off.vo5 <- c(9.158, 9.155, 9.148, 9.151, 9.152, 9.151, 9.152, 9.152, 9.152, 9.152, 9.152, 9.153, 9.152, 9.152, 9.153)
off.vo5.mean <- mean(off.vo5)
off.vo5.mean
# 9.152333
off.vo5.median <- median(off.vo5)
# 9.152

# R1187 Phototube lamp on test 5
# Vacuum: 4.7E-5
on.vo5 <- c(9.064, 9.062, 9.058, 9.058, 9.057, 9.057, 9.057, 9.058, 9.057, 9.058, 9.056, 9.054, 9.055, 9.055, 9.057,
            9.057, 9.061, 9.064, 9.063, 9.063, 9.060, 9.060, 9.060, 9.060, 9.059, 9.060, 9.059, 9.058, 9.058, 9.057,
            9.056, 9.056, 9.057, 9.058, 9.060, 9.059, 9.060, 9.060, 9.060, 9.059, 9.058, 9.058, 9.057, 9.057, 9.059)
plot(on.vo5) # very nice
# vacuum after: 5.3E-5
off.vo5.after <- c(9.161, 9.162, 9.161, 9.161, 9.162, 9.161, 9.161, 9.161, 9.161, 9.162, 9.164, 9.164, 9.164, 9.165, 9.166)
off.vo5.after.mean <- mean(off.vo5.after)
off.vo5.after.mean
# 9.1624

current.data.5 <- -c(9.158, 9.155, 9.148, 9.151, 9.152, 9.151, 9.152, 9.152, 9.152, 9.152, 9.152, 9.153, 9.152, 9.152, 9.153,
                        9.064, 9.062, 9.058, 9.058, 9.057, 9.057, 9.057, 9.058, 9.057, 9.058, 9.056, 9.054, 9.055, 9.055, 9.057,
                        9.057, 9.061, 9.064, 9.063, 9.063, 9.060, 9.060, 9.060, 9.060, 9.059, 9.060, 9.059, 9.058, 9.058, 9.057,
                        9.056, 9.056, 9.057, 9.058, 9.060, 9.059, 9.060, 9.060, 9.060, 9.059, 9.058, 9.058, 9.057, 9.057, 9.059,
                        9.161, 9.162, 9.161, 9.161, 9.162, 9.161, 9.161, 9.161, 9.161, 9.162, 9.164, 9.164, 9.164, 9.165, 9.166) / test.slope
plot(current.data.5, ylab = "Test 5 Current (mA)")

on.vo5.mean <- mean(on.vo5)
on.vo5.mean 
# 9.03656
on.vo5.median <- median(on.vo5)
# 9.058

delta.on.off5 <- off.vo5.mean - on.vo5.mean
delta.on.off5
# 0.1159778
delta.on.off5.median <- off.vo5.median - on.vo5.median
# 0.094

#### Phototube Current ############
cur.5 <- delta.on.off5 / test.slope
cur.5
# 0.33227581 uA 

cur.5.median <- delta.on.off5.median / test.slope
# 0.2697005 uA
###################################

#### TEST 6 #########################################################################################################
# R1187 Phototube lamp off test 6
# vo mV
# vacuum: 4.5E-5 Torr
off.vo6 <- c(9.155, 9.155, 9.156, 9.157 ,9.157)
off.vo6.vac.gauge.on <- c(9.122, 9.124)
# gauge 4.8E-5 Torr
# gauge off
off.vo6.vac.gauge.off <- c(9.162, 9.163)

off.vo6 <- c(9.167, 9.167, 9.168, 9.169, 9.168, 9.169, 9.168, 9.167, 9.167, 9.167, 9.168, 9.167, 9.169, 9.170, 9.170)
off.vo6.mean <- mean(off.vo6)
off.vo6.mean
# 9.1668067 
off.vo6.median <- median(off.vo6)
# 9.168

# R1187 Phototube lamp on test 6
# Vacuum: 4.8E-5 Torr
on.vo6 <- c(9.100, 9.101, 9.100, 9.099, 9.099, 9.098, 9.099, 9.096, 9.097, 9.096, 9.095, 9.092, 9.094, 9.095, 9.095,
            9.095, 9.092, 9.097, 9.098, 9.098, 9.099, 9.101, 9.103, 9.102, 9.105, 9.014, 9.098, 9.089, 9.099, 9.099, 
            9.098, 9.098, 9.097, 9.097, 9.096, 9.095, 9.095, 9.095, 9.094, 9.095, 9.095, 9.095, 9.094, 9.096, 9.095,
            9.092, 9.091, 9.091, 9.091, 9.092, 9.090, 9.091, 9.090, 9.089, 9.091, 9.089, 9.090, 9.089, 9.090, 9.090)
plot(on.vo6) # not as nice but decent
# vacuum after: 4.7E-5 Torr
off.vo6.after <- c(9.170, 9.169, 9.167, 9.167, 9.167, 9.168, 9.168, 9.167, 9.167, 9.167, 9.168, 9.169, 9.168, 9.167)
off.vo6.after.mean <- mean(off.vo6.after)
off.vo6.after.mean
# 9.167786

current.data.6 <- -c(9.167, 9.167, 9.168, 9.169, 9.168, 9.169, 9.168, 9.167, 9.167, 9.167, 9.168, 9.167, 9.169, 9.170, 9.170,
          9.100, 9.101, 9.100, 9.099, 9.099, 9.098, 9.099, 9.096, 9.097, 9.096, 9.095, 9.092, 9.094, 9.095, 9.095,
          9.095, 9.092, 9.097, 9.098, 9.098, 9.099, 9.101, 9.103, 9.102, 9.105, 9.014, 9.098, 9.089, 9.099, 9.099, 
          9.098, 9.098, 9.097, 9.097, 9.096, 9.095, 9.095, 9.095, 9.094, 9.095, 9.095, 9.095, 9.094, 9.096, 9.095,
          9.092, 9.091, 9.091, 9.091, 9.092, 9.090, 9.091, 9.090, 9.089, 9.091, 9.089, 9.090, 9.089, 9.090, 9.090,
          9.170, 9.169, 9.167, 9.167, 9.167, 9.168, 9.168, 9.167, 9.167, 9.167, 9.168, 9.169, 9.168, 9.167) / test.slope

plot(current.data.6, ylab = "Test 6 Current (mA)")
on.vo6.mean <- mean(on.vo6)
on.vo6.mean 
# 9.093933
on.vo6.median <- median(on.vo6)
# 9.095

delta.on.off6 <- off.vo6.mean - on.vo6.mean
delta.on.off6
# 0.07413333 
delta.on.off6.median <- off.vo6.median - on.vo6.median
# 0.073

#### Phototube Current ############
cur.6 <- delta.on.off6 / test.slope
cur.6
# 0.2126999 uA 

cur.6.median <- delta.on.off6.median / test.slope
# 0.2094482 uA
###################################

# The lamp was only turned on for a few seconds to minutes at a time
# As I ran multiple tests, the current seemed to be decreasing over time
# hopefully this is not due to any degredation of any windows. Each test
# was done between 4.7E-5 and 5.9E-5 Torr. 

current.medians <- c(cur.1.median, cur.2.median, cur.3.median, cur.4.median, cur.5.median, cur.6.median)
plot(current.medians)


current.mean <- mean(current)
current.mean
# 0.4548778 uA
current.mean.median <- median(current)
current.mean.median
# 0.4548778 uA 

################ Q E ################################
# max Current 
max.current <- max(current)
# 0.6501503 uA 

# Max Light Intensity measured (mW)
light.intensity <- max.current / radiant.sensitivity
light.intensity
# 8.125879 mW

electrons.sec <- max.current / electron.charge

efficiency <- electrons.sec / photons.sec
efficiency
# 0.4057919
######################################################

# 06-11-19 
#### TEST 6 #########################################################################################################
# R1187 Phototube lamp off test 7
# vo mV
# vacuum: 3.9E-5 Torr
off.vo7 <- c(9.175, 9.176, 9.175, 9.175, 9.176, 9.176, 9.177, 9.175, 9.175, 9.176, 9.174, 9.173, 9.172, 9.173, 9.175)
off.vo7.mean <- mean(off.vo7)
off.vo7.mean
# 9.174867
off.vo7.median <- median(off.vo7)
# 9.175

# R1187 Phototube lamp on test 7
on.vo7 <- c(8.950, 8.953, 8.951, 8.948, 8.947, 8.946, 8.945, 8.944, 8.943, 8.943, 8.942, 8.943, 8.943, 8.944, 9.098, 
            9.099, 9.099, 9.098, 9.095, 9.095, 9.094, 9.093, 9.093, 9.092, 9.090, 9.089, 9.091, 9.095, 9.096, 9.095,
            9.097, 9.097, 9.096, 9.096, 9.097, 9.098, 9.097, 9.096, 9.097, 9.096, 9.096, 9.096, 9.096, 9.097, 9.091)
on.vo7.predrop <- c(8.950, 8.953, 8.951, 8.948, 8.947, 8.946, 8.945, 8.944, 8.943, 8.943, 8.942, 8.943, 8.943, 8.944)
on.vo7.postdrop <- c(9.098, 9.099, 9.099, 9.098, 9.095, 9.095, 9.094, 9.093, 9.093, 9.092, 9.090, 9.089, 9.091, 9.095,
                     9.096, 9.095, 9.097, 9.097, 9.096, 9.096, 9.097, 9.098, 9.097, 9.096, 9.097, 9.096, 9.096, 9.096, 
                     9.096, 9.097, 9.091)
#############################################
on.vo7.postdrop.mean <- mean(on.vo7.postdrop)
on.vo7.postdrop.mean
delta.on.off7.postdrop <- (off.vo7.mean - on.vo7.postdrop.mean)
delta.on.off7.postdrop
cur.7.low <- delta.on.off7.postdrop / test.slope
cur.7.low
## Low Current
## 0.2282242 uA
#############################################

plot(on.vo7) # random drop in current for some reason????
             # Measuring voltage difference because the circuit 
             # is -current from the base measurement
plot(on.vo7.predrop)

# vacuum after: 3.7E-5 Torr
off.vo7.after <- c(9.179, 9.178, 9.180, 9.178, 9.178, 9.178, 9.179, 9.179, 9.177, 9.180, 9.180, 9.180, 9.179, 9.179)
off.vo7.after.mean <- mean(off.vo7.after)
off.vo7.after.mean
# 9.178857

on.vo7.mean <- mean(on.vo7)
on.vo7.mean 
# 9.04882
on.vo7.median <- median(on.vo7)
on.vo7.median
# 9.095

delta.on.off7 <- off.vo7.mean - on.vo7.mean
delta.on.off7
# 0.1260444
delta.on.off7.median <- off.vo7.median - on.vo7.median
delta.on.off7.median
# 0.08

current.data.7 <- -c(9.175, 9.176, 9.175, 9.175, 9.176, 9.176, 9.177, 9.175, 9.175, 9.176, 9.174, 9.173, 9.172, 9.173, 9.175,
                        8.950, 8.953, 8.951, 8.948, 8.947, 8.946, 8.945, 8.944, 8.943, 8.943, 8.942, 8.943, 8.943, 8.944, 9.098, 
                        9.099, 9.099, 9.098, 9.095, 9.095, 9.094, 9.093, 9.093, 9.092, 9.090, 9.089, 9.091, 9.095, 9.096, 9.095,
                        9.097, 9.097, 9.096, 9.096, 9.097, 9.098, 9.097, 9.096, 9.097, 9.096, 9.096, 9.096, 9.096, 9.097, 9.091,
                        9.179, 9.178, 9.180, 9.178, 9.178, 9.178, 9.179, 9.179, 9.177, 9.180, 9.180, 9.180, 9.179, 9.179) / test.slope
plot(current.data.7, ylab = "Test 7 Current (mA)")

#### Phototube Current ############
cur.7 <- delta.on.off7 / test.slope
cur.7
# 36.16409 uA 

## peak current
## 65.70636 uA

## Low Current
## 22.82242 uA

cur.7.median <- delta.on.off7.median / test.slope
cur.7.median
# 22.95323 uA

###################################

current <- c(cur.1, cur.2, cur.3, cur.4, cur.5, cur.6, cur.7)
plot(current)

