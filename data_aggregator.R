# Welcome! To data aggregator!
# TO DO: Handle bug on line 82, matches stuff it's not supposed to.

# Read in the data file
dat <- read.delim("./raw_data/Sub1.txt") 
head(dat)
#View(dat)

# Basic data import, label, and concatenate loop
dat1 <- read.delim("./raw_data/Sub1.txt")
dat1$ID = 1 
dat2 <- read.delim("./raw_data/Sub2.txt") 
dat2$ID = 2
dat <- rbind(dat1, dat2)

# Let's not repeat ourselves. 
# Let's make a loop so we don't have to type read.delim("SubX.txt" a billion times)

# With paste() we can make a string that varies with the loop
coolStuff = c("Shades", "Story", "Beans")
paste("Cool", coolStuff)
paste("Cool", coolStuff, sep="... ") # can change sep argument to whatever

paste("Sub", 1:27, sep="") # aha!
paste("Sub", 1:27, ".txt", sep="")

for (i in 1:27) {
  fileName = paste("./raw_data/Sub", i, ".txt", sep="")
  print(fileName)
}


dat = data.frame(NULL) # make empty container
for (i in 1:27) {
  fileName = paste("./raw_data/Sub", i, ".txt", sep="") # make filename
  temp = read.delim(file=fileName, stringsAsFactors=F) # read data with filename
  temp$ID = i # mark data with subject number
  dat = rbind(dat, temp) # staple it to previous
}

# make a column for race of face
# We want to match all columns where FaceFilename contains "Black"
# grep() function lets us search and match strings
row.names(mtcars)
grep("Merc", row.names(mtcars))
grep("Merc", row.names(mtcars), value=T)

# So let's set up a fresh column for race
dat$race = NA
# Where FaceFilename contains "Black", we set dat$race to "black"
dat$race[grep("Black", dat$FaceFilename)] = "Black"
#View(dat)
dat$race[grep("White", dat$FaceFilename)] = "White"
#View(dat)

# We can do a similar thing to fix the point of fixation
forehead_range = c(1:8)
eyes_range = c(9:16)
nose_range = c(17:24)

dat$fixation = NA # empty container
for (i in forehead_range) {
  dat$fixation[grep(i, dat$FaceFilename)] = "forehead"
} # note that this will still match 1 with 11, 21, but we're gonna fix those later
for (i in eyes_range) {
  dat$fixation[grep(i, dat$FaceFilename)] = "eyes"
}
for (i in nose_range) {
  dat$fixation[grep(i, dat$FaceFilename)] = "nose"
} # not a problem now because it won't match 21 to 1
#View(dat)
sub2dat = dat[dat$ID == 2,]
table(sub2dat$fixation, sub2dat$Block)

# Fix Block 3 fixation data, too
table(dat$FaceFilename[dat$Block==3]) # looks like range is 1-48
temp = dat$FaceFilename[dat$Block==3]
table(temp)

# We can do a similar thing to fix the point of fixation
forehead_range = c(1:8, 25:32)
eyes_range = c(9:16, 33:40) # BUG: Matches for forehead 29.
nose_range = c(17:24, 41:48)

for (i in forehead_range) {
  dat$fixation[grepl(i, dat$FaceFilename) & dat$Block == 3] = "forehead"
} # note that this will still match 1 with 11, 21, but we're gonna fix those later
for (i in eyes_range) {
  dat$fixation[grepl(i, dat$FaceFilename) & dat$Block == 3] = "eyes"
}
for (i in nose_range) {
  dat$fixation[grepl(i, dat$FaceFilename) & dat$Block == 3] = "nose"
} # not a problem now because it won't match 21 to 1

sub2dat = dat[dat$ID == 2,]
table(sub2dat$fixation, sub2dat$Block)

# export data for analysis
write.table(dat, "cleaned_data.txt", sep="\t", row.names=F)