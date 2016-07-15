year = c("1993-94", "1994-95","1995-96", "1996-97","1997-98", "1998-99","1999-00", "2000-01","2001-02","2002-03","2003-04"
         ,"2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15")

school = c("School", "District", "County", "State")

site = c()
chusd = list()

for(i in 1:length(year)){
  site[i] = paste("http://data1.cde.ca.gov/dataquest/Schgrad.asp?cSelect=Coalinga%5EHigh%5E%5E%5E%5E%5E%5E%5E--Coalinga-Huron%5E--1062125-1031376&cChoice=SchGrad&cYear=",year[i],"&ProgramName=All&cTopic=Graduates&cLevel=School&myTimeFrame=S", sep = "")
  chusd[[i]] = readLines(site[i])
  }

#################################################################################

grad.latino = lapply(chusd, function (x) grep("\\(\\'Hispanic\\'\\)[[:blank:]]*G", x))
uc.latino = lapply(chusd, function (x) grep("\\(\\'Hispanic\\'\\)[[:blank:]]*UC", x))

grad.l = list()
for(i in 1:22){
grad.l[[i]] = chusd[[i]][grad.latino[[i]]+1]
}
grad.l

uc.l = list()
for(i in 1:22){
uc.l[[i]] = chusd[[i]][uc.latino[[i]]+1]
}
uc.l

pattern = "[^[:digit:]]"
nothing = ""

grad.latino = lapply(grad.l, function (x) gsub(pattern = pattern, replacement = nothing, x))
uc.grad.latino = lapply(uc.l , function (x) gsub(pattern = pattern, replacement = nothing, x))

grad.latino = lapply(grad.latino, as.numeric)
uc.grad.latino = lapply(uc.grad.latino, as.numeric)

grad.latino = t(data.frame(grad.latino))
uc.grad.latino = t(data.frame(uc.grad.latino))

rownames(grad.latino) = year
rownames(uc.grad.latino) = year

colnames(grad.latino) = school
colnames(uc.grad.latino) = school

uc.percent = (uc.grad.latino[,2] / grad.latino[,2])*100
plot(uc.percent, type = "l", ylim = c(0,50), main = "Latino")

#################################################################################

grad.white = lapply(chusd, function (x) grep("\\(\\'White\\'\\)[[:blank:]]*G", x))
uc.grad.white = lapply(chusd, function (x) grep("\\(\\'White\\'\\)[[:blank:]]*UC", x))

grad.w = list()
for(i in 1:22){
  grad.w[[i]] = chusd[[i]][grad.white[[i]]+1]
}
grad.w

uc.w = list()
for(i in 1:22){
  uc.w[[i]] = chusd[[i]][uc.grad.white[[i]]+1]
}
uc.w

grad.white = lapply(grad.w, function (x) gsub(pattern = pattern, replacement = nothing, x))
uc.grad.white = lapply(uc.w , function (x) gsub(pattern = pattern, replacement = nothing, x))

grad.white = lapply(grad.white, as.numeric)
uc.grad.white = lapply(uc.grad.white, as.numeric)

grad.white = t(data.frame(grad.white))
uc.grad.white = t(data.frame(uc.grad.white))

rownames(grad.white) = year
rownames(uc.grad.white) = year

colnames(grad.white) = school
colnames(uc.grad.white) = school

uc.percent2 = (uc.grad.white[,2] / grad.white[,2])*100
plot(uc.percent2, type = "l", ylim = c(0,50), main = "White")

#################################################################################

plot(uc.percent2, xaxt = "n", type = "l", col = "red", ylim = c(0,50), main = "Coalinga Graduates with UC/CSU Required Courses", xlab = "Year", ylab = "Percentage")
lines(uc.percent, type = "l", col = "blue")
axis(1, at=1:22, labels=year)
legend("topright", c("White", "Hispanic"), col = c("red", "blue"), lty = 1)

