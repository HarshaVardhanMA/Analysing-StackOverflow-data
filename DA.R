# u can change
data <- read.csv('new.csv')  #loads the given dataset file,for running in your PC,plesae change the path of file

#list of programming languages
language_list <- c("A# .NET","css","logic4j","maven","A# (Axiom)","A-0 System","A+","A++","ABAP","ABC","ABC ALGOL","ABSET","ABSYS","ACC","Accent","Ace DASL","ACL2","ACT-III","Action!","ActionScript","Ada","Adenine","Agda","Agilent VEE","Agora","AIMMS","Alef","ALF","ALGOL 58","ALGOL 60","ALGOL 68","ALGOL W","Alice","Alma-0","AmbientTalk","Amiga E","AMOS","MPL","Apex","APL","App Inventor for Android's visual block language","AppleScript","Arc","Apache","aspûnet","ûhtaccess","arraylist","ûnet3û5","adoûnet","ARexx","Argus","AspectJ","Assembly language","ATS","Ateji PX","AutoHotkey","Autocoder","AutoIt","AutoLISP","Averest","AWK","Axum","Active Server Pages","ASP.NET","ajax","Babbage","Bash","BASIC","bc","BCPL","BeanShell","Batch","Bertrand","BETA","Bigwig","Bistro","BitC","BLISS","Blockly","BlooP","Blue","Boo","Boomerang","Bourne shell","BREW","BPEL","C","C--","C++","C#","Cach? ObjectScript","C Shell","Caml","Cayenne","CDuce","Cecil","Cesil","Ceylon","groovy","ehcache","axis","openofficeûorg","axis2","jabber","hibernate","CFEngine","CFML","Cg","Ch","Chapel","Charity","Charm","Chef","CHILL","CHIP-8","chomski","ChucK","CICS","Cilk","Citrine","CL","Claire","Clarion","Clean","Clipper","CLIST","Clojure","clover","CLU","CMS-2","COBOL","CobolScript","COBOL","Cobra","CODE","CoffeeScript","ColdFusion","COMAL","COMIT","CIL","Lisp","COMPASS","Pascal","CHR","Converge","Cool","Coq","unicode","Coral","svn","CPL","Cryptol","CUDA","Curl","Cyclone","Cython","DASL","Dart","DataFlex","Datalog","dBase","dbunit","DCL","Deesel","F","F#","Factor","Falcon","Fantom","FAUST","FFP","Fortran","F-Script","G","GNU E","html","HTML5","Hack","HAGGIS","HAL/S","Hamilton C shell","Harbour","Haskell","Haxe","High Level Assembly","HLSL","Hop","Hopscotch","Hope","Hugo","Hume","HyperTalk","J","J#","J++","JADE","Jako","JAL","Janus","JASS","Java","JavaScript","JCL","JEAN","JOSS","Joule","JOVIAL","Joy","JScript","JScript .NET","JavaFX Script","Julia","Jython","linux","M2001","M4","M#","Machine code","MAD","MAD/I","Magik","Magma","MATLAB","XML","Oak","Oberon","OBJ2","Lisp","LOGO","Pascal","octave","openCL","OPL","Oxygene","P#","ParaSail","PEARL","Perl","PDL","Perl6","Pharo","PHP","Phrogram","Pico","Picolisp","Pict","Pike","PIKT","PILOT","Pipelines","PostScript","PortablE","Powerhouse","PowerBuilder","PowerShell","PPL","Prolog","PROMAL","Promela","Python","quakeC","R","R++","Racket","RAPID","Rapira","Ratfiv","Ratfor","Ruby","RuneScript","Rust","S2","SAIL","SALSA","Scala","Scilab","SNOBOL","Snowball","SPARK","Squeak","Squirrel","Swift","T","tcl","TOM","T-SQL","Turbo C++","X++","X#")
language_list <- tolower(language_list)

#test question tags, if u want to test for other tags,change here
testTags <- c('html','css')  
testTags <- tolower(testTags)
#generates subset of testTags and is stored in variable subsets
f <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

subsets <- f(testTags)

#variable is a list that stores all the question posted time and the response time of each of the subsets.
variable <- list()
length(variable) <- 2*(length(subsets)-1)
#no of rows that should be trained,if u want to change the no of rows to be changed please change here.if u want to test for the data in nth row,train till n-1th row
rows_to_be_trained <- 71797
#test data's question posted time which will be used to predict the response time
#please change the value here.if u want to change the question ,see the row in the dataset and copy its timequestionposted(C Column) which is in epoch format.
test_data_xvalue <- 1243257819

for(j in 1:rows_to_be_trained)
{
  u <- c()
    record <- data[j,]
    tags <- toString(record$Tags)
    alltags <- unlist(strsplit(tags,';'))
      q <- which(testTags %in% alltags)
      a <- which((alltags[! tolower(alltags) %in% tolower(testTags)]) %in% tolower(language_list))
      
  if(length(q)!=0 && length(a)==0)
    {   
      for(h in 1:length(q))
        {
        u[h] <- testTags[q[h]]
        }
        index <- which(subsets %in% list(u))
        if(nchar(data$BestAnswerTime[j] - data$TimeQuestionposted[j])<5)
        {
          variable[[index-1]] <- append(variable[[index-1]],data$TimeQuestionposted[j])
          variable[[index-1+(length(subsets)-1)]] <- append(variable[[index-1+(length(subsets)-1)]],data$BestAnswerTime[j] - data$TimeQuestionposted[j])
          
        }
        
    }
      remove(u)
}
result <- c()
p <- length(variable)/2
# linear regression is performed for each of the subsets generated
for(k in 1:p)
{
  x <- variable[[k]]
  y <- variable[[k+(length(variable)%/%2)]]
  fit <- lm(y~x)
  result[k] <- predict(fit,data.frame(x = test_data_xvalue))
}

# expected time value is predicted furthermore by using weighted average method after using regression.

expected <- result[length(result)]
for(p in 1:length(result))
{
  if(result[p] < result[length(result)])
  {
    expected <- expected - (length(variable[[p]]) * result[p] / rows_to_be_trained )
  }
}
print(paste('Expected time in seconds for the answer is between: ',"<",expected-900,expected+900,">"))
