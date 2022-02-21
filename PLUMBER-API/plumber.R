
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#
#AUTHOR: Carlo R. M.A. Santagiustina
#MAIL: carlo.santagiustina@unive.it
#API VERISON: 0.1
#PROJECT: ISEED-AEA
#* @apiTitle  ISEED Argument Extractor & Aggregator
#* @apiContact carlo.santagiustina@unive.it
#* @apiVersion 0.1
#* @apiDescription This is a OpenAPI for the [ISEED project](https://iseedeurope.eu/). It can be used to test the functions developed for ISEED's argument extractor and aggregator (ISEED-AEA). Implemented functionalities include the extractions of cause-effect and if-then relations from strings of text.
#####################################
#### INSTALL AND LOAD LIBRARIES #####
#####################################
install_and_load <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
   sapply(pkg, require, character.only = TRUE)
}


packages <- c("plumber","dplyr","promises","future")
install_and_load (packages)
future::plan("multisession")

############################################
#### GENERIC PATTERN MATCHING FUNCTIONS ####
############################################

#### Sentence tokenizer ####

RegEx_match_naif=function(x){
   paste(x,sep="|",collapse = "|"
   )
}


RegEx_match=function(x){
   paste(
      paste("[^\\pL\\pM_-]",
            x,
            "[^\\pL\\pM_-]",
            sep = ""),
      paste("^",
            x,
            "[^\\pL\\pM_-]",
            sep = ""),
      paste("[^\\pL\\pM_-]",
            x,
            "$",
            sep = ""),
      paste("^",
            x,
            "$",
            sep = ""),
      sep="|",collapse = "|"
   )
}
RegEx_match_start_middle=function(x){
   paste(
      paste("[^\\pL\\pM_-]",
            x,
            "[^\\pL\\pM_-]",
            sep = ""),
      paste("^",
            x,
            "[^\\pL\\pM_-]",
            sep = ""),
      sep="|",collapse = "|"
   )
}

RegEx_match_middle_end=function(x){
   paste(
      paste("[^\\pL\\pM_-]",
            x,
            "[^\\pL\\pM_-]",
            sep = ""),
      paste("[^\\pL\\pM_-]",
            x,
            "$",
            sep = ""),
      sep="|",collapse = "|"
   )
}

RegEx_match_end=function(x){
   paste(
      paste("[^\\pL\\pM_-]",
            x,
            "$",
            sep = ""),
      paste("^",
            x,
            "$",
            sep = "")
      ,sep="|",collapse = "|"
   )
}


RegEx_match_start=function(x){
   paste(
      paste("^",
            x,
            "[^\\pL\\pM_-]",
            sep = ""),
      paste("^",
            x,
            "$",
            sep = ""),
      sep="|",collapse = "|"
   )
}

#### Regex Extracting ####

RegEx=function(x){
   paste(
      paste("(?<![\\pL\\pM_-])(",
            x,
            ")(?![\\pL\\pM_-])",
            sep = ""),sep="|",collapse = "|"
   )
} 
RegEx_not_by=function(x){
   paste(
      paste("(?<![\\pL\\pM_-])(",
            x,
            ")(?![[:space:]]{1}by[[:space:]]{1})",
            sep = ""),sep="|",collapse = "|"
   )
} 

RegEx_end=function(x){
   paste(
      paste("(?<![\\pL\\pM_-])(",
            x,
            ")$",
            sep = ""),sep="|",collapse = "|"
   )
} 
RegEx_start=function(x){
   paste(
      paste("^(",
            x,
            ")(?![\\pL\\pM_-])",
            sep = ""),sep="|",collapse = "|"
   )
}


#####################################
#####  VERBS LEXICONS AND REGEX #####
#####################################

####CAUSATION####

list_cause_verbs = c(
   "cause",
   "causes",
   "causing",
   "caused",
   "determine",
   "determines",
   "determining",
   "determined",
   "engender",
   "engenders",
   "engendered",
   "engendering",
   "entail",
   "entails",
   "entailed",
   "entailing",
   "provoke",
   "provokes",
   "provoked",
   "provoking",
   "trigger",
   "triggers",
   "triggered",
   "triggering"
)#"determine","determines","determining","determined" should they be included?
other_causal_operators = c(
   "leads to",
   "lead to",
   "led to",
   "leading to",
   "reason of",
   "result in",
   "results in",
   "resulted in",
   "resulting in"
)
#"relate to", "relates to", "related to", "relating to" not considered because direction ambiguous hence not causal

other_passive_causal_operators = c(
   "due to",
   "led by",
   "result from",
   "results from",
   "resulted from",
   "resulting from",
   "stem from",
   "stems from",
   "stemmed from",
   "stemming from",
   "derive from",
   "derives from",
   "derived from",
   "deriving from",
   "originate from",
   "originates from",
   "originated from",
   "originating from"
)#what about "explained by" ???

list_passive_cause_verbs = c(paste(list_cause_verbs[grepl("ed$", list_cause_verbs)], "by", sep = " "),
                             paste(list_cause_verbs[grepl("e$", list_cause_verbs)], "was", sep = " "))

cause_verbs_regex = RegEx(c(list_cause_verbs, other_causal_operators))

passive_cause_verbs_regex = RegEx(c(list_passive_cause_verbs, other_passive_causal_operators))

cause_verbs_regex_match = RegEx_match(c(list_cause_verbs, other_causal_operators))

passive_cause_verbs_regex_match = RegEx_match(c(list_passive_cause_verbs, other_passive_causal_operators))


cause_verbs_regex_match_all=RegEx_match(c(list_cause_verbs, other_causal_operators, other_passive_causal_operators))

####CREATION/GENERATION####
#"produces","producing","produced" "produce" is potentially ambiguous. Example: "30-40% of our produce comes from the EU" otherwise could remove sentences that contain: our produce, their produce...
#all creation/generation verbs
list_creation_verbs = c(
   "create",
   "creates",
   "creating",
   "created",
   "produce",
   "produces",
   "producing",
   "produced",
   "originate",
   "originates",
   "originating",
   "originated",
   "generate",
   "generates",
   "generated",
   "generating",
   "make",
   "makes",
   "making",
   "made"
)
list_passive_creation_verbs = c(paste(list_creation_verbs[grepl("ed$", list_creation_verbs)], "by", sep = " "), "made by")
creation_verbs_regex = RegEx(list_creation_verbs)

passive_creation_verbs_regex = RegEx(list_passive_creation_verbs)


creation_verbs_regex_match = RegEx_match(list_creation_verbs)

passive_creation_verbs_regex_match = RegEx_match(list_passive_creation_verbs)

creation_verbs_regex_match_all = RegEx_match(list_creation_verbs)


####DESTRUCTION####


list_destruction_verbs = c(
   "destroy",
   "destroys",
   "destroyed",
   "destroying",
   "demolish",
   "demolishes",
   "demolished",
   "demolishing",
   "annihilate",
   "annihilates",
   "annihilated",
   "annihilating",
   "obliterate",
   "obliterates",
   "obliterated",
   "obliterating",
   "cancel",
   "cancels",
   "cancelled",
   "canceled",
   "cancelling",
   "canceling",
   "eradicate",
   "eradicates",
   "eradicated",
   "eradicating",
   "extirpate",
   "extirpates",
   "extirpated",
   "extirpating",
   "eliminate",
   "eliminates",
   "eliminated",
   "eliminating",
   "wipe out",
   "wipes out",
   "wiped out",
   "wiping out",
   "wipe off",
   "wipes off",
   "wiped off",
   "wiping off",
   "kill",
   "kills",
   "killed",
   "killing"
)

list_passive_destruction_verbs = c(paste(list_destruction_verbs[grepl("ed |ed$", list_destruction_verbs)], "by", sep = " "))
destruction_verbs_regex = RegEx(list_destruction_verbs)
passive_destruction_verbs_regex = RegEx(list_passive_destruction_verbs )

destruction_verbs_regex_match = RegEx_match(list_destruction_verbs)
passive_destruction_verbs_regex_match = RegEx_match(list_passive_destruction_verbs )

destruction_verbs_regex_match_all = RegEx_match(list_destruction_verbs)


##################################
#####  MODALS (also negated) #####
##################################

# MODALS + OTHER NEGATORS IN (IS, HAVE TO) VERBAL FORMS (present continuous, present perfect, future, future perfect, past perfect,future continuous)
negators_modals_verbs = c(
   #HERE FOLLOW NEGATIONS OF THE VERB HAVE TO
   "had not had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "do not have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "does not have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "not having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "did not have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "were not having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "will not have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "have not had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "has not had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "have not been having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "had not been having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "will not have had to",
   #related to volition/responsability (should be considered a negator of relation?),
   "would not have to",
   #related to volition/responsability (should be considered a negator of relation?),
   "would not have had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "don't have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "doesn't have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "aren't having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "isn't having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "didn't have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "wasn't having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "weren't having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "won't have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "haven't had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "hasn't had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "haven't been having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "hadn't had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "hadn't been having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "won't have had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "wouldn't have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "wouldn't have had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "shouldn't have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "shouldn't have had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "should not have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "should not have had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "may not have to",
   #related to volition/responsability (should be considered a negator of relation?)
   "may not have had to",
   #related to volition/responsability (should be considered a negator of relation?)
   "not having to",
   #related to volition/responsability (should be considered a negator of relation?)
   "have not",
   #OK
   "haven't",
   #OK
   "had not",
   #OK
   "hadn't",
   "hasn't",
   #OK
   #HERE FOLLOW NEGATIONS OF THE VERB IS
   "isn't",
   #OK
   "aren't",
   #OK
   "is not",
   #OK
   "am not",
   #OK
   "are not",
   #OK
   "was not",
   "wasn't",
   #OK
   "were not",
   #OK
   "weren't",
   #OK
   "will not",
   #OK
   "won't",
   "wont",#frequently used in twitter even if incorrect spelling
   #OTHER
   "doesn't",
   "don't",
   "dont", #frequently used in twitter even if incorrect spelling
   "did't",
   "wouldn't",
   "shan't" ,
   "shouldn't",
   #OK
   # HERE FOLLOW NEGATED MODALS
   "could not",
   #OK
   "couldn't",
   #OK
   "would not",
   #OK
   "wouldn't",
   #OK
   "shouldn't",
   #OK
   "should not",
   #OK
   "may not",
   #OK
   "can't",
   #OK
   "cannot",
   "couldn't" ,
   #OK
   "might not",
   #OK
   "mustn't",
   #OK
   "must not",
   #OK
   "shall not",
   #OK
   "ought not to",
   #OK
   "not",#NEGATION WITH NOT AS LAST ENTRY
   "other than"
)

#MATCHES END OF SENTENCE
negators_modals_verbs_regex = RegEx_end(negators_modals_verbs)

#MATCHES EVERYWHERE
negators_modals_verbs_regex_everywhere = RegEx(negators_modals_verbs)

#-----------------------------------------------------------------#
#### NON NEGATED MODALS + NON NEGATED IS + NON NEGATED HAVE TO ####
#----------------------------------------------------------------#

modals_verbs = c(
   #HERE FOLLOW VERB HAVE TO
   "had ([a-z]{4,} ){1}had to",
   #related to volition/responsability 
   "do ([a-z]{4,} ){1}have to",
   #related to volition/responsability
   "does ([a-z]{4,} ){1}have to",
   #related to volition/responsability
   "having ([a-z]{4,} ){1}to",
   #related to volition/responsability 
   "did ([a-z]{4,} ){1}have to",
   #related to volition/responsability 
   "were ([a-z]{4,} ){1}having to",
   #related to volition/responsability
   "will ([a-z]{4,} ){1}have to",
   #related to volition/responsability 
   "have ([a-z]{4,} ){1}had to",
   #related to volition/responsability 
   "has ([a-z]{4,} ){1}had to",
   #related to volition/responsability 
   "have ([a-z]{4,} ){1}been having to",
   #related to volition/responsability
   "had ([a-z]{4,} ){1}been having to",
   #related to volition/responsability 
   "will ([a-z]{4,} ){1}have had to",
   #related to volition/responsability 
   "would ([a-z]{4,} ){1}have to",
   #related to volition/responsability
   "would ([a-z]{4,} ){1}have had to",
   #related to volition/responsability
   "do ([a-z]{4,} ){1}have to",
   #related to volition/responsability 
   "does ([a-z]{4,} ){1}have to",
   #related to volition/responsability
   "are ([a-z]{4,} ){1}having to",
   #related to volition/responsability
   "is ([a-z]{4,} ){1}having to",
   #related to volition/responsability 
   "did ([a-z]{4,} ){1}have to",
   #related to volition/responsability 
   "was ([a-z]{4,} ){1}having to",
   #related to volition/responsability
   "were ([a-z]{4,} ){1}having to",
   #related to volition/responsability 
   "ought ([a-z]{4,} ){1}to",
   ######RECENTLY ADDED####
   #HERE FOLLOW VERB HAVE TO
   "had had to",
   #related to volition/responsability 
   "do have to",
   #related to volition/responsability
   "does have to",
   #related to volition/responsability
   "having to",
   #related to volition/responsability 
   "did have to",
   #related to volition/responsability 
   "were having to",
   #related to volition/responsability
   "will have to",
   #related to volition/responsability 
   "have had to",
   #related to volition/responsability 
   "has had to",
   #related to volition/responsability 
   "have been having to",
   #related to volition/responsability
   "had been having to",
   #related to volition/responsability 
   "will have had to",
   #related to volition/responsability 
   "would have to",
   #related to volition/responsability
   "would have had to",
   #related to volition/responsability
   "do have to",
   #related to volition/responsability 
   "does have to",
   #related to volition/responsability
   "are having to",
   #related to volition/responsability
   "is having to",
   #related to volition/responsability 
   "did have to",
   #related to volition/responsability 
   "was having to",
   #related to volition/responsability
   "were having to",
   #related to volition/responsability 
   "have",
   #OK
   "had",
   "has",
   #OK
   #HERE FOLLOW  VERB IS
   "is",
   #OK
   "are",
   #OK
   "am",
   #OK
   "was",
   #OK
   "were",
   #OK
   "will",
   #OK
   # HERE FOLLOW MODALS
   "could",
   #OK
   "would",
   #OK
   "should",
   #OK
   "may",
   #OK
   "can",
   #OK
   "might",
   #OK
   "must",
   #OK
   "shall",
   #OK
   "ought to"#OK
)



#MATCHES END OF SENTENCE
modals_verbs_regex = RegEx_end(modals_verbs)

modals_verbs_regex_match=RegEx_match_end(modals_verbs)

#MATCHES EVERYWHERE
modals_verbs_regex_everywhere = RegEx(modals_verbs)
modals_verbs_regex_everywhere_match=RegEx_match(modals_verbs)

#source('functions/passive_form_end_sentence.R')

###################################
#####  Group matching functions####
###################################

regcapturedmatches<-function(x,m) {
   
   if (length(x) != length(m))
      stop(gettextf("%s and %s must have the same length",
                    sQuote("x"), sQuote("m")), domain = NA)
   
   ili <- is.list(m)
   useBytes <- if (ili) {
      any(unlist(lapply(m, attr, "useBytes")))
   } else {
      any(attr(m, "useBytes"))
   }
   if (useBytes) {
      asc <- iconv(x, "latin1", "ASCII")# conversion needed if strange encoding or accents are present in the text (shouldn't be so if only english is used)
      ind <- is.na(asc) | (asc != x)
      if (any(ind))
         Encoding(x[ind]) <- "bytes"
   }
   if (ili) {
      if (any(sapply(m, function(x) {is.null(attr(x,"capture.start"))})==T)) {
         stop("No capture data found (did you use perl=T?)")
      }
      starts<-lapply(m, function(x) {attr(x, "capture.start")})
      lengths<-lapply(m, function(x) {attr(x, "capture.length")})
   } else {
      if (is.null(attr(m,"capture.start"))) {
         stop("No capture data found (did you use perl=T?)")
      }
      x<-list(x)
      starts<-list(attr(m, "capture.start"))
      lengths<-list(attr(m, "capture.length"))
   }
   
   cleannames <- function(x) {
      if (!is.null(colnames(x))) {
         colnames(x) <- make.unique(make.names(colnames(x)))
      }
      x
   }
   starts <- lapply(starts, cleannames)
   lengths <- lapply(lengths, cleannames)
   
   
   Substring<-function(x,starts,lens) {
      if(all(starts<0)) {
         return(character())
      } else {
         x <- t(
            mapply(function(x,st,ln) substring(x,st,st+ln-1), 
                   x, data.frame(t(starts)), data.frame(t(lens)),
                   USE.NAMES=F)
         )
         if (!is.null(colnames(starts))) {
            colnames(x)<-colnames(starts)
         }
         x
      }
   }
   
   y<-Map(
      function(x, sos, mls) {
         Substring(x,sos,mls)
      },
      x,
      starts,
      lengths,
      USE.NAMES = FALSE
   )
   if (ili) {
      y
   } else {
      y[[1]]
   }
}

#########################################
##### CONSTRUCTIONS (miscellaneous) #####
#########################################

and_regex="([[:punct:][:space:]]){1,}(and also|but also|also|while|as well as|but([[:punct:][:space:]]){1,}meanwhile|and([[:punct:][:space:]]){1,}meanwhile|meanwhile|but([[:punct:][:space:]]){1,}at the same time|and([[:punct:][:space:]]){1,}at the same time|at the same time|and([[:punct:][:space:]]){1,}simultaneously|but([[:punct:][:space:]]){1,}simultaneously|simultaneously|and([[:punct:][:space:]]){1,}concurrently|but([[:punct:][:space:]]){1,}concurrently|concurrently|and([[:punct:][:space:]]){1,}jointly|but([[:punct:][:space:]]){1,}jointly|jointly|but|and){1,}([[:punct:][:space:]]){0,}(I|you|he|she|it|we|you|they){0,}([[:punct:][:space:]]){0,}$"

####SLOWER FOR EXTRACTING ####
passive_pattern = paste(passive_creation_verbs_regex,passive_destruction_verbs_regex,passive_cause_verbs_regex ,sep = "|")
patterns = paste(creation_verbs_regex,destruction_verbs_regex,cause_verbs_regex,sep = "|")

all_at_end=paste(
   c(list_creation_verbs,list_destruction_verbs,list_cause_verbs),
   collapse  = "|"
)

that_passive_end_regex = paste("(?<effect>[^:;,!]{2,})(?<connector> that | which |[,]{1}which ){1}(?<cause>[^:;,!]{2,})(?<rel_operator>",all_at_end,"){1}(?<effect_end> in [^:;,!]{2,}| on [^:;,!]{2,}| since [^:;,!]{2,}| at [^:;,!]{2,}| during [^:;,!]{2,}| to [^:;,!]{2,}| for [^:;,!]{2,}| while [^:;,!]{2,}| throughout [^:;,!]{2,}| all-over [^:;,!]{2,}| all over [^:;,!]{2,}| inside [^:;,!]{2,}|  outside [^:;,!]{2,}| everywhere [^:;,!]{2,}){0,}[[:punct:]]{0,}",sep = "")  #check list of prepositions of place and time

####FASTER FOR MATCHING ####
passive_pattern_match=  paste(passive_creation_verbs_regex_match,passive_destruction_verbs_regex_match,passive_cause_verbs_regex_match,sep = "|")
pattern_match=paste(creation_verbs_regex_match_all,destruction_verbs_regex_match_all,cause_verbs_regex_match_all,sep = "|")
pattern_match_all=paste(creation_verbs_regex_match_all,destruction_verbs_regex_match_all,cause_verbs_regex_match_all,sep = "|")

######################
####API FUNCTIONS ####
######################

#* @if-then relations extraction from single sentence
#* Extracts if-then relations from a sentence using a set of regular expressions, no NLP tool is employed so the function is not aware of Part-Of-Speech or Dependency-Relations among words. This function was designed to be used with short texts from social media. The text string must be a single sentence and must have been already pre-processed to remove non latin-1 characters like emojis, as well as URLs. Remember to set the correct language parameter ("en" by default).
#* @param text:str a character string containing a single sentence
#* @param lang:str the language ISO 639-1 (2-character) code of the language to be used. Currently available languages: English="en", Italian="it",  French="fr", German="de"  and Spanish="es"  
#*  @param ignore.case:bool  ignore case in RegEx
#* @get /get_if_then_string
#* @serializer unboxedJSON
if_then_extractor_sentence=function(text, 
                                    lang="en",
                                    perl=T,
                                    ignore.case = T){

   promises::future_promise({
      
      
   if(lang=="en"){
      if_then_pattern="(?<=^if |[[:space:][:punct:]]if )\\K([[:word:][:space:]'-]){1,}\\K([,] ){0,}\\K(then )(?=([[:word:]]{1,}){1,})"
      then_pattern=RegEx(c("if","then"))
      If_=c("if","IF","If")
      Then_=c("then","THEN","Then")
   }
   if(lang=="it"){
      if_then_pattern="(?<=^se |[[:space:][:punct:]]se )\\K([\\pL\\pM[:space:]'-]){1,}\\K([,] ){0,}\\K(allora )(?=([\\pL\\pM]{1,}){1,})"
      then_pattern=RegEx(c("se","allora"))
      If_=c("se","SE","Se")
      Then_=c("allora","ALLORA","Allora")
   } 
      if(lang=="fr"){
         if_then_pattern="(?<=^si |[[:space:][:punct:]]si )\\K([\\pL\\pM[:space:]'-]){1,}\\K([,] ){0,}\\K(alors )(?=([\\pL\\pM]{1,}){1,})"
         then_pattern=RegEx(c("si","alors"))
         If_=c("si","SI","Si")
         Then_=c("alors","ALLORS","Alors")
      }
      
      if(lang=="de"){
         if_then_pattern="(?<=^wenn |[[:space:][:punct:]]wenn )\\K([\\pL\\pM[:space:]'-]){1,}\\K([,] ){0,}\\K(dann )(?=([\\pL\\pM]{1,}){1,})"
         then_pattern=RegEx(c("wenn","dann"))
         If_=c("wenn","WENN","Wenn")
         Then_=c("dann","DANN","Dann")
      }
      
      if(lang=="es"){
         if_then_pattern="(?<=^si |[[:space:][:punct:]]si )\\K([[:word:][:space:]'-]){1,}\\K([,] ){0,}\\K(entonces )(?=([[:word:]]{1,}){1,})"
         then_pattern=RegEx(c("si","entonces"))
         If_=c("si","SI","Si")
         Then_=c("entonces","ENTONCES","Entonces")
      }
      
   x=list(text)
   
   has_operator = grepl(pattern = if_then_pattern, x, perl=T,ignore.case = ignore.case)
   operators=list()
   are_if_then=list()
   splits=list()
   IFs=list()
   THENs=list()
   positions=list()
   causal_data=list()
   #### FOR ALL SENTENCES THAT MATCHED THE IF_THEN REGEX  ####
   for (i in which(has_operator)) {
      if(grepl(pattern = if_then_pattern,
               x[[i]],
               perl = perl,
               ignore.case = ignore.case)) {
         positions[[i]] = gregexpr(
            pattern = then_pattern,
            x[[i]],
            perl = perl,
            ignore.case = ignore.case
         )
         operators[[i]]	= tolower(regmatches(x[[i]],
                                             positions[[i]])[[1]])
         IFs[[i]]= which(operators[[i]] %in% If_)
         THENs[[i]]= which(operators[[i]] %in% Then_)
         
         ##ONLY ONE IF THEN RELATION
         if(length(IFs[[i]])==length(THENs[[i]])  & length(IFs[[i]])==1  && (IFs[[i]]+1)==THENs[[i]]){
            splits[[i]] = regmatches(x[[i]], positions[[i]],
                                     invert = T)[[1]][-1]
            splits[[i]]=gsub("^([[:punct:][:space:]]){1,}|([[:punct:][:space:]]){1,}$","",splits[[i]])
            causal_data[[i]]=data.frame(IF=splits[[i]][IFs[[i]]], THEN=splits[[i]][THENs[[i]]], SENTENCE=x[[i]], ID=i, stringsAsFactors	= F)
            
         }
         ##MULTIPLE IF THEN RELATIONS (CORRECTLY FORMED)
         if(length(IFs[[i]])==length(THENs[[i]]) & identical(IFs[[i]]+1,THENs[[i]])){
            splits[[i]] = regmatches(x[[i]], positions[[i]],
                                     invert = T)[[1]][-1]
            splits[[i]]=gsub("^([[:punct:][:space:]]){1,}|([[:punct:][:space:]]){1,}$","",splits[[i]])
            #splits[[i]]=gsub("^(and )|( and)$","",splits[[i]])
            causal_data[[i]]=data.frame(IF=splits[[i]][IFs[[i]]], THEN=splits[[i]][THENs[[i]]], SENTENCE=x[[i]], ID=i, stringsAsFactors	= F)
         }
         ##MULTIPLE IF THEN RELATIONS (ADVANCED PAIRING)
         if(length(IFs[[i]])!=length(THENs[[i]]) | !identical(IFs[[i]]+1,THENs[[i]])){
            IFs[[i]]=intersect(IFs[[i]]+1,THENs[[i]])-1
            THENs[[i]]=intersect(IFs[[i]]+1,THENs[[i]])
            splits[[i]] = regmatches(x[[i]], positions[[i]],
                                     invert = T)[[1]][-1]
            splits[[i]]=gsub("^([[:punct:][:space:]]){1,}|([[:punct:][:space:]]){1,}$","",splits[[i]])
            #splits[[i]]=gsub("^(and )|( and)$","",splits[[i]])
            causal_data[[i]]=data.frame(IF=splits[[i]][IFs[[i]]], THEN=splits[[i]][THENs[[i]]], SENTENCE=x[[i]], stringsAsFactors	= F)
         }
      }
   }
   if(exists("causal_data")){
      return(dplyr::bind_rows(causal_data))
   }
   })
}


#* @causal relations extraction from single sentence.
#* Extracts cause-effect relations from a sentence using a set of regular expressions, no NLP tool is employed so the function is not aware of Part-Of-Speech or Dependency-Relations among words. This function was designed to be used with short texts from social media.
#* @param text:str a character string a vector of strings or a list of strings
#*  @param consider_passive:bool  consider passive form verbs
#*  @param consider_end_form:bool  consider end of sentence verbs
#* @get /get_cause_effect_string
#* @serializer unboxedJSON


relation_extractor = function(text,
                              consider_passive = T,
                              consider_end_form = T) {
   
   promises::future_promise({ 
   y=text
   all_at_end_=all_at_end
   pattern_ = patterns
   passive_pattern_ = passive_pattern
   pattern_match_ = pattern_match
   pattern_match_all_ = pattern_match_all
   that_passive_end_regex_ = that_passive_end_regex
   creation_verbs_regex_match_all_= creation_verbs_regex_match_all
   destruction_verbs_regex_match_all_=destruction_verbs_regex_match_all
   cause_verbs_regex_match_all_=cause_verbs_regex_match_all
   
   if(is.character(y) && !is.list(y)&& length(y)>1){
      # y=as.list(y)  
      y= quanteda::tokens(y, what = "sentence")
      names(y)=paste(names(y),".",sep = "")
      y= as.list(unlist(quanteda::tokens(y, what = "sentence"),use.names = T,recursive = F))
      names=names(y)
   }
   
   if(is.character(y) && !is.list(y) && length(y)==1){
      names=names(y)
      y=  as.list(unlist(quanteda::tokens(y, what = "sentence"),use.names = F,recursive = F))
      names(y)= paste(names,".",1:length(y),sep = "")
      names=names(y)
   }
   
   has_operator	= grepl(pattern_match_,
                        y,
                        perl = T,
                        ignore.case = T)
   
   # has_operator= rep(T, length(y))
   
   operators = list()
   are_if_then = list()
   are_passive = list()
   splits = list()
   removed_if = list()
   negation = list()
   and = list()
   n_relations = list()
   before = list()
   after = list()
   causal_data = list()
   which = list()
   end_form = list()
   contains_end_form=list()
   are_end_form = list()
   contained_end_form = list()
   negated_end_form = list()
   
   for (i in which(has_operator)) {
      
      tryCatch({
         #### match if contains end of sentence form ####
         if(grepl(that_passive_end_regex_,
                  y[[i]],
                  perl = T,
                  ignore.case = T)){
            contains_end_form[[i]]=TRUE
            #identify and extract end form
            end_form[[i]]=gsub(paste("^([^,:!?;]){1,}([,:!?;][[:space:]]){1,}(?=.*( that | which |[,]{1}which ){1}.*(",all_at_end,"){1}.*)",sep = ""),"",y[[i]],perl = T,
                               ignore.case = T)
            #end form pruning
            end_form[[i]]=   gsub(paste("(.*( that | which |[,]{1}which ){1}.*(",all_at_end,"){1}.*)\\K([,:!?;][[:space:]]){1,}([^,:!?;]){1,}$",sep = ""),"",end_form[[i]], perl = T,
                                  ignore.case = T)
            #identify different parts of the end form
            are_end_form[[i]]	= gregexpr(
               that_passive_end_regex_,
               end_form[[i]],
               perl = T,
               ignore.case = T
            )
            #extract matches of the end form
            end_form[[i]]	= regcapturedmatches(end_form[[i]],are_end_form[[i]])[[1]]
            if(length(end_form[[i]])>0){
               #paste the effect_end to the effect
               end_form[[i]][,"effect"]= paste(end_form[[i]][,"effect"],end_form[[i]][,"effect_end"],sep = "")
               end_form[[i]]	= end_form[[i]][,c("cause","rel_operator","effect")]
               
               #creates relation metadata (is the relation negated?)
               negated_end_form[[i]] =  grepl(
                  negators_modals_verbs_regex,
                  end_form[[i]]["effect"],
                  ignore.case = T,
                  perl = T
               )
               #clean the effect side
               end_form[[i]]["effect"]= gsub(
                  negators_modals_verbs_regex,
                  "",
                  end_form[[i]]["effect"],
                  ignore.case = T,
                  perl = T
               )
               #clean the cause side
               end_form[[i]]["cause"]=gsub("[[:space:]]{1,}$",
                                           ""                       ,
                                           end_form[[i]]["cause"],
                                           ignore.case = T,
                                           perl = T)
               
               end_form[[i]]["cause"]=gsub(modals_verbs_regex,
                                           "",
                                           end_form[[i]]["cause"],
                                           ignore.case = T,
                                           perl = T)
               are_end_form[[i]] = data.frame(
                  id =  names[i],
                  cause = end_form[[i]]["cause"],
                  rel_negation = negated_end_form[[i]],
                  rel_operator = end_form[[i]]["rel_operator"],
                  rel_passive_form = FALSE,
                  rel_creation= grepl( creation_verbs_regex_match_all_, end_form[[i]]["rel_operator"], ignore.case=T,perl=T ),
                  rel_destruction= grepl(destruction_verbs_regex_match_all_, end_form[[i]]["rel_operator"], ignore.case=T,perl=T ) ,
                  rel_causation =  grepl(cause_verbs_regex_match_all_,end_form[[i]]["rel_operator"], ignore.case=T,perl=T ) ,
                  rel_coref_res = FALSE,
                  effect = end_form[[i]]["effect"],
                  #sentence = y[[i]],
                  stringsAsFactors	= F,check.names = F,fix.empty.names = F,row.names = NULL
               )
               #identify the sentence fragment in which the end_form appears
               end_form[[i]]= base::strsplit(y[[i]],"(?<=[;:,?!.])", perl = TRUE)[[1]]
               contained_end_form[[i]]= !grepl(that_passive_end_regex_,
                                               end_form[[i]],
                                               perl = T,
                                               ignore.case = T)
               
               if(length(end_form[[i]])>1 && sum(contained_end_form[[i]])>=1){
                  #if multiple fragments contained in sentence prune end_form fragment before next processing step
                  y[[i]]=  paste(end_form[[i]][contained_end_form[[i]]],sep = "",collapse = "")} else{y[[i]]=""}
            }else{
               contains_end_form[[i]]=FALSE
            }
         }else{
            contains_end_form[[i]]=FALSE
         }
         ####passive and normal forms ####
         
         if(grepl(pattern = "[^:;,!]{4,}",y[[i]],perl = T,  ignore.case = T)){
            #extract relation's operators
            operators[[i]]	= regmatches(y[[i]],
                                        gregexpr(
                                           paste(passive_pattern_, pattern_, sep = "|"),
                                           y[[i]],
                                           perl = T,
                                           ignore.case = T
                                        ))[[1]]
            #name the operators NORMAL as default
            names(operators[[i]]) = rep("NORMAL", length(operators[[i]]))
            
            #identify which ones are passive
            are_passive[[i]] = grepl(
               passive_pattern_,
               unlist(operators[[i]]),
               perl = T,
               ignore.case = T
            )
            #name these ones PASSIVE
            names(operators[[i]])[are_passive[[i]]] = "PASSIVE"
            
            #if there is at least one passive process string as follows
            if (length(operators[[i]][are_passive[[i]]]) > 0) {
               splits[[i]] = regmatches(
                  y[[i]],
                  gregexpr(
                     RegEx(c(operators[[i]][are_passive[[i]]], operators[[i]][!are_passive[[i]]])),
                     y[[i]],
                     perl = T,
                     ignore.case = T
                  ),
                  invert = T
               )[[1]]
               
            } else{ #if no passive process string as follows
               splits[[i]] = regmatches(
                  y[[i]],
                  gregexpr(
                     RegEx(operators[[i]]),
                     y[[i]],
                     perl = T,
                     ignore.case = T
                  ),
                  invert = T
               )[[1]]
            }
            
            
            
            #####BUILD PAIRS ####
            if (length(operators[[i]]) > 0) {
               splits[[i]] = gsub("^([[:punct:][:space:]]){1,}|([[:punct:][:space:]]){1,}$",
                                  "",
                                  splits[[i]])
               negation[[i]] = grepl(
                  negators_modals_verbs_regex,
                  splits[[i]],
                  ignore.case = T,
                  perl = T
               )
               splits[[i]] = gsub(
                  negators_modals_verbs_regex,
                  "",
                  splits[[i]],
                  ignore.case = T,
                  perl = T
               )
               splits[[i]] = gsub(modals_verbs_regex,
                                  "",
                                  splits[[i]],
                                  ignore.case = T,
                                  perl = T)
               and[[i]] = grepl(and_regex,
                                splits[[i]],
                                perl = T,
                                ignore.case = T)
               splits[[i]] = gsub(
                  and_regex,
                  "",
                  splits[[i]],
                  perl = T,
                  ignore.case = T
               )
               
               which[[i]] = grepl("which[[:space:]]{0,}$",
                                  splits[[i]],
                                  ignore.case = T,
                                  perl = T)
               splits[[i]] = gsub(
                  "which[[:space:]]{0,}$|that[[:space:]]{0,}$",
                  "",
                  splits[[i]],
                  perl = T,
                  ignore.case = T
               )
               splits[[i]] = gsub(
                  "^[[:space:][:punct:]]{0,}|[[:space:][:punct:]]{0,}$",
                  "",
                  splits[[i]],
                  perl = T,
                  ignore.case = T
               )
               before[[i]] = splits[[i]][1:length(operators[[i]])]
               after[[i]] = splits[[i]][2:(length(operators[[i]]) + 1)]
               n_relations[[i]] = length(splits[[i]]) - 1
               for (j in	1:n_relations[[i]]) {
                  k = j
                  if (!are_passive[[i]][j]) {
                     if (j == 2 && (and[[i]][j] | which[[i]][j - 1])) {
                        k = 1
                        before[[i]][j] = splits[[i]][k]
                        after[[i]][j] = splits[[i]][j + 1]
                     } else{
                        if (j > 2 && (and[[i]][j] | which[[i]][j - 1])) {
                           k = max(which(!and[[i]][1:j - 1] & !which[[i]][1:j - 1]))
                           before[[i]][j] = splits[[i]][k]
                           after[[i]][j] = splits[[i]][j + 1]
                        }
                     }
                  } else{
                     if (j == 2 && (and[[i]][j] | which[[i]][j - 1])) {
                        k = 1
                        after[[i]][j] = splits[[i]][k]
                        before[[i]][j] = splits[[i]][j + 1]
                     } else{
                        if (j > 2 && (which[[i]][j - 1])) {
                           k = max(which(!which[[i]][1:j - 1]))
                           after[[i]][j] = splits[[i]][k]
                           before[[i]][j] = splits[[i]][j + 1]
                        } else{
                           after[[i]][j] = splits[[i]][k]
                           before[[i]][j] = splits[[i]][k + 1]
                        }
                     }
                  }
               }
               
               causal_data[[i]] = data.frame(
                  id =  names[i],
                  cause = before[[i]][1:n_relations[[i]]],
                  rel_negation = negation[[i]][1:n_relations[[i]]],
                  rel_operator = operators[[i]][1:n_relations[[i]]],
                  rel_passive_form = are_passive[[i]][1:n_relations[[i]]],
                  rel_creation= grepl( creation_verbs_regex_match_all_, operators[[i]][1:n_relations[[i]]], ignore.case=T,perl=T ),
                  rel_destruction= grepl(destruction_verbs_regex_match_all_, operators[[i]][1:n_relations[[i]]], ignore.case=T,perl=T ) ,
                  rel_causation =  grepl(cause_verbs_regex_match_all_,operators[[i]][1:n_relations[[i]]], ignore.case=T,perl=T ) ,
                  rel_coref_res = and[[i]][1:n_relations[[i]]] | which[[i]][1:n_relations[[i]]],
                  effect = after[[i]][1:n_relations[[i]]],
                  #sentence = y[[i]],
                  stringsAsFactors	= F,check.names = F,fix.empty.names = F,row.names = NULL
               )
               if(contains_end_form[[i]]){
                  causal_data[[i]]= tryCatch(rbind(are_end_form[[i]],causal_data[[i]]))
               }
               
            }else{
               if(contains_end_form[[i]]){
                  causal_data[[i]]= tryCatch(are_end_form[[i]])
               }
            }
         }else{
            if(contains_end_form[[i]]){
               causal_data[[i]]= tryCatch(are_end_form[[i]])
            }
         }
         
      })
   }
   tryCatch(dplyr::bind_rows(causal_data))
   })
}
