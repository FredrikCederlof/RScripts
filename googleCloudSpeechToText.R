# Fredrik Cederlöf, Head of CX & Analytics Collector Bank
# https://www.linkedin.com/in/fredrikcederlof/

# Google Cloud Speech to Text API
# https://github.com/ropensci/googleLanguageR

library(googleLanguageR)
library(devtools)
install_github("ropensci/googleLanguageR")

# set configurations
my_config <- list(metadata = list(interactionType = "DICTATION"))

# load audio file
my_audio <- "/Users/fredrikcederlof/sound.wav"

# set encodings settings
my_config <- list(encoding = "LINEAR16", enableAutomaticPunctuation = TRUE, enableSpeakerDiarization = TRUE, diarizationSpeakerCount = 2)
result <- gl_speech(my_audio, sampleRateHertz = 48000L, languageCode = "sv-SE", customConfig = my_config)

# create new data frames
dfTimings <- do.call(rbind.data.frame, result$timings)
dfTranscript <- do.call(rbind.data.frame, result$transcript)

# View start and stop time for each word
View(dfTimings)

View(result$transcript)
