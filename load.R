setwd("~/SISL/")

# == Reading data ==
mturk_data <- read.csv('data/fromdb/dumps/dump20150208/output_response.csv', header=T)
trial_list <- read.csv('data/fromdb/dumps/dump20150208/trial_list_event.csv')

# == Filtering out some bad workers. ==
# You can ignore that.

# scenario 8
mturk_data <- mturk_data[which(mturk_data$user_token %in% 
                                 c('A12EU7P50BDSWH-3WEV0KO0ON112NY9B26NFQ87F2ZSD8',
                                   'A19AOR3M6FZ0CZ-3HVVDCPGTF15FY6FWYYR2GEU0EMYTP',
                                   'A1NLJ1L4VCQYV2-3QRYMNZ7FZQB3Z21PWPGL0GNWUWNT2',
                                   'A1ODGBU73JE8S-3IUZPWIU1PG2NALSNO2BGS6I5OSWK4',
                                   'A2A9JD9UV8DQZX-3WEV0KO0ON112NY9B26NFQ87FN4SDJ',
                                   'A2W5RJFHZRE8IS-3LBXNTKX0S4XVE7RTW78S26941NX9G',
                                   'A37I58C4258IPX-33FBRBDW6P89EBDMX857ETQOI7CC8M',
                                   'A38DC3BG1ZCVZ2-39U1BHVTDM04ZV9R7D51CMIHQ8O3TA',
                                   'A3D2U4QF7821ZW-3WMINLGALCCNXZ61U6PLAKWJ91IAC7',
                                   'A3GOKNZGZGHL45-3PXX5PX6LY74NUEVG6TQZ0Q1RGIAB0',
                                   'AD1WGUMVD6KED-3ATTHHXXWBXQ6F0UXQDL5V5CNS9IXL',
                                   'AGVUHOBSCP6YC-32ZKVD547GW439FA7REGCSCVK1OB3S',
                                   'ALMX1BC2N51RV-3ZPBJO59KQACCEKT08UOBC157WJHDO',
                                   'ARYUDFK7TWXMW-31HQ4X3T3TJK0ZM1OJ4BY3GCOB1LSH')),]

# scenario 13
mturk_data <- mturk_data[which(!(mturk_data$user_token %in% 
                                 c('A12EU7P50BDSWH-3WEV0KO0ON112NY9B26NFQ87F2ZSD8',
                                   'A19AOR3M6FZ0CZ-3HVVDCPGTF15FY6FWYYR2GEU0EMYTP',
                                   'A1NLJ1L4VCQYV2-3QRYMNZ7FZQB3Z21PWPGL0GNWUWNT2',
                                   'A1ODGBU73JE8S-3IUZPWIU1PG2NALSNO2BGS6I5OSWK4',
                                   'A2A9JD9UV8DQZX-3WEV0KO0ON112NY9B26NFQ87FN4SDJ',
                                   'A2W5RJFHZRE8IS-3LBXNTKX0S4XVE7RTW78S26941NX9G',
                                   'A37I58C4258IPX-33FBRBDW6P89EBDMX857ETQOI7CC8M',
                                   'A38DC3BG1ZCVZ2-39U1BHVTDM04ZV9R7D51CMIHQ8O3TA',
                                   'A3D2U4QF7821ZW-3WMINLGALCCNXZ61U6PLAKWJ91IAC7',
                                   'A3GOKNZGZGHL45-3PXX5PX6LY74NUEVG6TQZ0Q1RGIAB0',
                                   'AD1WGUMVD6KED-3ATTHHXXWBXQ6F0UXQDL5V5CNS9IXL',
                                   'AGVUHOBSCP6YC-32ZKVD547GW439FA7REGCSCVK1OB3S',
                                   'ALMX1BC2N51RV-3ZPBJO59KQACCEKT08UOBC157WJHDO',
                                   'ARYUDFK7TWXMW-31HQ4X3T3TJK0ZM1OJ4BY3GCOB1LSH'))),]

# scenario 14 - 22
mturk_data <- subset(mturk_data, !(user_token %in% 
                                 c('A1T6EY2JB6S5ZZ-3E7TUJ2EGDVJX82A2W6WYV3HQB8D9D',
                                   'A2M5VW97GIYLHB-31LM9EDVOM1HPW6WD2BCJUTE0KMNJ0'
                                 )))

# == Loading parameters ===

# Learning/ Null 4-cues
learning_scenari <- c(14, 15, 16, 17)
null_scenari <- c(18, 19, 20 , 21, 22)
seqlen <- 12
num_cues <- 3564
block_structure <- c(rep(0, 2 * seqlen), sort(rep(1:6, 480)), rep(7, 540), rep(8,  seqlen * 10))

# Learning 6-cues
learning_scenari <- c(13)
seqlen <- 30
num_cues <- 4500
block_structure <- c(rep(0, 30), sort(rep(1:8, 540)), rep(9, seqlen * 5))

# == Computing logs  ==
mturk_data.learning <- subset(mturk_data, scenario_id %in% learning_scenari)
mturk_data.null <- subset(mturk_data, scenario_id %in% null_scenari)
mturk_data.learning.trial_log <- get_trial_log(mturk_data.learning, trial_list, block_structure, 2000, num_cues)
mturk_data.null.trial_log <- get_trial_log(mturk_data.null, trial_list, block_structure, 2000, num_cues)

# == Check sanity ==

# aggregate(speed ~ ppt, mturk_data.trial_log, function(x) min(x))

# == Select log for stats ==

mturk_data.trial_log <- mturk_data.learning.trial_log
mturk_data.trial_log <- mturk_data.null.trial_log



