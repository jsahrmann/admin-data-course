# Front matter ------------------------------------------------------
#
# SP2022.M19.PHS.5254.01
# Using Administrative Data for Clinical and Health Services Research
#
# Demonstrate processing string variables and defining functions.
#
# John Sahrmann
# 20220217


# Preface -----------------------------------------------------------

library(tidyverse)

setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")



# Constant definitions -----------------------------------------------


# Diagnoses -----------------------

# Acute myocardial infarction
# I21.0-I21.4, I22.0-I22.2, I22.8, I22.9
dx10_acuteMI <- c(
  "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
  "I213", "I214", "I220", "I221", "I222", "I228", "I229"
)
# Ischemic Stroke
# I63.0-I63.6, I63.8, I63.9
dx10_ischemicStroke <- c(
  "I6300", "I63011", "I63012", "I63013", "I63019", "I6302", "I63031",
  "I63032", "I63033", "I63039", "I6309", "I6310", "I63111", "I63112",
  "I63113", "I63119", "I6312", "I63131", "I63132", "I63133", "I63139",
  "I6319", "I6320", "I63211", "I63212", "I63213", "I63219", "I6322",
  "I63231", "I63232", "I63233", "I63239", "I6329", "I6330", "I63311",
  "I63312", "I63313", "I63319", "I63321", "I63322", "I63323",
  "I63329", "I63331", "I63332", "I63333", "I63339", "I63341",
  "I63342", "I63343", "I63349", "I6339", "I6340", "I63411", "I63412",
  "I63413", "I63419", "I63421", "I63422", "I63423", "I63429",
  "I63431", "I63432", "I63433", "I63439", "I63441", "I63442",
  "I63443", "I63449", "I6349", "I6350", "I63511", "I63512", "I63513",
  "I63519", "I63521", "I63522", "I63523", "I63529", "I63531",
  "I63532", "I63533", "I63539", "I63541", "I63542", "I63543",
  "I63549", "I6359", "I636", "I638", "I6381", "I6389", "I639"
)


# Procedures ----------------------

# Coronary artery bypass graft
# 02100*, 021048*, 021049*, 02104A*, 02104J*, 02104K*, 02104Z*,
# 02110*, 021148*, 021149*, 02114A*, 02114J*, 02114K*, 02114Z*,
# 02120*, 021248*, 021249*, 02124A*, 02124J*, 02124K*, 02124Z*,
# 02130*, 021348*, 021349*, 02134A*, 02134J*, 02134K*, 02134Z*
pr10_cabg <- c(
  "0210083", "0210088", "0210089", "021008C", "021008F", "021008W",
  "0210093", "0210098", "0210099", "021009C", "021009F", "021009W",
  "02100A3", "02100A8", "02100A9", "02100AC", "02100AF", "02100AW",
  "02100J3", "02100J8", "02100J9", "02100JC", "02100JF", "02100JW",
  "02100K3", "02100K8", "02100K9", "02100KC", "02100KF", "02100KW",
  "02100Z3", "02100Z8", "02100Z9", "02100ZC", "02100ZF", "0210444",
  "0210483", "0210488", "0210489", "021048C", "021048F", "021048W",
  "0210493", "0210498", "0210499", "021049C", "021049F", "021049W",
  "02104A3", "02104A8", "02104A9", "02104AC", "02104AF", "02104AW",
  "02104D4", "02104J3", "02104J8", "02104J9", "02104JC", "02104JF",
  "02104JW", "02104K3", "02104K8", "02104K9", "02104KC", "02104KF",
  "02104KW", "02104Z3", "02104Z8", "02104Z9", "02104ZC", "02104ZF",
  "0211083", "0211088", "0211089", "021108C", "021108F", "021108W",
  "0211093", "0211098", "0211099", "021109C", "021109F", "021109W",
  "02110A3", "02110A8", "02110A9", "02110AC", "02110AF", "02110AW",
  "02110J3", "02110J8", "02110J9", "02110JC", "02110JF", "02110JW",
  "02110K3", "02110K8", "02110K9", "02110KC", "02110KF", "02110KW",
  "02110Z3", "02110Z8", "02110Z9", "02110ZC", "02110ZF", "0211344",
  "02113D4", "0211444", "0211483", "0211488", "0211489", "021148C",
  "021148F", "021148W", "0211493", "0211498", "0211499", "021149C",
  "021149F", "021149W", "02114A3", "02114A8", "02114A9", "02114AC",
  "02114AF", "02114AW", "02114D4", "02114J3", "02114J8", "02114J9",
  "02114JC", "02114JF", "02114JW", "02114K3", "02114K8", "02114K9",
  "02114KC", "02114KF", "02114KW", "02114Z3", "02114Z8", "02114Z9",
  "02114ZC", "02114ZF", "0212083", "0212088", "0212089", "021208C",
  "021208F", "021208W", "0212093", "0212098", "0212099", "021209C",
  "021209F", "021209W", "02120A3", "02120A8", "02120A9", "02120AC",
  "02120AF", "02120AW", "02120J3", "02120J8", "02120J9", "02120JC",
  "02120JF", "02120JW", "02120K3", "02120K8", "02120K9", "02120KC",
  "02120KF", "02120KW", "02120Z3", "02120Z8", "02120Z9", "02120ZC",
  "02120ZF", "0212344", "02123D4", "0212444", "0212483", "0212488",
  "0212489", "021248C", "021248F", "021248W", "0212493", "0212498",
  "0212499", "021249C", "021249F", "021249W", "02124A3", "02124A8",
  "02124A9", "02124AC", "02124AF", "02124AW", "02124D4", "02124J3",
  "02124J8", "02124J9", "02124JC", "02124JF", "02124JW", "02124K3",
  "02124K8", "02124K9", "02124KC", "02124KF", "02124KW", "02124Z3",
  "02124Z8", "02124Z9", "02124ZC", "02124ZF", "0213083", "0213088",
  "0213089", "021308C", "021308F", "021308W", "0213093", "0213098",
  "0213099", "021309C", "021309F", "021309W", "02130A3", "02130A8",
  "02130A9", "02130AC", "02130AF", "02130AW", "02130J3", "02130J8",
  "02130J9", "02130JC", "02130JF", "02130JW", "02130K3", "02130K8",
  "02130K9", "02130KC", "02130KF", "02130KW", "02130Z3", "02130Z8",
  "02130Z9", "02130ZC", "02130ZF", "0213344", "02133D4", "0213444",
  "0213483", "0213488", "0213489", "021348C", "021348F", "021348W",
  "0213493", "0213498", "0213499", "021349C", "021349F", "021349W",
  "02134A3", "02134A8", "02134A9", "02134AC", "02134AF", "02134AW",
  "02134D4", "02134J3", "02134J8", "02134J9", "02134JC", "02134JF",
  "02134JW", "02134K3", "02134K8", "02134K9", "02134KC", "02134KF",
  "02134KW", "02134Z3", "02134Z8", "02134Z9", "02134ZC", "02134ZF"
)
# Percutaneous coronary intervention
# 0270*, 0271*, 0272*, 0273*, 02C0*, 02C1*, 02C2*, 02C3*
pr10_pci <- c(
  "0270046", "027004Z", "0270056", "027005Z", "0270066", "027006Z",
  "0270076", "027007Z", "02700D6", "02700DZ", "02700E6", "02700EZ",
  "02700F6", "02700FZ", "02700G6", "02700GZ", "02700T6", "02700TZ",
  "02700Z6", "02700ZZ", "0270346", "027034Z", "0270356", "027035Z",
  "0270366", "027036Z", "0270376", "027037Z", "02703D6", "02703DZ",
  "02703E6", "02703EZ", "02703F6", "02703FZ", "02703G6", "02703GZ",
  "02703T6", "02703TZ", "02703Z6", "02703ZZ", "0270446", "027044Z",
  "0270456", "027045Z", "0270466", "027046Z", "0270476", "027047Z",
  "02704D6", "02704DZ", "02704E6", "02704EZ", "02704F6", "02704FZ",
  "02704G6", "02704GZ", "02704T6", "02704TZ", "02704Z6", "02704ZZ",
  "0271046", "027104Z", "0271056", "027105Z", "0271066", "027106Z",
  "0271076", "027107Z", "02710D6", "02710DZ", "02710E6", "02710EZ",
  "02710F6", "02710FZ", "02710G6", "02710GZ", "02710T6", "02710TZ",
  "02710Z6", "02710ZZ", "0271346", "027134Z", "0271356", "027135Z",
  "0271366", "027136Z", "0271376", "027137Z", "02713D6", "02713DZ",
  "02713E6", "02713EZ", "02713F6", "02713FZ", "02713G6", "02713GZ",
  "02713T6", "02713TZ", "02713Z6", "02713ZZ", "0271446", "027144Z",
  "0271456", "027145Z", "0271466", "027146Z", "0271476", "027147Z",
  "02714D6", "02714DZ", "02714E6", "02714EZ", "02714F6", "02714FZ",
  "02714G6", "02714GZ", "02714T6", "02714TZ", "02714Z6", "02714ZZ",
  "0272046", "027204Z", "0272056", "027205Z", "0272066", "027206Z",
  "0272076", "027207Z", "02720D6", "02720DZ", "02720E6", "02720EZ",
  "02720F6", "02720FZ", "02720G6", "02720GZ", "02720T6", "02720TZ",
  "02720Z6", "02720ZZ", "0272346", "027234Z", "0272356", "027235Z",
  "0272366", "027236Z", "0272376", "027237Z", "02723D6", "02723DZ",
  "02723E6", "02723EZ", "02723F6", "02723FZ", "02723G6", "02723GZ",
  "02723T6", "02723TZ", "02723Z6", "02723ZZ", "0272446", "027244Z",
  "0272456", "027245Z", "0272466", "027246Z", "0272476", "027247Z",
  "02724D6", "02724DZ", "02724E6", "02724EZ", "02724F6", "02724FZ",
  "02724G6", "02724GZ", "02724T6", "02724TZ", "02724Z6", "02724ZZ",
  "0273046", "027304Z", "0273056", "027305Z", "0273066", "027306Z",
  "0273076", "027307Z", "02730D6", "02730DZ", "02730E6", "02730EZ",
  "02730F6", "02730FZ", "02730G6", "02730GZ", "02730T6", "02730TZ",
  "02730Z6", "02730ZZ", "0273346", "027334Z", "0273356", "027335Z",
  "0273366", "027336Z", "0273376", "027337Z", "02733D6", "02733DZ",
  "02733E6", "02733EZ", "02733F6", "02733FZ", "02733G6", "02733GZ",
  "02733T6", "02733TZ", "02733Z6", "02733ZZ", "0273446", "027344Z",
  "0273456", "027345Z", "0273466", "027346Z", "0273476", "027347Z",
  "02734D6", "02734DZ", "02734E6", "02734EZ", "02734F6", "02734FZ",
  "02734G6", "02734GZ", "02734T6", "02734TZ", "02734Z6", "02734ZZ",
  "02C0046", "02C004Z", "02C0056", "02C005Z", "02C0066", "02C006Z",
  "02C0076", "02C007Z", "02C00D6", "02C00DZ", "02C00E6", "02C00EZ",
  "02C00F6", "02C00FZ", "02C00G6", "02C00GZ", "02C00T6", "02C00TZ",
  "02C00Z6", "02C00ZZ", "02C0346", "02C034Z", "02C0356", "02C035Z",
  "02C0366", "02C036Z", "02C0376", "02C037Z", "02C03D6", "02C03DZ",
  "02C03E6", "02C03EZ", "02C03F6", "02C03FZ", "02C03G6", "02C03GZ",
  "02C03T6", "02C03TZ", "02C03Z6", "02C03ZZ", "02C0446", "02C044Z",
  "02C0456", "02C045Z", "02C0466", "02C046Z", "02C0476", "02C047Z",
  "02C04D6", "02C04DZ", "02C04E6", "02C04EZ", "02C04F6", "02C04FZ",
  "02C04G6", "02C04GZ", "02C04T6", "02C04TZ", "02C04Z6", "02C04ZZ",
  "02C1046", "02C104Z", "02C1056", "02C105Z", "02C1066", "02C106Z",
  "02C1076", "02C107Z", "02C10D6", "02C10DZ", "02C10E6", "02C10EZ",
  "02C10F6", "02C10FZ", "02C10G6", "02C10GZ", "02C10T6", "02C10TZ",
  "02C10Z6", "02C10ZZ", "02C1346", "02C134Z", "02C1356", "02C135Z",
  "02C1366", "02C136Z", "02C1376", "02C137Z", "02C13D6", "02C13DZ",
  "02C13E6", "02C13EZ", "02C13F6", "02C13FZ", "02C13G6", "02C13GZ",
  "02C13T6", "02C13TZ", "02C13Z6", "02C13ZZ", "02C1446", "02C144Z",
  "02C1456", "02C145Z", "02C1466", "02C146Z", "02C1476", "02C147Z",
  "02C14D6", "02C14DZ", "02C14E6", "02C14EZ", "02C14F6", "02C14FZ",
  "02C14G6", "02C14GZ", "02C14T6", "02C14TZ", "02C14Z6", "02C14ZZ",
  "02C2046", "02C204Z", "02C2056", "02C205Z", "02C2066", "02C206Z",
  "02C2076", "02C207Z", "02C20D6", "02C20DZ", "02C20E6", "02C20EZ",
  "02C20F6", "02C20FZ", "02C20G6", "02C20GZ", "02C20T6", "02C20TZ",
  "02C20Z6", "02C20ZZ", "02C2346", "02C234Z", "02C2356", "02C235Z",
  "02C2366", "02C236Z", "02C2376", "02C237Z", "02C23D6", "02C23DZ",
  "02C23E6", "02C23EZ", "02C23F6", "02C23FZ", "02C23G6", "02C23GZ",
  "02C23T6", "02C23TZ", "02C23Z6", "02C23ZZ", "02C2446", "02C244Z",
  "02C2456", "02C245Z", "02C2466", "02C246Z", "02C2476", "02C247Z",
  "02C24D6", "02C24DZ", "02C24E6", "02C24EZ", "02C24F6", "02C24FZ",
  "02C24G6", "02C24GZ", "02C24T6", "02C24TZ", "02C24Z6", "02C24ZZ",
  "02C3046", "02C304Z", "02C3056", "02C305Z", "02C3066", "02C306Z",
  "02C3076", "02C307Z", "02C30D6", "02C30DZ", "02C30E6", "02C30EZ",
  "02C30F6", "02C30FZ", "02C30G6", "02C30GZ", "02C30T6", "02C30TZ",
  "02C30Z6", "02C30ZZ", "02C3346", "02C334Z", "02C3356", "02C335Z",
  "02C3366", "02C336Z", "02C3376", "02C337Z", "02C33D6", "02C33DZ",
  "02C33E6", "02C33EZ", "02C33F6", "02C33FZ", "02C33G6", "02C33GZ",
  "02C33T6", "02C33TZ", "02C33Z6", "02C33ZZ", "02C3446", "02C344Z",
  "02C3456", "02C345Z", "02C3466", "02C346Z", "02C3476", "02C347Z",
  "02C34D6", "02C34DZ", "02C34E6", "02C34EZ", "02C34F6", "02C34FZ",
  "02C34G6", "02C34GZ", "02C34T6", "02C34TZ", "02C34Z6", "02C34ZZ"
)


# Input data ---------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")


# Variable creation (Part 1) -----------------------------------------

# Our goal is to determine whether a particular diagnosis or procedure
# was coded during an admission. To put this in more
# programming-specific terms, we want to ask whether any of the
# diagnosis codes recorded in I10_DXn (and perhaps I10_ECAUSEn) match
# any of the codes for our diagnosis or procedure of interest.

# In order to get a sense of what's involved, we'll pretend we have an
# admission with a single code.

DX1 <- "I220"

# Suppose we want to know if this code matches any of the codes for
# acute MI. One way we've compared values in the past is with `==`. So
# we could write

## DX1_isAcuteMI <- DX1 == "I2101" | DX1 == "I2102" | ...

# However, with the sheer number of codes in ICD-10-CM/PCS, this isn't
# practical. We've already seen a way of checking whether a value is
# in a specific set of values using `%in%`, so we can write this more
# concisely as

DX1_isAcuteMI <- DX1 %in% dx10_acuteMI
DX1_isAcuteMI
## [1] TRUE

# To add one layer of complexity, what if we have multiple admissions
# each with a single diagnosis code column, and we want to know
# whether the code associated with each admission is in the list of
# codes for the diagnosis or procedure of interest? We've actually
# been doing something like this all along, albeit with other
# variables (e.g., `PAY1`, `AHOUR`), so the fact that we're asking
# whether the code for each admission is in the list doesn't require
# anything new.

DXcols1 <- data.frame(
  KEY = 1:2,
  DX1 = c("I220", "E0800")
)

DXcols1_wAcuteMI <- DXcols1 %>%
  mutate(dx_acuteMI = if_else(DX1 %in% dx10_acuteMI, 1, 0))
DXcols1_wAcuteMI
## KEY   DX1 dx_acuteMI
##   1  I220          1
##   2 E0800          0

# R automatically "vectorizes" the operation over the entire column
# `DX1`.

# But we have multiple columns of diagnosis and procedure codes for
# each admission, and we need to ask whether (for each admission) any
# of the codes in the relevant columns match any of the codes in the
# list of diagnosis or procedure codes of interest.

# Again, this isn't really anything new.

DXcols2 <- data.frame(
  KEY = 1:3,
  DX1 = c("I220", "I209", "J80"),
  DX2 = c("E0800", "I220", "O9921")
)

DXcols2_wAcuteMI <- DXcols2 %>%
  mutate(
    dx_acuteMI = if_else(
      DX1 %in% dx10_acuteMI | DX2 %in% dx10_acuteMI, 1, 0)
  )
DXcols2_wAcuteMI
## KEY  DX1   DX2 dx_acuteMI
##   1 I220 E0800          1
##   2 I209  I220          1
##   3  J80 O9921          0

# So really our main problem is that we have LOTS of columns with
# diagnosis and procedure codes, and since we're also likely to have
# many diagnoses and procedures that we'd like to identify, it would
# be very tedious to have to write out such a long expression every
# time. Can we write this more concisely?

# What about this?

DXcols2_wAcuteMI <- DXcols2 %>%
  mutate(
    dx_acuteMI = if_else(
      c(DX1, DX2) %in% dx10_acuteMI, 1, 0)
  )
## Error: Problem with `mutate()` column `dx_AcuteMI`.
## i `dx_AcuteMI = if_else(c(DX1, DX2) %in% dx10_acuteMI, 1, 0)`.
## i `dx_AcuteMI` must be size 3 or 1, not 6.

# Uh oh, that didn't work. To explore why, let's evaluate the
# expression outside of `mutate`.

c(DXcols2$DX1, DXcols2$DX2) %in% dx10_acuteMI
## [1]  TRUE FALSE FALSE FALSE  TRUE FALSE

# We have a vector of TRUE/FALSE of length six, but there are only
# three observations in DXcols2. To define a new column, we need the
# expression to return one value for each observation; hence, the
# error.

# As it turns out, there already is a function---`any`--- that
# collapses a vector of TRUE/FALSE using logical 'or':
any(c(TRUE, FALSE))
## [1] TRUE

# So maybe if we wrap the conditional in `any`:

DXcols2_wAcuteMI <- DXcols2 %>%
  mutate(
    dx_acuteMI = if_else(
      any(c(DX1, DX2) %in% dx10_acuteMI), 1, 0)
  )
DXcols2_wAcuteMI
## KEY  DX1   DX2 dx_acuteMI
##   1 I220 E0800          1
##   2 I209  I220          1
##   3  J80 O9921          1

# We don't get the error, but the output is also wrong.

# To see what's going on, consider again the expression

c(DXcols2$DX1, DXcols2$DX2) %in% dx10_acuteMI
## [1]  TRUE FALSE FALSE FALSE  TRUE FALSE

# If we line up the TRUE/FALSE values with the columns in DXcols2, it
# looks like R evaluated

c("I220", "I209", "J80", "E0800", "I220", "O9921") %in% dx10_acuteMI
## [1]  TRUE FALSE FALSE FALSE  TRUE FALSE

# In words, R stacked the DX1 and DX2 columns into a single vector and
# asked whether each value was in `dx10_acuteMI`. 

# Similarly, `any` was given c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
# and returned TRUE, as it should, but then `if_else` returned 1,
# which was stored in each row of the column `dx_acuteMI`.

# What we wanted is for R to take DX1 and DX2 of the first row and ask
# whether any of those two values is in `dx10_acuteMI`, then take DX1
# and DX2 of the second row and ask whether any of those two values is
# in `dx10_acuteMI`, and similarly for the third row.

# It turns out that this is a nontrivial problem in R, and it will
# require us to write our own functions.


# A detour on functions-----------------------------------------------

# Functions allow us to avoid copying and pasting the same code over
# and over again, which can easily lead to errors, especially if we
# need to edit code that we've copied many times. They also give us a
# chance to describe what we're doing with a meaningful name---the
# name of the function.

# A function is defined using the syntax

## <functionName> <- function(<input-1>, <input-2>, <...>) {
##   <expression-1>
##   <expression-2>
##   <...>
## }

# where the pieces in between '<' and '>' are meant to be replaced
# with the name of the function, the name of its argument(s), and
# expressions in the function body. The last expression is what the
# function produces.

# The first step in solving our problem is to write a function whose
# input is a vector consisting of a list of codes. We've done all of
# the work for this above already, so we just need to define it as a
# function.

flag_acuteMI <- function(rowOfCodes) {
  if_else(any(rowOfCodes %in% dx10_acuteMI), 1, 0)
}


# Variable creation (Part 2) -----------------------------------------

# We've encapsulated our code into a function. How can we use it to
# avoid the error we encountered earlier?

# The Tidyverse (specifically the dplyr package) provides a special
# syntax for applying functions row-wise that's similar to `group_by`,
# which we've looked at previously.

DXcols2_wAcuteMI <- DXcols2 %>%
  rowwise(KEY) %>%
  mutate(
    dx_acuteMI = flag_acuteMI(c_across(c(DX1, DX2)))
  ) %>%
  ungroup()
DXcols2_wAcuteMI

# `rowwise` acts like `group_by`, it tells dplyr that we want to
# process the data 'by row'. `KEY` is the name of the variable that
# uniquely defines each row, but it's not necessary to include a
# variable unless you're going to use `summarise`. In other words,
# `rowwise()` in the code above would produce the same result since
# we're defining `dx_acuteMI` using `mutate`.

# The second key point here is that we've used `c_across` to tell
# dplyr which columns we want to process by row. We can use shorthands
# so we don't have to write each column name explicitly, as shown
# below where we apply this technique to the 1% sample of the Florida
# SID.

core1p_wAcuteMI <- core1p %>%
  rowwise(KEY) %>%
  mutate(
    dx_acuteMI = flag_acuteMI(c_across(starts_with("I10_DX")))
  ) %>%
  ungroup()
table(core1p_wAcuteMI$dx_acuteMI, useNA = "ifany")


# Sample problem 1 ----------------

# Define a flag variable for ischemic stroke.

flag_ischemicStroke <- function(rowOfCodes) {
  if_else(any(rowOfCodes %in% dx10_ischemicStroke), 1, 0)
}

core1p_wIschemicStroke <- core1p %>%
  rowwise(KEY) %>%
  mutate(
    dx_ischemicStroke =
      flag_ischemicStroke(c_across(starts_with("I10_DX")))
  ) %>%
  ungroup()

table(core1p_wIschemicStroke$dx_ischemicStroke, useNA = "ifany")

# Other techniques for matching strings

flag_ischemicStroke2 <- function(codes) {
  if_else(
    any(
      ("I630" <= codes & codes <= "I636")
      | str_sub(codes, 1, 4) %in% c("I638", "I639")
    ),
    1, 0
  )
}

core1p_wIschemicStroke2 <- core1p %>%
  rowwise(KEY) %>%
  mutate(
    dx_ischemicStroke =
      flag_ischemicStroke2(c_across(starts_with("I10_DX")))
  ) %>%
  ungroup()

table(core1p_wIschemicStroke2$dx_ischemicStroke, useNA = "ifany")


# Sample problem 2 ----------------

# Define a flag variable for PCI.

flag_pci <- function(codes) {
  if_else(any(codes %in% pr10_pci), 1, 0)
}

core1p_wPCI <- core1p %>%
  rowwise(KEY) %>%
  mutate(
    pr_pci =
      flag_pci(c_across(starts_with("I10_PR")))
  ) %>%
  ungroup()

table(core1p_wPCI$pr_pci, useNA = "ifany")


pr10_pci2 <- c(
  "0270", "0271", "0272", "0273", "02C0", "02C1", "02C2", "02C3"
)
                                                   
flag_pci2 <- function(codes) {
  if_else(any(str_sub(codes, 1, 4) %in% pr10_pci2), 1, 0)
}

core1p_wPCI2 <- core1p %>%
  rowwise(KEY) %>%
  mutate(
    pr_pci =
      flag_pci2(c_across(starts_with("I10_PR")))
  ) %>%
  ungroup()

table(core1p_wPCI2$pr_pci, useNA = "ifany")
