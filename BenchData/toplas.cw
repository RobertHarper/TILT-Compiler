*****************************
* Sample Session from Section 7.5,
* "The Concurrency Workbench: A Semantics-Based Tool for
* the Verification of Concurrent Systems", by Rance Cleaveland,
* Joachim Parrow, and Bernhard Steffen
* ACM Transactions on Programming Languages and Systems
* January, 1993, Volume 15, Number 1
******************************

*****************************
* Entered by David Tarditi. Apr 15 1993
* Errors in the specifications of
* senders and receivers found in the captions of Figures 11 and 12
* have (hopefully) been corrected.
*****************************

*****************************
* Specification of ABP
*****************************

bi SPEC
   send0.'rec0.SPEC + send1.'rec1.SPEC

***************************** 
* Definition of the medium
***************************** 

bi Medium
   s00.(t.'r00.Medium + t.Medium) + \
   s10.(t.'r10.Medium + t.Medium) + \
   s01.(t.'r01.Medium + t.Medium) + \
   s11.(t.'r11.Medium + t.Medium) + \
   sack0.(t.'rack0.Medium + t.Medium) + \
   sack1.(t.'rack1.Medium + t.Medium)

***************************** 
* Definition of the sender
***************************** 

bi S_0 
   send0.S00 + send1.S10

bi S00
   's00.(rack0.S_1 + rack1.S00 + t.S00)

*******
* This state was given incorrectly in the caption for Fig. 11
* in the toplas paper
*******

bi S10
   's10.(rack0.S_1 + rack1.S10 + t.S10)

bi S_1 
   send0.S01 + send1.S11
 
bi S01
   's01.(rack1.S_0 + rack0.S01 + t.S01)

bi S11
   's11.(rack1.S_0 + rack0.S11 + t.S11)

***************************** 
* Definition of receiver
***************************** 

bi R0
   r00.'rec0.'sack0.R1 + r10.'rec1.'sack0.R1 + \
   r01.'sack1.R0 + r11.'sack1.R0 + t.'sack1.R0

*****************************
* Definition of receiver state R1 was incorrectly in the caption
* for Fig. 12 
*****************************

bi R1
   r01.'rec0.'sack1.R0 + r11.'rec1.'sack1.R0 + \
   r00.'sack0.R1 + r10.'sack0.R1 + t.'sack0.R1

***********************************************
* Assembly into ABP
***********************************************

bi ABP
   (S_0|Medium|R0) \
   \{r00,r10,r01,r11,s00,s10,s01,s11,rack0,rack1,sack0,sack1}

eq
   ABP 
   SPEC

************************************************
* Check sorts
************************************************

sort SPEC

sort ABP

************************************************
* Check for equality
************************************************

mayeq
ABP
SPEC

***********************************************
* Check for deadlock, find deadlocked states
***********************************************

bpi
Deadlock
~<>T

cp
ABP
~Deadlock

fd
ABP

***********************************
* Corrected definition for sender
***********************************

bi S_0 
   send0.S00 + send1.S10

* S00 definition was wrong

*********
* S00 definition in caption for Fig. 13 was wrong
*********

bi S00
   's00.(rack0.S_1 + rack1.S00 + t.S00) + rack0.S00 + rack1.S00

*********
* S10 definition in caption was wrong 
*********

bi S10 
   's10.(rack0.S_1 + rack1.S10 + t.S10) + rack0.S10 + rack1.S10


bi S_1 
   send0.S01 + send1.S11

***********
* The definitions for S01 and S11 are also incorrect
***********

bi S01 
   's01.(rack1.S_0 + rack0.S01 + t.S01) + rack0.S01 + rack1.S01

bi S11 
   's11.(rack1.S_0 + rack0.S11 + t.S11) + rack0.S11 + rack1.S11

***********************************
* corrected definition for receiver
***********************************

bi R0 
   r00.'rec0.R0' + r10.'rec1.R0' + r01.R0" + r11.R0" + t.R0"

bi R0' 
   'sack0.R1 + r00.R0' + r10.R0' + r01.R0' + r11.R0'

bi R0"
   'sack1.R0 + r00.R0" + r10.R0" + r01.R0" + r11.R0"

*****
* Caption for Fig. 14 was incorrect: original was ... r11.'rec.R1" ...
*****

bi R1 
   r01.'rec0.R1' + r11.'rec1.R1' + r00.R1" + r10.R1" + t.R1"

bi R1'
   'sack1.R0 + r00.R1' + r10.R1' + r01.R1' + r11.R1'

bi R1" 
   'sack0.R1 + r00.R1" + r10. R1" + r01.R1" + r11.R1"

eq
ABP
SPEC

***
* Exit concurrency workbench
**

quit
