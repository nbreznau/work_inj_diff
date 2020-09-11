# Work-Injury Policy Diffusion, Breznau and Lanver

This is the code from the M. Windzio workshop, 11. Sept. The code has been converted into an Rmd file and the main dependent variable has been replaced by the first law of work-injury by country (project A02). 

We will work here to develop our analysis for a chapter for the book.

This will also provide a place where we can share our data and code.

The .Rmd file works best if you install GitHub Desktop and clone this repository to your local machine. Then you can always 'pull' (if there are new changes from others) or 'push' (if you make changes). It is ideal if you put your synchronized GitHub repositories in the folder C:/GitHub/, that way the code will run without any errors. Otherwise you will need to change the location of 'wd' in the setup code chunk to match your local directory. But if you do this, you will make it so the code does not run on others' machines. Thsu, you could also craete your own 'branch' of the Git, so that you can have your own code and work on it.


## Our results from the workshop: 



<table style="text-align:center"><caption><strong>Global Network Diffusion of First Work-Injury Laws</strong></caption>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">Introduction of Work-Injury Law</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">rate t(0-24)</td><td>-5.601<sup>***</sup></td><td>-5.621<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.272)</td><td>(0.271)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">rate t(25-49)</td><td>-5.090<sup>***</sup></td><td>-5.236<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.328)</td><td>(0.327)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">rate t(50-74)</td><td>-5.788<sup>***</sup></td><td>-6.125<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.545)</td><td>(0.562)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">rate t(75-99)</td><td>-6.197<sup>***</sup></td><td>-6.640<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.712)</td><td>(0.727)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">rate t(100-130)</td><td>-6.334<sup>***</sup></td><td>-6.898<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.844)</td><td>(0.864)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">state existed (=1,else=0)</td><td>0.264</td><td>0.086</td></tr>
<tr><td style="text-align:left"></td><td>(0.210)</td><td>(0.223)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">cultural spheres netw.: w. exposure (lag 1 year)</td><td>3.868<sup>***</sup></td><td>1.307</td></tr>
<tr><td style="text-align:left"></td><td>(0.868)</td><td>(1.280)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">colonies netw.: exposure</td><td>0.069</td><td>0.061</td></tr>
<tr><td style="text-align:left"></td><td>(0.250)</td><td>(0.250)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">trade net: w. exposure (lag 1 year)</td><td>0.068</td><td>0.067</td></tr>
<tr><td style="text-align:left"></td><td>(0.044)</td><td>(0.047)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">spatial proximity netw.: w. exposure</td><td></td><td>3.399<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.224)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">GDP per capita / 10000 USD</td><td>-0.086</td><td>-0.077</td></tr>
<tr><td style="text-align:left"></td><td>(0.055)</td><td>(0.056)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">democratization</td><td>0.240<sup>***</sup></td><td>0.244<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.052)</td><td>(0.053)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>7,448</td><td>7,448</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-616.695</td><td>-612.898</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>1,255.390</td><td>1,249.796</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

