The code produces a Shiny app for a generic elicitation exercise with four hypothetical parameters.

The code is designed to be easily customisable to produce bespoke Shiny apps for conducting structured expert elicitation.

The Shiny app content and design are based on the methods in the protocol for structured expert elicitation (SEE) funded by the Medical Research Council (MRC) written by Bojke et al. (2022) and the subsequent [step-by-step guide to SEE](https://www.york.ac.uk/che/research/teehta/elicitation/see/) written as a collaboration between Lumanity and the Centre for Health Economics at the University of York.

This document provides the app overview (section 1), how to customize the code to produce bespoke apps (section 2), and potential for further customisation (section 3).


# 1.	App overview


The code in this folder produces a basic Shiny app for expert elicitation that includes:
*	A “Home” page that includes a consent form and dummy questions about experts completing the exercise. The text on the home page and the consent form, and the questions about experts can be edited by the user, or removed.
*	An “Instructions” tab that explains the difference between uncertainty and variability, and instructions for how to complete the exercise. The instructions are specific to the elicitation method selected by the user (chips and bins, tertiles or quartiles). The text can also be edited by the user.
*	A dummy “Background information” tab that can be edited to provide any relevant background information (project details, relevant literature) to experts;
*	A “Questions” tab that contains four sub-tabs, one for each hypothetical elicitation question. The number of questions, the text and the elicitation method (chips and bins, tertiles or quartiles) can be modified by the user.



## 1.1.	Details on elicitation methods in the app


The app can include one of three elicitation methods: one fixed interval method Chips and Bins, also referred to as Roulette and histogram method (ref ####), or one of two variable interval methods quartiles, also referred to as the bisection method (ref ####), and tertiles (ref ####).

All elicitation methods first elicit the plausible range.

If using Chips and Bins, experts are then presented with a plot where they can express their uncertainty. The range on the x-axis of the plot is always slightly wider than the expert’s range, unless this is prevented by the limits of the parameter being elicited (e.g. if an experts’ lower limit is 0%, then the lower limit of the plot is also 0%). The bin-width on the plot (i.e. the range of intervals on the histogram) can take value of 1, 2 or 5, or their multiple of 10, 100 or 1000. The exact bin width is derived to make the total number of bins as close to 10 as possible.

If using either of the variable interval methods, experts are then asked to provide their quartiles or tertiles.

Once experts have expressed their uncertainty using the relevant method, they are presented with feedback on their 
quantities.

With Chips and Bins, the feedback includes the probability placed on expert’s mode range, and the probability on either side of the mode.

![](www/c+b_feedback.png)

With the variable interval methods, the feedback includes comparison of different parameter ranges (e.g. the proportion is equally likely to be between 20% and 40%, as it is to be outside this range).

Experts can then save their answers. Experts can also edit and re-save their answers at any point in the exercise.


## 1.2. Instructions for running the code


To run the code, follow steps:

1.	Download all files into a single folder with a name of your choosing
2.	Open the “app.R” file in Rstudio.
3.	Run the app from Rstudio.
4.	Disseminate the app with your experts via a web link by deploying it to a webpage. For details on how to do this, see the [share your appp Shiny tutorial](https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/).

The simplest way to share your app is via shinyapps.io. To do so, you need to open an account with [shinyapps.io](shinyapps.io), then click on the “Publish application” button in Rstudio (![](www/deploy_button.png) icon in the top right corner of the screen) and follow the onscreen instructions.

Note that when the app starts, it asks for a unique identifier. This is a number that should be provided to experts in advance, to ensure that each expert can be matched to their answers, without saving personal information on the Dropbox folder. The password is also used by the bookmarking function when experts complete the exercise in multiple sittings, allowing experts to continue from where they left off. Note that if more than one expert uses the same unique identifier, they will overwrite each other’s answers.

The dummy app works with the unique identifier “1234”. When customising the app, the list of unique identifiers must be set as described in section 2.2.


# 2. How to create bespoke apps


The code is designed to be easily customisable to produce bespoke Shiny apps for conducting structured expert elicitation.


## 2.1. Text-based files


Text on the home page, the consent form, instructions and background information can all be edited in word. The basic content is provided in four htm files in the “www” folder. To edit the text:
1. Right-click on the relevant htm file;
  
2. Select “Open with”;
  
3. Select “Word”.
 
![](www/save_htm.png)
 
Once you have finished editing the file, save it as a htm file, overwriting the original.

For guidance on the type of information that should be included in an elicitation exercise, see the [step-by-step guide to SEE](https://www.york.ac.uk/che/research/teehta/elicitation/see/).


## 2.2.	App content


App content and elicitation questions can be edited in the “manual_inputs.R” file. Specifically, the file allows the following to be edited:

a) The list of unique identifiers for individual experts participating in the study: “all_expert_ids“ vector where each element corresponds to the unique identifier provided to one expert so that the length of the vector equals the total number of experts who are invited to take part. Each expert should be provided one of these unique identifiers in their invitation to take part in structured expert elicitation.

b) Whether to include a consent form in the app: “include_consent” object set to TRUE (if consent form is included) or FALSE.

c) Whether to include questions about experts: “include_about_you” object set to TRUE (if questions are included) or FALSE. (The questions are entered in a separate file, described in section 2.3).

d) If c) is set to TRUE, you must specify the total number of questions about experts in the “n_about_you” object.

e) The elicitation method: “elicitation_method” object set to “chips and bins”, “quartiles” or “tertiles”.

f) The quantities being elicited: “quantity” vector where each element corresponds to one elicitation question so that the length of the vector equals the total number of elicitation questions.

g) Units for each quantity being elicited: “units” vector where each element corresponds to one elicitation question so that the length of the vector equals the total number of elicitation questions. When a quantity has no units, set to “”.

h) Lower and upper limit of each quantity being elicited: “quant_limit_lower” and “quant_limit_upper” vectors, where each element corresponds to one elicitation question so that the length of each vector is equal to the total number of elicitation questions. When a parameter has no limit, set to NA.

i) Elicitation questions, saved in “eli_que_text” vector where each element corresponds to one elicitation question so that the length of the vector equals the total number of elicitation questions.

j) Whether to lock the app content so that experts must click through each tab to complete the exercise, preventing skips between tabs: “conditional_release” set to TRUE (to force experts to complete each section) or FALSE (to make all tabs visible at all times). “conditional_release” is usually set to FALSE while editing the app, then TRUE in the final, published version.

k) Name of Dropbox folder where elicitation exercises will be saved: “folder_name” object. For further details on how to set up the Dropbox folder, see manual_inputs.R file.


## 2.3.	About you


If c) in section 2.2 is set to TRUE, the questions about experts should be added to the “about_you.R” file, using [standard Shiny widgets](https://shiny.rstudio.com/gallery/widget-gallery.html). Two examples are provided in the supplied code. 


# 3. Further customisations


The app is open access and designed to be used as a starting point for creating bespoke Shiny apps for conducting structured expert elicitation. All provided files, including server.R and ui.R can be edited to further customise the app. Some simple edits that can be made are provided here, but other options are available.
* Changing aesthetics using standard Shiny methods (e.g. [https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/), [https://shiny.rstudio.com/articles/css.html](https://shiny.rstudio.com/articles/css.html)), including addition of the title and logo on each page
* Addition of new tabs by editing the ui.R interface
* Alternate saving methods: the user may wish to store experts’ answers locally, or on server. The saving method can be edited by changing the “f_save_answers” function in the “functions.R” file, and removing objects that refer to dropbox (namely, token and drop_acc call in functions.R)
