The code produces a Shiny app for a generic elicitation exercise with four hypothetical parameters.

**** add reference to the SEE protocol and step-by-step guide when complete****

The code is designed to be easily customisable to produce bespoke Shiny apps for conducting structured expert elicitation.

This document provides the app overview (section 1), how to customize the code to produce bespoke apps (section 2), and potential for further customisation (section 3).


1.	App overview


The code in this folder produces a basic Shiny app for expert elicitation that includes:
  -	A “Home” page that includes a consent form and dummy questions about experts completing the exercise. The text on the home page and the consent form, and the questions about experts can be edited by the user, or removed.
  -	An “Instructions” tab that explains the difference between uncertainty and variability, and instructions for how to complete the exercise. The instructions are specific to the elicitation method selected by the user (chips an bins, tertiles or quartiles). The text can also be edited by the user.
  -	A dummy “Background information” tab that can be edited to provide any relevant background information (project details, relevant literature) to experts;
  -	A “Questions” tab that contains four sub-tabs, one for each hypothetical elicitation question. The number of questions, the text and the elicitation method (chips and bins, tertiles or quartiles) can be modified by the user.


1.1.	Details on elicitation methods in the app


The app can include one of three elicitation methods: one fixed interval method Chips and Bins, also referred to as Roulette and histogram method (ref ####), or one of two variable interval methods quartiles (ref ####) and tertiles (ref ####).

All elicitation methods first elicit the plausible range.

If using Chips and Bins, experts are then presented with a plot where they can express their uncertainty. The range on the x-axis of the plot is always slightly wider than the expert’s range, unless this is prevented by the limits of the parameter being elicited (e.g. if an experts’ lower limit is 0%, then the lower limit of the plot is also 0%). The bin-width on the plot (i.e. the range of intervals on the histogram) take value of 1, 2 or 5, or their multiple of 10, 100 or 1000. The exact bin width is derived to make the total number of bins as close to 10 as possible.

If using either of the variable interval methods, experts are then asked to provide their quartiles or tertiles.

Once experts have expressed their uncertainty using the relevant method, they are presented with feedback on their quantities.

With Chips and Bins, the feedback includes the probability placed on expert’s mode range, and the probability on either side of the mode.
 
With the variable interval methods, the feedback includes comparison of different parameter ranges (e.g. the proportion is equally likely to be between 20% and 40%, as it is to be outside this range).

Experts can then save their answers. Experts can also edit and re-save their answers at any point in the exercise.


2.2. Instructions for running the code


To run the code, follow these steps:

  a) Download all files into a single folder with a name of your choosing.
  
  b) Open the “manual_inputs.R” file in R.
  
  c) Set the app location in the “app_location” object.
  
  d) Run the app from Rstudio.

Note that when the app starts, it asks for a numeric password. This number should be provided to experts in advance, to ensure experts can be matched to their answers, without saving personal information on the Dropbox folder. The password is also used by the bookmarking function when experts complete the exercise in multiple sittings, allowing experts to continue from where they left off. The app works with any number entered in the box, but the expert will only be able to use bookmarking if they use the same password every time. Furthermore, if more than one expert uses the same password (for example, if they are asked to make one up themselves, rather than providing the password to them), they will overwrite each other’s answers.


2.	How to create bespoke apps


The code is designed to be easily customisable to produce bespoke Shiny apps for conducting structured expert elicitation.


  2.1. Text-based files


Text on the home page, the consent form, instructions and background information can all be edited in word. The basic content is provided in four htm files in the “www” folder. To edit the text:
  a) Right-click on the relevant htm file;
  
  b) Select “Open with”;
  
  c) Select “Word”.
 
Once you’ve finished editing the file, save it as a htm file, overwriting the original.

For guidance on the type of information that should be included in an elicitation exercise, see ref####.


  2.2.	App content


App content and elicitation questions can be edited in the “manual_inputs.R” file. Specifically, the file allows the following to be edited:

a)	Whether to include a consent form in the app: “include_consent” object set to TRUE (if consent form is included) or FALSE.

b)	Whether to include questions about experts: “include_about_you” object set to TRUE (if questions are included) or FALSE. (The questions are entered in a separate file, described in section 2.3).

c)	If b) is set to TRUE, you must specify the total number of questions about experts in the “n_about_you” object.

d)	The elicitation method: “elicitation_method” object set to “chips and bins”, “quartiles” or “tertiles”.

e)	The quantities being elicited: “quantity” vector where each element corresponds to one elicitation question so that the length of the vector equals the total number of elicitation questions.

f)	Units for each quantity being elicited: “units” vector where each element corresponds to one elicitation question so that the length of the vector equals the total number of elicitation questions. When a quantity has no units, set to “”.

g)	Lower and upper limit of each quantity being elicited: “quant_limit_lower” and “quant_limit_upper” vectors, where each element corresponds to one elicitation question so that the length of each vector is equal to the total number of elicitation questions. When a parameter has no limit, set to NA.

h)	Elicitation questions, saved in “eli_que_text” vector where each element corresponds to one elicitation question so that the length of the vector equals the total number of elicitation questions.

i)	Whether to lock the app content so that experts must click through each tab to complete the exercise, preventing skips between tabs: “conditional_release” set to TRUE (to force experts to complete each section) or FALSE (to make all tabs visible at all times). “conditional_release” is usually set to FALSE while editing the app, then TRUE in the final, published version.

j)	Name of Dropbox folder where elicitation exercises will be saved: “folder_name” object. For further details on how to set up the Dropbox folder, see manual_inputs.R file and ref#####.


  2.3.	About you


If b) in section 2.2 is set to TRUE, the questions about experts should be added to the “about_you.R” file, using the standard Shiny widgets (ref ####). Two examples are provided in the supplied code. 


3.	Further customisations


The app is open access and designed to be used as a starting point for creating bespoke Shiny apps for conducting structured expert elicitation. All provided files, including server.R and ui.R can be edited to further customise the app. Some simple edits that can be made are provided here, but other options are available.
  •	Changing aesthetics using standard Shiny methods (e.g. ref https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/, https://shiny.rstudio.com/articles/css.html), including addition of the title and logo on each page
  •	Addition of new tabs by editing the ui.R interface
  •	Alternate saving methods: the user may wish to store experts’ answers locally, or on server. The saving method can be edited by changing the “f_save_answers” function in the “functions.R” file, and removing objects that refer to dropbox (namely, token and drop_acc call in functions.R)
