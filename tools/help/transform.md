Modify type, transformation, creation, recoding, reordering, renaming and remove variables in the data.

#### Convert Variable Type

When you select Type from the 'Convert Variable Type' drop-down another drop-down selection 'Change variable type'is shown that will allow you to change the type or class of one or more (press 'Ctrl' or 'Shift' key on keyboard) variables  in your data. For example, to change a variable of type numeric to a variable of type character. Click the 'Save changes' button to save the changed variable(s) in the data set. 

1. As factor: convert a variable to type factor (i.e., as.factor(x))
2. As number: convert a variable to type numeric (i.e., as.numeric(x))
3. As integer: convert a variable to type integer (i.e., as.integer(x))
4. As character: convert a variable to type character (i.e., as.character(x))


#### Transform Variables

When you select Type from the 'Transform Variables' Type drop-down another drop-down selection 'Apply function' is shown that will allow you do apply common transformations to one or more (press 'Ctrl' or 'Shift' key on keyboard) variables in your data. For example, to take the (natural) log of a variable select the variable you want to change and choose Log from the 'Apply function' menu. A new variable is created with the prefix 'log.'. Click the 'Save changes' button to add the variable(s) to the data set. 

1. Log: create a log-transformed version of the selected variable (i.e., log(x))
2. Square: multiply a variable by itself (i.e. x^2 ) 
3. Square-root: take the square-root of a variable (i.e., x^.5 )
4. Center: create a new variable with a mean equal to zero (i.e., x - mean(x))
5. Standardize: create a new variable with a mean equal to zero and standard deviation equal to 1 (i.e., (x - mean(x)/sd(x)))
6. Invert: 1/x
7. Median split: create a new factor with two levels (Above and Below) that splits the variable values at the median
8. Deciles: create a new factor with 10 levels (deciles) that splits the variable values at the 10th, 20th, ..., 90th percentiles.

#### Create New Variable (A thing need to be recreat to avoid hacking...)

When you select Type from the 'Create New Variable' Type drop-down another drop-down text box is shown that will allow you to
write expression to create new variable. However, it only allow a simple syntax here
to allow you to write expression.

1. Basic syntax --- newvarname := expression
   
   The program will detech you use := symbol and what in the left will be used as 
   new variable name and what in the right will be used as the expression.

2. Expression function
   
   The current version support + - * / ( and :
   
   The ":" one is the special which allow you to write combination of variables
   
   Try "newvars:= cut:color" in our example data.



#### Recode

To use the recode feature choose 'Recode' from the 'Transformation type' dropdown menu then select the catergorical variable you want to change in 'Select Variable' menu. You will see two text boxes to allow you write command to achieve one-to-one recoding. You should follow the same syntax as describe below then press 'Enter' on keyboard to check the new recoding variable. Click 'Save changes' to add the new variable to the data. Some recode command examples are given below.

1. Multiple match, you should keep them in the same order and use ',' to separate them
  
  The below will show you correct.
  
![recode matching - correct](figures/Capture1.png)

2. Multiple match is sensitive to unequal length input

	The below will show you incorrect.
![recode matching - incorrect](figures/Capture2.png)

3. Type as you see, text box accept unmeaningful name. 

  The below will show correct, so be careful to this.

![recode matching - unmeaningful](figures/Capture3.png)

4. NA is not possible creating here cause every value is actually character string.
  
  The later chapter you can use remove missing value to remove NA but the NA value creating here is actually 'NA' so this is
  accepted in R.
![recode matching - unmeaningful](figures/Capture4.png)

#### Rename

Choose 'Rename' from the 'Transformation type' dropdown, select one or more variables and enter new names for them in the rename box shown. Separate each name by a ','. Press return to see the variables with their new names on screen and  press 'Save changes' to alter the variable names in the original data.

#### Remove columns

Choose 'Remove columns' from the 'Transformation type' dropdown and select one or more variables to remove. Press 'Save changes' to remove the variables from the original data. Note that this action cannot be undone. If you want to the original variables back you will have to reload the data through the Data > Manage page.

#### Remove missing

Choose 'Remove missing' from the 'Transformation type' dropdown to remove missing values. Press 'Save changes' to remove all rows with missing values from the data. If missing values were present you will see the number of observations in the data summary change (i.e., the value of n changes). Note that this action cannot be undone. If you want these rows back you will have to reload the data through the Data > Manage page.

#### Drop observations

Here I create a 'strict mode' that you can only allow to do so:

1. single number
 type '1' and press return

2. some numbers
 type '1,2,34' and press return. This should drop observation number 1,2,34
 
3. using ':' to save your finger
 type '1:1000' and press return. 

4. Extra notice
 There is situation for dropping according by row name rather than actual index.
 The default diamonds data has row name randomly cause it is random sampling.
 The future version should include syntax to handle '7718' to drop the first index.
 A possible suggestion is using /7718\\27822/ syntax to tell program drop observations
 based on row name "7718" and "27822" which is our first two observations.

&copy; Junjie Zeng (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>
