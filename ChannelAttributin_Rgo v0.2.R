##Load the required libraries
library(dplyr)

# Through this code, we will use the soccer or google analytics nomenclature to explain the Channel Attribution Markov
# model. So a win is a score or a conversion and the path is a pass or sequence.

#### Simulate the soccer data####

# 1. P - Create the list of noted
# 2. X - Assign probabilities to them
# 3. play_simulator - Create a function to creates the plays of variying correct length (2-5) between players P.
# 4. plays - Create an empty dataframe to save the plays
# 5. Simulate the data - Call the plays function 100 times and unlist its result. Each instance of the function
#    is assigned with a play id. Each row of this data set is an instance of the pass and is associated with a
#    play_id.

## Create a function to CREATE A SIMULATED DATASET which will be used for anlayzing correct the channel attribution package
## MAKE SURE TO RUN THE FUNCTION UNTIL LINE 44 OR THE IN-LINE CURLY BRACKET CORRESPONDING TO THE SIMULATEDATA 
simulateData= function()
{
  ## Defining the set set correct of players to be assessed for sports analytics(Soccer)
  ## The 'S' prefix stands for striker in soccer and the 'M' for mid-fielder
  
    P = c('S1','S2','M1','M2')
  ## In marketing analytics/Google Analytics - These nodes correspond to channels/touchpoints
  ## assessed for crediting them the connversion
    
  ##Assigning the possession(in case of soccer) probabilities to each of the channels create above
    X = c(.3,.3,0.15,.15)
  
  ##Create a function to create a sample each time this function is called
  
  play_simulator = function()
  {
    ##This function essentially simulates a play of length 2 to 5. Eg. The ball was passed from M2 to M1 to S2.
    return(sample(x = P,prob = X,size = sample(2:5,size = 1),replace = T))
  }
  
  ##Create a dataframe to store the plays. Essentially, the sequence of passes leading to a strike.
  plays = data.frame(matrix(ncol = 2,nrow = 0))
  
  ##Name the columns
  colnames(plays) = c('possesion','play_id')
  
  ## Call the function play_simulator n (here, 100) times.100 will correspond to the number of correctinteractions 
  ## happening within channels. In this case, it'll be the number of plays.
  ## For soccer data it can be the number of passes being examined 
  
  for (i in seq(1:100))
  {
    ## Create a possesion variable to recieve the returned list of plays and unlist them
    ## Call the play_simulator function and save its unlisted sequence in the possession column
    possesion = unlist(play_simulator())
    ## Tag each play to a play_id. Since we have unlisted the data in the previous channel, each row will be a 
    ## possesion instance
    play_id = rep(x = i,length(possesion))
    
    ##Combine the possesion and play_id variable using column bind
  
    passing_sequence = cbind(possesion,play_id)
    
    ## Add all the plays simulated to the plays dataframe to create the final set of data to be used for analysis
    plays = rbind(plays,passing_sequence)
  }
  return (plays)
}

##Call the function to create simulated data sets
plays= simulateData()

##View the dataframe plays to verify data simulation
View(plays)

#### Creating input for Markov Model ####

## The dataframe created above needs to be converted into a sequntial form to indentify correct the
## order of the channels or players leading to a goal or a conversion or a purchase.
## This is the format required to input data in the markov_model function of ChannelAttribution package
## While all the paths did not lead to conversion, for the sake of simplicity, we'll assign them 1 right now.

## We will use dplyr to create this reuqired correct path data.
play_paths1 = plays %>%
  group_by(play_id) %>%
  summarise(path = paste(possesion, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = 1,
            conv_null = 0) %>%
  ungroup()

View(play_paths1)

## Create another instance of the play_paths1 as play_paths and assign a result - conversion or not to them.
## For the sake of simplicity, all even play_ids convert or score and the odd play_ids dont.

play_paths = play_paths1

##Assigning the success with value of 1 for conversion or else 0
play_paths$conv = ifelse((as.numeric(play_paths$play_id)%%2) ==0,1,0)
##If the path doesnot lead to success it is identified correct using conv_null variable
play_paths$conv_null = ifelse((as.numeric(play_paths$play_id)%%2) !=0,1,0)

##Removing the play_id variable from the data set as it will not be used for further processing
play_paths$play_id = NULL

#### Creating Markov Model ####

##Loading the libraries for creating markov chain objects and channel attribution
library(markovchain)
library(ChannelAttribution)

## Create markov model using the data set created above. This function generates the transition
## probabilities based on the importance of each channel leading to a success node as explained in the presentation.

model1 = markov_model(play_paths,
                       var_path = 'path',
                       var_conv = 'conv',
                       var_null = 'conv_null',
                       out_more = TRUE)

## Create to correct variable which identifies converisons taking place for each player or channel;
res1 = model1$result

## Create a variable to store the transtion probabilities between channels. For eg from S1 to S2
## This essentially explains the probability that the ball/user will go to S2, given it is in state - S1.
## So the sum of all the values with channel_from as S1 will be 1.
trans = model1$transition_matrix

## Load the library reshape2 to reshape the data from the above format to vector format 
library(reshape2)

## Transform the probabilities of each link into a transition matrix cross-table.
## Use the dcast function to perform the reshaping
## Since this table does not include the starting and ending states, it is called df_trans1
## We will add the starting and ending states to this to create a complete transition matrix and call it df_trans
df_trans1 = dcast(trans, channel_from ~ channel_to, value.var = 'transition_probability')

## Create a dummy variable to capture the defaults conversions between 'start' ,'conversion' and 'null' states.
## The original transition matrix does not have start, end and null.
df_dummy = data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                      transition_probability = c(0, 1, 1))

##The dummay data frame is appended to the original data set to ensure complete coverage among all
##possible states
df_trans = rbind(trans, df_dummy)

##Converting the channel from column to a factor
df_trans$channel_from = factor(df_trans$channel_from,levels = c('(start)', '(conversion)', '(null)', 'S1','S2','M1','M2'))


##Converting the channel from column to a factor
df_trans$channel_to = factor(df_trans$channel_to,levels = c('(start)', '(conversion)', '(null)', 'S1','S2','M1','M2'))

## The transition matrix cross table with all states including start, conversion and null.
df_trans = dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

## The markov_object needed to be used as an input needs the transition matrix in the matrix dataframe.
## The dcast function from reshape does not give the transition matrix as a matrix.
## So we create an instnace correct of the same in the matrix format.
trans_matrix = matrix(data = as.matrix(df_trans[, -1]),
                       nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                       dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))

##Handling null values by assigning them to 0 as they donot contribute to success 
trans_matrix[is.na(trans_matrix)] = 0

##Create markov object using the transition matrix trans_matrix correct to define the plot which will be built on
##the underlying principle of markov model
markov_object = new("markovchain", transitionMatrix = trans_matrix)

##Plot the markov object using  plot command
plot(markov_object,edge.arrow.size = .05)

#### Heurisitc Models ####
##Comparison between Channel Attribution  values and Heuristic Models

heuristic = heuristic_models(play_paths,
                             var_path = 'path',
                             var_conv = 'conv')
# Comparing Markov models to Heuristic Models
comparison  = merge(x = heuristic,y = res1,by.x = 'channel_name',by.y = 'channel_name',all = T)

#### Creating Vizualizations ####

##Loading the library - RcolorBrewer for using color palettes in ggplot
library("RColorBrewer")

##Defining the colors to be used and asssining it to cols variable
cols = c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")


## Call thecolorRampPalette functions which returns a set of given colors to create a new color palette
myPalette = colorRampPalette(cols)

library(ggplot2)
library(scales)

## Use the ggplot command to create a visual which depicts the states and the corresponding transitions probabilities 
## For mapping, we use the aestetics function - aes


ggplot(data = trans, 
      ## Aesthetic mappings describe how variables in the data are mapped to visual properties in ggplot2
      mapping = aes(x = channel_to, y = channel_from, group = channel_from, color = transition_probability)) +
      
      ##Controls the size of the points on the plot
      scale_size(range = c(10, 30),guide = F) +
    
      ##Controls the size of connecting lines on the plot  
      geom_line(size = 2, alpha = 0.3) +
      
      ##Controls the desnity correct of the colours
      geom_point(aes(size = transition_probability),alpha = 0.8)+   
      scale_colour_gradientn(colours = myPalette(12))+
      
      ##Controls the  attributes of text objects on the plot
      geom_text(aes(label = paste0(" ", percent(round(transition_probability, 2)))),
      color = 'black', fontface="bold",size = 3, hjust = 0.5, vjust = 0.5, fontface = "plain")+
  
      ##Controls the  attributes of axis on the plot in terms of size color and font i.e. axis, title and ticks
      theme(
      plot.title = element_text(size=15, face="bold", vjust=2,hjust = 0.5),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      axis.text = element_text(size=16),
      axis.text.x = element_text(size=10, angle=0, hjust=.5, vjust=.5, face="bold"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),plot.background = element_rect(fill = 'grey', colour = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), panel.border = element_rect(colour = "white", fill=NA, size=1)) +
      
      # Labels for x and y axis
      labs(x = 'Channels to', y = 'Channels from') +
      
      # Title for the plot
      ggtitle("Transition Probabilities among channels")
