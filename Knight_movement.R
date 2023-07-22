library(data.table)
library(tidyverse)
df = data.table(expand.grid(c('A','B','C','D','E','F','G','H'),c(1:8)))
names(df) <- c('Rank', 'File')
df = df[order(Rank,File)]
df[,color := rep(c(0,1), 32)]
df[,id:= .GRP,.(Rank)]
df[,col2:=ifelse(id%%2==0,1,0)]
df[,color:=ifelse(col2==1,color,1-color)]
df = df[,.(Rank = as.factor(Rank),File = as.factor(File), color)]
init_plot = df %>% ggplot(aes(x = Rank, y = File))+
  geom_tile(aes(fill= color))+
  scale_fill_gradient(high = 'black', low = 'white')+
  theme_bw()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

# initial_position = df[sample(nrow(df),1),.(Rank,File)]
initial_position = data.table(Rank = 'E', File = 4)
init_plot = df %>% ggplot(aes(x = as.factor(x), y = as.factor(y)))+
  geom_tile(aes(fill= color))+
  scale_fill_gradient(high = 'black', low = 'white')+
  geom_text(data = initial_position, 
            aes(label = 'K'), 
            color = 'red', size = 5)
# Knight movements
initial_position
knight_movement <- function(position, movement){
  next_x = position[,x] + movement[,mov_x]
  next_y = position[,y] + movement[,mov_y]
  return( data.table(next_x, next_y))
}

mapping[Rank == initial_position[,Rank]]

next_positions <- function(initial_position){
  mapping = data.table(Rank = c('A','B','C','D','E','F','G','H'), 
                       Mapped = 1:8)
  mapped_coordinates = data.table(x = mapping[Rank == initial_position[,Rank],Mapped],
                                   y = initial_position[,File])
  knight_movements= data.table(mov_x = c(1,1,2,2,-1,-1,-2,-2),
                               mov_y = c(2,-2,1,-1,2,-2,1,-1))
  
  next_positions = data.table()
  for(i in 1:nrow(knight_movements)){
    temp_next = knight_movement(position = mapped_coordinates, 
                                movement = knight_movements[i,])
    next_positions = rbind(next_positions, temp_next)
  }
  mapped = mapping[next_positions, 
                   on = .(Mapped = next_x)][
    next_y %in% 1:8,.(Rank = Rank,
    File = next_y)][
      order(Rank, File)
      ] %>% 
    na.omit()
  mapped = unique(mapped)
  return(mapped)
}


initial_position
np = next_positions(initial_position = initial_position)
init_plot+
  geom_tile(data = initial_position,
            aes(
              x = Rank,
              y = File
            ),
            fill = 'blue')+
  geom_text(data = initial_position,
            aes(label = paste0('P1 ',Rank, File) ))+
  geom_tile(data = np,
            aes(
              x = Rank,
              y = File),
            fill = 'red')+
  geom_text(data = np,
            aes(label = paste0('P2 ',Rank, File) ))





init_plot+
  geom_tile(data = initial_position,
            aes(
              x = Rank,
              y = File
            ),
            fill = 'blue')+
  geom_text(data = initial_position,
            aes(label = paste0('P1 ',Rank, File) ))+
  geom_tile(data = data.table(Rank = 'E', File = 5),
            aes(
              x = Rank,
              y = File),
            fill = 'red')+
  geom_text(data = data.table(Rank = 'E', File = 5),
            aes(label = paste0('Position we want\n to get to') ))


jumper <- function(num_jumps, initial_position){
  temp_initial_positions = initial_position
  jumped_positions = data.table(initial_position, jump = 0)
  for( jump_number in 2:num_jumps){
    for(position_index in 1:nrow(temp_initial_positions)){
      next_pos = next_positions(temp_initial_positions[position_index,])
      jumped_positions = rbind(jumped_positions, 
                               data.table(next_pos, 
                                          jump = jump_number-1) 
                               )
    }
    temp_initial_positions = jumped_positions[jump==jump_number-1,]
  }
  return( jumped_positions)
}

initial_position = data.table(Rank = 'B', File = 6)
num_jumps = 6
jumps = jumper(num_jumps, initial_position)
b6 = init_plot+
  geom_tile(data = jumps[,.(min_jumps = min(jump) ),.(Rank, File)],
            aes(x = Rank, 
                y = File,
                group = min_jumps,
                fill = min_jumps))+
  geom_text(data = jumps[,.(min_jumps = min(jump) ),.(Rank, File)],
            aes(
              label = min_jumps,alpha = min_jumps/4
            ), color = 'Red', size = 10)+
  labs(title = paste0('Initial Position: ', jumps[jump==0,paste0(Rank,File)]))

num_jumps = 7
jump_data = data.table()
positions = data.table(Rank = c('A','D','E','H','G','C'),File = c(1,1,4,8,6,4))
for(i in 1:nrow(positions)){
  initial_position = positions[i,]
  jumps = jumper(num_jumps, initial_position)
  jumps[,initial_position := paste0(positions[i,paste0(Rank,File)])]
  jump_data = rbind(jump_data, jumps)
}

jump_data = jump_data[,.(min_jump = min(jump)),.(initial_position, Rank, File)]



mapping = data.table(Rank = c('A','B','C','D','E','F','G','H'), 
                     Mapped = 1:8)
diagonal = jump_data[mapping, on = .(Rank = Rank)][,
  `:=`(slope = -Mapped/File,
       intercept = File - Mapped),.(
  initial_position,
  Rank,
  File
)][slope==-1,][order(initial_position)]



diagonal = jump_data[,`:=` (init_rank = substr(initial_position,0,1), 
                 init_File = as.numeric(substr(initial_position,2,2)) )][mapping,
  on = .(init_rank = Rank)][,init_rank:=Mapped][,Mapped:=NULL][mapping,on = .(Rank = Rank)][,
  Rank_numeric:=Mapped][,Mapped:=NULL][,`:=`(
    slope = (File - init_File)/(Rank_numeric - init_rank)
  ),.(
    initial_position
  )][abs(slope) == 1]


init_plot+
  geom_vline(data = jump_data[min_jump==0,], 
             aes(xintercept = Rank),
             size = 15,
             alpha = 0.5,
             color = 'yellow')+
  geom_hline(data = jump_data[min_jump==0,], 
             aes(yintercept = File),
             size = 15,
             alpha = 0.5,
             color = 'Green')+
  geom_tile(data = diagonal,
              aes(x = Rank,
                  y = File,
                  group = initial_position),
            fill = 'Blue',
            alpha = 0.4)+
  geom_text(data = jump_data,
            aes(
              label = min_jump,alpha = min_jump/4
            ), color = 'Red', size = 10, alpha = 1)+
  facet_wrap(~initial_position,
             scales = 'free')+
  labs(title = 'Movement of the Knight')

  
  jump_data[min_jump==0,]
  

init_plot+
  geom_text(data = jumps[,.(min_jumps = min(jump) ),.(Rank, File)],
            aes(
              label = min_jumps
            ), color = 'Red', size = 10)+
  geom_line(data = data.table(Rank = c('H','A'), File= c(1,8)),
                              aes(x = Rank, 
                                  y = File,  
                                  group = 1), 
            size = 10,
            alpha = 0.5,
            color = 'yellow')+
  geom_line(data = data.table(Rank = c('A','H'), File= c(4,4)),
            aes(x = Rank, 
                y = File,  
                group = 1),
            size = 10,
            alpha = 0.5,
            color = 'Blue')+
  geom_line(data = data.table(Rank = c('E','E'), File= c(1,8)),
            aes(x = Rank, 
                y = File,  
                group = 1),
            size = 10,
            alpha = 0.5,
            color = 'Green')+
  geom_line(data = data.table(Rank = c('B','H'), File= c(1,7)),
            aes(x = Rank, 
                y = File,  
                group = 1),
            size = 10,
            alpha = 0.5,
            color = 'Yellow')


